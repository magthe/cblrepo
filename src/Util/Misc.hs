{-
 - Copyright 2011-2013 Per Magnus Therning
 -
 - Licensed under the Apache License, Version 2.0 (the "License");
 - you may not use this file except in compliance with the License.
 - You may obtain a copy of the License at
 -
 -     http://www.apache.org/licenses/LICENSE-2.0
 -
 - Unless required by applicable law or agreed to in writing, software
 - distributed under the License is distributed on an "AS IS" BASIS,
 - WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 - See the License for the specific language governing permissions and
 - limitations under the License.
 -}

module Util.Misc where

-- {{{1 imports
import qualified PkgDB as DB

import Codec.Archive.Tar as Tar
import Codec.Compression.GZip as GZip
import Control.Exception (onException)
import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Data.Either
import Data.List
import Data.Version
import Distribution.Compiler
import Distribution.Package as P
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.PackageDescription.Parse
import Distribution.System
import Distribution.Text
import Distribution.Verbosity
import Distribution.Version
import Options.Applicative
import Safe (lastMay)
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Posix.Files
import System.Process
import System.Unix.Directory
import Text.ParserCombinators.ReadP (readP_to_S)
import qualified Data.ByteString.Lazy.Char8 as BS

-- {{{1 dependency
depName (Dependency (PackageName n) _) = n
depVersionRange (Dependency _ vr) = vr

-- {{{ print functions
printUnSat (n, ds) = do
    putStrLn $ "Failed to satisfy the following dependencies for " ++ n ++ ":"
    mapM_ (putStrLn . ("  " ++) . display) ds

printBrksOth  ((n, v), brks) = do
    putStrLn $ "Adding " ++ n ++ " " ++ display v ++ " would break:"
    mapM_ (\ (bN, Just bD) -> putStrLn $ "  " ++ bN ++ " : " ++ display bD) brks

-- {{{1 program variables
progName = "cblrepo"
dbName = progName ++ ".db"

ghcDefVersion = Version [7, 8, 3] []
ghcVersionDep :: Version -> Int -> String
ghcVersionDep ghcVer ghcRel = "ghc=" ++ display ghcVer ++ "-" ++ show ghcRel

indexUrl = "http://hackage.haskell.org/packages/index.tar.gz"
indexFileName = "index.tar.gz"

-- {{{1 command line parser helpers
readerGhcVersion :: String -> ReadM Version
readerGhcVersion arg = case lastMay $ readP_to_S parseVersion arg of
    Just (v, "") -> return v
    _ -> fail $ "cannot parse value `" ++ arg ++ "`"

flagReader :: String -> ReadM (FlagName, Bool)
flagReader ('-':xs) = return (FlagName xs, False)
flagReader      xs  = return (FlagName xs, True)

strPairArg :: Monad m => String -> m (String, String)
strPairArg s = let
        (s0, s1) = break (== ',') s
    in if null s1 then error "Missing version" else return (s0, tail s1)

strTripleArg :: Monad m => String -> m (String, String, String)
strTripleArg s = let
        (s0, r1) = break (== ',') s
        (s1, r2) = break (== ',') (tail r1)
    in return (s0, s1, tail r2)

-- Helper for grabbing things out of the options
optGet :: MonadReader o m => (o -> s) -> m s
optGet f = liftM f ask

-- {{{1 command line argument type
data Cmds
    = CmdAdd
        { patchDir :: FilePath, ghcVer :: Version, flags :: FlagAssignment, cmdAddGhcPkgs :: [(String,String)]
        , cmdAddDistroPkgs :: [(String, String, String)], cmdAddUrlCbls :: [String]
        , cmdAddFileCbls :: [FilePath], cmdAddCbls :: [(String, String)] }
    | CmdBuildPkgs { pkgs :: [String] }
    | CmdBumpPkgs { inclusive :: Bool, pkgs :: [String] }
    | CmdSync { unused :: Bool }
    | CmdVersions { latest :: Bool, pkgs :: [String] }
    | CmdListPkgs
        { listGhc :: Bool, listDistro :: Bool, noListRepo :: Bool
        , hackageFmt :: Bool, pkgs :: [String] }
    | CmdUpdates { idxStyle :: Bool }
    | CmdUrls { pkgVers :: [(String, String)] }
    | CmdPkgBuild { ghcVer :: Version, ghcRel :: Int, patchDir :: FilePath, pkgs :: [String] }
    | CmdConvertDb { inDbFile :: FilePath, outDbFile :: FilePath }
    | CmdRemovePkg { pkgs :: [String] }
    deriving (Show)

data Opts = Opts
    { appDir :: FilePath
    , dbFile :: FilePath
    , dryRun :: Bool
    , optsCmd :: Cmds
    } deriving (Show)

-- {{{1 getFromURL
getFromURL url fn = do
    (ec, _, er) <- readProcessWithExitCode "curl" ["-f", "-L", "-o", fn, url] ""
    case ec of
        ExitSuccess -> return ()
        ExitFailure _ -> do
            hPutStrLn stderr ("Failed downloading " ++ url)
            hPutStrLn stderr er
            exitFailure

-- {{{1 applyPatchIfExist
applyPatch origFilename patchFilename = do
    (ec, _, _) <- liftIO $ readProcessWithExitCode "patch" [origFilename, patchFilename] ""
    case ec of
        ExitSuccess -> return ()
        ExitFailure _ ->
            throwError ("Failed patching " ++ origFilename ++ " with " ++ patchFilename)

applyPatchIfExist origFilename patchFilename =
    liftIO (fileExist patchFilename) >>= flip when (applyPatch origFilename patchFilename)

-- {{{1 index functions
readIndexFile indexLocation = exitOnException
    "Cannot open index file, have you run the 'sync' command?"
    (BS.readFile $ indexLocation </> indexFileName)

-- {{{1 package descriptions
-- {{{2 readCabal
data LocType = Url | Idx | File

-- | Read in a Cabal file.
readCabalFromUrl = readCabal
readCabalFromFile = readCabal
readCabalFromIdx pd (p, v) = readCabal pd (p ++ "," ++ v)

readCabal :: FilePath -> String -> FilePath -> ErrorT String IO GenericPackageDescription
readCabal patchDir loc tmpDir = let
        locType
            | isInfixOf "://" loc = Url
            | ',' `elem` loc = Idx
            | otherwise = File

        copyCabal tmpDir loc = copyFile loc fn >> return fn
            where fn = tmpDir </> takeFileName loc

        downloadCabal tmpDir loc = getFromURL loc fn >> return fn
            where
                fn = tmpDir </> takeFileName loc

        extractCabal tmpDir loc = let
                (p, _: v) = span (/= ',') loc
                path = p </> v </> p ++ ".cabal"
                pkgStr = p ++ " " ++ v
                fn = tmpDir </> (p ++ ".cabal")

                esFindEntry p (Next e es) = if p == entryPath e
                    then Just e
                    else esFindEntry p es
                esFindEntry _ _ = Nothing

                eGetContent e = let
                        ec = entryContent e
                    in case ec of
                        NormalFile c _ -> Just $ BS.unpack c
                        _ -> Nothing

            in do
                aD <- liftIO $ getAppUserDataDirectory "cblrepo"
                es <- liftM (Tar.read . GZip.decompress) (liftIO $ readIndexFile aD)
                e <- maybe (throwError $ "No entry for " ++ pkgStr)
                    return
                    (esFindEntry path es)
                cbl <- maybe (throwError $ "Failed to extract contents for " ++ pkgStr)
                    return
                    (eGetContent e)
                liftIO $ writeFile fn cbl
                return fn

        extractName fn = liftM name $ readPackageDescription silent fn
            where
                packageName (PackageName s) = s
                name = packageName . pkgName . package . packageDescription

    in do
        cblFn <- case locType of
            File -> liftIO $ copyCabal tmpDir loc
            Idx -> extractCabal tmpDir loc
            Url -> liftIO $ downloadCabal tmpDir loc
        pn <- liftIO $ extractName cblFn
        let patchFn = patchDir </> pn <.> "cabal"
        applyPatchIfExist cblFn patchFn
        liftIO $ readPackageDescription silent cblFn

-- {{{2 finalising
finalizePkg ghcVersion db gpd = let
        n = ((\ (P.PackageName n) -> n ) . P.pkgName . package . packageDescription) gpd
    in finalizePackageDescription
        [] -- no flags
        (checkAgainstDb db n)
        (Platform X86_64 buildOS) -- platform
        (CompilerId GHC ghcVersion)  -- compiler version
        [] -- no additional constraints
        gpd

checkAgainstDb db name dep = let
        dN = depName dep
        dVR = depVersionRange dep
    in (dN == name) ||
            (case DB.lookupPkg db dN of
                Nothing -> False
                Just (DB.CP _ p) -> withinRange (DB.version p) dVR)

-- {{{1 Command type
type Command a = ReaderT Opts IO a

runCommand cmds func = runReaderT func cmds

-- {{{1 ErrorT
withTempDirErrT fp func = let
        reWrapErrT (Left e) = throwError e
        reWrapErrT (Right v) = return v
    in do
        r <- liftIO $ withTemporaryDirectory fp (runErrorT . func)
        reWrapErrT r

exitOnErrors vs = let
        es = lefts vs
    in
        if not $ null $ lefts vs
            then liftIO $ mapM_ (hPutStrLn stderr) es >> exitFailure
            else return (rights vs)

exitOnException msg a = onException a $ hPutStrLn stderr msg >> exitFailure
