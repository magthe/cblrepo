{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-
 - Copyright 2011-2015 Per Magnus Therning
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
import Util.Dist
import Util.Cfg

import Control.Exception (onException)
import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.Reader
import Data.Either
import Data.List
import qualified Data.Version as DV
import Distribution.Compiler
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.System
import Distribution.Text
import Distribution.Version
import Options.Applicative
import Options.Applicative.Types
import Safe (lastMay)
import System.Exit
import System.IO
import System.Posix.Files
import System.Process
import System.Unix.Directory
import Text.ParserCombinators.ReadP hiding (many)

#if !MIN_VERSION_Cabal(2,0,0)
mkFlagName = FlagName
#endif

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

ghcDefVersion =
#if __GLASGOW_HASKELL__ >= 802
  mkVersion v
#else
  Version v []
#endif
  where
    v = reverse $ unfoldr (\x -> if x == 0 then Nothing else Just (x `mod` 10, x `div` 10)) __GLASGOW_HASKELL__

ghcDefRelease = 1 :: Int
ghcVersionDep :: Version -> Int -> String
ghcVersionDep ghcVer ghcRel = "ghc=" ++ display ghcVer ++ "-" ++ show ghcRel

#if __GLASGOW_HASKELL__ >= 802
parseVersion = (mkVersion . DV.versionBranch) <$> DV.parseVersion
#else
parseVersion = DV.parseVersion
#endif

-- {{{1 command line parser helpers
readPkgNVersion :: ReadP (String, Version)
readPkgNVersion = do
    n <- many (satisfy (/= ','))
    char ','
    v <- parseVersion
    return (n, v)

readFlag :: ReadP (FlagName, Bool)
readFlag = readNegFlag <++ readPosFlag
    where
        readNegFlag = do
            char '-'
            n <- many (satisfy (/= ','))
            return (mkFlagName n, False)

        readPosFlag = do
            n0 <- get
            n <- many (satisfy (/= ','))
            return (mkFlagName (n0 : n), True)

ghcVersionArgReader :: ReadM Version
ghcVersionArgReader = do
    arg <- readerAsk
    case lastMay $ readP_to_S parseVersion arg of
        Just (v, "") -> return v
        _ -> fail $ "cannot parse value `" ++ arg ++ "`"

pkgNVersionArgReader :: ReadM (String, Version)
pkgNVersionArgReader = do
    s <- readerAsk
    case lastMay (readP_to_S readPkgNVersion s) of
        Just (r, "") -> return r
        _ -> fail $ "Cannot parse '" ++ s ++ "' as PKG,VER"

ghcPkgArgReader :: ReadM (String, Version)
ghcPkgArgReader = pkgNVersionArgReader

distroPkgArgReader :: ReadM (String, Version, Int, Int)
distroPkgArgReader = let
        readDistroPkg = do
            (n, v) <- readPkgNVersion
            char ','
            x <- many (satisfy (/= ','))
            char ','
            r <- many (satisfy (/= ','))
            return (n, v, read x, read r)

    in do
        s <- readerAsk
        case lastMay (readP_to_S readDistroPkg s) of
            Just (r, "") -> return r
            _ -> fail $ "Cannot parse '" ++ s ++ "' as PKG,VER,XREV,REL"

strCblFileArgReader :: ReadM (FilePath, FlagAssignment)
strCblFileArgReader = let
        readWithFlags = do
            fn <- many $ satisfy (/= ':')
            char ':'
            fas <- sepBy readFlag (char ',')
            return (fn, fas)

        readWithoutFlags = do
            fn <- many $ satisfy (/= ':')
            return (fn, [])

    in do
        s <-readerAsk
        case lastMay (readP_to_S (readWithFlags <++ readWithoutFlags) s) of
            Just (r, "") -> return r
            _ -> fail $ "Cannot parse '" ++ s ++ "' as FILE[:FLAG,-FLAG,..]"

strCblPkgArgReader :: ReadM (String, Version, FlagAssignment)
strCblPkgArgReader = let
        readWithFlags = do
            (n, v) <- readPkgNVersion
            char ':'
            fas <- sepBy readFlag (char ',')
            return (n, v, fas)

        readWithoutFlags = do
            (n, v) <- readPkgNVersion
            return (n, v, [])

    in do
        s <- readerAsk
        case lastMay (readP_to_S (readWithFlags <++ readWithoutFlags) s) of
            Just (r, "") -> return r
            _ -> fail $ "Cannot parse: " ++ s

listFormatReader :: ReadM CmdListFormat
listFormatReader = do
    s <- readerAsk
    case s of
        "normal" -> return CmdListNormalFmt
        "short" -> return CmdListShortFmt
        "hackage" -> return CmdListHackageFmt
        _ -> fail $ "Cannot parse: " ++ s

-- {{{1 command line argument type
data CmdListFormat = CmdListNormalFmt | CmdListShortFmt | CmdListHackageFmt
    deriving (Eq, Show)

data Cmds
    = CmdAdd
        { patchDir :: FilePath, ghcVer :: Version, cmdAddGhcPkgs :: [(String, Version)]
        , cmdAddDistroPkgs :: [(String, Version, Int, Int)], cmdAddFileCbls :: [(FilePath, FlagAssignment)]
        , cmdAddCbls :: [(String, Version, FlagAssignment)] }
    | CmdBuildPkgs { pkgs :: [String] }
    | CmdBumpPkgs { inclusive :: Bool, pkgs :: [String] }
    | CmdUpdate { unused :: Bool }
    | CmdVersions { latest :: Bool, pkgs :: [String] }
    | CmdListPkgs
        { listGhc :: Bool, listDistro :: Bool, noListRepo :: Bool
        , listFmt :: CmdListFormat, pkgs :: [String] }
    | CmdUpgrades { idxStyle :: Bool, xrevs :: Bool }
    | CmdPkgBuild { ghcVer :: Version, ghcRel :: Int, patchDir :: FilePath, pkgs :: [String] }
    | CmdConvertDb { inDbFile :: FilePath, outDbFile :: FilePath }
    | CmdRemovePkg { pkgs :: [String] }
    | CmdExtract { cmdExtractPkgs :: [(String, Version)] }
    | CmdCreateConfig
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
            throwE ("Failed patching " ++ origFilename ++ " with " ++ patchFilename)

applyPatchIfExist origFilename patchFilename =
    liftIO (fileExist patchFilename) >>= flip when (applyPatch origFilename patchFilename)

-- {{{1 finalising package descriptions
finalizePkg ghcVersion db fa gpd = finalizePackageDescription
        fa
        (DB.checkAgainstDb db n)
        (Platform X86_64 buildOS) -- platform
        (unknownCompilerInfo (CompilerId GHC ghcVersion) NoAbiTag)  -- compiler version
        [] -- no additional constraints
        gpd
    where
        n = pkgNameStr gpd

-- {{{1 Command type
type Command = ReaderT (Opts, Cfg) IO

runCommand cmds func = runReaderT func cmds

-- {{{1 ExceptT
reThrowE :: Monad m => Either a b -> ExceptT a m b
reThrowE = either throwE return

withTempDirExceptT :: MonadIO m => FilePath -> (FilePath -> ExceptT e IO b) -> ExceptT e m b
withTempDirExceptT fp func = do
    r <- liftIO $ withTemporaryDirectory fp (runExceptT . func)
    reThrowE r

exitOnAnyLefts vs = let
        es = lefts vs
    in
        if not $ null $ lefts vs
            then liftIO $ mapM_ (hPutStrLn stderr) es >> exitFailure
            else return (rights vs)

exitOnException msg a = onException a $ hPutStrLn stderr msg >> exitFailure

-- {{{1 shell escaping
shEsc :: String -> String
shEsc s = concatMap escChar s
    where
        escChar '\"' = "\\\""
        escChar '`' = "\\`"
        escChar '\'' = "\\\'"
        escChar c = [c]
