{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-
 - Copyright 2011 Per Magnus Therning
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

module Util.Translation where

import PkgDB as DB
import Util.Misc
import Util.Dist

import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Except
import qualified Data.ByteString.Lazy as BSL (writeFile)
import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.Monoid hiding ((<>))
import           Distribution.Package as P
import           Distribution.PackageDescription as PD
import           Distribution.Text
import           Distribution.Version
import           Prelude hiding ( (<$>) )
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.IO
import           System.Process
import           System.Unix.Directory
import           Text.PrettyPrint.ANSI.Leijen hiding((</>))

#if MIN_VERSION_Cabal(2,0,0)
import Distribution.Package (mkPackageName)
import Distribution.PackageDescription (mkFlagName, unFlagName)
import Distribution.Text as T
#else
mkFlagName = FlagName
unFlagName (FlagName name) = name
mkPackageName = P.PackageName
mkVersion a = Version a []
#endif

-- {{{1 ShQuotedString
newtype ShQuotedString = ShQuotedString String
    deriving (Eq, Show)

instance Pretty ShQuotedString where
    pretty (ShQuotedString s) = char '"' <> text quotStr <> char '"'
        where
            quotStr = shEsc $ unwords $ lines s

-- {{{1 ShArray
newtype ShArray = ShArray [String]
    deriving (Eq, Show)

instance Monoid ShArray where
    mempty = ShArray []
    (ShArray a) `mappend` (ShArray b) = ShArray $ a ++ b

instance Pretty ShArray where
    pretty (ShArray a) = encloseSep (char '(') (char ')') (char ' ') (map (\ t -> char '"' <> text t <> char '"') a)

-- {{{1 ShVar
data ShVar a = ShVar String a
    deriving (Eq, Show)

{-# ANN shVarNewValue "HLint: ignore Eta reduce" #-}
shVarNewValue (ShVar n _) v = ShVar n v
shVarAppendValue (ShVar n v1) v2 = ShVar n (v1 `mappend` v2)
shVarValue (ShVar _ v) = v
shVarName (ShVar n _) = n
shVarRefName (ShVar n _) = "${" ++ n ++ "}"

instance Pretty a => Pretty (ShVar a) where
    pretty (ShVar n v) = text n <> char '=' <> pretty v

-- {{{1 ArchPkg
data ArchPkg = ArchPkg
    { apPkgName :: String
    , apHkgName :: String
    , apHasLibrary :: Bool
    , apLicenseFile :: Maybe FilePath
    , apCabalPatch :: Maybe FilePath
    , apPkgbuildPatch :: Maybe FilePath
    , apInstallPatch :: Maybe FilePath
    , apBuildPatch :: Maybe FilePath
    -- shell bits
    , apShHkgName :: ShVar String
    , apShPkgName :: ShVar String
    , apShPkgVer :: ShVar Version
    , apShPkgRel :: ShVar Int
    , apShXRev :: ShVar String
    , apShPkgDesc :: ShVar ShQuotedString
    , apShUrl :: ShVar ShQuotedString
    , apShLicence :: ShVar ShArray
    , apShMakeDepends :: ShVar ShArray
    , apShDepends :: ShVar ShArray
    , apShSource :: ShVar ShArray
    , apShInstall :: Maybe (ShVar ShQuotedString)
    , apShSha256Sums :: ShVar ShArray
    , apFlags :: FlagAssignment
    } deriving (Eq, Show)

-- {{{2 baseArchPkg
baseArchPkg = ArchPkg
    { apPkgName = ""
    , apHkgName = ""
    , apHasLibrary = False
    , apLicenseFile = Nothing
    , apCabalPatch = Nothing
    , apPkgbuildPatch = Nothing
    , apInstallPatch = Nothing
    , apBuildPatch = Nothing
    , apShHkgName = ShVar "_hkgname" ""
    , apShPkgName = ShVar "pkgname" ""
    , apShPkgVer = ShVar "_ver" (mkVersion [])
    , apShPkgRel = ShVar "pkgrel" 0
    , apShXRev = ShVar "_xrev" "0"
    , apShPkgDesc = ShVar "pkgdesc" (ShQuotedString "")
    , apShUrl = ShVar "url" (ShQuotedString "http://hackage.haskell.org/package/${_hkgname}")
    , apShLicence = ShVar "license" (ShArray [])
    , apShMakeDepends = ShVar "makedepends" (ShArray [])
    , apShDepends = ShVar "depends" (ShArray [])
    , apShSource = ShVar "source" (ShArray ["http://hackage.haskell.org/packages/archive/${_hkgname}/${_ver}/${_hkgname}-${_ver}.tar.gz", "original.cabal"])
    , apShInstall = Just $ ShVar "install" (ShQuotedString "${pkgname}.install")
    -- this is a trick to make sure that the user's setting for integrity
    -- checking in makepkg.conf isn't used, as long as this array contains
    -- something non-empty it will overrule
    , apShSha256Sums = ShVar "sha256sums" (ShArray ["0"])
    , apFlags = []
    }

-- {{{2 Pretty instance
instance Pretty ArchPkg where
    pretty (ArchPkg {..})
      = vsep [ text "# custom variables"
            , pretty apShHkgName
            , maybe empty (pretty . ShVar "_licensefile") apLicenseFile
            , pretty apShPkgVer
            , pretty apShXRev
            , empty, text "# PKGBUILD options/directives"
            , pretty apShPkgName
            , text "pkgver=${_ver}.x${_xrev}"
            , pretty apShPkgRel
            , pretty apShPkgDesc
            , pretty apShUrl
            , pretty apLicenseFile
            , text "arch=('i686' 'x86_64')"
            , pretty apShMakeDepends
            , pretty apShDepends
            , text "options=('strip' 'staticlibs')"
            , pretty apShSource
            , maybe empty pretty apInstallPatch
            , pretty apShSha256Sums
            , empty, text "# PKGBUILD functions"
            , empty
            , prepareFunction
            , empty
            , if apHasLibrary then libBuildFunction else exeBuildFunction
            , empty
            , if apHasLibrary then libPackageFunction else exePackageFunction
            , empty
            ]
            where
                prepareFunction = text "prepare() {" <>
                    nest 4 (empty <$>
                        text "cd \"${srcdir}/${_hkgname}-${_ver}\"" <$>
                        text "cp \"${srcdir}/original.cabal\" \"${srcdir}/${_hkgname}-${_ver}/${_hkgname}.cabal\"" <$>
                        empty <$>
                        maybe (text "# no source patch") (\ _ ->
                            text "patch -p4 < \"${srcdir}/source.patch\"")
                            apBuildPatch
                        ) <$>
                    char '}'
                libBuildFunction = text "build() {" <>
                    nest 4 (empty <$>
                        text "cd \"${srcdir}/${_hkgname}-${_ver}\"" <$>
                        empty <$>
                        nest 4 (text "runhaskell Setup configure -O --enable-library-profiling --enable-shared \\" <$>
                            text "--prefix=/usr --docdir=\"/usr/share/doc/${pkgname}\" \\" <$>
                            text "--libsubdir=\\$compiler/site-local/\\$pkgid" <> confFlags) <$>
                        text "runhaskell Setup build" <$>
                        text "runhaskell Setup haddock --hoogle --html" <$>
                        text "runhaskell Setup register --gen-script" <$>
                        text "runhaskell Setup unregister --gen-script" <$>
                        text "sed -i -r -e \"s|ghc-pkg.*unregister[^ ]* |&'--force' |\" unregister.sh"
                        ) <$>
                    char '}'

                exeBuildFunction = text "build() {" <>
                    nest 4 (empty <$> text "cd \"${srcdir}/${_hkgname}-${_ver}\"" <$>
                        empty <$>
                        text "runhaskell Setup configure -O --prefix=/usr --docdir=\"/usr/share/doc/${pkgname}\"" <> confFlags <$>
                        text "runhaskell Setup build"
                        ) <$>
                    char '}'

                confFlags = if null apFlags
                    then empty
                    else text " \\" <>
                        nest 4 (empty <$> (hsep . map (uncurry (<>)) $ zip (repeat $ text "-f") (map prettyFlag apFlags)))

                libPackageFunction = text "package() {" <>
                    nest 4 (empty <$>
                        text "cd \"${srcdir}/${_hkgname}-${_ver}\"" <$>
                        empty <$>
                        text "install -D -m744 register.sh   \"${pkgdir}/usr/share/haskell/${pkgname}/register.sh\"" <$>
                        text "install    -m744 unregister.sh \"${pkgdir}/usr/share/haskell/${pkgname}/unregister.sh\"" <$>
                        text "install -d -m755 \"${pkgdir}/usr/share/doc/ghc/html/libraries\"" <$>
                        text "ln -s \"/usr/share/doc/${pkgname}/html\" \"${pkgdir}/usr/share/doc/ghc/html/libraries/${_hkgname}\"" <$>
                        text "runhaskell Setup copy --destdir=\"${pkgdir}\"" <$>
                        maybe empty (\ _ -> text "install -D -m644 \"${_licensefile}\" \"${pkgdir}/usr/share/licenses/${pkgname}/LICENSE\"" <$>
                            text "rm -f \"${pkgdir}/usr/share/doc/${pkgname}/${_licensefile}\"") apLicenseFile
                        ) <$>
                    char '}'

                exePackageFunction = text "package() {" <>
                    nest 4 (empty <$> text "cd \"${srcdir}/${_hkgname}-${_ver}\"" <$>
                        text "runhaskell Setup copy --destdir=\"${pkgdir}\""
                        ) <$>
                    char '}'

-- {{{1 ArchInstall
data ArchInstall = ArchInstall
    { aiShPkgName :: ShVar String
    } deriving (Eq, Show)

-- {{{2 baseArchInstall
baseArchInstall = ArchInstall
    { aiShPkgName = ShVar "pkgname" ""
    }

-- {{{2 ArchInstall from ArchPackage
aiFromAP (ArchPkg { apShPkgName = pkgName }) =
    ArchInstall { aiShPkgName = pkgName }

-- {{{2 Pretty instance
instance Pretty ArchInstall where
    pretty (ArchInstall
        { aiShPkgName = pkgName
        }) = vsep
            [ text "# custom variables"
            , pretty pkgName
            , pretty (ShVar "HS_DIR" "usr/share/haskell/${pkgname}")
            , empty, text "# functions"
            , postInstallFunction
            , empty, preUpgradeFunction
            , empty, postUpgradeFunction
            , empty, preRemoveFunction
            , empty
            ]
            where
                postInstallFunction = text "post_install() {" <>
                    nest 4 (empty <$> text "${HS_DIR}/register.sh --verbose=0") <$>
                    char '}'
                preUpgradeFunction = text "pre_upgrade() {" <>
                    nest 4 (empty <$> text "${HS_DIR}/unregister.sh --verbose=0") <$>
                    char '}'
                postUpgradeFunction = text "post_upgrade() {" <>
                    nest 4 (empty <$> text "${HS_DIR}/register.sh --verbose=0") <$>
                    char '}'
                preRemoveFunction = text "pre_remove() {" <>
                    nest 4 (empty <$> text "${HS_DIR}/unregister.sh --verbose=0") <$>
                    char '}'

-- {{{1 extra instances
instance Pretty Version where
#if MIN_VERSION_Cabal(2,0,0)
    pretty = text . display
#else
    pretty (Version b _) = encloseSep empty empty (char '.') (map pretty b)
#endif

prettyFlag (unFlagName -> n, True) = text n
prettyFlag (unFlagName -> n, False) = text $ '-' : n

-- {{{1 translate
-- TODO:
--  • translation of extraLibDepends-libs to Arch packages
translate :: Version -> Int -> CblDB -> FlagAssignment -> PackageDescription -> ArchPkg
translate ghcVer ghcRel db fa pd = let
        ap = baseArchPkg
        hkgName = unPackageName $ packageName pd
        pkgVer = packageVersion pd
        pkgRel = maybe 1 pkgRelease (lookupPkg db hkgName)
        xrev = show $ Util.Dist.pkgXRev pd
        hasLib = isJust (library pd)
        licFn = let l = licenseFiles pd in if null l then Nothing else Just (head l)
        archName = (if hasLib then "haskell-" else "") ++ map toLower hkgName
        pkgDesc = synopsis pd
        url = if null (homepage pd) then "http://hackage.haskell.org/package/${_hkgname}" else homepage pd
        lic = display (license pd)
        makeDepends = if hasLib then [] else ghcVersionDep ghcVer ghcRel : calcExactDeps db pd
        depends = if hasLib then ghcVersionDep ghcVer ghcRel : calcExactDeps db pd else []
        extraLibDepends = maybe [] (extraLibs . libBuildInfo) (library pd)
        install = if hasLib then apShInstall ap else Nothing
    in ap
        { apPkgName = archName
        , apHkgName = hkgName
        , apHasLibrary = hasLib
        , apLicenseFile = licFn
        , apShHkgName = shVarNewValue (apShHkgName ap) hkgName
        , apShPkgName = shVarNewValue (apShPkgName ap) archName
        , apShPkgVer = shVarNewValue (apShPkgVer ap) pkgVer
        , apShPkgRel = shVarNewValue (apShPkgRel ap) pkgRel
        , apShXRev = shVarNewValue (apShXRev ap) xrev
        , apShPkgDesc = shVarNewValue (apShPkgDesc ap) (ShQuotedString pkgDesc)
        , apShUrl = shVarNewValue (apShUrl ap) (ShQuotedString url)
        , apShLicence = shVarNewValue (apShLicence ap) (ShArray [lic])
        , apShMakeDepends = shVarNewValue (apShMakeDepends ap) (ShArray makeDepends)
        , apShDepends = shVarNewValue (apShDepends ap) (ShArray $ depends ++ extraLibDepends)
        , apShInstall = install
        , apFlags = fa
        }

-- Calculate exact dependencies based on the package in a CblDB.  We assume the
-- same db has been used to finalise the package so it's all right to use
-- fromJust.
-- TODO:
--  • this is most likely too simplistic to create the Arch package names
--  correctly for all possible dependencies
calcExactDeps db pd = map depString deps
    where
        remPkgs = map DB.pkgName (filter isGhcPkg db) ++ [pkgNameStr pd]
        deps = filter (not . (`elem` remPkgs)) (map depName (buildDepends pd))

        depString n = "haskell-" ++ name ++ "=" ++ ver ++ ".x" ++ xrev ++ "-" ++ rel
            where
                pkg = fromJust $ lookupPkg db n
                name = map toLower $ DB.pkgName pkg
                ver = display $ DB.pkgVersion pkg
                xrev = show $ DB.pkgXRev pkg
                rel = show $ pkgRelease pkg

-- {{{1 stuff with patches
-- {{{2 addPatches
addPatches patchDir ap = let
        hkgName = apHkgName ap
        sources = apShSource ap
        fi tF fF v = if v then tF else fF
        cabalPatchFn = patchDir </> hkgName <.> "cabal"
        pkgbuildPatchFn = patchDir </> hkgName <.> "pkgbuild"
        installPatchFn = patchDir </> hkgName <.> "install"
        buildPatchFn = patchDir </> hkgName <.> "source"
    in do
        cabalPatch <- doesFileExist cabalPatchFn >>= fi (liftM Just $ canonicalizePath cabalPatchFn) (return Nothing)
        pkgBuildPatch <- doesFileExist pkgbuildPatchFn >>= fi (liftM Just $ canonicalizePath pkgbuildPatchFn) (return Nothing)
        installPatch <- doesFileExist installPatchFn >>= fi (liftM Just $ canonicalizePath installPatchFn) (return Nothing)
        buildPatch <- doesFileExist buildPatchFn >>= fi (liftM Just $ canonicalizePath buildPatchFn) (return Nothing)
        let sources' = shVarAppendValue sources
                (ShArray $ catMaybes [maybe Nothing (const $ Just "source.patch") buildPatch])
        return ap
            { apCabalPatch = cabalPatch
            , apPkgbuildPatch = pkgBuildPatch
            , apInstallPatch = installPatch
            , apBuildPatch = buildPatch
            , apShSource = sources'
            }

-- {{{2 copyPatches
copyPatches destDir ap = let
        cabalPatch = apCabalPatch ap
        buildPatch = apBuildPatch ap
    in do
        maybe (return ()) (\ fn -> copyFile fn (destDir </> "cabal.patch")) cabalPatch
        maybe (return ()) (\ fn -> copyFile fn (destDir </> "source.patch")) buildPatch

-- {{{1 addHashes
addHashes ap cbl tmpDir = let
        hashes = map (filter (`elem` "1234567890abcdef")) . lines . drop 11
        pkgbuildFn = tmpDir </> "PKGBUILD"
        installFn = tmpDir </> apPkgName ap <.> "install"
        cblFn = tmpDir </> "original.cabal"
        pkgbuildPatch = apPkgbuildPatch ap
    in do
        liftIO $ copyPatches tmpDir ap
        liftIO $ withFile pkgbuildFn WriteMode (`hPutDoc` pretty ap)
        liftIO $ withFile installFn WriteMode (`hPutDoc` (pretty $ aiFromAP ap))
        maybe (return ()) (void . applyPatch pkgbuildFn) pkgbuildPatch
        liftIO $ BSL.writeFile cblFn cbl
        (ec, out, _) <- liftIO $ withWorkingDirectory tmpDir (readProcessWithExitCode "makepkg" ["-g"] "")
        case ec of
            ExitFailure _ ->
                throwE $ "makepkg: error while calculating the source hashes for " ++ apHkgName ap
            ExitSuccess ->
                return (if "sha256sums=(" `isPrefixOf` out then replaced else ap)
                    where
                        replaced = ap { apShSha256Sums = shVarNewValue (apShSha256Sums ap) (ShArray $ hashes out) }
