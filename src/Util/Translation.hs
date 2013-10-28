{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}
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

--
import Add
import Distribution.PackageDescription.Parse
import Distribution.Verbosity
--

import PkgDB as DB
import Util.Misc

import Control.Monad
import Control.Monad.Error
import Data.Char
import Data.List
import Data.Maybe
import Data.Monoid hiding ((<>))
import Data.Version
import Distribution.Package as P
import Distribution.PackageDescription as PD
import Distribution.Text
import System.Directory
import System.Exit
import System.FilePath
import System.Process
import System.Unix.Directory
import Text.PrettyPrint.ANSI.Leijen hiding((</>))

-- {{{1 ShQuotedString
newtype ShQuotedString = ShQuotedString String
    deriving (Eq, Show)

instance Pretty ShQuotedString where
    pretty (ShQuotedString s) = char '"' <> text s <> char '"'

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

shVarNewValue (ShVar n _) v = ShVar n v
shVarAppendValue (ShVar n v1) v2 = ShVar n (v1 `mappend` v2)
shVarValue (ShVar _ v) = v

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
    , apShPkgRel :: ShVar String
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
    , apShPkgVer = ShVar "pkgver" (Version [] [])
    , apShPkgRel = ShVar "pkgrel" "0"
    , apShPkgDesc = ShVar "pkgdesc" (ShQuotedString "")
    , apShUrl = ShVar "url" (ShQuotedString "http://hackage.haskell.org/package/${_hkgname}")
    , apShLicence = ShVar "license" (ShArray [])
    , apShMakeDepends = ShVar "makedepends" (ShArray [])
    , apShDepends = ShVar "depends" (ShArray [])
    , apShSource = ShVar "source" (ShArray ["http://hackage.haskell.org/packages/archive/${_hkgname}/${pkgver}/${_hkgname}-${pkgver}.tar.gz"])
    , apShInstall = Just $ ShVar "install" (ShQuotedString "${pkgname}.install")
    -- this is a trick to make sure that the user's setting for integrity
    -- checking in makepkg.conf isn't used, as long as this array contains
    -- something non-empty it will overrule
    , apShSha256Sums = ShVar "sha256sums" (ShArray ["0"])
    , apFlags = []
    }

-- {{{2 Pretty instance
instance Pretty ArchPkg where
    pretty (ArchPkg
        { apHasLibrary = hasLib
        , apLicenseFile = licenseFile
        , apCabalPatch = cabalPatchFile
        , apBuildPatch = buildPatchFile
        , apShHkgName = hkgName
        , apShPkgName = pkgName
        , apShPkgVer = pkgVer
        , apShPkgRel = pkgRel
        , apShPkgDesc = pkgDesc
        , apShUrl = url
        , apShLicence = pkgLicense
        , apShMakeDepends = makeDepends
        , apShDepends = depends
        , apShSource = source
        , apShInstall = install
        , apShSha256Sums = sha256sums
        , apFlags = flags
        }) = vsep
            [ text "# custom variables"
            , pretty hkgName
            , maybe empty (\ fn -> pretty $ ShVar "_licensefile" fn) licenseFile
            , empty, text "# PKGBUILD options/directives"
            , pretty pkgName
            , pretty pkgVer
            , pretty pkgRel
            , pretty pkgDesc
            , pretty url
            , pretty pkgLicense
            , text "arch=('i686' 'x86_64')"
            , pretty makeDepends
            , pretty depends
            , text "options=('strip' 'staticlibs')"
            , pretty source
            , maybe empty pretty install
            , pretty sha256sums
            , empty, text "# PKGBUILD functions"
            , if hasLib then libBuildFunction else exeBuildFunction
            , empty
            , if hasLib then libPackageFunction else exePackageFunction
            , empty
            ]
            where
                libBuildFunction = text "build() {" <>
                    nest 4 (empty <$>
                        text "cd \"${srcdir}/${_hkgname}-${pkgver}\"" <$>
                        maybe empty (\ _ ->
                            text $ "patch " ++ shVarValue hkgName ++ ".cabal \"${srcdir}/cabal.patch\" ")
                            cabalPatchFile <$>
                        maybe empty (\ _ ->
                            text $ "patch -p4 < \"${srcdir}/source.patch\"")
                            buildPatchFile <$>
                        nest 4 (text "runhaskell Setup configure -O -p --enable-split-objs --enable-shared \\" <$>
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
                    nest 4 (empty <$> text "cd \"${srcdir}/${_hkgname}-${pkgver}\"" <$>
                        maybe empty
                            (\ _ -> text $ "patch " ++ shVarValue hkgName ++ ".cabal \"${srcdir}/cabal.patch\" ")
                            cabalPatchFile <$>
                        maybe empty (\ _ ->
                            text $ "patch -p4 < \"${srcdir}/source.patch\"")
                            buildPatchFile <$>
                        text "runhaskell Setup configure -O --prefix=/usr --docdir=\"/usr/share/doc/${pkgname}\"" <> confFlags <$>
                        text "runhaskell Setup build"
                        ) <$>
                    char '}'

                confFlags = if null flags
                    then empty
                    else text " \\" <>
                        nest 4 (empty <$> (hsep . map (uncurry (<>)) $ zip (repeat $ text "-f") (map pretty flags)))

                libPackageFunction = text "package() {" <>
                    nest 4 (empty <$>
                        text "cd \"${srcdir}/${_hkgname}-${pkgver}\"" <$>
                        text "install -D -m744 register.sh   \"${pkgdir}/usr/share/haskell/${pkgname}/register.sh\"" <$>
                        text "install    -m744 unregister.sh \"${pkgdir}/usr/share/haskell/${pkgname}/unregister.sh\"" <$>
                        text "install -d -m755 \"${pkgdir}/usr/share/doc/ghc/html/libraries\"" <$>
                        text "ln -s \"/usr/share/doc/${pkgname}/html\" \"${pkgdir}/usr/share/doc/ghc/html/libraries/${_hkgname}\"" <$>
                        text "runhaskell Setup copy --destdir=\"${pkgdir}\"" <$>
                        (maybe empty (\ _ -> text "install -D -m644 \"${_licensefile}\" \"${pkgdir}/usr/share/licenses/${pkgname}/LICENSE\"" <$>
                            text "rm -f \"${pkgdir}/usr/share/doc/${pkgname}/${_licensefile}\"") licenseFile)
                        ) <$>
                    char '}'

                exePackageFunction = text "package() {" <>
                    nest 4 (empty <$> text "cd \"${srcdir}/${_hkgname}-${pkgver}\"" <$>
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
            , empty, postRemoveFunction
            , empty
            ]
            where
                postInstallFunction = text "post_install() {" <>
                    nest 4 (empty <$> text "${HS_DIR}/register.sh" <$>
                        text "/usr/share/doc/ghc/html/libraries/arch-gen-contents-index") <$>
                    char '}'
                preUpgradeFunction = text "pre_upgrade() {" <>
                    nest 4 (empty <$> text "${HS_DIR}/unregister.sh") <$>
                    char '}'
                postUpgradeFunction = text "post_upgrade() {" <>
                    nest 4 (empty <$> text "${HS_DIR}/register.sh" <$>
                        text "/usr/share/doc/ghc/html/libraries/arch-gen-contents-index") <$>
                    char '}'
                preRemoveFunction = text "pre_remove() {" <>
                    nest 4 (empty <$> text "${HS_DIR}/unregister.sh") <$>
                    char '}'
                postRemoveFunction = text "post_remove() {" <>
                    nest 4 (empty <$> text "/usr/share/doc/ghc/html/libraries/arch-gen-contents-index") <$>
                    char '}'

-- {{{1 extra instances
instance Pretty Version where
    pretty (Version b _) = encloseSep empty empty (char '.') (map pretty b)

instance Pretty (FlagName, Bool) where
    pretty (FlagName n, True) = text n
    pretty (FlagName n, False) = text $ '-' : n

-- {{{1 translate
-- TODO:
--  • translation of extraLibDepends-libs to Arch packages
translate db fa pd = let
        ap = baseArchPkg
        (PackageName hkgName) = packageName pd
        pkgVer = packageVersion pd
        pkgRel = maybe "1" pkgRelease (lookupPkg db hkgName)
        hasLib = maybe False (const True) (library pd)
        licFn = let l = licenseFile pd in if null l then Nothing else Just l
        archName = (if hasLib then "haskell-" else "") ++ (map toLower hkgName)
        pkgDesc = synopsis pd
        url = if null (homepage pd) then "http://hackage.haskell.org/package/${_hkgname}" else (homepage pd)
        lic = display (license pd)
        makeDepends = if hasLib then [] else [ghcVersionDep] ++ calcExactDeps db pd
        depends = if hasLib then [ghcVersionDep] ++ calcExactDeps db pd else []
        extraLibDepends = maybe [] (extraLibs . libBuildInfo) (library pd)
        install = if hasLib then (apShInstall ap) else Nothing
    in ap
        { apPkgName = archName
        , apHkgName = hkgName
        , apHasLibrary = hasLib
        , apLicenseFile = licFn
        , apShHkgName = shVarNewValue (apShHkgName ap) hkgName
        , apShPkgName = shVarNewValue (apShPkgName ap) archName
        , apShPkgVer = shVarNewValue (apShPkgVer ap) pkgVer
        , apShPkgRel = shVarNewValue (apShPkgRel ap) pkgRel
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
calcExactDeps db pd = let
        n = (\ (P.PackageName n) -> n ) (P.packageName pd)
        remPkgs = (map DB.pkgName (filter isGhcPkg db)) ++ ghcPkgs ++ [n]
        deps = filter (not . flip elem remPkgs) (map depName (buildDepends pd))
        lookupPkgVer = display . DB.pkgVersion . fromJust . lookupPkg db
        depString n = let
                pkg = fromJust $ lookupPkg db n
                name = map toLower $ DB.pkgName pkg
                ver = display $ DB.pkgVersion pkg
                rel = pkgRelease pkg
            in "haskell-" ++ name ++ "=" ++ ver ++ "-" ++ rel
    in map depString deps

-- {{{2 ghcPkgs
-- libraries included in GHC, but not marked as provided by the Arch package
ghcPkgs = ["base", "bin-package-db", "ffi", "ghc", "ghc-binary", "ghc-prim", "haskell2010", "integer-gmp", "rts"]

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
                (ShArray $ catMaybes [maybe Nothing (const $ Just "cabal.patch") cabalPatch, maybe Nothing (const $ Just "source.patch") buildPatch])
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
addHashes ap tmpDir = let
        hashes = map (filter (`elem` "1234567890abcdef")) . lines . drop 11
        pkgbuildFn = tmpDir </> "PKGBUILD"
        pkgbuildPatch = apPkgbuildPatch ap
    in do
        liftIO $ copyPatches tmpDir ap
        liftIO $ writeFile pkgbuildFn (show $ pretty ap)
        maybe (return ()) (\ pfn -> (applyPatch pkgbuildFn pfn) >> return ()) pkgbuildPatch
        (ec, out, er) <- liftIO $ withWorkingDirectory tmpDir (readProcessWithExitCode "makepkg" ["-g"] "")
        case ec of
            ExitFailure _ -> do
                -- hPutStrLn stderr er
                throwError $ "makepkg: error while calculating the source hashes for " ++ (apHkgName ap)
            ExitSuccess -> do
                if "sha256sums=(" `isPrefixOf` out
                    then return ap { apShSha256Sums = shVarNewValue (apShSha256Sums ap) (ShArray $ hashes out) }
                    else return ap
