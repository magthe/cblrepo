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

module Main where

import Add
import BuildPkgs
import BumpPkgs
import Sync
import Versions
import ListPkgs
import Updates
import Util.Misc
import Urls
import PkgBuild
import ConvertDB
import Remove

import Paths_cblrepo

import Control.Monad
import Control.Monad.Reader
import Distribution.Text
import System.Console.CmdArgs
import System.Directory
import System.FilePath

-- {{{1 command line arguments
argAppDir = appDir := def += explicit += name "appdir" += help "application data directory" += typDir
argDbFile = dbFile := "cblrepo.db" += explicit += name "db" += help "package database" += typFile
argDryRun = dryRun := False += explicit += name "n" += help "dry run"

cmdAddPkg = record defCmdAdd
    [ argAppDir, argDbFile
    , patchDir := "patches" += explicit += name "patchdir" += help "location of patches" += typDir
    , argDryRun
    , pkgType := RepoPkgT += explicit += name "t" += name "type" += typ "TYPE" += help "type of package ((g)hcpkg, (d)istropkg, or (r)epopkg, default: r)"
    --, isBase := False += explicit += name "b" += name "base" += help "add a base package"
    , cbls := def += args += typ "PKGVERSPEC"
    ] += name "add" += help "add a package from a Cabal file" += details
        [ "The package version depends on the type of the package that's added:"
        , " 1. For a ghc package specify the name and version, e.g. 'base,4.3'"
        , " 2. For a distro package specify the name, version, and release, e.g. 'HUnit,1.2.2.3,2.1"
        , " 3. For a repo package provide a Cabal file by either"
        , "    - provide the filename of the  Cabal file"
        , "    - specify the URL where the Cabal file can be found (for file:// URLs the full absolute path is required"
        , "    - specify the name and version to load the Cabal file out of an index file (see idxupdate)"
        , "    All three format may be mixed on the command line."
        ]

cmdBumpPkgs = record defBumpPkgs
    [ argAppDir, argDbFile
    , argDryRun
    , inclusive := False += explicit += name "inclusive" += help "include listed packages"
    , pkgs := def += args += typ "PKG"
    ] += name "bump" += help "bump packages that need it after updating the named packages"

cmdBuildPkgs = record defBuildPkgs
    [ argAppDir, argDbFile
    , pkgs := def += args += typ "PKG"
    ] += name "build" += help "list packages that need rebuilding, in order"

cmdSync = record defSync [ argAppDir ] += name "sync" += help "update the index"

cmdVersions = record defVersions
    [ argAppDir
    , pkgs := def += args += typ "PKG"
    ] += name "versions" += help "list available versions"

cmdUpdates = record defUpdates [ argAppDir , argDbFile ] += name "updates" += help "check for available updates"

cmdListPkgs = record defListPkgs
    [ argAppDir, argDbFile
    , listAll := False += name "a" += explicit += help "list all packages"
    ] += name "list" += help "list packages in repo"

cmdUrls = record defUrls
    [ argAppDir
    , pkgVers := def += args += typ "STRING,STRING"
    ] += help "list urls of cabal files for the given packages" += details
        [ "The format for a package is <name>,<version>." ]

cmdPkgBuild = record defPkgBuild
    [ argAppDir, argDbFile
    , patchDir := "patches" += explicit += name "patchdir" += help "location of patches (patches)" += typDir
    , pkgs := def += args += typ "PKG"
    ] += help "create a PKGBUILD, and other files necessary for an Arch package"

cmdConvertDb = record defConvertDb
    [ argAppDir
    , inDbFile := "cblrepo.db" += explicit += name "i" += name "indb" += typFile += help "old database"
    , outDbFile := "new-cblrepo.db" += explicit += name "o" += name "outdb" += typFile += help "new database"
    ] += help "convert an old database to the new format"

cmdRemovePkg = record defRemovePkg
    [ argAppDir, argDbFile, argDryRun
    , pkgs := def += args += typ "PKG"
    ] += name "rm" += help "remove packages"

cmds = cmdArgsMode_ $ modes_
    [ cmdAddPkg
    , cmdBuildPkgs
    , cmdBumpPkgs
    , cmdSync
    , cmdVersions
    , cmdListPkgs
    , cmdUpdates
    , cmdUrls
    , cmdPkgBuild
    , cmdConvertDb
    , cmdRemovePkg
    ]
    += program progName
    += summary (progName ++ " v" ++ (display version))
    += help "maintain a database of dependencies of CABAL packages"

-- {{{1 main
main = do
    defAppDir <- getAppUserDataDirectory progName
    cmdArgsRun cmds >>= \ c -> do
        let aD = if null (appDir c) then defAppDir else (appDir c)
        let c' = c { appDir = aD }
        createDirectoryIfMissing True (aD)
        case c' of
            CmdAdd {} -> runReaderT add c'
            BuildPkgs {} -> runReaderT buildPkgs c'
            BumpPkgs {} -> runReaderT bumpPkgs c'
            Sync {} -> runReaderT sync c'
            Versions {} -> runReaderT versions c'
            ListPkgs {} -> runReaderT listPkgs c'
            Updates {} -> runReaderT updates c'
            Urls {} -> runReaderT urls c'
            PkgBuild {} -> runReaderT pkgBuild c'
            ConvertDb {} -> runReaderT convertDb c'
            RemovePkg {} -> runReaderT remove c'
