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
import Rename

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
    , cmdAddGhcPkgs := def += explicit += name "g" += name "ghc-pkg" += typ "PKG,VER" += help "GHC base package (multiple)"
    , cmdAddDistroPkgs := def += explicit += name "d" += name "distro-pkg" += typ "PKG,VER,REL" += help "distro package (multiple)"
    , cmdAddUrlCbls := def += explicit += name "u" += name "cbl-url" += typ "URL" += help "url of Cabal file (multiple)"
    , cmdAddFileCbls := def += explicit += name "f" += name "cbl-file" += typFile += help "Cabal file (multiple)"
    , cmdAddCbls := def += args += typ "PKG,VER"
    ] += name "add" += help "add a package to the database"

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

cmdUpdates = record defUpdates
    [ argAppDir, argDbFile
    , idxStyle := False += explicit += name "s" += help "a shorter output suitable for scripting"
    ] += name "updates" += help "check for available updates"

cmdListPkgs = record defCmdListPkgs
    [ argAppDir, argDbFile
    , listGhc := False += explicit += name "g" += name "ghc" += help "list ghc packages"
    , listDistro := False += explicit += name "d" += name "distro" += help "list distro packages"
    , listRename := False += explicit += name "rename" += help "list renamed packages"
    , noListRepo := False += explicit += name "no-repo" += help "do not list repo packages"
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

cmdRenamePkg = record defRenamePkg
    [ argAppDir, argDbFile, argDryRun
    , cmdRenameClearPkgs := def += explicit += name "c" += name "clear" += typ "PKG" += help "clear rename (multiple)"
    , cmdRenamePkgs := def += args += typ "PKG,RENAME"
    ] += name "rename" += help "rename packages"

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
    , cmdRenamePkg
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
            CmdAdd {} -> runCommand c' add
            BuildPkgs {} -> runCommand c' buildPkgs
            BumpPkgs {} -> runCommand c' bumpPkgs
            Sync {} -> runCommand c' sync
            Versions {} -> runCommand c' versions
            CmdListPkgs {} -> runCommand c' listPkgs
            Updates {} -> runCommand c' updates
            Urls {} -> runCommand c' urls
            PkgBuild {} -> runCommand c' pkgBuild
            ConvertDb {} -> runCommand c' convertDb
            RemovePkg {} -> runCommand c' remove
            RenamePkg {} -> runCommand c' rename
