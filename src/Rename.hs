{- Copyright 2012 Per Matthew William Cox
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

module Rename ( rename ) where

-- {{{1 imports
-- {{{1 local
import PkgDB
import Util.Misc

-- {{{1 system
import Control.Monad.Error
import Control.Monad.Reader
import qualified Data.Set as S
import System.Exit

-- {{{1 rename
rename :: Command ()
rename = do
    dbFn <- cfgGet dbFile
    db <- liftIO $ readDb dbFn
    dR <- cfgGet dryRun
    clears <- cfgGet cmdRenameClearPkgs
    renames <- cfgGet cmdRenamePkgs
    -- Encode these as one list for processing
    let targets = map (\c -> (c, Nothing)) clears ++
                  map (\(p, v) -> (p, Just v)) renames
    liftIO $ either
        (\s -> putStrLn s >> exitFailure)
        (\newDb -> unless dR $ saveDb newDb dbFn)
        (renameAll db targets)

-- Process database
renameAll :: CblDB -> [(String, Maybe String)] -> Either String CblDB
renameAll db rns = do foldM sanCheck S.empty rns
                      newDb <- foldM renameOne db rns
                      foldM dbCheck S.empty newDb
                      return newDb

-- Sanity check: only one rename op per package and no empty renames
sanCheck :: S.Set String -> (String, Maybe String) -> Either String (S.Set String)
sanCheck _ (n, Just "")            = throwError ("empty rename target for " ++ n)
sanCheck s (n, _) | n `S.member` s = throwError ("duplicate directives for " ++ n)
                  | otherwise      = return (S.insert n s)

-- Database check: no duplicate package names
dbCheck :: S.Set String -> CblPkg -> Either String (S.Set String)
dbCheck n pkg = let pkgName = archPackageName pkg in
   if pkgName `S.member` n
      then
          throwError ("collision on package name " ++ pkgName)
      else
          return (S.insert pkgName n)

-- Apply a new repository name the given package.
renameOne :: CblDB -> (String, Maybe String) -> Either String CblDB
renameOne db (pkg, rn) = mapM xl db where
   xl (pkg', rp@(RepoPkg {})) | pkg == pkg' = return (pkg, rp { repoName = rn })
   xl (pkg', _)               | pkg == pkg' = throwError ("only repo packages can be renamed")
   xl p                                     = return p
