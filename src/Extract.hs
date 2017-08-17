{-
 - Copyright 2014 - 2015 Per Magnus Therning
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

{-# LANGUAGE FlexibleContexts #-}
module Extract
    where

import Util.HackageIndex
import Util.Misc
import Util.Cfg

import Control.Monad.Trans.Except
import Control.Monad.Reader
import qualified Data.ByteString.Lazy as BSL
import Distribution.Text (display)
import Distribution.Version
import System.FilePath

extract :: Command ()
extract = do
    aD <- asks $ appDir . fst
    pkgsNVersions <- asks $ cmdExtractPkgs . optsCmd . fst
    cfg <- asks snd
    --
    idx <- liftIO $ readIndexFile aD (getIndexFileName cfg)
    _ <- mapM (runExceptT . extractAndSave idx) pkgsNVersions >>= exitOnAnyLefts
    return ()

extractAndSave :: MonadIO m => BSL.ByteString -> (String, Version) -> ExceptT String m ()
extractAndSave idx (pkg, ver) = maybe (throwE errorMsg) (liftIO . BSL.writeFile destFn) (extractCabal idx pkg ver)
    where
        destFn = pkg <.> "cabal"
        errorMsg = "Failed to extract Cabal for " ++ pkg ++ " " ++ display ver
