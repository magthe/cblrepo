{-
 - Copyright 2011 Per Magnus Therning
 - Copyright 2012 Per Matthew William Cox
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

module ConvertDB where

-- {{{1 imports
-- {{{2 local
import Util.Misc
import qualified OldPkgDB as ODB
import qualified PkgDB as NDB

-- {{{2 system
import Control.Monad.Reader
import Data.Version
import System.IO

convertDb :: Command ()
convertDb = do
    inDb <- cfgGet inDbFile >>= \ fn -> liftIO $ ODB.readDb fn
    outDbFn <- cfgGet outDbFile
    let newDb = map doConvert inDb
    liftIO $ NDB.saveDb newDb outDbFn

doConvert :: ODB.CblPkg -> NDB.CblPkg
doConvert (n, ODB.GhcPkg v)      = (n, NDB.GhcPkg v)
doConvert (n, ODB.DistroPkg v r) = (n, NDB.DistroPkg v r)
doConvert (n, ODB.RepoPkg v d r) = (n, NDB.RepoPkg v d r Nothing)
