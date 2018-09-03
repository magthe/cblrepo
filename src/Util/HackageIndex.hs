{-
 - Copyright 2014 Per Magnus Therning
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

module Util.HackageIndex
    where

import Util.Misc
import Util.Dist
import Util.Cfg

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import Control.Applicative
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Search as BSLS
import qualified Data.ByteString.Lazy.UTF8 as BSLU
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Text
import Distribution.Version
import System.FilePath
import Safe (atMay)

readIndexFile :: FilePath -> FilePath -> IO BSL.ByteString
readIndexFile indexLocation indexFilename = exitOnException
    "Cannot open index file, have you run the 'update' command?"
    (BSL.readFile $ indexLocation </> indexFilename)

type PkgVersions = M.Map String [(Version, Int)]

-- | Build up a map of packages and their versions.  The versions are listed in
-- descending order, i.e. `head` contains the latest version.
buildPkgVersions :: BSL.ByteString -- ^ the index
    -> PkgVersions
buildPkgVersions idx = createPkgVerMap M.empty entries
    where
        entries = Tar.read $ GZip.decompress idx

        createPkgVerMap acc (Tar.Next e es) = case ver of
                Nothing -> createPkgVerMap acc es
                Just ver -> createPkgVerMap (M.insertWith (++) (head parts) [(ver, xrev)] acc) es
            where
                parts = splitDirectories (Tar.entryPath e)
                ver = if length parts == 3 then parts `atMay` 1 >>= simpleParse
                      else Nothing
                content = case Tar.entryContent e of
                    Tar.NormalFile c _ -> Just $ BSLU.toString c
                    _ -> Nothing
                xrev = case parsePackageDescription <$> content of
                    Just (ParseOk _ gpd) -> pkgXRev (packageDescription gpd)
                    _ -> 0

        createPkgVerMap acc Tar.Done = acc
        createPkgVerMap _ (Tar.Fail _) = undefined

latestVersion :: PkgVersions
  -> String -- ^ package name
  -> Version
  -> Maybe (Version, Int)
latestVersion pnv pkg _ = last . sort <$> M.lookup  pkg pnv

thisVersion :: PkgVersions
  -> String -- ^ package name
  -> Version
  -> Maybe (Version, Int) -- ^ xrev
thisVersion pnvs pkgName pkgVer = do
  vers <- M.lookup pkgName pnvs
  find (\ (v, _) -> v == pkgVer) vers

extractCabal :: BSL.ByteString  -- ^ the index
    -> String                   -- ^ package name
    -> Version                  -- ^ package version
    -> Maybe BSL.ByteString
extractCabal idx pkg ver = fmap dosToUnix $ getContent entries
    where
        entries = Tar.read $ GZip.decompress idx
        pkgPath = pkg </> display ver </> pkg <.> "cabal"

        dosToUnix bs = BSLS.replace (BS.pack [0xd, 0xa]) (BSL.pack [0xa]) bs

        getContent (Tar.Next e es)
            | pkgPath == Tar.entryPath e =
                case Tar.entryContent e of
                    Tar.NormalFile c _ -> Just c
                    _ -> Nothing
            | otherwise = getContent es

        getContent _ = Nothing

getRevision :: BSL.ByteString -- ^ the index
    -> String                 -- ^ package name
    -> Version                -- ^ package version
    -> Maybe Int
getRevision idx pkg ver = do
    cblStr <- BSLU.toString <$> extractCabal idx pkg ver
    case parsePackageDescription cblStr of
        ParseOk _ gpd -> maybe (return 0) (return . read) $ lookup "x-revision" (customFieldsPD $ packageDescription gpd)
        _ -> fail "no good"
