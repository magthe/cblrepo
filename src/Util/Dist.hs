{-# LANGUAGE ViewPatterns #-}
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

module Util.Dist
    ( depName
    , depVersionRange
    , pkgNameStr
    , pkgXRev
    ) where

import Distribution.Package
import Distribution.PackageDescription
import Distribution.Version

-- | Extract the name of a 'Dependency'.
depName :: Dependency -> String
depName (Dependency (unPackageName -> n) _) = n

-- | Extract the version range from a 'Dependency'.
depVersionRange :: Dependency -> VersionRange
depVersionRange (Dependency _ vr) = vr

-- | Get the name from a 'Package', i.e. various variants of package
-- descriptions.
pkgNameStr :: Package pkg => pkg -> String
pkgNameStr = unPackageName . packageName

-- | Get the "x-revision" from a 'PackageDescription'.
pkgXRev :: PackageDescription -> Int
pkgXRev p = maybe 0 read $ lookup "x-revision" (customFieldsPD p)
