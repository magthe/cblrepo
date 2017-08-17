{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

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

module PkgTypes where

import           Control.Applicative
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Aeson.TH (deriveJSON)
import           Data.Text (unpack)
import qualified Data.Version as DV
import qualified Distribution.Package as P
import           Distribution.PackageDescription
import qualified Distribution.Version as V
import           Text.ParserCombinators.ReadP (readP_to_S)
import qualified Data.Vector as Vec

#if MIN_VERSION_Cabal(2,0,0)
import Distribution.Package (mkPackageName)
import Distribution.PackageDescription (mkFlagName, unFlagName)
import Distribution.Text as T
#else
mkFlagName = FlagName
unFlagName (FlagName name) = name
mkPackageName = P.PackageName
#endif

data Pkg = GhcPkg GhcPkgD
         | DistroPkg DistroPkgD
         | RepoPkg RepoPkgD
         deriving (Eq, Show)

data GhcPkgD = GhcPkgD { gpVersion :: V.Version }
             deriving (Eq, Show)

data DistroPkgD = DistroPkgD
                  { dpVersion :: V.Version
                  , dpXrev :: Int
                  , dpRelease :: Int
                  } deriving (Eq, Show)

data RepoPkgD = RepoPkgD
                { rpVersion :: V.Version
                , rpXrev :: Int
                , rpDeps :: [P.Dependency]
                , rpFlags :: FlagAssignment
                , rpRelease :: Int
                } deriving (Eq, Show)

data CblPkg = CP String Pkg
            deriving (Eq, Show)

type CblDB = [CblPkg]

instance Ord CblPkg where
  compare (CP n1 (GhcPkg d1)) (CP n2 (GhcPkg d2)) =
    compare (n1, gpVersion d1) (n2, gpVersion d2)
  compare (CP _ GhcPkg {}) _ = LT
  compare _ (CP _ GhcPkg {}) = GT

  compare (CP n1 (DistroPkg d1)) (CP n2 (DistroPkg d2)) =
    compare (n1, dpVersion d1, dpRelease d1) (n2, dpVersion d2, dpRelease d2)
  compare (CP _ DistroPkg {}) _ = LT
  compare _ (CP _ DistroPkg {}) = GT

  compare (CP n1 (RepoPkg d1)) (CP n2 (RepoPkg d2)) =
    compare (n1, rpVersion d1, rpRelease d1) (n2, rpVersion d2, rpRelease d2)

-- JSON instances
version2Json :: V.Version -> Value
#if MIN_VERSION_Cabal(2,0,0)
version2Json = toJSON . T.display
#else
version2Json = toJSON . DV.showVersion
#endif

json2Version :: Value -> Parser V.Version
json2Version = withText "Version" $ go . readP_to_S DV.parseVersion . unpack
  where
    go [(v,[])] =
#if MIN_VERSION_Cabal(2,0,0)
      return . V.mkVersion $ DV.versionBranch v
#else
      return v
#endif
    go (_ : xs) = go xs
    go _        = fail "could not parse Version"

dependencyList2Json :: [P.Dependency] -> Value
dependencyList2Json = toJSON . map convDep
  where
    convDep (P.Dependency (P.unPackageName -> n) vr)= (n, versionRange2Json vr)

json2DependencyList :: Value -> Parser [P.Dependency]
json2DependencyList = withArray "DependencyList" parseList
  where
    parseList = mapM (withArray "Dependency" parseDep) . Vec.toList

    parseDep a = do
      n <- withText "PackageName" (return . unpack) (a Vec.! 0)
      vr <- json2VersionRange (a Vec.! 1)
      return $ P.Dependency (mkPackageName n) vr

versionRange2Json :: V.VersionRange -> Value
versionRange2Json = V.foldVersionRange
  (object ["AnyVersion" .= ([]::[(Int,Int)])])
  (\ v -> object ["ThisVersion" .= version2Json v])
  (\ v -> object ["LaterVersion" .= version2Json v])
  (\ v -> object ["EarlierVersion" .= version2Json v])
  (\ vr0 vr1 -> object ["UnionVersionRanges" .= [vr0, vr1]])
  (\ vr0 vr1 -> object ["IntersectVersionRanges" .= [vr0, vr1]])

json2VersionRange :: Value -> Parser V.VersionRange
json2VersionRange = withObject "VersionRange" go
  where
    go :: Object -> Parser V.VersionRange
    go o =
      V.thisVersion <$> (o .: "ThisVersion" >>= json2Version) <|>
      V.laterVersion <$> (o .: "LaterVersion" >>= json2Version) <|>
      V.earlierVersion <$> (o .: "EarlierVersion" >>= json2Version) <|>
      V.WildcardVersion <$> (o .: "WildcardVersion" >>= json2Version) <|>
      nullaryOp V.anyVersion <$> o .: "AnyVersion" <|>
      (o .: "UnionVersionRanges" >>= parserPair V.unionVersionRanges) <|>
      (o .: "IntersectVersionRanges" >>= parserPair V.intersectVersionRanges)  <|>
      V.VersionRangeParens <$> (o .: "VersionRangeParens" >>= json2VersionRange)

    nullaryOp :: a -> Value -> a
    nullaryOp = const

    parserPair :: (V.VersionRange -> V.VersionRange -> V.VersionRange) -> Value -> Parser V.VersionRange
    parserPair f = withArray "parserPair" p
      where
        p a = do
          v1 <- json2VersionRange (a Vec.! 0)
          v2 <- json2VersionRange (a Vec.! 1)
          return $ f v1 v2

flagAssignment2Json :: FlagAssignment -> Value
flagAssignment2Json = toJSON . map convFlag
  where
    convFlag (unFlagName -> s, b) = (s, b)

json2FlagAssignment :: Value -> Parser FlagAssignment
json2FlagAssignment = withArray "FlagAssignment" parseList
  where
    parseList = mapM (withArray "SingleFlag" parseFlag) . Vec.toList

    parseFlag a = do
      n <- withText "FlagName" (return . unpack) (a Vec.! 0)
      b <- withBool "FlagBool" return (a Vec.! 1)
      return (mkFlagName n, b)

instance ToJSON GhcPkgD where
  toJSON (GhcPkgD v) = object ["gpVersion" .= version2Json v]

instance FromJSON GhcPkgD where
  parseJSON = withObject "GhcPkgD" (\ o -> GhcPkgD <$> (o .: "gpVersion" >>= json2Version))

instance ToJSON DistroPkgD where
  toJSON (DistroPkgD v x r)= object [ "dpVersion" .= version2Json v
                                    , "dpXrev" .= x
                                    , "dpRelease" .= r
                                    ]

instance FromJSON DistroPkgD where
  parseJSON = withObject "DistroPkgD" go
    where
      go o = do
        v <- o .: "dpVersion" >>= json2Version
        x <- o .: "dpXrev"
        r <- o .: "dpRelease"
        return $ DistroPkgD v x r

instance ToJSON RepoPkgD where
  toJSON (RepoPkgD v x ds fs r)= object [ "rpVersion" .= version2Json v
                                        , "rpXrev" .= x
                                        , "rpDeps" .= dependencyList2Json ds
                                        , "rpFlags" .= flagAssignment2Json fs
                                        , "rpRelease" .= r
                                        ]

instance FromJSON RepoPkgD where
  parseJSON = withObject "RepoPkgD" go
    where
      go o = do
        v <- o .: "rpVersion" >>= json2Version
        x <- o .: "rpXrev"
        ds <- o .: "rpDeps" >>= json2DependencyList
        fs <- o .: "rpFlags" >>= json2FlagAssignment
        r <- o .: "rpRelease"
        return $ RepoPkgD v x ds fs r

$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField, allNullaryToStringTag = False } ''Pkg)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField, allNullaryToStringTag = False } ''CblPkg)
