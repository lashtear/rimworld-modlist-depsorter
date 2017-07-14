{-# LANGUAGE Haskell2010 #-}
module ModSort (
  sortMods
  ) where

import qualified Data.IntMap as IntMap
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.Map    as Map
import           Data.Set    (Set)
import qualified Data.Set    as Set
import           Data.Text   (Text)

import           Mod
import           TSort

sortMods :: [Mod] -> [Mod]
sortMods modList =
  case kahn intBLD of
    Left e     -> error $ show e
    Right ints -> map (\i->intMod IntMap.! i) ints
  where
    intMod = nodeMap modList
    nameInt = revMap modNorm intMod
    intBLD = IntMap.mapWithKey makeBLD intMod
    makeBLD :: Int -> Mod -> BLD
    makeBLD i m =
      BLD { sub = i
          , req = resolveSet $ hardDep m
          , opt = resolveSet $ softDep m
          }
    resolve :: Text -> Int
    resolve name = nameInt Map.! name
    resolveSet :: Set Text -> IntSet
    resolveSet = IntSet.fromList . (map resolve) . Set.toList
