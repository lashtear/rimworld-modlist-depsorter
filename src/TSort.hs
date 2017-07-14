{-# LANGUAGE Haskell2010   #-}
{-# LANGUAGE TupleSections #-}
module TSort (
  BLD(..),
  KahnError,
  nodeMap,
  revMap,
  kahn
  ) where

import           Data.Foldable
import           Data.IntMap   (IntMap)
import qualified Data.IntMap   as IntMap
import           Data.IntSet   (IntSet)
import qualified Data.IntSet   as IntSet
import           Data.Map      (Map)
import qualified Data.Map      as Map

data Graph = Graph { superior :: IntMap IntSet -- ^ Edges by superior node
                   , inferior :: IntMap IntSet -- ^ Edges by inferior
                                              -- (dependent) node
                   , indeps   :: IntSet -- ^ Nodes with no dependencies
                   }
           deriving (Eq, Ord, Show)

noEdges :: Graph -> Bool
noEdges g = IntMap.null $ superior g

noIndeps :: Graph -> Bool
noIndeps g = IntSet.null $ indeps g

graphDropSuperior :: Graph -> Int -> Graph
graphDropSuperior g supN | IntMap.notMember supN $ superior g = g
graphDropSuperior g supN =
  let dependents = (superior g) IntMap.! supN
      newSup = IntMap.delete supN $ superior g
      newInf = IntMap.differenceWith cleanupInferior
               (inferior g) $
               IntMap.fromSet (const IntSet.empty) dependents
      newIndep = IntSet.union
                 (indeps g)
                 (IntSet.filter
                  (\d->IntMap.notMember d newInf)
                  dependents) in
    Graph { superior = newSup
          , inferior = newInf
          , indeps = newIndep
          }
  where
    cleanupInferior :: IntSet -> IntSet -> Maybe IntSet
    cleanupInferior a _ =
      case IntSet.delete supN a of
        s | IntSet.null s -> Nothing
        s -> Just s

graphDropNextIndep :: Graph -> (Int, Graph)
graphDropNextIndep g =
  let (i, newIndeps) = IntSet.deleteFindMin $ indeps g in
    ( i
    , graphDropSuperior (g { indeps = newIndeps }) i
    )

newGraph :: IntMap BLD -> Graph
newGraph imbld =
  let bd = bldDeps $ IntMap.elems imbld in
    Graph { superior = mapSet $ map prep bd
          , inferior = mapSet $ map (prep . rev) bd
          , indeps = IntMap.keysSet $ IntMap.filter (null . bldDep) imbld
          }
  where
    bldDep :: BLD -> [(Int, Int)]
    bldDep (BLD s r o) =
      map (,s) $ IntSet.toList $ IntSet.union r o
    bldDeps :: [BLD] -> [(Int, Int)]
    bldDeps bs = concatMap bldDep bs
    mapSet :: [(Int, IntSet)] -> IntMap IntSet
    mapSet as = IntMap.fromListWith IntSet.union as
    prep :: (Int, Int) -> (Int, IntSet)
    prep (a,b) = (a, IntSet.singleton b)
    rev (a,b) = (b,a)

-- | Bi-level dependency
data BLD = BLD { sub :: Int    -- ^ Subject
               , req :: IntSet -- ^ Required dependencies
               , opt :: IntSet -- ^ Optional dependencies
               }
         deriving (Eq, Ord, Show)

type KahnError = ([Int], Graph)

nodeMap :: Foldable t => t a -> IntMap a
nodeMap as = IntMap.fromList $ zip [1::Int ..] $ toList as

revMap :: Ord nodeName => (node -> nodeName) -> IntMap node -> Map nodeName Int
revMap f im =
  Map.fromList $ map (\(i,n)->(f n, i)) $ IntMap.toList im

kahn :: IntMap BLD -> Either KahnError [Int]
kahn imbld =
  case kahnLoop [] $ newGraph imbld of
    Left e  -> Left e
    Right r -> Right $ reverse r
  where
    kahnLoop :: [Int] -> Graph -> Either KahnError [Int]
    kahnLoop resA g | (noIndeps g) &&
                      (noEdges g)     = Right resA
    kahnLoop resA g | (noIndeps g)    = Left (resA, g)
    kahnLoop resA g =
      let (i, g') = graphDropNextIndep g in
        kahnLoop (i:resA) g'

