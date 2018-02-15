{-# LANGUAGE Haskell2010       #-}
{-# LANGUAGE OverloadedStrings #-}
module Mod (
  Mod(..),
  dotOfDeps,
  modFromPath
  ) where

import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Filesystem.Path.CurrentOS (FilePath, (</>))
import qualified Filesystem.Path.CurrentOS as FSP
import           Prelude                   hiding (FilePath)

import           Norm
import           XML

data Mod = Mod { modName   :: Text
               , modAuthor :: Text
               , modNorm   :: Text
               , modPath   :: FilePath
               , modKey    :: Text
               , hardDep   :: Set Text
               , softDep   :: Set Text
               } deriving (Eq, Show)

dotOfDeps :: [Mod] -> Text
dotOfDeps mods =
  Text.pack $
  "digraph {\nratio=auto;\nrankdir=LR;bgcolor=\"#222222\";\n" ++
  "color=\"#cccccc\";\nfontname=\"sans serif\";\n" ++
  "node [color=\"#cccccc\",fontcolor=\"#cccccc\",style=filled,fillcolor=\"#444444\"];\n" ++
  "edge [color=\"#888888\"];\n" ++
  (concatMap modDot mods) ++
  "}"
  where
    modDot m =
      (concatMap (depDot "solid" m) $ Set.toList $ hardDep m)++
      (concatMap (depDot "dotted" m) $ Set.toList $ softDep m)
    depDot style m dep = (mn dep) ++
                         " -> " ++
                         (mn $ modNorm m) ++
                         " [style="++style++"];\n"
    mn m = "\"" ++ (Text.unpack m) ++ "\""


modFromPath :: FilePath -> IO (Maybe Mod)
modFromPath moddir = do
  about <- docFromPath $ moddir </> "About/About.xml"
  case about of
    Nothing -> return Nothing
    Just a ->
      let name = fieldFromDoc "name" a
          author = fieldFromDoc "author" a
          normname = norm name in
        return $
        Just $
        Mod { modName = name
            , modAuthor = author
            , modNorm = normname
            , modPath = moddir
            , modKey = Text.pack $ FSP.encodeString $ FSP.filename $ moddir
            , hardDep = Set.empty
            , softDep = Set.empty
            }
