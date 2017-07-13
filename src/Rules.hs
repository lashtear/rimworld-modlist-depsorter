{-# LANGUAGE Haskell2010       #-}
{-# LANGUAGE OverloadedStrings #-}
module Rules (
  Rule(..),
  Exp(..),
  parseRules,
  applyRules
  ) where

import           Data.List                 (all, any)
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Filesystem.Path.CurrentOS (FilePath)
import           Prelude                   hiding (FilePath)


import           Mod                       (Mod (..), modNorm)
import           RuleLexer                 (lexRules)
import           RuleParser                (parse)
import           RuleTypes

expMatch :: Exp -> Text -> Bool
expMatch (ExpString s) t   = s==t
expMatch (ExpNot e) t      = not $ expMatch e t
expMatch (ExpAnd es) t     = all (\e->expMatch e t) es
expMatch (ExpOr es) t      = any (\e->expMatch e t) es
expMatch (ExpStart et) t   = et `Text.isPrefixOf` t
expMatch (ExpContain et) t = et `Text.isInfixOf` t
expMatch (ExpEnd et) t     = et `Text.isSuffixOf` t

objectResolve :: Set Text -> Exp -> Set Text
objectResolve s (ExpString t) =
  if Set.member t s
  then Set.singleton t
  else Set.empty
objectResolve s (ExpNot e)    =
  Set.difference s $ objectResolve s e
objectResolve s (ExpStart t)  =
  case Set.split t s of
    (_,ges) ->
      Set.fromList $ takeWhile (t `Text.isPrefixOf`) $ Set.toList ges
objectResolve s (ExpOr es) =
  Set.unions $ map (objectResolve s) es
objectResolve s (ExpAnd es) =
  let ors = map (objectResolve s) es in
    if any Set.null ors
    then Set.empty
    else Set.unions ors
objectResolve s e = Set.filter (expMatch e) s

applyRule :: Set Text -> Mod -> Rule -> Mod
applyRule modns m r@(Rule subject hs object) =
  if expMatch subject mn
  then let objr = objectResolve modns object in
         case (hs, objr) of
           (Hard, s) | Set.null s -> error $ "Mod "++(show m)++" harddep "++(show r)++" not met"
           (Hard, s) -> m { hardDep = Set.union (hardDep m) s }
           (Soft, s) -> m { softDep = Set.intersection modns $ Set.union (softDep m) s }
  else m
  where
    mn = modNorm m

applyRules :: Set Text -> [Rule] -> Mod -> Mod
applyRules modns rs m = foldl (applyRule modns) m rs

parseRules :: FilePath -> Text -> Either String [Rule]
parseRules fp t =
  case lexRules fp t of
    Left e       -> Left e
    Right tokens -> Right $ parse tokens
