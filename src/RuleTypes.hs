{-# LANGUAGE Haskell2010       #-}
{-# LANGUAGE OverloadedStrings #-}
module RuleTypes (
  Rule(..),
  Dep(..),
  Exp(..),
  expStart,
  expContain,
  Token(..)
  ) where

import           Data.Text (Text)

data Rule = Rule Exp Dep Exp
          deriving (Eq, Show)

data Dep = Hard | Soft
         deriving (Eq, Ord, Enum, Show)

data Exp = ExpString Text
         | ExpNot Exp
         | ExpAnd [Exp]
         | ExpOr [Exp]
         | ExpStart Text
         | ExpContain Text
         deriving (Eq, Show)

data Token = TokenHas
           | TokenNot
           | TokenAnd
           | TokenOr
           | TokenStart
           | TokenContain
           | TokenText Text
           | TokenHard
           | TokenSoft
           | TokenDot
           | TokenComma
           | TokenOP
           | TokenCP
           deriving (Eq, Show)

expStart :: Exp -> Exp
expStart (ExpAnd es)   = ExpAnd $ map expStart es
expStart (ExpOr es)    = ExpAnd $ map expStart es
expStart (ExpNot e)    = ExpNot $ expStart e
expStart (ExpString s) = ExpStart s
expStart e             = error $ "Unexpected "++show e++" in expStart"

expContain :: Exp -> Exp
expContain (ExpAnd es)   = ExpAnd $ map expContain es
expContain (ExpOr es)    = ExpAnd $ map expContain es
expContain (ExpNot e)    = ExpNot $ expContain e
expContain (ExpString s) = ExpContain s
expContain e             = error $ "Unexpected "++show e++" in expContain"
