{-# LANGUAGE Haskell2010       #-}
{-# LANGUAGE OverloadedStrings #-}
module RuleTypes (
  Rule(..),
  Dep(..),
  Exp(..),
  expContain,
  Token(..),
  Describable(..)
  ) where

import           Data.Text (Text)
import qualified Data.Text as Text

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
         | ExpEnd Text
         deriving (Eq, Show)

data Token = TokenHas
           | TokenNot
           | TokenAnd
           | TokenOr
           | TokenStart
           | TokenContain
           | TokenEnd
           | TokenText Text
           | TokenHard
           | TokenSoft
           | TokenDot
           | TokenComma
           | TokenOP
           | TokenCP
           deriving (Eq, Show)

expContain :: Exp -> Exp
expContain (ExpAnd es)   = ExpAnd $ map expContain es
expContain (ExpOr es)    = ExpAnd $ map expContain es
expContain (ExpNot e)    = ExpNot $ expContain e
expContain (ExpString s) = ExpContain s
expContain e             = error $ "Unexpected "++show e++" in expContain"

class (Show a) => Describable a where
  describe :: a -> Text
  describe = Text.pack . show

instance Describable Rule where
  describe (Rule s d o) = Text.concat
                          [ describe s
                          , hasHave s
                          , describe d
                          , " "
                          , describe o
                          , "."
                          ]
    where
      hasHave :: Exp -> Text
      hasHave (ExpOr _) = " have "
      hasHave _         = " has "

quote :: Text -> Text
quote t = Text.concat ["\"", t, "\""]

paren :: Text -> Text
paren t = Text.concat ["(", t, ")"]

conjList :: Text -> [Text] -> Text
conjList c ts =
  case reverse ts of
    [] -> ""
    a:[] -> a
    a:b:[] -> Text.intercalate " " [b, c, a]
    (lastt:ts') -> paren $
                  Text.intercalate ", " $
                  (reverse ts') ++ [Text.concat [c, " ", lastt]]

instance Describable Exp where
  describe (ExpString t)  = quote t
  describe (ExpStart t)   = Text.concat ["starting ", quote t]
  describe (ExpContain t) = Text.concat ["containing ", quote t]
  describe (ExpEnd t)     = Text.concat ["ending ", quote t]
  describe (ExpNot e)     = Text.concat ["not ", describe e]
  describe (ExpAnd es)    = conjList "and" $ map describe es
  describe (ExpOr es)     = conjList "or" $ map describe es

instance Describable Dep where
  describe Hard = "harddep"
  describe Soft = "softdep"

