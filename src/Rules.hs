{-# LANGUAGE Haskell2010       #-}
{-# LANGUAGE OverloadedStrings #-}
module Rules (
  Rule(..),
  Exp(..),
  parseRules,
  applyRules
  ) where

import           Control.Applicative
import qualified Data.Set            as Set
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Text.Parsec         hiding (many, (<|>))
import           Text.Parsec.Text

import           Mod                 (Mod (..), modNorm)
import           Norm

data Rule = RuleHard Exp Text
          | RuleSoft Exp Text
          deriving (Eq, Show)

data Exp = ExpString Text
         | ExpNot Exp
         | ExpAnd Exp Exp
         | ExpOr Exp Exp
         | ExpStart Text
         | ExpContain Text
         deriving (Eq, Show)

expMatch :: Exp -> Text -> Bool
expMatch (ExpString s) t  = s==t
expMatch (ExpNot e) t     = not $ expMatch e t
expMatch (ExpAnd a b) t   = (expMatch a t) && (expMatch b t)
expMatch (ExpOr a b) t    = (expMatch a t) || (expMatch b t)
expMatch (ExpStart s) t   = s `Text.isPrefixOf` t
expMatch (ExpContain s) t = s `Text.isInfixOf` t

pNewline :: Parser Char
pNewline = (((try endOfLine) <|> (char '\r')) *>
             return '\n') <?> "line terminator"

pQuoted :: Parser Text
pQuoted = (spaces *>
            between
            (char '"' <?> "open quote")
            (char '"' <?> "close quote")
            pQChars) <?> "quoted string"
  where
    pQChars :: Parser Text
    pQChars = (norm . Text.pack) <$> (many pQChar)
    pQChar :: Parser Char
    pQChar = (pEscape <|> (noneOf "\"\\\r\n")) <?>
             "valid character inside quote"
    pEscape :: Parser Char
    pEscape = (char '\\' *> anyChar) <?> "escape sequence"

pExp :: Parser Exp
pExp = try $
  ((spaces *>
    ((ExpString <$> pQuoted) <|>
     (ExpNot <$> ((try $ string "not") *> pExp)) <|>
     (ExpStart <$> ((try $ string "starting") *> pQuoted)) <|>
     (ExpContain <$> ((try $ string "containing" *> pQuoted))) <|>
     (ExpAnd <$> ((try $ string "and") *> pExp) <*> pExp) <|>
     (ExpOr <$> ((try $ string "or") *> pExp) <*> pExp) <|>
     between (spaces *> char '(') (spaces *> char ')') pExp)) <?>
   "expression")

pRule :: Parser Rule
pRule = do
  e <- pExp
  _ <- spaces *> (string "has") *> spaces <?> "has-clause"
  pHard e <|> pSoft e
  where
    pHard ex = (RuleHard ex) <$> (try $ (string "harddep") *> pQuoted)
    pSoft ex = (RuleSoft ex) <$> (try $ (string "softdep") *> pQuoted)

pRules :: Parser [Rule]
pRules = (endBy1 pRule ((pNewline *> return ()) <|> eof)) <* spaces <* eof

parseRules :: FilePath -> Text -> Either String [Rule]
parseRules file t =
  case parse pRules (show file) t of
    Left e  -> Left $ show e
    Right r -> Right r

applyRule :: Rule -> Mod -> Mod
applyRule (RuleHard e d) m | expMatch e (modNorm m) =
                               m { hardDep = Set.insert d $ hardDep m }
applyRule (RuleSoft e d) m | expMatch e (modNorm m) =
                               m { softDep = Set.insert d $ softDep m }
applyRule _ m = m

applyRules :: [Rule] -> Mod -> Mod
applyRules rs m = foldl (flip applyRule) m rs
