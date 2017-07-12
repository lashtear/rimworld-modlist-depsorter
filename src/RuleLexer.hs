{-# LANGUAGE Haskell2010       #-}
{-# LANGUAGE OverloadedStrings #-}
module RuleLexer (
  lexRules
  ) where

import           Control.Applicative
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Filesystem.Path.CurrentOS (FilePath)
import qualified Filesystem.Path.CurrentOS as FSP
import           Prelude                   hiding (FilePath)
import           Text.Parsec               hiding (many, (<|>))
import           Text.Parsec.Text

import           Norm                      (norm)
import           RuleTypes

wordMap :: Map String Token
wordMap = Map.fromList [("has", TokenHas),
                        ("have", TokenHas),
                        ("not", TokenNot),
                        ("and", TokenAnd),
                        ("or", TokenOr),
                        ("starting", TokenStart),
                        ("containing", TokenContain),
                        ("harddep", TokenHard),
                        ("softdep", TokenSoft)]

pReserved :: Parser Token
pReserved = do
  w <- try $ (spaces *> (many1 letter) <?> "reserved word")
  case Map.lookup w wordMap of
    Just t  -> return t
    Nothing -> fail "x"

pPunct :: Parser Token
pPunct = do
  p <- try $ (spaces *> (oneOf ".,()"))
  return $ case p of
             '.' -> TokenDot
             ',' -> TokenComma
             '(' -> TokenOP
             ')' -> TokenCP
             _   -> undefined

pQuoted :: Parser Token
pQuoted =
  (try $
    spaces *>
    between
    (char '"' <?> "open quote")
    (char '"' <?> "close quote")
    pQChars) <?> "quoted string"
  where
    pQChars :: Parser Token
    pQChars = (TokenText . norm . Text.pack) <$> (many pQChar)
    pQChar :: Parser Char
    pQChar = (pEscape <|> (noneOf "\"\\\r\n")) <?>
             "valid character inside quote"
    pEscape :: Parser Char
    pEscape = (char '\\' *> anyChar) <?> "escape sequence"

pToken :: Parser Token
pToken = pReserved <|> pPunct <|> pQuoted

pTokens :: Parser [Token]
pTokens = (many1 pToken) <* spaces <* eof

lexRules :: FilePath -> Text -> Either String [Token]
lexRules file t =
  case parse pTokens (FSP.encodeString file) t of
    Left e  -> Left $ show e
    Right r -> Right r
