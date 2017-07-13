{-# LANGUAGE Haskell2010       #-}
{-# LANGUAGE OverloadedStrings #-}
module Norm (norm) where

import qualified Data.Char               as Char
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           Data.Text.ICU           (toCaseFold)
import           Data.Text.ICU.Normalize (NormalizationMode (..), normalize)

norm :: Text -> Text
norm =
  (Text.intercalate $ Text.singleton ' ') .
  Text.words .
  Text.strip .
  normSpace .
  (toCaseFold False) .
  (normalize NFKC)
  where
    normSpace :: Text -> Text
    normSpace t | Text.null t = Text.empty
                | Char.isAlphaNum $ Text.head t =
                  case Text.span Char.isAlphaNum t of
                    (a,b) -> Text.append a $ normSpace b
                | otherwise = Text.cons ' ' $
                              normSpace $ Text.dropWhile (not . Char.isAlphaNum) t
