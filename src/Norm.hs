{-# LANGUAGE Haskell2010       #-}
{-# LANGUAGE OverloadedStrings #-}
module Norm (norm, foldPath) where

import qualified Data.Char                 as Char
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Data.Text.ICU             (toCaseFold)
import           Data.Text.ICU.Normalize   (NormalizationMode (..), normalize)
import qualified Filesystem.Path.CurrentOS as FSP

-- | Normalize mod names for easier comparison
norm :: Text -> Text
norm =
  (Text.intercalate $ Text.singleton ' ') .
  Text.words .
  Text.strip .
  normSpace .
  normPlus .
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
    -- dangit haplo
    normPlus :: Text -> Text
    normPlus t = case Text.stripSuffix "++" t of
      Just p  -> Text.append p " plus plus"
      Nothing -> t

-- | Normalize the last path component
foldPath :: FSP.FilePath -> FSP.FilePath
foldPath p =
  case reverse $ FSP.splitDirectories p of
    [] -> FSP.decodeString ""
    (f:fs) ->
      FSP.concat $
      reverse $
      (FSP.fromText $ toCaseFold False $ normalize NFC $ fe $ FSP.toText f):fs
      where
        -- As we're just normalizing, using the approximation is fine
        fe :: Either a a -> a
        fe x = case x of
                 Left l  -> l
                 Right r -> r
