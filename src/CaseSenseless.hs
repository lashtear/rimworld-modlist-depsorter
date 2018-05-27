{-# LANGUAGE Haskell2010       #-}
{-# LANGUAGE OverloadedStrings #-}
module CaseSenseless where

import           Control.Applicative       ((<|>))
import           Data.List                 (sort)
import qualified Data.Map                  as Map
import qualified Filesystem                as FS
import           Filesystem.Path.CurrentOS ((</>))
import qualified Filesystem.Path.CurrentOS as FSP

import           Norm                      (foldPath)

exists :: FSP.FilePath -> IO (Maybe FSP.FilePath)
exists f = do
  fe <- (||) <$> FS.isFile f <*> FS.isDirectory f
  return $
    if fe
    then Just f
    else Nothing

find :: FSP.FilePath -> FSP.FilePath -> IO (Maybe FSP.FilePath)
find path name = do
  fs <- FS.listDirectory path
  -- Generate a lookup table of folded filenames to original names,
  -- pointing at the very first found in the sorted directory list.
  let m = Map.fromListWith (flip const) $
        map (\p->(foldPath p, p)) $
        sort fs in
    return $ Map.lookup (foldPath name) m

nil :: FSP.FilePath
nil = FSP.decodeString ""

-- | Perform the inverse of 'Filesystem.Path.append'.
peel :: FSP.FilePath -> (FSP.FilePath, FSP.FilePath)
peel p =
  case reverse $ FSP.splitDirectories p of
    []     -> (nil, nil)
    (f:fs) -> (FSP.concat $ reverse fs, f)

-- | Perform case-insensitive directory traversal.
-- Works like 'Filesystem.Path.</>' but resolves path elements
-- case-insensitively as needed, if possible, or returns Nothing.
traverseSenseless :: FSP.FilePath -> FSP.FilePath -> IO (Maybe FSP.FilePath)
traverseSenseless a b = (exists $ a </> b) <|> (find a b)

validateSenseless :: FSP.FilePath -> IO (Maybe FSP.FilePath)
validateSenseless p = (exists p) <|> ((uncurry find) (peel p))

traverseAll :: FSP.FilePath -> [FSP.FilePath] -> IO (FSP.FilePath)
traverseAll base [] = return base
traverseAll base (p:ps) = do
  basep <- traverseSenseless base p
  case basep of
    Nothing -> return nil
    Just p' -> traverseAll p' ps

tryValidate :: IO FSP.FilePath -> IO FSP.FilePath
tryValidate pio = do
  p <- pio
  v <- validateSenseless p
  return $ case v of
             Nothing -> p
             Just vp -> vp
