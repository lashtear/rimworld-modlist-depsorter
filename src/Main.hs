{-# LANGUAGE Haskell2010       #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Data.Foldable             (toList)
import           Data.Function             (on)
import           Data.List                 (sortBy)
import qualified Data.Set                  as Set
import qualified Data.Text                 as Text
import qualified Data.Text.IO              as TextIO
import qualified Filesystem                as FS
import           Filesystem.Path.CurrentOS ((</>))
import qualified Filesystem.Path.CurrentOS as FSP
import           System.Environment        (getArgs)

import           Mod
import           Rules
import           XML                       (modsConfigData)

filterJust :: [Maybe a] -> [a]
filterJust as = concatMap toList as

main :: IO ()
main = do
  [ruleFile] <- getArgs
  ruleText <- TextIO.readFile ruleFile
  steamdir <- FS.getAppDataDirectory "Steam"
  let modtrees = [ steamdir </> "SteamApps/common/RimWorld/Mods"
                 , steamdir </> "SteamApps/workshop/content/294100"
                 ] in do
    moddirs <- concat <$> mapM FS.listDirectory modtrees
    mods <- ((sortBy (compare `on` modNorm)) . filterJust) <$>
           mapM modFromPath moddirs
    case parseRules (FSP.decodeString ruleFile) ruleText of
      Left e  -> putStrLn e
      Right r -> let rules = r in
        let modns = Set.fromList $ map modNorm mods
            depMods = map (applyRules modns rules) mods
            allMods = Set.fromList $ map modNorm depMods
            allHard = Set.unions $ map hardDep depMods
            missing = Set.difference allHard allMods in
          if not $ Set.null missing
          then putStrLn $ "missing: "++show missing
          else putStrLn $ Text.unpack $ modsConfigData (Text.pack "1557") (map modKey depMods)
--            putStrLn $ Text.unpack $ dotOfDeps depMods
