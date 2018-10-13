{-# LANGUAGE Haskell2010       #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Data.Foldable             (toList)
import           Data.Function             (on)
import           Data.List                 (sortBy)
import qualified Data.Set                  as Set
import qualified Data.Text.IO              as TextIO
import qualified Filesystem                as FS
import           Filesystem.Path.CurrentOS ((</>))
import qualified Filesystem.Path.CurrentOS as FSP
import           System.Environment        (getArgs)

import           CaseSenseless
import           Mod
import           ModSort
import           Rules
import           XML

filterJust :: [Maybe a] -> [a]
filterJust as = concatMap toList as

main :: IO ()
main = do
  [ruleFile] <- getArgs
  ruleText <- TextIO.readFile ruleFile
  steamdir <- tryValidate $ FS.getAppDataDirectory "Steam"
  unitydir <- tryValidate $ FS.getAppConfigDirectory "unity3d"
  rimconfdir <- traverseAll unitydir ["Ludeon Studios",
                                     "RimWorld by Ludeon Studios",
                                     "Config"]
  modsconfig <- tryValidate (return $ rimconfdir </> "ModsConfig.xml")
  build <- buildFromPath "2009" modsconfig
  modtrees <- mapM ((traverseAll steamdir) . ("SteamApps":))
    [["common", "RimWorld", "Mods"],
     ["workshop", "content", "294100"]]
  moddirs <- concat <$> mapM FS.listDirectory modtrees
  mods <- ((sortBy (compare `on` modNorm)) . filterJust) <$>
         mapM modFromPath moddirs
  case parseRules (FSP.decodeString ruleFile) ruleText of
    Left e  -> putStrLn e
    Right r ->
      let rules = r
          modns = Set.fromList $ map modNorm mods
          depMods = map (applyRules modns rules) mods
          allMods = Set.fromList $ map modNorm depMods
          allHard = Set.unions $ map hardDep depMods
          missing = Set.difference allHard allMods in
        if not $ Set.null missing
        then putStrLn $ "missing: "++show missing
        else do
          putStrLn $ "Found " ++ (show $ length mods) ++ " mods"
          putStrLn $ "Loaded " ++ (show $ length r) ++ " rules from "++ruleFile
          let cfgname = rimconfdir </> "ModsConfig.xml"
              cfgback = rimconfdir </> "ModsConfig-backup.xml" in do
            FS.rename cfgname cfgback
            TextIO.writeFile (FSP.encodeString cfgname) $
              modsConfigData build $
              map modKey $
              sortMods depMods
            putStrLn $ "Wrote \"" ++ (FSP.encodeString cfgname) ++ "\""
            TextIO.writeFile "mod-dependencies.dot" $
              dotOfDeps depMods
            putStrLn $ "Wrote mod-dependencies.dot"
