{-# LANGUAGE Haskell2010         #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
module XML (
  docFromNode,
  modsConfigData,
  fieldFromDoc,
  docFromPath
           ) where

import           Control.Exception         (catch)
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Data.Text.Lazy            as TextL
import           Filesystem.Path.CurrentOS (FilePath)
import qualified Filesystem.Path.CurrentOS as FSP
import           Prelude                   hiding (FilePath)
import           Text.Hamlet.XML           (xml)
import qualified Text.XML                  as XML
import           Text.XML.Cursor

docFromNode :: XML.Node -> XML.Document
docFromNode (XML.NodeElement e) =
  XML.Document { XML.documentPrologue =
                 XML.Prologue { XML.prologueBefore = []
                              , XML.prologueDoctype = Nothing
                              , XML.prologueAfter = []
                              }
               , XML.documentRoot = e
               , XML.documentEpilogue = []
               }
docFromNode _ = error "not a valid element node for document construction"

modsConfigData :: Text -> [Text] -> Text
modsConfigData buildId mods =
  case [xml|
<ModsConfigData>
  <buildNumber>#{buildId}
  <activeMods>
  $if null mods
    <li>Core
  $else
    $forall mod <- mods
      <li>#{mod}
|] of
    [n] ->
      Text.pack $
      TextL.unpack $
      XML.renderText prs $
      docFromNode n
    _ -> error "error in xml template"
  where
    prs = (XML.def :: XML.RenderSettings) { XML.rsPretty = True }

fieldFromDoc :: XML.Name -> XML.Document -> Text
fieldFromDoc field doc = case
  fromDocument doc $|
  element "ModMetaData" &/
  element field &/
  content of
    [n]   -> n
    _:_:_ -> error $ "multiple "++(show field)++" elements found in About.xml"
    []    -> error $ "no "++(show field)++" elements found in About.xml"

docFromPath :: FilePath -> IO (Maybe XML.Document)
docFromPath fp =
  catch
  (sequence $ Just $ XML.readFile XML.def $ FSP.encodeString fp)
  ( \ (_::XML.XMLException) -> return Nothing)