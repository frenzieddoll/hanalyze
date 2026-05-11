{-# LANGUAGE OverloadedStrings #-}
-- | Mermaid.js visualization of model DAGs.
--
-- Renders the 'ModelGraph' that 'Hanalyze.Model.HBM.buildModelGraph' derives
-- automatically from a polymorphic model into an HTML file (displayed
-- in the browser via the Mermaid CDN).
module Hanalyze.Viz.ModelGraph
  ( renderModelGraph
  , buildMermaid
  ) where

import Data.Text (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import qualified Data.Set as Set

import Hanalyze.Model.HBM (ModelGraph (..), Node (..), NodeKind (..))

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Render a model graph to an HTML file (Mermaid is loaded from CDN).
renderModelGraph :: FilePath -> Text -> ModelGraph -> IO ()
renderModelGraph path title_ mg = TIO.writeFile path (buildHtml title_ mg)

-- ---------------------------------------------------------------------------
-- HTML wrapper
-- ---------------------------------------------------------------------------

buildHtml :: Text -> ModelGraph -> Text
buildHtml title_ mg = T.unlines
  [ "<!DOCTYPE html>"
  , "<html><head>"
  , "  <meta charset=\"utf-8\">"
  , "  <title>" <> title_ <> "</title>"
  , "  <script src=\"https://cdn.jsdelivr.net/npm/mermaid@10/dist/mermaid.min.js\"></script>"
  , "  <style>"
  , "    body { font-family: sans-serif; padding: 30px; background: #f5f5f5; margin: 0; }"
  , "    h1   { color: #333; font-size: 1.3em; margin-bottom: 20px; }"
  , "    .wrap { background: white; padding: 30px; border-radius: 10px;"
  , "            box-shadow: 0 2px 10px rgba(0,0,0,.12); display: inline-block;"
  , "            min-width: 300px; }"
  , "    .legend { margin-top: 16px; font-size: .85em; color: #555; }"
  , "    .legend span { display: inline-block; width: 12px; height: 12px;"
  , "                   border-radius: 2px; margin-right: 4px; vertical-align: middle; }"
  , "  </style>"
  , "</head><body>"
  , "  <h1>" <> title_ <> "</h1>"
  , "  <div class=\"wrap\">"
  , "    <div class=\"mermaid\">"
  , buildMermaid mg
  , "    </div>"
  , "    <div class=\"legend\">"
  , "      <span style=\"background:#4C72B0\"></span>latent &nbsp;&nbsp;"
  , "      <span style=\"background:#DD8844\"></span>observed"
  , "    </div>"
  , "  </div>"
  , "  <script>mermaid.initialize({ startOnLoad: true, theme: 'default' });</script>"
  , "</body></html>"
  ]

-- ---------------------------------------------------------------------------
-- Mermaid diagram
-- ---------------------------------------------------------------------------

-- | Build the Mermaid @flowchart TD@ source for a 'ModelGraph'.
buildMermaid :: ModelGraph -> Text
buildMermaid mg = T.unlines $
  [ "flowchart TD" ] ++
  map mkNodeLine (mgNodes mg) ++
  [ "" ] ++
  map mkEdgeLine (mgEdges mg) ++
  [ "" ] ++
  [ "    classDef latent   fill:#4C72B0,color:#fff,stroke:#2a5080,stroke-width:1.5px" ] ++
  [ "    classDef observed fill:#DD8844,color:#fff,stroke:#b06020,stroke-width:1.5px" ] ++
  classAssignLines mg

mkNodeLine :: Node -> Text
mkNodeLine n = "    " <> nid <> shapeOpen <> escaped <> shapeClose
  where
    nid     = nodeId (nodeName n)
    label   = case nodeKind n of
      LatentN     -> nodeName n <> "\\n" <> nodeDist n <>
                     (if Set.null (nodeDeps n)
                       then ""
                       else " (deps: " <> T.intercalate "," (Set.toList (nodeDeps n)) <> ")")
      ObservedN k -> nodeName n <> "\\n" <> nodeDist n
                  <> "  (n=" <> T.pack (show k) <> ")"
    escaped = T.replace "\"" "&quot;" label
    (shapeOpen, shapeClose) = case nodeKind n of
      LatentN     -> ("[\"",  "\"]")
      ObservedN _ -> ("([\"", "\"])")

mkEdgeLine :: (Text, Text) -> Text
mkEdgeLine (from, to) = "    " <> nodeId from <> " --> " <> nodeId to

classAssignLines :: ModelGraph -> [Text]
classAssignLines mg =
  let latentIds   = [ nodeId (nodeName n) | n <- mgNodes mg, isLatent n ]
      observedIds = [ nodeId (nodeName n) | n <- mgNodes mg, not (isLatent n) ]
      assign cls ids
        | null ids  = []
        | otherwise = [ "    class " <> T.intercalate "," ids <> " " <> cls ]
  in assign "latent" latentIds ++ assign "observed" observedIds

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

nodeId :: Text -> Text
nodeId = T.map (\c -> if c `elem` (" -.+*/" :: String) then '_' else c)

isLatent :: Node -> Bool
isLatent n = case nodeKind n of { LatentN -> True; _ -> False }
