{-# LANGUAGE OverloadedStrings #-}
module Viz.ModelGraph
  ( renderModelGraph
  , buildMermaid
  ) where

import Data.Text (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

import Model.HBM        (ModelGraph (..), NodeInfo (..), NodeRole (..))
import Stat.Distribution (Distribution (..), distributionName)

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Render the model graph as a self-contained HTML file (Mermaid.js DAG).
-- Open the file in any browser to see the interactive diagram.
renderModelGraph :: FilePath -> Text -> ModelGraph -> IO ()
renderModelGraph path title_ mg =
  TIO.writeFile path (buildHtml title_ mg)

-- ---------------------------------------------------------------------------
-- HTML wrapper
-- ---------------------------------------------------------------------------

buildHtml :: Text -> ModelGraph -> Text
buildHtml title_ mg = T.unlines
  [ "<!DOCTYPE html>"
  , "<html>"
  , "<head>"
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
  , "</head>"
  , "<body>"
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
  , "</body>"
  , "</html>"
  ]

-- ---------------------------------------------------------------------------
-- Mermaid diagram
-- ---------------------------------------------------------------------------

buildMermaid :: ModelGraph -> Text
buildMermaid mg = T.unlines $
  [ "flowchart TD" ] ++
  map (mkNodeLine mg) (mgNodes mg) ++
  [ "" ] ++
  map mkEdgeLine (mgEdges mg) ++
  [ "" ] ++
  [ "    classDef latent   fill:#4C72B0,color:#fff,stroke:#2a5080,stroke-width:1.5px" ] ++
  [ "    classDef observed fill:#DD8844,color:#fff,stroke:#b06020,stroke-width:1.5px" ] ++
  classAssignLines mg

-- | One Mermaid node definition line.
mkNodeLine :: ModelGraph -> NodeInfo -> Text
mkNodeLine mg ni =
  "    " <> nid <> shapeOpen <> escaped <> shapeClose
  where
    nid        = nodeId (nodeName ni)
    hasParents = any ((== nodeName ni) . snd) (mgEdges mg)

    -- Root nodes show concrete prior params; non-root nodes show family only
    -- (because non-root params are placeholder 0s from collectNodes traversal).
    distLabel  = if hasParents
                 then distFamily (nodeDist ni)
                 else distributionName (nodeDist ni)

    label = case nodeRole ni of
      Latent      -> nodeName ni <> "\\n" <> distLabel
      Observed xs -> nodeName ni <> "\\n" <> distLabel
                     <> "  (n=" <> T.pack (show (length xs)) <> ")"

    -- Escape any double-quotes inside the label
    escaped = T.replace "\"" "&quot;" label

    -- Latent → rectangle  |  Observed → stadium (oval)
    (shapeOpen, shapeClose) = case nodeRole ni of
      Latent     -> ("[\"",  "\"]")
      Observed _ -> ("([\"", "\"])")

-- | One Mermaid edge line.
mkEdgeLine :: (Text, Text) -> Text
mkEdgeLine (from, to) = "    " <> nodeId from <> " --> " <> nodeId to

-- | class assignment lines (one per class, skipping if empty).
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

-- | Sanitise a variable name for use as a Mermaid node ID.
nodeId :: Text -> Text
nodeId = T.map (\c -> if c `elem` (" -.+*/" :: String) then '_' else c)

isLatent :: NodeInfo -> Bool
isLatent ni = case nodeRole ni of { Latent -> True; _ -> False }

-- | Distribution family name (no parameters).
-- Used for non-root nodes whose parameters are symbolic (not concrete).
distFamily :: Distribution -> Text
distFamily (Normal _ _)      = "Normal"
distFamily (Binomial _ _)    = "Binomial"
distFamily (Poisson _)       = "Poisson"
distFamily (Exponential _)   = "Exponential"
distFamily (Gamma _ _)       = "Gamma"
distFamily (Beta _ _)        = "Beta"
