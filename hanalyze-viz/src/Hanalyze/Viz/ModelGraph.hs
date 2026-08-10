-- |
-- Module      : Hanalyze.Viz.ModelGraph
-- Description : モデル DAG の Mermaid.js 可視化 (ModelGraph → HTML)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
{-# LANGUAGE OverloadedStrings #-}
-- | [日本語]: モデル DAG の Mermaid.js 可視化。
--
-- @Hanalyze.Model.HBM.buildModelGraph@ がポリモーフィックなモデルから
-- 自動導出する 'ModelGraph' を HTML ファイルへ描画する (Mermaid CDN 経由で
-- ブラウザに表示)。
--
-- == 3 ルートの選び方 (= hgg の過去の phase で 3 ルート併存方針を確立)
--
-- 同じ 'Hanalyze.Model.HBM.ModelGraph' を可視化する 3 種類のルート:
--
-- [@本 module@]: 'renderModelGraph' (= Mermaid HTML)。 出力 / 描画依存 =
--   .html + CDN script。 推奨用途 = GitHub README、 ノート。
-- [@Graphviz DOT@]: "Hanalyze.Viz.ModelGraphDot".renderModelGraphDot。
--   出力 / 描画依存 = .dot + dot CLI 別途。 推奨用途 = graphviz 連携、 加工。
-- [@hgg@]: @Graphics.Hgg.Bridge.Analyze.renderModelGraphSVG@
--   (= @hgg-analyze-bridge@ package)。 出力 / 描画依存 = .svg
--   (依存ゼロ)。 推奨用途 = production、 offline。
--
-- 3 ルートとも同じ 'Hanalyze.Model.HBM.ModelGraph' 構造 (= node / edge / plate)
-- を表現する。 visual layout は実装ごとに異なる。 本ルート (= Mermaid) の利点:
--
--   * GitHub / GitLab README で render される (= 添付画像不要、 文字列で済む)
--   * ノート系 tool (= Notion 等) に貼り付けやすい
--   * 軽量 (= .html 1 ファイル、 ~5KB)
--
-- 弱点 (= 上記の他ルートで補える):
--
--   * ブラウザ + ネット必須 (= offline 不可)
--   * production アプリ組込みには不向き (= hgg ルート推奨)
--   * 高度な layout (= graphviz dot 流) には不向き (= ModelGraphDot 推奨)
--
-- __本 module は撤廃されません__。 OSS 利用者の既存ワークフローを尊重して 3 ルート併存。
--
-- [English]: Mermaid.js visualization of model DAGs.
--
-- Renders the 'ModelGraph' that @Hanalyze.Model.HBM.buildModelGraph@ derives
-- automatically from a polymorphic model into an HTML file (displayed
-- in the browser via the Mermaid CDN).
--
-- == Choosing among the 3 routes (= a policy of maintaining 3 parallel
-- routes was established in an earlier hgg phase)
--
-- Three routes visualize the same 'Hanalyze.Model.HBM.ModelGraph':
--
-- [@This module@]: 'renderModelGraph' (= Mermaid HTML). Output /
--   rendering dependency = .html + CDN script. Recommended use = GitHub
--   README, notebooks.
-- [@Graphviz DOT@]: "Hanalyze.Viz.ModelGraphDot".renderModelGraphDot.
--   Output / rendering dependency = .dot + a separate dot CLI. Recommended
--   use = graphviz integration, post-processing.
-- [@hgg@]: @Graphics.Hgg.Bridge.Analyze.renderModelGraphSVG@
--   (= the @hgg-analyze-bridge@ package). Output / rendering
--   dependency = .svg (zero dependencies). Recommended use = production,
--   offline.
--
-- All three routes represent the same 'Hanalyze.Model.HBM.ModelGraph'
-- structure (= node / edge / plate); only the visual layout differs by
-- implementation. Advantages of this route (= Mermaid):
--
--   * Rendered directly by GitHub / GitLab READMEs (= no attached image
--     needed, plain text suffices)
--   * Easy to paste into note-taking tools (= Notion, etc.)
--   * Lightweight (= a single .html file, ~5KB)
--
-- Weaknesses (compensated for by the other routes above):
--
--   * Requires a browser + network (= not usable offline)
--   * Not well-suited to embedding in production apps (= the hgg
--     route is recommended instead)
--   * Not well-suited to advanced layouts (= graphviz dot style; use
--     ModelGraphDot instead)
--
-- __This module will not be removed.__ All 3 routes coexist to respect
-- OSS users' existing workflows.
module Hanalyze.Viz.ModelGraph
  ( renderModelGraph
  , buildMermaid
  ) where

import Data.Text (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.List (groupBy, sortOn)
import Data.Function (on)

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

-- | [日本語]: 'ModelGraph' から Mermaid の @flowchart TD@ ソースを生成する。
--   plate に属するノードは @subgraph plate_<name>["<name> × N"]@ で囲まれる
--   (nested plate も入れ子で出力する)。
--   [English]: Build the Mermaid @flowchart TD@ source for a 'ModelGraph'.
--   Nodes belonging to a plate are wrapped in
--   @subgraph plate_<name>["<name> × N"]@ (nested plates are emitted as
--   nested subgraphs too).
buildMermaid :: ModelGraph -> Text
buildMermaid mg = T.unlines $
  [ "flowchart TD" ] ++
  renderNodesGrouped 1 [] (mgNodes mg) (mgPlates mg) ++
  [ "" ] ++
  map mkEdgeLine (mgEdges mg) ++
  [ "" ] ++
  [ "    classDef latent   fill:#4C72B0,color:#fff,stroke:#2a5080,stroke-width:1.5px" ] ++
  [ "    classDef observed fill:#DD8844,color:#fff,stroke:#b06020,stroke-width:1.5px" ] ++
  classAssignLines mg

-- | [日本語]: nodePlates のスタックに沿って nodes をグルーピングし、 nested
--   subgraph を出力する。 @depth@ はインデント用。 @curPath@ は現在
--   出力中の plate path (外→内)。
--   [English]: Groups nodes along the stack of nodePlates and emits
--   nested subgraphs. @depth@ is for indentation; @curPath@ is the plate
--   path currently being emitted (outer to inner).
renderNodesGrouped :: Int -> [Text] -> [Node] -> Map.Map Text Int -> [Text]
renderNodesGrouped depth curPath ns plateSizes =
  let ind = T.replicate (depth * 4) " "
      -- 現位置に属する (= nodePlates == curPath) ノードを直接出力
      hereNodes = [n | n <- ns, nodePlates n == curPath]
      -- このスコープより内側 (= nodePlates が curPath で始まる かつ より長い) を集める
      innerNodes = [n | n <- ns, isStrictPrefix curPath (nodePlates n)]
      -- innerNodes を「curPath の直後の plate 名」 でグループ化
      keyOf n = (nodePlates n) !! length curPath
      sortedInner = sortOn keyOf innerNodes
      grouped = groupBy ((==) `on` keyOf) sortedInner
      hereLines = map (\n -> ind <> mkNodeLine n) hereNodes
      innerLines = concatMap (renderPlateGroup depth curPath plateSizes) grouped
  in hereLines ++ innerLines

renderPlateGroup :: Int -> [Text] -> Map.Map Text Int -> [Node] -> [Text]
renderPlateGroup _ _ _ [] = []
renderPlateGroup depth curPath plateSizes ns@(n0:_) =
  let plateName = (nodePlates n0) !! length curPath
      sz        = Map.findWithDefault 0 plateName plateSizes
      ind       = T.replicate (depth * 4) " "
      header    = ind <> "subgraph plate_" <> sanitize plateName
                <> "[\"" <> plateName <> " × " <> T.pack (show sz) <> "\"]"
      footer    = ind <> "end"
      inner     = renderNodesGrouped (depth + 1) (curPath ++ [plateName])
                                     ns plateSizes
  in [header] ++ inner ++ [footer]

isStrictPrefix :: Eq a => [a] -> [a] -> Bool
isStrictPrefix prefix xs =
  length prefix < length xs && take (length prefix) xs == prefix

sanitize :: Text -> Text
sanitize = T.map (\c -> if c `elem` (" -.+*/" :: String) then '_' else c)

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
      -- Phase 60.4: DeterministicN は従来非網羅 (deterministic を含むモデルで
      -- crash・ModelGraphDot の Phase 59.2 と同類) だったのを同時修正。
      DeterministicN -> nodeName n <> "\\n" <> nodeDist n
      DataN k -> nodeName n <> "\\n(n=" <> T.pack (show k) <> ")"
    escaped = T.replace "\"" "&quot;" label
    (shapeOpen, shapeClose) = case nodeKind n of
      LatentN     -> ("[\"",  "\"]")
      ObservedN _ -> ("([\"", "\"])")
      DeterministicN -> ("[\"", "\"]")
      DataN _     -> ("(\"", "\")")

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
