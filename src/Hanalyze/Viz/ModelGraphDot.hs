-- |
-- Module      : Hanalyze.Viz.ModelGraphDot
-- Description : モデル DAG の Graphviz DOT 出力 (PyMC model_to_graphviz 同等の plate 描画)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
{-# LANGUAGE OverloadedStrings #-}
-- | Graphviz DOT 出力 (Phase 40-A3、 PyMC @pm.model_to_graphviz@ 同等の plate 描画)。
--
-- 'Hanalyze.Model.HBM.buildModelGraph' が出す 'ModelGraph' を DOT
-- ソースに変換する。 plate は @subgraph cluster_<name>@ + @label="<name> × N"@
-- (右下サイズ数字) で囲まれ、 PyMC 流の角丸長方形描画になる。
--
-- 使い方:
--
-- > let g = HBM.buildModelGraph m
-- > let dot = renderModelGraphDot g
-- > T.writeFile "model.dot" dot
-- > -- graphviz CLI で PNG / SVG 化:
-- > -- $ dot -Tpng model.dot -o model.png
--
-- == 3 ルートの選び方 (= hgg Phase 2 で 3 ルート併存方針確立)
--
-- 同じ 'Hanalyze.Model.HBM.ModelGraph' を可視化する 3 種類のルート:
--
-- +---------------+--------------------------------------------------+---------------------+-----------------------+
-- | ルート        | 場所                                             | 出力 / 描画依存     | 推奨用途              |
-- +===============+==================================================+=====================+=======================+
-- | Mermaid HTML  | "Hanalyze.Viz.ModelGraph".renderModelGraph       | .html + CDN script  | GitHub README、 ノート |
-- +---------------+--------------------------------------------------+---------------------+-----------------------+
-- | __本 module__ | 'renderModelGraphDot' (= Graphviz DOT)           | .dot + dot CLI 別途 | graphviz 連携、 加工  |
-- +---------------+--------------------------------------------------+---------------------+-----------------------+
-- | hgg  | @Graphics.Hgg.Bridge.Analyze.renderModelGraphSVG@| .svg (依存ゼロ)     | production、 offline  |
-- |               | (= @hgg-analyze-bridge@ package)        |                     |                       |
-- +---------------+--------------------------------------------------+---------------------+-----------------------+
--
-- 3 ルートとも同じ 'Hanalyze.Model.HBM.ModelGraph' 構造 (= node / edge / plate)
-- を表現する。 visual layout は実装ごとに異なる。 本ルート (= Graphviz DOT) の利点:
--
--   * graphviz dot の高品質 layout (= Sugiyama framework 本家、 数十年の蓄積)
--   * @-Tpng@ @-Tsvg@ @-Tpdf@ @-Tps@ 等 多 format 出力
--   * @rank=same@ @constraint=false@ @cluster@ 等 dot 固有 directive で細かい制御
--   * 既存 graphviz エコシステム (= xdot、 gephi 等) と連携
--
-- 弱点 (= 上記表の他ルートで補える):
--
--   * @dot@ CLI が install 済必須 (= production 配布で外部依存)
--   * 出力は .dot text 中間ファイル (= 描画は別 step、 pipeline 化必要)
--
-- __本 module は撤廃されません__。 OSS 利用者の既存ワークフローを尊重して 3 ルート併存。
module Hanalyze.Viz.ModelGraphDot
  ( renderModelGraphDot
  , writeModelGraphDot
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

-- | 'ModelGraph' を Graphviz DOT 形式の 'Text' に変換する。
renderModelGraphDot :: ModelGraph -> Text
renderModelGraphDot mg = T.unlines $
  [ "digraph G {"
  , "    rankdir=TB;"
  , "    node [fontname=\"sans-serif\", fontsize=10];"
  , "    edge [arrowsize=0.7];"
  , ""
  ] ++
  renderNodesGrouped 1 [] (mgNodes mg) (mgPlates mg) ++
  [ "" ] ++
  map mkEdgeLine (mgEdges mg) ++
  [ "}" ]

-- | DOT をファイルに書き出す利便 helper。
writeModelGraphDot :: FilePath -> ModelGraph -> IO ()
writeModelGraphDot path mg = TIO.writeFile path (renderModelGraphDot mg)

-- ---------------------------------------------------------------------------
-- Node grouping by plate (nested cluster)
-- ---------------------------------------------------------------------------

renderNodesGrouped :: Int -> [Text] -> [Node] -> Map.Map Text Int -> [Text]
renderNodesGrouped depth curPath ns plateSizes =
  let ind = T.replicate (depth * 4) " "
      hereNodes = [n | n <- ns, nodePlates n == curPath]
      innerNodes = [n | n <- ns, isStrictPrefix curPath (nodePlates n)]
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
      header    = ind <> "subgraph cluster_" <> sanitize plateName <> " {"
      label     = ind <> "    label=\"" <> plateName <> " × "
                  <> T.pack (show sz) <> "\";"
      style     = ind <> "    style=\"rounded\";"
      labelloc  = ind <> "    labelloc=\"b\";"  -- 下に表示 (PyMC 流)
      footer    = ind <> "}"
      inner     = renderNodesGrouped (depth + 1) (curPath ++ [plateName])
                                     ns plateSizes
  in [header, label, style, labelloc] ++ inner ++ [footer]

isStrictPrefix :: Eq a => [a] -> [a] -> Bool
isStrictPrefix prefix xs =
  length prefix < length xs && take (length prefix) xs == prefix

-- ---------------------------------------------------------------------------
-- Node / Edge rendering
-- ---------------------------------------------------------------------------

mkNodeLine :: Node -> Text
mkNodeLine n =
  let nid     = nodeId (nodeName n)
      label   = case nodeKind n of
        LatentN        -> nodeName n <> "\\n" <> nodeDist n
        ObservedN k    -> nodeName n <> "\\n" <> nodeDist n
                          <> "\\n(n=" <> T.pack (show k) <> ")"
        DeterministicN -> nodeName n <> "\\n" <> nodeDist n
        -- Phase 60.4: データ slot は名前 + 長さのみ (分布を持たない)
        DataN k        -> nodeName n <> "\\n(n=" <> T.pack (show k) <> ")"
      escaped = T.replace "\"" "&quot;" label
      attrs = case nodeKind n of
        -- 潜在: 楕円・白塗り
        LatentN        -> "label=\"" <> escaped <> "\", shape=ellipse"
        -- 観測: 楕円・灰色塗り (PyMC 流)
        ObservedN _    -> "label=\"" <> escaped <> "\", shape=ellipse, "
                          <> "style=filled, fillcolor=lightgray"
        -- 決定的変換: 四角・白塗り (PyMC の Deterministic 流)
        DeterministicN -> "label=\"" <> escaped <> "\", shape=box"
        -- データ slot (pm.Data 相当): 角丸四角・灰塗り (PyMC ConstantData 流)
        DataN _        -> "label=\"" <> escaped <> "\", shape=box, "
                          <> "style=\"rounded,filled\", fillcolor=lightgray"
  in nid <> " [" <> attrs <> "];"

mkEdgeLine :: (Text, Text) -> Text
mkEdgeLine (from, to) = "    " <> nodeId from <> " -> " <> nodeId to <> ";"

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

nodeId :: Text -> Text
nodeId = T.map (\c -> if c `elem` (" -.+*/" :: String) then '_' else c)

sanitize :: Text -> Text
sanitize = nodeId

_unused :: Set.Set Text -> Set.Set Text
_unused = id
