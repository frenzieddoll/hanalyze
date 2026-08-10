{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns      #-}
-- |
-- Module      : Hanalyze.Model.DAG
-- Description : DAG (有向非巡回グラフ) の共通表現 (重み付き隣接行列)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: Directed Acyclic Graph (DAG) の共通表現。
--
-- 因果探索 (LiNGAM 系) / 将来の SEM / Bayesian Network の出力型を統一する。
-- 内部表現は __重み付き隣接行列__ で、 hmatrix の線形代数操作との親和性を保つ。
--
-- ## 規約
--
-- 重み行列 W (p × p) の要素 W[i, j] は __エッジ j → i の重み__ を表す。
-- これは構造方程式 X_i = Σ_j W[i, j] · X_j + e_i に対応する自然な向きで、
-- LiNGAM の B 行列と完全一致する。 W[i, i] = 0 (self-loop 禁止)。
--
-- ## DAG 判定
--
-- 'isAcyclic' は W の非零パターンから到達可能性を見て循環を検出する。
-- 浮動小数閾値の影響を避けるため、 判定は 'dagW' の __絶対値 > 0__ マスク
-- に対して実施。 ノイズで小さな非零が出る場合は事前に 'pruneByThreshold'
-- でクリーンナップする。
--
-- [English]: A common representation for Directed Acyclic Graphs
-- (DAGs).
--
-- Unifies the output type for causal discovery (LiNGAM family) \/
-- future SEM \/ Bayesian Network. The internal representation is a
-- __weighted adjacency matrix__, which stays compatible with hmatrix's
-- linear-algebra operations.
--
-- ## Convention
--
-- Element W[i, j] of the weight matrix W (p × p) represents
-- __the weight of the edge j → i__. This is the natural direction
-- corresponding to the structural equation
-- X_i = Σ_j W[i, j] · X_j + e_i, and matches LiNGAM's B matrix exactly.
-- W[i, i] = 0 (self-loops are forbidden).
--
-- ## DAG check
--
-- 'isAcyclic' detects cycles by looking at reachability over W's
-- nonzero pattern. To avoid the influence of floating-point noise, the
-- check is performed against a mask of 'dagW''s __absolute value > 0__.
-- If noise produces small nonzero values, clean them up beforehand with
-- 'pruneByThreshold'.
module Hanalyze.Model.DAG
  ( DAG (..)
  , Edge (..)
  -- 構築
  , mkDAG
  , fromAdjacency
  , fromBMatrix
  , withNames
  -- 操作
  , pruneByThreshold
  -- 問合せ
  , dagEdges
  , dagParents
  , dagChildren
  , dagNodeName
  , topoSort
  , isAcyclic
  , dagReachable
  -- 出力
  , toDOT
  ) where

import qualified Data.Set              as S
import qualified Data.Text             as T
import qualified Data.Vector           as V
import qualified Numeric.LinearAlgebra as LA
import           Data.Text             (Text)
import           Data.List             (foldl')

-- ===========================================================================
-- 型
-- ===========================================================================

data DAG = DAG
  { dagN     :: !Int
    -- ^ [日本語]: ノード数。 [English]: The number of nodes.
  , dagNames :: !(Maybe (V.Vector Text))
    -- ^ [日本語]: ノード名 (任意)。 'Nothing' なら "x0".."x(n-1)" を使う。
    --   [English]: Node names (optional). If 'Nothing', uses
    --   "x0".."x(n-1)".
  , dagW     :: !(LA.Matrix Double)
    -- ^ [日本語]: 重み付き隣接行列 (p × p)。 W[i, j] = エッジ j → i の重み。
    --   [English]: The weighted adjacency matrix (p × p).
    --   W[i, j] = the weight of the edge j → i.
  } deriving (Show)

data Edge = Edge
  { edgeFrom   :: !Int
  , edgeTo     :: !Int
  , edgeWeight :: !Double
  } deriving (Show, Eq)

-- ===========================================================================
-- 構築
-- ===========================================================================

-- | [日本語]: 重み付き隣接行列から DAG を作る。 ノード数は W の行数。 W が
--   p × p でない場合は呼出側のバグ (here で error)。
--   [English]: Builds a DAG from a weighted adjacency matrix. The node
--   count is W's row count. If W is not p × p, that is a caller bug
--   (raises an error here).
mkDAG :: LA.Matrix Double -> DAG
mkDAG w
  | LA.rows w /= LA.cols w =
      error "Hanalyze.Model.DAG.mkDAG: W は p × p 正方行列でなければならない"
  | otherwise = DAG
      { dagN     = LA.rows w
      , dagNames = Nothing
      , dagW     = w
      }

-- | [日本語]: 0/1 隣接行列から DAG。 重みはエッジ存在を 1 として保持。
--   [English]: Builds a DAG from a 0/1 adjacency matrix. The weight
--   holds 1 for edge presence.
fromAdjacency :: LA.Matrix Double -> DAG
fromAdjacency = mkDAG

-- | [日本語]: LiNGAM B 行列 + threshold から DAG を構築。 |B[i, j]| ≤ thr の
--   エッジは刈り取る。 対角要素は常に 0。
--   [English]: Builds a DAG from a LiNGAM B matrix + threshold. Edges
--   with |B[i, j]| ≤ thr are pruned. Diagonal elements are always 0.
fromBMatrix :: Double -> LA.Matrix Double -> DAG
fromBMatrix thr b = mkDAG (pruned b)
  where
    pruned m =
      let p = LA.rows m
          f i j
            | i == j                          = 0
            | abs (LA.atIndex m (i, j)) <= thr = 0
            | otherwise                       = LA.atIndex m (i, j)
      in LA.build (p, p) (\i j -> f (round i) (round j) :: Double)

-- | [日本語]: ノード名を付与する (length 不一致は呼出側のバグ)。
--   [English]: Attaches node names (a length mismatch is a caller bug).
withNames :: V.Vector Text -> DAG -> DAG
withNames ns g
  | V.length ns /= dagN g =
      error "Hanalyze.Model.DAG.withNames: ノード数と名前数が不一致"
  | otherwise = g { dagNames = Just ns }

-- ===========================================================================
-- 操作
-- ===========================================================================

-- | [日本語]: |W[i, j]| ≤ thr のエッジを 0 に。 自己ループは常に 0。
--   [English]: Zeroes out edges with |W[i, j]| ≤ thr. Self-loops are
--   always 0.
pruneByThreshold :: Double -> DAG -> DAG
pruneByThreshold thr g = g { dagW = pruned }
  where
    p = dagN g
    f i j
      | i == j                                = 0
      | abs (LA.atIndex (dagW g) (i, j)) <= thr = 0
      | otherwise                             = LA.atIndex (dagW g) (i, j)
    pruned = LA.build (p, p) (\i j -> f (round i) (round j) :: Double)

-- ===========================================================================
-- 問合せ
-- ===========================================================================

-- | [日本語]: 全エッジを (from, to, weight) のリストで返す (非零重みのみ)。
--   [English]: Returns all edges as a list of (from, to, weight)
--   (nonzero weights only).
dagEdges :: DAG -> [Edge]
dagEdges g =
  let p = dagN g
      w = dagW g
  in [ Edge j i (LA.atIndex w (i, j))
     | i <- [0 .. p - 1]
     , j <- [0 .. p - 1]
     , i /= j
     , LA.atIndex w (i, j) /= 0
     ]

-- | [日本語]: ノード i に直接影響を与えるノード集合 (W[i, j] ≠ 0 となる j のリスト)。
--   [English]: The set of nodes that directly influence node i (the
--   list of j with W[i, j] ≠ 0).
dagParents :: DAG -> Int -> [Int]
dagParents g i =
  [ j | j <- [0 .. dagN g - 1]
      , j /= i
      , LA.atIndex (dagW g) (i, j) /= 0 ]

-- | [日本語]: ノード i から直接影響を受けるノード集合 (W[k, i] ≠ 0 となる k のリスト)。
--   [English]: The set of nodes directly influenced by node i (the
--   list of k with W[k, i] ≠ 0).
dagChildren :: DAG -> Int -> [Int]
dagChildren g i =
  [ k | k <- [0 .. dagN g - 1]
      , k /= i
      , LA.atIndex (dagW g) (k, i) /= 0 ]

-- | [日本語]: ノード名取得 ('dagNames' が Nothing なら "x{idx}")。
--   [English]: Gets a node name (if 'dagNames' is Nothing, "x{idx}").
dagNodeName :: DAG -> Int -> Text
dagNodeName g i = case dagNames g of
  Just ns | i >= 0 && i < V.length ns -> ns V.! i
  _                                   -> T.pack ("x" <> show i)

-- | [日本語]: 到達可能性: from から to へ DAG エッジを辿って到達可能か。
--   [English]: Reachability: whether to is reachable from from by
--   following DAG edges.
dagReachable :: DAG -> Int -> Int -> Bool
dagReachable g from to = go S.empty [from]
  where
    go _    []     = False
    go seen (x:xs)
      | x == to               = True
      | x `S.member` seen     = go seen xs
      | otherwise             =
          let !seen' = S.insert x seen
              kids   = dagChildren g x
          in go seen' (kids ++ xs)

-- | [日本語]: 循環を含まないか。 全ノード対 (i, j) について 「j から i へ到達可能か
--   つ i → j のエッジが存在する」 ならば循環。
--   [English]: Whether the graph contains no cycle. For every node
--   pair (i, j), if "i is reachable from j, and an edge i → j exists"
--   then it is a cycle.
isAcyclic :: DAG -> Bool
isAcyclic g =
  let !p = dagN g
      cyclePair i j =
            i /= j
        &&  LA.atIndex (dagW g) (j, i) /= 0
        &&  dagReachable g j i
  in not $ or [ cyclePair i j | i <- [0 .. p - 1], j <- [0 .. p - 1] ]

-- | [日本語]: topological sort: 根 (parents なし) から葉までの並び。
--   循環を検出した場合は 'Nothing'。 Kahn のアルゴリズム (Pure 版)。
--   [English]: Topological sort: an ordering from the roots (no
--   parents) to the leaves. Returns 'Nothing' if a cycle is detected.
--   Kahn's algorithm (a pure version).
topoSort :: DAG -> Maybe [Int]
topoSort g =
  let !p     = dagN g
      inDeg0 = V.fromList [ length (dagParents g i) | i <- [0 .. p - 1] ]
      go acc inDeg remaining
        | null remaining = Just (reverse acc)
        | otherwise =
            case findRoot remaining inDeg of
              Nothing -> Nothing   -- 循環
              Just r  ->
                let kids   = dagChildren g r
                    inDegN = V.imap
                      (\idx v -> if idx `elem` kids then v - 1 else v)
                      inDeg
                in go (r : acc) inDegN (filter (/= r) remaining)
  in go [] inDeg0 [0 .. p - 1]
  where
    findRoot xs inDeg =
      case filter (\i -> (inDeg V.! i) == 0) xs of
        []    -> Nothing
        (h:_) -> Just h

-- ===========================================================================
-- 出力
-- ===========================================================================

-- | [日本語]: Graphviz DOT 形式で出力。 シェル経由で
--   @echo "..." | dot -Tpng -o dag.png@ で可視化可能。
--   [English]: Outputs the graph in Graphviz DOT format. Can be
--   visualized via the shell with
--   @echo "..." | dot -Tpng -o dag.png@.
toDOT :: DAG -> Text
toDOT g =
  let header = T.pack "digraph G {\n  rankdir=LR;\n"
      footer = T.pack "}\n"
      nodes  = T.concat
        [ T.pack "  " <> sanitize (dagNodeName g i)
          <> T.pack " [label=\"" <> dagNodeName g i <> T.pack "\"];\n"
        | i <- [0 .. dagN g - 1] ]
      edges  = T.concat
        [ T.pack "  " <> sanitize (dagNodeName g (edgeFrom e))
          <> T.pack " -> " <> sanitize (dagNodeName g (edgeTo e))
          <> T.pack " [label=\""
          <> T.pack (showWeight (edgeWeight e))
          <> T.pack "\"];\n"
        | e <- dagEdges g ]
  in header <> nodes <> edges <> footer
  where
    sanitize = T.replace (T.pack " ") (T.pack "_")
             . T.replace (T.pack "-") (T.pack "_")
    showWeight w = let r = round (w * 1000) :: Int
                   in show (fromIntegral r / 1000 :: Double)
