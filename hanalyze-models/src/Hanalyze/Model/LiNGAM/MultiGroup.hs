{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns      #-}
-- |
-- Module      : Hanalyze.Model.LiNGAM.MultiGroup
-- Description : MultiGroupLiNGAM (Shimizu 2012、群間で共通 DAG 構造・係数値のみ異なる LiNGAM 拡張)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: MultiGroupLiNGAM (Shimizu 2012): 複数群 (group) で __共通の DAG 構造__ を
--   仮定し、 群間で係数値は異なる可能性を許す LiNGAM 拡張。
--
-- ## モデル
--
-- 群 g = 1..G について、 観測 X^(g) は同じ causal order に従う SEM:
--
-- > X^(g) = B^(g) · X^(g) + e^(g)
--
-- 各 B^(g) の非零パターン (= DAG 構造) は __全群共通__ を仮定するが、 値は
-- 群ごとに異なってよい。 これは半導体現場の「異なる工場 / 装置号機 / 世代で
-- 同じ因果構造、 効き量だけ違う」 という想定とマッチする。
--
-- ## アルゴリズム
--
-- 1. 各群 X^(g) について @fitDirectLiNGAM@ を独立に実行 → B^(g)、 K^(g)
-- 2. 全群の K^(g) を集約して __多数決で共通 causal order__ を確定
--    (本実装: 各位置 j の頻度最大ノードを選び、 不一致時は位置 j の総合的
--    平均スコアを再計算)
-- 3. 共通 order に従い、 各群で再度 OLS で B^(g) を組み直す
-- 4. __共通 adjacency__: 各群で |B^(g)[i, j]| > thr となるエッジ数が
--    全群のうち過半数なら採用
--
-- ## リファレンス
--
-- Shimizu (2012) "Joint estimation of linear non-Gaussian acyclic models",
-- Neurocomputing 81. Python 実装は cdt15/lingam の `lingam/multi_group_lingam.py`。
--
-- [English]: MultiGroupLiNGAM (Shimizu 2012): a LiNGAM extension that assumes
--   a __common DAG structure__ across multiple groups while allowing the
--   coefficient values to differ between groups.
--
-- ## Model
--
-- For each group g = 1..G, the observations X^(g) follow the same SEM under
-- a common causal order:
--
-- > X^(g) = B^(g) · X^(g) + e^(g)
--
-- The non-zero pattern of each B^(g) (= DAG structure) is assumed to be
-- __common to all groups__, but the values may differ per group. This
-- matches the semiconductor-fab scenario of "different factories \/ tool
-- numbers \/ generations sharing the same causal structure but differing
-- only in effect magnitude."
--
-- ## Algorithm
--
-- 1. Run @fitDirectLiNGAM@ independently on each group's X^(g) → B^(g), K^(g)
-- 2. Aggregate all groups' K^(g) to determine the __common causal order by majority vote__
--    (this implementation: pick the most frequent node at
--    each position j, and on ties recompute the overall average score for
--    position j)
-- 3. Refit B^(g) for each group by OLS again, following the common order
-- 4. __Common adjacency__: adopt an edge if the count of groups where
--    |B^(g)[i, j]| > thr exceeds the majority threshold across all groups
--
-- ## Reference
--
-- Shimizu (2012) "Joint estimation of linear non-Gaussian acyclic models",
-- Neurocomputing 81. The Python implementation is cdt15/lingam's
-- `lingam/multi_group_lingam.py`.
module Hanalyze.Model.LiNGAM.MultiGroup
  ( MultiGroupConfig (..)
  , MultiGroupFit (..)
  , defaultMultiGroupConfig
  , fitMultiGroupLiNGAM
  , mgCommonDAG
  ) where

import qualified Numeric.LinearAlgebra as LA
import           Data.List             (foldl', sort, group, sortBy)
import           Data.Ord              (comparing, Down (..))

import qualified Hanalyze.Model.LiNGAM.Direct as DL
import qualified Hanalyze.Model.DAG           as DAG

-- ===========================================================================
-- 設定 / 結果
-- ===========================================================================

data MultiGroupConfig = MultiGroupConfig
  { mgcDirectCfg :: !DL.DirectLiNGAMConfig
  , mgcMajority  :: !Double
    -- ^ [日本語]: adjacency 多数決閾値 (0..1)、 default 0.5。 [English]: Adjacency
    --   majority-vote threshold (0..1), default 0.5.
  } deriving (Show)

defaultMultiGroupConfig :: MultiGroupConfig
defaultMultiGroupConfig = MultiGroupConfig
  { mgcDirectCfg = DL.defaultDirectLiNGAMConfig
  , mgcMajority  = 0.5
  }

data MultiGroupFit = MultiGroupFit
  { mgGroupFits      :: ![DL.DirectLiNGAMFit]
    -- ^ [日本語]: 各群独立 fit 結果。 [English]: Independent fit results per group.
  , mgCommonOrder    :: ![Int]
    -- ^ [日本語]: 多数決で確定した共通 causal order。 [English]: The common
    --   causal order determined by majority vote.
  , mgGroupBMats     :: ![LA.Matrix Double]
    -- ^ [日本語]: 共通 order で再 fit した各群 B 行列。 [English]: Each group's B
    --   matrix, refit under the common order.
  , mgCommonAdj      :: !(LA.Matrix Double)
    -- ^ [日本語]: 多数決による共通 adjacency マスク (0/1)。 [English]: The common
    --   adjacency mask (0\/1) determined by majority vote.
  } deriving (Show)

-- ===========================================================================
-- 主実装
-- ===========================================================================

fitMultiGroupLiNGAM :: MultiGroupConfig -> [LA.Matrix Double] -> MultiGroupFit
fitMultiGroupLiNGAM cfg groups =
  let !groupFits = [ DL.fitDirectLiNGAM (mgcDirectCfg cfg) g | g <- groups ]
      !p         = if null groupFits then 0 else LA.cols (DL.dlB (head groupFits))
      !commonOrd = majorityOrder p (map DL.dlOrder groupFits)
      -- 共通 order に従って各群で B を再度 OLS で組み立てる
      !commonBs  = [ refitWithOrder commonOrd g | g <- groups ]
      !commonAdj = majorityAdjacency
                    (mgcMajority cfg)
                    (DL.dlcPruneThr (mgcDirectCfg cfg))
                    commonBs
  in MultiGroupFit
       { mgGroupFits   = groupFits
       , mgCommonOrder = commonOrd
       , mgGroupBMats  = commonBs
       , mgCommonAdj   = commonAdj
       }

-- | [日本語]: 共通 adjacency に基づく DAG 表現。 重みは全群 B の平均を使う。
--   [English]: A DAG representation based on the common adjacency. The
--   weights use the average of all groups' B.
mgCommonDAG :: MultiGroupFit -> DAG.DAG
mgCommonDAG fit =
  let !bs   = mgGroupBMats fit
      !adj  = mgCommonAdj fit
      !p    = LA.rows adj
      !g    = fromIntegral (length bs) :: Double
      !meanB = LA.scale (1 / g) (foldl' (+) (LA.konst 0 (p, p)) bs)
      f i j
        | i == j                        = 0
        | LA.atIndex adj (i, j) == 0    = 0
        | otherwise                     = LA.atIndex meanB (i, j)
      w = LA.build (p, p) (\i j -> f (round i) (round j) :: Double)
  in DAG.mkDAG w

-- ===========================================================================
-- 内部
-- ===========================================================================

-- | [日本語]: 多数決で共通 causal order を決める。 各位置 j で最頻 node を取り、
--   重複が出たら未確定 node を残りから追加する fallback。
--   [English]: Determines the common causal order by majority vote. Takes the
--   most frequent node at each position j, and falls back to adding an
--   undetermined node from the remainder when a duplicate occurs.
majorityOrder :: Int -> [[Int]] -> [Int]
majorityOrder p orders
  | null orders = [0 .. p - 1]
  | otherwise =
      let posCount j = [ ord !! j | ord <- orders, length ord > j ]
          mostFreq xs =
            let !grouped = sortBy (comparing (Down . length))
                             (group (sort xs))
            in case grouped of
                 ((h:_):_) -> h
                 _         -> 0
          go acc unused j
            | j >= p = reverse acc
            | otherwise =
                let !cand = mostFreq (posCount j)
                in if cand `elem` unused
                     then go (cand : acc) (filter (/= cand) unused) (j + 1)
                     else
                       -- fallback: 残りから一番低 index
                       case unused of
                         []      -> reverse acc
                         (h : _) ->
                           go (h : acc) (filter (/= h) unused) (j + 1)
      in go [] [0 .. p - 1] 0

-- | [日本語]: 指定 causal order に従い X から B を OLS で組み立て直す。
--   [English]: Rebuilds B from X by OLS, following the given causal order.
refitWithOrder :: [Int] -> LA.Matrix Double -> LA.Matrix Double
refitWithOrder order x =
  let !p = LA.cols x
      mkRow j =
        let kj   = order !! j
            parents = take j order
        in if null parents
             then LA.fromList (replicate p 0)
             else
               let pm = LA.fromColumns
                     [ LA.flatten (x LA.¿ [pIdx]) | pIdx <- parents ]
                   y  = LA.flatten (x LA.¿ [kj])
                   beta = LA.flatten
                     (LA.linearSolveLS (LA.tr pm LA.<> pm)
                        (LA.asColumn (LA.tr pm LA.#> y)))
                   updates = zip parents (LA.toList beta)
                   coefV   = replicate p 0
                   filled  = foldl' (\acc (i, v) -> set acc i v) coefV updates
               in LA.fromList filled
      bRows = [ mkRow j | j <- [0 .. p - 1] ]
      pos i = case lookup i (zip order [0 ..]) of
                Just k -> k
                Nothing -> 0
      origOrderMat = LA.fromRows [ bRows !! pos i | i <- [0 .. p - 1] ]
  in origOrderMat
  where
    set xs i v = take i xs ++ [v] ++ drop (i + 1) xs

-- | [日本語]: 多数決による共通 adjacency: |B^(g)[i, j]| > thr が 全群中 majorityRatio
--   以上の比率で起こったら 1。
--   [English]: Common adjacency by majority vote: 1 if |B^(g)[i, j]| > thr
--   occurs at a rate at or above majorityRatio across all groups.
majorityAdjacency
  :: Double                -- majority ratio (0..1)
  -> Double                -- B threshold
  -> [LA.Matrix Double]
  -> LA.Matrix Double
majorityAdjacency majRatio thr bs =
  let !p = LA.rows (head bs)
      !g = fromIntegral (length bs) :: Double
      f i j
        | i == j    = 0
        | otherwise =
            let cnt = length [ () | b <- bs
                                  , abs (LA.atIndex b (i, j)) > thr ]
                rate = fromIntegral cnt / g
            in if rate >= majRatio then 1 else 0
  in LA.build (p, p) (\i j -> f (round i) (round j) :: Double)
