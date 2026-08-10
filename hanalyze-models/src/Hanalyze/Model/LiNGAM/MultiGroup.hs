{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns      #-}
-- |
-- Module      : Hanalyze.Model.LiNGAM.MultiGroup
-- Description : MultiGroupLiNGAM (Shimizu 2012、群間で共通 DAG 構造・係数値のみ異なる LiNGAM 拡張)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- MultiGroupLiNGAM (Shimizu 2012): 複数群 (group) で **共通の DAG 構造** を
--   仮定し、 群間で係数値は異なる可能性を許す LiNGAM 拡張。
--
-- ## モデル
--
-- 群 g = 1..G について、 観測 X^(g) は同じ causal order に従う SEM:
--
-- > X^(g) = B^(g) · X^(g) + e^(g)
--
-- 各 B^(g) の非零パターン (= DAG 構造) は **全群共通** を仮定するが、 値は
-- 群ごとに異なってよい。 これは半導体現場の「異なる工場 / 装置号機 / 世代で
-- 同じ因果構造、 効き量だけ違う」 という想定とマッチする。
--
-- ## アルゴリズム
--
-- 1. 各群 X^(g) について 'fitDirectLiNGAM' を独立に実行 → B^(g)、 K^(g)
-- 2. 全群の K^(g) を集約して **多数決で共通 causal order** を確定
--    (本実装: 各位置 j の頻度最大ノードを選び、 不一致時は位置 j の総合的
--    平均スコアを再計算)
-- 3. 共通 order に従い、 各群で再度 OLS で B^(g) を組み直す
-- 4. **共通 adjacency**: 各群で |B^(g)[i, j]| > thr となるエッジ数が
--    全群のうち過半数なら採用
--
-- ## リファレンス
--
-- Shimizu (2012) "Joint estimation of linear non-Gaussian acyclic models",
-- Neurocomputing 81. Python 実装は cdt15/lingam の `lingam/multi_group_lingam.py`。
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
    -- ^ adjacency 多数決閾値 (0..1)、 default 0.5
  } deriving (Show)

defaultMultiGroupConfig :: MultiGroupConfig
defaultMultiGroupConfig = MultiGroupConfig
  { mgcDirectCfg = DL.defaultDirectLiNGAMConfig
  , mgcMajority  = 0.5
  }

data MultiGroupFit = MultiGroupFit
  { mgGroupFits      :: ![DL.DirectLiNGAMFit]
    -- ^ 各群独立 fit 結果
  , mgCommonOrder    :: ![Int]
    -- ^ 多数決で確定した共通 causal order
  , mgGroupBMats     :: ![LA.Matrix Double]
    -- ^ 共通 order で再 fit した各群 B 行列
  , mgCommonAdj      :: !(LA.Matrix Double)
    -- ^ 多数決による共通 adjacency マスク (0/1)
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

-- | 共通 adjacency に基づく DAG 表現。 重みは全群 B の平均を使う。
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

-- | 多数決で共通 causal order を決める。 各位置 j で最頻 node を取り、
--   重複が出たら未確定 node を残りから追加する fallback。
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

-- | 指定 causal order に従い X から B を OLS で組み立て直す。
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

-- | 多数決による共通 adjacency: |B^(g)[i, j]| > thr が 全群中 majorityRatio
--   以上の比率で起こったら 1。
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
