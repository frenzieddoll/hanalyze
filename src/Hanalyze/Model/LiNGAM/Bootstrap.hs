{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns      #-}
-- |
-- Module      : Hanalyze.Model.LiNGAM.Bootstrap
-- Description : BootstrapLiNGAM (エッジ出現頻度・平均係数・符号一致率による DAG confidence 診断)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- BootstrapLiNGAM: 'DirectLiNGAM' を B 個の bootstrap サンプルに対し fit し、
--   エッジ毎の出現頻度 (confidence) と平均係数を出す。
--
-- ## アルゴリズム
--
-- 1. B 回の bootstrap サンプル (行を with-replacement で n 個抽出) を生成
-- 2. 各サンプルで 'fitDirectLiNGAM' を呼ぶ
-- 3. エッジ (j → i) ごとに:
--    * 出現頻度 = (|B[i, j]| > threshold となった bootstrap の数) / B
--    * 平均係数 = 出現した bootstrap での B[i, j] の平均
--    * 符号一致率 = sign の合致率 (符号の不安定性を診断)
--
-- ## 出力
--
-- 'BootstrapResult' は 'edgeProbability' / 'edgeMeanWeight' / 'signConsistency'
-- の 3 つの p × p 行列を保持。 これらを使って 「確からしい因果関係 のみ
-- 採用する DAG」 を構築できる。
--
-- ## リファレンス
--
-- Shimizu (2014) "Bayesian estimation of causal direction in acyclic structural
-- equation models with individual-specific confounder variables and
-- non-Gaussian distributions" (BootstrapLiNGAM の運用紹介)。
-- Python 実装は cdt15/lingam の `lingam/bootstrap.py`。
module Hanalyze.Model.LiNGAM.Bootstrap
  ( BootstrapConfig (..)
  , BootstrapResult (..)
  , defaultBootstrapConfig
  , fitBootstrapLiNGAM
  , fitBootstrapLiNGAMPure
  , confidenceDAG
  ) where

import qualified Numeric.LinearAlgebra      as LA
import qualified System.Random.MWC          as MWC
import           Control.Monad              (replicateM)
import           Control.Monad.ST           (runST)
import qualified Data.Vector                as V

import qualified Hanalyze.Model.LiNGAM.Direct as DL
import qualified Hanalyze.Model.DAG           as DAG

-- ===========================================================================
-- 設定
-- ===========================================================================

data BootstrapConfig = BootstrapConfig
  { bcNumBootstraps :: !Int
    -- ^ B (resample 回数)、 default 100
  , bcDirectCfg     :: !DL.DirectLiNGAMConfig
    -- ^ 各 bootstrap で使う DirectLiNGAM 設定
  , bcEdgeThreshold :: !Double
    -- ^ |B[i, j]| > thr のとき「エッジあり」 と数える、 default 0.05
  , bcSeed          :: !(Maybe Int)
  } deriving (Show)

defaultBootstrapConfig :: BootstrapConfig
defaultBootstrapConfig = BootstrapConfig
  { bcNumBootstraps = 100
  , bcDirectCfg     = DL.defaultDirectLiNGAMConfig
  , bcEdgeThreshold = 0.05
  , bcSeed          = Just 42
  }

-- ===========================================================================
-- 結果
-- ===========================================================================

data BootstrapResult = BootstrapResult
  { brEdgeProbability :: !(LA.Matrix Double)
    -- ^ p × p、 (i, j) = エッジ j → i の出現頻度 (0..1)
  , brEdgeMeanWeight  :: !(LA.Matrix Double)
    -- ^ p × p、 (i, j) = エッジが出現した bootstrap における B[i, j] の平均
  , brSignConsistency :: !(LA.Matrix Double)
    -- ^ p × p、 (i, j) = エッジが出現した bootstrap での符号合致率
    --   (1.0 = 全部同符号、 0.5 = 半々)
  , brNumBootstraps   :: !Int
  } deriving (Show)

-- ===========================================================================
-- 主実装
-- ===========================================================================

fitBootstrapLiNGAM :: BootstrapConfig -> LA.Matrix Double -> IO BootstrapResult
fitBootstrapLiNGAM cfg xs = do
  let !n = LA.rows xs
      !p = LA.cols xs
      !b = bcNumBootstraps cfg
      !thr = bcEdgeThreshold cfg
  gen <- case bcSeed cfg of
    Just s  -> MWC.initialize (V.fromList [fromIntegral s])
    Nothing -> MWC.createSystemRandom
  -- 各 bootstrap の B 行列を集める
  bMats <- replicateM b $ do
    idxs <- V.replicateM n (MWC.uniformR (0, n - 1) gen)
    let !resample = xs LA.? V.toList idxs
        !fit      = DL.fitDirectLiNGAM (bcDirectCfg cfg) resample
    pure (DL.dlB fit)
  let !probMat = computeEdgeProbability thr p bMats
      !meanMat = computeEdgeMeanWeight  thr p bMats
      !signMat = computeSignConsistency thr p bMats
  pure BootstrapResult
    { brEdgeProbability = probMat
    , brEdgeMeanWeight  = meanMat
    , brSignConsistency = signMat
    , brNumBootstraps   = b
    }

-- | 'fitBootstrapLiNGAM' の **seed 純粋版** (Phase 77.C・@df |->@ 用)。 @bcSeed@ (既定 42・
--   'Nothing' は 42 fallback) で 'runST'+MWC。 同 seed で IO 版とビット一致 (乱数列は monad 非依存)。
fitBootstrapLiNGAMPure :: BootstrapConfig -> LA.Matrix Double -> BootstrapResult
fitBootstrapLiNGAMPure cfg xs = runST $ do
  let !n = LA.rows xs
      !p = LA.cols xs
      !b = bcNumBootstraps cfg
      !thr = bcEdgeThreshold cfg
  gen <- MWC.initialize (V.fromList [fromIntegral (maybe 42 id (bcSeed cfg))])
  bMats <- replicateM b $ do
    idxs <- V.replicateM n (MWC.uniformR (0, n - 1) gen)
    let !resample = xs LA.? V.toList idxs
    pure (DL.dlB (DL.fitDirectLiNGAM (bcDirectCfg cfg) resample))
  pure BootstrapResult
    { brEdgeProbability = computeEdgeProbability thr p bMats
    , brEdgeMeanWeight  = computeEdgeMeanWeight  thr p bMats
    , brSignConsistency = computeSignConsistency thr p bMats
    , brNumBootstraps   = b
    }

-- | 「出現頻度 ≥ probThreshold かつ符号合致率 ≥ signThreshold」 のエッジだけ
--   採用した DAG を構築。 重みは 'brEdgeMeanWeight' を使う。
confidenceDAG
  :: Double           -- 出現頻度閾値 (例 0.7)
  -> Double           -- 符号合致率閾値 (例 0.8)
  -> BootstrapResult
  -> DAG.DAG
confidenceDAG probThr signThr res =
  let !p     = LA.rows (brEdgeProbability res)
      f i j
        | i == j                                     = 0
        | LA.atIndex (brEdgeProbability res) (i, j) < probThr = 0
        | LA.atIndex (brSignConsistency res) (i, j) < signThr = 0
        | otherwise = LA.atIndex (brEdgeMeanWeight res) (i, j)
      w = LA.build (p, p) (\i j -> f (round i) (round j) :: Double)
  in DAG.mkDAG w

-- ===========================================================================
-- 内部: 集計
-- ===========================================================================

computeEdgeProbability :: Double -> Int -> [LA.Matrix Double] -> LA.Matrix Double
computeEdgeProbability thr p bMats =
  let !n = fromIntegral (length bMats) :: Double
      f i j
        | i == j    = 0
        | otherwise =
            let !cnt = length [ () | b <- bMats
                                   , abs (LA.atIndex b (i, j)) > thr ]
            in fromIntegral cnt / n
  in LA.build (p, p) (\i j -> f (round i) (round j) :: Double)

computeEdgeMeanWeight :: Double -> Int -> [LA.Matrix Double] -> LA.Matrix Double
computeEdgeMeanWeight thr p bMats =
  let f i j
        | i == j    = 0
        | otherwise =
            let vs = [ LA.atIndex b (i, j)
                     | b <- bMats
                     , abs (LA.atIndex b (i, j)) > thr ]
            in if null vs then 0 else sum vs / fromIntegral (length vs)
  in LA.build (p, p) (\i j -> f (round i) (round j) :: Double)

computeSignConsistency :: Double -> Int -> [LA.Matrix Double] -> LA.Matrix Double
computeSignConsistency thr p bMats =
  let f i j
        | i == j    = 0
        | otherwise =
            let vs = [ LA.atIndex b (i, j)
                     | b <- bMats
                     , abs (LA.atIndex b (i, j)) > thr ]
            in if null vs then 0
               else let !nPos = length (filter (> 0) vs)
                        !nNeg = length (filter (< 0) vs)
                        !tot  = nPos + nNeg
                    in if tot == 0 then 0
                       else fromIntegral (max nPos nNeg) / fromIntegral tot
  in LA.build (p, p) (\i j -> f (round i) (round j) :: Double)
