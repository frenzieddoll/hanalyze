{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns      #-}
-- |
-- Module      : Hanalyze.Model.LiNGAM.Bootstrap
-- Description : BootstrapLiNGAM (エッジ出現頻度・平均係数・符号一致率による DAG confidence 診断)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: BootstrapLiNGAM: @DirectLiNGAM@ を B 個の bootstrap サンプルに対し fit し、
--   エッジ毎の出現頻度 (confidence) と平均係数を出す。
--
-- ## アルゴリズム
--
-- 1. B 回の bootstrap サンプル (行を with-replacement で n 個抽出) を生成
-- 2. 各サンプルで @fitDirectLiNGAM@ を呼ぶ
-- 3. エッジ (j → i) ごとに:
--    - 出現頻度 = (|B[i, j]| > threshold となった bootstrap の数) / B
--    - 平均係数 = 出現した bootstrap での B[i, j] の平均
--    - 符号一致率 = sign の合致率 (符号の不安定性を診断)
--
-- ## 出力
--
-- 'BootstrapResult' は @edgeProbability@ / @edgeMeanWeight@ / @signConsistency@
-- の 3 つの p × p 行列を保持。 これらを使って 「確からしい因果関係 のみ
-- 採用する DAG」 を構築できる。
--
-- ## リファレンス
--
-- Shimizu (2014) "Bayesian estimation of causal direction in acyclic structural
-- equation models with individual-specific confounder variables and
-- non-Gaussian distributions" (BootstrapLiNGAM の運用紹介)。
-- Python 実装は cdt15/lingam の `lingam/bootstrap.py`。
--
-- [English]: BootstrapLiNGAM: fits @DirectLiNGAM@ to B bootstrap samples,
--   and produces the per-edge occurrence frequency (confidence) and mean
--   coefficient.
--
-- ## Algorithm
--
-- 1. Generate B bootstrap samples (draw n rows with replacement).
-- 2. Call @fitDirectLiNGAM@ on each sample.
-- 3. For each edge (j → i):
--    - occurrence frequency = (the number of bootstraps where
--      |B[i, j]| > threshold) / B.
--    - mean coefficient = the mean of B[i, j] over the bootstraps where it
--      occurred.
--    - sign consistency = the sign-agreement rate (diagnoses sign
--      instability).
--
-- ## Output
--
-- 'BootstrapResult' holds three p × p matrices: @edgeProbability@ \/
-- @edgeMeanWeight@ \/ @signConsistency@. These can be used to build a "DAG
-- that adopts only sufficiently confident causal relations".
--
-- ## Reference
--
-- Shimizu (2014) "Bayesian estimation of causal direction in acyclic
-- structural equation models with individual-specific confounder
-- variables and non-Gaussian distributions" (introduces the
-- BootstrapLiNGAM procedure). The Python implementation is
-- cdt15/lingam's `lingam/bootstrap.py`.
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
    -- ^ [日本語]: B (resample 回数)、 default 100。
    --   [English]: B (the number of resamples), default 100.
  , bcDirectCfg     :: !DL.DirectLiNGAMConfig
    -- ^ [日本語]: 各 bootstrap で使う DirectLiNGAM 設定。
    --   [English]: The DirectLiNGAM configuration used for each bootstrap.
  , bcEdgeThreshold :: !Double
    -- ^ [日本語]: |B[i, j]| > thr のとき「エッジあり」 と数える、 default 0.05。
    --   [English]: Counted as "edge present" when |B[i, j]| > thr, default
    --   0.05.
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
    -- ^ [日本語]: p × p、 (i, j) = エッジ j → i の出現頻度 (0..1)。
    --   [English]: p × p; (i, j) = the occurrence frequency of edge j → i
    --   (0..1).
  , brEdgeMeanWeight  :: !(LA.Matrix Double)
    -- ^ [日本語]: p × p、 (i, j) = エッジが出現した bootstrap における B[i, j] の平均。
    --   [English]: p × p; (i, j) = the mean of B[i, j] over the bootstraps
    --   where the edge occurred.
  , brSignConsistency :: !(LA.Matrix Double)
    -- ^ [日本語]: p × p、 (i, j) = エッジが出現した bootstrap での符号合致率
    --   (1.0 = 全部同符号、 0.5 = 半々)。
    --   [English]: p × p; (i, j) = the sign-agreement rate over the
    --   bootstraps where the edge occurred (1.0 = all the same sign, 0.5 =
    --   evenly split).
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

-- | [日本語]: 'fitBootstrapLiNGAM' の __seed 純粋版__ (@df |->@ 用)。 @bcSeed@ (既定 42・
--   'Nothing' は 42 fallback) で 'runST'+MWC。 同 seed で IO 版とビット一致 (乱数列は monad 非依存)。
--   [English]: The __seed-pure version__ of 'fitBootstrapLiNGAM' (for
--   @df |->@). Runs 'runST'+MWC with @bcSeed@ (default 42; 'Nothing' falls
--   back to 42). Bit-identical to the IO version for the same seed (the
--   random sequence is monad-independent).
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

-- | [日本語]: 「出現頻度 ≥ probThreshold かつ符号合致率 ≥ signThreshold」 のエッジだけ
--   採用した DAG を構築。 重みは 'brEdgeMeanWeight' を使う。
--   [English]: Builds a DAG that adopts only edges with "occurrence
--   frequency ≥ probThreshold and sign consistency ≥ signThreshold". Uses
--   'brEdgeMeanWeight' for the weights.
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
