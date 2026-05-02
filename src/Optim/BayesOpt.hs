{-# LANGUAGE OverloadedStrings #-}
-- | Bayesian Optimization ループ (Phase V3)。
--
-- 単一目的:
-- 1. 初期点 (LHS or random) を評価
-- 2. GP を fit
-- 3. acquisition 関数を最大化して次の x を選ぶ
-- 4. x を評価して観測列に追加
-- 5. 2-4 を T 回反復
module Optim.BayesOpt
  ( BayesOptConfig (..)
  , defaultBayesOptConfig
  , bayesOpt
  , bayesOptMOWithNSGA
  ) where

import Control.Monad (forM, replicateM)
import System.Random.MWC (GenIO, uniform)

import Model.GP (Kernel (..), GPModel (..), GPResult (..),
                 fitGP, optimizeGP, initParamsFromData)
import Optim.Acquisition (ei, ucb, parEGO)
import Optim.NSGA       (Bounds, NSGAConfig (..), defaultNSGAConfig,
                         Solution (..), nsga2)

data BayesOptConfig = BayesOptConfig
  { boIterations :: Int        -- 評価予算 (初期点除く)
  , boInitPoints :: Int        -- 初期 LHS の点数
  , boKernel     :: Kernel
  , boUCBBeta    :: Double     -- UCB の β
  , boGridSize   :: Int        -- 内側最適化のグリッド密度 (1D 用)
  } deriving (Show)

defaultBayesOptConfig :: BayesOptConfig
defaultBayesOptConfig = BayesOptConfig
  { boIterations = 30
  , boInitPoints = 5
  , boKernel     = RBF
  , boUCBBeta    = 2.0
  , boGridSize   = 200
  }

-- | 単一目的 Bayesian Optimization (1D 限定の簡易版)。
--
-- 引数:
--   * cfg
--   * f: 目的関数 (1D, 最小化)
--   * (lo, hi): 探索範囲
--
-- 戻り値: (全観測 [(x, y)], 最良 (x*, y*))
bayesOpt :: BayesOptConfig
         -> (Double -> IO Double)
         -> (Double, Double)
         -> GenIO
         -> IO ([(Double, Double)], (Double, Double))
bayesOpt cfg f (lo, hi) gen = do
  -- 初期点 (uniform random, 簡易)
  initX <- replicateM (boInitPoints cfg) (do
              u <- uniform gen :: IO Double
              return (lo + u * (hi - lo)))
  initY <- mapM f initX
  let history0 = zip initX initY

  -- BO ループ
  let loop t hist
        | t == 0 = return hist
        | otherwise = do
            let xs = map fst hist
                ys = map snd hist
                yBest = minimum ys
                p0 = initParamsFromData xs ys
                pOpt = optimizeGP (boKernel cfg) xs ys p0
                model = GPModel (boKernel cfg) pOpt

                grid = [lo + fromIntegral i * (hi - lo)
                              / fromIntegral (boGridSize cfg - 1)
                       | i <- [0 .. boGridSize cfg - 1]]
                res = fitGP model xs ys grid
                mus = gpMean res
                sigs = zipWith (\u m -> (u - m) / 2) (gpUpper res) mus
                preds = zip mus sigs
                -- EI で最大化
                eiVals = [ei yBest 0.01 p | p <- preds]
                bestI = argmax eiVals
                xNext = grid !! bestI

            yNext <- f xNext
            loop (t - 1) (hist ++ [(xNext, yNext)])

  finalHist <- loop (boIterations cfg) history0
  let bestPair = head [pair | pair@(_, y) <- finalHist
                            , y == minimum (map snd finalHist)]
  return (finalHist, bestPair)

argmax :: Ord a => [a] -> Int
argmax xs = snd (maximum (zip xs [0..]))

-- ---------------------------------------------------------------------------
-- 多目的 BO with NSGA-II (Phase V4)
-- ---------------------------------------------------------------------------

-- | 多目的 BO の簡易版: 各観測点を NSGA-II で acquisition 関数値を
-- 多目的最適化することで提案する。
--
-- 内部で MultiGP を fit し、各目的の (μ, σ) を予測。
-- NSGA-II で「(目的 1 の μ, 目的 2 の μ, ...) の Pareto front」を求め、
-- そこから 1 点を選んで評価。
--
-- ※ シンプルな実装。EHVI ベースのより洗練された方式は将来拡張。
bayesOptMOWithNSGA
  :: Int                                -- イテレーション数
  -> Int                                -- 初期点
  -> Kernel
  -> ([Double] -> IO [Double])          -- 多目的関数
  -> Bounds
  -> GenIO
  -> IO [([Double], [Double])]          -- (x, y) の系列
bayesOptMOWithNSGA nIter nInit kern f bounds gen = do
  -- 初期点
  initX <- replicateM nInit (do
              vs <- forM bounds $ \(lo, hi) -> do
                u <- uniform gen :: IO Double
                return (lo + u * (hi - lo))
              return vs)
  initY <- mapM f initX
  let history0 = zip initX initY

  let loop t hist
        | t == 0 = return hist
        | otherwise = do
            -- 各目的に GP を fit (1D 入力前提の簡易版)
            -- 多次元入力の場合は MultiGP を別途準備
            -- ここでは bounds の最初の次元のみ使う簡易動作
            let xsFlat = map head (map fst hist)  -- 1D 入力前提
                ysAll = map snd hist
                qDim  = length (head ysAll)
                ysCol j = [y !! j | y <- ysAll]

            -- 各目的 j の GP モデルを fit
            let modelFor j =
                  let trainY = ysCol j
                      p0 = initParamsFromData xsFlat trainY
                      pOpt = optimizeGP kern xsFlat trainY p0
                  in GPModel kern pOpt

                models = [modelFor j | j <- [0 .. qDim - 1]]

                -- NSGA-II で Pareto front を探索 (acquisition surface 上)
                -- 各目的: μ - β σ (LCB) を最小化
                acqObjective xVec =
                  [ let trainY = ysCol j
                        m = models !! j
                        gpRes = fitGP m xsFlat trainY [head xVec]
                        mu = head (gpMean gpRes)
                        sigma = (head (gpUpper gpRes) - mu) / 2
                    in ucbToMin mu sigma
                  | j <- [0 .. qDim - 1] ]

                ucbToMin :: Double -> Double -> Double
                ucbToMin mu sigma = mu - 2.0 * sigma   -- LCB

            -- NSGA-II で Pareto front を 1 ステップ探索
            front <- nsga2 (defaultNSGAConfig { nsgaPopSize = 30
                                             , nsgaGenerations = 30 })
                          acqObjective bounds gen

            -- front から random 選択
            idx <- uniform gen :: IO Double
            let i = floor (idx * fromIntegral (length front))
                xNext = solDecision (front !! min i (length front - 1))
            yNext <- f xNext
            loop (t - 1) (hist ++ [(xNext, yNext)])

  loop nIter history0
