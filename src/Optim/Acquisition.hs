{-# LANGUAGE OverloadedStrings #-}
-- | Acquisition functions for Bayesian Optimization.
--
-- Single-objective:
--
--   * EI  — Expected Improvement (Mockus 1978).
--   * UCB — Upper Confidence Bound.
--   * PI  — Probability of Improvement.
--
-- Multi-objective:
--
--   * EHVI   — Expected Hypervolume Improvement.
--   * ParEGO — Tchebycheff scalarization + EI.
module Optim.Acquisition
  ( ei
  , ucb
  , pi_
    -- * 多目的
  , parEGO
  , ehvi2D
  ) where

import Statistics.Distribution     (cumulative, density)
import Statistics.Distribution.Normal (standard)

-- ---------------------------------------------------------------------------
-- 単一目的 acquisition 関数
-- ---------------------------------------------------------------------------

-- | Expected Improvement (最小化問題、ε-greedy with ξ):
-- EI(x) = E[max(y_best - y(x), 0)]
--       = (y_best - μ) Φ(z) + σ φ(z)
-- where z = (y_best - μ - ξ) / σ
ei :: Double      -- y_best (現在の最良値、最小化なので最小)
   -> Double      -- ξ (探索促進、典型 0.01)
   -> (Double, Double)   -- (μ, σ) 予測平均と標準偏差
   -> Double
ei yBest xi (mu, sigma)
  | sigma <= 0 = 0
  | otherwise =
      let z   = (yBest - mu - xi) / sigma
          phi = density standard z
          cdf = cumulative standard z
      in (yBest - mu - xi) * cdf + sigma * phi

-- | Upper Confidence Bound (LCB for minimization).
-- LCB(x) = μ - β σ。β 大で探索 (大きい σ を好む)、小で活用 (小さい μ を好む)。
ucb :: Double -> (Double, Double) -> Double
ucb beta (mu, sigma) = mu - beta * sigma

-- | Probability of Improvement.
-- PI(x) = P(y(x) < y_best - ξ) = Φ((y_best - μ - ξ) / σ)
pi_ :: Double -> Double -> (Double, Double) -> Double
pi_ yBest xi (mu, sigma)
  | sigma <= 0 = 0
  | otherwise =
      let z = (yBest - mu - xi) / sigma
      in cumulative standard z

-- ---------------------------------------------------------------------------
-- 多目的 acquisition
-- ---------------------------------------------------------------------------

-- | ParEGO (Knowles 2006): Tchebycheff scalarization + EI。
-- 各反復でランダム重みベクトル w を選び、scalarized 目的:
--   y_scalar(x) = max_j (w_j (y_j(x) - z*_j)) + ρ Σ_j w_j (y_j(x) - z*_j)
-- に対して EI を計算する。
--
-- 引数:
--   * weights: 各目的の重み w_j (≥ 0、合計 1)
--   * ideal: z* (各目的の最小値、最小化問題)
--   * rho: 0.05 程度
--   * yBest: scalarized 空間の現在最良
--   * (mus, sigmas): 各目的の予測 (μ_j, σ_j)
--
-- 戻り値: scalarized EI 値 (最大化したい)
parEGO :: [Double]              -- weights
       -> [Double]              -- ideal point z*
       -> Double                -- rho
       -> Double                -- y_best (scalarized)
       -> [(Double, Double)]    -- (μ_j, σ_j) per objective
       -> Double
parEGO weights ideal rho yBest preds =
  let -- scalarized μ: max_j (w_j (μ_j - z*_j)) + rho Σ ...
      diffs    = zipWith3 (\w mu zStar -> w * (mu - zStar)) weights (map fst preds) ideal
      muScalar = maximum diffs + rho * sum diffs
      -- scalarized σ: 簡易合算 (上界)
      sigSqs   = zipWith (\w (_, sg) -> (w * sg) ^ (2 :: Int)) weights preds
      sigScalar = sqrt (sum sigSqs)
  in ei yBest 0.01 (muScalar, sigScalar)

-- | Expected Hypervolume Improvement (2D 限定).
-- 既存の Pareto front P からなる HV に対し、新点 (μ, σ) を追加した場合の
-- 期待 HV 増分 を計算する。
--
-- 単純な計算 (Couckuyt 2014 の近似):
--   1. front を sort
--   2. 新点が改善する候補矩形について EI 風の積分で寄与計算
--
-- 完全な EHVI 計算は重いので、ここでは Monte Carlo 近似を提供。
ehvi2D :: [Double]                  -- 参照点 r (2D)
       -> [[Double]]                -- 現在の front (各点 [y1, y2])
       -> [(Double, Double)]        -- 各目的の (μ, σ)
       -> Int                       -- MC サンプル数
       -> Double
ehvi2D _ref _front _preds 0 = 0
ehvi2D ref front preds nSamples =
  let -- 現在 HV
      currentHV = hv2DSimple ref front
      -- MC: 新点 y_new = (μ_1 + σ_1 z_1, μ_2 + σ_2 z_2) で z ~ N(0, 1)
      sample i =
        let z1 = qnorm ((fromIntegral i + 0.5) / fromIntegral nSamples)
            z2 = qnorm ((fromIntegral i + 0.13) / fromIntegral nSamples)
            (m1, s1) = head preds
            (m2, s2) = preds !! 1
            yNew = [m1 + s1 * z1, m2 + s2 * z2]
            newFront = pareto2D (yNew : front)
            newHV = hv2DSimple ref newFront
        in max 0 (newHV - currentHV)
      improvements = [sample i | i <- [0 .. nSamples - 1]]
  in sum improvements / fromIntegral nSamples

-- 2D simplified HV
hv2DSimple :: [Double] -> [[Double]] -> Double
hv2DSimple [rx, ry] front =
  let valid  = [p | p <- front, head p < rx, p !! 1 < ry]
      sorted = sortByFst valid
      go _   [] acc = acc
      go yPrev (p:ps) acc =
        let xCur = head p
            yCur = p !! 1
        in if yCur >= yPrev
             then go yPrev ps acc
             else go yCur ps (acc + (rx - xCur) * (yPrev - yCur))
  in go ry sorted 0
hv2DSimple _ _ = 0

-- 2D Pareto front 抽出
pareto2D :: [[Double]] -> [[Double]]
pareto2D pts =
  [p | (i, p) <- indexed,
       not (any (\(j, q) -> j /= i && allLE q p && anyLT q p) indexed) ]
  where
    indexed = zip [0 :: Int ..] pts
    allLE a b = and (zipWith (<=) a b)
    anyLT a b = or (zipWith (<) a b)

sortByFst :: [[Double]] -> [[Double]]
sortByFst = qs
  where
    qs []     = []
    qs (p:xs) = qs [x | x <- xs, head x <= head p]
                ++ [p]
                ++ qs [x | x <- xs, head x > head p]

-- 標準正規分布の逆関数 (簡易、Beasley-Springer/Moro)
qnorm :: Double -> Double
qnorm p
  | p <= 0    = -1/0
  | p >= 1    =  1/0
  | otherwise =
      -- 近似 (誤差 < 4.5e-4 in central, やや悪化 in tails)
      let t = if p < 0.5 then sqrt (-2 * log p)
                          else sqrt (-2 * log (1 - p))
          c0 = 2.515517; c1 = 0.802853; c2 = 0.010328
          d1 = 1.432788; d2 = 0.189269; d3 = 0.001308
          num = c0 + c1 * t + c2 * t * t
          den = 1 + d1 * t + d2 * t * t + d3 * t * t * t
          x   = t - num / den
      in if p < 0.5 then -x else x
