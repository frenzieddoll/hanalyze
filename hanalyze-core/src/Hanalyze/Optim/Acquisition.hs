-- |
-- Module      : Hanalyze.Optim.Acquisition
-- Description : ベイズ最適化の獲得関数 (単一目的 EI/UCB/PI, 多目的 EHVI/ParEGO)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Acquisition functions for Bayesian Optimization.
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
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
module Hanalyze.Optim.Acquisition
  ( ei
  , ucb
  , pi_
    -- * Multi-objective
  , parEGO
  , ehvi2D
  ) where

import Statistics.Distribution     (cumulative, density)
import Statistics.Distribution.Normal (standard)

-- ---------------------------------------------------------------------------
-- 単一目的 acquisition 関数
-- ---------------------------------------------------------------------------

-- | Expected Improvement (minimization, with exploration parameter @ξ@).
--
-- @
-- EI(x) = E[max(y_best − y(x), 0)]
--       = (y_best − μ) Φ(z) + σ φ(z)
-- where z = (y_best − μ − ξ) / σ
-- @
ei :: Double               -- ^ Current best @y_best@ (minimum so far).
   -> Double               -- ^ Exploration trade-off @ξ@ (0.01 typical).
   -> (Double, Double)     -- ^ Predictive @(μ, σ)@.
   -> Double
ei yBest xi (mu, sigma)
  | sigma <= 0 = 0
  | otherwise =
      let z   = (yBest - mu - xi) / sigma
          phi = density standard z
          cdf = cumulative standard z
      in (yBest - mu - xi) * cdf + sigma * phi

-- | Lower Confidence Bound for minimization (sometimes called UCB).
--
-- @LCB(x) = μ − β σ@. Large @β@ encourages exploration (prefers large
-- @σ@); small @β@ encourages exploitation (prefers small @μ@).
ucb :: Double -> (Double, Double) -> Double
ucb beta (mu, sigma) = mu - beta * sigma

-- | Probability of Improvement.
--
-- @PI(x) = P(y(x) < y_best − ξ) = Φ((y_best − μ − ξ) / σ)@.
pi_ :: Double -> Double -> (Double, Double) -> Double
pi_ yBest xi (mu, sigma)
  | sigma <= 0 = 0
  | otherwise =
      let z = (yBest - mu - xi) / sigma
      in cumulative standard z

-- ---------------------------------------------------------------------------
-- 多目的 acquisition
-- ---------------------------------------------------------------------------

-- | ParEGO (Knowles 2006): Tchebycheff scalarization + EI.
--
-- Each iteration draws a random weight vector @w@ and computes EI on the
-- scalarized objective:
--
-- @
-- y_scalar(x) = max_j (w_j (y_j(x) − z*_j)) + ρ Σ_j w_j (y_j(x) − z*_j)
-- @
parEGO :: [Double]              -- ^ Weights @w@ (non-negative, sum to 1).
       -> [Double]              -- ^ Ideal point @z*@ (per-objective minima).
       -> Double                -- ^ ParEGO @ρ@ (≈ 0.05).
       -> Double                -- ^ Best scalarized value so far @y_best@.
       -> [(Double, Double)]    -- ^ Per-objective predictive @(μ_j, σ_j)@.
       -> Double                -- ^ Scalarized EI value (to be maximized).
parEGO weights ideal rho yBest preds =
  let -- scalarized μ: max_j (w_j (μ_j - z*_j)) + rho Σ ...
      diffs    = zipWith3 (\w mu zStar -> w * (mu - zStar)) weights (map fst preds) ideal
      muScalar = maximum diffs + rho * sum diffs
      -- scalarized σ: 簡易合算 (上界)
      sigSqs   = zipWith (\w (_, sg) -> (w * sg) ^ (2 :: Int)) weights preds
      sigScalar = sqrt (sum sigSqs)
  in ei yBest 0.01 (muScalar, sigScalar)

-- | Expected Hypervolume Improvement (2-objective only).
--
-- Computes the expected hypervolume gained by adding a candidate point
-- @(μ, σ)@ to the current Pareto front. The full EHVI integral is
-- expensive, so this implementation uses a Monte Carlo approximation.
ehvi2D :: [Double]                  -- ^ Reference point @r@ (2D).
       -> [[Double]]                -- ^ Current front (each point @[y1, y2]@).
       -> [(Double, Double)]        -- ^ Per-objective predictive @(μ, σ)@.
       -> Int                       -- ^ Number of Monte Carlo samples.
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
