{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Hanalyze.Model.GARCH
-- Description : GARCH(1,1) 条件付き分散モデル (Generalized AutoRegressive Conditional Heteroskedasticity)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- GARCH(1,1) — Generalized AutoRegressive Conditional Heteroskedasticity.
--
-- Bollerslev (1986). Models time-varying conditional variance for a
-- (de-meaned) return series:
--
-- @
--   y_t       = μ + ε_t,   ε_t = σ_t · z_t,   z_t ~ N(0, 1)
--   σ²_t      = ω + α · ε²_{t-1} + β · σ²_{t-1}
-- @
--
-- Constraints: @ω > 0, α ≥ 0, β ≥ 0, α + β < 1@ (stationarity).
--
-- Estimation by quasi-MLE under Gaussian innovations, optimized with
-- L-BFGS using numeric gradients. The constraints are enforced via a
-- reparametrization (softplus for ω, a stick-breaking sigmoid pair for
-- α and β capped at @0.999@).
--
-- @
-- import Hanalyze.Model.GARCH
--
-- let fit = fitGARCH ys                    -- GARCH(1,1) on the series
--     vh  = forecastGARCH fit 10           -- 10-step ahead σ² forecast
-- @
--
-- == Implemented
--
--   * 'fitGARCH' (GARCH(1,1) Gaussian QMLE)
--   * 'forecastGARCH' (h-step-ahead conditional variance)
module Hanalyze.Model.GARCH
  ( GARCHFit (..)
  , fitGARCH
  , forecastGARCH
  ) where

import qualified Numeric.LinearAlgebra      as LA
import qualified Hanalyze.Optim.LBFGS       as LBFGS
import qualified Hanalyze.Optim.Common      as OC
import           System.IO.Unsafe           (unsafePerformIO)

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | Fitted GARCH(1,1) model.
data GARCHFit = GARCHFit
  { gOmega      :: !Double            -- ^ Unconditional variance offset @ω@.
  , gAlpha      :: !Double            -- ^ ARCH coefficient @α@.
  , gBeta       :: !Double            -- ^ GARCH coefficient @β@.
  , gMu         :: !Double            -- ^ Mean of @y_t@.
  , gSigma2     :: !(LA.Vector Double) -- ^ In-sample conditional variance @σ²_t@.
  , gResiduals  :: !(LA.Vector Double) -- ^ In-sample residuals @ε_t = y_t - μ@.
  , gLogLik     :: !Double            -- ^ Maximized Gaussian log-likelihood.
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- Reparametrization helpers
-- ---------------------------------------------------------------------------

softplus :: Double -> Double
softplus x
  | x >  50 = x
  | x < -50 = exp x
  | otherwise = log1p (exp x)
  where log1p z = log (1 + z)

sigmoid :: Double -> Double
sigmoid x
  | x >  500 = 1
  | x < -500 = 0
  | otherwise = 1 / (1 + exp (-x))

-- | Map unconstrained @(θω, θα, θβ)@ to @(ω, α, β)@.
unpackParams :: Double -> Double -> Double -> (Double, Double, Double)
unpackParams t0 t1 t2 =
  let w   = softplus t0
      s   = sigmoid t1                -- total persistence ∈ (0, 1)
      sab = s * 0.999                 -- α + β strictly < 1
      r   = sigmoid t2                -- α-share of total ∈ (0, 1)
      a   = sab * r
      b   = sab * (1 - r)
  in (w, a, b)

-- Inverse of 'unpackParams' for warm-starting from a feasible point.
packParams :: Double -> Double -> Double -> (Double, Double, Double)
packParams w a b =
  let !sab = a + b
      !t0  = invSoftplus w
      !t1  = invSigmoid (sab / 0.999)
      !r   = if sab > 0 then a / sab else 0.5
      !t2  = invSigmoid r
  in (t0, t1, t2)
  where
    invSoftplus y
      | y > 50    = y
      | otherwise = log (exp y - 1)
    invSigmoid p =
      let pc = min 0.99999 (max 1e-5 p)
      in log (pc / (1 - pc))

-- ---------------------------------------------------------------------------
-- Recursion
-- ---------------------------------------------------------------------------

-- | Run the GARCH(1,1) σ² recursion. @σ²_0@ is initialized to the sample
-- variance of @ε@ (a standard QMLE starting choice; alternatives such as
-- the unconditional variance @ω/(1-α-β)@ are equivalent in the limit).
recurseSigma2
  :: Double            -- ^ ω.
  -> Double            -- ^ α.
  -> Double            -- ^ β.
  -> LA.Vector Double  -- ^ ε.
  -> LA.Vector Double  -- ^ σ² of same length as ε.
recurseSigma2 !w !a !b eps =
  let n      = LA.size eps
      var0   = LA.dot eps eps / fromIntegral n
      sig0   = max 1e-12 var0
      step !s2Prev !ePrev = w + a * ePrev * ePrev + b * s2Prev
      go !i !s2Prev acc
        | i >= n   = reverse acc
        | otherwise =
            let !s2 = if i == 0
                       then sig0
                       else step s2Prev (LA.atIndex eps (i - 1))
            in go (i + 1) s2 (s2 : acc)
  in LA.fromList (go 0 0 [])

-- | Negative Gaussian log-likelihood (to be minimized).
negLL
  :: LA.Vector Double  -- ^ ε.
  -> Double            -- ^ ω.
  -> Double            -- ^ α.
  -> Double            -- ^ β.
  -> Double
negLL eps w a b =
  let s2 = recurseSigma2 w a b eps
      n  = LA.size eps
      ll = sum [ let s = max 1e-12 (LA.atIndex s2 i)
                     e = LA.atIndex eps i
                 in log (2 * pi * s) + e * e / s
               | i <- [0 .. n - 1] ]
  in 0.5 * ll

-- ---------------------------------------------------------------------------
-- Fitting
-- ---------------------------------------------------------------------------

-- | Fit a GARCH(1,1) model to @y@ by Gaussian QMLE. The mean @μ@ is
-- estimated as the sample mean; ω/α/β are jointly optimized by L-BFGS
-- with numeric gradients in an unconstrained reparametrization.
--
-- Starting values: @α = 0.05@, @β = 0.90@, @ω = (1 - α - β) · Var(ε)@
-- (so that the unconditional variance matches the sample variance).
fitGARCH :: LA.Vector Double -> GARCHFit
fitGARCH y =
  let n     = LA.size y
      mu    = LA.sumElements y / fromIntegral n
      eps   = y - LA.scalar mu
      var0  = LA.dot eps eps / fromIntegral n
      a0    = 0.05
      b0    = 0.90
      w0    = max 1e-8 ((1 - a0 - b0) * var0)
      (t00, t10, t20) = packParams w0 a0 b0
      objL [t0, t1, t2] =
        let (w, a, b) = unpackParams t0 t1 t2
        in negLL eps w a b
      objL _ = error "fitGARCH: expected 3 parameters"
      cfg   = LBFGS.defaultLBFGSConfig
      res   = unsafePerformIO (LBFGS.runLBFGSNumeric cfg objL [t00, t10, t20])
      [t0, t1, t2] = OC.orBest res
      (w, a, b) = unpackParams t0 t1 t2
      s2    = recurseSigma2 w a b eps
  in GARCHFit
       { gOmega     = w
       , gAlpha     = a
       , gBeta      = b
       , gMu        = mu
       , gSigma2    = s2
       , gResiduals = eps
       , gLogLik    = negate (OC.orValue res)
       }

-- ---------------------------------------------------------------------------
-- Forecasting
-- ---------------------------------------------------------------------------

-- | @h@-step-ahead conditional variance forecast. The recursion is
--
-- @
--   σ²_{T+1} = ω + α · ε²_T + β · σ²_T
--   σ²_{T+k} = ω + (α + β) · σ²_{T+k-1}    (k ≥ 2)
-- @
--
-- so that the forecast converges to the unconditional variance
-- @ω / (1 - α - β)@.
forecastGARCH :: GARCHFit -> Int -> LA.Vector Double
forecastGARCH fit h
  | h <= 0    = LA.fromList []
  | otherwise =
      let w   = gOmega fit
          a   = gAlpha fit
          b   = gBeta fit
          s2  = gSigma2 fit
          eps = gResiduals fit
          n   = LA.size s2
          sT  = LA.atIndex s2 (n - 1)
          eT  = LA.atIndex eps (n - 1)
          s1  = w + a * eT * eT + b * sT
          go !k !prev
            | k > h     = []
            | k == 1    = s1 : go 2 s1
            | otherwise =
                let !nxt = w + (a + b) * prev
                in nxt : go (k + 1) nxt
      in LA.fromList (go 1 0)
