{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : Hanalyze.Model.VAR
-- Description : 多変量自己回帰 VAR(p) モデルの方程式別 OLS 推定と予測
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- VAR(p) — Vector AutoRegressive model.
--
-- Multivariate generalization of AR(p): for a @K@-dimensional series
-- @yₜ ∈ ℝᴷ@,
--
-- @
--   yₜ = c + A₁·yₜ₋₁ + A₂·yₜ₋₂ + … + Aₚ·yₜ₋ₚ + εₜ
-- @
--
-- where each @Aₗ@ is a @K × K@ coefficient matrix and @c@ is a length-@K@
-- intercept. Estimation is by equation-by-equation OLS, which is the
-- maximum-likelihood estimator for VAR under Gaussian innovations (the
-- stacked system has the same regressors in every equation, so SUR
-- collapses to OLS — Lütkepohl 2005 §3.2).
--
-- @
-- import Hanalyze.Model.VAR
--
-- let fit = fitVAR 2 yMat              -- VAR(2) on n × K series
--     fc  = forecastVAR fit yMat 10    -- 10-step ahead
-- @
--
-- == Implemented
--
--   * 'fitVAR' (equation-by-equation OLS, joint estimation)
--   * 'forecastVAR' (deterministic point forecast, h steps)
module Hanalyze.Model.VAR
  ( VARFit (..)
  , fitVAR
  , forecastVAR
  ) where

import qualified Numeric.LinearAlgebra as LA

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | Fitted VAR(p) model.
data VARFit = VARFit
  { varP         :: !Int              -- ^ Lag order @p@.
  , varK         :: !Int              -- ^ Series dimensionality @K@.
  , varConst     :: !(LA.Vector Double) -- ^ Intercept @c@ (length @K@).
  , varCoefs     :: ![LA.Matrix Double] -- ^ @[A₁, …, Aₚ]@, each @K × K@.
  , varResiduals :: !(LA.Matrix Double) -- ^ Residuals, @(n − p) × K@.
  , varSigma     :: !(LA.Matrix Double) -- ^ Residual covariance @K × K@.
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- Fitting
-- ---------------------------------------------------------------------------

-- | Fit a VAR(@p@) model to an @n × K@ series @Y@ by equation-by-equation
-- OLS. The first @p@ rows are consumed as the initial lag window;
-- @n − p@ effective observations are used. Requires @n > p · K + 1@.
fitVAR :: Int -> LA.Matrix Double -> VARFit
fitVAR p y =
  let n     = LA.rows y
      k     = LA.cols y
      neff  = n - p
      -- Design matrix Z: each row t = [1, y_{t-1}, y_{t-2}, …, y_{t-p}]
      -- (1 + p·K columns), for t = p, p+1, …, n-1.
      buildRow t =
        1.0 : concat [ LA.toList (LA.flatten (y LA.? [t - l]))
                     | l <- [1 .. p] ]
      zRows = [ buildRow t | t <- [p .. n - 1] ]
      z     = LA.fromLists zRows                -- (neff × (1 + p·K))
      yLag  = y LA.?? (LA.Drop p, LA.All)       -- (neff × K)
      -- OLS: B = (Zᵀ Z)⁻¹ Zᵀ Y. Use linearSolveLS (least squares) for
      -- numerical stability.
      bMat  = LA.linearSolveLS z yLag           -- ((1 + p·K) × K)
      cVec  = LA.flatten (bMat LA.? [0])        -- intercept (K,)
      coefs =
        [ LA.tr (bMat LA.?? ( LA.Pos (LA.idxs [ 1 + (l - 1) * k + j
                                              | j <- [0 .. k - 1] ])
                            , LA.All ))
        | l <- [1 .. p] ]
        -- Each block row of B is K rows giving Aₗᵀ; transpose for K × K Aₗ.
      yhat  = z LA.<> bMat
      resid = yLag - yhat
      sigma = (LA.tr resid LA.<> resid)
              / fromIntegral (max 1 (neff - (1 + p * k)))
  in VARFit
       { varP         = p
       , varK         = k
       , varConst     = cVec
       , varCoefs     = coefs
       , varResiduals = resid
       , varSigma     = sigma
       }

-- ---------------------------------------------------------------------------
-- Forecasting
-- ---------------------------------------------------------------------------

-- | Deterministic @h@-step-ahead point forecast (ε set to zero):
--
-- @
--   ŷ_{T+k} = c + Σₗ Aₗ · ŷ_{T+k-ℓ}
-- @
--
-- where @ŷ_{T+j} = y_{T+j}@ for @j ≤ 0@. The full input series @y@ is
-- accepted to supply the last @p@ rows used as initial history.
forecastVAR :: VARFit -> LA.Matrix Double -> Int -> LA.Matrix Double
forecastVAR fit y h
  | h <= 0    = LA.fromLists []
  | otherwise =
      let p    = varP fit
          n    = LA.rows y
          -- Initial history: last p rows of y, as a [Vector Double] list
          -- with index 0 = y_{T-1}, index 1 = y_{T-2}, …, index p-1 = y_{T-p}.
          hist0 = [ LA.flatten (y LA.? [n - 1 - i]) | i <- [0 .. p - 1] ]
          step !hist =
            let !pred_ =
                  varConst fit
                  + foldr1 (+)
                      [ (varCoefs fit !! (l - 1)) LA.#> (hist !! (l - 1))
                      | l <- [1 .. p] ]
            in (pred_, pred_ : init hist)
          go !k !hist acc
            | k > h     = reverse acc
            | otherwise =
                let (yk, hist') = step hist
                in go (k + 1) hist' (yk : acc)
      in LA.fromRows (go 1 hist0 [])
