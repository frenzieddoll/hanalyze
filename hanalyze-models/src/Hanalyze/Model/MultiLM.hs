{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hanalyze.Model.MultiLM
-- Description : Multivariate (multi-output) linear regression — 列別 OLS + 残差共分散推定
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Multivariate (multi-output) linear regression.
--
-- @Y = XB + E@ with @Y@ of shape @n × q@ (@q@ outputs), @X@ of shape
-- @n × p@, @B@ of shape @p × q@ and @E@ of shape @n × q@.
--
-- Solves each column independently by OLS (column-wise OLS) and
-- additionally estimates the residual covariance matrix @Σ@, which is
-- used for joint multi-output predictive intervals.
--
-- The API matches 'Hanalyze.Model.LM', so @fitLM@ can be called directly; this
-- module merely exposes the additional multi-output information
-- (@Σ@, correlation matrix).
module Hanalyze.Model.MultiLM
  ( MultiFit (..)
  , fitMultiLM
  , predictMultiLM
  , residualCovariance
  , residualCorrelation
  ) where

import qualified Numeric.LinearAlgebra as LA
import Hanalyze.Model.Core (FitResult (..))
import qualified Hanalyze.Model.LM as LM

-- | Augmented result for multi-output linear regression.
data MultiFit = MultiFit
  { mfFit         :: FitResult        -- ^ Underlying matrix-based fit.
  , mfResidCov    :: LA.Matrix Double -- ^ Residual covariance @Σ@ (@q × q@).
  , mfResidCor    :: LA.Matrix Double -- ^ Residual correlation matrix (@q × q@).
  , mfNumOutputs  :: Int              -- ^ Number of responses @q@.
  , mfNumPredict  :: Int              -- ^ Number of predictors @p@.
  , mfNumSamples  :: Int              -- ^ Number of observations @n@.
  } deriving (Show)

-- | Multi-output linear regression: @Y = XB + E@.
-- Delegates to 'LM.fitLM' and additionally returns the residual
-- covariance.
fitMultiLM :: LA.Matrix Double  -- ^ Design matrix @X@ (@n × p@).
           -> LA.Matrix Double  -- ^ Response @Y@ (@n × q@).
           -> MultiFit
fitMultiLM x y =
  let fit = LM.fitLM x y
      res = residuals fit
      n   = LA.rows y
      q   = LA.cols y
      p   = LA.cols x
      df  = max 1 (n - p)   -- 自由度補正
      -- Σ = (1/(n-p)) * Eᵀ E
      sigma = LA.scale (1 / fromIntegral df)
                       (LA.tr res LA.<> res)
      -- 相関行列: D⁻¹ Σ D⁻¹ where D = diag(sqrt(diag(Σ)))
      diagS = [ sqrt (sigma `LA.atIndex` (i, i))
              | i <- [0 .. q - 1] ]
      corr  = LA.fromLists
        [ [ if di == 0 || dj == 0 then 0
            else (sigma `LA.atIndex` (i, j)) / (di * dj)
          | j <- [0 .. q - 1]
          , let dj = diagS !! j ]
        | i <- [0 .. q - 1]
        , let di = diagS !! i ]
  in MultiFit fit sigma corr q p n

-- | Predict @Ŷ@ (@m × q@) for new inputs @X_new@ (@m × p@). A thin
-- wrapper around 'LM.predictLM'.
predictMultiLM :: MultiFit -> LA.Matrix Double -> LA.Matrix Double
predictMultiLM mf xNew =
  LM.predictLM (coefficients (mfFit mf)) xNew

-- | Residual covariance matrix (alias for 'mfResidCov').
residualCovariance :: MultiFit -> LA.Matrix Double
residualCovariance = mfResidCov

-- | Residual correlation matrix.
residualCorrelation :: MultiFit -> LA.Matrix Double
residualCorrelation = mfResidCor
