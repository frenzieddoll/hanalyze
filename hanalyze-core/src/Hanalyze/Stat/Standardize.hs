{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hanalyze.Stat.Standardize
-- Description : 入力特徴量の標準化 (z-score) ユーティリティ
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Input-feature standardization (z-score) utilities.
--
-- Use cases:
--
-- * In RFF / kernel models, a single shared length scale @ℓ@ breaks down
--   when features differ in magnitude. Fit @(μ, σ)@ with
--   'fitStandardizer', apply with 'applyStandardizer', and convert
--   model-returned predictions back to original units with
--   'unapplyStandardizer'.
-- * For interactive (JS) predictors where the user enters values in
--   original units (e.g. @energy=80 keV@) via a slider, expose 'stMu' /
--   'stSd' so the browser can apply @(v-μ)/σ@ before sending values into
--   the model. The fields are JSON-friendly.
--
-- Conventions:
--
-- * @y@ is /not/ standardized (the output scale of regression is preserved).
-- * Constant columns (std = 0) are treated as if std = 1, returning
--   @(x - μ)/1 = x - μ@ — effectively centering only.
-- * Single-row columns (n = 1) are likewise treated as std = 1.
module Hanalyze.Stat.Standardize
  ( Standardizer (..)
  , fitStandardizer
  , applyStandardizer
  , unapplyStandardizer
  , applyStandardizerCol
  , identityStandardizer
  ) where

import qualified Numeric.LinearAlgebra as LA

-- ---------------------------------------------------------------------------
-- 型
-- ---------------------------------------------------------------------------

-- | Per-feature mean and standard deviation. The list length is the
-- feature count @p@.
data Standardizer = Standardizer
  { stMu :: ![Double]   -- ^ Per-feature mean @μ@.
  , stSd :: ![Double]   -- ^ Per-feature standard deviation @σ@.
  } deriving (Eq, Show)

-- | The identity standardizer (@μ = 0, σ = 1@) of dimension @p@.
identityStandardizer :: Int -> Standardizer
identityStandardizer p = Standardizer (replicate p 0) (replicate p 1)

-- ---------------------------------------------------------------------------
-- 学習 (fit)
-- ---------------------------------------------------------------------------

-- | Learn the per-column @(mean, std)@ from an @n × p@ matrix.
--
-- * @std@ is the unbiased estimate (@n-1@ denominator).
-- * Columns whose @std@ is below @1e-12@ are coerced to @std = 1@ to
--   avoid divide-by-zero on constant features.
fitStandardizer :: LA.Matrix Double -> Standardizer
fitStandardizer x =
  let cols = LA.toColumns x
      mus  = map mean cols
      sds  = zipWith (\c m -> robustSd c m) cols mus
  in Standardizer mus sds
  where
    mean v
      | LA.size v == 0 = 0
      | otherwise      = LA.sumElements v / fromIntegral (LA.size v)
    robustSd v m =
      let n = LA.size v
      in if n <= 1
           then 1.0
           else
             let xs   = LA.toList v
                 ss   = sum [ (x' - m) * (x' - m) | x' <- xs ]
                 var  = ss / fromIntegral (n - 1)
                 sd0  = sqrt var
             in if sd0 < 1e-12 then 1.0 else sd0

-- ---------------------------------------------------------------------------
-- 適用 / 復元
-- ---------------------------------------------------------------------------

-- | Apply @(x - μ) / σ@ to every row.
applyStandardizer :: Standardizer -> LA.Matrix Double -> LA.Matrix Double
applyStandardizer s x =
  let cols  = LA.toColumns x
      cols' = zipWith3 transformCol cols (stMu s) (stSd s)
  in LA.fromColumns cols'
  where
    transformCol c m sd = LA.cmap (\v -> (v - m) / sd) c

-- | Apply @x · σ + μ@ to every row (standardized space → original units).
unapplyStandardizer :: Standardizer -> LA.Matrix Double -> LA.Matrix Double
unapplyStandardizer s x =
  let cols  = LA.toColumns x
      cols' = zipWith3 untransformCol cols (stMu s) (stSd s)
  in LA.fromColumns cols'
  where
    untransformCol c m sd = LA.cmap (\v -> v * sd + m) c

-- | Single-cell standardization for one column (used by the JS slider
-- predictor). Returns the value unchanged when the index is out of range.
applyStandardizerCol :: Standardizer -> Int -> Double -> Double
applyStandardizerCol s k v
  | k < 0 || k >= length (stMu s) = v
  | otherwise =
      let m  = stMu s !! k
          sd = stSd s !! k
      in (v - m) / sd
