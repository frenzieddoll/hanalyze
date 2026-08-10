{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hanalyze.Model.Core
-- Description : 全回帰モデル共通の Result 型と Model 型クラス
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Result type and 'Model' class shared by every regression model.
--
-- For multi-output support, the principal fields of 'FitResult' are
-- generalized to @Matrix Double@ (@n × q@) or @Vector Double@ (@q@-vector).
-- Single-output (@q = 1@) models can keep using the convenience accessors
-- ('coefficientsV', 'fittedV', 'residualsV', 'rSquared1'), which return
-- @Vector@ / @Double@ as before.
--
-- Migrating a single-output model to multi-output is just a matter of
-- calling @fitLM@ with @Matrix × Matrix@ and interpreting the result like
-- a @MultiFitResult@.
module Hanalyze.Model.Core
  ( FitResult (..)
  , Model (..)
  , PredictiveModel (..)
  , ResidualModel (..)
  , Band (..)
    -- * Vec / Scalar accessors (for @q = 1@)
  , coefficientsV
  , fittedV
  , residualsV
  , rSquared1
    -- * List conversion
  , fittedList
  , coeffList
    -- * Per-column access
  , coefficientsCol
  , fittedCol
  , residualsCol
  ) where

import qualified Numeric.LinearAlgebra as LA

-- | Multi-output regression fit result.
--
-- Shapes:
--
--   * 'coefficients' — @p × q@  (@p@ features × @q@ responses).
--   * 'fitted'       — @n × q@  (@n@ observations × @q@ responses).
--   * 'residuals'    — @n × q@.
--   * 'rSquared'     — vector of length @q@ (one R² per response).
--
-- Single-output models use @q = 1@ (a one-column matrix).
data FitResult = FitResult
  { coefficients :: LA.Matrix Double  -- ^ Coefficient matrix @p × q@.
  , fitted       :: LA.Matrix Double  -- ^ Fitted values @n × q@.
  , residuals    :: LA.Matrix Double  -- ^ Residuals @n × q@.
  , rSquared     :: LA.Vector Double  -- ^ Per-response R² (length @q@).
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- Vec / Scalar アクセサ (q = 1 用)
-- ---------------------------------------------------------------------------

-- | Coefficients of a single-output fit as a @Vector@. For multi-output
-- fits this returns just the first column; use 'coefficients' to access
-- all columns.
coefficientsV :: FitResult -> LA.Vector Double
coefficientsV = LA.flatten . coefficients

-- | Fitted values @ŷ@ of a single-output fit as a @Vector@.
fittedV :: FitResult -> LA.Vector Double
fittedV = LA.flatten . fitted

-- | Residuals of a single-output fit as a @Vector@.
residualsV :: FitResult -> LA.Vector Double
residualsV = LA.flatten . residuals

-- | R² of a single-output fit as a scalar 'Double'. For multi-output
-- fits this returns the first component; use 'rSquared' for all
-- responses.
rSquared1 :: FitResult -> Double
rSquared1 r = case LA.toList (rSquared r) of
  (h : _) -> h
  []      -> 0

-- ---------------------------------------------------------------------------
-- 後方互換ヘルパ (旧 Vec API 利用者用)
-- ---------------------------------------------------------------------------

-- | Fitted values as @[Double]@ (single-output).
fittedList :: FitResult -> [Double]
fittedList = LA.toList . fittedV

-- | Coefficients as @[Double]@ (single-output).
coeffList :: FitResult -> [Double]
coeffList = LA.toList . coefficientsV

-- ---------------------------------------------------------------------------
-- 列単位アクセス (多出力時)
-- ---------------------------------------------------------------------------

-- | Coefficients for response @j@ as a @Vector@.
coefficientsCol :: Int -> FitResult -> LA.Vector Double
coefficientsCol j r = LA.flatten (coefficients r LA.¿ [j])

-- | Fitted values @ŷ@ for response @j@ as a @Vector@.
fittedCol :: Int -> FitResult -> LA.Vector Double
fittedCol j r = LA.flatten (fitted r LA.¿ [j])

-- | Residuals for response @j@ as a @Vector@.
residualsCol :: Int -> FitResult -> LA.Vector Double
residualsCol j r = LA.flatten (residuals r LA.¿ [j])

-- ---------------------------------------------------------------------------
-- 不確実性帯
-- ---------------------------------------------------------------------------

-- | Uncertainty band drawn around the mean response.
data Band
  = NoBand      -- ^ No band.
  | CI Double   -- ^ Confidence interval at the given level (e.g. 0.95).
  | PI Double   -- ^ Prediction interval (Gaussian models only).
  deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Model クラス (多出力に対応)
-- ---------------------------------------------------------------------------

-- | Common interface implemented by every regression model.
--
-- @
-- fit     m X Y        :: FitResult       -- X (n×p), Y (n×q)
-- predict m beta Xnew  :: Matrix          -- ŷ (m × q), m = rows Xnew
-- @
class Model m where
  fit     :: m -> LA.Matrix Double -> LA.Matrix Double -> FitResult
  predict :: m
          -> LA.Matrix Double  -- ^ Coefficients @β@ of shape @p × q@.
          -> LA.Matrix Double  -- ^ Test input @X_new@ of shape @m × p@.
          -> LA.Matrix Double  -- ^ Predictions @ŷ@ of shape @m × q@.

-- ---------------------------------------------------------------------------
-- 能力別 protocol (Phase 46 / plot Phase 15 = analyze 統合 A 先行)
--
-- モデルの「能力」 を細粒度 class に割り、 持てる能力だけ instance を生やす
-- (spec §2.3 = god class を避ける)。 数値核は hmatrix で完結 (list 操作で書かない)。
-- これらは plot 非依存 = hanalyze-portable (toPlot/Plottable は別途 Hanalyze.Plot)。
-- ===========================================================================

-- | 残差を取り出せるフィット結果。 'toPlot' の残差診断図 (残差 vs fitted / QQ)
-- が要求する最小能力。
class ResidualModel r where
  -- | 残差ベクトル (単出力 @q = 1@ を想定。 多出力は 'residualsCol' を使う)。
  residualsOf :: r -> LA.Vector Double

-- | 新しい入力に対し予測できるフィット結果。 'toPlot' の回帰線・予測 band が
-- 要求する最小能力。
--
-- ⚠ 既定の意味は **線形予測子** @η = X_new · β@ (列 = 各応答)。 LM では平均応答に
-- 一致するが、 GLM の平均応答 @μ = g⁻¹(η)@ には逆リンクが要る (モデルタグ依存)
-- ため、 GLM は 'Model' の 'predict' を使うこと。 本 class は線形スケールの予測を
-- 与える低レベル能力と位置づける。
class PredictiveModel r where
  -- | @X_new (m×p)@ に対する線形予測子 @ŷ = X_new · β (m×q)@。
  predictAt :: r -> LA.Matrix Double -> LA.Matrix Double

-- ---------------------------------------------------------------------------
-- FitResult instances
--
-- 'FitResult' は LM / GLM / GLMM が共有する数値核 (= 1 instance で 3 モデルを覆う)。
-- ===========================================================================

instance ResidualModel FitResult where
  residualsOf = residualsV

instance PredictiveModel FitResult where
  -- ŷ = X_new · β  (β = coefficients、 線形予測子)
  predictAt res xNew = xNew LA.<> coefficients res
