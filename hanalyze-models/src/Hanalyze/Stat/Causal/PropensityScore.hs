-- |
-- Module      : Hanalyze.Stat.Causal.PropensityScore
-- Description : logistic regression による Propensity Score P(T=1|X) の推定
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: Propensity Score の推定。
--
-- @p_i = P(T = 1 | X_i)@ を logistic regression (GLM Binomial+Logit) で
-- 推定する。 観測研究での因果効果推定 (IPW / AIPW / CATE) の前提となる
-- 共変量バランス指標。
--
-- ## 使い方
--
-- @
--   let ps = propensityScore xConf treat
--       ps' = trimPropensity 0.01 0.99 ps   -- 重み発散防止
--       w   = ipwWeights ps' treat          -- t/p + (1-t)/(1-p)
-- @
--
-- Reference:
--   Rosenbaum & Rubin (1983) "The Central Role of the Propensity Score in
--   Observational Studies for Causal Effects". Biometrika 70:41-55.
--
-- [English]: Estimation of the Propensity Score.
--
-- Estimates @p_i = P(T = 1 | X_i)@ via logistic regression (GLM
-- Binomial+Logit). This is the covariate-balance measure that underlies
-- causal-effect estimation in observational studies (IPW \/ AIPW \/ CATE).
--
-- ## Usage
--
-- @
--   let ps = propensityScore xConf treat
--       ps' = trimPropensity 0.01 0.99 ps   -- prevent weight divergence
--       w   = ipwWeights ps' treat          -- t/p + (1-t)/(1-p)
-- @
--
-- Reference:
--   Rosenbaum & Rubin (1983) "The Central Role of the Propensity Score in
--   Observational Studies for Causal Effects". Biometrika 70:41-55.
module Hanalyze.Stat.Causal.PropensityScore
  ( PropensityScore (..)
  , propensityScore
  , trimPropensity
  , ipwWeights
  , attWeights
  ) where

import qualified Numeric.LinearAlgebra as LA
import qualified Hanalyze.Model.GLM   as GLM
import           Hanalyze.Model.Core   (coefficientsV, fittedV)

-- ---------------------------------------------------------------------------
-- 型
-- ---------------------------------------------------------------------------

data PropensityScore = PropensityScore
  { psScores :: !(LA.Vector Double)  -- ^ [日本語]: @p_i = P(T=1|X_i)@、 長さ @n@。 [English]: @p_i = P(T=1|X_i)@, length @n@.
  , psBeta   :: !(LA.Vector Double)  -- ^ [日本語]: logistic coefficients。 [English]: The logistic coefficients.
  , psN      :: !Int                 -- ^ [日本語]: サンプル数。 [English]: The sample count.
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- 推定
-- ---------------------------------------------------------------------------

-- | [日本語]: 共変量行列 @X@ (intercept 列は呼び出し側で付加) と二値処置 @T ∈ {0,1}@
-- から logistic regression で傾向スコアを推定。
--
-- @X@ は @n × p@、 @T@ は長さ @n@ の 0/1 vector。 intercept が欲しい場合は
-- @1@ 列を先頭に prepend して渡す。
--   [English]: Estimates the propensity score via logistic regression from
-- a covariate matrix @X@ (the intercept column must be added by the
-- caller) and a binary treatment @T ∈ {0,1}@.
--
-- @X@ is @n × p@, @T@ is a length-@n@ 0\/1 vector. If an intercept is
-- desired, prepend a @1@ column and pass that in.
propensityScore :: LA.Matrix Double -> LA.Vector Double -> PropensityScore
propensityScore x t =
  let (fit, _) = GLM.fitGLMFull GLM.Binomial GLM.Logit x t
  in PropensityScore
       { psScores = fittedV fit
       , psBeta   = coefficientsV fit
       , psN      = LA.size t
       }

-- | [日本語]: @[lo, hi]@ に clip。 @p_i@ が 0 / 1 に張り付くと IPW 重みが発散する
-- ので必須。 推奨値: @lo = 0.01@, @hi = 0.99@。
--   [English]: Clips to @[lo, hi]@. Necessary because IPW weights diverge
-- when @p_i@ sticks to 0 \/ 1. Recommended values: @lo = 0.01@, @hi = 0.99@.
trimPropensity :: Double -> Double -> PropensityScore -> PropensityScore
trimPropensity lo hi ps =
  ps { psScores = LA.cmap (clamp lo hi) (psScores ps) }
  where
    clamp a b v = max a (min b v)

-- ---------------------------------------------------------------------------
-- 重み (hmatrix Vector 演算)
-- ---------------------------------------------------------------------------

-- | [日本語]: ATE 用の Horvitz-Thompson 重み: @w_i = t_i/p_i + (1-t_i)/(1-p_i)@
--   [English]: The Horvitz-Thompson weight for ATE:
--   @w_i = t_i/p_i + (1-t_i)/(1-p_i)@.
ipwWeights :: PropensityScore -> LA.Vector Double -> LA.Vector Double
ipwWeights ps t =
  let p   = psScores ps
      one = LA.scalar 1
  in t / p + (one - t) / (one - p)

-- | [日本語]: ATT 用の重み: @w_i = t_i + (1-t_i) · p_i/(1-p_i)@
-- (treated は重み 1、 control は odds ratio で再重み付け)
--   [English]: The weight for ATT: @w_i = t_i + (1-t_i) · p_i/(1-p_i)@
-- (treated units get weight 1; control units are reweighted by the odds
-- ratio).
attWeights :: PropensityScore -> LA.Vector Double -> LA.Vector Double
attWeights ps t =
  let p   = psScores ps
      one = LA.scalar 1
  in t + (one - t) * p / (one - p)
