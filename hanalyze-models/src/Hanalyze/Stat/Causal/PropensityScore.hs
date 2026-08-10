-- |
-- Module      : Hanalyze.Stat.Causal.PropensityScore
-- Description : logistic regression による Propensity Score P(T=1|X) の推定
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Propensity Score の推定 (Phase 30-A1)。
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
  { psScores :: !(LA.Vector Double)  -- ^ @p_i = P(T=1|X_i)@、 長さ @n@
  , psBeta   :: !(LA.Vector Double)  -- ^ logistic coefficients
  , psN      :: !Int                 -- ^ サンプル数
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- 推定
-- ---------------------------------------------------------------------------

-- | 共変量行列 @X@ (intercept 列は呼び出し側で付加) と二値処置 @T ∈ {0,1}@
-- から logistic regression で傾向スコアを推定。
--
-- @X@ は @n × p@、 @T@ は長さ @n@ の 0/1 vector。 intercept が欲しい場合は
-- @1@ 列を先頭に prepend して渡す。
propensityScore :: LA.Matrix Double -> LA.Vector Double -> PropensityScore
propensityScore x t =
  let (fit, _) = GLM.fitGLMFull GLM.Binomial GLM.Logit x t
  in PropensityScore
       { psScores = fittedV fit
       , psBeta   = coefficientsV fit
       , psN      = LA.size t
       }

-- | @[lo, hi]@ に clip。 @p_i@ が 0 / 1 に張り付くと IPW 重みが発散する
-- ので必須。 推奨値: @lo = 0.01@, @hi = 0.99@。
trimPropensity :: Double -> Double -> PropensityScore -> PropensityScore
trimPropensity lo hi ps =
  ps { psScores = LA.cmap (clamp lo hi) (psScores ps) }
  where
    clamp a b v = max a (min b v)

-- ---------------------------------------------------------------------------
-- 重み (hmatrix Vector 演算)
-- ---------------------------------------------------------------------------

-- | ATE 用の Horvitz-Thompson 重み: @w_i = t_i/p_i + (1-t_i)/(1-p_i)@
ipwWeights :: PropensityScore -> LA.Vector Double -> LA.Vector Double
ipwWeights ps t =
  let p   = psScores ps
      one = LA.scalar 1
  in t / p + (one - t) / (one - p)

-- | ATT 用の重み: @w_i = t_i + (1-t_i) · p_i/(1-p_i)@
-- (treated は重み 1、 control は odds ratio で再重み付け)
attWeights :: PropensityScore -> LA.Vector Double -> LA.Vector Double
attWeights ps t =
  let p   = psScores ps
      one = LA.scalar 1
  in t + (one - t) * p / (one - p)
