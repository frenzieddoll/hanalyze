-- |
-- Module      : Hanalyze.Stat.Causal.IPW
-- Description : Inverse Probability Weighting (IPW) による ATE / ATT 推定
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Inverse Probability Weighting (IPW) による ATE / ATT 推定 (Phase 30-A2)。
--
-- Hajek 正規化推定量 (finite-sample で stable、 Horvitz-Thompson より低分散):
--
-- @
--   ATE = Σ(T·Y/p) / Σ(T/p)  -  Σ((1-T)·Y/(1-p)) / Σ((1-T)/(1-p))
--   ATT = Σ(T·Y) / Σ T       -  Σ((1-T)·(p/(1-p))·Y) / Σ((1-T)·(p/(1-p)))
-- @
--
-- ここで @p_i@ は 'PropensityScore' で推定した P(T=1 | X_i)。 重みは
-- @PropensityScore.ipwWeights@ / @attWeights@ で hmatrix Vector 演算で計算。
--
-- ## 使い方
--
-- @
--   let r = ipw xConf treat outcome           -- 共変量から PS 推定 + trim も内部で実施
--   print (ipwATE r, ipwATT r)
--
--   -- 既に PS を計算済 / カスタム trim したい場合:
--   let ps' = trimPropensity 0.05 0.95 (propensityScore x t)
--       r'  = ipwWith ps' t y
-- @
--
-- Reference:
--   Horvitz & Thompson (1952) "A Generalization of Sampling Without
--   Replacement from a Finite Universe". JASA 47:663-685.
module Hanalyze.Stat.Causal.IPW
  ( IPWResult (..)
  , ipw
  , ipwWith
  , defaultPSTrim
  ) where

import qualified Numeric.LinearAlgebra            as LA
import           Hanalyze.Stat.Causal.PropensityScore
                   (PropensityScore (..), propensityScore, trimPropensity,
                    ipwWeights, attWeights)

-- ---------------------------------------------------------------------------
-- 型
-- ---------------------------------------------------------------------------

data IPWResult = IPWResult
  { ipwATE        :: !Double
  , ipwATT        :: !Double
  , ipwWeightsATE :: !(LA.Vector Double)
  , ipwWeightsATT :: !(LA.Vector Double)
  , ipwPropensity :: !PropensityScore
  } deriving (Show)

-- | 既定の PS trim 範囲 @(0.01, 0.99)@ (Rosenbaum 慣例)。
defaultPSTrim :: (Double, Double)
defaultPSTrim = (0.01, 0.99)

-- ---------------------------------------------------------------------------
-- 推定
-- ---------------------------------------------------------------------------

-- | 共変量 @X@、 二値処置 @T@、 結果 @Y@ から ATE / ATT を IPW で推定。
-- 内部で 'propensityScore' + 'defaultPSTrim' を適用。
ipw :: LA.Matrix Double -> LA.Vector Double -> LA.Vector Double -> IPWResult
ipw x t y =
  let (lo, hi) = defaultPSTrim
      ps       = trimPropensity lo hi (propensityScore x t)
  in ipwWith ps t y

-- | 既に算出 (+trim) 済の PropensityScore を再利用する版。 同じ X から
-- ATE / ATT を複数バリアントで比べたい場合に有用。
ipwWith :: PropensityScore -> LA.Vector Double -> LA.Vector Double -> IPWResult
ipwWith ps t y =
  let p     = psScores ps
      one   = LA.scalar 1
      wATE  = ipwWeights ps t
      wATT  = attWeights ps t
      -- ATE (Hajek 正規化): 各群の重み付き平均の差
      --   μ̂_1 = Σ (T/p)·Y  /  Σ (T/p)
      --   μ̂_0 = Σ ((1-T)/(1-p))·Y / Σ ((1-T)/(1-p))
      w1     = t / p
      w0     = (one - t) / (one - p)
      mu1Hat = safeDiv (LA.sumElements (w1 * y)) (LA.sumElements w1)
      mu0Hat = safeDiv (LA.sumElements (w0 * y)) (LA.sumElements w0)
      ateHat = mu1Hat - mu0Hat
      -- ATT (Hajek 正規化): treated 平均と、 p/(1-p) で再重み付けした control 平均の差
      wt1    = t                       -- treated indicator
      wt0    = (one - t) * (p / (one - p))
      attMu1 = safeDiv (LA.sumElements (wt1 * y)) (LA.sumElements wt1)
      attMu0 = safeDiv (LA.sumElements (wt0 * y)) (LA.sumElements wt0)
      attHat = attMu1 - attMu0
  in IPWResult
       { ipwATE        = ateHat
       , ipwATT        = attHat
       , ipwWeightsATE = wATE
       , ipwWeightsATT = wATT
       , ipwPropensity = ps
       }
  where
    safeDiv num den = if abs den < 1e-12 then 0 else num / den
