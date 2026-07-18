-- |
-- Module      : Hanalyze.Stat.Causal.DoublyRobust
-- Description : Doubly Robust / Augmented IPW (AIPW) 推定量
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Doubly Robust / Augmented IPW (AIPW) 推定量 (Phase 30-A3)。
--
-- 結果モデル @μ̂_1(X)@ / @μ̂_0(X)@ と傾向スコア @p̂(X)@ の両方を使い、
-- どちらか一方が正しく指定されていれば一致性を持つ推定量:
--
-- @
--   ATE_AIPW = (1/n) Σ [ μ̂_1(X_i) - μ̂_0(X_i)
--                       + T_i (Y_i - μ̂_1(X_i)) / p̂_i
--                       - (1-T_i) (Y_i - μ̂_0(X_i)) / (1 - p̂_i) ]
-- @
--
-- 結果モデルは 'Hanalyze.Model.LM.fitLM' を流用 (= OLS、 線形)。 非線形が
-- 必要な場合は呼び出し側で X を拡張するか CATE module (30-A4) を使う。
--
-- Reference:
--   Robins, Rotnitzky, Zhao (1994) "Estimation of Regression Coefficients
--   When Some Regressors Are Not Always Observed". JASA 89:846-866.
module Hanalyze.Stat.Causal.DoublyRobust
  ( DoublyRobustResult (..)
  , doublyRobust
  , doublyRobustWith
  ) where

import qualified Numeric.LinearAlgebra as LA
import qualified Hanalyze.Model.LM    as LM
import           Hanalyze.Model.Core   (coefficientsV)
import           Hanalyze.Stat.Causal.PropensityScore
                   (PropensityScore (..), propensityScore, trimPropensity)
import           Hanalyze.Stat.Causal.IPW (defaultPSTrim)

-- ---------------------------------------------------------------------------
-- 型
-- ---------------------------------------------------------------------------

data DoublyRobustResult = DoublyRobustResult
  { drATE          :: !Double
  , drMu1Predicted :: !(LA.Vector Double)  -- ^ μ̂_1(X_i) for all i
  , drMu0Predicted :: !(LA.Vector Double)  -- ^ μ̂_0(X_i) for all i
  , drPropensity   :: !PropensityScore
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- AIPW
-- ---------------------------------------------------------------------------

-- | 共変量 @X@ (intercept 列を含む)、 二値処置 @T@、 結果 @Y@ から AIPW ATE
-- を推定。 内部で 'propensityScore' + 'defaultPSTrim' を適用、 outcome
-- model は OLS で群別 fit。
doublyRobust :: LA.Matrix Double -> LA.Vector Double -> LA.Vector Double
             -> DoublyRobustResult
doublyRobust x t y =
  let (lo, hi) = defaultPSTrim
      ps       = trimPropensity lo hi (propensityScore x t)
  in doublyRobustWith ps x t y

-- | 既存 PS を再利用する版。 PS と outcome model の組み合わせを変えて
-- 二重ロバスト性を検証したい場合に有用。
doublyRobustWith :: PropensityScore -> LA.Matrix Double -> LA.Vector Double
                 -> LA.Vector Double -> DoublyRobustResult
doublyRobustWith ps x t y =
  let n     = fromIntegral (LA.size t) :: Double
      one   = LA.scalar 1
      p     = psScores ps
      -- 群別 OLS: T=1 部分集合 / T=0 部分集合
      idx1 = filterIdx (== 1.0) t
      idx0 = filterIdx (== 0.0) t
      x1   = x LA.? idx1
      y1   = LA.fromList [LA.atIndex y i | i <- idx1]
      x0   = x LA.? idx0
      y0   = LA.fromList [LA.atIndex y i | i <- idx0]
      beta1 = coefficientsV (LM.fitLMVec x1 y1)
      beta0 = coefficientsV (LM.fitLMVec x0 y0)
      mu1   = LM.predictLMVec beta1 x
      mu0   = LM.predictLMVec beta0 x
      -- AIPW contribution per unit
      contrib = (mu1 - mu0)
              + t * (y - mu1) / p
              - (one - t) * (y - mu0) / (one - p)
      ateHat = LA.sumElements contrib / n
  in DoublyRobustResult
       { drATE          = ateHat
       , drMu1Predicted = mu1
       , drMu0Predicted = mu0
       , drPropensity   = ps
       }

-- ---------------------------------------------------------------------------
-- ヘルパ
-- ---------------------------------------------------------------------------

filterIdx :: (Double -> Bool) -> LA.Vector Double -> [Int]
filterIdx pr v =
  [ i | i <- [0 .. LA.size v - 1], pr (LA.atIndex v i) ]
