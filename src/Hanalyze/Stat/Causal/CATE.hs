-- |
-- Module      : Hanalyze.Stat.Causal.CATE
-- Description : Künzel et al. (2019) の S/T/X-Learner による CATE meta-learner 実装
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Conditional Average Treatment Effect (CATE) meta-learners (Phase 30-A4)。
--
-- Künzel et al. (2019) の 3 meta-learner を実装:
--
-- - 'SLearner': 単一モデル @μ̂(X, T)@、 @τ̂(X) = μ̂(X, 1) - μ̂(X, 0)@
-- - 'TLearner': 2 モデル @μ̂_1(X)@ / @μ̂_0(X)@、 @τ̂(X) = μ̂_1(X) - μ̂_0(X)@
-- - 'XLearner': T-learner の残差を再帰回帰、 PS で重み付け平均
--
-- base learner は 'CATELM' (= 'Hanalyze.Model.LM') と 'CATERF' (=
-- 'Hanalyze.Model.RandomForest') から選択。 将来 Causal Forest 等を追加する
-- ときは新 constructor を加える。
--
-- ## 使い方
--
-- @
--   gen <- MWC.create
--   r   <- fitCATE TLearner CATELM x t y gen
--   print (cateATE r)   -- average of cateEstimates
-- @
--
-- Reference:
--   Künzel, Sekhon, Bickel, Yu (2019) "Metalearners for estimating
--   heterogeneous treatment effects using machine learning".
--   PNAS 116:4156-4165.
module Hanalyze.Stat.Causal.CATE
  ( CATEBaseLearner (..)
  , CATELearner (..)
  , CATEResult (..)
  , fitCATE
  ) where

import qualified Numeric.LinearAlgebra      as LA
import qualified Data.Vector.Storable       as VS
import qualified Data.Vector.Unboxed        as VU
import qualified Hanalyze.Model.LM          as LM
import qualified Hanalyze.Model.RandomForest as RF
import           Hanalyze.Model.Core         (coefficientsV)
import           Hanalyze.Stat.Causal.PropensityScore
                   (PropensityScore (..), propensityScore, trimPropensity)
import           Hanalyze.Stat.Causal.IPW   (defaultPSTrim)
import qualified System.Random.MWC          as MWC

-- ---------------------------------------------------------------------------
-- 型
-- ---------------------------------------------------------------------------

-- | base learner 選択。 LM は OLS、 RF は Random Forest。
data CATEBaseLearner = CATELM | CATERF RF.RFConfig
  deriving (Show)

-- | meta-learner 選択。
data CATELearner = SLearner | TLearner | XLearner
  deriving (Show, Eq)

data CATEResult = CATEResult
  { cateEstimates :: !(LA.Vector Double)  -- ^ τ̂(X_i) for each unit
  , cateMethod    :: !CATELearner
  , cateATE       :: !Double               -- ^ mean of cateEstimates
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- Base learner abstraction
-- ---------------------------------------------------------------------------

-- | Train a base learner on (X, y) and return a predictor for new X.
-- Random forest path threads through @MWC.GenIO@; LM is pure but is
-- wrapped in @IO@ for uniform signature.
fitPredict :: CATEBaseLearner
           -> LA.Matrix Double -> LA.Vector Double -> MWC.GenIO
           -> IO (LA.Matrix Double -> LA.Vector Double)
fitPredict CATELM x y _ = do
  let beta = coefficientsV (LM.fitLMVec x y)
  pure (\xNew -> LM.predictLMVec beta xNew)
fitPredict (CATERF cfg) x y gen = do
  rf <- RF.fitRFV cfg x (VS.convert y :: VU.Vector Double)
                  gen
  pure (\xNew ->
          let rows = LA.toRows xNew
          in LA.fromList [RF.predictRF rf (LA.toList r) | r <- rows])

-- ---------------------------------------------------------------------------
-- fitCATE
-- ---------------------------------------------------------------------------

fitCATE :: CATELearner -> CATEBaseLearner
        -> LA.Matrix Double -> LA.Vector Double -> LA.Vector Double
        -> MWC.GenIO -> IO CATEResult
fitCATE method base x t y gen = case method of
  SLearner -> sLearner base x t y gen
  TLearner -> tLearner base x t y gen
  XLearner -> xLearner base x t y gen

-- ---------------------------------------------------------------------------
-- S-learner: 単一モデル on (X, T)
-- ---------------------------------------------------------------------------

sLearner :: CATEBaseLearner
         -> LA.Matrix Double -> LA.Vector Double -> LA.Vector Double
         -> MWC.GenIO -> IO CATEResult
sLearner base x t y gen = do
  let xt  = LA.fromBlocks [[x, LA.asColumn t]]
      n   = LA.rows x
      x1  = LA.fromBlocks [[x, LA.asColumn (LA.fromList (replicate n 1))]]
      x0  = LA.fromBlocks [[x, LA.asColumn (LA.fromList (replicate n 0))]]
  predict <- fitPredict base xt y gen
  let mu1 = predict x1
      mu0 = predict x0
      tauHat = mu1 - mu0
  pure CATEResult
    { cateEstimates = tauHat
    , cateMethod    = SLearner
    , cateATE       = LA.sumElements tauHat / fromIntegral n
    }

-- ---------------------------------------------------------------------------
-- T-learner: 2 モデル、 群別 fit
-- ---------------------------------------------------------------------------

tLearner :: CATEBaseLearner
         -> LA.Matrix Double -> LA.Vector Double -> LA.Vector Double
         -> MWC.GenIO -> IO CATEResult
tLearner base x t y gen = do
  let n    = LA.rows x
      idx1 = filterIdx (== 1.0) t
      idx0 = filterIdx (== 0.0) t
      x1   = x LA.? idx1
      y1   = LA.fromList [LA.atIndex y i | i <- idx1]
      x0   = x LA.? idx0
      y0   = LA.fromList [LA.atIndex y i | i <- idx0]
  pred1 <- fitPredict base x1 y1 gen
  pred0 <- fitPredict base x0 y0 gen
  let mu1 = pred1 x
      mu0 = pred0 x
      tauHat = mu1 - mu0
  pure CATEResult
    { cateEstimates = tauHat
    , cateMethod    = TLearner
    , cateATE       = LA.sumElements tauHat / fromIntegral n
    }

-- ---------------------------------------------------------------------------
-- X-learner: 残差再回帰 + PS 重み付け
-- ---------------------------------------------------------------------------

xLearner :: CATEBaseLearner
         -> LA.Matrix Double -> LA.Vector Double -> LA.Vector Double
         -> MWC.GenIO -> IO CATEResult
xLearner base x t y gen = do
  let n    = LA.rows x
      idx1 = filterIdx (== 1.0) t
      idx0 = filterIdx (== 0.0) t
      x1   = x LA.? idx1
      y1   = LA.fromList [LA.atIndex y i | i <- idx1]
      x0   = x LA.? idx0
      y0   = LA.fromList [LA.atIndex y i | i <- idx0]
  -- Step 1: T-learner と同じ outcome models
  pred1 <- fitPredict base x1 y1 gen
  pred0 <- fitPredict base x0 y0 gen
  -- Step 2: imputed treatment effects
  --   For T=1 units: D̃_1 = Y - μ̂_0(X)
  --   For T=0 units: D̃_0 = μ̂_1(X) - Y
  let mu0_at_x1 = pred0 x1
      mu1_at_x0 = pred1 x0
      dTilde1   = y1 - mu0_at_x1
      dTilde0   = mu1_at_x0 - y0
  -- Step 3: τ̂_1(X) を D̃_1 ~ X_{T=1} で fit、 τ̂_0(X) は D̃_0 ~ X_{T=0}
  tau1Pred <- fitPredict base x1 dTilde1 gen
  tau0Pred <- fitPredict base x0 dTilde0 gen
  let tau1At = tau1Pred x
      tau0At = tau0Pred x
  -- Step 4: PS 重み付け平均
  --   τ̂(X) = p̂(X) · τ̂_0(X) + (1 - p̂(X)) · τ̂_1(X)
  --   (treated が少ない領域では τ̂_0 を信頼、 control が少ない領域では τ̂_1)
      (lo, hi) = defaultPSTrim
  let ps     = trimPropensity lo hi (propensityScore x t)
      p      = psScores ps
      one    = LA.scalar 1
      tauHat = p * tau0At + (one - p) * tau1At
  pure CATEResult
    { cateEstimates = tauHat
    , cateMethod    = XLearner
    , cateATE       = LA.sumElements tauHat / fromIntegral n
    }

-- ---------------------------------------------------------------------------
-- ヘルパ
-- ---------------------------------------------------------------------------

filterIdx :: (Double -> Bool) -> LA.Vector Double -> [Int]
filterIdx pr v =
  [ i | i <- [0 .. LA.size v - 1], pr (LA.atIndex v i) ]
