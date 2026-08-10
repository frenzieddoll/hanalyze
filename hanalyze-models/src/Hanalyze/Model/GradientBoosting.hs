{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Hanalyze.Model.GradientBoosting
-- Description : 勾配ブースティング (Gradient Boosting Machine、 回帰 + 二値分類)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: Gradient Boosting Machine (回帰 + 二値分類)。
--
-- 弱学習器は 'Hanalyze.Model.RandomForest' の回帰木 ('RF.Tree' /
-- 'RF.buildTreeV') を流用 (bootstrap 無 + mtry = d で full-data /
-- 全特徴を使う通常の GBM 木に縮約)。
--
-- @
-- import qualified Hanalyze.Model.GradientBoosting as GB
-- gb <- GB.fitGBRegressor GB.defaultGBM x y
-- let yhat = GB.predictGBR gb x
-- @
--
-- 損失:
--
--   - 回帰: 二乗誤差 (negative gradient = 残差)
--   - 分類 (binary): log-loss (negative gradient = y - sigmoid(F))
--
-- [English]: Gradient Boosting Machine (regression + binary classification).
--
-- The weak learner reuses the regression tree from
-- 'Hanalyze.Model.RandomForest' ('RF.Tree' \/ 'RF.buildTreeV'),
-- reduced to an ordinary GBM tree that uses full data \/ all features (no
-- bootstrap, mtry = d).
--
-- @
-- import qualified Hanalyze.Model.GradientBoosting as GB
-- gb <- GB.fitGBRegressor GB.defaultGBM x y
-- let yhat = GB.predictGBR gb x
-- @
--
-- Loss:
--
--   - Regression: squared error (negative gradient = the residual).
--   - Classification (binary): log-loss (negative gradient =
--     y - sigmoid(F)).
module Hanalyze.Model.GradientBoosting
  ( GBConfig (..)
  , defaultGBM
  , GBRegressor (..)
  , GBClassifier (..)
  , fitGBRegressor
  , fitGBClassifier
  , predictGBR
  , predictGBRRow
  , predictGBC
  , predictGBCProbs
  ) where

import qualified Data.Vector.Unboxed   as VU
import qualified Numeric.LinearAlgebra as LA
import qualified Hanalyze.Model.RandomForest as RF

-- ---------------------------------------------------------------------------
-- Config
-- ---------------------------------------------------------------------------

-- | [日本語]: GBM 設定。
--   [English]: The GBM configuration.
data GBConfig = GBConfig
  { gbNRounds    :: !Int     -- ^ [日本語]: ブースティング回数 M。 [English]: The number of boosting rounds M.
  , gbMaxDepth   :: !Int     -- ^ [日本語]: 各弱学習器の最大深さ (典型 3-5)。 [English]: The maximum depth of each weak learner (typically 3-5).
  , gbMinSamples :: !Int     -- ^ [日本語]: 葉最小サンプル数。 [English]: The minimum number of samples per leaf.
  , gbLearnRate  :: !Double  -- ^ [日本語]: 学習率 η (typ 0.1)。 [English]: The learning rate η (typically 0.1).
  } deriving (Show)

defaultGBM :: GBConfig
defaultGBM = GBConfig
  { gbNRounds    = 100
  , gbMaxDepth   = 3
  , gbMinSamples = 2
  , gbLearnRate  = 0.1
  }

-- | [日本語]: 弱学習器設定 (full-data / 全特徴利用、 木の深さは gbMaxDepth)。
--   [English]: The weak-learner configuration (uses full data \/ all
--   features; tree depth is gbMaxDepth).
weakRFCfg :: Int -> GBConfig -> RF.RFConfig
weakRFCfg d cfg = RF.RFConfig
  { RF.rfTrees      = 1
  , RF.rfMaxDepth   = gbMaxDepth cfg
  , RF.rfMinSamples = gbMinSamples cfg
  , RF.rfMtry       = Just d
  , RF.rfBootstrap  = False
  }

-- ---------------------------------------------------------------------------
-- Regressor
-- ---------------------------------------------------------------------------

-- | [日本語]: 回帰 GBM。 予測 = init + η · Σ tree_m(x)。
--   [English]: A regression GBM. Prediction = init + η · Σ tree_m(x).
data GBRegressor = GBRegressor
  { gbrInit  :: !Double
  , gbrTrees :: ![RF.Tree]
  , gbrLR    :: !Double
  } deriving (Show)

fitGBRegressor :: GBConfig
               -> LA.Matrix Double   -- ^ [日本語]: X (n × d)。 [English]: X (n × d).
               -> VU.Vector Double   -- ^ [日本語]: y (n)。 [English]: y (n).
               -> GBRegressor
fitGBRegressor cfg x y =
  let !n     = VU.length y
      !d     = LA.cols x
      !cfgW  = weakRFCfg d cfg
      !lr    = gbLearnRate cfg
      !f0    = VU.sum y / fromIntegral n
      !preds0 = VU.replicate n f0
      idx    = VU.enumFromN 0 n

      step (!preds, !trees) _ =
        let !res = VU.zipWith (-) y preds
            !t   = RF.buildTreeV cfgW x res idx 0
            !upd = VU.map (\i -> lr * RF.predictTree t (rowList x i))
                          (VU.enumFromN 0 n)
            !preds' = VU.zipWith (+) preds upd
        in (preds', t : trees)

      (_, treesRev) = foldl step (preds0, []) [1 .. gbNRounds cfg]
  in GBRegressor f0 (reverse treesRev) lr

-- | [日本語]: 1 行を [Double] 化 (predictTree のための一時変換)。
--   [English]: Converts a single row to [Double] (a temporary conversion
--   for predictTree).
rowList :: LA.Matrix Double -> Int -> [Double]
rowList x i = LA.toList (LA.flatten (x LA.? [i]))

-- | [日本語]: 1 サンプルの予測。
--   [English]: Predicts a single sample.
predictGBRRow :: GBRegressor -> [Double] -> Double
predictGBRRow gb xs =
  gbrInit gb
    + gbrLR gb * sum [ RF.predictTree t xs | t <- gbrTrees gb ]

-- | [日本語]: 行列入力に対する予測 (n)。
--   [English]: Predicts for matrix input (n).
predictGBR :: GBRegressor -> LA.Matrix Double -> VU.Vector Double
predictGBR gb x =
  let !n = LA.rows x
  in VU.generate n (\i -> predictGBRRow gb (rowList x i))

-- ---------------------------------------------------------------------------
-- Classifier (binary)
-- ---------------------------------------------------------------------------

-- | [日本語]: 二値分類 GBM (logit + log-loss)。 ラベルは 0/1。
--   [English]: A binary-classification GBM (logit + log-loss). Labels are
--   0\/1.
data GBClassifier = GBClassifier
  { gbcInit  :: !Double          -- ^ [日本語]: logit(p̂_0)。 [English]: logit(p̂_0).
  , gbcTrees :: ![RF.Tree]
  , gbcLR    :: !Double
  } deriving (Show)

sigmoid :: Double -> Double
sigmoid z = 1 / (1 + exp (negate z))

clamp :: Double -> Double -> Double -> Double
clamp lo hi v = max lo (min hi v)

fitGBClassifier :: GBConfig
                -> LA.Matrix Double   -- ^ [日本語]: X (n × d)。 [English]: X (n × d).
                -> VU.Vector Int      -- ^ [日本語]: y ∈ {0,1} (n)。 [English]: y ∈ {0,1} (n).
                -> GBClassifier
fitGBClassifier cfg x y =
  let !n    = VU.length y
      !d    = LA.cols x
      !cfgW = weakRFCfg d cfg
      !lr   = gbLearnRate cfg
      !yD   = VU.map fromIntegral y :: VU.Vector Double
      !p0   = clamp 1e-6 (1 - 1e-6) (VU.sum yD / fromIntegral n)
      !f0   = log (p0 / (1 - p0))
      !logits0 = VU.replicate n f0
      idx   = VU.enumFromN 0 n

      step (!logits, !trees) _ =
        let !grad = VU.zipWith (\yi z -> yi - sigmoid z) yD logits
            !t    = RF.buildTreeV cfgW x grad idx 0
            !upd  = VU.map (\i -> lr * RF.predictTree t (rowList x i))
                           (VU.enumFromN 0 n)
            !logits' = VU.zipWith (+) logits upd
        in (logits', t : trees)

      (_, treesRev) = foldl step (logits0, []) [1 .. gbNRounds cfg]
  in GBClassifier f0 (reverse treesRev) lr

-- | [日本語]: クラス確率 p(y=1 | x) を返す。
--   [English]: Returns the class probability p(y=1 | x).
predictGBCProbs :: GBClassifier -> LA.Matrix Double -> VU.Vector Double
predictGBCProbs gb x =
  let !n = LA.rows x
      logit xs = gbcInit gb
                   + gbcLR gb * sum [ RF.predictTree t xs | t <- gbcTrees gb ]
  in VU.generate n (\i -> sigmoid (logit (rowList x i)))

-- | [日本語]: クラス予測 (閾値 0.5)。
--   [English]: Predicts the class (threshold 0.5).
predictGBC :: GBClassifier -> LA.Matrix Double -> VU.Vector Int
predictGBC gb x =
  VU.map (\p -> if p >= 0.5 then 1 else 0) (predictGBCProbs gb x)
