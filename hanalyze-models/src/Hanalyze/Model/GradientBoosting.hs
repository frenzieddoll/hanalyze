{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Hanalyze.Model.GradientBoosting
-- Description : 勾配ブースティング (Gradient Boosting Machine、 回帰 + 二値分類)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Gradient Boosting Machine (回帰 + 二値分類).
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
--   * 回帰: 二乗誤差 (negative gradient = 残差)
--   * 分類 (binary): log-loss (negative gradient = y - sigmoid(F))
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

-- | GBM 設定。
data GBConfig = GBConfig
  { gbNRounds    :: !Int     -- ^ ブースティング回数 M。
  , gbMaxDepth   :: !Int     -- ^ 各弱学習器の最大深さ (典型 3-5)。
  , gbMinSamples :: !Int     -- ^ 葉最小サンプル数。
  , gbLearnRate  :: !Double  -- ^ 学習率 η (typ 0.1)。
  } deriving (Show)

defaultGBM :: GBConfig
defaultGBM = GBConfig
  { gbNRounds    = 100
  , gbMaxDepth   = 3
  , gbMinSamples = 2
  , gbLearnRate  = 0.1
  }

-- | 弱学習器設定 (full-data / 全特徴利用、 木の深さは gbMaxDepth)。
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

-- | 回帰 GBM。 予測 = init + η · Σ tree_m(x).
data GBRegressor = GBRegressor
  { gbrInit  :: !Double
  , gbrTrees :: ![RF.Tree]
  , gbrLR    :: !Double
  } deriving (Show)

fitGBRegressor :: GBConfig
               -> LA.Matrix Double   -- ^ X (n × d)
               -> VU.Vector Double   -- ^ y (n)
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

-- | 1 行を [Double] 化 (predictTree のための一時変換)。
rowList :: LA.Matrix Double -> Int -> [Double]
rowList x i = LA.toList (LA.flatten (x LA.? [i]))

-- | 1 サンプルの予測。
predictGBRRow :: GBRegressor -> [Double] -> Double
predictGBRRow gb xs =
  gbrInit gb
    + gbrLR gb * sum [ RF.predictTree t xs | t <- gbrTrees gb ]

-- | 行列入力に対する予測 (n).
predictGBR :: GBRegressor -> LA.Matrix Double -> VU.Vector Double
predictGBR gb x =
  let !n = LA.rows x
  in VU.generate n (\i -> predictGBRRow gb (rowList x i))

-- ---------------------------------------------------------------------------
-- Classifier (binary)
-- ---------------------------------------------------------------------------

-- | 二値分類 GBM (logit + log-loss)。 ラベルは 0/1。
data GBClassifier = GBClassifier
  { gbcInit  :: !Double          -- ^ logit(p̂_0)
  , gbcTrees :: ![RF.Tree]
  , gbcLR    :: !Double
  } deriving (Show)

sigmoid :: Double -> Double
sigmoid z = 1 / (1 + exp (negate z))

clamp :: Double -> Double -> Double -> Double
clamp lo hi v = max lo (min hi v)

fitGBClassifier :: GBConfig
                -> LA.Matrix Double   -- ^ X (n × d)
                -> VU.Vector Int      -- ^ y ∈ {0,1} (n)
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

-- | クラス確率 p(y=1 | x) を返す。
predictGBCProbs :: GBClassifier -> LA.Matrix Double -> VU.Vector Double
predictGBCProbs gb x =
  let !n = LA.rows x
      logit xs = gbcInit gb
                   + gbcLR gb * sum [ RF.predictTree t xs | t <- gbcTrees gb ]
  in VU.generate n (\i -> sigmoid (logit (rowList x i)))

-- | クラス予測 (閾値 0.5)。
predictGBC :: GBClassifier -> LA.Matrix Double -> VU.Vector Int
predictGBC gb x =
  VU.map (\p -> if p >= 0.5 then 1 else 0) (predictGBCProbs gb x)
