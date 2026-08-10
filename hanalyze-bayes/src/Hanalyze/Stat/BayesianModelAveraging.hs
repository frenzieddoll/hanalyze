{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hanalyze.Stat.BayesianModelAveraging
-- Description : Bridge Sampling の log marginal を用いた真の Bayesian Model Averaging (BMA)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- True Bayesian Model Averaging (BMA) via Bridge Sampling log marginals.
--
-- @
--   p(θ | y) = Σ_k p(θ | y, M_k) · p(M_k | y)
--   p(M_k | y) ∝ p(y | M_k) · p(M_k)
-- @
--
-- 入力: 各モデルの **Bridge Sampling 推定 log marginal likelihood** +
-- prior model weights (省略時 uniform 1/K)。
--
-- 出力: posterior model weights + 重み付き予測の helper。
--
-- ## 既存 pseudo-BMA との位置付け
--
-- 既存 'Hanalyze.Stat.ModelSelect' の pseudo-BMA (= PSIS-LOO ベース近似) は
-- 軽量だが marginal likelihood を正しく計算していない (= LOO予測精度を代理
-- 指標として使う)。 本 module は Bridge Sampling 経由で **真の log marginal**
-- を使う BMA で、 解釈が一貫している (= Bayes Factor / 仮説検定と同じ基盤)。
--
-- Reference: Hoeting, Madigan, Raftery, Volinsky (1999) "Bayesian Model
-- Averaging: A Tutorial". Statistical Science 14(4):382-417.
module Hanalyze.Stat.BayesianModelAveraging
  ( BMAResult (..)
  , bayesianModelAveraging
  , averagePredictions
  ) where

import qualified Numeric.LinearAlgebra    as LA

-- ---------------------------------------------------------------------------
-- BMA
-- ---------------------------------------------------------------------------

data BMAResult = BMAResult
  { bmaWeights      :: ![Double]   -- ^ posterior model weights @p(M_k|y)@、 Σ = 1
  , bmaLogMarginals :: ![Double]   -- ^ 入力された per-model @log p(y|M_k)@ (引き継ぎ)
  , bmaLogPriors    :: ![Double]   -- ^ 入力された per-model @log p(M_k)@ (引き継ぎ)
  } deriving (Show)

-- | log marginal + log prior weights (省略時 uniform) から posterior model
-- weights を計算 (softmax 安定化)。
--
-- @
--   p(M_k | y) ∝ exp(log p(y|M_k) + log p(M_k))
-- @
--
-- 引数の長さは同じである必要 (異なる場合は短い方に合わせる)。 全 log
-- marginal が -∞ なら uniform fallback。
bayesianModelAveraging
  :: [Double]          -- ^ log marginals (Bridge Sampling 推定値 等)
  -> Maybe [Double]    -- ^ optional log prior weights (Nothing = uniform)
  -> BMAResult
bayesianModelAveraging logMs mPriors =
  let k = length logMs
      logPriors = case mPriors of
        Just ps | length ps == k -> ps
        _                        -> replicate k (- log (fromIntegral k))
      logUnnorm = zipWith (+) logMs logPriors
      ws = if all isInfinite logUnnorm
             then replicate k (1 / fromIntegral k)   -- fallback uniform
             else
               let m  = maximum logUnnorm
                   es = map (\x -> exp (x - m)) logUnnorm
                   s  = sum es
               in if s == 0 then replicate k (1 / fromIntegral k)
                            else map (/ s) es
  in BMAResult
       { bmaWeights      = ws
       , bmaLogMarginals = logMs
       , bmaLogPriors    = logPriors
       }

-- | per-model 予測ベクトル (= 各モデルから出した y* の posterior mean 等) を
-- BMA weights で加重平均。 全ベクトルは同じ長さである必要。
averagePredictions :: BMAResult -> [LA.Vector Double] -> LA.Vector Double
averagePredictions bma preds
  | null preds = LA.fromList []
  | length preds /= length (bmaWeights bma) =
      error "averagePredictions: number of predictions ≠ number of weights"
  | otherwise =
      foldr1 (+) [ LA.scale w v | (w, v) <- zip (bmaWeights bma) preds ]
