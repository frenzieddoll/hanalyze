{-# LANGUAGE OverloadedStrings #-}
-- | MCMC によるモデル比較指標。
--
-- WAIC (Widely Applicable Information Criterion) と
-- PSIS-LOO (Pareto Smoothed Importance Sampling LOO-CV) を提供する。
--
-- 参考文献:
--   Watanabe (2010) WAIC
--   Vehtari, Gelman, Gabry (2017) PSIS-LOO
--   Hosking-Wallis (1987) Pareto moment estimator
--
-- @
-- let logLikMat = chainLogLikMatrix model chain  -- [[Double]]
-- print (waic logLikMat)
-- print (loo  logLikMat)
-- @
module Stat.ModelSelect
  ( -- * WAIC
    WAICResult (..)
  , waic
  , chainWAIC
    -- * LOO-CV (PSIS)
  , LOOResult (..)
  , loo
  , chainLOO
    -- * ユーティリティ
  , chainLogLikMatrix
  ) where

import Data.List (sort, transpose)

import Model.HBM  (Model, perObsLogLiks)
import MCMC.Core  (Chain, chainSamples)

-- ---------------------------------------------------------------------------
-- 結果型
-- ---------------------------------------------------------------------------

-- | WAIC の計算結果。
data WAICResult = WAICResult
  { waicValue :: Double  -- ^ WAIC = −2(lppd − p_waic)、小さいほど良い
  , waicLppd  :: Double  -- ^ log pointwise predictive density
  , waicPwaic :: Double  -- ^ 有効パラメータ数 p_waic
  , waicSE    :: Double  -- ^ WAIC の推定標準誤差
  } deriving (Show)

-- | PSIS-LOO の計算結果。
data LOOResult = LOOResult
  { looValue   :: Double    -- ^ −2 × elpd_loo、小さいほど良い
  , looElpd    :: Double    -- ^ Σᵢ elpd_i (期待 log 予測密度)
  , looSE      :: Double    -- ^ 推定標準誤差
  , looKHat    :: [Double]  -- ^ 観測値ごとの Pareto k̂
                            --   k̂ < 0.5 → 良好, 0.5–0.7 → 許容, > 0.7 → 要注意
  , looKHatBad :: Int       -- ^ k̂ > 0.7 の観測値数
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- WAIC
-- ---------------------------------------------------------------------------

-- | WAIC を対数尤度行列から計算する。
--
-- logLikMat !! s !! i = log p(y_i | θ^s)  (行 = S サンプル、列 = N 観測値)
waic :: [[Double]] -> WAICResult
waic [] = WAICResult 0 0 0 0
waic logLikMat =
  let s     = fromIntegral (length logLikMat) :: Double
      cols  = transpose logLikMat   -- N 列それぞれに S 個の値
      n     = length cols

      -- lppd_i = log(1/S × Σ_s p(y_i|θ^s))
      --        = logSumExp(ll_{i,1..S}) − log S
      lppd_i  = map (\col -> logSumExp col - log s) cols
      lppd    = sum lppd_i

      -- p_waic_i = Var_s[log p(y_i|θ^s)]  (標本分散)
      pwaic_i = map sampleVar cols
      pwaic   = sum pwaic_i

      waicVal = -2 * (lppd - pwaic)

      -- 観測値ごとの WAIC 寄与から SE を推定
      contrib = zipWith (\l p -> -2 * (l - p)) lppd_i pwaic_i
      se      = sqrt (fromIntegral n * sampleVar contrib)

  in WAICResult waicVal lppd pwaic se

-- ---------------------------------------------------------------------------
-- LOO-CV (PSIS)
-- ---------------------------------------------------------------------------

-- | PSIS-LOO を対数尤度行列から計算する。
--
-- 各観測値について、重要度重みを Pareto 分布で平滑化した後に
-- 截頭 IS による LOO 推定値と Pareto k̂ 診断量を返す。
loo :: [[Double]] -> LOOResult
loo [] = LOOResult 0 0 0 [] 0
loo logLikMat =
  let s       = length logLikMat
      cols    = transpose logLikMat
      n       = length cols
      results = map (psisElpd s) cols
      elpd_i  = map fst results
      khat_i  = map snd results
      elpd    = sum elpd_i
      looVal  = -2 * elpd
      se      = sqrt (fromIntegral n * sampleVar elpd_i)
      nBad    = length (filter (> 0.7) khat_i)
  in LOOResult looVal elpd se khat_i nBad

-- | 1 観測値に対する PSIS 推定: (elpd_i, k̂_i)
--
-- アルゴリズム:
-- 1. 対数重要度重み log r_i^s = −log p(y_i|θ^s) を計算
-- 2. 上位 M = min(S/5, 3√S) 値から Pareto k̂ を推定
-- 3. 重みを √S で截頭して安定化し、正規化
-- 4. elpd_i = logSumExp(log W_s + log p(y_i|θ^s))
psisElpd :: Int -> [Double] -> (Double, Double)
psisElpd s colLL =
  let -- 対数重要度重み (正規化前): log r_i^s = −log p(y_i|θ^s)
      logR = map negate colLL

      -- Pareto k̂: 上位 M 個から推定
      m          = max 5 (min (s `div` 5) (floor (3 * sqrt (fromIntegral s :: Double))))
      sortedLogR = sort logR              -- 昇順
      topM       = drop (s - m) sortedLogR   -- 最大 m 個 (昇順のまま)
      khat       = paretoKhat topM

      -- 截頭 IS: 各対数重みを log(√S) でクリップ
      logCap  = 0.5 * log (fromIntegral s :: Double)
      capped  = map (min logCap) logR
      logZ    = logSumExp capped
      logW    = map (\r -> r - logZ) capped   -- 正規化対数重み

      -- elpd_i = log(Σ_s W_s × p(y_i|θ^s)) = logSumExp(logW + colLL)
      elpdi   = logSumExp (zipWith (+) logW colLL)

  in (elpdi, khat)

-- | 上位 M 個の対数重み (昇順) から Pareto 形状パラメータ k̂ を推定する。
--
-- Hosking-Wallis (1987) モーメント推定量:
--   excess = exp(r − u) − 1  (u = 下限閾値)
--   k̂ = 0.5 × (1 − μ² / s²)  where μ = mean(excess), s² = Var(excess)
paretoKhat :: [Double] -> Double
paretoKhat topM
  | length topM < 5 = 0
  | otherwise =
    let u      = head topM            -- 閾値 (最小値)
        excess = map (\r -> exp (r - u) - 1) topM
        mu     = mean excess
        var    = sampleVar excess
    in if var <= 0 || mu <= 0 then 0
       else 0.5 * (1 - mu ^ (2::Int) / var)

-- ---------------------------------------------------------------------------
-- Chain との連携
-- ---------------------------------------------------------------------------

-- | モデルとチェーンから対数尤度行列を生成する。
-- 行 = サンプル (バーンイン後)、列 = 観測値。
chainLogLikMatrix :: Model a -> Chain -> [[Double]]
chainLogLikMatrix model chain = map (perObsLogLiks model) (chainSamples chain)

-- | チェーンから WAIC を直接計算する。
chainWAIC :: Model a -> Chain -> WAICResult
chainWAIC model = waic . chainLogLikMatrix model

-- | チェーンから PSIS-LOO を直接計算する。
chainLOO :: Model a -> Chain -> LOOResult
chainLOO model = loo . chainLogLikMatrix model

-- ---------------------------------------------------------------------------
-- 数値ユーティリティ
-- ---------------------------------------------------------------------------

logSumExp :: [Double] -> Double
logSumExp [] = -1/0
logSumExp xs =
  let m = maximum xs
  in m + log (sum (map (\x -> exp (x - m)) xs))

mean :: [Double] -> Double
mean [] = 0
mean xs = sum xs / fromIntegral (length xs)

-- | 標本分散 (n-1 で割る)
sampleVar :: [Double] -> Double
sampleVar xs
  | length xs < 2 = 0
  | otherwise =
      let mu = mean xs
      in sum (map (\x -> (x - mu) ^ (2::Int)) xs)
         / fromIntegral (length xs - 1)
