{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | MCMC-based model comparison criteria.
--
-- Provides WAIC (Widely Applicable Information Criterion) and PSIS-LOO
-- (Pareto-Smoothed Importance Sampling LOO-CV), plus a @pm.compare@-style
-- weighting facility (pseudo-BMA / stacking).
--
-- References:
--
-- * Watanabe (2010) — WAIC.
-- * Vehtari, Gelman, Gabry (2017) — PSIS-LOO.
-- * Hosking & Wallis (1987) — generalized Pareto moment estimator.
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
    -- * LM / GLM 事後サンプリング (WAIC/LOO-CV 用)
  , lmPosteriorLogLiks
  , glmPosteriorLogLiks
  , lmePosteriorLogLiks
    -- * モデル比較の重み (PyMC `pm.compare` 相当)
  , CompareEntry (..)
  , CompareResult (..)
  , compareModels
  ) where

import Control.Monad (replicateM)
import Data.List (sort, transpose)
import qualified Numeric.LinearAlgebra as LA
import System.Random.MWC (GenIO)
import System.Random.MWC.Distributions (normal)

import Model.Core (FitResult (..), coefficientsV, residualsV)
import Model.GLM  (Family (..), LinkFn (..))
import Model.HBM  (ModelP, perObsLogLiks)
import MCMC.Core  (Chain, chainSamples)
import qualified Stat.Distribution as Dist

-- ---------------------------------------------------------------------------
-- 結果型
-- ---------------------------------------------------------------------------

-- | WAIC result.
data WAICResult = WAICResult
  { waicValue :: Double  -- ^ @WAIC = −2(lppd − p_waic)@; smaller is better.
  , waicLppd  :: Double  -- ^ Log pointwise predictive density.
  , waicPwaic :: Double  -- ^ Effective number of parameters @p_waic@.
  , waicSE    :: Double  -- ^ Estimated standard error of @WAIC@.
  } deriving (Show)

-- | PSIS-LOO result.
data LOOResult = LOOResult
  { looValue   :: Double    -- ^ @−2 × elpd_loo@; smaller is better.
  , looElpd    :: Double    -- ^ @Σᵢ elpd_i@ (expected log predictive density).
  , looSE      :: Double    -- ^ Standard error of @looValue@.
  , looKHat    :: [Double]  -- ^ Per-observation Pareto @k̂@; @< 0.5@ good,
                            --   @0.5–0.7@ acceptable, @> 0.7@ flag.
  , looKHatBad :: Int       -- ^ Number of observations with @k̂ > 0.7@.
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- WAIC
-- ---------------------------------------------------------------------------

-- | Compute WAIC from a log-likelihood matrix.
--
-- @logLikMat !! s !! i = log p(y_i | θ^s)@: rows are @S@ posterior
-- samples, columns are @N@ observations.
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

-- | Compute PSIS-LOO from a log-likelihood matrix.
--
-- For each observation, importance weights are smoothed by a Pareto
-- distribution; this returns the truncated-IS LOO estimate together with
-- the diagnostic Pareto @k̂@.
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

-- | PSIS estimate for a single observation: @(elpd_i, k̂_i)@.
--
-- Algorithm:
--
--   1. Compute log importance weights @log r_i^s = −log p(y_i|θ^s)@.
--   2. Fit a Pareto @k̂@ to the top @M = min(S/5, 3√S)@ values.
--   3. Truncate weights at @log √S@ and renormalize for stability.
--   4. @elpd_i = logSumExp(log W_s + log p(y_i|θ^s))@.
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

-- | Estimate the Pareto shape @k̂@ from the top-@M@ log-weights
-- (ascending).
--
-- Uses the Hosking-Wallis (1987) moment estimator:
--
-- @
-- excess = exp(r − u) − 1   (u = lower threshold)
-- k̂      = 0.5 × (1 − μ² / s²)   where  μ = mean excess, s² = Var excess
-- @
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

-- | Build a log-likelihood matrix from a model and a chain.
-- Rows are post-burnin samples, columns are observations.
chainLogLikMatrix :: ModelP r -> Chain -> [[Double]]
chainLogLikMatrix model chain = map (perObsLogLiks model) (chainSamples chain)

-- | Compute WAIC directly from a model and chain.
chainWAIC :: ModelP r -> Chain -> WAICResult
chainWAIC model = waic . chainLogLikMatrix model

-- | Compute PSIS-LOO directly from a model and chain.
chainLOO :: ModelP r -> Chain -> LOOResult
chainLOO model = loo . chainLogLikMatrix model

-- ---------------------------------------------------------------------------
-- LM / GLM 事後サンプリング (WAIC/LOO-CV 用)
-- ---------------------------------------------------------------------------

-- | Generate an @S × N@ log-likelihood matrix from a flat-prior LM
-- posterior.
--
-- Sampling scheme:
--
-- @
-- σ² ~ InvGamma((n−p)/2, RSS/2)   (drawn as RSS / χ²_{n-p})
-- β  ~ MVN(β̂,  σ² (X'X)⁻¹)
-- log p(y_i | β^s, σ^s) = log N(y_i; x_i·β^s, σ^s)
-- @
lmPosteriorLogLiks
  :: LA.Matrix Double  -- ^ Design matrix @X@ (@n×p@).
  -> LA.Vector Double  -- ^ Response @y@ (length @n@).
  -> FitResult         -- ^ OLS fit result.
  -> Int               -- ^ Number of posterior samples @S@.
  -> GenIO
  -> IO [[Double]]
lmPosteriorLogLiks x y fr s gen = do
  let n      = LA.rows x
      p      = LA.cols x
      df'    = n - p
      beta0  = coefficientsV fr
      rss    = let resV = residualsV fr in LA.dot resV resV
      xtxInv = LA.inv (LA.tr x LA.<> x)
      rChol  = LA.chol (LA.trustSym xtxInv)
      lChol  = LA.tr rChol
  replicateM s $ do
    chi2Vals <- replicateM df' (normal 0 1 gen)
    let chi2  = sum (map (^(2::Int)) chi2Vals)
        sigma = sqrt (rss / chi2)
    zVec <- fmap LA.fromList (replicateM p (normal 0 1 gen))
    let betaSamp = beta0 + LA.scale sigma (lChol LA.#> zVec)
        yHat     = LA.toList (x LA.#> betaSamp)
        ys       = LA.toList y
    return [ logNormDensity yi yhi sigma | (yi, yhi) <- zip ys yHat ]

-- | Generate an @S × N@ log-likelihood matrix from a Laplace-approximate
-- GLM posterior. For Gaussian-family models prefer 'lmPosteriorLogLiks'.
--
-- @
-- β ~ MVN(β̂,  Fisher⁻¹)
-- log p(y_i | β^s) = family-specific log-density
-- @
glmPosteriorLogLiks
  :: Family
  -> LinkFn
  -> LA.Matrix Double  -- ^ Design matrix @X@.
  -> LA.Vector Double  -- ^ Response @y@.
  -> LA.Matrix Double  -- ^ Inverse Fisher information.
  -> FitResult
  -> Int               -- ^ Number of posterior samples @S@.
  -> GenIO
  -> IO [[Double]]
glmPosteriorLogLiks family linkFn x y fisherInv fr s gen = do
  let p     = LA.rows fisherInv
      beta0 = coefficientsV fr
      rChol = LA.chol (LA.trustSym fisherInv)
      lChol = LA.tr rChol
  replicateM s $ do
    zVec <- fmap LA.fromList (replicateM p (normal 0 1 gen))
    let betaSamp = beta0 + lChol LA.#> zVec
        eta      = LA.toList (x LA.#> betaSamp)
        ys       = LA.toList y
    return [ glmLogDensity family linkFn yi ei | (yi, ei) <- zip ys eta ]

-- | Log-likelihood matrix for the **conditional** WAIC of a Gaussian
-- LME (random intercepts).
--
-- This is not a fully marginal GLMM posterior. It conditions on a point
-- estimate of the BLUPs @û@ and posterior-samples @(β, σ²)@ as if from
-- a residualized LM:
--
--   * @y' := y − Z·û@  (response with BLUP offset removed).
--   * @σ² ~ InvGamma((n−p)/2, RSS_cond/2)@ where @RSS_cond@ is the LME
--     conditional residual sum of squares.
--   * @β ~ MVN(β̂,  σ² (X'X)⁻¹)@.
--   * @log p(y_i | β^s, û_{j(i)}, σ^s) = log N(y_i; X_iβ^s + û_{j(i)}, σ^s)@.
--
-- Because @u@ is held fixed, @p_WAIC@ tends to be smaller than the true
-- value; this is still useful for comparing fixed-effect structures on
-- the same data (see Gelman, Hwang & Vehtari 2014, §3.3).
lmePosteriorLogLiks
  :: LA.Matrix Double  -- ^ Fixed-effect design matrix @X@ (@n×p@).
  -> LA.Vector Double  -- ^ Response @y@ (length @n@).
  -> [Double]          -- ^ Per-observation BLUP offset @û_{j(i)}@ (length @n@).
  -> FitResult         -- ^ Fixed-effect LME fit result.
  -> Int               -- ^ Number of posterior samples @S@.
  -> GenIO
  -> IO [[Double]]
lmePosteriorLogLiks x y offsets fr s gen = do
  let n      = LA.rows x
      p      = LA.cols x
      df'    = n - p
      beta0  = coefficientsV fr
      rss    = let resV = residualsV fr in LA.dot resV resV
      xtxInv = LA.inv (LA.tr x LA.<> x)
      rChol  = LA.chol (LA.trustSym xtxInv)
      lChol  = LA.tr rChol
  replicateM s $ do
    chi2Vals <- replicateM df' (normal 0 1 gen)
    let chi2    = sum (map (^(2::Int)) chi2Vals)
        sigSamp = sqrt (rss / chi2)
    zVec <- fmap LA.fromList (replicateM p (normal 0 1 gen))
    let betaSamp = beta0 + LA.scale sigSamp (lChol LA.#> zVec)
        yFix     = LA.toList (x LA.#> betaSamp)
        yCond    = zipWith (+) yFix offsets
        ys       = LA.toList y
    return [ logNormDensity yi yhi sigSamp | (yi, yhi) <- zip ys yCond ]

logNormDensity :: Double -> Double -> Double -> Double
logNormDensity y mu sig
  | sig <= 0  = -1/0
  | otherwise = let d = (y - mu) / sig
                in -0.5 * log (2 * pi) - log sig - 0.5 * d * d

glmLogDensity :: Family -> LinkFn -> Double -> Double -> Double
glmLogDensity family linkFn y eta =
  let mu = case linkFn of
              Identity -> eta
              Log      -> exp eta
              Logit    -> 1 / (1 + exp (-eta))
              Sqrt     -> eta * eta
  in case family of
       Gaussian -> logNormDensity y mu 1.0
       Poisson  -> Dist.logDensity (Dist.Poisson (max 1e-10 mu)) y
       Binomial -> Dist.logDensity (Dist.Binomial 1 (max 1e-8 (min (1-1e-8) mu))) y

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

-- ---------------------------------------------------------------------------
-- モデル比較の重み (Pseudo-BMA, ArviZ.compare 相当)
-- ---------------------------------------------------------------------------

-- | One candidate model for comparison: label and log-likelihood matrix.
data CompareEntry = CompareEntry
  { ceLabel    :: String          -- ^ Model label.
  , ceLogLikMat :: [[Double]]     -- ^ @S × N@ log-likelihood matrix.
  } deriving (Show)

-- | Per-model comparison result.
data CompareResult = CompareResult
  { crLabel     :: String          -- ^ Model label.
  , crWAIC      :: Double          -- ^ WAIC (smaller is better).
  , crLOO       :: Double          -- ^ LOO  (smaller is better).
  , crDeltaWAIC :: Double          -- ^ @ΔWAIC@ vs the best model.
  , crDeltaLOO  :: Double          -- ^ @ΔLOO@  vs the best model.
  , crSE        :: Double          -- ^ Standard error of @WAIC@.
  , crKHatBad   :: Int             -- ^ Number of observations with @k̂ > 0.7@.
  , crWeight    :: Double          -- ^ Pseudo-BMA weight (sums to 1 over models).
  } deriving (Show)

-- | Compare several models by WAIC / LOO and compute Pseudo-BMA weights.
--
-- Algorithm:
--
--   * Compute WAIC and LOO for each model.
--   * Use the best (minimum) model as baseline for @ΔWAIC@ / @ΔLOO@.
--   * Pseudo-BMA weight: @w_i = exp(elpd_i) / Σ exp(elpd_j)@.
--     (実用的には Δ から計算: w_i ∝ exp(-Δelpd_i))
compareModels :: [CompareEntry] -> [CompareResult]
compareModels entries =
  let waicResults = map (\e -> (ceLabel e, waic (ceLogLikMat e))) entries
      looResults  = map (\e -> (ceLabel e, loo  (ceLogLikMat e))) entries
      waicVals    = map (waicValue . snd) waicResults
      looVals     = map (looValue  . snd) looResults
      -- elpd_loo (= -looValue / 2) 基準で Pseudo-BMA 重みを計算
      elpds       = map (\v -> -v / 2) looVals
      maxElpd     = maximum elpds
      unnorm      = map (\e -> exp (e - maxElpd)) elpds
      total       = sum unnorm
      weights     = map (/ total) unnorm
      bestWaic    = minimum waicVals
      bestLoo     = minimum looVals
  in zipWith4 mkRow entries waicResults looResults weights
  where
    mkRow entry (lbl, w) (_, l) wt = CompareResult
      { crLabel     = lbl
      , crWAIC      = waicValue w
      , crLOO       = looValue  l
      , crDeltaWAIC = waicValue w - minimum (map (\e -> waicValue (waic (ceLogLikMat e))) entries)
      , crDeltaLOO  = looValue  l - minimum (map (\e -> looValue  (loo  (ceLogLikMat e))) entries)
      , crSE        = waicSE w
      , crKHatBad   = looKHatBad l
      , crWeight    = wt
      }
    zipWith4 f as bs cs ds = case (as, bs, cs, ds) of
      (a:as', b:bs', c:cs', d:ds') -> f a b c d : zipWith4 f as' bs' cs' ds'
      _ -> []
