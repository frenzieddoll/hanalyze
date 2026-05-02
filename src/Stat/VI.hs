{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | 変分推論 (ADVI — Automatic Differentiation Variational Inference)
--
-- Kucukelbir et al. (2017) の平均場正規 VI を実装。
-- HMC/NUTS と同じ unconstrained 変換を使い、Adam で ELBO を最大化する。
--
-- 近似族: q(u; φ) = Π_i Normal(u_i; μ_i, σ_i)
-- ELBO  = E_q[log p(θ,y) + log|J|] + Σ_i H[Normal(μ_i,σ_i)]
--       = E_q[logJointU(u)] + Σ_i ω_i + N/2 × (1 + log 2π)
--
-- 勾配 (reparameterization trick):
--   u^s = μ + σ ⊙ ε^s,  ε^s ~ N(0,I)
--   ∂ELBO/∂μ_i ≈ (1/S) Σ_s ∂logJointU/∂u_i |_{u^s}
--   ∂ELBO/∂ω_i ≈ (1/S) Σ_s ε_i^s × σ_i × ∂logJointU/∂u_i |_{u^s} + 1
--
-- @
-- let cfg = defaultVIConfig { viIterations = 1000 }
-- result <- advi model cfg initParams gen
-- print (viPostMeans result)
-- @
module Stat.VI
  ( VIConfig (..)
  , defaultVIConfig
  , VIResult (..)
  , advi
  ) where

import Control.Monad (forM, forM_, replicateM)
import Data.IORef
import qualified Data.Map.Strict as Map
import System.Random.MWC (GenIO)
import System.Random.MWC.Distributions (standard)

import Model.HBM (ModelP, Params, sampleNames, getTransforms)
import Optim.Adam (adamStep)
import MCMC.HMC  ( logJointU, paramsToVec, vecToParams
                 , toUnconstrainedParams, fromUnconstrainedParams )

-- ---------------------------------------------------------------------------
-- 設定
-- ---------------------------------------------------------------------------

data VIConfig = VIConfig
  { viIterations   :: Int     -- ^ Adam 反復回数
  , viSamples      :: Int     -- ^ ELBO 勾配の MC サンプル数 (推奨: 5–10)
  , viLearningRate :: Double  -- ^ Adam 学習率 α
  , viBeta1        :: Double  -- ^ Adam β₁ (default 0.9)
  , viBeta2        :: Double  -- ^ Adam β₂ (default 0.999)
  , viEpsilon      :: Double  -- ^ Adam ε (default 1e-8)
  , viNumDraws     :: Int     -- ^ 収束後に q から引くサンプル数
  , viGradStep     :: Double  -- ^ 数値勾配の有限差分刻み幅
  } deriving (Show)

defaultVIConfig :: VIConfig
defaultVIConfig = VIConfig
  { viIterations   = 1000
  , viSamples      = 5
  , viLearningRate = 0.1
  , viBeta1        = 0.9
  , viBeta2        = 0.999
  , viEpsilon      = 1e-8
  , viNumDraws     = 2000
  , viGradStep     = 1e-5
  }

-- ---------------------------------------------------------------------------
-- 結果
-- ---------------------------------------------------------------------------

data VIResult = VIResult
  { viPostMeans   :: Params    -- ^ 事後平均 (constrained space, サンプル平均)
  , viPostSDs     :: Params    -- ^ 事後 SD  (constrained space, サンプル標準偏差)
  , viMuU         :: [Double]  -- ^ 変分平均 μ (unconstrained space)
  , viSigmaU      :: [Double]  -- ^ 変分 SD  σ (unconstrained space)
  , viElboHistory :: [Double]  -- ^ ELBO の時系列 (収束確認用)
  , viDraws       :: [Params]  -- ^ 事後サンプル (constrained, viNumDraws 本)
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- ADVI
-- ---------------------------------------------------------------------------

-- | 平均場正規 ADVI を実行する。
--
-- 内部では unconstrained 空間で最適化し、サンプルを constrained 空間に戻す。
-- 制約付きパラメータ (Exponential→PositiveT など) は自動変換される。
advi :: ModelP r -> VIConfig -> Params -> GenIO -> IO VIResult
advi model cfg initP gen = do
  let names      = sampleNames model
      transforms = getTransforms model
      n          = length names
      initU      = paramsToVec names (toUnconstrainedParams transforms initP)

      -- unconstrained 空間での log p(θ,y) + log|J| (Jacobian 補正済み)
      logJ :: [Double] -> Double
      logJ uVec = logJointU model transforms (vecToParams names uVec)

      -- 有限差分勾配 ∂logJ/∂u
      h = viGradStep cfg
      numGrad :: [Double] -> [Double]
      numGrad uVec =
        [ let ui  = uVec !! i
              lp  = logJ (replaceAt i (ui + h) uVec)
              lm  = logJ (replaceAt i (ui - h) uVec)
              raw = (lp - lm) / (2 * h)
          in if isNaN raw || isInfinite raw then 0 else raw
        | i <- [0 .. n-1]
        ]

  -- 変分パラメータ: μ (unconstrained 平均), ω = log(σ) (log 標準偏差)
  muRef    <- newIORef initU
  omegaRef <- newIORef (replicate n 0.0)  -- σ = exp(0) = 1 で初期化

  -- Adam の 1次/2次モーメント
  m1MuRef <- newIORef (replicate n 0.0)
  m2MuRef <- newIORef (replicate n 0.0)
  m1OmRef <- newIORef (replicate n 0.0)
  m2OmRef <- newIORef (replicate n 0.0)

  elboRef <- newIORef []

  let b1    = viBeta1        cfg
      b2    = viBeta2        cfg
      eps_  = viEpsilon      cfg
      alpha = viLearningRate cfg
      sNum  = viSamples      cfg

  -- Adam ループ
  forM_ [1 .. viIterations cfg] $ \t -> do
    mu    <- readIORef muRef
    omega <- readIORef omegaRef
    let sigma = map exp omega

    -- MC 勾配推定
    mcResults <- forM [1 .. sNum] $ \_ -> do
      epsilons <- replicateM n (standard gen)
      let -- u^s = μ + σ ⊙ ε  (reparameterization)
          uVec = zipWith3 (\m s e -> m + s * e) mu sigma epsilons
          lj   = logJ uVec
          g    = numGrad uVec
          -- ∂ELBO/∂μ_i = ∂logJ/∂u_i
          dMu  = g
          -- ∂ELBO/∂ω_i = ε_i × σ_i × ∂logJ/∂u_i + 1  (+1 はエントロピー項)
          dOm  = zipWith3 (\e s gi -> e * s * gi + 1) epsilons sigma g
      return (lj, dMu, dOm)

    let sD    = fromIntegral sNum :: Double
        ljMC  = sum (map (\(l,_,_) -> l) mcResults) / sD
        -- ELBO = E[logJointU] + Σω + N/2×(1+log2π)
        elboV = ljMC + sum omega + fromIntegral n * 0.5 * (1 + log (2*pi))
        gMu   = map (/ sD) $ foldr1 (zipWith (+)) (map (\(_,g,_) -> g) mcResults)
        gOm   = map (/ sD) $ foldr1 (zipWith (+)) (map (\(_,_,g) -> g) mcResults)

    modifyIORef' elboRef (elboV :)

    -- Adam で μ を更新
    m1Mu <- readIORef m1MuRef
    m2Mu <- readIORef m2MuRef
    let (m1Mu', m2Mu', dxMu) = adamStep b1 b2 eps_ alpha t m1Mu m2Mu gMu
    writeIORef m1MuRef m1Mu'
    writeIORef m2MuRef m2Mu'
    writeIORef muRef   (zipWith (+) mu dxMu)

    -- Adam で ω を更新
    m1Om <- readIORef m1OmRef
    m2Om <- readIORef m2OmRef
    let (m1Om', m2Om', dxOm) = adamStep b1 b2 eps_ alpha t m1Om m2Om gOm
    writeIORef m1OmRef m1Om'
    writeIORef m2OmRef m2Om'
    writeIORef omegaRef (zipWith (+) omega dxOm)

  -- 収束後: q(u; φ*) からサンプリングして constrained 空間に変換
  muFinal    <- readIORef muRef
  omegaFinal <- readIORef omegaRef
  let sigmaFinal = map exp omegaFinal

  draws <- forM [1 .. viNumDraws cfg] $ \_ -> do
    epsilons <- replicateM n (standard gen)
    let uVec = zipWith3 (\m s e -> m + s * e) muFinal sigmaFinal epsilons
    return (fromUnconstrainedParams transforms (vecToParams names uVec))

  -- サンプルから事後平均・SD を計算
  let nD        = fromIntegral (viNumDraws cfg) :: Double
      getVals p = map (Map.findWithDefault 0 p) draws
      muP     p = let vs = getVals p in sum vs / nD
      sdP     p = let vs = getVals p
                      mu = muP p
                  in sqrt (sum (map (\v -> (v - mu) ^ (2::Int)) vs) / nD)
      postMeans = Map.fromList [(nm, muP nm) | nm <- names]
      postSDs   = Map.fromList [(nm, sdP nm) | nm <- names]

  elboHistory <- fmap reverse (readIORef elboRef)

  return VIResult
    { viPostMeans   = postMeans
    , viPostSDs     = postSDs
    , viMuU         = muFinal
    , viSigmaU      = sigmaFinal
    , viElboHistory = elboHistory
    , viDraws       = draws
    }

-- ---------------------------------------------------------------------------
-- 補助関数
-- ---------------------------------------------------------------------------

-- adamStep は Optim.Adam に集約 (Phase R0)。
-- 再 export することで既存の利用箇所はそのまま動く。

-- | リストの i 番目要素を x で置換する。
replaceAt :: Int -> Double -> [Double] -> [Double]
replaceAt i x xs = take i xs ++ [x] ++ drop (i + 1) xs
