{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- |
-- Module      : Hanalyze.Stat.VI
-- Description : 変分推論 (ADVI: Automatic Differentiation Variational Inference)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Variational inference (ADVI — Automatic Differentiation Variational
-- Inference).
--
-- Implements the mean-field normal VI of Kucukelbir et al. (2017). Uses
-- the same unconstrained transform as HMC/NUTS and maximizes the ELBO
-- with Adam.
--
-- Approximating family: @q(u; φ) = Π_i Normal(u_i; μ_i, σ_i)@
--
-- @
-- ELBO = E_q[log p(θ,y) + log|J|] + Σ_i H[Normal(μ_i, σ_i)]
--      = E_q[logJointU(u)] + Σ_i ω_i + N/2 × (1 + log 2π)
-- @
--
-- Gradient (reparameterization trick):
--
-- @
-- u^s = μ + σ ⊙ ε^s,  ε^s ~ N(0, I)
-- ∂ELBO/∂μ_i ≈ (1/S) Σ_s ∂logJointU/∂u_i |_{u^s}
-- ∂ELBO/∂ω_i ≈ (1/S) Σ_s ε_i^s × σ_i × ∂logJointU/∂u_i |_{u^s} + 1
-- @
--
-- @
-- let cfg = defaultVIConfig { viIterations = 1000 }
-- result <- advi model cfg initParams gen
-- print (viPostMeans result)
-- @
module Hanalyze.Stat.VI
  ( VIConfig (..)
  , defaultVIConfig
  , VIResult (..)
  , VIMethod (..)
  , advi
  , fullRankAdvi
  ) where

import Control.DeepSeq (force)
import Control.Monad (forM, forM_, replicateM, when)
import Data.IORef
import qualified Data.Map.Strict as Map
import System.Random.MWC (GenIO)
import System.Random.MWC.Distributions (standard)

import Hanalyze.Model.HBM (ModelP, Params, sampleNames, getTransforms)
import Hanalyze.Optim.Adam (adamStep)
import Hanalyze.MCMC.HMC  ( logJointU, paramsToVec, vecToParams
                 , toUnconstrainedParams, fromUnconstrainedParams )

-- ---------------------------------------------------------------------------
-- 設定
-- ---------------------------------------------------------------------------

-- | ADVI configuration.
data VIConfig = VIConfig
  { viIterations   :: Int     -- ^ Number of Adam iterations.
  , viSamples      :: Int     -- ^ Monte Carlo samples per ELBO gradient (5–10 typical).
  , viLearningRate :: Double  -- ^ Adam learning rate @α@.
  , viBeta1        :: Double  -- ^ Adam @β₁@ (default 0.9).
  , viBeta2        :: Double  -- ^ Adam @β₂@ (default 0.999).
  , viEpsilon      :: Double  -- ^ Adam @ε@ (default 1e-8).
  , viNumDraws     :: Int     -- ^ Number of post-fit draws from @q@.
  , viGradStep     :: Double  -- ^ Finite-difference step for numeric gradients.
  } deriving (Show)

-- | Sensible defaults for ADVI: 1000 iterations, 5 MC samples, Adam at
-- @α = 0.1@.
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

-- | VI 近似法。 mean-field (`advi`) と full-rank (`fullRankAdvi`) を区別する。
data VIMethod = MeanField | FullRank
  deriving (Show, Eq)

-- | ADVI result. mean-field と full-rank の両方が返す。 full-rank では
-- @viCovU@ に @n×n@ 下三角 Cholesky 因子 @L@ (unconstrained 空間) が入る。
data VIResult = VIResult
  { viPostMeans   :: Params           -- ^ Posterior means (constrained space, sample mean).
  , viPostSDs     :: Params           -- ^ Posterior SDs   (constrained space).
  , viMuU         :: [Double]         -- ^ Variational mean @μ@ (unconstrained).
  , viSigmaU      :: [Double]         -- ^ Variational SD   @σ@ (unconstrained、 mean-field の場合は対角要素、 full-rank なら L_ii)。
  , viCovU        :: Maybe [[Double]] -- ^ Full-rank ADVI: 下三角 Cholesky 因子 @L@ ([row][col])、 @LLᵀ = Σ@。 mean-field では @Nothing@。
  , viMethod      :: VIMethod         -- ^ どちらの近似法か。
  , viElboHistory :: [Double]         -- ^ ELBO trajectory (for convergence inspection).
  , viDraws       :: [Params]         -- ^ Posterior draws in the constrained space (length 'viNumDraws').
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- ADVI
-- ---------------------------------------------------------------------------

-- | Run mean-field normal ADVI.
--
-- Optimization happens in unconstrained space; samples are mapped back
-- to the constrained space on the way out. Constrained parameters
-- (e.g. @Exponential → PositiveT@) are transformed automatically.
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
        !ljMC = sum (map (\(l,_,_) -> l) mcResults) / sD
        -- ELBO = E[logJointU] + Σω + N/2×(1+log2π)
        !elboV = ljMC + sum omega + fromIntegral n * 0.5 * (1 + log (2*pi))
        !gMu   = force (map (/ sD) $ foldr1 (zipWith (+)) (map (\(_,g,_) -> g) mcResults))
        !gOm   = force (map (/ sD) $ foldr1 (zipWith (+)) (map (\(_,_,g) -> g) mcResults))

    modifyIORef' elboRef (elboV :)

    -- Adam で μ を更新
    m1Mu <- readIORef m1MuRef
    m2Mu <- readIORef m2MuRef
    let (m1Mu', m2Mu', dxMu) = adamStep b1 b2 eps_ alpha t m1Mu m2Mu gMu
    -- Phase Q3 (2026-05-14): 'zipWith (+)' / Adam の各リストは lazy で、
    -- IORef に書き戻すとそのまま thunk のまま積まれ、次イテレーションで
    -- 読み出されると `zipWith (+) thunk_{t-1} ...` が再帰的に重なる。
    -- iter=10000 K=20 で max residency 85 MB / 総 alloc 222 GB を観測。
    -- 'force' で spine + 各要素を NF にし、t 階層の thunk チェーンを断つ。
    writeIORef m1MuRef (force m1Mu')
    writeIORef m2MuRef (force m2Mu')
    writeIORef muRef   (force (zipWith (+) mu dxMu))

    -- Adam で ω を更新
    m1Om <- readIORef m1OmRef
    m2Om <- readIORef m2OmRef
    let (m1Om', m2Om', dxOm) = adamStep b1 b2 eps_ alpha t m1Om m2Om gOm
    writeIORef m1OmRef (force m1Om')
    writeIORef m2OmRef (force m2Om')
    writeIORef omegaRef (force (zipWith (+) omega dxOm))

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
    , viCovU        = Nothing
    , viMethod      = MeanField
    , viElboHistory = elboHistory
    , viDraws       = draws
    }

-- ---------------------------------------------------------------------------
-- Full-rank ADVI (Phase 37-A5)
-- ---------------------------------------------------------------------------

-- | Full-rank ADVI: 共分散を含めた変分近似 @q(u) = N(μ, LLᵀ)@ を最適化する。
--
-- 平均場 'advi' との違い:
--
-- * 変分パラメータは @μ@ (n-vector) と @L@ (下三角 n×n、 対角は log で
--   parameterize して正値保証)
-- * @u = μ + L·ε@ の reparameterization で勾配を取り、 ELBO の補正項は
--   @log|L| = Σ log L_ii = Σ ω_i@
-- * 推定共分散 @Σ = LLᵀ@ は @viCovU@ に入る (下三角 @L@ そのもの)
--
-- 平均場と比べて posterior の相関を捉えられるが、 パラメタ数 @O(n²)@、
-- 計算量も @O(n² S)@ per iteration なので n が大きいモデルでは重い。
-- 平均場が「SD を過小評価」 する hierarchical model で特に有用。
fullRankAdvi :: ModelP r -> VIConfig -> Params -> GenIO -> IO VIResult
fullRankAdvi model cfg initP gen = do
  let names      = sampleNames model
      transforms = getTransforms model
      n          = length names
      initU      = paramsToVec names (toUnconstrainedParams transforms initP)

      logJ :: [Double] -> Double
      logJ uVec = logJointU model transforms (vecToParams names uVec)

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

  -- 変分パラメータ: μ (n-vector)、 ω (n-vector、 ω_i = log L_ii)、
  -- offdiag (下三角の i > j 要素を行優先で並べた長さ n(n-1)/2 のリスト)
  muRef    <- newIORef initU
  omegaRef <- newIORef (replicate n 0.0)             -- L_ii = exp(0) = 1
  let nOff = n * (n - 1) `div` 2
  offRef   <- newIORef (replicate nOff 0.0)          -- off-diag は 0 で初期化

  -- Adam モーメント (μ / ω / offdiag それぞれ)
  m1MuRef <- newIORef (replicate n 0.0)
  m2MuRef <- newIORef (replicate n 0.0)
  m1OmRef <- newIORef (replicate n 0.0)
  m2OmRef <- newIORef (replicate n 0.0)
  m1OffRef <- newIORef (replicate nOff 0.0)
  m2OffRef <- newIORef (replicate nOff 0.0)

  elboRef <- newIORef []

  let b1    = viBeta1        cfg
      b2    = viBeta2        cfg
      eps_  = viEpsilon      cfg
      alpha = viLearningRate cfg
      sNum  = viSamples      cfg

  forM_ [1 .. viIterations cfg] $ \t -> do
    mu     <- readIORef muRef
    omega  <- readIORef omegaRef
    offdg  <- readIORef offRef
    let lMat  = buildL n omega offdg                  -- 下三角 L

    -- MC 勾配
    mcResults <- forM [1 .. sNum] $ \_ -> do
      epsilons <- replicateM n (standard gen)
      let uVec = vecAdd mu (matVec lMat epsilons)
          lj   = logJ uVec
          gU   = numGrad uVec                          -- ∂lp/∂u_i, length n
          dMu  = gU                                    -- ∂ELBO/∂μ_i = gU_i
          -- ∂ELBO/∂ω_i = ε_i × L_ii × gU_i + 1  (entropy +1)
          dOm  = [ epsilons !! i
                 * (lMat !! i !! i)
                 * (gU !! i) + 1
                 | i <- [0 .. n-1] ]
          -- ∂ELBO/∂L_ij (i > j) = ε_j × gU_i  (no entropy contribution)
          dOff = [ (epsilons !! j) * (gU !! i)
                 | i <- [1 .. n-1], j <- [0 .. i-1] ]
      return (lj, dMu, dOm, dOff)

    let sD    = fromIntegral sNum :: Double
        !ljMC = sum (map (\(l,_,_,_) -> l) mcResults) / sD
        -- ELBO = E[logJointU] + log|L| + n/2 (1 + log 2π)
        !elboV = ljMC + sum omega + fromIntegral n * 0.5 * (1 + log (2*pi))
        !gMu   = force (map (/ sD) $ foldr1 (zipWith (+))
                                     (map (\(_,g,_,_) -> g) mcResults))
        !gOm   = force (map (/ sD) $ foldr1 (zipWith (+))
                                     (map (\(_,_,g,_) -> g) mcResults))
        !gOff  = if nOff == 0
                   then []
                   else force (map (/ sD) $ foldr1 (zipWith (+))
                                            (map (\(_,_,_,g) -> g) mcResults))

    modifyIORef' elboRef (elboV :)

    -- Adam で μ
    m1Mu <- readIORef m1MuRef
    m2Mu <- readIORef m2MuRef
    let (m1Mu', m2Mu', dxMu) = adamStep b1 b2 eps_ alpha t m1Mu m2Mu gMu
    writeIORef m1MuRef (force m1Mu')
    writeIORef m2MuRef (force m2Mu')
    writeIORef muRef   (force (zipWith (+) mu dxMu))

    -- Adam で ω
    m1Om <- readIORef m1OmRef
    m2Om <- readIORef m2OmRef
    let (m1Om', m2Om', dxOm) = adamStep b1 b2 eps_ alpha t m1Om m2Om gOm
    writeIORef m1OmRef (force m1Om')
    writeIORef m2OmRef (force m2Om')
    writeIORef omegaRef (force (zipWith (+) omega dxOm))

    -- Adam で off-diagonal (n=1 のときは空)
    when (nOff > 0) $ do
      m1Off <- readIORef m1OffRef
      m2Off <- readIORef m2OffRef
      let (m1Off', m2Off', dxOff) = adamStep b1 b2 eps_ alpha t m1Off m2Off gOff
      writeIORef m1OffRef (force m1Off')
      writeIORef m2OffRef (force m2Off')
      writeIORef offRef   (force (zipWith (+) offdg dxOff))

  -- 収束後
  muFinal    <- readIORef muRef
  omegaFinal <- readIORef omegaRef
  offFinal   <- readIORef offRef
  let lFinal   = buildL n omegaFinal offFinal
      lDiag    = [ lFinal !! i !! i | i <- [0 .. n-1] ]

  draws <- forM [1 .. viNumDraws cfg] $ \_ -> do
    epsilons <- replicateM n (standard gen)
    let uVec = vecAdd muFinal (matVec lFinal epsilons)
    return (fromUnconstrainedParams transforms (vecToParams names uVec))

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
    , viSigmaU      = lDiag
    , viCovU        = Just lFinal
    , viMethod      = FullRank
    , viElboHistory = elboHistory
    , viDraws       = draws
    }

-- | 下三角 L を構築。 @omega@ は対角 (L_ii = exp ω_i)、
-- @offdg@ は (i, j) for i > j を行優先 (i 昇順、 同 i 内で j 昇順) で
-- 並べたリスト。 結果は @n × n@ 行列、 上三角は 0。
buildL :: Int -> [Double] -> [Double] -> [[Double]]
buildL n omega offdg =
  let -- offdg をインデックス map に変換
      offMap = Map.fromList (zip pairs offdg)
      pairs  = [ (i, j) | i <- [1 .. n-1], j <- [0 .. i-1] ]
      diag i = exp (omega !! i)
      row i  = [ if j < i  then Map.findWithDefault 0 (i, j) offMap
                 else if j == i then diag i
                 else 0
               | j <- [0 .. n-1] ]
  in [ row i | i <- [0 .. n-1] ]

-- | 行列・ベクトル積 @y = M·x@。
matVec :: [[Double]] -> [Double] -> [Double]
matVec mat x = [ sum (zipWith (*) row x) | row <- mat ]

-- | ベクトル足し算。
vecAdd :: [Double] -> [Double] -> [Double]
vecAdd = zipWith (+)

-- ---------------------------------------------------------------------------
-- 補助関数
-- ---------------------------------------------------------------------------

-- adamStep は Hanalyze.Optim.Adam に集約 (Phase R0)。
-- 再 export することで既存の利用箇所はそのまま動く。

-- | リストの i 番目要素を x で置換する。
replaceAt :: Int -> Double -> [Double] -> [Double]
replaceAt i x xs = take i xs ++ [x] ++ drop (i + 1) xs
