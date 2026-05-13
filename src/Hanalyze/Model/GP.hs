{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Gaussian-process regression.
--
-- Pick a kernel, fit it to training data and obtain the posterior
-- predictive at arbitrary test points. Hyperparameters can be tuned
-- automatically by maximizing the log marginal likelihood.
--
-- @
-- import Hanalyze.Model.GP
--
-- -- 訓練データ
-- let xs = [0, 0.5 .. 5]
--     ys = map (\x -> sin x + 0.1 * noise) xs
--
-- -- ハイパーパラメータをデータから初期化し最適化
-- let p0  = initParamsFromData xs ys
--     opt = optimizeGP RBF xs ys p0
--     res = fitGP (GPModel RBF opt) xs ys testXs
--
-- -- gpMean res, gpLower res, gpUpper res で結果を取得
-- @
module Hanalyze.Model.GP
  ( -- * カーネル型
    Kernel (..)
  , kernelName
    -- * Hyperparameters
  , GPParams (..)
  , defaultGPParams
  , initParamsFromData
  , initParamsFromDataMV
    -- * Model and result
  , GPModel (..)
  , GPResult (..)
    -- * Kernel computation
  , kernelFn
  , buildKernelMatrix
    -- * Inference
  , logMarginalLikelihood
  , fitGP
  , fitGPMulti
  , optimizeGP
    -- * Data for interactive prediction
  , GPPredData (..)
  , gpPredData
    -- * Multi-input (primary API; X is @n × p@, Y is @n × q@)
  , GPResultMV (..)
  , buildKernelMatrixMV
  , noiseKernelMV
  , logMarginalLikelihoodMV
  , fitGPMV
  , fitGPMVMulti
  , optimizeGPMV
  , optimizeGPMVCached
  ) where

import Data.Text (Text)
import qualified Numeric.LinearAlgebra as LA
import qualified Hanalyze.Optim.LBFGS as LBFGS
import qualified Hanalyze.Optim.Common as OC
import qualified Hanalyze.Stat.KernelDist as KD
import qualified Hanalyze.Stat.Cholesky   as Chol
import qualified Data.Vector.Storable         as VS
import qualified Data.Vector.Storable.Mutable as VSM
import           Control.Monad.ST             (runST)
import           System.IO.Unsafe             (unsafePerformIO)

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | GP kernel kind.
data Kernel
  = RBF
    -- ^ Squared exponential: @k(x,x') = σ_f² exp(−r²/(2ℓ²))@.
    --   Best for smooth functions; the most commonly used kernel.
  | Matern52
    -- ^ Matérn 5/2: @k(x,x') = σ_f²(1+√5 r/ℓ+5r²/(3ℓ²)) exp(−√5 r/ℓ)@.
    --   Slightly rougher than RBF; common in physical systems.
  | Periodic
    -- ^ Periodic: @k(x,x') = σ_f² exp(−2 sin²(π r/p)/ℓ²)@.
    --   For periodic patterns; set 'gpPeriod' appropriately.
  deriving (Show, Eq)

-- | Display name of a kernel.
kernelName :: Kernel -> Text
kernelName RBF       = "RBF"
kernelName Matern52  = "Mat\xe9rn 5/2"
kernelName Periodic  = "Periodic"

-- | GP hyperparameters.
data GPParams = GPParams
  { gpLengthScale  :: Double
    -- ^ Isotropic length scale @ℓ@; larger means smoother. Used unless
    --   'gpLengthScales' is 'Just' (= ARD), in which case the per-dim
    --   vector overrides this for multi-input kernel evaluation.
  , gpSignalVar    :: Double
    -- ^ Signal variance @σ_f²@; the variability of the function values.
  , gpNoiseVar     :: Double
    -- ^ Observation noise variance @σ_n²@; near 0 interpolates, larger
    --   smooths.
  , gpPeriod       :: Double
    -- ^ Period @p@ (only used by the @Periodic@ kernel).
  , gpLengthScales :: Maybe (LA.Vector Double)
    -- ^ Per-dim length scales for ARD (Automatic Relevance
    --   Determination). When 'Just' v, the multi-input kernel uses
    --   @D_ARD[i,j] = Σ_d (X[i,d] − X'[j,d])² / ℓ_d²@ instead of the
    --   isotropic distance / ℓ². Has no effect on the 1D 'kernelFn' /
    --   'fitGP' path. 'Nothing' = isotropic (default).
  } deriving (Show)

-- | Default hyperparameters: @ℓ = σ_f² = p = 1@, @σ_n² = 0.1@.
defaultGPParams :: GPParams
defaultGPParams = GPParams 1.0 1.0 0.1 1.0 Nothing

-- | Build a sensible initial 'GPParams' from data statistics, suitable
-- as a starting point for optimization.
initParamsFromData :: [Double] -> [Double] -> GPParams
initParamsFromData xs ys = GPParams
  { gpLengthScale  = max 0.01 ((xMax - xMin) / 4)
  , gpSignalVar    = max 0.01 yVar
  , gpNoiseVar     = max 1e-4 (yVar * 0.05)
  , gpPeriod       = max 0.01 (xMax - xMin)
  , gpLengthScales = Nothing
  }
  where
    xMin  = minimum xs
    xMax  = maximum xs
    yMean = sum ys / fromIntegral (length ys)
    yVar  = sum (map (\y -> (y - yMean) ^ (2 :: Int)) ys) / fromIntegral (length ys)

-- | Multi-input variant of 'initParamsFromData'. Computes the length
-- scale from the /average/ per-dimension range of @X@ rather than
-- collapsing the @n × p@ matrix into a flat list (which the previous
-- @MultiGP@ call site did via @concat (toLists trainX)@ — yielding
-- nonsensical @xMin/xMax@ statistics, a poor length-scale init, and
-- in turn slow LBFGS convergence).
initParamsFromDataMV :: LA.Matrix Double -> LA.Vector Double -> GPParams
initParamsFromDataMV trainX y =
  let p     = LA.cols trainX
      cols  = LA.toColumns trainX            -- p column vectors
      ranges = [ LA.maxElement c - LA.minElement c | c <- cols ]
      avgRng = if null ranges then 1.0
                              else sum ranges / fromIntegral (length ranges)
      ys    = LA.toList y
      yMean = LA.sumElements y / fromIntegral (LA.size y)
      yVar  = sum (map (\v -> (v - yMean) ^ (2 :: Int)) ys)
              / fromIntegral (LA.size y)
      _     = p
  in GPParams
       { gpLengthScale  = max 0.01 (avgRng / 4)
       , gpSignalVar    = max 0.01 yVar
       , gpNoiseVar     = max 1e-4 (yVar * 0.05)
       , gpPeriod       = max 0.01 avgRng
       , gpLengthScales = Nothing
       }

-- | A GP model: a kernel paired with its hyperparameters.
data GPModel = GPModel
  { gpKernel :: Kernel
  , gpParams :: GPParams
  } deriving (Show)

-- | GP posterior-predictive result.
data GPResult = GPResult
  { gpTestX :: [Double]   -- ^ Test points @x_*@.
  , gpMean  :: [Double]   -- ^ Posterior mean @μ(x_*)@.
  , gpVar   :: [Double]   -- ^ Posterior variance @σ²(x_*)@.
  , gpLower :: [Double]   -- ^ @mean − 2σ@ (≈ 95 % credible-interval lower).
  , gpUpper :: [Double]   -- ^ @mean + 2σ@ (≈ 95 % credible-interval upper).
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- Kernel
-- ---------------------------------------------------------------------------

-- | Evaluate the kernel function @k(x, x')@.
kernelFn :: Kernel -> GPParams -> Double -> Double -> Double
kernelFn RBF p x x' =
  let d = x - x'
      l = gpLengthScale p
  in gpSignalVar p * exp (-(d * d) / (2 * l * l))
kernelFn Matern52 p x x' =
  let d = abs (x - x')
      l = gpLengthScale p
      s = sqrt 5 * d / l
  in gpSignalVar p * (1 + s + s * s / 3) * exp (-s)
kernelFn Periodic p x x' =
  let d = abs (x - x')
      l = gpLengthScale p
      s = sin (pi * d / gpPeriod p)
  in gpSignalVar p * exp (-2 * s * s / (l * l))

-- | Build the kernel matrix @K(xs, xs')@ of shape @|xs| × |xs'|@.
--
-- Phase 11b (2026-05-14): rewritten to fill a flat 'Storable.Vector'
-- via @runST + MVector@ instead of materialising the @|xs|·|xs'|@
-- lazy @[Double]@ list that the previous @(n><m) [..]@ form created.
-- The thunk-heavy list cost ~30 MB at @n=768@ purely in cons cells
-- (one allocation per kernel call) and pressured GC; the strict
-- buffer is a single allocation. 'kernelFn' itself is unchanged so
-- 'Periodic' (signed-difference dependent) keeps working.
buildKernelMatrix :: Kernel -> GPParams -> [Double] -> [Double] -> LA.Matrix Double
buildKernelMatrix ker p xs xs' =
  let xv = VS.fromList xs
      yv = VS.fromList xs'
      n  = VS.length xv
      m  = VS.length yv
      out = runST $ do
        v <- VSM.unsafeNew (n * m)
        let go !i !j
              | i >= n    = pure ()
              | j >= m    = go (i + 1) 0
              | otherwise = do
                  let xi = VS.unsafeIndex xv i
                      yj = VS.unsafeIndex yv j
                  VSM.unsafeWrite v (i * m + j) (kernelFn ker p xi yj)
                  go i (j + 1)
        go 0 0
        VS.unsafeFreeze v
  in LA.reshape m out

-- ---------------------------------------------------------------------------
-- Inference
-- ---------------------------------------------------------------------------

-- ノイズ付きカーネル行列 K_y = K(X,X) + σ_n² I を構築する（最小ジッター付き）。
noiseKernel :: Kernel -> GPParams -> [Double] -> LA.Matrix Double
noiseKernel ker p xs =
  let n      = length xs
      k      = buildKernelMatrix ker p xs xs
      jitter = max (gpNoiseVar p) 1e-6
  in k `LA.add` LA.scale jitter (LA.ident n)

-- | Log marginal likelihood @log p(y | X, θ)@. Used as the objective
-- when optimizing GP hyperparameters.
--
-- @log p = −½ yᵀ Ky⁻¹ y − ½ log|Ky| − n/2 log(2π)@.
--
-- When the parameters are pathological (e.g. very small length scales)
-- and Cholesky fails, returns the penalty value @-10³⁰@ so the
-- optimizer steers away from that region.
logMarginalLikelihood :: [Double] -> [Double] -> Kernel -> GPParams -> Double
logMarginalLikelihood trainX trainY ker params =
  let n      = length trainX
      ky     = noiseKernel ker params trainX
      y      = LA.fromList trainY
      mR = case Chol.cholFactor ky of
             Just r  -> Just (r, ky)
             Nothing ->
               -- jitter を追加して再試行
               let kyJ = ky `LA.add` LA.scale 1e-4 (LA.ident n)
               in case Chol.cholFactor kyJ of
                    Just r  -> Just (r, kyJ)
                    Nothing -> Nothing
  in case mR of
       Nothing -> -1e30
       Just (r, _kyUsed)  ->
         let logDet  = 2 * sum (map log (LA.toList (LA.takeDiag r)))
             -- Reuse the already-computed Cholesky factor (avoids a
             -- second factorization in the inner GP HP loop).
             alpha   = LA.flatten
                       (Chol.cholSolveWithFactor r (LA.asColumn y))
             dataFit = LA.dot y alpha
         in -0.5 * dataFit - 0.5 * logDet - fromIntegral n / 2 * log (2 * pi)

-- | Single-output GP posterior prediction at @testX@.
-- 多出力 'fitGPMulti' に y を 1 列行列化して委譲、列 0 を取り出す。
--
-- 事後平均: μ_* = K_*ᵀ Ky⁻¹ y
-- 事後分散: σ²_i = k(x*_i, x*_i) − K_*[i] Ky⁻¹ K_*[i]ᵀ
fitGP :: GPModel -> [Double] -> [Double] -> [Double] -> GPResult
fitGP model trainX trainY testX =
  let yMat = LA.asColumn (LA.fromList trainY)
      (meanMat, varList) = fitGPMulti model trainX yMat testX
      mu = LA.toList (LA.flatten (meanMat LA.¿ [0]))
      stdList = map sqrt varList
  in GPResult
       { gpTestX  = testX
       , gpMean   = mu
       , gpVar    = varList
       , gpLower  = zipWith (\m s -> m - 2 * s) mu stdList
       , gpUpper  = zipWith (\m s -> m + 2 * s) mu stdList
       }

-- | Multi-output GP posterior prediction. @Y@ has shape @n × q@ (one
-- column per output task) and shares a single kernel and
-- ハイパーパラメータを共有する (Cholesky / Ky⁻¹ も共有)。
--
-- 戻り値: (事後平均行列 m × q, 事後分散ベクトル 長さ m)。
-- 分散は y に依らないため q 出力で共通。
fitGPMulti :: GPModel -> [Double] -> LA.Matrix Double -> [Double]
           -> (LA.Matrix Double, [Double])
fitGPMulti model trainX trainY testX =
  let ker    = gpKernel model
      params = gpParams model
      ky     = noiseKernel ker params trainX
      kStar  = buildKernelMatrix ker params testX trainX  -- (m × n)
      -- α = Ky⁻¹ Y via SPD Cholesky (n × q)
      alpha  = Chol.cholSolveJitter ky trainY
      meanMt = kStar LA.<> alpha                          -- (m × q)
      -- v = Ky⁻¹ K_*ᵀ via the same Cholesky factor (n × m).
      -- Then var_i = k(x*_i, x*_i) − K_*[i,:] · v[:,i].
      v       = Chol.cholSolveJitter ky (LA.tr kStar)
      diagKss = [kernelFn ker params x x | x <- testX]
      -- F1: vectorise diag(kStar · v).
      kStarDotV = LA.toList (KD.diagAB kStar v)
      varList   = zipWith (\d kv -> max 0 (d - kv)) diagKss kStarDotV
  in (meanMt, varList)

-- ---------------------------------------------------------------------------
-- Hyperparameter optimisation
-- ---------------------------------------------------------------------------

-- | Optimize GP hyperparameters by maximizing the log marginal likelihood.
--
-- Operates in log-space on @(ℓ, σ_f², σ_n²)@ using L-BFGS (numerical
-- central-difference gradients, no user-provided gradient required).
--
-- Typically 5-10× faster than the older @Hanalyze.Optim.GradAscent@ + numeric
-- gradient path, and less sensitive to the initial point.
-- Internally uses 'System.IO.Unsafe.unsafePerformIO', but L-BFGS is
-- deterministic so the result is referentially transparent.
optimizeGP :: Kernel -> [Double] -> [Double] -> GPParams -> GPParams
optimizeGP ker trainX trainY p0 =
  let u0   = [log (gpLengthScale p0), log (gpSignalVar p0), log (gpNoiseVar p0)]
      -- L-BFGS は最小化なので、log-mlik を最大化したいときは Maximize 指定
      cfg  = LBFGS.defaultLBFGSConfig
               { LBFGS.lbDir   = OC.Maximize
               , LBFGS.lbStop  = OC.defaultStopCriteria
                                   { OC.stMaxIter = 200, OC.stTolFun = 1e-8 }
               }
      result = unsafePerformIO $ LBFGS.runLBFGSNumeric cfg obj u0
      uOpt   = OC.orBest result
  in p0
       { gpLengthScale = exp (uOpt !! 0)
       , gpSignalVar   = exp (uOpt !! 1)
       , gpNoiseVar    = exp (uOpt !! 2)
       }
  where
    toParams u = p0
      { gpLengthScale = exp (u !! 0)
      , gpSignalVar   = exp (u !! 1)
      , gpNoiseVar    = exp (u !! 2)
      }
    obj u = logMarginalLikelihood trainX trainY ker (toParams u)

-- ---------------------------------------------------------------------------
-- Interactive prediction data (for Hanalyze.Viz.GPReport)
-- ---------------------------------------------------------------------------

-- | JavaScript 対話予測に必要な内部データ。
-- Ky⁻¹ と α = Ky⁻¹ y を事前に計算して保持する。
data GPPredData = GPPredData
  { pdTrainX :: [Double]     -- ^ 訓練点 X
  , pdAlpha  :: [Double]     -- ^ α = Ky⁻¹ y (長さ n)
  , pdKyInv  :: [[Double]]   -- ^ Ky⁻¹ を行リストで表現 (n × n)
  } deriving (Show)

-- | 訓練データから GPPredData を計算する。
gpPredData :: GPModel -> [Double] -> [Double] -> GPPredData
gpPredData model trainX trainY =
  let ker    = gpKernel model
      params = gpParams model
      n      = length trainX
      k      = buildKernelMatrix ker params trainX trainX
      jitter = max (gpNoiseVar params) 1e-6
      ky     = addToDiag jitter k
      -- SPD: solve via Cholesky rather than 'LA.inv'. Equivalent to
      -- 'kyInv = Ky⁻¹' (used to project the JS-side prediction
      -- formula); the explicit inverse is fine here because @n@ is
      -- typically small for the interactive viewer and the inverse is
      -- consumed downstream. Cholesky is more accurate than LU.
      kyInv  = Chol.cholSolveJitter ky (LA.ident n)
      alpha  = LA.toList (kyInv LA.#> LA.fromList trainY)
  in GPPredData trainX alpha (map LA.toList (LA.toRows kyInv))

-- ---------------------------------------------------------------------------
-- Multi-input (multivariate X) API
--
-- The kernel of every supported family ('RBF', 'Matern52', 'Periodic') is a
-- function of the Euclidean distance @r = ‖x − x'‖@, so the multi-input
-- version reduces to building the @n × n@ pairwise distance matrix once
-- (via 'Hanalyze.Stat.KernelDist.pairwiseSqDist') and applying the kernel function
-- element-wise via 'LA.cmap'.
--
-- A single shared length scale @ℓ@ is used across every input dimension.
-- For axis-specific length scales, scale columns of @X@ by @1 / ℓ_d@
-- before calling these functions.
-- ---------------------------------------------------------------------------

-- | Multi-input GP posterior result. Mirrors 'GPResult' but stores the
-- @m × p@ test-point matrix instead of a 1D list.
data GPResultMV = GPResultMV
  { gpmvTestX :: LA.Matrix Double  -- ^ Test points (@m × p@).
  , gpmvMean  :: LA.Vector Double  -- ^ Posterior mean (length @m@).
  , gpmvVar   :: LA.Vector Double  -- ^ Posterior variance (length @m@).
  , gpmvLower :: LA.Vector Double  -- ^ @mean − 2σ@.
  , gpmvUpper :: LA.Vector Double  -- ^ @mean + 2σ@.
  } deriving (Show)

-- | Apply the kernel function to an @m × n@ matrix of squared distances.
-- This is the per-element work that follows BLAS distance computation.
-- F2: per-element kernel evaluation via massiv's @A.map@. Measured
-- 1.7× faster than @LA.cmap@ on 2000×2000 matrices because
-- 'A.computeAs' produces a fused tight loop while @LA.cmap@ pays
-- per-element function-call overhead. Bigger payoff at large @n@
-- (kernel matrices for Kernel/GP are the main hot path).
applyKernel :: Kernel -> GPParams -> LA.Matrix Double -> LA.Matrix Double
applyKernel RBF p d2 =
  let l2 = gpLengthScale p ** 2
      sf = gpSignalVar p
  in KD.mapMatrix (\s -> sf * exp (- s / (2 * l2))) d2
applyKernel Matern52 p d2 =
  let l  = gpLengthScale p
      sf = gpSignalVar p
  in KD.mapMatrix (\s -> let r = sqrt (max 0 s)
                             u = sqrt 5 * r / l
                         in sf * (1 + u + u * u / 3) * exp (- u)) d2
applyKernel Periodic p d2 =
  let l  = gpLengthScale p
      sf = gpSignalVar p
      pr = gpPeriod p
  in KD.mapMatrix (\s -> let r = sqrt (max 0 s)
                             ss = sin (pi * r / pr)
                         in sf * exp (- 2 * ss * ss / (l * l))) d2

-- mapMatrix / mapVector は 'Hanalyze.Stat.KernelDist' に集約 (KD.mapMatrix)。

-- | Apply ARD scaling to (X, X') if 'gpLengthScales' is 'Just'. Returns
-- the (possibly rescaled) matrices and a 'GPParams' with @ℓ = 1@ so
-- that 'applyKernel' divides by 1 (the per-dim ℓ_d already absorbed
-- into the column scaling). 'Nothing' = isotropic, returns inputs and
-- params unchanged. The 'Periodic' kernel does not support ARD.
ardScaleXY
  :: Kernel -> GPParams -> LA.Matrix Double -> LA.Matrix Double
  -> (LA.Matrix Double, LA.Matrix Double, GPParams)
ardScaleXY Periodic p x y = (x, y, p)
ardScaleXY _        p x y = case gpLengthScales p of
  Nothing -> (x, y, p)
  Just ls ->
    let p_     = LA.cols x
        lsExt  = if LA.size ls == p_
                   then ls
                   else LA.konst (gpLengthScale p) p_  -- safety fallback
        invL   = LA.cmap (1 /) lsExt                 -- 1 / ℓ_d
        scaleCols m = m LA.<> LA.diag invL
        x'     = scaleCols x
        y'     = scaleCols y
        p'     = p { gpLengthScale = 1.0 }
    in (x', y', p')

-- | Build the kernel matrix @K(X, X')@ of shape @|X| × |X'|@ from
-- multi-input matrices. @X@ is @n × p@; @X'@ is @m × p@.
--
-- When 'gpLengthScales' is 'Just', uses ARD: each input dimension is
-- scaled by @1 / ℓ_d@ before computing pairwise squared distances.
buildKernelMatrixMV
  :: Kernel -> GPParams -> LA.Matrix Double -> LA.Matrix Double
  -> LA.Matrix Double
buildKernelMatrixMV ker p x x' =
  let (xs, ys, p') = ardScaleXY ker p x x'
  in applyKernel ker p' (KD.pairwiseSqDistXY xs ys)

-- | Add a scalar @c@ to the diagonal of a square matrix in one pass.
--
-- Replaces the @M + c·I@ pattern (which allocates a fresh @n × n@
-- identity scaled by @c@). With @runST@ + flat-index update, this
-- is one allocation of the result and an in-place fill — significant
-- in 'noiseKernelMV', which is on every log-marginal-likelihood
-- evaluation.
addToDiag :: Double -> LA.Matrix Double -> LA.Matrix Double
addToDiag c m =
  let n    = LA.rows m
      flat = LA.flatten m
      out = runST $ do
        v <- VSM.new (n * n)
        let go i
              | i >= n * n = pure ()
              | otherwise  = do
                  VSM.unsafeWrite v i (flat `VS.unsafeIndex` i)
                  go (i + 1)
        go 0
        let goDiag i
              | i >= n    = pure ()
              | otherwise = do
                  let !idx = i * n + i
                  d <- VSM.unsafeRead v idx
                  VSM.unsafeWrite v idx (d + c)
                  goDiag (i + 1)
        goDiag 0
        VS.unsafeFreeze v
  in LA.reshape n out

-- | Specialized kernel function for a fixed parameter set, returning a
-- monomorphic @Double -> Double@ that GHC can inline tightly into
-- @mkNoiseKernelFromD2@s inner loop.
{-# INLINE kernelOfParams #-}
kernelOfParams :: Kernel -> GPParams -> (Double -> Double)
kernelOfParams RBF p =
  let !l2 = gpLengthScale p ** 2
      !sf = gpSignalVar p
      !inv2L2 = 1 / (2 * l2)
  in \s -> sf * exp (- s * inv2L2)
kernelOfParams Matern52 p =
  let !l  = gpLengthScale p
      !sf = gpSignalVar p
      !invL = sqrt 5 / l
  in \s -> let r = sqrt (max 0 s)
               u = invL * r
           in sf * (1 + u + u * u / 3) * exp (- u)
kernelOfParams Periodic p =
  let !l  = gpLengthScale p
      !sf = gpSignalVar p
      !pr = gpPeriod p
      !invL2 = 1 / (l * l)
      !invPr = pi / pr
  in \s -> let r  = sqrt (max 0 s)
               ss = sin (invPr * r)
           in sf * exp (- 2 * ss * ss * invL2)

-- | Build the noise-augmented kernel matrix @K + jitter·I@ in a single
-- pass over the squared-distance matrix.
--
-- Replaces the previous @applyKernel d2 |> addToDiag jitter@ pipeline,
-- which allocated /two/ @n × n@ Storable vectors per evaluation: one
-- for the kernel-applied output, one for the diagonal-augmented copy.
-- This fused version emits a single @n²@ allocation and writes each
-- cell exactly once, branching on @i == j@ to fold the jitter into the
-- diagonal write. A @noiseKernelMVCached@ call profile fraction was
-- 35.3% of @optimizeGPMV@; halving its allocation footprint translates
-- to a measurable wall-time reduction in the LBFGS hot loop.
mkNoiseKernelFromD2
  :: Kernel -> GPParams -> Double -> LA.Matrix Double -> LA.Matrix Double
mkNoiseKernelFromD2 ker p jitter d2 =
  let n     = LA.rows d2
      flatD = LA.flatten d2
      kFn   = kernelOfParams ker p
      out   = runST $ do
        v <- VSM.new (n * n)
        let go i j
              | i >= n    = pure ()
              | j >= n    = go (i + 1) 0
              | otherwise = do
                  let !idx = i * n + j
                      !s   = flatD `VS.unsafeIndex` idx
                      !kij = kFn s
                      !val = if i == j then kij + jitter else kij
                  VSM.unsafeWrite v idx val
                  go i (j + 1)
        go 0 0
        VS.unsafeFreeze v
  in LA.reshape n out

-- | Multi-input @K + σ_n² I@. Uses the fused @mkNoiseKernelFromD2@ so
-- that the kernel evaluation and jitter-on-diagonal write happen in a
-- single @n²@ pass rather than two.
noiseKernelMV :: Kernel -> GPParams -> LA.Matrix Double -> LA.Matrix Double
noiseKernelMV ker p x =
  let (xs, _, p') = ardScaleXY ker p x x
      d2          = KD.pairwiseSqDist xs
      jitter      = max (gpNoiseVar p) 1e-6
  in mkNoiseKernelFromD2 ker p' jitter d2

-- | Like 'noiseKernelMV' but reuses a pre-computed pairwise squared
-- distance matrix @D = pairwiseSqDist trainX@. Valid only when no ARD
-- scaling is applied (isotropic kernel) — the kernel is then a
-- function of @D@ alone, independent of length scale. Single-pass
-- (kernel + jitter fused).
noiseKernelMVCached
  :: Kernel -> GPParams -> LA.Matrix Double -> LA.Matrix Double
noiseKernelMVCached ker p d2 =
  let jitter = max (gpNoiseVar p) 1e-6
  in mkNoiseKernelFromD2 ker p jitter d2

-- | D-cached version of 'logMarginalLikelihoodMV' — accepts a
-- pre-computed @D = pairwiseSqDist trainX@ instead of recomputing it
-- each call. Used by 'optimizeGPMV' in the isotropic case where @D@
-- is independent of the optimization variables.
logMarginalLikelihoodMVCached
  :: LA.Matrix Double  -- ^ Pre-computed @D@ (@n × n@).
  -> LA.Vector Double  -- ^ Training @y@ (length @n@).
  -> Kernel -> GPParams -> Double
logMarginalLikelihoodMVCached d2 y ker params =
  let n   = LA.rows d2
      ky  = noiseKernelMVCached ker params d2
      mR = case Chol.cholFactor ky of
             Just r  -> Just (r, ky)
             Nothing ->
               let kyJ = addToDiag 1e-4 ky
               in case Chol.cholFactor kyJ of
                    Just r  -> Just (r, kyJ)
                    Nothing -> Nothing
  in case mR of
       Nothing -> -1e30
       Just (r, _kyUsed) ->
         let logDet  = 2 * VS.sum (VS.map log (LA.takeDiag r))
             alpha   = LA.flatten
                       (Chol.cholSolveWithFactor r (LA.asColumn y))
             dataFit = LA.dot y alpha
         in -0.5 * dataFit - 0.5 * logDet
            - fromIntegral n / 2 * log (2 * pi)

-- | Multi-input log marginal likelihood.
logMarginalLikelihoodMV
  :: LA.Matrix Double  -- ^ Training @X@ (@n × p@).
  -> LA.Vector Double  -- ^ Training @y@ (length @n@).
  -> Kernel -> GPParams -> Double
logMarginalLikelihoodMV trainX y ker params =
  let n   = LA.rows trainX
      ky  = noiseKernelMV ker params trainX
      mR = case Chol.cholFactor ky of
             Just r  -> Just (r, ky)
             Nothing ->
               let kyJ = addToDiag 1e-4 ky
               in case Chol.cholFactor kyJ of
                    Just r  -> Just (r, kyJ)
                    Nothing -> Nothing
  in case mR of
       Nothing -> -1e30
       Just (r, _kyUsed) ->
         let logDet  = 2 * VS.sum (VS.map log (LA.takeDiag r))
             alpha   = LA.flatten
                       (Chol.cholSolveWithFactor r (LA.asColumn y))
             dataFit = LA.dot y alpha
         in -0.5 * dataFit - 0.5 * logDet
            - fromIntegral n / 2 * log (2 * pi)

-- | Multi-input single-output GP posterior prediction.
fitGPMV
  :: GPModel
  -> LA.Matrix Double    -- ^ Training @X@ (@n × p@).
  -> LA.Vector Double    -- ^ Training @y@ (length @n@).
  -> LA.Matrix Double    -- ^ Test @X_*@ (@m × p@).
  -> GPResultMV
fitGPMV model trainX y testX =
  let yMat               = LA.asColumn y
      (meanMat, varVec)  = fitGPMVMulti model trainX yMat testX
      mu                 = LA.flatten (meanMat LA.¿ [0])
      stdVec             = LA.cmap sqrt varVec
  in GPResultMV
       { gpmvTestX = testX
       , gpmvMean  = mu
       , gpmvVar   = varVec
       , gpmvLower = mu - LA.scale 2 stdVec
       , gpmvUpper = mu + LA.scale 2 stdVec
       }

-- | Multi-input multi-output GP posterior prediction. @Y@ has shape
-- @n × q@ (one column per output task). The variance does not depend on
-- @y@, so a single length-@m@ vector is shared by every output.
fitGPMVMulti
  :: GPModel
  -> LA.Matrix Double    -- ^ Training @X@ (@n × p@).
  -> LA.Matrix Double    -- ^ Training @Y@ (@n × q@).
  -> LA.Matrix Double    -- ^ Test @X_*@ (@m × p@).
  -> (LA.Matrix Double, LA.Vector Double)
fitGPMVMulti model trainX trainY testX =
  let ker    = gpKernel model
      params = gpParams model
      ky     = noiseKernelMV ker params trainX
      kStar  = buildKernelMatrixMV ker params testX trainX -- m × n
      -- α = Ky⁻¹ Y via SPD Cholesky (reused for v below by passing both
      -- right-hand sides through the same factorization).
      rhs    = trainY LA.||| LA.tr kStar           -- n × (q + m)
      sol    = Chol.cholSolveJitter ky rhs         -- n × (q + m)
      q      = LA.cols trainY
      alpha  = sol LA.?? (LA.All, LA.Take q)       -- n × q
      v      = sol LA.?? (LA.All, LA.Drop q)       -- n × m
      meanMt = kStar LA.<> alpha                   -- m × q
      sf     = gpSignalVar params
      diagKss = LA.konst sf (LA.rows testX)         -- k(x*, x*) = σ_f²
      -- F1: diagonal of (kStar · v) without forming the m×m product.
      -- 'KD.diagAB' = element-wise (kStar ⊙ vᵀ) · ones.
      varVec  = LA.cmap (max 0) (diagKss - KD.diagAB kStar v)
      -- Tested split-solve (alpha and v separately via cholFactor +
      -- cholSolveWithFactor, avoiding the concat allocation) but the
      -- saving is dwarfed by the @O(n² · (q+m))@ triangular-solve
      -- work itself. Keep the simpler concatenated form.
  in (meanMt, varVec)

-- | Multi-input GP hyperparameter optimization. Mirrors 'optimizeGP' but
-- accepts a multi-input training matrix.
--
-- When @gpLengthScales p0 = Just v@, optimizes per-dim length scales
-- (ARD): the parameter vector becomes
-- @[log ℓ_1, …, log ℓ_p, log σ_f², log σ_n²]@. Otherwise optimises the
-- isotropic @[log ℓ, log σ_f², log σ_n²]@.
optimizeGPMV
  :: Kernel -> LA.Matrix Double -> LA.Vector Double -> GPParams -> GPParams
optimizeGPMV ker trainX y p0 =
  optimizeGPMVCached ker Nothing trainX y p0

-- | Like 'optimizeGPMV' but accepts a /pre-computed/ pairwise squared
-- distance matrix. Used by 'Hanalyze.Model.MultiGP' to share @D = pairwiseSqDist
-- trainX@ across all @q@ outputs (the same @trainX@ is used for every
-- output, so re-computing @D@ inside each per-output optimisation is
-- pure waste). For ARD the cache is ignored (the kernel depends on
-- per-feature length scales and @D@ varies with the optimisation
-- variables).
optimizeGPMVCached
  :: Kernel
  -> Maybe (LA.Matrix Double)   -- ^ Pre-computed @D = pairwiseSqDist trainX@.
  -> LA.Matrix Double
  -> LA.Vector Double
  -> GPParams
  -> GPParams
optimizeGPMVCached ker mPreD trainX y p0
  -- Analytic-gradient fast path for the isotropic non-ARD case under
  -- the RBF kernel. Replaces the central-difference numeric gradient
  -- (which costs 6 × the Cholesky-based log-marginal-likelihood
  -- evaluation per LBFGS step) with a closed-form formula that re-uses
  -- a single explicit @Ky⁻¹@ for all three parameters. See
  -- 'optimizeRBFAnalytic'.
  | ker == RBF && not (isARDOf p0 (LA.cols trainX)) =
      optimizeRBFAnalytic mPreD trainX y p0
  | otherwise =
  let cfg  = LBFGS.defaultLBFGSConfig
               { LBFGS.lbDir   = OC.Maximize
               , LBFGS.lbStop  = OC.defaultStopCriteria
                                   { OC.stMaxIter = 200, OC.stTolFun = 1e-8 }
               }
      u0v    = LA.fromList initU
      -- Vector-native objective: takes the LBFGS state Vector directly.
      -- Saves the list conversion that 'runLBFGSNumeric' / 'runLBFGSWith'
      -- do on every objective and gradient call.
      objV uv = obj (LA.toList uv)
      -- Central-difference gradient on the Vector representation. We
      -- experimented with forward differences (half the evaluations
      -- per gradient) but L-BFGS needed more iterations to converge
      -- under the looser O(h) error, giving a net wall-time regression.
      h    = 1e-5 :: Double
      gradV uv =
        let n = LA.size uv
        in LA.fromList
             [ let plus  = uv VS.// [(i, uv VS.! i + h)]
                   minus = uv VS.// [(i, uv VS.! i - h)]
               in (objV plus - objV minus) / (2 * h)
             | i <- [0 .. n - 1] ]
      result = unsafePerformIO $ LBFGS.runLBFGSWithV cfg objV gradV u0v
      uOpt   = OC.orBest result
  in toParams uOpt
  where
    p      = LA.cols trainX
    isARD  = case gpLengthScales p0 of
               Just v | LA.size v == p && p > 0 -> True
               _                                -> False
    -- Pre-compute the pairwise squared distance matrix for the
    -- isotropic case. The kernel of every supported family is a
    -- function of @D@ alone (length scale enters via @applyKernel@),
    -- so the LBFGS log-marginal-likelihood loop reuses @D@ instead of
    -- recomputing 'pairwiseSqDist' on every evaluation. Profile
    -- (see bench/results/) showed 'pairwiseSqDist' was 26.8% of
    -- 'optimizeGPMV' wall time before this cache.
    -- For ARD, the per-dim length scales rescale columns of @X@, so
    -- @D@ depends on the optimization variables and cannot be cached.
    cachedD :: Maybe (LA.Matrix Double)
    cachedD
      | isARD     = Nothing
      | otherwise = case mPreD of
                      Just d  -> Just d                         -- caller-supplied
                      Nothing -> Just (KD.pairwiseSqDist trainX) -- compute now
    initU
      | isARD     = case gpLengthScales p0 of
                      Just v ->
                        let ls = LA.toList v
                        in map log ls
                           ++ [log (gpSignalVar p0), log (gpNoiseVar p0)]
                      Nothing ->
                        -- Cannot happen: isARD already requires Just.
                        [ log (gpLengthScale p0)
                        , log (gpSignalVar  p0)
                        , log (gpNoiseVar   p0) ]
      | otherwise = [ log (gpLengthScale p0)
                    , log (gpSignalVar  p0)
                    , log (gpNoiseVar   p0) ]
    toParams u
      | isARD     =
          let lsV = LA.fromList (map exp (take p u))
          in p0
               { gpLengthScales = Just lsV
               , gpSignalVar    = exp (u !! p)
               , gpNoiseVar     = exp (u !! (p + 1))
               }
      | otherwise = p0
          { gpLengthScale = exp (u !! 0)
          , gpSignalVar   = exp (u !! 1)
          , gpNoiseVar    = exp (u !! 2)
          }
    -- For ARD, add a weak log-normal prior on each ℓ_d centred at the
    -- initial value (Gaussian in log-space, σ_prior = 1.5 ≈ ratio 4.5).
    -- Without it, log marginal likelihood with only 30 BO points and
    -- many ℓ_d's tends to drive ℓ_d to extreme values (over-fit). The
    -- prior is informative enough to keep ℓ_d within ~one order of
    -- magnitude of the init while still letting individual dims relax.
    obj u
      | isARD     =
          case gpLengthScales p0 of
            Just v0 ->
              let lml   = logMarginalLikelihoodMV trainX y ker (toParams u)
                  logL0 = map log (LA.toList v0)
                  sig2  = 1.5 * 1.5
                  prior = sum [ -0.5 * (l - l0) ^ (2 :: Int) / sig2
                              | (l, l0) <- zip (take p u) logL0 ]
              in lml + prior
            Nothing ->
              -- Cannot happen by isARD construction; fall back to
              -- the un-prior-ed ARD likelihood.
              logMarginalLikelihoodMV trainX y ker (toParams u)
      | otherwise =
          case cachedD of
            Just d2 -> logMarginalLikelihoodMVCached d2 y ker (toParams u)
            Nothing -> logMarginalLikelihoodMV trainX y ker (toParams u)

-- | Whether the given 'GPParams' / input dimension imply ARD.
isARDOf :: GPParams -> Int -> Bool
isARDOf p0 p = case gpLengthScales p0 of
  Just v | LA.size v == p && p > 0 -> True
  _                                -> False

-- | Analytic-gradient L-BFGS for the isotropic RBF GP marginal
-- likelihood. Replaces the central-difference numeric gradient (6 extra
-- evaluations per LBFGS step) with a closed-form formula that re-uses
-- a single explicit @Ky⁻¹@ across all three parameters
-- @[log ℓ, log σ_f², log σ_n²]@.
--
-- For RBF, @∂Ky/∂(log θ_k)@ is:
--
-- *   @log ℓ@:    @K ⊙ (D / ℓ²)@
-- *   @log σ_f²@: @K@         (linear in @σ_f²@)
-- *   @log σ_n²@: @σ_n² · I@
--
-- and the gradient contribution is
-- @½ tr((α αᵀ − Ky⁻¹) ∂Ky/∂(log θ_k))@. We form @Ky⁻¹@ once per LBFGS
-- step (@O(n³)@ via @cholSolveJitter ky I@) and assemble each
-- coordinate of the gradient via element-wise sums (@O(n²)@). Total
-- work per step: roughly @n³/2 + O(n²)@ vs the numeric path's
-- @≈ n³ + O(n²)@, plus L-BFGS converges in fewer iterations when fed
-- exact gradients.
optimizeRBFAnalytic
  :: Maybe (LA.Matrix Double) -> LA.Matrix Double -> LA.Vector Double
  -> GPParams -> GPParams
optimizeRBFAnalytic mPreD trainX y p0 =
  let n     = LA.rows trainX
      d2    = case mPreD of
                Just d  -> d
                Nothing -> KD.pairwiseSqDist trainX
      cfg   = LBFGS.defaultLBFGSConfig
                { LBFGS.lbDir   = OC.Maximize
                , LBFGS.lbStop  = OC.defaultStopCriteria
                                    { OC.stMaxIter = 200
                                    , OC.stTolFun  = 1e-8 }
                }
      u0v   = LA.fromList
                [ log (gpLengthScale p0)
                , log (gpSignalVar  p0)
                , log (gpNoiseVar   p0) ]

      -- Build the kernel matrix and noise-augmented matrix from
      -- params (re-using the precomputed @D@).
      buildK uv =
        let !ll  = exp (uv VS.! 0)        -- length scale ℓ
            !sf2 = exp (uv VS.! 1)        -- σ_f²
            !sn2 = exp (uv VS.! 2)        -- σ_n²
            !inv2L2 = 1 / (2 * ll * ll)
            !kMat = LA.cmap (\s -> sf2 * exp (- s * inv2L2)) d2
            !ky   = addToDiag sn2 kMat
        in (ll, sf2, sn2, kMat, ky)

      -- Objective only (used by L-BFGS line search).
      objV uv =
        let (_, _, _, _, ky) = buildK uv
        in case Chol.cholFactor ky of
             Nothing -> -1e30
             Just r  ->
               let logDet = 2 * VS.sum (VS.map log (LA.takeDiag r))
                   alpha  = LA.flatten
                              (Chol.cholSolveWithFactor r (LA.asColumn y))
                   dataFit = LA.dot y alpha
               in -0.5 * dataFit - 0.5 * logDet
                  - fromIntegral n / 2 * log (2 * pi)

      -- Analytic gradient.
      gradV uv =
        let (ll, _sf2, sn2, kMat, ky) = buildK uv
        in case Chol.cholFactor ky of
             Nothing -> LA.fromList [0, 0, 0]   -- bail out at singular Ky
             Just r  ->
               let alpha  = LA.flatten
                              (Chol.cholSolveWithFactor r (LA.asColumn y))
                   -- Explicit @Ky⁻¹@ (n × n). 'cholSolveWithFactor'
                   -- against the n×n identity is an @O(n³)@ pair of
                   -- triangular solves but only happens once per LBFGS
                   -- gradient call.
                   kyInv  = Chol.cholSolveWithFactor r (LA.ident n)
                   -- Q = α αᵀ − Ky⁻¹. We don't materialise this
                   -- separately; instead each gradient component is
                   -- computed as @α^T V α − tr(Ky⁻¹ V)@ inline.
                   --
                   -- ∂Ky/∂(log ℓ) = K ⊙ (D / ℓ²)
                   !invL2 = 1 / (ll * ll)
                   !vL    = LA.scale invL2 (kMat * d2)
                   !aT_vL = LA.dot alpha (vL LA.#> alpha)
                   !tr_KyInv_vL = LA.sumElements (kyInv * vL)
                   !gLogL = 0.5 * (aT_vL - tr_KyInv_vL)
                   -- ∂Ky/∂(log σ_f²) = K
                   !aT_K   = LA.dot alpha (kMat LA.#> alpha)
                   !tr_KyInv_K = LA.sumElements (kyInv * kMat)
                   !gLogSf = 0.5 * (aT_K - tr_KyInv_K)
                   -- ∂Ky/∂(log σ_n²) = σ_n² I
                   !aT_a   = LA.dot alpha alpha
                   !tr_KyInv = LA.sumElements (LA.takeDiag kyInv)
                   !gLogSn = 0.5 * sn2 * (aT_a - tr_KyInv)
               in LA.fromList [gLogL, gLogSf, gLogSn]

      result = unsafePerformIO $ LBFGS.runLBFGSWithV cfg objV gradV u0v
      uOpt   = OC.orBest result
  in p0
       { gpLengthScale = exp (uOpt !! 0)
       , gpSignalVar   = exp (uOpt !! 1)
       , gpNoiseVar    = exp (uOpt !! 2)
       }
