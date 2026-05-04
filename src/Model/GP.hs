{-# LANGUAGE OverloadedStrings #-}
-- | Gaussian-process regression.
--
-- Pick a kernel, fit it to training data and obtain the posterior
-- predictive at arbitrary test points. Hyperparameters can be tuned
-- automatically by maximizing the log marginal likelihood.
--
-- @
-- import Model.GP
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
module Model.GP
  ( -- * カーネル型
    Kernel (..)
  , kernelName
    -- * Hyperparameters
  , GPParams (..)
  , defaultGPParams
  , initParamsFromData
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
  , logMarginalLikelihoodMV
  , fitGPMV
  , fitGPMVMulti
  , optimizeGPMV
  ) where

import Data.Text (Text)
import qualified Numeric.LinearAlgebra as LA
import qualified Optim.GradAscent
import qualified Optim.Numeric
import qualified Optim.LBFGS as LBFGS
import qualified Optim.Common as OC
import qualified Stat.KernelDist as KD
import qualified Stat.Cholesky   as Chol
import Control.Exception (SomeException, try, evaluate)
import System.IO.Unsafe (unsafePerformIO)

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
  { gpLengthScale :: Double
    -- ^ Length scale @ℓ@; larger means smoother.
  , gpSignalVar   :: Double
    -- ^ Signal variance @σ_f²@; the variability of the function values.
  , gpNoiseVar    :: Double
    -- ^ Observation noise variance @σ_n²@; near 0 interpolates, larger
    --   smooths.
  , gpPeriod      :: Double
    -- ^ Period @p@ (only used by the @Periodic@ kernel).
  } deriving (Show)

-- | Default hyperparameters: @ℓ = σ_f² = p = 1@, @σ_n² = 0.1@.
defaultGPParams :: GPParams
defaultGPParams = GPParams 1.0 1.0 0.1 1.0

-- | Build a sensible initial 'GPParams' from data statistics, suitable
-- as a starting point for optimization.
initParamsFromData :: [Double] -> [Double] -> GPParams
initParamsFromData xs ys = GPParams
  { gpLengthScale = max 0.01 ((xMax - xMin) / 4)
  , gpSignalVar   = max 0.01 yVar
  , gpNoiseVar    = max 1e-4 (yVar * 0.05)
  , gpPeriod      = max 0.01 (xMax - xMin)
  }
  where
    xMin  = minimum xs
    xMax  = maximum xs
    yMean = sum ys / fromIntegral (length ys)
    yVar  = sum (map (\y -> (y - yMean) ^ (2 :: Int)) ys) / fromIntegral (length ys)

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
buildKernelMatrix :: Kernel -> GPParams -> [Double] -> [Double] -> LA.Matrix Double
buildKernelMatrix ker p xs xs' =
  (n LA.>< m) [kernelFn ker p x x' | x <- xs, x' <- xs']
  where
    n = length xs
    m = length xs'

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
       Just (r, kyUsed)  ->
         let logDet  = 2 * sum (map log (LA.toList (LA.takeDiag r)))
             alpha   = LA.flatten
                       (Chol.cholSolveJitter kyUsed (LA.asColumn y))
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
      varList = zipWith3 (\d ks vi -> max 0 (d - LA.dot ks vi))
                  diagKss (LA.toRows kStar) (LA.toColumns v)
  in (meanMt, varList)

-- ---------------------------------------------------------------------------
-- Hyperparameter optimisation
-- ---------------------------------------------------------------------------

-- | Optimize GP hyperparameters by maximizing the log marginal likelihood.
--
-- Operates in log-space on @(ℓ, σ_f², σ_n²)@ using L-BFGS (numerical
-- central-difference gradients, no user-provided gradient required).
--
-- Typically 5-10× faster than the older @Optim.GradAscent@ + numeric
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
-- Interactive prediction data (for Viz.GPReport)
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
      ky     = k `LA.add` LA.scale jitter (LA.ident n)
      kyInv  = LA.inv ky
      alpha  = LA.toList (kyInv LA.#> LA.fromList trainY)
  in GPPredData trainX alpha (map LA.toList (LA.toRows kyInv))

-- ---------------------------------------------------------------------------
-- Multi-input (multivariate X) API
--
-- The kernel of every supported family ('RBF', 'Matern52', 'Periodic') is a
-- function of the Euclidean distance @r = ‖x − x'‖@, so the multi-input
-- version reduces to building the @n × n@ pairwise distance matrix once
-- (via 'Stat.KernelDist.pairwiseSqDist') and applying the kernel function
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
applyKernel :: Kernel -> GPParams -> LA.Matrix Double -> LA.Matrix Double
applyKernel RBF p d2 =
  let l2 = gpLengthScale p ** 2
      sf = gpSignalVar p
  in LA.cmap (\s -> sf * exp (- s / (2 * l2))) d2
applyKernel Matern52 p d2 =
  let l  = gpLengthScale p
      sf = gpSignalVar p
  in LA.cmap (\s -> let r = sqrt (max 0 s)
                        u = sqrt 5 * r / l
                    in sf * (1 + u + u * u / 3) * exp (- u)) d2
applyKernel Periodic p d2 =
  let l  = gpLengthScale p
      sf = gpSignalVar p
      pr = gpPeriod p
  in LA.cmap (\s -> let r = sqrt (max 0 s)
                        ss = sin (pi * r / pr)
                    in sf * exp (- 2 * ss * ss / (l * l))) d2

-- | Build the kernel matrix @K(X, X')@ of shape @|X| × |X'|@ from
-- multi-input matrices. @X@ is @n × p@; @X'@ is @m × p@.
buildKernelMatrixMV
  :: Kernel -> GPParams -> LA.Matrix Double -> LA.Matrix Double
  -> LA.Matrix Double
buildKernelMatrixMV ker p x x' =
  applyKernel ker p (KD.pairwiseSqDistXY x x')

-- | Multi-input @K + σ_n² I@.
noiseKernelMV :: Kernel -> GPParams -> LA.Matrix Double -> LA.Matrix Double
noiseKernelMV ker p x =
  let n      = LA.rows x
      k      = applyKernel ker p (KD.pairwiseSqDist x)
      jitter = max (gpNoiseVar p) 1e-6
  in k `LA.add` LA.scale jitter (LA.ident n)

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
               let kyJ = ky `LA.add` LA.scale 1e-4 (LA.ident n)
               in case Chol.cholFactor kyJ of
                    Just r  -> Just (r, kyJ)
                    Nothing -> Nothing
  in case mR of
       Nothing -> -1e30
       Just (r, kyUsed) ->
         let logDet  = 2 * sum (map log (LA.toList (LA.takeDiag r)))
             alpha   = LA.flatten
                       (Chol.cholSolveJitter kyUsed (LA.asColumn y))
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
      varVec  = LA.cmap (max 0)
                 (diagKss - LA.fromList
                   (zipWith LA.dot
                     (LA.toRows kStar) (LA.toColumns v)))
  in (meanMt, varVec)

-- | Multi-input GP hyperparameter optimization. Mirrors 'optimizeGP' but
-- accepts a multi-input training matrix.
optimizeGPMV
  :: Kernel -> LA.Matrix Double -> LA.Vector Double -> GPParams -> GPParams
optimizeGPMV ker trainX y p0 =
  let u0   = [log (gpLengthScale p0), log (gpSignalVar p0), log (gpNoiseVar p0)]
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
    obj u = logMarginalLikelihoodMV trainX y ker (toParams u)
