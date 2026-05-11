{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Random Fourier Features (RFF) — kernel approximation.
--
-- By Bochner's theorem, a stationary kernel
-- @k(x, x') = ∫ p(ω) e^{iω(x-x')} dω@ admits an explicit feature map
-- defined via @D@ frequencies @ω_j@ sampled from @p(ω)@ and uniform
-- phases @b_j@:
--
-- @
-- φ(x) = σ_f √(2/D) [cos(ω_j x + b_j)]_{j=1..D}
-- @
--
-- so that @k(x, x') ≈ φ(x)·φ(x')@ (Rahimi & Recht 2007).
--
-- Benefits:
--
--   * @O(n³)@ kernel computation reduces to @O(n D + D³)@ — linear in @n@.
--   * Ridge regression and GP posterior become @D@-dimensional linear
--     algebra.
--
-- This module supports both univariate and multivariate inputs (the
-- @MV@-suffixed APIs).
-- - 'sampleRFFRBF':      RBF カーネル (ω ~ N(0, 1/ℓ²))
-- - 'sampleRFFMatern52': Matérn 5/2 (ω ~ scaled t with df = 5)
-- - 'rffFeatures':  特徴行列 Φ を構築 (n × D)
-- - 'rffRidge':     RFF + Ridge 回帰 (=O(n³) Kernel Ridge の近似)
-- - 'rffGP':        RFF + ベイズ線形回帰 = GP 事後の近似 (mean + variance)
module Model.RFF
  ( RFFKernel (..)
  , RFFFeatures (..)
  , rffDim
    -- * Feature generation
  , sampleRFFRBF
  , sampleRFFMatern52
  , rffFeatures
  , rffApproxKernel
    -- * RFF ridge regression (primary API: multi-output)
  , RFFRidgeFit (..)
  , rffRidge
  , predictRFFRidge
  , RFFRidgeFitMulti (..)
  , rffRidgeMulti
  , predictRFFRidgeMulti
    -- * RFF GP (posterior mean + variance)
  , RFFGPFit (..)
  , rffGP
  , predictRFFGP
    -- * Multivariate input (@p@ dimensions)
  , RFFFeaturesMV (..)
  , sampleRFFRBFMV
  , sampleRFFMatern52MV
  , rffFeaturesMV
  , RFFRidgeFitMV (..)
  , rffRidgeMV
  , predictRFFRidgeMV
  , RFFRidgeFitMVMO (..)
  , rffRidgeMVMulti
  , predictRFFRidgeMVMulti
    -- * Marginal-likelihood maximization (auto-tune ℓ, σ_f, σ_n)
  , logMarginalLikRBFMV
  , maximizeMarginalLikRBFMV
  , maximizeMarginalLikRBFMV_DE
  , MLikResult (..)
    -- * LOOCV closed form (faster HP auto-tuning)
  , loocvRFFRidgeMV
  , gridSearchLOOCVRBFMV
  , gridSearchLOOCVRBFMV_DE
  , LOOCVResult (..)
  ) where

import Control.Exception (SomeException, try, evaluate)
import qualified Data.Vector as V
import qualified Data.Vector.Storable         as VS
import qualified Data.Vector.Storable.Mutable as VSM
import           Control.Monad.ST             (runST)
import qualified Numeric.LinearAlgebra as LA
import qualified System.IO.Unsafe
import System.IO.Unsafe (unsafePerformIO)
import qualified System.Random.MWC
import System.Random.MWC (GenIO, uniformR)
import qualified System.Random.MWC.Distributions as MWCD
import qualified Optim.DifferentialEvolution as DEM
import qualified Optim.Common as OCM
import qualified Stat.Cholesky as Chol

-- ---------------------------------------------------------------------------
-- 型
-- ---------------------------------------------------------------------------

-- | Supported kernels for RFF approximation.
data RFFKernel = RFFRBF | RFFMatern52
  deriving (Show, Eq)

-- | All the information needed to evaluate an RFF feature map.
data RFFFeatures = RFFFeatures
  { rffKernel      :: RFFKernel
  , rffOmegas      :: V.Vector Double   -- ^ Random frequencies @ω_j@ (length @D@).
  , rffBs          :: V.Vector Double   -- ^ Random phases @b_j ∈ [0, 2π)@.
  , rffSigmaF      :: Double            -- ^ Signal standard deviation @σ_f@.
  , rffLengthScale :: Double            -- ^ Length scale @ℓ@.
  } deriving (Show)

-- | Number of features @D@.
rffDim :: RFFFeatures -> Int
rffDim = V.length . rffOmegas

-- ---------------------------------------------------------------------------
-- 周波数サンプリング
-- ---------------------------------------------------------------------------

-- | Sample RFF features for the RBF kernel: @ω_j ~ N(0, 1/ℓ²)@,
-- @b_j ~ U(0, 2π)@.
sampleRFFRBF :: Int      -- ^ Feature dimension @D@.
             -> Double   -- ^ Length scale @ℓ@.
             -> Double   -- ^ Signal SD @σ_f@.
             -> GenIO -> IO RFFFeatures
sampleRFFRBF d ell sf gen = do
  ws <- V.replicateM d (MWCD.normal 0 (1/ell) gen)
  bs <- V.replicateM d (uniformR (0, 2*pi) gen)
  return RFFFeatures
    { rffKernel      = RFFRBF
    , rffOmegas      = ws
    , rffBs          = bs
    , rffSigmaF      = sf
    , rffLengthScale = ell
    }

-- | Sample RFF features for the Matérn 5/2 kernel:
-- @ω = z/√u@ where @z ~ N(0, 1/ℓ²)@ and @u ~ Gamma(ν, ν)@ with @ν = 5/2@.
-- This is a scaled @df = 5@ Student-t distribution, matching the
-- spectral density.
sampleRFFMatern52 :: Int -> Double -> Double -> GenIO -> IO RFFFeatures
sampleRFFMatern52 d ell sf gen = do
  let nu = 2.5 :: Double
  ws <- V.replicateM d $ do
    z <- MWCD.normal 0 (1/ell) gen
    -- mwc-random-distributions の gamma は (shape, scale) 渡し → mean = shape * scale
    -- Gamma(ν, 1/ν) で mean = 1
    u <- MWCD.gamma nu (1/nu) gen
    return (z / sqrt u)
  bs <- V.replicateM d (uniformR (0, 2*pi) gen)
  return RFFFeatures
    { rffKernel      = RFFMatern52
    , rffOmegas      = ws
    , rffBs          = bs
    , rffSigmaF      = sf
    , rffLengthScale = ell
    }

-- ---------------------------------------------------------------------------
-- 特徴写像
-- ---------------------------------------------------------------------------

-- | Feature matrix @Φ ∈ ℝ^{n×D}@.
-- @φ(x) = σ_f √(2/D) [cos(ω_j x + b_j)]_{j=1..D}@.
--
-- Single-pass 'runST' implementation: avoids the @[Double]@
-- list-comprehension @(n × D)@ + 'LA.fromList' round-trip the
-- previous version performed.
rffFeatures :: RFFFeatures -> [Double] -> LA.Matrix Double
rffFeatures rff xs =
  let d    = rffDim rff
      sf   = rffSigmaF rff
      coef = sf * sqrt (2 / fromIntegral d)
      -- Convert input list / boxed Vectors to Storable for fast access.
      xsV  = VS.fromList xs
      n    = VS.length xsV
      ws   = VS.fromList (V.toList (rffOmegas rff))
      bs   = VS.fromList (V.toList (rffBs     rff))
      out  = runST $ do
        v <- VSM.new (n * d)
        let go i j
              | i >= n    = pure ()
              | j >= d    = go (i + 1) 0
              | otherwise = do
                  let !x_  = xsV `VS.unsafeIndex` i
                      !w_  = ws  `VS.unsafeIndex` j
                      !b_  = bs  `VS.unsafeIndex` j
                      !val = coef * cos (w_ * x_ + b_)
                  VSM.unsafeWrite v (i * d + j) val
                  go i (j + 1)
        go 0 0
        VS.unsafeFreeze v
  in LA.reshape d out

-- | Kernel matrix approximated by RFF: @K[i,j] ≈ k(x_i, x_j) = φ(x_i)·φ(x_j)@.
rffApproxKernel :: RFFFeatures -> [Double] -> LA.Matrix Double
rffApproxKernel rff xs =
  let phi = rffFeatures rff xs
  in phi LA.<> LA.tr phi

-- ---------------------------------------------------------------------------
-- RFF Ridge 回帰
-- ---------------------------------------------------------------------------

-- | Single-output RFF ridge fit.
data RFFRidgeFit = RFFRidgeFit
  { rffrFeatures :: RFFFeatures
  , rffrWeights  :: LA.Vector Double   -- ^ Weight vector (length @D@).
  , rffrLambda   :: Double             -- ^ Ridge penalty @λ@.
  } deriving (Show)

-- | Single-output RFF ridge regression. Delegates to 'rffRidgeMulti' by
-- promoting @y@ to a one-column matrix.
rffRidge :: RFFFeatures -> [Double] -> [Double] -> Double -> RFFRidgeFit
rffRidge rff xs ys lam =
  let yMat = LA.asColumn (LA.fromList ys)
      mf   = rffRidgeMulti rff xs yMat lam
      w    = LA.flatten (rffrmWeights mf LA.¿ [0])
  in RFFRidgeFit rff w lam

-- | Predict at new inputs from a 'RFFRidgeFit'.
predictRFFRidge :: RFFRidgeFit -> [Double] -> [Double]
predictRFFRidge fit xNew =
  let phi  = rffFeatures (rffrFeatures fit) xNew
      yhat = phi LA.#> rffrWeights fit
  in LA.toList yhat

-- | Multi-output RFF ridge fit (1D inputs). @Y@ is @n × q@, weights @W@
-- are @D × q@.
data RFFRidgeFitMulti = RFFRidgeFitMulti
  { rffrmFeatures :: RFFFeatures
  , rffrmWeights  :: LA.Matrix Double   -- ^ Weight matrix (@D × q@).
  , rffrmLambda   :: Double             -- ^ Ridge penalty @λ@.
  } deriving (Show)

-- | Multi-output RFF ridge regression: @W = (ΦᵀΦ + λI)⁻¹ Φᵀ Y@.
-- SPD system; solved via Cholesky with diagonal regularizer applied
-- in place (@addToDiagRFF@).
rffRidgeMulti :: RFFFeatures -> [Double] -> LA.Matrix Double -> Double
              -> RFFRidgeFitMulti
rffRidgeMulti rff xs ys lam =
  let phi   = rffFeatures rff xs           -- n × D
      gram  = LA.tr phi LA.<> phi          -- D × D (SPD)
      regK  = addToDiagRFF lam gram
      rhs   = LA.tr phi LA.<> ys           -- D × q
      w     = Chol.cholSolveJitter regK rhs
  in RFFRidgeFitMulti rff w lam

-- | Multi-output prediction at new inputs from a 'RFFRidgeFitMulti'.
predictRFFRidgeMulti :: RFFRidgeFitMulti -> [Double] -> LA.Matrix Double
predictRFFRidgeMulti fit xNew =
  let phi = rffFeatures (rffrmFeatures fit) xNew
  in phi LA.<> rffrmWeights fit

-- ---------------------------------------------------------------------------
-- RFF GP (ベイズ線形回帰 with prior w ~ N(0, I))
-- ---------------------------------------------------------------------------

-- | Bayesian linear regression on RFF features (a Gaussian-process
-- approximation).
--
-- Prior: @w ~ N(0, I)@ (the @σ_f@ amplitude is already in the features).
--
-- Likelihood: @y = φᵀ w + ε@, @ε ~ N(0, σ_n²)@.
--
-- Posterior: @Σ⁻¹ = ΦᵀΦ / σ_n² + I@, @μ = Σ Φᵀ y / σ_n²@.
data RFFGPFit = RFFGPFit
  { rffgpFeatures :: RFFFeatures
  , rffgpSigma    :: LA.Matrix Double   -- ^ Posterior covariance @Σ@ (@D × D@).
  , rffgpMean     :: LA.Vector Double   -- ^ Posterior mean @μ@ (length @D@).
  , rffgpSigmaN   :: Double             -- ^ Observation noise SD @σ_n@.
  } deriving (Show)

-- | Fit an RFF-based Bayesian linear-regression GP.
rffGP :: RFFFeatures -> [Double] -> [Double] -> Double -> RFFGPFit
rffGP rff xs ys sigmaN =
  let phi    = rffFeatures rff xs
      d      = rffDim rff
      sigN2  = sigmaN ^ (2 :: Int)
      yV     = LA.fromList ys
      sigInv = LA.scale (1 / sigN2) (LA.tr phi LA.<> phi)
                 `LA.add` LA.ident d
      sigma  = LA.inv sigInv
      mu     = sigma LA.#> LA.scale (1 / sigN2) (LA.tr phi LA.#> yV)
  in RFFGPFit
       { rffgpFeatures = rff
       , rffgpSigma    = sigma
       , rffgpMean     = mu
       , rffgpSigmaN   = sigmaN
       }

-- | Per-test-point @(mean, variance of f)@. The observation-noise term
-- @σ_n²@ is /not/ added.
--
-- @mean = φ(x*)ᵀ μ@, @var = φ(x*)ᵀ Σ φ(x*)@.
predictRFFGP :: RFFGPFit -> [Double] -> [(Double, Double)]
predictRFFGP fit xNew =
  let rff   = rffgpFeatures fit
      phi   = rffFeatures rff xNew                  -- n_new × D
      mu    = rffgpMean fit
      sigma = rffgpSigma fit
      means = LA.toList (phi LA.#> mu)
      vars  = [ max 0 (LA.dot phi_i (sigma LA.#> phi_i))
              | phi_i <- LA.toRows phi ]
  in zip means vars

-- ---------------------------------------------------------------------------
-- 多変量入力 (p 次元) 対応 (Phase B-RFF)
-- ---------------------------------------------------------------------------

-- | Multivariate RFF feature-generation parameters. 'rffmvOmegas' is a
-- @p × D@ matrix; each column is one frequency vector @ω_j ∈ ℝ^p@.
data RFFFeaturesMV = RFFFeaturesMV
  { rffmvKernel      :: RFFKernel
  , rffmvDim         :: Int                -- ^ Input dimension @p@.
  , rffmvOmegas      :: LA.Matrix Double   -- ^ Frequencies (@p × D@).
  , rffmvBs          :: V.Vector Double    -- ^ Phases @b_j@ (length @D@).
  , rffmvSigmaF      :: Double             -- ^ Signal SD @σ_f@.
  , rffmvLengthScale :: Double             -- ^ Shared length scale @ℓ@
                                           --   (no ARD support yet).
  } deriving (Show)

-- | Sample multivariate RFF features for the RBF kernel.
-- Each component @ω_j[k] ~ N(0, 1/ℓ²)@ independently.
sampleRFFRBFMV
  :: Int -> Int -> Double -> Double -> GenIO -> IO RFFFeaturesMV
sampleRFFRBFMV p d ell sf gen = do
  let total = p * d
  ws <- V.replicateM total (MWCD.normal 0 (1/ell) gen)
  bs <- V.replicateM d (uniformR (0, 2*pi) gen)
  let omegaMat = LA.reshape d (LA.fromList (V.toList ws))
  return RFFFeaturesMV
    { rffmvKernel      = RFFRBF
    , rffmvDim         = p
    , rffmvOmegas      = omegaMat
    , rffmvBs          = bs
    , rffmvSigmaF      = sf
    , rffmvLengthScale = ell
    }

-- | Sample multivariate RFF features for the Matérn 5/2 kernel.
sampleRFFMatern52MV
  :: Int -> Int -> Double -> Double -> GenIO -> IO RFFFeaturesMV
sampleRFFMatern52MV p d ell sf gen = do
  let nu = 2.5 :: Double
  ws <- V.replicateM (p * d) $ do
    z <- MWCD.normal 0 (1/ell) gen
    u <- MWCD.gamma nu (1/nu) gen
    return (z / sqrt u)
  bs <- V.replicateM d (uniformR (0, 2*pi) gen)
  return RFFFeaturesMV
    { rffmvKernel      = RFFMatern52
    , rffmvDim         = p
    , rffmvOmegas      = LA.reshape d (LA.fromList (V.toList ws))
    , rffmvBs          = bs
    , rffmvSigmaF      = sf
    , rffmvLengthScale = ell
    }

-- | Multivariate feature matrix: @X (n × p) → Φ (n × D)@.
-- @φ_j(x) = σ_f √(2/D) cos(ω_jᵀ x + b_j)@.
--
-- Implementation: a single fused @runST + MVector@ pass writes the
-- @n × D@ output. The previous version went through
-- @LA.toRows xo + list comp (r + bs) + LA.fromRows + LA.cmap cos +
-- LA.scale coef@, allocating four @n × D@ intermediates and one list
-- of @n@ row vectors per call. This single-pass version emits one
-- @n × D@ allocation and computes
-- @coef · cos(xoFlat[i,j] + bs[j])@ in place.
rffFeaturesMV :: RFFFeaturesMV -> LA.Matrix Double -> LA.Matrix Double
rffFeaturesMV rff x =
  let d      = LA.cols (rffmvOmegas rff)
      sf     = rffmvSigmaF rff
      coef   = sf * sqrt (2 / fromIntegral d)
      -- X @ Ω → n × D (BLAS GEMM, kept).
      xo     = x LA.<> rffmvOmegas rff
      n      = LA.rows xo
      xoFlat = LA.flatten xo
      -- Phases as a Storable Vector (length D) for O(1) indexing.
      bs     = VS.fromList (V.toList (rffmvBs rff))
      out    = runST $ do
        v <- VSM.new (n * d)
        let go i j
              | i >= n    = pure ()
              | j >= d    = go (i + 1) 0
              | otherwise = do
                  let !idx = i * d + j
                      !z   = (xoFlat `VS.unsafeIndex` idx)
                           + (bs     `VS.unsafeIndex` j)
                      !val = coef * cos z
                  VSM.unsafeWrite v idx val
                  go i (j + 1)
        go 0 0
        VS.unsafeFreeze v
  in LA.reshape d out

-- | Multivariate RFF ridge fit.
data RFFRidgeFitMV = RFFRidgeFitMV
  { rffrmvFeatures :: RFFFeaturesMV
  , rffrmvWeights  :: LA.Vector Double   -- ^ Weights (length @D@).
  , rffrmvLambda   :: Double             -- ^ Ridge penalty @λ@.
  } deriving (Show)

-- | Single-output multivariate RFF ridge regression. Delegates to
-- 'rffRidgeMVMulti' by promoting @y@ to a one-column matrix.
rffRidgeMV :: RFFFeaturesMV -> LA.Matrix Double -> [Double] -> Double
           -> RFFRidgeFitMV
rffRidgeMV rff x ys lam =
  let yMat = LA.asColumn (LA.fromList ys)
      mf   = rffRidgeMVMulti rff x yMat lam
      w    = LA.flatten (rffrmvmWeights mf LA.¿ [0])
  in RFFRidgeFitMV rff w lam

-- | Predict at new inputs from a 'RFFRidgeFitMV'.
predictRFFRidgeMV :: RFFRidgeFitMV -> LA.Matrix Double -> [Double]
predictRFFRidgeMV fit xNew =
  let phi = rffFeaturesMV (rffrmvFeatures fit) xNew
  in LA.toList (phi LA.#> rffrmvWeights fit)

-- | Multivariate-input multi-output RFF ridge fit. @X@ is @n × p@,
-- @Y@ is @n × q@, weights @W@ are @D × q@.
data RFFRidgeFitMVMO = RFFRidgeFitMVMO
  { rffrmvmFeatures :: RFFFeaturesMV
  , rffrmvmWeights  :: LA.Matrix Double   -- ^ D × q
  , rffrmvmLambda   :: Double
  } deriving (Show)

-- | Multivariate-input multi-output RFF ridge regression:
-- @W = (ΦᵀΦ + λI)⁻¹ Φᵀ Y@.
--
-- The system is SPD by construction, so we solve via Cholesky rather
-- than the general LSQ path '(LA.<\>)'. The diagonal regularizer is
-- applied via @addToDiagRFF@ (in-place runST update) instead of
-- @gram + LA.scale lam (LA.ident d)@ which would allocate a fresh
-- @D × D@ identity.
rffRidgeMVMulti :: RFFFeaturesMV -> LA.Matrix Double -> LA.Matrix Double
                -> Double -> RFFRidgeFitMVMO
rffRidgeMVMulti rff x ys lam =
  let phi  = rffFeaturesMV rff x           -- n × D
      gram = LA.tr phi LA.<> phi           -- D × D (SPD)
      regK = addToDiagRFF lam gram          -- D × D
      rhs  = LA.tr phi LA.<> ys            -- D × q
      w    = Chol.cholSolveJitter regK rhs
  in RFFRidgeFitMVMO rff w lam

-- | Add a scalar to the diagonal of a square matrix in a single
-- 'runST' pass (no fresh @D × D@ identity allocation). Mirrors
-- 'Model.GP.addToDiag'; duplicated here to keep the modules
-- decoupled.
addToDiagRFF :: Double -> LA.Matrix Double -> LA.Matrix Double
addToDiagRFF c m =
  let d    = LA.rows m
      flat = LA.flatten m
      out  = runST $ do
        v <- VSM.new (d * d)
        let copy i
              | i >= d * d = pure ()
              | otherwise  = do
                  VSM.unsafeWrite v i (flat `VS.unsafeIndex` i)
                  copy (i + 1)
        copy 0
        let bumpDiag i
              | i >= d    = pure ()
              | otherwise = do
                  let !idx = i * d + i
                  d_old <- VSM.unsafeRead v idx
                  VSM.unsafeWrite v idx (d_old + c)
                  bumpDiag (i + 1)
        bumpDiag 0
        VS.unsafeFreeze v
  in LA.reshape d out

-- | Multi-output prediction at new inputs from a 'RFFRidgeFitMVMO'.
predictRFFRidgeMVMulti :: RFFRidgeFitMVMO -> LA.Matrix Double -> LA.Matrix Double
predictRFFRidgeMVMulti fit xNew =
  let phi = rffFeaturesMV (rffrmvmFeatures fit) xNew
  in phi LA.<> rffrmvmWeights fit

-- ---------------------------------------------------------------------------
-- 周辺尤度最大化 (RFF GP 流の HP チューニング、Phase 2)
-- ---------------------------------------------------------------------------

-- | Log marginal likelihood under the RBF kernel for multivariate input
-- @X@ (@n × p@) and observations @y@.
--
--   K_ij = σ_f² · exp(-‖x_i - x_j‖² / (2 ℓ²))
--   y | θ ~ N(0, K + σ_n² I)
--
--   log p(y|θ) = -½ yᵀ (K+σ_n² I)⁻¹ y - ½ log|K+σ_n² I| - n/2 log(2π)
--
-- Cholesky 分解で安定計算。ℓ が極小で K が特異化したら -∞ 近似値を返す。
logMarginalLikRBFMV
  :: LA.Matrix Double      -- ^ X (n × p)
  -> LA.Vector Double      -- ^ y (n)
  -> Double                -- ^ ℓ
  -> Double                -- ^ σ_f
  -> Double                -- ^ σ_n
  -> Double
logMarginalLikRBFMV x y ell sf sn =
  let n     = LA.rows x
      kMat  = rbfKernelMat x ell sf
      cMat  = kMat + LA.scale (sn * sn) (LA.ident n)
      -- Cholesky: cMat = Rᵀ R (R 上三角)。失敗時は jitter を加えて再試行。
      tryChol c =
        let result = unsafePerformIO $ try (evaluate (LA.chol (LA.sym c))) :: Either SomeException (LA.Matrix Double)
        in case result of
             Right r -> Just r
             Left _  -> Nothing
      mR = case tryChol cMat of
             Just r  -> Just r
             Nothing -> tryChol (cMat + LA.scale 1e-6 (LA.ident n))
  in case mR of
       Nothing -> -1e30  -- 特異 → ペナルティ
       Just r  ->
         let logDet  = 2 * sum (map log (LA.toList (LA.takeDiag r)))
             alpha   = cMat LA.<\> y
             dataFit = LA.dot y alpha
         in -0.5 * dataFit - 0.5 * logDet
            - fromIntegral n / 2 * log (2 * pi)

-- | RBF kernel matrix for inputs @X@ (@n × p@):
-- @K[i,j] = σ_f² · exp(−‖x_i − x_j‖² / (2ℓ²))@.
rbfKernelMat :: LA.Matrix Double -> Double -> Double -> LA.Matrix Double
rbfKernelMat x ell sf =
  let n     = LA.rows x
      sf2   = sf * sf
      twol2 = 2 * ell * ell
      rows  = LA.toRows x
  in LA.fromLists
       [ [ sf2 * exp (negate (LA.norm_2 (rows !! i - rows !! j) ^ (2::Int)) / twol2)
         | j <- [0 .. n-1] ]
       | i <- [0 .. n-1] ]

-- | Marginal-likelihood maximization result.
data MLikResult = MLikResult
  { mlEll      :: !Double
  , mlSigmaF   :: !Double
  , mlSigmaN   :: !Double
  , mlLogMlik  :: !Double
  , mlGridPts  :: !Int      -- ^ 評価したグリッド点数 (debug 用)
  } deriving (Show)

-- | Maximize the marginal likelihood by grid search over @(ℓ, σ_f, σ_n)@.
--
-- 戦略:
--
-- 1. ℓ は median pairwise distance を中心に log 等間隔で n_ℓ 点
-- 2. σ_f は std(y) を中心に log で n_σf 点
-- 3. σ_n は std(y)·{0.001..0.5} の log 等間隔で n_σn 点
-- 4. 全 n_ℓ × n_σf × n_σn 点で log-mlik を評価し最良を取る
-- 5. 最良点周辺で 1/3 の幅で同点数のグリッドを再探索 (1 段の coarse-to-fine)
--
-- デフォルトは (20, 8, 8) = 1280 点。最終的に 2560 点 (再探索込)。
-- n=200 までは数秒。
maximizeMarginalLikRBFMV
  :: LA.Matrix Double
  -> LA.Vector Double
  -> Maybe (Int, Int, Int)         -- ^ (n_ℓ, n_σf, n_σn). Default (20,8,8)
  -> MLikResult
maximizeMarginalLikRBFMV x y mGrid =
  let (nL, nSF, nSN) = case mGrid of
        Just g  -> g
        Nothing -> (20, 8, 8)
      yStd  = sampleStd (LA.toList y)
      ellM  = max 1e-3 (medianPairwiseDist x)
      sfM   = max 1e-6 yStd
      -- Stage 1: 広めグリッド
      ellGrid1 = logSpace (ellM * 0.05) (ellM * 20)   nL
      sfGrid1  = logSpace (sfM  * 0.25) (sfM  * 4)    nSF
      snGrid1  = logSpace (yStd * 1e-3) (yStd * 0.5)  nSN
      stage1   = bestOver x y ellGrid1 sfGrid1 snGrid1
      -- Stage 2: 最良点周辺で 1/3 幅
      (ell1, sf1, sn1, _) = stage1
      ellGrid2 = logSpace (ell1 / 3) (ell1 * 3) nL
      sfGrid2  = logSpace (sf1  / 2) (sf1  * 2) nSF
      snGrid2  = logSpace (sn1  / 3) (sn1  * 3) nSN
      stage2   = bestOver x y ellGrid2 sfGrid2 snGrid2
      (ell2, sf2, sn2, ml2) = stage2
  in MLikResult ell2 sf2 sn2 ml2
       (nL * nSF * nSN * 2)

-- | Differential-Evolution variant of 'maximizeMarginalLikRBFMV'.
--
-- coarse stage を Differential Evolution (`Optim.DifferentialEvolution`) で
-- 行い、fine stage は従来通りグリッド。
--
-- DE の探索空間は log 空間 (log_ℓ, log_σ_f, log_σ_n) の 3 次元。
-- 評価予算は generations 引数で制御 (典型 30-100 で集団 30、合計 900-3000 評価)。
-- グリッド版より広範囲を効率的に探索でき、log-mlik の局所解にハマりにくい。
maximizeMarginalLikRBFMV_DE
  :: LA.Matrix Double
  -> LA.Vector Double
  -> Int                                -- ^ DE generations
  -> System.Random.MWC.GenIO
  -> IO MLikResult
maximizeMarginalLikRBFMV_DE x y nGen gen = do
  let yStd  = sampleStd (LA.toList y)
      ellM  = max 1e-3 (medianPairwiseDist x)
      sfM   = max 1e-6 yStd
      -- log 空間の bounds (元の logSpace 範囲と一致)
      bounds =
        [ (log (ellM * 0.05),  log (ellM * 20))     -- log ℓ
        , (log (sfM  * 0.25),  log (sfM  * 4))      -- log σ_f
        , (log (yStd * 1e-3),  log (yStd * 0.5))    -- log σ_n
        ]
      -- 目的関数: log-mlik を最大化 → DE は最小化なので negate
      obj [le, lsf, lsn] = negate (logMarginalLikRBFMV x y (exp le) (exp lsf) (exp lsn))
      obj _              = 1e30
  let cfg = (DEM.defaultDEConfig bounds)
              { DEM.deStop = OCM.defaultStopCriteria { OCM.stMaxIter = nGen } }
  r <- DEM.runDEWith cfg obj gen
  let [le, lsf, lsn] = OCM.orBest r
      ell0 = exp le
      sf0  = exp lsf
      sn0  = exp lsn
      -- Stage 2 (fine grid) for refinement
      ellGrid2 = logSpace (ell0 / 3) (ell0 * 3) 8
      sfGrid2  = logSpace (sf0  / 2) (sf0  * 2) 6
      snGrid2  = logSpace (sn0  / 3) (sn0  * 3) 6
      (ell2, sf2, sn2, ml2) = bestOver x y ellGrid2 sfGrid2 snGrid2
      totalEvals = OCM.orIters r * DEM.dePopSize cfg + 8 * 6 * 6
  return $ MLikResult ell2 sf2 sn2 ml2 totalEvals

-- | Best @log p@ over the full Cartesian product of @(ellGrid, sfGrid, snGrid)@.
bestOver
  :: LA.Matrix Double -> LA.Vector Double
  -> [Double] -> [Double] -> [Double]
  -> (Double, Double, Double, Double)
bestOver x y ells sfs sns =
  let evaluations =
        [ (ell, sf, sn, logMarginalLikRBFMV x y ell sf sn)
        | ell <- ells, sf <- sfs, sn <- sns ]
      best = foldr1 (\a@(_,_,_,la) b@(_,_,_,lb) ->
                       if la >= lb then a else b) evaluations
  in best

-- | Log-spaced @n@ points between @lo@ and @hi@.
logSpace :: Double -> Double -> Int -> [Double]
logSpace lo hi n
  | n <= 1    = [lo]
  | lo <= 0   = logSpace 1e-9 hi n  -- 安全フォールバック
  | otherwise =
      let lLo = log lo
          lHi = log hi
          step = (lHi - lLo) / fromIntegral (n - 1)
      in [ exp (lLo + fromIntegral i * step) | i <- [0 .. n - 1] ]

-- | Median pairwise distance between rows (the standard median heuristic
-- for an RBF length scale).
medianPairwiseDist :: LA.Matrix Double -> Double
medianPairwiseDist x =
  let rows = LA.toRows x
      pairs = [ LA.norm_2 (rows !! i - rows !! j)
              | i <- [0 .. length rows - 1]
              , j <- [i+1 .. length rows - 1] ]
  in case pairs of
       [] -> 1.0
       _  ->
         let sorted = LA.toList (LA.fromList pairs)  -- to immutable
             sorted2 = qSort sorted
             k       = length sorted2 `div` 2
         in if null sorted2 then 1.0 else sorted2 !! k

qSort :: Ord a => [a] -> [a]
qSort []     = []
qSort (p:xs) = qSort [x | x <- xs, x <= p] ++ [p] ++ qSort [x | x <- xs, x > p]

sampleStd :: [Double] -> Double
sampleStd xs
  | length xs <= 1 = 1.0
  | otherwise =
      let n = fromIntegral (length xs)
          m = sum xs / n
          v = sum [ (x - m) * (x - m) | x <- xs ] / (n - 1)
      in if v <= 0 then 1.0 else sqrt v


-- ---------------------------------------------------------------------------
-- LOOCV 解析解 (Phase 3 — Ridge の closed-form leave-one-out cross-validation)
-- ---------------------------------------------------------------------------

-- | Result of LOOCV-based hyperparameter search.
data LOOCVResult = LOOCVResult
  { lcEll      :: !Double
  , lcSigmaF   :: !Double   -- ^ 信号 sd (= std(y) を使う簡易版)
  , lcLambda   :: !Double   -- ^ Ridge 正則化
  , lcLOOCV    :: !Double   -- ^ LOOCV(λ) = mean square LOO residual
  , lcGridPts  :: !Int
  } deriving (Show)

-- | Closed-form LOOCV for RFF ridge regression using a Cholesky
-- factorization plus the hat-matrix diagonal.
--
--   H = Φ (ΦᵀΦ + λI)⁻¹ Φᵀ
--   ŷ = H y
--   LOOCV(λ) = (1/n) Σᵢ ((y_i - ŷ_i) / (1 - H_ii))²
--
-- 本関数は与えられた特徴行列 @feats@ (= 既に ω/b/σ_f が決まったもの) と
-- Ridge λ に対して LOOCV を返す。グリッドサーチ側ではこれを多数の λ で
-- 呼び出すが、Φ は 1 度だけ計算すれば良いので外側でキャッシュする。
loocvRFFRidgeMV
  :: RFFFeaturesMV
  -> LA.Matrix Double           -- ^ X (n × p)
  -> LA.Vector Double           -- ^ y (n)
  -> Double                     -- ^ λ
  -> Double
loocvRFFRidgeMV feats x y lam =
  let phi = rffFeaturesMV feats x      -- n × D
  in loocvFromPhi phi y lam

-- | Φ から LOOCV を計算する内部実装 (グリッドサーチでキャッシュ用)。
-- Cholesky ベース (Φ_ridge = Φᵀ Φ + λI、A = chol(Φ_ridge))。
--   H = Φ Φ_ridge⁻¹ Φᵀ
--   T = Φ Φ_ridge⁻¹  → diag(H) = row-sum(T ⊙ Φ)
loocvFromPhi :: LA.Matrix Double -> LA.Vector Double -> Double -> Double
loocvFromPhi phi y lam =
  let n     = LA.rows phi
      d     = LA.cols phi
      gram  = LA.tr phi LA.<> phi             -- D × D
      regK  = gram + LA.scale lam (LA.ident d)
      -- 解析解: w = regK⁻¹ Φᵀ y
      w     = regK LA.<\> (LA.tr phi LA.#> y)
      yhat  = phi LA.#> w
      -- diag(H) = diag(Φ M Φᵀ) where M = regK⁻¹
      -- T = Φ M  (n × D)。Φ M Φᵀ の対角 = row(T) · row(Φ)
      tMat  = LA.tr (regK LA.<\> LA.tr phi)   -- T = Φ M、n × D
      hDiag = LA.fromList
                [ LA.dot (LA.flatten (tMat LA.? [i]))
                         (LA.flatten (phi  LA.? [i]))
                | i <- [0 .. n - 1] ]
      -- 1 - H_ii の極小ガード
      oneMinusH = LA.cmap (\h -> max 1e-12 (1 - h)) hDiag
      resid     = y - yhat
      ratios    = LA.toList resid `divList` LA.toList oneMinusH
      sse       = sum [ r * r | r <- ratios ]
  in sse / fromIntegral (max 1 n)
  where
    divList xs ys = zipWith (/) xs ys

-- | Search a log-spaced @(ℓ, λ)@ grid for the smallest LOOCV.
--
-- ℓ ごとに ω を新規サンプリングするため IO。グリッドサイズ default (8, 20):
-- ℓ 8 点 × λ 20 点 = 160 fit。各 fit O(n D + D³) で n=545, D=200 程度なら
-- 全体で数秒程度。
--
-- σ_f は std(y) 固定 (Ridge ↔ GP 等価では σ_f は ω 分散と一緒に動くべきだが、
-- λ で吸収できるので簡易化)。
gridSearchLOOCVRBFMV
  :: Int                               -- ^ p (入力次元)
  -> Int                               -- ^ D (特徴次元)
  -> LA.Matrix Double                  -- ^ X
  -> LA.Vector Double                  -- ^ y
  -> Maybe (Int, Int)                  -- ^ (n_ℓ, n_λ) default (8, 20)
  -> GenIO
  -> IO LOOCVResult
gridSearchLOOCVRBFMV p d x y mGrid gen = do
  let (nL, nLam) = case mGrid of { Just g -> g; Nothing -> (8, 20) }
      yStd  = sampleStd (LA.toList y)
      sf    = max 1e-9 yStd
      ellM  = max 1e-3 (medianPairwiseDist x)
      ellGrid = logSpace (ellM * 0.05) (ellM * 20)  nL
      lamGrid = logSpace (yStd * 1e-6) (yStd * 10)  nLam
  -- 各 ℓ について 1 度サンプリングしてから λ ループ
  evals <- mapM (\ell -> do
                   feats <- sampleRFFRBFMV p d ell sf gen
                   let phi = rffFeaturesMV feats x
                   let scoresAtLam = [ (ell, sf, lam, loocvFromPhi phi y lam)
                                     | lam <- lamGrid ]
                   return scoresAtLam)
                ellGrid
  let evaluations = concat evals
      best = foldr1 (\a@(_,_,_,la) b@(_,_,_,lb) ->
                       if la <= lb then a else b) evaluations
      (bEll, bSf, bLam, bL) = best
  return LOOCVResult
    { lcEll = bEll
    , lcSigmaF = bSf
    , lcLambda = bLam
    , lcLOOCV  = bL
    , lcGridPts = nL * nLam
    }

-- | Differential-Evolution variant of 'gridSearchLOOCVRBFMV'.
--
-- (log_ℓ, log_λ) の 2 次元空間を Differential Evolution で探索。
-- ω は ℓ ごとに新規サンプリング (RFF の特性上避けられない) のでコストは
-- グリッド版と同程度。グリッドの離散性が問題になる場合に有効。
gridSearchLOOCVRBFMV_DE
  :: Int                               -- ^ p (入力次元)
  -> Int                               -- ^ D (特徴次元)
  -> LA.Matrix Double                  -- ^ X
  -> LA.Vector Double                  -- ^ y
  -> Int                               -- ^ DE generations
  -> System.Random.MWC.GenIO
  -> IO LOOCVResult
gridSearchLOOCVRBFMV_DE p d x y nGen gen = do
  let yStd  = sampleStd (LA.toList y)
      sf    = max 1e-9 yStd
      ellM  = max 1e-3 (medianPairwiseDist x)
      bounds =
        [ (log (ellM * 0.05), log (ellM * 20))      -- log ℓ
        , (log (yStd * 1e-6), log (yStd * 10))      -- log λ
        ]
  -- 目的関数: log-space で受けた (log_ell, log_lam) で LOOCV を返す。
  -- ω サンプリングは IO を含むため `unsafePerformIO` を使うが、決定的シードを
  -- 内部で固定しないと毎回違う値が出る。簡略化のため: ℓ ごとに 1 度だけ
  -- サンプリングしたかったが、純粋関数化のため IO Ref キャッシュは省略。
  -- 各 DE 評価で feats を再サンプル (ノイズが入るが、実用上は最終 best 周辺で
  -- 十分平均化される)。
  --
  -- 評価をプリ計算: 候補集団のサイズ × generations 回 fresh sample。
  let cfg = (DEM.defaultDEConfig bounds)
              { DEM.deStop = OCM.defaultStopCriteria { OCM.stMaxIter = nGen } }
  -- ω サンプリング用の固定シード生成器を別途準備
  -- (DE 内のランダムは gen を共有、評価用の ω は新たに引く)
  obj <- pure $ \[le, llam] ->
    System.IO.Unsafe.unsafePerformIO $ do
      let ell = exp le
          lam = exp llam
      feats <- sampleRFFRBFMV p d ell sf gen
      let phi = rffFeaturesMV feats x
      pure (loocvFromPhi phi y lam)
  r <- DEM.runDEWith cfg obj gen
  let [le, llam] = OCM.orBest r
      bestEll = exp le
      bestLam = exp llam
      bestL   = OCM.orValue r
  return LOOCVResult
    { lcEll = bestEll
    , lcSigmaF = sf
    , lcLambda = bestLam
    , lcLOOCV  = bestL
    , lcGridPts = OCM.orIters r * DEM.dePopSize cfg
    }
