{-# LANGUAGE OverloadedStrings #-}
-- | Multi-output Gaussian processes.
--
-- Two strategies are offered; pick by how outputs should share
-- hyperparameters:
--
--   * __Shared-HP (default)__ — @fitMultiGP@ / @fitMultiGPMV@.
--     RBF only. A /single/ HP optimisation maximises the pooled marginal
--     likelihood @Σ_q log p(y_q | θ)@, and the resulting Cholesky factor
--     of @Ky@ is reused for every output's posterior solve. Mirrors
--     scikit-learn's @GaussianProcessRegressor.fit(X, Y::(n,q))@. About
--     @q@-fold faster than the per-output variant when @q > 1@.
--
--   * __Per-output independent HPs__ — @fitMultiGPIndep@ /
--     @fitMultiGPMVIndep@. Supports any 'Kernel' kind. Each output
--     runs its own LBFGS HP fit, so per-task flexibility is preserved
--     at @q × O(LBFGS)@ cost.
--
-- Both treat outputs as independent likelihoods (@B = I@ in the
-- Intrinsic Coregionalization Model). Co-kriging / LMC kernels with
-- learned cross-output correlations are not implemented.
module Hanalyze.Model.MultiGP
  ( MultiGPModel (..)
    -- * Default (shared-HP, RBF only)
  , MultiGPResult (..)
  , mgpStd
  , fitMultiGP
  , predictMultiGP
  , MultiGPResultMV (..)
  , fitMultiGPMV
    -- * Per-output independent HPs (any kernel)
  , fitMultiGPIndep
  , fitMultiGPMVIndep
  ) where

import qualified Numeric.LinearAlgebra as LA
import Hanalyze.Model.GP (Kernel (..), GPModel (..), GPParams (..),
                 GPResult (..),
                 fitGP, optimizeGP, initParamsFromData, initParamsFromDataMV,
                 GPResultMV (..), fitGPMV, optimizeGPMVCached)
import qualified Hanalyze.Stat.KernelDist as KD
import qualified Hanalyze.Stat.Cholesky   as Chol
import qualified Hanalyze.Optim.LBFGS     as LBFGS
import qualified Hanalyze.Optim.Common    as OC
import           System.IO.Unsafe (unsafePerformIO)

-- | Multi-output GP model with a per-output set of hyperparameters.
-- All outputs share the same kernel /type/ for simplicity; their
-- length-scales etc. are still optimized independently.
data MultiGPModel = MultiGPModel
  { mgpKernel :: Kernel
  , mgpParams :: [GPParams]   -- ^ Hyperparameters per output.
  } deriving (Show)

-- | Per-output GP fit results.
data MultiGPResult = MultiGPResult
  { mgpMean   :: [[Double]]   -- ^ Predictive means, one list per output (length @q@).
  , mgpLower  :: [[Double]]   -- ^ 95 % lower band (@mean − 2σ@) per output.
  , mgpUpper  :: [[Double]]   -- ^ 95 % upper band (@mean + 2σ@) per output.
  , mgpModels :: [GPModel]    -- ^ Underlying per-output 'GPModel's.
  } deriving (Show)

-- | Recover the per-output predictive standard deviation @σ@ from the
-- @mean@ / @upper@ bands.
mgpStd :: MultiGPResult -> [[Double]]
mgpStd r = zipWith (zipWith (\m u -> (u - m) / 2)) (mgpMean r) (mgpUpper r)

-- | Fit a multi-output GP with shared RBF hyperparameters (default API).
--
-- This is the 1D-input wrapper around 'fitMultiGPMV'. A single HP set
-- is learned by maximising the pooled marginal likelihood over all
-- @q@ outputs, then one Cholesky factor of @Ky = K + σ_n² I@ is
-- reused for each output's posterior solve.
--
-- For per-output independent HPs (any kernel kind), use
-- 'fitMultiGPIndep'.
fitMultiGP :: [Double]      -- ^ Training inputs (1D).
           -> [[Double]]    -- ^ Per-output training values (length @q@).
           -> [Double]      -- ^ Test inputs.
           -> MultiGPResult
fitMultiGP trainX trainYs testX =
  let xMat   = LA.asColumn (LA.fromList trainX)
      tMat   = LA.asColumn (LA.fromList testX)
      yVecs  = map LA.fromList trainYs
      r      = fitMultiGPMV xMat yVecs tMat
  in MultiGPResult
       { mgpMean   = map LA.toList (mgpmvMean   r)
       , mgpLower  = map LA.toList (mgpmvLower  r)
       , mgpUpper  = map LA.toList (mgpmvUpper  r)
       , mgpModels = mgpmvModels r
       }

-- | Fit a multi-output GP with per-output independent hyperparameters.
--
-- Each output runs its own 'optimizeGP' LBFGS loop, then is predicted
-- at @testX@. Supports any 'Kernel' kind. Use this when outputs need
-- distinct length-scales / noise levels.
--
-- For sklearn-style shared-HP behaviour (RBF, single HP optimisation,
-- much faster when @q > 1@), use 'fitMultiGP'.
fitMultiGPIndep :: Kernel        -- ^ Kernel kind shared by every output.
                -> [Double]      -- ^ Training inputs (1D).
                -> [[Double]]    -- ^ Per-output training values (length @q@).
                -> [Double]      -- ^ Test inputs.
                -> MultiGPResult
fitMultiGPIndep kern trainX trainYs testX =
  let perOutput :: [Double] -> (GPModel, GPResult)
      perOutput trainY =
        let p0   = initParamsFromData trainX trainY
            pOpt = optimizeGP kern trainX trainY p0
            mdl  = GPModel kern pOpt
            res  = fitGP mdl trainX trainY testX
        in (mdl, res)
      pairs   = map perOutput trainYs
      models  = map fst pairs
      results = map snd pairs
  in MultiGPResult
       { mgpMean   = map gpMean   results
       , mgpLower  = map gpLower  results
       , mgpUpper  = map gpUpper  results
       , mgpModels = models
       }

-- | Re-predict an existing 'MultiGPModel' at new test inputs (no
-- re-fitting).
predictMultiGP :: MultiGPModel
               -> [Double]    -- ^ Training inputs.
               -> [[Double]]  -- ^ Per-output training values.
               -> [Double]    -- ^ Test inputs.
               -> MultiGPResult
predictMultiGP mgp trainX trainYs testX =
  let kern    = mgpKernel mgp
      models  = zipWith (\p _ -> GPModel kern p) (mgpParams mgp) trainYs
      results = zipWith3 (\m _ ty -> fitGP m trainX ty testX)
                         models trainYs trainYs
  in MultiGPResult
       { mgpMean   = map gpMean   results
       , mgpLower  = map gpLower  results
       , mgpUpper  = map gpUpper  results
       , mgpModels = models
       }

-- ---------------------------------------------------------------------------
-- Multi-input (multivariate X) API
-- ---------------------------------------------------------------------------

-- | Multi-input multi-output GP fit result. Per-output mean / band
-- vectors (length @m@), with the optimized 'GPModel' that produced them.
data MultiGPResultMV = MultiGPResultMV
  { mgpmvMean   :: [LA.Vector Double]
  , mgpmvLower  :: [LA.Vector Double]
  , mgpmvUpper  :: [LA.Vector Double]
  , mgpmvModels :: [GPModel]
  } deriving (Show)

-- | Multi-output GP fit with multivariate input and /shared/ RBF
-- hyperparameters (default API).
--
-- Mirrors @sklearn.gaussian_process.GaussianProcessRegressor@'s
-- @fit(X, Y::(n,q))@ behaviour: one HP optimisation against the
-- pooled marginal likelihood @Σ_q log p(y_q | θ)@, then a single
-- Cholesky factor of @Ky = K + σ_n² I@ reused for every output's
-- posterior solve. Roughly @q@-fold faster than 'fitMultiGPMVIndep'
-- when @q > 1@.
--
-- RBF only. For other kernels (Matérn 5/2, periodic) or per-output
-- length-scales, use 'fitMultiGPMVIndep'.
fitMultiGPMV
  :: LA.Matrix Double          -- ^ Training @X@ (@n × p@).
  -> [LA.Vector Double]        -- ^ Per-output training values (length @q@).
  -> LA.Matrix Double          -- ^ Test inputs (@m × p@).
  -> MultiGPResultMV
fitMultiGPMV trainX trainYs testX =
  let q       = length trainYs
      yMat    = LA.fromColumns trainYs                   -- n × q
      sharedD = KD.pairwiseSqDist trainX
      -- Use the first output as the reference for HP initial values
      -- (any output works; the result of the joint optimisation is
      -- the same).
      p0      = case trainYs of
                  (y0 : _) -> initParamsFromDataMV trainX y0
                  []       -> error "fitMultiGPMV: no outputs"
      pOpt    = optimizeRBFAnalyticMulti sharedD trainX yMat p0
      mdl     = GPModel RBF pOpt
      results = [ fitGPMV mdl trainX yi testX | yi <- trainYs ]
  in MultiGPResultMV
       { mgpmvMean   = map gpmvMean   results
       , mgpmvLower  = map gpmvLower  results
       , mgpmvUpper  = map gpmvUpper  results
       , mgpmvModels = replicate q mdl  -- shared model
       }

-- | Like 'Hanalyze.Model.GP.optimizeRBFAnalytic' but the marginal likelihood is
-- the /sum/ over @q@ outputs sharing one kernel — single HP fit.
--
-- Internally factor Ky once per LBFGS step, solve @α = Ky⁻¹ Y@ as one
-- @n × q@ RHS, and assemble the gradient via
-- @∇L = ½ tr((α αᵀ − q Ky⁻¹) ∂Ky/∂θ)@.
optimizeRBFAnalyticMulti
  :: LA.Matrix Double          -- ^ Pre-computed @D = pairwiseSqDist trainX@.
  -> LA.Matrix Double          -- ^ Training @X@ (used only for shape; actual
                               --   computations go through @D@).
  -> LA.Matrix Double          -- ^ @Y@ (@n × q@), one column per output.
  -> GPParams                  -- ^ Initial params.
  -> GPParams
optimizeRBFAnalyticMulti d2 trainX yMat p0 =
  let n     = LA.rows trainX
      q     = LA.cols yMat
      qD    = fromIntegral q :: Double
      cfg   = optimizerConfig
      u0v   = LA.fromList
                [ log (gpLengthScale p0)
                , log (gpSignalVar  p0)
                , log (gpNoiseVar   p0) ]

      buildK uv =
        let !ll  = exp (uv `LA.atIndex` 0)
            !sf2 = exp (uv `LA.atIndex` 1)
            !sn2 = exp (uv `LA.atIndex` 2)
            !inv2L2 = 1 / (2 * ll * ll)
            !kMat = LA.cmap (\s -> sf2 * exp (- s * inv2L2)) d2
            !kyM  = kMat + LA.scale sn2 (LA.ident n)
        in (ll, sf2, sn2, kMat, kyM)

      objV uv =
        let (_, _, _, _, kyM) = buildK uv
        in case Chol.cholFactor kyM of
             Nothing -> -1e30
             Just r  ->
               let logDet = 2 * sum (map log (LA.toList (LA.takeDiag r)))
                   alpha  = Chol.cholSolveWithFactor r yMat   -- n × q
                   -- Σ_q y_qᵀ α_q  =  trace(Yᵀ α)  =  elementwise sum (Y ⊙ α)
                   dataFit = LA.sumElements (yMat * alpha)
               in -0.5 * dataFit - 0.5 * qD * logDet
                  - fromIntegral n * qD / 2 * log (2 * pi)

      gradV uv =
        let (ll, _sf2, sn2, kMat, kyM) = buildK uv
        in case Chol.cholFactor kyM of
             Nothing -> LA.fromList [0, 0, 0]
             Just r  ->
               let alpha = Chol.cholSolveWithFactor r yMat       -- n × q
                   kyInv = Chol.cholSolveWithFactor r (LA.ident n)
                   -- Σ_q (α_qᵀ V α_q) = elementwise sum of (α ⊙ (V α))
                   sumAVA v =
                     let vAlpha = v LA.<> alpha                 -- n × q
                     in LA.sumElements (alpha * vAlpha)
                   -- ∂Ky/∂(log ℓ)
                   !invL2 = 1 / (ll * ll)
                   !vL    = LA.scale invL2 (kMat * d2)
                   !aVa_L = sumAVA vL
                   !tr_L  = LA.sumElements (kyInv * vL)
                   !gLogL = 0.5 * (aVa_L - qD * tr_L)
                   -- ∂Ky/∂(log σ_f²) = K
                   !aVa_K = sumAVA kMat
                   !tr_K  = LA.sumElements (kyInv * kMat)
                   !gLogSf = 0.5 * (aVa_K - qD * tr_K)
                   -- ∂Ky/∂(log σ_n²) = σ_n² I
                   !aVa_I = LA.sumElements (alpha * alpha)        -- ‖α‖²_F
                   !tr_I  = LA.sumElements (LA.takeDiag kyInv)
                   !gLogSn = 0.5 * sn2 * (aVa_I - qD * tr_I)
               in LA.fromList [gLogL, gLogSf, gLogSn]

      result = unsafePerformIO $ LBFGS.runLBFGSWithV cfg objV gradV u0v
      uOpt   = OC.orBest result
  in p0
       { gpLengthScale = exp (uOpt !! 0)
       , gpSignalVar   = exp (uOpt !! 1)
       , gpNoiseVar    = exp (uOpt !! 2)
       }
  where
    optimizerConfig =
      LBFGS.defaultLBFGSConfig
        { LBFGS.lbDir   = OC.Maximize
        , LBFGS.lbStop  = OC.defaultStopCriteria
                            { OC.stMaxIter = 200, OC.stTolFun = 1e-8 }
        }

-- | Multi-output GP fit with multivariate input and /independent/
-- per-output hyperparameters.
--
-- Each output column runs its own LBFGS HP optimisation via
-- @optimizeGPMVCached@. Supports any 'Kernel' kind (RBF / Matérn 5/2
-- / periodic). Costs @q × O(LBFGS)@; use 'fitMultiGPMV' (shared HP)
-- for a roughly @q@-fold speed-up when outputs are homogeneous.
--
-- The pairwise distance matrix @D = pairwiseSqDist X@ is shared
-- across the @q@ per-output optimisations to save @(q − 1) × O(n²)@
-- work.
fitMultiGPMVIndep
  :: Kernel
  -> LA.Matrix Double          -- ^ Training @X@ (@n × p@).
  -> [LA.Vector Double]        -- ^ Per-output training values (length @q@).
  -> LA.Matrix Double          -- ^ Test inputs (@m × p@).
  -> MultiGPResultMV
fitMultiGPMVIndep kern trainX trainYs testX =
  let -- @D = pairwiseSqDist trainX@ is shared across all q outputs
      -- (trainX is the same input matrix), so we compute it once and
      -- pass it into 'optimizeGPMVCached'. Each output's HP loop then
      -- re-uses the same @D@ instead of recomputing it inside its own
      -- per-output cache. Saves @(q − 1) × O(n²)@ work for kernel that
      -- uses the isotropic length scale.
      sharedD = KD.pairwiseSqDist trainX
      perOutput :: LA.Vector Double -> (GPModel, GPResultMV)
      perOutput trainY =
        let p0   = initParamsFromDataMV trainX trainY
            pOpt = optimizeGPMVCached kern (Just sharedD) trainX trainY p0
            mdl  = GPModel kern pOpt
            res  = fitGPMV mdl trainX trainY testX
        in (mdl, res)
      pairs   = map perOutput trainYs
      models  = map fst pairs
      results = map snd pairs
  in MultiGPResultMV
       { mgpmvMean   = map gpmvMean   results
       , mgpmvLower  = map gpmvLower  results
       , mgpmvUpper  = map gpmvUpper  results
       , mgpmvModels = models
       }
