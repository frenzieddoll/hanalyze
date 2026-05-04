{-# LANGUAGE OverloadedStrings #-}
-- | Multi-output Gaussian processes.
--
-- A minimal implementation: **independent GPs**, fitting one GP per output.
-- This is the special case of the Intrinsic Coregionalization Model (ICM)
-- in which @B = I@.
--
-- More elaborate multi-task GPs (with learned cross-output correlations)
-- can be added in the future when needed. Independent GPs are sufficient
-- for Bayesian multi-objective optimization, where each acquisition
-- function is evaluated independently.
module Model.MultiGP
  ( MultiGPModel (..)
  , MultiGPResult (..)
  , mgpStd
  , fitMultiGP
  , predictMultiGP
  ) where

import Model.GP (Kernel (..), GPModel (..), GPParams, GPResult (..),
                 fitGP, optimizeGP, initParamsFromData)

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

-- | Fit a multi-output GP. Each output is optimized independently via
-- 'optimizeGP' and predicted at @testX@.
fitMultiGP :: Kernel        -- ^ Kernel kind shared by every output.
           -> [Double]      -- ^ Training inputs (1D).
           -> [[Double]]    -- ^ Per-output training values (length @q@).
           -> [Double]      -- ^ Test inputs.
           -> MultiGPResult
fitMultiGP kern trainX trainYs testX =
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
