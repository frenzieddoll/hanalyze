{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | Hamiltonian Monte Carlo (HMC) sampler.
--
-- Computes exact gradients of polymorphic 'Model.HBM' models ('ModelP') via
-- 'Numeric.AD.Mode.Forward'. Constrained parameters (@PositiveT@,
-- @UnitIntervalT@) are detected automatically from the prior distribution.
--
-- @
-- import Model.HBM
-- import MCMC.HMC
--
-- myModel :: ModelP ()
-- myModel = do
--   mu    <- sample "mu"    (Normal 0 10)
--   sigma <- sample "sigma" (Exponential 1)
--   observe "y" (Normal mu sigma) [1.5, 2.0, 1.8]
--
-- chain <- hmc myModel defaultHMCConfig (Map.fromList [("mu",0),("sigma",1)]) gen
-- @
module MCMC.HMC
  ( -- * Configuration
    HMCConfig (..)
  , defaultHMCConfig
    -- * Constraint-transform helpers
  , toUnconstrainedParams
  , fromUnconstrainedParams
  , logJointU
  , leapfrogWith
  , leapfrogWithM
    -- * Basic utilities
  , kinetic
  , kineticM
  , paramsToVec
  , vecToParams
    -- * Sampler
  , hmc
  , hmcChains
  ) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (forM, replicateM, when)
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import System.Random.MWC (GenIO, uniform)
import System.Random.MWC.Distributions (standard)

import Model.HBM (ModelP, Params, sampleNames, getTransforms,
                  logJointUnconstrained, gradADU)
import MCMC.Core (Chain (..), spawnGen)
import Stat.Distribution (Transform, toUnconstrained, fromUnconstrained)

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

-- | HMC configuration.
data HMCConfig = HMCConfig
  { hmcIterations    :: Int     -- ^ Total iterations (burn-in included).
  , hmcBurnIn        :: Int     -- ^ Burn-in iterations to discard.
  , hmcStepSize      :: Double  -- ^ Leapfrog step size @ε@.
  , hmcLeapfrogSteps :: Int     -- ^ Number of leapfrog steps per HMC iteration.
  } deriving (Show)

-- | Default HMC configuration: 2000 iterations, 500 burn-in,
-- @ε = 0.1@, 10 leapfrog steps.
defaultHMCConfig :: HMCConfig
defaultHMCConfig = HMCConfig
  { hmcIterations    = 2000
  , hmcBurnIn        = 500
  , hmcStepSize      = 0.1
  , hmcLeapfrogSteps = 10
  }

-- ---------------------------------------------------------------------------
-- パラメータ変換ユーティリティ
-- ---------------------------------------------------------------------------

-- | Pack parameters into a flat vector in the given name order.
paramsToVec :: [Text] -> Params -> [Double]
paramsToVec names params = map (\n -> Map.findWithDefault 0.0 n params) names

-- | Inverse of 'paramsToVec': pair names with values.
vecToParams :: [Text] -> [Double] -> Params
vecToParams names vals = Map.fromList (zip names vals)

-- | Apply 'toUnconstrained' to every named parameter; unmapped names are
-- left untouched.
toUnconstrainedParams :: Map Text Transform -> Params -> Params
toUnconstrainedParams transforms =
  Map.mapWithKey (\k v -> maybe v (`toUnconstrained` v) (Map.lookup k transforms))

-- | Apply 'fromUnconstrained' to every named parameter.
fromUnconstrainedParams :: Map Text Transform -> Params -> Params
fromUnconstrainedParams transforms =
  Map.mapWithKey (\k u -> maybe u (`fromUnconstrained` u) (Map.lookup k transforms))

-- ---------------------------------------------------------------------------
-- unconstrained 空間での log-joint (Jacobian 補正付き)
-- ---------------------------------------------------------------------------

-- | Log-joint of a polymorphic model in the unconstrained space (shared
-- with VI and NUTS).
logJointU :: ModelP r -> Map Text Transform -> Params -> Double
logJointU model transforms paramsU =
  let names     = sampleNames model
      transList = [Map.findWithDefault errT n transforms | n <- names]
      errT      = error "logJointU: transform missing"
  in logJointUnconstrained model names transList paramsU

-- ---------------------------------------------------------------------------
-- リープフロッグ積分
-- ---------------------------------------------------------------------------

-- | Kinetic energy @0.5 ‖r‖²@ for unit-mass momentum @r@.
kinetic :: [Double] -> Double
kinetic r = 0.5 * sum (map (^ (2 :: Int)) r)

-- | Kinetic energy with a diagonal mass matrix:
-- @½ rᵀ M⁻¹ r = ½ Σ M⁻¹_ii · r_i²@.
--
-- Used by NUTS (B11) when running with diagonal mass-matrix adaptation.
-- @kinetic = kineticM (repeat 1)@ recovers the identity-mass case.
kineticM :: [Double] -> [Double] -> Double
kineticM mInv r = 0.5 * sum (zipWith (\m_inv ri -> m_inv * ri * ri) mInv r)

-- | Leapfrog integrator with a user-supplied gradient function. Takes
-- the gradient function, parameter names, step size @ε@, number of
-- steps, initial @θ@ and momentum @r@, and returns the updated pair.
leapfrogWith
  :: ([Text] -> Params -> [Double])
  -> [Text]
  -> Double
  -> Int
  -> Params
  -> [Double]
  -> (Params, [Double])
leapfrogWith gradFn names eps steps theta0 r0 = go steps theta0 r0
  where
    go 0 theta r = (theta, r)
    go n theta r =
      let g      = gradFn names theta
          rHalf  = zipWith (\ri gi -> ri - (eps / 2) * gi) r g
          tVec'  = zipWith (\ti ri -> ti + eps * ri) (paramsToVec names theta) rHalf
          theta' = vecToParams names tVec'
          g'     = gradFn names theta'
          r'     = zipWith (\ri gi -> ri - (eps / 2) * gi) rHalf g'
      in go (n - 1) theta' r'

-- | Leapfrog integrator with a diagonal mass matrix.
--
--   * Position update: @θ' = θ + ε · M⁻¹ · r@ (so smaller @M_ii@
--     ⇒ slower per-step move along that coordinate, matching the
--     intent that posterior-narrow directions get smaller steps).
--   * Momentum update: @r' = r − (ε/2) · ∇U(θ)@ (unchanged).
--
-- @leapfrogWith = leapfrogWithM (repeat 1)@.
leapfrogWithM
  :: ([Text] -> Params -> [Double])
  -> [Text]
  -> [Double]                      -- ^ Diagonal @M⁻¹@ (length = number of params).
  -> Double                        -- ^ Step size @ε@.
  -> Int                           -- ^ Number of leapfrog steps.
  -> Params
  -> [Double]
  -> (Params, [Double])
leapfrogWithM gradFn names mInv eps steps theta0 r0 = go steps theta0 r0
  where
    go 0 theta r = (theta, r)
    go n theta r =
      let g      = gradFn names theta
          rHalf  = zipWith (\ri gi -> ri - (eps / 2) * gi) r g
          -- θ' = θ + ε · M⁻¹ · r
          tVec'  = zipWith3 (\ti m_inv ri -> ti + eps * m_inv * ri)
                            (paramsToVec names theta) mInv rHalf
          theta' = vecToParams names tVec'
          g'     = gradFn names theta'
          r'     = zipWith (\ri gi -> ri - (eps / 2) * gi) rHalf g'
      in go (n - 1) theta' r'

-- ---------------------------------------------------------------------------
-- HMC サンプラー (AD 勾配版)
-- ---------------------------------------------------------------------------

-- | HMC sampler for a polymorphic HBM model ('ModelP').
--
-- Uses AD gradients ('Numeric.AD.Mode.Forward'), so it is more accurate
-- and faster than numeric differentiation. Constraint transforms are
-- detected automatically from the priors via 'getTransforms'.
hmc :: ModelP r -> HMCConfig -> Params -> GenIO -> IO Chain
hmc m cfg initC gen = do
  let names      = sampleNames m
      trMap      = getTransforms m
      transList  = [Map.findWithDefault errT n trMap | n <- names]
      errT       = error "hmc: missing transform (should not happen)"

      initU = Map.fromList
        [ (n, toUnconstrained t v)
        | (n, t) <- zip names transList
        , Just v <- [Map.lookup n initC] ]

      total = hmcBurnIn cfg + hmcIterations cfg

      logJU :: Params -> Double
      logJU paramsU = logJointUnconstrained m names transList paramsU

      gradFn :: [Text] -> Params -> [Double]
      gradFn ns paramsU =
        let xs = [Map.findWithDefault 0 n paramsU | n <- ns]
        in map negate (gradADU m names transList xs)

  samplesRef  <- newIORef []
  energyRef   <- newIORef ([] :: [Double])
  acceptedRef <- newIORef (0 :: Int)

  let step currentU = do
        r <- forM names (\_ -> standard gen)
        let h0 = -(logJU currentU) + kinetic r
            (proposedU, rFinal) =
              leapfrogWith gradFn names
                           (hmcStepSize cfg) (hmcLeapfrogSteps cfg)
                           currentU r
            logAlpha = (logJU proposedU - kinetic rFinal)
                     - (logJU currentU  - kinetic r)
        u <- uniform gen
        nextU <- if log (u :: Double) < logAlpha
          then do modifyIORef' acceptedRef (+1); return proposedU
          else return currentU
        return (nextU, h0)

  let toConstrained pu = Map.fromList
        [ (n, fromUnconstrained t (Map.findWithDefault 0 n pu))
        | (n, t) <- zip names transList ]

  let loop 0 currentU = return currentU
      loop i currentU = do
        (nextU, h0) <- step currentU
        when (i <= hmcIterations cfg) $ do
          modifyIORef' samplesRef (toConstrained nextU :)
          modifyIORef' energyRef  (h0 :)
        loop (i - 1) nextU

  _ <- loop total initU
  samples  <- fmap reverse (readIORef samplesRef)
  energies <- fmap reverse (readIORef energyRef)
  accepted <- readIORef acceptedRef
  return Chain
    { chainSamples  = samples
    , chainAccepted = accepted
    , chainTotal    = total
    , chainEnergy   = energies
    , chainDivergences = []
    }

-- | Run 'hmc' on @numChains@ parallel chains (use @+RTS -N@ for CPU
-- parallelism).
hmcChains :: ModelP r -> HMCConfig -> Int -> Params -> GenIO -> IO [Chain]
hmcChains m cfg numChains initC baseGen = do
  gens <- replicateM numChains (spawnGen baseGen)
  mapConcurrently (\g -> hmc m cfg initC g) gens
