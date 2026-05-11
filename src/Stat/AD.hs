{-# LANGUAGE RankNTypes #-}
-- | Exact gradient computation via automatic differentiation (AD), with HMC
-- integration.
--
-- Uses reverse-mode AD from @Numeric.AD@ (ekmett/ad) to compute gradients.
-- More accurate than central-difference numerical differentiation, and runs
-- at comparable speed when the parameter count is small (< 100).
--
-- == Usage
--
-- The user writes @log p(θ, y)@ as a /Floating-polymorphic/ function. Fixed
-- observation values are lifted via @realToFrac@:
--
-- @
-- import Stat.AD
-- import Stat.Distribution (Transform (..))
--
-- -- θ = [mu, sigma]
-- myLogJoint :: [Double] -> LogJointF
-- myLogJoint obs [mu, sigma] =
--   logNormalF 0 10 mu                          -- prior: μ ~ N(0,10)
--   + logExpF 1 sigma                           -- prior: σ ~ Exp(1)
--   + sum [ logNormalObsF y mu sigma | y <- obs ] -- lik
--
-- chain <- hmcAD (myLogJoint myData)
--                [UnconstrainedT, PositiveT]
--                defaultHMCConfig
--                ["mu","sigma"]
--                (Map.fromList [("mu",0),("sigma",1)])
--                gen
-- @
module Stat.AD
  ( -- * 多相対数密度関数 (log-joint 記述用)
    LogJointF
  , Params
  , logNormalF
  , logNormalObsF
  , logExpF
  , logGammaF
  , logBetaF
  , logPoissonObsF
  , logBernoulliObsF
    -- * AD-gradient computation
  , gradAD
  , gradADU
    -- * HMC (AD variant)
  , hmcAD
  , hmcADChains
  ) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (forM, replicateM)
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Numeric.AD.Mode.Forward (grad)
import System.Random.MWC (GenIO, uniform)
import System.Random.MWC.Distributions (standard)

import MCMC.Core (Chain (..), spawnGen)
import MCMC.HMC (HMCConfig (..), leapfrogWith, kinetic)
import Stat.Distribution (Transform (..), toUnconstrained, fromUnconstrained)

-- | Named parameter map (parameter name → constrained-space value).
type Params = Map.Map Text Double

-- | Type alias for a 'Floating'-polymorphic log-joint function. The
-- argument @[a]@ is the constrained-space parameter vector.
type LogJointF = forall a. Floating a => [a] -> a

-- ---------------------------------------------------------------------------
-- 多相対数密度関数
-- ---------------------------------------------------------------------------

-- | @log N(x; μ₀, σ₀)@ where @μ₀@ and @σ₀@ are fixed @Double@
-- hyperparameters and @x@ is differentiable.
logNormalF :: Floating a => Double -> Double -> a -> a
logNormalF mu0 sig0 x =
  let mu  = realToFrac mu0
      sig = realToFrac sig0
  in negate (0.5 * log (2 * pi)) - log sig - 0.5 * ((x - mu) / sig) ^ (2::Int)
{-# INLINE logNormalF #-}

-- | @log N(y_obs; μ, σ)@ where @y_obs@ is a fixed observation and @μ@,
-- @σ@ are differentiable.
logNormalObsF :: Floating a => Double -> a -> a -> a
logNormalObsF y_obs mu sig =
  let y = realToFrac y_obs
  in negate (0.5 * log (2 * pi)) - log sig - 0.5 * ((y - mu) / sig) ^ (2::Int)
{-# INLINE logNormalObsF #-}

-- | @log Exp(x; rate)@ with fixed rate.
logExpF :: Floating a => Double -> a -> a
logExpF rate0 x =
  let r = realToFrac rate0
  in log r - r * x
{-# INLINE logExpF #-}

-- | @log Gamma(x; shape, rate)@ with fixed shape and rate.
--
-- @log p(x) = (α-1) log x − β x + α log β − log Γ(α)@.
-- @log Γ(α)@ is Stirling's approximation (treated as a fixed constant).
logGammaF :: Floating a => Double -> Double -> a -> a
logGammaF shape0 rate0 x =
  let a   = realToFrac shape0
      b   = realToFrac rate0
      lgA = realToFrac (stirlingLogGamma shape0)
  in (a - 1) * log x - b * x + a * log b - lgA
{-# INLINE logGammaF #-}

-- | @log Beta(x; α, β)@ with fixed shape parameters.
-- @log p(x) = (α-1) log x + (β-1) log(1-x) − log B(α,β)@.
logBetaF :: Floating a => Double -> Double -> a -> a
logBetaF alpha0 beta0 x =
  let a   = realToFrac alpha0
      b   = realToFrac beta0
      lbB = realToFrac (stirlingLogGamma alpha0 + stirlingLogGamma beta0
                        - stirlingLogGamma (alpha0 + beta0))
  in (a - 1) * log x + (b - 1) * log (1 - x) - lbB
{-# INLINE logBetaF #-}

-- | @log Poisson(k | λ)@ with @k@ a fixed (rounded) observation and @λ@
-- differentiable.
logPoissonObsF :: Floating a => Double -> a -> a
logPoissonObsF y_obs lam =
  let k  = fromIntegral (round y_obs :: Int) :: Double
      lf = realToFrac (logFactorial (round y_obs :: Int))
  in realToFrac k * log lam - lam - lf
{-# INLINE logPoissonObsF #-}

-- | @log Bernoulli(y | p)@ with @y ∈ {0, 1}@ a fixed observation and @p@
-- differentiable.
logBernoulliObsF :: Floating a => Double -> a -> a
logBernoulliObsF y_obs p
  | y_obs > 0.5 = log p
  | otherwise   = log (1 - p)
{-# INLINE logBernoulliObsF #-}

-- ---------------------------------------------------------------------------
-- AD 勾配計算
-- ---------------------------------------------------------------------------

-- | Compute the gradient of a constrained-space log-joint via AD.
--
-- @
-- gradAD logJoint [1.0, 0.5]  -- [∂/∂θ₁, ∂/∂θ₂]
-- @
gradAD :: LogJointF -> [Double] -> [Double]
gradAD f xs = grad f xs

-- | AD gradient of the log-joint in unconstrained space (with constraint
-- transforms and Jacobian correction applied automatically).
gradADU :: LogJointF -> [Transform] -> [Double] -> [Double]
gradADU logJointC transforms us =
  grad (logJointUF transforms logJointC) us

-- ---------------------------------------------------------------------------
-- 制約変換 (Floating 多相版)
-- ---------------------------------------------------------------------------

-- | Map an unconstrained value to its constrained image
-- (Floating-polymorphic).
invTransformF :: Floating a => Transform -> a -> a
invTransformF UnconstrainedT u = u
invTransformF PositiveT      u = exp u
invTransformF UnitIntervalT  u = 1 / (1 + exp (-u))  -- sigmoid
{-# INLINE invTransformF #-}

-- | Log-Jacobian @log |∂θ/∂u|@ for one parameter (Floating-polymorphic).
logJacF :: Floating a => Transform -> a -> a
logJacF UnconstrainedT _ = 0
logJacF PositiveT      u = u                     -- log(exp u) = u
logJacF UnitIntervalT  u =
  let p = 1 / (1 + exp (-u))
  in log p + log (1 - p)                         -- log σ(u)(1−σ(u))
{-# INLINE logJacF #-}

-- | Log-joint in unconstrained space, including constraint transforms
-- and the Jacobian correction.
logJointUF :: Floating a => [Transform] -> LogJointF -> [a] -> a
logJointUF transforms logJointC us =
  let thetas = zipWith invTransformF transforms us
      logJac  = sum (zipWith logJacF transforms us)
  in logJointC thetas + logJac

-- ---------------------------------------------------------------------------
-- HMC AD 版サンプラー
-- ---------------------------------------------------------------------------

-- | HMC sampler using AD gradients.
--
-- Same algorithm as 'MCMC.HMC.hmc', but gradients are computed exactly
-- with 'Numeric.AD.grad'. The user writes the log-joint in 'LogJointF'
-- form (i.e. @Floating@-polymorphic).
hmcAD
  :: LogJointF    -- ^ @log p(θ, y)@ as a 'LogJointF' (constrained space).
  -> [Transform]  -- ^ Per-parameter constraint kind (same order as the
                  --   parameter-name list).
  -> HMCConfig
  -> [Text]       -- ^ Parameter names (matches the initial-value @Params@ keys).
  -> Params       -- ^ Initial values (constrained space).
  -> GenIO
  -> IO Chain
hmcAD logJointC transforms cfg names initC gen = do
  let total   = hmcBurnIn cfg + hmcIterations cfg
      -- Unconstrained log-joint
      logJU u = logJointUF transforms logJointC
                  [Map.findWithDefault 0 n u | n <- names]
      -- AD gradient function: leapfrogWith の規約は ∇U = -∇logπ なので符号を反転
      gradFn ns paramsU =
        let xs = [Map.findWithDefault 0 n paramsU | n <- ns]
        in map negate (grad (logJointUF transforms logJointC) xs)
      -- Initial unconstrained params
      initU = Map.fromList
        [ (n, toUnconstrained t v)
        | (n, t) <- zip names transforms
        , Just v <- [Map.lookup n initC]
        ]

  samplesRef  <- newIORef []
  acceptedRef <- newIORef (0 :: Int)

  let step currentU = do
        r <- forM names (\_ -> standard gen)
        let (proposedU, rFinal) =
              leapfrogWith gradFn names
                           (hmcStepSize cfg) (hmcLeapfrogSteps cfg)
                           currentU r
            logAlpha = (logJU proposedU - kinetic rFinal)
                     - (logJU currentU  - kinetic r)
        u <- uniform gen
        if log (u :: Double) < logAlpha
          then do modifyIORef' acceptedRef (+1); return proposedU
          else return currentU

  let loop 0 currentU = return currentU
      loop i currentU = do
        nextU <- step currentU
        when (i <= hmcIterations cfg) $
          modifyIORef' samplesRef
            (Map.fromList
               [ (n, fromUnconstrained t (Map.findWithDefault 0 n nextU))
               | (n, t) <- zip names transforms
               ] :)
        loop (i - 1) nextU

  _ <- loop total initU
  samples  <- fmap reverse (readIORef samplesRef)
  accepted <- readIORef acceptedRef
  return Chain
    { chainSamples  = samples
    , chainAccepted = accepted
    , chainTotal    = total
    , chainEnergy   = []
    , chainDivergences = []
    }
  where
    when True  action = action
    when False _      = return ()

-- | Run 'hmcAD' on @numChains@ parallel chains.
hmcADChains
  :: LogJointF
  -> [Transform]
  -> HMCConfig
  -> Int
  -> [Text]
  -> Params
  -> GenIO
  -> IO [Chain]
hmcADChains logJointC transforms cfg numChains names initC baseGen = do
  gens <- replicateM numChains (spawnGen baseGen)
  mapConcurrently (\g -> hmcAD logJointC transforms cfg names initC g) gens

-- ---------------------------------------------------------------------------
-- 数値ユーティリティ
-- ---------------------------------------------------------------------------

-- Stirling 近似による log Γ(z) — z は固定 Double ハイパーパラメータ用
stirlingLogGamma :: Double -> Double
stirlingLogGamma z
  | z < 0.5   = log pi - log (sin (pi * z)) - stirlingLogGamma (1 - z)
  | z < 12    = stirlingLogGamma (z + 1) - log z
  | otherwise = (z - 0.5) * log z - z + 0.5 * log (2 * pi)
                + 1/(12*z) - 1/(360*z^(3::Int))

logFactorial :: Int -> Double
logFactorial n = sum (map log [2 .. fromIntegral n])
