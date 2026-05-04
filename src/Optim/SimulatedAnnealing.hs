-- | Simulated Annealing.
--
-- Kirkpatrick, Gelatt, Vecchi (1983). A physical analogy (cooling solids):
-- a random walk with probabilistic acceptance approaches a global
-- optimum.
--
-- Acceptance probability (Metropolis criterion):
--
--   * Improvement (@Δf < 0@): always accept.
--   * Deterioration (@Δf ≥ 0@): accept with probability @exp(-Δf / T)@.
--
-- Temperature schedule: @T_k = T_0 · α^k@ (geometric cooling, with
-- @α ∈ [0.85, 0.99]@).
--
-- Proposal: add @Normal(0, sigma)@ independently per dimension and reflect
-- against the bounds.
module Optim.SimulatedAnnealing
  ( SAConfig (..)
  , SACoolingSchedule (..)
  , defaultSAConfig
  , runSA
  , runSAWith
  ) where

import Control.Monad (forM)
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWCD
import Optim.Common

-- | Cooling schedule for the SA temperature.
--
--   * 'Geometric' α — @T_{k+1} = α · T_k@ (the original Kirkpatrick form).
--   * 'Linear'    a — @T_{k+1} = T_k − a@ (rarely useful in practice).
--   * 'LundyMees' β — @T_{k+1} = T_k / (1 + β · T_k)@ (Lundy & Mees 1986;
--     spends more time at low temperatures, robust default).
--   * 'Cauchy'    — @T_k = T_0 / (1 + k)@ ("fast SA"; matches the
--     Cauchy-distributed proposal in classical analyses).
data SACoolingSchedule
  = Geometric !Double
  | Linear    !Double
  | LundyMees !Double
  | Cauchy
  deriving (Show, Eq)

-- | SA configuration.
data SAConfig = SAConfig
  { saStop      :: !StopCriteria
  , saInitTemp  :: !Double            -- ^ Initial temperature @T₀@.
  , saSchedule  :: !SACoolingSchedule -- ^ Cooling schedule.
  , saStepSigma :: !Double            -- ^ Proposal SD.
  , saStepDecay :: !Double            -- ^ Per-iteration shrink for the SD
                                      --   (1.0 leaves the SD constant).
  , saBounds    :: !Bounds            -- ^ Per-dimension bounds for reflection.
  , saDir       :: !Direction
  } deriving (Show, Eq)

-- | Default configuration: 5000 iterations, @T₀ = 1.0@, geometric
-- cooling with @α = 0.995@, proposal SD 0.5 with decay 0.999.
--
-- Geometric is empirically the best general default; switch to
-- @LundyMees 0.2@ (slower asymptotic decay, retains exploration)
-- for very multi-modal problems with large budgets, or 'Cauchy' for
-- short-budget runs (rapid cool-down).
defaultSAConfig :: [(Double, Double)] -> SAConfig
defaultSAConfig bs = SAConfig
  { saStop      = defaultStopCriteria { stMaxIter = 5000 }
  , saInitTemp  = 1.0
  , saSchedule  = Geometric 0.995
  , saStepSigma = 0.5
  , saStepDecay = 0.999
  , saBounds    = bs
  , saDir       = Minimize
  }

-- | Apply the cooling schedule to the current temperature.
nextTemp :: SACoolingSchedule -> Double -> Int -> Double -> Double
nextTemp sched t0 iter t = case sched of
  Geometric alpha -> t * alpha
  Linear    a     -> max 1e-12 (t - a)
  LundyMees beta  -> t / (1 + beta * t)
  Cauchy          -> t0 / (1 + fromIntegral (iter + 1))

-- | Run SA with the default configuration built from @bounds@.
runSA :: [(Double, Double)]
      -> ([Double] -> Double)
      -> [Double]                  -- ^ Initial point.
      -> MWC.GenIO
      -> IO OptimResult
runSA bs f x0 gen = runSAWith (defaultSAConfig bs) f x0 gen

-- | Run SA with a user-specified configuration.
runSAWith :: SAConfig
          -> ([Double] -> Double)
          -> [Double]
          -> MWC.GenIO
          -> IO OptimResult
runSAWith cfg fUser x0 gen = do
  let f    = flipFor (saDir cfg) fUser
      f0   = f x0
  go 0 x0 f0 x0 f0 (saInitTemp cfg) (saStepSigma cfg) [f0]
  where
    f = flipFor (saDir cfg) fUser

    go iter x fx xBest fBest temp sigma hist
      | iter >= stMaxIter (saStop cfg) =
          mkRes (saDir cfg) xBest fBest hist iter False
      | temp < 1e-12 =
          mkRes (saDir cfg) xBest fBest hist iter True
      | otherwise = do
          xRaw <- forM x $ \xi -> do
                    eps <- MWCD.normal 0 sigma gen
                    pure (xi + eps)
          let xCand = clipToBounds (saBounds cfg) xRaw
          let fNew = f xCand
          u <- MWC.uniformR (0, 1 :: Double) gen
          let dF = fNew - fx
              accept = dF < 0 || u < exp (- dF / temp)
              (xN, fxN)  = if accept then (xCand, fNew) else (x, fx)
              (xBN, fBN) = if fxN < fBest then (xN, fxN) else (xBest, fBest)
              tempN  = nextTemp (saSchedule cfg) (saInitTemp cfg) iter temp
              sigmaN = sigma * saStepDecay cfg
              histN  = fBN : hist
          go (iter + 1) xN fxN xBN fBN tempN sigmaN histN

mkRes :: Direction -> [Double] -> Double -> [Double]
      -> Int -> Bool -> IO OptimResult
mkRes dir xb fb hist iter conv =
  let vUser = case dir of { Minimize -> fb; Maximize -> negate fb }
      hU    = case dir of
                Minimize -> reverse hist
                Maximize -> map negate (reverse hist)
  in pure $ OptimResult xb vUser hU iter conv
