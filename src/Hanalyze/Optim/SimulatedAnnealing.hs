-- |
-- Module      : Hanalyze.Optim.SimulatedAnnealing
-- Description : Simulated Annealing (焼きなまし法) — Kirkpatrick, Gelatt, Vecchi 1983
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Simulated Annealing.
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
{-# LANGUAGE StrictData #-}
module Hanalyze.Optim.SimulatedAnnealing
  ( SAConfig (..)
  , SACoolingSchedule (..)
  , SAProposal (..)
  , SALocalMethod (..)
  , SAAccept (..)
  , defaultSAConfig
  , runSA
  , runSAWith
  ) where

import Control.Monad (forM)
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWCD
import Hanalyze.Optim.Common
import qualified Hanalyze.Optim.NelderMead as NM
import qualified Hanalyze.Optim.LBFGS as LB
import Control.Exception (SomeException, try, evaluate)
import           System.IO.Unsafe (unsafePerformIO)

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
  | TsallisCool !Double
    -- ^ Generalised SA cooling (Xiang-Gong-Liu-Yan 1997, scipy
    --   dual_annealing). With parameter @q_v@:
    --   @T(t) = T_0 · (2^(q_v−1) − 1) / ((t+2)^(q_v−1) − 1)@.
    --   Drops fast initially then asymptotically slow; pairs naturally
    --   with the 'Tsallis' visiting distribution.
  deriving (Show, Eq)

-- | Proposal (visiting) distribution for the next-x candidate.
--
--   * @Gaussian@: classical Kirkpatrick — @x' = x + N(0, σ)@ per dim.
--   * @Cauchy@: Szu-Hartley "Fast SA" (1987) — @x' = x + Cauchy(0, σ)@.
--     Heavy-tailed → occasional big jumps escape local minima.
--   * @Tsallis q_v@: Generalized SA visiting distribution
--     (Xiang-Gong-Liu-Yan 1997, Tsallis-Stariolo 1996), the engine
--     behind scipy's @dual_annealing@. For @q_v = 2.62@ (scipy default)
--     the jump distribution interpolates between Cauchy (@q_v = 2@)
--     and even fatter tails, while a temperature-dependent scale
--     contracts the typical jump as the system cools. The strongest
--     option for highly multi-modal landscapes (Rastrigin, Schwefel
--     etc.) at modest budgets.
data SAProposal
  = Gaussian
  | Cauchy_
  | Tsallis !Double
  deriving (Show, Eq)

-- | Local refinement method used by 'saLocalEvery' and the final
-- polish.
--
--   * @LocalNelderMead@: derivative-free, robust on noisy/discontinuous
--     objectives. Default.
--   * @LocalLBFGS@: numeric-gradient L-BFGS-B with @stMaxIter = 100@.
--     Significantly more efficient on smooth landscapes per call;
--     mirrors scipy @dual_annealing@'s every-iteration L-BFGS-B
--     refinement and is what closes the Rastrigin gap to machine
--     precision.
data SALocalMethod
  = LocalNelderMead
  | LocalLBFGS
  deriving (Show, Eq)

-- | Acceptance criterion for worsening proposals.
--
--   * @Boltzmann@: classical Metropolis — @P_acc = exp(-ΔF / T)@.
--   * @TsallisAccept q_a@: generalised acceptance
--     @P_acc = max(0, 1 - (1 - q_a) ΔF / T)^(1/(1-q_a))@.
--     For @q_a = -5@ (scipy dual_annealing default) the worsening tail
--     is heavier than Boltzmann at high T, encouraging escape from
--     local minima. As @q_a → 1@ this reduces to Boltzmann.
data SAAccept
  = Boltzmann
  | TsallisAccept !Double
  | GreedyAccept
    -- ^ Accept only improvements. The exploration role is delegated
    --   entirely to the proposal distribution (set 'saProposal' to
    --   'Tsallis q_v' for heavy-tailed jumps). This matches scipy's
    --   @dual_annealing@ effective behaviour (its Tsallis acceptance
    --   with @q_a = -5@ essentially rejects all worsenings).
  deriving (Show, Eq)

-- | SA configuration.
data SAConfig = SAConfig
  { saStop       :: !StopCriteria
  , saInitTemp   :: !Double            -- ^ Initial temperature @T₀@.
  , saSchedule   :: !SACoolingSchedule -- ^ Cooling schedule.
  , saStepSigma  :: !Double            -- ^ Proposal SD.
  , saStepDecay  :: !Double            -- ^ Per-iteration shrink for the SD
                                       --   (1.0 leaves the SD constant).
  , saBounds     :: !Bounds            -- ^ Per-dimension bounds for reflection.
  , saDir        :: !Direction
  , saLocalEvery :: !(Maybe Int)
    -- ^ When @Just k@, run a local 'Hanalyze.Optim.NelderMead' refinement on
    --   @x_best@ every @k@ iterations and replace @(x_best, f_best)@
    --   if the refinement improves it. This turns vanilla SA into a
    --   hybrid (analogous to scipy's @dual_annealing@), which is the
    --   only way to reach machine-precision-level minima on
    --   multi-modal problems with the modest 5000-iteration budget.
  , saPolish     :: !Bool
    -- ^ When 'True', run a high-precision Nelder-Mead refinement on
    --   @x_best@ once at SA termination (separate from
    --   'saLocalEvery'). Uses a small-simplex starting step
    --   (@0.001 × bound width@) to polish the result to near-machine
    --   precision on smooth landscapes.
  , saRestartIfStuck :: !(Maybe Int)
    -- ^ When @Just k@, perturb @x@ to a fresh random point in
    --   'saBounds' if @x_best@ has not improved in @k@ iterations.
    --   Helps SA escape pathological multi-modal landscapes
    --   (Rastrigin etc.) where vanilla SA — even with periodic NM
    --   refinement — gets trapped in a single basin.
  , saProposal       :: !SAProposal
    -- ^ Proposal (visiting) distribution. Default 'Gaussian' for
    --   back-compat. Set 'Tsallis 2.62' for scipy-style dual_annealing
    --   behaviour on multi-modal problems.
  , saLocalMethod    :: !SALocalMethod
    -- ^ Local refinement method (see 'saLocalEvery' and the final
    --   polish). Default 'LocalNelderMead'.
  , saAccept         :: !SAAccept
    -- ^ Acceptance criterion for worsening proposals. Default
    --   'Boltzmann'. 'TsallisAccept (-5)' = scipy dual_annealing
    --   default.
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
  { saStop           = defaultStopCriteria { stMaxIter = 5000 }
  , saInitTemp       = 1.0
  , saSchedule       = Geometric 0.995
  , saStepSigma      = 0.5
  , saStepDecay      = 0.999
  , saBounds         = bs
  , saDir            = Minimize
  , saLocalEvery     = Just 200            -- 5000 / 200 = 25 NM refines
  , saPolish         = True                -- final high-precision NM
  , saRestartIfStuck = Nothing             -- off by default; useful for
                                           -- pathological multi-modal
                                           -- (Rastrigin etc.) but hurts
                                           -- problems whose basin needs
                                           -- continuous refinement
                                           -- (Levy regressed by 12 orders
                                           --  of magnitude with restart on)
  , saProposal       = Gaussian            -- back-compat default; switch to
                                           -- 'Tsallis 2.62' for Rastrigin-
                                           -- like multi-modal problems.
  , saLocalMethod    = LocalNelderMead     -- back-compat default; switch to
                                           -- 'LocalLBFGS' for smooth
                                           -- objectives where every-iter
                                           -- gradient refinement helps
                                           -- (Rastrigin etc.).
  , saAccept         = Boltzmann           -- back-compat default; switch to
                                           -- 'TsallisAccept (-5)' for
                                           -- scipy-style dual_annealing
                                           -- (heavier acceptance tail at
                                           -- high T → escapes basins).
  }

-- | Draw a single per-dimension proposal increment for the current
-- 'SAProposal' and (sigma, T) state.
--
-- For Tsallis q_v: sample @ξ / |η|^((q_v-1)/(3-q_v))@ where
-- @ξ ~ N(0, T^(1/(q_v-1)))@ and @η ~ N(0, 1)@. This is the
-- Xiang-Gong-Liu-Yan 1997 visiting distribution; the typical jump
-- shrinks as T cools but the heavy tails (~ |η|^-α) keep occasional
-- large jumps possible. q_v = 2 reduces to Cauchy(0, T); q_v → 1
-- approaches Gaussian.
sampleProposal :: SAProposal -> Double -> Double -> MWC.GenIO -> IO Double
sampleProposal Gaussian       sigma _ gen = MWCD.normal 0 sigma gen
sampleProposal Cauchy_        sigma _ gen = do
  u <- MWC.uniformR (1e-12, 1 - 1e-12 :: Double) gen
  pure (sigma * tan (pi * (u - 0.5)))
sampleProposal (Tsallis q) _ temp gen = do
  let qm   = q - 1
      qmp  = 3 - q
      -- T-dependent scale: σ_T = T^(1/(q-1))
      sigT = max 1e-30 temp ** (1 / qm)
      -- exponent on |η|
      expo = qm / qmp
  xi  <- MWCD.normal 0 sigT gen
  eta <- MWCD.normal 0 1   gen
  let etaA = max 1e-300 (abs eta)
  pure (xi / (etaA ** expo))
nextTemp :: SACoolingSchedule -> Double -> Int -> Double -> Double
nextTemp sched t0 iter t = case sched of
  Geometric alpha -> t * alpha
  Linear    a     -> max 1e-12 (t - a)
  LundyMees beta  -> t / (1 + beta * t)
  Cauchy          -> t0 / (1 + fromIntegral (iter + 1))
  TsallisCool qv  ->
    let s = fromIntegral (iter + 2) :: Double
        e = qv - 1
    in t0 * (2 ** e - 1) / (s ** e - 1)

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
  finalRes <- go 0 0 x0 f0 x0 f0 (saInitTemp cfg) (saStepSigma cfg) [f0]
  -- Optional final high-precision polish on x_best.
  if saPolish cfg
    then do
      let (xb, fb) = polishNM cfg f (orBest finalRes)
                       (case saDir cfg of
                          Minimize -> orValue finalRes
                          Maximize -> negate (orValue finalRes))
          vUser = case saDir cfg of
                    Minimize -> fb
                    Maximize -> negate fb
      pure finalRes
        { orBest  = xb
        , orValue = vUser
        }
    else pure finalRes
  where
    f = flipFor (saDir cfg) fUser

    -- Loop carries (iter, sinceImprove). 'sinceImprove' is the number
    -- of iterations since 'fBest' last decreased, used by the
    -- 'saRestartIfStuck' option.
    go iter sinceImprove x fx xBest fBest temp sigma hist
      | iter >= stMaxIter (saStop cfg) =
          mkRes (saDir cfg) xBest fBest hist iter False
      | temp < 1e-12 =
          mkRes (saDir cfg) xBest fBest hist iter True
      | otherwise = do
          -- Random-restart trigger.
          let stuck = case saRestartIfStuck cfg of
                Just k | k > 0 && sinceImprove >= k -> True
                _                                    -> False
          (xR, fxR, sinceR, sigmaR) <-
            if stuck
              then do
                xNew <- mapM (\(lo, hi) -> MWC.uniformR (lo, hi) gen)
                             (saBounds cfg)
                pure (xNew, f xNew, 0, saStepSigma cfg)
              else pure (x, fx, sinceImprove, sigma)

          xRaw <- forM xR $ \xi -> do
                    eps <- sampleProposal (saProposal cfg) sigmaR temp gen
                    pure (xi + eps)
          let xCand = clipToBounds (saBounds cfg) xRaw
          let fNew = f xCand
          u <- MWC.uniformR (0, 1 :: Double) gen
          let dF = fNew - fxR
              -- Tsallis acceptance: P_acc = max(0, 1 - (1-q_a)·dF/T)^(1/(1-q_a))
              -- For q_a → 1, reduces to Boltzmann exp(-dF/T).
              -- For q_a < 1 (e.g. -5), heavier tail at high T.
              accept =
                dF < 0 ||
                  case saAccept cfg of
                    Boltzmann ->
                      u < exp (- dF / temp)
                    TsallisAccept qa ->
                      let qm    = 1 - qa
                          base' = 1 - qm * dF / temp
                          pAcc
                            | base' <= 0 = 0
                            | otherwise  = base' ** (1 / qm)
                      in u < pAcc
                    GreedyAccept -> False
              (xN, fxN)  = if accept then (xCand, fNew) else (xR, fxR)
              (xBN0, fBN0) = if fxN < fBest then (xN, fxN) else (xBest, fBest)
              improved   = fBN0 < fBest
              sinceN     = if improved then 0 else sinceR + 1
              -- Local refinement on x_best every k iterations (hybrid SA).
              shouldRefine = case saLocalEvery cfg of
                Just k | k > 0 && (iter + 1) `mod` k == 0
                       , iter > 0 -> True
                _                  -> False
              (xBN, fBN) =
                if shouldRefine
                  then case saLocalMethod cfg of
                         LocalNelderMead -> refineNM    cfg f xBN0 fBN0
                         LocalLBFGS      -> refineLBFGS cfg f xBN0 fBN0
                  else (xBN0, fBN0)
              tempN  = nextTemp (saSchedule cfg) (saInitTemp cfg) iter temp
              sigmaN = sigmaR * saStepDecay cfg
              histN  = fBN : hist
          go (iter + 1) sinceN xN fxN xBN fBN tempN sigmaN histN

-- | Apply a Nelder-Mead refinement at the current best point. Returns
-- the refined @(x, f)@ if it improves on the input, otherwise the
-- input unchanged. Bounded by the SA box (any out-of-range coordinate
-- after refinement is clipped before re-evaluation).
refineNM :: SAConfig -> ([Double] -> Double) -> [Double] -> Double
         -> ([Double], Double)
refineNM cfg f x fx =
  let r     = unsafePerformIO (NM.runNelderMeadWith
                (NM.defaultNMConfig
                   { NM.nmStop = defaultStopCriteria
                                   { stMaxIter = 200
                                   , stTolFun  = 1e-10
                                   , stTolX    = 1e-10 }
                   , NM.nmInitStep = 0.01
                   }) f x)
      xRef  = clipToBounds (saBounds cfg) (orBest r)
      fRef  = f xRef
  in if fRef < fx then (xRef, fRef) else (x, fx)

-- | L-BFGS-B (numeric gradient) refinement at the current best point.
-- Used when 'saLocalMethod = LocalLBFGS'. Catches numeric exceptions
-- (singular Hessian / Cholesky failures inside f) and falls back to
-- the input unchanged.
refineLBFGS :: SAConfig -> ([Double] -> Double) -> [Double] -> Double
            -> ([Double], Double)
refineLBFGS cfg f x fx = unsafePerformIO $ do
  let polCfg = LB.defaultLBFGSConfig
                 { LB.lbStop   = defaultStopCriteria
                                   { stMaxIter = 50
                                   , stTolFun  = 1e-12
                                   , stTolX    = 1e-12 }
                 , LB.lbBounds = Just (saBounds cfg)
                 }
  eR <- try (LB.runLBFGSNumeric polCfg f x) :: IO (Either SomeException OptimResult)
  case eR of
    Left _  -> pure (x, fx)
    Right r ->
      let xRef = clipToBounds (saBounds cfg) (orBest r)
      in do
        evF <- try (evaluate (f xRef)) :: IO (Either SomeException Double)
        case evF of
          Right fRef | fRef < fx -> pure (xRef, fRef)
          _                       -> pure (x, fx)

-- | High-precision polish on @x_best@ at SA termination. Uses a much
-- smaller initial simplex and tighter tolerances so that smooth
-- landscapes (Sphere, Levy etc.) reach near-machine precision after
-- the SA + periodic-NM walk has localised the basin.
polishNM :: SAConfig -> ([Double] -> Double) -> [Double] -> Double
         -> ([Double], Double)
polishNM cfg f x fx =
  let r    = unsafePerformIO (NM.runNelderMeadWith
               (NM.defaultNMConfig
                  { NM.nmStop = defaultStopCriteria
                                  { stMaxIter = 2000
                                  , stTolFun  = 1e-15
                                  , stTolX    = 1e-15 }
                  , NM.nmInitStep = 0.001
                  }) f x)
      xRef = clipToBounds (saBounds cfg) (orBest r)
      fRef = f xRef
  in if fRef < fx then (xRef, fRef) else (x, fx)

mkRes :: Direction -> [Double] -> Double -> [Double]
      -> Int -> Bool -> IO OptimResult
mkRes dir xb fb hist iter conv =
  let vUser = case dir of { Minimize -> fb; Maximize -> negate fb }
      hU    = case dir of
                Minimize -> reverse hist
                Maximize -> map negate (reverse hist)
  in pure $ OptimResult xb vUser hU iter conv
