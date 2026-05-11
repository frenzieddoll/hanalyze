{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | No-U-Turn Sampler (NUTS).
--
-- Implements Hoffman & Gelman (2014) Algorithm 3, with Nesterov dual
-- averaging for step-size adaptation (Stan's strategy). Gradients are
-- exact, computed via 'Numeric.AD.Mode.Forward'.
--
-- Constrained parameters (@PositiveT@, @UnitIntervalT@) are detected
-- automatically from the prior distribution.
--
-- @
-- import Model.HBM
-- import MCMC.NUTS
--
-- chain <- nuts myModel defaultNUTSConfig
--                (Map.fromList [("mu",0),("sigma",1)]) gen
-- @
module MCMC.NUTS
  ( NUTSConfig (..)
  , defaultNUTSConfig
  , nuts
  , nutsChains
  ) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (foldM, replicateM, when)
import Data.IORef
import qualified Data.Map.Strict as Map
import qualified Data.Vector.Storable as VS
import System.Random.MWC (GenIO, uniform)
import System.Random.MWC.Distributions (standard)

import MCMC.Core (Chain (..), spawnGen)
import MCMC.HMC  (kineticMVS, leapfrogWithMVS)
import Model.HBM (ModelP, Params, sampleNames, getTransforms,
                  logJointUnconstrained, gradADU)
import Stat.Distribution (toUnconstrained, fromUnconstrained)

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

-- | NUTS configuration.
data NUTSConfig = NUTSConfig
  { nutsIterations    :: Int     -- ^ Total iterations (burn-in included).
  , nutsBurnIn        :: Int     -- ^ Burn-in iterations to discard.
  , nutsStepSize      :: Double  -- ^ Initial leapfrog step size @ε@.
  , nutsMaxDepth      :: Int     -- ^ Maximum tree depth (typically 10).
  , nutsAdaptStepSize :: Bool    -- ^ Enable Nesterov dual-averaging step-size adaptation.
  , nutsTargetAccept  :: Double  -- ^ Target acceptance rate (0.8 typical, 0.95 for hard problems).
  , nutsAdaptMass     :: Bool    -- ^ Enable diagonal mass-matrix adaptation (B11).
                                 --   Stan-style multi-window: init buffer (15% /
                                 --   ≥75 iter, M=I) → doubling windows
                                 --   25→50→100→200→… (M updated + dual avg
                                 --   restarted at each window end) → term buffer
                                 --   (10% / ≥50 iter, M frozen, ε converges).
                                 --   Recommended for posteriors with strongly
                                 --   varying scales across parameters.
  } deriving (Show)

-- | Default NUTS configuration: 2000 iterations, 500 burn-in, @ε = 0.1@,
-- max depth 10, dual averaging enabled, target acceptance 0.8,
-- diagonal mass-matrix adaptation off (opt-in via 'nutsAdaptMass').
defaultNUTSConfig :: NUTSConfig
defaultNUTSConfig = NUTSConfig
  { nutsIterations    = 2000
  , nutsBurnIn        = 500
  , nutsStepSize      = 0.1
  , nutsMaxDepth      = 10
  , nutsAdaptStepSize = True
  , nutsTargetAccept  = 0.8
  , nutsAdaptMass     = False
  }

-- ---------------------------------------------------------------------------
-- Dual averaging
-- ---------------------------------------------------------------------------

-- | Internal state for Nesterov's dual-averaging step-size adaptation.
data DualAvgState = DualAvgState
  { daLogEps     :: Double   -- ^ Current @log ε@ used for sampling.
  , daLogEpsBar  :: Double   -- ^ Running smoothed @log ε̄@ (post-adaptation value).
  , daH          :: Double   -- ^ Running average of (target − accept-stat).
  , daMu         :: Double   -- ^ Anchor @μ = log(10 ε₀)@.
  , daM          :: Int      -- ^ Iteration counter.
  }

-- | Initialize 'DualAvgState' from an initial step size @ε₀@.
initDualAvg :: Double -> DualAvgState
initDualAvg eps0 = DualAvgState
  { daLogEps    = log eps0
  , daLogEpsBar = log eps0
  , daH         = 0.0
  , daMu        = log (10 * eps0)
  , daM         = 0
  }

-- | Apply one dual-averaging update given the target acceptance @δ@ and
-- the observed acceptance statistic @α@ for the iteration.
updateDualAvg :: Double -> Double -> DualAvgState -> DualAvgState
updateDualAvg delta alpha da =
  let m      = daM da + 1
      gamma  = 0.05
      t0     = 10.0
      kappa  = 0.75
      hNew   = (1 - 1 / (fromIntegral m + t0)) * daH da
             + (1 / (fromIntegral m + t0)) * (delta - alpha)
      logEps = daMu da - sqrt (fromIntegral m) / gamma * hNew
      logEpsClip = max (-7) (min 5 logEps)
      logEpsBar = (fromIntegral m ** (-kappa)) * logEpsClip
                + (1 - fromIntegral m ** (-kappa)) * daLogEpsBar da
  in da { daLogEps = logEpsClip, daLogEpsBar = logEpsBar, daH = hNew, daM = m }

-- ---------------------------------------------------------------------------
-- 内部ツリー
-- ---------------------------------------------------------------------------

-- | Internal NUTS tree node. All position/momentum are 'VS.Vector
-- Double' rather than 'Params' (= @Map@) / @[Double]@: the
-- @doubleTree@ recursion creates up to @2¹⁰@ intermediate trees per
-- iteration, and the previous Map / list representation paid an
-- order-of-magnitude in allocation that swamped the actual leapfrog
-- arithmetic.
data NUTSTree = NUTSTree
  { ntThMinus :: VS.Vector Double
  , ntRMinus  :: VS.Vector Double
  , ntThPlus  :: VS.Vector Double
  , ntRPlus   :: VS.Vector Double
  , ntThPrime :: VS.Vector Double
  , ntN       :: Int
  , ntS       :: Bool
  , ntDiv     :: Bool
    -- ^ サブツリー中で divergent (|ΔH| > deltaMax) が発生したか
  }

deltaMax :: Double
deltaMax = 1000.0

-- | U-turn check on Storable Vectors. @(θ⁺ − θ⁻) · r⁻ < 0@ or
-- @(θ⁺ − θ⁻) · r⁺ < 0@ ⇒ trajectory has begun to retrace itself.
uTurnVS
  :: VS.Vector Double -> VS.Vector Double
  -> VS.Vector Double -> VS.Vector Double -> Bool
uTurnVS thMinus rMinus thPlus rPlus =
  let delta = VS.zipWith (-) thPlus thMinus
      d1    = VS.sum (VS.zipWith (*) delta rMinus)
      d2    = VS.sum (VS.zipWith (*) delta rPlus)
  in d1 < 0 || d2 < 0
{-# INLINE uTurnVS #-}

-- | Sample momentum @r ~ N(0, M)@ from the diagonal mass matrix
-- represented as @M⁻¹@. Per coordinate: @r_i = z / sqrt(M⁻¹_i)@,
-- @z ~ N(0,1)@. Storable-vector tight loop, no list allocation.
sampleMomentum :: VS.Vector Double -> GenIO -> IO (VS.Vector Double)
sampleMomentum mInv gen = do
  let n = VS.length mInv
  VS.generateM n $ \i -> do
    z <- standard gen
    return (z / sqrt (mInv `VS.unsafeIndex` i))
{-# INLINE sampleMomentum #-}

-- ---------------------------------------------------------------------------
-- ツリービルダー
-- ---------------------------------------------------------------------------

buildTree
  :: (VS.Vector Double -> VS.Vector Double)   -- ^ Gradient (negated grad of log π).
  -> (VS.Vector Double -> Double)             -- ^ Log target density.
  -> VS.Vector Double                         -- ^ Diagonal M⁻¹.
  -> Double                                   -- ^ Step size @ε@.
  -> VS.Vector Double                         -- ^ Position.
  -> VS.Vector Double                         -- ^ Momentum.
  -> Double                                   -- ^ @log u@ slice.
  -> Int                                      -- ^ Direction (±1).
  -> Int                                      -- ^ Recursion depth.
  -> GenIO
  -> IO NUTSTree
buildTree gradFn logPiFn mInv eps theta r logU dir depth gen
  | depth == 0 = do
      let (theta', r') = leapfrogWithMVS gradFn mInv
                            (fromIntegral dir * eps) 1 theta r
          h'  = -(logPiFn theta') + kineticMVS mInv r'
          n'  = if logU <= -h' then 1 else 0
          s'  = logU < deltaMax - h'
          divergent = not s'
      return NUTSTree
        { ntThMinus = theta', ntRMinus = r'
        , ntThPlus  = theta', ntRPlus  = r'
        , ntThPrime = theta', ntN = n', ntS = s'
        , ntDiv = divergent
        }
  | otherwise = do
      t1 <- buildTree gradFn logPiFn mInv eps theta r logU dir (depth - 1) gen
      if not (ntS t1) then return t1
      else do
        let (th0, r0) = if dir == -1
              then (ntThMinus t1, ntRMinus t1)
              else (ntThPlus  t1, ntRPlus  t1)
        t2 <- buildTree gradFn logPiFn mInv eps th0 r0 logU dir (depth - 1) gen
        let n1 = ntN t1; n2 = ntN t2
        thPrime' <-
          if n1 == 0 then return (ntThPrime t2)
          else if n2 == 0 then return (ntThPrime t1)
          else do
            u <- uniform gen :: IO Double
            return $ if u < min 1.0 (fromIntegral n2 / fromIntegral n1)
                     then ntThPrime t2
                     else ntThPrime t1
        let (minus', rMinus', plus', rPlus') = if dir == -1
              then (ntThMinus t2, ntRMinus t2, ntThPlus t1, ntRPlus t1)
              else (ntThMinus t1, ntRMinus t1, ntThPlus t2, ntRPlus t2)
            s' = ntS t2 && not (uTurnVS minus' rMinus' plus' rPlus')
        return NUTSTree
          { ntThMinus = minus', ntRMinus = rMinus'
          , ntThPlus  = plus',  ntRPlus  = rPlus'
          , ntThPrime = thPrime', ntN = n1 + n2, ntS = s'
          , ntDiv = ntDiv t1 || ntDiv t2
          }

-- ---------------------------------------------------------------------------
-- NUTS サンプラー
-- ---------------------------------------------------------------------------

-- | NUTS sampler for a polymorphic HBM model ('ModelP').
-- 軌道長は U-Turn 判定で自動決定。
nuts :: ModelP r -> NUTSConfig -> Params -> GenIO -> IO Chain
nuts m cfg initC gen = do
  let names      = sampleNames m
      trMap      = getTransforms m
      transList  = [Map.findWithDefault errT n trMap | n <- names]
      errT       = error "nuts: missing transform"

      -- Initial unconstrained position as a Storable Vector. The hot
      -- loop never touches 'Params' (= Map); we only convert at the
      -- boundary to record samples.
      initUV :: VS.Vector Double
      initUV = VS.fromList
        [ toUnconstrained t (Map.findWithDefault 0 n initC)
        | (n, t) <- zip names transList ]

      total   = nutsBurnIn cfg + nutsIterations cfg
      doAdapt = nutsAdaptStepSize cfg && nutsBurnIn cfg > 0

      -- Vector-native log target density. Builds the @Params@ map only
      -- once per call (the upstream 'logJoint' API still wants a Map).
      logPiFn :: VS.Vector Double -> Double
      logPiFn uv =
        let xs = VS.toList uv
            paramsU = Map.fromList (zip names xs)
        in logJointUnconstrained m names transList paramsU

      -- Vector-native gradient. 'gradADU' already takes a list, so the
      -- wrapping is essentially a Storable ↔ list pair (n_params is
      -- small so the conversion cost is negligible — the dominant
      -- expense is the AD pass itself).
      gradFn :: VS.Vector Double -> VS.Vector Double
      gradFn uv =
        let xs = VS.toList uv
            gs = gradADU m names transList xs
        in VS.fromList (map negate gs)

      toConstrained :: VS.Vector Double -> Params
      toConstrained uv = Map.fromList
        [ (n, fromUnconstrained t (uv `VS.unsafeIndex` i))
        | (i, (n, t)) <- zip [0..] (zip names transList) ]

  samplesRef    <- newIORef []
  energyRef     <- newIORef ([] :: [Double])
  divergenceRef <- newIORef ([] :: [Int])
  acceptedRef   <- newIORef (0 :: Int)
  daRef         <- newIORef (initDualAvg (nutsStepSize cfg))

  -- B11: Stan-style multi-window diagonal mass-matrix adaptation.
  --
  -- Schedule (warmup W):
  --   * init buffer  (max 75 / W÷7 iters): step-size adapt only, M = I
  --   * window phase: doubling windows 25 → 50 → 100 → 200 → ...
  --       At the end of each window: update M⁻¹ from window's
  --       Welford-accumulated diagonal variance, restart dual averaging.
  --   * term buffer  (max 50 / W÷10 iters): M frozen, step-size adapt
  --       continues to converge ε under the final geometry.
  let nParams       = length names
      adaptM        = nutsAdaptMass cfg && nutsBurnIn cfg > 0
      (windowEnds, initBuf, _termBuf) = stanWindows (nutsBurnIn cfg)
      windowPhaseEnd = if null windowEnds then 0 else last windowEnds
  mInvRef     <- newIORef (VS.replicate nParams 1.0)
  welfordRef  <- newIORef (emptyWelford nParams)

  let step :: VS.Vector Double -> Double -> VS.Vector Double
           -> IO (VS.Vector Double, Double, Double, Bool)
      step mInv eps currentU = do
        -- r ~ N(0, M)  ⇔  r_i = sqrt(M_ii) * z = z / sqrt(M⁻¹_ii)
        r0 <- sampleMomentum mInv gen
        u0 <- uniform gen :: IO Double
        let h0   = -(logPiFn currentU) + kineticMVS mInv r0
            logU = log u0 - h0
        let tree0 = NUTSTree
              { ntThMinus = currentU, ntRMinus = r0
              , ntThPlus  = currentU, ntRPlus  = r0
              , ntThPrime = currentU, ntN = 1, ntS = True
              , ntDiv = False
              }
        let doubleTree tree j =
              if not (ntS tree) then return tree
              else do
                u <- uniform gen :: IO Double
                let dir = if u < 0.5 then -1 else 1 :: Int
                    (th0, r0') = if dir == -1
                      then (ntThMinus tree, ntRMinus tree)
                      else (ntThPlus  tree, ntRPlus  tree)
                subtree <- buildTree gradFn logPiFn mInv eps th0 r0' logU dir j gen
                let n1 = ntN tree; n2 = ntN subtree
                thPrime' <-
                  if not (ntS subtree) || n2 == 0
                  then return (ntThPrime tree)
                  else do
                    u2 <- uniform gen :: IO Double
                    return $ if u2 < min 1.0 (fromIntegral n2 / fromIntegral n1)
                             then ntThPrime subtree
                             else ntThPrime tree
                let (minus', rMinus', plus', rPlus') = if dir == -1
                      then (ntThMinus subtree, ntRMinus subtree,
                            ntThPlus  tree,    ntRPlus  tree)
                      else (ntThMinus tree,    ntRMinus tree,
                            ntThPlus  subtree, ntRPlus  subtree)
                    s' = ntS subtree && not (uTurnVS minus' rMinus' plus' rPlus')
                return NUTSTree
                  { ntThMinus = minus', ntRMinus = rMinus'
                  , ntThPlus  = plus',  ntRPlus  = rPlus'
                  , ntThPrime = thPrime', ntN = n1 + n2, ntS = s'
                  , ntDiv = ntDiv tree || ntDiv subtree
                  }
        finalTree <- foldM doubleTree tree0 [0 .. nutsMaxDepth cfg - 1]
        let proposedU        = ntThPrime finalTree
            (thetaOne, rOne) = leapfrogWithMVS gradFn mInv eps 1 currentU r0
            hOne             = -(logPiFn thetaOne) + kineticMVS mInv rOne
            alpha            = min 1.0 (exp (h0 - hOne))
        when (proposedU /= currentU) $ modifyIORef' acceptedRef (+1)
        return (proposedU, alpha, h0, ntDiv finalTree)

  let loop 0 currentU _eps = return currentU
      loop i currentU eps = do
        mInv <- readIORef mInvRef
        (nextU, alpha, h0, divergent) <- step mInv eps currentU
        let isBurnIn   = i > nutsIterations cfg
            -- iteration index from start (1-based); total counts down.
            iterIdx    = total - i + 1
            -- Inside the window phase: collect samples for Welford.
            inWindowPhase = adaptM && isBurnIn
                            && iterIdx > initBuf
                            && iterIdx <= windowPhaseEnd
            -- This iteration ends a window: update M, restart DA.
            isWindowEnd   = adaptM && isBurnIn && iterIdx `elem` windowEnds
        when inWindowPhase $
          modifyIORef' welfordRef (\w -> welfordAddVS w nextU)
        when isWindowEnd $ do
          w <- readIORef welfordRef
          when (wN w >= 5) $ do  -- need a few samples to be meaningful
            writeIORef mInvRef (welfordMInvVS w)
            -- Restart dual averaging anchored at the current ε; it
            -- needs to re-converge under the new geometry.
            writeIORef daRef (initDualAvg eps)
          -- Reset Welford for the next window (window-local variance).
          writeIORef welfordRef (emptyWelford nParams)
        eps' <- if doAdapt && isBurnIn
          then do
            da <- readIORef daRef
            let da' = updateDualAvg (nutsTargetAccept cfg) alpha da
            writeIORef daRef da'
            return (exp (daLogEps da'))
          else do
            da <- readIORef daRef
            let epsBar = if doAdapt && not isBurnIn && i == nutsIterations cfg
                         then exp (daLogEpsBar da)
                         else eps
            return epsBar
        if not isBurnIn
          then do
            modifyIORef' samplesRef (toConstrained nextU :)
            modifyIORef' energyRef  (h0 :)
            when divergent $
              modifyIORef' divergenceRef
                ((nutsIterations cfg - i) :)
          else return ()
        loop (i - 1) nextU eps'

  _ <- loop total initUV (nutsStepSize cfg)
  samples  <- fmap reverse (readIORef samplesRef)
  energies <- fmap reverse (readIORef energyRef)
  divs     <- fmap reverse (readIORef divergenceRef)
  accepted <- readIORef acceptedRef
  return Chain
    { chainSamples     = samples
    , chainAccepted    = accepted
    , chainTotal       = total
    , chainEnergy      = energies
    , chainDivergences = divs
    }

-- ---------------------------------------------------------------------------
-- B11: Mass-matrix adaptation helpers
-- ---------------------------------------------------------------------------

-- | Welford online accumulator for diagonal sample variance.
--
-- Per-coordinate one-pass mean / M2; variance = M2 / (n − 1).
-- Used by Stan-style window adaptation to estimate posterior variance
-- without keeping the raw samples around.
-- | Plain (non-record) constructor: @Welford n mean m2@. Kept positional
-- because the @m2@ field is only ever pattern-matched, never read via a
-- selector (record syntax would generate an unused-binding warning).
-- | Storable-Vector Welford. The previous list-based form allocated
-- four @[Double]@ vectors per add (warmup ~500 iters × 4 cells = 10K
-- list cells per fit) and was hot during the mass-matrix adaptation
-- window phase.
data Welford = Welford !Int !(VS.Vector Double) !(VS.Vector Double)

wN :: Welford -> Int
wN (Welford n _ _) = n

emptyWelford :: Int -> Welford
emptyWelford p = Welford 0 (VS.replicate p 0) (VS.replicate p 0)

welfordAddVS :: Welford -> VS.Vector Double -> Welford
welfordAddVS (Welford n mean m2) x =
  let !n'   = n + 1
      !nD   = fromIntegral n' :: Double
      !d    = VS.zipWith (-) x mean
      !mean' = VS.zipWith (\me di -> me + di / nD) mean d
      !d2   = VS.zipWith (-) x mean'
      !m2'  = VS.zipWith3 (\m2i d1 d22 -> m2i + d1 * d22) m2 d d2
  in Welford n' mean' m2'

-- | Stan-regularised diagonal @M⁻¹@ from a Welford accumulator.
--
-- @σ̂² = (n / (n+5)) · sample_var + 1e-3 · (5 / (n+5))@.
-- The 1e-3 shrinkage target keeps the estimator non-degenerate when
-- @n@ is tiny; for moderate @n@ it reduces to the sample variance.
--
-- /Convention/: following Stan/blackjax, @M⁻¹@ stores the posterior
-- covariance directly (so @M⁻¹_ii = σ̂²_i@). With kinetic energy
-- @½ rᵀ M⁻¹ r@ and @r ~ N(0, M)@, this gives a per-leapfrog position
-- step @ε · σ̂_i@ in absolute units (i.e. @ε@ in posterior-sd units),
-- which is what NUTS needs for tree depth ~ @1/ε@.
welfordMInvVS :: Welford -> VS.Vector Double
welfordMInvVS (Welford n _ m2)
  | n < 2     = VS.replicate (VS.length m2) 1.0
  | otherwise =
      let nD     = fromIntegral n :: Double
          k      = 5.0 :: Double
          weight = nD / (nD + k)
          target = 1e-3
      in VS.map (\v -> let raw = v / (nD - 1)
                       in max 1e-12 (weight * raw + (1 - weight) * target))
                m2

-- | Stan-style adaptation schedule for a warmup of @W@ iterations.
--
-- Returns @(windowEndIters, initBuffer, termBuffer)@ where
-- @windowEndIters@ are 1-based iteration indices at which to update the
-- mass matrix, and @initBuffer@ / @termBuffer@ are the no-update
-- prefix / suffix lengths (Stan defaults: 15% / 10%, with floors of 75
-- and 50 iters respectively). Windows double in size starting from 25;
-- the last window absorbs any remainder.
--
-- For @W = 500@: @initBuffer = 75@, @termBuffer = 50@, middle = 375,
-- windows = @[100, 150, 250, 450]@.
stanWindows :: Int -> ([Int], Int, Int)
stanWindows w
  | w < 50    = ([], w, 0)
  | otherwise =
      let initB  = max 75 (w `div` 7)
          termB  = max 50 (w `div` 10)
          midLen = w - initB - termB
      in if midLen < 25
         then ([], w, 0)
         else (genW (initB + 1) midLen 25, initB, termB)
  where
    genW _     0    _     = []
    genW start rest wsize
      | wsize * 2 > rest =
          -- Next doubled window wouldn't fit; absorb the remainder.
          [start + rest - 1]
      | otherwise =
          let endIter = start + wsize - 1
          in endIter : genW (endIter + 1) (rest - wsize) (wsize * 2)

-- | Run 'nuts' on @numChains@ parallel chains.
nutsChains :: ModelP r -> NUTSConfig -> Int -> Params -> GenIO -> IO [Chain]
nutsChains m cfg numChains initC baseGen = do
  gens <- replicateM numChains (spawnGen baseGen)
  mapConcurrently (\g -> nuts m cfg initC g) gens
