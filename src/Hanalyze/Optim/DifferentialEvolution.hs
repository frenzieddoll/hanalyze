-- |
-- Module      : Hanalyze.Optim.DifferentialEvolution
-- Description : Differential Evolution (DE/rand/1/bin) — Storn & Price 1997
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Differential Evolution (DE/rand/1/bin) — Storn & Price 1997.
--
-- A gradient-free, global, simple-to-implement and empirically robust
-- evolutionary algorithm. Best suited to continuous non-convex problems,
-- typically effective in the 5-30 dimensional regime.
--
-- Algorithm (DE/rand/1/bin) — each generation, for every individual @i@:
--
--   1. Pick three distinct indices @a, b, c@ from the population (all
--      different from @i@).
--   2. Mutation: @v = a + F * (b - c)@ with mutation factor @F ∈ [0.5, 0.8]@
--      typical.
--   3. Binomial crossover: @u_j = v_j@ with probability @CR ∈ [0.7, 0.9]@,
--      otherwise @x_j@; at least one dimension is forced from @v@.
--   4. Selection: replace @x_i ← u@ if @f(u) ≤ f(x_i)@.
--
-- Cost: @N@ function evaluations per generation (population size). Easily
-- parallelizable, but this implementation is sequential.
{-# LANGUAGE StrictData #-}
module Hanalyze.Optim.DifferentialEvolution
  ( DEConfig (..)
  , DEStrategy (..)
  , defaultDEConfig
  , runDE
  , runDEWith
  ) where

import Data.List (minimumBy)
import Data.Ord (comparing)
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWCD
import Control.Monad (forM, forM_)
import Data.IORef
import Control.Exception (SomeException, try, evaluate)
import Hanalyze.Optim.Common
import qualified Hanalyze.Optim.LBFGS as LB

-- | DE strategy.
--
--   * 'ClassicRand1Bin' — DE/rand/1/bin with fixed @F@ / @CR@ from
--     'deF' / 'deCR' (the original Storn-Price 1997 formulation).
--   * 'JDE' — self-adaptive DE (Brest et al. 2006). Each individual
--     carries its own @F_i@ and @CR_i@; before each trial each is
--     re-sampled with probability @τ@ (defaults @τ_F = τ_CR = 0.1@):
--
--       @F_i  ←  F_l + r₁ · (F_u − F_l)@        (r₁ ~ U(0, 1))
--       @CR_i ←  r₂@                            (r₂ ~ U(0, 1))
--
--     where @F_l, F_u = 0.1, 0.9@. The new @(F_i, CR_i)@ are kept iff
--     the trial is accepted. Removes the manual @F@/@CR@ tuning that
--     classic DE is sensitive to.
data DEStrategy
  = ClassicRand1Bin
  | JDE
  deriving (Show, Eq)

-- | DE configuration.
--
-- @F@ (mutation factor) and @CR@ (crossover rate) defaults are typical
-- values. The population size should be roughly @5×D@ to @10×D@.
data DEConfig = DEConfig
  { deStop      :: !StopCriteria
  , dePopSize   :: !Int        -- ^ Population size @N@ (5×D – 10×D typical).
  , deF         :: !Double     -- ^ Mutation factor @F@ (initial value when 'JDE').
  , deCR        :: !Double     -- ^ Crossover probability @CR@ (initial value when 'JDE').
  , deBounds    :: !Bounds     -- ^ Per-dimension @(lo, hi)@; used for both
                               --   initialization and boundary reflection.
  , deStrategy  :: !DEStrategy -- ^ Trial-generation strategy.
  , deDir       :: !Direction
  , dePolish    :: !Bool
    -- ^ When 'True' (default), run a final L-BFGS-B (numeric gradient)
    --   refinement on @x_best@ at termination. Mirrors scipy's
    --   @differential_evolution(polish=True)@. Brings smooth landscapes
    --   (Sphere, Levy etc.) to near-machine precision after DE has
    --   localised the basin.
  } deriving (Show, Eq)

-- | Default configuration: 200 iterations, population @max(20, 10×D)@,
-- @F = 0.5@, @CR = 0.9@, **'JDE' self-adaptive** strategy, minimization.
--
-- 'JDE' is the recommended default because the classic @F = 0.7@ /
-- @CR = 0.9@ is brittle on diverse problem types (Sphere, Rastrigin
-- and Rosenbrock all want different settings). Switch to
-- 'ClassicRand1Bin' to recover the previous behaviour.
defaultDEConfig :: [(Double, Double)] -> DEConfig
defaultDEConfig bs = DEConfig
  { deStop     = defaultStopCriteria { stMaxIter = 200 }
  , dePopSize  = max 20 (10 * length bs)
  , deF        = 0.5
  , deCR       = 0.9
  , deBounds   = bs
  , deStrategy = JDE
  , deDir      = Minimize
  , dePolish   = True
  }

-- | Run DE with the default configuration built from @bounds@.
runDE :: [(Double, Double)]            -- ^ Per-dimension bounds.
      -> ([Double] -> Double)          -- ^ Objective.
      -> MWC.GenIO
      -> IO OptimResult
runDE bounds f gen = runDEWith (defaultDEConfig bounds) f gen

-- | Run DE with a user-supplied configuration.
runDEWith :: DEConfig
          -> ([Double] -> Double)
          -> MWC.GenIO
          -> IO OptimResult
runDEWith cfg fUser gen = do
  let f      = flipFor (deDir cfg) fUser
      n      = dePopSize cfg
  -- 初期集団: 各次元 (lo, hi) 一様乱数。
  -- 各個体に (F_i, CR_i) を持たせる (Classic では未使用、jDE では更新)。
  pop0 <- forM [1 .. n] $ \_ -> sampleUniformIn (deBounds cfg) gen
  let fPop0 = map f pop0
      pop0' = [ (x, fx, deF cfg, deCR cfg) | (x, fx) <- zip pop0 fPop0 ]
  popRef  <- newIORef pop0'
  histRef <- newIORef [minimum fPop0]
  iterRef <- newIORef 0
  convRef <- newIORef False
  let stop = deStop cfg
      maxI = stMaxIter stop

  let loop = do
        i <- readIORef iterRef
        if i >= maxI
          then return ()
          else do
            pop <- readIORef popRef
            let fs     = map (\(_, ff, _, _) -> ff) pop
                bestF  = minimum fs
                worstF = maximum fs
            if abs (worstF - bestF) < stTolFun stop
              then writeIORef convRef True
              else do
                pop' <- stepDE cfg f gen pop
                writeIORef popRef pop'
                let bestF' = minimum (map (\(_, ff, _, _) -> ff) pop')
                modifyIORef histRef (bestF' :)
                writeIORef iterRef (i + 1)
                loop
  loop
  popFinal <- readIORef popRef
  iters    <- readIORef iterRef
  conv     <- readIORef convRef
  histR    <- readIORef histRef
  let (xb, vb, _, _) = minimumBy (comparing (\(_, ff, _, _) -> ff)) popFinal
  -- Optional final L-BFGS-B polish on x_best (scipy parity).
  -- Numeric gradient because the user's f is opaque. Bounds stay
  -- within deBounds. If polish improves, replace; otherwise keep.
  (xPol, vPol) <-
    if dePolish cfg
      then do
        let polCfg = LB.defaultLBFGSConfig
                       { LB.lbStop   = defaultStopCriteria
                                         { stMaxIter = 100
                                         , stTolFun  = 1e-12
                                         , stTolX    = 1e-12 }
                       , LB.lbBounds = Just (deBounds cfg)
                       }
        -- Polish can fail (numeric grad → linearSolveSVDR etc. for
        -- objectives that internally invert near-singular matrices).
        -- Catch any exception and fall back to the unpolished best.
        eR <- try (LB.runLBFGSNumeric polCfg f xb) :: IO (Either SomeException OptimResult)
        case eR of
          Left _  -> pure (xb, vb)
          Right r ->
            let xR = clipToBounds (deBounds cfg) (orBest r)
            in do
              evR <- try (evaluate (f xR)) :: IO (Either SomeException Double)
              case evR of
                Right vR | vR < vb -> pure (xR, vR)
                _                  -> pure (xb, vb)
      else pure (xb, vb)
  let vUser    = case deDir cfg of { Minimize -> vPol; Maximize -> negate vPol }
      histUser = case deDir cfg of
                   Minimize -> reverse histR
                   Maximize -> map negate (reverse histR)
  return $ OptimResult xPol vUser histUser iters conv

-- | jDE re-sampling probabilities (Brest 2006 standard values).
jdeTau :: Double
jdeTau = 0.1

jdeFLo, jdeFHi :: Double
jdeFLo = 0.1
jdeFHi = 0.9

-- | 1 世代の更新。'DEStrategy' によって @F_i@/@CR_i@ の扱いが分かれる:
--
--   * 'ClassicRand1Bin': @F_i = deF cfg@, @CR_i = deCR cfg@ (固定)。
--   * 'JDE'            : 各 trial 前に確率 'jdeTau' で再サンプリング、
--     trial が採用された場合のみ新値を保持。
stepDE :: DEConfig
       -> ([Double] -> Double)
       -> MWC.GenIO
       -> [([Double], Double, Double, Double)]
       -> IO [([Double], Double, Double, Double)]
stepDE cfg f gen pop = do
  let n   = length pop
      d   = length (deBounds cfg)
      bs  = deBounds cfg
  newPop <- forM [0 .. n - 1] $ \i -> do
    let (xi, fi, fOld, crOld) = pop !! i
    -- jDE: confirm or refresh F_i / CR_i for this trial
    (fTrial, crTrial) <- case deStrategy cfg of
      ClassicRand1Bin -> return (deF cfg, deCR cfg)
      JDE             -> do
        u1 <- MWC.uniformR (0, 1) gen :: IO Double
        u2 <- MWC.uniformR (0, 1) gen :: IO Double
        u3 <- MWC.uniformR (0, 1) gen :: IO Double
        u4 <- MWC.uniformR (0, 1) gen :: IO Double
        let f'  = if u1 < jdeTau then jdeFLo + u2 * (jdeFHi - jdeFLo) else fOld
            cr' = if u3 < jdeTau then u4 else crOld
        return (f', cr')
    -- mutation 用に i と異なる 3 個体をランダム選択
    [a, b, c] <- pickThree n i gen
    let xa = let (x, _, _, _) = pop !! a in x
        xb' = let (x, _, _, _) = pop !! b in x
        xc' = let (x, _, _, _) = pop !! c in x
        v   = zipWith3 (\xai xbi xci -> xai + fTrial * (xbi - xci)) xa xb' xc'
        v'  = clipToBounds bs v
    -- crossover (binomial)
    jRand <- MWC.uniformR (0, d - 1) gen
    u <- forM (zip3 [0..] xi v') $ \(j, xj, vj) -> do
      r <- MWC.uniformR (0, 1) gen
      return $ if (r :: Double) < crTrial || j == jRand then vj else xj
    let fu = f u
    if fu <= fi
      then return (u,  fu, fTrial, crTrial)
      else return (xi, fi, fOld,   crOld)
  return newPop

-- | i と異なる 3 つの相異なるインデックスを集団 [0, n) から選ぶ。
pickThree :: Int -> Int -> MWC.GenIO -> IO [Int]
pickThree n i gen = do
  let pickOne avoid = do
        k <- MWC.uniformR (0, n - 1) gen
        if k `elem` avoid then pickOne avoid else return k
  a <- pickOne [i]
  b <- pickOne [i, a]
  c <- pickOne [i, a, b]
  return [a, b, c]

-- | (`sampleUniform` and `clipBound` are now provided by `Hanalyze.Optim.Common`
--    as `sampleUniformIn` / `clipToBounds`.)
