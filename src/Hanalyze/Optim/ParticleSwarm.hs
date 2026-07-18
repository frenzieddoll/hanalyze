-- |
-- Module      : Hanalyze.Optim.ParticleSwarm
-- Description : Particle Swarm Optimization (PSO) — Kennedy & Eberhart 1995
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Particle Swarm Optimization (PSO).
--
-- Kennedy & Eberhart (1995). A metaheuristic in which a swarm of particles
-- updates velocity by being attracted to its personal best (pbest) and the
-- global best (gbest).
--
-- Velocity / position update:
--
-- @
-- v_{t+1} = w · v_t + c_1 · r_1 · (pbest - x) + c_2 · r_2 · (gbest - x)
-- x_{t+1} = x_t + v_{t+1}
-- @
--
-- Here @w@ is inertia, @c_1@ the cognitive coefficient, @c_2@ the social
-- coefficient, and @r_1, r_2 ~ U(0, 1)@.
{-# LANGUAGE StrictData #-}
module Hanalyze.Optim.ParticleSwarm
  ( PSOConfig (..)
  , defaultPSOConfig
  , runPSO
  , runPSOWith
  ) where

import Control.Monad (forM, replicateM)
import Data.List (minimumBy)
import Data.Ord (comparing)
import Data.IORef
import qualified System.Random.MWC as MWC
import Hanalyze.Optim.Common

-- | PSO configuration.
data PSOConfig = PSOConfig
  { psoStop     :: !StopCriteria
  , psoNum      :: !Int        -- ^ Number of particles (20–50 typical).
  , psoInertia  :: !Double     -- ^ Inertia @w@ (0.4–0.9 typical).
  , psoCog      :: !Double     -- ^ Cognitive coefficient @c₁@ (1.5–2.0 typical).
  , psoSoc      :: !Double     -- ^ Social coefficient @c₂@ (1.5–2.0 typical).
  , psoBounds   :: !Bounds     -- ^ Per-dimension bounds.
  , psoVMax     :: !Double     -- ^ Velocity cap as a fraction of the
                               --   range per dimension (e.g. 0.5).
  , psoDir      :: !Direction
  } deriving (Show, Eq)

-- | Default configuration: 200 iterations, swarm size @max(20, 5×D)@,
-- @w = 0.7@, @c₁ = c₂ = 1.5@, @vMax = 0.5@.
defaultPSOConfig :: [(Double, Double)] -> PSOConfig
defaultPSOConfig bs = PSOConfig
  { psoStop    = defaultStopCriteria { stMaxIter = 200 }
  , psoNum     = max 20 (5 * length bs)
  , psoInertia = 0.7
  , psoCog     = 1.5
  , psoSoc     = 1.5
  , psoBounds  = bs
  , psoVMax    = 0.5
  , psoDir     = Minimize
  }

-- | Run PSO with the default configuration built from @bounds@.
runPSO :: [(Double, Double)]
       -> ([Double] -> Double)
       -> MWC.GenIO
       -> IO OptimResult
runPSO bs f gen = runPSOWith (defaultPSOConfig bs) f gen

-- | Run PSO with a user-specified configuration.
runPSOWith :: PSOConfig
           -> ([Double] -> Double)
           -> MWC.GenIO
           -> IO OptimResult
runPSOWith cfg fUser gen = do
  let f      = flipFor (psoDir cfg) fUser
      bs     = psoBounds cfg
      n      = length bs
      np     = psoNum cfg
      vMaxes = [ psoVMax cfg * (hi - lo) | (lo, hi) <- bs ]

  -- 初期化
  xs0 <- replicateM np (sampleUniformIn bs gen)
  vs0 <- replicateM np $ forM (zip bs vMaxes) $ \((lo, hi), vM) -> do
           u <- MWC.uniformR (-1, 1) gen
           return ((u :: Double) * vM * 0.1)
  let fs0 = map f xs0

  posRef     <- newIORef xs0
  velRef     <- newIORef vs0
  pbestRef   <- newIORef (zip xs0 fs0)
  gbestRef   <- newIORef (minimumBy (comparing snd) (zip xs0 fs0))
  histRef    <- newIORef [snd (minimumBy (comparing snd) (zip xs0 fs0))]
  iterRef    <- newIORef 0

  let stop = psoStop cfg
      maxI = stMaxIter stop

  let loop = do
        i <- readIORef iterRef
        if i >= maxI then return ()
          else do
            xs <- readIORef posRef
            vs <- readIORef velRef
            pb <- readIORef pbestRef
            (gbX, gbF) <- readIORef gbestRef
            -- 更新
            updated <- forM (zip3 xs vs pb) $ \(x, v, (px, pf)) -> do
              vNew <- forM (zip4 x v px gbX) $ \(xi, vi, pxi, gxi) -> do
                r1 <- MWC.uniformR (0, 1) gen :: IO Double
                r2 <- MWC.uniformR (0, 1) gen :: IO Double
                pure $ psoInertia cfg * vi
                       + psoCog cfg * r1 * (pxi - xi)
                       + psoSoc cfg * r2 * (gxi - xi)
              -- vMax クリップ
              let vClipped = zipWith (\vi vM -> max (-vM) (min vM vi)) vNew vMaxes
              -- 位置更新 + bounds 反射
              let xNew = clipToBounds bs (zipWith (+) x vClipped)
              let fNew = f xNew
              -- pbest 更新
              let (pxN, pfN) = if fNew < pf then (xNew, fNew) else (px, pf)
              return (xNew, vClipped, (pxN, pfN), fNew)
            let xsN = [a | (a, _, _, _) <- updated]
                vsN = [b | (_, b, _, _) <- updated]
                pbN = [c | (_, _, c, _) <- updated]
                bestC = minimumBy (comparing snd) [(a, d) | (a, _, _, d) <- updated]
                (gbXN, gbFN) = if snd bestC < gbF then bestC else (gbX, gbF)
            writeIORef posRef xsN
            writeIORef velRef vsN
            writeIORef pbestRef pbN
            writeIORef gbestRef (gbXN, gbFN)
            modifyIORef histRef (gbFN :)
            writeIORef iterRef (i + 1)
            loop
  loop
  (gbX, gbF) <- readIORef gbestRef
  iters      <- readIORef iterRef
  histR      <- readIORef histRef
  let vUser = case psoDir cfg of { Minimize -> gbF; Maximize -> negate gbF }
      hU    = case psoDir cfg of
                Minimize -> reverse histR
                Maximize -> map negate (reverse histR)
  return $ OptimResult gbX vUser hU iters False
  where
    zip4 (a:as) (b:bs) (c:cs) (d:ds) = (a, b, c, d) : zip4 as bs cs ds
    zip4 _ _ _ _ = []
