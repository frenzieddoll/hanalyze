{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-full-laziness -fno-cse #-}
-- | MCMC benchmarks (B7).
--
-- Compares hanalyze's MCMC.{HMC, NUTS} against PyMC / blackjax /
-- numpyro on a shared 8-schools-style hierarchical normal model.
-- Outputs the unified BenchRow CSV at @bench/results/haskell/mcmc.csv@.
--
-- Model:
--   mu      ~ Normal(0, 100)
--   tau     ~ Exponential(0.1)
--   theta_j ~ Normal(mu, tau)         (j = 1..3)
--   y_ij    ~ Normal(theta_j, sigma=5)
--
-- Iterations: warmup=500, samples=1000 (single chain, deterministic
-- starting point) — chosen so the bench finishes in seconds.
module Main where

import qualified Data.Map.Strict         as Map
import qualified Data.Text               as T
import qualified System.Random.MWC       as MWC

import           Model.HBM               (Distribution (..), ModelP, sample,
                                          observe)
import           MCMC.Core               (Chain, chainAccepted, chainTotal,
                                          chainVals, posteriorMean,
                                          posteriorSD)
import           MCMC.HMC                (HMCConfig (..), defaultHMCConfig, hmc)
import           MCMC.NUTS               (NUTSConfig (..), defaultNUTSConfig,
                                          nuts)
import           Stat.MCMC               (ess)

import           BenchUtil

-- ---------------------------------------------------------------------------
-- Shared model
-- ---------------------------------------------------------------------------

schoolData :: [[Double]]
schoolData =
  [ [72, 68, 75, 71]
  , [85, 88, 82, 90]
  , [61, 65, 58, 63]
  ]

sigmaY :: Double
sigmaY = 5.0

schoolModel :: ModelP ()
schoolModel = do
  mu  <- sample "mu"  (Normal 0 100)
  tau <- sample "tau" (Exponential 0.1)
  mapM_ (\(j, ys) -> do
    theta <- sample (T.pack ("theta_" ++ show (j :: Int)))
                    (Normal mu tau)
    observe (T.pack ("y_" ++ show j))
            (Normal theta (realToFrac sigmaY)) ys)
    (zip [1 ..] schoolData)

initParams :: Map.Map T.Text Double
initParams = Map.fromList
  [ ("mu",      73.0)
  , ("tau",     10.0)
  , ("theta_1", 71.5)
  , ("theta_2", 86.25)
  , ("theta_3", 61.75)
  ]

paramNames :: [T.Text]
paramNames = ["mu", "tau", "theta_1", "theta_2", "theta_3"]

-- Probe forces the full chain by summing posterior means + SDs.
probeChain :: Chain -> Double
probeChain ch =
  sum [ maybe 0 id (posteriorMean p ch)
      + maybe 0 id (posteriorSD   p ch)
      | p <- paramNames ]

acceptRate :: Chain -> Double
acceptRate ch =
  fromIntegral (chainAccepted ch) / max 1 (fromIntegral (chainTotal ch))

-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  rows <- mconcat <$> sequence
    [ benchHMC  "HMC_8schools_warm500_n1000"
    , benchNUTS "NUTS_8schools_warm500_n1000"
    ]
  writeRows "bench/results/haskell/mcmc.csv" rows
  putStrLn $ "wrote " ++ show (length rows)
          ++ " rows → bench/results/haskell/mcmc.csv"

-- ---------------------------------------------------------------------------
-- HMC
-- ---------------------------------------------------------------------------

benchHMC :: String -> IO [BenchRow]
benchHMC name = do
  let cfg = defaultHMCConfig
              { hmcIterations    = 1000
              , hmcBurnIn        = 500
              , hmcStepSize      = 0.05
              , hmcLeapfrogSteps = 30
              }
      run :: Int -> IO Chain
      run _ = do
        g <- MWC.create
        hmc schoolModel cfg initParams g
  (ms, ch) <- timeitTastyIO probeChain run
  let muEss  = ess (chainVals "mu"  ch)
      tauEss = ess (chainVals "tau" ch)
      muMean = maybe 0 id (posteriorMean "mu" ch)
      acc    = acceptRate ch
  return [ BenchRow "haskell" "mcmc" name ms muMean muEss
            ("HMC eps=0.05 L=30 accept=" ++ show acc
             ++ " ess(mu)=" ++ show muEss
             ++ " ess(tau)=" ++ show tauEss) ]

-- ---------------------------------------------------------------------------
-- NUTS
-- ---------------------------------------------------------------------------

benchNUTS :: String -> IO [BenchRow]
benchNUTS name = do
  let cfg = defaultNUTSConfig
              { nutsIterations    = 1000
              , nutsBurnIn        = 500
              , nutsStepSize      = 0.08
              , nutsAdaptStepSize = True
              , nutsAdaptMass     = True   -- B11: Stan-style multi-window
              }
      run :: Int -> IO Chain
      run _ = do
        g <- MWC.create
        nuts schoolModel cfg initParams g
  (ms, ch) <- timeitTastyIO probeChain run
  let muEss  = ess (chainVals "mu"  ch)
      tauEss = ess (chainVals "tau" ch)
      muMean = maybe 0 id (posteriorMean "mu" ch)
      acc    = acceptRate ch
  return [ BenchRow "haskell" "mcmc" name ms muMean muEss
            ("NUTS eps=0.08 dual-averaging mass-adapt accept=" ++ show acc
             ++ " ess(mu)=" ++ show muEss
             ++ " ess(tau)=" ++ show tauEss) ]
