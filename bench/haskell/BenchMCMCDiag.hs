{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-full-laziness -fno-cse #-}
-- | Diagnostic runner for the B7 MCMC bench (Phase B10b investigation).
--
-- Runs hanalyze NUTS on the 8-schools model with progressively
-- different configurations to localise the cause of poor ESS:
--
--   * default          : nutsStepSize=0.08, adapt=on (current)
--   * smaller-step     : nutsStepSize=0.02, adapt=off
--   * longer-warmup    : burnin 2000 (more dual-averaging)
--   * deeper-tree      : nutsMaxDepth=12 (default 10)
--
-- For each, prints accept rate, ESS(mu), ESS(tau), n samples > 1
-- distinct.
module Main where

import qualified Data.Map.Strict      as Map
import qualified Data.Text            as T
import qualified System.Random.MWC    as MWC
import           System.IO            (hSetBuffering, stdout, BufferMode (..))
import           Text.Printf          (printf)

import           Model.HBM            (Distribution (..), ModelP, sample,
                                       observe)
import           MCMC.Core            (Chain, chainAccepted, chainTotal,
                                       chainVals, posteriorMean,
                                       posteriorSD)
import           MCMC.NUTS            (NUTSConfig (..), defaultNUTSConfig,
                                       nuts)
import           Stat.MCMC            (ess)

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

runOne :: String -> NUTSConfig -> IO ()
runOne label cfg = do
  g <- MWC.create
  ch <- nuts schoolModel cfg initParams g
  reportChain label ch

reportChain :: String -> Chain -> IO ()
reportChain label ch = do
  let muV    = chainVals "mu"  ch
      tauV   = chainVals "tau" ch
      muEss  = ess muV
      tauEss = ess tauV
      acc    = fromIntegral (chainAccepted ch)
             / max 1 (fromIntegral (chainTotal ch)) :: Double
      muMean = maybe 0 id (posteriorMean "mu" ch)
      muSD   = maybe 0 id (posteriorSD   "mu" ch)
      tauMean = maybe 0 id (posteriorMean "tau" ch)
      muDistinct  = length (uniq muV)
      tauDistinct = length (uniq tauV)
      uniq xs = go xs []
        where go [] acc' = reverse acc'
              go (x:xs') acc'
                | x `elem` acc' = go xs' acc'
                | otherwise     = go xs' (x:acc')
  printf "=== %s ===\n" label
  printf "  accept       %.3f\n" acc
  printf "  mu  mean=%.3f sd=%.3f ess=%.1f distinct=%d\n"
    muMean muSD muEss muDistinct
  printf "  tau mean=%.3f                ess=%.1f distinct=%d\n"
    tauMean tauEss tauDistinct
  putStrLn ""

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  putStrLn "================================================="
  putStrLn "  NUTS on 8-schools — diagnostic (B10b)"
  putStrLn "  Goal: explain ESS(mu)=42 vs blackjax ess=810"
  putStrLn "================================================="
  putStrLn ""

  let baseCfg = defaultNUTSConfig
        { nutsIterations = 1000
        , nutsBurnIn     = 500
        , nutsStepSize   = 0.08
        , nutsMaxDepth   = 10
        , nutsAdaptStepSize = True
        , nutsTargetAccept  = 0.8
        }

  -- Reduced iterations for fast diagnostic. ESS scales linearly with
  -- iterations so ratios stay informative.
  let cfg = baseCfg { nutsIterations = 200, nutsBurnIn = 100 }

  -- 1. baseline
  runOne "baseline (eps=0.08, adapt=on)" cfg

  -- 2. small step, no adapt
  runOne "small-step (eps=0.02, adapt=off)"
    cfg { nutsStepSize = 0.02, nutsAdaptStepSize = False }

  -- 3. high target accept (forces smaller eps)
  runOne "high-target (eps=0.08, target=0.95)"
    cfg { nutsTargetAccept = 0.95 }

  -- 4. shallower tree (limit search space)
  runOne "shallow-tree (maxDepth=5)"
    cfg { nutsMaxDepth = 5 }

  -- 5. baseline at full 1000-sample size for comparison with B7
  runOne "full-size (1000 samples, baseline)" baseCfg
    { nutsIterations = 1000, nutsBurnIn = 500 }
