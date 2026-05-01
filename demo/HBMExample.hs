{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
-- Phase 4 + 5: Small hierarchical model example with MCMC inference
--
-- Hierarchical normal model for test scores across J schools:
--
--   μ      ~ Normal(0, 100)   -- global mean hyperprior
--   τ      ~ Exponential(0.1) -- between-school SD hyperprior
--   θ_j    ~ Normal(μ, τ)     -- school-specific mean  (j = 1..J)
--   y_ij   ~ Normal(θ_j, σ)   -- observations (σ = 5 treated as known)
--
module Main where

import Control.Monad (forM)
import qualified Data.Map.Strict as Map
import qualified Data.Text       as T
import qualified Data.Text.IO    as TIO
import Text.Printf (printf)

import Model.HBM
import MCMC.Core
import MCMC.MH   (MCMCConfig (..), defaultMCMCConfig, metropolis)
import MCMC.NUTS (nutsChains, NUTSConfig (..), defaultNUTSConfig)
import Stat.Distribution ()
import Stat.MCMC  (ess)
import Viz.Core      (openInBrowser)
import Viz.Report    (MCMCReport (..), defaultReport, renderReport)
import System.Random.MWC (createSystemRandom)

-- ---------------------------------------------------------------------------
-- Model
-- ---------------------------------------------------------------------------

sigma :: Double
sigma = 5.0   -- known observation SD

-- | Build the hierarchical model for the given group data.
schoolModel :: [[Double]] -> ModelP ()
schoolModel groupData = do
  mu  <- sample "mu"  (Normal 0 100)
  tau <- sample "tau" (Exponential 0.1)
  mapM_ (\(j, ys) -> do
    theta <- sample (T.pack ("theta_" ++ show j)) (Normal mu tau)
    observe (T.pack ("y_" ++ show j)) (Normal theta (realToFrac sigma)) ys)
    (zip [1 :: Int ..] groupData)

-- ---------------------------------------------------------------------------
-- Synthetic data  (3 schools, n = 4 each)
-- ---------------------------------------------------------------------------

schoolData :: [[Double]]
schoolData =
  [ [72, 68, 75, 71]    -- school 1: mean ≈ 71.5
  , [85, 88, 82, 90]    -- school 2: mean ≈ 86.25
  , [61, 65, 58, 63]    -- school 3: mean ≈ 61.75
  ]

schoolMeans :: [Double]
schoolMeans = map (\ys -> sum ys / fromIntegral (length ys)) schoolData

-- | Params near the MLE: global mean = grand mean, tau = inter-school SD,
-- theta_j = school sample mean.
trueParams :: Params
trueParams = Map.fromList $
  [ ("mu",  grandMean)
  , ("tau", interSD)
  ] ++
  zipWith (\j m -> (T.pack ("theta_" ++ show (j :: Int)), m))
          [1..] schoolMeans
  where
    grandMean = sum schoolMeans / fromIntegral (length schoolMeans)
    interSD   = sqrt (sum (map (\m -> (m - grandMean)^(2::Int)) schoolMeans)
                      / fromIntegral (length schoolMeans))

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

m :: ModelP ()
m = schoolModel schoolData

main :: IO ()
main = do

  -- ── 1. Model structure ─────────────────────────────────────────────────
  putStrLn "=== Model Structure ==="
  TIO.putStr (describeModel m)
  putStrLn $ "Latent variables: " ++ show (sampleNames m)
  putStrLn ""

  -- ── 1b. Build model graph (HBMP の Track 型で依存を自動抽出) ──────────
  let graph = buildModelGraph m

  -- ── 2. Log-joint at near-MLE parameters ────────────────────────────────
  putStrLn "=== Log-joint at near-MLE params ==="
  printParams trueParams
  printf "  logJoint      = %.4f\n" (logJoint      m trueParams)
  printf "  logPrior      = %.4f\n" (logPrior      m trueParams)
  printf "  logLikelihood = %.4f\n" (logLikelihood m trueParams)
  putStrLn ""

  -- ── 3. Effect of τ (between-school SD) ────────────────────────────────
  putStrLn "=== logJoint as τ varies (mu, theta_j fixed at near-MLE) ==="
  printf "  %-8s  %s\n" ("tau" :: String) ("logJoint" :: String)
  mapM_ (checkTau m) [0.5, 1, 2, 5, 10, 20, 50]
  putStrLn ""

  -- ── 4. Effect of μ (global mean) ──────────────────────────────────────
  putStrLn "=== logJoint as μ varies (others fixed at near-MLE) ==="
  printf "  %-8s  %s\n" ("mu" :: String) ("logJoint" :: String)
  mapM_ (checkMu m) [40, 55, 65, 73, 80, 90, 100]
  putStrLn ""

  -- ── 5. Invalid params ─────────────────────────────────────────────────
  putStrLn "=== Edge cases ==="
  let badTau = Map.insert "tau" (-1) trueParams
  printf "  tau = -1  (outside support): logJoint = %.4f\n" (logJoint m badTau)
  let missingTheta = Map.delete "theta_2" trueParams
  printf "  theta_2 missing:             logJoint = %.4f\n" (logJoint m missingTheta)
  putStrLn ""

  -- ── 6. Random Walk Metropolis ──────────────────────────────────────────
  putStrLn "=== Random Walk Metropolis (Phase 5) ==="
  gen <- createSystemRandom

  let names = sampleNames m
      cfg   = (defaultMCMCConfig names)
                { mcmcIterations = 5000
                , mcmcBurnIn     = 1000
                , mcmcStepSizes  = Map.fromList
                    [ ("mu",      5.0)
                    , ("tau",     2.0)
                    , ("theta_1", 3.0)
                    , ("theta_2", 3.0)
                    , ("theta_3", 3.0)
                    ]
                }

  chain <- metropolis m cfg trueParams gen

  printf "Acceptance rate: %.3f  (%d / %d)\n"
    (acceptanceRate chain)
    (chainAccepted chain)
    (chainTotal chain)
  putStrLn ""

  putStrLn "Posterior summaries (mean ± SD, 95% CI, ESS):"
  printf "  %-12s  %8s  %8s  %8s  %8s  %8s\n"
    ("param" :: String) ("mean" :: String) ("sd" :: String)
    ("2.5%" :: String) ("97.5%" :: String) ("ESS" :: String)
  mapM_ (printSummary chain) names
  putStrLn ""

  -- ── 7. Single-chain consolidated HTML report ─────────────────────────
  putStrLn "=== Generating consolidated report (single chain) ==="

  let report = (defaultReport "School Model — MCMC Report" chain names)
                 { reportGraph  = Just graph
                 , reportPairs  = [("mu", "tau")]
                 , reportMaxLag = 40
                 }
  renderReport "mcmc_report.html" report
  putStrLn "  mcmc_report.html  (model graph + summary + diagnostics + autocorr + pair plots)"

  -- ── 8. 4-chain NUTS + multi-chain report ──────────────────────────────
  putStrLn ""
  putStrLn "=== 4-chain NUTS (parallel) ==="
  let nutsCfg = defaultNUTSConfig
        { nutsIterations = 2000
        , nutsBurnIn     = 500
        , nutsStepSize   = 0.08
        }
  multiChains <- nutsChains m nutsCfg 4 trueParams gen
  mapM_ (\(i, ch) ->
    printf "  chain %d: accept=%.3f  mu_mean=%.2f  tau_mean=%.2f\n"
      (i :: Int)
      (acceptanceRate ch)
      (maybe 0 id $ posteriorMean "mu"  ch)
      (maybe 0 id $ posteriorMean "tau" ch)
    ) (zip [1..] multiChains)

  let multiReport = (defaultReport "School Model — 4-chain NUTS" (head multiChains) names)
                      { reportGraph  = Just graph
                      , reportChains = multiChains
                      , reportPairs  = [("mu", "tau")]
                      , reportMaxLag = 40
                      }
  renderReport "mcmc_report_multi.html" multiReport
  putStrLn "  mcmc_report_multi.html  (4-chain KDE + colored traces + R-hat)"
  openInBrowser "mcmc_report_multi.html"

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

printParams :: Params -> IO ()
printParams ps = mapM_ (\(k,v) -> printf "  %-12s = %.4f\n" k v) (Map.toAscList ps)

checkTau :: ModelP () -> Double -> IO ()
checkTau m tau =
  let ps = Map.insert "tau" tau trueParams
  in printf "  %-8.1f  %.4f\n" tau (logJoint m ps)

checkMu :: ModelP () -> Double -> IO ()
checkMu m mu =
  let ps = Map.insert "mu" mu trueParams
  in printf "  %-8.1f  %.4f\n" mu (logJoint m ps)

printSummary :: Chain -> T.Text -> IO ()
printSummary chain pname =
  let get f = maybe 0.0 id (f pname chain)
      mean_ = get posteriorMean
      sd_   = get posteriorSD
      lo    = get (posteriorQuantile 0.025)
      hi    = get (posteriorQuantile 0.975)
      ess_  = ess (chainVals pname chain)
  in printf "  %-12s  %8.3f  %8.3f  %8.3f  %8.3f  %8.0f\n"
       (T.unpack pname) mean_ sd_ lo hi ess_
