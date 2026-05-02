{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | 非中心化パラメタ化 (non-centered) のデモ。
--
-- Neal's funnel:
--   v ~ Normal(0, 3)
--   x | v ~ Normal(0, exp(v/2))
--
-- Centered: x を直接 sample → v が大きいと x のスケールが爆発、
--           小さいと潰れて HMC の事後分布が病的に。
-- Non-centered: x_raw ~ Normal(0, 1) と v は独立にサンプル、
--               x = exp(v/2) * x_raw を派生量として出す。
--
-- BFMI 値の改善で診断する (Phase E の energyPlot を流用)。
module Main where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Text.Printf (printf)
import System.Random.MWC (createSystemRandom)

import MCMC.Core (chainEnergy)
import MCMC.NUTS (nuts, defaultNUTSConfig, NUTSConfig (..))
import Model.HBM (ModelP, sample, Distribution (..),
                  nonCenteredNormal, augmentChainWithDeterministic)
import Stat.MCMC (bfmi)
import Viz.Core  (defaultConfig, OutputFormat (..), PlotConfig (..))
import Viz.MCMC  (energyPlotFile, posteriorSummaryFile,
                  printPosteriorSummary)

cfg :: NUTSConfig
cfg = defaultNUTSConfig
        { nutsIterations = 2000
        , nutsBurnIn     = 1000
        , nutsStepSize   = 0.1
        }

-- ---------------------------------------------------------------------------
-- Centered: x ~ Normal(0, exp(v/2))
-- ---------------------------------------------------------------------------
centeredFunnel :: ModelP ()
centeredFunnel = do
  v <- sample "v" (Normal 0 3)
  _ <- sample "x" (Normal 0 (exp (v / 2)))
  return ()

-- ---------------------------------------------------------------------------
-- Non-centered: x_raw ~ Normal(0,1) → x = exp(v/2) * x_raw
-- ---------------------------------------------------------------------------
nonCenteredFunnel :: ModelP ()
nonCenteredFunnel = do
  v <- sample "v" (Normal 0 3)
  _ <- nonCenteredNormal "x" 0 (exp (v / 2))
  return ()

main :: IO ()
main = do
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  非中心化パラメタ化 vs centered (Neal's funnel)"
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn ""

  gen <- createSystemRandom

  -- ── Centered ──
  putStrLn "[1] Centered: x ~ Normal(0, exp(v/2))"
  ch1 <- nuts centeredFunnel cfg
              (Map.fromList [("v", 0), ("x", 0)]) gen
  let bfmi1 = fromMaybe (0/0) (bfmi (chainEnergy ch1))
  printf "  BFMI = %.3f\n" bfmi1
  printPosteriorSummary ["v", "x"] [ch1]
  putStrLn ""

  -- ── Non-centered ──
  putStrLn "[2] Non-centered: x_raw ~ Normal(0,1), x = exp(v/2) * x_raw"
  ch2raw <- nuts nonCenteredFunnel cfg
                 (Map.fromList [("v", 0), ("x_raw", 0)]) gen
  let ch2   = augmentChainWithDeterministic nonCenteredFunnel ch2raw
      bfmi2 = fromMaybe (0/0) (bfmi (chainEnergy ch2raw))
  printf "  BFMI = %.3f\n" bfmi2
  printPosteriorSummary ["v", "x_raw", "x"] [ch2]
  putStrLn ""

  -- ── 可視化: Energy plot 比較 ──
  let ecfg t = (defaultConfig t)
                 { plotWidth = 600, plotHeight = 250 }
  energyPlotFile HTML "funnel-centered-energy.html"
    (ecfg "Centered funnel") ch1
  energyPlotFile HTML "funnel-noncenter-energy.html"
    (ecfg "Non-centered funnel") ch2raw
  putStrLn "  → funnel-centered-energy.html / funnel-noncenter-energy.html"

  posteriorSummaryFile "funnel-centered.html" "Centered funnel"
    ["v", "x"] [ch1]
  posteriorSummaryFile "funnel-noncenter.html" "Non-centered funnel"
    ["v", "x_raw", "x"] [ch2]
  putStrLn "  → funnel-centered.html / funnel-noncenter.html"
  putStrLn ""

  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  ✓ Non-centered では x_raw が posterior に保存され、"
  putStrLn "    x は派生量として記録される。BFMI で改善度を比較。"
  putStrLn "═══════════════════════════════════════════════════════════════"
