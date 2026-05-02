{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | Posterior summary table (az.summary 相当) のデモ。
--
-- 単一チェーン: mean / sd / 94% HDI / ESS
-- 多チェーン:    + R-hat (split R-hat、< 1.01 で収束)
module Main where

import qualified Data.Map.Strict as Map
import System.Random.MWC (createSystemRandom)

import MCMC.NUTS (nuts, nutsChains, defaultNUTSConfig, NUTSConfig (..))
import Model.HBM (ModelP, sample, observe, Distribution (..))
import Viz.MCMC (printPosteriorSummary, posteriorSummaryFile)

cfg :: NUTSConfig
cfg = defaultNUTSConfig
        { nutsIterations = 1000
        , nutsBurnIn     = 500
        , nutsStepSize   = 0.1
        }

simpleModel :: ModelP ()
simpleModel = do
  mu  <- sample "mu"    (Normal 0 5)
  sig <- sample "sigma" (HalfNormal 2)
  observe "y" (Normal mu sig)
    [1.2, 0.9, 1.4, 0.7, 1.1, 1.0, 1.3, 0.95, 1.05, 1.15,
     0.85, 1.25, 0.95, 1.18, 1.02]

main :: IO ()
main = do
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  Posterior summary (az.summary 相当)"
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn ""

  gen <- createSystemRandom

  -- ── 単一チェーン ──
  putStrLn "[1] 単一チェーン"
  ch <- nuts simpleModel cfg
              (Map.fromList [("mu", 1), ("sigma", 1)]) gen
  printPosteriorSummary ["mu", "sigma"] [ch]
  putStrLn ""

  -- ── 多チェーン (R-hat 付き) ──
  putStrLn "[2] 多チェーン (R-hat 付き)"
  chs <- nutsChains simpleModel cfg 4
                    (Map.fromList [("mu", 1), ("sigma", 1)]) gen
  printPosteriorSummary ["mu", "sigma"] chs
  putStrLn ""

  -- ── HTML 出力 ──
  posteriorSummaryFile "summary-single.html"
    "Posterior summary (single chain)" ["mu", "sigma"] [ch]
  posteriorSummaryFile "summary-multi.html"
    "Posterior summary (4 chains, R-hat)" ["mu", "sigma"] chs
  putStrLn "  → summary-single.html / summary-multi.html"
  putStrLn ""

  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  ✓ Posterior summary が動作 (mean/sd/HDI/ESS/R-hat)"
  putStrLn "═══════════════════════════════════════════════════════════════"
