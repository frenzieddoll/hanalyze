{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | pm.Deterministic 相当のデモ。
--
-- σ をサンプリングして、派生量 τ = 1/σ² (precision) と
-- log_sigma = log(σ) も保存する。Posterior summary には latent と
-- derived が同じテーブルに混ざって表示される。
module Main where

import qualified Data.Map.Strict as Map
import System.Random.MWC (createSystemRandom)

import Hanalyze.MCMC.NUTS (nuts, defaultNUTSConfig, NUTSConfig (..))
import Hanalyze.Model.HBM (ModelP, sample, observe, deterministic,
                  Distribution (..), augmentChainWithDeterministic)
import Hanalyze.Viz.MCMC (printPosteriorSummary, posteriorSummaryFile,
                 tracePlotHDIFile)
import Hanalyze.Viz.Core (defaultConfig, OutputFormat (..), PlotConfig (..))

cfg :: NUTSConfig
cfg = defaultNUTSConfig
        { nutsIterations = 1500
        , nutsBurnIn     = 500
        , nutsStepSize   = 0.1
        }

modelWithDeterministic :: ModelP ()
modelWithDeterministic = do
  mu  <- sample "mu"    (Normal 0 5)
  sig <- sample "sigma" (HalfNormal 2)
  -- 派生量 1: precision = 1/σ²
  _ <- deterministic "tau"       (1 / (sig * sig))
  -- 派生量 2: log(σ)
  _ <- deterministic "log_sigma" (log sig)
  -- 派生量 3: 信号対雑音比
  _ <- deterministic "snr"       (mu / sig)
  observe "y" (Normal mu sig)
    [1.2, 0.9, 1.4, 0.7, 1.1, 1.0, 1.3, 0.95, 1.05, 1.15,
     0.85, 1.25, 0.95, 1.18, 1.02]

main :: IO ()
main = do
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  pm.Deterministic デモ (派生量を Chain に保存)"
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn ""

  gen <- createSystemRandom
  rawCh <- nuts modelWithDeterministic cfg
                (Map.fromList [("mu", 1), ("sigma", 1)]) gen

  -- 派生量を Chain に注入
  let ch = augmentChainWithDeterministic modelWithDeterministic rawCh
  let names = ["mu", "sigma", "tau", "log_sigma", "snr"]

  putStrLn "[1] Posterior summary (latent + derived 混在)"
  printPosteriorSummary names [ch]
  putStrLn ""

  -- HTML 出力
  posteriorSummaryFile "summary-determ.html"
    "Posterior with deterministic" names [ch]
  let traceCfg = (defaultConfig "Trace (latent + derived)")
                   { plotWidth = 700, plotHeight = 90 }
  tracePlotHDIFile HTML "trace-determ.html" traceCfg 0.94 names ch
  putStrLn "  → summary-determ.html / trace-determ.html"
  putStrLn ""

  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  ✓ Deterministic 派生量が posterior summary / trace に出る"
  putStrLn "═══════════════════════════════════════════════════════════════"
