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
import Viz.MCMC (printPosteriorSummary, posteriorSummaryFile,
                 tracePlotHDIFile, rankPlotFile, ppcPlotFile,
                 pairScatterDivFile)
import Stat.PosteriorPredictive (posteriorPredictive)
import Viz.Core (defaultConfig, OutputFormat (..), PlotConfig (..))

cfg :: NUTSConfig
cfg = defaultNUTSConfig
        { nutsIterations = 1000
        , nutsBurnIn     = 500
        , nutsStepSize   = 0.1
        }

obsData :: [Double]
obsData =
  [1.2, 0.9, 1.4, 0.7, 1.1, 1.0, 1.3, 0.95, 1.05, 1.15,
   0.85, 1.25, 0.95, 1.18, 1.02]

simpleModel :: ModelP ()
simpleModel = do
  mu  <- sample "mu"    (Normal 0 5)
  sig <- sample "sigma" (HalfNormal 2)
  observe "y" (Normal mu sig) obsData

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

  -- ── HDI 帯付きトレース ──
  let traceCfg = (defaultConfig "Trace with 94% HDI")
                   { plotWidth = 700, plotHeight = 90 }
  tracePlotHDIFile HTML "trace-hdi.html" traceCfg 0.94 ["mu", "sigma"] ch
  putStrLn "  → trace-hdi.html (HDI 帯付きトレース)"

  -- ── Rank plot (多チェーン収束診断) ──
  let rankCfg = (defaultConfig "Rank plot — chain uniformity")
                  { plotWidth = 700, plotHeight = 100 }
  rankPlotFile HTML "rank.html" rankCfg 20 ["mu", "sigma"] chs
  putStrLn "  → rank.html (Rank plot, 4 chains)"

  -- ── Posterior predictive check ──
  preds <- posteriorPredictive simpleModel ch gen
  let yReps = [Map.findWithDefault [] "y" m | m <- preds]
  let ppcCfg = (defaultConfig "Posterior predictive check (y)")
                 { plotWidth = 700, plotHeight = 280 }
  ppcPlotFile HTML "ppc.html" ppcCfg obsData yReps 50
  putStrLn "  → ppc.html (PP check, 観測 vs 予測 50 ドロー)"

  -- ── Divergence overlay (Phase F5; Phase G4 で NUTS から自動取得予定) ──
  -- 現状はモック divergent indices [10, 50, 200, 500] で描画機構を検証。
  let divCfg = (defaultConfig "Pair plot — divergence overlay (mock)")
                 { plotWidth = 500, plotHeight = 400 }
      mockDiv = [10, 50, 200, 500]
  pairScatterDivFile HTML "pair-div.html" divCfg "mu" "sigma" ch mockDiv
  putStrLn "  → pair-div.html (4 mock divergent points)"
  putStrLn ""

  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  ✓ Posterior summary が動作 (mean/sd/HDI/ESS/R-hat)"
  putStrLn "═══════════════════════════════════════════════════════════════"
