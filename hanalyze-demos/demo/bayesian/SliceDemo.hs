{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | Slice sampler のデモ (Phase J3)。
--
-- Slice sampling (Neal 2003) はステップサイズ調整不要で、
-- log-density を評価できれば任意分布から sample できる univariate 法。
-- 多変量モデルは coordinate-wise sweep で扱う。
--
-- ここでは MH/NUTS と同じ Normal モデルで比較し、Slice の利点
-- (受容率調整不要、概ね高 ESS) を確認する。
module Main where

import qualified Data.Map.Strict as Map
import Text.Printf (printf)
import System.Random.MWC (createSystemRandom)

import Hanalyze.MCMC.MH    (metropolis, defaultMCMCConfig, MCMCConfig (..))
import Hanalyze.MCMC.NUTS  (nuts, defaultNUTSConfig, NUTSConfig (..))
import Hanalyze.MCMC.Slice (slice, defaultSliceConfig, SliceConfig (..))
import Hanalyze.MCMC.Core  (acceptanceRate)
import Hanalyze.Model.HBM  (ModelP, sample, observe, Distribution (..))
import Hanalyze.Viz.MCMC   (printPosteriorSummary)

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
  putStrLn "  Slice sampler vs Metropolis vs NUTS (Phase J3)"
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn ""

  gen <- createSystemRandom
  let init0 = Map.fromList [("mu", 1.0), ("sigma", 0.2)]

  -- ── Slice ──
  putStrLn "[1] Slice sampler (1000 iter sweep, 200 burn-in)"
  let scfg = (defaultSliceConfig ["mu", "sigma"])
               { sliceIterations = 1000
               , sliceBurnIn     = 200
               , sliceWidths     = Map.fromList
                   [("mu", 0.5), ("sigma", 0.2)]
               }
  chSlice <- slice simpleModel scfg init0 gen
  printPosteriorSummary ["mu", "sigma"] [chSlice]
  printf "  受容数 (sweep 内全 update のうち accept): %d\n"
         (case acceptanceRate chSlice of
            r -> round (r * 100 * 2 :: Double) :: Int)
  putStrLn ""

  -- ── Metropolis ──
  putStrLn "[2] Random Walk Metropolis (1500 iter, 500 burn-in)"
  let mcfg = (defaultMCMCConfig ["mu", "sigma"])
               { mcmcIterations = 1500
               , mcmcBurnIn     = 500
               , mcmcStepSizes  = Map.fromList
                   [("mu", 0.1), ("sigma", 0.05)]
               }
  chMH <- metropolis simpleModel mcfg init0 gen
  printPosteriorSummary ["mu", "sigma"] [chMH]
  printf "  受容率: %.1f%%\n" (acceptanceRate chMH * 100)
  putStrLn ""

  -- ── NUTS ──
  putStrLn "[3] NUTS (1000 iter, 500 burn-in)"
  let ncfg = defaultNUTSConfig
               { nutsIterations = 1000
               , nutsBurnIn     = 500
               , nutsStepSize   = 0.1
               }
  chNUTS <- nuts simpleModel ncfg init0 gen
  printPosteriorSummary ["mu", "sigma"] [chNUTS]
  putStrLn ""

  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  ✓ Slice sampler が動作 (ステップサイズ自動調整、勾配不要)"
  putStrLn "═══════════════════════════════════════════════════════════════"
