{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | Dirichlet 事前 + Categorical 観測のデモ。
--
-- 3 カテゴリの観測 (生起頻度: 50, 30, 20) に対し、
-- Dir(1,1,1) (一様事前) を Dirichlet にして π を推定。
--
-- 共役: 事後は Dir(1+50, 1+30, 1+20) = Dir(51, 31, 21) で
-- 平均は (51, 31, 21) / 103 = (0.495, 0.301, 0.204)。
-- これと推定値が一致するかを確認。
module Main where

import qualified Data.Map.Strict as Map
import System.Random.MWC (createSystemRandom)

import MCMC.NUTS (nuts, defaultNUTSConfig, NUTSConfig (..))
import Model.HBM (ModelP, dirichlet, observe, Distribution (..),
                  augmentChainWithDeterministic)
import Viz.MCMC (printPosteriorSummary, posteriorSummaryFile,
                 tracePlotHDIFile)
import Viz.Core (defaultConfig, OutputFormat (..), PlotConfig (..))

cfg :: NUTSConfig
cfg = defaultNUTSConfig
        { nutsIterations = 2000
        , nutsBurnIn     = 1000
        , nutsStepSize   = 0.1
        }

-- 生成データ: カテゴリ 0,1,2 の頻度 50, 30, 20
genObs :: [Double]
genObs = replicate 50 0 ++ replicate 30 1 ++ replicate 20 2

dirichletModel :: ModelP ()
dirichletModel = do
  pis <- dirichlet "pi" [1, 1, 1]   -- Dir(1,1,1) 一様事前
  observe "y" (Categorical pis) genObs

main :: IO ()
main = do
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  Dirichlet 事前 + Categorical 観測"
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn ""

  putStrLn "観測: カテゴリ 0/1/2 が 50/30/20 件 (合計 100)"
  putStrLn "事後 (共役): Dir(51, 31, 21)"
  putStrLn "  期待値: π_0 = 0.495, π_1 = 0.301, π_2 = 0.204"
  putStrLn ""

  gen <- createSystemRandom

  -- 初期値: Beta が UnitIntervalT 経由で sample されるため
  -- pi_b0, pi_b1 は (0,1)
  rawCh <- nuts dirichletModel cfg
                (Map.fromList [("pi_b0", 0.5), ("pi_b1", 0.5)]) gen
  let ch = augmentChainWithDeterministic dirichletModel rawCh

  putStrLn "[1] Posterior summary (β: stick-breaking 棒折り、π: 派生量)"
  let names = [ "pi_b0", "pi_b1"          -- raw latent (Beta)
              , "pi_0", "pi_1", "pi_2" ]  -- derived simplex π
  printPosteriorSummary names [ch]
  putStrLn ""

  -- HTML 出力
  posteriorSummaryFile "dirichlet-summary.html" "Dirichlet posterior" names [ch]
  let traceCfg = (defaultConfig "Dirichlet trace (β + π)")
                   { plotWidth = 700, plotHeight = 90 }
  tracePlotHDIFile HTML "dirichlet-trace.html" traceCfg 0.94 names ch
  putStrLn "  → dirichlet-summary.html / dirichlet-trace.html"
  putStrLn ""

  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  ✓ Dirichlet が stick-breaking 経由で latent 化"
  putStrLn "    π_0 + π_1 + π_2 = 1 がサンプル単位で自動的に成立"
  putStrLn "═══════════════════════════════════════════════════════════════"
