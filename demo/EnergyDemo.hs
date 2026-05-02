{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | NUTS の Energy plot / BFMI 診断デモ。
--
-- BFMI < 0.3 → reparameterization 推奨 (典型例: Neal's funnel)。
-- 0.3 以上が望ましく、PyMC の経験則ではしばしば 0.5 を目安にする。
--
-- 例 1: 単純なガウシアンモデル → BFMI 高い (= 健全)
-- 例 2: Neal's funnel (centered) → BFMI 低い ことを期待
module Main where

import qualified Data.Map.Strict as Map
import Text.Printf (printf)
import System.Random.MWC (createSystemRandom)

import MCMC.Core (chainEnergy)
import MCMC.NUTS (nuts, defaultNUTSConfig, NUTSConfig (..))
import Model.HBM (ModelP, sample, observe, Distribution (..))
import Stat.MCMC (bfmi)
import Viz.Core  (PlotConfig (..), defaultConfig, OutputFormat (..))
import Viz.MCMC  (energyPlotFile)

cfg :: NUTSConfig
cfg = defaultNUTSConfig
        { nutsIterations = 2000
        , nutsBurnIn     = 500
        , nutsStepSize   = 0.1
        }

-- ---------------------------------------------------------------------------
-- 例 1: 普通の正規回帰
-- ---------------------------------------------------------------------------

healthyModel :: ModelP ()
healthyModel = do
  mu  <- sample "mu"    (Normal 0 5)
  sig <- sample "sigma" (HalfNormal 2)
  observe "y" (Normal mu sig) [1.2, 0.9, 1.4, 0.7, 1.1, 1.0, 1.3, 0.95, 1.05, 1.15]

-- ---------------------------------------------------------------------------
-- 例 2: Neal's funnel (centered) — 病的な階層構造
-- ---------------------------------------------------------------------------
-- v ~ Normal(0, 3),  x | v ~ Normal(0, exp(v/2))
-- v が大きいと x の分散が爆発、小さいと潰れる → エネルギー方向の探索失敗。

funnelModel :: ModelP ()
funnelModel = do
  v <- sample "v" (Normal 0 3)
  _ <- sample "x" (Normal 0 (exp (v / 2)))
  return ()

main :: IO ()
main = do
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  Energy plot / BFMI 診断デモ"
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn ""

  gen <- createSystemRandom

  -- ── 例 1: 健全 ──
  putStrLn "[1] 健全な Gaussian モデル"
  ch1 <- nuts healthyModel cfg
              (Map.fromList [("mu", 1), ("sigma", 1)]) gen
  let es1   = chainEnergy ch1
      bfmi1 = bfmi es1
  printf "  Energy 列: %d 件、平均 = %.3f\n"
         (length es1) (sum es1 / fromIntegral (length es1))
  case bfmi1 of
    Just v  -> printf "  BFMI = %.3f  (>0.3 で良好、>0.5 で理想)\n" v
    Nothing -> putStrLn "  BFMI: 計算不能"
  energyPlotFile HTML "energy-healthy.html"
    (defaultConfig "Energy plot") { plotTitle = "Energy plot — healthy model"
                                 , plotWidth = 600, plotHeight = 250 } ch1
  putStrLn "  → energy-healthy.html"
  putStrLn ""

  -- ── 例 2: Funnel ──
  putStrLn "[2] Neal's funnel (centered parameterization)"
  ch2 <- nuts funnelModel cfg
              (Map.fromList [("v", 0), ("x", 0)]) gen
  let es2   = chainEnergy ch2
      bfmi2 = bfmi es2
  printf "  Energy 列: %d 件、平均 = %.3f\n"
         (length es2) (sum es2 / fromIntegral (length es2))
  case bfmi2 of
    Just v  -> printf "  BFMI = %.3f  (低い場合は reparameterization 推奨)\n" v
    Nothing -> putStrLn "  BFMI: 計算不能"
  energyPlotFile HTML "energy-funnel.html"
    (defaultConfig "Energy plot") { plotTitle = "Energy plot — Neal's funnel"
                                 , plotWidth = 600, plotHeight = 250 } ch2
  putStrLn "  → energy-funnel.html"
  putStrLn ""

  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  ✓ Energy plot / BFMI が動作"
  putStrLn "    NUTS のサンプル列は energy も保持 (chainEnergy フィールド)"
  putStrLn "═══════════════════════════════════════════════════════════════"
