{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | Phase 3.1 + 3.3: Forest plot と Pseudo-BMA モデル比較デモ。
module Main where

import qualified Data.Map.Strict as Map
import Text.Printf (printf)
import System.Random.MWC (createSystemRandom)

import Hanalyze.MCMC.Core (Chain)
import Hanalyze.MCMC.NUTS (nuts, defaultNUTSConfig, NUTSConfig (..))
import Hanalyze.Model.HBM (ModelP, sample, observe, Distribution (..))
import Hanalyze.Stat.ModelSelect
  (CompareEntry (..), CompareResult (..), compareModels, chainLogLikMatrix)
import Hanalyze.Viz.Core (PlotConfig (..), OutputFormat (..))
import Hanalyze.Viz.MCMC (forestPlotFile)

-- ---------------------------------------------------------------------------
-- 3 モデル: 分散事前を変えて比較
-- ---------------------------------------------------------------------------

obs :: [Double]
obs = [1.5, 2.1, 1.8, 2.5, 1.9, 2.3, 1.7, 2.0, 2.2, 1.6,
       2.0, 1.7, 2.4, 1.5, 2.1, 1.8, 2.3, 1.9, 2.0, 1.6]

modelHN :: ModelP ()
modelHN = do
  mu  <- sample "mu" (Normal 0 10)
  sig <- sample "sigma" (HalfNormal 5)
  observe "y" (Normal mu sig) obs

modelHC :: ModelP ()
modelHC = do
  mu  <- sample "mu" (Normal 0 10)
  sig <- sample "sigma" (HalfCauchy 2)
  observe "y" (Normal mu sig) obs

modelExp :: ModelP ()
modelExp = do
  mu  <- sample "mu" (Normal 0 10)
  sig <- sample "sigma" (Exponential 1)
  observe "y" (Normal mu sig) obs

cfg :: NUTSConfig
cfg = defaultNUTSConfig
        { nutsIterations = 1500
        , nutsBurnIn     = 500
        , nutsStepSize   = 0.1
        }

main :: IO ()
main = do
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  Phase 3.1/3.3: Forest plot + Model comparison weights"
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn ""
  printf "  3 つのモデル (異なる σ 事前):\n"
  putStrLn "    HN  : sigma ~ HalfNormal(5)"
  putStrLn "    HC  : sigma ~ HalfCauchy(2)"
  putStrLn "    Exp : sigma ~ Exponential(1)"
  putStrLn ""

  gen <- createSystemRandom
  let initP = Map.fromList [("mu", 0.0), ("sigma", 1.0)]

  putStrLn "[1] 3 モデルを NUTS で推論"
  ch1 <- nuts modelHN  cfg initP gen
  ch2 <- nuts modelHC  cfg initP gen
  ch3 <- nuts modelExp cfg initP gen
  putStrLn "  完了"
  putStrLn ""

  -- ── Forest plot ──
  putStrLn "[2] Forest plot 出力 (forest_compare.html)"
  let fcfg = PlotConfig "Posterior 95% CI per model" 600 400
      -- 各モデルを別 chain として渡すと色分けされる
      chs  = [ch1, ch2, ch3]
  forestPlotFile HTML "forest_compare.html" fcfg ["mu", "sigma"] chs
  putStrLn "  → forest_compare.html"
  putStrLn ""

  -- ── Pseudo-BMA model comparison ──
  putStrLn "[3] WAIC/LOO + Pseudo-BMA 重み (compareModels)"
  let entries =
        [ CompareEntry "HN"  (chainLogLikMatrix modelHN  ch1)
        , CompareEntry "HC"  (chainLogLikMatrix modelHC  ch2)
        , CompareEntry "Exp" (chainLogLikMatrix modelExp ch3)
        ]
      results = compareModels entries
  printf "  %-6s  %10s  %10s  %10s  %10s  %8s  %8s\n"
         ("model"::String) ("WAIC"::String) ("dWAIC"::String)
         ("LOO"::String)   ("dLOO"::String) ("SE"::String)
         ("weight"::String)
  mapM_ (\r ->
    printf "  %-6s  %10.3f  %10.3f  %10.3f  %10.3f  %8.3f  %8.3f%s\n"
           (crLabel r) (crWAIC r) (crDeltaWAIC r)
           (crLOO r)   (crDeltaLOO r) (crSE r) (crWeight r)
           ((if crDeltaWAIC r == 0 then " *" else "  ") :: String))
    results
  putStrLn ""
  putStrLn "  解釈:"
  putStrLn "    weight = Pseudo-BMA 重み (Σ = 1)"
  putStrLn "    重みが分散している = モデル選択の不確実性が高い"
  putStrLn "    重みが特定モデルに集中 = そのモデルが圧倒的"
  putStrLn ""

  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  ✓ Forest plot + compareModels が正常動作"
  putStrLn "═══════════════════════════════════════════════════════════════"
