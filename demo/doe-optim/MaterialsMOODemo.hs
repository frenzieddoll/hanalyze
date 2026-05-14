{-# LANGUAGE OverloadedStrings #-}
-- | Phase W: 統合デモ — 材料科学シナリオ。
--
-- シナリオ: 合金組成 (x ∈ [0, 1] が銅含有率) を最適化。
--   - 強度 (高いほうが良い): strength(x) = 100 * sin(3x) + 50x + 20
--   - コスト (低いほうが良い): cost(x) = 50 + 100*x
--   - 重量 (低いほうが良い): weight(x) = 10 + 5*x
--
-- 全 3 目的を NSGA-II で同時最適化、Pareto front を可視化。
module Main where

import Text.Printf (printf)
import System.Random.MWC (createSystemRandom)

import Hanalyze.Optim.NSGA   (Solution (..), NSGAConfig (..), defaultNSGAConfig,
                     nsga2)
import Hanalyze.Optim.Pareto (hypervolume)
import Hanalyze.Viz.Pareto   (parallelCoordinatesFile, paretoPairFile,
                              solutionsToPlotData)
import Hanalyze.Viz.Core     (defaultConfig, OutputFormat (..), PlotConfig (..))

-- 材料科学シナリオ: x ∈ [0, 1] (合金中の銅含有率)
-- すべて最小化問題に統一 (強度は -strength)
materialsObjective :: [Double] -> [Double]
materialsObjective [x] =
  let strength = 100 * sin (3 * x) + 50 * x + 20    -- 最大化 → 最小化のため -符号
      cost     = 50 + 100 * x
      weight   = 10 + 5 * x
  in [-strength, cost, weight]
materialsObjective _ = error "1D"

main :: IO ()
main = do
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  Phase W: 材料科学 統合デモ"
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn ""
  putStrLn "シナリオ: 合金の銅含有率 x ∈ [0, 1] を最適化"
  putStrLn "  目的 1 (-strength): 100·sin(3x) + 50x + 20 を最大化"
  putStrLn "  目的 2 (cost):       50 + 100x を最小化"
  putStrLn "  目的 3 (weight):     10 + 5x を最小化"
  putStrLn "  全 3 目的を最小化に統一して NSGA-II"
  putStrLn ""

  gen <- createSystemRandom

  -- NSGA-II で 3 目的最適化
  let cfg = defaultNSGAConfig
              { nsgaPopSize = 80
              , nsgaGenerations = 150
              }
  front <- nsga2 cfg materialsObjective [(0, 1)] gen
  printf "Pareto front サイズ: %d\n" (length front)
  putStrLn ""

  putStrLn "[1] Pareto front の代表点 (5 個)"
  let sortedFront = sortByObj 0 front
      idxs = [0, length sortedFront `div` 4
             , length sortedFront `div` 2
             , 3 * length sortedFront `div` 4
             , length sortedFront - 1]
      reps = [sortedFront !! i | i <- idxs, i < length sortedFront]
  printf "  %-15s %-15s %-15s %-15s\n"
         ("x (Cu 比率)" :: String) ("strength" :: String)
         ("cost" :: String) ("weight" :: String)
  mapM_ (\s -> do
            let [x] = solDecision s
                [neg_str, c, w] = solObjectives s
            printf "  %14.4f  %14.2f  %14.2f  %14.2f\n"
                   x (-neg_str) c w)
        reps
  putStrLn ""

  -- HV 評価
  let allObjs = map solObjectives front
      refPt = [-(-50.0), 200.0, 16.0]   -- 各目的の悪い値
  printf "[2] HV (ref = %s) = %.3f\n" (show refPt) (hypervolume refPt allObjs)
  putStrLn ""

  -- 可視化
  putStrLn "[3] 可視化"
  let vCfg t = (defaultConfig t)
                 { plotWidth = 700, plotHeight = 350 }
  -- 130 規約: Solution → PlotData に変換してから Viz に渡す
  let labels = ["-strength", "cost", "weight"]
      pdFront = solutionsToPlotData labels front
  parallelCoordinatesFile HTML "materials-parallel.html"
    (vCfg "材料 Pareto front — 並行座標 (-strength / cost / weight)")
    labels pdFront
  paretoPairFile HTML "materials-pair.html"
    (vCfg "材料 Pareto front — ペア散布")
    labels pdFront
  putStrLn "  → materials-parallel.html / materials-pair.html"
  putStrLn ""

  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  ✓ 材料 3 目的最適化が完了"
  putStrLn "    Pareto front から要件に応じて 1 点を選ぶ:"
  putStrLn "    - 強度重視: 銅含有率 高、コスト・重量増 (右端)"
  putStrLn "    - コスト重視: 銅含有率 低、強度低 (左端)"
  putStrLn "    - バランス: 中央付近"
  putStrLn "═══════════════════════════════════════════════════════════════"

  where
    sortByObj :: Int -> [Solution] -> [Solution]
    sortByObj j = qs
      where qs []     = []
            qs (p:xs) = qs [x | x <- xs, solObjectives x !! j <= solObjectives p !! j]
                       ++ [p]
                       ++ qs [x | x <- xs, solObjectives x !! j > solObjectives p !! j]
