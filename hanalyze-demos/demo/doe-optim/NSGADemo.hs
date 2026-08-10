{-# LANGUAGE OverloadedStrings #-}
-- | Phase S4: NSGA-II 本体の動作確認。
--
-- 古典的ベンチマーク ZDT1 と Schaffer 関数で Pareto front を再現。
-- 結果は HV / IGD で評価。
module Main where

import Text.Printf (printf)
import System.Random.MWC (createSystemRandom)

import Hanalyze.Optim.NSGA   (Solution (..), NSGAConfig (..), defaultNSGAConfig,
                     nsga2)
import Hanalyze.Optim.Pareto (hypervolume, igd)
import Hanalyze.Viz.Pareto    (paretoCompareFile, parallelCoordinatesFile,
                               solutionsToPlotData)
import Hanalyze.Viz.PlotData  (PlotData (..), fromMixedColumns)
import Hanalyze.Viz.Core      (defaultConfig, OutputFormat (..), PlotConfig (..))
import qualified Data.Vector  as V
import qualified Data.Text    as T

-- ---------------------------------------------------------------------------
-- ZDT1 (Zitzler-Deb-Thiele 2000):
--   f1(x) = x_1
--   f2(x) = g(x) * (1 - sqrt(f1/g))
--   g(x)  = 1 + 9 * (sum x_2..x_n) / (n-1)
-- 真の Pareto front: f1 ∈ [0, 1], f2 = 1 - sqrt(f1)
-- 全変数 [0, 1]
-- ---------------------------------------------------------------------------

zdt1 :: Int -> [Double] -> [Double]
zdt1 n x =
  let f1 = head x
      g  = 1 + 9 * sum (drop 1 x) / fromIntegral (n - 1)
      f2 = g * (1 - sqrt (f1 / g))
  in [f1, f2]

zdt1TrueFront :: Int -> [[Double]]
zdt1TrueFront k =
  [ [f1, 1 - sqrt f1]
  | i <- [0 .. k - 1]
  , let f1 = fromIntegral i / fromIntegral (k - 1) ]

-- ---------------------------------------------------------------------------
-- Schaffer 関数 (Schaffer 1985):
--   f1(x) = x²
--   f2(x) = (x - 2)²
-- 真の Pareto front: x ∈ [0, 2]
-- ---------------------------------------------------------------------------

schaffer :: [Double] -> [Double]
schaffer [x] = [x * x, (x - 2) ** 2]
schaffer _   = error "schaffer: 1 dim"

schafferTrueFront :: Int -> [[Double]]
schafferTrueFront k =
  [ [x * x, (x - 2) ** 2]
  | i <- [0 .. k - 1]
  , let x = 2 * fromIntegral i / fromIntegral (k - 1) ]

main :: IO ()
main = do
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  Phase S4: NSGA-II 本体の動作確認"
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn ""

  gen <- createSystemRandom

  -- ── Schaffer (1D, 簡易) ──
  putStrLn "[1] Schaffer 関数 (1 変数, 2 目的)"
  let cfg1 = defaultNSGAConfig { nsgaPopSize = 50, nsgaGenerations = 100 }
  front1 <- nsga2 cfg1 schaffer [(0, 2)] gen
  let objs1 = map solObjectives front1
  printf "  最終 front サイズ: %d\n" (length front1)
  printf "  HV (ref [4.5, 4.5]) = %.4f\n" (hypervolume [4.5, 4.5] objs1)
  printf "  IGD (vs 真の front) = %.4f\n"
         (igd (schafferTrueFront 100) objs1)
  printf "  サンプル: %s\n" (show (take 3 (map (round2 . solObjectives) front1)))
  putStrLn ""

  -- ── ZDT1 (10 変数) ──
  putStrLn "[2] ZDT1 (10 変数, 2 目的)"
  let n = 10
      cfg2 = defaultNSGAConfig { nsgaPopSize = 100, nsgaGenerations = 200 }
  front2 <- nsga2 cfg2 (zdt1 n) (replicate n (0, 1)) gen
  let objs2 = map solObjectives front2
  printf "  最終 front サイズ: %d\n" (length front2)
  printf "  HV (ref [1.1, 1.1]) = %.4f (真値 ~ 0.66)\n"
         (hypervolume [1.1, 1.1] objs2)
  printf "  IGD (vs 真の front) = %.4f (小さいほど良い)\n"
         (igd (zdt1TrueFront 200) objs2)
  printf "  端点 (f1 最小): %s\n"
         (show (round2 (head (sortBy12 objs2))))
  printf "  端点 (f2 最小): %s\n"
         (show (round2 (last (sortBy12 objs2))))
  putStrLn ""

  -- ── 可視化 (130 規約: PlotData 経由) ──
  let cmpCfg t = (defaultConfig t)
                   { plotWidth = 600, plotHeight = 400 }
      -- estimated front + true front を 1 つの PlotData に束ね、"src" 列で分ける
      buildCompare estObjs trueFront =
        let f1s = [ head o | o <- estObjs ]
                ++ [ head p | p <- trueFront ]
            f2s = [ o !! 1  | o <- estObjs ]
                ++ [ p !! 1  | p <- trueFront ]
            srcs = replicate (length estObjs) (T.pack "estimated")
                ++ replicate (length trueFront) (T.pack "true")
        in fromMixedColumns
             [ (T.pack "f1", V.fromList f1s)
             , (T.pack "f2", V.fromList f2s)
             ]
             [ (T.pack "src", V.fromList srcs) ]

  paretoCompareFile HTML "nsga-schaffer.html"
    (cmpCfg "Schaffer — NSGA-II 推定 vs 真の Pareto front")
    ("f1", "f2") "src"
    (buildCompare objs1 (schafferTrueFront 100))

  paretoCompareFile HTML "nsga-zdt1.html"
    (cmpCfg "ZDT1 (10D) — NSGA-II 推定 vs 真の Pareto front")
    ("f1", "f2") "src"
    (buildCompare objs2 (zdt1TrueFront 200))
  putStrLn "  → nsga-schaffer.html / nsga-zdt1.html"

  parallelCoordinatesFile HTML "nsga-zdt1-parallel.html"
    ((defaultConfig "ZDT1 final population — parallel coordinates")
       { plotWidth = 700, plotHeight = 350 })
    ["f1", "f2"] (solutionsToPlotData ["f1", "f2"] front2)
  putStrLn "  → nsga-zdt1-parallel.html (並行座標)"
  putStrLn ""

  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  ✓ NSGA-II 本体が動作 (Schaffer, ZDT1 で Pareto front 再現)"
  putStrLn "═══════════════════════════════════════════════════════════════"
  where
    round2 :: [Double] -> [Double]
    round2 = map (\v -> fromIntegral (round (v * 10000) :: Int) / 10000)
    sortBy12 :: [[Double]] -> [[Double]]
    sortBy12 = qs
      where qs []     = []
            qs (p:xs) = qs [x | x <- xs, head x <= head p]
                       ++ [p]
                       ++ qs [x | x <- xs, head x > head p]
