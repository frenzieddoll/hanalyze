{-# LANGUAGE OverloadedStrings #-}
-- | Viz.Bar と PNG/SVG 出力のデモ
module Main where

import Viz.Core (defaultConfig, OutputFormat (..), writeSpec)
import Viz.Bar

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  let cfg = defaultConfig "Bar Demo"

  -- ── 1. 縦棒グラフ → HTML ────────────────────────────────────────────
  let spec1 = barChart cfg "Month" "Sales"
                ["Jan","Feb","Mar","Apr","May","Jun"]
                [120, 95, 140, 108, 155, 130]
  writeSpec HTML "bar_vertical.html" spec1
  putStrLn "bar_vertical.html を生成"

  -- ── 2. 水平棒グラフ → HTML ──────────────────────────────────────────
  let spec2 = barChartH cfg "Country" "GDP (trillion USD)"
                ["Japan","Germany","USA","France","Canada"]
                [4.2, 4.1, 25.5, 2.8, 2.1]
  writeSpec HTML "bar_horizontal.html" spec2
  putStrLn "bar_horizontal.html を生成"

  -- ── 3. 積み上げ棒グラフ → HTML ──────────────────────────────────────
  let quarters = concatMap (replicate 3) ["Q1","Q2","Q3","Q4"]
      revenue  = [100,80,60, 120,90,70, 115,85,65, 130,100,80]
      products = concat (replicate 4 ["Product A","Product B","Product C"])
      spec3    = stackedBar cfg "Quarter" "Revenue" "Product"
                   quarters revenue products
  writeSpec HTML "bar_stacked.html" spec3
  putStrLn "bar_stacked.html を生成"

  -- ── 4. グループ別棒グラフ → HTML ────────────────────────────────────
  let spec4 = groupedBar cfg "Method" "ESS" "Case"
                ["MH","HMC","NUTS","MH","HMC","NUTS"]
                [120, 900, 1800, 80, 1200, 1900]
                ["Easy","Easy","Easy","Hard","Hard","Hard"]
  writeSpec HTML "bar_grouped.html" spec4
  putStrLn "bar_grouped.html を生成"

  -- ── 5. PNG 出力テスト ────────────────────────────────────────────────
  writeSpec PNG "bar_vertical.png" spec1
  putStrLn "bar_vertical.png を生成 (vl-convert)"

  -- ── 6. SVG 出力テスト ────────────────────────────────────────────────
  writeSpec SVG "bar_vertical.svg" spec1
  putStrLn "bar_vertical.svg を生成 (vl-convert)"

  putStrLn "\n完了"
