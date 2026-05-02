{-# LANGUAGE OverloadedStrings #-}
-- | Design of Experiments デモ (Phase O)。
--
-- 全要因/部分要因/ラテン方格/乱塊法/ANOVA/Power/質指標を一括検証。
module Main where

import Text.Printf (printf)

import qualified Design.Factorial as DF
import qualified Design.Block     as DB
import qualified Design.Mixed     as DM
import qualified Design.Anova     as DA
import qualified Design.Power     as DP
import qualified Design.Quality   as DQ

main :: IO ()
main = do
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  Design of Experiments デモ (Phase O)"
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn ""

  -- ── 1. 完全要因 2³ ──
  putStrLn "[1] 完全要因 2³ (3 因子各 2 水準 = 8 試行)"
  let d23 = DF.twoLevelFactorial 3
  printRows d23
  printf "  直交度スコア = %.4f (1 で完全直交)\n" (DQ.orthogonalityScore d23)
  printf "  D-efficiency = %.4f\n" (DQ.dEfficiency d23)
  printf "  条件数        = %.4f\n" (DQ.conditionNumber d23)
  putStrLn ""

  -- ── 2. 部分要因 2^(4-1): D=ABC ──
  putStrLn "[2] 部分要因 2^(4-1) (D = ABC)"
  let d4m1 = DF.fractionalFactorial 4 [[1, 2, 3]]
  printRows d4m1
  printf "  試行数: %d (= 完全 16 の半分)\n" (length d4m1)
  printf "  直交度スコア = %.4f\n" (DQ.orthogonalityScore d4m1)
  putStrLn ""

  -- ── 3. ラテン方格 4×4 ──
  putStrLn "[3] ラテン方格 4×4"
  let ls = DB.latinSquare 4
  mapM_ print ls
  putStrLn ""

  -- ── 4. 混合水準 2² × 3 ──
  putStrLn "[4] 混合水準 2² × 3 (= 12 試行)"
  let dMix = DF.mixedFactorial [2, 2, 3]
  printRows (take 4 dMix)
  putStrLn (printf "  ... (合計 %d 試行)" (length dMix) :: String)
  putStrLn ""

  -- ── 5. 乱塊法 (4 ブロック × 5 処理) ──
  putStrLn "[5] 乱塊法 4 ブロック × 5 処理 (各ブロック内ランダム順)"
  let rb = DB.randomizedBlock 4 5 42
  mapM_ (\(i, blk) -> printf "  Block %d: %s\n" (i :: Int) (show blk))
        (zip [1..] rb)
  putStrLn ""

  -- ── 6. ANOVA (一元配置) ──
  putStrLn "[6] 一元配置 ANOVA (3 群、各 5 観測)"
  let labels = concat [replicate 5 g | g <- ["A", "B", "C"]]
      vals   = [4.1, 4.5, 4.0, 4.3, 4.4   -- group A: mean 4.26
              , 5.0, 5.3, 5.2, 5.4, 4.9   -- group B: mean 5.16
              , 5.5, 5.8, 5.6, 5.9, 5.7] -- group C: mean 5.70
  DA.printAnovaTable (DA.oneWayAnova labels vals)
  putStrLn ""

  -- ── 7. 検出力解析 ──
  putStrLn "[7] 検出力解析"
  let d   = DP.cohensD 0 0.5 1.0       -- d = 0.5 (medium)
      pwr = DP.powerTTest d 30 30 0.05
  printf "  t 検定 (n=30 each, d=0.5, α=0.05): power = %.3f\n" pwr
  let n   = DP.sampleSizeTTest 0.5 0.8 0.05
  printf "  d=0.5, target power=0.8 → n = %d (each group)\n" n

  let f      = DP.cohensF [4.26, 5.16, 5.70] 0.30
      anovaP = DP.powerOneWayAnova f 3 5 0.05
  printf "  ANOVA (k=3, n=5/group, f=%.3f): power = %.3f\n" f anovaP
  putStrLn ""

  -- ── 8. 設計の質 ──
  putStrLn "[8] 設計の質 (2³ 完全要因に対する各指標)"
  printf "  直交?           %s\n" (show (DQ.isOrthogonal 1e-9 d23))
  printf "  直交度スコア    = %.4f\n" (DQ.orthogonalityScore d23)
  printf "  条件数          = %.4f\n" (DQ.conditionNumber d23)
  printf "  D-efficiency    = %.4f\n" (DQ.dEfficiency d23)
  printf "  A-efficiency    = %.4f\n" (DQ.aEfficiency d23)
  printf "  VIF (各列)      = %s\n"
         (show (map (\v -> read (printf "%.2f" v :: String) :: Double)
                    (DQ.vifList d23)))
  putStrLn ""

  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  ✓ 完全要因/部分要因/ラテン方格/乱塊法/ANOVA/Power/品質"
  putStrLn "    全て動作"
  putStrLn "═══════════════════════════════════════════════════════════════"

  where
    printRows :: [[Double]] -> IO ()
    printRows rs = do
      mapM_ (\r -> putStrLn ("  " ++ showRow r)) rs
    showRow = unwords . map (printf "%+5.1f")

    -- DM.crossDesign suppress unused warning
    _ = DM.crossDesign [[1]] [[2]]
