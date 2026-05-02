{-# LANGUAGE OverloadedStrings #-}
-- | 最適計画 (D-optimal / A-optimal) のデモ (Phase P2)。
--
-- 候補集合 (3 水準グリッド) から指定試行数の部分集合を Fedorov 交換で
-- 最適化する。線形 vs 二次モデルの両方で確認。
module Main where

import Text.Printf (printf)

import qualified Design.Optimal as DO
import qualified Design.Quality as DQ

main :: IO ()
main = do
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  最適計画 (D-optimal / A-optimal) — Phase P2"
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn ""

  -- ── 1. 線形モデル: k=3 因子、3 水準グリッド (27 候補) から 8 行選ぶ ──
  putStrLn "[1] 線形モデル (k=3 因子)"
  putStrLn "    候補: 3 水準グリッド = 27 候補。8 試行を選ぶ。"
  putStrLn ""
  let cands1 = DO.candidateGrid 3 3
      n1     = 8
  printf "  候補集合サイズ: %d\n" (length cands1)
  let (idxD, designD) = DO.dOptimal cands1 n1 42
      (idxA, designA) = DO.aOptimal cands1 n1 42
  printf "  D-optimal 選定: %s\n" (show idxD)
  printf "    D-eff       = %.4f\n" (DQ.dEfficiency designD)
  printf "    A-eff       = %.4f\n" (DQ.aEfficiency designD)
  printf "    条件数      = %.4f\n" (DQ.conditionNumber designD)
  putStrLn ""
  printf "  A-optimal 選定: %s\n" (show idxA)
  printf "    D-eff       = %.4f\n" (DQ.dEfficiency designA)
  printf "    A-eff       = %.4f\n" (DQ.aEfficiency designA)
  printf "    条件数      = %.4f\n" (DQ.conditionNumber designA)
  putStrLn ""
  putStrLn "  選ばれた D-optimal 設計:"
  mapM_ printRow designD
  putStrLn ""

  -- ── 2. 二次モデル: 候補は [1, x_i, x_i², x_i x_j] 拡張済 ──
  putStrLn "[2] 二次モデル (k=2 因子, 1 + 2 + 2 + 1 = 6 列)"
  putStrLn "    候補: 5 水準グリッド = 25 候補。10 試行を選ぶ。"
  putStrLn ""
  let cands2 = DO.quadraticCandidates 2 5
      n2     = 10
  printf "  候補集合サイズ: %d, 列数 (二次拡張後): %d\n"
         (length cands2) (length (head cands2))
  let (_, qDesign) = DO.dOptimal cands2 n2 7
  printf "  D-eff       = %.4f\n" (DQ.dEfficiency qDesign)
  printf "  条件数      = %.4f\n" (DQ.conditionNumber qDesign)
  putStrLn "  最初 5 行 (拡張済):"
  mapM_ printRow (take 5 qDesign)
  putStrLn ""

  -- ── 3. ランダム選択との比較 ──
  putStrLn "[3] 改善度比較 (D-optimal vs ランダム選択, k=3 線形, n=8)"
  let randDesigns = [ map (cands1 !!) (take n1 (DO.pseudoShuffle seed [0 .. length cands1 - 1]))
                    | seed <- [1..5] ]
      randDEffs = map DQ.dEfficiency randDesigns
      avgRand   = sum randDEffs / fromIntegral (length randDEffs)
  printf "  ランダム選択 5 種の平均 D-eff: %.4f\n" avgRand
  printf "  D-optimal:                     %.4f\n" (DQ.dEfficiency designD)
  printf "  改善率: %.1fx\n" (DQ.dEfficiency designD / avgRand)
  putStrLn ""

  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  ✓ Fedorov 交換で D-/A-optimal 設計を構築"
  putStrLn "═══════════════════════════════════════════════════════════════"

  where
    printRow row = putStrLn ("    " ++ unwords (map (printf "%+6.3f") row))

-- (Optimal モジュールに pseudoShuffleI を export してないので、
--  ここでは pseudoShuffle を直接使う代わりに)
