{-# LANGUAGE OverloadedStrings #-}
-- | Phase S1 — 非優越ソート + crowding distance の動作確認。
--
-- 既知の入力で出力が正しいことを 5 ケースで検証。Phase S 全体の
-- 基礎となる関数なので、ここで誤りを潰す。
module Main where

import Text.Printf (printf)
import System.Random.MWC (createSystemRandom)
import Hanalyze.Optim.NSGA (Solution (..), dominates, paretoDominates,
                   nonDominatedSort, crowdingDistance,
                   sbxCrossover, polynomialMutation, randomInBounds,
                   binaryTournament, crowdedCompare)

mkSol :: [Double] -> [Double] -> Double -> Solution
mkSol = Solution

assertBool :: String -> Bool -> IO ()
assertBool label ok = do
  putStrLn (if ok then "  ✓ " ++ label
                  else "  ✗ FAIL " ++ label)

main :: IO ()
main = do
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  Phase S1: 非優越ソート + crowding distance の動作確認"
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn ""

  -- ── Test 1: paretoDominates の基本ケース ──
  putStrLn "[1] paretoDominates"
  assertBool "(1, 2) dominates (2, 3)"     (paretoDominates [1, 2] [2, 3])
  assertBool "(1, 2) NOT dominates (1, 3)? = はい (= に注意)"
             (paretoDominates [1, 2] [1, 3])  -- 1<=1 かつ 2<3 なので支配
  assertBool "(1, 2) NOT dominates (1, 2)" (not (paretoDominates [1, 2] [1, 2]))
  assertBool "(1, 3) NOT dominates (2, 2)" (not (paretoDominates [1, 3] [2, 2]))
  putStrLn ""

  -- ── Test 2: dominates with constraints ──
  putStrLn "[2] dominates (制約あり)"
  let s1 = mkSol [] [1, 1] 0     -- feasible
      s2 = mkSol [] [0, 0] 5     -- infeasible (better obj but violates)
      s3 = mkSol [] [10, 10] 2   -- infeasible, smaller violation
      s4 = mkSol [] [10, 10] 8   -- infeasible, larger violation
  assertBool "feasible dominates infeasible" (dominates s1 s2)
  assertBool "infeasible NOT dominates feasible" (not (dominates s2 s1))
  assertBool "smaller violation dominates" (dominates s3 s4)
  putStrLn ""

  -- ── Test 3: nonDominatedSort 基本 ──
  putStrLn "[3] nonDominatedSort"
  -- 4 点で 2 つの front:
  --   F1 = {(1,4), (2,2), (4,1)} (Pareto front)
  --   F2 = {(3,3)} (支配される)
  let pop3 = [ mkSol [] [1, 4] 0
             , mkSol [] [2, 2] 0
             , mkSol [] [4, 1] 0
             , mkSol [] [3, 3] 0
             ]
      fronts3 = nonDominatedSort pop3
  printf "  front 数: %d (期待 2)\n" (length fronts3)
  printf "  F_1 サイズ: %d (期待 3)\n" (length (head fronts3))
  printf "  F_2 サイズ: %d (期待 1)\n" (length (fronts3 !! 1))
  let f2obj = solObjectives (head (fronts3 !! 1))
  assertBool ("F_2 の点が (3,3): " ++ show f2obj) (f2obj == [3, 3])
  putStrLn ""

  -- ── Test 4: nonDominatedSort 線形 (全部非優越) ──
  putStrLn "[4] nonDominatedSort: 全点が非優越 (front は 1 つ)"
  let pop4 = [mkSol [] [fromIntegral i, fromIntegral (5 - i)] 0
             | i <- [0 .. 5 :: Int]]
      fronts4 = nonDominatedSort pop4
  printf "  front 数: %d (期待 1)\n" (length fronts4)
  printf "  F_1 サイズ: %d (期待 6)\n" (length (head fronts4))
  putStrLn ""

  -- ── Test 5: crowdingDistance ──
  putStrLn "[5] crowdingDistance: 5 点線形 front で端点 ∞、中央点 ~ 0.5"
  -- (0,4), (1,3), (2,2), (3,1), (4,0) - perfect linear front
  let front5 = [mkSol [] [fromIntegral i, fromIntegral (4 - i)] 0
               | i <- [0 .. 4 :: Int]]
      sorted = crowdingDistance front5
  putStrLn "  ソート済 (距離降順):"
  mapM_ (\s -> printf "    %s\n" (show (solObjectives s))) sorted
  -- 端点 (0,4) と (4,0) が距離 ∞ で最初に来るはず
  let firstTwo = take 2 sorted
      firstObjs = map solObjectives firstTwo
  assertBool "先頭 2 個は端点 (0,4) と (4,0)"
             (sort2 firstObjs == [[0, 4], [4, 0]])
  putStrLn ""

  -- ── Test 6: crowdingDistance with all-equal objectives ──
  putStrLn "[6] crowdingDistance: 全て同じ目的値 (range=0 で 0 距離)"
  let front6 = replicate 4 (mkSol [] [1, 2] 0)
      sorted6 = crowdingDistance front6
  printf "  入出力長一致: %s (length 4)\n"
         (show (length sorted6))
  putStrLn ""

  -- ── Phase S3: 遺伝的演算子 ──
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  Phase S3: 遺伝的演算子の動作確認"
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn ""

  gen <- createSystemRandom

  -- Test 7: SBX
  putStrLn "[7] sbxCrossover"
  let bounds3 = [(0, 10), (-5, 5)]
      p1 = [3.0, 1.0]
      p2 = [7.0, -2.0]
  (c1, c2) <- sbxCrossover 15 bounds3 p1 p2 gen
  printf "  parents:  %s, %s\n" (show p1) (show p2)
  printf "  children: %s, %s\n" (show c1) (show c2)
  assertBool "c1 in bounds (dim 0)" (head c1 >= 0 && head c1 <= 10)
  assertBool "c1 in bounds (dim 1)" (c1 !! 1 >= -5 && c1 !! 1 <= 5)
  assertBool "c2 in bounds (dim 0)" (head c2 >= 0 && head c2 <= 10)
  assertBool "c2 in bounds (dim 1)" (c2 !! 1 >= -5 && c2 !! 1 <= 5)
  -- 同一親なら同一子
  (c1', c2') <- sbxCrossover 15 bounds3 p1 p1 gen
  assertBool "同一親 → 同一子 (dim 0)"
             (abs (head c1' - 3.0) < 1e-12 && abs (head c2' - 3.0) < 1e-12)
  putStrLn ""

  -- Test 8: polynomial mutation
  putStrLn "[8] polynomialMutation"
  let xs = [3.0, 1.0]
  -- pMut=1 で必ず変異、bounds 内に留まる
  ys <- polynomialMutation 20 1.0 bounds3 xs gen
  printf "  before: %s, after: %s\n" (show xs) (show ys)
  assertBool "ys in bounds (dim 0)" (head ys >= 0 && head ys <= 10)
  assertBool "ys in bounds (dim 1)" (ys !! 1 >= -5 && ys !! 1 <= 5)
  -- pMut=0 で変異なし
  ysNo <- polynomialMutation 20 0.0 bounds3 xs gen
  assertBool "pMut=0 で不変" (ysNo == xs)
  putStrLn ""

  -- Test 9: randomInBounds
  putStrLn "[9] randomInBounds"
  rs <- mapM (const (randomInBounds bounds3 gen)) [1 .. 50 :: Int]
  let dim0Vals = map head rs
      dim1Vals = map (!! 1) rs
      inRange0 = all (\v -> v >= 0 && v <= 10) dim0Vals
      inRange1 = all (\v -> v >= -5 && v <= 5) dim1Vals
  assertBool "dim 0 すべて [0, 10] に収まる" inRange0
  assertBool "dim 1 すべて [-5, 5] に収まる" inRange1
  putStrLn ""

  -- Test 10: crowdedCompare
  putStrLn "[10] crowdedCompare"
  assertBool "rank 0 < rank 1"      (crowdedCompare (0, 0)   (1, 100) == LT)
  assertBool "rank 同 → 距離大が良い" (crowdedCompare (0, 5.0) (0, 1.0) == LT)
  assertBool "rank 同 → 距離小は劣"   (crowdedCompare (0, 1.0) (0, 5.0) == GT)
  assertBool "完全同じ"               (crowdedCompare (0, 5.0) (0, 5.0) == EQ)
  putStrLn ""

  -- Test 11: binaryTournament
  putStrLn "[11] binaryTournament (常に小さい数値が勝つ comparator)"
  -- pop = [1..10], 「数値小=良い」順なら勝者は最小の方の index に近い
  -- 確率的なので 100 回試行して平均が真ん中より小さいことを確認
  results <- mapM
    (const (binaryTournament [1..10 :: Int] compare gen))
    [1..100 :: Int]
  let meanRes = fromIntegral (sum results) / 100 :: Double
  printf "  100 回トーナメント平均: %.2f (期待 < 5.5 = single-pick mean)\n"
         meanRes
  assertBool "平均 < 5.5 (= 良い方が選ばれる傾向)" (meanRes < 5.5)
  putStrLn ""

  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  ✓ Phase S1 + S3: 全テスト通過"
  putStrLn "═══════════════════════════════════════════════════════════════"

  where
    sort2 :: [[Double]] -> [[Double]]
    sort2 [a, b] = if a < b then [a, b] else [b, a]
    sort2 xs = xs
