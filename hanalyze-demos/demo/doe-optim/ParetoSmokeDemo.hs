{-# LANGUAGE OverloadedStrings #-}
-- | Phase S2 — Pareto utilities (HV, IGD, GD) の動作確認。
module Main where

import Text.Printf (printf)
import Hanalyze.Optim.Pareto (isNonDominated, paretoFront, hypervolume, igd, gd)

approxEq :: Double -> Double -> Bool
approxEq a b = abs (a - b) < 1e-6

assertBool :: String -> Bool -> IO ()
assertBool label ok = putStrLn (if ok then "  ✓ " ++ label else "  ✗ FAIL " ++ label)

main :: IO ()
main = do
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  Phase S2: Pareto utilities の動作確認"
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn ""

  -- Test 1: paretoFront
  putStrLn "[1] paretoFront"
  let pop = [[1, 4], [2, 2], [4, 1], [3, 3]]   -- (3,3) is dominated by (2,2)
      pf  = paretoFront pop
  printf "  入力 %s → front %s\n" (show pop) (show pf)
  assertBool "front 長 3" (length pf == 3)
  assertBool "front に (3,3) 含まない" ([3, 3] `notElem` pf)
  putStrLn ""

  -- Test 2: isNonDominated
  putStrLn "[2] isNonDominated"
  let ps = [[1, 4], [2, 2], [4, 1]]
  assertBool "(0, 0) は非優越 (誰も支配していない)"
             (isNonDominated [0, 0] ps)
  assertBool "(3, 3) は被支配"
             (not (isNonDominated [3, 3] ps))
  putStrLn ""

  -- Test 3: hypervolume 2D 既知ケース
  putStrLn "[3] hypervolume (2D)"
  -- 単一点 (1, 2), ref (3, 4): HV = (3-1) × (4-2) = 4
  let hv1 = hypervolume [3, 4] [[1, 2]]
  printf "  単一点 (1,2), ref (3,4): HV = %.3f (期待 4.0)\n" hv1
  assertBool "hv1 = 4.0" (approxEq hv1 4.0)

  -- 2 点 (1, 2), (2, 1), ref (3, 3):
  -- (1, 2): 寄与 (3-1)(3-2) = 2  だが (2, 1) も計算に入る
  -- 階段状で計算: x 昇順 (1,2), (2,1)
  --   (1, 2): 幅 (3-1) = 2、高さ (3-2) = 1 → 2
  --   (2, 1): 幅 (3-2) = 1、高さ (2-1) = 1 → 1   (前の y=2 から下がった分)
  -- 合計 3
  let hv2 = hypervolume [3, 3] [[1, 2], [2, 1]]
  printf "  2 点 (1,2),(2,1), ref (3,3): HV = %.3f (期待 3.0)\n" hv2
  assertBool "hv2 = 3.0" (approxEq hv2 3.0)

  -- 全点が ref より悪い → HV = 0
  let hv3 = hypervolume [1, 1] [[2, 2]]
  printf "  ref より悪い点: HV = %.3f (期待 0)\n" hv3
  assertBool "hv3 = 0" (approxEq hv3 0.0)
  putStrLn ""

  -- Test 4: hypervolume 3D
  putStrLn "[4] hypervolume (3D)"
  -- 単一点 (1, 1, 1), ref (2, 2, 2): HV = 1×1×1 = 1
  let hv3D = hypervolume [2, 2, 2] [[1, 1, 1]]
  printf "  単一点 (1,1,1), ref (2,2,2): HV = %.3f (期待 1.0)\n" hv3D
  assertBool "hv3D = 1.0" (approxEq hv3D 1.0)
  putStrLn ""

  -- Test 5: IGD
  putStrLn "[5] igd / gd"
  let trueF = [[0, 4], [1, 3], [2, 2], [3, 1], [4, 0]]
      estF  = [[0.1, 4.1], [2.1, 2.1], [4.1, 0.1]]
      igdV  = igd trueF estF
      gdV   = gd  trueF estF
  printf "  IGD = %.4f, GD = %.4f\n" igdV gdV
  assertBool "IGD > 0" (igdV > 0)
  assertBool "GD > 0" (gdV > 0)
  -- 完全一致なら IGD = GD = 0
  let igdSame = igd trueF trueF
  printf "  IGD(自分自身) = %.6f (期待 0)\n" igdSame
  assertBool "IGD(self) = 0" (approxEq igdSame 0.0)
  putStrLn ""

  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  ✓ Phase S2: Pareto utilities 動作確認"
  putStrLn "═══════════════════════════════════════════════════════════════"
