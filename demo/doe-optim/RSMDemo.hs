{-# LANGUAGE OverloadedStrings #-}
-- | RSM デモ (Phase P1)。
--
-- - CCD/Box-Behnken の設計行列を表示
-- - 既知の二次関数 y = 5 - (x1-1)² - 2(x2+0.5)² + ε から fit
-- - 極値を解析的に求めて真値と比較
module Main where

import Text.Printf (printf)
import qualified Numeric.LinearAlgebra as LA
import System.Random.MWC (createSystemRandom)
import qualified System.Random.MWC.Distributions as MWC

import qualified Design.RSM as RSM
import qualified Design.Quality as DQ

main :: IO ()
main = do
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  Response Surface Methodology (Phase P1)"
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn ""

  -- ── 1. CCD (rotatable, k=2) ──
  putStrLn "[1] CCD rotatable, k=2, 中心点 nC=3"
  let ccd2 = RSM.centralCompositeRotatable 2 3
      alpha = sqrt (sqrt 4) :: Double  -- (2^2)^(1/4) = √2 ≈ 1.414
  printf "  α = (2²)^(1/4) = %.4f\n" alpha
  printf "  試行数: %d (factorial 4 + 軸 4 + 中心 3)\n" (length ccd2)
  mapM_ printRow ccd2
  putStrLn ""

  -- ── 2. CCD 種類比較 (k=3, nC=2) ──
  putStrLn "[2] CCD 種類比較 (k=3, nC=2)"
  let ccc = RSM.centralCompositeRotatable 3 2
      ccf = RSM.centralComposite 3 RSM.CCF 2
  printf "  Circumscribed (rotatable): %d 試行, D-eff = %.4f\n"
         (length ccc) (DQ.dEfficiency ccc)
  printf "  Face-centered:             %d 試行, D-eff = %.4f\n"
         (length ccf) (DQ.dEfficiency ccf)
  putStrLn ""

  -- ── 3. Box-Behnken k=3 ──
  putStrLn "[3] Box-Behnken k=3, nC=3 (= 12 + 3 = 15 試行)"
  let bb = RSM.boxBehnken 3 3
  printf "  試行数: %d, D-eff = %.4f\n" (length bb) (DQ.dEfficiency bb)
  putStrLn "  最初 6 行:"
  mapM_ printRow (take 6 bb)
  putStrLn ""

  -- ── 4. 二次回帰 fit ──
  -- 真の関数: y = 5 - (x1-1)² - 2(x2+0.5)² + ε
  -- 極大は (1, -0.5) で y=5
  putStrLn "[4] 二次回帰: y = 5 - (x1-1)² - 2(x2+0.5)² + N(0, 0.1)"
  putStrLn "    真の極大: x* = (1.0, -0.5), y* = 5.0"
  let trueF [x1, x2] = 5 - (x1 - 1)^(2::Int) - 2 * (x2 + 0.5)^(2::Int)
      trueF _ = 0
  gen <- createSystemRandom
  ys <- mapM (\row -> do
                 e <- MWC.normal 0 0.1 gen
                 return (trueF row + e))
             ccd2
  printf "    観測 n=%d (CCD k=2)\n" (length ys)

  let fit = RSM.fitQuadratic ccd2 ys
  let names = RSM.quadraticTermNames 2
      betas = LA.toList (RSM.qfBeta fit)
  putStrLn ""
  putStrLn "  Fit 結果:"
  printf "    R² = %.4f\n" (RSM.qfR2 fit)
  mapM_ (\(n, b) -> printf "    %-8s = %+8.4f\n" n b)
        (zip (map (\t -> read (show t) :: String) names) betas)
  putStrLn ""

  -- ── 5. 極値推定 ──
  let (xStar, yStar, eigs) = RSM.optimumPoint fit
  putStrLn "[5] 極値の解析解 (∂ŷ/∂x = 0 → x* = -½ B⁻¹ b)"
  printf "  x* = [%.4f, %.4f]   (真値 [1.0, -0.5])\n"
         (head xStar) (xStar !! 1)
  printf "  y* = %.4f             (真値 5.0)\n" yStar
  printf "  Hessian 固有値 = %s\n" (show (map (\e -> read (printf "%.4f" e :: String) :: Double) eigs))
  let allNeg = all (< 0) eigs
      allPos = all (> 0) eigs
      kind :: String
      kind = if allNeg then "極大 (concave)"
               else if allPos then "極小 (convex)"
                 else "鞍点 (saddle)"
  printf "  → %s\n" kind
  putStrLn ""

  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  ✓ CCD / Box-Behnken / 二次回帰 / 極値推定すべて動作"
  putStrLn "═══════════════════════════════════════════════════════════════"

  where
    printRow row =
      putStrLn ("    " ++ unwords (map (printf "%+6.3f") row))
