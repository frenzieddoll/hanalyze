{-# LANGUAGE OverloadedStrings #-}
-- | Phase T3-T5: RRR / PLS / CCA のデモ。
module Main where

import qualified Numeric.LinearAlgebra as LA
import Text.Printf (printf)
import System.Random.MWC (createSystemRandom)
import qualified System.Random.MWC.Distributions as MWC

import Hanalyze.Model.Multivariate

main :: IO ()
main = do
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  Phase T3-T5: RRR / PLS / CCA"
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn ""

  -- データ: 真の B が rank 1 (= 1 つの latent factor で説明可能)
  -- u (p×1): "directions" of X が response に効く
  -- v (q×1): "loadings" on Y
  let n = 100 :: Int
      p = 5  :: Int
      q = 3  :: Int
  let uTrue = LA.asColumn (LA.fromList [1.0, -0.5, 0.3, 0.2, -0.1])  -- p × 1
      vTrue = LA.asColumn (LA.fromList [2.0, 1.0, -0.5])             -- q × 1
      bTrue = uTrue LA.<> LA.tr vTrue                                -- p × q (rank 1)

  printf "真の B (rank 1, %dx%d):\n" p q
  printM bTrue
  putStrLn ""

  gen <- createSystemRandom
  -- X ~ N(0, I)
  xRows <- mapM (const (mapM (const (MWC.standard gen)) [1 .. p])) [1 .. n]
  let xMat = LA.fromLists xRows
  -- Y = X B + noise
  noiseRows <- mapM (const (mapM (const (MWC.normal 0 0.3 gen)) [1 .. q])) [1 .. n]
  let nMat = LA.fromLists noiseRows
      yMat = (xMat LA.<> bTrue) + nMat

  -- ── RRR ──
  putStrLn "[1] Reduced Rank Regression (rank=1)"
  let rrr = reducedRankRegression 1 xMat yMat
  printf "  推定 B̂ (rank %d):\n" (rrrRank rrr)
  printM (rrrBeta rrr)
  let bDiff = rrrBeta rrr - bTrue
      maxErr = LA.maxElement (LA.cmap abs bDiff)
  printf "  B̂ - B 最大誤差: %.4f\n" maxErr
  putStrLn ""

  -- 比較: 通常 OLS (rank 制約なし)
  let bOLS = xMat LA.<\> yMat
  printf "  比較: OLS B̂ (rank %d):\n" (LA.rank bOLS)
  printM bOLS
  putStrLn ""

  -- ── PLS ──
  putStrLn "[2] PLS Regression (k=2 成分)"
  let plsFit = pls 2 xMat yMat
  printf "  推定 B̂ (PLS k=2):\n"
  printM (plsBeta plsFit)
  let bDiffPLS = plsBeta plsFit - bTrue
      maxErrPLS = LA.maxElement (LA.cmap abs bDiffPLS)
  printf "  B̂ - B 最大誤差 (PLS): %.4f\n" maxErrPLS
  putStrLn ""

  -- ── CCA ──
  putStrLn "[3] CCA"
  let ccaFit = cca xMat yMat
      corrs = LA.toList (ccaCorr ccaFit)
  printf "  Canonical correlations: %s\n"
         (show (map (\v -> fromIntegral (round (v * 1000) :: Int) / 1000 :: Double) corrs))
  printf "  最大相関 (= rank 1 構造を反映): %.4f\n" (head corrs)
  putStrLn ""

  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  ✓ RRR / PLS / CCA すべて動作"
  putStrLn "═══════════════════════════════════════════════════════════════"

  where
    printM :: LA.Matrix Double -> IO ()
    printM m = mapM_ (\row -> do
                        putStr "    "
                        mapM_ (printf "%+8.3f  ") (LA.toList row)
                        putStrLn "")
                     (LA.toRows m)
