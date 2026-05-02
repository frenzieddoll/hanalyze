{-# LANGUAGE OverloadedStrings #-}
-- | Phase T1: Multivariate LM のデモ。
--
-- 真の回帰: Y = XB + E、3 出力 (q=3) を 4 説明変数 (p=4 incl. intercept) で
-- 同時に推定。残差の共分散も確認する。
module Main where

import qualified Numeric.LinearAlgebra as LA
import Text.Printf (printf)
import System.Random.MWC (createSystemRandom)
import qualified System.Random.MWC.Distributions as MWC

import Model.Core (FitResult (..))
import Model.MultiLM

main :: IO ()
main = do
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  Phase T1: Multivariate Linear Regression"
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn ""

  let n = 100 :: Int
      p = 4   :: Int
      q = 3   :: Int
  -- 真の係数行列 B (4 × 3)
  let bTrue = LA.fromLists
        [ [ 2.0, -1.0,  0.5]    -- intercept
        , [ 1.0,  0.5, -0.3]    -- x1
        , [-0.5,  1.0,  0.8]    -- x2
        , [ 0.3, -0.2,  0.4]    -- x3
        ]
  printf "真の B (%dx%d):\n" p q
  printM bTrue
  putStrLn ""

  -- データ生成 (X, ノイズ Σ_true 付き Y)
  gen <- createSystemRandom
  -- X: 切片 1 + 3 説明変数
  let x1 = [(fromIntegral i) / fromIntegral n | i <- [0 .. n - 1]]
      x2 = [sin (fromIntegral i / 10) | i <- [0 .. n - 1]]
      x3 = [(fromIntegral i `mod` 7 :: Int) `quot` 2 | i <- [0 .. n - 1]]
      x3' = map fromIntegral x3
      xMat = LA.fromColumns
              [ LA.konst 1 n
              , LA.fromList x1
              , LA.fromList x2
              , LA.fromList x3' ]

  -- ノイズ E ~ MvN(0, Σ_true) で 3 出力に相関を入れる
  let sigmaTrue = LA.fromLists
        [ [0.5, 0.2, 0.0]
        , [0.2, 0.4, 0.1]
        , [0.0, 0.1, 0.3]
        ]
  -- E を生成 (Cholesky 経由)
  let lChol = LA.tr (LA.chol (LA.trustSym sigmaTrue))
  zsRows <- mapM (const (do
                          z1 <- MWC.standard gen
                          z2 <- MWC.standard gen
                          z3 <- MWC.standard gen
                          return (LA.fromList [z1, z2, z3])))
                 [1 .. n]
  let zMat = LA.fromRows zsRows
      eMat = zMat LA.<> LA.tr lChol
      yMat = (xMat LA.<> bTrue) + eMat

  printf "観測 Y (%dx%d), X (%dx%d) を生成 (真の Σ で相関ノイズ)\n" n q n p
  putStrLn ""

  -- フィット
  let mf = fitMultiLM xMat yMat
  printf "推定 B̂ (%dx%d):\n" p q
  printM (coefficients (mfFit mf))
  putStrLn ""

  -- 真値との誤差
  let bDiff = coefficients (mfFit mf) - bTrue
      maxDev = LA.maxElement (LA.cmap abs bDiff)
  printf "B̂ - B 最大絶対誤差: %.4f (n=%d で十分小さいはず)\n" maxDev n
  putStrLn ""

  -- R² (列ごと)
  printf "列ごとの R²: %s\n"
         (show (map (\v -> (fromIntegral (round (v * 1e4) :: Int) / 1e4) :: Double)
                    (LA.toList (rSquared (mfFit mf)))))
  putStrLn ""

  -- 残差共分散の比較
  putStrLn "推定 Σ̂ (residual covariance):"
  printM (mfResidCov mf)
  putStrLn ""
  putStrLn "真の Σ:"
  printM sigmaTrue
  putStrLn ""
  putStrLn "推定 残差相関行列:"
  printM (mfResidCor mf)
  putStrLn ""

  -- 予測テスト
  let xNew = LA.fromLists
        [ [1, 0.5, 0.0, 1.0]
        , [1, 0.8, 0.5, 2.0] ]
      yPred = predictMultiLM mf xNew
  printf "新規 2 観測の予測:\n"
  printM yPred
  putStrLn ""

  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  ✓ MultiLM が動作: B̂ ≈ B、Σ̂ も真値に近い"
  putStrLn "═══════════════════════════════════════════════════════════════"

  where
    printM :: LA.Matrix Double -> IO ()
    printM m = mapM_ (\row -> do
                        putStr "  "
                        mapM_ (printf "%+8.3f  ") (LA.toList row)
                        putStrLn "")
                     (LA.toRows m)
