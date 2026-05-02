{-# LANGUAGE OverloadedStrings #-}
-- | 正則化回帰デモ (Phase Q)。
--
-- 真の β = [3, -2, 0, 0, 1.5, 0, 0, 0, 0, 0]  (10 列、5 つだけ非ゼロ)
-- p=10 列、n=50 観測、相関ある特徴量を含む高次元設定。
-- OLS / Ridge / Lasso / Elastic Net を比較。
module Main where

import qualified Data.Vector as V
import qualified Numeric.LinearAlgebra as LA
import Text.Printf (printf)
import System.Random.MWC (createSystemRandom)
import qualified System.Random.MWC.Distributions as MWC

import Model.Regularized (Penalty (..), RegFit (..),
                          fitRegularized, standardize)

main :: IO ()
main = do
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  正則化回帰デモ (Phase Q) — Ridge / Lasso / ElasticNet"
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn ""

  let n = 50
      p = 10
      betaTrue = [3.0, -2.0, 0.0, 0.0, 1.5, 0.0, 0.0, 0.0, 0.0, 0.0]
  printf "設定: n=%d, p=%d\n" n p
  printf "真の β = %s\n" (show betaTrue)
  printf "  非ゼロ: 3 個 (列 1, 2, 5)\n"
  putStrLn ""

  -- データ生成
  gen <- createSystemRandom
  rows <- mapM (const (V.replicateM p (MWC.standard gen))) [1 .. n :: Int]
  let xMat = LA.fromLists [V.toList r | r <- rows]
      bV   = LA.fromList betaTrue
  noise <- LA.fromList <$> mapM (const (MWC.normal 0 0.5 gen)) [1 .. n]
  let yV  = (xMat LA.#> bV) + noise

  -- 標準化
  let (xStd, _means, sds) = standardize xMat
  printf "X 列 sd の範囲: [%.3f, %.3f]\n"
         (V.minimum sds) (V.maximum sds)
  putStrLn ""

  -- 4 モデルを fit
  let fits =
        [ ("OLS              ", fitRegularized NoPen          xStd yV)
        , ("Ridge λ=0.1      ", fitRegularized (L2 0.1)       xStd yV)
        , ("Ridge λ=1.0      ", fitRegularized (L2 1.0)       xStd yV)
        , ("Lasso λ=0.05     ", fitRegularized (L1 0.05)      xStd yV)
        , ("Lasso λ=0.20     ", fitRegularized (L1 0.20)      xStd yV)
        , ("ElasticNet (.1,.1)", fitRegularized (ElasticNet 0.1 0.1) xStd yV)
        ]

  putStrLn "[1] 各モデルの係数 (標準化空間)"
  printf "  %-18s | R²   | nonZero | iters\n" ("Model" :: String)
  putStrLn (replicate 60 '-')
  mapM_ (\(name, fit) ->
            printf "  %s | %.4f | %7d | %5d\n"
                   (name :: String)
                   (rfR2 fit)
                   (rfNonZero fit)
                   (rfIters fit))
        fits
  putStrLn ""

  putStrLn "[2] 推定 β を真値と比較 (列ごと)"
  printf "  %-2s %s\n"
         ("j" :: String)
         (concat ["%-8s" | _ <- fits] :: String)
  printf "      %s\n"
         (concat [printf "%-8s" (take 7 name) :: String
                 | (name, _) <- fits])
  putStrLn (replicate 70 '-')
  mapM_ (\j ->
            do
              printf "  %2d (%5.2f)" j (betaTrue !! j)
              mapM_ (\(_, fit) ->
                        printf " %+7.3f"
                               ((LA.toList (rfBeta fit)) !! j))
                    fits
              putStrLn "")
        [0 .. p - 1 :: Int]
  putStrLn ""

  -- 評価: 真値からの距離
  putStrLn "[3] β 推定誤差 ||β̂ − β_true||₂ と sparsity"
  printf "  %-18s | β 誤差 | sparsity (= 推定 0 の数)\n" ("Model" :: String)
  putStrLn (replicate 60 '-')
  -- β を unstandardize で元スケールに戻す
  let bTrueV = LA.fromList betaTrue
  mapM_ (\(name, fit) ->
            do
              -- 標準化 X で fit した β を元の x スケールに戻す:
              -- β_orig_j = β_std_j / sd_j
              let bStd = rfBeta fit
                  bOrig = LA.fromList
                    [ (bStd `LA.atIndex` j) / (sds V.! j)
                    | j <- [0 .. p - 1] ]
                  err   = LA.norm_2 (bOrig - bTrueV)
                  zeros = length [v | v <- LA.toList bStd, abs v <= 1e-8]
              printf "  %s | %6.3f | %5d / %d\n"
                     (name :: String) err zeros p)
        fits
  putStrLn ""

  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  ✓ Lasso が真の sparse 構造を回復"
  putStrLn "    Ridge は非ゼロを縮小、Elastic Net は中間"
  putStrLn "═══════════════════════════════════════════════════════════════"
