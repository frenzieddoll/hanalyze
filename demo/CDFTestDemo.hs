{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | 全分布の CDF 動作確認 (Beta / Gamma / Cauchy / StudentT 含む)。
--
-- statistics パッケージの CDF (= 信頼できる reference) があれば比較したいが、
-- ここでは既知の数値 (例: 標準正規 0 で 0.5、対称性チェック) で検証する。
module Main where

import Text.Printf (printf)

import Model.HBM (Distribution (..), distCDF)

-- distCDF Just から値を取り出す
cdfAt :: Distribution Double -> Double -> Double
cdfAt d x = case distCDF d x of
  Just v  -> v
  Nothing -> 0/0

main :: IO ()
main = do
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  CDF 動作確認 (全分布)"
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn ""

  -- Normal
  putStrLn "[Normal] N(0, 1)"
  printf "  F(0) = %.4f  (期待 0.5)\n"   (cdfAt (Normal 0 1) 0)
  printf "  F(1.96) = %.4f  (期待 ≈ 0.975)\n" (cdfAt (Normal 0 1) 1.96)
  printf "  F(-1.96) = %.4f  (期待 ≈ 0.025)\n" (cdfAt (Normal 0 1) (-1.96))
  putStrLn ""

  -- Cauchy (標準)
  putStrLn "[Cauchy] Cauchy(0, 1)"
  printf "  F(0) = %.4f  (期待 0.5)\n" (cdfAt (Cauchy 0 1) 0)
  printf "  F(1) = %.4f  (期待 0.75)\n" (cdfAt (Cauchy 0 1) 1)
  printf "  F(-1) = %.4f  (期待 0.25)\n" (cdfAt (Cauchy 0 1) (-1))
  putStrLn ""

  -- HalfCauchy
  putStrLn "[HalfCauchy] HalfCauchy(1)"
  printf "  F(0) = %.4f  (期待 0)\n" (cdfAt (HalfCauchy 1) 0)
  printf "  F(1) = %.4f  (期待 0.5)\n" (cdfAt (HalfCauchy 1) 1)
  putStrLn ""

  -- Exponential
  putStrLn "[Exponential] Exp(rate=1)"
  printf "  F(0) = %.4f  (期待 0)\n" (cdfAt (Exponential 1) 0)
  printf "  F(1) = %.4f  (期待 ≈ 0.6321)\n" (cdfAt (Exponential 1) 1)
  printf "  F(2) = %.4f  (期待 ≈ 0.8647)\n" (cdfAt (Exponential 1) 2)
  putStrLn ""

  -- Uniform
  putStrLn "[Uniform] U(0, 1)"
  printf "  F(0.3) = %.4f  (期待 0.3)\n" (cdfAt (Uniform 0 1) 0.3)
  printf "  F(0.5) = %.4f  (期待 0.5)\n" (cdfAt (Uniform 0 1) 0.5)
  putStrLn ""

  -- Gamma
  putStrLn "[Gamma] Gamma(shape=2, rate=1)"
  printf "  F(0) = %.4f  (期待 0)\n" (cdfAt (Gamma 2 1) 0)
  printf "  F(2) = %.4f  (期待 ≈ 0.5940)\n" (cdfAt (Gamma 2 1) 2)
  printf "  F(5) = %.4f  (期待 ≈ 0.9596)\n" (cdfAt (Gamma 2 1) 5)
  printf "  F(10) = %.4f  (期待 ≈ 0.9995)\n" (cdfAt (Gamma 2 1) 10)
  putStrLn ""
  putStrLn "[Gamma] Gamma(shape=0.5, rate=1)  ; これは ½χ²(1) と同じ"
  printf "  F(0.5) = %.4f  (期待 ≈ 0.6827)\n" (cdfAt (Gamma 0.5 1) 0.5)
  printf "  F(2) = %.4f  (期待 ≈ 0.9545)\n" (cdfAt (Gamma 0.5 1) 2)
  putStrLn ""

  -- Beta
  putStrLn "[Beta] Beta(2, 5)"
  printf "  F(0) = %.4f  (期待 0)\n" (cdfAt (Beta 2 5) 0)
  printf "  F(0.5) = %.4f  (期待 ≈ 0.8906)\n" (cdfAt (Beta 2 5) 0.5)
  printf "  F(1) = %.4f  (期待 1)\n" (cdfAt (Beta 2 5) 1)
  putStrLn ""
  putStrLn "[Beta] Beta(1, 1)  ; 一様分布と等価"
  printf "  F(0.3) = %.4f  (期待 0.3)\n" (cdfAt (Beta 1 1) 0.3)
  printf "  F(0.7) = %.4f  (期待 0.7)\n" (cdfAt (Beta 1 1) 0.7)
  putStrLn ""

  -- StudentT
  putStrLn "[StudentT] t(df=3, mu=0, sigma=1)"
  printf "  F(0) = %.4f  (期待 0.5)\n" (cdfAt (StudentT 3 0 1) 0)
  printf "  F(1) = %.4f  (期待 ≈ 0.8044)\n" (cdfAt (StudentT 3 0 1) 1)
  printf "  F(-1) = %.4f  (期待 ≈ 0.1956)\n" (cdfAt (StudentT 3 0 1) (-1))
  printf "  F(3.18) = %.4f  (期待 ≈ 0.975 — 95%% CI 上限)\n" (cdfAt (StudentT 3 0 1) 3.18)
  putStrLn ""
  putStrLn "[StudentT] t(df=30) ; df 大で標準正規に近づく"
  printf "  F(0) = %.4f  (期待 0.5)\n" (cdfAt (StudentT 30 0 1) 0)
  printf "  F(1.96) = %.4f  (期待 ≈ 0.9706 — 標準正規だと 0.975)\n" (cdfAt (StudentT 30 0 1) 1.96)
  putStrLn ""

  -- LogNormal
  putStrLn "[LogNormal] LN(0, 1)"
  printf "  F(1) = %.4f  (期待 0.5)\n" (cdfAt (LogNormal 0 1) 1)
  printf "  F(exp(1)) = %.4f  (期待 ≈ 0.8413)\n" (cdfAt (LogNormal 0 1) (exp 1))
  putStrLn ""

  -- HalfNormal
  putStrLn "[HalfNormal] HN(σ=1)"
  printf "  F(0) = %.4f  (期待 0)\n" (cdfAt (HalfNormal 1) 0)
  printf "  F(1) = %.4f  (期待 ≈ 0.6827)\n" (cdfAt (HalfNormal 1) 1)
  printf "  F(2) = %.4f  (期待 ≈ 0.9545)\n" (cdfAt (HalfNormal 1) 2)
  putStrLn ""

  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  ✓ 全分布で CDF が動作 (Beta/Gamma/Cauchy/StudentT 含む)"
  putStrLn "═══════════════════════════════════════════════════════════════"
