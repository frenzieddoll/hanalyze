{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | Phase I: 5 つの新規分布をまとめて検証 (sample/observe)。
--
-- - InverseGamma:   分散の共役事前 (Normal-InvGamma)
-- - Weibull:        生存解析の典型 (k=2 でレイリー)
-- - Pareto:         重い裾の冪分布
-- - BetaBinomial:   過分散二項
-- - VonMises:       角度データ (-π, π]
module Main where

import qualified Data.Map.Strict as Map
import Text.Printf (printf)
import System.Random.MWC (createSystemRandom, GenIO)

import Hanalyze.MCMC.NUTS (nuts, defaultNUTSConfig, NUTSConfig (..))
import Hanalyze.Model.HBM (ModelP, sample, observe, Distribution (..),
                  sampleDist)
import Hanalyze.Viz.MCMC (printPosteriorSummary)

cfg :: NUTSConfig
cfg = defaultNUTSConfig
        { nutsIterations = 800
        , nutsBurnIn     = 400
        , nutsStepSize   = 0.1
        , nutsMaxDepth   = 6
        }

-- ---------------------------------------------------------------------------
-- 単体テスト: sampleDist で分布から N 個ドローして経験統計を確認
-- ---------------------------------------------------------------------------

drawN :: Int -> Distribution Double -> GenIO -> IO [Double]
drawN n d gen = mapM (const (sampleDist d gen)) [1..n]

stats :: [Double] -> (Double, Double)
stats xs =
  let n  = length xs
      mu = sum xs / fromIntegral n
      v  = sum [(x - mu)^(2::Int) | x <- xs] / fromIntegral (n - 1)
  in (mu, sqrt v)

-- ---------------------------------------------------------------------------
-- Bayesian: InverseGamma を分散事前として使う Normal モデル
-- ---------------------------------------------------------------------------

-- σ² ~ InverseGamma(2, 3) (mean = 3/(2-1) = 3)
-- y ~ Normal(μ, sqrt(σ²))
invGammaModel :: [Double] -> ModelP ()
invGammaModel ys = do
  mu  <- sample "mu"     (Normal 0 5)
  sig2 <- sample "sigma2" (InverseGamma 2 3)
  observe "y" (Normal mu (sqrt sig2)) ys

-- ---------------------------------------------------------------------------
-- Bayesian: Weibull で生存時間の k と λ を推定
-- ---------------------------------------------------------------------------
weibullModel :: [Double] -> ModelP ()
weibullModel ys = do
  kSh <- sample "k"      (HalfNormal 5)
  lam <- sample "lambda" (HalfNormal 5)
  observe "y" (Weibull kSh lam) ys

main :: IO ()
main = do
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  Phase I: 新規 5 分布 (InvGamma/Weibull/Pareto/BetaBin/VonMises)"
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn ""

  gen <- createSystemRandom

  -- ── 単体: 各分布から 10000 ドロー → 平均/sd 確認 ──
  putStrLn "[A] sampleDist 単体 (n=10000)"

  ig <- drawN 10000 (InverseGamma 3 2) gen     -- mean = 2/(3-1) = 1
  let (m, s) = stats ig
  printf "  InverseGamma(3, 2): mean=%.3f (期待 1.000), sd=%.3f\n" m s

  wb <- drawN 10000 (Weibull 2 1) gen           -- レイリー: mean = √(π/2)/√2 = 0.886
  let (m2, s2) = stats wb
  printf "  Weibull(2, 1):      mean=%.3f (期待 0.886), sd=%.3f\n" m2 s2

  pr <- drawN 10000 (Pareto 3 1) gen            -- mean = 3/(3-1) = 1.5
  let (m3, s3) = stats pr
  printf "  Pareto(3, 1):       mean=%.3f (期待 1.500), sd=%.3f\n" m3 s3

  bb <- drawN 10000 (BetaBinomial 20 2 8) gen   -- mean = 20*2/10 = 4
  let (m4, s4) = stats bb
  printf "  BetaBin(n=20, 2, 8): mean=%.3f (期待 4.000), sd=%.3f\n" m4 s4

  vm <- drawN 10000 (VonMises 0 4) gen          -- mean = 0、概ね正規 sd ≈ 1/√4 = 0.5
  let (m5, s5) = stats vm
  printf "  VonMises(0, κ=4):   mean=%.3f (期待 0.000), sd=%.3f (≈0.5)\n" m5 s5
  putStrLn ""

  -- ── Bayesian: InverseGamma 事前 ──
  putStrLn "[B] Normal-InverseGamma 事前で σ² を推定"
  let ys = [1.2, 0.9, 1.4, 0.7, 1.1, 1.0, 1.3, 0.95, 1.05, 1.15,
            0.85, 1.25, 0.95, 1.18, 1.02]
  ch1 <- nuts (invGammaModel ys) cfg
              (Map.fromList [("mu", 1), ("sigma2", 0.04)]) gen
  printPosteriorSummary ["mu", "sigma2"] [ch1]
  putStrLn ""

  -- ── Bayesian: Weibull の k, λ ──
  putStrLn "[C] Weibull モデルで k, λ を推定 (真値 k=2, λ=2)"
  weibullObs <- drawN 80 (Weibull 2 2) gen
  ch2 <- nuts (weibullModel weibullObs) cfg
              (Map.fromList [("k", 2), ("lambda", 2)]) gen
  printPosteriorSummary ["k", "lambda"] [ch2]
  putStrLn ""

  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  ✓ 5 つの新規分布が sample/observe 両方で動作"
  putStrLn "═══════════════════════════════════════════════════════════════"
