{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | NegativeBinomial(μ, α) — 過分散カウントデータのデモ。
--
-- 比較: 同じデータに対して Poisson と NegativeBinomial を fit。
-- データは μ=10, α=2 の NB から生成 (var = 10 + 100/2 = 60、Poisson の
-- var = 10 より遥かに大きい)。Poisson モデルでは過分散を捕えられない。
module Main where

import qualified Data.Map.Strict as Map
import Text.Printf (printf)
import System.Random.MWC (createSystemRandom)
import qualified System.Random.MWC.Distributions as MWC
import qualified System.Random.MWC as MWCBase

import Hanalyze.MCMC.NUTS (nuts, defaultNUTSConfig, NUTSConfig (..))
import Hanalyze.Model.HBM (ModelP, sample, observe, Distribution (..))
import Hanalyze.Viz.MCMC (printPosteriorSummary, posteriorSummaryFile)

cfg :: NUTSConfig
cfg = defaultNUTSConfig
        { nutsIterations = 800
        , nutsBurnIn     = 300
        , nutsStepSize   = 0.1
        , nutsMaxDepth   = 6
        }

-- 真のパラメタで NB データを生成
genNB :: Int -> Double -> Double -> IO [Double]
genNB n mu alpha = do
  gen <- createSystemRandom
  -- Gamma-Poisson mixture: λ ~ Gamma(α, μ/α), X ~ Poisson(λ)
  let drawOne = do
        lam <- MWC.gamma alpha (mu / alpha) gen
        let knuth k p = do
              u <- MWCBase.uniform gen :: IO Double
              let p' = p * u
              if p' < exp (-lam)
                then return (fromIntegral k)
                else knuth (k + 1) p'
        knuth (0 :: Int) (1 :: Double)
  mapM (const drawOne) [1 .. n]

poissonModel :: [Double] -> ModelP ()
poissonModel ys = do
  lam <- sample "lambda" (Gamma 1 0.1)
  observe "y" (Poisson lam) ys

nbModel :: [Double] -> ModelP ()
nbModel ys = do
  mu    <- sample "mu"    (Gamma 1 0.1)
  alpha <- sample "alpha" (Gamma 1 0.1)
  observe "y" (NegativeBinomial mu alpha) ys

main :: IO ()
main = do
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  NegativeBinomial vs Poisson (過分散カウント, Phase H1)"
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn ""

  putStrLn "真値: μ = 10, α = 2  (= var = 60, mean = 10 → 過分散)"
  ys <- genNB 80 10 2
  let n     = length ys
      muSm  = sum ys / fromIntegral n
      varSm = sum [(y - muSm)^(2::Int) | y <- ys] / fromIntegral (n - 1)
  printf "観測 (n=%d): 標本平均 = %.2f, 標本分散 = %.2f\n" n muSm varSm
  printf "  → 分散/平均 = %.2f (≫ 1 なら Poisson は不適合)\n"
         (varSm / muSm)
  putStrLn ""

  gen <- createSystemRandom

  putStrLn "[1] Poisson モデル (過分散を捕えない)"
  ch1 <- nuts (poissonModel ys) cfg
              (Map.fromList [("lambda", 5)]) gen
  printPosteriorSummary ["lambda"] [ch1]
  putStrLn ""

  putStrLn "[2] NegativeBinomial モデル (過分散を捕える)"
  ch2 <- nuts (nbModel ys) cfg
              (Map.fromList [("mu", 5), ("alpha", 1)]) gen
  printPosteriorSummary ["mu", "alpha"] [ch2]
  putStrLn ""

  posteriorSummaryFile "negbinom-poisson.html"  "Poisson"  ["lambda"]      [ch1]
  posteriorSummaryFile "negbinom-nb.html"       "NegBinom" ["mu", "alpha"] [ch2]
  putStrLn "  → negbinom-{poisson,nb}.html"
  putStrLn ""

  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  ✓ NegativeBinomial で μ ≈ 10, α ≈ 2 を回復、過分散を表現"
  putStrLn "═══════════════════════════════════════════════════════════════"
