{-# LANGUAGE OverloadedStrings #-}
-- 1次元ガウスモデルで HMC / NUTS の動作を確認する。
--
-- モデル: μ ~ Normal(0, 10), y | μ ~ Normal(μ, 1), data = [1, 2, 3]
-- 解析的事後分布: μ|y ~ Normal(μ_post, σ_post)
--   σ_post^2 = 1 / (1/10^2 + 3/1^2) ≈ 0.332  → σ_post ≈ 0.577
--   μ_post   = σ_post^2 * (0/10^2 + 6/1)    ≈ 1.993
module Main where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Text.Printf (printf)
import System.Random.MWC (createSystemRandom)

import Model.HBM
import MCMC.Core (Chain (..), chainVals, posteriorMean, posteriorSD, acceptanceRate)
import MCMC.HMC
import MCMC.NUTS
import Stat.Distribution
import Stat.MCMC (rhat)

-- モデル1: μ のみ (unconstrained)
gaussModel :: [Double] -> Model Double
gaussModel ys = do
  mu <- sample "mu" (Normal 0 10)
  observe "y" (Normal mu 1) ys
  return mu

-- モデル2: sigma ~ Exponential(1) (constrained: sigma > 0)
-- データ: [1,2,3], 真値 sigma=1
-- 解析解は複雑だが sigma の事後平均は 1 付近に収束するはず
scaledModel :: [Double] -> Model Double
scaledModel ys = do
  sigma <- sample "sigma" (Exponential 1)
  observe "y" (Normal 0 sigma) ys
  return sigma

observed :: [Double]
observed = [1.0, 2.0, 3.0]

initP :: Map.Map T.Text Double
initP = Map.fromList [("mu", 0.0)]

initP2 :: Map.Map T.Text Double
initP2 = Map.fromList [("sigma", 1.5)]

main :: IO ()
main = do
  gen <- createSystemRandom
  let m = gaussModel observed

  putStrLn "=== HMC (unconstrained μ) ==="
  let hmcCfg = defaultHMCConfig
        { hmcIterations    = 3000
        , hmcBurnIn        = 500
        , hmcStepSize      = 0.3
        , hmcLeapfrogSteps = 5
        }
  ch1 <- hmc m hmcCfg initP gen
  printf "  acceptance rate : %.3f\n"  (acceptanceRate ch1)
  printf "  posterior mean  : %.4f  (expect ≈ 1.993)\n"
    (maybe 0 id $ posteriorMean "mu" ch1)
  printf "  posterior SD    : %.4f  (expect ≈ 0.577)\n"
    (maybe 0 id $ posteriorSD   "mu" ch1)

  putStrLn ""
  putStrLn "=== NUTS (unconstrained μ) ==="
  let nutsCfg = defaultNUTSConfig
        { nutsIterations = 3000
        , nutsBurnIn     = 500
        , nutsStepSize   = 0.3
        }
  ch2 <- nuts m nutsCfg initP gen
  printf "  acceptance rate : %.3f\n"  (acceptanceRate ch2)
  printf "  posterior mean  : %.4f  (expect ≈ 1.993)\n"
    (maybe 0 id $ posteriorMean "mu" ch2)
  printf "  posterior SD    : %.4f  (expect ≈ 0.577)\n"
    (maybe 0 id $ posteriorSD   "mu" ch2)

  -- 制約付きパラメータのテスト: sigma ~ Exponential (正値制約)
  putStrLn ""
  putStrLn "=== HMC (constrained σ ~ Exponential, PositiveT) ==="
  let m2 = scaledModel observed
      hmcCfg2 = defaultHMCConfig
        { hmcIterations    = 3000
        , hmcBurnIn        = 500
        , hmcStepSize      = 0.1
        , hmcLeapfrogSteps = 10
        }
  ch3 <- hmc m2 hmcCfg2 initP2 gen
  printf "  acceptance rate : %.3f\n"  (acceptanceRate ch3)
  printf "  posterior mean σ: %.4f  (expect > 0)\n"
    (maybe 0 id $ posteriorMean "sigma" ch3)
  printf "  posterior SD σ  : %.4f\n"
    (maybe 0 id $ posteriorSD "sigma" ch3)
  let samples3 = map (Map.findWithDefault 0 "sigma") (chainSamples ch3)
      minSigma = minimum samples3
  printf "  min σ sample    : %.6f  (must be > 0)\n" minSigma

  putStrLn ""
  putStrLn "=== NUTS (constrained σ ~ Exponential, PositiveT) ==="
  let nutsCfg2 = defaultNUTSConfig
        { nutsIterations = 3000
        , nutsBurnIn     = 500
        , nutsStepSize   = 0.1
        }
  ch4 <- nuts m2 nutsCfg2 initP2 gen
  printf "  acceptance rate : %.3f\n"  (acceptanceRate ch4)
  printf "  posterior mean σ: %.4f  (expect > 0)\n"
    (maybe 0 id $ posteriorMean "sigma" ch4)
  printf "  posterior SD σ  : %.4f\n"
    (maybe 0 id $ posteriorSD "sigma" ch4)
  let samples4 = map (Map.findWithDefault 0 "sigma") (chainSamples ch4)
      minSigma4 = minimum samples4
  printf "  min σ sample    : %.6f  (must be > 0)\n" minSigma4

  -- 並列チェーン + R-hat テスト
  putStrLn ""
  putStrLn "=== 4-chain NUTS (parallel) + split-R-hat ==="
  putStrLn "  Model: μ ~ Normal(0,10), y|μ ~ Normal(μ,1), data=[1,2,3]"
  let nutsCfgR = defaultNUTSConfig
        { nutsIterations = 2000
        , nutsBurnIn     = 500
        , nutsStepSize   = 0.3
        }
  chains <- nutsChains m nutsCfgR 4 initP gen
  let muVals = map (chainVals "mu") chains
      rhatMu = rhat muVals
  mapM_ (\(i, ch) ->
    printf "  chain %d: mean=%.4f  SD=%.4f  accept=%.3f\n"
      (i :: Int)
      (maybe 0 id $ posteriorMean "mu" ch)
      (maybe 0 id $ posteriorSD   "mu" ch)
      (acceptanceRate ch)
    ) (zip [1..] chains)
  case rhatMu of
    Nothing -> putStrLn "  R-hat: N/A"
    Just r  -> printf "  split-R-hat (μ): %.4f  (< 1.01 = converged)\n" r
