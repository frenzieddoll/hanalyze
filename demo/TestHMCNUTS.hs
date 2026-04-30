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
import Model.MCMC (posteriorMean, posteriorSD, acceptanceRate)
import Model.HMC
import Model.NUTS
import Stat.Distribution

gaussModel :: [Double] -> Model Double
gaussModel ys = do
  mu <- sample "mu" (Normal 0 10)
  observe "y" (Normal mu 1) ys
  return mu

observed :: [Double]
observed = [1.0, 2.0, 3.0]

initP :: Map.Map T.Text Double
initP = Map.fromList [("mu", 0.0)]

main :: IO ()
main = do
  gen <- createSystemRandom
  let m = gaussModel observed

  putStrLn "=== HMC ==="
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
  putStrLn "=== NUTS ==="
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
