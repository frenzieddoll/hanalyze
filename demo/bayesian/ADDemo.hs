{-# LANGUAGE OverloadedStrings #-}
-- | 自動微分 (AD) vs 数値微分のデモ。
--
-- Normal-Normal モデル:
--   mu    ~ Normal(0, 10)
--   sigma ~ Exponential(1)
--   y_i   ~ Normal(mu, sigma)
--
-- 比較項目:
--   1. 勾配の正確性: AD 勾配 vs 数値微分 vs 解析的勾配
--   2. HMC の収束: 数値微分版 vs AD 版 (同じパラメータで)
module Main where

import qualified Data.Map.Strict as Map
import Text.Printf (printf)
import System.Random.MWC (createSystemRandom)

import MCMC.Core   (Chain (..), posteriorMean, posteriorSD)
import MCMC.HMC    (HMCConfig (..), defaultHMCConfig, hmc, gradU)
import Model.HBM   (Model, sample, observe, sampleNames)
import qualified Stat.Distribution as D
import Stat.AD     (LogJointF, logNormalF, logNormalObsF, logExpF,
                    gradAD, hmcAD)
import Stat.Distribution (Transform (..))

-- ---------------------------------------------------------------------------
-- モデル定義 (HBM DSL 版)
-- ---------------------------------------------------------------------------

normalModel :: [Double] -> Model ()
normalModel ys = do
  mu    <- sample "mu"    (D.Normal 0 10)
  sigma <- sample "sigma" (D.Exponential 1)
  observe "y" (D.Normal mu sigma) ys

-- ---------------------------------------------------------------------------
-- 同じモデルの AD 用多相関数版
-- ---------------------------------------------------------------------------

-- theta = [mu, sigma]  (sigma は PositiveT 変換で扱われる)
normalLogJoint :: [Double] -> LogJointF
normalLogJoint ys [mu, sigma] =
  logNormalF 0 10 mu                            -- prior: mu ~ N(0,10)
  + logExpF 1 sigma                             -- prior: sigma ~ Exp(1)
  + sum [ logNormalObsF y mu sigma | y <- ys ]  -- lik: y_i ~ N(mu,sigma)
normalLogJoint _ _ = 0

-- ---------------------------------------------------------------------------
-- 解析的勾配 (検証用)
-- ---------------------------------------------------------------------------

analyticGrad :: [Double] -> [Double] -> [Double]
analyticGrad ys [mu, sigma] =
  let n   = fromIntegral (length ys) :: Double
      ss  = sum [ (y - mu)^(2::Int) | y <- ys ]
      dMu  = -(mu / 100.0)                   -- ∂ log N(mu;0,10)/∂mu
             + (sum ys - n*mu) / sigma^(2::Int) -- ∂ Σ log N(y;mu,sigma)/∂mu
      dSig = -1.0                             -- ∂ log Exp(sigma;1)/∂sigma
             - n/sigma                        -- ∂ Σ log N(y;mu,sigma)/∂sigma (1st term)
             + ss / sigma^(3::Int)            -- (2nd term)
  in [dMu, dSig]
analyticGrad _ _ = []

-- ---------------------------------------------------------------------------
-- main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  gen <- createSystemRandom

  -- 観測データ (真値: mu=2, sigma=1.5)
  let trueMu  = 2.0 :: Double
      trueSig = 1.5 :: Double
      obs     = [-0.5, 0.3, 1.2, 2.0, 2.8, 3.5, 4.1, 1.7, 2.3, 0.9
                ,  2.1, 1.4, 3.2, 2.7, 1.1, 2.5, 3.0, 1.8, 2.2, 2.6]
      testPt  = [1.5, 1.2]   -- 勾配評価点: [mu, sigma]

  putStrLn "=== 1. 勾配精度の比較 (評価点: mu=1.5, sigma=1.2) ==="

  let model    = normalModel obs
      names    = sampleNames model
      paramMap = Map.fromList (zip names testPt)
      numGrad  = gradU model names paramMap          -- 数値微分 (中心差分)
      adGrad   = gradAD (normalLogJoint obs) testPt  -- AD (正確)
      anGrad   = analyticGrad obs testPt             -- 解析解 (検証用)

  -- gradU は ∇U = -∇logπ を返す。AD は ∇logπ を返す。符号が逆なのは正常。
  putStrLn "\n  パラメータ     数値微分(-∇logπ)  AD(+∇logπ)     解析解(+∇logπ)"
  putStrLn "  ─────────────────────────────────────────────────────────────"
  mapM_ (\(nm, ng, ag, an) ->
    printf "  %-12s  %12.8f   %12.8f   %12.8f\n" nm ng ag an)
    (zip4 names numGrad adGrad anGrad)

  -- 符号を揃えて比較 (両者とも ∇logπ として)
  let maxDiff = maximum (zipWith (\a n -> abs (a + n)) adGrad numGrad)
  printf "\n  |AD - 数値微分| の最大値: %.2e  (符号揃え済み)\n\n" maxDiff

  -- ---------------------------------------------------------------------------
  putStrLn "=== 2. HMC 数値微分版 (gradU: 中心差分 h=1e-5) ==="
  let cfg   = defaultHMCConfig
                { hmcIterations = 2000, hmcBurnIn = 500
                , hmcStepSize = 0.1, hmcLeapfrogSteps = 5 }
      initP = Map.fromList [("mu", 0.0), ("sigma", 1.0)]

  chain1 <- hmc model cfg initP gen
  reportChain chain1 trueMu trueSig

  -- ---------------------------------------------------------------------------
  putStrLn "\n=== 3. HMC AD 版 (Numeric.AD.Mode.Forward.grad) ==="
  let transforms = [UnconstrainedT, PositiveT]
  chain2 <- hmcAD (normalLogJoint obs) transforms cfg
                  ["mu", "sigma"] initP gen
  reportChain chain2 trueMu trueSig

  putStrLn "\n✓ 完了"

reportChain :: Chain -> Double -> Double -> IO ()
reportChain ch trueMu trueSig = do
  let acc = fromIntegral (chainAccepted ch) / fromIntegral (chainTotal ch) :: Double
  printf "  mu   : mean=%7.4f  sd=%7.4f  (真値 %.1f)\n"
    (maybe 0 id (posteriorMean "mu"    ch))
    (maybe 0 id (posteriorSD   "mu"    ch))
    trueMu
  printf "  sigma: mean=%7.4f  sd=%7.4f  (真値 %.1f)\n"
    (maybe 0 id (posteriorMean "sigma" ch))
    (maybe 0 id (posteriorSD   "sigma" ch))
    trueSig
  printf "  受容率: %.1f%%  サンプル数: %d\n"
    (acc * 100) (length (chainSamples ch))

zip4 :: [a] -> [b] -> [c] -> [d] -> [(a,b,c,d)]
zip4 (a:as) (b:bs) (c:cs) (d:ds) = (a,b,c,d) : zip4 as bs cs ds
zip4 _ _ _ _ = []
