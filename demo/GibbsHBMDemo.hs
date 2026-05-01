{-# LANGUAGE OverloadedStrings #-}
-- | Gibbs サンプラー × HBM DSL 統合デモ
--
-- gibbsFromModel で共役ペアを自動検出し、GibbsUpdate を自動構築する。
-- 検出できた場合は純 Gibbs、できない場合はハイブリッド Gibbs+MH になる。
--
-- 検証する3モデル:
--   1. Gamma-Poisson   : λ ~ Gamma(2,1), y ~ Poisson(λ)       [全パラメータ共役]
--   2. Beta-Binomial   : p ~ Beta(2,2),  y ~ Binomial(10, p)  [全パラメータ共役]
--   3. Normal-Normal+σ : μ ~ Normal(0,10), σ ~ Exponential(1) [μ 共役, σ は MH]
module Main where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf (printf)
import System.Random.MWC (createSystemRandom)

import Model.HBM
import Stat.Distribution (Distribution (..))
import MCMC.Core   (Chain (..), chainVals, posteriorMean, posteriorSD)
import MCMC.Gibbs  (GibbsConfig (..), defaultGibbsConfig,
                    gibbsFromModel, gibbsMH)

-- ---------------------------------------------------------------------------
-- モデル定義
-- ---------------------------------------------------------------------------

-- Model 1: Gamma-Poisson  (全共役)
poissonModel :: [Double] -> Model ()
poissonModel ys = do
  lam <- sample "lambda" (Gamma 2 1)
  observe "y" (Poisson lam) ys
  return ()

-- Model 2: Beta-Binomial  (全共役; 各 y は 0/1 の Bernoulli)
binomModel :: Int -> Int -> Model ()
binomModel nTrials nSucc = do
  p <- sample "p" (Beta 2 2)
  let ys = replicate nSucc 1.0 ++ replicate (nTrials - nSucc) 0.0
  observe "y" (Binomial 1 p) ys
  return ()

-- Model 3: Normal 平均推定 (μ 共役, σ は非共役 → MH)
normalModel :: [Double] -> Model ()
normalModel ys = do
  mu    <- sample "mu"    (Normal 0 10)
  sigma <- sample "sigma" (Exponential 1)
  observe "y" (Normal mu sigma) ys
  return ()

-- ---------------------------------------------------------------------------
-- ヘルパー
-- ---------------------------------------------------------------------------

cfg :: GibbsConfig
cfg = defaultGibbsConfig { gibbsIterations = 3000, gibbsBurnIn = 500 }

printResult :: Text -> Chain -> Double -> IO ()
printResult name ch truth = do
  let vals = chainVals name ch
  let mn   = maybe 0 id (posteriorMean name ch)
  let sd   = maybe 0 id (posteriorSD   name ch)
  printf "  %-10s | mean=%7.4f  sd=%7.4f  truth=%7.4f  n=%d\n"
         (T.unpack name) mn sd truth (length vals)

-- ---------------------------------------------------------------------------
-- main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  gen <- createSystemRandom

  -- ── Model 1: Gamma-Poisson ──────────────────────────────────────────────
  let trueL  = 4.0 :: Double
      poisObs = replicate 30 trueL  -- 簡易 "データ": 全観測値 = 真の平均
      pModel  = poissonModel poisObs
      (gpUpdates, gpMH) = gibbsFromModel pModel

  putStrLn "\n=== Model 1: Gamma(2,1) + Poisson(λ) ==="
  printf "  検出: Gibbs=%d ブロック, MH=%d パラメータ\n"
         (length gpUpdates) (length gpMH)

  ch1 <- gibbsMH pModel cfg Map.empty (Map.singleton "lambda" 1.0) gen
  printResult "lambda" ch1 trueL

  -- ── Model 2: Beta-Binomial ──────────────────────────────────────────────
  let trueP   = 0.7 :: Double
      nT = 100; nS = 70  -- 100 試行, 70 成功
      bModel  = binomModel nT nS
      (bbUpdates, bbMH) = gibbsFromModel bModel

  putStrLn "\n=== Model 2: Beta(2,2) + Binomial(1,p) ==="
  printf "  検出: Gibbs=%d ブロック, MH=%d パラメータ\n"
         (length bbUpdates) (length bbMH)

  ch2 <- gibbsMH bModel cfg Map.empty (Map.singleton "p" 0.5) gen
  printResult "p" ch2 trueP

  -- ── Model 3: Normal + Exponential (混合) ────────────────────────────────
  let trueMu  = 2.0 :: Double
      trueSig = 1.5 :: Double
      normObs = map (* trueSig) [-1.5,-1..1.5] ++ [trueMu]   -- 粗いデータ
      nModel  = normalModel normObs
      (nnUpdates, nnMH) = gibbsFromModel nModel

  putStrLn "\n=== Model 3: Normal(0,10) + Exponential(1) [混合モード] ==="
  printf "  検出: Gibbs=%d ブロック (mu), MH=%d パラメータ (sigma)\n"
         (length nnUpdates) (length nnMH)

  let mhSteps = Map.singleton "sigma" 0.3
      init3   = Map.fromList [("mu", 0.0), ("sigma", 1.0)]
  ch3 <- gibbsMH nModel cfg mhSteps init3 gen
  printResult "mu"    ch3 trueMu
  printResult "sigma" ch3 trueSig

  putStrLn "\n✓ 完了"
