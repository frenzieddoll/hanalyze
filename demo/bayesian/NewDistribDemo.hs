{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | Phase 2.1 で追加した連続分布の動作確認デモ。
--
-- 各分布を事前分布として使ったモデルを NUTS で推論する。
module Main where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Text.Printf (printf)
import System.Random.MWC (createSystemRandom)

import MCMC.Core (Chain, chainSamples, posteriorMean, posteriorSD,
                  posteriorQuantile, acceptanceRate)
import MCMC.NUTS (nuts, defaultNUTSConfig, NUTSConfig (..))
import Model.HBM (ModelP, sample, observe, Distribution (..))

obsData :: [Double]
obsData = [1.5, 2.1, 1.8, 2.5, 1.9, 2.3, 1.7, 2.0, 2.2, 1.6]

cfg :: NUTSConfig
cfg = defaultNUTSConfig
        { nutsIterations = 1500
        , nutsBurnIn     = 500
        , nutsStepSize   = 0.1
        }

-- ---------------------------------------------------------------------------
-- 各モデル定義 (top-level: rank-2 type の monomorphisation 回避)
-- ---------------------------------------------------------------------------

halfNormalModel :: ModelP ()
halfNormalModel = do
  mu    <- sample "mu"    (Normal 0 10)
  sigma <- sample "sigma" (HalfNormal 5)
  observe "y" (Normal mu sigma) obsData

halfCauchyModel :: ModelP ()
halfCauchyModel = do
  mu    <- sample "mu"    (Normal 0 10)
  sigma <- sample "sigma" (HalfCauchy 2)
  observe "y" (Normal mu sigma) obsData

studentTObs :: [Double]
studentTObs = obsData ++ [10.0]  -- 外れ値追加

studentTModel :: ModelP ()
studentTModel = do
  mu    <- sample "mu"    (Normal 0 10)
  sigma <- sample "sigma" (HalfNormal 5)
  observe "y" (StudentT 3 mu sigma) studentTObs   -- df=3

normalRobustModel :: ModelP ()
normalRobustModel = do
  mu    <- sample "mu"    (Normal 0 10)
  sigma <- sample "sigma" (HalfNormal 5)
  observe "y" (Normal mu sigma) studentTObs

logNormalObs :: [Double]
logNormalObs = [exp (1.5 + n) | n <-
                  [0.20, -0.10, 0.30, -0.05, 0.15, -0.20, 0.05, 0.10, -0.15, 0.0]]
-- 真値: log y ~ Normal(1.5, ~0.16)

logNormalModel :: ModelP ()
logNormalModel = do
  mu  <- sample "mu_log"  (Normal 0 10)
  sig <- sample "sig_log" (HalfNormal 2)
  observe "y" (LogNormal mu sig) logNormalObs

cauchyPriorModel :: ModelP ()
cauchyPriorModel = do
  mu  <- sample "mu" (Cauchy 0 1)
  sig <- sample "sigma" (HalfNormal 5)
  observe "y" (Normal mu sig) obsData

uniformPriorModel :: ModelP ()
uniformPriorModel = do
  mu  <- sample "mu" (Uniform (-5) 5)
  sig <- sample "sigma" (HalfNormal 3)
  observe "y" (Normal mu sig) obsData

-- ---------------------------------------------------------------------------
-- 共通ランナー
-- ---------------------------------------------------------------------------

runOne
  :: String           -- ラベル
  -> ModelP ()        -- モデル
  -> Map.Map Text Double  -- 初期値
  -> [Text]           -- 表示するパラメータ名
  -> IO ()
runOne label m initP params = do
  putStrLn $ "─── " ++ label ++ " ───"
  gen <- createSystemRandom
  chain <- nuts m cfg initP gen
  printf "  Acceptance: %.1f%%, samples: %d\n"
         (acceptanceRate chain * 100 :: Double)
         (length (chainSamples chain))
  mapM_ (printParam chain) params

printParam :: Chain -> Text -> IO ()
printParam chain p =
  printf "  %-10s mean=%+.4f  sd=%.4f  95%% CI=[%+.4f, %+.4f]\n"
         (T.unpack p)
         (fromMaybe 0 (posteriorMean p chain))
         (fromMaybe 0 (posteriorSD   p chain))
         (fromMaybe 0 (posteriorQuantile 0.025 p chain))
         (fromMaybe 0 (posteriorQuantile 0.975 p chain))

main :: IO ()
main = do
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  Phase 2.1: 追加分布の動作確認"
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn ""

  let init1 = Map.fromList [("mu", 0.0), ("sigma", 1.0)]
      init4 = Map.fromList [("mu_log", 0.0), ("sig_log", 1.0)]

  putStrLn "[1] HalfNormal 分散事前"
  runOne "HalfNormal" halfNormalModel init1 ["mu", "sigma"]
  putStrLn ""

  putStrLn "[2] HalfCauchy 分散事前 (重い裾)"
  runOne "HalfCauchy" halfCauchyModel init1 ["mu", "sigma"]
  putStrLn ""

  putStrLn "[3] StudentT 観測 (df=3) — 外れ値ロバスト"
  putStrLn "    データに 10.0 の外れ値が混入"
  runOne "StudentT_obs" studentTModel init1 ["mu", "sigma"]
  putStrLn "    比較: Normal 観測 (外れ値の影響を受けやすい)"
  runOne "Normal_obs " normalRobustModel init1 ["mu", "sigma"]
  putStrLn ""

  putStrLn "[4] LogNormal 観測 (真値 mu_log=1.5)"
  runOne "LogNormal" logNormalModel init4 ["mu_log", "sig_log"]
  putStrLn ""

  putStrLn "[5] Cauchy 事前"
  runOne "CauchyPrior" cauchyPriorModel init1 ["mu", "sigma"]
  putStrLn ""

  putStrLn "[6] Uniform 事前 (mu ∈ [-5, 5])"
  runOne "UniformPrior" uniformPriorModel init1 ["mu", "sigma"]
  putStrLn ""

  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  ✓ 全分布が正常にサンプリング可能"
  putStrLn "═══════════════════════════════════════════════════════════════"
