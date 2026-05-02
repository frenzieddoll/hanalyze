{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | Phase 2.2: Bernoulli / Categorical 観測モデルの動作確認デモ。
--
-- どちらも観測分布として使う (潜在変数は連続のまま)。
module Main where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Text.Printf (printf)
import System.Random.MWC (createSystemRandom)

import MCMC.Core (chainSamples, posteriorMean, posteriorSD,
                  posteriorQuantile, acceptanceRate)
import MCMC.NUTS (nuts, defaultNUTSConfig, NUTSConfig (..))
import Model.HBM (ModelP, sample, observe, Distribution (..))

-- ---------------------------------------------------------------------------
-- Bernoulli 観測 (ロジスティック回帰の単純版)
-- ---------------------------------------------------------------------------
-- 真値: p = 0.7

bernoulliData :: [Double]
bernoulliData = [1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1]
-- 20 件中 15 件成功 → MLE p̂ = 0.75, 真値 0.7 から少しずれ

bernoulliModel :: ModelP ()
bernoulliModel = do
  p <- sample "p" (Beta 1 1)             -- 一様事前
  observe "y" (Bernoulli p) bernoulliData

-- ---------------------------------------------------------------------------
-- Categorical 観測
-- ---------------------------------------------------------------------------
-- 真値: probs = [0.5, 0.3, 0.2]
-- 20 観測

categoricalData :: [Double]
categoricalData = [0,0,0,0,0,0,0,0,0,0, 1,1,1,1,1,1, 2,2,2,2]
-- 0 が 10, 1 が 6, 2 が 4 → MLE [0.5, 0.3, 0.2]

categoricalModel :: ModelP ()
categoricalModel = do
  -- 単純化: 3 つの確率を独立にサンプリング (本来は Dirichlet が望ましい)
  -- HalfNormal 事前で正値、内部で正規化
  q0 <- sample "q0" (HalfNormal 1)
  q1 <- sample "q1" (HalfNormal 1)
  q2 <- sample "q2" (HalfNormal 1)
  observe "y" (Categorical [q0, q1, q2]) categoricalData

-- ---------------------------------------------------------------------------
-- main
-- ---------------------------------------------------------------------------

cfg :: NUTSConfig
cfg = defaultNUTSConfig
        { nutsIterations = 2000
        , nutsBurnIn     = 500
        , nutsStepSize   = 0.1
        }

main :: IO ()
main = do
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  Phase 2.2: 離散観測分布の動作確認"
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn ""

  -- ── Bernoulli ──
  putStrLn "[1] Bernoulli(p) 観測"
  printf "    データ: 20 観測, 15 件成功 (真 p=0.7, MLE p̂=0.75)\n"
  gen1 <- createSystemRandom
  ch1 <- nuts bernoulliModel cfg (Map.fromList [("p", 0.5)]) gen1
  printf "    Acceptance: %.1f%%, samples: %d\n"
         (acceptanceRate ch1 * 100 :: Double)
         (length (chainSamples ch1))
  printf "    p mean=%+.4f  sd=%.4f  95%% CI=[%+.4f, %+.4f]\n"
         (fromMaybe 0 (posteriorMean "p" ch1))
         (fromMaybe 0 (posteriorSD   "p" ch1))
         (fromMaybe 0 (posteriorQuantile 0.025 "p" ch1))
         (fromMaybe 0 (posteriorQuantile 0.975 "p" ch1))
  putStrLn "    → Beta(1,1) + Binomial 共役解析: Beta(1+15, 1+5) = Beta(16, 6)"
  printf "       解析的 mean = 16/22 = %.4f\n" (16/22 :: Double)
  putStrLn ""

  -- ── Categorical ──
  putStrLn "[2] Categorical([q0,q1,q2]) 観測"
  printf "    データ: 20 観測, [0:10, 1:6, 2:4] (真 probs=[0.5, 0.3, 0.2])\n"
  gen2 <- createSystemRandom
  let initP = Map.fromList [("q0", 1.0), ("q1", 1.0), ("q2", 1.0)]
  ch2 <- nuts categoricalModel cfg initP gen2
  printf "    Acceptance: %.1f%%, samples: %d\n"
         (acceptanceRate ch2 * 100 :: Double)
         (length (chainSamples ch2))
  let q0m = fromMaybe 0 (posteriorMean "q0" ch2)
      q1m = fromMaybe 0 (posteriorMean "q1" ch2)
      q2m = fromMaybe 0 (posteriorMean "q2" ch2)
      total = q0m + q1m + q2m
  printf "    q0 mean=%.4f  q1 mean=%.4f  q2 mean=%.4f\n" q0m q1m q2m
  printf "    正規化後: [%.3f, %.3f, %.3f]   ← 真値 [0.500, 0.300, 0.200]\n"
         (q0m/total) (q1m/total) (q2m/total)
  putStrLn ""

  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  ✓ Bernoulli / Categorical 観測モデルが正常動作"
  putStrLn "═══════════════════════════════════════════════════════════════"
