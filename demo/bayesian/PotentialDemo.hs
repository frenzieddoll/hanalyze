{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | Potential プリミティブのデモ (PyMC `pm.Potential` 相当)。
--
-- 任意の log-prob 項を log-joint に加える機能。3 つの典型用途を例示:
--   1. ソフト順序制約 (μ_1 < μ_2)
--   2. ベイズ的な L2 正則化 (ridge)
--   3. カスタム尤度 (既存分布で表せない観測モデル)
module Main where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Text.Printf (printf)
import System.Random.MWC (createSystemRandom)

import Hanalyze.MCMC.Core (chainSamples, posteriorMean, posteriorSD,
                  posteriorQuantile, acceptanceRate)
import Hanalyze.MCMC.NUTS (nuts, defaultNUTSConfig, NUTSConfig (..))
import Hanalyze.Model.HBM (ModelP, sample, observe, potential, Distribution (..))

cfg :: NUTSConfig
cfg = defaultNUTSConfig
        { nutsIterations = 1500
        , nutsBurnIn     = 500
        , nutsStepSize   = 0.1
        }

-- ---------------------------------------------------------------------------
-- 例 1: ソフト順序制約 mu1 < mu2
-- ---------------------------------------------------------------------------
-- 2 群のデータ。Potential で μ_1 < μ_2 を ソフトに強制する。
-- 制約違反時は -1000 の罰則を加える (実質ゼロ確率)。

obs1 :: [Double]
obs1 = [1.5, 2.0, 1.8, 2.1, 1.6]
obs2 :: [Double]
obs2 = [3.5, 3.8, 3.2, 3.6, 3.9]

-- 制約なし版
unconstrainedModel :: ModelP ()
unconstrainedModel = do
  mu1 <- sample "mu1" (Normal 0 10)
  mu2 <- sample "mu2" (Normal 0 10)
  sigma <- sample "sigma" (HalfNormal 5)
  observe "y1" (Normal mu1 sigma) obs1
  observe "y2" (Normal mu2 sigma) obs2

-- 制約付き版: Potential で μ_1 < μ_2 を強制
orderedModel :: ModelP ()
orderedModel = do
  mu1 <- sample "mu1" (Normal 0 10)
  mu2 <- sample "mu2" (Normal 0 10)
  sigma <- sample "sigma" (HalfNormal 5)
  -- ソフト制約: mu1 >= mu2 なら大きな罰則
  potential "order" (if mu1 < mu2 then 0 else -1000)
  observe "y1" (Normal mu1 sigma) obs1
  observe "y2" (Normal mu2 sigma) obs2

-- ---------------------------------------------------------------------------
-- 例 2: ベイズ的な L2 正則化 (ridge regression)
-- ---------------------------------------------------------------------------
-- 通常 β ~ Normal(0, σ_β) と書くのと等価だが、Potential で直接記述すると
-- 自由度がある (例えば lambda を別に決められる)。

xs2 :: [Double]
xs2 = [-2.0, -1.0, 0.0, 1.0, 2.0, -1.5, 0.5, 1.5, -0.5, 0.0]
ys2 :: [Double]
ys2 = [-3.5, -1.8, 0.2, 2.1, 4.0, -2.7, 1.0, 3.2, -0.9, 0.1]

ridgeModel :: ModelP ()
ridgeModel = do
  alpha <- sample "alpha" (Normal 0 100)   -- 切片はフラット事前
  beta  <- sample "beta"  (Normal 0 100)   -- 傾きもフラット
  sigma <- sample "sigma" (HalfNormal 5)
  -- Ridge ペナルティ: -0.5 * lambda * beta^2 (lambda=2.0)
  let lambda = 2.0
  potential "ridge" (-0.5 * lambda * beta * beta)
  -- 観測尤度
  mapM_ (\(x, y) -> let xC = realToFrac x
                    in observe "y" (Normal (alpha + beta * xC) sigma) [y])
        (zip xs2 ys2)

-- ---------------------------------------------------------------------------
-- 例 3: カスタム尤度 — Laplace ノイズ (頑健回帰)
-- ---------------------------------------------------------------------------
-- 既存の Distribution に Laplace は無いので、Potential で直接記述する。
-- log p(y|μ,b) = -log(2b) - |y - μ| / b

xs3, ys3 :: [Double]
xs3 = [0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 2.5]
ys3 = [0.1, 1.2, 2.0, 3.3, 4.1, 5.0, 8.0]   -- (2.5, 8.0) は外れ値

laplaceRegModel :: ModelP ()
laplaceRegModel = do
  alpha <- sample "alpha" (Normal 0 10)
  beta  <- sample "beta"  (Normal 0 10)
  b     <- sample "b"     (HalfNormal 3)   -- スケール
  -- Laplace 尤度を Potential で記述
  let logLapl mu y = -log (2 * b) - abs (realToFrac y - mu) / b
  mapM_ (\(x, y) -> let mu = alpha + beta * realToFrac x
                    in potential "laplace_lik" (logLapl mu y))
        (zip xs3 ys3)

-- ---------------------------------------------------------------------------
-- main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  Potential プリミティブのデモ (PyMC pm.Potential 相当)"
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn ""

  -- ── 例 1 ──
  putStrLn "[1] ソフト順序制約 mu1 < mu2"
  printf "    観測: y1 = %s (低)\n          y2 = %s (高)\n"
         (show obs1) (show obs2)
  gen <- createSystemRandom

  putStrLn "  制約なし: μ_1, μ_2 は独立にサンプリングされる"
  ch1 <- nuts unconstrainedModel cfg
              (Map.fromList [("mu1", 0.0), ("mu2", 0.0), ("sigma", 1.0)]) gen
  printf "    mu1 = %+.4f ± %.4f   mu2 = %+.4f ± %.4f\n"
         (fromMaybe 0 (posteriorMean "mu1" ch1)) (fromMaybe 0 (posteriorSD "mu1" ch1))
         (fromMaybe 0 (posteriorMean "mu2" ch1)) (fromMaybe 0 (posteriorSD "mu2" ch1))

  putStrLn "  制約付き: Potential で μ_1 < μ_2 を強制"
  ch2 <- nuts orderedModel cfg
              (Map.fromList [("mu1", 0.0), ("mu2", 4.0), ("sigma", 1.0)]) gen
  printf "    mu1 = %+.4f ± %.4f   mu2 = %+.4f ± %.4f\n"
         (fromMaybe 0 (posteriorMean "mu1" ch2)) (fromMaybe 0 (posteriorSD "mu1" ch2))
         (fromMaybe 0 (posteriorMean "mu2" ch2)) (fromMaybe 0 (posteriorSD "mu2" ch2))
  -- 制約違反サンプル数
  let violations = length [() | s <- chainSamples ch2
                              , let m1 = Map.findWithDefault 0 "mu1" s
                                    m2 = Map.findWithDefault 0 "mu2" s
                              , m1 >= m2]
  printf "    制約違反 (mu1 ≥ mu2) のサンプル数: %d / %d\n"
         violations (length (chainSamples ch2))
  putStrLn ""

  -- ── 例 2 ──
  putStrLn "[2] Ridge 正則化 (Potential で -0.5 * λ * β²)"
  printf "    データ: 直線 y ≈ 1.8x (10 点)\n"
  ch3 <- nuts ridgeModel cfg
              (Map.fromList [("alpha", 0.0), ("beta", 0.0), ("sigma", 1.0)]) gen
  printf "    alpha = %+.4f ± %.4f\n"
         (fromMaybe 0 (posteriorMean "alpha" ch3)) (fromMaybe 0 (posteriorSD "alpha" ch3))
  printf "    beta  = %+.4f ± %.4f   (Ridge により 0 寄りに縮小)\n"
         (fromMaybe 0 (posteriorMean "beta"  ch3)) (fromMaybe 0 (posteriorSD "beta"  ch3))
  printf "    sigma = %+.4f ± %.4f\n"
         (fromMaybe 0 (posteriorMean "sigma" ch3)) (fromMaybe 0 (posteriorSD "sigma" ch3))
  putStrLn ""

  -- ── 例 3 ──
  putStrLn "[3] カスタム Laplace 尤度 (頑健回帰)"
  printf "    データ: y ≈ x + ε (7 点中 (2.5, 8.0) は外れ値)\n"
  ch4 <- nuts laplaceRegModel cfg
              (Map.fromList [("alpha", 0.0), ("beta", 1.0), ("b", 1.0)]) gen
  printf "    alpha = %+.4f ± %.4f\n"
         (fromMaybe 0 (posteriorMean "alpha" ch4)) (fromMaybe 0 (posteriorSD "alpha" ch4))
  printf "    beta  = %+.4f ± %.4f   (外れ値に頑健 → 真値 1.0 に近い)\n"
         (fromMaybe 0 (posteriorMean "beta"  ch4)) (fromMaybe 0 (posteriorSD "beta"  ch4))
  printf "    b     = %+.4f ± %.4f   (Laplace スケール)\n"
         (fromMaybe 0 (posteriorMean "b"     ch4)) (fromMaybe 0 (posteriorSD "b"     ch4))
  putStrLn ""

  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  ✓ Potential が 3 つの典型用途で動作 (制約・正則化・カスタム尤度)"
  putStrLn "═══════════════════════════════════════════════════════════════"
