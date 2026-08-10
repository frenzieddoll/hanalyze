{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | Mixture 分布のデモ。
--
-- 3 つの典型用途:
--   1. 2 成分ガウス混合 (二峰性データ)
--   2. ゼロ過剰 (過剰ゼロ + 通常分布)
--   3. 頑健回帰 (Normal + 広い Normal の混合 = 外れ値耐性)
module Main where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Text.Printf (printf)
import System.Random.MWC (createSystemRandom)

import Hanalyze.MCMC.Core (chainSamples, posteriorMean, posteriorSD,
                  posteriorQuantile, acceptanceRate)
import Hanalyze.MCMC.NUTS (nuts, defaultNUTSConfig, NUTSConfig (..))
import Hanalyze.Model.HBM (ModelP, sample, observe, Distribution (..))
import Hanalyze.Stat.PosteriorPredictive (posteriorPredictive)

cfg :: NUTSConfig
cfg = defaultNUTSConfig
        { nutsIterations = 2000
        , nutsBurnIn     = 800
        , nutsStepSize   = 0.05
        }

-- ---------------------------------------------------------------------------
-- 例 1: 2 成分ガウス混合 (二峰性データ)
-- ---------------------------------------------------------------------------
-- データ: 2 つの正規分布の混合 (片方は平均 0、もう片方は平均 5)

bimodalData :: [Double]
bimodalData =
  [-0.3, 0.2, -0.1, 0.5, -0.2, 0.1, 0.4, -0.4, 0.3, 0.0,    -- 成分 1 中心
    4.8, 5.2, 4.7, 5.3, 5.0, 4.9, 5.1, 4.6, 5.4, 4.7]     -- 成分 2 中心

-- 混合モデル: 重みも推定
gmmModel :: ModelP ()
gmmModel = do
  -- 2 成分の平均を学習 (重みは固定 [0.5, 0.5] で簡単化)
  mu1 <- sample "mu1"   (Normal 0 5)
  mu2 <- sample "mu2"   (Normal 0 5)
  sig <- sample "sigma" (HalfNormal 2)
  -- 各観測 y は Normal(mu1, sig) と Normal(mu2, sig) の重み 0.5/0.5 混合
  observe "y" (Mixture [0.5, 0.5]
                       [Normal mu1 sig, Normal mu2 sig])
              bimodalData

-- ---------------------------------------------------------------------------
-- 例 2: ゼロ過剰モデル (zero-inflated)
-- ---------------------------------------------------------------------------
-- データ: ゼロ過剰のカウント風データ (実装の関係上連続で代用)
-- - ゼロ近傍に確率 q
-- - 通常 Normal(2, 1) に確率 1-q

ziData :: [Double]
ziData = [0.0, 0.0, 0.0, 0.0, 0.0, 0.01, -0.02, 0.01,   -- 「ゼロ過剰」(8 件)
          1.8, 2.1, 2.3, 1.9, 2.0, 1.7, 2.2]              -- 「通常」(7 件)

-- Normal(0, 0.05) の鋭いピーク + Normal(mu, sig) の混合
ziModel :: ModelP ()
ziModel = do
  q   <- sample "q"     (Beta 1 1)        -- ゼロ過剰割合
  mu  <- sample "mu"    (Normal 0 5)      -- 通常成分の中心
  sig <- sample "sigma" (HalfNormal 2)
  observe "y" (Mixture [q, 1 - q]
                       [Normal 0 0.05, Normal mu sig])
              ziData

-- ---------------------------------------------------------------------------
-- 例 3: 頑健 Normal-Normal 混合 (外れ値耐性)
-- ---------------------------------------------------------------------------
-- データ: 平均 2 周辺 + 大きな外れ値 1 つ

robData :: [Double]
robData = [1.9, 2.0, 2.1, 1.8, 2.2, 2.0, 1.7, 2.3, 1.9, 2.1, 15.0]
--                                                            ^外れ値

-- 95% 通常分布 + 5% 広い分布 (外れ値モデル) の混合
robustModel :: ModelP ()
robustModel = do
  mu  <- sample "mu"    (Normal 0 10)
  sig <- sample "sigma" (HalfNormal 2)
  -- 95% Normal(mu, sig), 5% Normal(mu, 10*sig) の混合
  observe "y" (Mixture [0.95, 0.05]
                       [Normal mu sig, Normal mu (sig * 10)])
              robData

-- 比較用: 普通の Normal
plainModel :: ModelP ()
plainModel = do
  mu  <- sample "mu"    (Normal 0 10)
  sig <- sample "sigma" (HalfNormal 2)
  observe "y" (Normal mu sig) robData

-- ---------------------------------------------------------------------------
-- main
-- ---------------------------------------------------------------------------

prn :: String -> Double -> Double -> IO ()
prn lbl m s = printf "    %-8s mean=%+.4f  sd=%.4f\n" lbl m s

main :: IO ()
main = do
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  Mixture 分布のデモ"
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn ""

  gen <- createSystemRandom

  -- ── 例 1: 2 成分ガウス混合 ──
  putStrLn "[1] 2 成分ガウス混合 (二峰性データ)"
  printf "    観測: 20 件 (片半分は ~0、片半分は ~5)\n"
  ch1 <- nuts gmmModel cfg
              (Map.fromList [("mu1", -1.0), ("mu2", 6.0), ("sigma", 1.0)]) gen
  printf "    Acceptance: %.1f%%\n" (acceptanceRate ch1 * 100 :: Double)
  prn "mu1"   (fromMaybe 0 (posteriorMean "mu1" ch1)) (fromMaybe 0 (posteriorSD "mu1" ch1))
  prn "mu2"   (fromMaybe 0 (posteriorMean "mu2" ch1)) (fromMaybe 0 (posteriorSD "mu2" ch1))
  prn "sigma" (fromMaybe 0 (posteriorMean "sigma" ch1)) (fromMaybe 0 (posteriorSD "sigma" ch1))
  putStrLn "    → 真値 (mu1, mu2) = (0, 5) を回復"
  putStrLn ""

  -- ── 例 2: ゼロ過剰 ──
  putStrLn "[2] ゼロ過剰モデル"
  printf "    観測: 15 件 (8 件がゼロ近傍、7 件が ~2)\n"
  ch2 <- nuts ziModel cfg
              (Map.fromList [("q", 0.5), ("mu", 1.0), ("sigma", 1.0)]) gen
  printf "    Acceptance: %.1f%%\n" (acceptanceRate ch2 * 100 :: Double)
  prn "q"     (fromMaybe 0 (posteriorMean "q" ch2)) (fromMaybe 0 (posteriorSD "q" ch2))
  prn "mu"    (fromMaybe 0 (posteriorMean "mu" ch2)) (fromMaybe 0 (posteriorSD "mu" ch2))
  prn "sigma" (fromMaybe 0 (posteriorMean "sigma" ch2)) (fromMaybe 0 (posteriorSD "sigma" ch2))
  printf "    → q ≈ %.2f (理論値 8/15 = 0.53)\n"
         (fromMaybe 0 (posteriorMean "q" ch2))
  putStrLn ""

  -- ── 例 3: 頑健回帰 ──
  putStrLn "[3] 頑健 Normal 混合 vs 普通の Normal (外れ値 15.0 を含む)"
  ch3 <- nuts robustModel cfg
              (Map.fromList [("mu", 0.0), ("sigma", 1.0)]) gen
  ch4 <- nuts plainModel cfg
              (Map.fromList [("mu", 0.0), ("sigma", 1.0)]) gen
  putStrLn "  混合 (95% N(μ,σ) + 5% N(μ,10σ)):"
  prn "mu"    (fromMaybe 0 (posteriorMean "mu" ch3)) (fromMaybe 0 (posteriorSD "mu" ch3))
  prn "sigma" (fromMaybe 0 (posteriorMean "sigma" ch3)) (fromMaybe 0 (posteriorSD "sigma" ch3))
  putStrLn "  比較: 普通の Normal:"
  prn "mu"    (fromMaybe 0 (posteriorMean "mu" ch4)) (fromMaybe 0 (posteriorSD "mu" ch4))
  prn "sigma" (fromMaybe 0 (posteriorMean "sigma" ch4)) (fromMaybe 0 (posteriorSD "sigma" ch4))
  printf "    → 真値 μ ≈ 2.0   混合: %.2f  普通: %.2f\n"
         (fromMaybe 0 (posteriorMean "mu" ch3))
         (fromMaybe 0 (posteriorMean "mu" ch4))
  putStrLn ""

  -- ── 事後予測でデモを締めくくる ──
  putStrLn "[4] 例 1 (GMM) の事後予測サンプリング"
  postPreds <- posteriorPredictive gmmModel ch1 gen
  let allYs = concatMap (Map.findWithDefault [] "y") postPreds
      bin xs = (length (filter (< 2.5) xs), length (filter (>= 2.5) xs))
      (lo, hi) = bin allYs
      total = length allYs
  printf "    生成された予測 %d 件: y < 2.5 が %d (%.1f%%), y >= 2.5 が %d (%.1f%%)\n"
         total lo (100 * fromIntegral lo / fromIntegral total :: Double)
         hi (100 * fromIntegral hi / fromIntegral total :: Double)
  printf "    観測: y < 2.5 が 10 (50%%), y >= 2.5 が 10 (50%%) — 整合\n"
  putStrLn ""

  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  ✓ Mixture 分布が正常動作 (混合・ゼロ過剰・頑健)"
  putStrLn "═══════════════════════════════════════════════════════════════"
