{-# LANGUAGE OverloadedStrings #-}
-- | 検出力解析 (Power analysis): サンプルサイズ決定、検出力計算。
--
-- 主要関数:
-- - 'powerTTest':         t 検定の検出力 (二群)
-- - 'sampleSizeTTest':    指定検出力に必要な n
-- - 'powerOneWayAnova':   F 検定 (一元配置 ANOVA) の検出力
-- - 'powerProportion':    比率検定 (二群) の検出力
module Design.Power
  ( -- * t 検定
    powerTTest
  , sampleSizeTTest
    -- * F 検定 (ANOVA)
  , powerOneWayAnova
  , sampleSizeOneWayAnova
    -- * 比率検定
  , powerProportion
    -- * 効果量の指標
  , cohensD
  , cohensF
  ) where

import qualified Statistics.Distribution as SD
import qualified Statistics.Distribution.StudentT as ST
import qualified Statistics.Distribution.Normal as NormalD
import qualified Statistics.Distribution.FDistribution as FD

-- ---------------------------------------------------------------------------
-- 効果量
-- ---------------------------------------------------------------------------

-- | Cohen's d: 二群の標準化差。
--   d = (μ_1 − μ_2) / σ_pooled
--   解釈: 0.2 = small, 0.5 = medium, 0.8 = large
cohensD :: Double -> Double -> Double -> Double
cohensD mu1 mu2 sigma = (mu1 - mu2) / sigma

-- | Cohen's f: 一元配置 ANOVA の効果量。
--   f = σ_means / σ_within
--   解釈: 0.10 = small, 0.25 = medium, 0.40 = large
cohensF :: [Double]    -- 群平均
        -> Double      -- σ_within (= √MSE)
        -> Double
cohensF means sigma =
  let k    = length means
      gm   = sum means / fromIntegral k
      var  = sum [(m - gm)^(2::Int) | m <- means] / fromIntegral k
  in sqrt var / sigma

-- ---------------------------------------------------------------------------
-- t 検定
-- ---------------------------------------------------------------------------

-- | 二群独立 t 検定の検出力 (両側、等分散仮定)。
--
-- 引数:
--   * @d@      — Cohen's d (効果量)
--   * @n1, n2@ — 各群のサンプルサイズ
--   * @alpha@  — 有意水準 (例 0.05)
powerTTest :: Double -> Int -> Int -> Double -> Double
powerTTest d n1 n2 alpha =
  let df  = n1 + n2 - 2
      ncp = d * sqrt (fromIntegral n1 * fromIntegral n2
                      / fromIntegral (n1 + n2))
      tCrit = SD.quantile (ST.studentT (fromIntegral df))
                          (1 - alpha / 2)
      -- 非心 t 分布の代わりに正規近似 (df 大なら良好)
      sigma = 1.0  -- t 分布近似なら sd ≈ 1
      pUpper = 1 - SD.cumulative (NormalD.normalDistr ncp sigma) tCrit
      pLower = SD.cumulative (NormalD.normalDistr ncp sigma) (-tCrit)
  in pUpper + pLower

-- | 指定検出力に必要なサンプルサイズ (各群同数を仮定)。
--
-- 二分探索で最小の n を探す。
sampleSizeTTest :: Double  -- d
                -> Double  -- target power
                -> Double  -- alpha
                -> Int
sampleSizeTTest d targetPow alpha = search 2 1000
  where
    search lo hi
      | lo >= hi = hi
      | otherwise =
          let mid = (lo + hi) `div` 2
              p   = powerTTest d mid mid alpha
          in if p >= targetPow then search lo mid else search (mid + 1) hi

-- ---------------------------------------------------------------------------
-- 一元配置 ANOVA の F 検定
-- ---------------------------------------------------------------------------

-- | 一元配置 ANOVA の検出力。
--
-- 引数:
--   * @f@        — Cohen's f (効果量)
--   * @k@        — 群数
--   * @n@        — 群あたりサンプル数 (等数仮定)
--   * @alpha@    — 有意水準
powerOneWayAnova :: Double -> Int -> Int -> Double -> Double
powerOneWayAnova f k n alpha =
  let dfBetween = k - 1
      dfWithin  = k * (n - 1)
      ncp       = f * f * fromIntegral (k * n)
      fCrit     = SD.quantile (FD.fDistribution dfBetween dfWithin)
                              (1 - alpha)
      -- 非心 F 分布 ≈ scaled F で近似
      mean1     = fromIntegral dfBetween + ncp
      var1      = 2 * mean1   -- chi² 近似
      -- 標準正規近似:
      z         = (fCrit * fromIntegral dfBetween - mean1) / sqrt var1
  in 1 - SD.cumulative (NormalD.normalDistr 0 1) z

sampleSizeOneWayAnova :: Double -> Int -> Double -> Double -> Int
sampleSizeOneWayAnova f k targetPow alpha = search 2 1000
  where
    search lo hi
      | lo >= hi = hi
      | otherwise =
          let mid = (lo + hi) `div` 2
              p   = powerOneWayAnova f k mid alpha
          in if p >= targetPow then search lo mid else search (mid + 1) hi

-- ---------------------------------------------------------------------------
-- 比率検定 (二群)
-- ---------------------------------------------------------------------------

-- | 二群比率検定 (z-test) の検出力 (両側)。
--
-- 引数: 真の比率 p1, p2、各群サンプル n1, n2、α。
powerProportion :: Double -> Double -> Int -> Int -> Double -> Double
powerProportion p1 p2 n1 n2 alpha =
  let n1d = fromIntegral n1; n2d = fromIntegral n2
      pP  = (n1d * p1 + n2d * p2) / (n1d + n2d)
      seH0 = sqrt (pP * (1 - pP) * (1/n1d + 1/n2d))
      seH1 = sqrt (p1 * (1 - p1) / n1d + p2 * (1 - p2) / n2d)
      delta = abs (p1 - p2)
      zAlpha = SD.quantile (NormalD.normalDistr 0 1) (1 - alpha / 2)
      crit = zAlpha * seH0
      z = (delta - crit) / seH1
  in SD.cumulative (NormalD.normalDistr 0 1) z
