{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Hanalyze.Stat.Descriptive
-- Description : 一次元記述統計 (mean/quantile/variance 等) の単一の正 (single source of truth)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- 一次元の記述統計 (descriptive statistics) の公開 API。
--
-- hanalyze の記述統計の **単一の正 (single source of truth)**。 従来は
-- @mean@ / @median@ / @quantile@ / @variance@ が 'Stat.GroupComparison' /
-- 'Stat.ModelSelect' / 'Stat.Effect' / 'Model.Quantile' / 'Stat.Bootstrap' 等に
-- 私的 helper として散在 (シグネチャ @[Double]@ / @[Int]@ / @LA.Vector@ 混在・
-- ほぼ未 export) していたのを、 ここに集約する (Phase 65)。
--
-- === 正準型 = 'Data.Vector.Generic.Vector' v Double
-- 'statistics' パッケージ自身と同じく @G.Vector v Double@ で多相。 これにより
-- Storable (= hmatrix @LA.Vector@)・Unboxed・boxed (@V.Vector@・DataFrame 列) の
-- いずれも **ゼロ変換**で渡せる (速度経路は list 化を挟まない)。 素の @[Double]@
-- 利用には末尾の @*L@ wrapper を用意する。
--
-- === 実装方針
-- @mean@ / @variance@ (n-1) / @sd@ は 'Statistics.Sample' を再利用。 @quantile@ は
-- R 既定の **type-7** (線形補間) を自前実装し R 一致を保証する (@median@ / @iqr@ /
-- @percentile@ はこれを呼ぶ)。 ソートは 'Data.Vector.Algorithms.Intro'。
--
-- === NA
-- 本モジュールは NA を扱わない (total・純粋)。 R の @na.rm = TRUE@ 相当は呼び手が
-- @mapMaybe id@ で除去してから 'Data.Vector.Generic.fromList' する。
module Hanalyze.Stat.Descriptive
  ( -- * 中心
    mean, median
    -- * 位置
  , quantile, percentile, minimum', maximum'
    -- * 散布
  , variance, sd, iqr, range'
    -- * [Double] 便宜 wrapper
  , meanL, medianL, quantileL, sdL, varianceL, iqrL
  ) where

import qualified Data.Vector.Generic            as G
import qualified Data.Vector.Storable           as VS
import qualified Data.Vector.Algorithms.Intro   as Intro
import qualified Statistics.Sample              as S

-- ===========================================================================
-- 中心
-- ===========================================================================

-- | 算術平均。 空なら NaN (R @mean(numeric(0))@)。
mean :: G.Vector v Double => v Double -> Double
mean v | G.null v  = nan
       | otherwise = S.mean v
{-# INLINE mean #-}

-- | 中央値 (= type-7 の 0.5 分位点・偶数長は中央 2 点の平均)。
median :: G.Vector v Double => v Double -> Double
median = quantile 0.5
{-# INLINE median #-}

-- ===========================================================================
-- 位置 (分位点は R 既定 type-7)
-- ===========================================================================

-- | R 既定 (type-7) の分位点。 確率を第 1 引数に取る (@quantile 0.95 v@)。
--
-- ソート済 0-index 列 @x[0..n-1]@・@h = (n-1) p@ として
-- @x[⌊h⌋] + (h - ⌊h⌋)(x[⌊h⌋+1] - x[⌊h⌋])@。 空なら NaN。
quantile :: G.Vector v Double => Double -> v Double -> Double
quantile p v
  | n == 0    = nan
  | n == 1    = G.head v
  | otherwise =
      let sorted = G.modify Intro.sort v
          h      = fromIntegral (n - 1) * p
          lo     = floor h
          lo'    = max 0 (min (n - 1) lo)
          hi'    = min (n - 1) (lo' + 1)
          frac   = h - fromIntegral lo'
          xlo    = G.unsafeIndex sorted lo'
          xhi    = G.unsafeIndex sorted hi'
      in xlo + frac * (xhi - xlo)
  where n = G.length v

-- | パーセンタイル (= @quantile (p/100)@)。
percentile :: G.Vector v Double => Double -> v Double -> Double
percentile p = quantile (p / 100)
{-# INLINE percentile #-}

-- | 最小値 (空なら NaN)。
minimum' :: G.Vector v Double => v Double -> Double
minimum' v | G.null v  = nan
           | otherwise = G.minimum v
{-# INLINE minimum' #-}

-- | 最大値 (空なら NaN)。
maximum' :: G.Vector v Double => v Double -> Double
maximum' v | G.null v  = nan
           | otherwise = G.maximum v
{-# INLINE maximum' #-}

-- ===========================================================================
-- 散布
-- ===========================================================================

-- | 標本分散 (n-1 で割る・R @var()@)。 n<2 なら NaN。
variance :: G.Vector v Double => v Double -> Double
variance v | G.length v < 2 = nan
           | otherwise       = S.varianceUnbiased v
{-# INLINE variance #-}

-- | 標準偏差 (= sqrt . variance・R @sd()@)。
sd :: G.Vector v Double => v Double -> Double
sd v | G.length v < 2 = nan
     | otherwise       = S.stdDev v
{-# INLINE sd #-}

-- | 四分位範囲 (= type-7 の 0.75 分位点 - 0.25 分位点・R @IQR()@)。
iqr :: G.Vector v Double => v Double -> Double
iqr v = quantile 0.75 v - quantile 0.25 v
{-# INLINE iqr #-}

-- | 範囲 (= 最大 - 最小)。
range' :: G.Vector v Double => v Double -> Double
range' v = maximum' v - minimum' v
{-# INLINE range' #-}

-- ===========================================================================
-- [Double] 便宜 wrapper (= f . VS.fromList)
-- ===========================================================================

meanL     :: [Double] -> Double
meanL      = mean     . VS.fromList
medianL   :: [Double] -> Double
medianL    = median   . VS.fromList
quantileL :: Double -> [Double] -> Double
quantileL p = quantile p . VS.fromList
sdL       :: [Double] -> Double
sdL        = sd       . VS.fromList
varianceL :: [Double] -> Double
varianceL  = variance . VS.fromList
iqrL      :: [Double] -> Double
iqrL       = iqr      . VS.fromList

-- ===========================================================================

nan :: Double
nan = 0 / 0
