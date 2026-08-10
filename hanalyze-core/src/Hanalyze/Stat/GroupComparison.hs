{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Hanalyze.Stat.GroupComparison
-- Description : 2 群間の多変量比較ランキング (Spotfire 風 "Good vs Bad")
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: 2 群間の多変量比較ランキング (Spotfire 風 "Good vs Bad")。
--
-- 「良品 vs 不良品」 を二値ラベルで分け、 各説明変数について
-- (i) 平均差、 (ii) Cohen's d 効果量、 (iii) Welch t-test p 値 を計算し、
-- 効果量の絶対値降順にランク付けして返す。 半導体品質解析等で頻出。
--
-- 単独検定ではなく __複数変数の並列比較に最適化__ された helper。
-- 多重比較補正は呼び出し側で `Hanalyze.Stat.MultipleTesting` を使う。
--
-- [English]: Multivariate group-comparison ranking between 2 groups
-- (Spotfire-style "Good vs Bad").
--
-- Splits observations into "good" vs "defective" via a binary label, and
-- for each explanatory variable computes (i) the mean difference, (ii)
-- Cohen's d effect size, and (iii) Welch's t-test p-value, returning them
-- ranked in descending order of absolute effect size. Common in
-- semiconductor quality analysis and similar domains.
--
-- A helper __optimized for comparing multiple variables in parallel__,
-- rather than a single test. Multiple-comparison correction is left to
-- the caller via `Hanalyze.Stat.MultipleTesting`.
module Hanalyze.Stat.GroupComparison
  ( -- * 結果型
    GroupCompResult (..)
    -- * 比較
  , goodVsBad
  ) where

import qualified Data.Vector           as V
import qualified Numeric.LinearAlgebra as LA
import           Data.List             (sortBy)
import           Data.Ord              (comparing, Down (..))
import           Data.Text             (Text)
import           Data.Vector           (Vector)

import qualified Hanalyze.Stat.Test    as ST
import qualified Hanalyze.Stat.Effect  as Eff

-- ===========================================================================
-- 型
-- ===========================================================================

-- | [日本語]: 1 変数の Good vs Bad 比較結果。
--   [English]: Good vs Bad comparison result for a single variable.
data GroupCompResult = GroupCompResult
  { gcrVarName  :: !Text     -- ^ [日本語]: 変数名 [English]: Variable name
  , gcrMeanG    :: !Double   -- ^ [日本語]: Good 群 (label = True) の平均 [English]: Mean of the Good group (label = True)
  , gcrMeanB    :: !Double   -- ^ [日本語]: Bad  群 (label = False) の平均 [English]: Mean of the Bad group (label = False)
  , gcrMeanDiff :: !Double   -- ^ Mean(Bad) − Mean(Good)
  , gcrEffect   :: !Double   -- ^ [日本語]: Cohen's d (signed; |gcrEffect| でランク) [English]: Cohen's d (signed; ranked by |gcrEffect|)
  , gcrPValue   :: !Double   -- ^ [日本語]: Welch's two-sided t-test の p 値 [English]: p-value from Welch's two-sided t-test
  , gcrNG       :: !Int      -- ^ [日本語]: Good 群サイズ [English]: Good group size
  , gcrNB       :: !Int      -- ^ [日本語]: Bad  群サイズ [English]: Bad group size
  } deriving (Show, Eq)

-- ===========================================================================
-- 公開関数
-- ===========================================================================

-- | [日本語]: 各説明変数について 2 群間の差を計算し、 効果量絶対値降順でランク付け。
--
-- 入力契約:
--
--   * 変数リストは非空 (1 変数以上)
--   * 各変数の Vector 長 = labels の長さ (一致しないと 'Left')
--   * 両群とも 2 個以上の観測必須 (Welch t-test の前提)
--
-- [English]: For each explanatory variable, compute the difference
-- between the 2 groups and rank in descending order of absolute effect
-- size.
--
-- Input contract:
--
--   * The variable list is non-empty (1 or more variables)
--   * Each variable's Vector length equals the length of labels (a
--     mismatch returns 'Left')
--   * Both groups must have 2 or more observations (a prerequisite for
--     Welch's t-test)
goodVsBad
  :: [(Text, Vector Double)]   -- ^ [日本語]: (変数名, 値ベクトル) のリスト [English]: List of (variable name, value vector)
  -> Vector Bool               -- ^ [日本語]: 群ラベル (True = Good、 False = Bad) [English]: Group label (True = Good, False = Bad)
  -> Either Text [GroupCompResult]
goodVsBad vars labels
  | null vars               = Left "goodVsBad: empty variable list"
  | V.null labels           = Left "goodVsBad: empty labels"
  | any (\(_, v) -> V.length v /= V.length labels) vars
                            = Left "goodVsBad: variable length mismatch with labels"
  | nG < 2 || nB < 2        = Left "goodVsBad: each group needs at least 2 observations"
  | otherwise =
      let results = map (compareOne labels) vars
      in Right (sortBy (comparing (Down . absEffect)) results)
  where
    nG = V.length (V.filter id labels)
    nB = V.length labels - nG
    absEffect = abs . gcrEffect

-- ---------------------------------------------------------------------------
-- 1 変数の比較
-- ---------------------------------------------------------------------------

compareOne :: Vector Bool -> (Text, Vector Double) -> GroupCompResult
compareOne labels (name, vals) =
  let (goodList, badList) = partitionByLabels labels vals
      gVec = LA.fromList goodList
      bVec = LA.fromList badList
      tr   = ST.tTestWelch gVec bVec ST.TwoSided
      pVal = ST.trPValue tr
      d    = Eff.cohenD bVec gVec   -- Mean(Bad) − Mean(Good) 方向
      mG   = mean goodList
      mB   = mean badList
  in GroupCompResult
       { gcrVarName  = name
       , gcrMeanG    = mG
       , gcrMeanB    = mB
       , gcrMeanDiff = mB - mG
       , gcrEffect   = d
       , gcrPValue   = pVal
       , gcrNG       = length goodList
       , gcrNB       = length badList
       }

-- | [日本語]: label が True の要素を good、 False を bad として分割。
--   [English]: Split elements into good (label = True) and bad (label =
--   False).
partitionByLabels :: Vector Bool -> Vector Double -> ([Double], [Double])
partitionByLabels labels vals = go 0 ([], [])
  where
    n = V.length vals
    go !i (gs, bs)
      | i >= n = (reverse gs, reverse bs)
      | otherwise =
          let v = vals V.! i
              l = labels V.! i
          in if l then go (i + 1) (v : gs, bs)
                  else go (i + 1) (gs, v : bs)

mean :: [Double] -> Double
mean [] = 0
mean xs = sum xs / fromIntegral (length xs)
