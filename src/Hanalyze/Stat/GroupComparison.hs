{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Hanalyze.Stat.GroupComparison
-- Description : 2 群間の多変量比較ランキング (Spotfire 風 "Good vs Bad")
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- 2 群間の多変量比較ランキング (Spotfire 風 "Good vs Bad")。
--
-- 「良品 vs 不良品」 を二値ラベルで分け、 各説明変数について
-- (i) 平均差、 (ii) Cohen's d 効果量、 (iii) Welch t-test p 値 を計算し、
-- 効果量の絶対値降順にランク付けして返す。 半導体品質解析等で頻出。
--
-- 単独検定ではなく **複数変数の並列比較に最適化**された helper。
-- 多重比較補正は呼び出し側で `Hanalyze.Stat.MultipleTesting` を使う。
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

-- | 1 変数の Good vs Bad 比較結果。
data GroupCompResult = GroupCompResult
  { gcrVarName  :: !Text     -- ^ 変数名
  , gcrMeanG    :: !Double   -- ^ Good 群 (label = True) の平均
  , gcrMeanB    :: !Double   -- ^ Bad  群 (label = False) の平均
  , gcrMeanDiff :: !Double   -- ^ Mean(Bad) − Mean(Good)
  , gcrEffect   :: !Double   -- ^ Cohen's d (signed; |gcrEffect| でランク)
  , gcrPValue   :: !Double   -- ^ Welch's two-sided t-test の p 値
  , gcrNG       :: !Int      -- ^ Good 群サイズ
  , gcrNB       :: !Int      -- ^ Bad  群サイズ
  } deriving (Show, Eq)

-- ===========================================================================
-- 公開関数
-- ===========================================================================

-- | 各説明変数について 2 群間の差を計算し、 効果量絶対値降順でランク付け。
--
-- 入力契約:
--
--   * 変数リストは非空 (1 変数以上)
--   * 各変数の Vector 長 = labels の長さ (一致しないと 'Left')
--   * 両群とも 2 個以上の観測必須 (Welch t-test の前提)
goodVsBad
  :: [(Text, Vector Double)]   -- ^ (変数名, 値ベクトル) のリスト
  -> Vector Bool               -- ^ 群ラベル (True = Good、 False = Bad)
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

-- | label が True の要素を good、 False を bad として分割。
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
