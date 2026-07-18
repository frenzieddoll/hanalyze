{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Hanalyze.Design.Sequential
-- Description : 逐次的応答曲面法 (Sequential RSM) — 最急上昇 path と sequential CCD 配置
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Sequential RSM (逐次的応答曲面法)。
--
-- 「初期 design → fit → steepest ascent path → 新中心 → 次 design」 の逐次
-- 最適化ワークフローを支える helper モジュール。 数値の重い部分は
-- @Hanalyze.Design.RSM@ の `fitQuadratic` / `optimumPoint` に委ね、 本モジュール
-- は steepest-ascent path 生成と sequential CCD 配置のみを提供する。
module Hanalyze.Design.Sequential
  ( -- * Steepest Ascent
    SteepestAscentResult (..)
  , steepestAscent
  , steepestAscentFromQuad
    -- * Sequential CCD
  , SequentialCCDResult (..)
  , sequentialCCD
  ) where

import qualified Numeric.LinearAlgebra as LA

import qualified Hanalyze.Design.RSM   as RSM

-- ===========================================================================
-- Steepest Ascent
-- ===========================================================================

-- | 最急上昇 / 最急下降 path の結果。
data SteepestAscentResult = SteepestAscentResult
  { sarDirection  :: !(LA.Vector Double)
    -- ^ 単位ベクトル化された steepest 方向 (length k)
  , sarStepPoints :: ![[Double]]
    -- ^ 試行点系列 (length nSteps + 1、 先頭 = center)
  , sarMaximize   :: !Bool
    -- ^ True = ascent、 False = descent
  } deriving (Show)

-- | 第一階係数 @b = [b_1, ..., b_k]@ から steepest ascent path を生成。
--
-- 方向ベクトル:
--
--   * @maximize = True@ なら @+b / |b|@
--   * @maximize = False@ なら @-b / |b|@
--
-- path: @[center, center + step·d, center + 2·step·d, ..., center + nSteps·step·d]@
--
-- @|b| = 0@ や @k = 0@ の場合は方向 0、 全 point = center を返す。
steepestAscent
  :: Bool          -- ^ True = ascent、 False = descent
  -> [Double]      -- ^ center (k 次元)
  -> [Double]      -- ^ first-order coefficients @b_1..b_k@
  -> Double        -- ^ step size (原座標スケール、 > 0 推奨)
  -> Int           -- ^ 試行点数 (= path 長 = nSteps + 1)
  -> SteepestAscentResult
steepestAscent maximize center bCoefs stepSize nSteps =
  let k       = length center
      bVec    = LA.fromList bCoefs
      cVec    = LA.fromList center
      normB   = sqrt (LA.sumElements (bVec * bVec))
      dirRaw  = if normB > 0
                  then LA.scale ((if maximize then 1 else -1) / normB) bVec
                  else LA.fromList (replicate k 0)
      points  = [ LA.toList (cVec + LA.scale (fromIntegral i * stepSize) dirRaw)
                | i <- [0 .. max 0 nSteps]
                ]
  in SteepestAscentResult
       { sarDirection  = dirRaw
       , sarStepPoints = points
       , sarMaximize   = maximize
       }

-- | 'RSM.QuadFit' から第一階係数を抽出して steepest ascent。
--
-- `QuadFit` の `qfBeta` レイアウトは @[b0, β_main, β_sq, β_int]@ なので、
-- 主効果 @β_main = b_1..b_k@ を取り出す。
steepestAscentFromQuad
  :: Bool                 -- ^ maximize?
  -> [Double]             -- ^ center
  -> RSM.QuadFit
  -> Double               -- ^ step size
  -> Int                  -- ^ nSteps
  -> SteepestAscentResult
steepestAscentFromQuad maximize center fit stepSize nSteps =
  let k     = RSM.qfK fit
      beta  = LA.toList (RSM.qfBeta fit)
      bMain = take k (drop 1 beta)
  in steepestAscent maximize center bMain stepSize nSteps

-- ===========================================================================
-- Sequential CCD
-- ===========================================================================

-- | 次の CCD を新しい中心で配置した結果。
data SequentialCCDResult = SequentialCCDResult
  { sccdCenter :: ![Double]      -- ^ 新しい design center (原座標)
  , sccdSpan   :: !Double        -- ^ 片側スパン (coded -1 ~ +1 が原座標で center ± span)
  , sccdCoded  :: ![[Double]]    -- ^ coded units (-α..+α) の design
  , sccdReal   :: ![[Double]]    -- ^ 原座標の design (= center + span · coded)
  } deriving (Show)

-- | 新中心と span で次の CCD を配置。
--
-- 内部で @Hanalyze.Design.RSM.centralComposite@ を呼び、 結果を新中心に
-- 平行移動 + スケーリングする。 coded units と原座標の両方を返すので、
-- canvas frontend で「coded で fit、 原座標で表示」 が一発で出来る。
sequentialCCD
  :: [Double]            -- ^ 新中心 (k 次元)
  -> Double              -- ^ 片側 span (> 0)
  -> Int                 -- ^ 因子数 k
  -> RSM.CCDType         -- ^ CCD 種別 (Circumscribed / Inscribed / FaceCentered)
  -> Int                 -- ^ center replications
  -> SequentialCCDResult
sequentialCCD center span_ k ccdT centerReps =
  let coded = RSM.centralComposite k ccdT centerReps
      real_ = [ zipWith (\c x -> c + span_ * x) center row | row <- coded ]
  in SequentialCCDResult
       { sccdCenter = center
       , sccdSpan   = span_
       , sccdCoded  = coded
       , sccdReal   = real_
       }
