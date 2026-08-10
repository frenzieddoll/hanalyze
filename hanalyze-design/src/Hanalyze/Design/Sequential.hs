{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Hanalyze.Design.Sequential
-- Description : 逐次的応答曲面法 (Sequential RSM) — 最急上昇 path と sequential CCD 配置
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: Sequential RSM (逐次的応答曲面法)。
--
-- 「初期 design → fit → steepest ascent path → 新中心 → 次 design」 の逐次
-- 最適化ワークフローを支える helper モジュール。 数値の重い部分は
-- @Hanalyze.Design.RSM@ の @fitQuadratic@ / @optimumPoint@ に委ね、 本モジュール
-- は steepest-ascent path 生成と sequential CCD 配置のみを提供する。
--
-- [English]: Sequential RSM (sequential response surface methodology).
--
-- A helper module supporting the sequential optimization workflow "initial
-- design -> fit -> steepest ascent path -> new center -> next design". The
-- heavy numerical work is delegated to @fitQuadratic@ \/ @optimumPoint@ in
-- @Hanalyze.Design.RSM@; this module only provides steepest-ascent
-- path generation and sequential CCD placement.
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

-- | [日本語]: 最急上昇 / 最急下降 path の結果。
--   [English]: The result of a steepest-ascent \/ steepest-descent path.
data SteepestAscentResult = SteepestAscentResult
  { sarDirection  :: !(LA.Vector Double)
    -- ^ [日本語]: 単位ベクトル化された steepest 方向 (length k)
    --   [English]: The unit-vectorized steepest direction (length k)
  , sarStepPoints :: ![[Double]]
    -- ^ [日本語]: 試行点系列 (length nSteps + 1、 先頭 = center)
    --   [English]: The sequence of trial points (length nSteps + 1; the
    --   first is the center)
  , sarMaximize   :: !Bool
    -- ^ [日本語]: True = ascent、 False = descent
    --   [English]: True = ascent, False = descent
  } deriving (Show)

-- | [日本語]: 第一階係数 @b = [b_1, ..., b_k]@ から steepest ascent path を生成。
--
--   方向ベクトル:
--
--     - @maximize = True@ なら @+b / |b|@
--     - @maximize = False@ なら @-b / |b|@
--
--   path: @[center, center + step·d, center + 2·step·d, ..., center + nSteps·step·d]@
--
--   @|b| = 0@ や @k = 0@ の場合は方向 0、 全 point = center を返す。
--   [English]: Generates a steepest-ascent path from the first-order
--   coefficients @b = [b_1, ..., b_k]@.
--
--   Direction vector:
--
--     - @+b / |b|@ when @maximize = True@
--     - @-b / |b|@ when @maximize = False@
--
--   path: @[center, center + step*d, center + 2*step*d, ..., center + nSteps*step*d]@
--
--   When @|b| = 0@ or @k = 0@, the direction is 0 and every point equals
--   the center.
steepestAscent
  :: Bool          -- ^ [日本語]: True = ascent、 False = descent [English]: True = ascent, False = descent
  -> [Double]      -- ^ [日本語]: center (k 次元) [English]: The center (k-dimensional)
  -> [Double]      -- ^ [日本語]: first-order coefficients @b_1..b_k@ [English]: First-order coefficients @b_1..b_k@
  -> Double        -- ^ [日本語]: step size (原座標スケール、 > 0 推奨) [English]: Step size (in raw-coordinate scale; > 0 recommended)
  -> Int           -- ^ [日本語]: 試行点数 (= path 長 = nSteps + 1) [English]: Number of trial points (= path length = nSteps + 1)
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

-- | [日本語]: 'RSM.QuadFit' から第一階係数を抽出して steepest ascent。
--
--   @QuadFit@ の @qfBeta@ レイアウトは @[b0, β_main, β_sq, β_int]@ なので、
--   主効果 @β_main = b_1..b_k@ を取り出す。
--   [English]: Extracts the first-order coefficients from 'RSM.QuadFit' for
--   steepest ascent.
--
--   Since @QuadFit@'s @qfBeta@ layout is @[b0, β_main, β_sq, β_int]@, the
--   main effects @β_main = b_1..b_k@ are extracted.
steepestAscentFromQuad
  :: Bool                 -- ^ [日本語]: maximize? [English]: maximize?
  -> [Double]             -- ^ [日本語]: center [English]: The center
  -> RSM.QuadFit
  -> Double               -- ^ [日本語]: step size [English]: Step size
  -> Int                  -- ^ [日本語]: nSteps [English]: nSteps
  -> SteepestAscentResult
steepestAscentFromQuad maximize center fit stepSize nSteps =
  let k     = RSM.qfK fit
      beta  = LA.toList (RSM.qfBeta fit)
      bMain = take k (drop 1 beta)
  in steepestAscent maximize center bMain stepSize nSteps

-- ===========================================================================
-- Sequential CCD
-- ===========================================================================

-- | [日本語]: 次の CCD を新しい中心で配置した結果。
--   [English]: The result of placing the next CCD at a new center.
data SequentialCCDResult = SequentialCCDResult
  { sccdCenter :: ![Double]      -- ^ [日本語]: 新しい design center (原座標) [English]: The new design center (raw coordinates)
  , sccdSpan   :: !Double        -- ^ [日本語]: 片側スパン (coded -1 ~ +1 が原座標で center ± span) [English]: The one-sided span (coded -1 to +1 corresponds to center ± span in raw coordinates)
  , sccdCoded  :: ![[Double]]    -- ^ [日本語]: coded units (-α..+α) の design [English]: The design in coded units (-α..+α)
  , sccdReal   :: ![[Double]]    -- ^ [日本語]: 原座標の design (= center + span · coded) [English]: The design in raw coordinates (= center + span * coded)
  } deriving (Show)

-- | [日本語]: 新中心と span で次の CCD を配置。
--
--   内部で @Hanalyze.Design.RSM.centralComposite@ を呼び、 結果を新中心に
--   平行移動 + スケーリングする。 coded units と原座標の両方を返すので、
--   canvas frontend で「coded で fit、 原座標で表示」 が一発で出来る。
--   [English]: Places the next CCD at the new center and span.
--
--   Internally calls @Hanalyze.Design.RSM.centralComposite@ and
--   translates + scales the result to the new center. Returns both coded
--   units and raw coordinates, so the canvas frontend can do "fit in coded,
--   display in raw coordinates" in one step.
sequentialCCD
  :: [Double]            -- ^ [日本語]: 新中心 (k 次元) [English]: The new center (k-dimensional)
  -> Double              -- ^ [日本語]: 片側 span (> 0) [English]: The one-sided span (> 0)
  -> Int                 -- ^ [日本語]: 因子数 k [English]: The number of factors, k
  -> RSM.CCDType         -- ^ [日本語]: CCD 種別 (Circumscribed / Inscribed / FaceCentered) [English]: The CCD kind (Circumscribed \/ Inscribed \/ FaceCentered)
  -> Int                 -- ^ [日本語]: center replications [English]: Center replications
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
