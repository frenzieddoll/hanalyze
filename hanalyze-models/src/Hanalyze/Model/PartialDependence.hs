{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hanalyze.Model.PartialDependence
-- Description : 任意モデル対応の Partial Dependence / ICE 純粋計算エンジン (model 非依存・非ゲート層)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: 部分従属 (Partial Dependence) / ICE の純粋計算エンジン — 任意モデル対応。
--
-- R @pdp::partial@ / sklearn @sklearn.inspection.partial_dependence@ 相当。 学習済モデルの
-- predict を「注目特徴を grid で振り、 他の特徴は訓練データの観測分布のまま」評価し、 全観測
-- 行で平均したものが PDP、 行ごとの曲線が ICE (individual conditional expectation)。
--
-- model 非依存 (predict 閉包のみを受ける) ゆえ __非ゲート層__ に置き、 図化は
-- 別パッケージ @hanalyze-plot@ の 'Hanalyze.Plot.ML'
-- (@cabal build --project-file=cabal.project.plot@ で build) が担う。
--
-- @
-- import Hanalyze.Model.PartialDependence
--
-- -- 任意モデルの predict 閉包を渡す (R pdp の pred.fun 流)。
-- let r = partialDependence trainX (\\m -> map (predictRF rf) (LA.toLists m)) 0 40
-- in  (pdpGrid r, pdpMean r)          -- 特徴 0 の PDP 曲線
-- @
--
-- [English]: A pure computation engine for Partial Dependence / ICE —
-- supports arbitrary models.
--
-- Equivalent to R's @pdp::partial@ \/ sklearn's
-- @sklearn.inspection.partial_dependence@. Evaluates a fitted model's
-- predict by "sweeping the feature of interest over a grid while
-- keeping the other features at their observed distribution in the
-- training data"; the average over all observation rows is the PDP,
-- and the per-row curves are the ICE (individual conditional
-- expectation).
--
-- Since it is model-independent (it only takes a predict closure), it
-- lives in the __non-gated layer__; visualization is handled by
-- 'Hanalyze.Plot.ML' in the separate @hanalyze-plot@
-- package (built via @cabal build --project-file=cabal.project.plot@).
--
-- @
-- import Hanalyze.Model.PartialDependence
--
-- -- Pass an arbitrary model's predict closure (in the style of R
-- -- pdp's pred.fun).
-- let r = partialDependence trainX (\\m -> map (predictRF rf) (LA.toLists m)) 0 40
-- in  (pdpGrid r, pdpMean r)          -- The PDP curve for feature 0
-- @
module Hanalyze.Model.PartialDependence
  ( -- * 結果型
    PDPResult (..)
    -- * 計算
  , partialDependence
  , partialDependenceGrid
    -- * 変換
  , centerICE
  ) where

import qualified Numeric.LinearAlgebra as LA
import           Data.List             (transpose)

-- ===========================================================================
-- 結果型
-- ===========================================================================

-- | [日本語]: 部分従属の計算結果。 grid・PDP 平均曲線・ICE 個体曲線群をまとめて返す。
--   [English]: The result of a partial-dependence computation. Returns
--   the grid, the PDP mean curve, and the group of ICE individual
--   curves together.
data PDPResult = PDPResult
  { pdpGrid :: ![Double]      -- ^ [日本語]: 注目特徴の grid 値 (長さ = grid 数)。 [English]: The grid values for the feature of interest (length = grid count).
  , pdpMean :: ![Double]      -- ^ [日本語]: PDP: 各 grid 値で全観測行の予測を平均 (長さ = grid 数)。 [English]: PDP: the average prediction across all observation rows at each grid value (length = grid count).
  , pdpIce  :: ![[Double]]    -- ^ [日本語]: ICE: 観測行ごとの曲線 (n 本・各長さ = grid 数)。 [English]: ICE: one curve per observation row (n curves, each of length = grid count).
  } deriving (Eq, Show)

-- ===========================================================================
-- 計算
-- ===========================================================================

-- | [日本語]: 注目特徴 j の観測 @[min,max]@ を等間隔 grid にして PDP/ICE を計算する。
--   grid 数 <2 は 2 に切り上げ。 空データ・列外 index は空結果 ('PDPResult' [] [] [])。
--   [English]: Computes PDP\/ICE by turning the observed @[min,max]@ of
--   feature j into an evenly-spaced grid. A grid count <2 is rounded up
--   to 2. Empty data or an out-of-range column index yields an empty
--   result ('PDPResult' [] [] []).
partialDependence
  :: LA.Matrix Double                 -- ^ [日本語]: 訓練特徴行列 X (n 行 × p 列)。 [English]: The training feature matrix X (n rows × p columns).
  -> (LA.Matrix Double -> [Double])   -- ^ [日本語]: predict: 行列の各行 → 予測値 (長さ = 行数)。 [English]: predict: each row of the matrix → a predicted value (length = row count).
  -> Int                              -- ^ [日本語]: 注目特徴の列 index j (0 始まり)。 [English]: The column index j of the feature of interest (0-based).
  -> Int                              -- ^ [日本語]: grid 数。 [English]: The grid count.
  -> PDPResult
partialDependence x predict j n
  | LA.rows x == 0 || j < 0 || j >= LA.cols x = PDPResult [] [] []
  | otherwise =
      let col  = LA.toList (LA.toColumns x !! j)
          lo   = minimum col
          hi   = maximum col
          m    = max 2 n
          grid = [ lo + (hi - lo) * fromIntegral i / fromIntegral (m - 1)
                 | i <- [0 .. m - 1] ]
      in partialDependenceGrid x predict j grid

-- | [日本語]: grid を明示指定する版。 分位点 grid や任意評価点を渡したいときに使う。
--   空 grid・空データ・列外 index は空結果。
--   [English]: The variant that explicitly specifies the grid. Use it
--   when passing a quantile grid or arbitrary evaluation points. An
--   empty grid, empty data, or an out-of-range column index yields an
--   empty result.
partialDependenceGrid
  :: LA.Matrix Double
  -> (LA.Matrix Double -> [Double])
  -> Int
  -> [Double]                         -- ^ [日本語]: 注目特徴の評価 grid。 [English]: The evaluation grid for the feature of interest.
  -> PDPResult
partialDependenceGrid x predict j grid
  | LA.rows x == 0 || j < 0 || j >= LA.cols x || null grid = PDPResult [] [] []
  | otherwise =
      let nrows  = LA.rows x
          cols   = LA.toColumns x
          -- 各 grid 値 g で X の j 列を定数 g に置換 → 全行 predict (長さ nrows)。
          predsAtG g =
            let xg = LA.fromColumns
                       [ if c == j then LA.konst g nrows else col
                       | (c, col) <- zip [0 ..] cols ]
            in predict xg
          byGrid = [ predsAtG g | g <- grid ]              -- grid × n
          means  = [ sum ps / fromIntegral nrows | ps <- byGrid ]
          ice    = transpose byGrid                        -- n × grid (行ごとの曲線)
      in PDPResult grid means ice

-- ===========================================================================
-- 変換
-- ===========================================================================

-- | [日本語]: 中心化 ICE (c-ICE)。 各 ICE 曲線を __左端 (grid[0]) の値が 0__ に
--   なるよう平行移動し、 PDP 平均も中心化後の ICE から取り直す。 個体間の傾き差を
--   見やすくする (sklearn @centered=True@ / R @ice()@ centered 相当)。 空結果はそのまま。
--   [English]: Centered ICE (c-ICE). Shifts each ICE curve so that
--   __the value at the left end (grid[0]) is 0__, and re-derives the
--   PDP mean from the centered ICE. Makes differences in slope between
--   individuals easier to see (equivalent to sklearn's
--   @centered=True@ \/ R's @ice()@ centered). An empty result passes
--   through unchanged.
centerICE :: PDPResult -> PDPResult
centerICE r
  | null (pdpGrid r) || null (pdpIce r) = r
  | otherwise =
      let ice'   = [ case curve of
                       (c0 : _) -> map (subtract c0) curve
                       []       -> curve
                   | curve <- pdpIce r ]
          nrows  = length ice'
          means' = case ice' of
                     [] -> []
                     _  -> map (\col -> sum col / fromIntegral nrows) (transpose ice')
      in r { pdpMean = means', pdpIce = ice' }
