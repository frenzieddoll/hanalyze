{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hanalyze.Model.PartialDependence
-- Description : 任意モデル対応の Partial Dependence / ICE 純粋計算エンジン (model 非依存・非ゲート層)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- 部分従属 (Partial Dependence) / ICE の純粋計算エンジン — 任意モデル対応 (Phase 75.27)。
--
-- R @pdp::partial@ / sklearn @sklearn.inspection.partial_dependence@ 相当。 学習済モデルの
-- predict を「注目特徴を grid で振り、 他の特徴は訓練データの観測分布のまま」評価し、 全観測
-- 行で平均したものが PDP、 行ごとの曲線が ICE (individual conditional expectation)。
--
-- model 非依存 (predict 閉包のみを受ける) ゆえ **非ゲート層** に置き、 図化は
-- 'Hanalyze.Plot.ML' がゲート (@plot-integration@) 配下で担う。
--
-- @
-- import Hanalyze.Model.PartialDependence
--
-- -- 任意モデルの predict 閉包を渡す (R pdp の pred.fun 流)。
-- let r = partialDependence trainX (\\m -> map (predictRF rf) (LA.toLists m)) 0 40
-- in  (pdpGrid r, pdpMean r)          -- 特徴 0 の PDP 曲線
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

-- | 部分従属の計算結果。 grid・PDP 平均曲線・ICE 個体曲線群をまとめて返す。
data PDPResult = PDPResult
  { pdpGrid :: ![Double]      -- ^ 注目特徴の grid 値 (長さ = grid 数)。
  , pdpMean :: ![Double]      -- ^ PDP: 各 grid 値で全観測行の予測を平均 (長さ = grid 数)。
  , pdpIce  :: ![[Double]]    -- ^ ICE: 観測行ごとの曲線 (n 本・各長さ = grid 数)。
  } deriving (Eq, Show)

-- ===========================================================================
-- 計算
-- ===========================================================================

-- | 注目特徴 j の観測 @[min,max]@ を等間隔 grid にして PDP/ICE を計算する。
--   grid 数 <2 は 2 に切り上げ。 空データ・列外 index は空結果 ('PDPResult' [] [] [])。
partialDependence
  :: LA.Matrix Double                 -- ^ 訓練特徴行列 X (n 行 × p 列)。
  -> (LA.Matrix Double -> [Double])   -- ^ predict: 行列の各行 → 予測値 (長さ = 行数)。
  -> Int                              -- ^ 注目特徴の列 index j (0 始まり)。
  -> Int                              -- ^ grid 数。
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

-- | grid を明示指定する版。 分位点 grid や任意評価点を渡したいときに使う。
--   空 grid・空データ・列外 index は空結果。
partialDependenceGrid
  :: LA.Matrix Double
  -> (LA.Matrix Double -> [Double])
  -> Int
  -> [Double]                         -- ^ 注目特徴の評価 grid。
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

-- | 中心化 ICE (c-ICE)。 各 ICE 曲線を **左端 (grid[0]) の値が 0** になるよう平行移動し、
--   PDP 平均も中心化後の ICE から取り直す。 個体間の傾き差を見やすくする
--   (sklearn @centered=True@ / R @ice()@ centered 相当)。 空結果はそのまま。
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
