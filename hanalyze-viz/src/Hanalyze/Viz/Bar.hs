-- |
-- Module      : Hanalyze.Viz.Bar
-- Description : 棒グラフ (縦・横・積み上げ・グループ化) の可視化
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
{-# LANGUAGE OverloadedStrings #-}
-- | Bar-chart visualizations.
--
--   * 'barChart'      — vertical bar chart by category.
--   * 'barChartH'     — horizontal bar chart (handy for long labels).
--   * 'stackedBar'    — stacked bar chart.
--   * 'groupedBar'    — grouped bar chart.
--   * 'barChartFile'  — write to HTML / PNG / SVG.
module Hanalyze.Viz.Bar
  ( barChart
  , barChartH
  , stackedBar
  , groupedBar
  , barChartFile
    -- * 130: PlotData ベースの汎用 spec API
  , barSpec
  ) where

import Data.Text (Text)
import qualified Data.Vector as V
import Graphics.Vega.VegaLite

import Hanalyze.Viz.Core     (PlotConfig (..), OutputFormat, writeSpec)
import Hanalyze.Viz.PlotData (PlotData, numericColumn, textColumn)

-- ---------------------------------------------------------------------------
-- 縦棒グラフ (カテゴリ → 数値)
-- ---------------------------------------------------------------------------

-- | A simple vertical bar chart.
--
-- @
-- barChart cfg "Month" "Sales"
--   ["Jan","Feb","Mar"] [120,95,140]
-- @
barChart :: PlotConfig
         -> Text     -- ^ X-axis label.
         -> Text     -- ^ Y-axis label.
         -> [Text]   -- ^ Categories.
         -> [Double] -- ^ Per-category values.
         -> VegaLite
barChart cfg xLabel yLabel cats vals =
  toVegaLite
    [ title (plotTitle cfg) []
    , dataFromColumns []
        . dataColumn xLabel (Strings cats)
        . dataColumn yLabel (Numbers vals)
        $ []
    , mark Bar [MColor "#4C72B0", MOpacity 0.85]
    , encoding
        . position X [ PName xLabel, PmType Nominal
                     , PAxis [AxTitle xLabel, AxLabelAngle (-30)]
                     , PSort [] ]
        . position Y [ PName yLabel, PmType Quantitative
                     , PAxis [AxTitle yLabel] ]
        $ []
    , widthStep 40
    , height (plotHeight cfg)
    ]

-- ---------------------------------------------------------------------------
-- 水平棒グラフ
-- ---------------------------------------------------------------------------

-- | A horizontal bar chart. Best when labels are long or for ranking
-- displays.
--
-- @
-- barChartH cfg "Country" "GDP" countries gdps
-- @
barChartH :: PlotConfig
          -> Text     -- ^ Y-axis (category) label.
          -> Text     -- ^ X-axis (value) label.
          -> [Text]   -- ^ Categories.
          -> [Double] -- ^ Per-category values.
          -> VegaLite
barChartH cfg yLabel xLabel cats vals =
  toVegaLite
    [ title (plotTitle cfg) []
    , dataFromColumns []
        . dataColumn yLabel (Strings cats)
        . dataColumn xLabel (Numbers vals)
        $ []
    , mark Bar [MColor "#4C72B0", MOpacity 0.85]
    , encoding
        . position Y [ PName yLabel, PmType Nominal
                     , PAxis [AxTitle yLabel]
                     , PSort [Descending] ]
        . position X [ PName xLabel, PmType Quantitative
                     , PAxis [AxTitle xLabel] ]
        $ []
    , width (plotWidth cfg)
    , heightStep 24
    ]

-- ---------------------------------------------------------------------------
-- 積み上げ棒グラフ
-- ---------------------------------------------------------------------------

-- | Stacked bar chart: each category shows its breakdown by group.
--
-- @
-- stackedBar cfg "Quarter" "Revenue" "Product"
--   ["Q1","Q1","Q1","Q2","Q2","Q2"]  -- x 軸カテゴリ (繰り返しOK)
--   [100, 80, 60, 120, 90, 70]       -- 値
--   ["A",  "B", "C", "A", "B", "C"] -- 色分けグループ
-- @
stackedBar :: PlotConfig -> Text -> Text -> Text
           -> [Text] -> [Double] -> [Text]
           -> VegaLite
stackedBar cfg xLabel yLabel colorLabel xCats vals colorCats =
  toVegaLite
    [ title (plotTitle cfg) []
    , dataFromColumns []
        . dataColumn xLabel    (Strings xCats)
        . dataColumn yLabel    (Numbers vals)
        . dataColumn colorLabel (Strings colorCats)
        $ []
    , mark Bar []
    , encoding
        . position X [ PName xLabel, PmType Nominal
                     , PAxis [AxTitle xLabel, AxLabelAngle (-30)]
                     , PSort [] ]
        . position Y [ PName yLabel, PmType Quantitative
                     , PAxis [AxTitle yLabel]
                     , PStack StZero ]
        . color [ MName colorLabel, MmType Nominal
                , MScale [SScheme "tableau10" []] ]
        $ []
    , widthStep 50
    , height (plotHeight cfg)
    ]

-- ---------------------------------------------------------------------------
-- グループ別棒グラフ
-- ---------------------------------------------------------------------------

-- | Grouped bar chart (side-by-side comparison).
--
-- @
-- groupedBar cfg "Method" "ESS" "Case"
--   ["MH","HMC","NUTS","MH","HMC","NUTS"]  -- x 軸
--   [120, 900, 1800, 80, 1200, 1900]       -- 値
--   ["Easy","Easy","Easy","Hard","Hard","Hard"]  -- グループ
-- @
groupedBar :: PlotConfig -> Text -> Text -> Text
           -> [Text] -> [Double] -> [Text]
           -> VegaLite
groupedBar cfg xLabel yLabel groupLabel xCats vals groupCats =
  toVegaLite
    [ title (plotTitle cfg) []
    , dataFromColumns []
        . dataColumn xLabel     (Strings xCats)
        . dataColumn yLabel     (Numbers vals)
        . dataColumn groupLabel (Strings groupCats)
        $ []
    , mark Bar []
    , encoding
        . position X [ PName groupLabel, PmType Nominal
                     , PAxis [AxTitle ""]
                     , PScale [SPaddingInner 0.1] ]
        . position Y [ PName yLabel, PmType Quantitative
                     , PAxis [AxTitle yLabel] ]
        . color [ MName groupLabel, MmType Nominal
                , MScale [SScheme "tableau10" []] ]
        . column [ FName xLabel, FmType Nominal
                 , FHeader [HTitle xLabel, HLabelAngle (-30)] ]
        $ []
    , height (plotHeight cfg)
    ]

-- ---------------------------------------------------------------------------
-- ファイル書き出し
-- ---------------------------------------------------------------------------

-- | Write a bar-chart spec to disk in the given output format.
barChartFile :: OutputFormat -> FilePath -> VegaLite -> IO ()
barChartFile = writeSpec

-- ---------------------------------------------------------------------------
-- 130: PlotData ベースの汎用 spec API
-- ---------------------------------------------------------------------------

-- | Build a Vega-Lite bar chart spec from a 'PlotData' source.
--
-- The category column must live in @pdText@ and the value column in
-- @pdNumeric@. Returns 'barChart' empty-data spec if either is missing.
barSpec
  :: PlotConfig
  -> Text          -- ^ category column (text)
  -> Text          -- ^ value column (numeric)
  -> PlotData
  -> VegaLite
barSpec cfg catCol valCol pd =
  let cats = maybe [] V.toList (textColumn    catCol pd)
      vals = maybe [] V.toList (numericColumn valCol pd)
  in barChart cfg catCol valCol cats vals
