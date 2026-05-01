{-# LANGUAGE OverloadedStrings #-}
-- | 棒グラフ系の可視化モジュール。
--
-- 提供する関数:
--   - 'barChart'        — カテゴリ別縦棒グラフ
--   - 'barChartH'       — 水平棒グラフ (ラベルが長い場合に便利)
--   - 'stackedBar'      — 積み上げ棒グラフ
--   - 'groupedBar'      — グループ別棒グラフ
--   - 'barChartFile'    — HTML/PNG/SVG ファイルに書き出し
module Viz.Bar
  ( barChart
  , barChartH
  , stackedBar
  , groupedBar
  , barChartFile
  ) where

import Data.Text (Text)
import Graphics.Vega.VegaLite

import Viz.Core (PlotConfig (..), OutputFormat, writeSpec)

-- ---------------------------------------------------------------------------
-- 縦棒グラフ (カテゴリ → 数値)
-- ---------------------------------------------------------------------------

-- | シンプルな縦棒グラフ。
--
-- @
-- barChart cfg "Month" "Sales"
--   ["Jan","Feb","Mar"] [120,95,140]
-- @
barChart :: PlotConfig -> Text -> Text -> [Text] -> [Double] -> VegaLite
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

-- | 水平棒グラフ。ラベルが長い場合やランキング表示に向いている。
--
-- @
-- barChartH cfg "Country" "GDP" countries gdps
-- @
barChartH :: PlotConfig -> Text -> Text -> [Text] -> [Double] -> VegaLite
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

-- | 積み上げ棒グラフ。各カテゴリにグループ別の内訳を重ねる。
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

-- | グループ別棒グラフ (並べて比較)。
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

-- | 棒グラフを指定フォーマットで書き出す。
barChartFile :: OutputFormat -> FilePath -> VegaLite -> IO ()
barChartFile = writeSpec
