{-# LANGUAGE OverloadedStrings #-}
-- | Visualization of Gaussian-process regression results.
--
-- Plots training data (scatter), the posterior mean (curve), and a 95 %
-- credible band.
module Viz.GP
  ( gpPlot
  , gpPlotFile
  ) where

import Model.GP     (GPResult (..))
import Viz.Core     (PlotConfig (..), OutputFormat, writeSpec)
import Data.Text    (Text)
import Graphics.Vega.VegaLite

-- | GP 予測プロットを構築する。
--
-- 描画要素:
--   - 散布点: 訓練データ (trainData)
--   - 青い曲線: 事後平均
--   - 青い帯: 平均 ± 2σ (≈95% 信用区間)
gpPlot
  :: PlotConfig
  -> Text              -- ^ x 軸の列名ラベル
  -> Text              -- ^ y 軸の列名ラベル
  -> [(Double, Double)] -- ^ 訓練データ (x, y)
  -> GPResult
  -> VegaLite
gpPlot cfg xCol yCol trainData res =
  toVegaLite
    [ title (plotTitle cfg) []
    , layer [bandLayer, meanLayer, pointLayer]
    , width  (plotWidth  cfg)
    , height (plotHeight cfg)
    ]
  where
    (trnX, trnY) = unzip trainData
    testXs = gpTestX  res
    means  = gpMean   res
    lowers = gpLower  res
    uppers = gpUpper  res

    -- 訓練点
    pointLayer = asSpec
      [ dataFromColumns []
          . dataColumn xCol (Numbers trnX)
          . dataColumn yCol (Numbers trnY)
          $ []
      , mark Point [MTooltip TTEncoding, MColor "black", MOpacity 0.8, MSize 40]
      , encoding
          . position X [PName xCol, PmType Quantitative, PAxis [AxTitle xCol]]
          . position Y [PName yCol, PmType Quantitative, PAxis [AxTitle yCol]]
          $ []
      ]

    -- 事後平均曲線
    meanLayer = asSpec
      [ dataFromColumns []
          . dataColumn xCol   (Numbers testXs)
          . dataColumn "mean" (Numbers means)
          $ []
      , mark Line [MColor "steelblue", MStrokeWidth 2.5]
      , encoding
          . position X [PName xCol,   PmType Quantitative]
          . position Y [PName "mean", PmType Quantitative, PAxis [AxTitle yCol]]
          $ []
      ]

    -- 95% 信用区間バンド
    bandLayer = asSpec
      [ dataFromColumns []
          . dataColumn xCol    (Numbers testXs)
          . dataColumn "lower" (Numbers lowers)
          . dataColumn "upper" (Numbers uppers)
          $ []
      , mark Area [MOpacity 0.2, MColor "steelblue"]
      , encoding
          . position X  [PName xCol,    PmType Quantitative]
          . position Y  [PName "lower", PmType Quantitative]
          . position Y2 [PName "upper"]
          $ []
      ]

-- | ファイルに書き出す。
gpPlotFile
  :: OutputFormat
  -> FilePath
  -> PlotConfig
  -> Text
  -> Text
  -> [(Double, Double)]
  -> GPResult
  -> IO ()
gpPlotFile fmt path cfg xCol yCol trainData res =
  writeSpec fmt path (gpPlot cfg xCol yCol trainData res)
