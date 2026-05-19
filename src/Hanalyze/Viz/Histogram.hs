{-# LANGUAGE OverloadedStrings #-}
-- | Histogram plotting.
--
-- 'histogramPlot' renders a basic histogram; 'histogramWithDensity'
-- overlays a fitted theoretical PDF (or PMF for discrete distributions).
-- 'histogramPlotFile' writes to HTML / PNG / SVG.
module Hanalyze.Viz.Histogram
  ( histogramPlot
  , histogramPlotFile
  , histogramWithDensity
  , histogramWithDensityFile
    -- * 130: PlotData ベースの汎用 spec API
  , histSpec
  ) where

import Hanalyze.Stat.Distribution (Distribution, isContinuous, supportRange, distributionName)
import qualified Hanalyze.Stat.Distribution as Dist
import Hanalyze.Viz.Core        (PlotConfig (..), OutputFormat, writeSpec)
import Hanalyze.Viz.PlotData    (PlotData, numericColumn)

import Data.Text (Text)
import qualified Data.Vector as V
import Graphics.Vega.VegaLite

-- ---------------------------------------------------------------------------
-- Pure histogram
-- ---------------------------------------------------------------------------

histogramPlot :: PlotConfig -> Text -> [Double] -> Maybe Int -> VegaLite
histogramPlot cfg xCol vals mBins =
  toVegaLite
    [ title (plotTitle cfg) []
    , dataFromColumns [] . dataColumn xCol (Numbers vals) $ []
    , mark Bar []
    , encoding
        . position X [ PName xCol, PmType Quantitative
                     , PBin [Step (binStepVal mBins vals)]
                     , PAxis [AxTitle xCol] ]
        . position Y [ PAggregate Count, PmType Quantitative
                     , PAxis [AxTitle "Count"] ]
        $ []
    , width  (plotWidth  cfg)
    , height (plotHeight cfg)
    ]

histogramPlotFile :: OutputFormat -> FilePath -> PlotConfig -> Text -> [Double] -> Maybe Int -> IO ()
histogramPlotFile fmt path cfg xCol vals mBins =
  writeSpec fmt path (histogramPlot cfg xCol vals mBins)

-- ---------------------------------------------------------------------------
-- Histogram + PDF/PMF overlay
-- ---------------------------------------------------------------------------

-- | Histogram with theoretical PDF/PMF overlaid.
-- Y-axis is Count; the PDF curve is scaled by (n × binStep) so both align.
histogramWithDensity
  :: PlotConfig
  -> Text           -- x axis label
  -> [Double]       -- observed data
  -> Maybe Int      -- bin count (Nothing → Sturges' rule)
  -> Distribution
  -> VegaLite
histogramWithDensity cfg xCol vals mBins dist =
  toVegaLite
    [ title (plotTitle cfg)
        [ TSubtitle (distributionName dist)
        , TSubtitleFontSize 11, TSubtitleColor "#555" ]
    , layer [histLayer, curveLayer]
    , width  (plotWidth  cfg)
    , height (plotHeight cfg)
    ]
  where
    n     = length vals
    step  = binStepVal mBins vals
    scale = fromIntegral n * step   -- PDF → Count scaling factor

    (xLo, xHi) = supportRange dist
    nGrid = 300 :: Int

    histLayer = asSpec
      [ dataFromColumns [] . dataColumn xCol (Numbers vals) $ []
      , mark Bar [MOpacity 0.55, MColor "#4C72B0"]
      , encoding
          . position X [ PName xCol, PmType Quantitative
                       , PBin [Step step]
                       , PAxis [AxTitle xCol] ]
          . position Y [ PAggregate Count, PmType Quantitative
                       , PAxis [AxTitle "Count"] ]
          $ []
      ]

    curveLayer = asSpec
      [ dataFromColumns []
          . dataColumn "x"     (Numbers gridX)
          . dataColumn "count" (Numbers gridY)
          $ []
      , mark (if isContinuous dist then Line else Point)
          [MColor "#DD4444", MStrokeWidth 2.0, MPoint (PMMarker [])]
      , encoding
          . position X [PName "x",     PmType Quantitative]
          . position Y [PName "count", PmType Quantitative]
          $ []
      ]

    (gridX, gridY) = unzip (scaledGrid dist xLo xHi nGrid scale)

-- | (x, pdf(x) * scale) for the overlay curve.
scaledGrid :: Distribution -> Double -> Double -> Int -> Double -> [(Double, Double)]
scaledGrid dist xLo xHi nPts scale
  | isContinuous dist =
      [ let x = xLo + fromIntegral i * (xHi - xLo) / fromIntegral (nPts - 1)
        in (x, Dist.density dist x * scale)
      | i <- [0 .. nPts - 1] ]
  | otherwise =
      [ (fromIntegral k, Dist.density dist (fromIntegral k) * scale)
      | k <- [round xLo .. round xHi :: Int] ]

histogramWithDensityFile
  :: OutputFormat -> FilePath -> PlotConfig -> Text -> [Double] -> Maybe Int -> Distribution -> IO ()
histogramWithDensityFile fmt path cfg xCol vals mBins dist =
  writeSpec fmt path (histogramWithDensity cfg xCol vals mBins dist)

-- ---------------------------------------------------------------------------
-- Bin helpers
-- ---------------------------------------------------------------------------

sturgesBins :: [Double] -> Int
sturgesBins xs = max 5 (ceiling (logBase 2 (fromIntegral (length xs) :: Double)) + 1)

binStepVal :: Maybe Int -> [Double] -> Double
binStepVal _ [] = 1
binStepVal mBins xs =
  let lo   = minimum xs
      hi   = maximum xs
      bins = maybe (sturgesBins xs) id mBins
  in (hi - lo) / fromIntegral bins

-- ---------------------------------------------------------------------------
-- 130: PlotData ベースの汎用 spec API
-- ---------------------------------------------------------------------------

-- | Build a Vega-Lite histogram spec from a 'PlotData' source.
--
-- @maxBins@ overrides Sturges' rule when provided. Returns an empty
-- (zero-row) spec if the column is missing from @pdNumeric@.
histSpec
  :: PlotConfig
  -> Text          -- ^ numeric column name
  -> Maybe Int     -- ^ max bin count (Nothing = Sturges)
  -> PlotData
  -> VegaLite
histSpec cfg col mBins pd =
  let vals = maybe [] V.toList (numericColumn col pd)
  in histogramPlot cfg col vals mBins
