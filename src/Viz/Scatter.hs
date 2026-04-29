{-# LANGUAGE OverloadedStrings #-}
module Viz.Scatter
  ( scatterPlot
  , scatterPlotFile
  , scatterWithLM
  , scatterWithLMFile
  , scatterWithLMCI
  , scatterWithLMCIFile
  , scatterWithSmooth
  , scatterWithSmoothFile
  , predictedVsActual
  , predictedVsActualFile
  ) where

import DataFrame.Core
import Model.Core  (FitResult, fittedList)
import Model.LM    (CIBand (..), SmoothFit (..))
import Viz.Core (PlotConfig (..))

import Data.List (sortBy)
import Data.Ord (comparing)
import Graphics.Vega.VegaLite
import Data.Text (Text)
import qualified Data.Vector as V

-- | Build a Vega-Lite scatter plot spec from two numeric columns.
scatterPlot :: PlotConfig -> DataFrame -> Text -> Text -> VegaLite
scatterPlot cfg df xCol yCol =
  toVegaLite
    [ title (plotTitle cfg) []
    , dataSpec
    , mark Point [MTooltip TTEncoding]
    , encSpec
    , width  (plotWidth  cfg)
    , height (plotHeight cfg)
    ]
  where
    xVals    = maybe [] V.toList (getNumeric xCol df)
    yVals    = maybe [] V.toList (getNumeric yCol df)

    dataSpec = dataFromColumns []
               . dataColumn xCol (Numbers xVals)
               . dataColumn yCol (Numbers yVals)
               $ []

    encSpec  = encoding
               . position X [PName xCol, PmType Quantitative, PAxis [AxTitle xCol]]
               . position Y [PName yCol, PmType Quantitative, PAxis [AxTitle yCol]]
               $ []

-- | Write scatter plot to an HTML file.
scatterPlotFile :: FilePath -> PlotConfig -> DataFrame -> Text -> Text -> IO ()
scatterPlotFile path cfg df xCol yCol =
  toHtmlFile path (scatterPlot cfg df xCol yCol)

-- | Scatter plot with a fitted regression line overlaid.
scatterWithLM :: PlotConfig -> DataFrame -> Text -> Text -> FitResult -> VegaLite
scatterWithLM cfg df xCol yCol res =
  toVegaLite
    [ title (plotTitle cfg) []
    , layer [pointLayer, lineLayer]
    , width  (plotWidth  cfg)
    , height (plotHeight cfg)
    ]
  where
    xVals = maybe [] V.toList (getNumeric xCol df)
    yVals = maybe [] V.toList (getNumeric yCol df)

    -- sort (x, ŷ) by x so the line renders cleanly
    pairs  = sortBy (comparing fst) (zip xVals (fittedList res))
    xLine  = map fst pairs
    yLine  = map snd pairs

    pointLayer = asSpec
      [ dataFromColumns []
          . dataColumn xCol (Numbers xVals)
          . dataColumn yCol (Numbers yVals)
          $ []
      , mark Point [MTooltip TTEncoding]
      , encoding
          . position X [PName xCol, PmType Quantitative, PAxis [AxTitle xCol]]
          . position Y [PName yCol, PmType Quantitative, PAxis [AxTitle yCol]]
          $ []
      ]

    lineLayer = asSpec
      [ dataFromColumns []
          . dataColumn xCol      (Numbers xLine)
          . dataColumn "fitted"  (Numbers yLine)
          $ []
      , mark Line [MColor "red", MStrokeWidth 2.0]
      , encoding
          . position X [PName xCol,      PmType Quantitative]
          . position Y [PName "fitted",  PmType Quantitative]
          $ []
      ]

-- | Write scatter + LM plot to an HTML file.
scatterWithLMFile :: FilePath -> PlotConfig -> DataFrame -> Text -> Text -> FitResult -> IO ()
scatterWithLMFile path cfg df xCol yCol res =
  toHtmlFile path (scatterWithLM cfg df xCol yCol res)

-- | Scatter plot with regression line and confidence band.
-- Layer order: CI band (bottom) → regression line → data points (top)
scatterWithLMCI :: PlotConfig -> DataFrame -> Text -> Text -> FitResult -> CIBand -> VegaLite
scatterWithLMCI cfg df xCol yCol res ci =
  toVegaLite
    [ title (plotTitle cfg) []
    , layer [ciLayer, lineLayer, pointLayer]
    , width  (plotWidth  cfg)
    , height (plotHeight cfg)
    ]
  where
    xVals = maybe [] V.toList (getNumeric xCol df)
    yVals = maybe [] V.toList (getNumeric yCol df)

    -- Sort all line/band data by x for clean rendering
    sorted4 = sortBy (comparing (\(x, _, _, _) -> x))
                [ (x, f, l, u)
                | ((x, f), (l, u)) <-
                    zip (zip xVals (fittedList res))
                        (zip (lowerBound ci) (upperBound ci))
                ]
    xSorted = [x | (x, _, _, _) <- sorted4]
    fSorted = [f | (_, f, _, _) <- sorted4]
    lSorted = [l | (_, _, l, _) <- sorted4]
    uSorted = [u | (_, _, _, u) <- sorted4]

    pointLayer = asSpec
      [ dataFromColumns []
          . dataColumn xCol (Numbers xVals)
          . dataColumn yCol (Numbers yVals)
          $ []
      , mark Point [MTooltip TTEncoding]
      , encoding
          . position X [PName xCol, PmType Quantitative, PAxis [AxTitle xCol]]
          . position Y [PName yCol, PmType Quantitative, PAxis [AxTitle yCol]]
          $ []
      ]

    lineLayer = asSpec
      [ dataFromColumns []
          . dataColumn xCol     (Numbers xSorted)
          . dataColumn "fitted" (Numbers fSorted)
          $ []
      , mark Line [MColor "red", MStrokeWidth 2.0]
      , encoding
          . position X [PName xCol,     PmType Quantitative]
          . position Y [PName "fitted", PmType Quantitative]
          $ []
      ]

    ciLayer = asSpec
      [ dataFromColumns []
          . dataColumn xCol    (Numbers xSorted)
          . dataColumn "lower" (Numbers lSorted)
          . dataColumn "upper" (Numbers uSorted)
          $ []
      , mark Area [MOpacity 0.15, MColor "red"]
      , encoding
          . position X  [PName xCol,    PmType Quantitative]
          . position Y  [PName "lower", PmType Quantitative]
          . position Y2 [PName "upper"]
          $ []
      ]

-- | Write scatter + LM + CI plot to an HTML file.
scatterWithLMCIFile :: FilePath -> PlotConfig -> DataFrame -> Text -> Text -> FitResult -> CIBand -> IO ()
scatterWithLMCIFile path cfg df xCol yCol res ci =
  toHtmlFile path (scatterWithLMCI cfg df xCol yCol res ci)

-- | Scatter plot with a smooth fitted curve and CI band from a fine grid.
-- Layer order: CI band → curve → data points
scatterWithSmooth :: PlotConfig -> DataFrame -> Text -> Text -> SmoothFit -> VegaLite
scatterWithSmooth cfg df xCol yCol sf =
  toVegaLite
    [ title (plotTitle cfg) []
    , layer [ciLayer, lineLayer, pointLayer]
    , width  (plotWidth  cfg)
    , height (plotHeight cfg)
    ]
  where
    xVals = maybe [] V.toList (getNumeric xCol df)
    yVals = maybe [] V.toList (getNumeric yCol df)

    pointLayer = asSpec
      [ dataFromColumns []
          . dataColumn xCol (Numbers xVals)
          . dataColumn yCol (Numbers yVals)
          $ []
      , mark Point [MTooltip TTEncoding]
      , encoding
          . position X [PName xCol, PmType Quantitative, PAxis [AxTitle xCol]]
          . position Y [PName yCol, PmType Quantitative, PAxis [AxTitle yCol]]
          $ []
      ]

    lineLayer = asSpec
      [ dataFromColumns []
          . dataColumn xCol     (Numbers (sfX sf))
          . dataColumn "fitted" (Numbers (sfFit sf))
          $ []
      , mark Line [MColor "red", MStrokeWidth 2.0]
      , encoding
          . position X [PName xCol,     PmType Quantitative]
          . position Y [PName "fitted", PmType Quantitative]
          $ []
      ]

    ciLayer = asSpec
      [ dataFromColumns []
          . dataColumn xCol    (Numbers (sfX sf))
          . dataColumn "lower" (Numbers (sfLower sf))
          . dataColumn "upper" (Numbers (sfUpper sf))
          $ []
      , mark Area [MOpacity 0.15, MColor "red"]
      , encoding
          . position X  [PName xCol,    PmType Quantitative]
          . position Y  [PName "lower", PmType Quantitative]
          . position Y2 [PName "upper"]
          $ []
      ]

scatterWithSmoothFile :: FilePath -> PlotConfig -> DataFrame -> Text -> Text -> SmoothFit -> IO ()
scatterWithSmoothFile path cfg df xCol yCol sf =
  toHtmlFile path (scatterWithSmooth cfg df xCol yCol sf)

-- | Predicted vs Actual diagnostic plot.
-- Points cluster around the identity line (y = x) for a good fit.
predictedVsActual :: PlotConfig -> [Double] -> [Double] -> VegaLite
predictedVsActual cfg actuals preds =
  toVegaLite
    [ title (plotTitle cfg) []
    , layer [identityLayer, pointLayer]
    , width  (plotWidth  cfg)
    , height (plotHeight cfg)
    ]
  where
    resids = zipWith (-) actuals preds
    lo     = minimum (actuals ++ preds)
    hi     = maximum (actuals ++ preds)

    pointLayer = asSpec
      [ dataFromColumns []
          . dataColumn "actual"    (Numbers actuals)
          . dataColumn "predicted" (Numbers preds)
          . dataColumn "residual"  (Numbers resids)
          $ []
      , mark Point [MTooltip TTEncoding]
      , encoding
          . position X [PName "actual",    PmType Quantitative, PAxis [AxTitle "Actual"]]
          . position Y [PName "predicted", PmType Quantitative, PAxis [AxTitle "Predicted"]]
          $ []
      ]

    -- Identity line: perfect predictions fall on y = x
    identityLayer = asSpec
      [ dataFromColumns []
          . dataColumn "ix" (Numbers [lo, hi])
          . dataColumn "iy" (Numbers [lo, hi])
          $ []
      , mark Line [MColor "gray", MStrokeWidth 1.5, MStrokeDash [6, 4]]
      , encoding
          . position X [PName "ix", PmType Quantitative]
          . position Y [PName "iy", PmType Quantitative]
          $ []
      ]

predictedVsActualFile :: FilePath -> PlotConfig -> [Double] -> [Double] -> IO ()
predictedVsActualFile path cfg actuals preds =
  toHtmlFile path (predictedVsActual cfg actuals preds)
