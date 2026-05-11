{-# LANGUAGE OverloadedStrings #-}
-- | Scatter plots and overlays.
--
-- Provides plain scatter, scatter-with-fit-line ('scatterWithLM' /
-- 'scatterWithSmooth'), grouped scatter and predicted-vs-actual
-- diagnostic plots.
module Hanalyze.Viz.Scatter
  ( scatterPlot
  , scatterPlotFile
  , scatterWithLM
  , scatterWithLMFile
  , scatterWithLMCI
  , scatterWithLMCIFile
  , scatterWithSmooth
  , scatterWithSmoothFile
  , scatterMultiY
  , scatterMultiYFile
  , scatterWithGroups
  , scatterWithGroupsFile
  , predictedVsActual
  , predictedVsActualFile
  ) where

import qualified DataFrame.Internal.DataFrame as DXD
import Hanalyze.DataIO.Convert (getDoubleVec)
import Hanalyze.Model.Core  (FitResult, fittedList)
import Hanalyze.Model.LM    (CIBand (..), SmoothFit (..))
import Hanalyze.Viz.Core    (PlotConfig (..), OutputFormat, writeSpec)

import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Vector as V
import Graphics.Vega.VegaLite

-- | Build a Vega-Lite scatter plot spec from two numeric columns.
scatterPlot :: PlotConfig -> DXD.DataFrame -> Text -> Text -> VegaLite
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
    xVals   = maybe [] V.toList (getDoubleVec xCol df)
    yVals   = maybe [] V.toList (getDoubleVec yCol df)
    dataSpec = dataFromColumns []
               . dataColumn xCol (Numbers xVals)
               . dataColumn yCol (Numbers yVals)
               $ []
    encSpec  = encoding
               . position X [PName xCol, PmType Quantitative, PAxis [AxTitle xCol]]
               . position Y [PName yCol, PmType Quantitative, PAxis [AxTitle yCol]]
               $ []

scatterPlotFile :: OutputFormat -> FilePath -> PlotConfig -> DXD.DataFrame -> Text -> Text -> IO ()
scatterPlotFile fmt path cfg df xCol yCol =
  writeSpec fmt path (scatterPlot cfg df xCol yCol)

-- | Scatter plot with a fitted regression line overlaid.
scatterWithLM :: PlotConfig -> DXD.DataFrame -> Text -> Text -> FitResult -> VegaLite
scatterWithLM cfg df xCol yCol res =
  toVegaLite
    [ title (plotTitle cfg) []
    , layer [pointLayer, lineLayer]
    , width  (plotWidth  cfg)
    , height (plotHeight cfg)
    ]
  where
    xVals  = maybe [] V.toList (getDoubleVec xCol df)
    yVals  = maybe [] V.toList (getDoubleVec yCol df)
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
          . dataColumn xCol     (Numbers xLine)
          . dataColumn "fitted" (Numbers yLine)
          $ []
      , mark Line [MColor "red", MStrokeWidth 2.0]
      , encoding
          . position X [PName xCol,     PmType Quantitative]
          . position Y [PName "fitted", PmType Quantitative]
          $ []
      ]

scatterWithLMFile :: OutputFormat -> FilePath -> PlotConfig -> DXD.DataFrame -> Text -> Text -> FitResult -> IO ()
scatterWithLMFile fmt path cfg df xCol yCol res =
  writeSpec fmt path (scatterWithLM cfg df xCol yCol res)

-- | Scatter plot with regression line and confidence band (training-point CI).
scatterWithLMCI :: PlotConfig -> DXD.DataFrame -> Text -> Text -> FitResult -> CIBand -> VegaLite
scatterWithLMCI cfg df xCol yCol res ci =
  toVegaLite
    [ title (plotTitle cfg) []
    , layer [ciLayer, lineLayer, pointLayer]
    , width  (plotWidth  cfg)
    , height (plotHeight cfg)
    ]
  where
    xVals = maybe [] V.toList (getDoubleVec xCol df)
    yVals = maybe [] V.toList (getDoubleVec yCol df)

    sorted4 = sortBy (comparing (\(x,_,_,_) -> x))
                [ (x, f, l, u)
                | ((x,f),(l,u)) <-
                    zip (zip xVals (fittedList res))
                        (zip (lowerBound ci) (upperBound ci))
                ]
    xSorted = [x | (x,_,_,_) <- sorted4]
    fSorted = [f | (_,f,_,_) <- sorted4]
    lSorted = [l | (_,_,l,_) <- sorted4]
    uSorted = [u | (_,_,_,u) <- sorted4]

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

scatterWithLMCIFile :: OutputFormat -> FilePath -> PlotConfig -> DXD.DataFrame -> Text -> Text -> FitResult -> CIBand -> IO ()
scatterWithLMCIFile fmt path cfg df xCol yCol res ci =
  writeSpec fmt path (scatterWithLMCI cfg df xCol yCol res ci)

-- | Scatter plot with smooth fitted curve.
-- Renders a CI/PI band when sfHasBand is True.
-- Shows an optional equation subtitle under the chart title.
scatterWithSmooth :: PlotConfig -> Maybe Text -> DXD.DataFrame -> Text -> Text -> SmoothFit -> VegaLite
scatterWithSmooth cfg mEquation df xCol yCol sf =
  toVegaLite
    [ title (plotTitle cfg) titleOpts
    , layer layers
    , width  (plotWidth  cfg)
    , height (plotHeight cfg)
    ]
  where
    xVals = maybe [] V.toList (getDoubleVec xCol df)
    yVals = maybe [] V.toList (getDoubleVec yCol df)

    titleOpts = case mEquation of
      Just eq -> [TSubtitle eq, TSubtitleFontSize 11, TSubtitleColor "#555"]
      Nothing -> []

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

    layers = (if sfHasBand sf then [ciLayer] else []) ++ [lineLayer, pointLayer]

scatterWithSmoothFile :: OutputFormat -> FilePath -> PlotConfig -> Maybe Text -> DXD.DataFrame -> Text -> Text -> SmoothFit -> IO ()
scatterWithSmoothFile fmt path cfg mEq df xCol yCol sf =
  writeSpec fmt path (scatterWithSmooth cfg mEq df xCol yCol sf)

-- | Scatter plot with multiple y columns as color-coded series (no regression).
scatterMultiY :: PlotConfig -> DXD.DataFrame -> Text -> [Text] -> VegaLite
scatterMultiY cfg df xCol yCols =
  toVegaLite
    [ title (plotTitle cfg) []
    , dataSpec
    , transform
        . foldAs yCols "series" "value"
        $ []
    , mark Point [MTooltip TTEncoding]
    , encoding
        . position X [PName xCol,    PmType Quantitative, PAxis [AxTitle xCol]]
        . position Y [PName "value", PmType Quantitative, PAxis [AxTitle "value"]]
        . color [MName "series", MmType Nominal]
        $ []
    , width  (plotWidth  cfg)
    , height (plotHeight cfg)
    ]
  where
    xVals = maybe [] V.toList (getDoubleVec xCol df)
    yData = foldr (\col f -> dataColumn col (Numbers (maybe [] V.toList (getDoubleVec col df))) . f)
                  id yCols

    dataSpec = dataFromColumns []
               . dataColumn xCol (Numbers xVals)
               . yData
               $ []

scatterMultiYFile :: OutputFormat -> FilePath -> PlotConfig -> DXD.DataFrame -> Text -> [Text] -> IO ()
scatterMultiYFile fmt path cfg df xCol yCols =
  writeSpec fmt path (scatterMultiY cfg df xCol yCols)

-- | Predicted vs Actual diagnostic plot.
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

predictedVsActualFile :: OutputFormat -> FilePath -> PlotConfig -> [Double] -> [Double] -> IO ()
predictedVsActualFile fmt path cfg actuals preds =
  writeSpec fmt path (predictedVsActual cfg actuals preds)

-- | Scatter with per-group conditional fitted lines (LME / GLMM).
-- Points are colour-coded by group; one fitted line per group shares the same colour scheme.
-- ptData: (group, x, y) for raw observations
-- lnData: (group, x, ŷ) for smooth conditional fits (grid-evaluated)
scatterWithGroups
  :: PlotConfig
  -> Text
  -> Text
  -> [(Text, Double, Double)]
  -> [(Text, Double, Double)]
  -> VegaLite
scatterWithGroups cfg xCol yCol ptData lnData =
  toVegaLite
    [ title (plotTitle cfg) []
    , layer [lineLayer, pointLayer]
    , width  (plotWidth  cfg)
    , height (plotHeight cfg)
    ]
  where
    (ptGrps, ptXs, ptYs) = unzip3 ptData
    (lnGrps, lnXs, lnYs) = unzip3 lnData

    pointLayer = asSpec
      [ dataFromColumns []
          . dataColumn xCol    (Numbers ptXs)
          . dataColumn yCol    (Numbers ptYs)
          . dataColumn "group" (Strings ptGrps)
          $ []
      , mark Point [MTooltip TTEncoding]
      , encoding
          . position X [PName xCol,  PmType Quantitative, PAxis [AxTitle xCol]]
          . position Y [PName yCol,  PmType Quantitative, PAxis [AxTitle yCol]]
          . color [MName "group", MmType Nominal]
          $ []
      ]

    lineLayer = asSpec
      [ dataFromColumns []
          . dataColumn xCol     (Numbers lnXs)
          . dataColumn "fitted" (Numbers lnYs)
          . dataColumn "group"  (Strings lnGrps)
          $ []
      , mark Line [MStrokeWidth 2.0]
      , encoding
          . position X [PName xCol,     PmType Quantitative]
          . position Y [PName "fitted", PmType Quantitative]
          . color [MName "group", MmType Nominal]
          $ []
      ]

scatterWithGroupsFile
  :: OutputFormat -> FilePath -> PlotConfig -> Text -> Text
  -> [(Text, Double, Double)] -> [(Text, Double, Double)] -> IO ()
scatterWithGroupsFile fmt path cfg xCol yCol ptData lnData =
  writeSpec fmt path (scatterWithGroups cfg xCol yCol ptData lnData)
