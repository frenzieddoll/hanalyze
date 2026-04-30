{-# LANGUAGE OverloadedStrings #-}
module Viz.MCMC
  ( -- * Standalone plots
    tracePlot,       tracePlotFile
  , posteriorPlot,   posteriorPlotFile
  , autocorrPlot,    autocorrPlotFile
  , pairScatter,     pairScatterFile
    -- * Combined diagnostic view (PyMC-style)
  , mcmcDiagnostics, mcmcDiagnosticsFile
  ) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Graphics.Vega.VegaLite

import Model.MCMC (Chain (..))
import Stat.MCMC  (autocorr, hdi)
import Viz.Core   (PlotConfig (..), OutputFormat, writeSpec)

-- ---------------------------------------------------------------------------
-- Trace plot  (one line per parameter, stacked vertically)
-- ---------------------------------------------------------------------------

tracePlot :: PlotConfig -> [Text] -> Chain -> VegaLite
tracePlot cfg names chain = toVegaLite
  [ title (plotTitle cfg) []
  , vConcat (map tracePanel names)
  ]
  where
    n = length (chainSamples chain)
    tracePanel pname =
      let vals = extractVals pname chain
      in asSpec
          [ dataFromColumns []
              . dataColumn "iter"  (Numbers (map fromIntegral [1 .. n]))
              . dataColumn "value" (Numbers vals)
              $ []
          , mark Line [MColor "#4C72B0", MStrokeWidth 1.0, MOpacity 0.7]
          , encoding
              . position X [ PName "iter",  PmType Quantitative
                           , PAxis [AxTitle "Iteration"] ]
              . position Y [ PName "value", PmType Quantitative
                           , PAxis [AxTitle pname] ]
              $ []
          , width  (plotWidth cfg)
          , height 90
          ]

tracePlotFile :: OutputFormat -> FilePath -> PlotConfig -> [Text] -> Chain -> IO ()
tracePlotFile fmt path cfg names chain =
  writeSpec fmt path (tracePlot cfg names chain)

-- ---------------------------------------------------------------------------
-- Posterior histogram  (one panel per parameter, with 94% HDI rule)
-- ---------------------------------------------------------------------------

posteriorPlot :: PlotConfig -> [Text] -> Chain -> VegaLite
posteriorPlot cfg names chain = toVegaLite
  [ title (plotTitle cfg) []
  , vConcat (map (\n -> mkHistPanel n (plotWidth cfg) 110 chain) names)
  ]

posteriorPlotFile :: OutputFormat -> FilePath -> PlotConfig -> [Text] -> Chain -> IO ()
posteriorPlotFile fmt path cfg names chain =
  writeSpec fmt path (posteriorPlot cfg names chain)

-- ---------------------------------------------------------------------------
-- Autocorrelation plot  (bar chart per parameter, stacked vertically)
-- ---------------------------------------------------------------------------

autocorrPlot :: PlotConfig -> Int -> [Text] -> Chain -> VegaLite
autocorrPlot cfg maxLag names chain = toVegaLite
  [ title (plotTitle cfg) []
  , vConcat (map acfPanel names)
  ]
  where
    acfPanel pname =
      let acData         = autocorr maxLag (extractVals pname chain)
          (lags, acVals) = unzip acData
      in asSpec
          [ dataFromColumns []
              . dataColumn "lag" (Numbers (map fromIntegral lags))
              . dataColumn "acf" (Numbers acVals)
              $ []
          , mark Bar [MColor "#4C72B0", MOpacity 0.8]
          , encoding
              . position X [ PName "lag", PmType Quantitative
                           , PAxis [AxTitle "Lag"] ]
              . position Y [ PName "acf", PmType Quantitative
                           , PScale [SDomain (DNumbers [-1, 1])]
                           , PAxis [AxTitle pname] ]
              $ []
          , width  (plotWidth cfg)
          , height 80
          ]

autocorrPlotFile :: OutputFormat -> FilePath -> PlotConfig -> Int -> [Text] -> Chain -> IO ()
autocorrPlotFile fmt path cfg maxLag names chain =
  writeSpec fmt path (autocorrPlot cfg maxLag names chain)

-- ---------------------------------------------------------------------------
-- Pair scatter  (joint posterior of two parameters)
-- ---------------------------------------------------------------------------

pairScatter :: PlotConfig -> Text -> Text -> Chain -> VegaLite
pairScatter cfg xName yName chain = toVegaLite
  [ title (plotTitle cfg) []
  , dataFromColumns []
      . dataColumn xName (Numbers (extractVals xName chain))
      . dataColumn yName (Numbers (extractVals yName chain))
      $ []
  , mark Point [MOpacity 0.25, MSize 15, MColor "#4C72B0"]
  , encoding
      . position X [PName xName, PmType Quantitative]
      . position Y [PName yName, PmType Quantitative]
      $ []
  , width  (plotWidth  cfg)
  , height (plotHeight cfg)
  ]

pairScatterFile :: OutputFormat -> FilePath -> PlotConfig -> Text -> Text -> Chain -> IO ()
pairScatterFile fmt path cfg xName yName chain =
  writeSpec fmt path (pairScatter cfg xName yName chain)

-- ---------------------------------------------------------------------------
-- Combined PyMC-style diagnostics  [posterior hist | trace] per parameter
-- ---------------------------------------------------------------------------

mcmcDiagnostics :: PlotConfig -> [Text] -> Chain -> VegaLite
mcmcDiagnostics cfg names chain = toVegaLite
  [ title (plotTitle cfg) []
  , vConcat (map rowFor names)
  ]
  where
    n = length (chainSamples chain)
    rowFor pname = asSpec
      [ hConcat [ mkHistPanel  pname 220 80 chain
                , mkTracePanel pname 420 80 n chain ] ]

mcmcDiagnosticsFile :: OutputFormat -> FilePath -> PlotConfig -> [Text] -> Chain -> IO ()
mcmcDiagnosticsFile fmt path cfg names chain =
  writeSpec fmt path (mcmcDiagnostics cfg names chain)

-- ---------------------------------------------------------------------------
-- Internal builders (return VLSpec for use in vConcat / hConcat)
-- ---------------------------------------------------------------------------

-- | Histogram with 94% HDI rule overlay.
mkHistPanel :: Text -> Double -> Double -> Chain -> VLSpec
mkHistPanel pname w h chain =
  let vals     = extractVals pname chain
      step     = binStep vals
      (lo, hi) = hdi 0.94 vals
  in asSpec
      [ layer
          [ asSpec
              [ dataFromColumns [] . dataColumn "x" (Numbers vals) $ []
              , mark Bar [MColor "#4C72B0", MOpacity 0.7]
              , encoding
                  . position X [ PName "x", PmType Quantitative
                               , PBin [Step step]
                               , PAxis [AxTitle pname] ]
                  . position Y [ PAggregate Count, PmType Quantitative
                               , PAxis [AxTitle ""] ]
                  $ []
              ]
          , asSpec  -- 94% HDI span
              [ dataFromColumns []
                  . dataColumn "lo" (Numbers [lo])
                  . dataColumn "hi" (Numbers [hi])
                  $ []
              , mark Rule [MColor "#DD4444", MStrokeWidth 2.5]
              , encoding
                  . position X  [PName "lo", PmType Quantitative]
                  . position X2 [PName "hi"]
                  $ []
              ]
          ]
      , width w, height h
      ]

-- | Trace line plot for one parameter.
mkTracePanel :: Text -> Double -> Double -> Int -> Chain -> VLSpec
mkTracePanel pname w h n chain =
  let vals = extractVals pname chain
  in asSpec
      [ dataFromColumns []
          . dataColumn "iter"  (Numbers (map fromIntegral [1 .. n]))
          . dataColumn "value" (Numbers vals)
          $ []
      , mark Line [MColor "#4C72B0", MStrokeWidth 1.0, MOpacity 0.7]
      , encoding
          . position X [ PName "iter",  PmType Quantitative
                       , PAxis [AxTitle "Iteration"] ]
          . position Y [ PName "value", PmType Quantitative
                       , PAxis [AxTitle ""] ]
          $ []
      , width w, height h
      ]

-- ---------------------------------------------------------------------------
-- Utilities
-- ---------------------------------------------------------------------------

extractVals :: Text -> Chain -> [Double]
extractVals pname chain =
  [v | ps <- chainSamples chain, Just v <- [Map.lookup pname ps]]

-- Sturges' rule for bin step width.
binStep :: [Double] -> Double
binStep [] = 1.0
binStep xs =
  let lo   = minimum xs
      hi   = maximum xs
      bins = max 5 (ceiling (logBase 2 (fromIntegral (length xs) :: Double)) + 1 :: Int)
  in (hi - lo) / fromIntegral bins
