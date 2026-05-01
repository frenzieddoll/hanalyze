{-# LANGUAGE OverloadedStrings #-}
-- | MCMC 診断プロット (Vega-Lite)。
--
-- 単一チェーン版と多チェーン版を提供します。
-- 事後分布は KDE (Kernel Density Estimation) で描画します。
module Viz.MCMC
  ( -- * 単一チェーン
    tracePlot,       tracePlotFile
  , posteriorPlot,   posteriorPlotFile
  , autocorrPlot,    autocorrPlotFile
  , pairScatter,     pairScatterFile
  , mcmcDiagnostics, mcmcDiagnosticsFile
    -- * 多チェーン (PyMC スタイル)
  , multiTracePlot,        multiTracePlotFile
  , mcmcDiagnosticsMulti,  mcmcDiagnosticsMultiFile
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Graphics.Vega.VegaLite

import MCMC.Core  (Chain (..), chainVals)
import Stat.MCMC  (autocorr, hdi, kde)
import Viz.Core   (PlotConfig (..), OutputFormat, writeSpec)

-- ---------------------------------------------------------------------------
-- Trace plot (単一チェーン)
-- ---------------------------------------------------------------------------

tracePlot :: PlotConfig -> [Text] -> Chain -> VegaLite
tracePlot cfg names chain = toVegaLite
  [ title (plotTitle cfg) []
  , vConcat (map tracePanel names)
  ]
  where
    n = length (chainSamples chain)
    tracePanel pname =
      let vals = chainVals pname chain
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
-- Multi-chain trace plot
-- ---------------------------------------------------------------------------

-- | 複数チェーンのトレースプロット。チェーンごとに色分けして重ねて表示。
multiTracePlot :: PlotConfig -> [Text] -> [Chain] -> VegaLite
multiTracePlot cfg names chains = toVegaLite
  [ title (plotTitle cfg) []
  , vConcat (map (mkMultiTracePanel' (plotWidth cfg) 90) names)
  ]
  where
    mkMultiTracePanel' w h pname = mkMultiTracePanel pname w h chains

multiTracePlotFile :: OutputFormat -> FilePath -> PlotConfig -> [Text] -> [Chain] -> IO ()
multiTracePlotFile fmt path cfg names chains =
  writeSpec fmt path (multiTracePlot cfg names chains)

-- ---------------------------------------------------------------------------
-- Posterior KDE plot (単一チェーン)
-- ---------------------------------------------------------------------------

posteriorPlot :: PlotConfig -> [Text] -> Chain -> VegaLite
posteriorPlot cfg names chain = toVegaLite
  [ title (plotTitle cfg) []
  , vConcat (map (\n -> mkKdePanel n (plotWidth cfg) 110 chain) names)
  ]

posteriorPlotFile :: OutputFormat -> FilePath -> PlotConfig -> [Text] -> Chain -> IO ()
posteriorPlotFile fmt path cfg names chain =
  writeSpec fmt path (posteriorPlot cfg names chain)

-- ---------------------------------------------------------------------------
-- Autocorrelation plot
-- ---------------------------------------------------------------------------

autocorrPlot :: PlotConfig -> Int -> [Text] -> Chain -> VegaLite
autocorrPlot cfg maxLag names chain = toVegaLite
  [ title (plotTitle cfg) []
  , vConcat (map acfPanel names)
  ]
  where
    acfPanel pname =
      let acData         = autocorr maxLag (chainVals pname chain)
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
-- Pair scatter
-- ---------------------------------------------------------------------------

pairScatter :: PlotConfig -> Text -> Text -> Chain -> VegaLite
pairScatter cfg xName yName chain = toVegaLite
  [ title (plotTitle cfg) []
  , dataFromColumns []
      . dataColumn xName (Numbers (chainVals xName chain))
      . dataColumn yName (Numbers (chainVals yName chain))
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
-- Combined PyMC-style: [KDE | trace]  (単一チェーン)
-- ---------------------------------------------------------------------------

mcmcDiagnostics :: PlotConfig -> [Text] -> Chain -> VegaLite
mcmcDiagnostics cfg names chain = toVegaLite
  [ title (plotTitle cfg) []
  , vConcat (map rowFor names)
  ]
  where
    n = length (chainSamples chain)
    rowFor pname = asSpec
      [ hConcat [ mkKdePanel   pname 220 80 chain
                , mkTracePanel pname 420 80 n chain ] ]

mcmcDiagnosticsFile :: OutputFormat -> FilePath -> PlotConfig -> [Text] -> Chain -> IO ()
mcmcDiagnosticsFile fmt path cfg names chain =
  writeSpec fmt path (mcmcDiagnostics cfg names chain)

-- ---------------------------------------------------------------------------
-- Combined PyMC-style: [KDE | multi-trace]  (多チェーン)
-- ---------------------------------------------------------------------------

-- | 複数チェーンの PyMC スタイル診断プロット。
-- 左: 全チェーン合算の KDE。右: チェーン別色分けトレース。
mcmcDiagnosticsMulti :: PlotConfig -> [Text] -> [Chain] -> VegaLite
mcmcDiagnosticsMulti cfg names chains = toVegaLite
  [ title (plotTitle cfg) []
  , vConcat (map rowFor names)
  ]
  where
    combined pname = concatMap (chainVals pname) chains
    rowFor pname = asSpec
      [ hConcat
          [ mkKdePanelFrom pname 220 80 (combined pname)
          , mkMultiTracePanel pname 420 80 chains
          ]
      ]

mcmcDiagnosticsMultiFile :: OutputFormat -> FilePath -> PlotConfig -> [Text] -> [Chain] -> IO ()
mcmcDiagnosticsMultiFile fmt path cfg names chains =
  writeSpec fmt path (mcmcDiagnosticsMulti cfg names chains)

-- ---------------------------------------------------------------------------
-- 内部: KDE パネル
-- ---------------------------------------------------------------------------

-- | KDE 密度プロット + 94% HDI ルール。
mkKdePanel :: Text -> Double -> Double -> Chain -> VLSpec
mkKdePanel pname w h chain =
  mkKdePanelFrom pname w h (chainVals pname chain)

mkKdePanelFrom :: Text -> Double -> Double -> [Double] -> VLSpec
mkKdePanelFrom pname w h vals =
  let kdeData      = kde 200 vals
      (xs, ys)     = unzip kdeData
      (lo, hi)     = hdi 0.94 vals
  in asSpec
      [ layer
          [ asSpec  -- KDE filled area
              [ dataFromColumns []
                  . dataColumn "x" (Numbers xs)
                  . dataColumn "y" (Numbers ys)
                  $ []
              , mark Area [MColor "#4C72B0", MOpacity 0.3]
              , encoding
                  . position X [ PName "x", PmType Quantitative
                               , PAxis [AxTitle pname] ]
                  . position Y [ PName "y", PmType Quantitative
                               , PAxis [AxTitle "Density", AxGrid False] ]
                  $ []
              ]
          , asSpec  -- KDE line
              [ dataFromColumns []
                  . dataColumn "x" (Numbers xs)
                  . dataColumn "y" (Numbers ys)
                  $ []
              , mark Line [MColor "#4C72B0", MStrokeWidth 2.0]
              , encoding
                  . position X [PName "x", PmType Quantitative]
                  . position Y [PName "y", PmType Quantitative]
                  $ []
              ]
          , asSpec  -- 94% HDI span (rule at bottom)
              [ dataFromColumns []
                  . dataColumn "lo" (Numbers [lo])
                  . dataColumn "hi" (Numbers [hi])
                  $ []
              , mark Rule [MColor "#DD4444", MStrokeWidth 3.5]
              , encoding
                  . position X  [PName "lo", PmType Quantitative]
                  . position X2 [PName "hi"]
                  $ []
              ]
          ]
      , width w, height h
      ]

-- ---------------------------------------------------------------------------
-- 内部: トレースパネル (単一チェーン)
-- ---------------------------------------------------------------------------

mkTracePanel :: Text -> Double -> Double -> Int -> Chain -> VLSpec
mkTracePanel pname w h n chain =
  let vals = chainVals pname chain
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
-- 内部: 多チェーントレースパネル
-- ---------------------------------------------------------------------------

mkMultiTracePanel :: Text -> Double -> Double -> [Chain] -> VLSpec
mkMultiTracePanel pname w h chains =
  let (iters, values, chainIds) = unzip3
        [ (fromIntegral i :: Double, v, T.pack (show c))
        | (c, ch) <- zip [1 :: Int ..] chains
        , (i, v)  <- zip [1 :: Int ..] (chainVals pname ch)
        ]
  in asSpec
      [ dataFromColumns []
          . dataColumn "iter"  (Numbers  iters)
          . dataColumn "value" (Numbers  values)
          . dataColumn "chain" (Strings  chainIds)
          $ []
      , mark Line [MStrokeWidth 1.0, MOpacity 0.7]
      , encoding
          . position X [ PName "iter",  PmType Quantitative
                       , PAxis [AxTitle "Iteration"] ]
          . position Y [ PName "value", PmType Quantitative
                       , PAxis [AxTitle ""] ]
          . color [ MName "chain", MmType Nominal
                  , MScale [SScheme "tableau10" []]
                  , MLegend [LTitle "Chain"] ]
          $ []
      , width w, height h
      ]
