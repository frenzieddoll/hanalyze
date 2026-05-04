{-# LANGUAGE OverloadedStrings #-}
-- | Pareto-front visualizations.
--
--   * 'paretoScatter'       — two-objective scatter (front highlighted).
--   * 'paretoPair'          — pairs scatter matrix for ≥ 3 objectives.
--   * 'parallelCoordinates' — multi-objective parallel coordinates.
--   * 'hypervolumeHistory'  — hypervolume convergence trace.
--   * 'paretoCompare'       — compare an approximate vs true front
--     (two objectives).
module Viz.Pareto
  ( paretoScatter
  , paretoScatterFile
  , paretoPair
  , paretoPairFile
  , parallelCoordinates
  , parallelCoordinatesFile
  , hypervolumeHistory
  , hypervolumeHistoryFile
  , paretoCompare
  , paretoCompareFile
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Graphics.Vega.VegaLite

import Optim.NSGA (Solution (..))
import Viz.Core   (PlotConfig (..), OutputFormat, writeSpec)

-- ---------------------------------------------------------------------------
-- 2 目的の散布図
-- ---------------------------------------------------------------------------

-- | Pareto-front scatter plot for a two-objective problem.
-- All solutions (front included) are scattered in blue; the front
-- (rank-0 individuals) is overlaid in thicker red.
paretoScatter :: PlotConfig
              -> [Solution]   -- ^ All individuals (or the population of interest).
              -> [Solution]   -- ^ Pareto front (highlighted).
              -> VegaLite
paretoScatter cfg allSol frontSol =
  let asObj2D s = case solObjectives s of
        (a : b : _) -> [a, b]
        _ -> [0, 0]
      allObjs   = map asObj2D allSol
      frontObjs = map asObj2D frontSol
      f1All = [head o | o <- allObjs]
      f2All = [o !! 1 | o <- allObjs]
      f1F   = [head o | o <- frontObjs]
      f2F   = [o !! 1 | o <- frontObjs]
  in toVegaLite
      [ title (plotTitle cfg) []
      , layer
          [ asSpec
              [ dataFromColumns []
                  . dataColumn "f1" (Numbers f1All)
                  . dataColumn "f2" (Numbers f2All)
                  $ []
              , mark Point [MOpacity 0.3, MSize 25, MColor "#4C72B0"]
              , encoding
                  . position X [PName "f1", PmType Quantitative,
                                PAxis [AxTitle "f1"]]
                  . position Y [PName "f2", PmType Quantitative,
                                PAxis [AxTitle "f2"]]
                  $ []
              ]
          , asSpec
              [ dataFromColumns []
                  . dataColumn "f1" (Numbers f1F)
                  . dataColumn "f2" (Numbers f2F)
                  $ []
              , mark Point [MOpacity 0.95, MSize 70, MColor "#DD2222"]
              , encoding
                  . position X [PName "f1", PmType Quantitative]
                  . position Y [PName "f2", PmType Quantitative]
                  $ []
              ]
          ]
      , width  (plotWidth cfg)
      , height (plotHeight cfg)
      ]

paretoScatterFile :: OutputFormat -> FilePath -> PlotConfig
                  -> [Solution] -> [Solution] -> IO ()
paretoScatterFile fmt path cfg allSol fSol =
  writeSpec fmt path (paretoScatter cfg allSol fSol)

-- ---------------------------------------------------------------------------
-- ペア散布行列 (3+ 目的)
-- ---------------------------------------------------------------------------

-- | For 3+ objectives, lay out all pairwise 2D scatter plots in a
-- grid. Diagonal cells (same objective vs itself) are omitted; only the
-- upper triangle is drawn.
paretoPair :: PlotConfig -> [Text] -> [Solution] -> VegaLite
paretoPair cfg objLabels front =
  let m       = length objLabels
      objs    = map solObjectives front
      panel i j =
        asSpec
          [ dataFromColumns []
              . dataColumn (objLabels !! i)
                  (Numbers [o !! i | o <- objs])
              . dataColumn (objLabels !! j)
                  (Numbers [o !! j | o <- objs])
              $ []
          , mark Point [MOpacity 0.7, MSize 35, MColor "#DD2222"]
          , encoding
              . position X [PName (objLabels !! i), PmType Quantitative]
              . position Y [PName (objLabels !! j), PmType Quantitative]
              $ []
          , width 200
          , height 200
          ]
      rows i = [panel i j | j <- [i + 1 .. m - 1]]
      gridRows = [asSpec [hConcat (rows i)] | i <- [0 .. m - 2]]
  in toVegaLite
      [ title (plotTitle cfg) []
      , vConcat gridRows
      ]

paretoPairFile :: OutputFormat -> FilePath -> PlotConfig
               -> [Text] -> [Solution] -> IO ()
paretoPairFile fmt path cfg labels front =
  writeSpec fmt path (paretoPair cfg labels front)

-- ---------------------------------------------------------------------------
-- 並行座標プロット
-- ---------------------------------------------------------------------------

-- | Multi-objective parallel-coordinates plot. One line per individual:
-- objectives along the @x@ axis, values on the @y@ axis.
parallelCoordinates :: PlotConfig -> [Text] -> [Solution] -> VegaLite
parallelCoordinates cfg labels front =
  let m       = length labels
      sols    = zip [0 :: Int ..] front
      rows    = [(T.pack (show i), labels !! j, (solObjectives s) !! j)
                | (i, s) <- sols
                , j <- [0 .. m - 1]
                , length (solObjectives s) > j ]
      ids     = [a | (a, _, _) <- rows]
      objs    = [b | (_, b, _) <- rows]
      vals    = [c | (_, _, c) <- rows]
  in toVegaLite
      [ title (plotTitle cfg) []
      , dataFromColumns []
          . dataColumn "id"   (Strings ids)
          . dataColumn "obj"  (Strings objs)
          . dataColumn "val"  (Numbers vals)
          $ []
      , mark Line [MOpacity 0.3, MStrokeWidth 1.0]
      , encoding
          . position X [PName "obj", PmType Nominal,
                        PAxis [AxTitle "Objective"]]
          . position Y [PName "val", PmType Quantitative,
                        PAxis [AxTitle "Value"]]
          . detail [DName "id", DmType Nominal]
          . color [MName "id", MmType Nominal,
                   MLegend [], MScale [SScheme "tableau10" []]]
          $ []
      , width  (plotWidth cfg)
      , height (plotHeight cfg)
      ]

parallelCoordinatesFile :: OutputFormat -> FilePath -> PlotConfig
                        -> [Text] -> [Solution] -> IO ()
parallelCoordinatesFile fmt path cfg labels front =
  writeSpec fmt path (parallelCoordinates cfg labels front)

-- ---------------------------------------------------------------------------
-- HV 収束履歴
-- ---------------------------------------------------------------------------

-- | Convergence plot for NSGA-II: per-generation hypervolume as a line
-- chart.
hypervolumeHistory :: PlotConfig -> [Double] -> VegaLite
hypervolumeHistory cfg hvs =
  let n   = length hvs
      gens = [fromIntegral i :: Double | i <- [0 .. n - 1]]
  in toVegaLite
      [ title (plotTitle cfg) []
      , dataFromColumns []
          . dataColumn "gen" (Numbers gens)
          . dataColumn "hv"  (Numbers hvs)
          $ []
      , mark Line [MColor "#1F77B4", MStrokeWidth 2.5]
      , encoding
          . position X [PName "gen", PmType Quantitative,
                        PAxis [AxTitle "Generation"]]
          . position Y [PName "hv",  PmType Quantitative,
                        PAxis [AxTitle "Hypervolume"]]
          $ []
      , width  (plotWidth cfg)
      , height (plotHeight cfg)
      ]

hypervolumeHistoryFile :: OutputFormat -> FilePath -> PlotConfig
                       -> [Double] -> IO ()
hypervolumeHistoryFile fmt path cfg hvs =
  writeSpec fmt path (hypervolumeHistory cfg hvs)

-- ---------------------------------------------------------------------------
-- 推定 vs 真 front の比較 (2D)
-- ---------------------------------------------------------------------------

-- | Overlay an estimated Pareto front (red) on the true Pareto front
-- (grey dashed). Useful for two-objective benchmark evaluation.
paretoCompare :: PlotConfig
              -> [[Double]]   -- ^ True front (sequence of points along the curve).
              -> [Solution]   -- ^ Estimated front.
              -> VegaLite
paretoCompare cfg trueFront estFront =
  let trueF1 = [head o | o <- trueFront]
      trueF2 = [o !! 1 | o <- trueFront]
      estObjs = map solObjectives estFront
      estF1 = [head o | o <- estObjs, length o >= 2]
      estF2 = [o !! 1 | o <- estObjs, length o >= 2]
  in toVegaLite
      [ title (plotTitle cfg) []
      , layer
          [ asSpec
              [ dataFromColumns []
                  . dataColumn "f1" (Numbers trueF1)
                  . dataColumn "f2" (Numbers trueF2)
                  $ []
              , mark Line [MColor "#888888", MStrokeWidth 2.0,
                           MStrokeDash [4, 4]]
              , encoding
                  . position X [PName "f1", PmType Quantitative,
                                PAxis [AxTitle "f1"]]
                  . position Y [PName "f2", PmType Quantitative,
                                PAxis [AxTitle "f2"]]
                  $ []
              ]
          , asSpec
              [ dataFromColumns []
                  . dataColumn "f1" (Numbers estF1)
                  . dataColumn "f2" (Numbers estF2)
                  $ []
              , mark Point [MColor "#DD2222", MOpacity 0.85, MSize 60]
              , encoding
                  . position X [PName "f1", PmType Quantitative]
                  . position Y [PName "f2", PmType Quantitative]
                  $ []
              ]
          ]
      , width  (plotWidth cfg)
      , height (plotHeight cfg)
      ]

paretoCompareFile :: OutputFormat -> FilePath -> PlotConfig
                  -> [[Double]] -> [Solution] -> IO ()
paretoCompareFile fmt path cfg trueF estF =
  writeSpec fmt path (paretoCompare cfg trueF estF)
