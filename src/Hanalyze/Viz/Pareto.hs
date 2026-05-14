{-# LANGUAGE OverloadedStrings #-}
-- | Pareto-front visualizations (130 規約: PlotData ベース).
--
-- 2026-05-14 (130 リクエスト) — 旧版は @[Solution]@ を直接受けていたが、
-- HPotfire の Vega-Lite 移行で全 Viz モジュールを @PlotConfig -> ... ->
-- PlotData -> VegaLite@ で揃える方針になり、Pareto も他と同じ規約に
-- 統一した。@[Solution]@ → 'PlotData' の変換は 'solutionsToPlotData'
-- を経由する。
--
--   * 'paretoScatter'       — two-objective scatter, optional highlight column.
--   * 'paretoPair'          — pairs scatter matrix for ≥ 3 objectives.
--   * 'parallelCoordinates' — multi-objective parallel coordinates.
--   * 'hypervolumeHistory'  — hypervolume convergence trace (gen vs hv).
--   * 'paretoCompare'       — overlay two fronts (e.g. estimated vs true).
module Hanalyze.Viz.Pareto
  ( -- * 130: PlotData ベース API
    paretoScatter
  , paretoScatterFile
  , paretoPair
  , paretoPairFile
  , parallelCoordinates
  , parallelCoordinatesFile
  , hypervolumeHistory
  , hypervolumeHistoryFile
  , paretoCompare
  , paretoCompareFile
    -- * 変換ヘルパ
  , solutionsToPlotData
  ) where

import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Vector          as V
import           Graphics.Vega.VegaLite

import           Hanalyze.Optim.NSGA  (Solution (..))
import           Hanalyze.Viz.Core    (PlotConfig (..), OutputFormat, writeSpec)
import           Hanalyze.Viz.PlotData
                  (PlotData (..), numericColumn, textColumn, fromMixedColumns)

-- ---------------------------------------------------------------------------
-- 変換ヘルパ
-- ---------------------------------------------------------------------------

-- | Convert a list of NSGA-II 'Solution' values to a 'PlotData' with one
-- numeric column per objective. @objLabels@ provides the column names
-- (length must match each solution's @solObjectives@); shorter
-- 'solObjectives' lists are padded with @0@.
--
-- This is the canonical bridge from optimisation results to Pareto
-- visualisations under the new 130 規約.
solutionsToPlotData :: [Text] -> [Solution] -> PlotData
solutionsToPlotData objLabels sols =
  let m       = length objLabels
      pad o   = take m (o ++ Prelude.repeat 0)
      cols    = [ ( lab
                  , V.fromList [ pad (solObjectives s) !! j | s <- sols ]
                  )
                | (j, lab) <- zip [0 :: Int ..] objLabels
                ]
  in fromMixedColumns cols []

-- 内部ヘルパ: 取り出し失敗時は空ベクタ
numCol :: Text -> PlotData -> [Double]
numCol n pd = maybe [] V.toList (numericColumn n pd)

txtCol :: Text -> PlotData -> [Text]
txtCol n pd = maybe [] V.toList (textColumn n pd)

-- ---------------------------------------------------------------------------
-- 2 目的の散布図
-- ---------------------------------------------------------------------------

-- | Pareto-front scatter plot for a two-objective problem on a single
-- 'PlotData'. The optional third argument is the name of a text column
-- in 'pdText' carrying a categorical highlight (e.g. @"front"@ /
-- @"all"@); when supplied, points are coloured by that column. Without
-- it, all points share a single colour.
paretoScatter :: PlotConfig
              -> (Text, Text)   -- ^ (xCol, yCol)
              -> Maybe Text     -- ^ optional highlight column (text)
              -> PlotData
              -> VegaLite
paretoScatter cfg (xCol, yCol) mHilite pd =
  let xs = numCol xCol pd
      ys = numCol yCol pd
      addHi cols = case mHilite of
        Just c  -> dataColumn c (Strings (txtCol c pd)) cols
        Nothing -> cols
      addColorEnc encs = case mHilite of
        Just c  -> color [MName c, MmType Nominal] encs
        Nothing -> encs
  in toVegaLite
      [ title (plotTitle cfg) []
      , dataFromColumns []
          . dataColumn xCol (Numbers xs)
          . dataColumn yCol (Numbers ys)
          . addHi
          $ []
      , mark Point [MOpacity 0.7, MSize 50]
      , encoding
          . position X [PName xCol, PmType Quantitative, PAxis [AxTitle xCol]]
          . position Y [PName yCol, PmType Quantitative, PAxis [AxTitle yCol]]
          . addColorEnc
          $ []
      , width  (plotWidth  cfg)
      , height (plotHeight cfg)
      ]

paretoScatterFile :: OutputFormat -> FilePath -> PlotConfig
                  -> (Text, Text) -> Maybe Text -> PlotData -> IO ()
paretoScatterFile fmt path cfg cols mHi pd =
  writeSpec fmt path (paretoScatter cfg cols mHi pd)

-- ---------------------------------------------------------------------------
-- ペア散布行列 (3+ 目的)
-- ---------------------------------------------------------------------------

-- | For 3+ objectives, lay out all pairwise 2D scatter plots in a
-- grid. Diagonal cells are omitted; only the upper triangle is drawn.
-- All @objCols@ must be present in 'pdNumeric'.
paretoPair :: PlotConfig -> [Text] -> PlotData -> VegaLite
paretoPair cfg objCols pd =
  let m       = length objCols
      colVec  = Map.fromList [ (c, numCol c pd) | c <- objCols ] :: Map Text [Double]
      lookupV c = Map.findWithDefault [] c colVec
      panel i j =
        let xC = objCols !! i
            yC = objCols !! j
        in asSpec
             [ dataFromColumns []
                 . dataColumn xC (Numbers (lookupV xC))
                 . dataColumn yC (Numbers (lookupV yC))
                 $ []
             , mark Point [MOpacity 0.7, MSize 35, MColor "#DD2222"]
             , encoding
                 . position X [PName xC, PmType Quantitative]
                 . position Y [PName yC, PmType Quantitative]
                 $ []
             , width  200
             , height 200
             ]
      rowsAt i = [panel i j | j <- [i + 1 .. m - 1]]
      gridRows = [asSpec [hConcat (rowsAt i)] | i <- [0 .. m - 2]]
  in toVegaLite
       [ title (plotTitle cfg) []
       , vConcat gridRows
       ]

paretoPairFile :: OutputFormat -> FilePath -> PlotConfig
               -> [Text] -> PlotData -> IO ()
paretoPairFile fmt path cfg labels pd =
  writeSpec fmt path (paretoPair cfg labels pd)

-- ---------------------------------------------------------------------------
-- 並行座標プロット
-- ---------------------------------------------------------------------------

-- | Multi-objective parallel-coordinates plot. One line per row in
-- 'PlotData'; objectives are spread along the @x@ axis. The @id@
-- column is synthesised from row index.
parallelCoordinates :: PlotConfig
                    -> [Text]    -- ^ objective column names (numeric)
                    -> PlotData
                    -> VegaLite
parallelCoordinates cfg labels pd =
  let n      = pdLength pd
      idsRow = [ T.pack (show (i :: Int)) | i <- [0 .. n - 1] ]
      rows   = [ (idsRow !! i, lab, numCol lab pd !! i)
               | i   <- [0 .. n - 1]
               , lab <- labels
               , let xs = numCol lab pd
               , length xs > i
               ]
      ids  = [ a | (a, _, _) <- rows ]
      objs = [ b | (_, b, _) <- rows ]
      vals = [ c | (_, _, c) <- rows ]
  in toVegaLite
      [ title (plotTitle cfg) []
      , dataFromColumns []
          . dataColumn "id"  (Strings ids)
          . dataColumn "obj" (Strings objs)
          . dataColumn "val" (Numbers vals)
          $ []
      , mark Line [MOpacity 0.3, MStrokeWidth 1.0]
      , encoding
          . position X [PName "obj", PmType Nominal, PAxis [AxTitle "Objective"]]
          . position Y [PName "val", PmType Quantitative, PAxis [AxTitle "Value"]]
          . detail   [DName "id", DmType Nominal]
          . color    [MName "id", MmType Nominal,
                      MLegend [], MScale [SScheme "tableau10" []]]
          $ []
      , width  (plotWidth  cfg)
      , height (plotHeight cfg)
      ]

parallelCoordinatesFile :: OutputFormat -> FilePath -> PlotConfig
                        -> [Text] -> PlotData -> IO ()
parallelCoordinatesFile fmt path cfg labels pd =
  writeSpec fmt path (parallelCoordinates cfg labels pd)

-- ---------------------------------------------------------------------------
-- HV 収束履歴
-- ---------------------------------------------------------------------------

-- | Convergence plot: per-generation hypervolume. @genCol@ and @hvCol@
-- must both live in 'pdNumeric'.
hypervolumeHistory :: PlotConfig
                   -> Text     -- ^ generation column name
                   -> Text     -- ^ hypervolume column name
                   -> PlotData
                   -> VegaLite
hypervolumeHistory cfg genCol hvCol pd =
  toVegaLite
    [ title (plotTitle cfg) []
    , dataFromColumns []
        . dataColumn genCol (Numbers (numCol genCol pd))
        . dataColumn hvCol  (Numbers (numCol hvCol  pd))
        $ []
    , mark Line [MColor "#1F77B4", MStrokeWidth 2.5]
    , encoding
        . position X [PName genCol, PmType Quantitative, PAxis [AxTitle "Generation"]]
        . position Y [PName hvCol,  PmType Quantitative, PAxis [AxTitle "Hypervolume"]]
        $ []
    , width  (plotWidth  cfg)
    , height (plotHeight cfg)
    ]

hypervolumeHistoryFile :: OutputFormat -> FilePath -> PlotConfig
                       -> Text -> Text -> PlotData -> IO ()
hypervolumeHistoryFile fmt path cfg genCol hvCol pd =
  writeSpec fmt path (hypervolumeHistory cfg genCol hvCol pd)

-- ---------------------------------------------------------------------------
-- 推定 vs 真 front の比較 (2D)
-- ---------------------------------------------------------------------------

-- | Overlay two 2D fronts (typically estimated red over true grey
-- dashed). The @groupCol@ (text) splits 'PlotData' into the two layers
-- by category; the first distinct value gets the line style, the
-- second gets the points. If @groupCol@ has fewer than two distinct
-- values, falls back to a single point series.
paretoCompare :: PlotConfig
              -> (Text, Text)   -- ^ (xCol, yCol)
              -> Text           -- ^ group column (text; e.g. "true"/"estimated")
              -> PlotData
              -> VegaLite
paretoCompare cfg (xCol, yCol) gCol pd =
  toVegaLite
    [ title (plotTitle cfg) []
    , dataFromColumns []
        . dataColumn xCol (Numbers (numCol xCol pd))
        . dataColumn yCol (Numbers (numCol yCol pd))
        . dataColumn gCol (Strings (txtCol gCol pd))
        $ []
    , mark Point [MOpacity 0.85, MSize 60]
    , encoding
        . position X [PName xCol, PmType Quantitative, PAxis [AxTitle xCol]]
        . position Y [PName yCol, PmType Quantitative, PAxis [AxTitle yCol]]
        . color    [MName gCol, MmType Nominal,
                    MScale [SScheme "set1" []]]
        $ []
    , width  (plotWidth  cfg)
    , height (plotHeight cfg)
    ]

paretoCompareFile :: OutputFormat -> FilePath -> PlotConfig
                  -> (Text, Text) -> Text -> PlotData -> IO ()
paretoCompareFile fmt path cfg cols gCol pd =
  writeSpec fmt path (paretoCompare cfg cols gCol pd)
