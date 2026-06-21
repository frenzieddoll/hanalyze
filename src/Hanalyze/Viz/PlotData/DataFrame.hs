{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | 'ToPlotData' instance for Hackage @dataframe@.
--
-- Kept in its own module so 'Hanalyze.Viz.PlotData' itself does not
-- depend on @dataframe@; future backends (Parquet streaming, SQL, ...)
-- can ship analogous adapter modules without touching the core.
--
-- The instance copies all numeric columns into @pdNumeric@ and all
-- (parseable) text columns into @pdText@. Columns that do not match
-- either projection are dropped silently — Vega specs only ever
-- reference columns by name, so missing columns surface as runtime
-- errors at the spec level rather than at conversion time.
module Hanalyze.Viz.PlotData.DataFrame
  ( -- Re-exports
    PlotData
  , ToPlotData (..)
  ) where

import qualified Data.Map.Strict          as Map
import           Data.Text                (Text)
import qualified Data.Vector              as V
import qualified DataFrame.Internal.DataFrame  as DX
import           Hanalyze.DataIO.Convert  (getDoubleVec, getTextVec)
import           Hanalyze.Viz.PlotData    (PlotData (..), ToPlotData (..),
                                           emptyPlotData)

instance ToPlotData DX.DataFrame where
  toPlotData df = case DX.columnNames df of
    [] -> emptyPlotData
    cs ->
      let pickNumeric n = (\v -> (n, v)) <$> getDoubleVec n df
          pickText    n = (\v -> (n, v)) <$> getTextVec   n df
          numCols       = [ c | Just c <- map pickNumeric cs ]
          txtCols       = [ c | Just c <- map pickText    cs ]
          rowLen        = maximum
                            (0 :  map (V.length . snd) numCols
                               ++ map (V.length . snd) txtCols)
      in PlotData
           { pdNumeric = Map.fromList numCols
           , pdText    = Map.fromList txtCols
           , pdLength  = rowLen
           }
