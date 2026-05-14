{-# LANGUAGE OverloadedStrings #-}
-- | Source-agnostic intermediate representation for plot data.
--
-- HPotfire and other downstream consumers want to feed data from a
-- variety of backends — Hackage @dataframe@, Parquet/Arrow, a SQL/REST
-- store, or in-memory @[Double]@ lists — into the same @*Spec@ functions
-- in @Hanalyze.Viz.*@. To avoid a hard dependency from @Viz@ on
-- @dataframe@ (and a future ripple if/when DB-backed sources land),
-- @Viz.*@ accepts only 'PlotData', and adapter modules ('toPlotData')
-- handle conversion at the boundary.
--
-- 'PlotData' is intentionally a /concrete/ record (not an opaque newtype
-- around a type class) so that:
--
--   * @Vega-Lite@ JSON serialisation can iterate columns directly;
--   * unit tests can construct fixtures without an instance dance;
--   * future backends only need a one-shot @toPlotData@ extraction.
--
-- The 'ToPlotData' class lets callers stay polymorphic when the source
-- type is uniform across a call site.
module Hanalyze.Viz.PlotData
  ( -- * Concrete intermediate type
    PlotData (..)
  , emptyPlotData
  , plotDataLength
  , plotDataColumns
  , numericColumn
  , textColumn
    -- * Construction helpers
  , fromNumericColumns
  , fromMixedColumns
    -- * Polymorphic boundary
  , ToPlotData (..)
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Text       (Text)
import qualified Data.Vector     as V

-- | A row-aligned, column-oriented snapshot of plot input.
--
-- Each column in 'pdNumeric' / 'pdText' MUST have the same length
-- ('pdLength'); 'fromNumericColumns' / 'fromMixedColumns' enforce this.
-- Columns may live in either the numeric or text map but not both.
data PlotData = PlotData
  { pdNumeric :: !(Map Text (V.Vector Double))
    -- ^ Numeric columns keyed by column name.
  , pdText    :: !(Map Text (V.Vector Text))
    -- ^ Text / categorical columns keyed by column name.
  , pdLength  :: !Int
    -- ^ Row count (invariant: equals every column's 'V.length').
  } deriving (Show)

-- | An empty 'PlotData' with zero rows.
emptyPlotData :: PlotData
emptyPlotData = PlotData Map.empty Map.empty 0

-- | Row count.
plotDataLength :: PlotData -> Int
plotDataLength = pdLength

-- | All column names (numeric + text), preserving 'Map' order
-- (alphabetical).
plotDataColumns :: PlotData -> [Text]
plotDataColumns pd = Map.keys (pdNumeric pd) ++ Map.keys (pdText pd)

-- | Look up a numeric column by name.
numericColumn :: Text -> PlotData -> Maybe (V.Vector Double)
numericColumn n = Map.lookup n . pdNumeric

-- | Look up a text column by name.
textColumn :: Text -> PlotData -> Maybe (V.Vector Text)
textColumn n = Map.lookup n . pdText

-- | Build a 'PlotData' from a list of numeric columns. All columns must
-- have the same length; an empty input yields 'emptyPlotData'.
fromNumericColumns :: [(Text, V.Vector Double)] -> PlotData
fromNumericColumns []   = emptyPlotData
fromNumericColumns cols =
  let n = V.length (snd (head cols))
  in if all ((== n) . V.length . snd) cols
       then PlotData
              { pdNumeric = Map.fromList cols
              , pdText    = Map.empty
              , pdLength  = n
              }
       else error "Hanalyze.Viz.PlotData.fromNumericColumns: \
                   \column lengths disagree"

-- | Build a 'PlotData' from a mix of numeric and text columns. All
-- columns must have the same length.
fromMixedColumns
  :: [(Text, V.Vector Double)]
  -> [(Text, V.Vector Text)]
  -> PlotData
fromMixedColumns numCols txtCols =
  let allLens =  map (V.length . snd) numCols
              ++ map (V.length . snd) txtCols
  in case allLens of
       []       -> emptyPlotData
       (n : ns) ->
         if all (== n) ns
           then PlotData
                  { pdNumeric = Map.fromList numCols
                  , pdText    = Map.fromList txtCols
                  , pdLength  = n
                  }
           else error "Hanalyze.Viz.PlotData.fromMixedColumns: \
                       \column lengths disagree"

-- | Adapter type class: anything that can be projected to 'PlotData'
-- (Hackage @dataframe@, future SQL row source, ...). Adapters live
-- alongside the source type to keep @Viz@ free of source dependencies;
-- e.g. @Hanalyze.Viz.PlotData.DataFrame@ provides the @ToPlotData@
-- instance for @DataFrame@.
class ToPlotData a where
  toPlotData :: a -> PlotData
