{-# LANGUAGE OverloadedStrings #-}
-- | Common plot configuration shared by every @Hanalyze.Viz.*@ module.
--
-- 'Hanalyze.Viz.Core' originally hosted 'PlotConfig' as a 3-field record
-- (title / width / height) so that the @writeSpec@ / @openInBrowser@
-- helpers had access to the basic geometry. As more downstream consumers
-- (notably HPotfire's Vega-Lite migration) needed colour scheme, facet
-- columns, legend placement, etc., the responsibility outgrew @Viz.Core@.
--
-- This module owns the canonical 'PlotConfig' definition and the
-- 'defaultConfig' constructor; @Viz.Core@ re-exports both for backwards
-- compatibility.
module Hanalyze.Viz.PlotConfig
  ( PlotConfig (..)
  , defaultConfig
  ) where

import Data.Text (Text)

-- | Common plot configuration. Existing required fields ('plotTitle' /
-- 'plotWidth' / 'plotHeight') are kept as-is for the in-tree call sites
-- that pre-date this module. New optional fields default to 'Nothing'
-- via 'defaultConfig' so adding fields here does not break callers that
-- only update the title or dimensions.
data PlotConfig = PlotConfig
  { plotTitle       :: Text
    -- ^ Plot title (mandatory; pass an empty string for an untitled chart).
  , plotWidth       :: Double
    -- ^ Plot width in pixels.
  , plotHeight      :: Double
    -- ^ Plot height in pixels.
  , plotColorScheme :: Maybe Text
    -- ^ Vega-Lite colour scheme name (e.g. @"viridis"@, @"category10"@).
  , plotFacetColumn :: Maybe Text
    -- ^ Column name to facet on (small multiples).
  , plotLegendPos   :: Maybe Text
    -- ^ Legend position (@"right"@ / @"bottom"@ / @"none"@ etc.).
  } deriving (Show)

-- | Default 600 × 400 'PlotConfig' with the given title; all optional
-- fields are 'Nothing'.
defaultConfig :: Text -> PlotConfig
defaultConfig t = PlotConfig
  { plotTitle       = t
  , plotWidth       = 600
  , plotHeight      = 400
  , plotColorScheme = Nothing
  , plotFacetColumn = Nothing
  , plotLegendPos   = Nothing
  }
