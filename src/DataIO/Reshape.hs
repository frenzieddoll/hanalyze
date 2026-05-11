{-# LANGUAGE OverloadedStrings #-}
-- | Data-frame reshaping helpers that are missing in Hackage
-- @dataframe@:
--
--   * 'pivotWider' — long → wide reshape (inverse of @meltLonger@).
--   * 'oneHot' — one-hot encoding of a categorical column.
--   * @lag@ / @lead@ — shift a numeric column for time-series feature
--     engineering.
--   * 'rollingMean' / 'rollingSum' — fixed-window rolling stats.
--
-- For @join@, @sortBy@, @meltLonger@ etc., use the upstream
-- @DataFrame@ API directly — those are first-class there.
module DataIO.Reshape
  ( pivotWider
  , oneHot
  , lagColumn
  , leadColumn
  , rollingMean
  , rollingSum
  , rollingApply
  ) where

import qualified Data.Text             as T
import qualified Data.Vector           as V
import qualified DataFrame             as DX
import qualified DataFrame.Internal.DataFrame as DXD
import qualified DataIO.Convert        as Conv
import           Data.Maybe            (fromMaybe)
import qualified Data.Set              as Set

-- ---------------------------------------------------------------------------
-- Pivot wider
-- ---------------------------------------------------------------------------

-- | Reshape a long-form DataFrame into wide form. Inverse of
-- @meltLonger@.
--
-- Given:
--
--   * a DataFrame with rows like @(id, name, value)@,
--   * @namesFrom@ = the column whose distinct values become new
--     column names,
--   * @valuesFrom@ = the column holding the values to spread,
--   * @idCols@ = identifier columns kept as the row key.
--
-- Produces a DataFrame where each unique value of @namesFrom@ becomes
-- a new column. Missing combinations are filled with NaN (as Double).
--
-- Example: long-form @[(1, "x", 10), (1, "y", 20), (2, "x", 30)]@ →
-- wide-form @[(1, 10, 20), (2, 30, NaN)]@ with columns
-- @[id, x, y]@.
pivotWider
  :: [T.Text]            -- ^ Identifier columns.
  -> T.Text              -- ^ Column with new column names (@namesFrom@).
  -> T.Text              -- ^ Column with values to spread (@valuesFrom@).
  -> DXD.DataFrame
  -> DXD.DataFrame
pivotWider idCols namesFrom valuesFrom df =
  let nameVec    = fromMaybe (error ("pivotWider: column '"
                                      ++ T.unpack namesFrom
                                      ++ "' not found"))
                     (Conv.getTextVec namesFrom df)
      valueVec   = fromMaybe (error ("pivotWider: column '"
                                      ++ T.unpack valuesFrom
                                      ++ "' not found"))
                     (Conv.getDoubleVec valuesFrom df)
      n          = V.length nameVec
      -- Distinct names (preserves order of first appearance).
      distinct   = orderedUnique (V.toList nameVec)
      -- Get id-column values per row as a tuple key.
      idColVecs  = [ fromMaybe (error ("pivotWider: id col '"
                                        ++ T.unpack c ++ "' not found"))
                       (Conv.getTextVec c df
                          `mappendMaybe`
                        fmap (V.map (T.pack . show))
                          (Conv.getDoubleVec c df))
                   | c <- idCols ]
      -- Group rows by id-key.
      keyOf i    = [vec V.! i | vec <- idColVecs]
      keys       = orderedUnique [keyOf i | i <- [0..n-1]]
      -- For each (key, name) compute the value (NaN if missing).
      lookup1 key name =
        let matching = [ V.unsafeIndex valueVec i
                       | i <- [0..n-1]
                       , keyOf i == key
                       , V.unsafeIndex nameVec i == name
                       ]
        in case matching of
             []    -> 0/0  -- NaN
             (v:_) -> v
      -- Build wide DataFrame.
      keyToTexts k = k                      -- already [Text]
      idCols' = [ (c, V.fromList [V.unsafeIndex (idColVecs !! ci) i
                                  | i <- rowIndices])
                | (ci, c) <- zip [0..] idCols ]
      rowIndices = [ head [i | i <- [0..n-1], keyOf i == k] | k <- keys ]
      _ = idCols'
      _ = keyToTexts
      -- Wide columns.
      wideCols   = [ (name,
                      V.fromList [lookup1 k name | k <- keys])
                   | name <- distinct ]
      -- Build via DX.fromList so the dataframe knows column types.
      idColData  = [ (c, DX.fromList (V.toList (V.fromList
                                                  [V.unsafeIndex (idColVecs !! ci) i
                                                  | i <- rowIndices])))
                   | (ci, c) <- zip [0..] idCols ]
      wideColData = [ (name,
                       DX.fromList (V.toList vs))
                    | (name, vs) <- wideCols ]
  in DX.fromNamedColumns (idColData ++ wideColData)

-- | Append the second 'Maybe' as a fallback if the first is Nothing.
mappendMaybe :: Maybe a -> Maybe a -> Maybe a
mappendMaybe (Just x) _ = Just x
mappendMaybe Nothing y  = y

-- ---------------------------------------------------------------------------
-- One-hot encoding
-- ---------------------------------------------------------------------------

-- | One-hot encode a categorical text column. Returns a DataFrame
-- with the original column dropped and one new 0/1 indicator column
-- per category (named "@<col>_<category>@").
--
-- @dropFirst@ controls whether to omit the first category (= drop
-- redundant column for use in regression to avoid multicollinearity).
oneHot
  :: Bool             -- ^ Drop first category?
  -> T.Text           -- ^ Categorical column name.
  -> DXD.DataFrame
  -> DXD.DataFrame
oneHot dropFirst colName df =
  let vec      = fromMaybe (error ("oneHot: column '"
                                    ++ T.unpack colName ++ "' not found"))
                   (Conv.getTextVec colName df)
      n        = V.length vec
      cats     = orderedUnique (V.toList vec)
      keep     = if dropFirst then drop 1 cats else cats
      indicator c =
        DX.fromList [ if V.unsafeIndex vec i == c then (1 :: Double)
                                                  else 0
                    | i <- [0..n-1] ]
      newCols  = [(colName <> "_" <> c, indicator c) | c <- keep]
      withoutOrig = DX.exclude [colName] df
  in foldr (\(name, col) d -> DX.insertColumn name col d)
           withoutOrig newCols

-- ---------------------------------------------------------------------------
-- Lag / Lead
-- ---------------------------------------------------------------------------

-- | Shift a numeric column @k@ positions forward (lag). The first @k@
-- entries become NaN. Useful for time-series feature engineering.
lagColumn
  :: Int              -- ^ k (positive).
  -> T.Text           -- ^ Source column.
  -> T.Text           -- ^ Output column name.
  -> DXD.DataFrame
  -> DXD.DataFrame
lagColumn k src out df =
  let vec = fromMaybe (error ("lagColumn: column '"
                               ++ T.unpack src ++ "' not found"))
              (Conv.getDoubleVec src df)
      n   = V.length vec
      shifted = V.fromList
        [ if i < k then 0/0
            else V.unsafeIndex vec (i - k)
        | i <- [0..n-1] ]
  in DX.insertColumn out (DX.fromList (V.toList shifted)) df

-- | Shift a numeric column @k@ positions backward (lead). The last
-- @k@ entries become NaN.
leadColumn
  :: Int
  -> T.Text
  -> T.Text
  -> DXD.DataFrame
  -> DXD.DataFrame
leadColumn k src out df =
  let vec = fromMaybe (error ("leadColumn: column '"
                               ++ T.unpack src ++ "' not found"))
              (Conv.getDoubleVec src df)
      n   = V.length vec
      shifted = V.fromList
        [ if i + k >= n then 0/0
            else V.unsafeIndex vec (i + k)
        | i <- [0..n-1] ]
  in DX.insertColumn out (DX.fromList (V.toList shifted)) df

-- ---------------------------------------------------------------------------
-- Rolling window
-- ---------------------------------------------------------------------------

-- | Rolling mean with a fixed window size. The first @(window-1)@
-- entries are NaN.
rollingMean
  :: Int              -- ^ Window size.
  -> T.Text           -- ^ Source column.
  -> T.Text           -- ^ Output column.
  -> DXD.DataFrame
  -> DXD.DataFrame
rollingMean win src out =
  rollingApply win mean src out
  where
    mean xs = sum xs / fromIntegral (length xs)

-- | Rolling sum with a fixed window size.
rollingSum
  :: Int
  -> T.Text
  -> T.Text
  -> DXD.DataFrame
  -> DXD.DataFrame
rollingSum win = rollingApply win sum

-- | Apply an arbitrary aggregation @f :: [Double] -> Double@ over a
-- rolling window. The first @(window-1)@ entries become NaN.
rollingApply
  :: Int
  -> ([Double] -> Double)
  -> T.Text
  -> T.Text
  -> DXD.DataFrame
  -> DXD.DataFrame
rollingApply win f src out df =
  let vec = fromMaybe (error ("rollingApply: column '"
                               ++ T.unpack src ++ "' not found"))
              (Conv.getDoubleVec src df)
      n   = V.length vec
      results = V.fromList
        [ if i + 1 < win then 0/0
            else f [V.unsafeIndex vec (i - win + 1 + j) | j <- [0..win-1]]
        | i <- [0..n-1] ]
  in DX.insertColumn out (DX.fromList (V.toList results)) df

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

-- | Distinct values, preserving order of first appearance.
orderedUnique :: Ord a => [a] -> [a]
orderedUnique = go Set.empty
  where
    go _    []     = []
    go seen (x:xs)
      | Set.member x seen = go seen xs
      | otherwise         = x : go (Set.insert x seen) xs
