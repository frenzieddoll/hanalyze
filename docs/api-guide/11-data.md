# Data Manipulation & Fit API

> 🌐 **English** | [日本語](11-data.ja.md)

> [📚 Index](README.md) | [01 quickstart](01-quickstart.md) | [02 regression](02-regression.md) | [03 bayesian-hbm](03-bayesian-hbm.md) | [04 multivariate](04-multivariate.md) | [05 ml](05-ml.md) | [06 timeseries](06-timeseries.md) | [07 survival](07-survival.md) | [08 causal](08-causal.md) | [09 doe](09-doe.md) | [10 stat](10-stat.md) | **11 data** | [12 plot](12-plot.md)

Data source loading, reshaping, dplyr-style transformation, and the direct-fit Fit API (`|->`). Detailed examples: [io/04-fit-api](../io/04-fit-api.md) and [io/01-dirty-data](../io/01-dirty-data.md).

| Domain | Module | Main API |
|---|---|---|
| Fit API | `Hanalyze.Plot` | `(\|->)` / `(\|->!)` (fit models by column name) |
| Vector Transforms (dplyr-style) | `Data.Transform` | `minRank` / `lag` / `cumsum` / `cut` … |
| DataFrame Analytics Verbs | `Data.Wrangle` | `summarise` / `mutate` / `groupBy` |
| Loading | `DataIO.CSV` / `DataIO.External` | `loadAuto` / `loadParquet` / `loadJSON` |
| Preprocessing & Reshaping | `DataIO.Clean` / `DataIO.Preprocess` | `cleanPipeline` / `meltLonger` / `imputeMean` |

---

## Fit API (`|->`)

Fit any model to a `ColumnSource` (assoc list / Hackage `DataFrame` / `Map`) using column names.

```haskell
(|->)  :: (ColumnSource d, Fit spec) => d -> spec -> Fitted spec   -- pure, deterministic
(|->!) :: (ColumnSource d, Fit spec) => d -> spec -> IO (Fitted spec)   -- IO version (progress bar, result bit-match)
```

```haskell
let fit = df |-> lm "x" "y"       -- 9 spec verbs from 02 regression
```

Primary documentation: [io/04-fit-api](../io/04-fit-api.md) ([12 plot](12-plot.md) for visualization).

---

## Vector Transforms (`Data.Transform`)

Pure `[a]→[b]` dplyr-style functions (DataFrame/IO-independent).

```haskell
minRank, denseRank, rowNumber :: Ord a => [a] -> [Int]   -- ranking (tie handling follows dplyr)
percentRank, cumeDist         :: Ord a => [a] -> [Double]
lag, lead                     :: Int -> a -> [a] -> [a]  -- offset (with fill value)
cumsum, cumprod               :: Num a => [a] -> [a]     -- cumulative sum, product
cummin, cummax                :: Ord a => [a] -> [a]     -- cumulative min, max
cummean                       :: [Double] -> [Double]
cut                           :: [Double] -> [Double] -> [Maybe Int]  -- binning (right=TRUE)
consecutiveId                 :: Eq a => [a] -> [Int]
```

→ Standard functions from R4DS Ch13. Used in examples in [io/04-fit-api](../io/04-fit-api.md) etc.

---

## DataFrame Analytics Verbs (`Data.Wrangle`)

Mirror the plot `df |>> layer …` pattern: aggregate and transform directly from DataFrame using dplyr-style syntax.

```haskell
class Summarisable g where
  summarise :: [(Text, Agg)] -> g -> DF.DataFrame
groupBy :: [Text] -> DF.DataFrame -> Grouped
mutate  :: [(Text, ColExpr)] -> DF.DataFrame -> DF.DataFrame
```

```haskell
-- write column_name =: aggregator (=: :: Text -> Agg -> (Text, Agg))
summarise [ "n" =: nOf, "q95" =: quantileOf 0.95 "y" ] (groupBy ["g"] df)
```

Return value is `DataFrame`, so chains with `df |>> layer …` / `df |-> spec` / verbs. Aggregators (`meanOf` / `quantileOf` / `nOf` / `nDistinctOf` etc) delegate to `Stat.Descriptive`. → [Stat](10-stat.md)

---

## Loading & Preprocessing (`DataIO`)

```haskell
loadAuto     :: FilePath -> IO DF.DataFrame          -- auto-detect CSV/TSV/SSV (cassava)
loadParquet  :: FilePath -> IO DF.DataFrame          -- DataIO.External
cleanPipeline :: [(Text, ColumnRule)] -> DF.DataFrame -> (DF.DataFrame, LogReport)   -- column cleaning DSL
meltLonger   :: [Text] -> [Text] -> Text -> Text -> Bool -> DF.DataFrame -> DF.DataFrame   -- wide → long (id cols, value cols, var name, value name, parse var as Double)
imputeMean   :: Text -> DF.DataFrame -> Maybe DF.DataFrame                            -- impute missing (by column name)
```

Loaded `DataFrame` is a `ColumnSource` directly, so pass to `df |-> spec` / `df |>> layer …`.
→ [io/01-dirty-data](../io/01-dirty-data.md) / [io/02-reshape](../io/02-reshape.md)
