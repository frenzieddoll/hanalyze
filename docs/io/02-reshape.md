# DataIO.Reshape — DataFrame Shaping Helpers

> 🌐 **English** | [日本語](02-reshape.ja.md)

> Complements missing operations from Hackage `dataframe`.
>
> - dataframe built-in: join (inner/left/right/full), sort, filter, take, aggregate
> - hanalyze additions: pivot_wider, one-hot encoding, lag/lead, rolling

## 1. pivot_wider

Inverse of `meltLonger`: long → wide transformation.

```haskell
import qualified Hanalyze.DataIO.Reshape as Reshape
import qualified DataFrame as DX

-- long-form
let df = DX.fromNamedColumns
           [ ("id",    DX.fromList [1, 1, 2, 2, 3])
           , ("name",  DX.fromList ["x", "y", "x", "y", "x"])
           , ("value", DX.fromList [10.0, 20, 30, 40, 50])
           ]

let wide = Reshape.pivotWider ["id"] "name" "value" df
-- columns: [id, x, y], rows grouped by id
-- missing values are NaN
```

## 2. One-hot encoding

```haskell
let df = DX.fromNamedColumns
           [ ("id", DX.fromList [1, 2, 3, 4, 5])
           , ("color", DX.fromList ["red", "blue", "red", "green", "blue"])
           ]

let dfEncoded = Reshape.oneHot False "color" df
-- columns: [id, color_red, color_blue, color_green]
-- original "color" column is removed

-- for regression (avoid multicollinearity)
let dfDrop = Reshape.oneHot True "color" df
-- one less column (first category dropped)
```

## 3. Lag / Lead (time series)

```haskell
-- previous period value in new column
let df' = Reshape.lagColumn 1 "price" "price_lag1" df

-- next period value in new column
let df'' = Reshape.leadColumn 1 "price" "price_lead1" df

-- NaN padding (lag at head k rows, lead at tail k rows)
```

## 4. Rolling window

```haskell
-- 7-period moving average
let df' = Reshape.rollingMean 7 "price" "price_ma7" df

-- 7-period sum
let df'' = Reshape.rollingSum 7 "volume" "volume_sum7" df

-- arbitrary aggregation function
let df''' = Reshape.rollingApply 7 (\xs -> maximum xs - minimum xs)
              "price" "price_range7" df
```

## 5. How to choose between Hackage dataframe and hanalyze (DataIO)

| Operation | Hackage dataframe | hanalyze (DataIO) |
|---|---|---|
| select / exclude | ✅ `DX.select` / `DX.exclude` | — |
| filter | ✅ `DX.filterWhere` | — |
| sort | ✅ `DX.sortBy` | — |
| join (inner/left/...) | ✅ `DX.innerJoin` etc. | — |
| aggregate / groupBy | ✅ `DX.groupBy` + `aggregate` | — |
| pivot_longer | — | ✅ `Hanalyze.DataIO.Preprocess.meltLonger` |
| **pivot_wider** | — | ✅ `Reshape.pivotWider` |
| **one-hot** | — | ✅ `Reshape.oneHot` |
| **lag/lead** | — | ✅ `Reshape.lagColumn` / `leadColumn` |
| **rolling** | — | ✅ `Reshape.rollingMean` etc. |
| imputation | — | ✅ `Hanalyze.DataIO.Preprocess.imputeMean` etc. |
