# DataIO.Reshape — DataFrame 整形ヘルパ

> Hackage `dataframe` に欠けている操作を hanalyze 側で補完。
>
> - dataframe 既存: join (inner/left/right/full)、sort、filter、take、aggregate
> - hanalyze 追加: pivot_wider、one-hot encoding、lag/lead、rolling

## 1. pivot_wider

`meltLonger` の逆: long → wide 変形。

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
-- 列: [id, x, y]、行は id ごと
-- 欠損は NaN
```

## 2. one-hot encoding

```haskell
let df = DX.fromNamedColumns
           [ ("id", DX.fromList [1, 2, 3, 4, 5])
           , ("color", DX.fromList ["red", "blue", "red", "green", "blue"])
           ]

let dfEncoded = Reshape.oneHot False "color" df
-- 列: [id, color_red, color_blue, color_green]
-- 元 "color" 列は削除

-- 回帰用 (multicollinearity 回避)
let dfDrop = Reshape.oneHot True "color" df
-- 1 列分減る (最初のカテゴリを drop)
```

## 3. Lag / Lead (時系列)

```haskell
-- 1 期前の値を新列に
let df' = Reshape.lagColumn 1 "price" "price_lag1" df

-- 1 期後の値を
let df'' = Reshape.leadColumn 1 "price" "price_lead1" df

-- NaN padding (lag は先頭 k 個、lead は末尾 k 個)
```

## 4. Rolling window

```haskell
-- 窓 7 の移動平均
let df' = Reshape.rollingMean 7 "price" "price_ma7" df

-- 窓 7 の合計
let df'' = Reshape.rollingSum 7 "volume" "volume_sum7" df

-- 任意の集約関数
let df''' = Reshape.rollingApply 7 (\xs -> maximum xs - minimum xs)
              "price" "price_range7" df
```

## 5. Hackage dataframe との使い分け

| 操作 | Hackage dataframe | hanalyze (DataIO) |
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
