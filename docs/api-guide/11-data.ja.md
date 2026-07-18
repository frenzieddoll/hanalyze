# データ操作・Fit API

> 🌐 [English](11-data.md) | **日本語**

> [📚 索引](README.ja.md) ｜ [01 quickstart](01-quickstart.ja.md) ｜ [02 regression](02-regression.ja.md) ｜ [03 bayesian-hbm](03-bayesian-hbm.ja.md) ｜ [04 multivariate](04-multivariate.ja.md) ｜ [05 ml](05-ml.ja.md) ｜ [06 timeseries](06-timeseries.ja.md) ｜ [07 survival](07-survival.ja.md) ｜ [08 causal](08-causal.ja.md) ｜ [09 doe](09-doe.ja.md) ｜ [10 stat](10-stat.ja.md) ｜ **11 data** ｜ [12 plot](12-plot.ja.md)

データ源の読み込み・整形・dplyr 流変換と、 そこから直接 fit する Fit API (`|->`)。 詳しい
作例は [io/04-fit-api](../io/04-fit-api.ja.md) ・ [io/01-dirty-data](../io/01-dirty-data.ja.md) を参照。

| 領域 | モジュール | 主な API |
|---|---|---|
| Fit API | `Hanalyze.Plot` | `(\|->)` / `(\|->!)` (列名でモデル fit) |
| ベクトル変換 (dplyr 流) | `Data.Transform` | `minRank` / `lag` / `cumsum` / `cut` … |
| DataFrame 解析動詞 | `Data.Wrangle` | `summarise` / `mutate` / `groupBy` |
| 読み込み | `DataIO.CSV` / `DataIO.External` | `loadAuto` / `loadParquet` / `loadJSON` |
| 前処理・整形 | `DataIO.Clean` / `DataIO.Preprocess` | `cleanPipeline` / `meltLonger` / `imputeMean` |

---

## Fit API (`|->`)

任意の `ColumnSource`(assoc list / Hackage `DataFrame` / `Map`)から列名でモデルを当てる。

```haskell
(|->)  :: (ColumnSource d, Fit spec) => d -> spec -> Fitted spec   -- 純粋・決定的
(|->!) :: (ColumnSource d, Fit spec) => d -> spec -> IO (Fitted spec)   -- IO 版 (進捗バー・結果ビット一致)
```

```haskell
let fit = df |-> lm "x" "y"       -- 02 regression の 9 spec 動詞
```

正本は [io/04-fit-api](../io/04-fit-api.ja.md) ([12 plot](12-plot.ja.md) で描画へ)。

---

## ベクトル変換 (`Data.Transform`)

純粋 `[a]→[b]`(DataFrame/IO 非依存)の dplyr 流関数。

```haskell
minRank, denseRank, rowNumber :: Ord a => [a] -> [Int]   -- 順位 (tie 法は dplyr 準拠)
percentRank, cumeDist         :: Ord a => [a] -> [Double]
lag, lead                     :: Int -> a -> [a] -> [a]  -- オフセット (埋め値付き)
cumsum, cumprod               :: Num a => [a] -> [a]     -- 累積和・積
cummin, cummax                :: Ord a => [a] -> [a]     -- 累積最小・最大
cummean                       :: [Double] -> [Double]
cut                           :: [Double] -> [Double] -> [Maybe Int]  -- 区間化 (right=TRUE)
consecutiveId                 :: Eq a => [a] -> [Int]
```

→ R4DS Ch13 の定番。 [io/04-fit-api](../io/04-fit-api.ja.md) 等の作例で使用。

---

## DataFrame 解析動詞 (`Data.Wrangle`)

plot の `df |>> layer …` と対称に、 DataFrame から直接 dplyr 風に集約・変換。

```haskell
class Summarisable g where
  summarise :: [(Text, Agg)] -> g -> DF.DataFrame
groupBy :: [Text] -> DF.DataFrame -> Grouped
mutate  :: [(Text, ColExpr)] -> DF.DataFrame -> DF.DataFrame
```

```haskell
-- 列名 =: 集約子 で書く (=: :: Text -> Agg -> (Text, Agg))
summarise [ "n" =: nOf, "q95" =: quantileOf 0.95 "y" ] (groupBy ["g"] df)
```

戻り値が `DataFrame` なので `df |>> layer …` / `df |-> spec` / 本動詞へ連鎖できる。
集約子 (`meanOf` / `quantileOf` / `nOf` / `nDistinctOf` 等) は `Stat.Descriptive` に委譲。 → [Stat](10-stat.ja.md)

---

## 読み込み・前処理 (`DataIO`)

```haskell
loadAuto     :: FilePath -> IO DF.DataFrame          -- CSV/TSV/SSV 自動判別 (cassava)
loadParquet  :: FilePath -> IO DF.DataFrame          -- DataIO.External
cleanPipeline :: [(Text, ColumnRule)] -> DF.DataFrame -> (DF.DataFrame, LogReport)   -- 列クリーニング DSL
meltLonger   :: [Text] -> [Text] -> Text -> Text -> Bool -> DF.DataFrame -> DF.DataFrame   -- wide → long (id列, 値列, var名, value名, var を Double parse)
imputeMean   :: Text -> DF.DataFrame -> Maybe DF.DataFrame                            -- 欠損補完 (列名)
```

読み込んだ `DataFrame` はそのまま `ColumnSource` なので `df |-> spec` / `df |>> layer …` に渡せる。
→ [io/01-dirty-data](../io/01-dirty-data.ja.md) / [io/02-reshape](../io/02-reshape.ja.md)
