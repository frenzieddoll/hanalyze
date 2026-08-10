# hanalyze-frame

[`hanalyze`](../README.ja.md) の**データ入出力層**。 Hackage の
`dataframe` を唯一のデータ表現として採用し、 **汚いデータの読み込み**から
**tidyverse 流の整形**までを担う。

依存は `hanalyze-core` + `dataframe-*` / `cassava` / `regex-tdfa` 等。
`-bayes` と並んで core の直上に位置し、 上位の `-models` / `-design` /
`-viz` はすべてこの層のデータ表現を前提にする。

## 主要 module (全 14 module)

### 読み込み (`Hanalyze.DataIO.*`)

| Module | 役割 |
|---|---|
| `DataIO.CSV` | CSV / TSV / SSV を `DataFrame` として直接返すローダ群。 `loadAuto` が拡張子から判別。 `loadAutoSafe` は `Either` + ログを返す防衛版 |
| `DataIO.Sniff` | delimiter・comment 記号・header 有無・NA 候補を先頭 8KB から自動推測 |
| `DataIO.Health` | 読み込み済み `DataFrame` の疑わしいパターンを警告コード W001〜W008 として検出 |
| `DataIO.Clean` | Health の警告を数値化ルールへ変換する列単位クリーニング DSL |
| `DataIO.Log` | ローダ / 前処理が共有する構造化警告 (`LogEntry` / `LogReport`) |
| `DataIO.External` | Parquet / JSON ローダ (`dataframe` 経由) |
| `DataIO.Convert` | `DataFrame` から数値 / Text 列を安全に `Vector` へ抽出する変換層 |

### 整形 (`Hanalyze.Data.*` / `DataIO.*`)

| Module | 役割 |
|---|---|
| `Data.Wrangle` | dplyr 風の `summarise` / `mutate` / `groupBy` を `DataFrame` in/out で提供。 hgg の pipe 記法と対称設計 |
| `Data.Transform` | dplyr 流の順位・オフセット・累積・区間化を純粋な `[a] -> [b]` として提供 |
| `Data.Factor` | forcats 流の因子型と水準操作 (`fct_*` 相当) |
| `Data.Strings` | stringr 流の Text 純粋操作 (`str_*` 相当) |
| `Data.ColumnSource` | 「列名 → 数値列」 を引ける最小抽象型クラス (plot 非依存) |
| `DataIO.Reshape` | `dataframe` に無い reshape 操作 (pivotWider / oneHot / lag・lead / rolling) |
| `DataIO.Preprocess` | 欠損の検出・除去・補完 / 列選択 / 派生列 / melt |

## 単体で使う

```cabal
build-depends: hanalyze-frame, dataframe-core
```

```haskell
{-# LANGUAGE OverloadedStrings #-}
import           Hanalyze.DataIO.CSV  (loadAuto)
import           Hanalyze.Data.Wrangle
import           DataFrame.Operators         ((|>))

main = do
  Right df <- loadAuto "flights.csv"      -- IO (Either ParseError DataFrame)
  let out = df |> groupBy ["month"]
               |> summarise [ "mean" =: meanOf "dep_delay"
                            , "q95"  =: quantileOf 0.95 "dep_delay"
                            , "n"    =: nOf ]
  print out
  -- month |        mean        |        q95         |  n
  -- ------|--------------------|--------------------|----
  -- 1     | 4.5                | 11.549999999999999 | 3
  -- 2     | 11.166666666666666 | 23.25              | 3
```

集約子は既定で NA 除去 (dplyr の `na.rm = TRUE` 相当)、 群の並びはキー昇順。

通常は umbrella package `hanalyze` を依存に書けば
`import Hanalyze` からこれらも使える。 層を直接指定するのは
依存を最小化したいときのみで十分。

## 関連 docs

- 汚いデータ対策 (W001-W008 / auto-sniff / clean DSL):
  [docs/io/01-dirty-data.ja.md](../docs/io/01-dirty-data.ja.md)
- reshape (pivot_wider / one-hot / lag-lead / rolling):
  [docs/io/02-reshape.ja.md](../docs/io/02-reshape.ja.md)
- long-form 再グリッド: [docs/io/03-regrid.ja.md](../docs/io/03-regrid.ja.md)
- `df |-> model` 統一 fit API: [docs/io/04-fit-api.ja.md](../docs/io/04-fit-api.ja.md)

← [repository README](../README.ja.md)
