# 汚いデータの読込ガイド (Phase A)

実務の CSV はほぼ確実にどこか壊れています。`hanalyze` は壊れ方を **警告コード**
として明示し、**`LoadOpts`** で選択的に修復できる設計です。本書は典型的な
壊れ方とその対処を fixture 別に解説します。

## ロード API の使い分け

| API | 戻り値 | 用途 |
|---|---|---|
| `loadAuto` | `IO (Either ParseError DXD.DataFrame)` | 整形済データ向け、最もシンプル。健全性検査なし |
| `loadAutoSafe` | `IO (Either ParseError (Loaded DXD.DataFrame))` | 防衛的に開く。例外をすべて `Left` 化、ロード後に `inspectWithPreview` を自動実行 |
| `loadAutoSafeWith opts` | 同上 | `LoadOpts` で skip / comment / no-header / strict を指定可 |

`Loaded a = (a, LogReport)` で値とログがペアになって返ります。

```haskell
import DataIO.CSV (loadAutoSafeWith, defaultLoadOpts, LoadOpts (..))
import qualified DataIO.Log as Log

Right (df, lg) <- loadAutoSafeWith
                    (defaultLoadOpts { loSkip = 3 })
                    "noisy.csv"
Log.printLogReport lg
-- df を後段の Model.* / Viz.* にそのまま渡せる
```

## 警告コード一覧

| Code | 意味 | 検出元 |
|---|---|---|
| W001 | 列名が全て数値 → ヘッダ行が無いファイル疑い | `inspectDataFrame` |
| W002 | 先頭付近に `#` / `!` 始まりの行 | `inspectWithPreview` |
| W003 | 列ごとの非 null セル数に乖離 (= ragged) | `inspectDataFrame` |
| W004 | 列名の重複 / 空 / 前後空白 / 列数不一致 | `inspectDataFrame` + `inspectWithPreview` |
| W005 | 1 列 DataFrame + 生データに `;` / `\t` / `|` が頻出 = delimiter ミスマッチ | `inspectWithPreview` |
| W006 | NA 表現 (NA / null / n/a / - / 空 等) が同列に複数種混在 | `inspectDataFrame` |
| W007 | text 列に「数字 + 単位」(`12.3kg` 等) が過半 | `inspectDataFrame` |
| W008 | text 列に通貨記号 / 桁区切りつき数値 (`$1,234.56`) が過半 | `inspectDataFrame` |

加えて I010 / I011 / I012 は `LoadOpts` での前処理 (skip / comment / no-header)
履歴を表す Info コードです。

## 19 fixture と推奨対応

`data/dirty/` 以下に下記 19 ファイルがあります。`cabal run dirty-data-demo`
で全ファイルを順に読み、出るコードを一覧表示できます。

| Fixture | 症状 | 期待 W コード | 推奨対応 |
|---|---|---|---|
| `01_clean.csv` | 健全 | (none) | — |
| `02_no_header.csv` | ヘッダ無し | W001 | `--no-header` |
| `03_preamble.csv` | コメント 3 行 | W002 | `--skip 3` または `--comment '#'` |
| `04_ragged.csv` | 列数バラつき | W003 (rows >= 6 で発火) | CSV を整形 |
| `05_dup_header.csv` | 重複列名 `x,y,x` | W004 ×2 | CSV を修正 |
| `06_blank_unnamed.csv` | 空列名 `x,,y,` | W004 ×4 | CSV を修正 |
| `07_mixed_na.csv` | NA / null / n/a / - / 空 | W003 + W006 | `imputeMean` 等で正規化 |
| `08_thousands_currency.csv` | `$1,234.56` 等 | W008 | Phase C `parseCurrency` 予定 |
| `09_quotes_commas.csv` | RFC 4180 quote escape | (none) | 正常処理 |
| `10_bom.csv` | UTF-8 BOM | (none) | 自動 strip |
| `11_semicolon_eu.csv` | `;` 区切り EU | W005 | (Phase B sniff 予定) |
| `12_real.tsv` | TSV (拡張子正しい) | (none) | — |
| `13_crlf.csv` | tab + `.csv` 拡張子 + CRLF | W005 | 拡張子修正 or Phase B |
| `14_wrong_ext.csv` | 同上 | W005 | 同上 |
| `15_trailing_blank.csv` | 末尾空行 | (none) | 自動 |
| `16_dates_units.csv` | `5kg` / `11.5cm` | W007 | Phase C `stripUnits` 予定 |
| `17_empty.csv` | 完全空 | Left | — |
| `18_header_only.csv` | ヘッダのみ | Left | — |
| `19_whitespace.csv` | セル前後の空白 | (none) | 自動 trim |

## CLI からの修復例

全サブコマンド (`info` / `regress` / `hist` / `taguchi analyze` / `ridge` /
`kernel` / `spline` / `quantile` / `gam` / `rf`) で同じフラグが使えます。

```bash
# ヘッダ無し → col0/col1 を生成して回帰
hanalyze regress --no-header data/dirty/02_no_header.csv col0 col1 LM

# コメント 3 行を skip して回帰
hanalyze regress --skip 3 data/dirty/03_preamble.csv x y LM

# strict: 警告が出たら止める (CI 用)
hanalyze regress --strict data/raw.csv x y LM
```

## ライブラリ利用例 (LogReport を取り回す)

```haskell
import DataIO.CSV    (loadAutoSafe)
import DataIO.Log    (entries, hasWarnings, lgCode)
import qualified DataIO.Log as Log

main :: IO ()
main = do
  Right (df, lg) <- loadAutoSafe "raw.csv"
  Log.printLogReport lg
  -- 警告ありなら停止
  when (hasWarnings lg && wantStrict) $
    error ("aborted: " ++ show (map lgCode (entries lg)))
  -- 通常通り処理
  ...
```

## Phase B: 自動推論 (`DataIO.Sniff`、完了)

冒頭 8 KB を読んで、ユーザが明示指定しなかった項目を自動補完する。
- delimiter (`,;\t|` から variance 昇順 + median 降順で選定)
- コメント文字 (`#` / `!` 始まりの先頭連続行)
- ヘッダ有無 (1 行目が全て numeric token なら無し判定)

`LoadOpts.loSniff` のデフォルトは `True`。`--no-sniff` で切れる。
sniff の結果は `I013` Info コードとしてログに残るので、何が自動修復されたか
常に追跡できる。これにより `data/dirty/{02,03,11,13,14}` は引数無しで
そのまま読めるようになった (5/19 → 14/19 がデフォルトで正常読込)。

```bash
# どれも引数無しで読める (sniff 自動推論)
hanalyze info data/dirty/02_no_header.csv
hanalyze info data/dirty/03_preamble.csv
hanalyze info data/dirty/11_semicolon_eu.csv
```

## Phase C 予告

`DataIO.Clean` で `stripUnits` / `parseCurrency` / `parseDate` /
`coerceNumeric` などの列変換を提供し、W007 (#16) / W008 (#08) を CLI から
ワンコマンドで救済する予定。
