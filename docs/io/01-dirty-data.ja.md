# 汚いデータの読込ガイド (Phase A)

> 🌐 [English](01-dirty-data.md) | **日本語**

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

## Wide-form データの長尺化 (melt / pivot_longer)

「1 行 1 水準・列名が水準 (時刻 / 位置 / index 等)・歯抜けセル」という形の
CSV — 例えば `data/io/wide_sample.csv`:

```
name,x1,x2,1,2,3,4,5,6,7,8,9,10
a,1,0,1,,3,,5,,7,,9,
b,2,0,,4,,8,10,,14,,,20
c,3,0,0.1,0.2,,0.4,,0.6,,0.8,,1
d,4,0,,1,1.5,2,2.5,,,4,,5
e,5,0,3,,9,12,,18,,,,30
```

をそのまま列ごとの y にすると「歯抜け」+ 「t 間の連続性が失われる」の二重で
回帰に向きません。`DataIO.Preprocess.meltLonger` で long-form (tidy) に
ほどけば、列名 (1〜10) がそのまま新しい説明変数 t になり、NA セルは
「未観測サンプル」として自然に drop されます。

### CLI 例

```bash
# wide → long
hanalyze melt data/io/wide_sample.csv \
    --id name,x1,x2 \
    --vars 1,2,3,4,5,6,7,8,9,10 \
    --var t --value y \
    --output data/io/melted_sample.csv
# → 27 行 × 5 列 (name, x1, x2, t, y)、NA は自動 drop

# 多変量 RFF Ridge で「列に対して非線形」な関係を捉える
hanalyze kernel data/io/melted_sample.csv "x1 t" y \
    --method rff --features 200 --bandwidth 1.0 --lambda 0.001 \
    --group name --xaxis t \
    --out trash/rff_mv_plot.html \
    --report trash/rff_mv_report.html
# → R²=1.0000、横軸 t / 縦軸 y / 色 name の対話的散布図 + 予測曲線、
#    + ReportBuilder 統合 HTML レポート
```

### ライブラリ例

```haskell
import qualified DataIO.Preprocess as Pp
import qualified Model.RFF         as RFF
import qualified Viz.ReportBuilder as RB
import qualified Viz.ReportInstances as RI

main = do
  Right (df0, _) <- CSV.loadAutoSafe "data/io/wide_sample.csv"
  let df = Pp.meltLonger ["name", "x1", "x2"]
                         (map (T.pack . show) [1..10 :: Int])
                         "t" "y" True df0
  -- ... fit RFF ridge multivariate ...
  let rep = RI.RFFMVReport fit "name" "t"
      cfg = RB.defaultReportConfig "Multivariate RFF Ridge"
  RB.renderReport "out.html" cfg
    (RB.toReport cfg df ["x1", "t"] "y" rep)
```

### インタラクティブ予測 (--interactive)

`--interactive` を `--report` と一緒に渡すと、HTML 内の **副軸スライダ**
(横軸 `--xaxis` で指定した列以外) を動かすと、ブラウザ JS 側で RFF 特徴量を
再計算して予測曲線が即座に更新されます。RFF の重みと周波数を JSON で埋め込んで
おき、`φ_j(x_new) = σ_f √(2/D) cos(ω_jᵀx_new + b_j)` を JavaScript で評価する
仕組みです。

### 入力標準化 (`--standardize`) と HP 自動決定 (`--auto-hp`)

scale 差の大きい特徴 (energy 30–200 keV、dose 1e13–2e15 cm⁻²、z 0–200 nm
など) を共通長さスケール ℓ で扱うと精度が出ません。これを 2 つのフラグで
解決します。

| フラグ | 動作 |
|---|---|
| `--standardize` | fit 前に X を z-score 化 (`Stat.Standardize`)。予測 / プロット / インタラクティブ JS は raw 単位で受け取った値を逆算で標準化空間に変換してから RFF を評価 |
| `--auto-hp`     | `Model.RFF.maximizeMarginalLikRBFMV` で `(ℓ, σ_f, σ_n)` を周辺尤度最大化で自動決定。`--bandwidth` / `--lambda` は無視 (σ_n² が λ に対応) |

周辺尤度最大化のアルゴリズム:

1. K_ij = σ_f² · exp(-‖x_i - x_j‖² / (2ℓ²)) を厳密に構築
2. log p(y|θ) = -½yᵀ(K+σ_n²I)⁻¹y - ½log|K+σ_n²I| - n/2 log(2π) を Cholesky 経由で計算
3. (log ℓ, log σ_f, log σ_n) を log-spaced 20×8×8 グリッドで全評価
4. 最良点周辺で 1/3 幅の coarse-to-fine 1 段
5. 確定した ℓ で改めて RFF (D 個の ω) を sampling し、Ridge fit

コードは正攻法ですがグリッドベースで最適化は局所最適のリスクあり。n が小さい
うち (n ≤ 200 程度) は全 2560 点評価でも数秒で終わります。

### 例: 半導体ポテンシャル風データ

8 条件 (energy/dose) × 30 z 点 = 240 行のドーパント濃度プロファイル
(`data/io/potential_long.csv`)。物理係数 (B in Si 簡易):
`Rp(E) = 1.5·E^0.7`、`σ(E) = 0.4·Rp`、`N_peak = D / (√(2π)·σ)`。

```bash
# 生成器は git 管理外 (確認用)。再生成は cabal exe を一時的に追加して実行。

# 横軸 z 固定、energy / dose スライダで予測曲線が動く + 自動 HP + 標準化
hanalyze kernel data/io/potential_long.csv "energy dose z" y \
    --method rff --features 400 \
    --standardize --auto-hp \
    --group name --xaxis z \
    --out trash/potential_plot.html \
    --report trash/potential_report.html \
    --interactive
```

stdout 例:
```
  Standardize: ON
    μ = [105, 8.3e14, 63.7]
    σ = [60, 7.7e14, 47]
  Auto-HP: 周辺尤度最大化を実行中...
    ℓ = 0.41   (標準化空間)
    σ_f = 1.0e13
    σ_n = 6.1e11   (λ = σ_n² = 3.7e23)
    log_mlik = -7085   (2560 点評価)
RFF (multivariate) Ridge fit:
  R^2 = 0.9947
  RMSE = 8.3e11
```

ブラウザで `potential_report.html` を開くと、energy / dose のスライダが
**raw 単位** で表示され、操作するたびに JS が `(v-μ)/σ` で標準化空間に変換 →
予測曲線を再描画します。

### 残課題 — ARD-RFF (将来)

`--standardize` で全特徴を共通スケールに揃えた後でも、本当に「物理的に
重要な変数」と「ほぼ無関係な変数」を区別したい場合は、**列ごとに独立な
長さスケール ℓ_k** を持つ ARD (Automatic Relevance Determination) RFF が
有効。実装は `sampleRFFRBFMV` の引数を `[Double]` (各次元 ℓ) に拡張する
だけ、CV / 周辺尤度のグリッド次元数だけ計算量が増えます。標準化単独で
十分なケースは多いので、現状は ARD は未実装。

melt 後は通常の DataFrame なので原則どのモデルにも乗せられますが、複数
説明変数 (例 `"x1 t"`) を CLI / Model API 両方で扱えるかは個別:

| 多変量 OK | 1 変数のみ |
|---|---|
| LM / GLM / GLMM | GP (`Model.GP`) |
| Ridge / Lasso / ElasticNet | Spline (`Model.Spline`) |
| HBM | Kernel NW / KR (`Model.Kernel` の 1D 版) |
| GAM (各 feature を独立 spline で) | |
| Random Forest | |
| Quantile | |
| **RFF (`rffRidgeMV`、`hanalyze kernel ... --method rff`)** | |

GP / Spline / Kernel NW・KR の多変量化は将来の拡張候補。

## Phase C: クリーニング DSL (`DataIO.Clean`、完了)

通貨記号 / 桁区切り / 単位 / decimal point 違いなど、Phase A の Health
検査では警告止まりだった列を、明示的なルール適用で数値化できる。

### `ColumnRule`

| ルール | 例 | 結果 |
|---|---|---|
| `StripUnits`     | `"12.3kg"`    | `12.3`     |
| `ParseCurrency`  | `"$1,234.56"` | `1234.56`  |
| `ParseDecimalEU` | `"3,14"`      | `3.14`     |
| `TrimText`       | `"  abc  "`   | `"abc"`    |
| `CoerceNumeric`  | 上記いずれか  | 最初に成功した変換を採用 |

各ルールは I100〜I104 の Info コードを出し、成功率が 50% 未満なら追加で
`I*L` 警告を発して別ルールへの示唆を出します。

### ライブラリ利用

```haskell
import qualified DataIO.Clean as Clean
import           DataIO.Clean (ColumnRule (..), cleanPipeline)

(df', lg) = cleanPipeline
  [ ("price",  ParseCurrency)
  , ("weight", StripUnits)
  , ("price2", CoerceNumeric)  -- 万能変換
  ] df
Log.printLogReport lg
```

### CLI 利用 (`hanalyze clean`)

```bash
# 単位剥がし
hanalyze clean data/dirty/16_dates_units.csv \
    --rule weight=StripUnits \
    --rule length_cm=StripUnits

# 通貨記号 + 桁区切り
hanalyze clean data/dirty/08_thousands_currency.csv \
    --rule price=ParseCurrency

# 万能変換 (最初に成功したルールを採用)
hanalyze clean data/dirty/08_thousands_currency.csv \
    --rule price=CoerceNumeric
```

`hanalyze clean` も他の CLI と同じく `--no-header` / `--skip N` /
`--comment CH` / `--delim CH` / `--strict` / `--no-sniff` を併用できます。
