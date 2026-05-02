# HTML レポート — `Viz.ReportBuilder` と `Reportable`

> 🌐 [English](02-report-builder.md) | **日本語**

> 関連: [01-visualization.ja.md](01-visualization.ja.md) (棒グラフ・ヒストグラム等の単発プロット),
> `Viz.AnalysisReport` (LM/GLM/GLMM/GP/HBM 専用の詳細レポート)

## 目次

1. [概要・設計思想](#1-概要設計思想)
2. [Section 型と smart constructors](#2-section-型と-smart-constructors)
3. [`renderReport` でレポート生成](#3-renderreport-でレポート生成)
4. [`Reportable` 型クラス](#4-reportable-型クラス)
5. [モデル別の使用例](#5-モデル別の使用例)
6. [CLI からの利用](#6-cli-からの利用)
7. [カスタムレポートの作り方](#7-カスタムレポートの作り方)
8. [既存 `Viz.AnalysisReport` との関係](#8-既存-vizanalysisreport-との関係)
9. [よくあるパターンと落とし穴](#9-よくあるパターンと落とし穴)

---

## 1. 概要・設計思想

`Viz.ReportBuilder` は **コンポジション型 HTML レポートビルダ**。
分析結果を **section の並び** として組み立て、自己完結 HTML
(Vega-Lite アセット込み) を 1 ファイルで出力する。

### 設計原則

| 原則 | 内容 |
|---|---|
| **コンポジション** | `[ReportSection]` を構築するだけ。各セクションは独立した HTML チャンク。|
| **フォーマット非依存** | 内部で Vega-Lite spec を JSON 化し、HTML テンプレに埋め込む。Mermaid DAG も対応。|
| **モデル拡張容易** | 新しいモデル/分析を追加したら `Reportable` instance を 1 つ書くだけ。|
| **既存資産の流用** | `hvega` (Vega-Lite spec)、`Viz.Assets` (オフライン JS バンドル) を再利用。|
| **既存 AnalysisReport と並存** | LM/GLM/GLMM/GP/HBM の詳細レポートは従来の `Viz.AnalysisReport` に残す。新規モデル (ridge/kernel/spline/RFF/RobustGP/quantile/gam/rf) は ReportBuilder で対応。|

### なぜ別モジュールが必要だったか

既存の `Viz.AnalysisReport` (~2000 行) は 5 種類のモデル
(`ModelFit = RegFit | MixFit | GPFit | HBMFit | NoRegFit`) に
特化した sum-type ベースで、新モデルを増やすたびに本体を編集する必要があった。
ここに `ridge / kernel / spline / RFF / RobustGP / quantile / gam / rf` の
8 種類を追加するのは現実的ではないため、**section 単位のレゴ式** に切り替えた。

---

## 2. Section 型と smart constructors

`ReportSection` は HTML 1 セクションを表す sum type:

```haskell
data ReportSection
  = SecDataOverview DataFrame [Text] Text                  -- データの統計
  | SecModelOverview Text Text (Maybe Text)                -- モデル種別 / 式 / Mermaid
  | SecCoefficients [(Text, Double)] (Maybe (Text, Double))  -- 係数表 + R²
  | SecFitScatter Text Text [Double] [Double] (Maybe SmoothCurve)  -- 散布+曲線
  | SecResiduals [Double] [Double]                         -- fitted vs residual
  | SecBarChart Text [(Text, Double)]                      -- バーチャート
  | SecVega Text VegaLite                                  -- 任意 Vega-Lite
  | SecMermaid Text                                        -- Mermaid DAG
  | SecTable Text [Text] [[Text]]                          -- ヘッダ + 行
  | SecKeyValue Text [(Text, Text)]                        -- 'Key: Value' 表
  | SecMarkdown Text Text                                  -- 説明文
  | SecHtml Text Text                                      -- raw HTML (escape hatch)
```

**smart constructors** はすべて `sec` プレフィックス付き:

| 関数 | 用途 |
|---|---|
| `secDataOverview df xCols yCol` | データの行数 + 列ごとの型/N/min/max/mean/median/sd 表 |
| `secModelOverview type formula mMermaid` | モデル種別と数式、オプションで DAG 図 |
| `secCoefficients coeffs (Just ("R²", r2))` | 係数表 + 評価指標 (R², Pseudo-R¹ 等) |
| `secFitScatter xc yc xs ys (Just smooth)` | 観測散布図 + 滑らか曲線 (信頼帯付き可) |
| `secResiduals fitted residuals` | fitted vs residuals 散布図 |
| `secBarChart "Importance" pairs` | (label, value) のリストでバーチャート |
| `secVega "title" spec` | 自前の Vega-Lite spec をそのまま埋め込む |
| `secMermaid "graph TD; A-->B"` | Mermaid 構文でフロー/DAG を埋め込む |
| `secTable "title" headers rows` | 任意のテーブル |
| `secKeyValue "title" [("Trees", "100")]` | 単純なメトリクス一覧 |
| `secMarkdown "Notes" text` | 説明テキスト |
| `secHtml "title" rawHtml` | HTML を直接渡す (escape hatch、サニタイズなし) |

### `SmoothCurve` (信頼帯付き滑らか曲線)

```haskell
data SmoothCurve = SmoothCurve
  { scXs    :: [Double]   -- グリッド点
  , scYs    :: [Double]   -- 中央値 (= 予測曲線)
  , scLower :: [Double]   -- 信頼帯下限 (空リストならバンドなし)
  , scUpper :: [Double]   -- 信頼帯上限
  }
```

`scLower` / `scUpper` が空リストならバンドを描かず、線のみ。

---

## 3. `renderReport` でレポート生成

```haskell
renderReport :: FilePath -> ReportConfig -> [ReportSection] -> IO ()

data ReportConfig = ReportConfig
  { rcTitle    :: Text   -- ヘッダのタイトル + <title>
  , rcSubtitle :: Text   -- 空文字なら非表示
  }

defaultReportConfig :: Text -> ReportConfig
```

### 最小例

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Viz.ReportBuilder

main :: IO ()
main = do
  let cfg = defaultReportConfig "Hello report"
      sections =
        [ secMarkdown "Intro" "This is a tiny report."
        , secKeyValue "Stats" [("Total runs", "42"), ("Status", "OK")]
        , secBarChart "Sales" [("Jan", 120), ("Feb", 95), ("Mar", 140)]
        ]
  renderReport "hello.html" cfg sections
```

→ `hello.html` (~830 KB、Vega-Lite アセット込み) が生成され、ブラウザで開ける。

---

## 4. `Reportable` 型クラス

各フィット結果型から既定の section 群を生成する型クラス:

```haskell
class Reportable a where
  toReport :: ReportConfig
           -> DataFrame
           -> [Text]    -- x 列名
           -> Text      -- y 列名
           -> a
           -> [ReportSection]
```

### 提供 instance (`Viz.ReportInstances`)

| 型 | モジュール | 含まれるセクション |
|---|---|---|
| `RegFit`         | `Model.Regularized` | DataOverview / ModelOverview / Coefficients (β + R²) / KeyValue (penalty/λ/sparsity) / FitScatter / Residuals |
| `SplineFit`      | `Model.Spline`      | DataOverview / ModelOverview / KeyValue (kind/knots) / FitScatter / Residuals |
| `KernelRidgeFit` | `Model.Kernel`      | DataOverview / ModelOverview / KeyValue (kernel/h/λ) / FitScatter / Residuals |
| `RFFRidgeFit`    | `Model.RFF`         | DataOverview / ModelOverview / KeyValue (D/ℓ/σ_f/λ) / FitScatter / Residuals |
| `RobustGPFit`    | `Model.GPRobust`    | DataOverview / ModelOverview / KeyValue (kernel/likelihood/IRLS iter) |

### 利用例

```haskell
{-# LANGUAGE OverloadedStrings #-}
import qualified Numeric.LinearAlgebra as LA
import qualified Data.Vector as V

import DataIO.CSV         (loadAuto)
import DataFrame.Core     (DataFrame, getNumeric)
import Model.Regularized  (Penalty (..), fitRegularized)
import Viz.ReportBuilder
import Viz.ReportInstances ()    -- instances を可視化

main :: IO ()
main = do
  Right df <- loadAuto "data.csv"
  let Just xVec = getNumeric "x" df
      Just yVec = getNumeric "y" df
      n = V.length xVec
      xMat = LA.fromColumns
               [ LA.konst 1 n
               , LA.fromList (V.toList xVec) ]
      yLA  = LA.fromList (V.toList yVec)
      fit  = fitRegularized (L2 0.1) xMat yLA
      cfg  = defaultReportConfig "Ridge demo"
  renderReport "ridge_lib.html" cfg (toReport cfg df ["x"] "y" fit)
```

ライブラリ利用者にとって最も簡単な API: **`toReport cfg df xCols yCol fit`** を `renderReport` に渡すだけ。

---

## 5. モデル別の使用例

CLI からは全モデルが `--report [FILE]` で同じ構造の HTML を生成。
ライブラリからは `Reportable` instance を経由する。

### Ridge / Lasso / Elastic Net

```bash
hanalyze ridge data.csv "x1 x2 x3" y --penalty lasso --lambda 0.05 --report report.html
```

レポート構成:
- データ概要 (4 列の統計表)
- モデル概要 (Lasso, formula y ~ x1 + x2 + x3, λ=0.05)
- 係数表 + R²
- Fit summary (penalty / λ / sparsity / RMSE)
- **正則化パスプロット** (λ ∈ [1e-4, 1e2] 対数掃引、Lasso ではスパース化過程)
- 散布+曲線 (単変数のみ)
- 残差プロット

ライブラリから:

```haskell
import Model.Regularized (fitRegularized, Penalty (..))
import Viz.ReportBuilder
import Viz.ReportInstances ()
let fit = fitRegularized (L1 0.05) xMat yLA
renderReport "out.html" (defaultReportConfig "Lasso") (toReport cfg df xCols "y" fit)
```

### Spline (B-spline / Natural cubic)

```bash
hanalyze spline data.csv x y --type natural --knots 8 --report
```

レポート構成: Data / Model / KeyValue (kind, knots, RMSE) / FitScatter (knots を含む滑らか曲線) / Residuals。

```haskell
import Model.Spline
let fit = fitSpline (BSpline 3) [0, 1, 2, 3, 4, 5] xVec yVec
renderReport "spline.html" cfg (toReport cfg df ["x"] "y" fit)
```

### Kernel (Nadaraya-Watson / Kernel Ridge / RFF)

```bash
hanalyze kernel data.csv x y --method kr --bandwidth 0.5 --report
hanalyze kernel data.csv x y --method rff --features 200 --report
```

ライブラリ:

```haskell
import qualified Model.Kernel as K
import qualified Model.RFF    as R
let krFit  = K.kernelRidge K.Gaussian 0.5 0.01 xVec yVec   -- KernelRidgeFit
gen <- createSystemRandom
feats <- R.sampleRFFRBF 200 0.6 1.0 gen
let rffFit = R.rffRidge feats (V.toList xVec) (V.toList yVec) 0.01  -- RFFRidgeFit

renderReport "kr.html"  cfg (toReport cfg df ["x"] "y" krFit)
renderReport "rff.html" cfg (toReport cfg df ["x"] "y" rffFit)
```

### Quantile / GAM / Random Forest

これらは現在 CLI 専用で `--report` を提供 (Reportable instance はまだなし)。
CLI から呼び出すと、それぞれ専用のレポートが生成される:

```bash
hanalyze quantile data.csv x y --taus 0.1,0.5,0.9 --report
hanalyze gam      data.csv "x1 x2 x3" y --knots 8 --report
hanalyze rf       data.csv "x1 x2 x3" y --trees 200 --report
```

各レポートに含まれるセクション (CLI ハンドラが直接構築):

| サブコマンド | 特殊セクション |
|---|---|
| `quantile` | Multiple quantile fits (`--taus` 指定時、複数 τ の線を重ね描き) |
| `gam`      | 各特徴の Partial effect (s_j(x_j) を partial residual と重ねる) |
| `rf`       | Feature importance (バーチャート) |

将来 `Reportable` instance を追加すればライブラリからも同じセクション構成を組める。

### Robust GP

```haskell
import Model.GP        (GPParams (..))
import Model.GPRobust
let hp  = GPParams 0.6 1.0 0.05 1.0
    fit = fitGPRobust RBF hp (RCauchy 0.5) trainX trainY
renderReport "rgp.html" cfg (toReport cfg df ["x"] "y" fit)
```

レポート: Data / Model / KeyValue (kernel/likelihood/IRLS iterations)。fit 自体は scatter/residual 表示なし (生 GP モデルの可視化は別途必要)。

### タグチ分析 — `Viz.Taguchi`

タグチ分析は固有の構造 (要因効果 + SN 比) があるため、専用の `Viz.Taguchi.renderTaguchiReport` を持つ:

```haskell
import qualified Design.Orthogonal as OA
import qualified Design.Taguchi    as TG
import qualified Viz.Taguchi       as VTG

let Right ad = OA.assignFactors OA.l9 specs
    sns      = TG.snRatioRows TG.SmallerBetter yMatrix
    fes      = TG.analyzeSN ad sns
    opts     = TG.optimalLevels fes
    tr = VTG.TaguchiReport
           { VTG.trTitle     = "Taguchi: chemical optimization"
           , VTG.trArrayName = OA.oaName (OA.adArray ad)
           , VTG.trSNType    = TG.SmallerBetter
           , VTG.trPerRunSN  = sns
           , VTG.trEffects   = fes
           , VTG.trOptimal   = opts
           , VTG.trPredicted = TG.predictSN fes sns
           }
VTG.renderTaguchiReport "taguchi.html" tr
```

CLI: `hanalyze taguchi analyze L9 -f ... --csv runs.csv --report taguchi.html`。

---

## 6. CLI からの利用

すべての fit 系サブコマンドが `--report [FILE]` フラグに対応:

```bash
hanalyze regress  data.csv x y LM   --report
hanalyze ridge    data.csv x y      --penalty ridge --report
hanalyze kernel   data.csv x y      --method kr    --report
hanalyze spline   data.csv x y      --report
hanalyze quantile data.csv x y      --tau 0.5      --report
hanalyze gam      data.csv "x1 x2 x3" y --knots 8  --report
hanalyze rf       data.csv "x1 x2" y --trees 100   --report
hanalyze taguchi  analyze L9 -f ... --csv ... --report
```

`--report` の引数を省略すると `<subcommand>.html` (例: `ridge.html`) になる。
明示する場合: `--report path/to/myreport.html`。

`regress` (LM/GLM/GLMM/GP/HBM) は **`Viz.AnalysisReport`** で生成 (こちらは
DAG・MCMC 診断・対話的予測を含むより詳細なレポート)。それ以外は **`Viz.ReportBuilder`** ベース。

---

## 7. カスタムレポートの作り方

複数モデルを比較する独自レポート、ドメイン固有の表示を作りたい場合は
section を直接組み立てる:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Viz.ReportBuilder
import Graphics.Vega.VegaLite (VegaLite, toVegaLite, dataFromColumns,
                                dataColumn, mark, encoding, position,
                                Mark (..), Position (..), PName, PmType,
                                Numbers, Strings, MType (..))

myReport :: IO ()
myReport = do
  let cfg = ReportConfig
              { rcTitle    = "Custom comparison"
              , rcSubtitle = "Ridge vs Lasso vs ElasticNet"
              }
      sections =
        [ secMarkdown "Setup"
            "We compare three regularized regressions on the same dataset."
        , secTable "Hyperparameters"
            ["Model", "λ", "α"]
            [ ["Ridge",      "0.10", "—"]
            , ["Lasso",      "0.05", "—"]
            , ["ElasticNet", "0.10", "0.5"] ]
        , secBarChart "RMSE comparison"
            [("Ridge", 0.42), ("Lasso", 0.39), ("ElasticNet", 0.38)]
        , secVega "Custom Vega-Lite chart" myCustomSpec
        , secKeyValue "Conclusion"
            [ ("Best model", "ElasticNet")
            , ("Reason",     "Lowest RMSE + sparsity 4/10")
            ]
        ]
  renderReport "comparison.html" cfg sections

myCustomSpec :: VegaLite
myCustomSpec = ...  -- 任意の Vega-Lite spec
```

### 既存 Reportable instance を組み合わせる

```haskell
let baseSections = toReport cfg df ["x"] "y" myFit  -- 既定 sections
    extra = [ secMarkdown "Note" "Cross-validation results below."
            , secVega "CV trace" cvSpec
            ]
renderReport "out.html" cfg (baseSections ++ extra)
```

順序は呼び出し順で保たれる。

---

## 8. 既存 `Viz.AnalysisReport` との関係

| 項目 | `Viz.AnalysisReport` | `Viz.ReportBuilder` |
|---|---|---|
| 対象 | LM / GLM / GLMM / GP / HBM | ridge / kernel / spline / RFF / RobustGP / quantile / gam / rf |
| 設計 | sum-type `ModelFit` ベース | section 並び (リスト) ベース |
| 拡張 | 新モデル = ModelFit に variant 追加 + 各 section ハンドラ書き直し | 新モデル = `Reportable` instance を 1 つ書くだけ |
| レポート構成 | 5 セクション固定 (Data / Model / Result / Interactive / Appendix) | 任意の section 順序 |
| 対話的予測 | 内蔵 (JS で散布図上に予測点を表示) | なし (将来計画) |
| MCMC 統合 | HBM の DAG / トレース / 自己相関を内蔵 | なし (`Viz.Report` と組合せ) |

**選択指針**:
- LM / GLM / GLMM / GP / HBM の標準的な分析 → `AnalysisReport`
- 上記以外、または独自モデル/カスタム可視化 → `ReportBuilder`
- HBM の MCMC 診断専用 → `Viz.Report`

将来的には ReportBuilder を基盤として AnalysisReport も再構築する方向で検討。

---

## 9. よくあるパターンと落とし穴

### Vega-Lite spec の hvega 慣用句

```haskell
import Graphics.Vega.VegaLite

myChart :: VegaLite
myChart = toVegaLite
  [ dataFromColumns []
      . dataColumn "x" (Numbers [1,2,3])
      . dataColumn "y" (Numbers [2,4,6])
      $ []
  , mark Line [MStrokeWidth 2.5]
  , encoding
      . position X [PName "x", PmType Quantitative]
      . position Y [PName "y", PmType Quantitative]
      $ []
  , width  500
  , height 300
  ]
```

`secVega "title" myChart` で section 化。

### ファイルサイズ

各レポートは **800-870 KB** ほど (Vega-Lite + Mermaid アセット込み)。
オフライン動作させる代償として大きめ。アセットは `Viz.Assets` で
ハードコードされている。

### 数値フォーマット

`secCoefficients` / `secFitScatter` などは内部で `printf "%.4f"` 相当の
4 桁表示を使用。整数値は自動的に整数表示 (`150` であって `150.0` にはならない)。
カスタマイズしたい場合は `secKeyValue` で自分で `printf` 結果を渡す。

### Mermaid 図の描画

Mermaid は CDN から取得 (オフラインでない場合のみ動作):

```haskell
secMermaid "graph LR\n  A[mu] --> B[theta]\n  B --> C[y]"
```

オフライン化が必要なら `Viz.ModelGraph` の出力 (HBM の DAG 自動抽出) を
HTML 文字列に整形して `secHtml` に渡す方法もある。

### Reportable で扱えない情報

`toReport :: ... -> a -> [ReportSection]` のシグネチャの通り、引数は
`(ReportConfig, DataFrame, [Text], Text, a)` のみ。これ以外の情報
(例: 外部 CV 結果、別のチェーンの比較) を含めたい場合は、生成された
sections に独自 section を追加する手作業が必要:

```haskell
let base = toReport cfg df xs y fit
    augmented = base ++ [secVega "External" extSpec]
renderReport path cfg augmented
```

### 現状で `Reportable` instance がないモデル

- `Quantile` / `GAM` / `Random Forest` は CLI のハンドラ内部で section を
  直接組み立てている (instance なし)。ライブラリから使う場合は CLI コードを
  参考に手動で section を構築する必要がある。
- 将来的に instance を追加予定 (タスクとしてバックログ)。

---

## 関連ドキュメント

- [01-visualization.ja.md](01-visualization.ja.md) — 単発プロット (棒グラフ、ヒストグラム、散布図、Mermaid DAG 等)
- [../doe-optim/03-orthogonal-taguchi.ja.md](../doe-optim/03-orthogonal-taguchi.ja.md) — 直交表とタグチメソッド (Viz.Taguchi 含む)
- [../regression/06-quantile-gam-rf.ja.md](../regression/06-quantile-gam-rf.ja.md) — Quantile / GAM / Random Forest
- 既存 `Viz.AnalysisReport` (LM/GLM/GLMM/GP/HBM 専用) — ソース読みでの理解推奨
