# HTML レポート — `Viz.ReportBuilder` と `Reportable`

> 🌐 [English](02-report-builder.md) | **日本語**

> 関連: [01-visualization.ja.md](01-visualization.ja.md) (棒グラフ・ヒストグラム等の単発プロット),
> `Viz.AnalysisReport` (**非推奨** — LM/GLM/GLMM/GP/HBM 専用 sum-type ベース、CLI `regress --report` 互換のため残置)
>
> **状態**: `Viz.ReportBuilder` が今後の標準。`Viz.AnalysisReport` は非推奨化済み (`{-# DEPRECATED #-}`) で、機能パリティが取れたら削除予定。新規モデル/可視化はすべて ReportBuilder で実装する。

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
| **AnalysisReport の後継** | `Viz.AnalysisReport` (非推奨) を置き換える今後の標準。LM/GLM/GLMM/GP/HBM 含む全モデルの詳細レポートは ReportBuilder + `Reportable` instance で構築する方針。|

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
| `LMReport`       | `Viz.ReportInstances` | DataOverview / ModelOverview / Collapsible(回帰結果: StatRow + 係数 + 散布図 + 残差) / InteractiveMulti |
| `GLMReport`      | `Viz.ReportInstances` | DataOverview / ModelOverviewLink / Collapsible(回帰結果: StatRow + 係数 + 散布図 + 残差) / InteractiveMulti |
| `GLMMReport`     | `Viz.ReportInstances` | DataOverview / ModelOverviewLink / Collapsible(R²/σ²_u/σ²/ICC + 固定効果 + **BLUP 表** + 残差) / InteractiveMulti |
| `GPReport`       | `Viz.ReportInstances` | DataOverview / ModelOverviewExtras (カーネル) / Collapsible(ハイパーパラメータ + LML + 残差) / InteractiveLM (信頼帯付き) |
| `HBMLinearReport`| `Viz.ReportInstances` | DataOverview / ModelOverviewExtras (サンプラー + DAG) / Collapsible(R²/受容率 + 事後平均係数 + **MCMC 診断 (KDE/トレース/自己相関/ペア)** + 残差) / InteractiveLM (信用区間付き) |
| `QRFit`          | `Model.Quantile`    | DataOverview / ModelOverview / Collapsible(τ-分位点 + Pseudo R¹ + Pinball + 係数 + 散布図 + 残差) |
| `GAMFit`         | `Model.GAM`         | DataOverview / ModelOverview / Collapsible(R²/degree/knots + **特徴ごとの partial effect カード** + 残差) |
| `RFReport`       | `Viz.ReportInstances` | DataOverview / ModelOverview / Collapsible(R² + Trees/Features + **Feature importance** + 残差) |
| `RegFit`         | `Model.Regularized` | DataOverview / ModelOverview / Coefficients (β + R²) / KeyValue (penalty/λ/sparsity) / FitScatter / Residuals |
| `SplineFit`      | `Model.Spline`      | DataOverview / ModelOverview / KeyValue (kind/knots) / FitScatter / Residuals |
| `KernelRidgeFit` | `Model.Kernel`      | DataOverview / ModelOverview / KeyValue (kernel/h/λ) / FitScatter / Residuals |
| `RFFRidgeFit`    | `Model.RFF`         | DataOverview / ModelOverview / KeyValue (D/ℓ/σ_f/λ) / FitScatter / Residuals |
| `RobustGPFit`    | `Model.GPRobust`    | DataOverview / ModelOverview / KeyValue (kernel/likelihood/IRLS iter) |

`LMReport` / `GLMReport` のラッパ型:

```haskell
-- LM
data LMReport = LMReport
  { lmrFit    :: FitResult     -- Model.LM.fitLM 等の結果
  , lmrSmooth :: Maybe SmoothFit  -- Model.LM.fitPolyWithSmooth が返す滑らか曲線 (信頼帯付き可)
  }

-- GLM
data GLMReport = GLMReport
  { glmrFit    :: FitResult
  , glmrFamily :: Family       -- Gaussian / Binomial / Poisson
  , glmrLink   :: LinkFn       -- Identity / Log / Logit / Sqrt
  , glmrSmooth :: Maybe SmoothFit
  }
```

利用例 (LM):

```haskell
import qualified Model.Core as Core
import qualified Model.LM   as LM
import qualified Viz.ReportBuilder   as RB
import qualified Viz.ReportInstances as RI

main = do
  Right df <- DataIO.CSV.loadAuto "data.csv"
  case LM.fitPolyWithSmooth (Core.CI 0.95) 100 df "x" "y" of
    Just (fit, sf) -> do
      let cfg    = RB.defaultReportConfig "My LM"
          report = RI.LMReport fit (Just sf)
      RB.renderReport "lm.html" cfg (RB.toReport cfg df ["x"] "y" report)
    Nothing -> putStrLn "fit failed"
```

### 折りたたみ・グループ化

| 関数 | 内容 |
|---|---|
| `secCollapsible title open children`              | 子セクションを `<details>` で囲む。`open=False` で初期閉。回帰結果や MCMC 診断のように「普段は閉じておく」項目に使う。 |

データ概要セクション (`secDataOverview`) も自動で折りたたみ式:
- Statistics (デフォルト開) — n/min/Q1/median/Q3/max/mean/SD/**Skew/Kurtosis/Missing**
- Histograms per column (デフォルト閉) — 各 numeric 列の Vega-Lite ヒストグラム

### MCMC / 事後分布関連セクション

| 関数 | 内容 |
|---|---|
| `secMCMCDiagnostics title params chain`           | KDE + トレース (`Viz.MCMC.mcmcDiagnostics` 経由) |
| `secMCMCDiagnosticsMulti title params chains`     | 多チェーン版 (チェーン色分け) |
| `secMCMCAutocorr title maxLag params chain`       | 自己相関バーチャート |
| `secMCMCPair title pa pb chain`                   | 2 パラメータのペアスキャッター |
| `secPosteriorSummary title rows`                  | mean/SD/2.5%/97.5%/ESS/R-hat テーブル |

### モデル比較・診断セクション (Cycle 1 で追加)

| 関数 | 内容 |
|---|---|
| `secComparisonTable title headers rows mBest`   | モデル比較テーブル。`mBest = Just i` でその行 (0-based) を黄色背景でハイライト。WAIC/LOO/RMSE 等の最良モデル強調に使用。 |
| `secForestPlot title rows`                       | Forest plot — `[(name, lower, mean, upper)]` から HDI/CI 横棒 + 中央値点を描画。階層モデルの BLUP やベイズ係数比較に。 |
| `secFeatureImportance title items`               | 特徴量重要度バー — `[(label, value)]` を **降順ソート** して `secBarChart` 化。RF / GBM の importance 表示用。 |
| `secPPC title observed reps`                     | Posterior Predictive Check — 観測 KDE (太線) + 各 replicate KDE (薄線) を重ね描き。`reps :: [[Double]]` は事後予測サンプル群。 |

### Markdown appendix

| 関数 | 内容 |
|---|---|
| `secAppendixFromMd title path`                    | 指定の md ファイルを読込、簡易パーサで HTML 化、appendix セクションに |
| `renderSimpleMarkdown txt`                        | 自前 markdown→HTML パーサ (見出し/段落/list/bold/italic/code/link 対応) |

`docs/principles/{lm,glm,glmm,gp,hbm}.ja.md` に各モデルの短い原理解説を
配置済み。比較デモが自動的に読み込んで appendix セクション化する。

### 対話的予測

| 関数 | 内容 |
|---|---|
| `secInteractiveLM title xc yc xs ys smooth (xMin, xMax)` | **単変数** 用。スライダーで x を変えるとグリッド線形補間で予測値 + 信頼帯を表示。GP/HBM のような非線形・MCMC 経由の予測曲線でも使える。 |
| `secInteractiveMulti title im` | **多変量** 用。`InteractiveModel` (係数+リンク関数) を渡すと、左側に各 x_j の slider + 主軸 dropdown、右側に scatter + 予測曲線。slider 変化のたび JS で β₀ + Σβ_j x_j → invLink で y_hat 再計算 + scatter 再描画。CI は σ_hat ± 1.96 で帯描画。 |

`InteractiveModel`:
```haskell
data InteractiveModel = InteractiveModel
  { imXCols     :: [Text]               -- 説明変数名
  , imYCol      :: Text                  -- 応答名
  , imXValues   :: [[Double]]           -- 観測 (n × p)
  , imYValues   :: [Double]
  , imIntercept :: Double                -- β₀
  , imBetas     :: [Double]              -- [β_j]
  , imLink      :: Text                  -- "identity" | "log" | "logit" | "sqrt"
  , imSlider    :: [(Double, Double, Double)]  -- (min, mid, max) per x
  , imCISigma   :: Maybe Double          -- 残差 σ_hat (CI 用、Nothing で帯なし)
  }
```

### LM/GLM/GLMM/GP/HBM の比較デモ

`cabal run analysis-compare-demo` で各モデルについて 既存 AnalysisReport と
新 ReportBuilder の両方の HTML を `trash/cmp_<model>_{AR,RB}.html` として生成。
サイドバイサイドで内容を比較できる。

| モデル | AR 版サイズ | RB 版サイズ | 主な違い |
|---|---|---|---|
| LM       | ~866 KB | ~870 KB | RB は対話的予測 + Markdown 説明追加 |
| GLM (Poisson) | ~862 KB | ~875 KB | 同上 |
| GLMM (LME)    | ~849 KB | ~833 KB | RB は BLUPs 表 + 分散成分 KeyValue |
| GP (RBF) | ~914 KB | ~866 KB | RB はシンプル化 (kernel switcher なし) |
| HBM      | ~867 KB | **~1.1 MB** | RB は MCMC 診断 + 自己相関 + ペア + 事後要約を全部含む |

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

CLI から `--report` で生成される他、ライブラリからも `Reportable` instance 経由で同等のレポートを構築可能 (Cycle 3 で追加):

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

ライブラリから直接構築する例:

```haskell
import qualified Model.Quantile      as Q
import qualified Model.GAM           as GAM
import qualified Model.RandomForest  as RF
import qualified Viz.ReportBuilder   as RB
import qualified Viz.ReportInstances as RI

-- Quantile (τ = 0.5 で中央値回帰)
let qfit = Q.fitQuantile 0.5 xMat yVec
RB.renderReport "qr.html" cfg (RB.toReport cfg df ["x"] "y" qfit)

-- GAM
let gfit = GAM.fitGAM 3 5 0.01 [xVec1, xVec2] yVec
RB.renderReport "gam.html" cfg (RB.toReport cfg df ["x1","x2"] "y" gfit)

-- Random Forest (yHat/yObs を別途渡す)
gen <- createSystemRandom
rf <- RF.fitRF RF.defaultRFConfig rows ys gen
let yHat = V.fromList [ RF.predictRF rf row | row <- rows ]
    rep  = RI.RFReport rf yHat (V.fromList ys)
RB.renderReport "rf.html" cfg (RB.toReport cfg df ["x1","x2"] "y" rep)
```

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

`Viz.AnalysisReport` は **非推奨 (deprecated)**。`{-# DEPRECATED #-}` プラグマ付きで、import すると GHC 警告が出る。
今後の標準は `Viz.ReportBuilder`。

| 項目 | `Viz.AnalysisReport` (非推奨) | `Viz.ReportBuilder` (★ 標準) |
|---|---|---|
| 対象 | LM / GLM / GLMM / GP / HBM 専用 | 全モデル (RegFit/Spline/Kernel/RFF/RobustGP に instance 済み + LM/GLM/GLMM/GP/HBM は instance 化予定) |
| 設計 | sum-type `ModelFit` (~2000 行、密結合) | section 並び + `Reportable` typeclass (拡張容易) |
| 拡張 | ModelFit に variant 追加 + 各 section ハンドラ書き直し | `Reportable` instance を 1 つ書くだけ |
| レポート構成 | 5 セクション固定 (Data / Model / Result / Interactive / Appendix) | 任意の section 順序 |
| 対話的予測 | 内蔵 | `secInteractiveLM` / `secInteractiveMulti` で対応 |
| MCMC 統合 | HBM 用に内蔵 | `secMCMCDiagnostics` / `secMCMCAutocorr` / `secMCMCPair` / `secPosteriorSummary` で対応 |
| 状態 | 削除予定 (CLI `regress --report` の互換のため当面残置) | 育成中 (LM/GLM/GLMM/GP/HBM の `Reportable` instance 化が次の課題) |

**選択指針**:
- 新規実装 → 必ず `ReportBuilder`
- LM/GLM/GLMM/GP/HBM の `regress` CLI → 当面は `AnalysisReport` (将来 ReportBuilder 移行)
- HBM の MCMC 診断のみ単独で見たい → `Viz.Report`

### 移行ロードマップ

1. **Phase 1 (完了)**: `Reportable` instance を LM/GLM/GLMM/GP/HBM に追加 (sum-type なしで CLI 同等のレポートを生成)
   - ✅ `LMReport` / `GLMReport` (Cycle 2)
   - ✅ `QRFit` / `GAMFit` / `RFReport` (Cycle 3 — 横展開)
   - ✅ `GLMMReport` / `GPReport` / `HBMLinearReport` (Cycle 4)
2. **Phase 2**: CLI `regress --report` を ReportBuilder 経路に切り替え (次サイクル)
3. **Phase 3**: `Viz.AnalysisReport` を削除 (Phase 2 後)

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

### LM/GLM/GLMM/GP/HBM の Reportable instance

これらは現状 `Viz.AnalysisReport` 専用 (sum-type ベース)。比較デモ
`AnalysisCompareDemo.hs` では各モデルから section を直接構築する例を
示しているので、Reportable instance 化したい場合はそれを参考にできる。

例: HBM 用の section パターン:
```haskell
RB.secDataOverview df xCols yCol
RB.secModelOverview "Bayesian Linear Regression (HBM, NUTS)" formula Nothing
RB.secCoefficients [(α posterior mean), (β posterior mean), (σ posterior mean)] Nothing
RB.secPosteriorSummary "Posterior summary" rows  -- mean/SD/quantile/ESS/R-hat
RB.secMCMCDiagnostics "MCMC diagnostics" params chain
RB.secMCMCAutocorr "Autocorrelation" 40 params chain
RB.secMCMCPair "Pair scatter (α, β)" "alpha" "beta" chain
RB.secFitScatter xc yc xs ys (Just credibleBand)
RB.secInteractiveLM "Interactive prediction" xc yc xs ys credibleBand range
```

---

## 関連ドキュメント

- [01-visualization.ja.md](01-visualization.ja.md) — 単発プロット (棒グラフ、ヒストグラム、散布図、Mermaid DAG 等)
- [../doe-optim/03-orthogonal-taguchi.ja.md](../doe-optim/03-orthogonal-taguchi.ja.md) — 直交表とタグチメソッド (Viz.Taguchi 含む)
- [../regression/06-quantile-gam-rf.ja.md](../regression/06-quantile-gam-rf.ja.md) — Quantile / GAM / Random Forest
- 既存 `Viz.AnalysisReport` (LM/GLM/GLMM/GP/HBM 専用) — ソース読みでの理解推奨
