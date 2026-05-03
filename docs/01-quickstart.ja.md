# クイックスタート

> 🌐 [English](01-quickstart.md) | **日本語**

hanalyze は **汎用統計分析ツールキット** です。古典的回帰・実験計画・
多目的最適化・ベイズ統計・MCMC など多領域をカバーします。

## ビルドと実行

```bash
cabal build           # ライブラリ + 全実行ファイル
cabal test            # テストスイート
cabal run <demo-name> # 個別デモ
```

バイナリを直接実行する場合 (cabal run が曖昧な時):
```
dist-newstyle/build/x86_64-linux/ghc-9.6.7/hanalyze-0.1.0.0/x/<demo>/build/<demo>/<demo>
```

CPU 並列化 (多チェーン MCMC など):
```bash
cabal run hbm-example -- +RTS -N4   # 4 スレッド
```

## CLI サブコマンド (Phase C 以降)

`hanalyze` はサブコマンド形式で呼び出せます (bare 形式 `hanalyze <file> <xcols> <ycols> ...`
は `regress` の legacy エイリアスとして残置):

```bash
cabal run hanalyze -- help              # サブコマンド一覧
cabal run hanalyze -- info data.csv     # 列の型と基本統計量
cabal run hanalyze -- hist data.csv col --fit normal 0 1  # ヒストグラム + 理論密度
cabal run hanalyze -- regress data.csv x y LM --report    # 既存の回帰 (= bare 形式)
```

| サブコマンド | 状態 | 機能 |
|---|---|---|
| `regress` / bare | ✅ | LM/GLM/GLMM/GP/HBM 回帰 |
| `info` | ✅ | 列ごとの型・基本統計 (n / min / max / mean / median / sd / unique) |
| `hist` | ✅ | ヒストグラム単体 (`--fit`/`--format`/`--out`) |
| `doe` | ✅ | 直交表 Lₙ (L4/L8/L9/L12/L16/L18) (Phase E1) |
| `taguchi` | ✅ | タグチメソッド (SN 比 + 要因効果 + 内/外配置) (Phase E2) |
| `ridge` | ✅ | Ridge / Lasso / Elastic Net (+ regularization path) |
| `kernel` | ✅ | カーネル回帰 (Nadaraya-Watson / Kernel Ridge / RFF) |
| `spline` | ✅ | B-spline / Natural cubic |
| `quantile` | ✅ | 分位点回帰 (τ-quantile, MM-IRLS) |
| `gam` | ✅ | Generalized Additive Model |
| `rf` | ✅ | Random Forest 回帰 |

---

## 「やりたいこと」逆引き — どのデモ / CLI を使うか

### 1. 古典的な回帰

| やりたいこと | 推奨アプローチ | 例 |
|---|---|---|
| 単純な線形回帰 (信頼区間付き) | CLI: `LM` | `hanalyze data.csv x y LM --ci 0.95 --report` |
| ロジスティック回帰 / ポアソン回帰 | CLI: `GLM` | `hanalyze data.csv x y GLM -d binomial -l logit --report` |
| 多項式回帰 (列ごとに次数指定) | CLI: `LM --degree -1 2 -2 3` | デモ: `glmm-demo` (LME バリアント) |
| 混合効果 (LME / GLMM) | CLI: `--group COL` | デモ: `glmm-demo` |
| ガウス過程回帰 (非線形) | CLI: `GP` / デモ: `gp-demo` | RBF/Matérn/Periodic を自動比較 |

### 2. 非線形・正則化回帰

| やりたいこと | 推奨アプローチ | 例 |
|---|---|---|
| スプライン回帰 (B-spline / Natural cubic) | デモ: `spline-demo` | RMSE 0.05 の精度 |
| カーネル回帰 (Nadaraya-Watson / Kernel Ridge) | デモ: `kernel-demo` | LOO-CV で bandwidth 選定 |
| Ridge / Lasso / Elastic Net | デモ: `regularized-demo` | 変数選択 + sparse モデル |
| 多次元出力 (MultiLM / RRR / PLS / CCA) | デモ: `multilm-demo` / `multivariate-demo` | 共通の低次元構造を抽出 |

### 3. 実験計画と多目的最適化

| やりたいこと | 推奨アプローチ | 例 |
|---|---|---|
| 完全/部分要因計画 / ラテン方格 / 乱塊 | デモ: `doe-demo` | 直交性・D-eff・VIF 評価 |
| 応答曲面法 (RSM) | デモ: `rsm-demo` | CCD/Box-Behnken + 二次回帰 + 極値 |
| D-/A-optimal 設計 (Fedorov) | デモ: `optimaldoe-demo` | 候補集合からの選別 |
| ANOVA / 検出力解析 / サンプルサイズ計算 | デモ: `doe-demo` | t/F/比率検定の検出力 |
| 多目的最適化 (NSGA-II) | デモ: `nsga-demo` | ZDT1/Schaffer の Pareto front |
| Bayesian Optimization (単/多目的) | デモ: `bayesopt-demo` | EI / acquisition 最大化 |
| 統合 (実験計画 + 3 目的最適化) | デモ: `materials-moo-demo` | 合金の強度/コスト/重量 |

### 4. ベイズ推論

| やりたいこと | 推奨アプローチ | 例 |
|---|---|---|
| 単回帰のベイズ版 (CLI から一発) | CLI: `HBM --report --waic` | `hanalyze data.csv x y HBM --report` |
| 階層モデル (グループ構造) | デモ: `simpson-paradox` / `hbm-random-slope` | カスタム階層は `Model.HBM` で直接記述 |
| ランダム傾きモデル | デモ: `hbm-random-slope` | M1 (β 共通) vs M2 (β_g) を WAIC 比較 |
| ベイズ A/B テスト | デモ: `clinical-trial` | Beta-Binomial、決定理論 |
| 多チェーン NUTS + R-hat 診断 | デモ: `hbm-example` | 4 チェーン並列、`mcmc_report_multi.html` |

### サンプラー選択 / 性能

| やりたいこと | 推奨アプローチ | 例 |
|---|---|---|
| MH/HMC/NUTS の比較 | デモ: `bench-mcmc` | 易/難の 2 ケースで ESS/s を計測 |
| 共役モデルの高速サンプリング | デモ: `gibbs-hbm-demo` | Gibbs 自動検出、ハイブリッド Gibbs+MH |
| 変分推論 (大規模・高速近似) | デモ: `vi-demo` | ADVI vs NUTS 精度比較 |
| サンプラーの精度確認 | デモ: `test-hmc-nuts` | 1D ガウスで HMC/NUTS 動作検証 |

### モデル比較 / 解釈

| やりたいこと | 推奨アプローチ | 例 |
|---|---|---|
| WAIC / LOO-CV でモデル選択 | CLI: `--waic` または デモ: `gibbs-demo` | LM/GLM/HBM/LME に対応 |
| 複数モデルを 1 つの HTML で並列比較 | デモ: `simpson-paradox` | LM/GLMM/HBM の係数・予測曲線・WAIC を一覧 |
| シンプソンのパラドックスを再現 | デモ: `simpson-paradox` | `simpson_compare.html` |
| HBM 階層拡張の妥当性検証 | デモ: `hbm-random-slope` | ランダム切片のみ vs +ランダム傾き |

### 可視化

| やりたいこと | 推奨アプローチ | 例 |
|---|---|---|
| HTML レポート (DAG・MCMC 診断・予測曲線統合) | CLI: `--report` | LM/GLM/GLMM/GP/HBM 全対応 (legacy `Viz.AnalysisReport` 経由、後継は `Viz.ReportBuilder`) |
| 棒グラフ・ヒストグラム単体 | デモ: `bar-demo` / CLI: `--hist COL` | PNG/SVG 出力可能 |
| MCMC 単独レポート (KDE + トレース + DAG) | `Viz.Report.renderReport` | デモ: `hbm-example` |
| プロットを PNG/SVG にエクスポート | CLI: `--format png` | 各 NamedPlot が個別画像化 |
| モデル DAG 単独 HTML | `Viz.ModelGraph.renderModelGraph` | Track 型による依存自動抽出 |

---

## 最小の完全ワークフロー (Haskell から、4 タスク別)

### A. 線形回帰 (3 行)

```haskell
import qualified Numeric.LinearAlgebra as LA
import Model.LM (fitLMVec, designMatrix)

let dm    = designMatrix xs
    fit   = fitLMVec dm ys     -- β, ŷ, residuals, R²
    beta  = coefficientsV fit
```

### B. ベイズ階層モデル (NUTS + HTML レポート)

```haskell
import qualified Data.Map.Strict as Map
import Model.HBM
import MCMC.NUTS  (nuts, defaultNUTSConfig)
import Viz.Report (defaultReport, renderReport)

myModel :: ModelP ()
myModel = do
  mu <- sample "mu" (Normal 0 10)
  observe "y" (Normal mu 2) [1.2, 2.3, 3.1, 2.8, 1.9]

main :: IO ()
main = do
  gen <- createSystemRandom
  chain <- nuts myModel defaultNUTSConfig
                (Map.fromList [("mu", 0.0)]) gen
  renderReport "report.html"
               (defaultReport "My Model" chain ["mu"])
```

### C. 実験計画 + ANOVA

```haskell
import Design.Factorial (twoLevelFactorial)
import Design.Anova     (oneWayAnova, printAnovaTable)

let design = twoLevelFactorial 3   -- 2³ 完全要因 = 8 試行
-- 観測 ys を集めた後:
printAnovaTable (oneWayAnova labels ys)
```

### D. 多目的最適化 (NSGA-II)

```haskell
import Optim.NSGA (nsga2, defaultNSGAConfig)

let f xs = [head xs ^ 2, (head xs - 2) ^ 2]   -- 2 目的
front <- nsga2 defaultNSGAConfig f [(0, 2)] gen
-- front :: [Solution] が Pareto 近似
```

---

## モジュール早見表

### 回帰 / 統計モデル
| 用途 | モジュール | 主要関数 |
|---|---|---|
| OLS / 多項式 / 信頼帯 | `Model.LM` | `fitLM`, `fitLMVec`, `fitPolyWithSmooth` |
| GLM (Gaussian/Binomial/Poisson) | `Model.GLM` | `fitGLM`, `fitGLMWithSmooth` |
| LME / GLMM | `Model.GLMM` | `fitLME`, `fitLMEDataFrame` |
| スプライン (B-spline / Natural) | `Model.Spline` | `fitSpline`, `fitSplineMulti` |
| カーネル回帰 / Kernel Ridge | `Model.Kernel` | `nwRegression`, `kernelRidge` |
| Ridge / Lasso / Elastic Net | `Model.Regularized` | `fitRegularized` (sum-type penalty) |
| **RFF (Random Fourier Features)** | `Model.RFF` | `sampleRFFRBF`, `rffRidge`, `rffGP` |
| 多変量 LR / RRR / PLS / CCA | `Model.MultiLM` / `Model.Multivariate` | |
| ガウス過程 / Multi-output GP | `Model.GP` / `Model.MultiGP` | `optimizeGP`, `fitGP` |
| **ロバスト GP** (StudentT/Cauchy) | `Model.GPRobust` | `fitGPRobust`, `predictGPRobust` |
| **Quantile regression** (τ-quantile) | `Model.Quantile` | `fitQuantile`, `predictQuantile` |
| **GAM** (additive B-spline) | `Model.GAM` | `fitGAM`, `predictGAM`, `predictGAMComponent` |
| **Random Forest** (回帰) | `Model.RandomForest` | `fitRF`, `predictRF`, `featureImportance` |

### データ I/O・前処理
| 用途 | モジュール | 主要関数 |
|---|---|---|
| CSV / TSV / SSV (cassava) | `DataIO.CSV` | `loadAuto`, `loadCSV`, `loadTSV` (Hackage `DataFrame` を直接返す) |
| 防衛的 CSV ローダ | `DataIO.CSV` | `loadAutoSafe`, `loadAutoSafeWith`, `LoadOpts` (--no-header / --skip / --comment / --delim / --strict / --no-sniff) |
| Parquet / JSON | `DataIO.External` | `loadParquet`, `loadJSON` |
| DataFrame → ベクタ抽出 | `DataIO.Convert` | `getDoubleVec`, `getTextVec`, `getMaybeTextVec` |
| 構造化ログ | `DataIO.Log` | `LogEntry`, `LogReport`, `printLogReport` |
| 健全性検査 (W001..W008) | `DataIO.Health` | `inspectDataFrame`, `inspectWithPreview` |
| 自動推論 (delimiter / 有ヘッダ / skip) | `DataIO.Sniff` | `sniffBytes`, `sniffFile` (Phase B) |
| 列クリーニング DSL | `DataIO.Clean` | `ColumnRule`, `applyRule`, `cleanPipeline`, `dedupeColumns`, `fillBlankNames` (Phase C) |
| Wide → Long 変形 | `DataIO.Preprocess` | `meltLonger`, `hanalyze melt --id ... --vars ...` |
| 多変量 RFF Ridge | `Model.RFF` | `RFFFeaturesMV`, `rffRidgeMV`, `predictRFFRidgeMV` (CLI: `hanalyze kernel "x1 t" y --method rff`) |
| 入力標準化 (z-score) | `Stat.Standardize` | `Standardizer`, `fitStandardizer`, `applyStandardizer`, `unapplyStandardizer` (CLI: `--standardize`) |
| 周辺尤度最大化 (RFF/GP HP) | `Model.RFF` | `logMarginalLikRBFMV`, `maximizeMarginalLikRBFMV` (CLI: `--auto-hp`) |
| 欠損補完 / フィルタ / 派生列 | `DataIO.Preprocess` | `imputeMean`, `imputeMedian`, `dropMissingRows`, `filterRowsByNumeric`, `deriveNumeric`, `mapNumeric` |
| groupBy / aggregate | `DataIO.Preprocess` | `groupByMean`, `groupBySum`, `groupByMin`, `groupByMax`, `groupByMedian`, `groupByCount`, `groupByAggregate` |

### 実験計画法
| 用途 | モジュール | 主要関数 |
|---|---|---|
| 完全 / 部分要因 / 混合水準 | `Design.Factorial` | `fullFactorial`, `fractionalFactorial` |
| ラテン方格 / 乱塊法 | `Design.Block` | `latinSquare`, `randomizedBlock` |
| ANOVA (一元/二元) | `Design.Anova` | `oneWayAnova`, `twoWayAnova` |
| 検出力解析 / サンプルサイズ | `Design.Power` | `powerTTest`, `sampleSizeTTest` |
| 直交性 / D-eff / VIF | `Design.Quality` | `dEfficiency`, `vifList` |
| RSM (CCD / Box-Behnken) | `Design.RSM` | `centralComposite`, `boxBehnken` |
| D-/A-optimal | `Design.Optimal` | `dOptimal`, `aOptimal` |
| **直交表 Lₙ (L4/L8/L9/L12/L16/L18)** | `Design.Orthogonal` | `lookupOA`, `assignFactors`, `renderCSV` |
| **タグチメソッド (SN 比・要因効果・内外配置)** | `Design.Taguchi` | `snRatio`, `analyzeSN`, `optimalLevels`, `makeInnerOuter` |

### 最適化
| 用途 | モジュール | 主要関数 |
|---|---|---|
| Adam / 勾配上昇 / 数値勾配 | `Optim.Adam` / `Optim.GradAscent` / `Optim.Numeric` | |
| NSGA-II + Pareto | `Optim.NSGA` / `Optim.Pareto` | `nsga2`, `hypervolume` |
| Bayesian Optimization | `Optim.BayesOpt` / `Optim.Acquisition` | `bayesOpt`, `ei` |
| Desirability function | `Optim.Desirability` | `overallDesirability` |

### ベイズ統計 / MCMC
| 用途 | モジュール | 主要関数 |
|---|---|---|
| 多相 DSL (27 分布) | `Model.HBM` | `sample`, `observe`, `ModelP r` |
| HMC / NUTS | `MCMC.HMC` / `MCMC.NUTS` | `hmc`, `nuts`, `nutsChains` |
| Gibbs / MH / Slice | `MCMC.Gibbs` / `MCMC.MH` / `MCMC.Slice` | |
| 変分推論 | `Stat.VI` | `advi` |
| WAIC / LOO / Pseudo-BMA | `Stat.ModelSelect` | `waic`, `loo`, `compareModels` |

### 可視化
| 用途 | モジュール | 主要関数 |
|---|---|---|
| MCMC レポート (KDE / trace / DAG) | `Viz.Report` / `Viz.MCMC` | `renderReport`, `tracePlotHDI` |
| Pareto front (5 種) | `Viz.Pareto` | `paretoScatter`, `parallelCoordinates` |
| 散布 / 棒 / ヒスト | `Viz.Scatter` / `Viz.Bar` / `Viz.Histogram` | |
| 多モデル比較レポート | `Viz.ReportBuilder` (★ 標準) / `Viz.AnalysisReport` (非推奨) | `renderReport` / `writeAnalysisReport` |
