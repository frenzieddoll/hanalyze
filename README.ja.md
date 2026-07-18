# hanalyze

> 🌐 [English](README.md) | **日本語**

[![License: BSD-3](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](LICENSE)
[![GHC](https://img.shields.io/badge/GHC-9.6.7-blueviolet.svg)](https://www.haskell.org/ghc/)

**hanalyze** は Haskell-native な統計解析エンジニアリング基盤です。回帰 / GLMM / ベイズ推論 (HMC/NUTS/Gibbs/ADVI/SMC) / ガウス過程 / 機械学習 (SVM / gradient boosting / ニューラルネット) / 生存解析 (KM / Cox / AFT / 競合リスク) / 時系列 (ARIMA / GARCH / 状態空間) / 因果探索 (LiNGAM) と処置効果推定 / 実験計画 (古典的 + カスタム最適設計) / 多目的最適化 / native plotting / HTML レポートを 1 つの API で統合します。
モデリング・最適化の主要ロジックは Haskell で実装し、線形代数計算は hmatrix/BLAS/LAPACK を利用します。**R/Stan/Python ブリッジは不要**。
ベンチマーク済みの範囲では Python/R 実装と概ね同等の結果を確認しています。性能は領域により異なり、最適化系や中小規模 MCMC では速いケースが多い一方、大規模 ML/GLM では sklearn より遅いケースがあります (詳細は下記)。

---

## 特徴

- **Haskell-native**: 型で dtype/API の取り違えを減らし、必要な形状チェックは実行時に行う
- **算法は Haskell で実装、数値計算は BLAS**: 線形代数は hmatrix/BLAS/LAPACK 経由。R/Stan/Python ブリッジ不要
- **Native plotting**: [hgg](https://github.com/frenzieddoll/hgg) 統合で 90+ の図種を実装 (`plot-integration` フラグ) — 純 Haskell SVG 出力、ブラウザ不要 ([Gallery](#gallery) 参照)
- **HTML レポート統合**: MathJax/Mermaid + Vega-Lite 可視化を 1 関数で生成。PNG/SVG 出力は対応プロットで利用可
- **汚いデータ防衛**: 8 種類の警告 + 自動推論 (delim/header/encoding) + クリーニング DSL
- **Hackage `dataframe`**: Polars-like DF を直接利用。CSV ネイティブ、Parquet/JSON は `dataframe` 経由

---

## Gallery

下のすべての図 (および [`docs/`](docs/) に 90+ の図) は解析結果から hgg
統合を通じて直接生成されます — 純 Haskell、SVG 出力。

| | |
|:--:|:--:|
| ![Linear regression with CI band](docs/images/lm-scatter-ci.svg)<br>線形回帰 — fit + 95% CI ([docs](docs/regression/01-lm.md)) | ![HBM MCMC dashboard](docs/images/hbm-dashboard.svg)<br>ベイズ MCMC dashboard — trace / density / R̂ / ESS ([docs](docs/bayesian/viz-diagnostics.md)) |
| ![Gaussian process mean and credible band](docs/images/gp-mean-ci.svg)<br>ガウス過程 — mean + credible band ([docs](docs/regression/04-gp.md)) | ![Kernel SVM decision boundary](docs/images/svm-rbf-boundary.svg)<br>Kernel SVM (RBF) — decision boundary + support vectors ([docs](docs/ml/usage-ml-extensions.md)) |
| ![DOE prediction profiler](docs/images/doe-profiler.svg)<br>DOE prediction profiler — response vs 各因子 + CI ([docs](docs/api-guide/09-doe.md)) | ![RSM 3D response surface](docs/images/rsm-surface-3d.svg)<br>RSM response surface (3D) ([docs](docs/doe/01-doe.md)) |
| ![DirectLiNGAM causal DAG](docs/images/lingam-dag.svg)<br>DirectLiNGAM 因果探索 — estimated DAG ([docs](docs/api-guide/08-causal.md)) | ![Kaplan-Meier survival curves](docs/images/km-survival.svg)<br>Kaplan-Meier 生存曲線 ([docs](docs/regression/10-survival.md)) |
| ![Time-series forecast](docs/images/ts-forecast.svg)<br>時系列 forecast ([docs](docs/regression/09-timeseries.md)) | ![k-means clusters with 95% ellipses](docs/images/kmeans-ellipse.svg)<br>k-means clusters + 95% ellipses ([docs](docs/stat/05-cluster.md)) |

---

## できること

機能を**ジャンル別**に整理。各機能は専用 docs (使い方) + theory docs (理論) にリンク。
完全な API リファレンスは [`docs/api-guide/`](docs/api-guide/README.md) (12 章) を参照。

### 統計推測 (`Hanalyze.Stat.*`)

| 機能 | モジュール | 使い方 | 理論 |
|---|---|---|---|
| 仮説検定 12 種 (t/χ²/ANOVA/Wilcoxon/KS/Shapiro/Levene/Bartlett/...) | `Hanalyze.Stat.Test` | [stat/01-test.ja.md](docs/stat/01-test.ja.md) | — |
| 多重比較補正 (Bonferroni/Holm/BH/BY) | `Hanalyze.Stat.MultipleTesting` | [stat/06-multipletesting.ja.md](docs/stat/06-multipletesting.ja.md) | — |
| Bootstrap CI / 並べ替え検定 | `Hanalyze.Stat.Bootstrap` | [stat/07-bootstrap.ja.md](docs/stat/07-bootstrap.ja.md) | — |
| 効果量 + Power 解析 (Cohen's d/η²/Cramér V/n推定) | `Hanalyze.Stat.Effect` | [stat/09-effect.ja.md](docs/stat/09-effect.ja.md) | — |
| Cross-validation (k-fold/stratified/LOO) + Grid search | `Hanalyze.Stat.CV` | [stat/04-cv.ja.md](docs/stat/04-cv.ja.md) | — |

### 回帰 (`Hanalyze.Model.*`)

| 機能 | モジュール | 使い方 | 理論 |
|---|---|---|---|
| 線形回帰 (LM) + 推論統計 (SE/t/p, F, AIC/BIC, leverage, Cook's) | `Hanalyze.Model.LM` / `Hanalyze.Model.LM.Diagnostics` | [regression/01-lm.ja.md](docs/regression/01-lm.ja.md) | [principles/lm.ja.md](docs/principles/lm.ja.md) |
| GLM (Binomial / Poisson / Gaussian) | `Hanalyze.Model.GLM` | [regression/02-glm.ja.md](docs/regression/02-glm.ja.md) | [principles/glm.ja.md](docs/principles/glm.ja.md) |
| GLMM / 混合効果モデル (LME) | `Hanalyze.Model.GLMM` | [regression/03-glmm.ja.md](docs/regression/03-glmm.ja.md) | [principles/glmm.ja.md](docs/principles/glmm.ja.md) |
| スプライン回帰 (B-spline / NaturalCubic) | `Hanalyze.Model.Spline` | [regression/04-spline.ja.md](docs/regression/04-spline.ja.md) | [regression/theory-regression-extensions.ja.md](docs/regression/theory-regression-extensions.ja.md) |
| カーネル回帰 (NW / Kernel Ridge) + 多次元入力 | `Hanalyze.Model.Kernel` | [regression/04-kernel.ja.md](docs/regression/04-kernel.ja.md) | 同上 |
| 正則化 (Ridge / Lasso / ElasticNet) | `Hanalyze.Model.Regularized` | [regression/04-regularized.ja.md](docs/regression/04-regularized.ja.md) | 同上 |
| 頑健回帰 (Huber / Tukey biweight M-estimators、IRLS) | `Hanalyze.Model.Robust` | [regression/usage-regularized-advanced.md](docs/regression/usage-regularized-advanced.md) | — |
| ガウス過程 (RBF / Matérn / Periodic + ARD + 多入力) | `Hanalyze.Model.GP` | [regression/04-gp.ja.md](docs/regression/04-gp.ja.md) | [principles/gp.ja.md](docs/principles/gp.ja.md) |
| Random Fourier Features (大規模 GP 近似) | `Hanalyze.Model.RFF` | [regression/04-rff.ja.md](docs/regression/04-rff.ja.md) | [regression/theory-regression-extensions.ja.md](docs/regression/theory-regression-extensions.ja.md) |
| 多変量回帰 / 多出力 GP | `Hanalyze.Model.{Multivariate,MultiGP,MultiOutput}` | [regression/05-multivariate.ja.md](docs/regression/05-multivariate.ja.md) | [regression/theory-multivariate.ja.md](docs/regression/theory-multivariate.ja.md) |
| 分位点回帰 (Quantile) | `Hanalyze.Model.Quantile` | [regression/06-quantile.ja.md](docs/regression/06-quantile.ja.md) | [regression/theory-regression-extensions.ja.md](docs/regression/theory-regression-extensions.ja.md) |
| 一般化加法モデル (GAM) | `Hanalyze.Model.GAM` | [regression/06-gam.ja.md](docs/regression/06-gam.ja.md) | 同上 |
| ランダムフォレスト (回帰) | `Hanalyze.Model.RandomForest` | [regression/06-randomforest.ja.md](docs/regression/06-randomforest.ja.md) | 同上 |
| 多出力回帰 + 対話 HTML | `Hanalyze.Model.MultiOutput` | [regression/07-multireg.ja.md](docs/regression/07-multireg.ja.md) | [regression/theory-multivariate.ja.md](docs/regression/theory-multivariate.ja.md) |

### 機械学習 (`Hanalyze.Model.*` / `Hanalyze.Stat.*`)

| 機能 | モジュール | 使い方 | 理論 |
|---|---|---|---|
| PCA + 累積寄与率 + 標準化 | `Hanalyze.Model.PCA` | [stat/02-pca.ja.md](docs/stat/02-pca.ja.md) | — |
| クラスタリング (K-means + k-means++ + silhouette) | `Hanalyze.Model.Cluster` | [stat/05-cluster.ja.md](docs/stat/05-cluster.ja.md) | — |
| 決定木 (CART 分類器) | `Hanalyze.Model.DecisionTree` | [regression/08-decisiontree.ja.md](docs/regression/08-decisiontree.ja.md) | — |
| Kernel SVM (C-SVC、SMO dual solver) + CV ハイパーパラメータチューニング | `Hanalyze.Model.SVM` | [ml/usage-ml-extensions.md](docs/ml/usage-ml-extensions.md) | — |
| Gradient boosting (回帰 + 二値分類) | `Hanalyze.Model.GradientBoosting` | [ml/usage-ml-extensions.md](docs/ml/usage-ml-extensions.md) | — |
| k-NN / Naive Bayes (Gaussian + Multinomial) / MLP ニューラルネット (mini-batch SGD + Adam) | `Hanalyze.Model.{KNN,NaiveBayes,NeuralNetwork}` | [ml/usage-ml-extensions.md](docs/ml/usage-ml-extensions.md) + [api-guide/05-ml.md](docs/api-guide/05-ml.md) | — |
| Random forest 分類器 (+ permutation importance) | `Hanalyze.Model.RandomForestClassifier` | [api-guide/05-ml.md](docs/api-guide/05-ml.md) | — |
| MDS (古典的 / Sammon) | `Hanalyze.Model.MDS` | [ml/usage-ml-extensions.md](docs/ml/usage-ml-extensions.md) | — |
| 階層クラスタリング (agglomerative + dendrogram) | `Hanalyze.Model.HierarchicalCluster` | [stat/05-cluster.ja.md](docs/stat/05-cluster.ja.md) | — |
| 潜在クラス分析 (EM) + graphical-lasso 相関ネットワーク | `Hanalyze.Model.LatentClassAnalysis` / `Hanalyze.Stat.CorrelationNetwork` | [stat/usage-misc-stat.md](docs/stat/usage-misc-stat.md) | — |
| 関数型データ解析 (basis smoothing + FPCA) | `Hanalyze.Model.FDA` | [fda/usage-fda.md](docs/fda/usage-fda.md) | — |
| 時系列 (ARIMA / Holt-Winters / STL / ACF / PACF) | `Hanalyze.Model.TimeSeries` | [regression/09-timeseries.ja.md](docs/regression/09-timeseries.ja.md) | — |
| GARCH(1,1) ボラティリティ / 線形ガウス状態空間 (Kalman filter + RTS smoother) / VAR(p) | `Hanalyze.Model.{GARCH,StateSpace,VAR}` | [timeseries/usage-ts-surv-advanced.md](docs/timeseries/usage-ts-surv-advanced.md) | — |
| 生存解析 (Kaplan-Meier / Nelson-Aalen / Log-rank / Cox PH) | `Hanalyze.Model.Survival` | [regression/10-survival.ja.md](docs/regression/10-survival.ja.md) | — |
| パラメトリック生存 (AFT) + 競合リスク (CIF) | `Hanalyze.Model.{AFT,CompetingRisks}` | [api-guide/07-survival.md](docs/api-guide/07-survival.md) | — |
| 分類評価メトリクス (Confusion / AUC / F1 / MCC / log-loss / Brier) | `Hanalyze.Stat.ClassMetrics` | [stat/03-classmetrics.ja.md](docs/stat/03-classmetrics.ja.md) | — |
| モデル解釈 (Permutation imp / PDP / ICE) | `Hanalyze.Stat.Interpret` | [stat/13-interpret.ja.md](docs/stat/13-interpret.ja.md) | — |

### 因果推論 (`Hanalyze.Model.LiNGAM.*` / `Hanalyze.Stat.Causal.*`)

| 機能 | モジュール | 使い方 | 理論 |
|---|---|---|---|
| LiNGAM 因果探索 (DirectLiNGAM / ICA-LiNGAM / Pairwise / VAR-LiNGAM / MultiGroup / ParceLiNGAM + bootstrap edge confidence) | `Hanalyze.Model.LiNGAM.*` | [api-guide/08-causal.md](docs/api-guide/08-causal.md) | — |
| 処置効果 (傾向スコア / IPW / doubly robust AIPW / CATE S-T-X meta-learners) | `Hanalyze.Stat.Causal.*` | [causal/usage-causal.md](docs/causal/usage-causal.md) | — |

### ベイズ (`Hanalyze.MCMC.*` / `Hanalyze.Stat.*` / `Hanalyze.Model.HBM`)

| 機能 | モジュール | 使い方 | 理論 |
|---|---|---|---|
| 確率分布 27 種 (Truncated/Censored/MvNormal/LKJ/Multinomial/...) | `Hanalyze.Stat.Distribution` | [bayesian/01-distributions.ja.md](docs/bayesian/01-distributions.ja.md) | [bayesian/theory-distributions.ja.md](docs/bayesian/theory-distributions.ja.md) |
| 確率モデル DSL (HBM 多相 free monad、`deterministic` / `dataNamed` 含む) | `Hanalyze.Model.HBM` | [bayesian/02-probabilistic-model.ja.md](docs/bayesian/02-probabilistic-model.ja.md) | [principles/hbm.ja.md](docs/principles/hbm.ja.md) |
| MCMC サンプラー (MH / HMC / NUTS / Slice / tempered SMC) | `Hanalyze.MCMC.{MH,HMC,NUTS,Slice,SMC}` | [bayesian/03-mcmc-samplers.ja.md](docs/bayesian/03-mcmc-samplers.ja.md) | [bayesian/theory-mcmc.ja.md](docs/bayesian/theory-mcmc.ja.md) / [theory-hmc-nuts.ja.md](docs/bayesian/theory-hmc-nuts.ja.md) |
| Gibbs サンプリング (共役自動検出 + Hybrid) | `Hanalyze.MCMC.Gibbs` | [bayesian/04-gibbs.ja.md](docs/bayesian/04-gibbs.ja.md) | [bayesian/theory-mcmc.ja.md](docs/bayesian/theory-mcmc.ja.md) |
| 変分推論 (ADVI 平均場 Adam) | `Hanalyze.Stat.VI` | [bayesian/05-vi.ja.md](docs/bayesian/05-vi.ja.md) | [bayesian/theory-advanced.ja.md](docs/bayesian/theory-advanced.ja.md) |
| モデル比較 (WAIC / PSIS-LOO / Pseudo-BMA) | `Hanalyze.Stat.ModelSelect` | [bayesian/06-model-comparison.ja.md](docs/bayesian/06-model-comparison.ja.md) | [bayesian/theory-bayesian-basics.ja.md](docs/bayesian/theory-bayesian-basics.ja.md) |
| 事後予測チェック; 一部の PyMC 風モデリング機能 | `Hanalyze.Stat.PosteriorPredictive` | [02-pymc-comparison.ja.md](docs/02-pymc-comparison.ja.md) | — |
| 周辺尤度 (bridge sampling) / ベイズ因子 / ベイズモデル平均化 | `Hanalyze.Stat.{BridgeSampling,BayesFactor,BayesianModelAveraging}` | — | — |
| ベイズ A/B テスト (NUTS 経由の平均差 + ROPE/HDI 判定) | `Hanalyze.MCMC.BayesianTest` | — | — |
| 連鎖診断 (R̂、ESS incl. arviz 互換 `essBulk`、HDI、BFMI、rank histogram、KDE、自己相関) | `Hanalyze.Stat.MCMC` | [bayesian/viz-diagnostics.md](docs/bayesian/viz-diagnostics.md) | — |

### 最適化 (`Hanalyze.Optim.*`)

| 機能 | モジュール | 使い方 | 理論 |
|---|---|---|---|
| 単目的 (勾配法): NM / L-BFGS / Brent | `Hanalyze.Optim.NelderMead`<br>`Hanalyze.Optim.LBFGS`<br>`Hanalyze.Optim.LineSearch` | [optim/01-singleobj.ja.md](docs/optim/01-singleobj.ja.md) | [optim/theory-singleobj.ja.md](docs/optim/theory-singleobj.ja.md) |
| 単目的 (進化計算): DE / CMA-ES / SA / PSO | `Hanalyze.Optim.DifferentialEvolution`<br>`Hanalyze.Optim.CMAES`<br>`Hanalyze.Optim.SimulatedAnnealing`<br>`Hanalyze.Optim.ParticleSwarm` | [optim/01-singleobj.ja.md](docs/optim/01-singleobj.ja.md) | [optim/theory-singleobj.ja.md](docs/optim/theory-singleobj.ja.md) |
| 多目的 (NSGA-II + Pareto) | `Hanalyze.Optim.{NSGA,Pareto}` | [optim/02-multi-objective.ja.md](docs/optim/02-multi-objective.ja.md) | [optim/theory-pareto-moo.ja.md](docs/optim/theory-pareto-moo.ja.md) |
| 獲得関数 (EHVI / ParEGO / EI / LCB / PI) | `Hanalyze.Optim.Acquisition` | [optim/02-multi-objective.ja.md](docs/optim/02-multi-objective.ja.md) | [optim/theory-bayesopt.ja.md](docs/optim/theory-bayesopt.ja.md) |
| ベイズ最適化 (BO + GP-Hedge + 解析勾配) | `Hanalyze.Optim.BayesOpt` | [optim/01-singleobj.ja.md](docs/optim/01-singleobj.ja.md) | [optim/theory-bayesopt.ja.md](docs/optim/theory-bayesopt.ja.md) |
| アルゴリズム選択ガイド | — | [optim/03-algorithm-guide.ja.md](docs/optim/03-algorithm-guide.ja.md) | — |

### 実験計画 (`Hanalyze.Design.*`)

| 機能 | モジュール | 使い方 | 理論 |
|---|---|---|---|
| DOE (Factorial / Block / Mixed / RSM / Optimal / Power / Quality) | `Hanalyze.Design.{Factorial,Block,Mixed,RSM,Optimal,Power,Quality,MultiRSM,Anova}` | [doe/01-doe.ja.md](docs/doe/01-doe.ja.md) | [doe/theory-doe.ja.md](docs/doe/theory-doe.ja.md) |
| 直交表 (L4/L8/L9/L12/L16/L18) + タグチ法 (SN比 + 内/外配置) + 工程能力 (Cp/Cpk) | `Hanalyze.Design.{Orthogonal,Taguchi,Quality}` | [doe/02-orthogonal-taguchi.ja.md](docs/doe/02-orthogonal-taguchi.ja.md) | [doe/theory-doe.ja.md](docs/doe/theory-doe.ja.md) |
| カスタム最適設計 (coordinate exchange + modified Fedorov: D/A/G/I criteria、ベイズ D、線形制約、split-plot、augment メニュー、設計比較 via efficiency/FDS/alias) | `Hanalyze.Design.Custom.*` | [doe/usage-custom-design.md](docs/doe/usage-custom-design.md) + [manual](docs/doe/manual-custom-design.md) | — |
| DOE workflow layer (R 風の対話的 `Design` object over low-level design functions) | `Hanalyze.Design.Workflow` | [api-guide/09-doe.md](docs/api-guide/09-doe.md) | — |

### 可視化 (`Hanalyze.Viz.*`)

| 機能 | モジュール | 使い方 |
|---|---|---|
| 散布図 / Bar / ヒストグラム / MCMC 診断 / GP 描画 / Pareto 描画 | `Hanalyze.Viz.{Scatter,Bar,Histogram,MCMC,GP,Pareto,ModelGraph,Taguchi}` | [visualization/01-visualization.ja.md](docs/visualization/01-visualization.ja.md) |
| 統合 HTML レポート (MathJax + Mermaid + 対話的) | `Hanalyze.Viz.ReportBuilder` | [visualization/02-report-builder.ja.md](docs/visualization/02-report-builder.ja.md) |
| 統一 fit-and-plot 演算子 `df \|-> spec` (LM/GLM/GAM/GP/HBM/... 全モデルの統一エントリーポイント) + plot 不要な係数診断 | `Hanalyze.Fit` / `Hanalyze.Diagnostics` | [io/04-fit-api.md](docs/io/04-fit-api.md) |
| **hgg 統合** (experimental): `toPlot`/`Plottable` が fitted model (LM line+CI / GP mean+credible band) を layer grammar に重ね合わせ。`module Hanalyze` quickstart エントリー。フラグ制御 (`plot-integration`、default off)。 | `Hanalyze.Plot` + `module Hanalyze` | [visualization/03-plot-integration.md](docs/visualization/03-plot-integration.md) |
| **HBM ModelGraph (3 ルート)**: Mermaid HTML / Graphviz DOT / hgg 直 SVG | `Hanalyze.Viz.{ModelGraph,ModelGraphDot}` + `Hgg.Plot.Bridge.Analyze` | 下記「ModelGraph の 3 ルート」 参照 |

#### ModelGraph — 3 ルート

HBM モデルの DAG を可視化する方法は 3 つあり、用途に応じて使い分ける:

| ルート | モジュール | 出力 / 依存 | 推奨用途 |
|---|---|---|---|
| **Mermaid HTML** | `Hanalyze.Viz.ModelGraph.renderModelGraph` | `.html` + Mermaid CDN script | GitHub / GitLab README、 ノート添付 — GitHub 上で自動 render |
| **Graphviz DOT** | `Hanalyze.Viz.ModelGraphDot.renderModelGraphDot` | `.dot` text + `dot` CLI (要 install) | graphviz エコシステム連携 (xdot / gephi / `dot -Tpng`)、細かい directive 制御 (`rank=same` / `constraint=false` 等) |
| **hgg 直** | `Hgg.Plot.Bridge.Analyze.renderModelGraphSVG` ([hgg-analyze-bridge](https://github.com/frenzieddoll/hgg)) | `.svg` (**依存ゼロ**、純 Haskell) | production アプリ組込み、offline batch、大規模 DAG の高速生成 |

3 ルートとも同じ `Hanalyze.Model.HBM.ModelGraph` を入力とする。品質と依存のトレードオフ:

- Mermaid: 軽量、ただし offline 不可
- Graphviz DOT: 最高品質 layout、ただし dot CLI 必須
- hgg: 中間品質 (graphviz dot の 70-80% 同等、純 Haskell)。依存ゼロが必要なら唯一の選択肢

コード例 (`hgg-analyze-bridge` を依存に追加した場合):

```haskell
import qualified Hanalyze.Viz.ModelGraph    as Mermaid
import qualified Hanalyze.Viz.ModelGraphDot as Dot
import qualified Data.Text.IO               as TIO
import           Hgg.Plot.Bridge.Analyze (renderModelGraphSVG)
import           Hanalyze.Model.HBM          (buildModelGraph)

main = do
  let mg = buildModelGraph myHBM
  Mermaid.renderModelGraph "out/dag.html" "My HBM" mg            -- Route 1
  TIO.writeFile "out/dag.dot" (Dot.renderModelGraphDot mg)       -- Route 2
  renderModelGraphSVG     "out/dag.svg"  "My HBM" mg             -- Route 3
```

注記: 標準プロット用には hgg も native PNG (Rasterific) と PDF backend を備えています。
ModelGraph SVG ルートの PNG / PDF が必要な場合は、`rsvg-convert` / `inkscape` 等で変換してください。

### データ I/O (`Hanalyze.DataIO.*`)

| 機能 | モジュール | 使い方 |
|---|---|---|
| CSV/TSV/SSV (cassava) + Parquet/JSON (Hackage `dataframe`) | `Hanalyze.DataIO.{CSV,External,Convert}` | [io/01-dirty-data.ja.md](docs/io/01-dirty-data.ja.md) |
| 汚いデータ防衛 (W001-W008 警告 + 自動 sniff + クリーニング DSL) | `Hanalyze.DataIO.{Health,Sniff,Clean,Log}` | [io/01-dirty-data.ja.md](docs/io/01-dirty-data.ja.md) |
| 整形 (pivot_wider / one-hot / lag-lead / rolling window) | `Hanalyze.DataIO.Reshape` | [io/02-reshape.ja.md](docs/io/02-reshape.ja.md) |
| 前処理 (impute / groupBy / derived columns / melt) | `Hanalyze.DataIO.Preprocess` | [io/01-dirty-data.ja.md](docs/io/01-dirty-data.ja.md) |
| Long-form regrid (`regridLong`) | `Hanalyze.DataIO.Preprocess` + `Hanalyze.Stat.Interpolate` | [io/03-regrid.ja.md](docs/io/03-regrid.ja.md) |

---

## クイックスタート

### 30 秒で動かす CLI

```bash
git clone https://github.com/frenzieddoll/hanalyze
cd hanalyze
cabal build all

# price と promo で sales を回帰し、HTML レポートを出力。
hanalyze regress data/readme/sales.csv "price promo" sales --report sales.html
# β₀=185.05  β(price)=-4.37  β(promo)=+32.29  R²=0.995
```

`data/readme/sales.csv` は本リポジトリ同梱の 20 行デモ CSV (`price`, `promo`,
`sales`)。生成される `sales.html` には係数表・診断・対話的予測ウィジェット
までが入っており、コマンド 1 本でそのまま共有できます。

### 30 秒で動かす Haskell API

```haskell
import qualified Hanalyze.Stat.Test as ST
import qualified Numeric.LinearAlgebra as LA

main = do
  let xs = LA.fromList [12, 14, 13, 15, 17, 11]
      ys = LA.fromList [18, 22, 20, 19, 25, 17]
      result = ST.tTestWelch xs ys ST.TwoSided
  print (ST.trPValue result, ST.trEffect result)
  -- (0.012, Just ("Cohen's d", -1.85))
```

詳しい入門は [docs/01-quickstart.ja.md](docs/01-quickstart.ja.md) 参照。

---

## CLI ツール

```
hanalyze help                     subcommand 一覧
hanalyze regress <file> <x> <y>   LM/GLM/GP/HBM 等の回帰 + HTML レポート
hanalyze info <file>              列ごとの型/統計
hanalyze hist <file> <col>        ヒストグラム + 理論 PDF 重ね描き
hanalyze ridge <file> ...         正則化回帰 (Ridge/Lasso/EN)
hanalyze kernel <file> ...        カーネル回帰 (NW/KR/RFF) + 多次元入力
hanalyze spline <file> ...        スプライン回帰
hanalyze multireg <file> ...      多出力回帰 + 対話的 HTML
hanalyze melt <file> ...          long-form 変換
hanalyze regrid <file> ...        time-axis grid 揃え
hanalyze doe ortho <NAME> -f ...  直交表生成
hanalyze taguchi sn / analyze     タグチメソッド
hanalyze clean <file> --rule ...  汚いデータのクリーニング
```

各コマンドの詳細フラグは `hanalyze <cmd> --help`、または [docs/01-quickstart.ja.md](docs/01-quickstart.ja.md) 参照。

---

## サンプル / デモ

`demo/` 配下に多数のデモ (このリリース時点で 76)。代表例:

| デモ | 概要 |
|---|---|
| `demo/regression/HBMRegressionDemo.hs` | HBM ベイズ線形回帰 + NUTS + HTML |
| `demo/regression/RFFDemo.hs` | RFF で大規模 GP の高速近似 |
| `demo/regression/RobustGPDemo.hs` | StudentT 観測尤度の頑健 GP |
| `demo/doe-optim/NSGADemo.hs` | ZDT 問題で NSGA-II + Pareto |
| `demo/doe-optim/BayesOptDemo.hs` | Branin/Hartmann6 で BO |
| `demo/bayesian/HBMComparisonDemo.hs` | WAIC/LOO で HBM 比較 |
| `demo/bayesian/SimpsonParadoxDemo.hs` | 階層モデルでパラドックスを解明 |
| `demo/io/DirtyDataDemo.hs` | 19 種の汚い CSV を自動防衛 |

実行: `dist-newstyle/build/x86_64-linux/ghc-9.6.7/hanalyze-0.2.0.0/x/<demo-name>/build/<demo-name>/<demo-name>` で起動。

---

## hanalyze が刺さる場所

Python/R を全面置換するのではなく、Haskell 統合・単一バイナリ CLI・レポート
統合が効くワークフローを主戦場に据えています。

**刺さる**

- Haskell ネイティブのパイプラインで、Python を呼ばずに統計/ベイズ/最適化を完結させたい
- 単一バイナリで配布したい (`hanalyze` 1 本で動く、Python venv 不要)
- 汚い CSV の防衛 + クリーニング + 解析を 1 ワークフローで済ませたい
- DoE / タグチ / 直交表で製造業・実験系を回したい
- 解析結果をそのまま HTML レポート化して共有したい
- 型安全な解析パイプラインで dtype/API のミスを早期に潰したい

**正面勝負を避ける**

- 大規模 DataFrame 処理 (pandas / polars / data.table を使う)
- GPU 深層学習 (PyTorch / JAX を使う)
- scikit-learn の成熟したモデル群すべて
- Stan / PyMC の MCMC 診断エコシステム全体
- ggplot2 の表現力すべて

---

## Python との比較

> R は機能対応のみ。数値ベンチは Python に対してのみ実施しています。

下の数値は `bench/results/{haskell,python}/*.csv` の最新ラン。
ベンチ条件 (`OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1`、single-thread、
固定 seed) は [bench/results/SUMMARY.md](bench/results/SUMMARY.md) 参照。

| 領域 | このベンチでの結果 |
|---|---|
| **単目的最適化** (DE/CMAES/L-BFGS/NM) | scipy より速いケースが多い (Rosenbrock_2D/DE 134×、Ackley/CMAES 49×、Griewank/CMAES 54×)。Sphere_30D/L-BFGS の目的関数値はこの実行で 8.1e-40 vs scipy 2.6e-11 |
| **多目的最適化** (NSGA-II) | ZDT/DTLZ 系で同等〜やや有利 (DTLZ2_3 1.43× 速、ZDT1/2/3 は pymoo と ±5%)。HV/IGD は概ね pymoo と同等以上 |
| **ベイズ最適化** (BO) | Branin で同等 (1.15×)、Hartmann6 はこの実行で -3.07 vs skopt -2.77 |
| **シミュレーテッドアニーリング** (Tsallis SA) | 同等。Rastrigin_10D はこの実行で 0.0 (scipy `dual_annealing` 7.8e-14) |
| **古典回帰** (LM/Ridge/Lasso/GLMM) | ベンチケースでは概ね同等。LME はこの実行で statsmodels 比 30× 速 |
| **大規模 GLM/Lasso** (n ≥ 10k) | sklearn より現状遅い (3-5×、Cython inner loop に追従不能) |
| **カーネル/GP** | sklearn より現状遅い (2.5-4.7×) |
| **ベイズ MCMC** (NUTS/HMC) | 8-schools で ESS 839 (blackjax 810 と同等品質)。PyMC 比 7.4× 速、blackjax 比 2.8× 遅 (JAX-JIT 構造差) |
| **HBM (確率プログラミング)** | 多相 DSL で一部の PyMC 風モデリング機能 + 一部の分布 (Truncated/Censored/MvNormal/LKJ/...) を提供 |
| **VI / WAIC / LOO** | ADVI は小規模 logistic で numpyro SVI より 3.0× 速、LOO は arviz より 2.9× 速 (ベンチケースでは) |
| **仮説検定 / Bootstrap / k-fold** | Welch t-test 39×、KS 11×、k-fold 2.2× 速 vs scipy/sklearn (ベンチケースでは) |
| **時系列 / Spline / GAM** | ARIMA 128× 速、Spline PCHIP 互角、GAM はベンチケースで pygam の 1.6× 遅 |
| **生存解析** (KM/Cox PH) | lifelines と概ね同等 |
| **多出力回帰 / Regrid** | MultiLM 2.3× 速、`regridLong` は pandas+scipy 自前合成版より 20× 速 |
| **可視化** | hvega 経由の Vega-Lite (grammar-of-graphics 系)。HTML レポート同梱 |

機能対応表は [docs/comparison/python-r.ja.md](docs/comparison/python-r.ja.md)、数値詳細は [bench/results/SUMMARY.md](bench/results/SUMMARY.md) を参照。

---

## ベンチマークハイライト

代表的な結果。各項目は **1 つのベンチ設定** での値で、絶対値は反復数・seed・
許容誤差に依存します。条件は SUMMARY を参照。
NUTS はさらに posteriordb reference posteriors に対して検証されています
([bench/posteriordb/](bench/posteriordb/) 参照)。

- **NUTS 8-schools** (warmup 500, samples 1000): hanalyze 1492 ms / ESS(mu) 839 vs blackjax 530 ms / ESS 810 (この実行)
- **Holt-Winters seasonal n=500 p=12**: hanalyze 0.19 ms vs statsmodels MLE 96 ms (この実行。なお hanalyze は固定 α=0.3 closed-form、statsmodels は MLE fit)
- **Sphere_30D/DE**: このベンチでは hanalyze 1.0e-26、scipy 2.8e-5
- **Sphere_30D/L-BFGS**: このベンチでは hanalyze 8.1e-40、scipy 2.6e-11
- **Rastrigin_10D/SA**: この実行で hanalyze 0.0、scipy `dual_annealing` 7.8e-14
- **Hartmann6/BO**: この実行で hanalyze -3.07、skopt -2.77
- **DTLZ2_3/NSGA-II**: hanalyze 528 ms vs pymoo 758 ms (この実行で 1.43× 速)
- **DE Rosenbrock_2D**: hanalyze 1.2 ms vs scipy 164 ms (この実行で 134× 速)
- **Constrained Quad2D (eq)**: hanalyze 0.062 ms vs scipy SLSQP 0.69 ms (この実行)
- **regridLong (jagged long-form)**: hanalyze 0.99 ms vs pandas+scipy 合成 19.4 ms (この実行)

再現: `OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 cabal run bench-{regression,kernel,optim,mo,bo,mcmc-b7,mcmc-extras,ts-extras,optim-plus,stat-util,multi-output,regrid}`、続いて Python 側を `bench/python/bench_*.py` で実行 (詳細 [bench/README.md](bench/README.md))。

---

## アーキテクチャ

```mermaid
graph TD
  IO[DataIO.* CSV/Parquet/JSON]
  IO --> DF[Hackage dataframe]
  DF --> Models[Model.* 回帰/ML/ベイズ/時系列/生存]
  DF --> Stat[Stat.* 検定/CV/効果量/解釈]
  Models --> Optim[Optim.* 最適化]
  Models --> MCMC[MCMC.* サンプラー]
  Models --> Viz[Viz.* HTML/PNG/SVG]
  Stat --> Viz
  MCMC --> Viz
  Optim --> Design[Design.* DOE/タグチ]
```

**全モジュール Hackage `dataframe` を直接やり取り**。独自 DataFrame.Core は廃止済。

---

## ロードマップと API 安定性

- **Stable** (minor 更新で API 後方互換を維持予定): `Hanalyze.DataIO.*`、`Hanalyze.Stat.{Test, Bootstrap, MultipleTesting, ClassMetrics, CV, Effect, Distribution}`、`Hanalyze.Model.{LM, GLM, Spline, Regularized, RandomForest, DecisionTree, TimeSeries, Survival, GAM}`、`Hanalyze.Optim.{NelderMead, LBFGS, DifferentialEvolution, CMAES, NSGA, BayesOpt, SimulatedAnnealing, ParticleSwarm}`、`Hanalyze.Design.*`、`Hanalyze.Viz.{Scatter, Bar, Histogram}`
- **Experimental** (API は変動の可能性): `Hanalyze.Model.HBM` DSL、`Hanalyze.MCMC.NUTS` (mass-matrix adaptation は opt-in)、`Hanalyze.Stat.VI` (ADVI)、`Hanalyze.Model.{GP, RFF, GPRobust, GLMM}`、`Hanalyze.Model.{SVM, GradientBoosting, NeuralNetwork}`、`Hanalyze.Model.LiNGAM.*`、`Hanalyze.Design.Custom.*`、`df |-> spec` fit 演算子 (`Hanalyze.Fit`)、hgg 統合 (`plot-integration` フラグ)、`Hanalyze.Viz.ReportBuilder`。挙動はベンチで検証済ですが型シグネチャが変わる可能性あり
- **将来検討**: hmatrix/Massiv/Accelerate を切替えるバックエンド typeclass。実装スケジュールは未定。(以前計画していたトップレベル統一 API と fit 演算子 API は 0.2.0.0 で `module Hanalyze` と `Hanalyze.Fit` として実現済み。)

---

## モジュール構成

```
src/
  DataIO/      — CSV/JSON/Parquet IO + 健全性検査 + sniff + clean DSL + reshape (9 mod)
  Stat/        — 検定/分布/補間/効果量/CV/Bootstrap/解釈 etc. (21 mod)
  Model/       — LM/GLM/GLMM/Spline/Kernel/GP/RFF/HBM/PCA/Cluster/Tree/TS/Survival 等 (23 mod)
  Optim/       — 単目的 (NM/LBFGS/DE/CMAES/SA/PSO) + 多目的 (NSGA/BO/Pareto) (18 mod)
  Design/      — Factorial/Block/RSM/Optimal/Orthogonal/Taguchi 等 (11 mod)
  Viz/         — Vega-Lite ベース可視化 + ReportBuilder (15 mod)
  MCMC/        — MH/HMC/NUTS/Gibbs/Slice (6 mod)
```

このリリース時点で 212 モジュール、~1,390 テスト例。

---

## ビルド

```bash
cabal build all                  # ライブラリ + 全実行ファイル (76 demo)
cabal test                       # hspec test suite
cabal repl                       # 対話実行
```

主要依存: `hmatrix` (BLAS/LAPACK)、`hvega` (Vega-Lite)、`statistics`、`mwc-random`、`dataframe` (Hackage Polars-like)、`massiv` (並列配列)、`ad` (自動微分)、`async`。

GHC 9.6.7 + cabal 3.14.2 で確認済。

---

## ベンチマーク実行

```bash
# 1. 共通テストデータ生成 (固定 seed、deterministic)
cabal run bench-data-gen

# 2. Haskell 側
OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 \
  cabal run bench-regression bench-kernel bench-optim bench-mo bench-bo

# 3. Python 側 (venv 必要、bench/requirements.txt)
OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 \
  bench/venv/bin/python bench/python/bench_regression.py
# (similarly for kernel, optim, mo, bo)

# 4. 集約 (Markdown 表)
bench/venv/bin/python bench/aggregate.py > bench/results/SUMMARY.md
```

---

## 開発・貢献

- **Issue / PR**: [github.com/frenzieddoll/hanalyze](https://github.com/frenzieddoll/hanalyze)
- **テスト追加**: `test/Spec.hs` に hspec 形式で
- **ベンチ追加**: `bench/haskell/Bench*.hs` + 対応 Python 版
- **コーディング規約**: `CONTRIBUTING.md` に詳細 (hot path で list 経由禁止、unsafe 最小限、等)

---

## ライセンス

BSD-3-Clause License — 詳細は [LICENSE](LICENSE) を参照。

## 著者

Toshiaki Honda <frenzieddoll@gmail.com>
