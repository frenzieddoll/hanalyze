# hanalyze の機能対応 (Python / R 参照)

> 🌐 [English](python-r.md) | **日本語**

> 統計分析ライブラリとしての位置づけを、**Python**
> (numpy / scipy / sklearn / statsmodels / pymoo / skopt / lifelines /
> PyMC) と **R** (base + tidyverse + caret + survival + forecast +
> lme4) の対応モジュール名で示します。

> **R との数値比較は実施していません。** 表中の R 列は「同等機能を
> 提供する代表的パッケージ」の参照のみです。実測は Python 側だけで
> 行っています — 詳細は
> [bench/results/SUMMARY.md](../../bench/results/SUMMARY.md) を参照。

## 1. 機能対応表

凡例: ✅ Python と数値 bench あり / 🟡 機能あり、bench 未実施 /
独自 = 同等機能を持つ Python/R パッケージが少ない領域。

| 領域 | hanalyze | Python | R | 数値 bench |
|---|---|---|---|---|
| 古典回帰 (LM/Ridge/Lasso) | `Model.LM` / `Model.Regularized` | sklearn / statsmodels | base R / glmnet | ✅ |
| GLM (Binomial/Poisson) | `Model.GLM` | statsmodels | base R `glm()` | ✅ |
| 混合効果 (LME / GLMM) | `Model.GLMM` | statsmodels MixedLM | lme4 | ✅ |
| カーネル法 / GP | `Model.{Kernel,GP,RFF}` | sklearn KernelRidge / GP | kernlab / GPML | ✅ |
| 単目的最適化 | `Optim.{NM,LBFGS,DE,CMAES,SA,PSO}` | scipy.optimize | optimx | ✅ |
| 多目的最適化 | `Optim.NSGA` | pymoo | mco / emoa | ✅ |
| ベイズ最適化 | `Optim.BayesOpt` | scikit-optimize / GPyOpt | mlrMBO | ✅ |
| ベイズ MCMC | `MCMC.{MH,HMC,NUTS,Gibbs}` | PyMC / NumPyro | rstan / brms | ✅ |
| HBM (確率プログラミング) | `Model.HBM` 多相 DSL | PyMC / Pyro / Stan | rstan / brms | 🟡 (機能対応は [PyMC 比較](../02-pymc-comparison.ja.md) を参照) |
| 変分推論 (ADVI) | `Stat.VI` | PyMC / Pyro | rstan ADVI | 🟡 |
| モデル比較 (WAIC / LOO) | `Stat.ModelSelect` | ArviZ | loo (R) | ✅ (`mcmc_extras` suite) |
| 実験計画 (DOE) | `Design.*` | pyDOE / pyDOE2 | DoE.base | 🟡 |
| 直交表 + タグチ | `Design.{Orthogonal,Taguchi}` | (限定的) | qualityTools / DoE.base | 🟡 |
| 仮説検定 | `Stat.Test` | scipy.stats | base R / rstatix | ✅ (`stat_util` suite) |
| 多重比較補正 | `Stat.MultipleTesting` | statsmodels | `p.adjust` | ✅ |
| Bootstrap CI / 並べ替え | `Stat.Bootstrap` | scipy.stats / arch | boot | ✅ |
| 効果量 + Power 解析 | `Stat.Effect` | statsmodels.stats.power | pwr / effsize | 🟡 |
| PCA / 次元削減 | `Model.PCA` | sklearn.decomposition | prcomp | ✅ (`ml` suite) |
| クラスタリング (K-means) | `Model.Cluster` | sklearn.cluster | cluster / mclust | ✅ |
| 決定木 (CART 分類) | `Model.DecisionTree` | sklearn.tree | rpart / tree | ✅ |
| 時系列 (ARIMA / Holt-Winters) | `Model.TimeSeries` | statsmodels.tsa | forecast / fable | ✅ (`survts` / `ts_extras` suite) |
| 生存解析 (KM / Cox) | `Model.Survival` | lifelines | survival | ✅ |
| 分類評価 (AUC / F1 / Brier) | `Stat.ClassMetrics` | sklearn.metrics | pROC / mlr3measures | 🟡 |
| Cross-validation | `Stat.CV` | sklearn.model_selection | caret / rsample | 🟡 |
| 解釈 (Permutation imp / PDP / ICE) | `Stat.Interpret` | sklearn.inspection / shap / pdpbox | iml | 🟡 |
| データ操作 (DataFrame) | Hackage `dataframe` + `DataIO.Reshape` | pandas | tidyverse | 🟡 |
| 可視化 | `Viz.*` (Vega-Lite ベース) | matplotlib / seaborn / plotly | ggplot2 | (質的比較のみ) |
| CSV / Parquet / JSON I/O | `DataIO.{CSV,External}` | pandas / pyarrow | readr / arrow | 🟡 |
| 汚いデータ防衛 | `DataIO.{Health,Sniff,Clean}` | (DIY) | janitor (一部) | 独自 |

## 2. 数値ベンチ

実測値は単一ファイルに集約しています:

- [`bench/results/SUMMARY.md`](../../bench/results/SUMMARY.md) — Suite 別の
  時間 / 精度・Python 側との比較・真値併記
- [`bench/README.md`](../../bench/README.md) — 計測条件 (single-thread、固定 seed)
  と実行手順

## 3. 性質ごとの傾向 (実測ベース)

`SUMMARY.md` に現れる傾向を要約します。詳細値はそちらを参照してください。

### 速度が同等以上に出やすい領域

- 古典回帰 (LM / Ridge / LME): hmatrix BLAS が効く
- 単目的最適化 / 多目的最適化 / ベイズ最適化: GHC compile + polish step
  内蔵で精度を取りやすい
- ベイズ NUTS: mass matrix adaptation 込みで blackjax と同等の ESS

### 速度が劣後しやすい領域

- 大規模な GLM-IRLS / Lasso / Kernel ridge / GP fit (n ≥ 10⁴)
- 原因: sklearn / statsmodels は **inner loop が Cython / SIMD 化** されて
  おり、BLAS 経由を必須とする hmatrix では原理的な dispatch overhead が残る
- 解消には FFI または手書き SIMD が必要 (詳細:
  [`bench/results/OPEN_ISSUES.md`](../../bench/results/OPEN_ISSUES.md))

## 4. 領域別の特徴

### 強み

- 型安全 — 行列次元 / dtype mismatch がコンパイル時検出
- HBM 多相 DSL — 同一モデルから 4 種解釈 (構造検査 / log joint / AD 勾配 /
  依存抽出) が Free monad で導ける
- 汚いデータ防衛 (`DataIO.{Health,Sniff,Clean}`) — pandas / R で明示的に
  対応するパッケージは限定的
- 単一バイナリ — Python interpreter 不要、CLI として完結

### 弱み

- Cython / Fortran 級の SIMD inline ループは hmatrix 経由では到達不能
  (大規模 element-wise で差が出る)
- 並列化は BLAS lock contention で逆効果になる場面がある
- エコシステム規模は Python sklearn 拡張 / R Bioconductor ほどの広さはない
- Haskell 型システムの学習コスト

## 5. ユースケース別の選び方

| ユースケース | 推奨 |
|---|---|
| 研究 / 実験 / 中規模実用 (n ≤ 10⁴) | hanalyze |
| 大規模 ML production (n ≥ 10⁵、throughput 重視) | sklearn / PyTorch |
| ベイズ階層モデルの試作 → 解釈 | hanalyze (HBM DSL) または PyMC |
| R エコシステム特有のドメイン (生態学 / 計量経済 等) | R |

## 関連ドキュメント

- 数値ベンチ: [`bench/results/SUMMARY.md`](../../bench/results/SUMMARY.md)
- ベンチ実行手順: [`bench/README.md`](../../bench/README.md)
- PyMC 機能対応: [`docs/02-pymc-comparison.ja.md`](../02-pymc-comparison.ja.md)
- 各機能の使い方: [`docs/`](../) 配下の各章
