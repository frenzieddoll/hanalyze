# hanalyze vs Python / R 比較

> 統計分析ライブラリとしての位置づけを、Python (numpy/scipy/sklearn/
> statsmodels/pymoo/skopt/lifelines/PyMC) と R (base + tidyverse +
> caret + survival + forecast + lme4) の主要機能と比較。

## 1. 比較サマリー (一行表)

| 領域 | hanalyze | Python | R | 状態 |
|---|---|---|---|---|
| **古典回帰 (LM/Ridge/Lasso)** | `Model.LM/Regularized` | sklearn / statsmodels | base R / glmnet | ✅ benched |
| **GLM (Binomial/Poisson)** | `Model.GLM` | statsmodels | base R glm() | ✅ benched |
| **混合効果 (LME/GLMM)** | `Model.GLMM` | statsmodels MixedLM | lme4 | ✅ benched (LME 30× 速) |
| **カーネル法 / GP** | `Model.{Kernel,GP,RFF}` | sklearn KernelRidge / GP | kernlab / GPML | ✅ benched |
| **ベイズ MCMC** | `MCMC.{MH,HMC,NUTS,Gibbs}` | PyMC / NumPyro | rstan / brms | 🟡 未 bench |
| **HBM (確率プログラミング)** | `Model.HBM` 多相 DSL | PyMC / Pyro / Stan | rstan / brms | 🟡 PyMC parity 確認済、bench 未 |
| **変分推論 (ADVI)** | `Stat.VI` | PyMC / Pyro | rstan ADVI | 🟡 未 bench |
| **モデル比較 (WAIC/LOO)** | `Stat.ModelSelect` | ArviZ | loo (R) | 🟡 未 bench |
| **単目的最適化** | `Optim.{NM,LBFGS,DE,CMAES,SA,PSO}` | scipy.optimize | optimx | ✅ benched (10-100× 速) |
| **多目的最適化** | `Optim.NSGA` | pymoo | mco / emoa | ✅ benched (1.1-1.6× 速) |
| **ベイズ最適化** | `Optim.BayesOpt` | scikit-optimize / GPyOpt | mlrMBO | ✅ benched (Hartmann6 で勝利) |
| **実験計画 (DOE)** | `Design.*` | pyDOE / pyDOE2 | DoE.base | 🟡 未 bench |
| **直交表 + タグチ** | `Design.{Orthogonal,Taguchi}` | (限定的) | qualityTools / DoE.base | 🟡 未 bench |
| **仮説検定** | `Stat.Test` | scipy.stats | base R / rstatix | 🟡 未 bench |
| **多重比較補正** | `Stat.MultipleTesting` | statsmodels | p.adjust | 🟡 未 bench |
| **Bootstrap CI / 並べ替え検定** | `Stat.Bootstrap` | scipy.stats / arch | boot | 🟡 未 bench |
| **効果量 + Power 解析** | `Stat.Effect` | statsmodels.stats.power | pwr / effsize | 🟡 未 bench |
| **PCA / 次元削減** | `Model.PCA` | sklearn.decomposition | prcomp | 🟡 未 bench |
| **クラスタリング (K-means)** | `Model.Cluster` | sklearn.cluster | cluster / mclust | 🟡 未 bench |
| **決定木 (CART 分類)** | `Model.DecisionTree` | sklearn.tree | rpart / tree | 🟡 未 bench |
| **時系列 (ARIMA/Holt-Winters)** | `Model.TimeSeries` | statsmodels.tsa | forecast / fable | 🟡 未 bench |
| **生存解析 (KM/Cox)** | `Model.Survival` | lifelines | survival | 🟡 未 bench |
| **分類評価 (AUC/F1/Brier/...)** | `Stat.ClassMetrics` | sklearn.metrics | pROC / mlr3measures | 🟡 未 bench |
| **Cross-validation** | `Stat.CV` | sklearn.model_selection | caret / rsample | 🟡 未 bench |
| **解釈 (Permutation imp/PDP/ICE)** | `Stat.Interpret` | sklearn.inspection / shap / pdpbox | iml | 🟡 未 bench |
| **データ操作 (DataFrame)** | Hackage `dataframe` + `DataIO.Reshape` | pandas | tidyverse | 🟡 未 bench |
| **可視化** | `Viz.*` (Vega-Lite) | matplotlib / seaborn / plotly | ggplot2 | 🟡 未 bench |
| **CSV/Parquet/JSON I/O** | `DataIO.{CSV,External}` | pandas / pyarrow | readr / arrow | 🟡 未 bench |
| **dirty data 防衛** | `DataIO.{Health,Sniff,Clean}` | (なし、DIY) | janitor (一部) | (独自機能) |

## 2. ベンチ済領域 (✅) の詳細

### 2.1 単目的最適化 — hanalyze 圧勝

| Bench | hanalyze | scipy | 評価 |
|---|---|---|---|
| Sphere_30D/L-BFGS | **8.1e-40** / 0.05 ms | 2.6e-11 / 1.67 ms | scipy 29 桁越え、31× 速 |
| Sphere_30D/DE | **1.0e-26** / 277 ms | 4.5e-5 / 4852 ms | scipy 21 桁越え、17× 速 |
| Ackley_10D/CMAES | **4.0e-15** / 2.6 ms | 1.3e-6 / 134 ms | scipy 9 桁越え、52× 速 |
| Levy_10D/DE | **8.3e-21** / 45 ms | 8.0e-17 / 1768 ms | scipy 4 桁越え、40× 速 |
| Rosenbrock_2D/NelderMead | **3.3e-13** / 0.06 ms | 4.6e-18 / 4.83 ms | 78× 速 (精度ほぼ同) |
| Rastrigin_10D/SA | **0.0** ⭐ / 2.4 s | 5.7e-14 / 193 ms | 機械精度 parity |

**hanalyze の優位**: hmatrix BLAS の効率使用、polish step (DE/CMAES に L-BFGS local 内蔵)、Tsallis SA の重い裾分布、GHC compiled の overhead 小。

### 2.2 多目的最適化 (NSGA-II) — pymoo 越え

全 4 ZDT/DTLZ 問題で HV/IGD を pymoo より良い値に。per-gen 速度も
1.1-1.6× 速い (DTLZ2_3 で 1.6×)。

NF1 (SBX boundary correction)、NF3 (random-permutation tournament)、
NF4 (重複除去) を pymoo source 解析から実装。

### 2.3 ベイズ最適化 — Hartmann6 で skopt 越え

| Bench | hanalyze | skopt | 評価 |
|---|---|---|---|
| **Hartmann6/BO** | **-3.07** / 9.6 s | -2.77 / 7.1 s | skopt 大幅越え |
| Branin/BO | 0.86 / 22 s | 0.398 / 5.5 s | skopt 2× 圏 |

A (真 ARD)、B (GP-Hedge: EI/LCB/PI 動的混合)、C (kernel + EI 解析勾配)
で実装。Hartmann6 で skopt を decisively 越え (true opt の 92%)。

### 2.4 古典回帰 — 同等以上

| Bench | hanalyze | sklearn | 比 |
|---|---|---|---|
| LM_n100000_p100 | 642 ms | 668 ms | 1.04× |
| Ridge_n1000_p5 | 0.04 ms | 0.54 ms | **15×** |
| LME_n2000_p5_g20 | 1.4 ms | 43 ms | **30×** |
| LME_n10000_p10_g50 | 19.9 ms | 97 ms | 4.9× |

LME (混合効果モデル) で特に優位。EM exact 推定が高速。

### 2.5 大規模 ML (n ≥ 10k) — sklearn の Cython native に劣後

| Bench | hanalyze | sklearn | 比 |
|---|---|---|---|
| GLM_logit_n10k | 14.9 ms | 4.2 ms | 0.28× (= 3.6× 遅) |
| Lasso_n10k×p50 | 7.4 ms | 2.4 ms | 0.32× (= 3.1× 遅) |
| KR_n2000 | 384 ms | 176 ms | 0.46× (= 2.2× 遅) |
| GP_fit_n1000 | 200 ms | 42 ms | 0.21× (= 4.7× 遅) |

**原因**: BLAS dispatch overhead と sklearn の Cython inline SIMD の差。
完全 sklearn 並みには C/FFI 必要 (Cython の inline SIMD 差)。

## 3. 未ベンチ領域 (🟡) - 比較 todo

以下は**機能実装は完了しているが、Python/R との実測比較が未実施**。
実装精度が同等であることは hspec test で確認済。

### 3.1 ベイズ MCMC
- 比較相手: PyMC (Python)、rstan (R)
- 比較項目: NUTS warmup + サンプリング時間、有効サンプル数 (ESS)/秒、R-hat
- 想定モデル: 8-schools (centered/non-centered)、線形回帰、階層 logistic
- 期待: hanalyze の純 Haskell NUTS が PyMC の C++ コア (Stan) と
  どこまで戦えるか。並列 4-chain で fairness 確保

### 3.2 仮説検定
- 比較相手: scipy.stats、rstatix (R)
- 比較項目: t-test/χ²/ANOVA/Mann-Whitney/Wilcoxon の数値一致 + 速度
- 期待: 数値一致は完全一致 (同じアルゴリズム)、速度は scipy と同等
  か若干速 (Haskell の closure 起動コスト)

### 3.3 PCA / クラスタリング
- 比較相手: sklearn.decomposition / sklearn.cluster
- 比較項目: PCA の SVD 速度、K-means convergence、silhouette
- データ: iris / mnist 部分集合 / 合成 blob

### 3.4 決定木分類
- 比較相手: sklearn.tree.DecisionTreeClassifier、rpart (R)
- 比較項目: 学習時間、予測精度、葉ノード数

### 3.5 時系列
- 比較相手: statsmodels.tsa (Python)、forecast / fable (R)
- 比較項目: ARIMA の AIC、Holt-Winters の予測 RMSE、ACF/PACF 数値一致
- データ: AirPassengers (R 標準)、合成 AR(2)/seasonal

### 3.6 生存解析
- 比較相手: lifelines (Python)、survival (R)
- 比較項目: KM/Cox PH の係数一致、log-rank p-value、計算時間
- データ: lung dataset (R survival 標準)

### 3.7 分類評価 + CV + 解釈
- 比較相手: sklearn.metrics / sklearn.model_selection / sklearn.inspection
- 数値一致が主、速度は副次的

### 3.8 多重比較補正 + Bootstrap + Effect
- 比較相手: scipy.stats / statsmodels / R (p.adjust, boot, pwr)
- 数値一致が主

### 3.9 データ操作
- 比較相手: pandas (Python)、dplyr/data.table (R)
- 比較項目: CSV 読込 / groupBy 集約 / join / pivot
- データ: 大規模 CSV (1M 行)、複数ファイル join
- **注**: 主要操作は Hackage `dataframe` ネイティブ。hanalyze 拡張部
  (pivot_wider/one-hot/lag/rolling) のみ独自

### 3.10 実験計画
- 比較相手: pyDOE / pyDOE2 (Python)、DoE.base / qualityTools (R)
- 比較項目: 直交表生成、最適計画、Power 計算

### 3.11 可視化
- 比較相手: matplotlib / ggplot2
- 数値ベンチではなく**機能カバレッジ + 出力品質の質的比較**
- HTML/PNG/SVG 出力、Mermaid、対話的 GUI

### 3.12 HBM
- 比較相手: PyMC、Stan、NumPyro
- 比較項目: 同一モデル (例: 8-schools) の事後分布一致、サンプリング
  速度、ESS/秒
- 既に「PyMC 互換性」は [docs/02-pymc-comparison.ja.md](../02-pymc-comparison.ja.md) で確認済 (Truncated/Censored/MvNormal/LKJ/...)

## 4. 比較で見えた構造的な強み・弱み

### 強み (Haskell ならでは)

1. **型安全**: 行列次元、dtype mismatch がコンパイル時検出
2. **アルゴリズム精度**: GHC 最適化 + 慎重な数値実装で機械精度多数
3. **モジュール構造**: Pure FP の合成性、refactor 容易
4. **HBM 多相 DSL**: 同一モデルから 4 解釈 (Free monad)
5. **dirty data 防衛**: pandas/R に明示的存在しない領域 (Janitor 部分的に類似)
6. **単一バイナリ**: Python interp 不要、CLI として完結

### 弱み (構造的、容易には埋まらない)

1. **Cython/Fortran 級の SIMD**: 大規模 elementwise 演算で 3-5× 遅い
2. **GIL なし並列**: 利点だが BLAS contention で逆効果になることも (F5 で確認)
3. **エコシステム規模**: Python の sklearn 拡張 / R の Bioconductor ほどの
   専門ドメイン特化パッケージ群はない
4. **学習コスト**: Haskell の型システムへの学習コスト

## 5. 結論

**hanalyze は scipy/skopt/pymoo を accuracy で多数の領域で凌ぐ**が、
**大規模 ML での速度は sklearn の Cython native loop に劣る**。

研究・実験・中規模実用には十分、production の高 throughput 用途は
sklearn / pytorch / R 推奨。

---

## 関連ドキュメント

- ベンチマーク詳細: [bench/results/SUMMARY.md](../../bench/results/SUMMARY.md)
- ベンチマーク手順: [bench/README.md](../../bench/README.md)
- PyMC 互換確認: [docs/02-pymc-comparison.ja.md](../02-pymc-comparison.ja.md)
- 各機能の使い方: 各 [docs/](../) サブディレクトリ
