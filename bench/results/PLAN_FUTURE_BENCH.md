# 今後のベンチ計画 (B6+)

最終更新: 2026-05-07 (Tier 1-5 全完了)

## ✅ 完了状況 (2026-05-07)

| Tier | Suite | 状態 |
|---|---|---|
| 1 | B6 ML Classical (PCA/KMeans/DT/RF) | ✅ 完了 |
| 2 | B7 MCMC (HMC/NUTS) | ✅ 完了 (B11 mass matrix で blackjax ESS 凌駕) |
| 2 | B7 残 (Gibbs/ADVI/WAIC/LOO) | ✅ 完了 (`mcmc_extras` suite) |
| 3 | B8 TS/Survival (ARIMA/Cox/KM/Quantile) | ✅ 完了 |
| 3 | B8 残 (Holt-Winters/GAM/Spline) | ✅ 完了 (`ts_extras` suite) |
| 4a | B9 Optim+ (Constrained/Adam/CMAESFull) | ✅ 完了 (`optim_plus` suite) |
| 4b | B10 Stat util (Bootstrap/tests/CV/...) | ✅ 完了 (`stat_util` suite) |
| 5a | B12 Multi-output | ✅ 完了 (`multi_output` suite) |
| 5b | B13 Regrid | ✅ 完了 (`regrid` suite) |

詳細は [SUMMARY.md](SUMMARY.md) と [OPEN_ISSUES.md](OPEN_ISSUES.md)。

---

## 旧計画 (履歴用に保持)


## 背景

Phase B1-B5 で regression / kernel / optim / mo / bo を整備済。残りの
Haskell library モジュール (約 50+) は未ベンチ。Hackage 公開後の信頼性
強化のため、段階的に拡充する計画。

## カバレッジ

### ✅ 既存 (B1-B5)

| Suite | 主モジュール | Python 比較 |
|---|---|---|
| regression | Model.{LM,GLM,GLMM,Regularized} | sklearn / statsmodels |
| kernel | Model.{Kernel,GP,GPRobust,RFF}, Stat.KernelDist | sklearn |
| optim | Optim.{NelderMead,LBFGS,DE,CMAES,SA,PSO} | scipy / cma / pyswarms |
| mo | Optim.NSGA | pymoo |
| bo | Optim.BayesOpt | scikit-optimize |

### ❌ 未カバー

主要 numerical で約 50 modules。下に Tier 別計画。

## Tier 1: ML 王道 (sklearn 直接比較)

**Suite: B6 — ML Classical** (工数 2 日)

| Bench | Module | Python 比較 | 試験データ |
|---|---|---|---|
| PCA | `Model.PCA` | `sklearn.decomposition.PCA` | n=10000 d=20 → 5 components |
| KMeans | `Model.Cluster` | `sklearn.cluster.KMeans` | n=10000 d=10, k=5 |
| DecisionTree | `Model.DecisionTree` | `sklearn.tree.DecisionTreeClassifier` | n=10000 p=20 |
| RandomForest | `Model.RandomForest` | `sklearn.ensemble.RandomForestClassifier` | n_trees=100 |

期待: PCA は SVD 経由で hmatrix 強い、ツリー系は sklearn の Cython 実装が速い予想。

## Tier 2: HBM / MCMC (本ライブラリの目玉)

**Suite: B7 — MCMC** (工数 2 日)

| Bench | Module | Python 比較 | 試験問題 |
|---|---|---|---|
| HMC | `MCMC.HMC` | `pymc` HMC / `blackjax.hmc` | 階層正規 (8 schools) |
| NUTS | `MCMC.NUTS` | `pymc` NUTS / `blackjax.nuts` | 同上 |
| Gibbs | `MCMC.Gibbs` | (純粋比較なし、conjugate のみ) | Beta-Binomial |
| ADVI | `Stat.VI` | `pymc.fit(method='advi')` / `numpyro.infer.SVI` | logistic posterior |
| WAIC/LOO | `Stat.ModelSelect` | `arviz.waic` / `arviz.loo` | sample-based 計算精度 |

期待: PyMC は backend に PyTensor (graph compile) を持つので冷起動は遅いが、定常状態は速いはず。**ESS/min 等の効率指標で勝負**できる可能性あり。

## Tier 3: 時系列・生存・補間

**Suite: B8 — TS/Survival/Spline/GAM** (工数 3.5 日)

| Bench | Module | Python 比較 | データ |
|---|---|---|---|
| ARIMA | `Model.TimeSeries` | `statsmodels.tsa.arima.ARIMA` | n=1000 ARIMA(1,1,1) |
| Holt-Winters | `Model.TimeSeries` | `statsmodels.tsa.holtwinters` | seasonal n=500 |
| Cox PH | `Model.Survival` | `lifelines.CoxPHFitter` | n=2000 events~50% |
| KM / NA | `Model.Survival` | `lifelines.{KaplanMeierFitter,NelsonAalenFitter}` | 同上 |
| Quantile | `Model.Quantile` | `statsmodels.regression.quantile_regression` | n=10000 p=10 |
| GAM | `Model.GAM` | `pygam.GAM` | n=2000 splines=10 |
| Spline | `Stat.Interpolate` | `scipy.interpolate.{interp1d,CubicSpline,PchipInterpolator}` | n=1000 grid |

## Tier 4: Optim 拡張・Stat utility

**Suite: B9 — Optim+** (工数 1.5 日)

| Bench | Module | Python 比較 |
|---|---|---|
| Constrained | `Optim.Constrained` | `scipy.optimize.minimize` (SLSQP / trust-constr) |
| Adam | `Optim.Adam` | torch.optim.Adam (要 torch 追加) |
| CMAESFull | `Optim.CMAESFull` | `cma` library full |

**Suite: B10 — Stat util** (工数 3 日)

| Bench | Module | Python 比較 |
|---|---|---|
| Bootstrap | `Stat.Bootstrap` | `scipy.stats.bootstrap` |
| t/KS/MW test | `Stat.Test` | `scipy.stats.{ttest_ind,ks_2samp,mannwhitneyu}` |
| MultipleTesting (Holm/BH) | `Stat.MultipleTesting` | `statsmodels.stats.multitest` |
| QuasiRandom (Halton/Sobol) | `Stat.QuasiRandom` | `scipy.stats.qmc.{Halton,Sobol}` |
| CV (k-fold) | `Stat.CV` | `sklearn.model_selection.KFold` |
| Interpret (PDP/ICE) | `Stat.Interpret` | `sklearn.inspection.partial_dependence` |
| ClassMetrics | `Stat.ClassMetrics` | `sklearn.metrics.*` |

## Tier 5: 多出力・Regrid

**Suite: B11 — Multi-output** (工数 1 日)

| Bench | Module | Python 比較 |
|---|---|---|
| MultiLM/GP | `Model.{MultiLM,Multivariate,MultiGP,MultiOutput}` | `sklearn.multioutput.MultiOutputRegressor` |

**Suite: B12 — Regrid** (工数 0.5 日)

| Bench | Module | Python 比較 |
|---|---|---|
| regridLong | `DataIO.Preprocess.regridLong` | pandas + scipy.interpolate 自前合成 |

## Tier 6 (skip 推奨)

ROI 低のため後回し / 不実施:

- `Design.{Factorial,RSM,Optimal,Taguchi,Orthogonal}` vs `pyDOE3` — 出力一致確認のみで十分
- `DataIO.{CSV,Sniff,Health,Clean,Reshape}` vs `pandas.read_csv` — 速度比較は意味薄い (Sniff/Health は Python に該当機能なし)
- `Viz.*` 全部 — 視覚化、速度比較は対象外

## 総工数 / 進路選択肢

| 選択肢 | スコープ | 工数 |
|---|---|---|
| A | B6 + B7 のみ | 4 日 |
| B | B6 + B7 + B8 | 7-8 日 |
| C | B6〜B12 全部 | 13.5 日 |
| D | B6〜B10 まで | 11 日 |
| E | 個別ピック | 工数小 |

私の推奨: **A** (主要 sklearn / PyMC 比較で Hackage 公開時の説得力強化)、
順次 **B** → **D** → **C** へ拡張。

## 必要な Python ライブラリ (準備済 ✅)

`bench/requirements.txt` に以下を追加済 (2026-05-06):

| ライブラリ | 版 | 用途 |
|---|---|---|
| pymc | 5.28.5 | B7 HMC/NUTS 比較 |
| blackjax | 1.5 | B7 HMC/NUTS lightweight 比較 |
| arviz | 0.23.4 | B7 WAIC/LOO/ESS/R-hat 比較 |
| lifelines | 0.30.3 | B8 Survival (Cox/KM/NA) 比較 |

既存で B6/B8-B10 の他 Python 側ライブラリは全て揃っている (numpy, scipy,
pandas, sklearn, statsmodels, pygam, jax, numpyro, cma, pyswarms, skopt,
pymoo)。

## 再開コマンド

```bash
# Python lib インストール (再構築時)
bench/venv/bin/pip install -r bench/requirements.txt

# import 確認
bench/venv/bin/python -c "import pymc, blackjax, arviz, lifelines; print('ok')"

# 着手するときは、選択肢 A を例にすると:
# 1. feature/perf-bench-b6 ブランチを切る
# 2. bench/haskell/BenchML.hs を新設 (PCA, KMeans, Tree, Forest)
# 3. bench/python/bench_ml.py を新設
# 4. cabal stanza 追加
# 5. SUMMARY.md 更新
```
