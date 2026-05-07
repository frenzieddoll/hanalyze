# PyMC 機能比較

> 🌐 [English](02-pymc-comparison.md) | **日本語**

hanalyze の **ベイズ統計部分** (`Model.HBM` / MCMC / VI) を PyMC と
比較した機能対応表。古典的回帰・実験計画・多目的最適化など hanalyze
独自領域はここでは比較対象外。

PyMC にあって hanalyze にない機能を一覧化し、実装ロードマップとして使う。

## ステータス凡例

- ✅ 実装済み
- 🚧 部分実装中
- ❌ 未実装
- ➖ 対象外

## 確率分布

| PyMC | hanalyze | 備考 |
|---|---|---|
| `Normal` | ✅ `Normal` | |
| `Exponential` | ✅ `Exponential` | |
| `Gamma` | ✅ `Gamma` | rate パラメタ化 |
| `Beta` | ✅ `Beta` | |
| `Poisson` | ✅ `Poisson` | |
| `Binomial` | ✅ `Binomial` | |
| `Uniform` | ✅ `Uniform` | |
| `StudentT` | ✅ `StudentT` | df / loc / scale |
| `Cauchy` | ✅ `Cauchy` | |
| `HalfNormal` | ✅ `HalfNormal` | |
| `HalfCauchy` | ✅ `HalfCauchy` | |
| `LogNormal` | ✅ `LogNormal` | |
| `Bernoulli` | ✅ `Bernoulli` | 観測のみ |
| `Categorical` | ✅ `Categorical` | 観測のみ |
| `MvNormal` | ✅ `MvNormal` (観測) + `mvNormalLatent` | Cholesky で観測も latent も対応 |
| `Dirichlet` | ✅ `dirichlet` | stick-breaking で latent 化 |
| `LKJCholeskyCov` | ✅ `lkjCorrCholesky` | CPC 法、K 任意次元 (J1 で K=3 検証) |
| `Mixture` | ✅ `Mixture` | log-sum-exp、観測/サンプル両対応 |
| `Truncated*` | ✅ `Truncated` | 任意分布を切り詰め (CDF 必要) |
| `Censored` | ✅ `Censored` | 検出限界 (Tobit 風) |
| `Bound` | ❌ | Stretch — `Truncated` でほぼ代替可 |
| `Multinomial` | ✅ `Multinomial` | 観測専用、Dirichlet と組合せ |
| `NegativeBinomial` | ✅ `NegativeBinomial` | (μ, α) parameterization |
| `ZeroInflated*` | ✅ `ZeroInflatedPoisson` / `ZeroInflatedBinomial` | |
| `InverseGamma` | ✅ `InverseGamma` | 分散の共役事前 |
| `Weibull` | ✅ `Weibull` | 生存解析 |
| `Pareto` | ✅ `Pareto` | 重い裾 |
| `BetaBinomial` | ✅ `BetaBinomial` | 過分散二項 |
| `VonMises` | ✅ `VonMises` | 角度データ (`logBesselI0` 実装) |
| `Wishart` | ❌ | Stretch — LKJ で代替推奨 |
| `Multivariate-t` | ❌ | Stretch |

## サンプラー

| PyMC | hanalyze | 備考 |
|---|---|---|
| `pymc.sample` (NUTS) | ✅ `MCMC.NUTS` | dual averaging 付き |
| HMC | ✅ `MCMC.HMC` | |
| Metropolis | ✅ `MCMC.MH` | |
| `CompoundStep` (Gibbs+MH) | ✅ `MCMC.Gibbs.gibbsMH` | 共役自動検出 |
| Slice | ✅ `MCMC.Slice` | 勾配不要、ステップサイズ自動調整 |
| `pymc.fit` (ADVI) | ✅ `Stat.VI.advi` | 平均場のみ |
| Full-rank ADVI | ❌ | Stretch |
| 正規化フロー | ❌ | Stretch |
| SMC (逐次モンテカルロ) | ❌ | Stretch |

## 事後ワークフロー

| PyMC | hanalyze | 備考 |
|---|---|---|
| `pm.sample_posterior_predictive` | ✅ `Stat.PosteriorPredictive.posteriorPredictive` | |
| `pm.sample_prior_predictive` | ✅ `Stat.PosteriorPredictive.priorPredictive` | |
| `pm.set_data` (再構築なしでデータ差し替え) | ✅ `dataNamed` / `withData` | Rank-2 多相対応 |
| `pm.Deterministic` (派生量) | ✅ `deterministic` | `augmentChainWithDeterministic` で Chain に注入 |
| `pm.Potential` (任意 log 項追加) | ✅ `potential` | ソフト制約・カスタム尤度・正則化 |

## 診断・可視化 (ArviZ 相当)

| PyMC / ArviZ | hanalyze | 備考 |
|---|---|---|
| トレースプロット | ✅ `Viz.MCMC.tracePlot` | |
| 事後 KDE | ✅ `Viz.MCMC.posteriorPlot` | |
| ペア散布 | ✅ `Viz.MCMC.pairScatter` | |
| 自己相関 | ✅ `Viz.MCMC.autocorrPlot` | |
| Forest plot | ✅ `Viz.MCMC.forestPlot` | |
| Energy plot (NUTS) | ✅ `Viz.MCMC.energyPlot` | BFMI 値も表示 |
| BFMI スコア | ✅ `Stat.MCMC.bfmi` | Betancourt 2016 |
| ESS / R-hat 表 | ✅ `Viz.Report` | |
| 事後予測プロット | ❌ | TODO — `posteriorPredictive` 結果を Vega-Lite で |
| HDI 帯 (トレース / KDE) | 🚧 一部 | |
| ランクプロット (PyMC `plot_rank`) | ❌ | TODO — チェーン間の収束診断 |
| Divergences の散布表示 | ❌ | TODO — NUTS の divergent 検出&可視化 |
| Posterior table (`az.summary` 相当) | 🚧 `Viz.Report` 内に部分 | TODO — 単独ヘルパ |

## モデル比較

| PyMC | hanalyze | 備考 |
|---|---|---|
| `pm.waic` | ✅ `Stat.ModelSelect.waic` | |
| `pm.loo` (PSIS-LOO) | ✅ `Stat.ModelSelect.loo` | k̂ 診断付き |
| `pm.compare` (モデル重み) | ✅ `Stat.ModelSelect.compareModels` | | Pseudo-BMA |
| ベイズファクター/周辺尤度 | ❌ | Stretch |

## モデリングプリミティブ

| PyMC | hanalyze | 備考 |
|---|---|---|
| 階層モデル | ✅ `Model.HBM` 経由 | |
| ランダム切片 | ✅ demo: `simpson-paradox` | |
| ランダム傾き | ✅ demo: `hbm-random-slope` | |
| 非中心化パラメタ化 | ✅ `nonCenteredNormal` | Neal's funnel で BFMI 改善実証 |
| 時系列 (AR / GP) | ✅ `ar1Latent` + GP | |
| ODE 尤度 | ❌ | Stretch (ODE ソルバ必要) |
| ベイズ NN | ❌ | Stretch |
| カスタム log-density | ✅ `potential` | 任意 log 項を加算可能 |
| Mixture モデル | ✅ `Mixture` | log-sum-exp |
| Tobit / 検出限界 | ✅ `Censored` | |
| 切り詰め分布 | ✅ `Truncated` | |

## 残ストレッチ機能

以下は研究レベル / 優先度低のため未実装:

- [ ] `Wishart` / `Multivariate-t` (LKJ で代替推奨)
- [ ] Full-rank ADVI / Normalizing flows / SMC
- [ ] ODE 尤度 (Runge-Kutta + AD)
- [ ] ベイズ NN (隠れ層、研究レベル)
- [ ] ベイズファクター / 周辺尤度 (重要度サンプリング系)

### 実装状況の可視化

`pymc-status-demo` 実行ファイルで PyMC 機能の実装状況を棒グラフ HTML として
出力する:

```bash
cabal run pymc-status-demo
# → pymc-status.html (カテゴリ別の ✅/🚧/❌ 件数)
```

> 機能追加の経緯・コミット履歴は [CHANGELOG.md](../CHANGELOG.md) を参照。
