# PyMC 機能比較

> 🌐 [English](08-pymc-comparison.md) | **日本語**

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
| `Uniform` | ✅ `Uniform` | Phase 2.1 完了 |
| `StudentT` | ✅ `StudentT` | Phase 2.1 — df / loc / scale |
| `Cauchy` | ✅ `Cauchy` | Phase 2.1 |
| `HalfNormal` | ✅ `HalfNormal` | Phase 2.1 |
| `HalfCauchy` | ✅ `HalfCauchy` | Phase 2.1 |
| `LogNormal` | ✅ `LogNormal` | Phase 2.1 |
| `Bernoulli` | ✅ `Bernoulli` | Phase 2.2 — 観測のみ |
| `Categorical` | ✅ `Categorical` | Phase 2.2 — 観測のみ |
| `MvNormal` | ❌ | Phase 2.4 — 多変量 |
| `Dirichlet` | ❌ | Phase 2.4 |
| `LKJCholeskyCov` | ❌ | Stretch — 共分散事前 |
| `Mixture` | ❌ | Phase 2.5 |
| `Truncated*` | ❌ | Stretch |
| `Censored` | ❌ | Stretch |
| `Bound` | ❌ | Stretch |

## サンプラー

| PyMC | hanalyze | 備考 |
|---|---|---|
| `pymc.sample` (NUTS) | ✅ `MCMC.NUTS` | dual averaging 付き |
| HMC | ✅ `MCMC.HMC` | |
| Metropolis | ✅ `MCMC.MH` | |
| `CompoundStep` (Gibbs+MH) | ✅ `MCMC.Gibbs.gibbsMH` | 共役自動検出 |
| Slice | ❌ | Stretch |
| `pymc.fit` (ADVI) | ✅ `Stat.VI.advi` | 平均場のみ |
| Full-rank ADVI | ❌ | Stretch |
| 正規化フロー | ❌ | Stretch |
| SMC (逐次モンテカルロ) | ❌ | Stretch |

## 事後ワークフロー

| PyMC | hanalyze | 備考 |
|---|---|---|
| `pm.sample_posterior_predictive` | ✅ `Stat.PosteriorPredictive.posteriorPredictive` | Phase 2.3 完了 |
| `pm.sample_prior_predictive` | ✅ `Stat.PosteriorPredictive.priorPredictive` | Phase 2.3 完了 |
| `pm.set_data` (再構築なしでデータ差し替え) | ❌ | Stretch (DSL に `Data` プリミティブが要る) |
| `pm.Deterministic` (派生量) | ❌ | Stretch |
| `pm.Potential` (任意 log 項追加) | ❌ | Stretch |

## 診断・可視化 (ArviZ 相当)

| PyMC / ArviZ | hanalyze | 備考 |
|---|---|---|
| トレースプロット | ✅ `Viz.MCMC.tracePlot` | |
| 事後 KDE | ✅ `Viz.MCMC.posteriorPlot` | |
| ペア散布 | ✅ `Viz.MCMC.pairScatter` | |
| 自己相関 | ✅ `Viz.MCMC.autocorrPlot` | |
| Forest plot | ✅ `Viz.MCMC.forestPlot` | Phase 3.1 完了 |
| Energy plot (NUTS) | ❌ | Phase 3.2 |
| ESS / R-hat 表 | ✅ `Viz.Report` | |
| 事後予測プロット | ❌ | Phase 2.3 と連動 |
| HDI 帯 (トレース / KDE) | 🚧 一部 | |

## モデル比較

| PyMC | hanalyze | 備考 |
|---|---|---|
| `pm.waic` | ✅ `Stat.ModelSelect.waic` | |
| `pm.loo` (PSIS-LOO) | ✅ `Stat.ModelSelect.loo` | k̂ 診断付き |
| `pm.compare` (モデル重み) | ✅ `Stat.ModelSelect.compareModels` | Phase 3.3 完了 — Pseudo-BMA |
| ベイズファクター/周辺尤度 | ❌ | Stretch |

## モデリングプリミティブ

| PyMC | hanalyze | 備考 |
|---|---|---|
| 階層モデル | ✅ `Model.HBM` 経由 | |
| ランダム切片 | ✅ demo: `simpson-paradox` | |
| ランダム傾き | ✅ demo: `hbm-random-slope` | |
| 非中心化パラメタ化 | ❌ | Phase 3.4 (ヘルパ) |
| 時系列 (AR / GP) | 🚧 GP のみ (`Model.GP`) | AR 未実装 |
| ODE 尤度 | ❌ | Stretch (ODE ソルバ必要) |
| ベイズ NN | ❌ | Stretch |
| カスタム log-density | ➖ | `observe` で任意分布が使えるので対応済み |

## 実装ロードマップ (このブランチ)

優しい順に並べた、自己完結したコミット単位:

1. ✅ **Phase 2.1 — 連続分布**
   `Uniform`, `StudentT`, `Cauchy`, `HalfNormal`, `HalfCauchy`, `LogNormal`
   → demo: `new-distrib-demo`
2. ✅ **Phase 2.2 — 離散観測分布**
   `Bernoulli`, `Categorical` (観測専用; DSL が `Floating a` 多相のため
   潜在変数としては不可)
   → demo: `discrete-obs-demo`
3. ✅ **Phase 2.3 — 事後/事前予測サンプリング**
   `Stat.PosteriorPredictive` モジュール: `posteriorPredictive`,
   `priorPredictive`, `samplePrior`, `posteriorPredictiveSummary`
   → demo: `ppc-demo`
4. ❌ **Phase 2.4 — 多変量分布**
   `MvNormal` (Cholesky), `Dirichlet` — DSL 拡張が必要
5. ❌ **Phase 2.5 — 混合分布** log-sum-exp で重み付き対数尤度
6. ✅ **Phase 3.1 — Forest plot** (`Viz.MCMC.forestPlot`)
   → demo: `forest-compare`
7. ❌ **Phase 3.2 — Energy plot** (NUTS BFMI; ステップごとのエネルギーを露出する必要)
8. ✅ **Phase 3.3 — `compare` モデル重み** (`Stat.ModelSelect.compareModels`)
   elpd_loo に基づく Pseudo-BMA
   → demo: `forest-compare`
