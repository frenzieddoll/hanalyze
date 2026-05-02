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
| `Uniform` | ❌ | Phase 2.1 |
| `StudentT` | ❌ | Phase 2.1 — df / loc / scale |
| `Cauchy` | ❌ | Phase 2.1 |
| `HalfNormal` | ❌ | Phase 2.1 — 分散の事前分布で頻出 |
| `HalfCauchy` | ❌ | Phase 2.1 — 重い裾の分散事前 |
| `LogNormal` | ❌ | Phase 2.1 |
| `Bernoulli` | ❌ | Phase 2.2 — 離散 |
| `Categorical` | ❌ | Phase 2.2 — 離散 |
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
| `pm.sample_posterior_predictive` | ❌ | Phase 2.3 |
| `pm.sample_prior_predictive` | ❌ | Phase 2.3 (軽量) |
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
| Forest plot | ❌ | Phase 3.1 |
| Energy plot (NUTS) | ❌ | Phase 3.2 |
| ESS / R-hat 表 | ✅ `Viz.Report` | |
| 事後予測プロット | ❌ | Phase 2.3 と連動 |
| HDI 帯 (トレース / KDE) | 🚧 一部 | |

## モデル比較

| PyMC | hanalyze | 備考 |
|---|---|---|
| `pm.waic` | ✅ `Stat.ModelSelect.waic` | |
| `pm.loo` (PSIS-LOO) | ✅ `Stat.ModelSelect.loo` | k̂ 診断付き |
| `pm.compare` (モデル重み) | ❌ | Phase 3.3 |
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

1. **Phase 2.1 — 連続分布** (このコミット群)
   `Uniform`, `StudentT`, `Cauchy`, `HalfNormal`, `HalfCauchy`, `LogNormal`
2. **Phase 2.2 — 離散潜在変数**
   `Bernoulli`, `Categorical` (HMC/NUTS は勾配が要るため Gibbs/MH のみ)
3. **Phase 2.3 — 事後/事前予測サンプリング**
   チェーンから新しい観測を生成 + 観測ごとの予測密度
4. **Phase 2.4 — 多変量分布**
   `MvNormal` (Cholesky), `Dirichlet`
5. **Phase 2.5 — 混合分布**
   log-sum-exp で重み付き対数尤度
6. **Phase 3.1 — Forest plot**
7. **Phase 3.2 — Energy plot** (NUTS 診断)
8. **Phase 3.3 — `compare` (モデル重み)** Pseudo-BMA / stacking
