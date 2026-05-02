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
| `MvNormal` | 🚧 `MvNormal` | Phase D — **観測専用**。自前 Cholesky で AD/Track 互換 |
| `Dirichlet` | ❌ | TODO — シンプレックス制約の変換が必要 |
| `LKJCholeskyCov` | ❌ | Stretch — 共分散事前 |
| `Mixture` | ✅ `Mixture` | Phase B — log-sum-exp、観測/サンプル両対応 |
| `Truncated*` | ✅ `Truncated` | Phase C — 任意分布を切り詰め (CDF 必要) |
| `Censored` | ✅ `Censored` | Phase C — 検出限界 (Tobit 風) |
| `Bound` | ❌ | Stretch — `Truncated` でほぼ代替可 |
| Multinomial | ❌ | TODO — 多項観測 |
| Wishart / InverseGamma | ❌ | TODO — 共役事前用 |
| ZeroInflated (Poisson/Binomial) | ❌ | TODO — ゼロ過剰モデル |
| NegativeBinomial | ❌ | TODO — 過分散カウント |
| Weibull / Pareto / Beta-Binomial | ❌ | TODO |

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
| `pm.set_data` (再構築なしでデータ差し替え) | ❌ | TODO (DSL に `Data` プリミティブが要る) |
| `pm.Deterministic` (派生量) | ❌ | TODO — 名前付き派生量を Chain に保存する仕組み |
| `pm.Potential` (任意 log 項追加) | ✅ `potential` | Phase A — ソフト制約・カスタム尤度・正則化 |

## 診断・可視化 (ArviZ 相当)

| PyMC / ArviZ | hanalyze | 備考 |
|---|---|---|
| トレースプロット | ✅ `Viz.MCMC.tracePlot` | |
| 事後 KDE | ✅ `Viz.MCMC.posteriorPlot` | |
| ペア散布 | ✅ `Viz.MCMC.pairScatter` | |
| 自己相関 | ✅ `Viz.MCMC.autocorrPlot` | |
| Forest plot | ✅ `Viz.MCMC.forestPlot` | Phase 3.1 完了 |
| Energy plot (NUTS) | ✅ `Viz.MCMC.energyPlot` | Phase E — BFMI 値も表示 |
| BFMI スコア | ✅ `Stat.MCMC.bfmi` | Phase E — Betancourt 2016 |
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
| `pm.compare` (モデル重み) | ✅ `Stat.ModelSelect.compareModels` | Phase 3.3 完了 — Pseudo-BMA |
| ベイズファクター/周辺尤度 | ❌ | Stretch |

## モデリングプリミティブ

| PyMC | hanalyze | 備考 |
|---|---|---|
| 階層モデル | ✅ `Model.HBM` 経由 | |
| ランダム切片 | ✅ demo: `simpson-paradox` | |
| ランダム傾き | ✅ demo: `hbm-random-slope` | |
| 非中心化パラメタ化 | ❌ | TODO — Neal's funnel 用ヘルパ |
| 時系列 (AR / GP) | 🚧 GP のみ (`Model.GP`) | AR / 状態空間モデル未実装 |
| ODE 尤度 | ❌ | Stretch (ODE ソルバ必要) |
| ベイズ NN | ❌ | Stretch |
| カスタム log-density | ✅ `potential` | Phase A — 任意 log 項を加算可能 |
| Mixture モデル | ✅ `Mixture` | Phase B — log-sum-exp |
| Tobit / 検出限界 | ✅ `Censored` | Phase C |
| 切り詰め分布 | ✅ `Truncated` | Phase C |

## 実装ロードマップ

### 完了した PyMC parity フェーズ

| Phase | 機能 | demo | コミット |
|---|---|---|---|
| 2.1 | 連続分布 (Uniform/StudentT/Cauchy/HalfNormal/HalfCauchy/LogNormal) | `new-distrib-demo` | (master) |
| 2.2 | 離散観測分布 (Bernoulli/Categorical) | `discrete-obs-demo` | (master) |
| 2.3 | 事後/事前予測サンプリング | `ppc-demo` | (master) |
| 3.1 | Forest plot | `forest-compare` | (master) |
| 3.3 | `compare` モデル重み (Pseudo-BMA) | `forest-compare` | (master) |
| **A** | **`pm.Potential` 相当** | `potential-demo` | `0a59ce9` |
| **B** | **`pm.Mixture` 相当** | `mixture-demo` | `aa29606` |
| **C** | **`Truncated` / `Censored`** | `trunc-censor-demo` | `ab51fa0` |
| **C+** | **Beta/Gamma/Cauchy/StudentT CDF** | `cdf-test` | `ed2a413` |
| **D** | **`MvNormal` (観測専用)** | `mvnormal-demo` | `d476eb2` |
| **E** | **Energy plot / BFMI** | `energy-demo` | `68b4b8e` |

### 残課題 TODO (優先度順)

#### 高 — 主要な PyMC 機能ギャップ

- [ ] **`pm.Deterministic`** — 派生量 (e.g. `tau = 1/sigma^2`) を Chain に保存する仕組み。
      DSL に `deterministic :: Text -> a -> Model a a` を追加し、`Chain` にも別フィールドが必要。
- [ ] **`Dirichlet` 分布** — シンプレックス制約 (棒折り変換) が要。Mixture の重みや
      Categorical の事前として頻用。
- [ ] **`MvNormal` の latent 対応** — 現状観測専用。Cholesky factor 自体を latent に
      する場合、正定性制約 + LKJ 事前が必要。
- [ ] **Divergences の検出と可視化** — NUTS で energy 跳躍が大きい反復をマークし
      pair plot 上に重ねる (PyMC `plot_pair(divergences=True)` 相当)。
- [ ] **非中心化パラメタ化ヘルパ** — Neal's funnel 等で raw + scale の自動変換。
- [ ] **`pm.set_data`** — DSL に `Data` プレースホルダを追加、データのみ差し替え可能に。

#### 中 — 統計分布

- [ ] **NegativeBinomial** — 過分散カウントデータ。
- [ ] **Multinomial** — 多項観測。
- [ ] **ZeroInflated Poisson / Binomial** — ゼロ過剰モデル。
- [ ] **Weibull / Pareto / Beta-Binomial / VonMises** — 補助分布群。
- [ ] **Wishart / InverseGamma** — 共役事前として。

#### 中 — 可視化・診断

- [ ] **事後予測プロット (`pp_check`)** — `posteriorPredictive` 結果を観測に重ねる
      KDE / ECDF / ヒストグラム。
- [ ] **Posterior table (`az.summary` 相当)** — 単独ヘルパ (`Viz.Report` から分離)。
- [ ] **Rank plot** — 多チェーン収束の視覚診断。
- [ ] **HDI 帯付きトレース** — トレース上に 94% HDI を重ね描き。

#### 低 — Stretch

- [ ] **`LKJCholeskyCov`** — 共分散行列の事前 (相関行列 + scale 分解)。
- [ ] **Slice sampler / SMC / Full-rank ADVI / Normalizing flows** — サンプラー多様化。
- [ ] **AR / 状態空間モデル** — 時系列。
- [ ] **ODE 尤度** — Runge-Kutta ソルバ + AD。
- [ ] **ベイズ NN** — 隠れ層 + 重み事前。

### 実装状況の可視化

`pymc-status-demo` 実行ファイルで PyMC 機能の実装状況を棒グラフ HTML として
出力する:

```bash
cabal run pymc-status-demo
# → pymc-status.html (カテゴリ別の ✅/🚧/❌ 件数)
```
