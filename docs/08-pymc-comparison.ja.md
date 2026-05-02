# PyMC 機能比較

> 🌐 [English](08-pymc-comparison.md) | **日本語**

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
| `Uniform` | ✅ `Uniform` | Phase 2.1 完了 |
| `StudentT` | ✅ `StudentT` | Phase 2.1 — df / loc / scale |
| `Cauchy` | ✅ `Cauchy` | Phase 2.1 |
| `HalfNormal` | ✅ `HalfNormal` | Phase 2.1 |
| `HalfCauchy` | ✅ `HalfCauchy` | Phase 2.1 |
| `LogNormal` | ✅ `LogNormal` | Phase 2.1 |
| `Bernoulli` | ✅ `Bernoulli` | Phase 2.2 — 観測のみ |
| `Categorical` | ✅ `Categorical` | Phase 2.2 — 観測のみ |
| `MvNormal` | ✅ `MvNormal` (観測) + `mvNormalLatent` | Phase D / G6 — Cholesky で観測も latent も対応 |
| `Dirichlet` | ✅ `dirichlet` | Phase G2 — stick-breaking で latent 化 |
| `LKJCholeskyCov` | ✅ `lkjCorrCholesky` | Phase H4 — CPC 法、K 任意次元 (J1 で K=3 検証) |
| `Mixture` | ✅ `Mixture` | Phase B — log-sum-exp、観測/サンプル両対応 |
| `Truncated*` | ✅ `Truncated` | Phase C — 任意分布を切り詰め (CDF 必要) |
| `Censored` | ✅ `Censored` | Phase C — 検出限界 (Tobit 風) |
| `Bound` | ❌ | Stretch — `Truncated` でほぼ代替可 |
| `Multinomial` | ✅ `Multinomial` | Phase H2 — 観測専用、Dirichlet と組合せ |
| `NegativeBinomial` | ✅ `NegativeBinomial` | Phase H1 — (μ, α) parameterization |
| `ZeroInflated*` | ✅ `ZeroInflatedPoisson` / `ZeroInflatedBinomial` | Phase H3 |
| `InverseGamma` | ✅ `InverseGamma` | Phase I — 分散の共役事前 |
| `Weibull` | ✅ `Weibull` | Phase I — 生存解析 |
| `Pareto` | ✅ `Pareto` | Phase I — 重い裾 |
| `BetaBinomial` | ✅ `BetaBinomial` | Phase I — 過分散二項 |
| `VonMises` | ✅ `VonMises` | Phase I — 角度データ (`logBesselI0` 実装) |
| `Wishart` | ❌ | Stretch — LKJ で代替推奨 |
| `Multivariate-t` | ❌ | Stretch |

## サンプラー

| PyMC | hanalyze | 備考 |
|---|---|---|
| `pymc.sample` (NUTS) | ✅ `MCMC.NUTS` | dual averaging 付き |
| HMC | ✅ `MCMC.HMC` | |
| Metropolis | ✅ `MCMC.MH` | |
| `CompoundStep` (Gibbs+MH) | ✅ `MCMC.Gibbs.gibbsMH` | 共役自動検出 |
| Slice | ✅ `MCMC.Slice` | Phase J3 — 勾配不要、ステップサイズ自動調整 |
| `pymc.fit` (ADVI) | ✅ `Stat.VI.advi` | 平均場のみ |
| Full-rank ADVI | ❌ | Stretch |
| 正規化フロー | ❌ | Stretch |
| SMC (逐次モンテカルロ) | ❌ | Stretch |

## 事後ワークフロー

| PyMC | hanalyze | 備考 |
|---|---|---|
| `pm.sample_posterior_predictive` | ✅ `Stat.PosteriorPredictive.posteriorPredictive` | Phase 2.3 完了 |
| `pm.sample_prior_predictive` | ✅ `Stat.PosteriorPredictive.priorPredictive` | Phase 2.3 完了 |
| `pm.set_data` (再構築なしでデータ差し替え) | ✅ `dataNamed` / `withData` | Phase G5 + H5 — Rank-2 多相対応 |
| `pm.Deterministic` (派生量) | ✅ `deterministic` | Phase G1 — `augmentChainWithDeterministic` で Chain に注入 |
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
| 非中心化パラメタ化 | ✅ `nonCenteredNormal` | Phase G3 — Neal's funnel で BFMI 改善実証 |
| 時系列 (AR / GP) | ✅ `ar1Latent` + GP | Phase J2 (AR) / GP は既存 |
| ODE 尤度 | ❌ | Stretch (ODE ソルバ必要) |
| ベイズ NN | ❌ | Stretch |
| カスタム log-density | ✅ `potential` | Phase A — 任意 log 項を加算可能 |
| Mixture モデル | ✅ `Mixture` | Phase B — log-sum-exp |
| Tobit / 検出限界 | ✅ `Censored` | Phase C |
| 切り詰め分布 | ✅ `Truncated` | Phase C |

## 実装ロードマップ

### 完了したフェーズ (test/newFunction ブランチ)

| Phase | 機能 | demo | commit |
|---|---|---|---|
| 2.1 | 連続分布 6 種 | `new-distrib-demo` | master |
| 2.2 | 離散観測分布 (Bernoulli/Categorical) | `discrete-obs-demo` | master |
| 2.3 | 事後/事前予測サンプリング | `ppc-demo` | master |
| 3.1 | Forest plot | `forest-compare` | master |
| 3.3 | Pseudo-BMA モデル重み | `forest-compare` | master |
| **A** | `pm.Potential` 相当 | `potential-demo` | `0a59ce9` |
| **B** | `pm.Mixture` 相当 | `mixture-demo` | `aa29606` |
| **C** | `Truncated` / `Censored` 分布 | `trunc-censor-demo` | `ab51fa0` |
| **C+** | Beta/Gamma/Cauchy/StudentT/HalfCauchy の CDF | `cdf-test` | `ed2a413` |
| **D** | `MvNormal` 観測専用 (Cholesky) | `mvnormal-demo` | `d476eb2` |
| **E** | Energy plot / BFMI 診断 | `energy-demo` | `68b4b8e` |
| **F1** | Posterior summary (`az.summary` 相当) | `summary-demo` | `58ab3c5` |
| **F2** | HDI-shaded trace plot | `summary-demo` | `0a46311` |
| **F3** | Rank plot (多チェーン収束) | `summary-demo` | `951b741` |
| **F4** | Posterior predictive check (`pp_check`) | `summary-demo` | `a07620f` |
| **F5** | Divergence overlay (描画機構) | `summary-demo` | `095e45b` |
| **G1** | `pm.Deterministic` 派生量 | `deterministic-demo` | `746f3fc` |
| **G2** | `Dirichlet` (stick-breaking) | `dirichlet-demo` | `c41da79` |
| **G3** | `nonCenteredNormal` ヘルパ | `noncentered-demo` | `4f45466` |
| **G4** | NUTS divergence 検出 | `noncentered-demo` | `5ee98ed` |
| **G5** | `pm.set_data` (`dataNamed`/`withData`) | `setdata-demo` | `695a7aa` |
| **G6** | `mvNormalLatent` (latent 化) | `mvnormal-latent-demo` | `866904e` |
| **H1** | `NegativeBinomial` (過分散カウント) | `negbinom-demo` | `d89907b` |
| **H2** | `Multinomial` (観測) | `multinomial-demo` | `b2f8b3c` |
| **H3** | `ZeroInflatedPoisson` / `ZeroInflatedBinomial` | `zeroinflated-demo` | `68a7ffe` |
| **H4** | `lkjCorrCholesky` (LKJ 相関事前) | `lkj-demo` | `57f0cc2` |
| **H5** | `withData` Rank-2 多相対応 | `setdata-demo` | `d046b79` |
| **H6** | `Stat.Summary` 切り出し | (refactor) | `3cc343d` |
| **I** | `InverseGamma`/`Weibull`/`Pareto`/`BetaBinomial`/`VonMises` | `newdistribs-demo` | `ea6faa5` |
| **J1** | LKJ K=3 検証 | `lkj3d-demo` | `480ab17` |
| **J2** | `ar1Latent` (AR(1) 状態空間) | `ar1-demo` | `1ad8b84` |
| **J3** | Slice sampler | `slice-demo` | `e1f7a28` |

### 残課題 TODO (Stretch)

主要な PyMC 機能ギャップは Phase A〜J で解消済み。残るのは研究レベル
または高度すぎて優先度の低いもの:

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
