# PyMC 機能比較

> 🌐 [English](02-pymc-comparison.md) | **日本語**

hanalyze の **ベイズ統計部分** (`Hanalyze.Model.HBM` / MCMC / VI) を PyMC と
比較した機能対応表。古典的回帰・実験計画・多目的最適化など hanalyze
独自領域はここでは比較対象外。

PyMC にあって hanalyze にない機能を一覧化する。

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
| `Bound` | ✅ `Bound` | Phase 39-A3、 PyMC 互換 (内部は `Truncated` 委譲) |
| `Multinomial` | ✅ `Multinomial` | 観測専用、Dirichlet と組合せ |
| `NegativeBinomial` | ✅ `NegativeBinomial` | (μ, α) parameterization |
| `ZeroInflated*` | ✅ `ZeroInflatedPoisson` / `ZeroInflatedBinomial` | `ZeroInflatedNegativeBinomial` は Phase 37-A3 候補 |
| `InverseGamma` | ✅ `InverseGamma` | 分散の共役事前 |
| `Weibull` | ✅ `Weibull` | 生存解析 |
| `Pareto` | ✅ `Pareto` | 重い裾 |
| `BetaBinomial` | ✅ `BetaBinomial` | 過分散二項 |
| `VonMises` | ✅ `VonMises` | 角度データ (`logBesselI0` 実装) |
| `SkewNormal` | ✅ `SkewNormal` | Phase 37-A2、 Henze 1986 sampling |
| `Logistic` | ✅ `Logistic` | Phase 37-A2、 closed-form CDF |
| `Gumbel` | ✅ `Gumbel` | Phase 37-A2、 closed-form CDF (極値分布) |
| `AsymmetricLaplace` | ✅ `AsymmetricLaplace` | Phase 37-A2、 PyMC `(b, κ, μ)` 順、 closed-form CDF |
| `Triangular` | ✅ `Triangular` | Phase 39-A1、 closed-form CDF、 弱情報事前 |
| `Kumaraswamy` | ✅ `Kumaraswamy` | Phase 39-A1、 closed-form CDF、 Beta 代替 |
| `Rice` | ✅ `Rice` | Phase 39-A1、 `logBesselI0` 経由、 MRI / Rayleigh 拡張 |
| `OrderedLogistic` | ✅ `OrderedLogistic` | Phase 37-A3、 cuts 列で K カテゴリ |
| `DiscreteUniform` | ✅ `DiscreteUniform` | Phase 37-A3、 包含両端 |
| `Geometric` | ✅ `Geometric` | Phase 37-A3、 PyMC 慣例 (support 1, 2, …) |
| `HyperGeometric` | ✅ `HyperGeometric` | Phase 37-A3、 N/K/n パラメータ |
| `ZeroInflatedNegativeBinomial` | ✅ `ZeroInflatedNegativeBinomial` | Phase 37-A3、 ψ + NegBin の混合 |
| `DiscreteWeibull` | ✅ `DiscreteWeibull` | Phase 39-A1、 整数 Weibull、 観測専用 |
| `Wishart` | ✅ `Wishart` | Phase 39-A2、 観測専用 (k² flatten)、 共分散プライアの直接表現 |
| `OrderedProbit` | ✅ `OrderedProbit` | Phase 39-A3、 OrderedLogistic の Probit 版 (link 差し替え) |
| `MvStudentT` | ✅ `MvStudentT` | Phase 37-A4、 観測専用、 Cholesky 経由の Mahalanobis |
| `DirichletMultinomial` | ✅ `DirichletMultinomial` | Phase 37-A4、 観測専用、 K-vector counts |

## サンプラー

| PyMC | hanalyze | 備考 |
|---|---|---|
| `pymc.sample` (NUTS) | ✅ `Hanalyze.MCMC.NUTS` | dual averaging 付き |
| HMC | ✅ `Hanalyze.MCMC.HMC` | |
| Metropolis | ✅ `Hanalyze.MCMC.MH` | |
| `CompoundStep` (Gibbs+MH) | ✅ `Hanalyze.MCMC.Gibbs.gibbsMH` | 共役自動検出 |
| Slice | ✅ `Hanalyze.MCMC.Slice` | 勾配不要、ステップサイズ自動調整 |
| `pymc.fit` (ADVI) | ✅ `Hanalyze.Stat.VI.advi` | 平均場のみ |
| Full-rank ADVI | ✅ `Hanalyze.Stat.VI.fullRankAdvi` | Phase 37-A5、 下三角 Cholesky 因子 L で相関学習、 `viCovU` に L 出力 |
| 正規化フロー | ❌ | Stretch (研究レベル) |
| SMC (逐次モンテカルロ) | ✅ `Hanalyze.MCMC.SMC` | Phase 29-A1 (annealing + bridge sampling 経路) |

## 事後ワークフロー

| PyMC | hanalyze | 備考 |
|---|---|---|
| `pm.sample_posterior_predictive` | ✅ `Hanalyze.Stat.PosteriorPredictive.posteriorPredictive` | |
| `pm.sample_prior_predictive` | ✅ `Hanalyze.Stat.PosteriorPredictive.priorPredictive` | |
| `pm.set_data` (再構築なしでデータ差し替え) | ✅ `dataNamed` / `withData` | Rank-2 多相対応 |
| `pm.Deterministic` (派生量) | ✅ `deterministic` | `augmentChainWithDeterministic` で Chain に注入 |
| `pm.Potential` (任意 log 項追加) | ✅ `potential` | ソフト制約・カスタム尤度・正則化 |

## 診断・可視化 (ArviZ 相当)

| PyMC / ArviZ | hanalyze | 備考 |
|---|---|---|
| トレースプロット | ✅ `Hanalyze.Viz.MCMC.tracePlot` | |
| 事後 KDE | ✅ `Hanalyze.Viz.MCMC.posteriorPlot` | |
| ペア散布 | ✅ `Hanalyze.Viz.MCMC.pairScatter` | |
| 自己相関 | ✅ `Hanalyze.Viz.MCMC.autocorrPlot` | |
| Forest plot | ✅ `Hanalyze.Viz.MCMC.forestPlot` | |
| Energy plot (NUTS) | ✅ `Hanalyze.Viz.MCMC.energyPlot` | BFMI 値も表示 |
| BFMI スコア | ✅ `Hanalyze.Stat.MCMC.bfmi` | Betancourt 2016 |
| ESS / R-hat 表 | ✅ `Hanalyze.Viz.Report` | |
| 事後予測プロット | ✅ `Hanalyze.Viz.MCMC.ppcPlot` / `ppcPlotFile` | observed + posterior draws overlay |
| HDI 帯 (トレース / KDE) | ✅ `Hanalyze.Viz.MCMC.tracePlotHDI` / `tracePlotHDIFile` | |
| ランクプロット (PyMC `plot_rank`) | ✅ `Hanalyze.Viz.MCMC.rankPlot` / `rankPlotFile` | chain 間 rank ヒストグラム |
| Divergences の散布表示 | ✅ `Hanalyze.Viz.MCMC.pairScatterDiv` / `pairScatterDivFile` | NUTS divergent を pair scatter に重ねる |
| Posterior table (`az.summary` 相当) | ✅ `Hanalyze.Stat.Summary.posteriorSummary` / `Hanalyze.Viz.MCMC.posteriorSummaryHtml` / `printPosteriorSummary` | mean/sd/HDI/ESS/R̂ 表 |

## モデル比較

| PyMC | hanalyze | 備考 |
|---|---|---|
| `pm.waic` | ✅ `Hanalyze.Stat.ModelSelect.waic` | |
| `pm.loo` (PSIS-LOO) | ✅ `Hanalyze.Stat.ModelSelect.loo` | k̂ 診断付き |
| `pm.compare` (モデル重み、 Pseudo-BMA) | ✅ `Hanalyze.Stat.ModelSelect.compareModels` | LOO ベース |
| 真の BMA (周辺尤度ベース) | ✅ `Hanalyze.Stat.BayesianModelAveraging` | Phase 29-A3、 BridgeSampling と組合せ |
| ベイズファクター/周辺尤度 | ✅ `Hanalyze.Stat.BayesFactor` + `BridgeSampling` | Phase 29-A2 (Bridge を primary に使うこと、 SMC log marginal は bias あり) |

## モデリングプリミティブ

| PyMC | hanalyze | 備考 |
|---|---|---|
| 階層モデル | ✅ `Hanalyze.Model.HBM` 経由 | パターン書き方ガイド: [02-probabilistic-model.ja.md](bayesian/02-probabilistic-model.ja.md) |
| ランダム切片 | ✅ demo: `simpson-paradox` | docs: [demos.ja.md](bayesian/demos.ja.md) |
| ランダム傾き | ✅ demo: `hbm-random-slope` | docs: [demos.ja.md](bayesian/demos.ja.md) |
| 非中心化パラメタ化 | ✅ `nonCenteredNormal` | Neal's funnel で BFMI 改善実証 |
| Multi-level (3 階層 nested) | ✅ パターン 6 (組合せで) | helper 化は Phase 37-A6 候補 |
| Crossed random effects | ✅ パターン 7 (組合せで) | helper 化は Phase 37-A6 候補 |
| GLMM 風 1 行 helper | ✅ `glmmRandomIntercept` | Phase 37-A6、 Gaussian / Binomial / Poisson 対応、 random intercept |
| Hidden Markov Model (HMM) | ✅ `hmmLatent` + `hmmForwardLogLik` | Phase 39-A4、 K 状態 + Dirichlet 遷移 prior + log-space forward algorithm でマージナル化 (NUTS 互換)、 emission は外から `potential` で組み込む形 |
| Dirichlet Process / Stick-breaking 無限混合 | ✅ `dpStickBreaking` | Phase 39-A5、 truncation level T で打ち切る有限近似 |
| Ordered cuts helper (increasing 保証) | ✅ `orderedCuts` | Phase 39-A6、 c_min + HalfNormal 増分の累積 |
| 時系列 (AR / GP) | ✅ `ar1Latent` + GP | GP は既存 |
| ODE 尤度 | ❌ | Stretch (ODE ソルバ必要、 別 Phase) |
| ベイズ NN | ❌ | Stretch (研究レベル、 別 Phase) |
| カスタム log-density | ✅ `potential` | 任意 log 項を加算可能 |
| Mixture モデル | ✅ `Mixture` | log-sum-exp |
| Tobit / 検出限界 | ✅ `Censored` | |
| 切り詰め分布 | ✅ `Truncated` | |

## 残機能 (Phase 37 + Stretch、 2026-05-30 更新)

### Phase 37 で扱う予定 (実装可能、 PR 化見込みあり)

- [x] **分布 A2 (連続)**: ~~`SkewNormal` / `Logistic` / `Gumbel` / `AsymmetricLaplace`~~ ✅ (Phase 37-A2 完了 2026-05-30)
      残: `Triangular` / `Kumaraswamy` / `Rice` (低優先、 Phase 37-A2 候補)
- [x] **分布 A3 (離散)**: ~~`OrderedLogistic` / `DiscreteUniform` / `Geometric` / `HyperGeometric` / `ZeroInflatedNegativeBinomial`~~ ✅ (Phase 37-A3 完了 2026-05-30)
      残: `DiscreteWeibull` (低優先)
- [x] **分布 A4 (多変量)**: ~~`MvStudentT` / `DirichletMultinomial`~~ ✅ (Phase 37-A4 完了 2026-05-30)
      残: `Wishart` (LKJ で代替可、 後回し)
- [x] **A5 サンプラー**: ~~Full-rank ADVI~~ ✅ (Phase 37-A5 完了 2026-05-30、 `Hanalyze.Stat.VI.fullRankAdvi`)
- [x] **A6 階層 helper (中核)**: ~~`glmmRandomIntercept`~~ ✅ (Phase 37-A6 完了 2026-05-30、 Gaussian/Binomial/Poisson 対応)
      残: `hmmLatent` / `dpStickBreaking` (Phase 39 へ繰越、 forward-backward / 無限混合は独立実装重い)
- [ ] **A7 残 viz**: 大半は既実装 (rankPlot / ppcPlot / pairScatterDiv /
      tracePlotHDI / posteriorSummary) のため、 残作業はナビゲーション整理と
      docs アップデート中心

### Stretch (本 Phase スコープ外)

- [ ] 正規化フロー / Stein VI / Pathfinder (研究レベル)
- [ ] ODE 尤度 (Runge-Kutta + AD、 独立 Phase 推奨)
- [ ] ベイズ NN (BNN / Bayes by Backprop、 独立 Phase 推奨)
- [ ] `Bound` distribution (`Truncated` で代替可、 単独追加価値低)

### Phase 29 で閉じた項目 (2026 春)

- [x] SMC (`Hanalyze.MCMC.SMC`)
- [x] Bayes factor / 周辺尤度 (`Hanalyze.Stat.BayesFactor` + `BridgeSampling`)
- [x] 真 BMA (`Hanalyze.Stat.BayesianModelAveraging`)

### 実装状況の可視化

`pymc-status-demo` 実行ファイルで PyMC 機能の実装状況を棒グラフ HTML として
出力する:

```bash
cabal run pymc-status-demo
# → pymc-status.html (カテゴリ別の ✅/🚧/❌ 件数)
```
