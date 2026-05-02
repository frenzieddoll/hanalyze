# PyMC feature comparison

> 🌐 **English** | [日本語](02-pymc-comparison.ja.md)

A feature mapping that compares the **Bayesian portion** of hanalyze
(`Model.HBM` / MCMC / VI) against PyMC. Areas exclusive to hanalyze
(classical regression, DOE, multi-objective optimization, etc.) are out of
scope here.

This page lists features that PyMC has but hanalyze does not, and serves as
the implementation roadmap.

## Status legend

- ✅ Implemented
- 🚧 Partial
- ❌ Not implemented
- ➖ Out of scope

## Probability distributions

| PyMC | hanalyze | Notes |
|---|---|---|
| `Normal` | ✅ `Normal` | |
| `Exponential` | ✅ `Exponential` | |
| `Gamma` | ✅ `Gamma` | rate parameterization |
| `Beta` | ✅ `Beta` | |
| `Poisson` | ✅ `Poisson` | |
| `Binomial` | ✅ `Binomial` | |
| `Uniform` | ✅ `Uniform` | Phase 2.1 done |
| `StudentT` | ✅ `StudentT` | Phase 2.1 — df / loc / scale |
| `Cauchy` | ✅ `Cauchy` | Phase 2.1 |
| `HalfNormal` | ✅ `HalfNormal` | Phase 2.1 |
| `HalfCauchy` | ✅ `HalfCauchy` | Phase 2.1 |
| `LogNormal` | ✅ `LogNormal` | Phase 2.1 |
| `Bernoulli` | ✅ `Bernoulli` | Phase 2.2 — observed only |
| `Categorical` | ✅ `Categorical` | Phase 2.2 — observed only |
| `MvNormal` | ✅ `MvNormal` (observed) + `mvNormalLatent` | Phase D / G6 — Cholesky-based, both observed and latent |
| `Dirichlet` | ✅ `dirichlet` | Phase G2 — latent via stick-breaking |
| `LKJCholeskyCov` | ✅ `lkjCorrCholesky` | Phase H4 — CPC method, arbitrary K (validated for K=3 in J1) |
| `Mixture` | ✅ `Mixture` | Phase B — log-sum-exp, observed and sample both supported |
| `Truncated*` | ✅ `Truncated` | Phase C — truncation of any distribution (CDF required) |
| `Censored` | ✅ `Censored` | Phase C — detection limits (Tobit-style) |
| `Bound` | ❌ | Stretch — almost replaceable by `Truncated` |
| `Multinomial` | ✅ `Multinomial` | Phase H2 — observed only, combined with Dirichlet |
| `NegativeBinomial` | ✅ `NegativeBinomial` | Phase H1 — (μ, α) parameterization |
| `ZeroInflated*` | ✅ `ZeroInflatedPoisson` / `ZeroInflatedBinomial` | Phase H3 |
| `InverseGamma` | ✅ `InverseGamma` | Phase I — conjugate prior for variance |
| `Weibull` | ✅ `Weibull` | Phase I — survival analysis |
| `Pareto` | ✅ `Pareto` | Phase I — heavy tails |
| `BetaBinomial` | ✅ `BetaBinomial` | Phase I — over-dispersed binomial |
| `VonMises` | ✅ `VonMises` | Phase I — angular data (`logBesselI0` implemented) |
| `Wishart` | ❌ | Stretch — LKJ recommended as substitute |
| `Multivariate-t` | ❌ | Stretch |

## Samplers

| PyMC | hanalyze | Notes |
|---|---|---|
| `pymc.sample` (NUTS) | ✅ `MCMC.NUTS` | with dual averaging |
| HMC | ✅ `MCMC.HMC` | |
| Metropolis | ✅ `MCMC.MH` | |
| `CompoundStep` (Gibbs+MH) | ✅ `MCMC.Gibbs.gibbsMH` | automatic conjugacy detection |
| Slice | ✅ `MCMC.Slice` | Phase J3 — gradient-free, auto step-size adaptation |
| `pymc.fit` (ADVI) | ✅ `Stat.VI.advi` | mean-field only |
| Full-rank ADVI | ❌ | Stretch |
| Normalizing flows | ❌ | Stretch |
| SMC (Sequential Monte Carlo) | ❌ | Stretch |

## Posterior workflow

| PyMC | hanalyze | Notes |
|---|---|---|
| `pm.sample_posterior_predictive` | ✅ `Stat.PosteriorPredictive.posteriorPredictive` | Phase 2.3 done |
| `pm.sample_prior_predictive` | ✅ `Stat.PosteriorPredictive.priorPredictive` | Phase 2.3 done |
| `pm.set_data` (data swap without rebuild) | ✅ `dataNamed` / `withData` | Phase G5 + H5 — Rank-2 polymorphism |
| `pm.Deterministic` (derived quantities) | ✅ `deterministic` | Phase G1 — injected into Chain via `augmentChainWithDeterministic` |
| `pm.Potential` (arbitrary log term) | ✅ `potential` | Phase A — soft constraints / custom likelihoods / regularization |

## Diagnostics & visualization (ArviZ equivalent)

| PyMC / ArviZ | hanalyze | Notes |
|---|---|---|
| Trace plot | ✅ `Viz.MCMC.tracePlot` | |
| Posterior KDE | ✅ `Viz.MCMC.posteriorPlot` | |
| Pair scatter | ✅ `Viz.MCMC.pairScatter` | |
| Autocorrelation | ✅ `Viz.MCMC.autocorrPlot` | |
| Forest plot | ✅ `Viz.MCMC.forestPlot` | Phase 3.1 done |
| Energy plot (NUTS) | ✅ `Viz.MCMC.energyPlot` | Phase E — BFMI shown |
| BFMI score | ✅ `Stat.MCMC.bfmi` | Phase E — Betancourt 2016 |
| ESS / R-hat table | ✅ `Viz.Report` | |
| Posterior predictive plot | ❌ | TODO — render `posteriorPredictive` results in Vega-Lite |
| HDI band (trace / KDE) | 🚧 partial | |
| Rank plot (PyMC `plot_rank`) | ❌ | TODO — convergence diagnostic across chains |
| Divergences as scatter | ❌ | TODO — detect & visualize NUTS divergences |
| Posterior table (`az.summary`) | 🚧 partial inside `Viz.Report` | TODO — standalone helper |

## Model comparison

| PyMC | hanalyze | Notes |
|---|---|---|
| `pm.waic` | ✅ `Stat.ModelSelect.waic` | |
| `pm.loo` (PSIS-LOO) | ✅ `Stat.ModelSelect.loo` | with k̂ diagnostics |
| `pm.compare` (model weights) | ✅ `Stat.ModelSelect.compareModels` | Phase 3.3 done — Pseudo-BMA |
| Bayes factor / marginal likelihood | ❌ | Stretch |

## Modeling primitives

| PyMC | hanalyze | Notes |
|---|---|---|
| Hierarchical models | ✅ via `Model.HBM` | |
| Random intercept | ✅ demo: `simpson-paradox` | |
| Random slope | ✅ demo: `hbm-random-slope` | |
| Non-centered parameterization | ✅ `nonCenteredNormal` | Phase G3 — BFMI improvement demonstrated on Neal's funnel |
| Time series (AR / GP) | ✅ `ar1Latent` + GP | Phase J2 (AR) / GP existing |
| ODE likelihoods | ❌ | Stretch (needs ODE solver) |
| Bayesian NN | ❌ | Stretch |
| Custom log-density | ✅ `potential` | Phase A — add arbitrary log terms |
| Mixture models | ✅ `Mixture` | Phase B — log-sum-exp |
| Tobit / detection limits | ✅ `Censored` | Phase C |
| Truncated distributions | ✅ `Truncated` | Phase C |

## Implementation roadmap

### Completed phases (test/newFunction branch)

| Phase | Feature | demo | commit |
|---|---|---|---|
| 2.1 | 6 continuous distributions | `new-distrib-demo` | master |
| 2.2 | Discrete observation distributions (Bernoulli/Categorical) | `discrete-obs-demo` | master |
| 2.3 | Posterior/prior predictive sampling | `ppc-demo` | master |
| 3.1 | Forest plot | `forest-compare` | master |
| 3.3 | Pseudo-BMA model weights | `forest-compare` | master |
| **A** | `pm.Potential` equivalent | `potential-demo` | `0a59ce9` |
| **B** | `pm.Mixture` equivalent | `mixture-demo` | `aa29606` |
| **C** | `Truncated` / `Censored` distributions | `trunc-censor-demo` | `ab51fa0` |
| **C+** | CDF for Beta/Gamma/Cauchy/StudentT/HalfCauchy | `cdf-test` | `ed2a413` |
| **D** | `MvNormal` observed only (Cholesky) | `mvnormal-demo` | `d476eb2` |
| **E** | Energy plot / BFMI diagnostic | `energy-demo` | `68b4b8e` |
| **F1** | Posterior summary (`az.summary`) | `summary-demo` | `58ab3c5` |
| **F2** | HDI-shaded trace plot | `summary-demo` | `0a46311` |
| **F3** | Rank plot (multi-chain convergence) | `summary-demo` | `951b741` |
| **F4** | Posterior predictive check (`pp_check`) | `summary-demo` | `a07620f` |
| **F5** | Divergence overlay (rendering) | `summary-demo` | `095e45b` |
| **G1** | `pm.Deterministic` derived quantities | `deterministic-demo` | `746f3fc` |
| **G2** | `Dirichlet` (stick-breaking) | `dirichlet-demo` | `c41da79` |
| **G3** | `nonCenteredNormal` helper | `noncentered-demo` | `4f45466` |
| **G4** | NUTS divergence detection | `noncentered-demo` | `5ee98ed` |
| **G5** | `pm.set_data` (`dataNamed`/`withData`) | `setdata-demo` | `695a7aa` |
| **G6** | `mvNormalLatent` (latent form) | `mvnormal-latent-demo` | `866904e` |
| **H1** | `NegativeBinomial` (over-dispersed counts) | `negbinom-demo` | `d89907b` |
| **H2** | `Multinomial` (observed) | `multinomial-demo` | `b2f8b3c` |
| **H3** | `ZeroInflatedPoisson` / `ZeroInflatedBinomial` | `zeroinflated-demo` | `68a7ffe` |
| **H4** | `lkjCorrCholesky` (LKJ correlation prior) | `lkj-demo` | `57f0cc2` |
| **H5** | `withData` Rank-2 polymorphism | `setdata-demo` | `d046b79` |
| **H6** | `Stat.Summary` extraction | (refactor) | `3cc343d` |
| **I** | `InverseGamma`/`Weibull`/`Pareto`/`BetaBinomial`/`VonMises` | `newdistribs-demo` | `ea6faa5` |
| **J1** | LKJ K=3 validation | `lkj3d-demo` | `480ab17` |
| **J2** | `ar1Latent` (AR(1) state space) | `ar1-demo` | `1ad8b84` |
| **J3** | Slice sampler | `slice-demo` | `e1f7a28` |

### Remaining TODO (Stretch)

The major PyMC feature gaps are addressed in Phases A–J. What remains is
research-level or low-priority advanced functionality:

- [ ] `Wishart` / `Multivariate-t` (LKJ recommended as substitute)
- [ ] Full-rank ADVI / Normalizing flows / SMC
- [ ] ODE likelihoods (Runge-Kutta + AD)
- [ ] Bayesian NN (hidden layers, research-level)
- [ ] Bayes factor / marginal likelihood (importance-sampling family)

### Visualizing implementation status

The `pymc-status-demo` executable produces an HTML bar chart of the PyMC
parity status:

```bash
cabal run pymc-status-demo
# → pymc-status.html (✅/🚧/❌ counts per category)
```
