# PyMC Feature Comparison

> 🌐 **English** | [日本語](08-pymc-comparison.ja.md)

This document tracks features available in PyMC that hanalyze does not yet provide,
and serves as the implementation roadmap.

## Status legend

- ✅ Implemented
- 🚧 In progress / partial
- ❌ Not implemented
- ➖ Not applicable / out of scope

## Distributions

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
| `Bernoulli` | ✅ `Bernoulli` | Phase 2.2 — observation only |
| `Categorical` | ✅ `Categorical` | Phase 2.2 — observation only |
| `MvNormal` | ❌ | Phase 2.4 — multivariate |
| `Dirichlet` | ❌ | Phase 2.4 |
| `LKJCholeskyCov` | ❌ | Stretch — covariance prior |
| `Mixture` | ❌ | Phase 2.5 |
| `Truncated*` | ❌ | Stretch |
| `Censored` | ❌ | Stretch |
| `Bound` | ❌ | Stretch |

## Samplers

| PyMC | hanalyze | Notes |
|---|---|---|
| `pymc.sample` (NUTS) | ✅ `MCMC.NUTS` | with dual averaging |
| HMC | ✅ `MCMC.HMC` | |
| Metropolis | ✅ `MCMC.MH` | |
| `CompoundStep` (Gibbs+MH) | ✅ `MCMC.Gibbs.gibbsMH` | auto-conjugate detection |
| Slice | ❌ | Stretch |
| `pymc.fit` (ADVI) | ✅ `Stat.VI.advi` | mean-field only |
| Full-rank ADVI | ❌ | Stretch |
| Normalizing flows | ❌ | Stretch |
| SMC (Sequential MC) | ❌ | Stretch |

## Posterior workflow

| PyMC | hanalyze | Notes |
|---|---|---|
| `pm.sample_posterior_predictive` | ✅ `Stat.PosteriorPredictive.posteriorPredictive` | Phase 2.3 done |
| `pm.sample_prior_predictive` | ✅ `Stat.PosteriorPredictive.priorPredictive` | Phase 2.3 done |
| `pm.set_data` (replace data without rebuild) | ❌ | Stretch (DSL would need a `Data` primitive) |
| `pm.Deterministic` (named transformations) | ❌ | Stretch |
| `pm.Potential` (custom log-prob terms) | ❌ | Stretch |

## Diagnostics & visualization (ArviZ-equivalent)

| PyMC / ArviZ | hanalyze | Notes |
|---|---|---|
| Trace plot | ✅ `Viz.MCMC.tracePlot` | |
| Posterior KDE | ✅ `Viz.MCMC.posteriorPlot` | |
| Pair plot | ✅ `Viz.MCMC.pairScatter` | |
| Autocorrelation | ✅ `Viz.MCMC.autocorrPlot` | |
| Forest plot | ✅ `Viz.MCMC.forestPlot` | Phase 3.1 done |
| Energy plot (NUTS) | ❌ | Phase 3.2 |
| ESS / R-hat tables | ✅ `Viz.Report` | |
| Posterior predictive plot | ❌ | depends on Phase 2.3 |
| HDI shading on traces / KDE | 🚧 partial | |

## Model-comparison

| PyMC | hanalyze | Notes |
|---|---|---|
| `pm.waic` | ✅ `Stat.ModelSelect.waic` | |
| `pm.loo` (PSIS-LOO) | ✅ `Stat.ModelSelect.loo` | with k̂ diagnostic |
| `pm.compare` (model weights) | ✅ `Stat.ModelSelect.compareModels` | Phase 3.3 done — Pseudo-BMA |
| Bayes factor / marginal likelihood | ❌ | Stretch |

## Modeling primitives

| PyMC | hanalyze | Notes |
|---|---|---|
| Hierarchical models | ✅ via `Model.HBM` | |
| Random intercept (LME-equivalent in HBM) | ✅ demo: `simpson-paradox` | |
| Random slope | ✅ demo: `hbm-random-slope` | |
| Non-centered parameterization | ❌ | Phase 3.4 (helpers) |
| Time-series (AR, GP) | 🚧 GP only (`Model.GP`) | AR is missing |
| ODE-based likelihood | ❌ | Stretch (needs ODE solver) |
| Bayesian neural networks | ❌ | Stretch |
| Custom log-density | ➖ | already supported via `observe` of any distribution |

## Implementation roadmap (this branch)

The work is ordered easiest-first. Each phase is a self-contained commit.

1. ✅ **Phase 2.1 — Continuous distributions**
   `Uniform`, `StudentT`, `Cauchy`, `HalfNormal`, `HalfCauchy`, `LogNormal`
   → demo: `new-distrib-demo`
2. ✅ **Phase 2.2 — Discrete observations**
   `Bernoulli`, `Categorical` (observation distributions only;
   they cannot serve as latents because the DSL is `Floating a`-polymorphic)
   → demo: `discrete-obs-demo`
3. ✅ **Phase 2.3 — Posterior / prior predictive sampling**
   `posteriorPredictive`, `priorPredictive`, `samplePrior`,
   `posteriorPredictiveSummary` in `Stat.PosteriorPredictive`
   → demo: `ppc-demo`
4. ❌ **Phase 2.4 — Multivariate distributions**
   `MvNormal` (Cholesky-friendly), `Dirichlet` — requires DSL extension
5. ❌ **Phase 2.5 — Mixture distributions**
   log-sum-exp weighted likelihood
6. ✅ **Phase 3.1 — Forest plot** (`Viz.MCMC.forestPlot`)
   → demo: `forest-compare`
7. ❌ **Phase 3.2 — Energy plot** (NUTS BFMI; requires exposing per-step energy)
8. ✅ **Phase 3.3 — `compare` model weights** (`Stat.ModelSelect.compareModels`)
   Pseudo-BMA based on elpd_loo
   → demo: `forest-compare`
