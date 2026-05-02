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
| `MvNormal` | 🚧 `MvNormal` | Phase D — **observation-only**, hand-rolled Cholesky, AD/Track-compatible |
| `Dirichlet` | ❌ | TODO — needs simplex transform |
| `LKJCholeskyCov` | ❌ | Stretch — covariance prior |
| `Mixture` | ✅ `Mixture` | Phase B — log-sum-exp; both sampling and observation |
| `Truncated*` | ✅ `Truncated` | Phase C — truncate any CDF-bearing distribution |
| `Censored` | ✅ `Censored` | Phase C — detection limits (Tobit-style) |
| `Bound` | ❌ | Stretch — `Truncated` covers most cases |
| Multinomial | ❌ | TODO |
| Wishart / InverseGamma | ❌ | TODO — for conjugate priors |
| ZeroInflated (Poisson/Binomial) | ❌ | TODO |
| NegativeBinomial | ❌ | TODO — overdispersed counts |
| Weibull / Pareto / Beta-Binomial | ❌ | TODO |

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
| `pm.set_data` (replace data without rebuild) | ❌ | TODO (DSL would need a `Data` primitive) |
| `pm.Deterministic` (named transformations) | ❌ | TODO — store named derived quantities in Chain |
| `pm.Potential` (custom log-prob terms) | ✅ `potential` | Phase A — soft constraints, custom likelihoods, regularization |

## Diagnostics & visualization (ArviZ-equivalent)

| PyMC / ArviZ | hanalyze | Notes |
|---|---|---|
| Trace plot | ✅ `Viz.MCMC.tracePlot` | |
| Posterior KDE | ✅ `Viz.MCMC.posteriorPlot` | |
| Pair plot | ✅ `Viz.MCMC.pairScatter` | |
| Autocorrelation | ✅ `Viz.MCMC.autocorrPlot` | |
| Forest plot | ✅ `Viz.MCMC.forestPlot` | Phase 3.1 done |
| Energy plot (NUTS) | ✅ `Viz.MCMC.energyPlot` | Phase E — also displays BFMI |
| BFMI score | ✅ `Stat.MCMC.bfmi` | Phase E — Betancourt 2016 |
| ESS / R-hat tables | ✅ `Viz.Report` | |
| Posterior predictive plot | ❌ | TODO — visualize `posteriorPredictive` results |
| HDI shading on traces / KDE | 🚧 partial | |
| Rank plot | ❌ | TODO — multi-chain convergence diagnostic |
| Divergence overlay | ❌ | TODO — flag NUTS divergent transitions |
| Posterior table (`az.summary`) | 🚧 partial in `Viz.Report` | TODO — standalone helper |

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
| Non-centered parameterization | ❌ | TODO — helper for Neal's funnel etc. |
| Time-series (AR, GP) | 🚧 GP only (`Model.GP`) | AR / state-space are missing |
| ODE-based likelihood | ❌ | Stretch (needs ODE solver) |
| Bayesian neural networks | ❌ | Stretch |
| Custom log-density | ✅ `potential` | Phase A — arbitrary log-prob terms |
| Mixture models | ✅ `Mixture` | Phase B — log-sum-exp |
| Tobit / detection limits | ✅ `Censored` | Phase C |
| Truncated distributions | ✅ `Truncated` | Phase C |

## Implementation roadmap

### Completed PyMC parity phases

| Phase | Feature | Demo | Commit |
|---|---|---|---|
| 2.1 | Continuous distributions (Uniform/StudentT/Cauchy/HalfNormal/HalfCauchy/LogNormal) | `new-distrib-demo` | (master) |
| 2.2 | Discrete observations (Bernoulli/Categorical) | `discrete-obs-demo` | (master) |
| 2.3 | Posterior / prior predictive sampling | `ppc-demo` | (master) |
| 3.1 | Forest plot | `forest-compare` | (master) |
| 3.3 | `compare` model weights (Pseudo-BMA) | `forest-compare` | (master) |
| **A** | **`pm.Potential` equivalent** | `potential-demo` | `0a59ce9` |
| **B** | **`pm.Mixture` equivalent** | `mixture-demo` | `aa29606` |
| **C** | **`Truncated` / `Censored`** | `trunc-censor-demo` | `ab51fa0` |
| **C+** | **Beta/Gamma/Cauchy/StudentT CDFs** | `cdf-test` | `ed2a413` |
| **D** | **`MvNormal` (observation-only)** | `mvnormal-demo` | `d476eb2` |
| **E** | **Energy plot / BFMI** | `energy-demo` | `68b4b8e` |

### Remaining TODO (priority-ordered)

#### High — major PyMC feature gaps

- [ ] **`pm.Deterministic`** — store derived quantities (e.g. `tau = 1/sigma^2`) in `Chain`.
      Would add `deterministic :: Text -> a -> Model a a` to the DSL plus a new field.
- [ ] **`Dirichlet` distribution** — needs simplex constraint (stick-breaking).
      Used heavily for mixture weights and Categorical priors.
- [ ] **`MvNormal` as latent** — currently observation-only. Latent Cholesky factor
      requires positive-definiteness constraint + an LKJ prior.
- [ ] **Divergence detection / overlay** — flag NUTS iterations with large energy jumps
      and overlay them on pair plots (`plot_pair(divergences=True)` analogue).
- [ ] **Non-centered parameterization helper** — automatic raw + scale split for funnels.
- [ ] **`pm.set_data`** — add a `Data` placeholder to the DSL so observations can swap.

#### Medium — additional distributions

- [ ] **NegativeBinomial** — overdispersed counts.
- [ ] **Multinomial** — multivariate count observations.
- [ ] **ZeroInflated Poisson / Binomial** — zero-inflated count models.
- [ ] **Weibull / Pareto / Beta-Binomial / VonMises** — auxiliary distributions.
- [ ] **Wishart / InverseGamma** — for conjugate priors.

#### Medium — visualization & diagnostics

- [ ] **Posterior predictive plot (`pp_check`)** — overlay posterior draws on observations.
- [ ] **Posterior table (`az.summary`)** — standalone helper extracted from `Viz.Report`.
- [ ] **Rank plot** — multi-chain convergence visualization.
- [ ] **HDI-shaded trace plot** — overlay 94% HDI band on traces.

#### Low — stretch goals

- [ ] **`LKJCholeskyCov`** — covariance prior (correlation + scale decomposition).
- [ ] **Slice sampler / SMC / Full-rank ADVI / Normalizing flows**.
- [ ] **AR / state-space models**.
- [ ] **ODE likelihoods** — Runge–Kutta solver + AD.
- [ ] **Bayesian neural networks**.

### Status visualization

Run the `pymc-status-demo` executable to dump category-wise implementation
counts as both a console report and an HTML stacked bar chart:

```bash
cabal run pymc-status-demo
# → pymc-status.html
```
