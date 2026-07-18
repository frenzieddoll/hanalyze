# PyMC feature comparison

> 🌐 **English** | [日本語](02-pymc-comparison.ja.md)

A feature mapping that compares the **Bayesian portion** of hanalyze
(`Hanalyze.Model.HBM` / MCMC / VI) against PyMC. Areas exclusive to hanalyze
(classical regression, DOE, multi-objective optimization, etc.) are out of
scope here.

This page lists features that PyMC has but hanalyze does not.

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
| `Uniform` | ✅ `Uniform` | |
| `StudentT` | ✅ `StudentT` | df / loc / scale |
| `Cauchy` | ✅ `Cauchy` | |
| `HalfNormal` | ✅ `HalfNormal` | |
| `HalfCauchy` | ✅ `HalfCauchy` | |
| `LogNormal` | ✅ `LogNormal` | |
| `Bernoulli` | ✅ `Bernoulli` | observed only |
| `Categorical` | ✅ `Categorical` | observed only |
| `MvNormal` | ✅ `MvNormal` (observed) + `mvNormalLatent` | Cholesky-based, both observed and latent |
| `Dirichlet` | ✅ `dirichlet` | latent via stick-breaking |
| `LKJCholeskyCov` | ✅ `lkjCorrCholesky` | CPC method, arbitrary K (validated for K=3 in J1) |
| `Mixture` | ✅ `Mixture` | log-sum-exp, observed and sample both supported |
| `Truncated*` | ✅ `Truncated` | truncation of any distribution (CDF required) |
| `Censored` | ✅ `Censored` | detection limits (Tobit-style) |
| `Bound` | ✅ `Bound` | Phase 39-A3, PyMC-compatible (internally delegates to `Truncated`) |
| `Multinomial` | ✅ `Multinomial` | observed only, combined with Dirichlet |
| `NegativeBinomial` | ✅ `NegativeBinomial` | (μ, α) parameterization |
| `ZeroInflated*` | ✅ `ZeroInflatedPoisson` / `ZeroInflatedBinomial` | `ZeroInflatedNegativeBinomial` is a Phase 37-A3 candidate |
| `InverseGamma` | ✅ `InverseGamma` | conjugate prior for variance |
| `Weibull` | ✅ `Weibull` | survival analysis |
| `Pareto` | ✅ `Pareto` | heavy tails |
| `BetaBinomial` | ✅ `BetaBinomial` | over-dispersed binomial |
| `VonMises` | ✅ `VonMises` | angular data (`logBesselI0` implemented) |
| `SkewNormal` | ✅ `SkewNormal` | Phase 37-A2, Henze 1986 sampling |
| `Logistic` | ✅ `Logistic` | Phase 37-A2, closed-form CDF |
| `Gumbel` | ✅ `Gumbel` | Phase 37-A2, closed-form CDF (extreme-value distribution) |
| `AsymmetricLaplace` | ✅ `AsymmetricLaplace` | Phase 37-A2, PyMC `(b, κ, μ)` order, closed-form CDF |
| `Triangular` | ✅ `Triangular` | Phase 39-A1, closed-form CDF, weakly informative prior |
| `Kumaraswamy` | ✅ `Kumaraswamy` | Phase 39-A1, closed-form CDF, Beta alternative |
| `Rice` | ✅ `Rice` | Phase 39-A1, via `logBesselI0`, MRI / Rayleigh extension |
| `OrderedLogistic` | ✅ `OrderedLogistic` | Phase 37-A3, K categories via cuts vector |
| `DiscreteUniform` | ✅ `DiscreteUniform` | Phase 37-A3, both endpoints inclusive |
| `Geometric` | ✅ `Geometric` | Phase 37-A3, PyMC convention (support 1, 2, …) |
| `HyperGeometric` | ✅ `HyperGeometric` | Phase 37-A3, N/K/n parameters |
| `ZeroInflatedNegativeBinomial` | ✅ `ZeroInflatedNegativeBinomial` | Phase 37-A3, mixture of ψ + NegBin |
| `DiscreteWeibull` | ✅ `DiscreteWeibull` | Phase 39-A1, integer Weibull, observed only |
| `Wishart` | ✅ `Wishart` | Phase 39-A2, observed only (k² flatten), direct expression of covariance priors |
| `OrderedProbit` | ✅ `OrderedProbit` | Phase 39-A3, Probit version of OrderedLogistic (link swap) |
| `MvStudentT` | ✅ `MvStudentT` | Phase 37-A4, observed only, Mahalanobis via Cholesky |
| `DirichletMultinomial` | ✅ `DirichletMultinomial` | Phase 37-A4, observed only, K-vector counts |

## Samplers

| PyMC | hanalyze | Notes |
|---|---|---|
| `pymc.sample` (NUTS) | ✅ `Hanalyze.MCMC.NUTS` | with dual averaging |
| HMC | ✅ `Hanalyze.MCMC.HMC` | |
| Metropolis | ✅ `Hanalyze.MCMC.MH` | |
| `CompoundStep` (Gibbs+MH) | ✅ `Hanalyze.MCMC.Gibbs.gibbsMH` | automatic conjugacy detection |
| Slice | ✅ `Hanalyze.MCMC.Slice` | gradient-free, auto step-size adaptation |
| `pymc.fit` (ADVI) | ✅ `Hanalyze.Stat.VI.advi` | mean-field only |
| Full-rank ADVI | ✅ `Hanalyze.Stat.VI.fullRankAdvi` | Phase 37-A5, learns correlation via lower-triangular Cholesky factor L, L output as `viCovU` |
| Normalizing flows | ❌ | Stretch (research-level) |
| SMC (Sequential Monte Carlo) | ✅ `Hanalyze.MCMC.SMC` | Phase 29-A1 (annealing + bridge sampling pathway) |

## Posterior workflow

| PyMC | hanalyze | Notes |
|---|---|---|
| `pm.sample_posterior_predictive` | ✅ `Hanalyze.Stat.PosteriorPredictive.posteriorPredictive` | |
| `pm.sample_prior_predictive` | ✅ `Hanalyze.Stat.PosteriorPredictive.priorPredictive` | |
| `pm.set_data` (data swap without rebuild) | ✅ `dataNamed` / `withData` | Rank-2 polymorphism |
| `pm.Deterministic` (derived quantities) | ✅ `deterministic` | injected into Chain via `augmentChainWithDeterministic` |
| `pm.Potential` (arbitrary log term) | ✅ `potential` | soft constraints / custom likelihoods / regularization |

## Diagnostics & visualization (ArviZ equivalent)

| PyMC / ArviZ | hanalyze | Notes |
|---|---|---|
| Trace plot | ✅ `Hanalyze.Viz.MCMC.tracePlot` | |
| Posterior KDE | ✅ `Hanalyze.Viz.MCMC.posteriorPlot` | |
| Pair scatter | ✅ `Hanalyze.Viz.MCMC.pairScatter` | |
| Autocorrelation | ✅ `Hanalyze.Viz.MCMC.autocorrPlot` | |
| Forest plot | ✅ `Hanalyze.Viz.MCMC.forestPlot` | |
| Energy plot (NUTS) | ✅ `Hanalyze.Viz.MCMC.energyPlot` | BFMI shown |
| BFMI score | ✅ `Hanalyze.Stat.MCMC.bfmi` | Betancourt 2016 |
| ESS / R-hat table | ✅ `Hanalyze.Viz.Report` | |
| Posterior predictive plot | ✅ `Hanalyze.Viz.MCMC.ppcPlot` / `ppcPlotFile` | observed + posterior draws overlay |
| HDI band (trace / KDE) | ✅ `Hanalyze.Viz.MCMC.tracePlotHDI` / `tracePlotHDIFile` | |
| Rank plot (PyMC `plot_rank`) | ✅ `Hanalyze.Viz.MCMC.rankPlot` / `rankPlotFile` | rank histogram across chains |
| Divergences as scatter | ✅ `Hanalyze.Viz.MCMC.pairScatterDiv` / `pairScatterDivFile` | overlays NUTS divergences on pair scatter |
| Posterior table (`az.summary` equivalent) | ✅ `Hanalyze.Stat.Summary.posteriorSummary` / `Hanalyze.Viz.MCMC.posteriorSummaryHtml` / `printPosteriorSummary` | mean/sd/HDI/ESS/R̂ table |

## Model comparison

| PyMC | hanalyze | Notes |
|---|---|---|
| `pm.waic` | ✅ `Hanalyze.Stat.ModelSelect.waic` | |
| `pm.loo` (PSIS-LOO) | ✅ `Hanalyze.Stat.ModelSelect.loo` | with k̂ diagnostics |
| `pm.compare` (model weights, Pseudo-BMA) | ✅ `Hanalyze.Stat.ModelSelect.compareModels` | LOO-based |
| True BMA (marginal-likelihood based) | ✅ `Hanalyze.Stat.BayesianModelAveraging` | Phase 29-A3, combined with BridgeSampling |
| Bayes factor / marginal likelihood | ✅ `Hanalyze.Stat.BayesFactor` + `BridgeSampling` | Phase 29-A2 (use Bridge as primary; SMC log marginal has bias) |

## Modeling primitives

| PyMC | hanalyze | Notes |
|---|---|---|
| Hierarchical models | ✅ via `Hanalyze.Model.HBM` | Patterns guide: [02-probabilistic-model.ja.md](bayesian/02-probabilistic-model.ja.md) |
| Random intercept | ✅ demo: `simpson-paradox` | docs: [demos.ja.md](bayesian/demos.ja.md) |
| Random slope | ✅ demo: `hbm-random-slope` | docs: [demos.ja.md](bayesian/demos.ja.md) |
| Non-centered parameterization | ✅ `nonCenteredNormal` | BFMI improvement demonstrated on Neal's funnel |
| Multi-level (3-level nested) | ✅ pattern 6 (by composition) | helper candidate: Phase 37-A6 |
| Crossed random effects | ✅ pattern 7 (by composition) | helper candidate: Phase 37-A6 |
| GLMM-style one-line helper | ✅ `glmmRandomIntercept` | Phase 37-A6, Gaussian / Binomial / Poisson, random intercept |
| Hidden Markov Model (HMM) | ✅ `hmmLatent` + `hmmForwardLogLik` | Phase 39-A4, K states + Dirichlet transition prior + log-space forward algorithm for marginalisation (NUTS-compatible); emissions are wired in externally via `potential` |
| Dirichlet Process / Stick-breaking infinite mixture | ✅ `dpStickBreaking` | Phase 39-A5, finite approximation truncated at level T |
| Ordered cuts helper (monotonically increasing) | ✅ `orderedCuts` | Phase 39-A6, cumulative sum of c_min + HalfNormal increments |
| Time series (AR / GP) | ✅ `ar1Latent` + GP | GP existing |
| ODE likelihoods | ❌ | Stretch (needs ODE solver, separate Phase) |
| Bayesian NN | ❌ | Stretch (research-level, separate Phase) |
| Custom log-density | ✅ `potential` | add arbitrary log terms |
| Mixture models | ✅ `Mixture` | log-sum-exp |
| Tobit / detection limits | ✅ `Censored` | |
| Truncated distributions | ✅ `Truncated` | |

## Remaining items (Phase 37 + Stretch, updated 2026-05-30)

### Planned for Phase 37 (implementable, PR-ready)

- [x] **Distributions A2 (continuous)**: ~~`SkewNormal` / `Logistic` / `Gumbel` / `AsymmetricLaplace`~~ ✅ (Phase 37-A2 done 2026-05-30)
      Remaining: `Triangular` / `Kumaraswamy` / `Rice` (low priority, Phase 37-A2 candidates)
- [x] **Distributions A3 (discrete)**: ~~`OrderedLogistic` / `DiscreteUniform` / `Geometric` / `HyperGeometric` / `ZeroInflatedNegativeBinomial`~~ ✅ (Phase 37-A3 done 2026-05-30)
      Remaining: `DiscreteWeibull` (low priority)
- [x] **Distributions A4 (multivariate)**: ~~`MvStudentT` / `DirichletMultinomial`~~ ✅ (Phase 37-A4 done 2026-05-30)
      Remaining: `Wishart` (LKJ can substitute, deferred)
- [x] **A5 sampler**: ~~Full-rank ADVI~~ ✅ (Phase 37-A5 done 2026-05-30, `Hanalyze.Stat.VI.fullRankAdvi`)
- [x] **A6 hierarchical helper (core)**: ~~`glmmRandomIntercept`~~ ✅ (Phase 37-A6 done 2026-05-30, Gaussian/Binomial/Poisson)
      Remaining: `hmmLatent` / `dpStickBreaking` (deferred to Phase 39; forward-backward / infinite mixture are heavy standalone implementations)
- [ ] **A7 remaining viz**: most are already implemented (rankPlot / ppcPlot / pairScatterDiv /
      tracePlotHDI / posteriorSummary), so remaining work centers on navigation cleanup and
      docs updates

### Stretch (out of scope for this Phase)

- [ ] Normalizing flows / Stein VI / Pathfinder (research-level)
- [ ] ODE likelihoods (Runge-Kutta + AD, separate Phase recommended)
- [ ] Bayesian NN (BNN / Bayes by Backprop, separate Phase recommended)
- [ ] `Bound` distribution (replaceable by `Truncated`, low added value as a standalone)

### Items closed in Phase 29 (spring 2026)

- [x] SMC (`Hanalyze.MCMC.SMC`)
- [x] Bayes factor / marginal likelihood (`Hanalyze.Stat.BayesFactor` + `BridgeSampling`)
- [x] True BMA (`Hanalyze.Stat.BayesianModelAveraging`)

### Visualizing implementation status

The `pymc-status-demo` executable produces an HTML bar chart of the PyMC
parity status:

```bash
cabal run pymc-status-demo
# → pymc-status.html (✅/🚧/❌ counts per category)
```
