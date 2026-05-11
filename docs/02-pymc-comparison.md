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
| `Bound` | ❌ | Stretch — almost replaceable by `Truncated` |
| `Multinomial` | ✅ `Multinomial` | observed only, combined with Dirichlet |
| `NegativeBinomial` | ✅ `NegativeBinomial` | (μ, α) parameterization |
| `ZeroInflated*` | ✅ `ZeroInflatedPoisson` / `ZeroInflatedBinomial` | |
| `InverseGamma` | ✅ `InverseGamma` | conjugate prior for variance |
| `Weibull` | ✅ `Weibull` | survival analysis |
| `Pareto` | ✅ `Pareto` | heavy tails |
| `BetaBinomial` | ✅ `BetaBinomial` | over-dispersed binomial |
| `VonMises` | ✅ `VonMises` | angular data (`logBesselI0` implemented) |
| `Wishart` | ❌ | Stretch — LKJ recommended as substitute |
| `Multivariate-t` | ❌ | Stretch |

## Samplers

| PyMC | hanalyze | Notes |
|---|---|---|
| `pymc.sample` (NUTS) | ✅ `Hanalyze.MCMC.NUTS` | with dual averaging |
| HMC | ✅ `Hanalyze.MCMC.HMC` | |
| Metropolis | ✅ `Hanalyze.MCMC.MH` | |
| `CompoundStep` (Gibbs+MH) | ✅ `Hanalyze.MCMC.Gibbs.gibbsMH` | automatic conjugacy detection |
| Slice | ✅ `Hanalyze.MCMC.Slice` | gradient-free, auto step-size adaptation |
| `pymc.fit` (ADVI) | ✅ `Hanalyze.Stat.VI.advi` | mean-field only |
| Full-rank ADVI | ❌ | Stretch |
| Normalizing flows | ❌ | Stretch |
| SMC (Sequential Monte Carlo) | ❌ | Stretch |

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
| Posterior predictive plot | ❌ | TODO — render `posteriorPredictive` results in Vega-Lite |
| HDI band (trace / KDE) | 🚧 partial | |
| Rank plot (PyMC `plot_rank`) | ❌ | TODO — convergence diagnostic across chains |
| Divergences as scatter | ❌ | TODO — detect & visualize NUTS divergences |
| Posterior table (`az.summary`) | 🚧 partial inside `Hanalyze.Viz.Report` | TODO — standalone helper |

## Model comparison

| PyMC | hanalyze | Notes |
|---|---|---|
| `pm.waic` | ✅ `Hanalyze.Stat.ModelSelect.waic` | |
| `pm.loo` (PSIS-LOO) | ✅ `Hanalyze.Stat.ModelSelect.loo` | with k̂ diagnostics |
| `pm.compare` (model weights) | ✅ `Hanalyze.Stat.ModelSelect.compareModels` | Pseudo-BMA |
| Bayes factor / marginal likelihood | ❌ | Stretch |

## Modeling primitives

| PyMC | hanalyze | Notes |
|---|---|---|
| Hierarchical models | ✅ via `Hanalyze.Model.HBM` | |
| Random intercept | ✅ demo: `simpson-paradox` | |
| Random slope | ✅ demo: `hbm-random-slope` | |
| Non-centered parameterization | ✅ `nonCenteredNormal` | BFMI improvement demonstrated on Neal's funnel |
| Time series (AR / GP) | ✅ `ar1Latent` + GP | GP existing |
| ODE likelihoods | ❌ | Stretch (needs ODE solver) |
| Bayesian NN | ❌ | Stretch |
| Custom log-density | ✅ `potential` | add arbitrary log terms |
| Mixture models | ✅ `Mixture` | log-sum-exp |
| Tobit / detection limits | ✅ `Censored` | |
| Truncated distributions | ✅ `Truncated` | |

## Remaining stretch items

Research-level or low-priority unimplemented features:

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
