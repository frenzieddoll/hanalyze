# hanalyze-bayes

The **Bayesian inference layer** of [`hanalyze`](../README.md): the
hierarchical Bayesian model (HBM) DSL, the MCMC samplers, automatic
differentiation, and Bayesian model comparison.

It depends on `hanalyze-core` only — not on dataframe. Sitting next to
`-frame` directly above core, it can be used as a **standalone sampling library
without pulling in any data representation**.

## Main modules (26 in total)

### HBM DSL (`Hanalyze.Model.HBM.*`)

| Module | Role |
|---|---|
| `Model.HBM` | Facade over 8 submodules (Util / Distribution / Sampling / Model / Track / Eval / IR / Gradient). Normally the only import you need |
| `Model.HBM.Model` | The polymorphic free-monad model DSL (`sample` / `observe` / `dataNamed*`) |
| `Model.HBM.Distribution` | Polymorphic distribution ADT with densities and CDFs |
| `Model.HBM.Eval` | log-joint / likelihood interpreter and DAG construction |
| `Model.HBM.Gradient` / `Model.HBM.IR` / `Model.HBM.VecAD` | AD gradient compiler, intermediate representation, and the in-house reverse-mode AD (the per-draw hot path of NUTS) |
| `Model.HBM.Ast` / `Model.HBM.Interp` | AST + JSON decoder of the dialog DSL and its interpreter |

### Samplers (`Hanalyze.MCMC.*`)

| Module | Role |
|---|---|
| `MCMC.NUTS` | No-U-Turn Sampler — Hoffman & Gelman (2014) Algorithm 3. The workhorse sampler |
| `MCMC.HMC` | Hamiltonian Monte Carlo with exact AD gradients |
| `MCMC.MH` / `MCMC.Slice` | Random-walk Metropolis-Hastings / slice sampler (Neal 2003) |
| `MCMC.Gibbs` | Gibbs sampler for conjugate priors (analytic full conditionals) |
| `MCMC.SMC` | Sequential Monte Carlo over a tempered target |
| `MCMC.BayesianTest` | Bayesian A/B test — samples the group mean difference with NUTS and decides via ROPE / HDI |
| `MCMC.Progress` | Progress display aggregated across chains (stderr) |

### Inference and model comparison (`Hanalyze.Stat.*`)

| Module | Role |
|---|---|
| `Stat.AD` | The automatic-differentiation layer behind HMC / NUTS |
| `Stat.VI` | Variational inference (ADVI) |
| `Stat.BridgeSampling` | Marginal likelihood log p(y) via bridge sampling (Meng & Wong 1996) |
| `Stat.BayesFactor` | Bayes factors on top of bridge sampling (Kass & Raftery 1995) |
| `Stat.BayesianModelAveraging` | True BMA from the log marginal likelihoods |
| `Stat.PosteriorPredictive` | Prior / posterior predictive sampling (PyMC's `sample_*_predictive`) |

## Using it standalone

```cabal
-- Chain and posterior statistics are core types, so depend on core too
-- if you touch them directly
build-depends: hanalyze-bayes, hanalyze-core, containers
```

```haskell
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Map.Strict as Map
import Hanalyze.Model.HBM (ModelP, sample, observe, Distribution (..))
import Hanalyze.MCMC.NUTS (nutsPure, defaultNUTSConfig)
import Hanalyze.MCMC.Core (posteriorMean, posteriorSD)

myModel :: ModelP ()
myModel = do
  mu <- sample "mu" (Normal 0 10)
  observe "y" (Normal mu 2) [1.2, 2.3, 3.1, 2.8, 1.9]   -- observe takes [Double]

main = do
  -- nutsPure takes a seed (Word32) and returns a Chain purely and deterministically
  let chain = nutsPure myModel defaultNUTSConfig (Map.fromList [("mu", 0.0)]) 42
  print (posteriorMean "mu" chain, posteriorSD "mu" chain)
  -- (Just 2.2673259586131507,Just 0.8340460004499451)
```

`Chain` is a core type (`Hanalyze.MCMC.Core`), so posterior summaries
(`posteriorMean` / `posteriorSD` / `posteriorQuantile`) and diagnostics
(`rhat` / `ess` / `hdi` in `Hanalyze.Stat.MCMC`) come from core.
Add the `-viz` layer (`Hanalyze.Viz.*`) when you need HTML reports or
DAG figures.

## Related docs

- Writing probabilistic models: [docs/bayesian/02-probabilistic-model.md](../docs/bayesian/02-probabilistic-model.md)
- Choosing a sampler: [docs/bayesian/03-mcmc-samplers.md](../docs/bayesian/03-mcmc-samplers.md)
- Gibbs: [docs/bayesian/04-gibbs.md](../docs/bayesian/04-gibbs.md) /
  variational inference: [docs/bayesian/05-vi.md](../docs/bayesian/05-vi.md)
- Model comparison (WAIC / LOO): [docs/bayesian/06-model-comparison.md](../docs/bayesian/06-model-comparison.md)
- Marginal likelihood / Bayes factor / BMA: [docs/bayesian/07-advanced-marginal-likelihood.ja.md](../docs/bayesian/07-advanced-marginal-likelihood.ja.md) (ja only for now)
- Bayesian A/B test: [docs/bayesian/usage-bayesian-ab-test.md](../docs/bayesian/usage-bayesian-ab-test.md)
- Theory: [docs/bayesian/theory-hmc-nuts.md](../docs/bayesian/theory-hmc-nuts.md)

← [repository README](../README.md)
