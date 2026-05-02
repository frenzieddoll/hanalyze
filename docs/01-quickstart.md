# Quickstart

> 🌐 **English** | [日本語](01-quickstart.ja.md)

## Build & Run

```bash
cabal build           # library + all executables
cabal test            # test suite
cabal run hbm-example # hierarchical Bayes + 4-chain NUTS → HTML report
```

To run a binary directly (when `cabal run` is ambiguous):
```
dist-newstyle/build/x86_64-linux/ghc-9.6.7/hanalyze-0.1.0.0/x/<demo>/build/<demo>/<demo>
```

CPU parallelism (multi-chain):
```bash
cabal run hbm-example -- +RTS -N4   # 4 threads
```

---

## "What to do" → which demo / CLI

### Classical regression

| Goal | Recommended | Example |
|---|---|---|
| Simple linear regression with confidence band | CLI: `LM` | `hanalyze data.csv x y LM --ci 0.95 --report` |
| Logistic / Poisson regression | CLI: `GLM` | `hanalyze data.csv x y GLM -d binomial -l logit --report` |
| Polynomial regression (per-column degree) | CLI: `LM --degree -1 2 -2 3` | demo: `glmm-demo` (LME variant) |
| Mixed effects (LME / GLMM) | CLI: `--group COL` | demo: `glmm-demo` |
| Gaussian process regression (non-linear) | CLI: `GP` / demo: `gp-demo` | Auto-compares RBF/Matérn/Periodic |

### Bayesian inference

| Goal | Recommended | Example |
|---|---|---|
| One-shot Bayesian linear regression | CLI: `HBM --report --waic` | `hanalyze data.csv x y HBM --report` |
| Hierarchical model (group structure) | demo: `simpson-paradox` / `hbm-random-slope` | Custom hierarchies via `Model.HBM` |
| Random slope model | demo: `hbm-random-slope` | M1 (β common) vs M2 (β_g) compared by WAIC |
| Bayesian A/B test | demo: `clinical-trial` | Beta-Binomial, decision theory |
| Multi-chain NUTS + R-hat | demo: `hbm-example` | 4-chain parallel, `mcmc_report_multi.html` |

### Sampler choice / performance

| Goal | Recommended | Example |
|---|---|---|
| MH/HMC/NUTS comparison | demo: `bench-mcmc` | Easy/hard cases with ESS/sec |
| Fast sampling for conjugate models | demo: `gibbs-hbm-demo` | Auto-detect conjugates, hybrid Gibbs+MH |
| Variational inference (large/fast) | demo: `vi-demo` | ADVI vs NUTS accuracy |
| Sampler accuracy verification | demo: `test-hmc-nuts` | 1D Gaussian sanity check |

### Model comparison / interpretation

| Goal | Recommended | Example |
|---|---|---|
| Model selection via WAIC / LOO-CV | CLI: `--waic` or demo: `gibbs-demo` | LM/GLM/HBM/LME supported |
| Compare multiple models in one HTML | demo: `simpson-paradox` | Coefficients, prediction overlay, WAIC table |
| Reproduce Simpson's paradox | demo: `simpson-paradox` | `simpson_compare.html` |
| Validate hierarchical extensions | demo: `hbm-random-slope` | random intercept only vs +random slope |

### Visualization

| Goal | Recommended | Example |
|---|---|---|
| AnalysisReport (DAG / MCMC / curves integrated) | CLI: `--report` | Full LM/GLM/GLMM/GP/HBM support |
| Standalone bar/histogram | demo: `bar-demo` / CLI: `--hist COL` | PNG/SVG output |
| Standalone MCMC report (KDE / trace / DAG) | `Viz.Report.renderReport` | demo: `hbm-example` |
| Export plots as PNG/SVG | CLI: `--format png` | Each NamedPlot as a separate image |
| Standalone DAG HTML | `Viz.ModelGraph.renderModelGraph` | Auto-derived dependencies via Track type |

---

## Minimal end-to-end workflow (Haskell)

Five lines from "model → NUTS → HTML report":

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
import qualified Data.Map.Strict as Map
import System.Random.MWC (createSystemRandom)
import Model.HBM                              -- Distribution (..), sample, observe
import MCMC.NUTS  (nuts, defaultNUTSConfig)
import MCMC.Core  (posteriorMean, posteriorSD)
import Viz.Report (defaultReport, renderReport)

-- 1. Model: μ ~ Normal(0,10), y ~ Normal(μ, σ=2), 5 observations
myModel :: ModelP ()
myModel = do
  mu <- sample "mu" (Normal 0 10)
  observe "y" (Normal mu 2) [1.2, 2.3, 3.1, 2.8, 1.9]

main :: IO ()
main = do
  gen <- createSystemRandom
  -- 2. NUTS (AD gradients + dual averaging)
  chain <- nuts myModel defaultNUTSConfig (Map.fromList [("mu", 0.0)]) gen
  -- 3. Posterior statistics
  print (posteriorMean "mu" chain)
  print (posteriorSD   "mu" chain)
  -- 4. HTML report (KDE + trace + autocorrelation)
  renderReport "report.html" (defaultReport "My Model" chain ["mu"])
```

---

## Module quick-reference

| Purpose | Module | Key functions |
|---|---|---|
| Model definition (polymorphic DSL) | `Model.HBM` | `sample`, `observe`, `ModelP r` |
| HMC | `MCMC.HMC` | `hmc`, `hmcChains` |
| NUTS | `MCMC.NUTS` | `nuts`, `nutsChains` |
| Gibbs (auto-conjugate detection) | `MCMC.Gibbs` | `gibbsMH`, `gibbsFromModel` |
| Random Walk MH | `MCMC.MH` | `metropolis` |
| Variational inference | `Stat.VI` | `advi` |
| WAIC / LOO | `Stat.ModelSelect` | `waic`, `loo`, `lmPosteriorLogLiks`, `lmePosteriorLogLiks` |
| Classical regression | `Model.LM` / `Model.GLM` / `Model.GLMM` | `fitPolyWithSmooth`, `fitGLMWithSmooth`, `fitLMEDataFrame` |
| Gaussian Process | `Model.GP` | `optimizeGP`, `fitGP`, `gpPredData` |
| MCMC report | `Viz.Report` | `defaultReport`, `renderReport` |
| Multi-model comparison report | `Viz.AnalysisReport` | `writeAnalysisReport`, `writeComparisonReport` |
| DAG visualization | `Viz.ModelGraph` | `renderModelGraph` |
| Scatter / bar / histogram | `Viz.Scatter` / `Viz.Bar` / `Viz.Histogram` | each `*File` function |

For deeper details:
- [Probabilistic Programming DSL](02-probabilistic-model.md)
- [MCMC Sampler Guide](03-mcmc-samplers.md)
- [Gibbs Sampling](04-gibbs.md)
- [Variational Inference (ADVI)](05-variational-inference.md)
- [Model Comparison (WAIC/LOO)](06-model-comparison.md)
- [Visualization](07-visualization.md)
