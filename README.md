# hanalyze

> 🌐 **English** | [日本語](README.ja.md)

**A general-purpose statistical analysis, optimization, and visualization toolkit written in Haskell.**
Usable both as a CLI tool and as a Haskell library.

## Coverage

| Category | Highlights |
|---|---|
| **Classical regression** | LM (OLS) / GLM (IRLS) / GLMM / polynomial / confidence bands / **Quantile (median / τ-quantile)** |
| **Nonlinear & regularized** | B-spline / Natural cubic / Kernel Ridge / Ridge / Lasso / Elastic Net / **RFF** / **GAM** |
| **Ensemble** | **Random Forest regression (CART + bagging + feature subset)** |
| **Multi-output models** | Multivariate LM / RRR / PLS / CCA / Multi-output GP |
| **Time series** | AR(1) / Gaussian Process |
| **Robust regression** | **Robust GP (StudentT / Cauchy likelihoods + IRLS)** |
| **Design of Experiments (DOE)** | Full/fractional factorial / Latin square / RCBD / RSM (CCD/Box-Behnken) / D-optimal / **orthogonal arrays Lₙ** / **Taguchi method** / ANOVA / power analysis |
| **Multi-objective optimization** | NSGA-II / Pareto front / HV/IGD / Desirability / Bayesian MOO |
| **Bayesian / HBM** | Free monad DSL / polymorphic interpretation / 27 distributions / automatic conjugacy detection |
| **MCMC samplers** | MH / HMC / NUTS (dual averaging + AD gradients) / Slice / Gibbs |
| **Variational inference** | ADVI (mean-field) |
| **Model comparison** | WAIC / PSIS-LOO / Pseudo-BMA |
| **Visualization** | Vega-Lite based, HTML/PNG/SVG output, 15+ diagnostic plots |

---

## Feature highlights

### Classical regression (one-shot CLI)
```bash
hanalyze data.csv x y LM --ci 0.95 --report
hanalyze data.csv x y GLM -d binomial -l logit --report
hanalyze data.csv "x1 x2" y LM --degree -1 2 -2 3 --waic
```

### Multi-objective optimization (NSGA-II)
```haskell
import Optim.NSGA (nsga2, defaultNSGAConfig)
front <- nsga2 defaultNSGAConfig objFn bounds gen   -- Pareto front
```

### Random Fourier Features (RFF) for fast kernel methods
```haskell
import Model.RFF
gen   <- createSystemRandom
feats <- sampleRFFRBF 200 0.6 1.0 gen     -- D=200, ℓ=0.6, σ_f=1.0
let fit  = rffGP feats trainX trainY 0.15  -- σ_n=0.15
    pred = predictRFFGP fit testX          -- (mean, variance) per test point
```
At n=1500, **14× faster** than exact GP with matching accuracy
(measured by `cabal run rff-demo`).

### Robust GP (heavy-tailed likelihoods for outlier-resistance)
```haskell
import Model.GP        (GPParams (..))
import Model.GPRobust
let hp = GPParams 0.6 1.0 0.05 1.0
    fit = fitGPRobust RBF hp (RCauchy 0.5) trainX trainY   -- MAP via IRLS
    preds = predictGPRobust fit testX                       -- (mean, variance)
```
With 3 outliers injected, Gaussian GP gives RMSE=0.44 while Cauchy GP gives
**0.019 (95.7% improvement)** (measured by `cabal run robust-gp-demo`).

### Data preprocessing (missing-value imputation, derivations, groupBy, Parquet/JSON)
```haskell
import DataIO.CSV         (loadAuto)
import DataIO.External    (loadParquet, loadJSON)
import DataIO.Preprocess  (countMissing, imputeMean, deriveNumeric,
                           filterRowsByNumeric, groupByMean, groupByCount)
Right df0 <- loadAuto "data.csv"          -- returns Hackage DataFrame directly
let df1          = imputeMean "score" df0          -- mean-fill missing values
    df2          = filterRowsByNumeric "score" (>= 50) df1
    Just summary = groupByMean "category" "score" df2  -- per-group mean
```

### Reading dirty CSV
Auto-detects missing headers / comment lines / mixed NAs / units / currencies as
**warning codes (W001–W008)**; delimiter / comment / header presence are inferred
from the leading bytes. Remaining numeric conversions are applied per-column via
the **cleaning DSL**:
```
hanalyze clean data.csv --rule price=ParseCurrency   # $1,234.56 → 1234.56
hanalyze clean data.csv --rule weight=StripUnits     # "5.2kg" → 5.2
```

### Design of Experiments
```haskell
import Design.Factorial (twoLevelFactorial)
import Design.RSM       (centralCompositeRotatable)
import Design.Power     (sampleSizeTTest)

let design = centralCompositeRotatable 2 3   -- CCD k=2, 11 runs
let n = sampleSizeTTest 0.5 0.8 0.05         -- d=0.5 → n = 64
```

### Bayesian hierarchical model (polymorphic DSL)

```haskell
myModel :: ModelP ()
myModel = do
  mu  <- sample "mu"    (Normal 0 10)
  sig <- sample "sigma" (HalfNormal 1)
  observe "y" (Normal mu sig) [1.5, 2.0, 1.8]
```

A free-monad DSL from which **four interpretations** (structural inspection / log joint / AD gradient / dependency tracking) can be extracted from a single model definition. The same model is consumable by NUTS / HMC / Gibbs / VI.

---

## Documentation (docs/)

### Getting started & overview

| Page | Contents |
|---|---|
| [Quick start](docs/01-quickstart.md) | Build, minimal workflow, **goal → which demo** lookup table |
| [PyMC comparison & roadmap](docs/02-pymc-comparison.md) | Feature parity with PyMC and implementation plan |

### 1. Regression and statistical models — `docs/regression/`

| Page | Contents |
|---|---|
| [LM (linear regression)](docs/regression/01-lm.md) | OLS, confidence bands, diagnostics, assumption checks (★ in-depth) |
| [GLM](docs/regression/02-glm.md) | All exponential family members + IRLS + link functions |
| [GLMM (mixed-effects models)](docs/regression/03-glmm.md) | LME/GLMM, mixture distributions, derivation of the negative binomial |
| [Splines / kernels / regularization](docs/regression/04-spline-kernel-regularized.md) | B-spline / Natural cubic / Kernel Ridge / Ridge / Lasso / ElasticNet |
| [Multi-output models](docs/regression/05-multivariate.md) | MultiLM / RRR / PLS / CCA / MultiGP |
| [Quantile / GAM / Random Forest](docs/regression/06-quantile-gam-rf.md) | Quantile regression, additive models, ensemble trees (★ new methods) |
| [Theory — regression extensions](docs/regression/theory-regression-extensions.md) | Spline bases, kernel methods, L1/L2 regularization, bias-variance |
| [Theory — multivariate regression](docs/regression/theory-multivariate.md) | Mathematical background of OLS / RRR / PLS / CCA / Multi-GP |

### 2. Design of Experiments and optimization — `docs/doe-optim/`

| Page | Contents |
|---|---|
| [Design of Experiments (DOE)](docs/doe-optim/01-doe.md) | Full/fractional factorial / Latin square / RCBD / RSM / D-optimal / ANOVA / power analysis |
| [Multi-objective optimization](docs/doe-optim/02-multi-objective.md) | NSGA-II / Pareto / Bayesian MOO |
| [Orthogonal arrays and Taguchi method](docs/doe-optim/03-orthogonal-taguchi.md) | Lₙ tables, four SN ratios, inner/outer arrays, robust design (★ unified guide) |
| [Theory — Design of Experiments](docs/doe-optim/theory-doe.md) | Orthogonality, efficiency criteria, RSM, power, sample size |
| [Theory — Pareto efficiency and MOO](docs/doe-optim/theory-pareto-moo.md) | NSGA-II algorithm, HV/IGD, scalarization, ZDT |
| [Theory — Bayesian Optimization](docs/doe-optim/theory-bayesopt.md) | EI / UCB / PI / EHVI / ParEGO / q-EHVI |

### 3. Bayesian statistics and probabilistic modeling — `docs/bayesian/`

| Page | Contents |
|---|---|
| [Distribution relationship map](docs/bayesian/01-distributions.md) | Limits / conjugacy / specializations of 27 distributions visualized in Mermaid |
| [Probabilistic programming DSL](docs/bayesian/02-probabilistic-model.md) | Patterns for Model.HBM (Beta-Binomial / hierarchical normal / polymorphic interpretation) |
| [MCMC sampler selection guide](docs/bayesian/03-mcmc-samplers.md) | When to use MH / HMC / NUTS, tuning, R-hat |
| [Gibbs sampling](docs/bayesian/04-gibbs.md) | Conjugate updates, ESS/s comparison |
| [Variational inference (ADVI)](docs/bayesian/05-vi.md) | VI vs NUTS, ELBO convergence, mean-field limitations |
| [Model comparison (WAIC/LOO)](docs/bayesian/06-model-comparison.md) | WAIC, PSIS-LOO, Pareto k̂ diagnostics |
| [Theory — distribution fundamentals](docs/bayesian/theory-distributions.md) | Formulas, intuition, and use cases for all 27 distributions |
| [Theory — Bayesian fundamentals](docs/bayesian/theory-bayesian-basics.md) | Prior / likelihood / posterior, conjugacy, HBM, posterior predictive, workflow |
| [Theory — MCMC fundamentals](docs/bayesian/theory-mcmc.md) | Markov chains, ergodicity, MH, Gibbs, Slice, convergence diagnostics |
| [Theory — HMC / NUTS](docs/bayesian/theory-hmc-nuts.md) | Hamiltonians, leapfrog, constraint transforms, NUTS, dual averaging, BFMI |
| [Theory — VI / model selection / advanced topics](docs/bayesian/theory-advanced.md) | ELBO, ADVI, WAIC, PSIS-LOO, Mixture, LKJ, AR, **Truncated/Censored in detail** |

### 4. Visualization — `docs/visualization/`

| Page | Contents |
|---|---|
| [Visualization overview](docs/visualization/01-visualization.md) | Report / Bar / Histogram / PNG/SVG output |
| [HTML report builder](docs/visualization/02-report-builder.md) | Viz.ReportBuilder + Reportable typeclass usage (★ unified report API, going-forward standard) |

### 5. Data I/O — `docs/io/`

| Page | Contents |
|---|---|
| [Dirty data reading guide](docs/io/01-dirty-data.md) | W001..W008 / LoadOpts / 19 fixtures / sniff / clean DSL / CLI repair examples |

---

## Build

```bash
cabal build              # library + all executables
cabal test               # tests
```

## Demo catalog

Run with `cabal run <demo-name>` (HTML/PNG output is written to the current directory).
For task-oriented guidance, see [docs/01-quickstart.md](docs/01-quickstart.md).

Sources are organized into genre-based subdirectories under `demo/` mirroring the README structure:

| Directory | Contents | Files |
|---|---|---|
| `demo/regression/`    | LM/GLM/GP/Spline/Kernel/RFF/RobustGP, etc. | 8 |
| `demo/doe-optim/`     | DOE/RSM/Optimal/NSGA/Pareto/BayesOpt | 9 |
| `demo/bayesian/`      | HBM/MCMC/Gibbs/VI/PPC + probability distributions | 42 |
| `demo/visualization/` | Bar/Histogram, etc. | 1 |
| `demo/io/`            | DataIO (Preprocess / External) | 2 |

### Sample-data and output directories

| Directory | Contents |
|---|---|
| `data/regression/`    | Regression demo data (`test_lm.csv`, `test_poisson.csv`) |
| `data/distributions/` | Distribution samples (`normal.csv`, `poisson.csv`, `exponential.csv`) |
| `trash/`              | HTML/PNG/SVG generated by demos and tests (gitignored; user cleans up manually) |

### Getting started (start here)

| Demo | Contents | What you learn |
|---|---|---|
| `hbm-example`     | Hierarchical normal model + 4-chain NUTS → `mcmc_report*.html` | How to write the HBM DSL, MCMC reports |
| `hbm-regression`  | Bayesian simple regression + ReportBuilder (DAG / MCMC / credible intervals) | Report integration for HBM regression |
| `gp-demo`         | GP regression (RBF/Matérn/Periodic) + LML comparison | Kernel selection, how to use GPs |

### Model comparison and paradoxes

| Demo | Contents | What you learn |
|---|---|---|
| `simpson-paradox` | LM/GLMM/HBM compared on Simpson's paradox → 4 HTML files | Importance of hierarchical structure, model selection |
| `hbm-random-slope`| Random intercept vs. random intercept + random slope (M1 vs M2) compared via WAIC | Hierarchical model extension, WAIC-based model selection |
| `clinical-trial`  | Bayesian A/B test (clinical trial Beta-Binomial) | Two-group comparison, decision theory |

### Sampler deep dive

| Demo | Contents | What you learn |
|---|---|---|
| `bench-mcmc`     | Performance comparison of MH / HMC / NUTS | Sampler selection, ESS/s |
| `test-hmc-nuts`  | HMC/NUTS accuracy test (sanity check on 1D Gaussian) | Sampler validation |
| `gibbs-demo`     | Gibbs + WAIC/LOO model comparison | Conjugate updates, model comparison |
| `gibbs-hbm-demo` | Gibbs × HBM DSL integration (automatic conjugacy detection) | Conjugacy detection, hybrid Gibbs+MH |
| `vi-demo`        | Variational inference (ADVI) vs NUTS | Speed and limitations of VI |

### Classical regression and visualization

| Demo | Contents | What you learn |
|---|---|---|
| `glmm-demo`     | LME / GLMM (random intercept) | Mixed-effects models |
| `bar-demo`      | Viz.Bar (bar / stacked) + PNG/SVG export | Visualization, image export |
| `new-sections-demo` | RB's 4 new sections (`secComparisonTable` / `secForestPlot` / `secFeatureImportance` / `secPPC`) showcased in one report | Report builder extensions, model comparison + forest plot + feature importance + PPC |

### PyMC parity additions (this branch)

| Demo | Contents | What you learn |
|---|---|---|
| `new-distrib-demo`  | Six continuous distributions (Uniform / StudentT / Cauchy / HalfNormal / HalfCauchy / LogNormal) | Robust priors and observation distributions |
| `discrete-obs-demo` | Bernoulli / Categorical observations | Discrete observation likelihoods |
| `ppc-demo`          | Prior/posterior predictive sampling + Bayesian p-value | PPC workflow |
| `forest-compare`    | Forest plot + Pseudo-BMA model comparison | Multi-model summary, ArviZ-style output |
| `potential-demo`    | `pm.Potential` equivalent (soft constraints / custom likelihoods / regularization) | Adding arbitrary log terms |
| `mixture-demo`      | `pm.Mixture` (2-component Gaussian mixture) | log-sum-exp, latent clusters |
| `trunc-censor-demo` | `Truncated` / `Censored` distributions (survival analysis, Tobit) | Observation models using CDFs |
| `cdf-test`          | CDF validation for Beta/Gamma/Cauchy/StudentT/HalfCauchy | Incomplete gamma / incomplete beta |
| `mvnormal-demo`     | `MvNormal` for observations (via Cholesky) | Multivariate observation likelihoods |
| `energy-demo`       | NUTS energy plot + BFMI diagnostic | Detection of pathological posteriors |
| `pymc-status-demo`  | PyMC parity status report (counts per category + TODO list) | Implementation status visualization |
| `summary-demo`      | Posterior summary (`az.summary` equivalent) + HDI trace + rank plot + PPC + divergence overlay | 5 visualization primitives |
| `deterministic-demo` | Save derived quantities (τ=1/σ², log σ, snr=μ/σ) into a Chain via `pm.Deterministic` | Declaring deterministic quantities |
| `noncentered-demo`  | Centered vs non-centered on Neal's funnel (BFMI 0.65→1.02, ESS 7.6×) | Non-centered reparameterization + divergence detection |
| `dirichlet-demo`    | Dirichlet prior (stick-breaking) + Categorical observations → matches the conjugate solution | Dirichlet latent variables |
| `setdata-demo`      | Swap from training to test data via `withData` (Rank-2 polymorphism) | `pm.set_data` |
| `mvnormal-latent-demo` | NUTS inference of a 2D hierarchical model `μ ~ MvN([0,0], [[1,0.8],[0.8,1]])` | MvNormal latent |
| `negbinom-demo`     | NegativeBinomial for over-dispersed counts (recover μ=10, α=2; comparison with Poisson) | Over-dispersion modeling |
| `multinomial-demo`  | Multinomial observations + Dirichlet prior (T=5 trials × N=20, exact match with conjugate) | Multinomial observations |
| `zeroinflated-demo` | ZIP recovers structural zeros (ψ=0.4) | Zero-inflation |
| `lkj-demo` / `lkj3d-demo` | Recover 2D / 3D correlation matrices under an LKJ(η=1) prior | Priors over correlation matrices |
| `newdistribs-demo`  | Joint validation of InverseGamma / Weibull / Pareto / BetaBinomial / VonMises | Five new distributions |
| `ar1-demo`          | AR(1) state-space model (estimate ϕ=0.7 from a 30-step series) | Time series |
| `slice-demo`        | Slice sampler compared with MH/NUTS (tuning-free, gradient-free, high ESS) | Slice sampling |

### Regression extensions (LM derivatives)

| Demo | Contents | What you learn |
|---|---|---|
| `spline-demo`       | Fit B-spline (k=3, 10 coefficients) and natural cubic spline to sin + noise (RMSE 0.05) | Nonlinear smoothing |
| `kernel-demo`       | Nadaraya-Watson + Kernel Ridge, bandwidth selection by LOO-CV | Nonparametric regression |
| `regularized-demo`  | OLS / Ridge / Lasso / Elastic Net compared on sparse β=[3,-2,0,0,1.5,0,…] | Regularization and variable selection |

### Design of Experiments (DOE)

| Demo | Contents | What you learn |
|---|---|---|
| `doe-demo`          | Full/fractional factorial / Latin square / RCBD / ANOVA / power analysis / quality criteria, all together | DOE basics |
| `rsm-demo`          | CCD (rotatable / face-centered) + Box-Behnken + quadratic regression, optimum estimation (0.975, -0.517, 5.06 ≈ true 1, -0.5, 5) | Response Surface Methodology |
| `optimaldoe-demo`   | Build D-optimal designs via Fedorov exchange (D-eff=1.0, 1.7× improvement over random) | Optimal design |

> 📊 **PyMC feature comparison and roadmap**: see [docs/02-pymc-comparison.md](docs/02-pymc-comparison.md).
> A bar chart of implementation status across categories is produced as `pymc-status.html` by `cabal run pymc-status-demo`.

---

## Using as a CLI tool

Invoke either via subcommands (recommended) or in the bare form (legacy = `regress`):

```
cabal run hanalyze -- <subcommand> [args...]
cabal run hanalyze -- <file> <xcols> <ycols> [LM|GLM|...] [opts]   # legacy = regress
```

### Subcommands

| Subcommand | Purpose | Status |
|---|---|---|
| `regress`    | Classical / Bayesian regression (LM/GLM/GLMM/GP/HBM) | ✅ implemented |
| `info`       | Per-column type and basic statistics | ✅ implemented |
| `hist`       | Standalone histogram (with optional density overlay) | ✅ implemented |
| `doe`        | Orthogonal arrays Lₙ (L4/L8/L9/L12/L16/L18) | ✅ implemented (Phase E1) |
| `taguchi`    | Taguchi method (SN ratio + factor effects + inner/outer) | ✅ implemented (Phase E2) |
| `ridge`      | Ridge / Lasso / Elastic Net (+ regularization path) | ✅ implemented |
| `kernel`     | Kernel regression / RFF approximation | ✅ implemented |
| `spline`     | B-spline / natural cubic | ✅ implemented |
| `quantile`   | Quantile regression (τ-quantile, MM-IRLS) | ✅ implemented |
| `gam`        | Generalized Additive Model (additive B-spline + Ridge) | ✅ implemented |
| `rf`         | Random Forest regression (CART + bagging) | ✅ implemented |
| `help`       | List subcommands | ✅ |

### `regress` (= bare form)

```
hanalyze regress <file> <xcols> <ycols> [LM|GLM|NoReg|GP|HBM] [options]
hanalyze         <file> <xcols> <ycols> [LM|GLM|NoReg|GP|HBM] [options]   # equivalent
```

| Option | Description |
|---|---|
| `-d DIST` | Distribution: `gaussian` / `binomial` / `poisson` |
| `-l LINK` | Link function: `identity` / `log` / `logit` / `sqrt` |
| `--degree SPEC` | Polynomial degree. `N` for all columns; `-1 N1 -2 N2` per column |
| `--ci [LEVEL]` | Confidence interval (default 0.95) |
| `--pi [LEVEL]` | Prediction interval (Gaussian only) |
| `--group COL` | Mixed-effects model (LME / GLMM) |
| `--report [FILE]` | Generate HTML analysis report (default: `report.html`) |
| `--waic` | Compute WAIC / LOO-CV and include in the report |
| `--format FMT` | `html` / `png` / `svg`; for `png/svg` plots inside the report are rendered as images |

```bash
# Linear regression + confidence interval + HTML report
cabal run hanalyze -- regress data.tsv x y LM --ci 0.95 --report

# Poisson GLM (per-column polynomial degree) + WAIC
cabal run hanalyze -- regress data.tsv "x1 x2" y GLM -d poisson -l log --degree -1 2 -2 3 --waic --report

# Mixed-effects model (LME) + WAIC
cabal run hanalyze -- regress data.tsv x y LM --group school --waic --report

# Bayesian linear regression (HBM): NUTS posteriors for α/β/σ → HTML report
cabal run hanalyze -- regress data.csv x y HBM --report --waic

# Gaussian process regression (RBF/Matérn/Periodic comparison)
cabal run hanalyze -- regress data.csv x y GP --report

# Bare form (subcommand omitted = regress)
cabal run hanalyze -- data.csv x y LM --report --format png
```

### `info` — inspect a dataset

```bash
cabal run hanalyze -- info data.csv
# File:    data.csv
# Rows:    100
# Columns: 3
#
#   name                 type        n        min        max       mean     median         sd
#   ------------------------------------------------------------------------------------------
#   group                text      100  unique=3   top: A(40), B(35), C(25)
#   x                    numeric   100    -2.34       3.45       0.12       0.10       1.04
#   y                    numeric   100    -1.20       8.71       3.45       3.21       1.95
```

When a text column contains missing-value strings (`NA`, `null`, ...) or null-bitmap missings, a `NA=N` suffix is shown.

### `hist` — standalone histogram

```
hanalyze hist <file> <col> [--fit DIST PARAMS] [--format FMT] [--out FILE]
```

```bash
# Plain histogram
cabal run hanalyze -- hist data.csv score

# Overlay a normal density
cabal run hanalyze -- hist data.csv score --fit normal 0 1

# Compare against Poisson and export as PNG
cabal run hanalyze -- hist data.csv counts --fit poisson 3 --format png --out hist.png
```

### `ridge` / `kernel` / `spline` — additional regression models

```bash
# Regularized regression (--penalty ridge|lasso|elasticnet, --lambda L, --alpha A)
hanalyze ridge data.csv "x1 x2 x3" y --penalty lasso --lambda 0.05 --report

# Kernel regression (--method nw|kr|rff, --kernel gaussian|... , --bandwidth, --features)
hanalyze kernel data.csv x y --method kr --bandwidth 0.5 --report
hanalyze kernel data.csv x y --method rff --features 200 --report

# Spline (--type bspline|natural, --knots, --degree)
hanalyze spline data.csv x y --type natural --knots 8 --report
```

Each subcommand exposes the main hyperparameters (bandwidth/lambda/knots/...)
directly, and writes a scatter+fit plot to `--out` (HTML/PNG/SVG).
With `--report [FILE]` it also produces a self-contained HTML report combining
**data overview + model overview + coefficients/hyperparameters + scatter+fit + residuals**.
`ridge --report` additionally includes a **regularization path plot** (λ swept on
log scale 1e-4..1e2, visualizing each coefficient's shrinkage; for Lasso, the
sparsification trajectory).

### `doe` — generate experimental designs from orthogonal arrays Lₙ

```
hanalyze doe list                                          # list available arrays
hanalyze doe ortho <NAME>                                  # raw table (column names F1, F2, ...)
hanalyze doe ortho <NAME> -f NAME=v1,v2,... [-f ...]       # with factor assignment
hanalyze doe ortho <NAME> [-f ...] [--csv|--tsv|--pretty] [--out FILE]
```

Available standard arrays (`hanalyze doe list`):

| Array | Runs | Max factors | Levels |
|---|---|---|---|
| L4(2³)        | 4  | 3  | 2 levels |
| L8(2⁷)        | 8  | 7  | 2 levels |
| L9(3⁴)        | 9  | 4  | 3 levels |
| L12(2¹¹)      | 12 | 11 | 2 levels (Plackett-Burman) |
| L16(2¹⁵)      | 16 | 15 | 2 levels |
| L18(2¹×3⁷)    | 18 | 8  | 1 factor at 2 levels + 7 factors at 3 levels (Taguchi-recommended) |

```bash
# Raw L9 table (4 three-level columns, 9 runs)
cabal run hanalyze -- doe ortho L9

# Assign factors with mixed integer / decimal / text levels
cabal run hanalyze -- doe ortho L9 -f temp=150,180,210 -f rate=0.1,0.2,0.5 -f catalyst=A,B,C --csv

# L18 with mixed-level factors (material at 2 levels + temperature/speed at 3 levels), to CSV file
cabal run hanalyze -- doe ortho L18 \
    -f material=steel,alu \
    -f temp=150,180,210 \
    -f speed=low,med,high \
    --csv --out design.csv
```

**Orthogonal arrays vs. the Taguchi method** — orthogonal arrays are a
mathematical construct (orthogonal evaluation of main effects with the
fewest runs); the Taguchi method is a methodology that uses those arrays
for **robust design that minimizes variability** (= orthogonal arrays + the
SN ratio + inner/outer arrays for control vs. noise factors).

### `taguchi` — Taguchi method (SN ratio + factor effects + inner/outer)

```
hanalyze taguchi sn <type> <values...>             # compute one SN ratio
hanalyze taguchi analyze <ARRAY> -f F=v1,...        # analyze observations CSV
                  --csv FILE [--sntype TYPE]
hanalyze taguchi cross <INNER> <OUTER>              # inner × outer cross design
                  -f Fc=... -fn Fn=... [--out FILE]
```

| SN type | Use case | Formula |
|---|---|---|
| `smaller`             | smaller-the-better (defect rate, error, noise) | η = -10 log₁₀(Σy²/n) |
| `larger`              | larger-the-better (strength, life, efficiency) | η = -10 log₁₀(Σ(1/y²)/n) |
| `nominal`             | nominal-the-best (mean²/variance) | η = 10 log₁₀(μ²/σ²) |
| `nominal-target=M`    | nominal-the-best (target value M) | η = -10 log₁₀(Σ(y-M)²/n) |

```bash
# Compute one SN ratio
cabal run hanalyze -- taguchi sn smaller 1.2 1.5 0.9 1.1

# Analyze a CSV with 9 inner runs × 3 repetitions (L9 design)
# (CSV columns: Run,temp,time,catalyst,y1,y2,y3 — observation columns can have any name starting with y)
cabal run hanalyze -- taguchi analyze L9 \
    -f temp=150,180,210 -f time=10,20,30 -f catalyst=A,B,C \
    --csv runs.csv --sntype smaller
# → prints per-run SN ratios, factor effects (mean SN per level),
#   optimal levels, and predicted SN

# Generate an inner-L9 × outer-L4 cross-design template (cells empty, fill in after measurement)
cabal run hanalyze -- taguchi cross L9 L4 \
    -f temp=150,180,210 -f time=10,20,30 -f catalyst=A,B,C \
    --noise humidity=low,high --noise vibration=on,off \
    --out cross.csv

# Generate an HTML report (factor-effect bar charts + optimum table)
cabal run hanalyze -- taguchi analyze L9 \
    -f temp=150,180,210 -f time=10,20,30 -f catalyst=A,B,C \
    --csv runs.csv --sntype smaller --report taguchi.html
```

---

## Using as a library

Add `hanalyze` to the `build-depends` field of `hanalyze.cabal`.

---

## Module layout

```
DataIO/
  CSV.hs           -- standard + defensive loaders (loadAuto / loadAutoSafe / loadAutoSafeWith)
                   -- LoadOpts (--no-header / --skip / --comment / --strict) supported
  External.hs      -- Parquet / JSON loaders backed by Hackage `dataframe`
  Convert.hs       -- helpers to extract V.Vector Double / Text from a DataFrame
                   -- (deepseq-based exception capture for Hackage internals)
  Preprocess.hs    -- missing-value imputation / filter / derived columns / column selection / groupBy
  Log.hs           -- structured log (Severity / LogEntry / LogReport / Loaded)
  Health.hs        -- health checks (W001 missing-header ... W008 currency, 9 codes)

Stat/
  Distribution.hs  -- probability distributions (Normal / Gamma / Beta / ...)
  MCMC.hs          -- diagnostics (ESS / HDI / R-hat / KDE)
  ModelSelect.hs   -- model comparison (WAIC / PSIS-LOO)
  VI.hs            -- variational inference (ADVI / Adam)

Model/
  HBM.hs           -- polymorphic probabilistic programming DSL (AD gradients, Track-based dependency extraction)
  RFF.hs           -- Random Fourier Features (O(nD) kernel-method approximation)
  GPRobust.hs      -- Robust GP (StudentT / Cauchy likelihoods + IRLS MAP)
  Quantile.hs      -- Quantile regression (τ-quantile, MM-IRLS)
  GAM.hs           -- Generalized Additive Model (additive B-splines + Ridge)
  RandomForest.hs  -- Random Forest regression (CART + bagging + feature importance)

MCMC/
  Core.hs          -- Chain type and posterior statistics (usable standalone)
  MH.hs            -- Random Walk Metropolis-Hastings
  HMC.hs           -- Hamiltonian Monte Carlo (AD gradients)
  NUTS.hs          -- No-U-Turn Sampler (AD gradients + dual averaging)
  Gibbs.hs         -- Gibbs sampling + hybrid Gibbs+MH (automatic conjugacy detection)

Design/
  Orthogonal.hs    -- orthogonal arrays Lₙ (L4/L8/L9/L12/L16/L18 + factor assignment)
  Taguchi.hs       -- Taguchi method (4 SN ratios + factor effects + inner/outer cross design)

Viz/
  MCMC.hs          -- diagnostic plots (KDE / trace / autocorr / pair scatter)
  Report.hs        -- integrated HTML report (multi-chain with R-hat)
  Taguchi.hs       -- Taguchi-analysis HTML report (factor-effect bar charts + optimum table)
  ReportBuilder.hs -- ★ compositional reports (ReportSection + Reportable typeclass,
                                                with interactive prediction + MCMC sections; going-forward standard)
  ReportInstances.hs -- Reportable RegFit/SplineFit/KernelRidgeFit/RFFRidgeFit/RobustGPFit
  AnalysisReport.hs -- [DEPRECATED] LM/GLM/GLMM/GP/HBM-specific sum-type report (kept for CLI regress --report compatibility, scheduled for removal)
  ModelGraph.hs    -- Mermaid.js DAG
  Bar.hs           -- bar charts (vertical / horizontal / stacked / grouped)
  Histogram.hs     -- histograms (with optional theoretical-density overlay)
  Scatter.hs       -- scatter and regression curves
  Core.hs          -- PlotConfig / OutputFormat / openInBrowser (PNG/SVG via vl-convert)
```

---

## API reference

### `Stat.Distribution` — probability distributions

```haskell
import Stat.Distribution

data Distribution
  = Normal      Double Double   -- μ σ
  | Binomial    Int    Double   -- n p
  | Poisson     Double          -- λ (rate)
  | Exponential Double          -- λ (rate)
  | Gamma       Double Double   -- α (shape) β (rate)
  | Beta        Double Double   -- α β

density          :: Distribution -> Double -> Double
logDensity       :: Distribution -> Double -> Double
isContinuous     :: Distribution -> Bool
supportRange     :: Distribution -> (Double, Double)
distributionName :: Distribution -> Text
parseDistribution :: String -> [Double] -> Either String Distribution
```

```haskell
logDensity (Normal 0 1) 1.96   -- ≈ -2.837
density    (Poisson 3)  2      -- P(X=2 | λ=3)
supportRange (Beta 2 5)        -- (0.0, 1.0)
```

---

### `Stat.MCMC` — MCMC diagnostics

```haskell
import Stat.MCMC

-- Autocorrelation (lag 0..maxLag)
autocorr :: Int -> [Double] -> [(Int, Double)]

-- Highest-density interval
hdi :: Double -> [Double] -> (Double, Double)
-- hdi 0.94 samples → (lower, upper)

-- Effective sample size (Geyer's initial monotone sequence estimator)
ess :: [Double] -> Double

-- Split R-hat convergence diagnostic (Vehtari et al. 2021)
-- Input: a list of per-chain sample lists for one parameter
-- R-hat < 1.01 indicates convergence
rhat :: [[Double]] -> Maybe Double

-- Kernel Density Estimation (Gaussian kernel, Silverman bandwidth)
-- Returns nPoints (x, density) pairs
kde :: Int -> [Double] -> [(Double, Double)]
```

```haskell
import Stat.MCMC
import MCMC.Core (chainVals)

-- ESS and R-hat
let muSamples = map (chainVals "mu") chains   -- chains :: [Chain]
    essVal    = ess (head muSamples)
    rhatVal   = rhat muSamples                -- R-hat < 1.01 = converged

-- KDE data for density plots
let kdePoints = kde 200 (chainVals "mu" chain)  -- [(x, density)]
```

---

### `Model.HBM` — polymorphic probabilistic programming DSL

A DSL whose continuation is polymorphic as `forall a. (Floating a, Ord a) => Model a r`.
A single model definition admits four interpretations: structural inspection, log joint, AD gradient, and dependency tracking.

```haskell
import Model.HBM   -- exports Distribution (..), sample, observe

-- Polymorphic DSL type
type ModelP r = forall a. (Floating a, Ord a) => Model a r

-- Declare a latent variable
sample  :: Text -> Distribution a -> Model a a
-- Condition on observed data (assumed i.i.d.)
observe :: Text -> Distribution a -> [Double] -> Model a ()
```

#### Example model

```haskell
import qualified Data.Text as T

-- Hierarchical normal model with 3 schools
-- μ ~ Normal(0, 100),  τ ~ Exponential(0.1)
-- θ_j ~ Normal(μ, τ)  (j=1..J)
-- y_ij ~ Normal(θ_j, 5)
schoolModel :: [[Double]] -> ModelP ()
schoolModel groupData = do
  mu  <- sample "mu"  (Normal 0 100)
  tau <- sample "tau" (Exponential 0.1)
  mapM_ (\(j, ys) -> do
    theta <- sample (T.pack ("theta_" ++ show j)) (Normal mu tau)
    observe (T.pack ("y_" ++ show j)) (Normal theta 5) ys)
    (zip [1::Int ..] groupData)

-- Inspect the structure
describeModel (schoolModel dat)
-- Model nodes:
--   [latent]   mu ~ Normal
--   [latent]   tau ~ Exponential
--   [latent]   theta_1 ~ Normal
--   [observed] y_1 ~ Normal  (n=4)
--   ...
```

> **Note**: a rank-2 type like `ModelP` cannot be `let`-bound, so `m :: ModelP () = schoolModel dat`
> does not work. Use a top-level binding (`m = schoolModel dat`) or inline the call site.

#### Four interpretations

```haskell
import qualified Data.Map.Strict as Map

let ps = Map.fromList [("mu", 73.0), ("tau", 10.0), ...]

-- 1. Structural inspection (a = Double)
collectNodes  (schoolModel dat)              -- :: [Node]
describeModel (schoolModel dat)              -- :: Text

-- 2. Numeric log joint (a = Double)
logJoint      (schoolModel dat) ps           -- log p(θ, y)
logPrior      (schoolModel dat) ps           -- log p(θ)
logLikelihood (schoolModel dat) ps           -- log p(y | θ)

-- 3. AD gradient (a = Forward Double, machine-epsilon precision)
gradAD  (schoolModel dat) ["mu","tau"] [0,1] -- :: [Double]
gradADU (schoolModel dat) names trans us     -- with constraint transforms (for HMC)

-- 4. Dependency tracking (a = Track)
extractDeps (schoolModel dat)                -- :: [Node] (carrying nodeDeps)
buildModelGraph (schoolModel dat)            -- auto-built Mermaid DAG
```

#### Key API

```haskell
type Params = Map Text Double

-- Interpreters
logJoint, logPrior, logLikelihood :: (Floating a, Ord a) => Model a r -> Map Text a -> a
sampleNames    :: ModelP r -> [Text]
collectNodes   :: ModelP r -> [Node]
describeModel  :: ModelP r -> Text
perObsLogLiks  :: ModelP r -> Params -> [Double]   -- for WAIC/LOO

-- AD gradients
gradAD  :: ModelP r -> [Text] -> [Double] -> [Double]
gradADU :: ModelP r -> [Text] -> [Transform] -> [Double] -> [Double]

-- Dependency tracking + DAG
extractDeps     :: ModelP r -> [Node]            -- Node carries nodeDeps :: Set Text
buildModelGraph :: ModelP r -> ModelGraph        -- builds the DAG automatically (no manual edges)

-- Constraint transforms (for HMC/NUTS/VI)
getTransforms        :: ModelP r -> Map Text Transform   -- inferred from priors
logJointUnconstrained :: (Floating a, Ord a) => Model a r -> [Text] -> [Transform] -> Map Text a -> a

-- Structural extraction (for Gibbs conjugacy detection)
runObserveDists :: Model Double r -> Map Text Double -> [(Text, Distribution Double, [Double])]
priorList       :: Model Double r -> [(Text, Distribution Double)]
```

---

### `MCMC.Core` — Chain type and posterior statistics

Common types not tied to any MCMC algorithm. Importable on its own.

```haskell
import MCMC.Core

data Chain = Chain
  { chainSamples  :: [Map Text Double]  -- post-burn-in samples
  , chainAccepted :: Int
  , chainTotal    :: Int
  }

acceptanceRate   :: Chain -> Double
posteriorMean    :: Text -> Chain -> Maybe Double
posteriorSD      :: Text -> Chain -> Maybe Double
posteriorQuantile :: Double -> Text -> Chain -> Maybe Double
-- posteriorQuantile 0.025 "mu" chain  → lower 2.5%

-- Sample series usable for R-hat etc.
chainVals :: Text -> Chain -> [Double]

-- For parallel chains: spawn a child GenIO independent of the base GenIO
spawnGen :: GenIO -> IO GenIO
```

---

### `MCMC.MH` — Random Walk Metropolis-Hastings

```haskell
import MCMC.MH
import System.Random.MWC (createSystemRandom)

data MCMCConfig = MCMCConfig
  { mcmcIterations :: Int              -- post-burn-in samples
  , mcmcBurnIn     :: Int              -- burn-in steps to discard
  , mcmcStepSizes  :: Map Text Double  -- per-parameter proposal SD
  }

defaultMCMCConfig :: [Text] -> MCMCConfig
-- mcmcIterations=2000, mcmcBurnIn=500, stepSize=1.0

metropolis       :: Model a -> MCMCConfig -> Params -> GenIO -> IO Chain
metropolisChains :: Model a -> MCMCConfig -> Int    -> Params -> GenIO -> IO [Chain]
-- metropolisChains m cfg 4 init_ gen  -- 4 chains in parallel (use +RTS -N for CPU parallelism)
```

```haskell
main :: IO ()
main = do
  let m   = schoolModel dat
      cfg = (defaultMCMCConfig (sampleNames m))
              { mcmcIterations = 5000
              , mcmcBurnIn     = 1000
              , mcmcStepSizes  = Map.fromList
                  [("mu", 5.0), ("tau", 2.0),
                   ("theta_1", 3.0), ("theta_2", 3.0), ("theta_3", 3.0)]
              }
      init_ = Map.fromList [("mu",73),("tau",10),
                            ("theta_1",71.5),("theta_2",86.25),("theta_3",61.75)]
  gen   <- createSystemRandom
  chain <- metropolis m cfg init_ gen
  -- Target acceptance rate: 0.20 ~ 0.50
```

---

### `MCMC.HMC` — Hamiltonian Monte Carlo

Powered by exact gradients via `Numeric.AD.Mode.Forward`.
Constrained parameters (Exponential / Gamma → positive, Beta → unit interval) are
mapped into the unconstrained space (log / logit transforms) before the leapfrog.
Jacobian corrections are applied automatically, so initial values are passed in the
ordinary parameter space.

```haskell
import MCMC.HMC
import System.Random.MWC (createSystemRandom)

data HMCConfig = HMCConfig
  { hmcIterations    :: Int
  , hmcBurnIn        :: Int
  , hmcStepSize      :: Double  -- leapfrog step size ε
  , hmcLeapfrogSteps :: Int     -- number of leapfrog steps L
  }

defaultHMCConfig :: HMCConfig
-- hmcIterations=2000, hmcBurnIn=500, hmcStepSize=0.1, hmcLeapfrogSteps=10

hmc       :: Model a -> HMCConfig -> Params -> GenIO -> IO Chain
hmcChains :: Model a -> HMCConfig -> Int    -> Params -> GenIO -> IO [Chain]
```

```haskell
main :: IO ()
main = do
  let m   = gaussianModel observed   -- μ ~ Normal(0,10), σ ~ Exponential(1)
      cfg = defaultHMCConfig
              { hmcIterations    = 3000
              , hmcBurnIn        = 500
              , hmcStepSize      = 0.1
              , hmcLeapfrogSteps = 10
              }
      init_ = Map.fromList [("mu", 0.0), ("sigma", 1.0)]
  gen   <- createSystemRandom
  chain <- hmc m cfg init_ gen
  -- σ samples are guaranteed > 0 (log transform never escapes the support)
  print (posteriorMean "sigma" chain)

  -- 4 chains in parallel
  chains <- hmcChains m cfg 4 init_ gen
```

**Tuning guidelines:**
- Tune `hmcStepSize` so the acceptance rate is in 60–80%.
- For hierarchical models or strongly correlated parameters, increasing `hmcLeapfrogSteps` to 20–50 typically improves efficiency.

---

### `MCMC.NUTS` — No-U-Turn Sampler

Implements Hoffman & Gelman (2014) Algorithm 3.
The trajectory length is determined automatically by the U-turn criterion, so
`hmcLeapfrogSteps` does not need tuning. Constraint transforms are applied
automatically as in HMC.

```haskell
import MCMC.NUTS
import System.Random.MWC (createSystemRandom)

data NUTSConfig = NUTSConfig
  { nutsIterations    :: Int
  , nutsBurnIn        :: Int
  , nutsStepSize      :: Double  -- initial step size ε₀
  , nutsMaxDepth      :: Int     -- maximum tree depth (default 10)
  , nutsAdaptStepSize :: Bool    -- adapt ε via dual averaging during burn-in (default True)
  , nutsTargetAccept  :: Double  -- target acceptance δ (default 0.8)
  }

defaultNUTSConfig :: NUTSConfig
-- nutsIterations=2000, nutsBurnIn=500, nutsStepSize=0.1,
-- nutsMaxDepth=10, nutsAdaptStepSize=True, nutsTargetAccept=0.8

nuts       :: Model a -> NUTSConfig -> Params -> GenIO -> IO Chain
nutsChains :: Model a -> NUTSConfig -> Int    -> Params -> GenIO -> IO [Chain]
```

```haskell
main :: IO ()
main = do
  let m   = schoolModel dat
      -- nutsAdaptStepSize=True (default) adapts ε during burn-in
      cfg = defaultNUTSConfig { nutsStepSize = 0.1 }
      -- Disable adaptation and use a fixed ε:
      -- cfg = defaultNUTSConfig { nutsStepSize = 0.08, nutsAdaptStepSize = False }
      init_ = Map.fromList [("mu",73),("tau",10),
                            ("theta_1",71.5),("theta_2",86.25),("theta_3",61.75)]
  gen <- createSystemRandom

  -- Single chain
  chain <- nuts m cfg init_ gen

  -- 4 chains in parallel (check convergence with R-hat)
  chains <- nutsChains m cfg 4 init_ gen
  let rhatMu = rhat (map (chainVals "mu") chains)
  print rhatMu  -- Just 1.001 → converged
```

---

### Multi-chain runs and R-hat convergence diagnostics

```haskell
import MCMC.NUTS  (nutsChains, defaultNUTSConfig, NUTSConfig (..))
import MCMC.Core  (chainVals)
import Stat.MCMC  (rhat, ess)
import System.Random.MWC (createSystemRandom)

main :: IO ()
main = do
  gen <- createSystemRandom
  let cfg = defaultNUTSConfig { nutsIterations = 2000, nutsStepSize = 0.1 }

  -- Run 4 chains in parallel (use +RTS -N to control CPU parallelism)
  chains <- nutsChains model cfg 4 initParams gen

  -- R-hat per parameter
  let params = sampleNames model
  mapM_ (\p -> do
    let r = rhat (map (chainVals p) chains)
    putStrLn $ show p ++ ": R-hat = " ++ show r
    ) params
  -- "mu":    R-hat = Just 1.001  (< 1.01 = converged)
  -- "sigma": R-hat = Just 1.003
```

---

### `Viz.Report` — integrated MCMC HTML report (recommended)

```haskell
import Viz.Report
import MCMC.Core (Chain)
import Model.HBM (ModelGraph)

data MCMCReport = MCMCReport
  { reportTitle    :: Text
  , reportGraph    :: Maybe ModelGraph  -- Nothing to omit the model graph
  , reportChain    :: Chain             -- representative chain (used for autocorr / pair)
  , reportChains   :: [Chain]           -- all parallel chains (empty = single-chain mode)
  , reportParams   :: [Text]
  , reportPairs    :: [(Text, Text)]    -- pair scatter
  , reportMaxLag   :: Int               -- max lag for autocorr
  }

defaultReport :: Text -> Chain -> [Text] -> MCMCReport
-- reportGraph=Nothing, reportChains=[], reportPairs=[], reportMaxLag=40

renderReport :: FilePath -> MCMCReport -> IO ()
```

**Single-chain report:**

```haskell
let report = (defaultReport "My Model" chain names)
               { reportGraph = Just graph
               , reportPairs = [("mu", "tau")]
               }
renderReport "report.html" report
```

**Multi-chain report (with R-hat column):**

```haskell
chains <- nutsChains model cfg 4 initParams gen

let report = (defaultReport "My Model" (head chains) names)
               { reportGraph  = Just graph
               , reportChains = chains   -- setting this enables multi-chain mode
               , reportPairs  = [("mu", "tau")]
               }
renderReport "report_multi.html" report
```

Multi-chain HTML layout:
- **Model Graph** — Mermaid.js DAG
- **Posterior Summary** — stat-box + Mean/SD/2.5%/97.5%/ESS/**R-hat** table (R-hat < 1.01 in green, ≥ 1.01 in red)
- **MCMC Diagnostics** — KDE density (94% HDI) + per-chain colored trace
- **Autocorrelation** — autocorrelation of the representative chain
- **Pair Scatter** — joint posterior scatter

---

### `Viz.MCMC` — individual MCMC plots

For when you want fine-grained control over plots without going through `Viz.Report`.

```haskell
import Viz.MCMC
import Viz.Core (defaultConfig, OutputFormat (..))

-- Single chain: [KDE | trace] stacked vertically (PyMC style)
mcmcDiagnostics     :: PlotConfig -> [Text] -> Chain  -> VegaLite
mcmcDiagnosticsFile :: OutputFormat -> FilePath -> PlotConfig -> [Text] -> Chain  -> IO ()

-- Multi chain: [merged KDE | colored trace] stacked vertically
mcmcDiagnosticsMulti     :: PlotConfig -> [Text] -> [Chain] -> VegaLite
mcmcDiagnosticsMultiFile :: OutputFormat -> FilePath -> PlotConfig -> [Text] -> [Chain] -> IO ()

-- Multi-chain trace only
multiTracePlot     :: PlotConfig -> [Text] -> [Chain] -> VegaLite
multiTracePlotFile :: OutputFormat -> FilePath -> PlotConfig -> [Text] -> [Chain] -> IO ()

-- Autocorrelation bar chart
autocorrPlot     :: PlotConfig -> Int -> [Text] -> Chain -> VegaLite
autocorrPlotFile :: OutputFormat -> FilePath -> PlotConfig -> Int -> [Text] -> Chain -> IO ()

-- Pair scatter (joint posterior)
pairScatter     :: PlotConfig -> Text -> Text -> Chain -> VegaLite
pairScatterFile :: OutputFormat -> FilePath -> PlotConfig -> Text -> Text -> Chain -> IO ()

-- KDE only / trace only
posteriorPlot     :: PlotConfig -> [Text] -> Chain -> VegaLite
tracePlot         :: PlotConfig -> [Text] -> Chain -> VegaLite
```

```haskell
let cfg = defaultConfig "School Model"

-- Single-chain diagnostics
mcmcDiagnosticsFile HTML "diag.html" cfg names chain

-- Multi-chain diagnostics
mcmcDiagnosticsMultiFile HTML "diag_multi.html" cfg names chains

-- Individual plots
autocorrPlotFile HTML "acf.html"  cfg 40 names chain
pairScatterFile  HTML "pair.html" (defaultConfig "μ vs τ") "mu" "tau" chain
```

---

### `Viz.Histogram` — histograms

```haskell
import Viz.Histogram
import Viz.Core (defaultConfig, OutputFormat (..))

-- Plain histogram
histogramPlot     :: PlotConfig -> Text -> [Double] -> Maybe Int -> VegaLite
histogramPlotFile :: OutputFormat -> FilePath -> PlotConfig -> Text -> [Double] -> Maybe Int -> IO ()

-- Overlay theoretical density
histogramWithDensity     :: PlotConfig -> Text -> [Double] -> Maybe Int -> Distribution -> VegaLite
histogramWithDensityFile :: OutputFormat -> FilePath -> PlotConfig -> Text -> [Double] -> Maybe Int -> Distribution -> IO ()
```

```haskell
let vals = [1.2, 3.4, 2.1, ...]
histogramWithDensityFile HTML "hist.html"
  (defaultConfig "Score Distribution") "score" vals Nothing (Normal 2.5 1.0)
```

---

## Full workflow example (NUTS, 4 chains)

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad (forM)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import System.Random.MWC (createSystemRandom)

import Model.HBM
import MCMC.Core  (chainVals, posteriorMean, posteriorSD)
import MCMC.NUTS  (nutsChains, defaultNUTSConfig, NUTSConfig (..))
import Stat.Distribution
import Stat.MCMC  (rhat, ess)
import Viz.Core   (openInBrowser)
import Viz.Report (MCMCReport (..), defaultReport, renderReport)

-- 1. Model definition (σ has an Exponential prior → positivity is handled automatically)
myModel :: [Double] -> Model Double
myModel ys = do
  mu    <- sample "mu"    (Normal 0 10)
  sigma <- sample "sigma" (Exponential 1)
  observe "y" (Normal mu sigma) ys
  return mu

main :: IO ()
main = do
  let dat   = [1.2, 2.3, 3.1, 2.8, 1.9]
      m     = myModel dat
      names = sampleNames m
      cfg   = defaultNUTSConfig { nutsIterations = 3000, nutsStepSize = 0.1 }
      init_ = Map.fromList [("mu", 0.0), ("sigma", 1.0)]

  gen <- createSystemRandom

  -- 2. 4 chains of NUTS in parallel
  chains <- nutsChains m cfg 4 init_ gen

  -- 3. Convergence check via R-hat
  mapM_ (\p -> do
    let r = rhat (map (chainVals p) chains)
    putStrLn $ T.unpack p ++ ": R-hat = " ++ show r
    ) names

  -- 4. Build the model graph
  let graph = buildModelGraph m [("mu", "y"), ("sigma", "y")]

  -- 5. Multi-chain integrated report
  let report = (defaultReport "Gaussian Model" (head chains) names)
                 { reportGraph  = Just graph
                 , reportChains = chains   -- multi-chain mode: R-hat column + colored trace
                 , reportPairs  = []
                 }
  renderReport "report.html" report
  openInBrowser "report.html"
```

---

## Sampler selection guide

| Sampler | When to use | Notes |
|---|---|---|
| `MCMC.MH` (Metropolis) | Simple sanity-check models | ESS collapses on high-dim or strongly correlated targets |
| `MCMC.HMC` | Continuous parameters, moderate-size models | Both `stepSize` and `leapfrogSteps` need tuning |
| `MCMC.NUTS` | **Recommended default** | Only `stepSize` to tune; `leapfrogSteps` not needed |
| `MCMC.Gibbs` | Conjugate models (very fast) | Cannot be used for non-conjugate parameters |

Details → [MCMC sampler selection guide](docs/bayesian/03-mcmc-samplers.md) / [Gibbs sampling](docs/bayesian/04-gibbs.md)

**Step-size guidelines:**
- NUTS: target acceptance 60–85%. Too low → reduce `stepSize`.
- HMC: same. If acceptance is low, also reduce `leapfrogSteps`.
- MH: target 20–50%. Tune `mcmcStepSizes` per parameter.

**Constrained parameters:**
- `Exponential` / `Gamma` → positivity (`PositiveT`: log transform)
- `Beta` → unit interval (`UnitIntervalT`: logit transform)
- HMC / NUTS apply Jacobian corrections automatically; pass initial values in the natural parameterization.

---

### `MCMC.Gibbs` — Gibbs sampling

Samples directly from a parameter's conjugate full conditional.
With no rejection step, conjugate models reach 3–5× higher ESS/sec than NUTS.

```haskell
import MCMC.Gibbs

-- Pre-built conjugate updates
normalNormal :: Text -> Double -> Double -> [Double] -> Double -> GibbsUpdate
-- Direct sampling from the conditional posterior of μ ~ Normal(μ₀,σ₀), y ~ Normal(μ,σ_lik)

betaBinomial :: Text -> Double -> Double -> Int -> Int -> GibbsUpdate
-- p ~ Beta(α,β), y ~ Binomial(n,p), k successes → Beta(α+k, β+n-k)

gammaPoisson :: Text -> Double -> Double -> [Double] -> GibbsUpdate
-- λ ~ Gamma(α,β), y ~ Poisson(λ) → Gamma(α+Σy, β+n)

gibbs       :: [GibbsUpdate] -> GibbsConfig -> Params -> GenIO -> IO Chain
gibbsChains :: [GibbsUpdate] -> GibbsConfig -> Int    -> Params -> GenIO -> IO [Chain]
```

```haskell
let updates = [ normalNormal "mu" 0 10 obsData 2.0 ]  -- σ_lik=2 known
    cfg     = defaultGibbsConfig { gibbsIterations = 5000 }
chain <- gibbs updates cfg (Map.fromList [("mu", 0.0)]) gen
```

Details → [Gibbs sampling guide](docs/bayesian/04-gibbs.md)

---

### `Stat.VI` — variational inference (ADVI)

Approximates the posterior with a normal family and maximizes the ELBO with Adam.
Faster than NUTS, but the mean-field assumption ignores cross-parameter correlations.

```haskell
import Stat.VI

advi :: Model a -> VIConfig -> Params -> GenIO -> IO VIResult

data VIResult = VIResult
  { viPostMeans   :: Params    -- posterior means
  , viPostSDs     :: Params    -- posterior SDs
  , viElboHistory :: [Double]  -- ELBO convergence trace
  , viDraws       :: [Params]  -- posterior draws
  }
```

```haskell
let cfg = defaultVIConfig { viIterations = 500, viNumDraws = 5000 }
result <- advi model cfg initP gen
print (viPostMeans result)
```

Details → [Variational inference guide](docs/bayesian/05-vi.md)

---

### `Stat.ModelSelect` — model comparison (WAIC / PSIS-LOO)

Computes information criteria from MCMC chains for model comparison. Lower is better.

```haskell
import Stat.ModelSelect

chainWAIC :: Model a -> Chain -> WAICResult
chainLOO  :: Model a -> Chain -> LOOResult

data WAICResult = WAICResult
  { waicValue :: Double  -- WAIC (lower is better)
  , waicLppd  :: Double  -- log pointwise predictive density
  , waicPwaic :: Double  -- effective number of parameters
  , waicSE    :: Double  -- standard error
  }

data LOOResult = LOOResult
  { looValue   :: Double    -- LOO-CV (lower is better)
  , looKHat    :: [Double]  -- per-observation Pareto k̂ (> 0.7 is concerning)
  , looKHatBad :: Int       -- number of observations with k̂ > 0.7
  }
```

```haskell
let waicA = chainWAIC modelA chainA
    waicB = chainWAIC modelB chainB
printf "ΔWAIC(A−B) = %.3f\n" (waicValue waicA - waicValue waicB)
-- Negative → A is better; rule of thumb: |ΔWAIC| > SE
```

Details → [Model comparison guide](docs/bayesian/06-model-comparison.md)

---

### `Viz.Bar` — bar charts

```haskell
import Viz.Bar
import Viz.Core (defaultConfig, OutputFormat (..))

-- Vertical / horizontal bars
barChartFile  HTML "bar.html"  cfg "category" "value" labels vals
barChartHFile HTML "barh.html" cfg "value" "category" labels vals

-- Stacked / grouped bars
stackedBarFile HTML "stacked.html" cfg "x" "y" "group" xs ys groups
groupedBarFile HTML "grouped.html" cfg "x" "y" "group" xs ys groups
```

Details → [Visualization guide](docs/visualization/01-visualization.md)

---

## Notes

- **Low ESS**: check the trace plot for poor mixing and re-tune the step size. NUTS substantially improves ESS-per-time over HMC.
- **High R-hat** (≥ 1.01): increase burn-in, scatter the initial values, or adjust the step size.
- **Distribution shown for non-root nodes**: `collectNodes` continues latent variables with the placeholder value `0`, so distribution parameters of nodes with dependencies do not have meaningful values. The model graph displays only the family name.
- **Test data**: place under `demo/` (do not use `/tmp`).
- **CPU parallelism**: `nutsChains` / `hmcChains` / `metropolisChains` parallelize across OS threads with `+RTS -N`. Example: `cabal run hbm-example -- +RTS -N4`.
- **Removed legacy modules** `Model.MCMC` / `Model.HMC` / `Model.NUTS`. Use `MCMC.MH` / `MCMC.HMC` / `MCMC.NUTS` instead.
