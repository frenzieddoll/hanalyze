# hanalyze

> 🌐 **English** | [日本語](README.ja.md)

[![License: BSD-3](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](LICENSE)
[![GHC](https://img.shields.io/badge/GHC-9.6.7-blueviolet.svg)](https://www.haskell.org/ghc/)

**hanalyze** is a Haskell-native statistical engineering toolkit: regression, GLMM, Bayesian inference (HMC/NUTS/Gibbs/ADVI/SMC), Gaussian processes, machine learning (SVM / gradient boosting / neural networks), survival analysis (KM / Cox / AFT / competing risks), time series (ARIMA / GARCH / state space), causal discovery (LiNGAM) and treatment-effect estimation, design of experiments (classical + custom optimal design), multi-objective optimisation, native plotting, and HTML reporting integrated under one API.
Core modelling and optimisation logic is implemented in Haskell, with numerical linear algebra delegated to hmatrix/BLAS/LAPACK. **No R/Stan/Python bridge required**.
Benchmarks (see below) show competitive accuracy with Python/R references in the tested cases. Performance varies by domain: optimisation and small-to-medium MCMC workloads are often faster in these benchmarks, while large-scale ML/GLM workloads are currently slower than sklearn.

---

## Highlights

- **Haskell-native**: types catch many dtype/API mismatches; shape checks happen at runtime where needed
- **Algorithms in Haskell, BLAS for numerics**: hmatrix/BLAS/LAPACK powers linear algebra; no R/Stan/Python bridge
- **Native plotting**: 90+ documented figure types through the [hgg](https://github.com/frenzieddoll/hgg) grammar-of-graphics integration (`plot-integration` flag) — pure-Haskell SVG output, no browser required (see [Gallery](#gallery))
- **HTML reporting**: MathJax/Mermaid + Vega-Lite visualisations in one call; PNG/SVG export available for supported plots
- **Dirty-data defence**: 8 warning codes + auto-sniff (delim/header/encoding) + cleaning DSL
- **Hackage `dataframe`**: Polars-like DataFrame used directly; CSV native, Parquet/JSON support through `dataframe`

---

## Gallery

Every figure below (and 90+ more across [`docs/`](docs/)) is generated straight
from analysis results via the hgg integration — pure Haskell, SVG out.

| | |
|:--:|:--:|
| ![Linear regression with CI band](docs/images/lm-scatter-ci.svg)<br>Linear regression — fit + 95% CI ([docs](docs/regression/01-lm.md)) | ![HBM MCMC dashboard](docs/images/hbm-dashboard.svg)<br>Bayesian MCMC dashboard — trace / density / R̂ / ESS ([docs](docs/bayesian/viz-diagnostics.md)) |
| ![Gaussian process mean and credible band](docs/images/gp-mean-ci.svg)<br>Gaussian process — mean + credible band ([docs](docs/regression/04-gp.md)) | ![Kernel SVM decision boundary](docs/images/svm-rbf-boundary.svg)<br>Kernel SVM (RBF) — decision boundary + support vectors ([docs](docs/ml/usage-ml-extensions.md)) |
| ![DOE prediction profiler](docs/images/doe-profiler.svg)<br>DOE prediction profiler — response vs each factor + CI ([docs](docs/api-guide/09-doe.md)) | ![RSM 3D response surface](docs/images/rsm-surface-3d.svg)<br>RSM response surface (3D) ([docs](docs/doe/01-doe.md)) |
| ![DirectLiNGAM causal DAG](docs/images/lingam-dag.svg)<br>DirectLiNGAM causal discovery — estimated DAG ([docs](docs/api-guide/08-causal.md)) | ![Kaplan-Meier survival curves](docs/images/km-survival.svg)<br>Kaplan-Meier survival curves ([docs](docs/regression/10-survival.md)) |
| ![Time-series forecast](docs/images/ts-forecast.svg)<br>Time-series forecast ([docs](docs/regression/09-timeseries.md)) | ![k-means clusters with 95% ellipses](docs/images/kmeans-ellipse.svg)<br>k-means clusters + 95% ellipses ([docs](docs/stat/05-cluster.md)) |

---

## Capabilities

Features grouped by category. Each capability links to a usage doc and (where relevant) a theory doc.
The full API reference lives in [`docs/api-guide/`](docs/api-guide/README.md) (12 chapters).

### Statistical inference (`Hanalyze.Stat.*`)

| Feature | Module | Usage | Theory |
|---|---|---|---|
| 12 hypothesis tests (t/χ²/ANOVA/Wilcoxon/KS/Shapiro/Levene/Bartlett/...) | `Hanalyze.Stat.Test` | [stat/01-test.md](docs/stat/01-test.md) | — |
| Multiple-testing correction (Bonferroni/Holm/BH/BY) | `Hanalyze.Stat.MultipleTesting` | [stat/06-multipletesting.md](docs/stat/06-multipletesting.md) | — |
| Bootstrap CI / permutation tests | `Hanalyze.Stat.Bootstrap` | [stat/07-bootstrap.md](docs/stat/07-bootstrap.md) | — |
| Effect size + power analysis (Cohen's d/η²/Cramér V/n estimation) | `Hanalyze.Stat.Effect` | [stat/09-effect.md](docs/stat/09-effect.md) | — |
| Cross-validation (k-fold/stratified/LOO) + Grid search | `Hanalyze.Stat.CV` | [stat/04-cv.md](docs/stat/04-cv.md) | — |

### Regression (`Hanalyze.Model.*`)

| Feature | Module | Usage | Theory |
|---|---|---|---|
| Formula DSL (declare models as `"y x = b0 + b1*x + bg ! group"` or R `"y ~ x + C(g)"`; `ModelFrame` / `designMatrixF` / `fitLMF` + missing policy / contrast `C(g, Sum)` / WLS `fitWLSF` / nonlinear `fitNLS` / random effects `(1+x|g)` via `fitMixedLME`/`fitMixedGLMM`) | `Hanalyze.Model.Formula` / `.Frame` / `.Design` / `.RFormula` / `.Nonlinear` / `.Mixed` | [regression/11-formula-dsl.md](docs/regression/11-formula-dsl.md) | — |
| Linear regression (LM) + inference stats (SE/t/p, F, AIC/BIC, leverage, Cook's) | `Hanalyze.Model.LM` / `Hanalyze.Model.LM.Diagnostics` | [regression/01-lm.md](docs/regression/01-lm.md) | [principles/lm.md](docs/principles/lm.md) |
| GLM (Binomial / Poisson / Gaussian) | `Hanalyze.Model.GLM` | [regression/02-glm.md](docs/regression/02-glm.md) | [principles/glm.md](docs/principles/glm.md) |
| GLMM / mixed-effects model (LME) | `Hanalyze.Model.GLMM` | [regression/03-glmm.md](docs/regression/03-glmm.md) | [principles/glmm.md](docs/principles/glmm.md) |
| Spline regression (B-spline / NaturalCubic) | `Hanalyze.Model.Spline` | [regression/04-spline.md](docs/regression/04-spline.md) | [regression/theory-regression-extensions.md](docs/regression/theory-regression-extensions.md) |
| Kernel regression (NW / Kernel Ridge) + multi-D inputs | `Hanalyze.Model.Kernel` | [regression/04-kernel.md](docs/regression/04-kernel.md) | same |
| Regularised (Ridge / Lasso / ElasticNet) | `Hanalyze.Model.Regularized` | [regression/04-regularized.md](docs/regression/04-regularized.md) | same |
| Robust regression (Huber / Tukey biweight M-estimators, IRLS) | `Hanalyze.Model.Robust` | [regression/usage-regularized-advanced.md](docs/regression/usage-regularized-advanced.md) | — |
| Gaussian process (RBF / Matérn / Periodic + ARD + multi-input) | `Hanalyze.Model.GP` | [regression/04-gp.md](docs/regression/04-gp.md) | [principles/gp.md](docs/principles/gp.md) |
| Random Fourier Features (large-scale GP approximation) | `Hanalyze.Model.RFF` | [regression/04-rff.md](docs/regression/04-rff.md) | [regression/theory-regression-extensions.md](docs/regression/theory-regression-extensions.md) |
| Multivariate regression / Multi-output GP | `Hanalyze.Model.{Multivariate,MultiGP,MultiOutput}` | [regression/05-multivariate.md](docs/regression/05-multivariate.md) | [regression/theory-multivariate.md](docs/regression/theory-multivariate.md) |
| Quantile regression | `Hanalyze.Model.Quantile` | [regression/06-quantile.md](docs/regression/06-quantile.md) | [regression/theory-regression-extensions.md](docs/regression/theory-regression-extensions.md) |
| Generalized additive model (GAM) | `Hanalyze.Model.GAM` | [regression/06-gam.md](docs/regression/06-gam.md) | same |
| Random forest (regression) | `Hanalyze.Model.RandomForest` | [regression/06-randomforest.md](docs/regression/06-randomforest.md) | same |
| Multi-output regression + interactive HTML | `Hanalyze.Model.MultiOutput` | [regression/07-multireg.md](docs/regression/07-multireg.md) | [regression/theory-multivariate.md](docs/regression/theory-multivariate.md) |
| Partial Least Squares (PLS) regression — NIPALS + VIP + CV component selection | `Hanalyze.Model.PLS` | — | — |
| Linear / Quadratic Discriminant Analysis (LDA / QDA) | `Hanalyze.Model.Discriminant` | — | — |
| Gauge R&R (Measurement System Analysis, ANOVA-based crossed / nested) | `Hanalyze.Design.GaugeRR` | — | — |

### Machine learning (`Hanalyze.Model.*` / `Hanalyze.Stat.*`)

| Feature | Module | Usage | Theory |
|---|---|---|---|
| PCA + cumulative variance + standardisation | `Hanalyze.Model.PCA` | [stat/02-pca.md](docs/stat/02-pca.md) | — |
| Clustering (K-means + k-means++ + silhouette) | `Hanalyze.Model.Cluster` | [stat/05-cluster.md](docs/stat/05-cluster.md) | — |
| Decision tree (CART classifier) | `Hanalyze.Model.DecisionTree` | [regression/08-decisiontree.md](docs/regression/08-decisiontree.md) | — |
| Kernel SVM (C-SVC, SMO dual solver) + CV hyperparameter tuning | `Hanalyze.Model.SVM` | [ml/usage-ml-extensions.md](docs/ml/usage-ml-extensions.md) | — |
| Gradient boosting (regression + binary classification) | `Hanalyze.Model.GradientBoosting` | [ml/usage-ml-extensions.md](docs/ml/usage-ml-extensions.md) | — |
| k-NN / Naive Bayes (Gaussian + Multinomial) / MLP neural network (mini-batch SGD + Adam) | `Hanalyze.Model.{KNN,NaiveBayes,NeuralNetwork}` | [ml/usage-ml-extensions.md](docs/ml/usage-ml-extensions.md) + [api-guide/05-ml.md](docs/api-guide/05-ml.md) | — |
| Random forest classifier (+ permutation importance) | `Hanalyze.Model.RandomForestClassifier` | [api-guide/05-ml.md](docs/api-guide/05-ml.md) | — |
| MDS (classical / Sammon) | `Hanalyze.Model.MDS` | [ml/usage-ml-extensions.md](docs/ml/usage-ml-extensions.md) | — |
| Hierarchical clustering (agglomerative + dendrogram) | `Hanalyze.Model.HierarchicalCluster` | [stat/05-cluster.md](docs/stat/05-cluster.md) | — |
| Latent class analysis (EM) + graphical-lasso correlation network | `Hanalyze.Model.LatentClassAnalysis` / `Hanalyze.Stat.CorrelationNetwork` | [stat/usage-misc-stat.md](docs/stat/usage-misc-stat.md) | — |
| Functional data analysis (basis smoothing + FPCA) | `Hanalyze.Model.FDA` | [fda/usage-fda.md](docs/fda/usage-fda.md) | — |
| Time series (ARIMA / Holt-Winters / STL / ACF / PACF) | `Hanalyze.Model.TimeSeries` | [regression/09-timeseries.md](docs/regression/09-timeseries.md) | — |
| GARCH(1,1) volatility / linear-Gaussian state space (Kalman filter + RTS smoother) / VAR(p) | `Hanalyze.Model.{GARCH,StateSpace,VAR}` | [timeseries/usage-ts-surv-advanced.md](docs/timeseries/usage-ts-surv-advanced.md) | — |
| Survival analysis (Kaplan-Meier / Nelson-Aalen / Log-rank / Cox PH) | `Hanalyze.Model.Survival` | [regression/10-survival.md](docs/regression/10-survival.md) | — |
| Parametric survival (AFT) + competing risks (CIF) | `Hanalyze.Model.{AFT,CompetingRisks}` | [api-guide/07-survival.md](docs/api-guide/07-survival.md) | — |
| Classification metrics (Confusion / AUC / F1 / MCC / log-loss / Brier) | `Hanalyze.Stat.ClassMetrics` | [stat/03-classmetrics.md](docs/stat/03-classmetrics.md) | — |
| Model interpretation (Permutation imp / PDP / ICE) | `Hanalyze.Stat.Interpret` | [stat/13-interpret.md](docs/stat/13-interpret.md) | — |
| SPC control charts (X̄-R / I-MR / p / np / c / u) + Western Electric / Nelson 8-rule sets | `Hanalyze.Stat.SPC` | — | — |
| Weibull MLE (censored / uncensored) + B_p life + Wald CI | `Hanalyze.Model.Weibull` | — | — |
| Accelerated-life models (Arrhenius / Eyring / Inverse Power Law) | `Hanalyze.Model.Reliability` | — | — |
| NSGA-II all-fronts (rank ≥ 1 alternatives) + per-generation progress callback | `Hanalyze.Optim.NSGA` | — | — |
| Good vs Bad parallel comparison (Welch t + Cohen's d ranking) | `Hanalyze.Stat.GroupComparison` | — | — |
| Hotelling T² (1-/2-sample) + one-way MANOVA (Wilks' Λ + Rao F) | `Hanalyze.Stat.Test` | — | — |
| Lasso/Ridge/ElasticNet λ auto-selection via k-fold CV + 1-SE rule | `Hanalyze.Model.Regularized` | — | — |
| D-optimal Augment Design (sequential addition with fixed existing rows) | `Hanalyze.Design.Optimal` | — | — |
| Space-filling designs (LHS / Maximin LHS / Halton) | `Hanalyze.Design.SpaceFilling` | — | — |
| Definitive Screening Design (k=4 verified, others structural) | `Hanalyze.Design.DSD` | — | — |
| Mixture design (Simplex Lattice / Simplex Centroid) | `Hanalyze.Design.Mixture` | — | — |
| Sequential RSM (steepest ascent + next CCD placement) | `Hanalyze.Design.Sequential` | — | — |

### Causal inference (`Hanalyze.Model.LiNGAM.*` / `Hanalyze.Stat.Causal.*`)

| Feature | Module | Usage | Theory |
|---|---|---|---|
| LiNGAM causal discovery (DirectLiNGAM / ICA-LiNGAM / Pairwise / VAR-LiNGAM / MultiGroup / ParceLiNGAM + bootstrap edge confidence) | `Hanalyze.Model.LiNGAM.*` | [api-guide/08-causal.md](docs/api-guide/08-causal.md) | — |
| Treatment effects (propensity score / IPW / doubly robust AIPW / CATE S-T-X meta-learners) | `Hanalyze.Stat.Causal.*` | [causal/usage-causal.md](docs/causal/usage-causal.md) | — |

### Bayesian (`Hanalyze.MCMC.*` / `Hanalyze.Stat.*` / `Hanalyze.Model.HBM`)

| Feature | Module | Usage | Theory |
|---|---|---|---|
| 27 probability distributions (Truncated/Censored/MvNormal/LKJ/Multinomial/...) | `Hanalyze.Stat.Distribution` | [bayesian/01-distributions.md](docs/bayesian/01-distributions.md) | [bayesian/theory-distributions.md](docs/bayesian/theory-distributions.md) |
| Probabilistic model DSL (HBM polymorphic free monad, incl. `deterministic` / `dataNamed`) | `Hanalyze.Model.HBM` | [bayesian/02-probabilistic-model.md](docs/bayesian/02-probabilistic-model.md) | [principles/hbm.md](docs/principles/hbm.md) |
| MCMC samplers (MH / HMC / NUTS / Slice / tempered SMC) | `Hanalyze.MCMC.{MH,HMC,NUTS,Slice,SMC}` | [bayesian/03-mcmc-samplers.md](docs/bayesian/03-mcmc-samplers.md) | [bayesian/theory-mcmc.md](docs/bayesian/theory-mcmc.md) / [theory-hmc-nuts.md](docs/bayesian/theory-hmc-nuts.md) |
| Sampling progress display (aggregate one-liner; IO verb `df \|->! spec`, bit-identical to the pure verb) | `Hanalyze.MCMC.Progress` | [io/04-fit-api.md](docs/io/04-fit-api.md) | — |
| Gibbs sampling (auto-conjugate detection + hybrid) | `Hanalyze.MCMC.Gibbs` | [bayesian/04-gibbs.md](docs/bayesian/04-gibbs.md) | [bayesian/theory-mcmc.md](docs/bayesian/theory-mcmc.md) |
| Variational inference (ADVI mean-field Adam) | `Hanalyze.Stat.VI` | [bayesian/05-vi.md](docs/bayesian/05-vi.md) | [bayesian/theory-advanced.md](docs/bayesian/theory-advanced.md) |
| Model comparison (WAIC / PSIS-LOO / Pseudo-BMA) | `Hanalyze.Stat.ModelSelect` | [bayesian/06-model-comparison.md](docs/bayesian/06-model-comparison.md) | [bayesian/theory-bayesian-basics.md](docs/bayesian/theory-bayesian-basics.md) |
| Posterior predictive checks; selected PyMC-style modelling features | `Hanalyze.Stat.PosteriorPredictive` | [02-pymc-comparison.md](docs/02-pymc-comparison.md) | — |
| Marginal likelihood (bridge sampling) / Bayes factors / Bayesian model averaging | `Hanalyze.Stat.{BridgeSampling,BayesFactor,BayesianModelAveraging}` | — | — |
| Bayesian A/B test (mean difference via NUTS + ROPE/HDI decision) | `Hanalyze.MCMC.BayesianTest` | — | — |
| Chain diagnostics (R̂, ESS incl. arviz-compatible `essBulk`, HDI, BFMI, rank histogram, KDE, autocorrelation) | `Hanalyze.Stat.MCMC` | [bayesian/viz-diagnostics.md](docs/bayesian/viz-diagnostics.md) | — |

### Optimisation (`Hanalyze.Optim.*`)

| Feature | Module | Usage | Theory |
|---|---|---|---|
| Single-obj (gradient): NM / L-BFGS / Brent | `Hanalyze.Optim.NelderMead`<br>`Hanalyze.Optim.LBFGS`<br>`Hanalyze.Optim.LineSearch` | [optim/01-singleobj.md](docs/optim/01-singleobj.md) | [optim/theory-singleobj.md](docs/optim/theory-singleobj.md) |
| Single-obj (evolutionary): DE / CMA-ES / SA / PSO | `Hanalyze.Optim.DifferentialEvolution`<br>`Hanalyze.Optim.CMAES`<br>`Hanalyze.Optim.SimulatedAnnealing`<br>`Hanalyze.Optim.ParticleSwarm` | [optim/01-singleobj.md](docs/optim/01-singleobj.md) | [optim/theory-singleobj.md](docs/optim/theory-singleobj.md) |
| Multi-objective (NSGA-II + Pareto) | `Hanalyze.Optim.{NSGA,Pareto}` | [optim/02-multi-objective.md](docs/optim/02-multi-objective.md) | [optim/theory-pareto-moo.md](docs/optim/theory-pareto-moo.md) |
| Acquisition functions (EHVI / ParEGO / EI / LCB / PI) | `Hanalyze.Optim.Acquisition` | [optim/02-multi-objective.md](docs/optim/02-multi-objective.md) | [optim/theory-bayesopt.md](docs/optim/theory-bayesopt.md) |
| Bayesian optimisation (BO + GP-Hedge + analytic gradient) | `Hanalyze.Optim.BayesOpt` | [optim/01-singleobj.md](docs/optim/01-singleobj.md) | [optim/theory-bayesopt.md](docs/optim/theory-bayesopt.md) |
| Algorithm selection guide | — | [optim/03-algorithm-guide.md](docs/optim/03-algorithm-guide.md) | — |

### Design of experiments (`Hanalyze.Design.*`)

| Feature | Module | Usage | Theory |
|---|---|---|---|
| DoE (Factorial / Block / Mixed / RSM / Optimal / Power / Quality) | `Hanalyze.Design.{Factorial,Block,Mixed,RSM,Optimal,Power,Quality,MultiRSM,Anova}` | [doe/01-doe.md](docs/doe/01-doe.md) | [doe/theory-doe.md](docs/doe/theory-doe.md) |
| Orthogonal arrays (L4/L8/L9/L12/L16/L18) + Taguchi (S/N + inner/outer) + process capability (Cp/Cpk) | `Hanalyze.Design.{Orthogonal,Taguchi,Quality}` | [doe/02-orthogonal-taguchi.md](docs/doe/02-orthogonal-taguchi.md) | [doe/theory-doe.md](docs/doe/theory-doe.md) |
| Custom optimal design (coordinate exchange + modified Fedorov: D/A/G/I criteria, Bayesian D, linear constraints, split-plot, augment menus, design comparison via efficiency/FDS/alias) | `Hanalyze.Design.Custom.*` | [doe/usage-custom-design.md](docs/doe/usage-custom-design.md) + [manual](docs/doe/manual-custom-design.md) | — |
| DOE workflow layer (R-style interactive `Design` object over the low-level design functions) | `Hanalyze.Design.Workflow` | [api-guide/09-doe.md](docs/api-guide/09-doe.md) | — |

### Visualisation (`Hanalyze.Viz.*`)

| Feature | Module | Usage |
|---|---|---|
| Scatter / bar / histograms / MCMC diagnostics / GP plot / Pareto plot | `Hanalyze.Viz.{Scatter,Bar,Histogram,MCMC,GP,Pareto,ModelGraph,Taguchi}` | [visualization/01-visualization.md](docs/visualization/01-visualization.md) |
| Integrated HTML report (MathJax + Mermaid + interactive) | `Hanalyze.Viz.ReportBuilder` | [visualization/02-report-builder.md](docs/visualization/02-report-builder.md) |
| Unified fit-and-plot operator `df \|-> spec` (one entry point across LM/GLM/GAM/GP/HBM/... specs) + plot-free coefficient diagnostics | `Hanalyze.Fit` / `Hanalyze.Diagnostics` | [io/04-fit-api.md](docs/io/04-fit-api.md) |
| **hgg integration** (experimental): `toPlot`/`Plottable` overlays a fitted model (LM line+CI / GP mean+credible band) on the layer grammar; `module Hanalyze` quickstart entry. Flag-gated (`plot-integration`, default off). | `Hanalyze.Plot` + `module Hanalyze` | [visualization/03-plot-integration.md](docs/visualization/03-plot-integration.md) |
| **HBM ModelGraph (3 routes)**: Mermaid HTML / Graphviz DOT / direct SVG via hgg | `Hanalyze.Viz.{ModelGraph,ModelGraphDot}` + `Hgg.Plot.Bridge.Analyze` | see "ModelGraph — 3 routes" below |

#### ModelGraph — 3 routes

There are three ways to visualise the DAG of an HBM model; pick by use case:

| Route | Module | Output / Deps | When to use |
|---|---|---|---|
| **Mermaid HTML** | `Hanalyze.Viz.ModelGraph.renderModelGraph` | `.html` + Mermaid CDN script | GitHub / GitLab READMEs, notebook attachments — auto-rendered on GitHub |
| **Graphviz DOT** | `Hanalyze.Viz.ModelGraphDot.renderModelGraphDot` | `.dot` text + `dot` CLI (install required) | graphviz ecosystem interop (xdot / gephi / `dot -Tpng`), fine-grained directives (`rank=same` / `constraint=false` etc) |
| **hgg direct** | `Hgg.Plot.Bridge.Analyze.renderModelGraphSVG` ([hgg-analyze-bridge](https://github.com/frenzieddoll/hgg)) | `.svg` (**zero deps**, pure Haskell) | production app embedding, offline batch, fast rendering of large DAGs |

All three routes take the same `Hanalyze.Model.HBM.ModelGraph` as input. Layout
quality vs dependency trade-off:

- Mermaid: lightweight, but no offline rendering
- Graphviz DOT: best layout quality, but requires the `dot` CLI
- hgg: intermediate quality (roughly 70-80% of graphviz dot, pure Haskell); the only option when zero dependencies are required

Code example (with `hgg-analyze-bridge` added as a dependency):

```haskell
import qualified Hanalyze.Viz.ModelGraph    as Mermaid
import qualified Hanalyze.Viz.ModelGraphDot as Dot
import qualified Data.Text.IO               as TIO
import           Hgg.Plot.Bridge.Analyze (renderModelGraphSVG)
import           Hanalyze.Model.HBM          (buildModelGraph)

main = do
  let mg = buildModelGraph myHBM
  Mermaid.renderModelGraph "out/dag.html" "My HBM" mg            -- Route 1
  TIO.writeFile "out/dag.dot" (Dot.renderModelGraphDot mg)       -- Route 2
  renderModelGraphSVG     "out/dag.svg"  "My HBM" mg             -- Route 3
```

Note: for standard plots, hgg also ships native PNG (Rasterific) and PDF
backends. For the ModelGraph SVG route, convert via `rsvg-convert` / `inkscape`
when PNG / PDF is needed.

### Data I/O (`Hanalyze.DataIO.*`)

| Feature | Module | Usage |
|---|---|---|
| CSV/TSV/SSV (cassava) + Parquet/JSON (Hackage `dataframe`) | `Hanalyze.DataIO.{CSV,External,Convert}` | [io/01-dirty-data.md](docs/io/01-dirty-data.md) |
| Dirty-data defence (W001-W008 warnings + auto-sniff + clean DSL) | `Hanalyze.DataIO.{Health,Sniff,Clean,Log}` | [io/01-dirty-data.md](docs/io/01-dirty-data.md) |
| Reshape (pivot_wider / one-hot / lag-lead / rolling window) | `Hanalyze.DataIO.Reshape` | [io/02-reshape.md](docs/io/02-reshape.md) |
| Preprocessing (impute / groupBy / derived columns / melt) | `Hanalyze.DataIO.Preprocess` | [io/01-dirty-data.md](docs/io/01-dirty-data.md) |
| Long-form regrid (`regridLong`) | `Hanalyze.DataIO.Preprocess` + `Hanalyze.Stat.Interpolate` | [io/03-regrid.md](docs/io/03-regrid.md) |

---

## Quick start

### 30 seconds via CLI

```bash
git clone https://github.com/frenzieddoll/hanalyze
cd hanalyze
cabal build all

# Regress sales on price + promo, write an HTML report.
hanalyze regress data/readme/sales.csv "price promo" sales --report sales.html
# β₀=185.05  β(price)=-4.37  β(promo)=+32.29  R²=0.995
```

`data/readme/sales.csv` is a 20-row demo CSV shipped with the repository
(`price`, `promo`, `sales`). The generated `sales.html` includes coefficients,
fit diagnostics, and an interactive prediction widget — straight from one
command.

### 30 seconds via Haskell API

```haskell
import qualified Hanalyze.Stat.Test as ST
import qualified Numeric.LinearAlgebra as LA

main = do
  let xs = LA.fromList [12, 14, 13, 15, 17, 11]
      ys = LA.fromList [18, 22, 20, 19, 25, 17]
      result = ST.tTestWelch xs ys ST.TwoSided
  print (ST.trPValue result, ST.trEffect result)
  -- (0.012, Just ("Cohen's d", -1.85))
```

A single `import Hanalyze` re-exports the core entry points (linear / GLM models,
descriptive stats, tests, effect sizes, distributions, plotting helpers and CSV
I/O) for quick exploration; reach for the individual `Hanalyze.Model.*` /
`Hanalyze.Stat.*` modules when you need their full surface.

See [docs/01-quickstart.md](docs/01-quickstart.md) for a fuller introduction.

---

## CLI

```
hanalyze help                     list subcommands
hanalyze regress <file> <x> <y>   LM/GLM/GP/HBM regression + HTML report
hanalyze info <file>              per-column type/statistics
hanalyze hist <file> <col>        histogram with theoretical PDF overlay
hanalyze ridge <file> ...         regularised regression (Ridge/Lasso/EN)
hanalyze kernel <file> ...        kernel regression (NW/KR/RFF), multi-D inputs
hanalyze spline <file> ...        spline regression
hanalyze multireg <file> ...      multi-output regression + interactive HTML
hanalyze melt <file> ...          long-form transform
hanalyze regrid <file> ...        time-axis grid alignment
hanalyze doe ortho <NAME> -f ...  orthogonal-array generation
hanalyze taguchi sn / analyze     Taguchi method
hanalyze clean <file> --rule ...  dirty-data cleaning
```

For per-command flags, run `hanalyze <cmd> --help` or see [docs/01-quickstart.md](docs/01-quickstart.md).

---

## Examples / demos

`demo/` contains many demos (76 as of this release). Highlights:

| Demo | Summary |
|---|---|
| `demo/regression/HBMRegressionDemo.hs` | HBM Bayesian linear regression with NUTS + HTML |
| `demo/regression/RFFDemo.hs` | Large-scale GP via Random Fourier Features |
| `demo/regression/RobustGPDemo.hs` | Robust GP with Student-t observation likelihood |
| `demo/doe-optim/NSGADemo.hs` | NSGA-II + Pareto on the ZDT suite |
| `demo/doe-optim/BayesOptDemo.hs` | BO on Branin / Hartmann6 |
| `demo/bayesian/HBMComparisonDemo.hs` | Compare HBMs with WAIC / LOO |
| `demo/bayesian/SimpsonParadoxDemo.hs` | Disentangle Simpson's paradox via hierarchical model |
| `demo/io/DirtyDataDemo.hs` | Auto-defend against 19 dirty CSV variants |

Run: `dist-newstyle/build/x86_64-linux/ghc-9.6.7/hanalyze-0.2.0.0/x/<demo-name>/build/<demo-name>/<demo-name>`.

---

## Where hanalyze fits

Rather than a complete Python/R replacement, hanalyze targets specific
workflows where Haskell integration, single-binary CLI, and tight reporting
add value.

**Strong fit**

- Haskell-native pipelines that need stats/Bayes/optim without calling out to Python
- Single-binary CLI distribution (one `hanalyze` binary, no Python venv)
- Dirty-CSV defence + cleaning + analysis in one workflow
- DoE / Taguchi / orthogonal arrays for manufacturing and process tuning
- HTML reports straight from the analysis (no separate templating step)
- Type-safe analysis pipelines that catch dtype/API mismatches early

**Not a goal — keep using existing tools for**

- Large-scale DataFrame work (pandas / polars / data.table)
- GPU deep learning (PyTorch / JAX)
- The full breadth of scikit-learn's mature model zoo
- The full Stan / PyMC MCMC diagnostics ecosystem
- The full expressive range of ggplot2

---

## Comparison vs Python

> R is included in the feature map only — no numerical bench against R has been run.

Numbers below come from `bench/results/{haskell,python}/*.csv`; see
[bench/results/SUMMARY.md](bench/results/SUMMARY.md) for the full table and
benchmark conditions (`OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1`,
single-thread, deterministic seeds).

| Domain | Result in these benchmarks |
|---|---|
| **Single-objective optim** (DE/CMAES/L-BFGS/NM) | Often faster than scipy in tested cases (Rosenbrock_2D/DE 134×, Ackley/CMAES 49×, Griewank/CMAES 54×). On Sphere_30D/L-BFGS the reported objective value is 8.1e-40 vs scipy 2.6e-11 in this run. |
| **Multi-objective optim** (NSGA-II) | Comparable or favourable in the ZDT/DTLZ suite (DTLZ2_3 1.43× faster, ZDT1/2/3 within ±5% of pymoo). HV/IGD figures match or slightly improve on pymoo in these runs. |
| **Bayesian optim** (BO) | Comparable on Branin (1.15×); on Hartmann6 the best objective in this run was -3.07 vs skopt -2.77. |
| **Simulated annealing** (Tsallis SA) | Comparable; Rastrigin_10D reaches 0.0 in this run (scipy `dual_annealing` reports 7.8e-14). |
| **Classical regression** (LM/Ridge/Lasso/GLMM) | Comparable in tested cases; LME 30× faster than statsmodels in our LME run. |
| **Large-scale GLM/Lasso** (n ≥ 10k) | Currently slower than sklearn (3-5× in tested cases) — sklearn's Cython inner loops dominate. |
| **Kernel/GP** | Currently slower than sklearn (2.5-4.7× in tested cases). |
| **Bayesian MCMC** (NUTS/HMC) | NUTS with ESS comparable to blackjax (mu: 839 vs 810) on the 8-schools benchmark; 7.4× faster than PyMC; 2.8× slower than blackjax (JAX-JIT advantage). |
| **HBM (probabilistic programming)** | Polymorphic DSL with selected PyMC-style modelling features and selected distributions (Truncated/Censored/MvNormal/LKJ/...). |
| **VI / WAIC / LOO** | ADVI 3.0× faster than numpyro SVI on a small logistic posterior; LOO 2.9× faster than arviz on (S=1000, N=200) log-lik matrix. |
| **Hypothesis tests / bootstrap / k-fold** | Welch t-test 39× faster, KS 11×, k-fold split 2.2× faster than scipy/sklearn in tested cases. |
| **Time series / Spline / GAM** | ARIMA 128× faster than statsmodels; Spline PCHIP comparable to scipy; GAM ~1.6× slower than pygam in tested cases. |
| **Survival analysis** (KM/Cox PH) | Comparable to lifelines in tested cases (KM/CoxPH). |
| **Multi-output regression / Regrid** | MultiLM 2.3× faster than sklearn; `regridLong` 20× faster than a hand-written pandas+scipy synthesis. |
| **Visualisation** | Vega-Lite specs via hvega (grammar-of-graphics-style); HTML reports built-in. |

See [docs/comparison/python-r.md](docs/comparison/python-r.md) for the feature map, and [bench/results/SUMMARY.md](bench/results/SUMMARY.md) for numbers.

---

## Benchmark highlights

Selected results from `bench/results/SUMMARY.md`. Each entry is a single
benchmark configuration; absolute objective values depend on iteration
counts, seeds, and tolerances — see the SUMMARY for full conditions.
NUTS is additionally validated against posteriordb reference posteriors
(see [bench/posteriordb/](bench/posteriordb/)).

- **NUTS 8-schools** (warmup 500, samples 1000): hanalyze 1492 ms with ESS(mu) 839 vs blackjax 530 ms / ESS 810 in this run
- **Holt-Winters seasonal n=500 p=12**: hanalyze 0.19 ms vs statsmodels MLE 96 ms in this run (note: hanalyze uses fixed α=0.3 closed-form; statsmodels does MLE)
- **Sphere_30D/DE**: hanalyze 1.0e-26 vs scipy 2.8e-5 on this benchmark
- **Sphere_30D/L-BFGS**: hanalyze 8.1e-40 vs scipy 2.6e-11 on this benchmark
- **Rastrigin_10D/SA**: hanalyze 0.0 vs scipy `dual_annealing` 7.8e-14 in this run
- **Hartmann6/BO**: hanalyze -3.07 vs skopt -2.77 in this run
- **DTLZ2_3/NSGA-II**: hanalyze 528 ms vs pymoo 758 ms (1.43× faster in this run)
- **DE Rosenbrock_2D**: hanalyze 1.2 ms vs scipy 164 ms (134× faster in this run)
- **Constrained Quad2D (eq)**: hanalyze 0.062 ms vs scipy SLSQP 0.69 ms in this run
- **regridLong on jagged long-form**: hanalyze 0.99 ms vs pandas+scipy synthesis 19.4 ms in this run

Reproduce: `OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 cabal run bench-{regression,kernel,optim,mo,bo,mcmc-b7,mcmc-extras,ts-extras,optim-plus,stat-util,multi-output,regrid}`, then `bench/python/bench_*.py` (see [bench/README.md](bench/README.md)).

---

## Architecture

```mermaid
graph TD
  IO[DataIO.* CSV/Parquet/JSON]
  IO --> DF[Hackage dataframe]
  DF --> Models[Model.* regression/ML/Bayesian/TS/Survival]
  DF --> Stat[Stat.* tests/CV/effect/interpret]
  Models --> Optim[Optim.* optimisation]
  Models --> MCMC[MCMC.* samplers]
  Models --> Viz[Viz.* HTML/PNG/SVG]
  Stat --> Viz
  MCMC --> Viz
  Optim --> Design[Design.* DoE/Taguchi]
```

**All modules talk to Hackage `dataframe` directly**. The internal `DataFrame.Core` was retired.

---

## Roadmap & API stability

- **Stable** (API expected to remain backward-compatible within minor versions): `Hanalyze.DataIO.*`, `Hanalyze.Stat.{Test, Bootstrap, MultipleTesting, ClassMetrics, CV, Effect, Distribution}`, `Hanalyze.Model.{LM, GLM, Spline, Regularized, RandomForest, DecisionTree, TimeSeries, Survival, GAM}`, `Hanalyze.Optim.{NelderMead, LBFGS, DifferentialEvolution, CMAES, NSGA, BayesOpt, SimulatedAnnealing, ParticleSwarm}`, `Hanalyze.Design.*`, `Hanalyze.Viz.{Scatter, Bar, Histogram}`.
- **Experimental** (API may evolve): `Hanalyze.Model.HBM` DSL, `Hanalyze.MCMC.NUTS` (mass-matrix adaptation is opt-in), `Hanalyze.Stat.VI` (ADVI), `Hanalyze.Model.{GP, RFF, GPRobust, GLMM}`, `Hanalyze.Model.{SVM, GradientBoosting, NeuralNetwork}`, `Hanalyze.Model.LiNGAM.*`, `Hanalyze.Design.Custom.*`, the `df |-> spec` fit operator (`Hanalyze.Fit`), the hgg integration (`plot-integration` flag), `Hanalyze.Viz.ReportBuilder`. Behaviour is benchmarked but type signatures may shift.
- **Future direction**: a backend-abstraction typeclass for swapping hmatrix/Massiv/Accelerate is under consideration but not on a fixed schedule. (The unified top-level re-export layer and the fit-operator API planned earlier landed in 0.2.0.0 as `module Hanalyze` and `Hanalyze.Fit`.)

---

## Module layout

```
src/
  DataIO/      — CSV/JSON/Parquet IO + health checks + sniff + clean DSL + reshape (9 mods)
  Stat/        — tests/distributions/effect/CV/bootstrap/interpret/causal/MCMC diagnostics (33 mods)
  Model/       — LM/GLM/GLMM/GP/HBM/SVM/GBM/NN/Cluster/TS/Survival/LiNGAM/FDA etc. (75 mods)
  Optim/       — single-obj (NM/LBFGS/DE/CMAES/SA/PSO) + multi-obj (NSGA/BO/Pareto) (18 mods)
  Design/      — Factorial/Block/RSM/Orthogonal/Taguchi + Custom optimal design (30 mods)
  Viz/         — Vega-Lite-based visualisation + ReportBuilder (19 mods)
  MCMC/        — MH/HMC/NUTS/Gibbs/Slice/SMC + progress (9 mods)
  Math/ Data/ Plot/ + Fit/Diagnostics — numeric kernels, data helpers, hgg integration, fit operator
```

As of this release: 212 modules, ~1,390 test examples.

---

## Build

```bash
cabal build all                  # library + all executables (76 demos)
cabal test                       # hspec test suite
cabal repl                       # interactive REPL
```

Major dependencies: `hmatrix` (BLAS/LAPACK), `hvega` (Vega-Lite), `statistics`, `mwc-random`, `dataframe` (Hackage Polars-like), `massiv` (parallel arrays), `ad` (auto-diff), `async`.

Tested on GHC 9.6.7 + cabal 3.14.2.

---

## Running benchmarks

```bash
# 1. Generate shared test data (fixed-seed, deterministic)
cabal run bench-data-gen

# 2. Haskell side
OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 \
  cabal run bench-regression bench-kernel bench-optim bench-mo bench-bo

# 3. Python side (need bench/venv from bench/requirements.txt)
OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 \
  bench/venv/bin/python bench/python/bench_regression.py
# (similarly for kernel, optim, mo, bo)

# 4. Aggregate (Markdown table)
bench/venv/bin/python bench/aggregate.py > bench/results/SUMMARY.md
```

---

## Development

- **Issues / PRs**: [github.com/frenzieddoll/hanalyze](https://github.com/frenzieddoll/hanalyze)
- **Adding tests**: append hspec specs in `test/Spec.hs`
- **Adding benchmarks**: place `bench/haskell/Bench*.hs` and matching Python script
- **Coding rules**: see `CONTRIBUTING.md` (no list-passing on hot paths, minimise `unsafe*`, ...)

---

## License

BSD-3-Clause License — see [LICENSE](LICENSE).

## Author

Toshiaki Honda <frenzieddoll@gmail.com>
