# hanalyze-models

The **model layer** of [`hanalyze`](../README.md). It owns the
**model zoo** — everything from classical regression through machine
learning, multivariate analysis, time series, survival/reliability, and
causal inference. It is the largest of the six split layers (67 modules).

It depends on the three layers `core` (numerical foundation) / `frame`
(dataframe interop) / `bayes` (MCMC), plus 12 external packages. `-design`
(DoE) and `-viz` sit on top of this layer. It is the only layer that depends
on `megaparsec`, since it owns the Formula DSL parser.

## Main modules (67 in total)

### Basic regression (`Hanalyze.Model.*`)

| Module | Role |
|---|---|
| `Model.LM` / `Model.LM.Diagnostics` | Least-squares regression and residual diagnostics (leverage / Cook's distance / VIF) |
| `Model.GLM` / `Model.GLMM` | GLM by IRLS (unifying Gaussian / Binomial / Poisson) and mixed-effects GLM |
| `Model.MultiLM` | Multi-output linear regression |
| `Model.Regularized` / `Model.RegularizedAdvanced` | Lasso / Ridge / Elastic Net + MCP / SCAD / Adaptive / Group. λ is auto-selected via k-fold CV plus the 1-SE rule |
| `Model.Robust` / `Model.Quantile` | Robust regression via M-estimation / quantile regression |
| `Model.Spline` / `Model.GAM` | Spline smoothing / generalized additive models |

### Formula DSL (`Model.Formula.*`)

| Module | Role |
|---|---|
| `Model.Formula` | Parser and AST for the **canonical Formula DSL front-end** (`y ~ x1 + x2*x3`) |
| `Model.Formula.Design` / `.Frame` | AST → model matrix, and joining with a dataframe |
| `Model.Formula.Mixed` / `.Nonlinear` / `.RFormula` | Interpretation of mixed-effects / nonlinear terms / R-compatible formulas |

### Multivariate & dimensionality reduction

| Module | Role |
|---|---|
| `Model.PCA` | Principal component analysis (variance maximization, no response) |
| `Model.PLS` | Partial least squares — maximizes covariance with the response, plus VIP and CV-based component selection |
| `Model.Discriminant` | Discriminant analysis (LDA = linear boundary / QDA = quadratic boundary) |
| `Model.Multivariate` | The RRR / PLS / CCA family of multivariate regression |
| `Model.MDS` / `Model.FDA` | Multidimensional scaling / functional data analysis |
| `Model.Cluster` / `Model.HierarchicalCluster` / `Model.LatentClassAnalysis` | k-means / hierarchical clustering / latent class analysis |

### Machine learning

| Module | Role |
|---|---|
| `Model.RandomForest` / `Model.RandomForestClassifier` | Random forest (regression / classification) |
| `Model.DecisionTree` / `Model.GradientBoosting` | Decision trees / gradient boosting |
| `Model.SVM` / `Model.KNN` / `Model.NaiveBayes` / `Model.NeuralNetwork` | SVM / k-nearest neighbors / naive Bayes / NN |
| `Model.Kernel` / `Model.KernelRegression` | Kernel function family / kernel regression |
| `Model.PartialDependence` | Model interpretation via PDP / ICE |

### Gaussian processes & multi-output

| Module | Role |
|---|---|
| `Model.GP` / `Model.GPRobust` | GP regression (RBF / Matérn / Periodic + ARD) / outlier-robust GP |
| `Model.MultiGP` / `Model.MultiOutput` | Multi-output GP / a unified API for multi-output regression |
| `Model.RFF` | Large-scale GP approximation via Random Fourier Features |

### Time series

| Module | Role |
|---|---|
| `Model.TimeSeries` | Entry point for the ARIMA family |
| `Model.VAR` / `Model.GARCH` / `Model.StateSpace` | Vector autoregression / GARCH / state-space models (Kalman filter) |

### Survival & reliability

| Module | Role |
|---|---|
| `Model.Survival` | Kaplan-Meier / Cox proportional hazards |
| `Model.AFT` / `Model.CompetingRisks` | Accelerated failure time models / competing risks (CIF) |
| `Model.Weibull` | Weibull MLE (with censoring support) + B_p life + Wald confidence intervals |
| `Model.Reliability` | Accelerated life testing (Arrhenius / Eyring / Inverse Power Law) |
| `Model.ReliabilityBlockDiagram` | System reliability for series / parallel / k-of-n configurations |

### Causal inference

| Module | Role |
|---|---|
| `Model.LiNGAM.Direct` / `.ICA` / `.Pairwise` / `.Parce` | Structure estimation using non-Gaussianity (DirectLiNGAM / ICA-LiNGAM, etc.) |
| `Model.LiNGAM.VAR` / `.Bootstrap` / `.MultiGroup` | Time-series variant / bootstrap confidence / simultaneous multi-group estimation |
| `Model.DAG` | DAG representation and search |
| `Stat.Causal.PropensityScore` / `.IPW` / `.DoublyRobust` / `.CATE` | Propensity score / IPW / doubly robust estimation / conditional average treatment effect |

### Miscellaneous

| Module | Role |
|---|---|
| `Model.FitYByX` | Equivalent of JMP's "Fit Y by X" — auto-selects a method from the combination of variable types |
| `Stat.ModelSelect` | Model selection via AIC / BIC |
| `Optim.BayesOpt` | Bayesian optimization (GP + acquisition function). Placed in this layer because it uses GP |

## Using it standalone

If you do not need the umbrella package, you can depend on this package
directly. Since the `FitResult` result type lives in the core layer, **you
also need `hanalyze-core` explicitly**:

```cabal
build-depends: hanalyze-models, hanalyze-core, hmatrix
```

```haskell
import qualified Numeric.LinearAlgebra as LA
import Hanalyze.Model.Core (FitResult (..))
import Hanalyze.Model.LM   (fitLM)

main :: IO ()
main = do
  let x = LA.fromLists [ [1, 1.0], [1, 2.0], [1, 3.0], [1, 4.0], [1, 5.0] ]
      y = LA.fromLists [ [2.1], [3.9], [6.2], [7.8], [10.1] ]
      fit = fitLM x y
  print (LA.toLists (coefficients fit))
  print (LA.toList (rSquared fit))
  -- [[5.000000000000132e-2],[1.9899999999999998]]
  -- [0.9973053289009771]
```

You must add the intercept column (`1`) yourself. To build a model matrix
from a formula, use `Model.Formula`.

Normally you would just depend on the umbrella package `hanalyze` and
get all of the above from a single `import Hanalyze`. Naming a layer
directly is only worth it when you want to minimize dependencies.

## Related docs

- Linear regression: [docs/regression/01-lm.md](../docs/regression/01-lm.md) /
  GLM: [02-glm.md](../docs/regression/02-glm.md)
- Regularized regression: [04-regularized.md](../docs/regression/04-regularized.md) /
  [usage-regularized-advanced.md](../docs/regression/usage-regularized-advanced.md)
- PLS: [usage-pls.md](../docs/regression/usage-pls.md) /
  Discriminant analysis (LDA/QDA): [usage-discriminant.md](../docs/regression/usage-discriminant.md)
- Weibull / B_p life: [usage-weibull.md](../docs/regression/usage-weibull.md) /
  Accelerated life testing: [usage-reliability.md](../docs/regression/usage-reliability.md)
- Survival analysis: [10-survival.md](../docs/regression/10-survival.md) /
  Multi-output: [05-multivariate.md](../docs/regression/05-multivariate.md)
- Formula DSL: [11-formula-dsl.md](../docs/regression/11-formula-dsl.md)
- Causal inference: [docs/causal/](../docs/causal/) /
  Bayesian optimization: [docs/optim/01-singleobj.md](../docs/optim/01-singleobj.md)

← [repository README](../README.md)
