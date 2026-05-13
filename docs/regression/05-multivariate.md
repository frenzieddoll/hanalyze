# Multi-output models — usage

> 🌐 **English** | [日本語](05-multivariate.ja.md)

> Multi-response models for joint estimation and multi-objective optimisation.
> Theory: [docs/regression/theory-multivariate.md](theory-multivariate.md).

## Design policy

All major regression models in hanalyze follow a single unified policy:
**multi-output (Y :: Matrix n×q) is the primary API; single-output is a thin
wrapper that lifts to a 1-column matrix and delegates.**

- Common base: `Hanalyze.Model.MultiOutput` (`asMultiY` / `fromMultiY` / `r2Multi` / `rmseMulti` / `mseMulti`)
- Each model exposes a `fitXMulti` / `XFitMulti` family
- q=1 numerically matches the legacy single-output API (verified by 10 hspec tests)

## Module quick reference

| Module | Single-output | Multi-output (primary) |
|---|---|---|
| `Hanalyze.Model.LM`           | `fitLMVec`            | `fitLM` (multi-output from the start) |
| `Hanalyze.Model.MultiLM`      | —                     | `fitMultiLM` + residual covariance Σ |
| `Hanalyze.Model.GLM`          | `fitGLM`              | `fitGLMMulti` (per-column IRLS) |
| `Hanalyze.Model.GLMM`         | `fitLME` / `fitGLMM`  | `fitLMEMulti` / `fitGLMMMulti` |
| `Hanalyze.Model.Spline`       | `fitSpline`           | `fitSplineMulti` (shared basis) |
| `Hanalyze.Model.Kernel`       | `kernelRidge` / `nwRegression` | `kernelRidgeMulti` / `nwRegressionMulti` |
| `Hanalyze.Model.Regularized`  | `fitRegularized`      | `fitRegularizedMulti` (Ridge/OLS via closed-form matrix solve) |
| `Hanalyze.Model.RFF`          | `rffRidge` / `rffRidgeMV` | `rffRidgeMulti` / `rffRidgeMVMulti` |
| `Hanalyze.Model.GP`           | `fitGP`               | `fitGPMulti` (shared Ky⁻¹ and shared variance) |
| `Hanalyze.Model.GPRobust`     | `fitGPRobust`         | `fitGPRobustMulti` (per-column IRLS, shared K) |
| `Hanalyze.Model.MultiGP`      | —                     | `fitMultiGP` / `fitMultiGPMV` (shared HP, RBF, default) + `fitMultiGPIndep` / `fitMultiGPMVIndep` (per-output independent HPs) |
| `Hanalyze.Model.Multivariate` | —                     | RRR / PLS / CCA |
| `Hanalyze.Model.HBM`          | `observe` (1 col)     | `observeColumns` helper + `MvNormal` observation |

---

## 1. Multivariate Linear Regression (`Hanalyze.Model.MultiLM`)

When you also want the residual covariance Σ:

```haskell
import Model.MultiLM

let mf    = fitMultiLM xMat yMat   -- X (n×p), Y (n×q)
let yPred = predictMultiLM mf xNewMat
let sigma = mfResidCov mf          -- residual covariance q×q
let corr  = mfResidCor mf          -- residual correlation q×q
```

API:
- `fitMultiLM :: Matrix -> Matrix -> MultiFit`
- `predictMultiLM :: MultiFit -> Matrix -> Matrix`
- `mfResidCov`, `mfResidCor`: residual covariance / correlation

---

## 2. Multi-output Spline / Kernel / Regularized / RFF

All use a single matrix-form solve (Ridge/OLS) or shared computation +
per-column iteration (Lasso/EN/IRLS):

```haskell
import Model.Spline      (fitSplineMulti, predictSplineMulti, BSpline (..))
import Model.Kernel      (kernelRidgeMulti, predictKernelRidgeMulti
                         , autoTuneKernelRidgeMulti, defaultHGrid, defaultLamGrid)
import Model.Regularized (fitRegularizedMulti, predictRegularizedMulti, Penalty (..))
import Model.RFF         (rffRidgeMulti, predictRFFRidgeMulti, sampleRFFRBF)

-- Spline (q outputs at once)
let sf    = fitSplineMulti (BSpline 3) knots xs yMat
let yPred = predictSplineMulti sf xsNew

-- Kernel Ridge (one Cholesky for q outputs)
let kr    = kernelRidgeMulti Gaussian 0.1 0.05 xs yMat
let yPred = predictKernelRidgeMulti kr xsNew

-- LOOCV-analytic auto-tuning of h and λ
let (krBest, hStar, lamStar, looMSE) =
      autoTuneKernelRidgeMulti Gaussian xs yMat
        (defaultHGrid xs) defaultLamGrid

-- Regularized (Ridge: closed form B = (X'X+λI)⁻¹ X'Y)
let rf    = fitRegularizedMulti (L2 0.1) xMat yMat
let yPred = predictRegularizedMulti rf xNewMat

-- RFF Ridge (1D input)
gen <- createSystemRandom
rff <- sampleRFFRBF 100 1.0 1.0 gen
let mf    = rffRidgeMulti rff xList yMat 0.01
let yPred = predictRFFRidgeMulti mf xListNew
```

---

## 3. RRR / PLS / CCA (`Hanalyze.Model.Multivariate`)

```haskell
import Model.Multivariate

-- Reduced-rank regression
let rrr = reducedRankRegression r xMat yMat
let yPred = predictRRR rrr xNew

-- PLS (k components)
let pls' = pls k xMat yMat
let yPred = predictPLS pls' xNew

-- CCA
let ccaFit = cca xMat yMat
-- ccaCorr ccaFit: canonical correlations
-- ccaA, ccaB:    per-side bases (n × r)
```

---

## 4. Multi-output GP

Three flavours, distinguished by how hyperparameters are shared across
outputs:

| API | HP optimisations | Kernels | Use case |
|---|---|---|---|
| `GP.fitGPMulti` / `GP.fitGPMVMulti` | 0 (HPs supplied) | any | HPs are already chosen; reuse Cholesky to predict q outputs |
| `MultiGP.fitMultiGP` / `fitMultiGPMV` (★ default) | **1** (pooled marginal lik + shared Cholesky) | RBF only | sklearn-style: homogeneous q outputs. ~q× faster than per-output for @q > 1@ |
| `MultiGP.fitMultiGPIndep` / `fitMultiGPMVIndep` | **q** (each output independent) | any (RBF / Matérn52 / Periodic) | Outputs need distinct length-scales or noise |

None of these model cross-output correlations (Co-kriging / LMC);
@B = I@ in the ICM sense.

### Shared HP (default, RBF, `Hanalyze.Model.MultiGP.fitMultiGP`)
A single HP set is learnt by maximising the pooled marginal likelihood
@Σ_q log p(y_q | θ)@, then one Cholesky factor of @Ky = K + σ_n² I@
is reused for every output's posterior solve. Mirrors sklearn's
@GaussianProcessRegressor.fit(X, Y::(n,q))@:

```haskell
import Hanalyze.Model.MultiGP

let res = fitMultiGP trainX trainYs testX
-- mgpMean, mgpLower, mgpUpper, mgpModels (all q models identical)
```

Multi-input form (X is @n × p@):

```haskell
let resMV = fitMultiGPMV trainX trainYs testX
-- mgpmvMean :: [Vector Double], mgpmvModels :: [GPModel] (all identical)
```

### Cholesky-only sharing (HPs known, `Hanalyze.Model.GP.fitGPMulti`)
When HPs are already fixed externally (e.g. inside an MCMC step):

```haskell
import qualified Hanalyze.Model.GP as GP

let model = GP.GPModel GP.RBF GP.defaultGPParams
let (meanMat, varList) = GP.fitGPMulti model trainX trainYMat testX
-- meanMat :: Matrix (m × q)
-- varList :: [Double] (length m, shared across q outputs)
```

### Per-output independent HPs (any kernel, `Hanalyze.Model.MultiGP.fitMultiGPIndep`)
Use this when each output needs its own length-scale / noise, or when
the kernel is Matérn 5/2 or Periodic:

```haskell
import Hanalyze.Model.MultiGP

let res = fitMultiGPIndep RBF trainX trainYs testX
-- mgpModels has a distinct GPParams for each output
```

---

## 5. Robust multi-output GP (`Hanalyze.Model.GPRobust`)

StudentT / Cauchy observation likelihood for outlier robustness, per-column IRLS:

```haskell
import qualified Model.GP as GP
import qualified Model.GPRobust as GPR

let mf = GPR.fitGPRobustMulti GP.RBF GP.defaultGPParams
                              (GPR.RStudentT 4 0.3) trainX trainYMat
let (meanMat, varList) = GPR.predictGPRobustMulti mf testX
```

---

## 6. CLI: `hanalyze multireg`

Wide CSV (1 row = one input value, columns = output tasks) goes straight in:

```bash
hanalyze multireg data.csv x 'y_*' \
    --method kernel-rbf --auto-hp \
    --xaxis 'z [nm]' --xaxis-min 0 --xaxis-max 200 \
    --report out.html
```

Produces an interactive HTML where one input slider recomputes all q
predictions live in the browser. Details: [07-multireg.md](07-multireg.md).

---

## 7. Demos

```bash
cabal run multilm-demo                  # B̂ ≈ B, Σ̂ ≈ Σ_true
cabal run multivariate-demo             # RRR/PLS/CCA recover a rank-1 structure
cabal run potential-multiout-demo       # multi-output OLS, interactive HTML
cabal run potential-multikr-demo        # multi-output RBF Kernel Ridge + LOOCV auto-HP
```
