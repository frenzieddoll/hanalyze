# Probabilistic programming DSL (Model.HBM)

> 🌐 **English** | [日本語](02-probabilistic-model.ja.md)

> Related demos:
> - [`hbm-example`](../demo/HBMExample.hs) — hierarchical normal model + 4-chain NUTS
> - [`hbm-regression`](../demo/HBMRegressionDemo.hs) — Bayesian simple regression (with AnalysisReport)
> - [`clinical-trial`](../demo/ClinicalTrial.hs) — Beta-Binomial A/B test
> - [`simpson-paradox`](../demo/SimpsonParadoxDemo.hs) — LM/GLMM/HBM compared on Simpson's example
> - [`hbm-random-slope`](../demo/HBMRandomSlopeDemo.hs) — random-slope extension

## Overview

`Model.HBM` is a polymorphic probabilistic programming DSL implemented as a
free monad. Models can be written declaratively, similar to Stan or PyMC.

The continuation is polymorphic as `forall a. (Floating a, Ord a) => Model a r`,
so a single model definition supports **four interpretations**:

| Interpretation | Specialization | Use |
|---|---|---|
| Structural inspection | `a = Double` | `collectNodes`, `describeModel` |
| log joint evaluation | `a = Double` | `logJoint`, `logPrior`, `logLikelihood` |
| AD gradient | `a = Forward Double` | `gradAD`, `gradADU` (machine-epsilon precision) |
| Dependency tracking | `a = Track` | `extractDeps`, `buildModelGraph` (auto DAG) |

The samplers (`MCMC.HMC` / `NUTS` / `Gibbs`) leverage AD gradients and
automatic constraint transforms.

---

## Basic API

```haskell
import Model.HBM     -- exports Distribution(..), sample, observe

-- Type alias for the polymorphic model
type ModelP r = forall a. (Floating a, Ord a) => Model a r

-- Declare a latent variable (return value is `a`, used in subsequent sample/observe)
sample  :: Text -> Distribution a -> Model a a

-- Condition on observed data (assumed i.i.d.)
observe :: Text -> Distribution a -> [Double] -> Model a ()
```

The return value of `sample` has type `a` (polymorphic) and can flow into
the distribution parameters of subsequent `sample`/`observe` calls
(equivalent to Stan's `~` syntax).

> **Note**: `ModelP` is a rank-2 type, so a local binding like
> `let m = schoolModel dat` runs into monomorphization issues. Use a
> top-level binding (`m :: ModelP () ; m = schoolModel dat`) or inline the
> call site.

---

## Available distributions

```haskell
data Distribution a
  = Normal      a a       -- Normal(μ, σ)    — continuous, ℝ
  | Binomial    Int a     -- Binomial(n, p)  — discrete, [0, n]
  | Poisson     a         -- Poisson(λ)      — discrete, non-negative integers
  | Exponential a         -- Exponential(λ)  — continuous, positive
  | Gamma       a a       -- Gamma(α, β)     — continuous, positive (rate=β)
  | Beta        a a       -- Beta(α, β)      — continuous, (0,1)
```

Distribution parameters are polymorphic in `a`, so values from another
`sample` can be passed in directly (e.g., `Normal mu sigma` with
`mu, sigma :: a`).

HMC/NUTS automatically map constrained distributions
(Exponential/Gamma → positive, Beta → unit interval) into the unconstrained
space for sampling.

---

## Pattern 1: simple normal model

```haskell
-- μ ~ Normal(0, 10)
-- y_i ~ Normal(μ, σ=2)  (σ known)
normalMean :: [Double] -> ModelP ()
normalMean ys = do
  mu <- sample "mu" (Normal 0 10)
  observe "y" (Normal mu 2) ys
```

---

## Pattern 2: constrained parameter (σ unknown)

```haskell
-- μ ~ Normal(0, 10)
-- σ ~ Exponential(1)   ← HMC/NUTS uses log transform to enforce positivity
-- y_i ~ Normal(μ, σ)
normalUnknownSigma :: [Double] -> ModelP ()
normalUnknownSigma ys = do
  mu    <- sample "mu"    (Normal 0 10)
  sigma <- sample "sigma" (Exponential 1)
  observe "y" (Normal mu sigma) ys
```

---

## Pattern 3: A/B test (Beta-Binomial)

```haskell
-- p_ctrl ~ Beta(1,1),  y_ctrl ~ Binomial(50, p_ctrl), recover k_ctrl=18
-- p_trt  ~ Beta(1,1),  y_trt  ~ Binomial(50, p_trt),  recover k_trt =31
clinicalModel :: ModelP ()
clinicalModel = do
  pCtrl <- sample "p_ctrl" (Beta 1 1)
  pTrt  <- sample "p_trt"  (Beta 1 1)
  observe "y_ctrl" (Binomial 50 pCtrl) [18]
  observe "y_trt"  (Binomial 50 pTrt)  [31]
```

---

## Pattern 4: hierarchical normal model (3 schools)

The do-notation return values let lower-level distribution parameters
inherit upper-level draws.

```haskell
import Control.Monad (forM_)
import qualified Data.Text as T

-- μ ~ Normal(0, 100)
-- τ ~ Exponential(0.1)
-- θ_j ~ Normal(μ, τ)
-- y_ij ~ Normal(θ_j, 5)
schoolModel :: [[Double]] -> ModelP ()
schoolModel groupData = do
  mu  <- sample "mu"  (Normal 0 100)
  tau <- sample "tau" (Exponential 0.1)
  forM_ (zip [1::Int ..] groupData) $ \(j, ys) -> do
    theta <- sample (T.pack ("theta_" ++ show j)) (Normal mu tau)
    observe (T.pack ("y_" ++ show j)) (Normal theta 5) ys

schoolData :: [[Double]]
schoolData =
  [ [72, 68, 75, 71]   -- school 1
  , [85, 88, 82, 90]   -- school 2
  , [61, 65, 58, 63]   -- school 3
  ]
```

---

## Inspecting model structure

```haskell
-- Get the list of latent variable names
sampleNames :: ModelP r -> [Text]
sampleNames (schoolModel schoolData)
-- ["mu","tau","theta_1","theta_2","theta_3"]

-- Evaluate log densities (for sampler debugging)
logJoint      :: ModelP r -> Params -> Double  -- log p(θ, y)
logPrior      :: ModelP r -> Params -> Double  -- log p(θ)
logLikelihood :: ModelP r -> Params -> Double  -- log p(y | θ)
```

```haskell
import qualified Data.Map.Strict as Map
let ps = Map.fromList [("mu",73),("tau",10),
                       ("theta_1",71.5),("theta_2",86.25),("theta_3",61.75)]
logJoint (schoolModel schoolData) ps  -- ≈ -52.4
```

---

## Auto-extracted model graph

Renders a Mermaid.js DAG into HTML.
Dependencies are **auto-extracted** by AD-style propagation through the
`Track` type, so no edges need to be written by hand.

```haskell
import Model.HBM      (buildModelGraph, extractDeps)
import Viz.ModelGraph (renderModelGraph)

-- Auto-build the dependency graph (the DSL's Track type propagates parents per node)
let graph = buildModelGraph (schoolModel schoolData)
renderModelGraph "model.html" "School Model" graph
-- Open in a browser to see the DAG

-- Node-level dependency extraction is also available
extractDeps (schoolModel schoolData)
-- [Node "mu"      LatentN "Normal"      {}
-- ,Node "tau"     LatentN "Exponential" {}
-- ,Node "theta_1" LatentN "Normal"      {"mu","tau"}    -- depends on mu, tau
-- ,Node "y_1"     (ObservedN 4) "Normal" {"theta_1"}    -- depends on theta_1
-- ,...]
```

Passing this `ModelGraph` to `reportGraph` of `Viz.Report.MCMCReport`
embeds the DAG inside the MCMC report HTML.

---

## Per-observation log-likelihood

Used internally by WAIC / LOO computation (`Stat.ModelSelect`), but also
callable directly for debugging.

```haskell
perObsLogLiks :: ModelP r -> Params -> [Double]
-- Returns logDensity for each observation of each observe node, flattened
```

```haskell
perObsLogLiks (schoolModel schoolData) ps
-- [-2.1, -2.3, -1.8, -2.0, ...]  (one entry per observation)
```

---

## AD gradients (machine-epsilon precision)

Exact gradients via `Numeric.AD.Mode.Forward`.
HMC/NUTS use this internally, so end-users typically don't call it directly.

```haskell
gradAD  :: ModelP r -> [Text] -> [Double] -> [Double]
gradADU :: ModelP r -> [Text] -> [Transform] -> [Double] -> [Double]  -- with constraint transforms

-- Evaluate ∂log p(θ,y) / ∂θ at θ=(1.5, 1.2)
let g = gradAD (normalUnknownSigma obs) ["mu", "sigma"] [1.5, 1.2]
-- Compared to numeric differentiation (central diff h=1e-5), the relative error is ~10⁻¹⁰
```

`gradADU` returns the gradient in the unconstrained space using constraint
transforms (`PositiveT`/`UnitIntervalT`) detected from the priors (used
internally by HMC/NUTS).

---

## How polymorphic interpretation works

Thanks to the rank-2 type `type ModelP r = forall a. (Floating a, Ord a) => Model a r`,
specializing the same model definition at different `a` produces multiple
interpretations:

```haskell
-- a = Double           → numeric log joint evaluation
logJoint myModel ps :: Double

-- a = Forward s Double → AD gradient
gradAD myModel names xs :: [Double]

-- a = Track            → automatic dependency graph extraction
extractDeps myModel :: [Node]

-- a = Double (placeholder)
collectNodes myModel  :: [Node]    -- structure only (no dependency info)
```

The `Track` type has a `Floating` instance that propagates a dependency
set `Set Text` through every arithmetic operation. Building
`Normal mu sigma` automatically records "this distribution depends on
`mu` and `sigma`", so `buildModelGraph` can construct the edges
automatically.
