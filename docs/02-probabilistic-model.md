# Probabilistic Programming DSL (Model.HBM)

> 🌐 **English** | [日本語](02-probabilistic-model.ja.md)

> Related demos:
> - [`hbm-example`](../demo/HBMExample.hs) — hierarchical normal model + 4-chain NUTS
> - [`hbm-regression`](../demo/HBMRegressionDemo.hs) — Bayesian linear regression with AnalysisReport
> - [`clinical-trial`](../demo/ClinicalTrial.hs) — Beta-Binomial A/B test
> - [`simpson-paradox`](../demo/SimpsonParadoxDemo.hs) — LM/GLMM/HBM compared on Simpson's paradox
> - [`hbm-random-slope`](../demo/HBMRandomSlopeDemo.hs) — random slope extension

## Overview

`Model.HBM` is a polymorphic probabilistic-programming DSL implemented as a free monad,
letting you write models declaratively in the style of Stan or PyMC.

Continuations are polymorphic in `forall a. (Floating a, Ord a) => Model a r`, so a single
model definition supports **four interpretations**:

| Interpretation | Specialization | Use |
|---|---|---|
| Structure inspection | `a = Double` | `collectNodes`, `describeModel` |
| Log-joint evaluation | `a = Double` | `logJoint`, `logPrior`, `logLikelihood` |
| AD gradient | `a = Forward Double` | `gradAD`, `gradADU` (machine-epsilon precision) |
| Dependency tracking | `a = Track` | `extractDeps`, `buildModelGraph` (automatic DAG extraction) |

Samplers (`MCMC.HMC`/`NUTS`/`Gibbs`) leverage AD gradients and automatic constraint transforms.

---

## Core API

```haskell
import Model.HBM     -- exports Distribution(..), sample, observe

-- Polymorphic model alias
type ModelP r = forall a. (Floating a, Ord a) => Model a r

-- Latent variable (the returned `a` flows into subsequent sample/observe)
sample  :: Text -> Distribution a -> Model a a

-- Condition on observed data (i.i.d. assumption)
observe :: Text -> Distribution a -> [Double] -> Model a ()
```

The return value of `sample` is polymorphic `a` — pass it directly into downstream
`sample` / `observe` distributions (the equivalent of Stan's `~`).

> **Note**: `ModelP` is a rank-2 type, so `let m = schoolModel dat` causes a
> monomorphization issue. Use a top-level binding (`m :: ModelP () ; m = schoolModel dat`)
> or inline the call at each use site.

---

## Available distributions

```haskell
data Distribution a
  = Normal      a a       -- Normal(μ, σ)    — continuous, real line
  | Binomial    Int a     -- Binomial(n, p)  — discrete, [0..n]
  | Poisson     a         -- Poisson(λ)      — discrete, non-negative integers
  | Exponential a         -- Exponential(λ)  — continuous, positive
  | Gamma       a a       -- Gamma(α, β)     — continuous, positive (rate=β)
  | Beta        a a       -- Beta(α, β)      — continuous, (0,1)
```

Distribution parameters are polymorphic `a`, so values from one `sample` flow directly
into another (e.g. `Normal mu sigma` with `mu, sigma :: a`).

HMC/NUTS automatically map constrained distributions (Exponential/Gamma → positive,
Beta → unit interval) into unconstrained space.

---

## Pattern 1: Simple normal model

```haskell
-- μ ~ Normal(0, 10)
-- y_i ~ Normal(μ, σ=2)  (σ known)
normalMean :: [Double] -> ModelP ()
normalMean ys = do
  mu <- sample "mu" (Normal 0 10)
  observe "y" (Normal mu 2) ys
```

---

## Pattern 2: Constrained parameter (unknown σ)

```haskell
-- μ ~ Normal(0, 10)
-- σ ~ Exponential(1)   ← HMC/NUTS use a log transform to enforce σ > 0
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
-- p_ctrl ~ Beta(1,1),  y_ctrl ~ Binomial(50, p_ctrl), 18 successes
-- p_trt  ~ Beta(1,1),  y_trt  ~ Binomial(50, p_trt),  31 successes
clinicalModel :: ModelP ()
clinicalModel = do
  pCtrl <- sample "p_ctrl" (Beta 1 1)
  pTrt  <- sample "p_trt"  (Beta 1 1)
  observe "y_ctrl" (Binomial 50 pCtrl) [18]
  observe "y_trt"  (Binomial 50 pTrt)  [31]
```

---

## Pattern 4: Hierarchical normal (3 schools)

`do`-notation return values let lower-level distributions take parameters from upstream.

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
-- List of latent variable names
sampleNames :: ModelP r -> [Text]
sampleNames (schoolModel schoolData)
-- ["mu","tau","theta_1","theta_2","theta_3"]

-- Log-density evaluation (useful for debugging samplers)
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

## Model graph (auto-extracted dependencies)

Visualize the DAG as Mermaid.js HTML.
Dependencies are **automatically extracted** via `Track`-type propagation through the
arithmetic operations — you don't need to write edges by hand.

```haskell
import Model.HBM      (buildModelGraph, extractDeps)
import Viz.ModelGraph (renderModelGraph)

-- Auto-build the dependency DAG (parent variables propagate via Track type)
let graph = buildModelGraph (schoolModel schoolData)
renderModelGraph "model.html" "School Model" graph
-- Open in browser to see the DAG

-- Or inspect dependencies node-by-node
extractDeps (schoolModel schoolData)
-- [Node "mu"      LatentN "Normal"      {}
-- ,Node "tau"     LatentN "Exponential" {}
-- ,Node "theta_1" LatentN "Normal"      {"mu","tau"}    -- depends on mu, tau
-- ,Node "y_1"     (ObservedN 4) "Normal" {"theta_1"}    -- depends on theta_1
-- ,...]
```

Pass this `ModelGraph` to `Viz.Report.MCMCReport.reportGraph` to embed the DAG in
the MCMC report HTML.

---

## Per-observation log-likelihoods

Used internally by WAIC / LOO (`Stat.ModelSelect`); also useful for debugging.

```haskell
perObsLogLiks :: ModelP r -> Params -> [Double]
-- Returns the logDensity of each observed data point as a flat list
```

```haskell
perObsLogLiks (schoolModel schoolData) ps
-- [-2.1, -2.3, -1.8, -2.0, ...]  (one entry per observation)
```

---

## AD gradient (machine-epsilon precision)

Compute exact gradients via `Numeric.AD.Mode.Forward`.
HMC/NUTS use this internally, so users typically don't need to call it directly.

```haskell
gradAD  :: ModelP r -> [Text] -> [Double] -> [Double]
gradADU :: ModelP r -> [Text] -> [Transform] -> [Double] -> [Double]  -- with constraint transforms

-- Evaluate ∂log p(θ,y) / ∂θ at θ=(1.5, 1.2)
let g = gradAD (normalUnknownSigma obs) ["mu", "sigma"] [1.5, 1.2]
-- Relative error vs central-difference numerical (h=1e-5) is ~10⁻¹⁰
```

`gradADU` returns the gradient in unconstrained space, with constraint transforms
(`PositiveT` / `UnitIntervalT`) detected from priors automatically applied
(used internally by HMC/NUTS).

---

## How polymorphic interpretation works

The rank-2 type `type ModelP r = forall a. (Floating a, Ord a) => Model a r`
lets the same model definition be specialized to different `a` for different purposes:

```haskell
-- a = Double           → numerical log-joint
logJoint myModel ps :: Double

-- a = Forward s Double → AD gradient
gradAD myModel names xs :: [Double]

-- a = Track            → automatic dependency-graph extraction
extractDeps myModel :: [Node]

-- a = Double (placeholder)
collectNodes myModel  :: [Node]    -- structure only (no dependencies)
```

The `Track` type has a `Floating` instance whose arithmetic propagates a dependency
set `Set Text`. When you build `Normal mu sigma`, it automatically records
"this distribution depends on `mu` and `sigma`", which is what enables
`buildModelGraph` to derive edges automatically.
