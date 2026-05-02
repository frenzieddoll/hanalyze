# MCMC Sampler Selection Guide

> 🌐 **English** | [日本語](03-mcmc-samplers.ja.md)

> Related demos:
> - [`bench-mcmc`](../demo/BenchMCMC.hs) — MH/HMC/NUTS performance comparison (easy/hard cases)
> - [`test-hmc-nuts`](../demo/TestHMCNUTS.hs) — HMC/NUTS sanity check on a 1D Gaussian
> - [`hbm-example`](../demo/HBMExample.hs) — 4-chain NUTS + R-hat diagnostic

## Sampler comparison

| Sampler | Module | Best for | Key tuning knobs |
|---|---|---|---|
| Metropolis-Hastings | `MCMC.MH` (`metropolis`) | Quick checks, simple models | `mcmcStepSizes` (target accept 20–50%) |
| HMC | `MCMC.HMC` (`hmc`) | Continuous parameters, medium scale | `hmcStepSize`, `hmcLeapfrogSteps` |
| **NUTS** | `MCMC.NUTS` (`nuts`) | **Recommended default** | `nutsStepSize` (others auto-tuned via dual averaging) |
| Gibbs / hybrid | `MCMC.Gibbs` (`gibbsMH`) | Conjugate models (very fast) | None (direct sampling) |

HMC/NUTS use exact gradients via `Numeric.AD.Mode.Forward`, which is more accurate
and faster than numerical-difference variants.
All samplers accept polymorphic models (`ModelP r`) and apply constraint transforms
(PositiveT/UnitIntervalT) automatically.

---

## Constrained parameters

HMC / NUTS automatically map constrained distributions into unconstrained space.

| Distribution | Constraint | Transform |
|---|---|---|
| `Exponential(λ)`, `Gamma(α,β)` | positive (>0) | log: u = log(θ) |
| `Beta(α,β)` | unit interval (0,1) | logit: u = log(θ/(1-θ)) |
| `Normal(μ,σ)` | real line | none |

Pass initial values in the natural (constrained) space; Jacobian corrections are
applied automatically.

---

## MCMC.MH — Random Walk Metropolis-Hastings

### When to use
- Sanity checks, prototyping
- Simple models with 1–3 parameters
- Models containing discrete parameters (HMC/NUTS need gradients)

### API

```haskell
import MCMC.MH

data MCMCConfig = MCMCConfig
  { mcmcIterations :: Int
  , mcmcBurnIn     :: Int
  , mcmcStepSizes  :: Map Text Double  -- proposal SD per parameter
  }

defaultMCMCConfig :: [Text] -> MCMCConfig
-- iterations=2000, burnIn=500, stepSize=1.0 (all parameters)

metropolis       :: ModelP r -> MCMCConfig -> Params -> GenIO -> IO Chain
metropolisChains :: ModelP r -> MCMCConfig -> Int    -> Params -> GenIO -> IO [Chain]
```

### Example

```haskell
import MCMC.MH
import qualified Data.Map.Strict as Map

let m   = normalMean [1.2, 2.3, 3.1]
    cfg = (defaultMCMCConfig (sampleNames m))
            { mcmcIterations = 5000
            , mcmcBurnIn     = 1000
            , mcmcStepSizes  = Map.fromList [("mu", 0.5)]  -- aim for 20-50% acceptance
            }
chain <- metropolis m cfg (Map.fromList [("mu", 0.0)]) gen
```

---

## MCMC.HMC — Hamiltonian Monte Carlo

### When to use
- Continuous parameters, ~10 to several dozen dimensions
- Research use where you want fine-grained trajectory control vs NUTS

### API

```haskell
import MCMC.HMC

data HMCConfig = HMCConfig
  { hmcIterations    :: Int
  , hmcBurnIn        :: Int
  , hmcStepSize      :: Double  -- leapfrog step size ε
  , hmcLeapfrogSteps :: Int     -- number of leapfrog steps L
  }

defaultHMCConfig :: HMCConfig
-- iterations=2000, burnIn=500, stepSize=0.1, leapfrogSteps=10

hmc       :: ModelP r -> HMCConfig -> Params -> GenIO -> IO Chain
hmcChains :: ModelP r -> HMCConfig -> Int    -> Params -> GenIO -> IO [Chain]
```

### Example

```haskell
import MCMC.HMC

let cfg = defaultHMCConfig
            { hmcIterations    = 3000
            , hmcStepSize      = 0.1    -- target 60-80% acceptance
            , hmcLeapfrogSteps = 15     -- raise to 20-50 for strong correlations
            }
chain <- hmc m cfg initP gen
```

### Tuning guide
- Acceptance < 60% → reduce `hmcStepSize`
- Acceptance > 90% → increase `hmcStepSize` (under-utilized)
- Low ESS → increase `hmcLeapfrogSteps`

---

## MCMC.NUTS — No-U-Turn Sampler (recommended)

Implementation of Hoffman & Gelman (2014) Algorithm 3.
Tree depth is determined adaptively by the U-turn criterion, so `leapfrogSteps`
tuning is unnecessary. During burn-in, dual averaging auto-tunes the step size.

### API

```haskell
import MCMC.NUTS

data NUTSConfig = NUTSConfig
  { nutsIterations    :: Int
  , nutsBurnIn        :: Int
  , nutsStepSize      :: Double  -- initial ε (seed for auto-tuning)
  , nutsMaxDepth      :: Int     -- max tree depth (default 10)
  , nutsAdaptStepSize :: Bool    -- dual-averaging adaptation (default True)
  , nutsTargetAccept  :: Double  -- target acceptance δ (default 0.8)
  }

defaultNUTSConfig :: NUTSConfig

nuts       :: ModelP r -> NUTSConfig -> Params -> GenIO -> IO Chain
nutsChains :: ModelP r -> NUTSConfig -> Int    -> Params -> GenIO -> IO [Chain]
```

### Basic usage

```haskell
import MCMC.NUTS

-- Just supply an initial stepSize (it's auto-tuned during burn-in)
let cfg = defaultNUTSConfig { nutsIterations = 2000, nutsStepSize = 0.1 }
chain <- nuts m cfg initP gen
```

### 4 chains in parallel + R-hat convergence

```haskell
import MCMC.NUTS
import MCMC.Core  (chainVals)
import Stat.MCMC  (rhat, ess)

chains <- nutsChains m cfg 4 initP gen  -- +RTS -N4 for OS-thread parallelism

-- R-hat < 1.01 → converged
let params = sampleNames m
forM_ params $ \p -> do
  let r = rhat (map (chainVals p) chains)
  printf "%s: R-hat = %s, ESS = %.0f\n"
    (T.unpack p) (show r) (ess (chainVals p (head chains)))
```

### Initial step-size guide

| Model | Suggested initial stepSize |
|---|---|
| Simple normal model | 0.3–1.0 |
| Hierarchical (2–3 levels) | 0.05–0.3 |
| Beta-Binomial | 0.1–0.5 |

With `nutsAdaptStepSize = True` (default), the late burn-in switches to the
auto-tuned ε, so the initial value can be approximate.

---

## Common multi-chain pattern

MH / HMC / NUTS all expose a `<sampler>Chains` function for parallel chains.

```haskell
-- 4 chains in parallel (uses async); add +RTS -N4 to allocate threads
chains <- nutsChains m cfg 4 initP gen

-- Check convergence
let allParams = sampleNames m
    converged = all (\p -> maybe False (< 1.01) (rhat (map (chainVals p) chains))) allParams
if converged
  then putStrLn "Converged"
  else putStrLn "Warning: not converged"
```

---

## MCMC.Core — Chain type & statistics

Common interface returned by all samplers.

```haskell
import MCMC.Core

data Chain = Chain
  { chainSamples  :: [Map Text Double]  -- post-burn-in samples
  , chainAccepted :: Int
  , chainTotal    :: Int
  }

acceptanceRate    :: Chain -> Double
posteriorMean     :: Text -> Chain -> Maybe Double
posteriorSD       :: Text -> Chain -> Maybe Double
posteriorQuantile :: Double -> Text -> Chain -> Maybe Double

chainVals :: Text -> Chain -> [Double]  -- sample sequence (pass to Stat.MCMC.ess etc.)
```

---

## Stat.MCMC — Diagnostic statistics

```haskell
import Stat.MCMC

ess      :: [Double] -> Double          -- effective sample size (Geyer estimator)
rhat     :: [[Double]] -> Maybe Double  -- Split R-hat (Vehtari et al. 2021)
hdi      :: Double -> [Double] -> (Double, Double)  -- shortest HDI
autocorr :: Int -> [Double] -> [(Int, Double)]      -- autocorrelation
kde      :: Int -> [Double] -> [(Double, Double)]   -- KDE density
```

**R-hat interpretation:**
- `Just 1.00`: perfect convergence
- `< Just 1.01`: converged (Vehtari et al. recommended threshold)
- `>= Just 1.01`: under-converged — increase burn-in or adjust step size
