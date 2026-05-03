# MCMC sampler selection guide

> 🌐 **English** | [日本語](03-mcmc-samplers.ja.md)

> Related demos:
> - [`bench-mcmc`](../demo/BenchMCMC.hs) — MH/HMC/NUTS performance comparison (easy/hard cases)
> - [`test-hmc-nuts`](../demo/TestHMCNUTS.hs) — HMC/NUTS sanity check on a 1D Gaussian
> - [`hbm-example`](../demo/HBMExample.hs) — 4-chain NUTS + R-hat diagnostics

## Sampler comparison

| Sampler | Module | Best for | Main tuning |
|---|---|---|---|
| Metropolis-Hastings | `MCMC.MH` (`metropolis`) | Sanity check / simple models | `mcmcStepSizes` (target 20-50% accept) |
| HMC | `MCMC.HMC` (`hmc`) | Continuous params, mid-size | `hmcStepSize`, `hmcLeapfrogSteps` |
| **NUTS** | `MCMC.NUTS` (`nuts`) | **Recommended for most cases** | `nutsStepSize` (others auto-tuned by dual averaging) |
| Gibbs / hybrid | `MCMC.Gibbs` (`gibbsMH`) | Conjugate models (very fast) | None (direct sampling) |

HMC and NUTS use exact gradients via `Numeric.AD.Mode.Forward`, so they are more
accurate and faster than numerical-derivative versions.
All samplers accept the polymorphic `ModelP r` and automatically apply constraint
transforms (PositiveT/UnitIntervalT).

---

## Constrained parameters

HMC / NUTS automatically lift constrained distributions to **unconstrained space**:

| Distribution | Constraint | Transform |
|---|---|---|
| `Exponential(λ)`, `Gamma(α,β)` | positive (>0) | log: u = log(θ) |
| `Beta(α,β)` | unit interval (0,1) | logit: u = log(θ/(1-θ)) |
| `Normal(μ,σ)` | all reals | none |

Pass initial values in the usual constrained space — Jacobian corrections are applied
transparently.

---

## MCMC.MH — Random Walk Metropolis-Hastings

### When to use
- Sanity checks / prototyping.
- 1–3 parameters, simple models.
- Discrete parameters (HMC/NUTS need gradients).

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

metropolis       :: Model a -> MCMCConfig -> Params -> GenIO -> IO Chain
metropolisChains :: Model a -> MCMCConfig -> Int    -> Params -> GenIO -> IO [Chain]
```

### Example

```haskell
import MCMC.MH
import qualified Data.Map.Strict as Map

let m   = normalMean [1.2, 2.3, 3.1]
    cfg = (defaultMCMCConfig (sampleNames m))
            { mcmcIterations = 5000
            , mcmcBurnIn     = 1000
            , mcmcStepSizes  = Map.fromList [("mu", 0.5)]  -- target 20–50 % accept
            }
chain <- metropolis m cfg (Map.fromList [("mu", 0.0)]) gen
```

---

## MCMC.HMC — Hamiltonian Monte Carlo

### When to use
- Continuous parameters, ~10–dozens of dimensions.
- Research-grade trajectory control (more knobs than NUTS).

### API

```haskell
import MCMC.HMC

data HMCConfig = HMCConfig
  { hmcIterations    :: Int
  , hmcBurnIn        :: Int
  , hmcStepSize      :: Double  -- leapfrog step ε
  , hmcLeapfrogSteps :: Int     -- leapfrog step count L
  }

defaultHMCConfig :: HMCConfig
-- iterations=2000, burnIn=500, stepSize=0.1, leapfrogSteps=10

hmc       :: Model a -> HMCConfig -> Params -> GenIO -> IO Chain
hmcChains :: Model a -> HMCConfig -> Int    -> Params -> GenIO -> IO [Chain]
```

### Example

```haskell
import MCMC.HMC

let cfg = defaultHMCConfig
            { hmcIterations    = 3000
            , hmcStepSize      = 0.1    -- target 60-80 % accept
            , hmcLeapfrogSteps = 15     -- raise to 20-50 if strongly correlated
            }
chain <- hmc m cfg initP gen
```

### Tuning rules of thumb
- Acceptance < 60 %: shrink `hmcStepSize`.
- Acceptance > 90 %: grow `hmcStepSize` (under-utilising).
- Low ESS: increase `hmcLeapfrogSteps`.

---

## MCMC.NUTS — No-U-Turn Sampler (recommended)

Implementation of Hoffman & Gelman (2014) Algorithm 3.
Trajectory length is decided automatically by the U-turn criterion, so `leapfrogSteps`
needs no tuning. Step size is auto-adapted by Dual Averaging during burn-in.

### API

```haskell
import MCMC.NUTS

data NUTSConfig = NUTSConfig
  { nutsIterations    :: Int
  , nutsBurnIn        :: Int
  , nutsStepSize      :: Double  -- initial ε (seed for adaptation)
  , nutsMaxDepth      :: Int     -- max tree depth (default 10)
  , nutsAdaptStepSize :: Bool    -- Dual Averaging on (default True)
  , nutsTargetAccept  :: Double  -- target acceptance δ (default 0.8)
  }

defaultNUTSConfig :: NUTSConfig

nuts       :: Model a -> NUTSConfig -> Params -> GenIO -> IO Chain
nutsChains :: Model a -> NUTSConfig -> Int    -> Params -> GenIO -> IO [Chain]
```

### Example: basic usage

```haskell
import MCMC.NUTS

-- Just hand it an initial stepSize (auto-tuned during burn-in)
let cfg = defaultNUTSConfig { nutsIterations = 2000, nutsStepSize = 0.1 }
chain <- nuts m cfg initP gen
```

### Example: 4-chain parallel + R-hat convergence check

```haskell
import MCMC.NUTS
import MCMC.Core  (chainVals)
import Stat.MCMC  (rhat, ess)

chains <- nutsChains m cfg 4 initP gen  -- pass +RTS -N4 for OS-thread parallelism

-- R-hat < 1.01 = converged
let params = sampleNames m
forM_ params $ \p -> do
  let r = rhat (map (chainVals p) chains)
  printf "%s: R-hat = %s, ESS = %.0f\n"
    (T.unpack p) (show r) (ess (chainVals p (head chains)))
```

### Initial step-size guidance

| Model | Recommended initial stepSize |
|---|---|
| Simple normal | 0.3–1.0 |
| Hierarchical (2–3 levels) | 0.05–0.3 |
| Beta-Binomial | 0.1–0.5 |

With `nutsAdaptStepSize = True` (default) the second half of burn-in uses the auto-tuned ε,
so the initial value just needs to be in the right ballpark.

---

## Multi-chain pattern

MH / HMC / NUTS all expose `<sampler>Chains` for parallel chain execution.

```haskell
-- Run 4 chains asynchronously in parallel (via the `async` library).
-- Pass +RTS -N4 to allocate threads.
chains <- nutsChains m cfg 4 initP gen

-- Convergence check
let allParams = sampleNames m
converged = all (\p -> maybe False (< 1.01) (rhat (map (chainVals p) chains))) allParams
if converged
  then putStrLn "converged"
  else putStrLn "warning: may not have converged"
```

---

## MCMC.Core — Chain type and statistics

Common interface for the `Chain` returned by every sampler.

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

chainVals :: Text -> Chain -> [Double]  -- sample sequence (feed to Stat.MCMC.ess etc.)
```

---

## Stat.MCMC — diagnostic statistics

```haskell
import Stat.MCMC

ess     :: [Double] -> Double          -- effective sample size (Geyer estimator)
rhat    :: [[Double]] -> Maybe Double  -- Split R-hat (Vehtari et al. 2021)
hdi     :: Double -> [Double] -> (Double, Double)   -- shortest-interval HDI
autocorr :: Int -> [Double] -> [(Int, Double)]       -- autocorrelation
kde     :: Int -> [Double] -> [(Double, Double)]     -- KDE density
```

**Reading R-hat:**
- `Just 1.00`: fully converged.
- `< Just 1.01`: treated as converged (recommended threshold per Vehtari et al.).
- `>= Just 1.01`: under-converged — increase burn-in or adjust stepSize.
