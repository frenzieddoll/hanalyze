# hanalyze

> 🌐 **English** | [日本語](README.ja.md)

Statistical analysis and visualization library in Haskell.
Usable both as a CLI tool and as a Haskell library.

---

## DSL Highlights

`Model.HBM` is a polymorphic free-monad DSL — write a model once, get four interpretations:

```haskell
-- Write once, use four ways
type ModelP r = forall a. (Floating a, Ord a) => Model a r

myModel :: ModelP ()
myModel = do
  mu    <- sample "mu"    (Normal 0 10)
  sigma <- sample "sigma" (Exponential 1)
  observe "y" (Normal mu sigma) [1.5, 2.0, 1.8]
```

| Interpretation | Specialization | Use |
|---|---|---|
| Structure inspection | `a = Double` | `collectNodes`, `describeModel` |
| Log joint | `a = Double` | `logJoint`, `logPrior`, `logLikelihood` |
| AD gradient | `a = Forward Double` | `gradAD`, `gradADU` (machine-epsilon precision) |
| Dependency tracking | `a = Track` | `extractDeps` for automatic DAG extraction |

Samplers (`MCMC.HMC`/`NUTS`/`Gibbs`/`MH`) all accept `ModelP`, with AD gradients and automatic constraint transformation (PositiveT/UnitIntervalT).

---

## Documentation (docs/)

| Page | Content |
|---|---|
| [Quickstart](docs/01-quickstart.md) | Build, minimal workflow, **"what to do" → which demo** lookup |
| [Probabilistic Programming DSL](docs/02-probabilistic-model.md) | Model.HBM patterns (Beta-Binomial / hierarchical normal / polymorphic interpretations) |
| [MCMC Sampler Guide](docs/03-mcmc-samplers.md) | MH / HMC / NUTS selection, tuning, R-hat |
| [Gibbs Sampling](docs/04-gibbs.md) | Conjugate updates, ESS/s comparison |
| [Variational Inference (ADVI)](docs/05-variational-inference.md) | VI vs NUTS, ELBO convergence, mean-field limitations |
| [Model Comparison (WAIC/LOO)](docs/06-model-comparison.md) | WAIC, PSIS-LOO, Pareto k̂ diagnostic |
| [Visualization](docs/07-visualization.md) | Report, Bar, Histogram, PNG/SVG export |

---

## Build

```bash
cabal build              # library + all executables
cabal test               # test suite
```

## Demos

Run with `cabal run <demo-name>` (HTML/PNG output to current directory).
For task-based usage, see [docs/01-quickstart.md](docs/01-quickstart.md).

### Getting Started

| Demo | Content | What you'll learn |
|---|---|---|
| `hbm-example`     | Hierarchical normal model + 4-chain NUTS → `mcmc_report*.html` | HBM DSL syntax, MCMC reports |
| `hbm-regression`  | Bayesian linear regression + AnalysisReport (DAG / MCMC / credible bands) | HBM regression integration with AnalysisReport |
| `gp-demo`         | GP regression (RBF / Matérn / Periodic) + LML comparison | Kernel selection, GP usage |

### Model Comparison & Paradoxes

| Demo | Content | What you'll learn |
|---|---|---|
| `simpson-paradox` | LM/GLMM/HBM compared on Simpson's-paradox data → 4 HTML reports | Importance of hierarchy, model selection |
| `hbm-random-slope`| Random intercept vs random intercept + random slope (M1 vs M2) compared via WAIC | Hierarchical extensions, WAIC-based selection |
| `clinical-trial`  | Bayesian A/B test (clinical Beta-Binomial) | Two-group comparison, decision theory |

### Sampler Deep-dive

| Demo | Content | What you'll learn |
|---|---|---|
| `bench-mcmc`     | MH / HMC / NUTS performance comparison | Sampler selection, ESS/s |
| `test-hmc-nuts`  | HMC/NUTS accuracy test (1D Gaussian sanity check) | Sampler verification |
| `gibbs-demo`     | Gibbs + WAIC/LOO model comparison | Conjugate updates, model selection |
| `gibbs-hbm-demo` | Gibbs × HBM DSL integration (auto-conjugate detection) | Hybrid Gibbs+MH |
| `vi-demo`        | Variational Inference (ADVI) vs NUTS | VI speed and limitations |

### Classical Regression & Plotting

| Demo | Content | What you'll learn |
|---|---|---|
| `glmm-demo`     | LME / GLMM (random intercept) | Mixed-effects models |
| `bar-demo`      | Viz.Bar (bar / stacked / grouped) + PNG/SVG export | Visualization, image export |

---

## CLI Usage

```
cabal run hanalyze -- <file> <xcols> <ycols> [LM|GLM|NoReg|GP|HBM] [options]
```

| Option | Description |
|---|---|
| `-d DIST` | Distribution: `gaussian` / `binomial` / `poisson` |
| `-l LINK` | Link function: `identity` / `log` / `logit` / `sqrt` |
| `--degree SPEC` | Polynomial degree. `N` for all columns, or `-1 N1 -2 N2` per column |
| `--ci [LEVEL]` | Confidence interval (default 0.95) |
| `--pi [LEVEL]` | Prediction interval (Gaussian only) |
| `--group COL` | Mixed-effects model (LME / GLMM) |
| `--hist COL` | Histogram display |
| `--fit DIST` | Overlay theoretical density |
| `--report [FILE]` | Generate HTML analysis report (default: `report.html`) |
| `--waic` | Compute WAIC / LOO-CV and embed in report |
| `--format FMT` | `html` / `png` / `svg`. With `png/svg`, plots in the report are also exported as images |

```bash
# Linear regression with confidence band + AnalysisReport
cabal run hanalyze -- data.tsv x y LM --ci 0.95 --report

# Poisson GLM (per-column polynomial degrees) + WAIC
cabal run hanalyze -- data.tsv "x1 x2" y GLM -d poisson -l log --degree -1 2 -2 3 --waic --report

# Mixed-effects model (LME) + WAIC
cabal run hanalyze -- data.tsv x y LM --group school --waic --report

# Bayesian linear regression (HBM): NUTS for α/β/σ posteriors → AnalysisReport
cabal run hanalyze -- data.csv x y HBM --report --waic

# Gaussian Process regression (RBF/Matérn/Periodic comparison)
cabal run hanalyze -- data.csv x y GP --report

# Histogram with theoretical normal density overlay
cabal run hanalyze -- data.csv x y NoReg --hist score --fit normal

# Report + plot PNG export
cabal run hanalyze -- data.csv x y LM --report --format png
```

---

## Library Usage

Add `hanalyze` to your `build-depends` in `hanalyze.cabal`.

---

## Module Layout

```
Stat/
  Distribution.hs  -- Probability distributions (Normal / Gamma / Beta / ...)
  MCMC.hs          -- Diagnostic statistics (ESS / HDI / R-hat / KDE)
  ModelSelect.hs   -- Model comparison (WAIC / PSIS-LOO)
  VI.hs            -- Variational Inference (ADVI / Adam)

Model/
  HBM.hs           -- Polymorphic probabilistic-programming DSL
                   --   (AD gradients, Track-based dependency extraction)

MCMC/
  Core.hs          -- Chain type, posterior statistics (independently usable)
  MH.hs            -- Random Walk Metropolis-Hastings
  HMC.hs           -- Hamiltonian Monte Carlo (AD gradients)
  NUTS.hs          -- No-U-Turn Sampler (AD gradients + dual averaging)
  Gibbs.hs         -- Gibbs sampling + hybrid Gibbs+MH (auto-conjugate detection)

Viz/
  MCMC.hs          -- Diagnostic plots (KDE / trace / autocorr / pair scatter)
  Report.hs        -- Integrated HTML report (R-hat for multi-chain)
  AnalysisReport.hs -- Multi-section report for LM/GLM/GLMM/GP/HBM, with comparison view
  ModelGraph.hs    -- Mermaid.js DAG
  Bar.hs           -- Bar charts (vertical / horizontal / stacked / grouped)
  Histogram.hs     -- Histograms (with theoretical density overlay)
  Scatter.hs       -- Scatter plots, regression curves
  Core.hs          -- PlotConfig / OutputFormat / openInBrowser (PNG/SVG via vl-convert)
```

---

## API Reference

### `Stat.Distribution` — Probability distributions

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

### `Stat.MCMC` — MCMC diagnostic statistics

```haskell
import Stat.MCMC

-- Autocorrelation (lag 0..maxLag)
autocorr :: Int -> [Double] -> [(Int, Double)]

-- Highest density interval (shortest interval containing the given probability mass)
hdi :: Double -> [Double] -> (Double, Double)
-- hdi 0.94 samples → (lower, upper)

-- Effective Sample Size (Geyer's initial monotone sequence estimator)
ess :: [Double] -> Double

-- Split-R-hat convergence diagnostic (Vehtari et al. 2021)
-- Input: list of per-chain sample sequences for the same parameter
-- R-hat < 1.01 indicates convergence
rhat :: [[Double]] -> Maybe Double

-- Kernel Density Estimation (Gaussian kernel, Silverman bandwidth)
-- Returns nPoints (x, density) pairs
kde :: Int -> [Double] -> [(Double, Double)]
```

```haskell
import Stat.MCMC
import MCMC.Core (chainVals)

let muSamples = map (chainVals "mu") chains   -- chains :: [Chain]
    essVal    = ess (head muSamples)
    rhatVal   = rhat muSamples                -- R-hat < 1.01 = converged

let kdePoints = kde 200 (chainVals "mu" chain)  -- [(x, density)]
```

---

### `Model.HBM` — Polymorphic probabilistic programming DSL

A DSL whose continuations are polymorphic in `forall a. (Floating a, Ord a) => Model a r`,
allowing four interpretations of the same model: structure inspection, log-joint evaluation,
AD gradients, and dependency tracking.

```haskell
import Model.HBM   -- exports Distribution (..), sample, observe

-- Polymorphic DSL type alias
type ModelP r = forall a. (Floating a, Ord a) => Model a r

-- Latent variable
sample  :: Text -> Distribution a -> Model a a
-- Condition on observed data (i.i.d. assumption)
observe :: Text -> Distribution a -> [Double] -> Model a ()
```

#### Example

```haskell
import qualified Data.Text as T

-- Hierarchical normal model for J schools:
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

describeModel (schoolModel dat)
-- Model nodes:
--   [latent]   mu ~ Normal
--   [latent]   tau ~ Exponential
--   [latent]   theta_1 ~ Normal
--   [observed] y_1 ~ Normal  (n=4)
--   ...
```

> **Note**: `ModelP` has rank-2 type, so it can't be `let`-bound (e.g. `m :: ModelP () = schoolModel dat`).
> Use top-level binding (`m = schoolModel dat`) or inline the call at each use site.

#### Four interpretations

```haskell
import qualified Data.Map.Strict as Map

let ps = Map.fromList [("mu", 73.0), ("tau", 10.0), ...]

-- 1. Structure inspection (a = Double)
collectNodes  (schoolModel dat)              -- :: [Node]
describeModel (schoolModel dat)              -- :: Text

-- 2. Log-joint numerical evaluation (a = Double)
logJoint      (schoolModel dat) ps           -- log p(θ, y)
logPrior      (schoolModel dat) ps           -- log p(θ)
logLikelihood (schoolModel dat) ps           -- log p(y | θ)

-- 3. AD gradient (a = Forward Double, machine-epsilon precision)
gradAD  (schoolModel dat) ["mu","tau"] [0,1] -- :: [Double]
gradADU (schoolModel dat) names trans us     -- with constraint transforms (HMC)

-- 4. Dependency tracking (a = Track)
extractDeps (schoolModel dat)                -- :: [Node] (with nodeDeps)
buildModelGraph (schoolModel dat)            -- Mermaid DAG auto-built
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
extractDeps     :: ModelP r -> [Node]            -- Node has nodeDeps :: Set Text
buildModelGraph :: ModelP r -> ModelGraph        -- auto-built (no manual edges needed)

-- Constraint transforms (used by HMC/NUTS/VI)
getTransforms        :: ModelP r -> Map Text Transform   -- auto-detected from priors
logJointUnconstrained :: (Floating a, Ord a) => Model a r -> [Text] -> [Transform] -> Map Text a -> a

-- Structure extraction (for Gibbs conjugate detection)
runObserveDists :: Model Double r -> Map Text Double -> [(Text, Distribution Double, [Double])]
priorList       :: Model Double r -> [(Text, Distribution Double)]
```

---

### `MCMC.Core` — Chain type and posterior statistics

Common types independent of the sampling algorithm. Importable on its own.

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
-- e.g. posteriorQuantile 0.025 "mu" chain  → lower 2.5% quantile

-- Sample sequence for a single parameter (used by R-hat etc.)
chainVals :: Text -> Chain -> [Double]

-- For parallel chains: spawn an independent child GenIO from a base GenIO
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
  , mcmcStepSizes  :: Map Text Double  -- proposal SD per parameter
  }

defaultMCMCConfig :: [Text] -> MCMCConfig
-- iterations=2000, burnIn=500, stepSize=1.0

metropolis       :: ModelP r -> MCMCConfig -> Params -> GenIO -> IO Chain
metropolisChains :: ModelP r -> MCMCConfig -> Int    -> Params -> GenIO -> IO [Chain]
-- metropolisChains m cfg 4 init gen  -- 4 chains in parallel (CPU parallel via +RTS -N)
```

```haskell
main :: IO ()
main = do
  let cfg = (defaultMCMCConfig (sampleNames m))
              { mcmcIterations = 5000
              , mcmcBurnIn     = 1000
              , mcmcStepSizes  = Map.fromList
                  [("mu", 5.0), ("tau", 2.0), ("theta_1", 3.0), ...]
              }
  gen   <- createSystemRandom
  chain <- metropolis (schoolModel dat) cfg initParams gen
  -- Target acceptance rate: 0.20 ~ 0.50
```

---

### `MCMC.HMC` — Hamiltonian Monte Carlo

Uses precise gradients via `Numeric.AD.Mode.Forward`. Constrained parameters
(Exponential / Gamma → positive, Beta → unit interval) are mapped into unconstrained
space by log / logit transforms, leapfrogged, then mapped back. Jacobian corrections
are applied automatically — pass initial values as ordinary parameter values.

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

**Tuning:**
- Adjust `hmcStepSize` so acceptance rate is 60–80%.
- For hierarchical models or strong correlations, increase `hmcLeapfrogSteps` to 20–50.

---

### `MCMC.NUTS` — No-U-Turn Sampler

Implements Hoffman & Gelman (2014) Algorithm 3.
Tree depth is determined adaptively via U-turn detection, so no `leapfrogSteps` tuning.
Uses the same constraint transforms as HMC.

```haskell
import MCMC.NUTS

data NUTSConfig = NUTSConfig
  { nutsIterations    :: Int
  , nutsBurnIn        :: Int
  , nutsStepSize      :: Double  -- initial ε₀
  , nutsMaxDepth      :: Int     -- max tree depth (default 10)
  , nutsAdaptStepSize :: Bool    -- dual averaging during burn-in (default True)
  , nutsTargetAccept  :: Double  -- target acceptance δ (default 0.8)
  }

nuts       :: ModelP r -> NUTSConfig -> Params -> GenIO -> IO Chain
nutsChains :: ModelP r -> NUTSConfig -> Int    -> Params -> GenIO -> IO [Chain]
```

```haskell
chains <- nutsChains model cfg 4 initParams gen
let rhatMu = rhat (map (chainVals "mu") chains)
print rhatMu  -- Just 1.001 → converged
```

---

### Multi-chain & R-hat convergence diagnostic

```haskell
import MCMC.NUTS  (nutsChains, defaultNUTSConfig, NUTSConfig (..))
import MCMC.Core  (chainVals)
import Stat.MCMC  (rhat, ess)

main :: IO ()
main = do
  gen <- createSystemRandom
  let cfg = defaultNUTSConfig { nutsIterations = 2000 }

  -- 4 chains in parallel (use +RTS -N for OS-thread parallelism)
  chains <- nutsChains model cfg 4 initParams gen

  -- Check R-hat for each parameter
  mapM_ (\p -> do
    let r = rhat (map (chainVals p) chains)
    putStrLn $ show p ++ ": R-hat = " ++ show r
    ) (sampleNames model)
  -- "mu":    R-hat = Just 1.001  (< 1.01 = converged)
  -- "sigma": R-hat = Just 1.003
```

---

### `Viz.Report` — Integrated MCMC HTML report (recommended)

```haskell
import Viz.Report
import MCMC.Core (Chain)
import Model.HBM (ModelGraph)

data MCMCReport = MCMCReport
  { reportTitle    :: Text
  , reportGraph    :: Maybe ModelGraph  -- Nothing to omit DAG
  , reportChain    :: Chain             -- main chain (used for autocorr / pair plots)
  , reportChains   :: [Chain]           -- all parallel chains (empty = single-chain mode)
  , reportParams   :: [Text]
  , reportPairs    :: [(Text, Text)]    -- pair scatter plots
  , reportMaxLag   :: Int               -- max autocorr lag
  }

defaultReport :: Text -> Chain -> [Text] -> MCMCReport
renderReport  :: FilePath -> MCMCReport -> IO ()
```

**Single-chain report:**
```haskell
let report = (defaultReport "My Model" chain names)
               { reportGraph = Just graph, reportPairs = [("mu", "tau")] }
renderReport "report.html" report
```

**Multi-chain report (with R-hat column):**
```haskell
chains <- nutsChains model cfg 4 initParams gen
let report = (defaultReport "My Model" (head chains) names)
               { reportGraph  = Just graph
               , reportChains = chains   -- enables multi-chain mode
               , reportPairs  = [("mu", "tau")]
               }
renderReport "report_multi.html" report
```

The multi-chain HTML contains:
- **Model Graph** — Mermaid.js DAG
- **Posterior Summary** — stat boxes + Mean/SD/2.5%/97.5%/ESS/**R-hat** table (R-hat < 1.01 green, ≥ 1.01 red)
- **MCMC Diagnostics** — KDE density (94% HDI) + per-chain colored traces
- **Autocorrelation** — for the main chain
- **Pair Scatter** — joint posterior

---

### `Viz.AnalysisReport` — Multi-section analysis report

For LM / GLM / GLMM / GP / HBM, generates a single HTML containing data summary, model
overview, regression results, interactive prediction, and appendix sections.

```haskell
writeAnalysisReport
  :: FilePath -> AnalysisReportConfig -> DataFrame -> [Text] -> Text
  -> ModelFit -> [NamedPlot] -> IO ()

-- ModelFit type covers all model kinds
data ModelFit
  = RegFit FitSummary       -- LM / GLM
  | MixFit GLMMSummary      -- LME / GLMM
  | GPFit  GPFitSummary     -- Gaussian Process
  | HBMFit HBMRegSummary    -- Hierarchical Bayes (NUTS posterior)
  | NoRegFit
```

For comparing multiple models on one HTML page (predictions overlay + coefficients +
WAIC/LOO table):

```haskell
data CompareEntry = CompareEntry
  { ceLabel :: Text   -- e.g. "LM (Pooled)"
  , ceColor :: Text   -- CSS color for plot
  , ceFit   :: ModelFit
  }

writeComparisonReport
  :: FilePath -> AnalysisReportConfig -> DataFrame -> [Text] -> Text
  -> [CompareEntry] -> IO ()
```

See [`SimpsonParadoxDemo.hs`](demo/SimpsonParadoxDemo.hs) for an end-to-end example
producing four reports (LM, GLMM, HBM, and a side-by-side comparison).

---

### `Viz.MCMC` — Standalone MCMC plots

For finer control without `Viz.Report`.

```haskell
mcmcDiagnostics      :: PlotConfig -> [Text] -> Chain  -> VegaLite
mcmcDiagnosticsMulti :: PlotConfig -> [Text] -> [Chain] -> VegaLite
multiTracePlot       :: PlotConfig -> [Text] -> [Chain] -> VegaLite
autocorrPlot         :: PlotConfig -> Int -> [Text] -> Chain -> VegaLite
pairScatter          :: PlotConfig -> Text -> Text -> Chain -> VegaLite
posteriorPlot        :: PlotConfig -> [Text] -> Chain -> VegaLite
tracePlot            :: PlotConfig -> [Text] -> Chain -> VegaLite
```

Each `*Plot` has a corresponding `*PlotFile :: OutputFormat -> FilePath -> ... -> IO ()`
for direct file output (HTML / PNG / SVG via `vl-convert`).

---

### `Viz.Histogram` — Histograms

```haskell
import Viz.Histogram

histogramPlot         :: PlotConfig -> Text -> [Double] -> Maybe Int -> VegaLite
histogramWithDensity  :: PlotConfig -> Text -> [Double] -> Maybe Int -> Distribution -> VegaLite
```

```haskell
histogramWithDensityFile HTML "hist.html"
  (defaultConfig "Score Distribution") "score" vals Nothing (Normal 2.5 1.0)
```

---

## Sampler selection guide

| Sampler | Best for | Caveats |
|---|---|---|
| `MCMC.MH` (Metropolis) | Quick model sanity checks | ESS collapses in high-dim / high-correlation cases |
| `MCMC.HMC` | Continuous parameters, medium-sized models | Tune both `stepSize` and `leapfrogSteps` |
| `MCMC.NUTS` | **Recommended default** | Tune only `stepSize`; `leapfrogSteps` not needed |
| `MCMC.Gibbs` | Conjugate models (very fast) | Cannot handle non-conjugate parameters |

See [MCMC Sampler Guide](docs/03-mcmc-samplers.md) and [Gibbs Guide](docs/04-gibbs.md) for details.

**Step-size guidelines:**
- NUTS: ideal acceptance is 60–85%. Too low → reduce `stepSize`.
- HMC: same target. If low, also lower `leapfrogSteps`.
- MH: 20–50% target; tune `mcmcStepSizes` per parameter.

**Constrained parameters:**
- `Exponential` / `Gamma` → positive constraint (`PositiveT`: log transform)
- `Beta` → unit interval constraint (`UnitIntervalT`: logit transform)
- HMC / NUTS apply Jacobian correction automatically; pass initial values in the natural (unconstrained) scale.

---

### `MCMC.Gibbs` — Gibbs sampling

Directly samples from the conjugate full-conditional distribution where one is available.
No rejection step → typically 3–5× higher ESS/sec than NUTS for fully conjugate models.

```haskell
import MCMC.Gibbs

-- Built-in conjugate updates
normalNormal :: Text -> Double -> Double -> [Double] -> Double -> GibbsUpdate
-- μ ~ Normal(μ₀,σ₀), y ~ Normal(μ,σ_lik) → directly sample from conditional posterior

betaBinomial :: Text -> Double -> Double -> Int -> Int -> GibbsUpdate
-- p ~ Beta(α,β), y ~ Binomial(n,p), k successes → Beta(α+k, β+n-k)

gammaPoisson :: Text -> Double -> Double -> [Double] -> GibbsUpdate
-- λ ~ Gamma(α,β), y ~ Poisson(λ) → Gamma(α+Σy, β+n)

gibbs       :: [GibbsUpdate] -> GibbsConfig -> Params -> GenIO -> IO Chain
gibbsChains :: [GibbsUpdate] -> GibbsConfig -> Int    -> Params -> GenIO -> IO [Chain]

-- Auto-detect conjugates from a HBM model + hybrid Gibbs+MH for non-conjugate parameters
gibbsFromModel :: ModelP r -> ([GibbsUpdate], [Text])
gibbsMH        :: ModelP r -> GibbsConfig -> Map Text Double -> Params -> GenIO -> IO Chain
```

See [Gibbs Sampling Guide](docs/04-gibbs.md) for details.

---

### `Stat.VI` — Variational Inference (ADVI)

Approximates the posterior with a Gaussian family and maximises the ELBO via Adam.
Faster than NUTS but ignores parameter correlations (mean-field).

```haskell
import Stat.VI

advi :: ModelP r -> VIConfig -> Params -> GenIO -> IO VIResult

data VIResult = VIResult
  { viPostMeans   :: Params    -- posterior means
  , viPostSDs     :: Params    -- posterior SDs
  , viElboHistory :: [Double]  -- ELBO trajectory
  , viDraws       :: [Params]  -- posterior samples
  }
```

See [Variational Inference Guide](docs/05-variational-inference.md).

---

### `Stat.ModelSelect` — Model comparison (WAIC / PSIS-LOO)

Computes information criteria from a MCMC chain. Smaller is better.

```haskell
import Stat.ModelSelect

chainWAIC :: ModelP r -> Chain -> WAICResult
chainLOO  :: ModelP r -> Chain -> LOOResult

data WAICResult = WAICResult
  { waicValue :: Double  -- WAIC (smaller is better)
  , waicLppd  :: Double  -- log pointwise predictive density
  , waicPwaic :: Double  -- effective parameter count
  , waicSE    :: Double  -- standard error
  }

data LOOResult = LOOResult
  { looValue   :: Double    -- LOO-CV (smaller is better)
  , looKHat    :: [Double]  -- per-observation Pareto k̂ (> 0.7 is concerning)
  , looKHatBad :: Int       -- count of observations with k̂ > 0.7
  }

-- For frequentist models (LM / GLM / LME), posterior-sampling helpers:
lmPosteriorLogLiks  :: Matrix Double -> Vector Double -> FitResult -> Int -> GenIO -> IO [[Double]]
glmPosteriorLogLiks :: Family -> LinkFn -> ... -> IO [[Double]]
lmePosteriorLogLiks :: Matrix Double -> Vector Double -> [Double] -> FitResult -> Int -> GenIO -> IO [[Double]]
```

See [Model Comparison Guide](docs/06-model-comparison.md).

---

### `Viz.Bar` — Bar charts

```haskell
import Viz.Bar

barChartFile  HTML "bar.html"  cfg "category" "value" labels vals
barChartHFile HTML "barh.html" cfg "value" "category" labels vals
stackedBarFile HTML "stacked.html" cfg "x" "y" "group" xs ys groups
groupedBarFile HTML "grouped.html" cfg "x" "y" "group" xs ys groups
```

See [Visualization Guide](docs/07-visualization.md).

---

## Notes

- **Low ESS**: Inspect the trace plot for poor mixing and re-tune step sizes. NUTS gives substantially higher ESS/time than HMC.
- **High R-hat (≥ 1.01)**: Increase burn-in, disperse initial values, or adjust step sizes.
- **Non-root distribution display**: `collectNodes` continues with placeholder `0` for latent variables, so distribution parameters at non-root nodes aren't meaningful (only family names shown in graphs).
- **Test data**: place under `demo/` (not `/tmp`).
- **CPU parallelism**: `nutsChains` / `hmcChains` / `metropolisChains` use OS-thread parallelism with `+RTS -N`. Example: `cabal run hbm-example -- +RTS -N4`.
