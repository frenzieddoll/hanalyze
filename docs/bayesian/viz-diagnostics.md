# MCMC Diagnostics Viz Guide (Hanalyze.Viz.MCMC)

> 🌐 **English** | [日本語](viz-diagnostics.ja.md)
>
> A guide to common **convergence diagnostics viz / posterior predictive viz / summary tables** across major samplers like NUTS / HMC / mean-field ADVI / Full-rank ADVI, all in one place.
>
> For model specification, see [02-probabilistic-model.md](02-probabilistic-model.md);
> for model comparison, see [06-model-comparison.md](06-model-comparison.md).
>
> **Two output paths**: ① `Hanalyze.Viz.MCMC` = VegaLite/HTML (interactive, bundled into one page via `renderReport`), ② `*Of` extractors in `Hanalyze.Plot` = hgg SVG (main flow of `df |-> hbm`, composed with `toPlot`). The correspondence table below shows a 1:1 mapping.

## Provided Diagnostics Viz / Tables

| Function | PyMC / ArviZ Equivalent | Use |
|---|---|---|
| `tracePlot` / `tracePlotFile` | `az.plot_trace` | Value evolution in each chain |
| `tracePlotHDI` / `tracePlotHDIFile` | `az.plot_trace` + HDI band | Trace with HDI overlay |
| `posteriorPlot` / `posteriorPlotFile` | `az.plot_posterior` | Posterior KDE |
| `pairScatter` / `pairScatterFile` | `az.plot_pair` | 2-parameter joint scatter |
| `pairScatterDiv` / `pairScatterDivFile` | `az.plot_pair(divergences=True)` | NUTS divergent overlaid on scatter |
| `autocorrPlot` / `autocorrPlotFile` | `az.plot_autocorr` | Autocorrelation (lag 0..N) |
| `forestPlot` / `forestPlotFile` | `az.plot_forest` | Forest plot of multiple parameters |
| `energyPlot` / `energyPlotFile` | `az.plot_energy` | NUTS energy + BFMI |
| `rankPlot` / `rankPlotFile` | `az.plot_rank` | Rank uniformity across chains, complements R̂ |
| `ppcPlot` / `ppcPlotFile` | `az.plot_ppc` | Posterior predictive vs observed overlay |
| `mcmcDiagnostics` / `mcmcDiagnosticsFile` | — | Trace + KDE set (1 chain) |
| `mcmcDiagnosticsMulti` / `mcmcDiagnosticsMultiFile` | — | Overlaid trace + KDE of multiple chains |
| `posteriorSummary` (`Hanalyze.Stat.Summary`) | `az.summary` | mean / SD / HDI / ESS / R̂ table |
| `posteriorSummaryHtml` / `posteriorSummaryFile` | — | Above as HTML table |
| `printPosteriorSummary` | — | Above as stdout text |
| `hbmSummary` / `printHBMSummary` (`Hanalyze.Model.Wrappers`) | `az.summary(idata)` | Quick summary from trained `HBMModel` (latent + deterministic, no manual wiring) |
| `hbmSummaryDf` | DataFrame version of `az.summary(...)` | Above as `DataFrame` (param / mean / sd / hdi_lo / hdi_hi / ess_bulk, + r_hat when multi-chain) |
| `hbmDrawsDf` | `idata.posterior` → DataFrame | Posterior draws as `DataFrame` (1 parameter per column, chains concatenated). Passes to `Hanalyze.Data.Wrangle` `summarise` / `groupBy` etc. |

## Plot Extractor Path (hgg SVG) Diagnostics Figures

The table above shows the `Hanalyze.Viz.MCMC` (VegaLite/HTML) path. The **`Hanalyze.Plot` (plot-integration flag, hgg SVG) path** uses `*Of` extractors that directly take `HBMModel`, fitting naturally into the `df |-> hbm` workflow (composable via `toPlot` / `<>`). Phase 73 added `autocorrOf` / `rankOf`, achieving **symmetric coverage between HTML and SVG diagnostic figures**.

### Viz (HTML) ↔ Plot (SVG) Correspondence Table

| Diagnostic | Viz.MCMC (HTML) | Plot Extractor (SVG) |
|---|---|---|
| trace | `tracePlot` / `multiTracePlot` | `tracesOf` / `tracesOfWith` (`toByChain`) |
| Marginal posterior density | `posteriorPlot` | `marginalsOf` / `marginalsByChainOf` |
| Forest (HDI) | `forestPlot` | `forestOf` |
| Posterior predictive (PPC) | `ppcPlot` | `ppcOf` |
| Autocorrelation | `autocorrPlot` | `autocorrOf` (Phase 73.1) |
| Rank plot | `rankPlot` | `rankOf` (Phase 73.2, requires ≥2 chains) |
| Pair (divergences) | `pairScatterDiv` | `pairOf` |
| Energy | `energyPlot` | `energyOf` (BFMI numeric via `bfmi`) |
| DAG structure | — | `dagOf` / `dagOfRaw` (equivalent to `pm.model_to_graphviz`) |
| Divergence index | — | `divergencesOf` / `tracesOf` (divergence rug on by default) |
| Summary table | `posteriorSummary` | — (table, not a figure) |

> Rank-normalized histogram computation is centralized in `Stat.MCMC.rankHist` and shared by both Viz and Plot paths (no double implementation). `autocorrOf` material derives from low-level `Stat.MCMC.autocorr`.

```haskell
fit = df |-> hbm (defaultHBM { hbmSeed = Just 42 }) model
saveSVG "trace_div.svg" (subplots (tracesOfWith defaultTraceOpts { toByChain = True } fit)
                           <> selectPanels ["tau_b1", "b1_2"] <> subplotCols 1)
saveSVG "pair.svg"   (head (pairOf fit [("tau_b1", "b1_2")]))
saveSVG "energy.svg" (energyOf fit)
```

`tracesOf` has divergence rug **on by default**, so models with divergences show divergent draw positions as red vertical bars along each panel's bottom (identical to ArviZ `plot_trace`). The example below is centered 8-schools (funnel causes divergences in NUTS's small τ region, typical for funnels), fit with 4 chains (showing τ + 2 schools). Divergences concentrate near τ → 0 (the funnel's neck), visible in the rug:

![Divergences in centered 8-schools (tracesOf, divergence rug on by default)](../images/hbm-trace-divergent.svg)

Looking at the same funnel fit via `pairOf` (joint scatter with divergence emphasis) and `energyOf` (energy distribution), divergences concentrate at the funnel's neck and BFMI is low. Marginal posterior density per parameter via `marginalsOf`:

![Divergence pair plot (pairOf): divergences (red) concentrate at the funnel neck τ–θ](../images/hbm-pair.svg)

![Energy (energyOf): ΔE (orange) narrower than marginal E (blue) = low BFMI (insufficient exploration)](../images/hbm-energy.svg)

![Marginal posterior density (marginalsOf): posterior distribution per parameter](../images/hbm-marginals.svg)

Complete usage example in umbrella `experiments/hbm-damped-saturation/ExampleHighLevel.hs` (canonical high-level API implementation). For divergence index per chain, read `chainDivergences` directly (0-based, post-burn-in, Stan-style criterion |ΔH| > 1000).

A compact 2×2 diagnostic dashboard via `dashboardOf m "obs"` gives an overview (structure `dagOf` / estimates `forestOf` / fit `ppcOf` / sampler health `energyOf`):

![HBM Diagnostics Dashboard (compact 2×2)](../images/hbm-dashboard.svg)

The thorough `dashboardFullOf m "obs"` — top 2×2 + per-parameter [posterior density | trace] (more coefficients = more rows below):

![HBM Diagnostics Dashboard (full)](../images/hbm-dashboard-full.svg)

Individual extractor figures — model structure (`dagOf`) / trace (`tracesOf`, 1 panel per param) / 94% HDI forest (`forestOf`) / posterior predictive check (`ppcOf`) / posterior prediction (`epred`):

![Model structure DAG (dagOf)](../images/hbm-dag.svg)

![Trace plot (tracesOf, 1 panel per param)](../images/hbm-trace.svg)

![94% HDI forest (forestOf)](../images/hbm-forest.svg)

![Posterior predictive check PPC (ppcOf)](../images/hbm-ppc.svg)

![Posterior prediction (epred, scatter + posterior mean)](../images/hbm-epred.svg)

Convergence diagnostics — autocorrelation (`autocorrOf`) / rank plot (`rankOf`, chain uniformity):

![Autocorrelation (autocorrOf)](../images/hbm-autocorr.svg)

![Rank plot (rankOf, chain dodge)](../images/hbm-rank.svg)

### Generating These Figures (`df |-> hbm` + Extractors)

Each figure above applies one extractor to the fitted `HBMModel`. Since the model holds its posterior, most extractors need no dataframe except `epred` (overlays posterior mean on observation scatter):

```haskell
import Hanalyze.Plot     (hbm, defaultHBM, (|->), toPlot, epred, tracesOf, forestOf
                               , dagOf, ppcOf, autocorrOf, rankOf)
import Hgg.Plot.Spec        (ColData (..), layer, scatter, subplots, subplotCols, vconcat)
import Hgg.Plot.Frame       ((|>>))
import Hgg.Plot.Backend.SVG (saveSVGBound)
import Data.Text                (Text)

-- df = [("x", NumData …), ("y", NumData …)]; `model :: ModelP ()` is your program
let m    = df |-> hbm defaultHBM model
    noDf = [] :: [(Text, ColData)]
saveSVGBound "hbm-dag.svg"    (noDf |>> toPlot (dagOf m))             -- Structure DAG
saveSVGBound "hbm-trace.svg"  (noDf |>> vconcat (tracesOf m))  -- 1 panel per param
saveSVGBound "hbm-forest.svg" (noDf |>> toPlot (forestOf m))          -- 94% HDI forest
saveSVGBound "hbm-ppc.svg"    (noDf |>> toPlot (ppcOf m "y"))         -- PPC ("y" = observe name)
saveSVGBound "hbm-epred.svg"  (df   |>> (layer (scatter "x" "y") <> toPlot (epred m "x" "mu")))
-- Convergence diagnostics (1 figure per param, stacked vertically with vconcat):
saveSVGBound "hbm-autocorr.svg" (noDf |>> vconcat (autocorrOf m))
saveSVGBound "hbm-rank.svg"     (noDf |>> vconcat (rankOf m))
```

`ppcOf` is pure (`ppcOf m "y" :: PPCSpec`, IO version `ppcOfIO` also available). `tracesOf m :: [VisualSpec]` (1 panel per param, bundle with `subplots`; divergence rug on by default, overlay by chain via `tracesOfWith defaultTraceOpts { toByChain = True }`). `epred m "x" "mu"` evaluates deterministic mean `"mu"` along `"x"`. Default shows posterior HDI (= **CI** equivalent) band, but switch to **prediction interval (PI)** with observation noise via `<> bandMode BandPI` (equivalent to frequentist notation), or nested fan chart with `<> bandMode BandCIPI` (PI auto-detects the observation node's predictive distribution from the model, samples it with fixed seed for determinism).

## Typical Usage Patterns

### Run 4 chains with NUTS and emit comprehensive HTML report

```haskell
import Hanalyze.MCMC.NUTS  (nutsChains, defaultNUTSConfig)
import Hanalyze.Viz.Report (defaultReport, renderReport, reportChains)

main = do
  gen <- createSystemRandom
  chs <- nutsChains model defaultNUTSConfig 4 initParams gen
  let rep = (defaultReport "My Model — 4-chain" (head chs) sampleVarNames)
              { reportChains = chs }
  renderReport "model.html" rep
```

`renderReport` bundles trace / posterior / pair / autocorr / forest / energy / rank / posterior summary into a single HTML. For individual viz only, call individual functions as below.

### Rank plot (inter-chain convergence diagnostics)

```haskell
import Hanalyze.Viz.MCMC (rankPlotFile)
import Hanalyze.Viz.Core (defaultConfig, OutputFormat (..))

let cfg = defaultConfig "Rank plot — Δ mu_alpha"
rankPlotFile HTML "rank_mu.html" cfg 20 ["mu_alpha"] chs
-- nBins = 20 is PyMC's default
```

Requires ≥2 chains (1 chain gives uniform ranks).

### Divergence scatter (visualizing NUTS step failures)

```haskell
import Hanalyze.Viz.MCMC (pairScatterDivFile)

-- Overlay divergent points on joint scatter of mu and tau
pairScatterDivFile HTML "div_mu_tau.html" cfg "mu" "tau" chain
```

Hierarchical models often show divergences skewed toward the funnel in μ–τ scatter, a **sign to use non-centered parameterization** ([02-probabilistic-model.md Pattern 4, Form C](02-probabilistic-model.md)).

### Posterior predictive check (PPC)

```haskell
import Hanalyze.Stat.PosteriorPredictive (posteriorPredictive)
import Hanalyze.Viz.MCMC (ppcPlotFile)

predDraws <- posteriorPredictive model chain gen
ppcPlotFile HTML "ppc.html" cfg observed predDraws 50
-- nOverlay = 50 predictive samples overlaid on observed KDE
```

### Posterior summary table (az.summary equivalent)

```haskell
import Hanalyze.Viz.MCMC (printPosteriorSummary, posteriorSummaryFile)

-- Table to stdout
printPosteriorSummary ["mu", "tau", "theta_1"] [chain]

-- HTML table
posteriorSummaryFile HTML "summary.html" cfg ["mu", "tau"] [chain]
```

Shows mean / SD / 2.5% / 97.5% / ESS / R̂ (R̂ valid when ≥2 chains).

From a trained `HBMModel` (`hbmModelPure` / result of `df |-> hbm`), one-liner helpers skip name enumeration and manual chain wiring. Quantities derived from `deterministic` are included by default (Phase 103):

```haskell
import Hanalyze.Model.Wrappers (printHBMSummary, hbmSummaryDf, hbmDrawsDf)
import qualified Hanalyze.Data.Wrangle as W

printHBMSummary m                 -- az.summary-style stdout table
df  = hbmSummaryDf m              -- Same table as DataFrame
drs = hbmDrawsDf m                -- Posterior draws (1 param per column)
W.summarise ["mu_mean" W.=: W.meanOf "mu"] drs   -- Free-form aggregation
```

## Hierarchical Model-Specific Diagnostics

| Symptom | Viz to Check | Remedy |
|---|---|---|
| Chains separated (R̂ > 1.05) | `tracePlot` / `rankPlot` | Lower step size / increase iterations |
| Funnel (μ–τ pair funnel-shaped) | `pairScatterDiv` | **Switch to non-centered parameterization** |
| BFMI < 0.3 | `energyPlot` | Consider reparameterization, non-centering, etc. |
| Divergences among group parameters | `pairScatter` per group | Weaken group-level prior to weakly informative / non-center |
| PPC deviates from observed | `ppcPlot` | Model mis-specification, consider family / link / overdispersion |

Detailed hierarchical model examples: [demos.md](demos.md).
