# MCMC diagnostic viz guide (Hanalyze.Viz.MCMC)

> 🌐 **English** | [日本語](viz-diagnostics.ja.md)
>
> A consolidated reference for the **convergence diagnostic viz / posterior
> predictive viz / summary tables** shared across the major samplers
> (NUTS / HMC / mean-field ADVI / full-rank ADVI).
>
> For how to write models see
> [02-probabilistic-model.md](02-probabilistic-model.md); for model
> comparison see [06-model-comparison.md](06-model-comparison.md).

## Diagnostic viz / tables provided

| Function | PyMC / ArviZ equivalent | Use |
|---|---|---|
| `tracePlot` / `tracePlotFile` | `az.plot_trace` | per-chain value evolution |
| `tracePlotHDI` / `tracePlotHDIFile` | `az.plot_trace` + HDI band | trace overlaid with HDI |
| `posteriorPlot` / `posteriorPlotFile` | `az.plot_posterior` | posterior KDE |
| `pairScatter` / `pairScatterFile` | `az.plot_pair` | 2-parameter joint scatter |
| `pairScatterDiv` / `pairScatterDivFile` | `az.plot_pair(divergences=True)` | overlay NUTS divergences on scatter |
| `autocorrPlot` / `autocorrPlotFile` | `az.plot_autocorr` | autocorrelation (lag 0..N) |
| `forestPlot` / `forestPlotFile` | `az.plot_forest` | forest plot listing multiple parameters |
| `energyPlot` / `energyPlotFile` | `az.plot_energy` | NUTS energy + BFMI |
| `rankPlot` / `rankPlotFile` | `az.plot_rank` | complements R̂ via rank uniformity across chains |
| `ppcPlot` / `ppcPlotFile` | `az.plot_ppc` | posterior predictive vs observation overlay |
| `mcmcDiagnostics` / `mcmcDiagnosticsFile` | — | trace + KDE set (1 chain) |
| `mcmcDiagnosticsMulti` / `mcmcDiagnosticsMultiFile` | — | overlaid trace + KDE across multiple chains |
| `posteriorSummary` (`Hanalyze.Stat.Summary`) | `az.summary` | mean / SD / HDI / ESS / R̂ table |
| `posteriorSummaryHtml` / `posteriorSummaryFile` | — | emit the above as an HTML table |
| `printPosteriorSummary` | — | stdout text version of the above |
| `hbmSummary` / `printHBMSummary` (`Hanalyze.Model.Wrappers`) | `az.summary(idata)` | one-shot summary from a fitted `HBMModel` (latent + deterministic, no manual wiring) |
| `hbmSummaryDf` | `az.summary(...)` as DataFrame | the above as a `DataFrame` (param / mean / sd / hdi_lo / hdi_hi / ess_bulk, + r_hat when multi-chain) |
| `hbmDrawsDf` | `idata.posterior` → DataFrame | posterior draws as a `DataFrame` (one column per parameter, chains concatenated) — feed `Hanalyze.Data.Wrangle` verbs (`summarise`, `groupBy`, …) |

## Diagnostic figures (hgg SVG)

The `Hanalyze.Plot` path (flag `plot-integration`) exposes per-extractor
diagnostics that take an `HBMModel` directly, so they ride the `df |-> hbm` flow.
First, `dashboardOf m "obs"` gives a compact 2×2 at-a-glance dashboard — structure
(`dagOf`) / estimates (`forestOf`) / fit (`ppcOf`) / sampler health (`energyOf`):

![HBM diagnostic dashboard (compact 2×2)](../images/hbm-dashboard.svg)

For a thorough check including convergence, `dashboardFullOf m "obs"` — the same 2×2
on top, then per-parameter [posterior | trace] below (more coefficients just add
rows):

![HBM diagnostic dashboard (full)](../images/hbm-dashboard-full.svg)

The individual extractors — model structure (`dagOf`), trace (`tracesOf`, one
panel per parameter), 94% HDI forest (`forestOf`), posterior predictive check
(`ppcOf`) and posterior prediction (`epred`):

![Model structure DAG (dagOf)](../images/hbm-dag.svg)

![Trace plot (tracesOf, one panel per parameter)](../images/hbm-trace.svg)

![94% HDI forest (forestOf)](../images/hbm-forest.svg)

![Posterior predictive check (ppcOf)](../images/hbm-ppc.svg)

![Posterior prediction (epred, scatter + posterior mean)](../images/hbm-epred.svg)

Convergence diagnostics — autocorrelation (`autocorrOf`) and rank plot (`rankOf`,
chain uniformity). Added in Phase 73, these close the autocorr/rank gap so the
SVG path now matches the HTML (`Viz.MCMC`) coverage:

![Autocorrelation (autocorrOf)](../images/hbm-autocorr.svg)

![Rank plot (rankOf, chains dodged)](../images/hbm-rank.svg)

### Generating these figures (`df |-> hbm` + extractors)

Each figure above is one extractor on the fitted `HBMModel`. The model holds its
own posterior, so every extractor needs no data frame (`noDf`) except `epred`,
which overlays the posterior mean on the observed scatter:

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
saveSVGBound "hbm-dag.svg"    (noDf |>> toPlot (dagOf m))             -- structure DAG
saveSVGBound "hbm-trace.svg"  (noDf |>> vconcat (tracesOf m))  -- one panel per parameter
saveSVGBound "hbm-forest.svg" (noDf |>> toPlot (forestOf m))          -- 94% HDI forest
saveSVGBound "hbm-ppc.svg"    (noDf |>> toPlot (ppcOf m "y"))         -- PPC ("y" = the observe name)
saveSVGBound "hbm-epred.svg"  (df   |>> (layer (scatter "x" "y") <> toPlot (epred m "x" "mu")))
-- convergence diagnostics (one figure per parameter, bundled via vconcat):
saveSVGBound "hbm-autocorr.svg" (noDf |>> vconcat (autocorrOf m))
saveSVGBound "hbm-rank.svg"     (noDf |>> vconcat (rankOf m))
```

`ppcOf` is pure (`ppcOf m "y" :: PPCSpec`; an `IO` twin `ppcOfIO` also exists);
`tracesOf m :: [VisualSpec]` (one panel per parameter, bundled via `subplots`;
divergence rug is on by default — use `tracesOfWith defaultTraceOpts { toByChain
= True }` for per-chain overlay); `epred m "x" "mu"` evaluates the deterministic
mean `"mu"` along `"x"`. Its default band is the posterior HDI of μ (the
frequentist **CI**); `<> bandMode BandPI` switches to the observation-noise
**predictive interval (PI)** and `<> bandMode BandCIPI` to a nested fan chart
(same spelling as the frequentist API; PI auto-detects the observation node's
predictive distribution and samples it under a fixed seed, so it stays
deterministic).

Because the divergence rug is **on by default**, a model with divergences draws a
red tick at the bottom of each panel marking the divergent draws (same as ArviZ
`az.plot_trace`). Below is a centered eight-schools fit (a funnel: NUTS diverges
where τ approaches zero) over 4 chains, with τ and two schools selected. The rug
shows divergences clustering at the funnel neck:

![Centered eight-schools divergences (tracesOf, rug on by default)](../images/hbm-trace-divergent.svg)

The same funnel fit through `pairOf` (joint scatter + divergence highlight) and
`energyOf` (energy distribution) shows divergences clustering in the funnel neck
and a low BFMI. Marginal posteriors come from `marginalsOf` (one per parameter):

![Divergence pair plot (pairOf): divergences cluster in the τ–θ funnel neck](../images/hbm-pair.svg)

![Energy (energyOf): ΔE (orange) narrower than marginal E (blue) = low BFMI](../images/hbm-energy.svg)

![Marginal posteriors (marginalsOf): one density per parameter](../images/hbm-marginals.svg)

## Typical usage patterns

### Run 4 NUTS chains and emit an HTML report covering everything

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

`renderReport` consolidates trace / posterior / pair / autocorr / forest /
energy / rank / posterior summary into a single HTML. When you want only
individual viz, call the per-function entry points below.

### Rank plot (cross-chain convergence diagnostic)

```haskell
import Hanalyze.Viz.MCMC (rankPlotFile)
import Hanalyze.Viz.Core (defaultConfig, OutputFormat (..))

let cfg = defaultConfig "Rank plot — Δ mu_alpha"
rankPlotFile HTML "rank_mu.html" cfg 20 ["mu_alpha"] chs
-- nBins = 20 is PyMC's default
```

Requires ≥ 2 chains (with only 1, the rank is trivially uniform).

### Divergence scatter (visualise NUTS step failures)

```haskell
import Hanalyze.Viz.MCMC (pairScatterDivFile)

-- Overlay divergent points on the joint of mu and tau
pairScatterDivFile HTML "div_mu_tau.html" cfg "mu" "tau" chain
```

In hierarchical models, divergences often cluster in the funnel-prone μ-τ
scatter, which is a sign that you should use a **non-centered
parametrisation**
([02-probabilistic-model.md pattern 4 Form C](02-probabilistic-model.md)).

### Posterior predictive check (PPC)

```haskell
import Hanalyze.Stat.PosteriorPredictive (posteriorPredictive)
import Hanalyze.Viz.MCMC (ppcPlotFile)

predDraws <- posteriorPredictive model chain gen
ppcPlotFile HTML "ppc.html" cfg observed predDraws 50
-- nOverlay = 50 predictive samples overlaid on the observed KDE
```

### Posterior summary table (`az.summary` equivalent)

```haskell
import Hanalyze.Viz.MCMC (printPosteriorSummary, posteriorSummaryFile)

-- stdout table
printPosteriorSummary ["mu", "tau", "theta_1"] [chain]

-- HTML table
posteriorSummaryFile HTML "summary.html" cfg ["mu", "tau"] [chain]
```

Columns are mean / SD / 2.5% / 97.5% / ESS / R̂ (R̂ is meaningful only
when chain count ≥ 2).

With a fitted `HBMModel` (from `hbmModelPure` / `df |-> hbm`), the one-shot
helpers skip the manual name/chain wiring and include `deterministic`
derived quantities by default (Phase 103):

```haskell
import Hanalyze.Model.Wrappers (printHBMSummary, hbmSummaryDf, hbmDrawsDf)
import qualified Hanalyze.Data.Wrangle as W

printHBMSummary m                 -- az.summary-style stdout table
df  = hbmSummaryDf m              -- same table as a DataFrame
drs = hbmDrawsDf m                -- draws: 1 column per parameter
W.summarise ["mu_mean" W.=: W.meanOf "mu"] drs   -- free-form aggregation
```

## Diagnostic guidelines specific to hierarchical models

| Symptom | viz to look at | Remedy |
|---|---|---|
| Chains separate (R̂ > 1.05) | `tracePlot` / `rankPlot` | lower step size / increase iterations |
| Funnel (μ-τ pair is funnel-shaped) | `pairScatterDiv` | switch to **non-centered parametrisation** |
| BFMI < 0.3 | `energyPlot` | consider reparameterisation, non-centering, etc. |
| Divergences between per-group parameters | per-group pair via `pairScatter` | weaken group-level prior / non-centered |
| PPC deviates from observation | `ppcPlot` | model mis-specification; consider family / link / overdispersion |

Detailed hierarchical-model examples: [demos.md](demos.md).
