# hgg integration — `toPlot` / `Plottable` / `module Hanalyze`

> 🌐 **English** | [日本語](03-plot-integration.ja.md)

> Related: [01-visualization.md](01-visualization.md) (existing one-shot `Hanalyze.Viz.*` plots) /
> [../regression/01-lm.md](../regression/01-lm.md) (LM) /
> hgg itself (`hgg/`, layer grammar, SVG/PNG backends).

A bridge that turns a fitted statistical model into a **`toPlot :: m -> VisualSpec`** and
overlays it on hgg's layer grammar (`df |>> (layer (scatter ..) <> toPlot fit)`).
This is the **model-out track** (analyze Phase 46 / plot Phase 15).

> ⚠ **Experimental + flag-isolated.** Built only when the cabal `flag plot-integration`
> (default **off**) is on. With the flag off, analyze stays a plot-independent standalone
> (upstream-hanalyze compatible). → read [§6 Build & dependencies](#6-build--dependencies) first.

---

## 1. What it solves

- **Goal**: take the output of `fitLM` / `fitGP` etc. and overlay a regression line /
  confidence band on a scatter plot.
- **Approach**: do not reinvent rendering. Convert the model to a `VisualSpec` (hgg's
  plot currency) and ride on the existing layer composition (`<>`) and DataFrame bind (`|>>`).
- **Dependency is one-way** `analyze → hgg-core`. plot knows nothing about analyze.

## 2. Quickstart (LM)

```haskell
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Vector              as V
import qualified Numeric.LinearAlgebra    as LA
import           Hgg.Plot.Backend.SVG (saveSVGBound)
import           Hgg.Plot.Frame       ((|>>))
import           Hgg.Plot.Spec        (ColData (..), layer, scatter)
import           Hanalyze.Plot            (lm, (|->), toPlot)

main :: IO ()
main = do
  let xs = [1,2,3,4,5,6,7,8] :: [Double]
      ys = [2.1,3.9,6.2,7.8,10.3,11.7,14.1,16.0]
      df = [ ("x", NumData (V.fromList xs))
           , ("y", NumData (V.fromList ys)) ]
      m  = df |-> lm "x" "y"                                -- ① fit (high-level verb)
      plot = df |>> (layer (scatter "x" "y") <> toPlot m)   -- ② scatter + line + CI band
  saveSVGBound "lm.svg" plot                                 -- ③ write SVG
```

- `scatter "x" "y"` plots the df columns; `toPlot m` is the `VisualSpec` for the regression
  line + 95% CI band.
- `<>` is the `VisualSpec` Monoid (no new combinator needed); `|>>` binds the df into a `BoundPlot`.

## 3. The `Plottable` protocol

```haskell
class Plottable m where
  toPlot          :: m -> VisualSpec     -- the headline figure (the layer to overlay)
  diagnosticPlots :: m -> [VisualSpec]   -- a bundle of diagnostics (default = [toPlot m])
```

It expresses only the final "can be drawn" capability. Model capabilities (predict / residuals)
live in the **neutral protocol** (`PredictiveModel` / `ResidualModel` in `Hanalyze.Model.Core`, §5).

### 3.1 `LMModel` (linear model)

`FitResult` (the numeric core) does not retain the design matrix X, which `confidenceBand`
needs, so a drawing-oriented type bundles X alongside the result.

```haskell
lmModel :: LA.Vector Double -> LA.Vector Double -> LMModel   -- (x, y) → fit + X bundled
```

`toPlot LMModel` = regression line through the x-sorted training points + the 95% mean-response
band from `confidenceBand`. `diagnosticPlots` = line + residuals-vs-fitted.

### 3.2 `GPResult` (Gaussian process)

`GPResult` (`Hanalyze.Model.GP`) is **self-contained** — it already holds the prediction grid,
posterior mean and credible band — so the **result type itself** is made `Plottable` without
bundling X (demonstrating the protocol holds for shapes other than `FitResult`).

```haskell
import Hanalyze.Model.GP (GPModel (..), Kernel (..), fitGP, defaultGPParams)

let gmod = GPModel RBF defaultGPParams
    gres = fitGP gmod trainX trainY grid     -- GPResult
    plot = gdf |>> (layer (scatter "x" "y") <> toPlot gres)
```

`toPlot GPResult` = the GP posterior-mean curve + the `mean ± 2σ` credible band.

> 💡 **Making the band visible**: with dense training points and `optimizeGP`, the GP nearly
> interpolates and the credible band collapses to a hairline. For demos use **sparse training
> points + no optimization (`defaultGPParams`)** so the band widens between/at the edges — the
> classic GP posterior picture.

### 3.3 `GLMMResultRE` (mixed-effects model ─ caterpillar plot)

A mixed-effects model (`GLMMResultRE` in `Hanalyze.Model.GLMM`, random intercept + slope) is
drawn as a **caterpillar plot**: each group's **random effect (BLUP) sorted ascending by value**,
laid out with the forest mark (horizontal bars), with a reference line at 0 (= zero deviation
from the fixed effect). Reading the spread across groups and spotting outlying groups at a glance
is the canonical GLMM-specific picture.

```haskell
import Hanalyze.Plot (glmmF, toPlot, (|->), Fit (..))

-- Fit y ~ x + (1|group) → (GLMMResultRE, fixed-effect coefficient names)
let Right (re, _) = fitEither (glmmF "y ~ x + (1|group)") df
    plot = noDf |>> toPlot re        -- caterpillar of column 0 (usually random intercept)
```

`toPlot GLMMResultRE` = the caterpillar for random-effect **column 0** (usually intercept).
`diagnosticPlots GLMMResultRE` = the list of caterpillars for all r columns (intercept + slopes).

> ⚠️ **No CI bars yet (points only)**: `GLMMResultRE` stores neither the per-group conditional
> variance nor the group sizes `n_j` (the scalar-only `glmmBLUPSE` is for `GLMMResult` and does
> not apply), so a BLUP standard error cannot be computed from the result alone. Once a conditional
> variance is stored, the forest error half-width can be filled in to draw bands (the forest mark
> already supports symmetric CIs).

## 4. `module Hanalyze` (quickstart entry point)

A single `import Hanalyze` re-exports the core (model fitting, descriptive stats, tests, effect
sizes, distributions, plotting, CSV I/O). The umbrella itself is **plot-independent** (no flag).

```haskell
import Hanalyze     -- Model.{Core,LM,GLM} + Stat.{Summary,Test,Effect,Distribution}
                    -- + Viz.{Core,Scatter,Bar,Histogram} + DataIO.CSV
```

> ⚠ **Name clash**: GLM's `Family` and `Hanalyze.Stat.Distribution` both export `Binomial` / `Poisson`.
> The umbrella favours GLM (`Poisson :: Family`). When you need the distribution value
> `Poisson λ`, import `Hanalyze.Stat.Distribution` directly.

## 5. Neutral protocol (portable)

The following in `Hanalyze.Model.Core` are plot-independent and **cherry-pickable upstream**
(`(hanalyze-portable)`).

```haskell
class PredictiveModel m where predictAt   :: m -> LA.Matrix Double -> LA.Matrix Double
class ResidualModel   m where residualsOf :: m -> LA.Vector Double
```

Instances exist for the shared `FitResult` (LM/GLM/GLMM). `Plottable` (plot-dependent) sits on top.

## 6. Build & dependencies

| | flag off (default) | flag on |
|---|---|---|
| `Hanalyze.Plot` | not built | built (depends on `hgg-core`/`-svg`) |
| standalone | ✅ plot-independent, upstream-compatible | analyze → plot-core (one-way) |

```bash
# the flag-on integration build root is cabal.project.plot
cabal build --project-file=cabal.project.plot hanalyze
cabal test  --project-file=cabal.project.plot hanalyze-plot-test
cabal run   --project-file=cabal.project.plot plot-integration-demo

# flag-off standalone regression (portable)
cabal test hanalyze-test
```

## 7. Viewer (no compare.html dependency)

`plot-integration-demo` writes `design/plot-integration/viewer.html`, a **self-contained HTML**
that embeds the HS-backend SVGs inline (no PS bundle / esbuild — just open it in a browser).
It tiles the integration figures (LM / GP) with plain examples (scatter / line / bar / hist).

For individual SVG text/files:

```haskell
import Hgg.Plot.Backend.SVG (renderBound, saveSVGBound)  -- BoundPlot → Text / file
import Hgg.Plot.Backend.SVG (renderSVG, saveSVG)         -- inline-column VisualSpec
```

## 8. Portability / cherry-pick discipline

- **portable** (`(hanalyze-portable)`, upstream candidate): the neutral protocol
  (`PredictiveModel` / `ResidualModel`) + the umbrella `module Hanalyze` (plot-independent).
- **non-portable** (do not cherry-pick): everything in `Hanalyze.Plot.*` (`Plottable` / `toPlot` /
  `LMModel` / `GPResult` instance). It depends on `hgg-core`, isolated under the flag.

## 9. A note on "equivalence" (vs geom_smooth)

- The **LM** CI band is `confidenceBand` (an X-based mean-response band, 95%), roughly
  **equivalent** to ggplot `geom_smooth(method="lm")`'s CI ribbon (the line is the same OLS fit).
- **GP is different**: the line is the **GP posterior mean** (not a loess/lm smoother), and the
  band is the **Bayesian `mean ± 2σ` credible band** (~95%, not a frequentist CI). So **do not**
  call the GP plot "geom_smooth-equivalent" — it reuses the same drawing machinery
  (`regressionLineCI`) but the band's statistical meaning differs by model.

## 10. Branch B: stat-in (ggplot-style)

Sections 1-9 cover **Branch A** (`toPlot`): build a model first, then plot it. **Branch B** instead
writes the stat *inside* the plot grammar — like ggplot's `geom_smooth(method="lm")` — and
**delegates the regression to analyze**.

Provided by `Hgg.Plot.Bridge.Stat` in `hgg-analyze-bridge` (the reverse edge
`plot → analyze` lives in this isolated package; `hgg-core` stays analyze-independent, so
there is no cycle).

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Hgg.Plot.Backend.SVG (saveSVGWith)
import Hgg.Plot.Bridge.Stat (compileStats, lm, smooth)
import Hgg.Plot.Spec        (ColData(..), Resolver, layer, scatter)
import qualified Data.Vector as V

main :: IO ()
main = do
  let xs = V.fromList [1..30] ; ys = V.fromList (map (\x -> 2*x + 3) [1..30])
      r name | name == "x" = Just (NumData xs)
             | name == "y" = Just (NumData ys)
             | otherwise   = Nothing
      -- scatter + regression line + 95% confidence band
      spec = layer (scatter "x" "y") <> compileStats r [lm "x" "y"]
  saveSVGWith "lm-stat-in.svg" r spec
```

- `lm "x" "y"` = `parseModel "y ~ x"` + `fitLMF` (the [Formula DSL](../regression/11-formula-dsl.md))
  for the fit + `confidenceBand` for the 95% band → plot-core `line` + `band`.
  **The regression is delegated to analyze** (= ggplot `geom_smooth(method="lm")`).
- `smooth "x" "y" n` = `y ~ bs(x,n)` B-spline **curve only** (no band).
- `compileStats :: Resolver -> [Stat] -> VisualSpec`. The Resolver is available at bind/render time,
  so compose as `layer scatter <> compileStats r [...]`.

> On the signature: the plan had `compileStats :: Resolver -> VisualSpec -> VisualSpec` (embedding the
> stat in a VisualSpec), but that needs a new `MarkKind` in plot-core = HS/PS renderer + JSON codec
> parity work. Instead the stat tag and resolution live in the bridge as `Resolver -> [Stat] ->
> VisualSpec` (no changes to plot-core types, renderers, or PS parity).

> On "equivalence": the `lm` band is roughly equivalent to `geom_smooth(method="lm")` CI (narrow at
> the center, wider at the ends). `smooth` is curve-only (no band), so it differs from geom_smooth's
> ribbon (same discipline as GAM in §9).
