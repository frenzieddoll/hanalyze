# hanalyze-plot

The **integration layer** between [`hanalyze`](../README.md) and the
sibling project **hgg**. Its 8 modules provide `toPlot` /
`Plottable`, which turn a fitted analysis model into an hgg
`VisualSpec`.

Its dependencies differ from the other layers in two ways:

- It sits **above the umbrella package `hanalyze`** (it imports `Fit`
  and `Wrappers`). Pointing the dependency the other way — umbrella → plot —
  would create a package cycle, and the cabal file says so explicitly.
- It depends on `hgg-{core,svg,3d,custom}` from a sibling repo, so
  it is **not part of the default `cabal.project`**. Build it through the
  dedicated build root `cabal.project.plot`:

```bash
cabal build --project-file=cabal.project.plot hanalyze-plot
cabal test  --project-file=cabal.project.plot hanalyze-plot-test
```

> **If you want Vega-Lite figures and HTML reports**, use
> [`hanalyze-viz`](../hanalyze-viz/README.md) instead. This
> package targets static SVG / PDF / PNG rendering.

## Main modules (all 8)

| Module | Role |
|---|---|
| `Hanalyze.Plot` | Entry point of the integration layer; re-exports the instances below together with the `Fit` API (`\|->`, `lm`, `glm`, …) |
| `Plot.Core` | Model-family-agnostic skeleton — `Plottable` / `SingleVarModel` / `MultiVarModel` and the grid evaluation core |
| `Plot.Linear` | Instances for the linear family (`LMModel`, `GLMModel`, `WeightedLMModel`, …) |
| `Plot.Bayes` | Instances for the Bayesian / HBM family (`ChainModel`, `ForestSpec`, `PPCSpec`, `DagSpec`, `GLMMResultRE`) plus extractors |
| `Plot.ML` | Instances and extractors for the ML / statistical model family |
| `Plot.Robust` | Instances for robust and quantile regression |
| `Plot.Smooth` | Instances for smoothing and kernel methods |
| `Plot.Wrappers` | Instances for the generic wrapper types (`MultiFit`, `RegModel`) |

There is exactly one core type class, in `Plot.Core`:

```haskell
class Plottable m where
  -- | The one representative figure (composable with other layers via <>).
  toPlot          :: m -> VisualSpec

  -- | A bundle of diagnostic figures (for reports); defaults to just toPlot.
  diagnosticPlots :: m -> [VisualSpec]
  diagnosticPlots m = [toPlot m]
```

Support for a new model type is therefore a single instance away.

## Usage

```cabal
build-depends: hanalyze-plot
```

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Graphics.Hgg.Frame       ((|>>))
import Graphics.Hgg.Spec        (layer, scatter)
import Graphics.Hgg.Backend.SVG (saveSVGBound)
import Hanalyze.Plot     (toPlot, statModel, grid, (|->), lm)

main :: IO ()
main = do
  let m       = df |-> lm "x" "y"
      lmPlot  = df |>> (layer (scatter "x" "y") <> toPlot m)
  saveSVGBound "lm-scatter-ci.svg" lmPlot
```

The flow is a single line: `df |-> lm "x" "y"` (fit) → `toPlot` (figure) →
`|>>` to overlay it on other layers. Before handing a model to `toPlot` you
can **compose rendering options** onto it, e.g. `statModel m <> grid 200`
(grid resolution, band type via `bandMode` / `piMethod`, colour via
`statColor`, …).

This is the exact path used by the `plot-integration-demo` executable
(`hanalyze-demos/demo-plot/PlotIntegrationDemo.hs`), which walks
through LM, GLM, spline, GP and quantile regression examples:

```bash
cabal run --project-file=cabal.project.demos plot-integration-demo
```

## Tests

The `hanalyze-plot-test` suite (`test-plot/Spec.hs`) checks the structure and
numerics of `toPlot` output per model type. It used to live in the umbrella
package and was moved here in Phase 106.4, because an umbrella component
depending on this package would close a cycle.

## Related docs

- Static-rendering integration: [docs/visualization/03-plot-integration.md](../docs/visualization/03-plot-integration.md)
- Visualization overview: [01-visualization.md](../docs/visualization/01-visualization.md)
- API reference: [api-guide/12-plot.md](../docs/api-guide/12-plot.md)

← [repository README](../README.md)
