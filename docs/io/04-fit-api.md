# Fitting models from a DataFrame: `df |-> spec`

> 🌐 **English** | [日本語](04-fit-api.ja.md)

`hanalyze` exposes a single verb, `(|->)`, for fitting **any** model from **any** data
source. It is the Haskell counterpart of R's `lm(y ~ x, data = df)` experience.

```haskell
df |-> lm "x" "y"                      -- simple LM (two columns)   → LMModel
df |-> glm Gaussian Identity "x" "y"   -- two-variable GLM          → GLMModel
df |-> lmF  "y ~ x1 + x2"              -- formula LM (R-style)       → MultiLMModel
df |-> glmF Poisson Log "y ~ x1 + x2"  -- formula GLM                → MultiGLMModel
df |-> glmmF "y ~ x + (1|g)"           -- linear mixed model         → (GLMMResultRE, [Text])
df |-> hbm defaultHBM model            -- HBM (hand-written program) → HBMModel
```

There is **no new numerics** here — every spec is a thin wrapper that pulls the columns it
needs and calls the existing fit function (`lmModel`, `multiLMModel`, `hbmModelPure`, …).

## Data sources: `ColumnSource`

Any type that can answer "give me this column as numbers" is a data source.

```haskell
class ColumnSource d where
  lookupCol   :: Text -> d -> Maybe [Double]   -- numeric column (Nothing if absent)
  columnNames :: d -> [Text]
  toFrame     :: d -> DataFrame                -- for the formula path (Phase 47)
```

| Source | Where |
|---|---|
| `[(Text, [Double])]` | core (portable) |
| `Map Text [Double]` | core (portable) |
| `DataFrame` (Hackage `dataframe`) | core (portable) |
| `[(Text, ColData)]` (hgg) | flag `plot-integration` |

The `DataFrame` source keeps `toFrame = id`, so factor columns and missing values flow
through the **Phase 47** path (`MissingPolicy` / contrast / response detection) unchanged.
The assoc / `Map` sources are numeric by construction.

## Two-variable shortcut vs. formula

| Builder | Kind | Result type |
|---|---|---|
| `lm "x" "y"`, `glm`, `spline`, `robust`, `quantile` | two columns (convenience) | single-variable model → `toPlot` route 1 |
| `lmF`, `glmF`, `glmmF "y ~ …"` | R-style formula | multi-variable model → effect plot |

The `F` suffix means *formula*. Two-variable builders return single-variable model types
(`LMModel` etc.) so they plug straight into the route-1 plotting combinators.

## pure vs. total

```haskell
fitWith   :: (ColumnSource d, Fit spec) => spec -> d -> Fitted spec                 -- pure (error on failure)
fitEither :: (ColumnSource d, Fit spec) => spec -> d -> Either String (Fitted spec) -- total
(|->)     :: (ColumnSource d, Fit spec) => d -> spec -> Fitted spec                 -- = fitWith spec d
```

`(|->)` / `fitWith` are **pure but not total**: a missing column or a formula parse error is
an `error` (same convention as the pure samplers in Phase 50). For validation pipelines use
`fitEither`, which returns `Left msg`.

## IO verb with progress: `df |->! spec`

```haskell
fitIO  :: (ColumnSource d, Fit spec) => spec -> d -> IO (Fitted spec)  -- default = pure . fitWith
(|->!) :: (ColumnSource d, Fit spec) => d -> spec -> IO (Fitted spec)  -- = fitIO spec d
```

MCMC training takes seconds to minutes and the pure verb is silent by
construction. `(|->!)` (Phase 61) is the IO twin: for `hbm` specs it renders a
one-line progress display on stderr while sampling —

```
chains 2/4 done | draw 3400/8000 (warmup) | div 12 | 380.0 it/s
```

(`\r`-overwritten on a terminal, one line per 10% on a non-TTY). The choice of
verb *is* the choice of side effect: `|->` is pure and silent, `|->!` shows
progress. The result is **bit-identical** to the pure verb with the same
config (the chain seeds are derived by the same rule), so you can prototype
interactively with `|->!` and keep `|->` in tests and pipelines. Every other
spec defaults to `fitIO = pure . fitWith` (same error semantics as `|->`).

## HBM in one line — and `dataScatterOf`

HBM takes a hand-written `ModelP` program (not a formula). `df |-> hbm cfg model` binds the
data-frame columns to the model's data slots (the role-suffixed trio
`dataNamedX` / `dataNamedObs` / `dataNamedIx`; `dataNamed` is a synonym of
`dataNamedX`) and runs `hbmModelPure` (deterministic by
the seed in `cfg`).

Since Phase 60.3 the binding follows these rules (no silent drops):

- `dataNamedX` / `dataNamedObs` slots ← numeric columns (Double / Int /
  **Integer** / Maybe variants / numeric Text). An **empty placeholder**
  (`dataNamedX "x" []`) with no matching column makes `fitEither` return
  `Left` (previously it silently trained on an empty list). A placeholder
  that carries actual values falls back to its default when the column is
  missing.
- `dataNamedIx` slots (discrete indices, slot-tagged `Ix`) ← Int / Integer columns
  bind directly; **Text factor columns are auto-coded against sorted
  (lexicographic) levels** (R `factor()` / pandas parity, invariant to row
  shuffles). The levels are available after the fit via
  `hbmFactorLevels m` (`[("g", ["A","B"])]` → code 0 = "A"). Non-integral
  numeric columns are a `Left`.

```haskell
model = do
  gs <- dataNamedIx  "g" []     -- auto-coded 0,1,2.. from the Text factor column "g"
  x  <- dataNamedX   "x" []     -- values come back in the model's numeric type [a]
  ys <- dataNamedObs "y" []     -- raw [Double] for observe
  ...
  let mu = b0s !!! g + b1 * xi  -- no round/realToFrac; adds a g→mu DAG edge
``` The fitted model keeps its data, so `dataScatterOf` lets you write the
data frame **once** and still overlay the observed scatter with every extractor:

```haskell
let m = df |-> hbm defaultHBM model            -- df appears only here
noDf |>> (dataScatterOf m "x" "y" <> toPlot (epred m "x" "mu"))
noDf |>> toPlot (forestOf m)
```

**Selecting parameters** (≈ ArviZ `var_names`): for hierarchical models with many
latents, the hgg **Phase 18** `<>` combinators apply directly (per-param
extractors put the parameter name in each panel title; `forestOf` uses it as the
category row). Both mean **selection + display order = enumeration order**:

```haskell
-- only 3 variables from the trace grid (stacked in this order, per-chain overlay)
noDf |>> subplots (tracesOfWith defaultTraceOpts { toByChain = True } m)
       <> selectPanels ["b1_0", "b1_1", "sigma"] <> subplotCols 1
-- only the group slopes from the forest (top to bottom: b1_0, b1_1, b1_2)
noDf |>> toPlot (forestOf m) <> scaleYDiscreteLimits ["b1_0", "b1_1", "b1_2"]
```

**Sampling diagnostics** (Phase 59, ArviZ-style): extractors that visualize NUTS
divergent draws and energy. Spot hierarchical funnels (divergences clustering at
small τ) at a glance:

```haskell
-- trace + divergence rug (ArviZ plot_trace style; rug is on by default in tracesOf); selectPanels applies too
noDf |>> subplots (tracesOfWith defaultTraceOpts { toByChain = True } m) <> selectPanels ["tau_b1", "b1_2"]
-- joint scatter + highlighted divergences (ArviZ plot_pair(divergences=True) style)
noDf |>> head (pairOf m [("tau_b1", "b1_2")])
-- marginal vs transition energy (ArviZ plot_energy style)
noDf |>> energyOf m
-- pooled divergence indices (all chains, same order as mergeChains)
divergencesOf m :: [Int]
```

`dagOf` now defaults to the plate-collapsed look (indexed RVs inside a plate fold
into one node, matching PyMC `model_to_graphviz`; Phase 59.3). Use `dagOfRaw` for
the expanded per-index nodes.

## Not in this API (follow-ups)

- **formula → HBM** auto-generation (brms-style `bayes "y ~ x + (1|g)"`) — a separate phase.
- **route-1 spec → route-2 stat** auto-generation (`statOf (lm "x" "y")`).
