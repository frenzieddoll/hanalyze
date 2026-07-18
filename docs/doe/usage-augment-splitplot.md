# Custom Design: Augment + Split-Plot

> Augmentation and Split-Plot extensions to the Custom Design Core (Phase 24).
>
> Spec: `specification/spec/hanalyze-doe-custom-design-spec.md` v0.1.1
> §2.5 / §2.6
> Phases: 25-3 through 25-9
> Prerequisite: Phase 24 complete (`Coordinate.coordinateExchange` working)

Japanese reference: [`usage-augment-splitplot.ja.md`](usage-augment-splitplot.ja.md).

## Module map

| Module | Role |
|---|---|
| `Hanalyze.Design.Custom.Augment`   | `augmentMenu` (Replicate / AddCenter / AddAxial / AddRuns / Foldover) |
| `Hanalyze.Design.Custom.SplitPlot` | `generateSplitPlot` (REML D-opt for HardToChange factors) |

## Augment 5 menu

```haskell
data AugmentMenu
  = Replicate Int
  | AddCenter Int
  | AddAxial  Double
  | AddRuns   Int
  | Foldover  FoldoverKind

data FoldoverKind = FullFoldover | PartialFoldover [Text]

augmentMenu :: CustomDesignSpec -> AugmentMenu -> IO (Either Text AugmentMenuResult)
```

- `cdsInitial` must be `Just _`; `Nothing` → `Left`.
- `AddAxial` assumes coded space `[-1, 1]`.
- `AddRuns` builds a candidate set = continuous ±1 corners × categorical full
  levels (Cartesian product), then dispatches to `Hanalyze.Design.Optimal.augmentDesign`.

## Split-Plot

```haskell
data SplitPlotConfig = SplitPlotConfig
  { spcNWhole   :: !Int     -- # whole plots, user-supplied
  , spcVarRatio :: !Double  -- η = σ²_WP / σ², default 1.0
  }

generateSplitPlot :: CustomDesignSpec -> SplitPlotConfig -> IO (Either Text SplitPlotDesign)
```

A factor with `fRole = HardToChange` is the whole-plot factor; it stays
constant within each whole plot. The criterion is approximate REML D-opt:
`X' M^{-1} X` with `M = I + η Z Z^T`. The implementation evaluates a
Cholesky-transformed surrogate via `critValueM` (DOpt det), preserving the
direction of optimization but not the exact Goos-Vandebroek scale.

## Known limitations

- `VeryHardToChange` (strip-plot) → `Left`
- Categorical HardToChange (whole-plot) → `Left` (deferred to GLMM-integrated future commit)
- Categorical factors in Foldover keep their indices unchanged (no sign concept)
- Conditional constraint NOT-clauses are not supported (AND/OR only)
