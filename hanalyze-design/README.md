# hanalyze-design

The **design of experiments (DoE) layer** of [`hanalyze`](../README.md).
It owns both the **generation** of designs (factorial / orthogonal arrays /
RSM / optimal design / space-filling / mixture) and the **evaluation** of the
resulting data (ANOVA / power / process capability / measurement system
analysis) — 30 modules in total.

It depends on the three layers `core` / `frame` / `models`, plus 10 external
packages. Because model fitting is delegated to `-models`, the quadratic
model for RSM and the information-matrix computation for optimal design are
not self-contained here — they use regression from the models layer. `-viz`
sits on top of this layer.

## Main modules (30 in total)

### Classical design generation (`Hanalyze.Design.*`)

| Module | Role |
|---|---|
| `Design.Factorial` | Full / two-level / three-level / fractional / mixed-level factorial designs — the entry point for DoE |
| `Design.Orthogonal` / `Design.Taguchi` | Orthogonal arrays (L8 / L9 / L12, …) / robust design via SN ratio and inner/outer arrays |
| `Design.Mixed` / `Design.Block` | Mixed-level designs / blocking (randomized block design) |
| `Design.DSD` | Definitive Screening Design (Jones-Nachtsheim 2011) |
| `Design.RSM` | Response surface methodology — CCD/Box-Behnken generation, quadratic model fit, analytical solution for the extremum |

### Optimal design, space filling, and mixture

| Module | Role |
|---|---|
| `Design.Optimal` | D / A / I / E / G-optimal (Fedorov exchange algorithm) plus augmenting an existing design (`augmentDesign`) |
| `Design.SpaceFilling` | Space-filling designs — LHS / Maximin LHS / Halton (for computer experiments) |
| `Design.Mixture` | Mixture designs — Simplex Lattice / Simplex Centroid (component proportions sum to 1) |
| `Design.MultiRSM` | Simultaneous optimization of multiple responses (used together with Desirability) |

### Custom Design (`Design.Custom.*`)

A general-purpose design-generation system where the user assembles factors,
model, and constraints. 11 modules.

| Module | Role |
|---|---|
| `Custom.Factor` / `Custom.Model` | Factor definitions (continuous / categorical / discrete numeric) / specifying model terms |
| `Custom.Constraint` / `Design.Constraint` | Narrowing candidate points under linear/nonlinear constraints |
| `Custom.Augment` | Menu for adding runs (`AddRuns`, etc.) — calls `Design.Optimal.augmentDesign` |
| `Custom.SplitPlot` | Split-plot experiments (whole plot / sub plot) |
| `Custom.Bayesian` | Bayesian D-optimal design (DuMouchel-Jones 1994) |
| `Custom.Power` / `Custom.Compare` | Power evaluation / comparison of multiple designs |
| `Custom.Coordinate` / `Custom.Structured` / `Custom.RegionMoment` | Coordinate-exchange algorithm / structured designs / region moment matrix (I-optimal) |

### Analysis & evaluation

| Module | Role |
|---|---|
| `Design.Anova` | Analysis of variance (significance of factor effects) |
| `Design.Diagnostics` | Design diagnostics (confounding / alias structure / condition number) |
| `Design.Power` | Power and required-sample-size calculations |
| `Design.Quality` | Process capability indices (Cp / Cpk, etc.) |
| `Design.GaugeRR` | Gauge R&R — measurement system analysis (per AIAG MSA 4th ed.) |

### Sequential & workflow

| Module | Role |
|---|---|
| `Design.Sequential` | Sequential RSM — steepest-ascent path generation and placement of the next CCD |
| `Design.Workflow` | Support for the design → experiment → analysis → next-design cycle |

## Using it standalone

If you only need to generate designs, this package alone is sufficient:

```cabal
build-depends: hanalyze-design
```

```haskell
import Hanalyze.Design.Factorial (twoLevelFactorial, fullFactorial)

main :: IO ()
main = do
  mapM_ print (twoLevelFactorial 3)
  -- [-1.0,-1.0,-1.0] / [-1.0,-1.0,1.0] / … / [1.0,1.0,1.0]  (2³ = 8 run)
  mapM_ print (fullFactorial [[180, 200, 220], [10, 20]])
  -- [180.0,10.0] / [180.0,20.0] / [200.0,10.0] / … / [220.0,20.0]  (3×2 = 6 run)
```

`twoLevelFactorial k` produces a coded (`±1`) `2^k` design; `fullFactorial`
takes the Cartesian product of the given per-factor level lists directly, so
the resulting design table stays **in the original units**.

Normally you would just depend on the umbrella package `hanalyze` and
get all of the above from a single `import Hanalyze`. Naming a layer
directly is only worth it when you want to minimize dependencies.

## Related docs

- DoE entry point: [docs/doe/01-doe.md](../docs/doe/01-doe.md) /
  theory: [theory-doe.md](../docs/doe/theory-doe.md)
- Orthogonal arrays and Taguchi methods: [02-orthogonal-taguchi.md](../docs/doe/02-orthogonal-taguchi.md)
- Custom Design: [usage-custom-design.md](../docs/doe/usage-custom-design.md) /
  [manual-custom-design.md](../docs/doe/manual-custom-design.md)
- Augmenting runs and split-plot experiments: [usage-augment-splitplot.md](../docs/doe/usage-augment-splitplot.md) /
  low-level API: [usage-doptimal-augment.md](../docs/doe/usage-doptimal-augment.md)
- Bayesian D-optimal: [usage-bayesian-d.md](../docs/doe/usage-bayesian-d.md)
- Space filling: [usage-space-filling.md](../docs/doe/usage-space-filling.md) /
  mixture designs: [usage-mixture.md](../docs/doe/usage-mixture.md)
- Sequential RSM: [usage-sequential-rsm.md](../docs/doe/usage-sequential-rsm.md)
- Gauge R&R: [usage-gauge-rr.md](../docs/doe/usage-gauge-rr.md)

← [repository README](../README.md)
