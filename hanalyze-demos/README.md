# hanalyze-demos

Home of the **demo and benchmark executables** for
[`hanalyze`](../README.md). This package has no library — just
**151 executables**.

It began with Phase 106.4, which isolated the plot-dependent executables;
Phase 109.2 then moved the remaining demo/bench executables out of the
umbrella package, leaving it as "library + test only". This package is **opt-in**: it appears
in neither the default `cabal.project` nor `cabal.project.plot`. Use its own
build root:

```bash
cabal build --project-file=cabal.project.demos hanalyze-demos
cabal run   --project-file=cabal.project.demos glmm-demo
cabal run   --project-file=cabal.project.demos posteriordb-radon
```

`cabal.project.demos` `import`s `cabal.project.plot` and adds this package, so
the sibling `hgg-*` packages are built along with it.

## Breakdown (all 151 executables)

Measured by resolving each `main-is` against its `hs-source-dirs`.

| Directory | Count | Contents | Example executables |
|---|---|---|---|
| `bench/haskell` | 43 | Haskell-side benchmarks (emit the CSV the Python aggregator reads) | `bench-mcmc`, `bench-bo`, `bench-data-gen` |
| `demo/bayesian` | 37 | HBM DSL, MCMC and variational inference demos | `hbm-example`, `plate-notation-demo` |
| `../bench/posteriordb/<NN-name>` | 35 | Cross-implementation posteriordb benchmarks (one model per executable) | `posteriordb-radon`, `posteriordb-eight-schools` |
| `demo/doe-optim` | 11 | Design of experiments and optimisation | `optimaldoe-demo`, `materials-moo-demo` |
| `demo/regression` | 9 | Regression, GP, robust regression | `gp-demo`, `robust-gp-demo` |
| `demo/io` | 6 | CSV loading, cleaning, regridding | `dirty-data-demo`, `preprocess-demo` |
| `demo-plot` | 3 | hgg integration (`toPlot`) in practice | `plot-integration-demo`, `readme-dag-demo` |
| `demo` | 2 | End-to-end demos | `glmm-demo`, `integrated-demo` |
| `demo/doe` | 2 | Sequential RSM and sample-size design | `doe-rsm-samplesize-demo` |
| `demo/visualization` | 2 | Vega-Lite visualization | `bar-demo`, `new-sections-demo` |
| `../experiments/phase104-…` | 1 | Throwaway investigation probe | `phase104-probe-prof` |

## Shared modules

| Module | Location | Role |
|---|---|---|
| `Common` | `../bench/posteriordb/Common.hs` | Shared by the 35 posteriordb executables. `summarize` is a lightweight stand-in for `arviz.summary` (mean / sd / 94% HDI / ESS / R-hat / MCSE), paired with `_common.py` on the Python side |
| `BenchUtil` | `bench/haskell/BenchUtil.hs` | Shared by the 43 benchmarks. Writes the uniform CSV rows (`BenchRow`) consumed by the Python aggregator, plus timing helpers (`timeit` / `timeitIO`) |

> `../bench/posteriordb` and `../experiments` still live at the repository
> root and are referenced by relative path. Moving them under this package
> (`git mv`) is deferred until Phase 89 (the posteriordb benchmark) closes.

## Which one should I run?

- **To learn the library** → the `demo/` family; `integrated-demo` and
  `glmm-demo` are the entry points
- **To learn plotting** → `demo-plot/` (static SVG) and
  `demo/visualization/` (Vega-Lite)
- **To learn HBM / MCMC** → `demo/bayesian/`
- **To measure performance** → `bench/haskell/` (CSV output); for
  cross-implementation comparison, `posteriordb-*`

Run benchmarks **sequentially, never concurrently** — parallel runs distort
the measurements.

## Related docs

- Running benchmarks and results: [bench/README.md](../bench/README.md) /
  [bench/RESULTS_tier12.md](../bench/RESULTS_tier12.md)
- Comparison against PyMC: [docs/02-pymc-comparison.md](../docs/02-pymc-comparison.md)
- Comparison against Python / R: [docs/comparison/python-r.md](../docs/comparison/python-r.md)
- Visualization: [docs/visualization/01-visualization.md](../docs/visualization/01-visualization.md)
- Static-rendering integration: [visualization/03-plot-integration.md](../docs/visualization/03-plot-integration.md)

← [repository README](../README.md)
