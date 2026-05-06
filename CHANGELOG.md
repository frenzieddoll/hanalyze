# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [PVP](https://pvp.haskell.org/) versioning.

## [Unreleased]

### Performance (Phase 1-13)
- Build flags: added `-O2 -funbox-strict-fields` to all 75 stanzas (library +
  executables + tests) via the new `common opt` block.
- Strict data: enabled `{-# LANGUAGE StrictData #-}` on 22 hot-path modules
  (Optim.{NSGA,LBFGS,DE,CMAES,CMAESFull,SA,PSO,Common,BayesOpt,Acquisition,
  Pareto,NelderMead,LineSearch}, Model.{GLM,Regularized,RFF,GP,Kernel},
  Stat.{KernelDist,Cholesky}, MCMC.{HMC,NUTS}).
- INLINE pragmas on hot-path wrappers: `Stat.Cholesky.{cholSolve,cholFactor,
  cholSolveWithFactor}`, `Stat.KernelDist.{diagAB,rowDotsAB,rowSqNorms}`,
  `Optim.Common.flipFor`, plus 9 polymorphic helpers in `Stat.AD`.
- `Stat.KernelDist.pairwiseSqDist` rewritten with `runST + Storable.Mutable`
  flat-index loop; massiv dependency removed from this hot path
  (16-26% speedup on KR/Gram benchmarks).
- `Model.GLM.glmLogLik` switched from list-based `zipWith`+`sum` to
  `VS.zipWith`+`VS.sum` (~20% speedup on GLM_logit_n=10000).
- `Model.GLM.irlsStep` weight/working-response computation switched from
  massiv `MA.map`/`MA.zipWith3` to `VS.map`/`VS.zipWith3`.
- `Stat.ModelSelect.lmPosteriorLogLiks`/`glmPosteriorLogLiks` switched to
  the same `VS.zipWith`-based pattern (avoids per-sample `LA.toList`
  allocations).
- Benchmark infrastructure: added `bench-tasty` (focused tasty-bench
  micro suite) and `bench-profile` (profiling runner with
  `cabal.project.local: profiling-detail: late-toplevel`). Migrated
  `bench-regression` and `bench-kernel` to use the new
  `BenchUtil.timeitTasty` (adaptive iteration, 5% relative stdev) instead
  of fixed-N `timeit`. CSV output schema is preserved.
- Reverted experiments documented for future reference (all in
  `bench/results/perf_profile_findings.md`):
  parallel `Strategies` on `Stat.Bootstrap` (Storable allocator
  contention), mutable axpy in Lasso CD (BLAS daxpy already optimal),
  `VS.map`-based `mapMatrix`/`mapVector` (massiv's fused map wins on
  large matrices).

### Documentation
- Added Haddock `>>>` examples to a curated set of pure helpers
  (`Stat.Interpolate.interp1d`, `Stat.AdaptiveGrid.uniformGrid`,
  `Optim.Common.projectToBounds` / `inBounds`, `Model.MultiOutput.asMultiY`,
  `DataIO.Log.hasErrors`). The doctest runner test-suite is deferred until
  the cabal/doctest package-db wiring is settled; the examples remain
  valid as Haddock documentation.
- Updated `bench/results/SUMMARY.md` and `bench/results/OPEN_ISSUES.md`
  to reflect Phase 1-13 numbers; deleted stale `bench/results/REPORT.md`
  (Phase B0-B5) and the 160k-line auto-generated `bench/results/summary.md`.

### Release engineering
- `cabal sdist` and `cabal haddock --haddock-for-hackage` both succeed
  cleanly (`cabal check` reports no errors or warnings). Hackage candidate
  upload is left as a manual step:
  ```
  cabal upload dist-newstyle/sdist/hanalyze-0.1.0.0.tar.gz                 # candidate
  cabal upload --documentation dist-newstyle/hanalyze-0.1.0.0-docs.tar.gz   # candidate docs
  cabal upload --publish dist-newstyle/sdist/hanalyze-0.1.0.0.tar.gz       # final
  ```

## [0.1.0.0] - 2026-05-04

Initial Hackage release.

### Models
- Linear models: `Model.LM`, `Model.GLM` (Gaussian / Binomial / Poisson + IRLS),
  `Model.GLMM` (LME via exact EM, GLMM via Laplace).
- Smoothers: `Model.Spline` (B-spline / natural cubic),
  `Model.Kernel` (Nadaraya-Watson + kernel ridge).
- Gaussian process: `Model.GP` (RBF / Matérn / periodic, single + multi output),
  `Model.GPRobust` (Student-t / Cauchy via IRLS MAP),
  `Model.RFF` (random Fourier features, multi-output).
- Regularization: `Model.Regularized` (ridge / lasso / elastic net).
- Probabilistic DSL: `Model.HBM` (free monad with structure / log-joint / AD /
  dependency interpretations).

### MCMC and inference
- `MCMC.MH`, `MCMC.HMC`, `MCMC.NUTS`, `MCMC.Gibbs`, `MCMC.Slice`.
- `Stat.VI` (mean-field ADVI), `Stat.ModelSelect` (WAIC / PSIS-LOO / pseudo-BMA),
  `Stat.MCMC` (split R-hat, ESS, autocorrelation, KDE).

### Distributions
- `Stat.Distribution`: 27 distributions including Truncated, Censored, MvNormal,
  Dirichlet, LKJ, Multinomial, ZeroInflated, AR(1).

### Design of Experiments
- `Design.Factorial`, `Design.Block`, `Design.RSM`, `Design.Optimal`,
  `Design.Anova`, `Design.Power`, `Design.Quality`, `Design.MultiRSM`,
  `Design.Orthogonal` (L4-L18), `Design.Taguchi` (4 SN ratios, inner/outer).

### Optimization
- Single-objective: `Optim.NelderMead`, `Optim.LBFGS`, `Optim.LineSearch`,
  `Optim.DifferentialEvolution`, `Optim.CMAES`, `Optim.CMAESFull`,
  `Optim.SimulatedAnnealing`, `Optim.ParticleSwarm`.
- Multi-objective: `Optim.NSGA`, `Optim.Pareto`, `Optim.Acquisition`,
  `Optim.BayesOpt`, `Optim.Desirability`.
- Constrained: `Optim.Constrained` (augmented Lagrangian + penalty).
- Unified `Optim.Common.Bounds` API for box constraints across all algorithms.

### Data I/O
- `DataIO.CSV` with `loadAuto` / `loadAutoSafe` / `loadAutoSafeWith`,
  `DataIO.External` (Parquet / JSON via @dataframe@),
  `DataIO.Convert`, `DataIO.Preprocess` (NA handling, group-by, melt, regrid).
- Dirty-data defense: `DataIO.Log` (W001..W008), `DataIO.Health`,
  `DataIO.Sniff` (delimiter / header / comment auto-detection),
  `DataIO.Clean` (column-cleaning DSL).
- Long-form regrid: `Stat.Interpolate` (Linear / NaturalSpline / PCHIP),
  `Stat.AdaptiveGrid` (peak |dy/dz|-based grid), `regridLong`.

### Visualization
- `Viz.Core` (HTML / PNG / SVG via @vl-convert@), `Viz.Bar`, `Viz.Scatter`,
  `Viz.Histogram`, `Viz.MCMC` (PyMC-style diagnostics),
  `Viz.ModelGraph` (Mermaid DAG via Track interpretation),
  `Viz.ReportBuilder` (compositional report API, 11 `Reportable` instances,
  20+ section helpers including `secInterpolation`).

### Command-line interface
- `hanalyze` with subcommands: `regress`, `info`, `hist`, `doe`, `taguchi`,
  `ridge`, `kernel`, `spline`, `multireg`, `clean`, `melt`, `regrid`.

[Unreleased]: https://github.com/frenzieddoll/hanalyze/compare/v0.1.0.0...HEAD
[0.1.0.0]: https://github.com/frenzieddoll/hanalyze/releases/tag/v0.1.0.0
