# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [PVP](https://pvp.haskell.org/) versioning.

## [Unreleased]

### Release engineering
- `cabal sdist` and `cabal haddock --haddock-for-hackage` both succeed
  cleanly (`cabal check` reports no errors or warnings). Hackage candidate
  upload is left as a manual step:
  ```
  cabal upload dist-newstyle/sdist/hanalyze-0.1.0.0.tar.gz                 # candidate
  cabal upload --documentation dist-newstyle/hanalyze-0.1.0.0-docs.tar.gz   # candidate docs
  cabal upload --publish dist-newstyle/sdist/hanalyze-0.1.0.0.tar.gz       # final
  ```

### Documentation
- Added Haddock `>>>` examples to a curated set of pure helpers
  (`Stat.Interpolate.interp1d`, `Stat.AdaptiveGrid.uniformGrid`,
  `Optim.Common.projectToBounds` / `inBounds`, `Model.MultiOutput.asMultiY`,
  `DataIO.Log.hasErrors`). The doctest runner test-suite is deferred until
  the cabal/doctest package-db wiring is settled; the examples remain
  valid as Haddock documentation.

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
