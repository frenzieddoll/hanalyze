# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [PVP](https://pvp.haskell.org/) versioning.

## [Unreleased]

### Fixed (P1: RFF OOM)
- `Hanalyze.Model.RFF.medianPairwiseDist`: rewrote with BLAS gram matrix
  (`Hanalyze.Stat.KernelDist.pairwiseSqDist`) + `Data.Vector.Algorithms.Intro.sort`
  on a flat `Vector`. The previous implementation built an `O(n²)` list of pair
  distances using `rows !! i` (each `O(i)`, so `O(n³)` walks total) and ran a
  naive list quicksort, which exploded space to many GB of thunks and OOM-killed
  WSL2 around `n=768` (e.g. inside `maximizeMarginalLikRBFMV`).
- `Hanalyze.Model.RFF.rbfKernelMat`: rewrote as
  `LA.cmap (...) (KD.pairwiseSqDist x)`. The old nested list comprehension with
  `rows !! i / rows !! j` shared the same `O(n³)` shape and hit the same WSL2
  OOM via `logMarginalLikRBFMV`.
- Removed the file-local naive `qSort` from `RFF.hs`.
- New `bench-rff-oom` executable as a regression guard. Post-fix:
  `maximizeMarginalLikRBFMV` with a 3·2·2 grid runs at `n=768` in ~10 s and
  ~45 MiB peak residency (was OOM).

### Fixed (P4: Tier-2 O(n²) helpers in Preprocess)
- `Hanalyze.DataIO.Preprocess.dropMissingRows`: cache per-column Text
  `Vector` once instead of calling `tryColumnAsList` + @xs !! i@ inside
  the inner row loop. O(rows² × cols) → O(rows × cols).
- `Hanalyze.DataIO.Preprocess.sliceColumn` (`tryAs`): convert the
  column to a `Vector` once and use `unsafeIndex` instead of
  @xs !! i@ in a list comprehension. O(n²) → O(n).

### Fixed (P3: GC pressure / O(n²) helpers)
- `Hanalyze.Model.GP.buildKernelMatrix` (1D variant): rewrote with a
  flat `Storable.Vector` filled via `runST + MVector` instead of
  materialising the @|xs|·|xs'|@ lazy `[Double]` list that the old
  `(n><m) [..]` form created (~30 MB of cons cells at `n=768`, pure
  GC pressure). API is unchanged so the `Periodic` kernel keeps its
  signed-difference behaviour.
- `Hanalyze.Model.GLMM.buildGroups`: replaced `sort . nub` with
  `Set.toAscList . Set.fromList` (O(n log n) vs O(n²)). Important for
  grouping vectors with thousands of distinct group IDs.

### Fixed (P2: stray naive quicksorts)
- `Hanalyze.Model.Quantile.quantile`: replaced file-local naive list quicksort
  with `Data.List.sort` (mergesort, O(n log n) / O(n) space). Pivot-bias could
  push the old version to O(n²) space on adversarial inputs.
- `Hanalyze.Stat.Test.sortVec` and the file-local `qsort` used by
  `mannWhitneyManual`: same replacement (`Data.List.sort` /
  `sortBy (comparing fst)`). Both `qSort`/`qsort` definitions removed.

## [0.1.0.1] - 2026-05-14

Initial Hackage release. (Version 0.1.0.0 was uploaded only as a
candidate and never published; the multi-output GP API was rearranged
before publication — see below.)

### Multi-output GP — API のデフォルトを shared-HP に変更
- `Hanalyze.Model.MultiGP.fitMultiGP` / `fitMultiGPMV` の **挙動を sklearn 流
  shared-HP 版に置き換え**。1 回の HP 最適化で全 q 出力の合算周辺尤度を
  最大化し、`Ky = K + σ_n² I` の Cholesky を再利用する (RBF 専用、
  `q > 1` で旧版比 ~q× 速い)。
- 旧来の per-output 独立 HP 版 (任意カーネル対応) は
  `fitMultiGPIndep` / `fitMultiGPMVIndep` に **改名**。
- 旧 `fitMultiGPMVSharedHP` は新しい `fitMultiGPMV` に統合済 (削除)。
- 既存ユーザーは `fitMultiGP kern ...` を `fitMultiGPIndep kern ...` に
  置き換えれば従来の挙動を維持できる。

### LM diagnostics + Taguchi/Quality 拡張
- `Hanalyze.Model.LM.Diagnostics` (new module): inference and residual diagnostics
  for OLS — `ciTValue`, `lmStdErrors[Multi]`, `CoefStats` /
  `lmCoefStats[Multi]` (SE / t / two-sided p), `FStat` / `lmFStatistic`
  (whole-model F, follows R-style df1 = p − 1, df2 = n − p), `ICs` /
  `lmInformationCriteria` (R `lm()` convention with k = p + 1, σ counted),
  `hatDiagonal`, `standardizedResiduals`, `cooksDistance`,
  `predictorStdDevs`. Multi-output (Matrix p × q) is the canonical form;
  Vector wrappers cover q = 1.
- `Hanalyze.Design.Orthogonal.OAMetadata` + `listArraysWithSize`: structured
  metadata (name / runs / factors / levels / description) for the
  standard L4–L18 arrays.
- `Hanalyze.Design.Taguchi.SNDetails` + `snRatioWithDetails`: SN ratio bundled
  with sample mean / variance / N.
- `Hanalyze.Design.Taguchi.FactorEffectExt` + `factorEffectsTable`: factor-effect
  rows enriched with `feeRange` and `feeContribution`.
- `Hanalyze.Design.Quality.Capability` + `processCapability` /
  `processCapabilityUpper` / `processCapabilityLower`: Cp / Cpk for
  two-sided and one-sided spec limits.

### Performance (Phase 1-13)
- Build flags: added `-O2 -funbox-strict-fields` to all 75 stanzas (library +
  executables + tests) via the new `common opt` block.
- Strict data: enabled `{-# LANGUAGE StrictData #-}` on 22 hot-path modules
  (Optim.{NSGA,LBFGS,DE,CMAES,CMAESFull,SA,PSO,Common,BayesOpt,Acquisition,
  Pareto,NelderMead,LineSearch}, Model.{GLM,Regularized,RFF,GP,Kernel},
  Stat.{KernelDist,Cholesky}, MCMC.{HMC,NUTS}).
- INLINE pragmas on hot-path wrappers: `Hanalyze.Stat.Cholesky.{cholSolve,cholFactor,
  cholSolveWithFactor}`, `Hanalyze.Stat.KernelDist.{diagAB,rowDotsAB,rowSqNorms}`,
  `Hanalyze.Optim.Common.flipFor`, plus 9 polymorphic helpers in `Hanalyze.Stat.AD`.
- `Hanalyze.Stat.KernelDist.pairwiseSqDist` rewritten with `runST + Storable.Mutable`
  flat-index loop; massiv dependency removed from this hot path
  (16-26% speedup on KR/Gram benchmarks).
- `Hanalyze.Model.GLM.glmLogLik` switched from list-based `zipWith`+`sum` to
  `VS.zipWith`+`VS.sum` (~20% speedup on GLM_logit_n=10000).
- `Hanalyze.Model.GLM.irlsStep` weight/working-response computation switched from
  massiv `MA.map`/`MA.zipWith3` to `VS.map`/`VS.zipWith3`.
- `Hanalyze.Stat.ModelSelect.lmPosteriorLogLiks`/`glmPosteriorLogLiks` switched to
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
  parallel `Strategies` on `Hanalyze.Stat.Bootstrap` (Storable allocator
  contention), mutable axpy in Lasso CD (BLAS daxpy already optimal),
  `VS.map`-based `mapMatrix`/`mapVector` (massiv's fused map wins on
  large matrices).

### Documentation
- Added Haddock `>>>` examples to a curated set of pure helpers
  (`Hanalyze.Stat.Interpolate.interp1d`, `Hanalyze.Stat.AdaptiveGrid.uniformGrid`,
  `Hanalyze.Optim.Common.projectToBounds` / `inBounds`, `Hanalyze.Model.MultiOutput.asMultiY`,
  `Hanalyze.DataIO.Log.hasErrors`). The doctest runner test-suite is deferred until
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

### Models
- Linear models: `Hanalyze.Model.LM`, `Hanalyze.Model.GLM` (Gaussian / Binomial / Poisson + IRLS),
  `Hanalyze.Model.GLMM` (LME via exact EM, GLMM via Laplace).
- Smoothers: `Hanalyze.Model.Spline` (B-spline / natural cubic),
  `Hanalyze.Model.Kernel` (Nadaraya-Watson + kernel ridge).
- Gaussian process: `Hanalyze.Model.GP` (RBF / Matérn / periodic, single + multi output),
  `Hanalyze.Model.GPRobust` (Student-t / Cauchy via IRLS MAP),
  `Hanalyze.Model.RFF` (random Fourier features, multi-output).
- Regularization: `Hanalyze.Model.Regularized` (ridge / lasso / elastic net).
- Probabilistic DSL: `Hanalyze.Model.HBM` (free monad with structure / log-joint / AD /
  dependency interpretations).

### MCMC and inference
- `Hanalyze.MCMC.MH`, `Hanalyze.MCMC.HMC`, `Hanalyze.MCMC.NUTS`, `Hanalyze.MCMC.Gibbs`, `Hanalyze.MCMC.Slice`.
- `Hanalyze.Stat.VI` (mean-field ADVI), `Hanalyze.Stat.ModelSelect` (WAIC / PSIS-LOO / pseudo-BMA),
  `Hanalyze.Stat.MCMC` (split R-hat, ESS, autocorrelation, KDE).

### Distributions
- `Hanalyze.Stat.Distribution`: 27 distributions including Truncated, Censored, MvNormal,
  Dirichlet, LKJ, Multinomial, ZeroInflated, AR(1).

### Design of Experiments
- `Hanalyze.Design.Factorial`, `Hanalyze.Design.Block`, `Hanalyze.Design.RSM`, `Hanalyze.Design.Optimal`,
  `Hanalyze.Design.Anova`, `Hanalyze.Design.Power`, `Hanalyze.Design.Quality`, `Hanalyze.Design.MultiRSM`,
  `Hanalyze.Design.Orthogonal` (L4-L18), `Hanalyze.Design.Taguchi` (4 SN ratios, inner/outer).

### Optimization
- Single-objective: `Hanalyze.Optim.NelderMead`, `Hanalyze.Optim.LBFGS`, `Hanalyze.Optim.LineSearch`,
  `Hanalyze.Optim.DifferentialEvolution`, `Hanalyze.Optim.CMAES`, `Hanalyze.Optim.CMAESFull`,
  `Hanalyze.Optim.SimulatedAnnealing`, `Hanalyze.Optim.ParticleSwarm`.
- Multi-objective: `Hanalyze.Optim.NSGA`, `Hanalyze.Optim.Pareto`, `Hanalyze.Optim.Acquisition`,
  `Hanalyze.Optim.BayesOpt`, `Hanalyze.Optim.Desirability`.
- Constrained: `Hanalyze.Optim.Constrained` (augmented Lagrangian + penalty).
- Unified `Hanalyze.Optim.Common.Bounds` API for box constraints across all algorithms.

### Data I/O
- `Hanalyze.DataIO.CSV` with `loadAuto` / `loadAutoSafe` / `loadAutoSafeWith`,
  `Hanalyze.DataIO.External` (Parquet / JSON via @dataframe@),
  `Hanalyze.DataIO.Convert`, `Hanalyze.DataIO.Preprocess` (NA handling, group-by, melt, regrid).
- Dirty-data defense: `Hanalyze.DataIO.Log` (W001..W008), `Hanalyze.DataIO.Health`,
  `Hanalyze.DataIO.Sniff` (delimiter / header / comment auto-detection),
  `Hanalyze.DataIO.Clean` (column-cleaning DSL).
- Long-form regrid: `Hanalyze.Stat.Interpolate` (Linear / NaturalSpline / PCHIP),
  `Hanalyze.Stat.AdaptiveGrid` (peak |dy/dz|-based grid), `regridLong`.

### Visualization
- `Hanalyze.Viz.Core` (HTML / PNG / SVG via @vl-convert@), `Hanalyze.Viz.Bar`, `Hanalyze.Viz.Scatter`,
  `Hanalyze.Viz.Histogram`, `Hanalyze.Viz.MCMC` (PyMC-style diagnostics),
  `Hanalyze.Viz.ModelGraph` (Mermaid DAG via Track interpretation),
  `Hanalyze.Viz.ReportBuilder` (compositional report API, 11 `Reportable` instances,
  20+ section helpers including `secInterpolation`).

### Command-line interface
- `hanalyze` with subcommands: `regress`, `info`, `hist`, `doe`, `taguchi`,
  `ridge`, `kernel`, `spline`, `multireg`, `clean`, `melt`, `regrid`.

[Unreleased]: https://github.com/frenzieddoll/hanalyze/compare/v0.1.0.0...HEAD
[0.1.0.0]: https://github.com/frenzieddoll/hanalyze/releases/tag/v0.1.0.0
