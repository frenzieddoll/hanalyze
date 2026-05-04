# hanalyze vs Python — Benchmark Report

Phase B0–B5, run on WSL2 (Arch Linux, Python 3.14.4, GHC 9.6.7,
OpenBLAS), all measurements with `OPENBLAS_NUM_THREADS=1` and
`OMP_NUM_THREADS=1` for fair single-thread comparison.

The unified per-row schema lives at `bench/results/{haskell,python}/*.csv`
with columns `system, suite, name, time_ms, acc_main, acc_aux, extra`.
Aggregate any subset with::

    bench/venv/bin/python bench/aggregate.py [--suite <name>]

Each suite is reproducible through the matching cabal target and
Python script (see `bench/README.md`).

## TL;DR

| Suite | Speed | Accuracy |
|---|---|---|
| **Regression (LM/LME/Ridge)** | hanalyze ≥ Python | identical (4-digit R²) |
| **Regression (GLM Logit/Poisson)** | **300-5000× slower** (IRLS overhead) | identical |
| **Regression (Lasso/EN)** | 10× slower (CD loop) | identical |
| **Kernel/GP** | 10-170× slower (uses `<\>` not Cholesky) | identical R² |
| **Single-objective optim** | **5-160× faster** | Python often a digit deeper |
| **Multi-objective NSGA-II** | 5-13× slower | hanalyze noticeably worse on ZDT |
| **BayesOpt** | 5× faster | skopt much closer to known optimum |

## B1 — Classical regression

Identical R² to 4+ digits across all 15 cases. Speed picture:

- **LM (OLS)**: roughly tied; hanalyze 1.1× faster on LM 100k×100, 8× on
  LM 1k×5 (lower constant overhead).
- **LME (mixed effects)**: **6-29× faster** than `statsmodels.MixedLM` —
  exact-EM beats the L-BFGS-based variational fit on small data.
- **Ridge**: 21× faster on small (1k×5), parity at 10k×50.
- **GLM (Logistic / Poisson)**: hanalyze IRLS is 300-5000× slower than
  sklearn's L-BFGS-based weighted-likelihood solver. **Improvement
  candidate**: rewrite IRLS to share Cholesky across iterations and add
  a log-likelihood-based stopping rule.
- **Lasso / ElasticNet**: 10× slower; the CD loop in
  `Model.Regularized` is pure-Haskell list-of-Vector while sklearn runs
  Cython.

## B2 — Kernel & GP

R² agrees to 4 digits everywhere. Speed:

- **gramMatrix construction**: hanalyze 10× slower than
  `sklearn.metrics.pairwise.rbf_kernel` (BLAS GEMM + `cmap` vs Cython).
- **Kernel ridge fit (n=2000)**: **170× slower** than
  `sklearn.KernelRidge`. Root cause: hanalyze solves @(K + λI) α = Y@
  with the general LSQ path `LA.<\>` (= `dgels`, QR), not the
  SPD-specific Cholesky (`dpotrf` + `dpotrs`). Switching to Cholesky
  is the highest-leverage perf win in this report.
- **GP fit (HP fixed)**: 8-9× slower (same root cause).
- **GP HP optimization**: 67× slower (each L-BFGS step pays the KR
  cost above).
- **NW / RFF**: 14× slower; will improve when the SPD solver is
  upgraded.

## B3 — Single-objective optimization

7 algorithms × 6 test functions × 30 seeds.

- **Speed**: hanalyze is **5-160× faster** in almost every cell —
  ahead-of-time compilation + direct BLAS calls vs Python's per-iter
  overhead pays off massively at the optimization scale (many objective
  evals).
- **Accuracy**: Python typically converges *one digit deeper* than
  hanalyze. Specific concerns:
  - `Rastrigin_10D/SA`: hanalyze stalls at 17.9; `dual_annealing`
    reaches 7.8e-14.
  - `Sphere_30D/DE`: hanalyze 43, scipy 2.8e-5 — hanalyze's DE looks
    under-tuned (default F / CR or limited generations).
  - `Rosenbrock_2D/NelderMead`: hanalyze 1e-9, scipy 1e-18 (scipy
    converges to machine precision).

This is the "fast but shallower" pattern. Improvement candidates:
adaptive cooling for SA, F/CR auto-adaptation for DE, tighter NM
stopping criterion.

## B4 — Multi-objective optimization

Only 4 problems (ZDT1/2/3 m=2, DTLZ2 m=3) with 100 generations. To
compare on equal footing, hanalyze writes its Pareto set and
`pymoo.indicators` scores both sides.

- **Speed**: hanalyze 5-13× slower (pymoo's vectorized numpy NSGA2 is
  hard to beat).
- **Quality**: hanalyze is noticeably worse on ZDT problems
  (HV ≈ 0 because no point dominates the reference; ZDT2 keeps only
  6 Pareto points). DTLZ2 (3 obj) is close (HV 2.65 vs 2.72,
  IGD 0.10 vs 0.08).
- Likely causes: SBX / polynomial-mutation parameters, crowding
  distance implementation, or simply too few generations for these
  problems. **Improvement candidate**: reproduce pymoo's defaults
  exactly and / or run more generations.

## B5 — Bayesian optimization

Branin (2D) and Hartmann6 (6D), 5 seeds, budget 30 evaluations after
init.

- **Speed**: hanalyze 5× faster (1-1.3 s vs 5-7 s).
- **Quality**: skopt is closer to the known optimum.
  - Branin: hanalyze f=1.05, skopt f=0.398, optimum 0.398 (skopt at
    machine precision).
  - Hartmann6: hanalyze f=−1.88, skopt f=−2.77, optimum −3.32 (skopt
    83 % vs hanalyze 56 % of the way).
- Probable cause: hanalyze's acquisition optimization (Brent / L-BFGS
  multi-start) and / or the GP HP optimization underneath might be
  stopping early. Improvement candidate: reuse the K6 Cholesky-based
  GP solver here too, and increase the multi-start count.

## High-leverage improvement queue

In rough order of expected wall-time impact across the suite:

1. **K6 — Cholesky for SPD solvers**. Replace `LA.<\>` with
   `dpotrf` + `dpotrs` in `Model.Kernel.kernelRidgeMV` and
   `Model.GP.fitGPMV`. Estimate: 2-5× speedup on KR/GP, cascades to
   GP HP optimization (67× slower today) and BayesOpt.
2. **G1 — GLM IRLS overhaul**. Reuse Cholesky factor inside `runIRLS`
   and add a log-likelihood stopping rule. Estimate: 10-50× on
   `GLM_logit_*` and `GLM_poisson_*`.
3. **N1 — NSGA-II quality**. Match pymoo's SBX / poly-mutation
   parameters, audit crowding distance. Estimate: HV improves to
   pymoo level on ZDT.
4. **S1 — SA / DE accuracy**. Adaptive cooling for SA (Lundy-Mees or
   Cauchy), F/CR self-adaptation for DE.
5. **R1 — Lasso / EN CD loop**. Move the CD inner loop to a strict
   `Storable` Vector with `Numeric.LinearAlgebra` updates instead of
   list-of-`Vector`. Estimate: 5-10× on Lasso/EN.

These are tracked separately and not part of K6 itself.

## Reproduction

```bash
# 1. Generate shared input data (deterministic)
cabal run bench-data-gen

# 2. Haskell bench
OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 \
  cabal run bench-regression bench-kernel bench-optim bench-mo bench-bo

# 3. Python bench
OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 \
  bench/venv/bin/python bench/python/bench_regression.py
# (similarly for bench_kernel.py, bench_optim.py, bench_mo.py, bench_bo.py)

# 4. Aggregate (Markdown table on stdout)
bench/venv/bin/python bench/aggregate.py
```
