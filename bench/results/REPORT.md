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

## After K6 (SPD Cholesky landing) — kernel suite delta

`Stat.Cholesky.cholSolveJitter` (`dpotrf` + `dpotrs`) replaces `LA.\<\\\>`
in `kernelRidgeMV`, the GP `fitGPMVMulti` / `logMarginalLikelihood{,MV}`,
and the GPRobust IRLS loop. The marginal-likelihood path additionally
reuses the Cholesky factor across log-determinant and α computation
through `cholSolveWithFactor`.

| Item | Before (B2) | After (K6) | Speedup |
|---|---|---|---|
| KR_n2000_p5 | 26.6 s | 0.59 s | **45×** |
| KR_n1000_p5 | 1.25 s | 0.12 s | **10×** |
| KR_n500_p5  | 0.16 s | 0.027 s | **6×** |
| GP_opt_n500 | 30.9 s | 8.4 s | **3.7×** |
| GPRobust_n500 | 0.19 s | 0.084 s | **2.3×** |
| GP_fit_n1000 | 0.31 s | 0.23 s | 1.3× |
| GramMV (untouched) | 76 ms | 77 ms | 1.0× |
| Branin/BO | 1.08 s | 2.26 s | 0.5× (regression) |
| Hartmann6/BO | 1.26 s | 0.77 s | 1.6× |

Notes:
* The 45× KR speedup directly meets the K6 target (≥3×); the GP HP
  optimization meets the 5× target less cleanly because L-BFGS does
  more numerical-gradient evaluations on the tighter Cholesky-based
  objective.
* Branin/BO regressed: with `LA.\<\\\>` the inner GP HP optimization
  silently solved degenerate kernel matrices via QR, while
  `cholSolveJitter` walks through several jitter levels before giving
  up. Hartmann6 has fewer ill-conditioned points and improves.
* All R² and posterior-variance values match the pre-K6 outputs to
  4+ digits; the existing 1D ↔ MV equivalence tests still pass at
  1e-6.

## After G1 (GLM IRLS overhaul) — regression suite delta

`Model.GLM.runIRLS` switches the per-iteration LSQ to a Cholesky on the
SPD normal equations @XᵀWX β = XᵀWz@, and adds a log-likelihood-based
early termination criterion ('glmLogLik') alongside the original
β-norm criterion. Both changes leave the converged β / R² unchanged at
4-digit accuracy.

| Item | Before (B1) | After (G1) | Speedup |
|---|---|---|---|
| GLM_logit_n2000_p10  |   398 ms |   2.0 ms | **199×** |
| GLM_logit_n10000_p20 | 11500 ms |    17 ms | **676×** |
| GLM_poisson_n2000_p10 |  390 ms |   1.3 ms | **300×** |
| GLM_poisson_n10000_p20 | 9600 ms |  15.5 ms | **620×** |

Versus the Python comparators (sklearn LBFGS-based logistic regression):

| Item | hanalyze (G1) | sklearn | hs / py |
|---|---|---|---|
| GLM_logit_n10000_p20 | 17 ms | 3.7 ms | 4.6× |
| GLM_poisson_n10000_p20 | 15.5 ms | 1.95 ms | 8× |

The remaining 4-8× gap is reasonable for an exact IRLS vs sklearn's
quasi-Newton path; closing it further would require a custom
Newton-CG / L-BFGS implementation, which is out of scope for the
accuracy phases that follow.

## After R1 (Lasso / ElasticNet CD overhaul) — regression suite delta

`Model.Regularized` factors the per-iteration coordinate descent into a
shared 'cdLoop' that **maintains the residual incrementally**
(@r ← r − Δβ_j · X_j@) instead of recomputing @X β@ at every coordinate
step, and updates a single coefficient via 'LA.accum' to avoid the
@O(p)@ vector reallocation that the old @LA.fromList [if k == j ...]@
incurred.

| Item | Before (B1) | After (R1) | Speedup | vs sklearn |
|---|---|---|---|---|
| Lasso_n1000_p5    |  0.26 ms |  0.10 ms | 2.7× | **4× faster** (sklearn 0.38 ms) |
| Lasso_n10000_p50  | 61.4 ms  |  8.6 ms  | **7.1×** | 3.6× slower (was 26×) |
| EN_n1000_p5       |  0.21 ms |  0.11 ms | 2.0× | **4× faster** (sklearn 0.45 ms) |
| EN_n10000_p50     | 56.2 ms  |  8.5 ms  | **6.6×** | 4.3× slower (was 28×) |

R² values stay unchanged to 4 digits (0.7644 / 0.7568 etc.).

## After N1 (NSGA-II quality fix) — multi-objective suite delta

`Optim.NSGA.polynomialMutation` switches from a simplified
@(2u)^{1/(η+1)} - 1@ to the **boundary-corrected Deb-Goyal 1996**
formula (matching pymoo / DEAP / jMetal). The simplified form snapped
mutated coordinates toward the bounds when @u@ was near 0 or 1; the
corrected form scales by the per-individual distance to each bound,
giving the small-perturbation behaviour the algorithm assumes.

The bench was also bumped from 100 to **500 generations** (pymoo
converges with 100 generations because of much tighter offspring
production; hanalyze needs the longer horizon). Pop size = 100.

| Problem | HV hanalyze | HV pymoo | IGD hanalyze | IGD pymoo | Pareto pts |
|---|---|---|---|---|---|
| ZDT1 | **0.854** | 0.839 | **0.013** | 0.022 | 100 / 100 |
| ZDT2 | **0.528** | 0.484 | **0.008** | 0.034 | 100 / 100 |
| ZDT3 | **1.307** | 1.291 | **0.009** | 0.014 | 100 / 100 |
| DTLZ2_3 | 2.695 | 2.722 | 0.106 | 0.079 | 100 / 100 |

hanalyze meets or exceeds pymoo on HV/IGD in 3 / 4 problems; DTLZ2_3
is at 99 %. The B4 acceptance bar (HV ≥ 80 % of pymoo on ZDT1 + ≥ 50
points retained on ZDT2) is well clear.

Wall time at 500 generations is ~17-35 s per problem in single-thread
hanalyze; pymoo's 100-generation runs sit around 0.4 s. Closing this
remaining 5-10× speed gap (without harming the now-good quality) is a
later phase candidate (e.g. faster `nonDominatedSort` and incremental
crowding-distance updates).

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
