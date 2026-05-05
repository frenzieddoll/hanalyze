# hanalyze vs Python / R comparison

> Where hanalyze sits among Python (numpy/scipy/sklearn/statsmodels/
> pymoo/skopt/lifelines/PyMC) and R (base + tidyverse + caret +
> survival + forecast + lme4) statistical libraries.

## 1. One-line summary

| Domain | hanalyze | Python | R | Status |
|---|---|---|---|---|
| **Classical regression (LM/Ridge/Lasso)** | `Model.LM/Regularized` | sklearn / statsmodels | base R / glmnet | ✅ benched |
| **GLM (Binomial/Poisson)** | `Model.GLM` | statsmodels | base R glm() | ✅ benched |
| **Mixed effects (LME/GLMM)** | `Model.GLMM` | statsmodels MixedLM | lme4 | ✅ benched (LME 30× faster) |
| **Kernel methods / GP** | `Model.{Kernel,GP,RFF}` | sklearn KernelRidge / GP | kernlab / GPML | ✅ benched |
| **Bayesian MCMC** | `MCMC.{MH,HMC,NUTS,Gibbs}` | PyMC / NumPyro | rstan / brms | 🟡 not yet benched |
| **HBM (probabilistic programming)** | `Model.HBM` polymorphic DSL | PyMC / Pyro / Stan | rstan / brms | 🟡 PyMC parity confirmed, bench todo |
| **Variational inference (ADVI)** | `Stat.VI` | PyMC / Pyro | rstan ADVI | 🟡 not yet benched |
| **Model comparison (WAIC/LOO)** | `Stat.ModelSelect` | ArviZ | loo (R) | 🟡 not yet benched |
| **Single-objective optimisation** | `Optim.{NM,LBFGS,DE,CMAES,SA,PSO}` | scipy.optimize | optimx | ✅ benched (10-100× faster) |
| **Multi-objective optimisation** | `Optim.NSGA` | pymoo | mco / emoa | ✅ benched (1.1-1.6× faster) |
| **Bayesian optimisation** | `Optim.BayesOpt` | scikit-optimize / GPyOpt | mlrMBO | ✅ benched (wins on Hartmann6) |
| **DoE** | `Design.*` | pyDOE / pyDOE2 | DoE.base | 🟡 not yet benched |
| **Orthogonal arrays + Taguchi** | `Design.{Orthogonal,Taguchi}` | (limited) | qualityTools / DoE.base | 🟡 not yet benched |
| **Hypothesis tests** | `Stat.Test` | scipy.stats | base R / rstatix | 🟡 not yet benched |
| **Multiple-testing correction** | `Stat.MultipleTesting` | statsmodels | p.adjust | 🟡 not yet benched |
| **Bootstrap CI / permutation tests** | `Stat.Bootstrap` | scipy.stats / arch | boot | 🟡 not yet benched |
| **Effect size + power analysis** | `Stat.Effect` | statsmodels.stats.power | pwr / effsize | 🟡 not yet benched |
| **PCA / dimensionality reduction** | `Model.PCA` | sklearn.decomposition | prcomp | 🟡 not yet benched |
| **Clustering (K-means)** | `Model.Cluster` | sklearn.cluster | cluster / mclust | 🟡 not yet benched |
| **Decision tree (CART classifier)** | `Model.DecisionTree` | sklearn.tree | rpart / tree | 🟡 not yet benched |
| **Time series (ARIMA/Holt-Winters)** | `Model.TimeSeries` | statsmodels.tsa | forecast / fable | 🟡 not yet benched |
| **Survival analysis (KM/Cox)** | `Model.Survival` | lifelines | survival | 🟡 not yet benched |
| **Classification metrics (AUC/F1/Brier/...)** | `Stat.ClassMetrics` | sklearn.metrics | pROC / mlr3measures | 🟡 not yet benched |
| **Cross-validation** | `Stat.CV` | sklearn.model_selection | caret / rsample | 🟡 not yet benched |
| **Interpretability (Permutation imp/PDP/ICE)** | `Stat.Interpret` | sklearn.inspection / shap / pdpbox | iml | 🟡 not yet benched |
| **Data manipulation (DataFrame)** | Hackage `dataframe` + `DataIO.Reshape` | pandas | tidyverse | 🟡 not yet benched |
| **Visualisation** | `Viz.*` (Vega-Lite) | matplotlib / seaborn / plotly | ggplot2 | 🟡 not yet benched |
| **CSV/Parquet/JSON I/O** | `DataIO.{CSV,External}` | pandas / pyarrow | readr / arrow | 🟡 not yet benched |
| **Dirty-data defence** | `DataIO.{Health,Sniff,Clean}` | (none, DIY) | janitor (partial) | (unique to hanalyze) |

## 2. Benched-domains (✅) — details

### 2.1 Single-objective optimisation — hanalyze dominant

| Bench | hanalyze | scipy | Verdict |
|---|---|---|---|
| Sphere_30D/L-BFGS | **8.1e-40** / 0.05 ms | 2.6e-11 / 1.67 ms | scipy 29 orders worse, hanalyze 31× faster |
| Sphere_30D/DE | **1.0e-26** / 277 ms | 4.5e-5 / 4852 ms | scipy 21 orders worse, hanalyze 17× faster |
| Ackley_10D/CMAES | **4.0e-15** / 2.6 ms | 1.3e-6 / 134 ms | scipy 9 orders worse, hanalyze 52× faster |
| Levy_10D/DE | **8.3e-21** / 45 ms | 8.0e-17 / 1768 ms | scipy 4 orders worse, hanalyze 40× faster |
| Rosenbrock_2D/NelderMead | **3.3e-13** / 0.06 ms | 4.6e-18 / 4.83 ms | hanalyze 78× faster (almost-equal accuracy) |
| Rastrigin_10D/SA | **0.0** ⭐ / 2.4 s | 5.7e-14 / 193 ms | machine-precision parity |

**Why hanalyze wins**: efficient hmatrix BLAS use, polish step (DE/CMAES embed L-BFGS local), Tsallis SA's heavy-tailed visiting distribution, low GHC compiled overhead.

### 2.2 Multi-objective optimisation (NSGA-II) — beats pymoo

Wins HV/IGD on all 4 ZDT/DTLZ problems. Per-generation speed 1.1-1.6× faster than pymoo (1.6× on DTLZ2_3).

NF1 (SBX boundary correction), NF3 (random-permutation tournament), NF4 (deduplication) implemented from pymoo source analysis.

### 2.3 Bayesian optimisation — wins Hartmann6

| Bench | hanalyze | skopt | Verdict |
|---|---|---|---|
| **Hartmann6/BO** | **-3.07** / 9.6 s | -2.77 / 7.1 s | hanalyze decisively wins |
| Branin/BO | 0.86 / 22 s | 0.398 / 5.5 s | hanalyze within 2× of skopt |

A (true ARD), B (GP-Hedge: dynamic EI/LCB/PI mixing), C (analytic gradient for kernel and EI). Hartmann6: hanalyze reaches 92% of true optimum.

### 2.4 Classical regression — at parity or better

| Bench | hanalyze | sklearn | Ratio |
|---|---|---|---|
| LM_n100000_p100 | 642 ms | 668 ms | 1.04× |
| Ridge_n1000_p5 | 0.04 ms | 0.54 ms | **15×** |
| LME_n2000_p5_g20 | 1.4 ms | 43 ms | **30×** |
| LME_n10000_p10_g50 | 19.9 ms | 97 ms | 4.9× |

Particularly fast on mixed-effects models (exact-EM is fast).

### 2.5 Large-scale ML (n ≥ 10k) — sklearn Cython native wins

| Bench | hanalyze | sklearn | Ratio |
|---|---|---|---|
| GLM_logit_n10k | 14.9 ms | 4.2 ms | 0.28× (3.6× slower) |
| Lasso_n10k×p50 | 7.4 ms | 2.4 ms | 0.32× (3.1× slower) |
| KR_n2000 | 384 ms | 176 ms | 0.46× (2.2× slower) |
| GP_fit_n1000 | 200 ms | 42 ms | 0.21× (4.7× slower) |

**Cause**: BLAS dispatch overhead vs sklearn's Cython inline SIMD. Massiv brought 1.4-3× improvements (F1+F2+F4); reaching full parity needs C/FFI.

## 3. Not-yet-benched domains (🟡) — comparison todos

These are **implemented and unit-tested** but not yet benchmarked against Python/R. Implementation correctness is verified via 238 hspec tests.

### 3.1 Bayesian MCMC (Phase BENCH-MCMC, priority ★★)
- vs PyMC (Python), rstan (R)
- Metrics: NUTS warmup + sampling time, ESS/sec, R-hat
- Models: 8-schools (centered/non-centered), linear regression, hierarchical logistic
- Open question: how does hanalyze's pure-Haskell NUTS compare to PyMC's C++ Stan core?

### 3.2 Hypothesis tests (Phase BENCH-TEST, priority ★)
- vs scipy.stats, rstatix (R)
- Metrics: numerical agreement on t/χ²/ANOVA/Mann-Whitney/Wilcoxon plus speed
- Expected: same numbers (same algorithms), comparable or slightly faster speed (Haskell closure overhead is minor)

### 3.3 PCA / Clustering (Phase BENCH-MLBASIC, priority ★★)
- vs sklearn.decomposition / sklearn.cluster
- Metrics: SVD speed, K-means convergence, silhouette
- Data: iris, mnist subset, synthetic blobs

### 3.4 Decision tree classifier (Phase BENCH-TREE, priority ★)
- vs sklearn.tree.DecisionTreeClassifier, rpart (R)
- Metrics: training time, accuracy, leaf count

### 3.5 Time series (Phase BENCH-TS, priority ★★)
- vs statsmodels.tsa (Python), forecast / fable (R)
- Metrics: ARIMA AIC, Holt-Winters forecast RMSE, ACF/PACF agreement
- Data: AirPassengers (R standard), synthetic AR(2)/seasonal

### 3.6 Survival analysis (Phase BENCH-SURV, priority ★)
- vs lifelines (Python), survival (R)
- Metrics: KM/Cox PH coefficient agreement, log-rank p-value, runtime
- Data: lung dataset (from R survival)

### 3.7 Classification metrics + CV + interpretability (Phase BENCH-MLEVAL, priority ★)
- vs sklearn.metrics / sklearn.model_selection / sklearn.inspection
- Numerical agreement primary, speed secondary

### 3.8 Multiple testing + bootstrap + effect size (Phase BENCH-INFER, priority ★)
- vs scipy.stats / statsmodels / R (p.adjust, boot, pwr)
- Numerical agreement primary

### 3.9 Data manipulation (Phase BENCH-DATA, priority ★★)
- vs pandas (Python), dplyr/data.table (R)
- Metrics: CSV read, groupBy aggregate, join, pivot
- Data: 1M-row CSVs, multi-file joins
- **Note**: most operations are native Hackage `dataframe`; only the hanalyze extensions (pivot_wider/one-hot/lag/rolling) are unique

### 3.10 DoE (Phase BENCH-DOE, priority ★)
- vs pyDOE / pyDOE2 (Python), DoE.base / qualityTools (R)
- Metrics: orthogonal-array generation, optimal designs, power calculation

### 3.11 Visualisation (Phase BENCH-VIZ, priority ☆)
- vs matplotlib / ggplot2
- Not numerical — qualitative comparison of feature coverage and output quality
- HTML/PNG/SVG output, Mermaid, interactive widgets

### 3.12 HBM (Phase BENCH-HBM, priority ★)
- vs PyMC, Stan, NumPyro
- Metrics: posterior agreement on the same model (e.g. 8-schools), sampling speed, ESS/sec
- "PyMC compatibility" already confirmed for Truncated/Censored/MvNormal/LKJ/etc. in [docs/02-pymc-comparison.md](../02-pymc-comparison.md)

## 4. Structural strengths and weaknesses

### Strengths (Haskell-native)

1. **Type safety**: matrix-shape and dtype mismatches caught at compile time
2. **Algorithmic accuracy**: GHC optimisations + careful numerics → machine precision in many domains
3. **Module structure**: pure FP composability, easy to refactor
4. **HBM polymorphic DSL**: free-monad encoding allows 4 interpretations of the same model
5. **Dirty-data defence**: a feature largely absent from pandas/R (Janitor only partially overlaps)
6. **Single binary**: no Python interpreter required, complete CLI

### Structural weaknesses (hard to close without C/FFI)

1. **Cython/Fortran-class SIMD**: hanalyze is 3-5× slower on large element-wise operations
2. **GIL-free threading**: a Haskell win, but BLAS contention can make it counterproductive (confirmed in F5 phase)
3. **Ecosystem breadth**: no equivalent of sklearn's specialised extensions or R's Bioconductor / domain packages
4. **Learning curve**: Haskell's type system adoption cost

## 5. Conclusion

**hanalyze beats scipy/skopt/pymoo on accuracy in many domains**, but **large-scale ML routines lag sklearn's Cython native loops on speed**.

Suitable for research / experimentation / mid-scale practical use. For high-throughput production, sklearn / pytorch / R remain the recommended choices.

---

## See also

- Benchmark details: [bench/results/SUMMARY.md](../../bench/results/SUMMARY.md)
- How to run benchmarks: [bench/README.md](../../bench/README.md)
- PyMC compatibility: [docs/02-pymc-comparison.md](../02-pymc-comparison.md)
- Per-feature usage docs: subdirectories under [docs/](../)
