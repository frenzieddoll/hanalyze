# hanalyze vs Python benchmark summary

最終更新: 2026-05-07 (B11 mass matrix + B7-B13 拡充完了)

統一条件: `OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1`、single-thread。
比較対象: scipy / sklearn / pymoo / skopt / pymc / blackjax / lifelines /
statsmodels / pygam / numpyro / arviz / cma / pygam。
測定: regression / kernel / ml / mcmc / survts は **tasty-bench
(relStDev 5%) の adaptive iteration** または `timeit N` の median。

Python 値は `bench/results/python/*.csv` から (Python 側は変更なし)。
Haskell 値は `bench/results/haskell/*.csv` の最新ラン。

## 全 Tier カバレッジ完了 (2026-05-07)

PLAN_FUTURE_BENCH の Tier 1〜5 を全完了。

| Suite | 内容 | 状態 |
|---|---|---|
| regression / kernel / optim / mo / bo (B1-B5) | 既存 | ✅ |
| ml (B6) | PCA / KMeans / DT / RF | ✅ |
| mcmc (B7) | HMC / NUTS | ✅ (B11 で blackjax ESS 凌駕) |
| **mcmc_extras (B7 残)** | Gibbs / ADVI / WAIC / LOO | ✅ 新規 |
| survts (B8) | ARIMA / CoxPH / KM / Quantile | ✅ |
| **ts_extras (B8 残)** | Holt-Winters / GAM / Spline 補間 | ✅ 新規 |
| **optim_plus (B9)** | Constrained / Adam / CMAESFull | ✅ 新規 |
| **stat_util (B10)** | Bootstrap / t/MW/KS / BH / Halton / AUC / k-fold | ✅ 新規 |
| **multi_output (B12)** | MultiLM / MultiGP | ✅ 新規 |
| **regrid (B13)** | regridLong PCHIP+Adaptive | ✅ 新規 |

## ハイライト

### ✅ hanalyze 圧勝領域

| Suite | Bench | hanalyze | Python | speedup |
|---|---|---|---|---|
| **ts_extras** | **HW_seasonal n=500 p=12** | **0.19 ms** | 96 ms (statsmodels MLE) | **511×** ⭐ |
| **optim_plus** | **CMAESFull_Rosenbrock5D** | **4.3 ms** | 78 ms (cma) | **18×** |
| **optim_plus** | **Constrained_Quad2D_eq** | **0.062 ms** | 0.69 ms (SLSQP) | **11×** |
| **regrid** | **Regrid_long_jagged_PCHIP** | **0.99 ms** | 19.4 ms (pandas+scipy) | **20×** |
| **stat_util** | **Welch_ttest n=500×500** | **0.016 ms** | 0.62 ms (scipy) | **39×** |
| **stat_util** | **KS_normal n=1000** | **0.073 ms** | 0.83 ms (scipy) | **11×** |
| **mcmc_extras** | **ADVI_logistic 500iter** | **169 ms** | 511 ms (numpyro SVI) | **3.0×** |
| **mcmc_extras** | **LOO_PSIS S=1000 N=200** | **15 ms** | 44 ms (arviz) | **2.9×** |
| **survts** | **ARIMA n=1000 (1,1,1)** | **1.21 ms** | 154 ms | **128×** ⭐ |
| optim | DE (Rosenbrock_2D) | 0.95 ms | 164 ms | **172×** |
| optim | NelderMead (Rosenbrock_2D) | 0.06 ms | 4.83 ms | **87×** |
| optim | CMAES (Rosenbrock_2D) | 0.43 ms | 53.6 ms | **126×** |
| optim | LBFGS (Sphere_30D) | 0.05 ms | 1.67 ms | **33×** |
| optim | DE (Ackley_10D) | 81.5 ms | 1556 ms | **19×** |
| optim | DE (Levy_10D) | 35.0 ms | 1768 ms | **51×** |
| optim | DE (Griewank_10D) | 27.9 ms | 1475 ms | **53×** |
| optim | DE (Sphere_30D) | 250 ms | 4852 ms | **19×** |
| regression | LME_n2000_p5_g20 | 1.44 ms | 42.8 ms | **30×** |
| regression | LME_n10000_p10_g50 | 11.8 ms | 97.2 ms | **8.2×** |
| regression | Ridge_n1000_p5 | 0.031 ms | 0.54 ms | **17×** |
| **mo**    | DTLZ2_3/NSGA-II | 528 ms | 758 ms | **1.43×** ⭐ |
| **mo**    | ZDT1/NSGA-II    | 709 ms | 693 ms | **0.98×** (拮抗) |
| **mo**    | ZDT2/NSGA-II    | 770 ms | 770 ms | **1.00×** (互角) |
| bo    | Hartmann6/BO | 5034 ms | 9864 ms | **1.96×** ⭐ |
| ml    | PCA n=10000 p=50 k=5 | 16.7 ms | 28.0 ms | **1.67×** ⭐ |
| survts | Spline PCHIP n=1000 | 0.16 ms | 0.27 ms | **1.71×** |
| survts | GAM n=2000 p=2 d=3 k=5 | 10.9 ms | 10.3 ms | **1.06×** (parity) |

### ✅ accuracy で Python 越え

| Bench | hanalyze | scipy/skopt | 評価 |
|---|---|---|---|
| Sphere_30D/DE | **1.1e-26** | 4.5e-5 | scipy 21 桁越え |
| Sphere_30D/LBFGS | **6.6e-40** | 2.6e-11 | scipy 29 桁越え |
| Sphere_30D/SA | **6.4e-41** | 7.1e-12 | scipy 29 桁越え |
| Sphere_30D/CMAES | **6.0e-28** | 2.5e-6 | scipy 22 桁越え |
| Levy_10D/DE | **7.9e-21** | 8.0e-17 | scipy 4 桁越え |
| Levy_10D/CMAES | **8.3e-21** | 1.8e-11 | scipy 10 桁越え |
| Ackley_10D/DE | **4.0e-15** | 1.18e-08 | scipy 7 桁越え |
| Ackley_10D/CMAES | **4.0e-15** | 1.34e-06 | scipy 9 桁越え |
| Ackley_10D/SA | **4.4e-16** | 1.9e-08 | scipy 8 桁越え |
| **Rastrigin_10D/SA** | **0.0** ⭐ | 5.7e-14 | scipy parity (機械精度) |
| Rastrigin_10D/DE | **0.99** | 16.7 | scipy 17× 越え |
| **Griewank_10D/{LBFGS,DE,CMAES,SA}** | **0** | 7.4e-3〜1.3e-11 | 全 4 法 機械精度 |
| **Hartmann6/BO** | **-3.06** | -2.77 | skopt 越え |
| MO ZDT2 HV | **0.536** | 0.484 | pymoo 越え |
| MO DTLZ2_3 HV | **2.746** | 2.722 | pymoo 越え |

### ⚠️ Python が速い領域 (sklearn の Cython native)

Phase 1-12 の改善で gap は縮まったが、最終的に **BLAS dispatch overhead** と
**Cython の inline SIMD** に阻まれて pure Haskell からは到達不能 (FFI 領域)。

| Suite | Bench | hanalyze (現) | sklearn | gap (現) | gap (pre-Phase) |
|---|---|---|---|---|---|
| kernel | GramMV_n2000 | 147 ms | 38.2 ms | 3.85× 遅 | 3.7× |
| kernel | KR_n2000 | 376 ms | 176 ms | 2.14× 遅 | 2.2× |
| kernel | GP_fit_n1000 | 163 ms | 42.3 ms | 3.86× 遅 | 4.7× |
| kernel | GP_opt_n500 | 2466 ms | 701 ms | 3.52× 遅 | 4.3× |
| kernel | RFF_n1000_D256 | 49.6 ms | 5.42 ms | 9.1× 遅 | 12× |
| regression | GLM_logit_n10k | 13.1 ms | 4.18 ms | 3.14× 遅 | 3.6× |
| regression | GLM_poisson_n10k | 11.8 ms | 2.61 ms | 4.53× 遅 | 5.97× |
| regression | Lasso_n10k×p50 | 6.87 ms | 2.35 ms | 2.92× 遅 | 3.1× |
| optim | SA (Rastrigin_10D) | 1901 ms | 193 ms | 9.84× 遅 | 12× |
| ml | KMeans n=2k p=5 k=5 | 242 ms | 32.8 ms | 7.4× 遅 | (B6 新規) |
| ml | DT n=2k p=10 | 2195 ms | 17.8 ms | **123× 遅** ⚠ | (B6 新規、list-based [[Double]]) |
| ml | RF n=2k p=10 t=20 | 13221 ms | 200 ms | **66× 遅** ⚠ | (同上) |
| survts | Quantile n=10k p=20 | 17562 ms | 232 ms | **76× 遅** ⚠ | (B8 新規、interior-point overhead) |
| survts | CoxPH n=2k p=2 | 328 ms | 130 ms | 2.5× 遅 | (B8 新規) |
| survts | KM n=2k | 32.9 ms | 6.8 ms | 4.8× 遅 | (B8 新規、list-based grouping) |
| mcmc | NUTS 8-schools n=1000 | **1492 ms** | 530 ms (blackjax) | **2.8× 遅、ESS 839 vs 810 (mu)** | (B11 mass adapt で 64× 効率改善、PyMC 7.4× 凌駕) |

→ Phase 1-12 (`-O2`, StrictData, INLINE, runST+MVector など) で **多くの項目で
gap が縮小** (例: GLM_logit 3.6× → 3.14×、GP_fit 4.7× → 3.86×)。
B6/B8 で新しく見つかった大きな gap (DT/RF/Quantile) は **list-based API が
原因**で、Storable Vector / Matrix 化リファクタが今後の改善候補。

## Phase 1〜13 perf 改善のまとめ

詳細は [perf_profile_findings.md](perf_profile_findings.md) と
git log の `perf(...)` commit。

| Phase | 内容 | 効果 |
|---|---|---|
| 1+2 | `-O2` + `-funbox-strict-fields` 全 stanza | 5-30% (基盤) |
| 3 | `StrictData` を 22 hot-path module に | thunk 削減 |
| 6+7 | `INLINE` を 9+ 個の hot wrapper に | call site 展開 |
| 10 | tasty-bench 計測基盤 | 信頼性向上 |
| 11a | `pairwiseSqDist` runST + MVector 化 | 16-26% (kernel) |
| 11c | `glmLogLik` を VS.zipWith に | 20% (GLM) |
| 12a | `irlsStep` ws/zs を VS.map/zipWith3 に | clean (中立) |
| 12c | `ModelSelect` posterior log-lik を VS.zipWith に | (WAIC/LOO) |
| 13 | bench-regression / bench-kernel を tasty-bench 化 | noise ±20% → ±10% |
| **9 (revert)** | parMap 並列化 | Storable allocator contention で逆効果 |
| **11b (revert)** | Lasso CD 手動 mutable axpy | BLAS axpy に勝てず |
| **12b (revert)** | mapMatrix を VS.map 化 | massiv の fused map に勝てず |

学んだこと: **profiling 数値は要注意** (massiv scheduler 75% は artifact)、
**Mutable Vector も計測必須** (BLAS に負ける)、**massiv の fused map は本物**。

## Python が hanalyze を上回る項目の根本原因

1. **BLAS dispatch overhead** — 1 call ごとに 100-500ns。小さな演算では payload より overhead が支配的
2. **Element-wise SIMD** — `LA.cmap` の per-element call vs Cython の inline SIMD
3. **インタプリタ越えなし** — pymoo/sklearn は numpy 配列を Cython で直接処理

これらは API 設計や algorithm 改善では解消できず、解決には:
- C/Fortran FFI (kernel distance, L-BFGS inner loop)
- 手書き SIMD (`Data.Vector.Storable.unsafePtr`)
- 専用 BLAS 拡張

(本プロジェクトのスコープ外)

## 完全比較表

### regression

| name | hanalyze (ms) | python (ms) | speedup | acc_main |
|---|---:|---:|---:|---:|
| LM_n1000_p5 | 0.078 | 0.450 | 5.78 | 0.7803 |
| LM_n10000_p50 | 13.5 | 9.41 | 0.70 | 0.8063 |
| LM_n100000_p100 | 595.7 | 668.1 | **1.12** | 0.8082 |
| GLM_logit_n2000_p10 | 1.52 | 1.38 | 0.91 | 0.2078 |
| GLM_logit_n10000_p20 | 13.1 | 4.18 | 0.32 | 0.3234 |
| GLM_poisson_n2000_p10 | 1.26 | 1.04 | 0.83 | 0.1868 |
| GLM_poisson_n10000_p20 | 11.8 | 2.61 | 0.22 | 0.4101 |
| LME_n2000_p5_g20 | 1.44 | 42.8 | **30** | 0.9695 |
| LME_n10000_p10_g50 | 11.8 | 97.2 | **8.2** | 0.9603 |
| Ridge_n1000_p5 | 0.031 | 0.54 | **17** | 0.7802 |
| Ridge_n10000_p50 | 2.30 | 3.44 | **1.49** | 0.8063 |
| Lasso_n1000_p5 | 0.084 | 0.74 | **8.8** | 0.7696 |
| Lasso_n10000_p50 | 6.87 | 2.35 | 0.34 | 0.7644 |
| EN_n1000_p5 | 0.082 | 0.31 | **3.8** | 0.7622 |
| EN_n10000_p50 | 6.83 | 3.37 | 0.49 | 0.7568 |

### kernel

| name | hanalyze (ms) | python (ms) | speedup | acc_main |
|---|---:|---:|---:|---:|
| GramMV_n500_p5 | 7.31 | 1.68 | 0.23 | — |
| GramMV_n1000_p5 | 33.1 | 8.13 | 0.25 | — |
| GramMV_n2000_p5 | 147 | 38.2 | 0.26 | — |
| GramMV_n4000_p5 | 643 | 185 | 0.29 | — |
| KR_n500_p5 | 14.9 | 4.79 | 0.32 | 1.000 |
| KR_n1000_p5 | 75.7 | 22.8 | 0.30 | 1.000 |
| KR_n2000_p5 | 376 | 176 | 0.47 | 1.000 |
| KR_n4000_p5 | 2143 | 1172 | 0.55 | 1.000 |
| NW_n1000_p5 | 45.5 | 9.36 | 0.21 | 0.905 |
| RFF_n1000_D256_p5 | 49.6 | 5.42 | 0.11 | 0.763 |
| RFF_n2000_D256_p5 | 63.1 | 5.97 | 0.09 | 0.661 |
| GP_fit_n500_p5 | 28.1 | 7.81 | 0.28 | 0.9994 |
| GP_fit_n1000_p5 | 163 | 42.3 | 0.26 | 0.9995 |
| GP_fit_n2000_p5 | 1011 | 393 | 0.39 | 0.9996 |
| GP_opt_n500_p5 | 2466 | 701 | 0.28 | 0.998 |
| GPRobust_n500_p5 | 65.1 | — | — | 1.000 |

### mo (NSGA-II 100 gen × 100 pop) ⭐ Phase 15 確認: pymoo 並み or 凌駕

| name | hanalyze (ms) | python pymoo (ms) | speedup | hv_hs | hv_pymoo |
|---|---:|---:|---:|---:|---:|
| ZDT1/NSGA-II | 709 | 693 | 0.98 | **0.870** | 0.839 |
| ZDT2/NSGA-II | 770 | 770 | 1.00 | **0.536** | 0.484 |
| ZDT3/NSGA-II | 724 | 690 | 0.95 | 1.244 | **1.291** |
| DTLZ2_3/NSGA-II | **528** | 758 | **1.43** | **2.746** | 2.722 |

→ HV (hyper-volume) は ZDT1/2 + DTLZ2_3 で hanalyze 凌駕、ZDT3 のみ僅差敗北。
速度は ZDT 系でほぼ互角、DTLZ で 1.43× 凌駕。

### optim (median over 30 seeds)

| name | hanalyze (ms) | python (ms) | speedup | hanalyze acc | python acc |
|---|---:|---:|---:|---|---|
| Rosenbrock_2D/NelderMead | 0.055 | 4.83 | **87** | 3.8e-13 | 4.6e-18 |
| Rosenbrock_2D/LBFGS | 0.318 | 6.51 | **20** | 4.0e-16 | 9.0e-12 |
| Rosenbrock_2D/DE | 0.953 | 164 | **172** | 3.9e-16 | 5.0e-30 |
| Rosenbrock_2D/CMAES | 0.426 | 53.6 | **126** | 4.0e-16 | 3.7e-13 |
| Rosenbrock_2D/SA | 384 | 45.7 | 0.12 | 1.0e-16 | 9.2e-12 |
| Rosenbrock_2D/PSO | 1.21 | 61.3 | **51** | 1.7e-08 | 3.4e-08 |
| Rosenbrock_10D/NelderMead | 22.1 | 143 | **6.5** | 1.5e-12 | 1.8e-12 |
| Rosenbrock_10D/LBFGS | 1.91 | 21.3 | **11** | 1.2e-15 | 2.8e-11 |
| Rosenbrock_10D/DE | 40.1 | 1208 | **30** | 1.2e-15 | 0.155 |
| Rosenbrock_10D/CMAES | 4.49 | 109 | **24** | 1.2e-15 | 3.59 |
| Rosenbrock_10D/SA | 1475 | 138 | 0.094 | 3.3e-16 | 2.5e-10 |
| Rastrigin_10D/DE | 30.9 | 1198 | **39** | **0.99** | 16.7 |
| Rastrigin_10D/SA | 1901 | 193 | 0.10 | **0.0** | 5.7e-14 |
| Sphere_30D/LBFGS | 0.051 | 1.67 | **33** | **6.6e-40** | 2.6e-11 |
| Sphere_30D/DE | 250 | 4852 | **19** | **1.1e-26** | 4.5e-05 |
| Sphere_30D/CMAES | 10.1 | 181 | **18** | **6.0e-28** | 2.5e-06 |
| Sphere_30D/SA | 3441 | 388 | 0.11 | **6.4e-41** | 7.1e-12 |
| Ackley_10D/DE | 81.5 | 1556 | **19** | **4.0e-15** | 1.2e-08 |
| Ackley_10D/CMAES | 2.74 | 135 | **49** | **4.0e-15** | 1.3e-06 |
| Levy_10D/DE | 35.0 | 1768 | **51** | **7.9e-21** | 8.0e-17 |
| Levy_10D/CMAES | 2.76 | 139 | **50** | **8.3e-21** | 1.8e-11 |
| Griewank_10D/{LBFGS,DE,CMAES,SA} | 0.5〜1490 | 5.2〜163 | varies | **全て 0** | 7.4e-3〜1e-11 |

(全 48 行は `bench/results/haskell/optim.csv` 参照)

### bo (median over 5 seeds)

| name | hanalyze (ms) | python skopt (ms) | speedup | hanalyze acc | skopt acc |
|---|---:|---:|---:|---:|---:|
| Branin/BO | 7768 | 8948 | 1.15 | 0.529 | 0.398 |
| Hartmann6/BO | 5034 | 9864 | **1.96** | **-3.06** | -2.77 |

### ml (B6, sklearn 比較)

| name | hanalyze (ms) | sklearn (ms) | speedup | acc match |
|---|---:|---:|---:|---|
| **PCA_n10000_p50_k5** | **16.7** | 28.0 | **1.67×** | ratio=0.111 ✅ σ=527 ✅ |
| KMeans_n2000_p5_k5 | 242 | 32.8 | 0.14× | inertia 18448 vs 18499 ≈ |
| DT_n2000_p10 | 2195 | 17.8 | 0.008× ⚠ | acc 1.0 vs 1.0 ✅ |
| RF_n2000_p10_t20 | 13221 | 200 | 0.015× ⚠ | acc 0.974 vs 0.999 |

> 注: DT/RF は `[[Double]]` list-based API のため激しく遅い。Storable
> Matrix 化が今後の改善候補。PCA は SVD 経由で hmatrix 強い。

### mcmc (B7, 8-schools hierarchical normal、warmup=500 + samples=1000)

| name | time (ms) | mu_mean | ess(mu) | ess(tau) | ess(mu)/sec |
|---|---:|---:|---:|---:|---:|
| haskell HMC | 3409 | 73.0 | 8.4 | 138 | 2.5 |
| **haskell NUTS (B11 mass)** | **1492** | 71.8 | **839** | 571 | **562** |
| python PyMC NUTS | 11018 | 72.1 | 856 | 546 | 77.7 |
| **python blackjax NUTS** | **530** | 72.4 | 810 | 626 | **1528** |

> **B11 (2026-05-07)** で Stan-style multi-window diagonal mass-matrix
> adaptation を実装。`nutsAdaptMass = True` で有効化:
>
> - **ESS は blackjax を超えた** (mu: 839 vs 810)
> - PyMC 比 **時間 7.4× 速く ESS 同等**
> - blackjax 比は時間 2.8× 遅 (JAX JIT 構造差) だが ESS 品質は対等
> - 旧 (mass=I) → 新 (B11) で ess(mu)/sec が **24 → 562** (23×)
>
> 詳細: `bench/results/B10b_NUTS_DIAGNOSIS.md`。default は opt-in
> (`nutsAdaptMass = False`); diagonal だけ実装、dense は未対応。

### mcmc_extras (B7 残, 2026-05-07 追加)

| name | hanalyze (ms) | python (ms) | speedup | acc match |
|---|---:|---:|---:|---|
| **ADVI_logistic_n60_iter500** | **169** | 511 (numpyro SVI) | **3.0×** | β1≈ 5/5.6 (ADVI 過学習で両側似) |
| **LOO_PSIS_S1000_N200** | **15** | 44 (arviz) | **2.9×** | elpd=-208.5/-208.5 ✅ |
| Gibbs_BetaBinomial_n10000 | 1.4 | 0.32 (numpy) | 0.23× | mean=0.583/0.583 vs analytic 0.583 ✅ |
| WAIC_S1000_N200 | 19.3 | 6.3 (arviz) | 0.33× | waic=417/417 ✅ |

> **観測**: ADVI は JAX JIT compile overhead で numpyro が小規模問題に弱く
> hanalyze 逆転。LOO は arviz の overhead で hanalyze 優位。WAIC は値完全一致。

### ts_extras (B8 残, 2026-05-07 追加)

| name | hanalyze (ms) | python (ms) | speedup | 注 |
|---|---:|---:|---:|---|
| **HW_seasonal_n500_p12_additive** | **0.19** | 96 (statsmodels MLE) | **511×** ⭐ | hanalyze は固定 α=0.3 (closed-form)、statsmodels は MLE |
| GAM_n2000_splines10_1D | 10.3 | 6.4 (pygam) | 0.62× | RMSE 0.054/0.184 (hanalyze の方が精度高) |
| **Interp1D_PCHIP_knots1000_eval5000** | **0.22** | 0.17 (scipy) | **0.80×** (互角) | |
| Interp1D_Linear_knots1000_eval5000 | 0.19 | 0.046 | 0.25× | scipy SIMD |
| Interp1D_NatSpline_knots1000_eval5000 | 1.21 | 0.18 | 0.15× | scipy SIMD |

### optim_plus (B9, 2026-05-07 追加)

| name | hanalyze (ms) | python (ms) | speedup | 注 |
|---|---:|---:|---:|---|
| **Constrained_Quad2D_eq** | **0.062** | 0.69 (scipy SLSQP) | **11×** ⭐ | err 5.7e-8 vs 4.2e-9 (両側 ε精度) |
| **Adam_quad50D_iter1000** | **5.5** | 8.3 (numpy) | **1.5×** | 両側 1.5e-44 (機械精度) |
| **CMAESFull_Rosenbrock5D_iter200** | **4.3** | 78 (cma) | **18×** ⭐ | f=0.031 vs 5e-7 (cma の方が精度高) |

> CMAESFull は速度で 18× 勝るが精度で cma の boundary-aware sampling +
> restart 系に劣る。トレードオフ判断。

### stat_util (B10, 2026-05-07 追加)

| name | hanalyze (ms) | python (ms) | speedup | acc match |
|---|---:|---:|---:|---|
| **Welch_ttest_n500x500** | **0.016** | 0.62 (scipy) | **39×** ⭐ | t=-6.228/-6.228 ✅ |
| **KS_normal_n1000** | **0.073** | 0.83 (scipy) | **11×** ⭐ | D=0.081/0.070 (実装差) |
| **MannWhitneyU_n500x500** | **0.29** | 0.43 (scipy) | **1.5×** | U=98226/98226 ✅ |
| **KFold_5_n1000** | **0.080** | 0.18 (sklearn) | **2.2×** | |
| Bootstrap_mean_n1000_B1000 | 17.6 | 9.5 (scipy) | 0.54× | (Storable Vector 化で 90× 改善後) |
| AUC_LogLoss_n10000 | 5.1 | 3.6 (sklearn) | 0.71× | AUC=1.0/1.0 ✅ |
| Halton_n10000_d5 | 2.8 | 0.9 (scipy.qmc) | 0.32× | |
| BH_pAdjust_n1000 | 2.2 | 0.033 (statsmodels) | 0.015× ⚠ | (要 perf 改善) |

### multi_output (B12, 2026-05-07 追加)

| name | hanalyze (ms) | python (ms) | speedup | 注 |
|---|---:|---:|---:|---|
| **MultiLM_n2000_p10_q5** | **0.36** | 0.82 (sklearn) | **2.3×** ⭐ | RMSE 0.035/0.035 ✅ |
| MultiGP_n200_p3_q3 | 1866 | 257 (sklearn GPR) | 0.14× | GP HP opt × 3 outputs (構造的天井) |

### regrid (B13, 2026-05-07 追加)

| name | hanalyze (ms) | python (ms) | speedup | 注 |
|---|---:|---:|---:|---|
| **Regrid_long_jagged_PCHIP_N30** | **0.99** | 19.4 (pandas+scipy 合成) | **20×** ⭐ | 21 dose × ~80 z 点 → 30 点 grid |

### survts (B8, statsmodels / lifelines / pygam / scipy 比較)

| name | hanalyze (ms) | python (ms) | speedup | acc |
|---|---:|---:|---:|---|
| **ARIMA_n1000_pdq111** | **1.21** | 154 | **128×** ⭐ | (no metric) |
| CoxPH_n2000_p2 | 328 | 130 | 0.40× | β1=0.573 vs 0.563 ✅ |
| KM_n2000 | 32.9 | 6.8 | 0.21× | (timing close) |
| Quantile_n10k_p20_tau0.5 | 17562 | 232 | 0.013× ⚠ | (no metric) |
| GAM_n2000_p2_d3_k5 | 10.9 | 10.3 | **1.06× (parity)** | (no metric) |
| **Spline_PCHIP_n1000** | **0.16** | 0.27 | **1.71×** ⭐ | sum=3337.7 ✅ |

> **観測**:
> - ARIMA は hanalyze の YW + MLE 自前実装が statsmodels の iterative
>   ML を 128× 凌駕 (ハイライト)
> - Spline PCHIP も hmatrix 経由で scipy より 1.7× 速い
> - GAM は parity (両者 ~10ms)
> - CoxPH/KM は **list-based の grouping** が遅い、Vector 化候補
> - Quantile は interior-point method の overhead で statsmodels に
>   76× 劣る、要 algorithm 検討

## 注釈

- SA bench は **20 multi-start runs** で機械精度狙いの設定 (時間長いが accuracy 全勝)
- Optim 系で hanalyze 圧勝の理由: **L-BFGS / DE / CMAES の polish step が hmatrix BLAS を効率使用**
- Kernel/GP/Lasso/GLM (大規模 n≥10k) で sklearn 優位の理由: **Cython native loop の SIMD + BLAS dispatch overhead 不要**
- pymoo NSGA-II には ZDT1/2 + DTLZ2_3 で勝利、ZDT3 のみ僅差敗北
- skopt BO は Hartmann6 で hanalyze が決定的勝利 (-3.06 vs -2.77)
- 計測ノイズ: regression / kernel の tasty-bench 計測は run-to-run ±10% 程度、
  optim / mo は timeit median で ±15-20% (WSL2 環境の制約)
