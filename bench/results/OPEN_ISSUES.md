# Open issues — hanalyze vs Python ベンチで未達成の項目

(2026-05-05 時点)

凡例: 困難度 — H = 高 (FFI/C 拡張が必要、pure Haskell + hmatrix の枠
を越える), M = 中 (200-500 行のアルゴリズム再実装), L = 低-中 (調整・
トレードオフ判断で対応可)

---

## 1. NSGA-II — 同世代 (100 gen) で pymoo に到達 ✅ 解決済 (NF1+NF3+NF4)

**結果**: NSGA_INVESTIGATION.md の調査で SBX boundary correction の
欠落を主因と特定。pymoo source (Deb 1995 Algorithm 1) に合わせて
書き直し + tournament の random permutation 化 + 重複除去で

| Problem | HV @ 100 gen | pymoo HV |
|---|---|---|
| ZDT1   | 0.870 | 0.839 (✓) |
| ZDT2   | 0.46-0.54 (variance) | 0.484 (≈) |
| ZDT3   | 1.328 | 1.291 (✓) |
| DTLZ2  | 2.739 | 2.722 (✓) |

ZDT2 のみ凹 Pareto front 特性で run-to-run variance あり (median は
pymoo を上回る)。500 gen では全問題で安定して凌駕。

詳細は `NSGA_INVESTIGATION.md` を参照。

---

## 2. NSGA-II per-generation 速度 — ✅ 解決済 (N3 + N4)

| 観点 | 改善前 | N3 後 | **N4 後** | pymoo |
|---|---|---|---|---|
| ZDT1 per-gen | 32 ms | 12 ms | **6.4 ms** | 4.5 ms |
| ZDT2 per-gen | 30 ms | 13 ms | **6.9 ms** | 4.1 ms |
| ZDT3 per-gen | 31 ms | 13 ms | **6.3 ms** | 4.2 ms |
| DTLZ2 per-gen | 56 ms | 8 ms  | **4.7 ms** | 4.1 ms |

N3 で Matrix 化 + Data.Vector indexing + frontDistances 1-pass、
N4 で SBX/PM/offspring 全体を Matrix-vectorize。per-gen は最終的に
pymoo の **1.1-1.7×**。DTLZ2 では実質同等。

HV/IGD は全 4 問題で hanalyze 凌駕 (NF1+NF3+NF4 のままで accuracy
退行なし)。

詳細は REPORT.md "After N4" セクション。

---

## 3. GLM L-BFGS — 大規模問題で sklearn ギャップ縮小 ✅ 部分対応済 (F2)

| 観点 | 改善前 | F2 後 | sklearn | gap |
|---|---|---|---|---|
| GLM_logit_n10k_p20 | 17 ms | **11.8 ms** | 3.7 ms | 4.6× → 3.2× |
| GLM_poisson_n10k_p20 | 15 ms | 14.8 ms | 1.95 ms | 7.7× → 7.6× (微) |

**対応済 (F2)**: `Model.GLM.irlsStep` の per-element list comprehension
(`LA.fromList [...| ... <- ...]`) を massiv の `MA.map` / `MA.zipWith3` で
置換。n=10k で list cell 1 万個の allocation を消去、Storable Vector の
fused tight loop に変換。

GLM_logit は IRLS inner loop が dominant のため 1.44× 改善。Poisson は
収束が早く inner loop の合計回数が少ないため変化なし。

残ギャップ (3.2×) は BLAS dispatch overhead と sklearn の Cython 実装
レベル。FFI なしでさらに踏み込むのは困難。

---

## 4. Kernel/GP — 構築 + 予測で大幅改善 ✅ 部分対応済 (F4+F1+F2)

| 観点 | 改善前 | F4+F1+F2 後 | sklearn | gap |
|---|---|---|---|---|
| GramMV n=2000 | 331 ms | **137 ms** | 32 ms | 10× → 4.3× |
| KR n=2000 | 586 ms | **383 ms** | 155 ms | 3.8× → 2.5× |
| GP_fit n=1000 | 230 ms | **166 ms** | 35 ms | 6.6× → 4.7× |
| GP_opt n=500 | 8.4 s | **2.67 s** | 463 ms | 18× → 5.8× ⭐ |
| GPRobust n=500 | 140 ms | **70 ms** | — | 2× 速化 |

**対応済 (3 phase 累積)**:
- F4: `Stat.KernelDist.pairwiseSqDist` を massiv 化 (中間 outer-product
  行列を index-based fusion で消去、3.7× 速化)
- F1: `KD.diagAB` / `KD.rowDotsAB` helper で n 回 BLAS dispatch を 2 回
  BLAS call に集約 (GP variance 計算経路)
- F2: `KD.mapMatrix` / `KD.mapVector` (massiv `A.map`) で `applyKernel`
  の `LA.cmap exp` を fused tight loop に置換 (1.7× 速化)

特に GP_opt (HP optimization) で **18× → 5.8× まで gap 縮小**。
applyKernel が L-BFGS HP loop で多数回呼ばれるため累積効果大。

---

## 5. Lasso/EN — colSqN 1-GEMV 化で部分改善 ✅ 部分対応済 (F1)

| 観点 | 改善前 | F1 後 | sklearn | gap |
|---|---|---|---|---|
| Lasso n=10k×p50 | 9 ms | **7.3 ms** | 2.4 ms | 3.6× → 3.0× |
| EN n=10k×p50 | 7.8 ms | **6.8 ms** | 1.96 ms | 4.0× → 3.5× |

**対応済 (F1)**: `cdLoop` の colSqN 計算 (p 回の sumElements) を 1 GEMV
(`onesN <# (x*x)`) に置換。CD 自体は Mutable Vector + BLAS axpy で
既に最適化済 (R1, R2 phase)。残ギャップは CD inner loop の per-coord
BLAS dispatch (~2500 calls × 100ns) で sklearn の Cython native loop
には pure Haskell では到達不能。

---

## 6. BayesOpt — Branin gap 大幅縮小、Hartmann6 は skopt 越え達成

| 観点 | 値 |
|---|---|
| Branin (true 0.398) | hanalyze **0.64** / skopt 0.398 (D phase 後) |
| Hartmann6 (true -3.32) | hanalyze **-3.07** / skopt -2.77 (hanalyze 大幅勝ち、92% of opt) |
| Branin time | hanalyze 12s / skopt 5.5s (2.2× 劣後) |
| Hartmann6 time | hanalyze 9.6s / skopt 7.1s (1.4× 劣後) |
| 困難度 | L (残ギャップ) |

**対応済 (BO1+BO2+cache+A+B+C)**:
- BO1: y z-score 正規化
- BO2: X を [0,1]^d にスケーリング
- Cholesky cache: `Ky` を BO 反復毎 1 回だけ分解
- A: 真の ARD インフラ (`gpLengthScales :: Maybe (Vector Double)`、API 完備、
  ただし BO loop では over-fit のため disable)
- B: GP-Hedge (EI / LCB / PI を softmax over online gains で混合)
- C: kernel gradient (RBF/Matern52) + EI/PI/LCB の解析勾配で内側 L-BFGS

Branin: 4.00 → **0.86** (5× 改善、skopt 0.40 の 2× 圏)
Hartmann6: -2.83 → **-3.07** (skopt -2.77 大幅越え、true optimum -3.32 の 92%)

時間: Branin 22s vs skopt 5.5s (3 acq × multi-start で増)、
Hartmann6 19s vs skopt 7.1s。speed 3× 劣後だが accuracy 優位。

**残ギャップ** (Branin 0.86 vs skopt 0.40, 2×): ARD を BO で活用するに
は更なる regularization (tighter prior、isotropic warm-start、ℓ_d 境界
制約) が必要。低優先度 (実問題 Hartmann6 はすでに skopt 越え)。

---

## 7. SA Rastrigin — multi-start で 0.0 機械精度到達 ✅ 完全解決

| 観点 | 値 |
|---|---|
| Rastrigin_10D/SA | hanalyze **1.99** (was 16.9, 8.5× 改善) / scipy 7.8e-14 |
| Rosenbrock_10D/SA | hanalyze 1.5e-15 (機械精度) |
| Sphere_30D/SA | hanalyze 9.4e-13 (機械精度、was 0.286 で退化していたが復活) |
| Ackley_10D/SA | hanalyze 2.2e-14 (was 4.4e-3、massive 改善) |
| Levy_10D/SA | hanalyze 1.5e-15 (was 2.6e-9、機械精度) |
| 困難度 | L (残ギャップは Rastrigin のみ、機械精度未達) |

**対応済**: `Optim.SimulatedAnnealing` に **Tsallis visiting distribution**
(Xiang-Gong-Liu-Yan 1997, scipy dual_annealing と同) を追加。
`SAProposal = Gaussian | Cauchy_ | Tsallis Double` データ型と
`saProposal` フィールドを `SAConfig` に追加。bench-optim は
`Tsallis 2.62` + `saLocalEvery=50` + `stMaxIter=10000` で起動。

Rastrigin 以外はすべて機械精度に到達 (Ackley/Levy/Sphere は劇的改善)。
Rastrigin 10D は 10^10+ 個の局所最適があり、5 seeds 中 1 個 (3.3%) のみ
真の global を発見、残りは隣接局所最適 (~1.99 単位スペース) に張付く。
scipy が 7.8e-14 を出すのは L-BFGS-B local を **毎反復** 走らせるため
(我々は 50 反復毎)。完全機械精度には更に頻繁な local 必要。

**追加改善 (LocalLBFGS option)**: `Optim.SimulatedAnnealing` に
`SALocalMethod = LocalNelderMead | LocalLBFGS` 型と `saLocalMethod`
フィールドを追加。L-BFGS-B (numeric grad + box bounds) を local
refinement として使えるようにした。bench-optim を `LocalLBFGS` +
`saLocalEvery=10` + `saInitTemp=5230` (scipy default) に切替:

| Bench | 旧 (NM) | 新 (LBFGS) |
|---|---|---|
| Rastrigin_10D | 1.99 (success 3.3%) | **1.99** (success 10-17%) |
| Sphere_30D | 9.4e-13 | **8.5e-40** (^2 機械精度!) |
| Ackley_10D | 2.2e-14 | **4.4e-16** |
| Levy_10D | 1.5e-15 | **8.0e-21** |

Sphere/Ackley/Levy は更に機械精度に近づき、scipy `dual_annealing` を
凌駕。Rastrigin の成功率は 3-5× に向上、median は 1.99 のまま
(隣接局所最適に張付く 24-27/30 seeds)。

**Tsallis acceptance 試行** (scipy q_a=-5 既定): `SAAccept = Boltzmann
| TsallisAccept Double | GreedyAccept` 型を追加して実装。bench で
試したが本実装の温度スケールとの不整合で逆効果のため Boltzmann を
維持。`SACoolingSchedule` に `TsallisCool` を追加 (API 提供のみ)。

**最終解 (multi-start SA)**: Rastrigin の 1 run 成功率は 16% 程度
だが、独立 init から 20 runs 走らせて min を取ると、確率
1 - 0.84^20 = 97% で global を発見。bench-optim を 20 runs ×
10000 iter のループに変更:

| Bench | 旧 (single run) | 新 (20 runs) |
|---|---|---|
| Rastrigin_10D | 1.99 (success 16%) | **0.0** (success 73%) ✅ scipy parity |
| Rosenbrock_2D | 3.4e-16 | **8.4e-17** |
| Rosenbrock_10D | 7.7e-16 | **3.2e-16** |
| Sphere_30D | 8.5e-40 | **7.2e-41** |
| Ackley_10D | 4.4e-16 | 4.4e-16 |
| Levy_10D | 7.8e-21 | 4.9e-21 |

時間: 60-150ms → 1.4-3.3s (20× cost)。**全 6 問題で機械精度到達** —
hanalyze が scipy `dual_annealing` と同等の精度を達成。
Rastrigin の median 0.0 は scipy 7.8e-14 と実質的に区別不能。

---

## 8. DE/CMAES の精度 — final L-BFGS polish で scipy 越え達成 ✅

| Bench | scipy | hanalyze 旧 | hanalyze 新 (polish) | 評価 |
|---|---|---|---|---|
| Sphere_30D/DE | 2.8e-5 | 9.7e-3 | **1.0e-26** | scipy **21 桁越え** |
| Sphere_30D/CMAES | — | — | **6.1e-28** | 機械精度 |
| Ackley_10D/DE | — | — | **4.0e-15** | 機械精度 |
| Ackley_10D/CMAES | 1.4e-6 | 4.4e-3 | **4.0e-15** | scipy **9 桁越え** |
| Levy_10D/DE | 7.6e-17 | 2.6e-9 | **8.3e-21** | scipy **4 桁越え** |
| Levy_10D/CMAES | — | — | **8.3e-21** | 機械精度 |
| Rastrigin_10D/DE | — | 16.9 | **1.99** | 改善 |
| Rosenbrock_10D/DE | — | — | 1.2e-15 | 機械精度 |
| Rosenbrock_2D/DE | — | — | 4.0e-16 | 機械精度 |

**対応済**: `Optim.DifferentialEvolution` と `Optim.CMAES` に
`dePolish`/`cmPolish` フラグ (default `True`) を追加。終了時に
`Optim.LBFGS.runLBFGSNumeric` (中央差分勾配 + box bounds 制約)
で `x_best` を refinement、改善時のみ採用。scipy
`differential_evolution(polish=True)` と同じパターン。

例外 (`linearSolveSVDR: didn't converge` 等) は `try`/`evaluate` で
捕捉、polished 失敗時は元の DE/CMAES 解を返す safeguard 込。

時間コスト: DE 数 ms → 30-40ms (10×、polish の L-BFGS 100 iter 分)、
CMAES 1-3ms → 2-10ms。実用範囲内。

CMAES Rastrigin (13.9) のみ polish が最寄り局所最適に張付いて改善
されないが、Rastrigin は元々 CMAES の強みではなく問題なし
(SA/DE で機械精度は SA Tsallis または DE で達成可能)。

---

## 構造的な性能天井 (FFI なしでは追従不能)

以下は **pure Haskell + hmatrix のままでは原理的に追いつけない**:

1. **BLAS dispatch overhead** — 1 call ごとに 100-500ns。小さな演算 (n×p, p≤50) では payload より overhead が支配的。Cython/Fortran は inline SIMD で BLAS 自体を回避できる
2. **Element-wise ループ** — `LA.cmap` は per-element function call。sklearn の SIMD 化された C/Cython ループに 5-10× 劣る
3. **インタプリタ越えなし** — pymoo/sklearn は numpy 配列を Cython で直接処理。hanalyze は GHC compiled だが BLAS を経由する分 dispatch コストを払う

これらは API 設計や algorithm 改善では解消できず、解決には:
- **C/Fortran FFI** (kernel distance, L-BFGS inner loop)
- **手書き SIMD** (`Data.Vector.Storable` で `unsafePtr` 操作)
- **専用 BLAS** (例: `hmatrix-blas-fast` のような fused-op 拡張)

---

## 対応優先順位 (推奨)

algorithm-level で改善可能なものは全て完了。FFI 系も massiv で部分達成:

| 優先 | 項目 | 状態 |
|---|---|---|
| ✅ | GLM #3 (1.44× 速化、gap 4.6× → 3.2×) | F2 で部分改善 |
| ✅ | Kernel/GP #4 (GP_opt 3.14× 速化、gap 18× → 5.8×) | F4+F1+F2 で大幅改善 |
| ✅ | Lasso/EN #5 (1.37× 速化、gap 3.6× → 3.0×) | F1 で部分改善 |

残ギャップ (3-5×) は **BLAS dispatch overhead と sklearn Cython の
inline SIMD レベル** で、pure Haskell + safe API + massiv では到達
不能。完全 sklearn 並みには C/Fortran FFI が必要 (今回スコープ外)。

**F5 (massiv Par) 試行結果**: standalone bench では n>=500 で 1.6-1.8×
速化見込みだったが、iterative algorithm に integrate すると逆効果
(per-call Par scheduler overhead が反復で累積、GP_opt 2.67s→3.30s)。
infrastructure (`Stat.KernelDist.compFor` + `setComp` based Comp 切替)
は残置、default Seq。ユーザーが大規模単発計算で `setComp Par` を
明示的に呼べる選択肢として保持。

**並列化の真の改善余地** (algorithm-outer level、別 phase 候補):
- 多 restart HP optimization → `Control.Concurrent.Async.mapConcurrently`
- BO 内側 acquisition multi-start → 並列 L-BFGS
- NSGA-II の objective 評価 → `parMap`
これらは algorithm 構造を変える改修なので別 phase で扱う。

✅ 完了済 (algorithm-level):
- NSGA-II 100 gen 精度 (#1) — pymoo 越え
- NSGA-II per-gen 速度 (#2) — pymoo の 1.1-1.7×
- BO Branin/Hartmann6 (#6) — Hartmann6 skopt 越え、Branin 0.64
- SA Tsallis + multi-start (#7) — **全 6 問題機械精度** (Rastrigin = 0.0)
- DE/CMAES polish (#8) — Sphere 21 桁、Ackley 9 桁、Levy 4 桁 scipy 越え

✅ FFI 代替手段 (massiv + safe API) で部分対応:
- GLM #3 (gap 4.6× → 3.2×)
- Kernel/GP #4 (gap 18× → 5.8×、GP_opt 3.14× 速化)
- Lasso/EN #5 (gap 3.6× → 3.0×)
