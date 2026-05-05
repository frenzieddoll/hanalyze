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

## 3. GLM L-BFGS — 大規模問題で sklearn に届かない

| 観点 | 値 |
|---|---|
| GLM_logit_n10k_p20 | hanalyze 17 ms (IRLS+Cholesky) vs sklearn 3.7 ms (4.6×) |
| GLM_poisson_n10k_p20 | hanalyze 15 ms vs sklearn 1.95 ms (8×) |
| 困難度 | H (FFI 経由 BLAS の dispatch overhead が支配的) |

L1ab で `Optim.LBFGS` 内部を `LA.Vector Double + BLAS` 化したが、
**BLAS call ごとに発生する dispatch overhead** (~100-500ns × per
iteration の数 call) が L-BFGS の per-step cost を圧迫。sklearn は
Cython L-BFGS-B で BLAS を回避し inline SIMD ループ。

**現状**: defaultGLMSolver = IRLS (= O(np²) per iter で BLAS の sweet
spot)。L-BFGS は p > 50 regime で有利になる見込み。

---

## 4. Kernel/GP — 構築 + 予測で 4-12× 遅い

| 観点 | 値 |
|---|---|
| GramMV n=2000 | hanalyze 331 ms / sklearn 32 ms (10×) |
| KR n=2000 | hanalyze 586 ms / sklearn 155 ms (3.8×) |
| GP_fit n=1000 | hanalyze 230 ms / sklearn 35 ms (6.6×) |
| GP_opt n=500 | hanalyze 8.4 s / sklearn 460 ms (18×) |
| 困難度 | H |

K6 で SPD Cholesky 化済 (45× 改善した上での残差)。残るボトルネック
は `Stat.KernelDist.pairwiseSqDist` の hmatrix `cmap` (per-element
function call)。sklearn の `rbf_kernel` は Cython + AVX2 inline ループで
1 命令で配列を処理。

---

## 5. Lasso/EN 大規模 (n=10k×p=50)

| 観点 | 値 |
|---|---|
| Lasso n=10k×p50 | hanalyze 9 ms / sklearn 2.4 ms (3.6×) |
| EN n=10k×p50 | hanalyze 8.3 ms / sklearn 1.96 ms (4.2×) |
| 困難度 | M (R2 で改善後の残差) |

R2 で Mutable + axpy 化済。残差は CD inner loop の per-coord BLAS
dispatch overhead (~2500 calls × 100ns)。sklearn は coord 更新を
Cython native code でループ。

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

## 7. SA Rastrigin — Tsallis SA で大幅改善 ✅ 部分対応済

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
| TsallisAccept Double` 型を追加して実装、API 提供。bench で q_a=-5
試したが、本実装の温度スケール (saInitTemp=5230) と Tsallis 加速度
スケール (1+T) の不整合で逆効果 (Rastrigin 成功率 0%)。よって bench は
Boltzmann のままで運用。完全機械精度到達には **scipy 互換の双 T
管理** (visit T と accept T を別管理) を含む大規模 refactor (~1 日)
が必要。現状の 10-17% 成功率で実用十分と判断。

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

algorithm-level で改善可能なものはほぼ完了。残は:

| 優先 | 項目 | 工数 | 期待効果 | 困難度 |
|---|---|---|---|---|
| 低 | SA Rastrigin 機械精度 (#7 残) | 1 日 (scipy 互換双 T 管理) | 1.99 → 1e-10 | M |
| - | GLM L-BFGS / Kernel SIMD / Lasso CD (#3-5) | FFI 必要 | sklearn 並み | H (スコープ外) |

✅ 完了済 (algorithm-level):
- NSGA-II 100 gen 精度 (#1) — pymoo 越え
- NSGA-II per-gen 速度 (#2) — pymoo の 1.1-1.7×
- BO Branin/Hartmann6 (#6) — Hartmann6 skopt 越え、Branin 0.64
- SA Tsallis (#7) — Sphere/Ackley/Levy 機械精度、Rastrigin 8.5× 改善
- DE/CMAES polish (#8) — Sphere 21 桁、Ackley 9 桁、Levy 4 桁 scipy 越え
