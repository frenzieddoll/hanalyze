# Open issues — hanalyze vs Python ベンチで未達成の項目

(2026-05-05 時点)

凡例: 困難度 — H = 高 (FFI/C 拡張が必要、pure Haskell + hmatrix の枠
を越える), M = 中 (200-500 行のアルゴリズム再実装), L = 低-中 (調整・
トレードオフ判断で対応可)

---

## 1. NSGA-II — 同世代 (100 gen) で pymoo に到達できない ★最優先

| 観点 | 値 |
|---|---|
| ZDT1 HV @ 100 gen | hanalyze 0 / pymoo 0.84 |
| ZDT2 HV @ 100 gen | hanalyze 0 / pymoo 0.48 |
| ZDT3 HV @ 100 gen | hanalyze 0.24 / pymoo 1.29 |
| DTLZ2_3 HV @ 100 gen | hanalyze 2.69 / pymoo 2.72 |
| 500 gen での状況 | hanalyze は HV/IGD で pymoo を 3/4 問題で凌駕 |
| 困難度 | M (実装内部の subtle 差を pymoo source 級に詰める必要) |

**症状**: アルゴリズム params (popSize=100, ηc=15, ηm=20, prob_var=0.5,
mutation prob=1/d) は pymoo と完全一致しているのに、ZDT 問題で 100
gen での収束速度が 5× 程度劣る。500 gen では追いつき・追い越す。

**仮説 (要検証)**:
- `nonDominatedSort` の出力順や rank 計算の細部が異なる
- `crowdingDistance` の正規化や端点処理
- `selectTopN` での front 跨ぎ選別の挙動
- Tournament が 4-way だったり、selection pressure が違う

**対応方針**: pymoo source / Deb 2002 論文を読み、1 世代分の
trace を hanalyze ↔ pymoo で diff して原因特定 → 修正 (実装は調査後)。

---

## 2. NSGA-II per-generation 速度

| 観点 | 値 |
|---|---|
| 1 世代あたり時間 | hanalyze ~32 ms / pymoo ~5 ms (~6×) |
| ボトルネック | nonDominatedSort, crowdingDistance がリスト経由 O(MN²) |
| 困難度 | M (1-2 日の refactor; 内部表現を 2D Matrix に) |

pymoo は numpy で全要素同時処理 (vectorized non-dominated sort,
SBX, polynomial mutation 全て array op)。hanalyze はリスト/個別個体
処理。

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

## 6. BayesOpt — Branin で skopt に大幅劣後

| 観点 | 値 |
|---|---|
| Branin (true 0.398) | hanalyze 4.0 / skopt 0.398 |
| Hartmann6 (true -3.32) | hanalyze -2.83 / skopt -2.77 (hanalyze 勝) |
| 困難度 | M |

Hartmann6 (実問題に近い) では skopt 越え達成済。Branin は C∞ smooth
で defaultBayesOptConfig.boKernel = Matern52 が不利 (RBF 必要)。

**対応案**: smoothness pre-screen で kernel auto-select、または
`BayesOptConfig` に `boKernelMode = AutoKernel` を追加。実装は ~半日。

---

## 7. SA Rastrigin (pathological multi-modal)

| 観点 | 値 |
|---|---|
| Rastrigin_10D/SA | hanalyze 16.9 / scipy.dual_annealing 7.8e-14 |
| 困難度 | M |

scipy.dual_annealing は Generalized SA (Tsallis 1996) + L-BFGS-B
local restart の組み合わせ。hanalyze の SA + NM hybrid (S2/S3) では
Rastrigin の多数の局所最適から escape できず。

**対応案**: Tsallis acceptance で fat-tail 受容 + adaptive restart。
実装は ~1 日。

---

## 8. DE/CMAES の精度 (Sphere/Levy/Ackley)

| 観点 | 値 |
|---|---|
| Sphere_30D/DE | hanalyze 9.7e-3 / scipy 2.8e-5 |
| Levy_10D/DE | hanalyze 2.6e-9 / scipy 7.6e-17 |
| Ackley_10D/CMAES | hanalyze 4.4e-3 / scipy 1.4e-6 |
| 困難度 | L |

トレードオフ問題。jDE (S1b) で大幅改善済だが scipy の自動再起動には
及ばず。反復数 ↑ で追えるが速度を犠牲にする。

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

| 優先 | 項目 | 工数 | 期待効果 | 困難度 |
|---|---|---|---|---|
| 1 | NSGA-II 同世代精度 (#1) | 調査 + 修正 = 1-2 日 | 100 gen で pymoo 並み = MO 完全勝利 | M |
| 2 | NSGA-II per-gen 速度 (#2) | 1-2 日 (Matrix 化) | 5-10× 速度 | M |
| 3 | BO Branin (#6) | 半日 (kernel auto-select) | Branin 完全勝利 | M |
| 4 | SA Rastrigin (#7) | 1 日 (Tsallis) | 機械精度到達 | M |
| 5 | DE/CMAES 精度 (#8) | トレードオフ判断 | 部分改善 | L |
| - | GLM L-BFGS / Kernel SIMD / Lasso CD (#3-5) | FFI 必要 | sklearn 並み | H (今回スコープ外) |
