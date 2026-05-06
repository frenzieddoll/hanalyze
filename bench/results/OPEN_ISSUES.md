# Open issues — hanalyze vs Python ベンチで未達成の項目

最終更新: 2026-05-06 (Phase 1〜13 perf 改善後)

凡例: 困難度 — H = 高 (FFI/C 拡張が必要、pure Haskell + hmatrix の枠
を越える), M = 中 (200-500 行のアルゴリズム再実装), L = 低-中 (調整・
トレードオフ判断で対応可)

---

## 全 algorithm-level 改善は完了 ✅

2026-05-06 時点で、過去に open だった全 8 項目について改善作業が完了しました。
詳細は [SUMMARY.md](SUMMARY.md) と [perf_profile_findings.md](perf_profile_findings.md)。

| 項目 | 状態 | 備考 |
|---|---|---|
| #1 NSGA-II 100gen 精度 | ✅ pymoo 越え | NF1+NF3+NF4 (ZDT1/2 + DTLZ2_3 で HV 凌駕) |
| #2 NSGA-II per-gen 速度 | ✅ pymoo 並み or 凌駕 | N3+N4 + Phase 1-12 累積で DTLZ2_3=1.43× faster |
| #3 GLM L-BFGS gap | ✅ 部分対応 (gap 4.6× → 3.14×) | F2 + Phase 11c で IRLS 改善 |
| #4 Kernel/GP gap | ✅ 部分対応 (GP_opt gap 18× → 3.5×) | F4+F1+F2 + Phase 11a で大幅改善 |
| #5 Lasso/EN gap | ✅ 部分対応 (gap 3.6× → 2.92×) | F1 + Phase 1-12 |
| #6 BO Branin/Hartmann6 | ✅ Hartmann6 skopt 越え | Halton + Matern + multi-start |
| #7 SA multimodal | ✅ 全 6 問題 機械精度 | Tsallis + multi-start |
| #8 DE/CMAES 精度 | ✅ scipy 越え (Sphere 21 桁、Ackley/Levy 9-10 桁) | polish step + bound handling |

**結論**: pure Haskell + hmatrix の枠で出来る改善は全て完了。残り gap は全て
**FFI なしでは到達不能な構造的天井** (BLAS dispatch overhead、Cython の inline
SIMD レベル)。

---

## 構造的な性能天井 (FFI なしでは追従不能)

以下 3 点は **pure Haskell + hmatrix のままでは原理的に追いつけない**:

1. **BLAS dispatch overhead** — 1 call ごとに 100-500ns。小さな演算 (n×p, p≤50) では payload より overhead が支配的。Cython/Fortran は inline SIMD で BLAS 自体を回避できる
2. **Element-wise ループ** — `LA.cmap` は per-element function call。sklearn の SIMD 化された C/Cython ループに 5-10× 劣る
3. **インタプリタ越えなし** — pymoo/sklearn は numpy 配列を Cython で直接処理。hanalyze は GHC compiled だが BLAS を経由する分 dispatch コストを払う

これらは API 設計や algorithm 改善では解消できず、解決には:
- **C/Fortran FFI** (kernel distance, L-BFGS inner loop)
- **手書き SIMD** (`Data.Vector.Storable` で `unsafePtr` 操作)
- **専用 BLAS** (例: `hmatrix-blas-fast` のような fused-op 拡張)

---

## 残ギャップ詳細 (Phase 1-13 後の最新値)

| 項目 | hanalyze | sklearn/scipy | gap | 原因 | 対処 |
|---|---:|---:|---:|---|---|
| GramMV n=2000 | 147 ms | 38.2 ms | 3.85× | sklearn `rbf_kernel` は Cython | FFI |
| KR n=2000 | 376 ms | 176 ms | 2.14× | 同上 + sklearn の wrapper も Cython | FFI |
| GP_fit n=1000 | 163 ms | 42.3 ms | 3.86× | sklearn の Cholesky と elementwise が C | FFI |
| GP_opt n=500 | 2466 ms | 701 ms | 3.52× | 上記 × HP iter | FFI |
| RFF n=1000 D=256 | 49.6 ms | 5.42 ms | 9.1× | sklearn `RBFSampler` 完全 SIMD | FFI |
| GLM_logit n=10k | 13.1 ms | 4.18 ms | 3.14× | sklearn `LogisticRegression` lbfgs が Cython | FFI |
| GLM_poisson n=10k | 11.8 ms | 2.61 ms | 4.53× | 同上 | FFI |
| Lasso n=10k×p50 | 6.87 ms | 2.35 ms | 2.92× | sklearn CD inner loop が Cython | FFI |
| SA Rastrigin_10D | 1901 ms | 193 ms | 9.84× | scipy `dual_annealing` が optimized C path | 別 algorithm |

(全項目について Phase 1-13 で 5-30% gap 縮小、しかし FFI 領域では到達困難)

---

## 並列化候補 — 試行済 / 不採用

過去の試行で逆効果と判明した並列化:

- **Phase 9 (`parMap rdeepseq` / `parListChunk`)** — Storable allocator
  contention で逆効果 (詳細: `perf_profile_findings.md`)
- **Phase A (SA multi-start を `Async.mapConcurrently` 並列化)** —
  OpenBLAS の lock contention で serial 化、`-threaded` overhead 加算
- **Phase C (NSGA-II objective を parMap)** — objective が µs オーダーで
  spark 設定 overhead に負ける

**結論 (parallelism の現実)**: bench 用の cheap objective + BLAS-heavy
inner loop の組合せでは algorithm-outer 並列化は overhead で逆効果。
真に有効なのは「expensive user f (engineering simulation 等)」の場合のみ。
その用途ではユーザーが `Control.Parallel.Strategies` /
`Control.Concurrent.Async` を直接呼べば済む。

`parallel` package と `Solution` の `NFData` instance は依存に残置 (今後の
ユーザー拡張用)。

---

## 今後の方針

`OPEN_ISSUES` のスコープ (algorithm-level / pure Haskell) では完了。
さらなる改善は以下のいずれか:

1. **C/Fortran FFI 導入** — `kernel distance` / `L-BFGS inner loop` /
   `pairwise SqDist` の SIMD ループ。本プロジェクトの「全 Haskell 自前
   実装」という設計方針に反するため、別プロジェクト枠とすべき。
2. **GHC + LLVM upgrade** — GHC 9.10 / 9.12 + LLVM 19 等の組合せで
   `-fllvm` を利用。現状 GHC 9.6.7 + LLVM 22 は非互換。
3. **Hackage 公開してコミュニティから FFI patch を受ける** — 公開後の
   外部貢献に期待。

→ 公開準備 (Phase D) に移行。
