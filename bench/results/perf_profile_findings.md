# プロファイル取得 (Phase 11) — 結果まとめ

実施日: 2026-05-06
ブランチ: `feature/perf-optim`
Phase 1-7 (`-O2` + `-funbox-strict-fields` + `StrictData` + `INLINE`) 適用後の状態。

## 取得方法

```bash
cabal build --enable-profiling --enable-library-profiling bench-profile
BIN=$(cabal list-bin --enable-profiling bench-profile)
OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 "$BIN" +RTS -p -RTS <target>
# target: kr | gram | glm | lasso | psd
```

`cabal.project.local` に `profiling-detail: late-toplevel` を指定 (デフォルト
の `exported-functions` は INLINE を抑制してプロファイルが歪むため)。

## 主要 cost center (top 5)

### `psd` — `Stat.KernelDist.pairwiseSqDist` (n=2000, p=5)

| centre | %time | %alloc |
|---|---|---|
| `trivialScheduler_` (massiv) | **75.7** | 0.0 |
| `hmatrixToMassiv` (変換) | 17.5 | 32.9 |
| `multiplyR` (BLAS GEMM) | 2.7 | 32.9 |
| `pairwiseSqDist` 本体 | 0.0 | 32.9 |

**所見**: 計算自体 (BLAS GEMM) は 2.7% time のみ。**75% は massiv の
scheduler dispatch overhead**、17% が hmatrix↔massiv 変換コスト。
massiv を撤去して pure hmatrix で書き直すべき。

### `kr` — `Model.Kernel.kernelRidgeMV` (n=1000, p=5)

| centre | %time | %alloc |
|---|---|---|
| `gramMatrixMV` | 25.0 | 36.5 |
| `kernelRidgeMV` (outer) | 22.3 | 36.6 |
| `trivialScheduler_` (massiv) | **18.3** | 0.0 |
| `chol` (LAPACK Cholesky) | 14.6 | 3.3 |
| `kernelFromSqDist` | 9.0 | 0.0 |

**所見**: massiv overhead が 18% 食っている。`gramMatrixMV` は内部で
`pairwiseSqDist` を呼ぶため、psd と同じ問題が連鎖。psd を直せば
KR も改善する見込み。

### `glm` — `Model.GLM.fitGLMFull` logit (n=10000, p=20)

| centre | %time | %alloc |
|---|---|---|
| `runIRLS` | 19.3 | **51.2** |
| `multiplyR` (BLAS) | 16.8 | 1.6 |
| `glmLogLik` | 11.3 | 8.3 |
| `trivialScheduler_` (massiv) | 9.8 | 8.9 |
| `safeMu` | 9.8 | 13.7 |

**所見**: `runIRLS` が **alloc の半分** を占有。各 IRLS 反復で重み
ベクトル / fitted ベクトルを fresh allocate している可能性。Mutable
Vector 候補だがアルゴリズム的に in-place で意味が出るかは要検討。
`safeMu` 13.7% alloc も気になる (logit/probit 等の link 関数経由)。

### `lasso` — `Model.Regularized.fitRegularized L1` (n=10000, p=50)

| centre | %time | %alloc |
|---|---|---|
| `multiplyR` (BLAS) | 32.8 | 1.4 |
| `cdLoop` (CD) | 15.5 | **38.0** |
| `endOfInput` (CSV 読込) | 13.8 | 6.8 |
| `$sfoldM'` (in cdLoop) | 5.2 | **22.6** |
| `toBoundedRealFloat` (CSV) | 8.4 | 6.0 |

**所見**: CSV 読込が 13.8% も食っている (1 反復あたり 1 回読込のため
本来は除外したい — bench inner loop 外に出す)。`cdLoop` 自体は
本質的アルゴリズム部分で 38% alloc。`$sfoldM'` 22.6% alloc は
列ごとループの monadic fold が allocation を生んでいる兆候。

## アクション計画

| 優先 | Phase | 内容 | 期待効果 |
|---|---|---|---|
| 高 | 11a | `pairwiseSqDist` から massiv を撤去、pure hmatrix 化 | psd 4× 期待、KR/Gram 連鎖改善 |
| 中 | 11b | `Lasso.cdLoop` の `$sfoldM'` 22% alloc 解析 | Mutable Vector 候補? |
| 中 | 11c | `GLM.runIRLS` 51% alloc 解析 | 同上 |
| 後 | 8 | Mutable Vector 化 (11b/c が要請する場合) | 個別判断 |
| 後 | 9 | Strategies 並列化 | コア数倍 |

11a は massiv 撤去という別系統の改善 (Mutable Vector ではない) だが、
プロファイル証拠から最も効きそう。先に実施する。

## 補足: bench 計測時の注意

Lasso プロファイルで `endOfInput` (CSV パーサ) が 13.8% を占めて
いるのは、`bench-profile` の per-iter loop に `readCsvXY` が含まれない
ようにしてあるはずだが…と思って再確認したが、`runN` 内では action
は固定 (1 回パース → 200 回 fit) になっている。これは多分 `200 *
fit_alloc` < `1 * read_alloc` の関係で alloc 比が CSV 寄りになって
いるだけで、time は fit が支配的のはず。

## Phase 9 (Strategies / 並列化) — 結果: 不採用

実施日: 2026-05-06
試行: `Stat.Bootstrap.bootstrap` / `permutationTest` を
`Control.Parallel.Strategies.parListChunk rdeepseq` で並列化。

### 計測 (n=2000, reps=2000, sampleVar、3-run median)

| 設定 | median time |
|---|---|
| `-N1` (sequential) | 480 ms |
| `-N4 -A256m` | 665 ms (**38% slower**) |
| `-N4 -A256m` 単発 | 317 ms (1 度だけ観測、再現せず) |

### 結論: parallel 化は逆効果

理由 (推測):
1. **Storable Vector allocation contention** — `LA.fromList` が
   foreign C heap 経由でアロケーションするため、複数スレッドからの
   同時 alloc がシリアル化する
2. **GC pressure across threads** — n=2000 × reps=2000 = 4M Double
   分の Storable garbage を 4 スレッドで生成し、parallel GC 同期で
   overhead が拡大
3. **Memory bandwidth saturation** — sampleVar の inner loop は
   メモリ帯域律速で、コア追加の効果が小さい

### 状況により有効になる可能性

- 単発で `-A256m` 指定時に 1.81× speedup を観測したが、再現せず
- spark 単位の作業量 > ~ms オーダーで、Storable allocation を
  伴わないワークロードでは効く可能性あり (e.g. permutationTest を
  純数値だけで完結させた場合)
- MCMC chain 並列化 (`mapConcurrently`) は機能している (各 chain が
  数秒の長尺ワーク + 独立 GenIO) — Strategies と異なるパターン

### 行動

bootstrap / permutationTest への並列化は実装せず revert。
本コードベースで parMap/parListChunk が広く有効でないことが確認できたので、
今後の並列化候補は `mapConcurrently` ベース (long-running independent
chains) に限定する方針。
