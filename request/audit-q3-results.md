# Q3 修正結果 + Q2-B 計測まとめ (2026-05-14)

`audit-q1-findings.md` の赤・黄項目について bench で実測 → 該当箇所を修正。
全 254 tests pass。

## 確定した修正 (3 件)

### 1. Stat.VI ADVI: chained zipWith thunk leak (commit `7afcfdd`)

| iters | K | residency_pre | residency_post | 削減率 |
|---|---|---|---|---|
| 100 | 20 | 940 KB | 92 KB | -90% |
| 1000 | 20 | 8.6 MB | 127 KB | -98.5% |
| 10000 | 20 | 85 MB | 487 KB | **-99.4%** |

`writeIORef muRef (zipWith (+) mu dxMu)` 等 4 箇所が lazy で、iter T で
`zipWith (+) thunk_{T-1} ...` の T 階層チェーンが retain されていた。
`Control.DeepSeq.force` + bang pattern で各 IORef 書込み + 中間値 (elboV /
gMu / gOm) を NF 化。**residency が iter 数とほぼ独立**になった。

副作用として総 alloc は同じ (deferred work が前倒し評価) → wall time は 2x
程度増えた。次回は `LA.Vector` + BLAS 化で alloc/time とも下げる。

bench: `bench-mem-vi`

### 2. Optim.Adam runAdamMaximize: 同種 leak (commit `c28c106`)

`Hanalyze.Stat.VI` と完全に同根 (Adam 一般実装)。`writeIORef xRef x' /
m1Ref m1' / m2Ref m2'` を全て `force` 経由に。runAdam / runAdamMinimize は
全部このループを再利用するので副作用範囲は広い。254 tests pass。

bench: 個別 bench は未作成 (Stat.VI bench で同等のパスを測定済み)

### 3. DataIO.Preprocess.collectInOrder: O(n²) アグリゲーション (commit `86a27d4`)

| n | alloc_pre | alloc_post | time_pre | time_post |
|---|---|---|---|---|
| 1k | 5.5 MB | 1.6 MB | 1.8 ms | 3.0 ms |
| 10k | 305 MB | 14 MB (-95%) | 38 ms | 6.5 ms (-83%) |
| 50k | **10.4 GB** | 71 MB (-99.3%) | **1.2 s** | 35 ms (-97%) |
| 200k | (OOM) | 282 MB | (×) | 135 ms |

旧実装は `foldl + lookup + vs ++ [v]` の三重で per-row O(n) → 全体 O(n²)。
groupBy {Mean,Sum,Min,Max,Median,Count,Aggregate} の 7 関数全てが経由する
hot path。Map に (初出 index, [value] in reverse) を保持 → O(n log n) に。

bench: `bench-mem-aggregate`

## 計測のみ (leak なし)

| bench | 観測 | 判定 |
|---|---|---|
| `bench-mem-mcmc` (MH 10000 K=20) | residency 78 KB constant | ✅ Data.Map.Strict の値 WHNF + spine strict が効いている |
| `bench-mem-mcmc` (HMC/NUTS) | productivity 92-99% | ✅ chainSamples の線形成長は意図通り |
| `bench-mem-nsga2` (1000 gen × 100 pop) | residency 281 KB constant | ✅ generationLoop の前世代参照は GC で回収 |
| `bench-mem-bo` (1D 100 iter / 10D 50 iter) | residency < 1 MB | ✅ history は小、GP 再 fit が時間支配だが leak なし |
| `bench-mem-vi` (post-fix) | 上述 | ✅ |
| `bench-mem-aggregate` (post-fix) | 上述 | ✅ |

## 未実施 / スキップ

| bench | 理由 |
|---|---|
| `bench-mem-regrid` | `Map.insertWith (++) [(z,y)] acc` を再分析 → 各 insert で WHNF 強制 + 1 cons cell のみ生成、O(n) total。leak なし。 |
| `bench-mem-hbm-grad` | Forward AD は call-stack に閉じる。retain なし (推定)。優先度低。 |
| `bench-mem-report-html` | RAM ではなく disk size の話 (HTML に n=10⁴ curve を埋込むと数 MB)。今回の対象外。 |

## audit 残課題 (将来)

1. **Stat.VI と Optim.Adam の BLAS 化**: 上述 leak fix の副作用で wall time が
   2x 増えたので、`LA.Vector` 経由に書き直すと元の速度を取り戻しつつ、追加
   の alloc 削減が見込める。
2. **Optim.Adam の history**: iter 数だけ全 trajectory を retain。ユーザが要らない
   ケースが多いので、`adamSaveHistory :: Bool` フラグで opt-out できると親切。
3. **MCMC chainSamples の Storable 化**: 現状 `[Map Text Double]` で、長 chain
   で Map のオーバーヘッドが大。`(Vector Text, Matrix Double)` 形式の方が
   メモリ効率良いが API 互換性破る。priority 低。
4. **`hp2pretty` での詳細 heap profile** は今回未実施。leak が残った疑い時の次手。

## bench 一覧 (cabal build && run)

```
bench-mem-vi         iters K          # ADVI
bench-mem-aggregate  n_rows n_groups   # groupByMean
bench-mem-nsga2      gens pop dim     # ZDT1
bench-mem-bo         iters [dim]      # Forrester / sphere
bench-mem-mcmc       sampler iters K  # mh / hmc / nuts
```

実行時は必ず `+RTS -s -M<cap>m` を付ける (cap=256m を default に、漸増)。
