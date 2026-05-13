# Q1 grep audit (2026-05-14)

`request/audit-plan.md` (旧 audit) を補完する 8 軸 grep の結果。
新規ヒットを **赤 / 黄 / 緑** で分類。Q2 bench / Q3 修正の対象決定に使う。

凡例:
- 🔴 Red — leak の確度高い、Q2 bench 必須 + 高確率で Q3 修正
- 🟡 Yellow — leak の可能性あり、bench で実測して判定
- 🟢 Green — 読んだ結果問題なし(誤検知)

## 1. 厳格化漏れ (`foldl` not `foldl'`)

| 場所 | 入力規模 | 評価 |
|---|---|---|
| `Optim/LBFGS.hs:241,252` | history m (デフォルト 10) | 🟢 m 小 |
| `Optim/NSGA.hs:621` | objectives m (典型 ≤ 5) | 🟢 |
| `DataIO/CSV.hs:90` | 列数 (10s) | 🟢 |
| `DataIO/Preprocess.hs:430,569,682` | 列数〜行数 | 🟡 682 は `Map.insertWith (++) i [(z, y)] acc` で **list append** が右結合に蓄積 → regrid 大規模で leak の可能性 |
| `Stat/Distribution.hs:241` | k (二項係数、小) | 🟢 |
| `DataIO/Clean.hs:245,268` | 列数 | 🟢 |

## 2. lazy IO

全件 `BS.readFile` (strict) または `TIO.readFile` (strict Text)。`hGetContents` はゼロ件。🟢

## 3. MCMC chain 蓄積

`modifyIORef' samplesRef (next :)` パターンは spine strict だが、**要素 (Map.Map Text Double)** は value-lazy。10k iter × 100 param で〜10⁶ thunk。

| ファイル | 種別 | 評価 |
|---|---|---|
| `MCMC/MH.hs:78` | next = 新しい Map | 🟡 Q2-B で実測 |
| `MCMC/HMC.hs:283` | `toConstrained nextU :` — `toConstrained` は exp/logit 等 lazy | 🔴 thunk 確実 |
| `MCMC/NUTS.hs:381` | 同上 | 🔴 |
| `MCMC/Gibbs.hs:192,423` | next | 🟡 |
| `MCMC/Slice.hs:119` | next | 🟡 |
| `Stat/AD.hs:240` | 同上 | 🟡 |

## 4. VI Adam の lazy thunk leak ★最有力

`Stat/VI.hs:176, 184`:

```haskell
writeIORef muRef    (zipWith (+) mu dxMu)    -- thunk!
writeIORef omegaRef (zipWith (+) omega dxOm) -- thunk!
```

iter t で `mu = thunk_{t-1}` を読んで `thunk_t = zipWith (+) thunk_{t-1} dxMu_t` を書き戻す。
**T iter × n param の chained thunk** が evaluation 時に一気に展開 → メモリ膨張 + GC 刺さり確実。

🔴 **修正必須**。`writeIORef muRef $! force (zipWith (+) mu dxMu)` か `LA.Vector` 化。

## 5. Optim Adam (Stat/VI と類似だが LA.Vector 経由)

`Optim/Adam.hs:107` `modifyIORef' histRef (x' :)`。x' は `LA.Vector Double` なので LA layer 強制済み。spine も strict。🟢

ただし `histRef` は **全 iter 分の x を保持**(数千〜数万 iter で n*8 bytes × T)。これは設計通りだが文書化要。🟡

## 6. 中間 n×n 構築

`LA.fromRows [...]` ヒット箇所は全て:
- 列数 p × p (Survival hessian、PCA mu broadcast、Cluster KMeans++ idx) で n と独立
- 1D 構築 (Cluster, BayesOpt diffs)

🟢

## 7. `foldr` で list append 系

| 場所 | 評価 |
|---|---|
| `Optim/NSGA.hs:457,752,771,813` | accumulator が `IS.IntSet` か小リスト 🟢 |
| `Viz/ReportBuilder.hs:972` `unique = foldr (\x acc -> if x \`elem\` acc then acc else x:acc) []` | 🟡 O(n²) `elem` だが unique 対象が小 (列名等) |
| `DataIO/Reshape.hs:146` `foldr (\(name, col) d -> DX.insertColumn name col d)` | 列数 🟢 |

## 8. Viz HTML 全データ埋込

`Viz/ReportBuilder.hs` で `data: { values: obs }` パターンが多数。n=10⁴ の curve を出すと HTML が数 MB-数十 MB。Heap には届かないが **disk / browser** で問題化する可能性。🟡 Q2-B `bench-mem-report-html` で確認。

## 9. その他追加発見

- `MCMC/Slice.hs:108` `let sweep current = foldr (\_ _ -> id) id [] \`seq\` ...` — dead code っぽい seq、無害だが奇妙。🟢
- `Model/Survival.hs:130` `splitByEvent = foldr step ([], [])` — pair の append。観測数次第で 🟡

## Q2-B bench 確定リスト (priority 順)

| # | bench | 対象 | 想定 leak |
|---|---|---|---|
| 1 | `bench-mem-vi-advi` | `Stat.VI.runVI` | 🔴 #4 chained zipWith thunk |
| 2 | `bench-mem-mcmc-nuts` | `runNUTS` 10k iter | 🔴 #3 toConstrained thunk |
| 3 | `bench-mem-mcmc-hmc` | `runHMC` 10k iter | 🔴 同 |
| 4 | `bench-mem-mcmc-mh` | `runMH` 100k iter | 🟡 |
| 5 | `bench-mem-regrid` | id 数 10→1000 | 🟡 #1 Preprocess:682 |
| 6 | `bench-mem-aggregate` | group 数増 | 🟡 |
| 7 | `bench-mem-hbm-grad` | HBM Forward AD | 🟡 |
| 8 | `bench-mem-nsga2-long` | nsga2 500 gen | 🟡 |
| 9 | `bench-mem-bayesopt` | BO 100 iter | 🟡 |
| 10 | `bench-mem-report-html` | n=10⁴ curve | 🟡 |
