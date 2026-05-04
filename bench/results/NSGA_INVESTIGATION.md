# NSGA-II 100-gen 収束ギャップの原因調査

(2026-05-05)

## 課題

100 generations × 100 popSize で:

| 問題 | hanalyze HV | pymoo HV | 達成率 |
|---|---|---|---|
| ZDT1 | 0 | 0.84 | 0% |
| ZDT2 | 0 | 0.48 | 0% |
| ZDT3 | 0.24 | 1.29 | 19% |
| DTLZ2_3 | 2.69 | 2.72 | 99% |

500 gen では追いつくのに、100 gen で大きく差がつく。pymoo source と
Deb 1995 / 2002 論文を照合して原因を特定。

## 調査対象 (pymoo 0.6.1.6 source)

* `pymoo/algorithms/moo/nsga2.py` (NSGA2 main, binary_tournament)
* `pymoo/operators/crossover/sbx.py` (SBX implementation)
* `pymoo/operators/mutation/pm.py` (PM)
* `pymoo/operators/selection/tournament.py` (TournamentSelection)
* `pymoo/operators/survival/rank_and_crowding/classes.py` (RankAndCrowding)
* `pymoo/util/nds/non_dominated_sorting.py` (NDS)
* `pymoo/core/duplicate.py` (DefaultDuplicateElimination)
* `pymoo/core/infill.py` (Mating loop with duplicate retry)

## ハンアラに発覚した実装差分

凡例 — ✓ : pymoo と同等、 ✗ : 差分あり

| # | 機能 | hanalyze 現状 | pymoo | 推定影響 |
|---|---|---|---|---|
| **1** | **SBX crossover 数式** | **簡易版 Deb 1995** (boundary 補正なし) | **完全版 Deb 1995 Algorithm 1** (boundary-aware β_q) | ★★★ 高 |
| 2 | tournament 比較関数 | rank + crowding | **dom + crowding** (default `comp_by_dom_and_crowding`) | ★ 低-中 |
| 3 | tournament index 選択 | iid uniform | **random permutation** (各個体が等回数参加) | ★★ 中 |
| **4** | **重複除去** | **なし** | **DefaultDuplicateElimination** (epsilon=1e-16、最大 100 retry) | ★★★ 高 |
| 5 | SBX `prob_bin` (子の binomial 交換) | なし | **per-dim 50% で c1↔c2 入れ替え** | ★★ 中 |
| 6 | Polynomial mutation 数式 | boundary-aware ✓ (N1 fix) | boundary-aware ✓ | ✓ |
| 7 | per-dim 交叉確率 | 50% gating ✓ | `prob_var = 0.5` ✓ | ✓ |
| 8 | 親レベル交叉確率 | 0.9 ✓ | `prob = 0.9` ✓ | ✓ |
| 9 | n_offsprings | popSize ✓ | popSize ✓ | ✓ |
| 10 | fast non-dominated sort | Deb 2002 標準 ✓ | Cython 同アルゴリズム ✓ | ✓ |

## 最大の犯人 — #1: SBX の boundary correction 欠落

### hanalyze 現実装 (簡易版)

```haskell
-- src/Optim/NSGA.hs
sbxOneVar etaC gen (lo, hi) (a, b) = do
  flip_ <- uniform gen
  if flip_ >= 0.5 || abs (a - b) < 1e-12
    then return (a, b)
    else do
      u <- uniform gen
      let beta
            | u < 0.5    = (2 * u) ** (1 / (etaC + 1))
            | otherwise  = (1 / (2 * (1 - u))) ** (1 / (etaC + 1))
          c1 = 0.5 * ((1 + beta) * a + (1 - beta) * b)
          c2 = 0.5 * ((1 - beta) * a + (1 + beta) * b)
          clip x = min hi (max lo x)
      return (clip c1, clip c2)
```

`beta` は **境界 `lo` / `hi` を全く参照しない**。範囲外に出た場合は
`clip` で押し戻すのみ。

### pymoo 実装 (boundary-aware)

```python
# pymoo/operators/crossover/sbx.py
def cross_sbx(X, xl, xu, eta, prob_var, prob_bin, ...):
    p1, p2 = X[0][cross], X[1][cross]
    # smaller / larger を識別
    sm = p1 < p2
    y1 = np.where(sm, p1, p2)
    y2 = np.where(sm, p2, p1)

    def calc_betaq(beta):
        alpha = 2.0 - np.power(beta, -(eta + 1.0))
        mask = (rand <= (1.0 / alpha))
        betaq[mask] = np.power((rand * alpha), (1.0 / (eta + 1.0)))[mask]
        betaq[~mask] = np.power((1.0 / (2.0 - rand * alpha)),
                                 (1.0 / (eta + 1.0)))[~mask]
        return betaq

    delta = (y2 - y1)
    beta = 1.0 + (2.0 * (y1 - _xl) / delta)        # ← 下限までの距離で β
    betaq = calc_betaq(beta)
    c1 = 0.5 * ((y1 + y2) - betaq * delta)

    beta = 1.0 + (2.0 * (_xu - y2) / delta)        # ← 上限までの距離で β
    betaq = calc_betaq(beta)
    c2 = 0.5 * ((y1 + y2) + betaq * delta)
```

**重要点**:

* `beta = 1 + 2*(y1 - xl) / delta` — 子 c1 用は **下限までの距離** で β
* `beta = 1 + 2*(xu - y2) / delta` — 子 c2 用は **上限までの距離** で β
* α = 2 - β^(-(η+1)) で随伴値を作り、`rand` の閾値を α 依存に
* これは Deb (1995) 論文 Algorithm 1 の正式な数式

### なぜこれが ZDT で致命的か

ZDT 問題 (1, 2, 3) の真の Pareto front は **`x_2..x_30 = 0`** で達成される
(g(x) = 1 + 9 Σ x_i / 29 が 1 になる条件)。すなわち **下限 (xl=0) に
集中させる必要がある**。

簡易 SBX (hanalyze):
- 親 1: x_i = 0.001 (= ほぼ最適)
- 親 2: x_i = 0.5  (= 中央)
- β は親の値に依存せず、u だけで決まる
- 子 1 = 0.5*((1+β)*0.001 + (1-β)*0.5) ≈ 0.25 (中央寄り)
- 親 1 の良い性質を**完全に失う**

boundary-aware SBX (pymoo):
- 親 1 の x_i = 0.001 は xl=0 に非常に近い → β = 1 + 2*0.001/0.499 ≈ 1.004
- α = 2 - 1.004^(-16) ≈ 0.06 (非常に小さい)
- ⇒ rand <= 1/α (= 16.7) は常に真 → β_q = (rand * α)^(1/16) はほぼ 0
- 子 1 = 0.5*((y1+y2) - β_q*delta) ≈ 0.5*(y1+y2) で y1≈0 に近い → **0.001 を保つ**

これが ZDT で 5× の収束差になっている主因と推定。

## 第 2 候補 — #4: 重複除去なし

pymoo:
- `DefaultDuplicateElimination(epsilon=1e-16)` で生成済み offspring と
  current pop に対して **L2 距離 ≤ epsilon の個体を除去**
- 必要数 (= popSize) 揃わなければ最大 100 回 mating を retry

hanalyze:
- 重複除去なし。SBX で `abs(a - b) < 1e-12` なら親をそのままコピーする
  short-circuit はあるが、これは「親同士が同一」の場合のみ。
- 結果: 同一 offspring が複数生成 → 有効 popSize が縮む

ZDT のような high-dim 問題では同一 offspring の発生は稀だが、ZDT2/3 で
「Pareto 6 点しか残らない」現象は重複除去なしの影響と推定。

## 第 3 候補 — #3: tournament の random permutation

pymoo:
```python
n_random = n_select * n_parents * pressure
n_perms = math.ceil(n_random / len(pop))
P = random_permutations(n_perms, len(pop))[:n_random]
```
全個体が **正確に同じ回数** tournament に参加する。

hanalyze:
```haskell
i <- uniformR (0, n - 1) gen
j <- uniformR (0, n - 1) gen
```
iid uniform で同じ個体が複数回 tournament に出ることも、全く出ない
こともある。ポアソン分布で variance が高い → selection pressure の
ばらつきが大きい。

## 第 4 候補 — #5: prob_bin (binomial 子交換)

pymoo の SBX:
```python
b = bitwise_xor(rand < prob_bin, X[0, cross] > X[1, cross])
child1, child2 = np.where(b, (child2, child1), (child1, child2))
```

各次元独立に確率 0.5 で c1 と c2 を swap。これは追加の diversity 生成
機構。

hanalyze は per-dim swap なし。

## 第 5 候補 — #2: tournament_type (dom vs rank)

pymoo は `comp_by_dom_and_crowding` がデフォルト:
- 直接的な Pareto dominance を比較 (a ≻ b なら a を選択)
- 非比較なら crowding distance で

hanalyze は rank-based (= front 番号で比較):
- F_2 vs F_3 なら F_2 を常に選択 (= **強い convergence pressure**)

実は hanalyze の方が **convergence pressure は強い**。これは
ZDT 系でハの字に効くハズだが、SBX bug のせいで生かせていない。

ただ rank-based は **dominance を transitively 推論**しているので、
「F_2 の任意の個体が F_3 の任意の個体より良い」という前提に立つが、
実は 2 個体間の直接 dominance では tie になることも多い。dom-based
ならそこで crowding に落ちて diversity が保たれる。

## 推定影響度ランキング

| 修正 | 期待される ZDT1 HV @ 100 gen | 工数 |
|---|---|---|
| #1 SBX boundary 補正 | 0 → ~0.5 (主因解消) | 1 時間 |
| +#4 重複除去 | ~0.6 | 1-2 時間 |
| +#3 random permutation tournament | ~0.7 | 30 分 |
| +#5 prob_bin | ~0.75 | 30 分 |
| +#2 dom-based tournament (option 化) | ~0.8 | 30 分 |

合計 3-5 時間で pymoo の HV 0.84 に到達可能と試算。

## 修正方針 (実装は別 phase)

### Phase NF1 (最重要、単独で大半の効果): SBX boundary 補正

`Optim.NSGA.sbxOneVar` を Deb 1995 Algorithm 1 (boundary-aware) に
書き換える。引数の `(lo, hi)` は既に渡っているので追加は不要。

数式 (Pure Haskell、no Mutable):

```haskell
sbxOneVar etaC gen (lo, hi) (a, b) = do
  -- per-dim crossover 50%
  flip_ <- uniform gen :: IO Double
  if flip_ >= 0.5 || abs (a - b) < 1e-14 || hi <= lo
    then return (a, b)
    else do
      u <- uniform gen :: IO Double
      let (y1, y2) = if a < b then (a, b) else (b, a)
          delta   = y2 - y1
          calcBetaQ beta =
            let alpha = 2 - beta ** (-(etaC + 1))
                inv   = 1 / alpha
            in if u <= inv
                 then (u * alpha) ** (1 / (etaC + 1))
                 else (1 / (2 - u * alpha)) ** (1 / (etaC + 1))
          beta1  = 1 + 2 * (y1 - lo) / delta
          beta2  = 1 + 2 * (hi - y2) / delta
          bq1    = calcBetaQ beta1
          bq2    = calcBetaQ beta2
          c1     = 0.5 * ((y1 + y2) - bq1 * delta)
          c2     = 0.5 * ((y1 + y2) + bq2 * delta)
          clip x = min hi (max lo x)
      return (clip c1, clip c2)
```

### Phase NF2: 重複除去

`evaluateSolution` の前に L_∞ 距離で 1e-12 以下の重複を除去 →
不足分は SBX をリトライ。最大 5-10 回程度のリトライで十分。

### Phase NF3: tournament を random permutation 化

n×2 配列を fisher-yates で permute、ペアで tournament に。

### Phase NF4: prob_bin (per-dim child swap)

SBX 後に per-dim 50% で c1, c2 を swap。

### Phase NF5: dom-based tournament option

`NSGAConfig` に `nsgaTournamentType = ByRank | ByDomination` を追加、
default を `ByDomination` (= pymoo 互換) に。

## 確認事項

1. **NF1 のみ先行実装** で 100 gen での ZDT1 HV を測定 → 効果検証
2. ベンチで以前の 500 gen 結果も維持できるか確認 (boundary-aware SBX
   が exploration を抑制する側面もあるため、長 budget では悪化リスク)
3. **mutable Vector 使用なし** のまま実現可能 (per-individual の SBX
   計算で純粋関数で十分)

mutable は使いません。アルゴリズム的に必要な箇所もありません。

実装着手の許可をお願いします。
