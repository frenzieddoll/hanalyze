# 学習資料 9 — Pareto 効率と多目的最適化

## 1. Pareto 支配

最小化問題で:

$$ a \succ b \iff (\forall i: a_i \le b_i) \land (\exists j: a_j < b_j) $$

「a は b を支配する」 = a がすべての目的で b 以下、かつ少なくとも 1 つで真に良い。

**Pareto 最適**: 自分を支配する点が存在しない解。
**Pareto front**: 全 Pareto 最適解の集合。

## 2. NSGA-II (Deb 2002)

### 2.1 Fast non-dominated sort O(MN²)

```
for each p in P:
  S_p = {q : p dominates q}
  n_p = |{q : q dominates p}|
  if n_p = 0: p ∈ F_1

for i = 1, 2, ...:
  for each p in F_i:
    for each q in S_p:
      n_q -= 1
      if n_q = 0: q ∈ F_{i+1}
```

### 2.2 Crowding distance

各 front 内で多様性を測る:

$$ \text{cd}(i) = \sum_m \frac{f_m(i+1) - f_m(i-1)}{f_m^{\max} - f_m^{\min}} $$

(各目的でソートし前後の差を正規化加算、両端は ∞)

### 2.3 Crowded comparison operator

$$ a \prec_n b \iff (\text{rank}(a) < \text{rank}(b)) \lor
                    (\text{rank}(a) = \text{rank}(b) \land \text{cd}(a) > \text{cd}(b)) $$

## 3. 遺伝的演算子

### 3.1 SBX (Simulated Binary Crossover)

```
β = (2u)^(1/(η+1))      if u < 0.5
  = (1/(2(1-u)))^(1/(η+1))   else
c1 = 0.5 ((1+β)p1 + (1-β)p2)
c2 = 0.5 ((1-β)p1 + (1+β)p2)
```

η 大 → 親付近、η 小 → 探索広い。

### 3.2 Polynomial mutation

```
δq = (2u)^(1/(η+1)) - 1     if u < 0.5
   = 1 - (2(1-u))^(1/(η+1)) else
y' = y + δq (yU - yL)
```

## 4. 評価指標

### 4.1 Hypervolume

参照点 $r$ から見た front が支配する体積。**大きい方が良い** (収束 + 多様性両方を反映)。

### 4.2 IGD (Inverted Generational Distance)

$$ \text{IGD} = \frac{1}{|R|} \sum_{r \in R} \min_{e \in E} d(r, e) $$

真 front の各点から推定 front への最短距離平均。**小さい方が良い** (多様性も評価)。

### 4.3 GD (Generational Distance)

推定 front の各点から真 front への最短距離平均。収束のみ評価。

## 5. Scalarization 系手法

多目的を 1 目的に集約:

| 手法 | 集約式 |
|---|---|
| 重み付き和 | $\sum w_i f_i$ |
| Tchebycheff | $\max_i w_i (f_i - z_i^*)$ |
| ε-constraint | $\min f_1$ s.t. $f_i \le \epsilon_i$ |
| Desirability | $D = (\prod d_j)^{1/q}$ |

## 6. 古典ベンチマーク

### 6.1 ZDT1

$$ f_1(x) = x_1, \quad f_2(x) = g(x)\!\left(1 - \sqrt{f_1/g}\right) $$

$$ g(x) = 1 + 9 \cdot \frac{1}{n-1}\sum_{i=2}^n x_i $$

真の Pareto front: $f_2 = 1 - \sqrt{f_1}$, $g = 1$ で達成。

### 6.2 Schaffer

$$ f_1(x) = x^2, \quad f_2(x) = (x - 2)^2 $$

真の front: $x \in [0, 2]$。
