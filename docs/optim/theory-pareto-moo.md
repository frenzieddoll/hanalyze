# Study Material 9 — Pareto efficiency and multi-objective optimization

> 🌐 **English** | [日本語](theory-pareto-moo.ja.md)

## 1. Pareto dominance

For a minimization problem:

$$ a \succ b \iff (\forall i: a_i \le b_i) \land (\exists j: a_j < b_j) $$

"a dominates b" = a is no worse than b in every objective and strictly better in at least one.

**Pareto-optimal**: no point dominates this one.
**Pareto front**: the set of all Pareto-optimal solutions.

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

Diversity measure within a front:

$$ \text{cd}(i) = \sum_m \frac{f_m(i+1) - f_m(i-1)}{f_m^{\max} - f_m^{\min}} $$

(sort along each objective; sum the normalised gaps to neighbours; endpoints get ∞.)

### 2.3 Crowded comparison operator

$$ a \prec_n b \iff (\text{rank}(a) < \text{rank}(b)) \lor
                    (\text{rank}(a) = \text{rank}(b) \land \text{cd}(a) > \text{cd}(b)) $$

## 3. Genetic operators

### 3.1 SBX (Simulated Binary Crossover)

```
β = (2u)^(1/(η+1))      if u < 0.5
  = (1/(2(1-u)))^(1/(η+1))   else
c1 = 0.5 ((1+β)p1 + (1-β)p2)
c2 = 0.5 ((1-β)p1 + (1+β)p2)
```

Larger η → close to parents; smaller η → wider exploration.

### 3.2 Polynomial mutation

```
δq = (2u)^(1/(η+1)) - 1     if u < 0.5
   = 1 - (2(1-u))^(1/(η+1)) else
y' = y + δq (yU - yL)
```

## 4. Evaluation metrics

### 4.1 Hypervolume

Volume dominated by the front as seen from a reference point $r$. **Larger is better**
(captures both convergence and diversity).

### 4.2 IGD (Inverted Generational Distance)

$$ \text{IGD} = \frac{1}{|R|} \sum_{r \in R} \min_{e \in E} d(r, e) $$

Mean shortest distance from each true front point to the estimated front.
**Smaller is better** (also rewards diversity).

### 4.3 GD (Generational Distance)

Mean shortest distance from each estimated point to the true front. Convergence only.

## 5. Scalarization methods

Aggregate multi-objective into single-objective:

| Method | Aggregation |
|---|---|
| Weighted sum | $\sum w_i f_i$ |
| Tchebycheff | $\max_i w_i (f_i - z_i^*)$ |
| ε-constraint | $\min f_1$ s.t. $f_i \le \epsilon_i$ |
| Desirability | $D = (\prod d_j)^{1/q}$ |

## 6. Classical benchmarks

### 6.1 ZDT1

$$ f_1(x) = x_1, \quad f_2(x) = g(x)\!\left(1 - \sqrt{f_1/g}\right) $$

$$ g(x) = 1 + 9 \cdot \frac{1}{n-1}\sum_{i=2}^n x_i $$

True Pareto front: $f_2 = 1 - \sqrt{f_1}$, achieved at $g = 1$.

### 6.2 Schaffer

$$ f_1(x) = x^2, \quad f_2(x) = (x - 2)^2 $$

True front: $x \in [0, 2]$.
