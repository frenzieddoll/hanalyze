# Study Material 10 — Bayesian Optimization

> 🌐 **English** | [日本語](theory-bayesopt.ja.md)

## 1. Motivation

Optimization of expensive-to-evaluate objectives (simulations, experiments).
Use a **surrogate model** to pick the next evaluation point intelligently.

## 2. The loop

```
1. Evaluate initial points by LHS / random
2. Fit a GP on the observation set D = {(x_i, y_i)}
3. Maximise the acquisition function a(x) to choose x_next
4. Evaluate y_next = f(x_next) and append to D
5. Repeat 2-4 until budget exhausted
```

## 3. Acquisition functions

### 3.1 Expected Improvement (EI)

$$ \text{EI}(x) = E[\max(y_{\text{best}} - y(x), 0)] $$
$$ = (y_{\text{best}} - \mu - \xi) \Phi(z) + \sigma \phi(z), \quad z = (y_{\text{best}} - \mu - \xi)/\sigma $$

### 3.2 Upper / Lower Confidence Bound

$$ \text{LCB}(x) = \mu(x) - \beta \sigma(x) $$

$\beta$ controls the explore-exploit trade-off.

### 3.3 Probability of Improvement (PI)

$$ \text{PI}(x) = \Phi\!\left(\frac{y_{\text{best}} - \mu - \xi}{\sigma}\right) $$

## 4. Multi-objective BO

### 4.1 ParEGO (Knowles 2006)

At each iteration, pick a random weight $w$ and apply Tchebycheff scalarisation:

$$ y_{\text{scalar}}(x) = \max_j w_j (y_j - z_j^*) + \rho \sum_j w_j (y_j - z_j^*) $$

then compute single-objective EI. Simple but effective.

### 4.2 EHVI (Expected Hypervolume Improvement)

$$ \text{EHVI}(x) = E[\text{HV}(P \cup \{y(x)\}) - \text{HV}(P)] $$

The **expected hypervolume increase** if the new point joins the front. Information-rich.
Closed form in 2D, MC approximation in 3D+.

### 4.3 q-EHVI (batched)

Propose $q$ points at once. Useful for parallel experiments.

## 5. NSGA-II vs Bayesian MOO

| | NSGA-II | Bayesian MOO |
|---|---|---|
| Evaluation cost | cheap | expensive |
| # of evaluations | thousands to tens of thousands | tens to hundreds |
| Multi-objective support | ◎ | ○ (via acquisition design) |
| Parallelism | natural (per generation) | via batched acquisitions |

## 6. hanalyze implementation

```haskell
-- single-objective
import Optim.BayesOpt (bayesOpt, defaultBayesOptConfig)
(history, best) <- bayesOpt cfg f (lo, hi) gen

-- multi-objective (acquisition maximisation inside NSGA-II)
import Optim.BayesOpt (bayesOptMOWithNSGA)
hist <- bayesOptMOWithNSGA nIter nInit RBF f bounds gen
```
