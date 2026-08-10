# Causal Inference: Propensity Score / IPW / DR / CATE (Phase 30)

Phase 30 (2026-05-29) added **causal-effect estimation from observational
data** built on the Rubin causal model. Given a confounder matrix and a
binary treatment, the module estimates ATE / ATT / CATE with bias
corrections from the propensity score.

---

## 0. Overview

| Feature | API | Use |
|---|---|---|
| Propensity Score | `Hanalyze.Stat.Causal.PropensityScore.propensityScore` | Estimate @p(X) = P(T=1\|X)@ via logistic GLM + `trimPropensity` |
| IPW | `Hanalyze.Stat.Causal.IPW.ipw` | Hajek-normalised ATE / ATT (lower variance than Horvitz-Thompson) |
| Doubly Robust (AIPW) | `Hanalyze.Stat.Causal.DoublyRobust.doublyRobust` | Outcome model + PS; consistent if either is correct |
| CATE meta-learners | `Hanalyze.Stat.Causal.CATE.fitCATE` | Heterogeneous treatment effects (S / T / X-learner, base = LM \| RF) |

The IPW / DR / CATE entry points apply `defaultPSTrim = (0.01, 0.99)`
internally so weights stay finite. Pass a hand-tuned PS through
`ipwWith` / `doublyRobustWith` when needed.

Effect estimation assumes the causal DAG is given. When the structure
itself is unknown, causal-discovery methods such as LiNGAM recover a
directed graph from observational data — the figure below shows a DAG
estimated by LiNGAM (`x0 → x1 → x2`), which can then anchor the
confounder set used by the estimators above.

![DAG estimated by LiNGAM (x0 → x1 → x2)](../images/lingam-dag.svg)

---

## 1. Propensity Score

```haskell
import qualified Numeric.LinearAlgebra as LA
import qualified Hanalyze.Stat.Causal.PropensityScore as PS

let x = LA.fromColumns
          [ LA.fromList (replicate n 1)  -- intercept
          , LA.fromList x1s
          , LA.fromList x2s ]
    t = LA.fromList ts
    ps  = PS.propensityScore x t
    ps' = PS.trimPropensity 0.01 0.99 ps
    w   = PS.ipwWeights ps' t  -- ATE weights
```

---

## 2. IPW (Hajek-normalised)

```haskell
import qualified Hanalyze.Stat.Causal.IPW as IPW

let r = IPW.ipw x t y
print (IPW.ipwATE r, IPW.ipwATT r)
```

```
ATE_Hajek = Σ(T·Y/p) / Σ(T/p)  -  Σ((1-T)·Y/(1-p)) / Σ((1-T)/(1-p))
ATT_Hajek = Σ(T·Y) / Σ T       -  Σ((1-T)·p/(1-p)·Y) / Σ((1-T)·p/(1-p))
```

---

## 3. Doubly Robust (AIPW)

```haskell
import qualified Hanalyze.Stat.Causal.DoublyRobust as DR

let r = DR.doublyRobust x t y
print (DR.drATE r)
```

Per-group OLS for `μ̂_1(X)` / `μ̂_0(X)`, PS-weighted residual correction.

```
ATE_AIPW = (1/n) Σ [ μ̂_1(X_i) - μ̂_0(X_i)
                    + T_i (Y_i - μ̂_1(X_i)) / p̂_i
                    - (1-T_i) (Y_i - μ̂_0(X_i)) / (1 - p̂_i) ]
```

Double robustness: consistent if **either** the outcome model **or** the
PS is correctly specified.

---

## 4. CATE meta-learners

```haskell
import qualified Hanalyze.Stat.Causal.CATE as CATE
import qualified Hanalyze.Model.RandomForest as RF
import qualified System.Random.MWC as MWC

gen <- MWC.create
r <- CATE.fitCATE CATE.TLearner CATE.CATELM x t y gen

let rfCfg = RF.defaultRFConfig { RF.rfTrees = 100 }
r' <- CATE.fitCATE CATE.XLearner (CATE.CATERF rfCfg) x t y gen
```

| Method | Algorithm | Strength | Weakness |
|---|---|---|---|
| **S-learner** | Single model on (X, T) | Sample-efficient | T effect can wash out; LM without X·T interaction → constant CATE |
| **T-learner** | Per-group fit μ_1, μ_0 | Recovers heterogeneity directly | Variance high when group sizes are unbalanced |
| **X-learner** | T-learner residuals re-regressed + PS-weighted average | Robust to unbalanced groups | 4 sub-models needed |

Künzel, Sekhon, Bickel, Yu (2019) PNAS 116:4156-4165.

---

## 5. Caveats

- **Positivity**: if `p_i` saturates at 0 / 1 the IPW weights blow up.
  Always trim. If even trim leaves huge variance, switch to ATT-only or
  restrict to the overlap region.
- **No unmeasured confounders**: the backend does not verify the DAG
  assumption. Missing a confounder biases the estimate.
- **S-learner with LM**: without explicit X·T interaction columns the
  CATE collapses to a constant. Use T / X-learner or add interactions.

---

## 6. References

- Rosenbaum & Rubin (1983) Biometrika 70:41-55. (Propensity Score)
- Horvitz & Thompson (1952) JASA 47:663-685. (IPW)
- Robins, Rotnitzky, Zhao (1994) JASA 89:846-866. (AIPW)
- Künzel et al. (2019) PNAS 116:4156-4165. (Meta-learners)
- Comparable libraries: R `MatchIt` / `WeightIt` / `tmle`, Python
  `econml`, `DoWhy`.
