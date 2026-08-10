# Bayesian D-optimal Design

> DuMouchel-Jones (1994) Bayesian modification + Compound criterion +
> multivariate Cp integration for Custom Designs.
>
> Spec: `specification/spec/hanalyze-doe-custom-design-spec.md` §2.7
> Phase: 26
> Prerequisite: Phase 24 + 25

Japanese reference: [`usage-bayesian-d.ja.md`](usage-bayesian-d.ja.md).

## Concept

$$ \max\, \det(X^T X + K) $$

`K` is a `p × p` prior precision matrix. Typical DuMouchel-Jones diagonal:

| Term | K_jj |
|---|---|
| intercept / main effects | 0 |
| 2-factor interaction     | τ² |
| quadratic / nested       | τ² |

`K = 0` reduces to classic D-opt (verified by unit test).

## Usage

```haskell
import Hanalyze.Design.Custom.Bayesian
import Hanalyze.Design.Optimal (OptCriterion (..))

let pp = priorPrecisionDefault factors model 1.0
    spec = ... { cdsCriterion = BayesianD (precisionToMatrix pp) }
```

Custom K per term:

```haskell
let myK (TPower _ 2) = 0.5
    myK (TInter _)   = 2.0
    myK _            = 0
    pp = priorPrecisionFromTerms factors model myK
```

## Compound criterion normalization

```haskell
import qualified Hanalyze.Design.Custom.Compare as CCMP

let ws  = [(0.7, DOpt), (0.5, AOpt), (-0.1, IOpt)]
    ws' = CCMP.normalizeCompoundWeights ws
-- ws' = [(0.583, DOpt), (0.417, AOpt), (0.0, IOpt)]
```

Negative weights are clamped to 0; positive weights are scaled to sum to 1.

## Limitations

- Bayesian D-efficiency reported by `Compare.dcEffTable` uses classic D
  (compare Bayesian designs by `crCriterionValue` directly)
- Compound weight normalization is linear; geometric-mean variant is future work
- Multivariate Cp integration into `Compare` requires a response matrix API extension
  (deferred to canvas integration)
