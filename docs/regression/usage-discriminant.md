# Discriminant analysis (LDA / QDA)

Module: `Hanalyze.Model.Discriminant` (`hanalyze-models`)

A classical method for discriminating between multiple classes from
continuous explanatory variables. It assumes a normal distribution per class
and assigns by comparing `log p(class) + log f(x | class)` (the
log-posterior).

| Method | Covariance assumption | Decision boundary |
|---|---|---|
| **LDA** | Common (pooled) across classes | Linear |
| **QDA** | Separate per class | Quadratic |

When there are many classes or few samples per class, LDA is more stable
since it only needs to estimate a single covariance. If the spread clearly
differs across classes, use QDA.

This doc covers the **low-level, matrix API**. For the dataframe-level API
used as `df |-> ldaOf cols clsCol` and the decision-boundary plot, see
[api-guide/04-multivariate.md](../api-guide/04-multivariate.md) §Discriminant
analysis (LDA). **QDA has no dataframe-level API**, so when you need a
quadratic boundary, use `fitQDA` from this doc directly.

## 0. API

```haskell
data DiscriminantMethod = LDA | QDA

fitLDA :: Matrix Double     -- X (n × p)
       -> V.Vector Int      -- y (n) integer class labels
       -> Either Text DiscriminantFit

fitQDA :: Matrix Double -> V.Vector Int -> Either Text DiscriminantFit

predictDiscriminant
  :: DiscriminantFit
  -> Matrix Double                       -- X_new (m × p)
  -> (V.Vector Int, Matrix Double)       -- (predicted labels, posterior matrix m × K)
```

Labels are `Data.Vector`'s **`Vector Int`** (not `hmatrix`'s `Vector`). X is
`hmatrix`'s `Matrix Double`.

> The second return value of `predictDiscriminant` is the **row-normalized
> posterior probability** (each row sums to 1), not the raw log-posterior
> (`Discriminant.hs:129-136` normalizes via `exp(logp − max)`).

## 1. Usage

Fit LDA on a 2-class, 2-variable dataset and predict for 3 new points.

```haskell
import qualified Data.Vector           as V
import qualified Numeric.LinearAlgebra as LA
import Hanalyze.Model.Discriminant

main :: IO ()
main = do
  let x = LA.fromLists
            [ [ 1.0, 1.2 ], [ 1.4, 0.9 ], [ 0.8, 1.1 ], [ 1.1, 1.3 ]
            , [ 4.0, 4.2 ], [ 4.4, 3.9 ], [ 3.8, 4.1 ], [ 4.1, 4.3 ] ]
      y    = V.fromList [ 0, 0, 0, 0, 1, 1, 1, 1 ]
      xNew = LA.fromLists [ [ 1.2, 1.0 ], [ 4.2, 4.0 ], [ 2.6, 2.6 ] ]
  case fitLDA x y of
    Left err -> putStrLn ("error: " ++ show err)
    Right f  -> do
      let (preds, post) = predictDiscriminant f xNew
      print (V.toList preds)
      print (LA.toLists post)
      print (LA.toList (dfPriors f), LA.toList (dfClasses f))
      print (LA.toLists (dfMeans f))
```

Output:

```
[0,1,0]
[[1.0,1.0542564890450664e-207],[1.8579928589071955e-199,1.0],
 [0.8697481923940898,0.13025180760591012]]
([0.5,0.5],[0.0,1.0])
[[1.0750000000000002,1.125],[4.074999999999999,4.125]]
```

How to read this:

- Points 1 and 2 sit close to their respective class centroids, so the
  posteriors are nearly 1 / 0. With low within-group spread, posteriors
  become extreme (the exponent's magnitude blows up).
- Point 3, `(2.6, 2.6)`, sits right next to the midpoint of the two class
  means `(2.575, 2.625)`, giving a posterior of 0.87 / 0.13. **Only points
  near the boundary get intermediate probabilities**, so the posterior
  column can be used to flag "samples that could go either way".
- `dfClasses` holds the sorted class labels (`Int` stored as `Double`), and
  `dfPriors` is the prior probability estimated from the sample proportions.
  The posterior matrix's columns follow the order of `dfClasses`.

## 2. Switching to QDA

Just call a different function; the shape of the retained covariance
changes.

```haskell
  case fitQDA x y of
    Left err -> putStrLn ("error: " ++ show err)
    Right f  -> do
      let (preds, _) = predictDiscriminant f xNew
      print (V.toList preds, length (dfCovariances f))
```

```
([0,1,0],2)
```

## 3. The `DiscriminantFit` result type

| Field | LDA | QDA |
|---|---|---|
| `dfMeans` | `K × p` mean vector per class | Same |
| `dfCovariance` | Pooled covariance (`p × p`) | **Empty** (unused) |
| `dfCovariances` | **Empty** | Per-class covariance (K of them) |
| `dfPriors` | Class prior probabilities (length K, sum to 1) | Same |
| `dfClasses` | Sorted class labels (length K) | Same |
| `dfMethod` | `LDA` | `QDA` |

Which fields are empty depends on the method, so check `dfMethod` first and
then read the corresponding fields.

For numerical stability, the log-determinant and Mahalanobis distance are
computed internally via Cholesky decomposition. When a class's sample count
falls below the number of variables `p`, its covariance becomes singular and
`Left` is returned (QDA estimates covariance per class, so it breaks down
before LDA does).

## 4. Choosing an approach

| Goal | Use |
|---|---|
| Discriminate with a linear boundary, assuming common covariance | `fitLDA` (this doc) |
| Spread differs by class → quadratic boundary | `fitQDA` (this doc) |
| One-shot from a dataframe + decision-boundary plot | `df \|-> ldaOf cols clsCol` ([api-guide/04](../api-guide/04-multivariate.md)) |
| Many, strongly correlated explanatory variables (regression side) | [PLS](usage-pls.md) |
| Nonlinear boundaries via machine learning | [Random forest](06-randomforest.md) / [decision tree](08-decisiontree.md) |
| Evaluation metrics for discrimination results | [`Stat.ClassMetrics`](../stat/03-classmetrics.md) |

## See also

- Package: [hanalyze-models](../../hanalyze-models/README.md)
- Dataframe-level API and decision-boundary plot: [api-guide/04-multivariate.md](../api-guide/04-multivariate.md) §Discriminant analysis (LDA)
- PLS regression (the regression-side multivariate method): [usage-pls.md](usage-pls.md)
- Classification metrics: [stat/03-classmetrics.md](../stat/03-classmetrics.md)
