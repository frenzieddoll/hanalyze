# PLS regression (Partial Least Squares)

Module: `Hanalyze.Model.PLS` (`hanalyze-models`)

A low-rank regression method for situations with many, strongly correlated
explanatory variables (spectroscopy, materials design, process parameters).
Where PCA maximizes the variance of X while ignoring the response, **PLS
maximizes the covariance between X and the response Y** when choosing latent
components. It is the standard chemometrics technique, giving both
predictions and variable importance (VIP) from a single model.

This doc covers the **low-level, matrix API**. For the dataframe-level API
used as `df |-> plsOf ...` and the score / loading / VIP plots, see
[api-guide/04-multivariate.md](../api-guide/04-multivariate.md) §PLS; for how
PLS fits among the other multi-output models, see
[05-multivariate.md](05-multivariate.md) §3.

## 0. API

```haskell
data PLSConfig = PLSConfig
  { plsN_Components :: !Int
  , plsAlgorithm    :: !PLSAlgorithm   -- NIPALS (default) | SIMPLS (future)
  , plsScale        :: !Bool           -- True standardizes X, Y column-wise
  , plsTol          :: !Double         -- NIPALS convergence tolerance
  , plsMaxIter      :: !Int            -- NIPALS max iterations
  }

defaultPLS :: PLSConfig

fitPLS  :: PLSConfig -> Matrix Double -> Matrix Double -> Either Text PLSFit
fitPLS1 :: PLSConfig -> Matrix Double -> Vector Double -> Either Text PLSFit

predictPLS  :: PLSFit -> Matrix Double -> Matrix Double
predictPLS1 :: PLSFit -> Matrix Double -> Vector Double
```

`fitPLS` handles multi-output response (Y is an `n × q` matrix), while
`fitPLS1` is a thin wrapper for a single response (`Vector`). Both return
`Left Text` on failure (e.g. the number of components exceeds the number of
variables). `predictPLS1` is the single-column counterpart matching a fit
built with `fitPLS1`.

## 1. Usage

Build a 2-component PLS model on a 4-variable dataset where x1 and x2 drive
the response and x3 / x4 are noise.

```haskell
import qualified Numeric.LinearAlgebra as LA
import Hanalyze.Model.PLS

main :: IO ()
main = do
  let x = LA.fromLists
            [ [ 1.0,  2.1, 0.5, 3.3 ]
            , [ 2.0,  4.1, 0.6, 3.1 ]
            , [ 3.0,  5.9, 0.4, 3.4 ]
            , [ 4.0,  8.2, 0.7, 3.2 ]
            , [ 5.0,  9.8, 0.5, 3.3 ]
            , [ 6.0, 12.1, 0.6, 3.1 ]
            , [ 7.0, 14.0, 0.4, 3.5 ]
            , [ 8.0, 16.2, 0.7, 3.2 ] ]
      y   = LA.fromList [ 3.0, 5.1, 7.2, 9.4, 11.1, 13.3, 15.2, 17.4 ]
      cfg = defaultPLS { plsN_Components = 2 }
  case fitPLS1 cfg x y of
    Left err -> putStrLn ("error: " ++ show err)
    Right f  -> do
      print (LA.toList (plsVIP f))
      print (LA.toList (plsR2X f), LA.toList (plsR2Y f))
      print (LA.toList (predictPLS1 f x))
```

Output:

```
[1.3941438613552546,1.394105087799994,0.3038126115482986,0.14328920084861047]
([0.5161208250491462,0.4462476355758236],[0.9959755615498295,2.6134823081460905e-3])
[3.21017141904792,5.0132738248537185,7.04305021855597,9.578399655271525,
 11.028475763850459,12.980625583912904,15.300252120176811,17.54575141433071]
```

How to read this:

- **VIP** — x1 / x2 are 1.39, x3 / x4 are 0.30 / 0.14. **VIP > 1** is the
  conventional threshold for "an important variable", so the two noise
  variables are correctly identified as unimportant.
- **`plsR2Y`** — the first component alone explains 99.6% of the variance in
  Y. The second component contributes only 0.26%, so this dataset really
  only needs one component.
- **`plsR2X`** — how much of the X-side variance each component captures
  (0.52 / 0.45). Looking at this separately from the Y explanation rate lets
  you spot "components that reproduce X well but don't help Y".

## 2. The `PLSFit` result type

| Field | Content |
|---|---|
| `plsScoresT` | T (`n × K`) — X's latent scores. The coordinates for the score plot |
| `plsLoadingsP` / `plsLoadingsQ` | P (`p × K`) / Q (`q × K`) — X / Y loadings |
| `plsWeightsW` | W (`p × K`) — X's weights |
| `plsCoef` | β (`p × q`) regression coefficients (**original scale**). `Ŷ = (X − X̄) · β + Ȳ` |
| `plsXMean` / `plsXStd` | Column means / standard deviations of X (std is 1 when `plsScale = False`) |
| `plsYMean` / `plsYStd` | The same for Y |
| `plsR2X` / `plsR2Y` | Explained variance ratio of X / Y per component (length K) |
| `plsVIP` | Variable Importance in Projection (length p) |
| `plsConfig` | The `PLSConfig` used to fit |

`plsCoef` is in the original scale, so computing
`(X − plsXMean) · plsCoef + plsYMean` yourself, without going through
`predictPLS`, gives the same values.

## 3. Choosing the number of components with CV

The number of components K is a hyperparameter. Choose it by comparing MSE
across k-fold CV:

```haskell
import qualified Data.Vector.Unboxed as VU
import qualified System.Random.MWC   as MWC

selectPLSComponentsCV
  :: Int                 -- k of the k-fold
  -> Int                 -- maxK (upper bound on components to search)
  -> Matrix Double       -- X
  -> Matrix Double       -- Y (for a single response, use LA.asColumn to make a column)
  -> MWC.GenIO
  -> IO PLSLambdaSelection
```

```haskell
  gen <- MWC.initialize (VU.fromList [42])       -- fix the seed for reproducibility
  sel <- selectPLSComponentsCV 4 3 x (LA.asColumn y) gen
  print (plsBestK sel, plsOneSeK sel)
  print (plsCVMSEs sel)
```

Output:

```
(3,3)
[1.1296833242752071,0.10686514418350236,2.501483388975259e-2]
```

| Field | Content |
|---|---|
| `plsBestK` | Number of components with the lowest CV MSE |
| `plsOneSeK` | Number of components chosen by the **1-SE rule** (smallest K within 1 standard error of the minimum MSE) |
| `plsCVMSEs` | CV MSE for K = 1 .. maxK (length maxK) |
| `plsCVSDs` | Standard deviations in the same order |

The fold split depends on randomness, so this **takes an `MWC.GenIO` and
returns `IO`**. When putting results into docs or tests, fix the seed with
`MWC.initialize` (`MWC.createSystemRandom` changes every time). Note that Y
must be passed as a matrix even for a single response, hence `LA.asColumn`.

## 4. Choosing an approach

| Goal | Use |
|---|---|
| Predict by reducing rank in the direction that drives the response | `fitPLS` / `fitPLS1` (this doc) |
| Look only at the structure of X, ignoring the response | [`Model.PCA`](../api-guide/04-multivariate.md) |
| Build and plot in one shot from a dataframe | `df \|-> plsOf cfg xcols ycols` ([api-guide/04](../api-guide/04-multivariate.md) §PLS) |
| Suppress multicollinearity with a penalty | [Ridge / Lasso / Elastic Net](04-regularized.md) |
| Multi-output linear regression (no rank reduction) | [05-multivariate.md](05-multivariate.md) §1 |

## See also

- Package: [hanalyze-models](../../hanalyze-models/README.md)
- Dataframe-level API and diagnostic plots: [api-guide/04-multivariate.md](../api-guide/04-multivariate.md) §PLS
- Where PLS fits among multi-output models: [05-multivariate.md](05-multivariate.md) §3
- Discriminant analysis (the classification-side multivariate method): [usage-discriminant.md](usage-discriminant.md)
