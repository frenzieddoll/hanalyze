# Generalized Additive Model (GAM)

> 🌐 **English** | [日本語](06-gam.ja.md)

> Additive model where each explanatory variable is expressed by an individual smooth function.
> Balance of interpretability + nonlinearity. `Hanalyze.Model.GAM` module.
>
> Related: [06-quantile.md](06-quantile.md) / [04-spline.md](04-spline.md)


## Why Use It?

LM (linear regression) assumes linearity. Spline handles nonlinearity in one variable.
GAM extends this **additively to multiple variables**:

$$ y = \beta_0 + \sum_{j=1}^{p} s_j(x_j) + \varepsilon $$

Each $s_j$ is a **smooth function** of variable $x_j$.

Applications:
- **Epidemiology**: Age + BMI + smoking years each contribute nonlinearly to mortality rate
- **Environment**: Temperature + humidity + wind speed each contribute nonlinearly to air pollution index
- **Marketing**: Price + ad spend + seasonality contribute nonlinearly to sales

GAM advantages:
- **Interpretability**: plotting each $s_j$ reveals factor effects at a glance
- **Flexibility**: no assumed function form (auto-estimated via B-spline)
- **Additivity**: cannot express high-dimensional interactions, but reduces overfitting

## Algorithm

For each feature $x_j$, construct B-spline basis $B_j(x_j)$ (degree d, K knots).
Unified design matrix:

$$ X = [\mathbf{1} \mid B_1 \mid B_2 \mid \ldots \mid B_p] $$

Ridge-regularized least squares (intercept column exempted from penalty):

$$ \beta = (X^T X + \lambda S)^{-1} X^T y, \quad S = \text{diag}(0, 1, 1, \ldots) $$

Each $B_j$ is **column-centered** (subtract column mean) → ensures identifiability.
$\beta_0$ represents the mean of y, and $s_j$ only the variation component.

Prediction:

$$ \hat{y}(x) = \beta_0 + \sum_j s_j(x_j) $$

Each $s_j(x)$ can be extracted separately (`predictGAMComponent`) → visualization of **partial effects**.

## Library API

```haskell
import Hanalyze.Model.GAM

data GAMFit = GAMFit
  { gamDegree    :: Int
  , gamKnots     :: [[Double]]            -- Knots per feature
  , gamBetas     :: [Vector Double]        -- Spline coefficients per feature
  , gamColMeans  :: [Vector Double]        -- Column means (for centering)
  , gamIntercept :: Double
  , gamYHat      :: Vector Double
  , gamResid     :: Vector Double
  , gamR2        :: Double
  , gamLambda    :: Double
  }

fitGAM :: Int                  -- B-spline degree (3 recommended)
       -> Int                  -- Num interior knots (start around 5)
       -> Double               -- Ridge λ (around 0.01)
       -> [V.Vector Double]    -- Explanatory variables
       -> V.Vector Double      -- y
       -> GAMFit

predictGAM :: GAMFit -> [V.Vector Double] -> V.Vector Double

predictGAMComponent :: GAMFit -> Int -> V.Vector Double -> V.Vector Double
-- ^ Partial effect s_j(x_j) for j-th feature only
```

## Usage Example

```haskell
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Vector as V
import Hanalyze.Model.GAM

main :: IO ()
main = do
  let n = 100
      xs1 = V.fromList [ fromIntegral i / 10 | i <- [0..n-1] ]
      xs2 = V.fromList [ sin (fromIntegral i / 5) | i <- [0..n-1] ]
      ys  = V.fromList [ x1 * x1 + sin (3 * x2)   -- nonlinear + nonlinear
                       | (x1, x2) <- zip (V.toList xs1) (V.toList xs2) ]
      fit = fitGAM 3 8 0.01 [xs1, xs2] ys
  putStrLn $ "Intercept: " ++ show (gamIntercept fit)
  putStrLn $ "R²:        " ++ show (gamR2 fit)

  -- Extract partial effect for each feature
  let s1 = predictGAMComponent fit 0 xs1   -- s_1(x_1)
      s2 = predictGAMComponent fit 1 xs2   -- s_2(x_2)
  -- Plot s1 / s2 to see nonlinear effect of each factor
  putStrLn $ "s_1 range: " ++ show (V.minimum s1, V.maximum s1)
```

Drawing the $s_j$ extracted via `predictGAMComponent` reveals the nonlinear effect
(additive smooth curve) of that variable. This is the core interpretability output of GAM.

![GAM additive smooth curve (scatter + smooth curve)](../images/gam-smooth.svg)

## CLI

```bash
hanalyze gam data.csv "x1 x2 x3" y \
    --knots 8 \
    --lambda 0.05 \
    --report
```

Report includes **partial effects for each feature** drawn in separate sections
(partial residuals + smooth curve).

## Visualization via Reportable

Currently `Reportable GAMFit` is not provided. Build custom reports referencing CLI handler:

```haskell
import qualified Hanalyze.Viz.ReportBuilder as RB

let baseSec = [ RB.secDataOverview df xCols yCol
              , RB.secModelOverview "GAM" formula Nothing
              , RB.secKeyValue "Fit summary"
                  [ ("Degree",   T.pack (show (gamDegree fit)))
                  , ("Knots",    T.pack (show ...))
                  , ("Intercept",T.pack (printf "%.4f" (gamIntercept fit)))
                  , ("R²",       T.pack (printf "%.4f" (gamR2 fit)))
                  ]
              ]
    -- Partial effects
    partialSec j c xVec =
      RB.secVega ("Partial effect: s(" <> c <> ")") (mySpec j c xVec)
    -- ↑ mySpec created by referring to gamPartialSpec in app/Main.hs
```

## Caveats

- **Overfitting risk**: increasing knot count K makes overfitting more likely. Regularize with λ,
  or keep K around 5-10.
- **Additivity assumption**: interactions like $s_j(x_j) \cdot s_k(x_k)$ cannot be expressed.
  If needed, construct 2D tensor product basis separately in `Hanalyze.Model.Spline`, or switch to
  Random Forest / Gradient Boosting.
- **Extrapolation is dangerous**: outside training range, each $s_j$ may oscillate unnaturally.

---


---

## Related Links

- Linear regression: [01-lm.md](01-lm.md)
- Regularization: [04-regularized.md](04-regularized.md)
- Theoretical background: [theory-regression-extensions.md](theory-regression-extensions.md)
