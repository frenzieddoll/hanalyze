# Quantile / GAM / Random Forest

> 🌐 **English** | [日本語](06-quantile-gam-rf.ja.md)

> Related: [04-spline-kernel-regularized.md](04-spline-kernel-regularized.md),
> [01-lm.md](01-lm.md)

Three regression methods for problems where OLS / GLM struggle (outliers,
asymmetric distributions, complex nonlinear structure, feature interactions).

## Contents

1. [Quantile regression](#1-quantile-regression)
2. [Generalized Additive Model (GAM)](#2-generalized-additive-model-gam)
3. [Random Forest (regression)](#3-random-forest-regression)
4. [Method comparison and selection guide](#4-method-comparison-and-selection-guide)

---

## 1. Quantile regression

### 1.1 What it is for

OLS fits the **mean**; quantile regression fits the τ-quantile (τ ∈ (0, 1)):

| τ | Estimand |
|---|---|
| 0.5 | **median** (outlier-robust) |
| 0.1 / 0.9 | lower / upper prediction bands |
| 0.05 / 0.95 | wider prediction bands |

Use cases:
- **Income**: model the median and the upper 10% separately.
- **Medicine**: 5/50/95-percentile growth curves for children.
- **Energy**: peak-demand modelling (95th percentile).
- **Finance**: VaR (Value at Risk = 1% loss quantile).

### 1.2 Loss — pinball loss

OLS uses squared error Σ r²; quantile regression uses an **asymmetric absolute error**:

$$ \rho_\tau(u) = u\,(\tau - \mathbb{1}[u < 0]) = \begin{cases} \tau u   & u \ge 0 \\ (\tau - 1) u & u < 0 \end{cases} $$

Called the pinball / check loss. At τ=0.5 it reduces to the standard absolute error |u|/2 → median estimation.
At τ=0.9 positive residuals get weight 0.9, negative residuals 0.1 → estimates the upper quantile.

### 1.3 Algorithm — MM-IRLS (Hunter–Lange)

|u| is non-differentiable, so it is approximated quadratically (Majorization–Minimization):

$$ |u| \le \frac{u^2 + u_k^2}{2|u_k|}  \quad \text{(equality at } u = u_k \text{)} $$

This reduces the pinball loss to a sequence of **weighted least squares**:

1. β₀ = OLS solution.
2. Iterate k:
   - r = y - X β_k
   - w_i = 1 / (2 max(|r_i|, ε))
   - y'_i = y_i + (τ - 0.5) / w_i
   - β_{k+1} = (Xᵀ W X)⁻¹ Xᵀ W y'
3. Stop at ||β_{k+1} - β_k|| < tol.

Implemented in `Model.Quantile.fitQuantile` (max 100 iter, tol 1e-7).

### 1.4 Metric — Pseudo R¹_τ (Koenker–Machado 1999)

The standard R² is mean-based and inappropriate for quantile regression. Instead:

$$ R^1_\tau = 1 - \frac{V_\tau(\text{model})}{V_\tau(\text{intercept-only})} $$

where $V_\tau(m) = \Sigma \rho_\tau(r_i)$. Range (-∞, 1]; 0 = same as intercept-only, 1 = perfect fit.

### 1.5 Library API

```haskell
import Model.Quantile

data QRFit = QRFit
  { qfTau     :: Double
  , qfBeta    :: Vector Double
  , qfYHat    :: Vector Double
  , qfResid   :: Vector Double
  , qfPinball :: Double         -- Σ ρ_τ(r_i)
  , qfR1      :: Double         -- pseudo R¹_τ
  , qfIters   :: Int
  }

fitQuantile :: Double          -- τ ∈ (0, 1)
            -> Matrix Double   -- X (with intercept column)
            -> Vector Double   -- y
            -> QRFit

predictQuantile :: QRFit -> Matrix Double -> Vector Double

pinballLoss :: Double -> [Double] -> Double      -- helper for separate computation
pseudoR1    :: Double -> Double -> Double        -- modelV, baseV → R¹_τ
```

### 1.6 Example

```haskell
{-# LANGUAGE OverloadedStrings #-}
import qualified Numeric.LinearAlgebra as LA
import qualified Data.Vector as V
import Model.Quantile

main :: IO ()
main = do
  let xs = V.fromList [1, 2, 3, 4, 5, 6, 7, 8, 9, 10 :: Double]
      ys = V.fromList [2.1, 3.5, 4.8, 6.2, 7.9, 9.1, 10.5, 11.8, 13.2, 14.5]
      n  = V.length xs
      xMat = LA.fromColumns
               [ LA.konst 1 n
               , LA.fromList (V.toList xs) ]
      yLA  = LA.fromList (V.toList ys)
      fitMed  = fitQuantile 0.5  xMat yLA   -- median
      fitLow  = fitQuantile 0.1  xMat yLA   -- lower 10 %
      fitHigh = fitQuantile 0.9  xMat yLA   -- upper 90 %
  putStrLn $ "Median: "    ++ show (LA.toList (qfBeta fitMed))
  putStrLn $ "10% bound: " ++ show (LA.toList (qfBeta fitLow))
  putStrLn $ "90% bound: " ++ show (LA.toList (qfBeta fitHigh))
  putStrLn $ "Pseudo R¹ (median): " ++ show (qfR1 fitMed)
```

### 1.7 CLI

```bash
# Median regression
hanalyze quantile data.csv x y --tau 0.5 --report

# Multiple quantile fits overlaid in one chart (10 / 50 / 90 %)
hanalyze quantile data.csv x y --taus 0.1,0.5,0.9 --report
```

With `--taus` the report adds a **Multiple quantile fits** section (observed scatter + quantile lines, tableau10-coloured).

### 1.8 Visualization via Reportable

A `Reportable QRFit` instance is not yet provided (the CLI handler builds sections directly).
To produce an equivalent report from your own code:

```haskell
import qualified Viz.ReportBuilder as RB

let cfg = RB.defaultReportConfig "Quantile demo"
    sections =
      [ RB.secDataOverview df ["x"] "y"
      , RB.secModelOverview "Quantile (τ=0.5)" "Q_τ(y|x) = β₀ + β₁ x" Nothing
      , RB.secCoefficients
          [("intercept", LA.toList (qfBeta fitMed) !! 0)
          ,("β₁",        LA.toList (qfBeta fitMed) !! 1)]
          (Just ("Pseudo R¹_τ", qfR1 fitMed))
      , RB.secKeyValue "Fit summary"
          [ ("τ",            "0.500")
          , ("Pinball loss", T.pack (printf "%.4f" (qfPinball fitMed)))
          , ("Iterations",   T.pack (show (qfIters fitMed)))
          ]
      , RB.secFitScatter "x" "y" xs ys
          (Just (RB.SmoothCurve grid yhat [] []))
      , RB.secResiduals
          (LA.toList (qfYHat fitMed))
          (LA.toList (qfResid fitMed))
      ]
RB.renderReport "out.html" cfg sections
```

### 1.9 Caveats

- **MM-IRLS is slow**: up to 100 iterations. For large N the WLS inversion (O(p³) per iteration) dominates.
- **Unstable when τ is close to 0 / 1**: at τ=0.01 / 0.99 the ε-smoothing effect is large and estimates wobble.
- **Going non-linear**: the API above is plain linear quantile regression. For non-linear quantiles, build spline bases and pass them in.

---

## 2. Generalized Additive Model (GAM)

### 2.1 What it is for

LM assumes linearity. Splines handle 1D non-linearity. GAM extends this to **multiple
variables in an additive fashion**:

$$ y = \beta_0 + \sum_{j=1}^{p} s_j(x_j) + \varepsilon $$

Each $s_j$ is a **smooth function** of variable $x_j$.

Use cases:
- **Epidemiology**: age + BMI + smoking-years each contribute non-linearly to mortality.
- **Environment**: temperature + humidity + wind speed each contribute non-linearly to air quality index.
- **Marketing**: price + ad spend + season each contribute non-linearly to sales.

GAM strengths:
- **Interpretability**: plotting each $s_j$ shows the per-factor effect at a glance.
- **Flexibility**: no functional-form assumption (B-spline auto-fits).
- **Additivity**: can't represent multidimensional interactions, but consequently overfits less.

### 2.2 Algorithm

For each feature $x_j$ build a B-spline basis $B_j(x_j)$ (degree d, K knots).
Stacked design matrix:

$$ X = [\mathbf{1} \mid B_1 \mid B_2 \mid \ldots \mid B_p] $$

Ridge-regularised least squares (intercept column exempt from penalty):

$$ \beta = (X^T X + \lambda S)^{-1} X^T y, \quad S = \text{diag}(0, 1, 1, \ldots) $$

Each $B_j$ is **column-mean centred** (subtract per-column mean) for identifiability.
$\beta_0$ represents the y-mean; $s_j$ carries only the deviation.

Prediction:

$$ \hat{y}(x) = \beta_0 + \sum_j s_j(x_j) $$

Each $s_j(x)$ can be extracted alone (`predictGAMComponent`) → **partial effect** plotting.

### 2.3 Library API

```haskell
import Model.GAM

data GAMFit = GAMFit
  { gamDegree    :: Int
  , gamKnots     :: [[Double]]            -- knots per feature
  , gamBetas     :: [Vector Double]        -- spline coefficients per feature
  , gamColMeans  :: [Vector Double]        -- column means (for centring)
  , gamIntercept :: Double
  , gamYHat      :: Vector Double
  , gamResid     :: Vector Double
  , gamR2        :: Double
  , gamLambda    :: Double
  }

fitGAM :: Int                  -- B-spline degree (recommend 3)
       -> Int                  -- number of internal knots (5 or so to start)
       -> Double               -- Ridge λ (≈ 0.01)
       -> [V.Vector Double]    -- predictors
       -> V.Vector Double      -- y
       -> GAMFit

predictGAM :: GAMFit -> [V.Vector Double] -> V.Vector Double

predictGAMComponent :: GAMFit -> Int -> V.Vector Double -> V.Vector Double
-- ^ partial effect s_j(x_j) of the j-th feature only
```

### 2.4 Example

```haskell
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Vector as V
import Model.GAM

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

  -- Extract per-feature partial effects
  let s1 = predictGAMComponent fit 0 xs1   -- s_1(x_1)
      s2 = predictGAMComponent fit 1 xs2   -- s_2(x_2)
  -- Plot s1 / s2 to see each feature's nonlinear effect
  putStrLn $ "s_1 range: " ++ show (V.minimum s1, V.maximum s1)
```

### 2.5 CLI

```bash
hanalyze gam data.csv "x1 x2 x3" y \
    --knots 8 \
    --lambda 0.05 \
    --report
```

The report contains a **partial effect** section per feature (partial residuals + smooth curve).

### 2.6 Visualization via Reportable

`Reportable GAMFit` is not yet provided. Build sections by hand following the CLI handler:

```haskell
import qualified Viz.ReportBuilder as RB

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
    -- ↑ pattern your mySpec after gamPartialSpec in app/Main.hs
```

### 2.7 Caveats

- **Overfitting risk**: large K overfits. Tune via λ or keep K in 5–10.
- **Additivity assumption**: cannot represent interactions like $s_j(x_j) \cdot s_k(x_k)$.
  If needed, build a 2D tensor-product basis with `Model.Spline`, or switch to Random Forest / Gradient Boosting.
- **Extrapolation is dangerous**: outside the training range each $s_j$ may oscillate unnaturally.

---

## 3. Random Forest (regression)

### 3.1 What it is for

Decision trees (CART) are powerful non-linear / interaction models but overfit easily.
**Random Forest** averages many trees to suppress overfitting:

- Multidimensional **interactions** handled naturally.
- No feature scaling required.
- Robust to missing values / outliers (split-based).
- **Feature importance** as a by-product.

Use cases:
- **Marketing**: 50+ features (demographics, history, geography) → churn prediction.
- **Manufacturing**: high-dimensional, correlated sensor data → anomaly detection.
- **Medicine**: biomarker → disease prediction.

### 3.2 Algorithm — CART + Bagging + Random Subspace

#### CART

At each internal node:
1. Pick a feature.
2. Pick a threshold.
3. Split data into left (≤ threshold) / right (> threshold).
4. Greedily choose the split with maximum **variance reduction**.
5. Stop (become a leaf) when sample count is small or max depth is reached.
6. Leaf prediction = mean of y in that node.

#### Bagging (Bootstrap Aggregating)

n trees each built on a different **bootstrap sample** (with replacement). Predictions are
the mean across trees. Variance reduces toward 1/n → suppresses overfitting.

#### Random Subspace

Each split picks **mtry features at random** (default d/3). This decorrelates trees and
amplifies bagging's effect.

#### Feature importance

`Model.RandomForest` provides a simple version: **count of splits per feature**.
More principled metrics:
- **Mean Decrease in Impurity (MDI)**: aggregate variance reductions at splits.
- **Permutation Importance**: shuffle a column, measure MSE increase.

(Currently only the simple split-count is implemented; MDI / Permutation are future work.)

### 3.3 Library API

```haskell
import Model.RandomForest

data RFConfig = RFConfig
  { rfTrees      :: Int       -- number of trees (default 100)
  , rfMaxDepth   :: Int       -- maximum depth (default 12)
  , rfMinSamples :: Int       -- minimum samples per leaf (default 3)
  , rfMtry       :: Maybe Int -- candidate features per split (default d/3)
  , rfBootstrap  :: Bool      -- use bootstrap (default True)
  }

defaultRFConfig :: RFConfig

data RandomForest = ...    -- internally a list of Trees

fitRF :: RFConfig
      -> [[Double]]        -- rows = samples, columns = features
      -> [Double]          -- y
      -> GenIO
      -> IO RandomForest

predictRF :: RandomForest -> [Double] -> Double
featureImportance :: RandomForest -> Vector Double  -- normalised (sums to 1)

-- single-tree API exposed too
data Tree = Leaf Double | Node !Int !Double !Tree !Tree
buildTree   :: RFConfig -> [[Double]] -> [Double] -> GenIO -> IO Tree
predictTree :: Tree -> [Double] -> Double
```

### 3.4 Example

```haskell
{-# LANGUAGE OverloadedStrings #-}
import qualified System.Random.MWC as MWC
import qualified Data.Vector as V
import Model.RandomForest

main :: IO ()
main = do
  let n = 100
      rows = [ [ fromIntegral i / 10
               , sin (fromIntegral i / 5)
               , fromIntegral (i `mod` 3)
               ] | i <- [0..n-1] ]
      ys = [ row !! 0 + 2 * row !! 1 + row !! 2 + 0.1 * sin (fromIntegral i)
           | (i, row) <- zip [0..] rows ]
      cfg = defaultRFConfig
              { rfTrees = 200
              , rfMaxDepth = 10
              }
  gen <- MWC.createSystemRandom
  forest <- fitRF cfg rows ys gen
  let yhat = map (predictRF forest) rows
      imp  = featureImportance forest
  putStrLn $ "Feature importance: " ++ show (V.toList imp)
  putStrLn $ "RMSE: " ++ show (sqrt (sum [(y-h)^(2::Int) | (y,h) <- zip ys yhat] / fromIntegral n))
```

### 3.5 CLI

```bash
hanalyze rf data.csv "x1 x2 x3 x4" y \
    --trees 200 \
    --max-depth 12 \
    --min-samples 3 \
    --report
```

The report includes a **Feature importance** bar chart (`SecBarChart`).

### 3.6 Visualization via Reportable

`Reportable RandomForest` is not yet provided (CLI builds sections directly). Custom report:

```haskell
import qualified Viz.ReportBuilder as RB
import qualified Data.Vector as V

let imp = V.toList (featureImportance forest)
    cfg = RB.defaultReportConfig "RF demo"
    sections =
      [ RB.secDataOverview df xCols yCol
      , RB.secModelOverview "Random Forest" formula Nothing
      , RB.secKeyValue "Fit summary"
          [ ("Trees", T.pack (show (rfTrees cfg)))
          , ("R²",    T.pack (printf "%.4f" r2))
          ]
      , RB.secBarChart "Feature importance" (zip xCols imp)
      , RB.secResiduals yhat resid
      ]
RB.renderReport "rf.html" cfg sections
```

### 3.7 Caveats

- **Limited interpretability**: explaining individual predictions is hard. For per-feature
  effects, GAM or splines are more suitable.
- **Training time**: O(N · n log n · d) for N trees, n samples, depth d. Roughly seconds-to-tens-of-seconds at 100 × 1000 × depth 12.
- **Importance bias**: continuous and high-cardinality features get chosen more often as
  split candidates and end up over-credited. **Permutation Importance** is the standard
  remedy but is not yet implemented.
- **Out-of-Bag (OOB) score** is not implemented. Measure generalisation with a held-out
  test set or k-fold CV.

---

## 4. Method comparison and selection guide

| | LM/GLM | **Quantile** | Spline | **GAM** | Kernel | **RF** |
|---|---|---|---|---|---|---|
| Linearity | linear | linear (τ-quantile) | nonlinear (1D) | additive nonlinear | nonlinear (kernel) | nonlinear + interactions |
| Interpretability | ◎ coefficients | ◎ quantile coefs | ○ 1D curve | ○ partial effects | △ black-box | △ importance only |
| Overfit resistance | ◎ simple | ○ simple | △ depends on knots | ○ stable via Ridge | △ depends on h | ◎ bagging |
| Outlier robustness | × | **◎ median** | × | × | △ | ○ split criterion |
| Feature interactions | manual | manual | manual | × (additive only) | × | **◎ automatic** |
| Scale invariance | × (need normalisation) | × | × | × | × | ◎ unnecessary |
| Training time | fast | medium (iterative) | fast | fast | O(n²)–… | O(N·n log n·d) |
| Prediction interval | CI/PI | **multiple τ** | needs bootstrap | bootstrap | bootstrap | bootstrap |

### Selection by use case

| Goal | First choice |
|---|---|
| **Simple linear relationship** | LM (`Model.LM`) |
| **Many outliers** | Quantile (τ=0.5) |
| **Prediction interval (10 % / 90 %)** | Quantile (τ=0.1, 0.9) |
| **1-variable nonlinearity** | Spline (B-spline) |
| **Multi-variable nonlinearity + interpretability** | GAM |
| **Complex feature interactions** | Random Forest |
| **# features d ≫ # samples n** | Lasso (sparsity) |
| **Large data + nonlinear** | RFF (`Model.RFF`) |

### Combination strategies

- **Exploration**: start with Random Forest to inspect feature importance.
- **Interpretable model**: visualise the most important features in detail with GAM.
- **Production prediction**: choose between GAM and RF via CV.
- **Anomaly detection**: learn the 95 / 99 % upper quantile and flag exceedances.

---

## Related docs

- [01-lm.md](01-lm.md) — linear regression basics (a useful precursor to choosing Quantile / GAM / RF)
- [04-spline-kernel-regularized.md](04-spline-kernel-regularized.md) — Spline / Kernel / Regularized
- [theory-regression-extensions.md](theory-regression-extensions.md) — mathematical background
- [../visualization/02-report-builder.md](../visualization/02-report-builder.md) — custom reports via `Viz.ReportBuilder`
