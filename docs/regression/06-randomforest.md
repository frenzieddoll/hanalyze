# Random Forest (Regression)

> 🌐 **English** | [日本語](06-randomforest.ja.md)

> Decision tree + bagging + random feature selection. `Hanalyze.Model.RandomForest` module.
> For classification, see [08-decisiontree.md](08-decisiontree.md) (classification uses CART alone).
>
> Related: [06-quantile.md](06-quantile.md) / [06-gam.md](06-gam.md)


## Why Use It?

Decision trees (CART) are powerful for nonlinear + interaction modeling but prone to overfitting.
**Random Forest** reduces overfitting by averaging many trees:

- Naturally handles high-dimensional **interactions**
- Feature scaling not required
- Robust to missing values and outliers (split-based)
- **Feature importance** is a byproduct

Applications:
- **Marketing**: Churn prediction from 50+ features (customer attributes, purchase history, geography)
- **Manufacturing**: Anomaly detection from sensor data (high-dimensional, highly correlated)
- **Healthcare**: Disease prediction from biomarkers

## Algorithm — CART + Bagging + Random Subspace

### CART (Classification And Regression Tree)

At each internal node:
1. Select a feature
2. Select a threshold
3. Partition data into left (≤ threshold) / right (> threshold)
4. Greedily choose the split that **maximizes variance reduction**
5. Create a leaf when samples in node are few or max depth is reached
6. Leaf prediction value = mean of y in node

### Bagging (Bootstrap Aggregating)

Build n trees, each on a **different bootstrap sample** (sampling with replacement from original data).
Prediction is the average of n trees. Variance approaches 1/n → reduces overfitting.

### Random Subspace

At each split, **randomly select mtry features** (default d/3). This reduces correlation between trees,
increasing bagging effectiveness.

### Feature Importance

Simple version in `Hanalyze.Design.RandomForest`: **count of splits performed per feature**.
More principled metrics:
- **Mean Decrease in Impurity (MDI)**: aggregate variance reduction at splits
- **Permutation Importance**: randomly permute one column and measure MSE increase

(Current implementation is simple split count. MDI/Permutation is a future task.)

Feature importance can be visualized as bar chart with `toPlot` (`Plottable RandomForest`).

## Library API

```haskell
import Hanalyze.Model.RandomForest

data RFConfig = RFConfig
  { rfTrees      :: Int       -- Number of trees (default 100)
  , rfMaxDepth   :: Int       -- Max depth (default 12)
  , rfMinSamples :: Int       -- Min samples in leaf (default 3)
  , rfMtry       :: Maybe Int -- Num candidate features per split (default d/3)
  , rfBootstrap  :: Bool      -- Use bootstrap (default True)
  }

defaultRFConfig :: RFConfig

data RandomForest = ...    -- Internally contains list of Trees

fitRF :: RFConfig
      -> [[Double]]        -- rows = samples, columns = features
      -> [Double]          -- y
      -> GenIO
      -> IO RandomForest

predictRF :: RandomForest -> [Double] -> Double
featureImportance :: RandomForest -> Vector Double  -- normalized (sum to 1)

-- Single tree API also exported
data Tree = Leaf Double | Node !Int !Double !Tree !Tree
buildTree   :: RFConfig -> [[Double]] -> [Double] -> GenIO -> IO Tree
predictTree :: Tree -> [Double] -> Double
```

## Usage Example

```haskell
{-# LANGUAGE OverloadedStrings #-}
import qualified System.Random.MWC as MWC
import qualified Data.Vector as V
import Hanalyze.Model.RandomForest

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

## CLI

```bash
hanalyze rf data.csv "x1 x2 x3 x4" y \
    --trees 200 \
    --max-depth 12 \
    --min-samples 3 \
    --report
```

Report includes a **Feature importance** bar chart (`SecBarChart`).

## Visualization via Reportable

Currently `Reportable RandomForest` is not provided (CLI constructs sections directly).
Example of custom report:

```haskell
import qualified Hanalyze.Viz.ReportBuilder as RB
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

## Caveats

- **Interpretability is limited**: difficult to justify individual predictions. For detailed
  feature effects, GAM or Spline is preferable.
- **Training time**: O(N · n log n · d) where N = tree count, n = data size, d = depth.
  100 trees × 1000 samples × depth 12 takes seconds to tens of seconds.
- **Feature importance bias**: continuous features with many categories are more likely to be
  selected for splits, causing importance to be overestimated. **Permutation Importance**
  is recommended as an alternative but currently unimplemented.
- **Out-of-Bag (OOB) score** is unimplemented. To measure generalization, use separate train/test
  split or k-fold CV.

---


---

## Related Links

- Linear regression: [01-lm.md](01-lm.md)
- Regularization: [04-regularized.md](04-regularized.md)
- Theoretical background: [theory-regression-extensions.md](theory-regression-extensions.md)
