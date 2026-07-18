# Model.DecisionTree — CART for Classification

> 🌐 **English** | [日本語](08-decisiontree.ja.md)

> Equivalent to sklearn `DecisionTreeClassifier`.
> For regression, see existing [`Hanalyze.Model.RandomForest`](06-randomforest.md).

## 1. API

```haskell
data DTree
  = DLeaf
      { dlClassProbs :: Map Int Double  -- class → probability
      , dlMajority   :: Int
      }
  | DNode
      { dnFeature :: Int
      , dnThr     :: Double
      , dnLeft    :: DTree   -- ≤ thr
      , dnRight   :: DTree   -- > thr
      }

data DTConfig = DTConfig
  { dtMaxDepth        :: Maybe Int
  , dtMinSamplesSplit :: Int
  , dtMinSamplesLeaf  :: Int
  , dtMinImpurity     :: Double
  }

fitDT          :: DTConfig -> [[Double]] -> [Int] -> DTree
predictDT      :: DTree -> [Double] -> Int
predictDTProbs :: DTree -> [Double] -> Map Int Double
```

## 2. Usage Example

```haskell
import qualified Hanalyze.Model.DecisionTree as DT

-- Iris-like data
let xs = [[5.1, 3.5, 1.4, 0.2], [4.9, 3.0, 1.4, 0.2], ...]
    ys = [0, 0, 1, 1, 2, 2, ...]

let tree = DT.fitDT DT.defaultDTConfig xs ys

-- Prediction
DT.predictDT tree [5.0, 3.4, 1.5, 0.2]   -- :: Int
DT.predictDTProbs tree [5.0, 3.4, 1.5, 0.2]
-- fromList [(0, 0.95), (1, 0.05), (2, 0.0)]
```

## 3. Hyperparameter Configuration

| Parameter | Default | Effect |
|---|---|---|
| `dtMaxDepth` | `Nothing` (∞) | Deeper → overfitting. Set `Just 5` to control |
| `dtMinSamplesSplit` | 2 | Minimum samples for node split |
| `dtMinSamplesLeaf` | 1 | Minimum samples per leaf |
| `dtMinImpurity` | 0 | Minimum Gini impurity to attempt split |

```haskell
-- Prevent overfitting
let cfg = DT.defaultDTConfig
            { DT.dtMaxDepth = Just 5
            , DT.dtMinSamplesLeaf = 5
            }
```

## 4. Algorithm

CART (Classification And Regression Trees, Breiman et al. 1984):
- **Criterion**: Gini impurity = 1 - Σ p_i²
- **Split search**: Maximize gain across all feature × threshold candidates
- **Threshold**: Midpoint between adjacent distinct values
- **Recursion**: Depth-first construction until stop conditions

The constructed tree can be visualized as a tree diagram. Rectangles represent split nodes (e.g., `f1 ≤ 3.50`), ovals represent leaf nodes (predicted class `y=0/1`):

![Decision tree diagram](../images/decisiontree.svg)

## 5. Ensemble

Bagging across multiple trees:
- See existing `Hanalyze.Model.RandomForest` (regression only) for reference
- Classification RF can be built by constructing multiple `fitDT` with bootstrap × random feature subset (future implementation)

## 6. Notes

- **Feature scaling not needed** (split-based, not distance-based)
- **Missing values not supported**: Impute first with `imputeMean` etc.
- **Multiclass supported**: Integer labels OK
