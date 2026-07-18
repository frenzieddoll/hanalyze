# Model.PCA — Principal Component Analysis

> 🌐 **English** | [日本語](02-pca.ja.md)

> hanalyze's `Hanalyze.Model.PCA` provides SVD-based PCA.
> Equivalent to sklearn `decomposition.PCA`.

## 1. API

```haskell
data PCAStandardize = NoStandardize | Center | CenterScale

data PCAResult = PCAResult
  { pcaMean           :: Vector Double  -- per-column mean
  , pcaScale          :: Vector Double  -- per-column SD (non-1 only with CenterScale)
  , pcaStandardize    :: PCAStandardize
  , pcaComponents     :: Matrix Double  -- k × p, principal components (loadings)
  , pcaSingularValues :: Vector Double  -- σ_i, descending
  , pcaExplainedVar   :: Vector Double  -- σ_i² / (n-1)
  , pcaExplainedRatio :: Vector Double  -- explained variance ratio
  , pcaNSamples       :: Int
  , pcaNFeatures      :: Int
  }

pca           :: PCAStandardize -> Maybe Int -> Matrix Double -> PCAResult
pcaTransform  :: PCAResult -> Matrix Double -> Matrix Double  -- m × k scores
pcaInverse    :: PCAResult -> Matrix Double -> Matrix Double  -- reconstruction
pcaCumExplained :: PCAResult -> Vector Double                  -- cumulative explained ratio
```

## 2. Usage Example

```haskell
import qualified Hanalyze.Model.PCA as PCA
import qualified Numeric.LinearAlgebra as LA

-- iris-like data (n × p)
let xs = LA.fromLists [[5.1, 3.5, 1.4, 0.2], [4.9, 3.0, 1.4, 0.2], ...]

-- Standardized PCA with 2 principal components
let result = PCA.pca PCA.CenterScale (Just 2) xs
    scores = PCA.pcaTransform result xs   -- n × 2 (low-dimensional representation)

-- Explained variance ratio
LA.toList (PCA.pcaExplainedRatio result)
-- [0.7296, 0.2285] : PC1 explains 73%, PC2 explains 23%

-- Reconstruction (with information loss)
let xRecon = PCA.pcaInverse result scores
```

## 3. Standardization Mode Selection

| Mode | Usage |
|---|---|
| `NoStandardize` | Already normalized or same units |
| `Center` | Mean-centering only (default-like) |
| `CenterScale` | Feature scales differ (e.g., height cm vs weight kg) |

## 4. Choosing Number of Principal Components

Plotting explained variance ratio per principal component as a bar chart (scree plot)
lets you visually judge where the ratio drops sharply (elbow) and sets a target for
principal component retention. In the figure below, PC1 dominates, with small contributions
from PC2 onward.

![PCA scree plot: explained variance ratio per component, PC1 dominant](../images/pca-scree.svg)

```haskell
-- Principal components up to 90% cumulative explained variance
let cum = LA.toList (PCA.pcaCumExplained (PCA.pca PCA.Center Nothing xs))
    k90 = length (takeWhile (< 0.9) cum) + 1
    pca90 = PCA.pca PCA.Center (Just k90) xs
```

## 5. References

- Pearson (1901) "On lines and planes of closest fit to systems of
  points in space"
- Jolliffe (2002) *Principal Component Analysis*, Springer
