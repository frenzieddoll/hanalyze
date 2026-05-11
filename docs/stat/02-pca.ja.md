# Model.PCA — 主成分分析

> hanalyze の `Hanalyze.Model.PCA` は SVD ベースの PCA を提供します。
> sklearn `decomposition.PCA` 相当。

## 1. API

```haskell
data PCAStandardize = NoStandardize | Center | CenterScale

data PCAResult = PCAResult
  { pcaMean           :: Vector Double  -- per-col mean
  , pcaScale          :: Vector Double  -- per-col SD (CenterScale 時のみ != 1)
  , pcaStandardize    :: PCAStandardize
  , pcaComponents     :: Matrix Double  -- k × p, 主成分 (loadings)
  , pcaSingularValues :: Vector Double  -- σ_i, descending
  , pcaExplainedVar   :: Vector Double  -- σ_i² / (n-1)
  , pcaExplainedRatio :: Vector Double  -- 寄与率
  , pcaNSamples       :: Int
  , pcaNFeatures      :: Int
  }

pca           :: PCAStandardize -> Maybe Int -> Matrix Double -> PCAResult
pcaTransform  :: PCAResult -> Matrix Double -> Matrix Double  -- m × k scores
pcaInverse    :: PCAResult -> Matrix Double -> Matrix Double  -- 復元
pcaCumExplained :: PCAResult -> Vector Double                  -- 累積寄与率
```

## 2. 使用例

```haskell
import qualified Model.PCA as PCA
import qualified Numeric.LinearAlgebra as LA

-- iris-like データ (n × p)
let xs = LA.fromLists [[5.1, 3.5, 1.4, 0.2], [4.9, 3.0, 1.4, 0.2], ...]

-- 標準化 PCA で 2 主成分
let result = PCA.pca PCA.CenterScale (Just 2) xs
    scores = PCA.pcaTransform result xs   -- n × 2 (低次元表現)

-- 寄与率
LA.toList (PCA.pcaExplainedRatio result)
-- [0.7296, 0.2285] : PC1 が 73%、PC2 が 23%

-- 復元 (情報損失あり)
let xRecon = PCA.pcaInverse result scores
```

## 3. 標準化モードの選び方

| モード | 用途 |
|---|---|
| `NoStandardize` | 既に正規化済み or 同じ単位 |
| `Center` | 平均除去のみ (デフォルト的) |
| `CenterScale` | 各列のスケールが異なる (例: 身長 cm vs 体重 kg) |

## 4. 主成分数の選び方

```haskell
-- 累積寄与率 90% に達する主成分数
let cum = LA.toList (PCA.pcaCumExplained (PCA.pca PCA.Center Nothing xs))
    k90 = length (takeWhile (< 0.9) cum) + 1
    pca90 = PCA.pca PCA.Center (Just k90) xs
```

## 5. 参考文献

- Pearson (1901) "On lines and planes of closest fit to systems of
  points in space"
- Jolliffe (2002) *Principal Component Analysis*, Springer
