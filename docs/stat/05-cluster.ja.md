# Model.Cluster — クラスタリング

> sklearn.cluster 相当。K-means + シルエット係数を実装。
> hierarchical / DBSCAN は後続フェーズ予定。

## 1. K-means

```haskell
import qualified Model.Cluster as Cl
import qualified Numeric.LinearAlgebra as LA
import qualified System.Random.MWC as MWC

gen <- MWC.createSystemRandom

let xs = LA.fromLists [[1, 2], [1.1, 2.1], [10, 10], [10.5, 10.5], ...]
    cfg = Cl.defaultKMeansConfig 2  -- k=2

result <- Cl.kMeans cfg xs gen

Cl.kmrCentroids result   -- 2 × p centroid 行列
Cl.kmrLabels result      -- 各 sample の cluster ID
Cl.kmrInertia result     -- within-cluster SS
Cl.kmrConverged result   -- True なら tol 内収束
```

## 2. 設定

```haskell
data KMeansConfig = KMeansConfig
  { kmK        :: Int       -- cluster 数
  , kmInit     :: KMeansInit  -- Forgy | KMeansPlus
  , kmMaxIter  :: Int       -- default 300
  , kmTol      :: Double    -- default 1e-4
  , kmRestarts :: Int       -- default 10 (multi-restart で最良 inertia 採用)
  }
```

`KMeansPlus` (k-means++) は default。Forgy より初期化品質が高く、
平均 O(log k) 近似保証 (Arthur-Vassilvitskii 2007)。

## 3. クラスタ数の選び方 (Elbow + Silhouette)

```haskell
-- Elbow: inertia vs k
let inertias = forM [2 .. 8] $ \k -> do
      r <- Cl.kMeans (Cl.defaultKMeansConfig k) xs gen
      pure (k, Cl.kmrInertia r)

-- Silhouette: k=2 から最大の値を選ぶ
silhouettes <- forM [2 .. 8] $ \k -> do
  r <- Cl.kMeans (Cl.defaultKMeansConfig k) xs gen
  pure (k, Cl.silhouette xs (Cl.kmrLabels r))
```

| Silhouette 値 | 解釈 |
|---|---|
| ≥ 0.7 | 強構造 |
| 0.5 ~ 0.7 | 妥当な構造 |
| 0.25 ~ 0.5 | 弱い構造 |
| < 0.25 | 構造なし or 過剰分割 |

## 4. 注意

- **特徴量のスケーリング必須** (Euclidean 距離前提)。`Stat.Standardize`
  の `applyStandardizer` で前処理推奨
- **空 cluster 対応**: 現状は zero vector (改善余地あり)
- **大規模 n**: Mini-batch K-means は未実装
