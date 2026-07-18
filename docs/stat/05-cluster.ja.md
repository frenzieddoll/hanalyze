# Model.Cluster — クラスタリング

> sklearn.cluster 相当。K-means + シルエット係数 + 階層クラスタリング (dendrogram) を実装。
> DBSCAN は後続フェーズ予定。

## 1. K-means

```haskell
import qualified Hanalyze.Model.Cluster as Cl
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

`kmrLabels` で得たクラスタ ID を色分けして散布図に重ねると、各サンプルが
どのクラスタに割り当てられたかを直接確認できます。下図は k=3 での割り当て結果です。

![KMeans のクラスタ散布図 (ラベルで色分け・k=3)](../images/kmeans-clusters.svg)

### クラスタを囲む (凸包 / 95% 楕円)

散布点に加えて群を「囲む」と、各クラスタの広がりが一目で分かります。plot 連携層
(`Hanalyze.Plot`) に 2 通りの囲み方があります。群色は `clusterScatterOf` と一致します。

```haskell
-- clusterHullOf    = 凸包の輪郭 (ggplot geom_encircle 相当・実データの外郭)
-- clusterEllipseOf = 95% 共分散楕円 (ggplot stat_ellipse 相当・正規分布仮定の等確率線)
cdf |>> ( clusterScatterOf cdf kres "x" "y"
          <> clusterHullOf    cdf kres "x" "y"     -- または clusterEllipseOf
          <> centroidsOf kres 0 1 )
```

凸包は実データの外郭、楕円は分布仮定の等確率線で、用途が異なります (両方提供)。

![KMeans クラスタ + 凸包の輪郭](../images/kmeans-hull.svg)

![KMeans クラスタ + 95% 共分散楕円](../images/kmeans-ellipse.svg)

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

## 4. 階層クラスタリング (dendrogram)

`Hanalyze.Model.HierarchicalCluster` は凝集型 (agglomerative) 階層クラスタリングを
実装します (Single / Complete / Average / Ward linkage)。結果 `HClusterFit` はそのまま
`toPlot` で **dendrogram** に描けます (scipy `dendrogram` / R `hclust` 流)。

```haskell
import qualified Hanalyze.Model.HierarchicalCluster as HC

let hfit = HC.fitHierarchical HC.Ward xs   -- xs = n × p 行列

-- 既定 dendrogram (単色)
noDf |>> (toPlot hfit <> title "Hierarchical clustering (Ward)")

-- 色閾値で葉クラスタを色分け (scipy color_threshold 流)
noDf |>> dendrogramOf' defaultDendroOpts { doColorThreshold = Just 3.0 } hfit
```

縦軸 `height` はマージ高さ (2 クラスタが結合する時の非類似度。Ward は結合時の分散増分)。
低い高さで結合するものほど似ています。閾値未満のサブツリーは `cutTree` のクラスタで
色分けされ、閾値超のリンクは単色になります。

![階層クラスタリングの dendrogram (Ward・色閾値で 3 クラスタ)](../images/dendrogram.svg)

> dendrogram は plot の custom mark (Phase 48・`hgg-custom` の `dendrogramMark`) で
> 描画します (U 字リンクを焼き込み線分で emit・HS/PS parity 有)。群の囲み (輪郭のみ) は
> 現状 annotation ベースで、任意ポリゴンの半透明塗りは将来 plot 正式 mark
> (`MPolygon`/`MTile`) 移譲時に対応します。

## 5. 注意

- **特徴量のスケーリング必須** (Euclidean 距離前提)。`Hanalyze.Stat.Standardize`
  の `applyStandardizer` で前処理推奨
- **空 cluster 対応**: 現状は zero vector (改善余地あり)
- **大規模 n**: Mini-batch K-means は未実装
