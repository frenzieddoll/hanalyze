# Stat.Bootstrap — Bootstrap + 並べ替え検定

> resampling-based な統計推測手法。
> パラメトリック仮定なしで CI / 仮説検定が可能。

## 1. Bootstrap

```haskell
import qualified Stat.Bootstrap as Boot
import qualified System.Random.MWC as MWC

gen <- MWC.createSystemRandom

let xs = LA.fromList [1.2, 1.5, 0.8, 2.1, 1.7, ...]

-- 平均の bootstrap 分布 (1000 resamples)
distribution <- Boot.bootstrap 1000 Boot.sampleMean xs gen

-- 95% percentile CI
(lo, hi) <- Boot.bootstrapCI 5000 0.95 Boot.sampleMean xs gen

-- BCa (bias-corrected & accelerated, Efron 1987)
(loBca, hiBca) <- Boot.bootstrapBcaCI 5000 0.95 Boot.sampleMean xs gen
```

## 2. 並べ替え検定

```haskell
-- 2 群の平均差を H0 (差なし) で検定
let groupA = LA.fromList [1, 2, 3, 4, 5]
    groupB = LA.fromList [4, 5, 6, 7, 8]

(diff, pVal) <- Boot.permutationTest 5000 groupA groupB gen
-- diff = mean(A) - mean(B) = -3.0
-- pVal = 0.008  (有意)
```

## 3. ユーティリティ統計量

```haskell
Boot.sampleMean   :: Vector Double -> Double
Boot.sampleVar    :: Vector Double -> Double  -- unbiased (n-1)
Boot.sampleMedian :: Vector Double -> Double
```

任意の `Vector Double -> Double` を bootstrap に渡せる。

## 4. CI の選び方

| CI 種類 | 適用場面 | 計算量 |
|---|---|---|
| `bootstrapCI` (percentile) | 統計量がほぼ symmetric | 標準 |
| `bootstrapBcaCI` (BCa) | 統計量が biased / skewed (例: median, var) | + jackknife (n 倍) |
| Normal-theory CI (`Stat.Test`) | 正規性が成立 | O(1) |

## 5. ベストプラクティス

- **resample 数**: CI なら 5000+、検定なら 5000+
- **再現性**: `MWC.initialize seed` で固定 seed
- **ベル型でない統計量**: BCa を使う (median, var, ratio など)
- **小サンプル (n < 30)**: bootstrap が parametric より頑健
