# Stat.Interpret — モデル解釈

> SHAP / LIME / `sklearn.inspection` 相当。
> モデル非依存な解釈手法 (model-agnostic explanation)。

## 1. Permutation Feature Importance (Breiman 2001)

最も影響の大きい feature を特定:

```haskell
import qualified Hanalyze.Stat.Interpret as Interp
import qualified System.Random.MWC as MWC

gen <- MWC.createSystemRandom

-- 任意のモデル (predict 関数) と score 関数
let predict :: [[Double]] -> [Double]
    predict = -- 自分の学習済みモデルから

    score :: [Double] -> [Double] -> Double
    score yTrue yPred = -- 例: R², accuracy など (高いほど良い)

    cfg = Interp.defaultPermutationConfig  -- 30 repeats

result <- Interp.permutationImportance cfg predict score xs ys gen

Interp.piMeanImportance result  -- per-feature mean importance
Interp.piStdImportance result   -- 標準偏差
Interp.piBaselineScore result   -- shuffle なしのスコア
```

各 feature を 30 回 shuffle してスコア低下分の平均を importance とする。

## 2. Partial Dependence Plot (Friedman 2001)

ある feature の **限界効果** (他の feature を marginalise):

```haskell
-- feature 0 を [0, 1, 2, 3, 4, 5] で variation
let grid = [0, 1, 2, 3, 4, 5]
    pdp  = Interp.partialDependence predict xs 0 grid

Interp.pdpFeatureValues pdp   -- grid そのまま
Interp.pdpMeanPredict pdp     -- 各 grid 値での平均予測
```

非線形関係も可視化可能。

## 3. ICE (Individual Conditional Expectation, Goldstein 2015)

各 sample ごとの partial dependence curve:

```haskell
let ice = Interp.icePlot predict xs 0 grid

Interp.iceCurves ice   -- per-sample curve のリスト
Interp.iceMean ice     -- 平均 curve (= partial dependence)
```

ICE curves が分散していれば **interaction 効果** あり (heterogeneous response)。

## 4. 使用例

```haskell
-- Random Forest 分類器を解釈
import qualified Hanalyze.Model.RandomForest as RF
import qualified Hanalyze.Stat.ClassMetrics as CM

let cfg = RF.defaultRFConfig  -- ... (existing module)
rf <- RF.fitRF cfg xsTrain ysTrain gen

let predict :: [[Double]] -> [Double]
    predict = map (RF.predictRF rf)

    score :: [Double] -> [Double] -> Double
    score yt yp =
      let yi  = map round yt :: [Int]
          ypi = map round yp :: [Int]
          c   = CM.confusionMatrix yi ypi
      in CM.accuracy c

-- どの feature が最も重要?
imp <- Interp.permutationImportance Interp.defaultPermutationConfig
                                    predict score xsTest ysTest gen

-- 上位 feature の partial dependence で関係性確認
let topIdx = -- argmax of imp
    grid   = [from .. to]  -- feature の range
    pdp    = Interp.partialDependence predict xsTest topIdx grid
```

## 5. 手法の選び方

| 知りたいこと | 手法 |
|---|---|
| どの feature が重要? | `permutationImportance` |
| feature の限界効果は単調? 非線形? | `partialDependence` |
| 効果は均一? 個体差あり? | `icePlot` |
| 局所的な貢献度 (1 サンプル単位) | SHAP (今後追加予定) |

## 6. 注意点

- **計算コスト**: permutation imp は `nFeatures × nRepeats × predict-cost`
- **相関 feature**: shuffle で artificial な点が生成される可能性 → conditional permutation の方が良いケースあり
- **train data で計算するか test data で?**: test data 推奨 (overfit を反映しない)
