# Random Forest (回帰)

> 🌐 [English](06-randomforest.md) | **日本語**

> 決定木 + bagging + ランダム特徴選択。`Hanalyze.Model.RandomForest` モジュール。
> 分類版は [08-decisiontree.ja.md](08-decisiontree.ja.md) を参照 (分類は CART 単体)。
>
> 関連: [06-quantile.ja.md](06-quantile.ja.md) / [06-gam.ja.md](06-gam.ja.md)


## 何のために使うか

決定木 (CART) は強力な非線形 + 交互作用モデルだが過学習しやすい。
**Random Forest** は多数の木の平均で過学習を抑える:

- 多次元の **交互作用** を自然に扱う
- 特徴のスケーリング不要
- 欠損値・外れ値にも頑健 (split ベースなので)
- **特徴重要度** が副産物として得られる

応用例:
- **マーケティング**: 50+ 特徴 (顧客属性、購買履歴、地理) からチャーン予測
- **製造**: センサーデータ (高次元・相関多) からの異常検知
- **医療**: バイオマーカーから病気予測

## アルゴリズム — CART + Bagging + Random Subspace

### CART (Classification And Regression Tree)

各内部ノードで:
1. 特徴を 1 つ選ぶ
2. 閾値を選ぶ
3. データを左 (≤ 閾値) / 右 (> 閾値) に分割
4. 分割後の **分散減少** が最大になる split を greedy に選ぶ
5. ノード内サンプル数が少ない/最大深さに達したら葉にする
6. 葉の予測値 = ノード内 y の平均

### Bagging (Bootstrap Aggregating)

n 本の木をそれぞれ **異なる bootstrap サンプル** (元データから復元抽出) で
構築。予測は n 本の平均。バリアンスが 1/n に近づく → 過学習抑制。

### Random Subspace

各 split で **mtry 個の特徴をランダムに選ぶ** (デフォルト d/3)。これにより
木の間の相関が下がり、bagging の効果が増す。

### 特徴重要度

`Hanalyze.Design.RandomForest` の簡易版: **各特徴で行われた split の回数**。
もう少し原則的な指標:
- **Mean Decrease in Impurity (MDI)**: split 時の分散減少を集計
- **Permutation Importance**: 1 列ランダム並べ替えして MSE 増を測る

(現状の実装は単純な split 回数。MDI/Permutation は将来課題)

## ライブラリ API

```haskell
import Model.RandomForest

data RFConfig = RFConfig
  { rfTrees      :: Int       -- ツリー数 (default 100)
  , rfMaxDepth   :: Int       -- 最大深さ (default 12)
  , rfMinSamples :: Int       -- 葉の最小サンプル (default 3)
  , rfMtry       :: Maybe Int -- split 候補特徴数 (default d/3)
  , rfBootstrap  :: Bool      -- bootstrap 使用 (default True)
  }

defaultRFConfig :: RFConfig

data RandomForest = ...    -- 内部に Tree のリスト

fitRF :: RFConfig
      -> [[Double]]        -- 行 = サンプル, 列 = 特徴
      -> [Double]          -- y
      -> GenIO
      -> IO RandomForest

predictRF :: RandomForest -> [Double] -> Double
featureImportance :: RandomForest -> Vector Double  -- 正規化済 (合計 1)

-- 単一木 API も公開
data Tree = Leaf Double | Node !Int !Double !Tree !Tree
buildTree   :: RFConfig -> [[Double]] -> [Double] -> GenIO -> IO Tree
predictTree :: Tree -> [Double] -> Double
```

## 使用例

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

## CLI

```bash
hanalyze rf data.csv "x1 x2 x3 x4" y \
    --trees 200 \
    --max-depth 12 \
    --min-samples 3 \
    --report
```

レポートに **Feature importance** バーチャートが含まれる (`SecBarChart`)。

## Reportable による可視化

現状 `Reportable RandomForest` 未提供 (CLI で section 直接構築)。
独自レポート例:

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

## 注意点

- **解釈性は限定的**: 個別の予測の理由付けが難しい。各特徴の効果を
  詳しく見たいなら GAM や Spline の方が向く。
- **訓練時間**: ツリー数 N、データ規模 n、深さ d で O(N · n log n · d)。
  100 木 × 1000 サンプル × 深さ 12 で数秒〜数十秒。
- **特徴重要度の偏り**: 連続値かつカテゴリ数の多い特徴ほど split 候補に
  選ばれやすく、importance が過大評価されがち。代替として **Permutation
  Importance** が推奨されるが現状未実装。
- **Out-of-Bag (OOB) score** は未実装。汎化性能を測るには別途 train/test
  split か k-fold CV を行う。

---


---

## 関連リンク

- 線形回帰: [01-lm.ja.md](01-lm.ja.md)
- 正則化: [04-regularized.ja.md](04-regularized.ja.md)
- 理論背景: [theory-regression-extensions.ja.md](theory-regression-extensions.ja.md)
