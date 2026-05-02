# Quantile / GAM / Random Forest

> 🌐 [English](06-quantile-gam-rf.md) | **日本語**

> 関連: [04-spline-kernel-regularized.ja.md](04-spline-kernel-regularized.ja.md),
> [01-lm.ja.md](01-lm.ja.md)

OLS や GLM では捉えにくい問題 (外れ値・非対称分布・複雑な非線形構造・
特徴間の相互作用) に対応する 3 つの回帰手法。

## 目次

1. [Quantile regression (分位点回帰)](#1-quantile-regression-分位点回帰)
2. [Generalized Additive Model (GAM)](#2-generalized-additive-model-gam)
3. [Random Forest (回帰)](#3-random-forest-回帰)
4. [3 手法の比較・選択指針](#4-3-手法の比較選択指針)

---

## 1. Quantile regression (分位点回帰)

### 1.1 何のために使うか

通常 OLS は **平均** を fit するが、Quantile regression は τ-分位点 (τ ∈ (0, 1))
を fit する:

| τ | 推定対象 |
|---|---|
| 0.5 | **中央値** (外れ値に強い) |
| 0.1 / 0.9 | 下側/上側予測区間 |
| 0.05 / 0.95 | より広い予測区間 |

応用例:
- **収入分布**: 中位収入と上位 10% 収入を別々にモデリング
- **医療**: 子供の身長の 5%/50%/95% パーセンタイル曲線
- **電力需要**: ピーク需要 (95 パーセンタイル) のモデリング
- **金融**: VaR (Value at Risk = 損失の 1% 分位点)

### 1.2 損失関数 — Pinball loss

通常の OLS は二乗誤差 Σ r²。Quantile regression は **非対称な絶対誤差**:

$$ \rho_\tau(u) = u\,(\tau - \mathbb{1}[u < 0]) = \begin{cases} \tau u   & u \ge 0 \\ (\tau - 1) u & u < 0 \end{cases} $$

これを Pinball loss / Check loss と呼ぶ。τ=0.5 で標準的な絶対誤差 |u|/2 に
一致 → 中央値推定。τ=0.9 だと「正残差は 0.9 倍重み、負残差は 0.1 倍重み」と
いう非対称損失で **上側分位点** を推定。

### 1.3 アルゴリズム — MM-IRLS (Hunter-Lange)

| u | は微分不能なので、二次関数で逐次近似 (Majorization-Minimization):

$$ |u| \le \frac{u^2 + u_k^2}{2|u_k|}  \quad \text{(等号は } u = u_k \text{)} $$

これを使って Pinball loss を二次関数に置き換え、**重み付き最小二乗** に
帰着:

1. β₀ = OLS 解で初期化
2. 反復 k:
   - r = y - X β_k
   - w_i = 1 / (2 max(|r_i|, ε))
   - y'_i = y_i + (τ - 0.5) / w_i
   - β_{k+1} = (Xᵀ W X)⁻¹ Xᵀ W y'
3. ||β_{k+1} - β_k|| < tol で停止

`Model.Quantile.fitQuantile` で実装。max 100 iter, tol 1e-7。

### 1.4 評価指標 — Pseudo R¹_τ (Koenker-Machado 1999)

通常の R² は平均ベースなので分位点回帰には不適切。代わりに:

$$ R^1_\tau = 1 - \frac{V_\tau(\text{model})}{V_\tau(\text{intercept-only})} $$

ここで $V_\tau(m) = \Sigma \rho_\tau(r_i)$。値域は (-∞, 1]、0 は intercept-only
と同等、1 が完全 fit。

### 1.5 ライブラリ API

```haskell
import Model.Quantile

data QRFit = QRFit
  { qfTau     :: Double
  , qfBeta    :: Vector Double
  , qfYHat    :: Vector Double
  , qfResid   :: Vector Double
  , qfPinball :: Double         -- Σ ρ_τ(r_i)
  , qfR1      :: Double         -- Pseudo R¹_τ
  , qfIters   :: Int
  }

fitQuantile :: Double          -- τ ∈ (0, 1)
            -> Matrix Double   -- X (intercept 列付き)
            -> Vector Double   -- y
            -> QRFit

predictQuantile :: QRFit -> Matrix Double -> Vector Double

pinballLoss :: Double -> [Double] -> Double      -- 個別 SN 計算用
pseudoR1    :: Double -> Double -> Double        -- modelV, baseV → R¹_τ
```

### 1.6 使用例

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
      fitMed  = fitQuantile 0.5  xMat yLA   -- 中央値
      fitLow  = fitQuantile 0.1  xMat yLA   -- 下側 10%
      fitHigh = fitQuantile 0.9  xMat yLA   -- 上側 90%
  putStrLn $ "Median: "    ++ show (LA.toList (qfBeta fitMed))
  putStrLn $ "10% bound: " ++ show (LA.toList (qfBeta fitLow))
  putStrLn $ "90% bound: " ++ show (LA.toList (qfBeta fitHigh))
  putStrLn $ "Pseudo R¹ (median): " ++ show (qfR1 fitMed)
```

### 1.7 CLI

```bash
# 中央値回帰
hanalyze quantile data.csv x y --tau 0.5 --report

# 複数分位を 1 枚のチャートで重ね描き (10%/50%/90%)
hanalyze quantile data.csv x y --taus 0.1,0.5,0.9 --report
```

`--taus` 指定時のレポートに **Multiple quantile fits** セクションが
追加され、観測散布 + 各分位線が tableau10 カラーで色分け表示される。

### 1.8 Reportable による可視化

現状 `Reportable QRFit` instance は未提供 (CLI ハンドラが直接 section を
構築)。ライブラリから同等のレポートを作る場合:

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

### 1.9 注意点

- **MM-IRLS は遅い**: max 100 iter まで反復。データ規模 N が大きいと
  WLS の逆行列計算がボトルネック (O(p³) per iteration)。
- **τ が 0 / 1 に近いと不安定**: τ=0.01 / 0.99 だと ε による平滑化の影響が
  大きく、推定がブレやすい。
- **多変量化**: 上記 API は単純な p × β の線形 quantile。非線形版が必要なら
  spline 基底を別途張って fitQuantile に渡す。

---

## 2. Generalized Additive Model (GAM)

### 2.1 何のために使うか

LM (線形回帰) は線形性を仮定する。Spline は 1 変数の非線形性を扱える。
GAM はこれを **多変数で加法的に** 拡張:

$$ y = \beta_0 + \sum_{j=1}^{p} s_j(x_j) + \varepsilon $$

各 $s_j$ は変数 $x_j$ の **滑らかな関数**。

応用例:
- **疫学**: 年齢 + BMI + 喫煙年数 がそれぞれ非線形に死亡率に寄与
- **環境**: 気温 + 湿度 + 風速 がそれぞれ非線形に大気汚染指数に寄与
- **マーケティング**: 価格 + 広告費 + 季節 が非線形に売上に寄与

GAM の利点:
- **解釈性**: 各 $s_j$ をプロットすると因子の効果が一目瞭然
- **柔軟性**: 関数形を仮定しない (B-spline で自動推定)
- **加法性**: 多次元交互作用は表現できないが、その分過学習しにくい

### 2.2 アルゴリズム

各特徴 $x_j$ について B-spline 基底 $B_j(x_j)$ (次数 d、ノット K 個) を構築。
統合計画行列:

$$ X = [\mathbf{1} \mid B_1 \mid B_2 \mid \ldots \mid B_p] $$

Ridge 正則化付き最小二乗 (intercept 列はペナルティ免除):

$$ \beta = (X^T X + \lambda S)^{-1} X^T y, \quad S = \text{diag}(0, 1, 1, \ldots) $$

各 $B_j$ は **列平均で中央化** (列ごとに平均を引く) → 識別性確保。
$\beta_0$ は y の平均を表し、$s_j$ は変動成分のみ。

予測:

$$ \hat{y}(x) = \beta_0 + \sum_j s_j(x_j) $$

各 $s_j(x)$ は単独でも取り出せる (`predictGAMComponent`) → **partial effect** の可視化。

### 2.3 ライブラリ API

```haskell
import Model.GAM

data GAMFit = GAMFit
  { gamDegree    :: Int
  , gamKnots     :: [[Double]]            -- 各特徴のノット
  , gamBetas     :: [Vector Double]        -- 各特徴の spline 係数
  , gamColMeans  :: [Vector Double]        -- 列平均 (中央化用)
  , gamIntercept :: Double
  , gamYHat      :: Vector Double
  , gamResid     :: Vector Double
  , gamR2        :: Double
  , gamLambda    :: Double
  }

fitGAM :: Int                  -- B-spline degree (3 推奨)
       -> Int                  -- 内部ノット数 (5 程度から開始)
       -> Double               -- Ridge λ (0.01 程度)
       -> [V.Vector Double]    -- 説明変数
       -> V.Vector Double      -- y
       -> GAMFit

predictGAM :: GAMFit -> [V.Vector Double] -> V.Vector Double

predictGAMComponent :: GAMFit -> Int -> V.Vector Double -> V.Vector Double
-- ^ j 番目の特徴の partial effect s_j(x_j) のみ
```

### 2.4 使用例

```haskell
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Vector as V
import Model.GAM

main :: IO ()
main = do
  let n = 100
      xs1 = V.fromList [ fromIntegral i / 10 | i <- [0..n-1] ]
      xs2 = V.fromList [ sin (fromIntegral i / 5) | i <- [0..n-1] ]
      ys  = V.fromList [ x1 * x1 + sin (3 * x2)   -- 非線形+非線形
                       | (x1, x2) <- zip (V.toList xs1) (V.toList xs2) ]
      fit = fitGAM 3 8 0.01 [xs1, xs2] ys
  putStrLn $ "Intercept: " ++ show (gamIntercept fit)
  putStrLn $ "R²:        " ++ show (gamR2 fit)

  -- 各特徴の partial effect を取り出す
  let s1 = predictGAMComponent fit 0 xs1   -- s_1(x_1)
      s2 = predictGAMComponent fit 1 xs2   -- s_2(x_2)
  -- s1 / s2 をプロットすれば各因子の非線形効果が見える
  putStrLn $ "s_1 range: " ++ show (V.minimum s1, V.maximum s1)
```

### 2.5 CLI

```bash
hanalyze gam data.csv "x1 x2 x3" y \
    --knots 8 \
    --lambda 0.05 \
    --report
```

レポートには **各特徴の partial effect** が個別セクションで描画される
(partial residual + smooth curve)。

### 2.6 Reportable による可視化

現状 `Reportable GAMFit` 未提供。CLI ハンドラを参考に独自構築:

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
    -- ↑ mySpec は app/Main.hs の gamPartialSpec を参考に作成
```

### 2.7 注意点

- **過学習リスク**: ノット数 K を大きくすると過学習しやすい。λ で正則化、
  または K を 5-10 程度に抑える。
- **加法性の仮定**: $s_j(x_j) \cdot s_k(x_k)$ のような交互作用は表現できない。
  必要なら `Model.Spline` で 2D テンソル積基底を別途構築するか、Random Forest
  / Gradient Boosting に切り替える。
- **外挿は危険**: 訓練範囲外の x で各 $s_j$ は非自然な振動をすることがある。

---

## 3. Random Forest (回帰)

### 3.1 何のために使うか

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

### 3.2 アルゴリズム — CART + Bagging + Random Subspace

#### CART (Classification And Regression Tree)

各内部ノードで:
1. 特徴を 1 つ選ぶ
2. 閾値を選ぶ
3. データを左 (≤ 閾値) / 右 (> 閾値) に分割
4. 分割後の **分散減少** が最大になる split を greedy に選ぶ
5. ノード内サンプル数が少ない/最大深さに達したら葉にする
6. 葉の予測値 = ノード内 y の平均

#### Bagging (Bootstrap Aggregating)

n 本の木をそれぞれ **異なる bootstrap サンプル** (元データから復元抽出) で
構築。予測は n 本の平均。バリアンスが 1/n に近づく → 過学習抑制。

#### Random Subspace

各 split で **mtry 個の特徴をランダムに選ぶ** (デフォルト d/3)。これにより
木の間の相関が下がり、bagging の効果が増す。

#### 特徴重要度

`Design.RandomForest` の簡易版: **各特徴で行われた split の回数**。
もう少し原則的な指標:
- **Mean Decrease in Impurity (MDI)**: split 時の分散減少を集計
- **Permutation Importance**: 1 列ランダム並べ替えして MSE 増を測る

(現状の実装は単純な split 回数。MDI/Permutation は将来課題)

### 3.3 ライブラリ API

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

### 3.4 使用例

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

レポートに **Feature importance** バーチャートが含まれる (`SecBarChart`)。

### 3.6 Reportable による可視化

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

### 3.7 注意点

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

## 4. 3 手法の比較・選択指針

| | LM/GLM | **Quantile** | Spline | **GAM** | Kernel | **RF** |
|---|---|---|---|---|---|---|
| 線形性 | 線形 | 線形 (τ-quantile) | 非線形 (1D) | 加法的非線形 | 非線形 (カーネル) | 非線形 + 交互作用 |
| 解釈性 | ◎ 係数 | ◎ 分位点係数 | ○ 1D 曲線 | ○ partial effects | △ ブラックボックス | △ 重要度のみ |
| 過学習耐性 | ◎ 簡素 | ○ 簡素 | △ knots 次第 | ○ Ridge で安定 | △ bandwidth 次第 | ◎ bagging |
| 外れ値耐性 | × | **◎ 中央値** | × | × | △ | ○ split 基準 |
| 特徴間交互作用 | 手動 | 手動 | 手動 | × (加法のみ) | × | **◎ 自動** |
| スケール変動耐性 | × (要正規化) | × | × | × | × | ◎ 不要 |
| 訓練時間 | 高速 | 中 (反復) | 高速 | 高速 | O(n²) 〜 | O(N·n log n·d) |
| 予測区間 | CI/PI | **複数 τ で取得** | bootstrap 必要 | bootstrap | bootstrap | bootstrap |

### ユースケース別の選択

| 目的 | 第一選択 |
|---|---|
| **シンプルな線形関係** | LM (`Model.LM`) |
| **外れ値が多い** | Quantile (τ=0.5) |
| **予測区間 (10%/90%)** | Quantile (τ=0.1, 0.9) |
| **1 変数の非線形** | Spline (B-spline) |
| **多変数の非線形 + 解釈性** | GAM |
| **特徴間に複雑な交互作用** | Random Forest |
| **特徴数 d ≫ サンプル数 n** | Lasso (sparsity) |
| **大規模データ + 非線形** | RFF (`Model.RFF`) |

### 組合せ戦略

- **データ探索**: まず Random Forest で feature importance を見る
- **解釈モデル**: 重要な特徴を GAM で詳しく可視化
- **予測本番**: GAM か RF (CV で選択)
- **異常検出**: Quantile で 95% / 99% 上限を学習し、超過を検出

---

## 関連ドキュメント

- [01-lm.ja.md](01-lm.ja.md) — 線形回帰の基礎 (Quantile/GAM/RF を選ぶ前段の考察)
- [04-spline-kernel-regularized.ja.md](04-spline-kernel-regularized.ja.md) — Spline/Kernel/Regularized
- [theory-regression-extensions.ja.md](theory-regression-extensions.ja.md) — 数学的背景
- [../visualization/02-report-builder.ja.md](../visualization/02-report-builder.ja.md) — `Viz.ReportBuilder` でカスタムレポート
