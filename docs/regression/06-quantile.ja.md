# 分位点回帰 (Quantile Regression)

> 🌐 [English](06-quantile.md) | **日本語**

> τ-分位点を直接 fit する手法。中央値・四分位点・極値域の予測に。
> `Model.Quantile` モジュール。
>
> 関連: [06-gam.ja.md](06-gam.ja.md) (加法モデル) / [06-randomforest.ja.md](06-randomforest.ja.md)


## 何のために使うか

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

## 損失関数 — Pinball loss

通常の OLS は二乗誤差 Σ r²。Quantile regression は **非対称な絶対誤差**:

$$ \rho_\tau(u) = u\,(\tau - \mathbb{1}[u < 0]) = \begin{cases} \tau u   & u \ge 0 \\ (\tau - 1) u & u < 0 \end{cases} $$

これを Pinball loss / Check loss と呼ぶ。τ=0.5 で標準的な絶対誤差 |u|/2 に
一致 → 中央値推定。τ=0.9 だと「正残差は 0.9 倍重み、負残差は 0.1 倍重み」と
いう非対称損失で **上側分位点** を推定。

## アルゴリズム — MM-IRLS (Hunter-Lange)

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

## 評価指標 — Pseudo R¹_τ (Koenker-Machado 1999)

通常の R² は平均ベースなので分位点回帰には不適切。代わりに:

$$ R^1_\tau = 1 - \frac{V_\tau(\text{model})}{V_\tau(\text{intercept-only})} $$

ここで $V_\tau(m) = \Sigma \rho_\tau(r_i)$。値域は (-∞, 1]、0 は intercept-only
と同等、1 が完全 fit。

## ライブラリ API

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

## 使用例

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

## CLI

```bash
# 中央値回帰
hanalyze quantile data.csv x y --tau 0.5 --report

# 複数分位を 1 枚のチャートで重ね描き (10%/50%/90%)
hanalyze quantile data.csv x y --taus 0.1,0.5,0.9 --report
```

`--taus` 指定時のレポートに **Multiple quantile fits** セクションが
追加され、観測散布 + 各分位線が tableau10 カラーで色分け表示される。

## Reportable による可視化

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

## 注意点

- **MM-IRLS は遅い**: max 100 iter まで反復。データ規模 N が大きいと
  WLS の逆行列計算がボトルネック (O(p³) per iteration)。
- **τ が 0 / 1 に近いと不安定**: τ=0.01 / 0.99 だと ε による平滑化の影響が
  大きく、推定がブレやすい。
- **多変量化**: 上記 API は単純な p × β の線形 quantile。非線形版が必要なら
  spline 基底を別途張って fitQuantile に渡す。

---


---

## 関連リンク

- 線形回帰: [01-lm.ja.md](01-lm.ja.md)
- 正則化: [04-regularized.ja.md](04-regularized.ja.md)
- 理論背景: [theory-regression-extensions.ja.md](theory-regression-extensions.ja.md)
