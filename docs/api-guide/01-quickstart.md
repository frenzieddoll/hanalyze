# クイックスタート ─ 最短で fit → 描画

> [📚 索引](README.md) ｜ **01 quickstart** ｜ [02 regression](02-regression.md) ｜ [03 bayesian-hbm](03-bayesian-hbm.md) ｜ [04 multivariate](04-multivariate.md) ｜ [05 ml](05-ml.md) ｜ [06 timeseries](06-timeseries.md) ｜ [07 survival](07-survival.md) ｜ [08 causal](08-causal.md) ｜ [09 doe](09-doe.md) ｜ [10 stat](10-stat.md) ｜ [11 data](11-data.md) ｜ [12 plot](12-plot.md)

hanalyze で **データを当てはめて 1 枚出す**ための最短経路を示す。 各モデルの
シグネチャと最小例は [02 regression](02-regression.md) 以降が辞書になる。 fit API の
全体像は [11 data](11-data.md) と [`docs/io/04-fit-api.md`](../io/04-fit-api.md)、 描画への
変換は [12 plot](12-plot.md) を参照。

> **黄金律 (これだけ先に覚える)**
>
> 1. **当てはめ** = `df |-> spec`。 データ源 `df` と「何を当てるか」 の spec
>    (`lm "x" "y"` 等) を `|->` で繋ぐと fit 済みモデルが返る。
> 2. **描画** = `toPlot model` でモデルを layer 化し、 `df |>> (layer (scatter "x" "y") <> toPlot model)`
>    でデータと重畳して `saveSVGBound` で保存する。

このページの構成:
**[30 秒で 1 枚 (LM)](#lm-30s)** ｜ **[データ源](#data-source)** ｜ **[低レベル (行列 API)](#low-level)**

---

## 30 秒で 1 枚 (線形回帰) {#lm-30s}

万能動詞 `df |-> lm` で当てはめ、 `toPlot` で散布図に回帰線 + 95% CI 帯を重ねる。

```haskell
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Vector              as V
import           Hanalyze.Plot     (lm, (|->), toPlot)
import           Hgg.Plot.Spec        (ColData (..), layer, scatter)
import           Hgg.Plot.Frame       ((|>>))
import           Hgg.Plot.Backend.SVG (saveSVGBound)

main :: IO ()
main = do
  let df  = [ ("x", NumData (V.fromList [1,2,3,4,5,6,7,8]))
            , ("y", NumData (V.fromList [2.1,3.9,6.2,7.8,10.3,11.7,14.1,16.0])) ]
      fit = df |-> lm "x" "y"        -- LMModel: β, ŷ, residuals, R²
  saveSVGBound "lm.svg"             -- 散布図 + OLS 直線 + 95% CI 帯
    $ df |>> layer (scatter "x" "y") <> toPlot fit
```

![散布図 + 回帰線 + CI 帯](../images/lm-scatter-ci.svg)

`df |-> lm "x" "y"` の `lm` は **9 つの spec 動詞**の 1 つ。 同じ形で `glm` (GLM) ・
`spline` ・ `robust` ・ `quantile` ・ Formula 版 `lmF` / `glmF` / `glmmF` ・ ベイズ `hbm`
が使える ([README 早見表](README.md#演算子抽出子-早見表))。

---

## データ源 ─ 何を `df` に渡せるか {#data-source}

`|->` / `|>>` の左辺は **`ColumnSource`** なら何でもよい。 列名で引ければ同じ書き方で動く。

| データ源 | 例 |
|---|---|
| assoc list | `[("x", NumData (V.fromList xs)), ("y", NumData (V.fromList ys))]` |
| Hackage `DataFrame` | CSV ローダ (`loadAuto` 等・[11 data](11-data.md)) が返す `DataFrame` をそのまま |
| `Map Text ColData` | `Map.fromList [("x", NumData …), …]` |

`ColData` の構成子は数値列 `NumData (V.Vector Double)` と 文字列 (カテゴリ) 列
`TxtData (V.Vector Text)` の 2 つ ([`Hgg.Plot.Spec`](../../src/hanalyze/Analyze/Plot.hs))。 データを持たない図 (HBM の forest 等)
には空源 `noDf = [] :: [(Text, ColData)]` を渡す ([03 bayesian-hbm](03-bayesian-hbm.md))。

---

## 低レベル (行列 API) {#low-level}

既に `hmatrix` の `Vector` / `Matrix` を持っていて、 数値の `FitResult` だけが欲しい場合は
モデルモジュールの行列 API を直接呼ぶ (描画は伴わない)。

```haskell
import Hanalyze.Model.LM   (fitLMVec, designMatrix)
import Hanalyze.Model.Core (coefficientsV, rSquared1)

let fit  = fitLMVec (designMatrix xs) ys   -- FitResult: β, ŷ, residuals, R²
    beta = coefficientsV fit
    r2   = rSquared1 fit
```

高レベル `df |-> lm` は内部でこの行列 API を呼び、 さらに描画に必要な設計行列を保持した
`LMModel` を返す (`toPlot` が CI 帯を描けるのはこのため)。 各モデルページでも
**高レベルを主・低レベルを `**低レベル**` ラベルで併記**する。

→ fit API の全体像: [11 data](11-data.md) / [`docs/io/04-fit-api.md`](../io/04-fit-api.md)
→ 描画への変換と抽出子一覧: [12 plot](12-plot.md)
