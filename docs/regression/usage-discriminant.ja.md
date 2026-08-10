# 判別分析 (LDA / QDA)

対象 module: `Hanalyze.Model.Discriminant` (`hanalyze-models`)

連続説明変数から複数クラスを判別する古典的手法。 クラスごとの正規分布を
仮定し、 `log p(class) + log f(x | class)` (log-posterior) を比べて割り当てる。

| 手法 | 共分散の仮定 | 決定境界 |
|---|---|---|
| **LDA** | 全クラスで共通 (pooled) | 線形 |
| **QDA** | クラスごとに別 | 二次 |

クラス数が多い / 各クラスの標本が少ないときは、 推定する共分散が 1 個で済む
LDA が安定する。 クラスごとに広がりが明らかに違うなら QDA。

本 doc は **行列レベルの低レベル API** を扱う。 dataframe から
`df |-> ldaOf cols clsCol` で使う df レベル API と決定境界図は
[api-guide/04-multivariate.md](../api-guide/04-multivariate.ja.md) §判別分析 (LDA)
を参照。 **QDA は df レベル API を持たない**ので、 二次境界が必要なときは
本 doc の `fitQDA` を直接使う。

## 0. API

```haskell
data DiscriminantMethod = LDA | QDA

fitLDA :: Matrix Double     -- X (n × p)
       -> V.Vector Int      -- y (n) 整数クラスラベル
       -> Either Text DiscriminantFit

fitQDA :: Matrix Double -> V.Vector Int -> Either Text DiscriminantFit

predictDiscriminant
  :: DiscriminantFit
  -> Matrix Double                       -- X_new (m × p)
  -> (V.Vector Int, Matrix Double)       -- (予測ラベル, posterior 行列 m × K)
```

ラベルは `Data.Vector` の **`Vector Int`** (`hmatrix` の `Vector` ではない)。
X は `hmatrix` の `Matrix Double`。

> `predictDiscriminant` の第 2 返り値は **行ごとに正規化済の posterior 確率**
> (各行の合計 = 1) であり、 log-posterior そのものではない
> (`Discriminant.hs:129-136` で `exp(logp − max)` を正規化している)。

## 1. 使い方

2 クラス・2 変数のデータで LDA を建て、 新規 3 点を予測する。

```haskell
import qualified Data.Vector           as V
import qualified Numeric.LinearAlgebra as LA
import Hanalyze.Model.Discriminant

main :: IO ()
main = do
  let x = LA.fromLists
            [ [ 1.0, 1.2 ], [ 1.4, 0.9 ], [ 0.8, 1.1 ], [ 1.1, 1.3 ]
            , [ 4.0, 4.2 ], [ 4.4, 3.9 ], [ 3.8, 4.1 ], [ 4.1, 4.3 ] ]
      y    = V.fromList [ 0, 0, 0, 0, 1, 1, 1, 1 ]
      xNew = LA.fromLists [ [ 1.2, 1.0 ], [ 4.2, 4.0 ], [ 2.6, 2.6 ] ]
  case fitLDA x y of
    Left err -> putStrLn ("error: " ++ show err)
    Right f  -> do
      let (preds, post) = predictDiscriminant f xNew
      print (V.toList preds)
      print (LA.toLists post)
      print (LA.toList (dfPriors f), LA.toList (dfClasses f))
      print (LA.toLists (dfMeans f))
```

実行結果:

```
[0,1,0]
[[1.0,1.0542564890450664e-207],[1.8579928589071955e-199,1.0],
 [0.8697481923940898,0.13025180760591012]]
([0.5,0.5],[0.0,1.0])
[[1.0750000000000002,1.125],[4.074999999999999,4.125]]
```

読み方:

- 1 点目と 2 点目は各クラスの中心近傍なので posterior がほぼ 1 / 0。 群内の
  ばらつきが小さいデータでは posterior が極端になる (指数の桁が飛ぶ)
- 3 点目 `(2.6, 2.6)` は 2 クラス平均の中点 `(2.575, 2.625)` のすぐ近くで、
  posterior は 0.87 / 0.13。 **境界付近だけ確率が中間になる**ので、
  「どちらとも言い切れない標本」 の検出に posterior 列を使える
- `dfClasses` はソート済のクラスラベル (`Int` を `Double` で保持)、
  `dfPriors` は標本比から求めた事前確率。 posterior 行列の列順は
  `dfClasses` の順

## 2. QDA に切り替える

呼ぶ関数を変えるだけ。 保持される共分散の形が変わる。

```haskell
  case fitQDA x y of
    Left err -> putStrLn ("error: " ++ show err)
    Right f  -> do
      let (preds, _) = predictDiscriminant f xNew
      print (V.toList preds, length (dfCovariances f))
```

```
([0,1,0],2)
```

## 3. 結果型 `DiscriminantFit`

| フィールド | LDA | QDA |
|---|---|---|
| `dfMeans` | `K × p` 各クラスの平均ベクトル | 同じ |
| `dfCovariance` | pooled covariance (`p × p`) | **空** (使わない) |
| `dfCovariances` | **空** | クラス別 covariance (K 個) |
| `dfPriors` | クラス事前確率 (長さ K、 合計 1) | 同じ |
| `dfClasses` | ソート済クラスラベル (長さ K) | 同じ |
| `dfMethod` | `LDA` | `QDA` |

どちらの手法かによって空になるフィールドが入れ替わるので、
`dfMethod` を見てから対応するフィールドを読む。

数値安定化のため、 内部では Cholesky 分解経由で log-determinant と
Mahalanobis 距離を計算している。 クラスの標本数が変数数 `p` を下回ると
共分散が特異になり `Left` が返る (QDA はクラスごとに推定するため
LDA より先に破綻する)。

## 4. 使い分け

| やりたいこと | 使うもの |
|---|---|
| 線形境界で判別・共分散を共通と仮定 | `fitLDA` (本 doc) |
| クラスごとに広がりが違う → 二次境界 | `fitQDA` (本 doc) |
| dataframe から一発 + 決定境界図 | `df \|-> ldaOf cols clsCol` ([api-guide/04](../api-guide/04-multivariate.ja.md)) |
| 説明変数が多く相関が強い (回帰側) | [PLS](usage-pls.ja.md) |
| 非線形境界を機械学習で | [Random forest](06-randomforest.ja.md) / [決定木](08-decisiontree.ja.md) |
| 判別結果の評価指標 | [`Stat.ClassMetrics`](../stat/03-classmetrics.ja.md) |

## 関連

- package: [hanalyze-models](../../hanalyze-models/README.ja.md)
- df レベル API と決定境界図: [api-guide/04-multivariate.md](../api-guide/04-multivariate.ja.md) §判別分析 (LDA)
- PLS 回帰 (回帰側の多変量手法): [usage-pls.ja.md](usage-pls.ja.md)
- 分類指標: [stat/03-classmetrics.ja.md](../stat/03-classmetrics.ja.md)
