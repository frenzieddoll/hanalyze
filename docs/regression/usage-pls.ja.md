# PLS 回帰 (Partial Least Squares)

対象 module: `Hanalyze.Model.PLS` (`hanalyze-models`)

説明変数が多く互いに強く相関している状況 (分光分析・材料設計・工程パラメータ)
で使う低ランク回帰。 PCA が応答を無視して X の分散を最大化するのに対し、
**PLS は X と応答 Y の共分散を最大化**する潜在成分を取る。
chemometrics の標準手法で、 予測と変数重要度 (VIP) を 1 モデルで得られる。

本 doc は **行列レベルの低レベル API** を扱う。 dataframe から
`df |-> plsOf ...` で使う df レベル API と score / loading / VIP 図は
[api-guide/04-multivariate.md](../api-guide/04-multivariate.ja.md) §PLS、
多出力モデル群の中での位置づけは
[05-multivariate.ja.md](05-multivariate.ja.md) §3 を参照。

## 0. API

```haskell
data PLSConfig = PLSConfig
  { plsN_Components :: !Int
  , plsAlgorithm    :: !PLSAlgorithm   -- NIPALS (既定) | SIMPLS (将来)
  , plsScale        :: !Bool           -- True で X, Y を列ごとに標準化
  , plsTol          :: !Double         -- NIPALS 収束許容誤差
  , plsMaxIter      :: !Int            -- NIPALS 最大反復
  }

defaultPLS :: PLSConfig

fitPLS  :: PLSConfig -> Matrix Double -> Matrix Double -> Either Text PLSFit
fitPLS1 :: PLSConfig -> Matrix Double -> Vector Double -> Either Text PLSFit

predictPLS  :: PLSFit -> Matrix Double -> Matrix Double
predictPLS1 :: PLSFit -> Matrix Double -> Vector Double
```

`fitPLS` は多出力 (Y が `n × q` 行列)、 `fitPLS1` は単一応答 (`Vector`) 用の
薄いラッパ。 いずれも失敗を `Left Text` で返す (成分数が変数数を超える等)。
`predictPLS1` は `fitPLS1` で建てた fit に対応する 1 列版。

## 1. 使い方

x1 と x2 が応答に効き、 x3 / x4 はノイズという 4 変数のデータで 2 成分の
PLS を建てる。

```haskell
import qualified Numeric.LinearAlgebra as LA
import Hanalyze.Model.PLS

main :: IO ()
main = do
  let x = LA.fromLists
            [ [ 1.0,  2.1, 0.5, 3.3 ]
            , [ 2.0,  4.1, 0.6, 3.1 ]
            , [ 3.0,  5.9, 0.4, 3.4 ]
            , [ 4.0,  8.2, 0.7, 3.2 ]
            , [ 5.0,  9.8, 0.5, 3.3 ]
            , [ 6.0, 12.1, 0.6, 3.1 ]
            , [ 7.0, 14.0, 0.4, 3.5 ]
            , [ 8.0, 16.2, 0.7, 3.2 ] ]
      y   = LA.fromList [ 3.0, 5.1, 7.2, 9.4, 11.1, 13.3, 15.2, 17.4 ]
      cfg = defaultPLS { plsN_Components = 2 }
  case fitPLS1 cfg x y of
    Left err -> putStrLn ("error: " ++ show err)
    Right f  -> do
      print (LA.toList (plsVIP f))
      print (LA.toList (plsR2X f), LA.toList (plsR2Y f))
      print (LA.toList (predictPLS1 f x))
```

実行結果:

```
[1.3941438613552546,1.394105087799994,0.3038126115482986,0.14328920084861047]
([0.5161208250491462,0.4462476355758236],[0.9959755615498295,2.6134823081460905e-3])
[3.21017141904792,5.0132738248537185,7.04305021855597,9.578399655271525,
 11.028475763850459,12.980625583912904,15.300252120176811,17.54575141433071]
```

読み方:

- **VIP** — x1 / x2 が 1.39、 x3 / x4 が 0.30 / 0.14。 慣習的に **VIP > 1 が
  「効いている変数」**の目安なので、 ノイズ 2 変数を正しく落とせている
- **`plsR2Y`** — 第 1 成分だけで Y の分散の 99.6% を説明。 第 2 成分の寄与は
  0.26% しかないので、 実質 1 成分で足りるデータだと分かる
- **`plsR2X`** — 各成分が X 側の分散をどれだけ拾ったか (0.52 / 0.45)。 Y の
  説明率と別に見ることで「X はよく再現するが Y に効かない成分」 を見つけられる

## 2. 結果型 `PLSFit`

| フィールド | 内容 |
|---|---|
| `plsScoresT` | T (`n × K`) — X の潜在スコア。 score plot の座標 |
| `plsLoadingsP` / `plsLoadingsQ` | P (`p × K`) / Q (`q × K`) — X / Y の loading |
| `plsWeightsW` | W (`p × K`) — X の weight |
| `plsCoef` | β (`p × q`) 回帰係数 (**元スケール**)。 `Ŷ = (X − X̄) · β + Ȳ` |
| `plsXMean` / `plsXStd` | X の列平均 / 列標準偏差 (`plsScale = False` なら std は 1) |
| `plsYMean` / `plsYStd` | Y 側の同様の量 |
| `plsR2X` / `plsR2Y` | 各成分の X / Y 説明分散率 (長さ K) |
| `plsVIP` | 変数重要度 (Variable Importance in Projection、 長さ p) |
| `plsConfig` | fit に使った `PLSConfig` |

`plsCoef` は元スケールの係数なので、 `predictPLS` を通さず自分で
`(X − plsXMean) · plsCoef + plsYMean` を計算しても同じ値になる。

## 3. 成分数を CV で選ぶ

成分数 K はハイパーパラメータ。 k-fold CV で MSE を比べて選ぶ:

```haskell
import qualified Data.Vector.Unboxed as VU
import qualified System.Random.MWC   as MWC

selectPLSComponentsCV
  :: Int                 -- k-fold の k
  -> Int                 -- maxK (探索する成分数の上限)
  -> Matrix Double       -- X
  -> Matrix Double       -- Y (1 応答なら LA.asColumn で列にする)
  -> MWC.GenIO
  -> IO PLSLambdaSelection
```

```haskell
  gen <- MWC.initialize (VU.fromList [42])       -- 再現したいので seed 固定
  sel <- selectPLSComponentsCV 4 3 x (LA.asColumn y) gen
  print (plsBestK sel, plsOneSeK sel)
  print (plsCVMSEs sel)
```

実行結果:

```
(3,3)
[1.1296833242752071,0.10686514418350236,2.501483388975259e-2]
```

| フィールド | 内容 |
|---|---|
| `plsBestK` | CV MSE 最小の成分数 |
| `plsOneSeK` | **1-SE ルール**による成分数 (最小 MSE + 1 標準誤差以内で最小の K) |
| `plsCVMSEs` | K = 1 .. maxK の CV MSE (長さ maxK) |
| `plsCVSDs` | 同じ順の標準偏差 |

fold の割り方が乱数依存なので、 **`MWC.GenIO` を取り `IO` を返す**。 結果を
doc やテストに載せるときは `MWC.initialize` で seed を固定する
(`MWC.createSystemRandom` は毎回変わる)。 単一応答でも Y は行列で渡すので
`LA.asColumn` が要る点に注意。

## 4. 使い分け

| やりたいこと | 使うもの |
|---|---|
| 応答に効く方向で低ランク化して予測する | `fitPLS` / `fitPLS1` (本 doc) |
| 応答を無視して X の構造だけ見る | [`Model.PCA`](../api-guide/04-multivariate.ja.md) |
| dataframe から一発で建てて図にする | `df \|-> plsOf cfg xcols ycols` ([api-guide/04](../api-guide/04-multivariate.ja.md) §PLS) |
| 多重共線性を罰則で抑える | [Ridge / Lasso / Elastic Net](04-regularized.ja.md) |
| 多出力の線形回帰 (低ランク化なし) | [05-multivariate.ja.md](05-multivariate.ja.md) §1 |

## 関連

- package: [hanalyze-models](../../hanalyze-models/README.ja.md)
- df レベル API と診断図: [api-guide/04-multivariate.md](../api-guide/04-multivariate.ja.md) §PLS
- 多出力モデル群の中での位置: [05-multivariate.ja.md](05-multivariate.ja.md) §3
- 判別分析 (分類側の多変量手法): [usage-discriminant.ja.md](usage-discriminant.ja.md)
