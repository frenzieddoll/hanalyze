# 空間充填計画 (LHS / Maximin LHS / Halton)

対象 module: `Hanalyze.Design.SpaceFilling` (`hanalyze-design`)

シミュレーション (コンピュータ実験) や surrogate モデルの学習点を配置する
ための DoE。 実験誤差が無く応答が滑らかとは限らない相手なので、
要因計画のように端点へ寄せるより **入力空間を均等に覆う**方が有利。

| 関数 | 方式 | 決定性 |
|---|---|---|
| `latinHypercube` | 層化ランダム (各次元のセルを 1 度ずつ) | 乱数依存 |
| `latinHypercubeMaximin` | LHS を初期解に点間最小距離を最大化 | 乱数依存 |
| `haltonDesign` | Halton 低偏差列 | **決定的** |

出力は全て `[0, 1)^d` 上の点。 実際の因子範囲へのスケーリングは
利用側で行う (§3)。

## 0. API

```haskell
data SpaceFillingDesign = SpaceFillingDesign
  { sfdMatrix  :: !(Matrix Double)  -- n × d、 [0,1)^d 上の点
  , sfdNPoints :: !Int
  , sfdNDims   :: !Int
  , sfdMinDist :: !Double           -- 点間最小ユークリッド距離 (大きいほど良い)
  , sfdMethod  :: !Text             -- "LHS" / "MaximinLHS" / "Halton"
  }

latinHypercube        :: Int -> Int        -> MWC.GenIO -> IO SpaceFillingDesign
latinHypercubeMaximin :: Int -> Int -> Int -> MWC.GenIO -> IO SpaceFillingDesign
                      -- n      d     nTries
haltonDesign          :: Int -> Int -> SpaceFillingDesign      -- 純粋関数

designMinDistance :: Matrix Double -> Double
```

`latinHypercube*` は `MWC.GenIO` を取る `IO` アクション、
`haltonDesign` は乱数を使わない純粋関数。

## 1. Latin Hypercube

8 点 × 2 次元。 再現したいので `MWC.create` (固定 seed) を使う。

```haskell
import qualified Numeric.LinearAlgebra as LA
import qualified System.Random.MWC     as MWC
import Hanalyze.Design.SpaceFilling

main :: IO ()
main = do
  gen <- MWC.create
  lhs <- latinHypercube 8 2 gen
  print (sfdNPoints lhs, sfdNDims lhs, sfdMethod lhs)
  print (sfdMinDist lhs)
  mapM_ print (LA.toLists (sfdMatrix lhs))
```

実行結果:

```
(8,2,"LHS")
0.15917993786380238
[3.1012953603702514e-3,0.7336283364127267]
[0.5283808232035737,0.3147626793809132]
[0.21760800849316259,0.10765495638623752]
[0.2699204433479854,0.6097739846510616]
[0.6812301915661529,0.8840376332763719]
[0.8901926889521693,0.20508481375541604]
[0.4619085958091119,0.45939910495740116]
[0.8153674317579357,0.7837126085149456]
```

各列を `[0, 1/8), [1/8, 2/8), …` の 8 セルに切り、 **どの列もセルを 1 度ずつ
使う**のが LHS の性質。 列ごとに周辺分布が均等になる一方、 2 次元で見ると
偶然近づく点が残る (ここでは最小距離 0.159)。

## 2. Maximin LHS で距離を稼ぐ

層化を保ったまま、 列内の値を入れ替えて点間最小距離を改善する局所探索。

```haskell
  gen2 <- MWC.create
  mx <- latinHypercubeMaximin 8 2 1000 gen2
  print (sfdMethod mx, sfdMinDist mx)
```

```
("MaximinLHS",0.26779098411089647)
```

同じ 8 点 2 次元で最小距離が **0.159 → 0.268 (約 1.7 倍)**。 `nTries` は
swap 試行の総回数で、 1000 程度から実用的な改善が得られる (n, d による)。
点数が多いほど試行を増やす必要がある。

## 3. Halton (決定的)

```haskell
  let hal = haltonDesign 8 2
  print (sfdMethod hal, sfdMinDist hal)
  mapM_ print (LA.toLists (sfdMatrix hal))
```

```
("Halton",0.167244369149893)
[0.5,0.3333333333333333]
[0.25,0.6666666666666666]
[0.75,0.1111111111111111]
[0.125,0.4444444444444444]
[0.625,0.7777777777777777]
[0.375,0.2222222222222222]
[0.875,0.5555555555555556]
[6.25e-2,0.8888888888888888]
```

第 1 列が基数 2、 第 2 列が基数 3 の van der Corput 列。 **同じ `(n, d)` なら
常に同じ点集合**なので、 seed 管理なしで再現でき、 点を後から追加しても
既存点が動かない (逐次追加に向く)。 ただし次元が高くなると低次基数の
相関が出るため、 目安として d ≲ 10。

## 4. 因子範囲へのスケーリングと品質確認

```haskell
  -- 温度 180〜220 ℃、 圧力 10〜30 MPa へ写す
  let lo = LA.fromList [180, 10]
      hi = LA.fromList [220, 30]
      scaled = LA.fromRows
        [ lo + (hi - lo) * row | row <- LA.toRows (sfdMatrix lhs) ]

  -- 任意の行列の点間最小距離を測る
  print (designMinDistance (LA.fromLists [[0, 0], [3, 4]]))
```

```
5.0
```

`designMinDistance` は行数 < 2 のとき 0 を返す。 `sfdMinDist` は生成直後の
`[0,1)^d` 上の値なので、 **スケーリング後の距離を比べたいときは
`designMinDistance` を再計算**する。 方式の比較 (LHS vs Maximin vs Halton) は
同じ `(n, d)` で最小距離を並べるのが手軽な指標。

## 5. 使い分け

| やりたいこと | 使うもの |
|---|---|
| 手早く均等に散らす | `latinHypercube` (本 doc) |
| 点間距離を最大化したい (GP の学習点等) | `latinHypercubeMaximin` (本 doc) |
| seed 無しで再現・後から点を追加 | `haltonDesign` (本 doc) |
| 実験誤差のある実機実験 | [要因計画 / RSM](01-doe.ja.md) |
| 少数因子から効く因子を選別 | [DSD / スクリーニング](01-doe.ja.md) |
| 逐次に次の 1 点を賢く選ぶ | [`Optim.BayesOpt`](../optim/01-singleobj.ja.md) |
| 準乱数列そのもの (`haltonSamples` 等) | `Hanalyze.Stat.QuasiRandom` (core 層。 使用例は [semiconductor-design-workflow.md](../manual/semiconductor-design-workflow.ja.md) §5.2) |

## 関連

- package: [hanalyze-design](../../hanalyze-design/README.ja.md)
- DoE 入口: [01-doe.ja.md](01-doe.ja.md)
- 半導体プロセスでの適用事例: [manual/semiconductor-design-workflow.md](../manual/semiconductor-design-workflow.ja.md) §4.3
