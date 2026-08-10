# 逐次的応答曲面法 (Sequential RSM)

対象 module: `Hanalyze.Design.Sequential` (`hanalyze-design`)

RSM を 1 回で終わらせず、 **「小さい計画 → fit → 最急上昇方向へ移動 →
新しい中心で次の計画」** を繰り返して最適条件に近づけるワークフローを支える
helper。 探索範囲が広く最初から二次モデルを張れないときの定石。

```
初期 2^k 計画 ──fit──> 1 次係数 ──steepestAscent──> 移動先の候補点列
                                                          │
                          次の CCD ←──sequentialCCD──── 新しい中心
```

数値の重い部分 (二次モデルの fit・極値の解析解) は
[`Design.RSM`](01-doe.ja.md) 側にあり、 本 module は
**path 生成と次計画の配置**だけを担う。

## 0. API

```haskell
data SteepestAscentResult = SteepestAscentResult
  { sarDirection  :: !(Vector Double)  -- 単位ベクトル化された方向 (長さ k)
  , sarStepPoints :: ![[Double]]       -- 試行点列 (長さ nSteps + 1、 先頭 = center)
  , sarMaximize   :: !Bool             -- True = ascent、 False = descent
  }

steepestAscent
  :: Bool -> [Double] -> [Double] -> Double -> Int -> SteepestAscentResult
  --  最大化?   center     1 次係数 b   step size  試行点数

steepestAscentFromQuad
  :: Bool -> [Double] -> RSM.QuadFit -> Double -> Int -> SteepestAscentResult

data SequentialCCDResult = SequentialCCDResult
  { sccdCenter :: ![Double]      -- 新しい design center (原座標)
  , sccdSpan   :: !Double        -- 片側スパン (coded ±1 が原座標で center ± span)
  , sccdCoded  :: ![[Double]]    -- coded units (−α..+α) の design
  , sccdReal   :: ![[Double]]    -- 原座標の design (= center + span · coded)
  }

sequentialCCD
  :: [Double] -> Double -> Int -> RSM.CCDType -> Int -> SequentialCCDResult
  --  新中心      span     因子数 k  CCD 種別      中心点の反復数
```

`steepestAscentFromQuad` は fit 済の `QuadFit` から 1 次係数を抜き出す版で、
係数を自分で取り出す手間が省ける。

## 1. fit から path を引く

温度 200 ℃ / 圧力 30 を中心に 2^2 + 中心点で実験し、 1 次モデルを fit して
最急上昇方向へ 4 歩進む。

```haskell
import qualified Numeric.LinearAlgebra as LA
import qualified Hanalyze.Design.RSM        as RSM
import qualified Hanalyze.Design.Sequential as Seq

main :: IO ()
main = do
  let xs = [[-1,-1], [1,-1], [-1,1], [1,1], [0,0]]           -- coded
      ys = [ 40 + 3*x1 + 4*x2 | [x1, x2] <- xs ]              -- 観測応答
      qf = RSM.fitQuadratic xs ys
      sa = Seq.steepestAscentFromQuad True [200, 30] qf 5.0 4
  print (LA.toList (Seq.sarDirection sa))
  mapM_ print (Seq.sarStepPoints sa)
```

実行結果:

```
[0.5999999999999999,0.7999999999999999]
[200.0,30.0]
[203.0,34.0]
[206.0,38.0]
[209.0,42.0]
[212.0,46.0]
```

- 方向は `b / |b|` = `(3,4)/5` = `(0.6, 0.8)`。 **係数の大きい因子ほど大きく
  動く**のが最急上昇の性質
- 点列は `center + i · step · direction` (step = 5.0)。 先頭が中心なので
  長さは `nSteps + 1 = 5`
- **step size は原座標のスケール**で解釈される。 因子の単位が大きく違う
  (温度 ℃ と圧力 MPa 等) 場合は coded 座標で path を引き、 後で戻す方が安全

実務ではこの点列を上から順に 1 点ずつ実験し、 応答が改善しなくなった
ところで打ち切る。

## 2. 新しい中心で次の CCD を置く

```haskell
  let center = last (Seq.sarStepPoints sa)
      ccd    = Seq.sequentialCCD center 5.0 2 RSM.CCF 2
  print (Seq.sccdCenter ccd, Seq.sccdSpan ccd)
  print (length (Seq.sccdReal ccd))
  mapM_ print (Seq.sccdReal ccd)
```

```
([212.0,46.0],5.0)
10
[207.0,41.0]
[207.0,51.0]
[217.0,41.0]
[217.0,51.0]
[207.0,46.0]
[217.0,46.0]
[212.0,41.0]
[212.0,51.0]
[212.0,46.0]
[212.0,46.0]
```

`RSM.CCF` (face-centered、 α = 1) で k = 2・中心点 2 反復なので
**4 (要因) + 4 (軸) + 2 (中心) = 10 run**。 `sccdReal` は
`center + span · coded` を計算済なので、 そのまま実験計画表として使える。
`sccdCoded` を見れば解析用の coded 行列が得られる。

`RSM.CCDType` の選択:

| 種別 | α | 使う場面 |
|---|---|---|
| `RSM.CCC α` | 指定値 (回転可能性なら `(2^k)^(1/4)`) | 因子を範囲外まで振れる |
| `RSM.CCF` | 1 | **範囲の外に出せない** (装置限界・安全) |
| `RSM.CCI α` | 1 (要因部を `1/α` 倍に縮小) | 範囲が絶対上限のとき |

## 3. 反復の止めどころ

1 次係数が小さくなり二次の曲率が見えてきたら、 path を引くのをやめて
CCD を fit し `RSM.optimumPoint` で極値を解く。 曲率の判定・二次モデルの
解析は [01-doe.ja.md](01-doe.ja.md) の RSM 節を参照。

## 4. 使い分け

| やりたいこと | 使うもの |
|---|---|
| 広い範囲から最適点へ段階的に寄せる | 本 doc (`steepestAscent` + `sequentialCCD`) |
| 最初から二次モデルを張って極値を解く | [RSM (CCD / Box-Behnken)](01-doe.ja.md) |
| 既存の実験に点を追加して精度を上げる | [D-optimal augment](usage-doptimal-augment.ja.md) |
| 複数応答を同時に最適化 | [`Design.MultiRSM`](01-doe.ja.md) / Desirability |
| モデルを介さず黒箱最適化する | [`Optim.BayesOpt`](../optim/01-singleobj.ja.md) |

## 関連

- package: [hanalyze-design](../../hanalyze-design/README.ja.md)
- RSM 本体: [01-doe.ja.md](01-doe.ja.md)
- 実験の追加: [usage-doptimal-augment.ja.md](usage-doptimal-augment.ja.md)
