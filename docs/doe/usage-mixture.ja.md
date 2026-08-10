# 配合計画 (Mixture Design)

対象 module: `Hanalyze.Design.Mixture` (`hanalyze-design`)

成分比の合計が常に 1 になる制約下の DoE。 材料・化学プロセスで
「A を増やすなら B か C を減らすしかない」 状況を扱う。 各実験点は
`x_i ≥ 0` かつ `Σ x_i = 1` を満たすため、 通常の要因計画のように
各因子を独立に動かせない。

| 方式 | 生成規則 | 点数 |
|---|---|---|
| `SimplexLattice d` | 各成分が `{0, 1/d, …, 1}` から値を取り合計 1 | `C(m+d−1, d)` |
| `SimplexCentroid` | 任意 k 成分を均等に `1/k`、 他は 0 (k = 1..m) | `2^m − 1` |

## 0. API

```haskell
data MixtureDesignType
  = SimplexLattice !Int   -- 次数 d
  | SimplexCentroid

data MixtureResult = MixtureResult
  { mdMatrix      :: !(Matrix Double)   -- nRuns × m、 各行の合計 = 1
  , mdNComponents :: !Int               -- m (成分数)
  , mdNRuns       :: !Int               -- 実験数
  , mdType        :: !MixtureDesignType
  }

mixtureDesign :: MixtureDesignType -> Int -> Either Text MixtureResult
```

第 2 引数が成分数 m。 `Matrix` は `hmatrix`。

## 1. Simplex Lattice

3 成分・次数 2 (= 各成分が 0 / 0.5 / 1 を取る) の格子。

```haskell
import qualified Numeric.LinearAlgebra as LA
import Hanalyze.Design.Mixture

main :: IO ()
main = do
  case mixtureDesign (SimplexLattice 2) 3 of
    Left err -> putStrLn ("error: " ++ show err)
    Right r  -> do
      print (mdNComponents r, mdNRuns r)
      mapM_ print (LA.toLists (mdMatrix r))
```

実行結果:

```
(3,6)
[0.0,0.0,1.0]
[0.0,0.5,0.5]
[0.0,1.0,0.0]
[0.5,0.0,0.5]
[0.5,0.5,0.0]
[1.0,0.0,0.0]
```

点数は `C(3+2−1, 2) = C(4,2) = 6`。 内容は **3 頂点 (純成分) + 3 辺の中点
(2 成分を半々)** で、 三角座標 (ternary plot) の頂点と辺を埋める配置になる。
次数 d を上げると辺上の刻みが細かくなり、 内部点も現れる。

## 2. Simplex Centroid

```haskell
  case mixtureDesign SimplexCentroid 3 of
    Left err -> putStrLn ("error: " ++ show err)
    Right r  -> do
      print (mdNRuns r)
      mapM_ print (LA.toLists (mdMatrix r))
```

```
7
[1.0,0.0,0.0]
[0.0,1.0,0.0]
[0.0,0.0,1.0]
[0.5,0.5,0.0]
[0.5,0.0,0.5]
[0.0,0.5,0.5]
[0.3333333333333333,0.3333333333333333,0.3333333333333333]
```

`2^3 − 1 = 7` 点 = 頂点 3 + 辺中点 3 + **全体重心 1**。 SimplexLattice 2 との
差は最後の重心点で、 **3 成分すべてを混ぜた条件を必ず 1 点含む**のが
centroid 方式の利点。 3 成分の相乗効果 (Scheffé モデルの `x1·x2·x3` 項) を
見たいならこちら。

## 3. 実験値へのスケーリング

出力は比率 (合計 1) なので、 総量が決まっているなら掛けるだけでよい:

```haskell
  -- 総量 500 g の配合に変換
  let grams = LA.scale 500 (mdMatrix r)
```

下限 / 上限のある制約付き配合 (例: 成分 A は 20% 以上) は
**Extreme Vertices design** が必要で、 本 module では未提供
(`Mixture.hs` の haddock に将来 Phase で追加予定と明記)。 それまでは
[Custom Design](usage-custom-design.ja.md) の制約機能で代替する。

## 4. 使い分け

| やりたいこと | 使うもの |
|---|---|
| 頂点と辺を規則的に埋める | `SimplexLattice d` (本 doc) |
| 全成分を混ぜた重心点も含めたい | `SimplexCentroid` (本 doc) |
| 成分に上下限の制約がある | [Custom Design](usage-custom-design.ja.md) |
| 独立に動かせる因子の計画 | [要因計画 / RSM](01-doe.ja.md) |
| 既存の配合実験に点を追加する | [D-optimal augment](usage-doptimal-augment.ja.md) |

## 関連

- package: [hanalyze-design](../../hanalyze-design/README.ja.md)
- DoE 入口: [01-doe.ja.md](01-doe.ja.md)
- 制約付き設計: [usage-custom-design.ja.md](usage-custom-design.ja.md)
