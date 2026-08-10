# 良品 vs 不良品の一括群間比較 (Good vs Bad)

対象 module: `Hanalyze.Stat.GroupComparison` (`hanalyze-core`)

二値ラベル (良品 / 不良品、 合格 / 不合格) で分けた 2 群について、
**多数の説明変数を一度に比較して効果量順にランク付け**する。
Spotfire の "Good vs Bad" 相当。 半導体・製造の品質解析で「どのパラメータが
不良と最も結びついているか」 を素早く絞り込む用途。

単独の検定ではなく **複数変数の並列比較に最適化**された helper なので、
1 変数だけを厳密に検定したいときは `Hanalyze.Stat.Test` を直接使う。

## 0. API

```haskell
goodVsBad
  :: [(Text, Vector Double)]   -- (変数名, 値ベクトル)
  -> Vector Bool               -- 群ラベル (True = Good, False = Bad)
  -> Either Text [GroupCompResult]
```

各変数について (i) 平均差、 (ii) Cohen's d、 (iii) Welch の t 検定 p 値 を
計算し、 **|Cohen's d| の降順**で返す。

`Left` になる条件: 変数リストが空 / ラベルが空 / 変数長とラベル長の不一致 /
どちらかの群が 2 観測未満 (Welch t 検定の前提)。

## 1. 使い方

```haskell
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Vector as V
import Hanalyze.Stat.GroupComparison

main :: IO ()
main = case goodVsBad vars labels of
  Left err -> putStrLn ("error: " ++ show err)
  Right rs -> mapM_ (\r -> print (gcrVarName r, gcrMeanDiff r, gcrEffect r, gcrPValue r)) rs
  where
    labels = V.fromList [True, True, True, True, False, False, False, False]
    vars =
      [ ("temp",     V.fromList [200, 202, 199, 201, 215, 218, 212, 216])
      , ("pressure", V.fromList [ 30,  31,  29,  30,  31,  29,  30,  30])
      ]
```

実行結果:

```
("temp",14.75,7.41371417676301,2.504838886050367e-4)
("pressure",0.0,0.0,1.0)
```

`temp` は不良群で平均が 14.75 高く、 Cohen's d = 7.41 と極端に大きい。
`pressure` は差なし。 **返り値は既にランク済**なので、 先頭数件を見れば
「効いている変数」 が分かる。

## 2. 結果型 `GroupCompResult`

| フィールド | 内容 |
|---|---|
| `gcrVarName` | 変数名 |
| `gcrMeanG` / `gcrMeanB` | Good 群 (`True`) / Bad 群 (`False`) の平均 |
| `gcrMeanDiff` | **Mean(Bad) − Mean(Good)** (符号に注意) |
| `gcrEffect` | Cohen's d (符号つき。 ランクは絶対値) |
| `gcrPValue` | Welch の両側 t 検定の p 値 |
| `gcrNG` / `gcrNB` | 各群のサイズ |

## 3. 多重比較補正

`goodVsBad` は**補正をしない生の p 値**を返す。 変数が多いほど偽陽性が増えるので、
スクリーニング後に `Hanalyze.Stat.MultipleTesting` を通す:

```haskell
import qualified Hanalyze.Stat.MultipleTesting as MT

let ps      = map gcrPValue rs
    adjusted = MT.benjaminiHochberg ps    -- FDR 制御
```

## 4. 使い分け

| やりたいこと | 使うもの |
|---|---|
| 多変数を一括スクリーニングして順位を見る | `goodVsBad` (本 doc) |
| 1 変数を厳密に検定する | [`Stat.Test`](01-test.ja.md) の `tTestWelch` 等 |
| 差の大きさを事後分布で判断したい | [Bayesian A/B test](../bayesian/usage-bayesian-ab-test.ja.md) |
| 多変量として同時に差を見たい | [Hotelling T² / MANOVA](usage-multivariate-test.ja.md) |

## 関連

- package: [hanalyze-core](../../hanalyze-core/README.ja.md)
- 効果量の解釈: [09-effect.ja.md](09-effect.ja.md)
- 多重比較: [06-multipletesting.ja.md](06-multipletesting.ja.md)
