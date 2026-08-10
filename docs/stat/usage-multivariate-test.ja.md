# 多変量平均の検定 — Hotelling T² と MANOVA

対象 module: `Hanalyze.Stat.Test` (`hanalyze-core`)

複数の応答変数を**同時に**扱って平均ベクトルの差を検定する。 変数ごとに
t 検定を繰り返すと多重性で偽陽性が増え、 変数間の相関も無視されるが、
Hotelling T² / MANOVA はそれを 1 つの検定にまとめる。

他の検定と同じく結果は共通の `TestResult` 型で返る。

## 0. API

| 関数 | 帰無仮説 |
|---|---|
| `hotellingsT2 :: Matrix Double -> Vector Double -> TestResult` | 1 標本: μ = μ₀ |
| `hotellingsT2TwoSample :: Matrix Double -> Matrix Double -> TestResult` | 2 標本 (等分散仮定): μ_X = μ_Y |
| `manova :: [Matrix Double] -> TestResult` | 一元配置: 全群の μ が等しい (Wilks' Λ) |

入力行列は **行 = 観測、 列 = 変数**。 いずれも `Numeric.LinearAlgebra`
(hmatrix) の `Matrix Double` を取る。

## 1. 使い方

```haskell
import qualified Numeric.LinearAlgebra as LA
import qualified Hanalyze.Stat.Test as ST

grpA, grpB :: LA.Matrix Double
grpA = LA.fromLists [[5.1,3.5],[4.9,3.0],[4.7,3.2],[4.6,3.1],[5.0,3.6]]
grpB = LA.fromLists [[6.3,3.3],[5.8,2.7],[7.1,3.0],[6.3,2.9],[6.5,3.0]]

main :: IO ()
main = do
  let t2 = ST.hotellingsT2TwoSample grpA grpB
      mv = ST.manova [grpA, grpB]
  print (ST.trMethod t2, ST.trStatistic t2, ST.trPValue t2)
  print (ST.trMethod mv, ST.trStatistic mv, ST.trPValue mv)
```

実行結果:

```
("Hotelling T\178 (2-sample)",33.06381127031984,2.713676352444471e-4)
("MANOVA (one-way, Wilks' \923)",33.06381127031985,2.7136763524444673e-4)
```

**2 群の場合、 MANOVA は Hotelling T² と数学的に等価**なので、 統計量も
p 値も (浮動小数の丸め以外) 一致する。 3 群以上で初めて MANOVA の出番になる。

## 2. 適用条件と「結果なし」 の扱い

これらは例外を投げず、 前提を満たさないときは「結果なし」 の `TestResult`
(`trStatistic = 0` / `trPValue = 1/0` = Infinity / `trDf = Nothing` /
理由が `trNote`) を返す。 **`trNote` を必ず確認すること**。 主な条件:

| 関数 | 必要条件 |
|---|---|
| `hotellingsT2` | 観測数 n ≥ 2、 変数数 p ≥ 1、 **n > p** (共分散が特異にならないこと)、 μ₀ の長さ = p |
| `hotellingsT2TwoSample` | 各群 n ≥ 2、 両群の変数数が一致、 **n₁ + n₂ > p + 1** |
| `manova` | 群数 k ≥ 2、 各群 n ≥ 2、 全群で変数数が一致 |

**n > p** は実務で最も引っかかりやすい制約。 変数が観測数より多い場合は
主成分分析 ([02-pca.ja.md](02-pca.ja.md)) 等で次元を落としてから適用する。

## 3. 結果の読み方

`TestResult` の主なフィールド:

| フィールド | 内容 |
|---|---|
| `trMethod` | `"Hotelling T² (2-sample)"` / `"MANOVA (one-way, Wilks' Λ)"` 等 |
| `trStatistic` | 検定統計量 (F 変換後の値) |
| `trDf` | `Just (df1, Just df2)` = F 分布の分子・分母自由度 |
| `trPValue` | p 値 |
| `trNote` | 前提を満たさなかった場合の理由 |

有意になったあと「どの変数が効いているか」 を知りたい場合は、
変数ごとの一括比較 ([usage-group-comparison.ja.md](usage-group-comparison.ja.md))
や多重比較補正つきの個別検定へ進む。

## 4. 使い分け

| 状況 | 使うもの |
|---|---|
| 1 群の平均ベクトルが既知の基準値と違うか | `hotellingsT2` |
| 2 群の平均ベクトルが違うか | `hotellingsT2TwoSample` (= 2 群 MANOVA) |
| 3 群以上 | `manova` |
| 変数ごとに順位づけしたい | [usage-group-comparison.ja.md](usage-group-comparison.ja.md) |
| 1 変数だけ | [01-test.ja.md](01-test.ja.md) |

## 関連

- package: [hanalyze-core](../../hanalyze-core/README.ja.md)
- 検定全般: [01-test.ja.md](01-test.ja.md)
- 多重比較: [06-multipletesting.ja.md](06-multipletesting.ja.md)
