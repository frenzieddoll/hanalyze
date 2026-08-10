# Gauge R&R (測定システム解析)

対象 module: `Hanalyze.Design.GaugeRR` (`hanalyze-design`)

「そのデータのばらつきは、 本当に工程のばらつきか。 測っているせいではないか」
を分離するのが測定システム解析 (MSA)。 Gauge R&R は測定値の分散を

- **σ²_part** — 部品本来のばらつき (これが見たい信号)
- **σ²_repeatability** — 同じ人が同じ部品を測り直したときのばらつき (装置由来)
- **σ²_reproducibility** — 操作者 / 装置間のばらつき

に ANOVA で分解する。 AIAG MSA Manual 4th ed. 準拠。

| API | 実験配置 |
|---|---|
| `gaugeRRCrossed` | **Crossed** — 全操作者が全部品を測る (通常こちら) |
| `gaugeRRNested` | **Nested** — 操作者ごとに測る部品が異なる (破壊検査等) |

## 0. API

```haskell
gaugeRRCrossed
  :: V.Vector Int       -- 操作者 ID (長さ n)
  -> V.Vector Int       -- 部品 ID   (長さ n)
  -> V.Vector Double    -- 測定値     (長さ n)
  -> Either Text GaugeRRResult

gaugeRRNested :: V.Vector Int -> V.Vector Int -> V.Vector Double
              -> Either Text GaugeRRResult
```

`V.Vector` は **`Data.Vector`** (boxed)。 3 本のベクトルは同じ長さの
「縦持ち (long format)」 で渡す — 1 行 = 1 測定。

## 1. 使い方

操作者 2 名 × 部品 3 個 × 繰り返し 2 回 = 12 測定の crossed 試験。

```haskell
import qualified Data.Vector as V
import Hanalyze.Design.GaugeRR

main :: IO ()
main = do
  let ops   = V.fromList [1,1,1,1,1,1, 2,2,2,2,2,2]
      parts = V.fromList [1,1,2,2,3,3, 1,1,2,2,3,3]
      ys    = V.fromList [ 10.1, 10.2, 12.0, 12.1, 14.2, 14.0
                         , 10.3, 10.2, 12.2, 12.3, 14.1, 14.3 ]
  case gaugeRRCrossed ops parts ys of
    Left err -> putStrLn ("error: " ++ show err)
    Right r  -> do
      print (grrPctGRR r, grrPctPart r, grrNumDistinct r)
      print (grrRepeatVar r, grrReproducVar r, grrPartVar r, grrTotalVar r)
      print (grrPctRepeat r, grrPctReproduc r)
```

実行結果:

```
(0.4678860059549129,99.53211399404508,20.565093992226018)
(1.0000000000000049e-2,8.333333333333285e-3,3.8999999999999986,3.918333333333332)
(0.255210548702681,0.21267545725223191)
```

読み方:

- **%GRR = 0.47%** — 全分散のうち測定系が占める割合。 部品間の差 (10 / 12 / 14)
  が測定の揺れ (±0.1) に比べて圧倒的に大きいので、 測定システムは合格
- **%Part = 99.5%** — 見たい信号が支配的
- **ndc = 20.6** — number of distinct categories。 測定系が部品を何段階に
  区別できるかの指標で、 `1.41 · (σ_part / σ_GRR)`。 **AIAG は ≥ 5 を要求**
  するので余裕でクリア
- 内訳は繰り返し性 0.26% > 再現性 0.21% で、 どちらも無視できる水準

## 2. 結果型 `GaugeRRResult`

| フィールド | 内容 |
|---|---|
| `grrPartVar` | σ²_part |
| `grrRepeatVar` | σ²_repeatability (繰り返し) |
| `grrReproducVar` | σ²_reproducibility (操作者間) |
| `grrTotalVar` | 上 3 つの和 |
| `grrPctRepeat` / `grrPctReproduc` | 各成分 ÷ total × 100 |
| `grrPctGRR` | (repeat + reproduc) ÷ total × 100 |
| `grrPctPart` | part ÷ total × 100 |
| `grrNumDistinct` | ndc = `1.41 · (σ_part / σ_GRR)` |

> **`grrPct*` は分散比 (σ²) の百分率**。 AIAG の帳票でよく見る %Study Variation
> は標準偏差比 (σ) の百分率なので**値が一致しない**。 σ 比が要るときは
> 自分で計算する:
>
> ```haskell
> sqrt ((grrRepeatVar r + grrReproducVar r) / grrTotalVar r) * 100
> -- 上のデータでは 6.84021933825892 (分散比の 0.47% に対して σ 比は 6.84%)
> ```

判定の目安 (分散比ではなく σ 比で語られる慣習に注意):

| %GRR (σ 比) | 判定 |
|---|---|
| < 10% | 合格 |
| 10 〜 30% | 用途次第で許容 |
| > 30% | 不合格 — 測定系の改善が必要 |

## 3. Crossed と Nested

```haskell
  -- 破壊検査など、 操作者ごとに別の部品を測るしかない場合
  case gaugeRRNested ops parts ys of ...
```

Nested では「部品」 が操作者の中に入れ子になるため、 操作者間の差と
部品間の差を分離できない。 **測定で部品が壊れない限り crossed を使う**。

`Left` になる条件: 3 ベクトルの長さ不一致、 繰り返しが無く分散が推定できない
配置、 水準数が足りない場合。

## 4. 使い分け

| やりたいこと | 使うもの |
|---|---|
| 測定系のばらつきを分解して合否判定 | `gaugeRRCrossed` (本 doc) |
| 破壊検査で入れ子配置しか取れない | `gaugeRRNested` (本 doc) |
| 工程が管理状態にあるかを時系列で見る | [SPC 管理図](../stat/usage-spc.ja.md) |
| 要因が効いているかの一般的な分散分析 | [`Design.Anova`](01-doe.ja.md) |
| 工程能力指数 (Cp / Cpk) | [`Design.Quality`](01-doe.ja.md) |

## 関連

- package: [hanalyze-design](../../hanalyze-design/README.ja.md)
- SPC 管理図: [stat/usage-spc.ja.md](../stat/usage-spc.ja.md)
- DoE 入口: [01-doe.ja.md](01-doe.ja.md)
