# 統計的工程管理 (SPC) — 管理図と判定ルール

対象 module: `Hanalyze.Stat.SPC` (`hanalyze-core`)

管理図 (control chart) の fit と、 異常パターンの判定ルール
(Western Electric / Nelson) を提供する。 fit と判定は分離されており、
判定ルールは `SPCChartResult` を受け取る純粋関数として適用する。

## 0. 全体像

| API | 役割 |
|---|---|
| `fitSPC :: SPCChart -> SPCInput -> Either Text [SPCChartResult]` | 管理図を fit。 X̄-R / I-MR は 2 chart を返す |
| `checkRules :: [SPCRule] -> SPCChartResult -> [SPCViolation]` | 判定ルールを適用し違反点を列挙 |
| `westernElectricRules` / `nelsonRules` | 既定のルールセット |

## 1. chart 種別と入力の対応

`fitSPC` は chart 種別と入力の組合せが合わないと `Left` を返す。

| `SPCChart` | 対応する `SPCInput` | 用途 |
|---|---|---|
| `XR` | `VarSubgroups (Vector (Vector Double))` | subgroup 平均と範囲。 subgroup サイズは全群同一 (2〜15) |
| `IMR` | `VarIndividual (Vector Double)` | 個別値 + 移動範囲 (subgroup が取れない工程) |
| `P` | `AttrProportion (Vector Int) (Vector Int)` | 不良率 (不良数, sample size)。 sample size 可変 |
| `NP` | `AttrCount (Vector Int) Int` | 不良数 (sample size 一定) |
| `C` | `AttrDefects (Vector Int)` | 単位あたり欠陥数 (unit size 一定) |
| `U` | `AttrDefectRate (Vector Int) (Vector Int)` | 単位あたり欠陥率 (unit size 可変) |
| `EWMAChart` | `EWMAInput xs λ L μ₀ σ₀` | 指数重み付き移動平均。 小さな平均シフトの検出に強い |
| `CUSUMChart` | `CUSUMInput xs μ₀ σ₀ k h` | 累積和 (両側 C⁺ / C⁻)。 持続的な平均シフトの検出に強い |

EWMA / CUSUM は `σ₀ <= 0` を渡すと `xs` の標本標準偏差で代用する。

X̄-R chart の管理限界は Montgomery (9th ed.) Appendix VI の定数
(A2 / D3 / D4 / d2) を使う。 subgroup サイズが 2〜15 の外だと `Left`。

## 2. 基本の使い方 (X̄-R chart)

```haskell
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Vector as V
import Hanalyze.Stat.SPC

subs :: V.Vector (V.Vector Double)
subs = V.fromList (map V.fromList
  [ [10.1, 10.3,  9.8, 10.0]
  , [10.2,  9.9, 10.4, 10.1]
  , [ 9.7, 10.0,  9.9,  9.8]
  , [10.5, 10.6, 10.4, 10.7]
  , [10.0,  9.8, 10.2, 10.1]
  , [11.8, 11.6, 11.9, 11.7]   -- 明らかに外れた subgroup
  ])

main :: IO ()
main = case fitSPC XR (VarSubgroups subs) of
  Left err     -> putStrLn ("error: " ++ show err)
  Right charts -> mapM_ report charts
  where
    report r = do
      putStrLn (show (spcChartName r) ++ ": CL=" ++ show (spcCenter r)
                ++ " UCL=" ++ show (V.head (spcUCL r))
                ++ " LCL=" ++ show (V.head (spcLCL r)))
      print (checkRules westernElectricRules r)
```

実行結果:

```
"X-bar": CL=10.395833333333334 UCL=10.675283333333335 LCL=10.116383333333333
[SPCViolation {vRuleName = "Western Electric 1", vRuleNumber = 1, vPointIndex = 5, vChartName = "X-bar"}
,SPCViolation {vRuleName = "Western Electric 3", vRuleNumber = 3, vPointIndex = 4, vChartName = "X-bar"}]
"R": CL=0.38333333333333314 UCL=0.8747666666666662 LCL=0.0
[]
```

X̄ chart 側で 6 番目 (`vPointIndex = 5`、 0 origin) が 3σ 越え (WE ルール 1) として
検出され、 R chart 側は管理状態のまま = 「ばらつきではなく水準がずれた」 と読める。

## 3. 結果型 `SPCChartResult`

| フィールド | 内容 |
|---|---|
| `spcPoints` | 点ごとの統計量 (X̄ / R / 個別値 / MR / p̂ / np / c / u …) |
| `spcCenter` | 中心線 (CL) |
| `spcUCL` / `spcLCL` | 上下の管理限界 (**点ごとの Vector**) |
| `spcSigma` | 推定 σ。 ゾーン A/B/C の境界計算に使う |
| `spcChartName` | `"X-bar"` / `"R"` / `"I"` / `"MR"` / `"p"` / `"np"` / `"c"` / `"u"` |

不変条件として `length spcPoints == length spcUCL == length spcLCL`。
固定 limit chart (X̄-R / I-MR / np / c) では UCL/LCL は全要素同値、
変動 limit chart (p / u) では点ごとに異なる。

## 4. 判定ルール

```haskell
data SPCRule = SPCRule
  { ruleName   :: Text                      -- "Western Electric 1" 等
  , ruleNumber :: Int                       -- 1..8
  , ruleCheck  :: SPCChartResult -> [Int]   -- 違反点の 0-origin index
  }
```

`westernElectricRules` と `nelsonRules` が既定セット。 どちらもルール 1 は
「3σ を越えた点」 で共通、 以降は連 (run) や傾向 (trend) の検出が異なる
(例: Nelson 2 = 同じ側に 9 連、 Nelson 3 = 6 点連続の単調増減)。

ルールは `[SPCRule]` の並びなので、 独自ルールを足したり一部だけ使ったりできる:

```haskell
-- 検出子 (SPCChartResult -> [Int]) は自前で書く。
-- パターン検出のヘルパ (runSameSide 等) は internal なので export されていない
let overCenter r = [ i | (i, x) <- zip [0 ..] (V.toList (spcPoints r))
                       , x > spcCenter r + 2 * spcSigma r ]
    myRules = take 2 westernElectricRules
              ++ [SPCRule "custom: CL+2σ 超え" 99 overCenter]
in checkRules myRules chart
```

## 5. 注意点

- 変動 limit chart (`P` / `U`) では `spcSigma` は平均 sample size から算出した
  代表値なので、 ゾーン判定 (2σ / 1σ 帯を使うルール) は**近似**になる。
  3σ 越え (ルール 1) は点ごとの UCL/LCL を使うので影響を受けない。
- X̄-R の subgroup サイズは 2〜15。 それを外れる場合は `IMR` か
  `EWMAChart` / `CUSUMChart` を検討する。
- 小さな平均シフト (1σ 程度) の早期検出は X̄-R より EWMA / CUSUM が有利。

## 6. 参考文献

- Montgomery, D.C. *Introduction to Statistical Quality Control*, 9th ed. —
  管理図定数 (Appendix VI) と Western Electric ルールの出典
- Nelson, L.S. (1984) "The Shewhart Control Chart — Tests for Special Causes",
  *Journal of Quality Technology* 16(4)

## 関連

- package: [hanalyze-core](../../hanalyze-core/README.ja.md)
- 群間比較: [usage-group-comparison.ja.md](usage-group-comparison.ja.md)
