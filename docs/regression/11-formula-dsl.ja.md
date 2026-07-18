# Formula DSL — モデルを式で宣言する

> `Hanalyze.Model.Formula{,.Frame,.Design,.RFormula}` を使い、 モデルを **式 (formula)** として
> 宣言し、 設計行列の生成と線形回帰の当てはめまでを一気通貫で行う方法を解説します。
> 関連: [01-lm.ja.md](01-lm.ja.md) (線形回帰) / [04-spline.ja.md](04-spline.ja.md) (基底)

## 目次

1. [全体像 (AST が正本・2 つの front-end)](#1-全体像)
2. [クイックスタート](#2-クイックスタート)
3. [独自構文 (正本 front-end)](#3-独自構文-正本-front-end)
4. [R / patsy 構文 (サブ front-end)](#4-r--patsy-構文-サブ-front-end)
5. [factor・交互作用・基底展開](#5-factor交互作用基底展開)
6. [線形性の検出](#6-線形性の検出)
7. [検証結果](#7-検証結果)
8. [Phase 47 拡充機能 (欠損 policy / contrast / weights・offset / 非線形)](#8-phase-47-拡充機能-欠損-policy--contrast--weightsoffset--非線形)
9. [現状の制限 (残り)](#9-現状の制限-残り)

---

## 1. 全体像

正本は **`Formula` AST** です。 文字列でも型 DSL でもなく、 2 つの front-end が同じ AST に落ちます。

```
文字列 (独自構文 or R 構文) ──parse──▶ Formula AST ──modelFrame──▶ ModelFrame ──designMatrixF──▶ 設計行列 ──fitLM──▶ FitResult
```

- **独自・明示係数構文 = 正本 front-end**: `"y x group = b0 + b1*x + bg ! group"`
  (`+`/`*` は本物の算術で false-friend なし)。
- **R / patsy 構文 = サブ front-end**: `"y ~ x + C(group)"` (statsmodels 互換・オラクル用)。
- `parseModel` が文字列に `~` を含むかで自動 dispatch します。

ポイント: **線形 OLS では係数名は当てはめに影響しません** (設計行列の各列に 1 係数が付くだけ)。
係数名が効くのは ① 報告 ② 非線形検出 (パラメータが非線形位置に現れたら検出) です。

## 2. クイックスタート

高レベル動詞 `df |-> lmF "y ~ x"` は任意の [`ColumnSource`](../io/04-fit-api.ja.md) から
formula を直接当てはめ、 `MultiLMModel` を返します。 多変量モデルの effect plot レイヤは
`statModelMulti m (along "x")` (= `ModelSpec`・`Plottable`・1 変数に沿って予測し他を固定)
から得ます。 GLM / 混合モデルは `glmF` / `glmmF` で扱えます:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Hanalyze.Plot     (lmF, glmF, glmmF, (|->), toPlot, statModelMulti, along, holdAt, HoldAgg (..))
import Hanalyze.Model.GLM (Family (..), LinkFn (..))
import Hgg.Plot.Spec        (ColData (..), layer, scatter)
import Hgg.Plot.Frame       ((|>>))
import Hgg.Plot.Backend.SVG (saveSVGBound)
import qualified Data.Vector as V

main :: IO ()
main = do
  let df = [ ("x", NumData (V.fromList [1,2,3,4,5]))
           , ("y", NumData (V.fromList [2.1,3.9,6.1,7.9,10.1])) ]
      m  = df |-> lmF "y ~ x"            -- MultiLMModel (R 構文の formula)
  -- df |-> glmF Poisson Log "y ~ x1 + x2"  -- formula GLM   → MultiGLMModel
  -- df |-> glmmF "y ~ x + (1|g)"           -- 混合モデル    → (GLMMResultRE, [Text])
  saveSVGBound "effect.svg"                -- effect plot: x に沿った予測・他は固定
    (df |>> (layer (scatter "x" "y") <> toPlot (statModelMulti m (along "x") <> holdAt Median)))
```

**低レベル (AST + `fitLMF`)** — `FitResult` とラベルが必要な場合は、 parse /
設計行列 / 当てはめのパイプラインを明示的に駆動します:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Hanalyze.Model.Formula          (parseFormula)
import Hanalyze.Model.Formula.RFormula (parseModel)
import Hanalyze.Model.Formula.Design   (fitLMF)
import Hanalyze.Model.Core             (coefficientsV)
import qualified DataFrame as DX
import qualified Numeric.LinearAlgebra as LA

main :: IO ()
main = do
  let df = DX.fromNamedColumns
             [ ("x", DX.fromList ([1,2,3,4,5] :: [Double]))
             , ("y", DX.fromList ([2.1,3.9,6.1,7.9,10.1] :: [Double])) ]
  case parseModel "y x = b0 + b1*x" >>= \f -> fitLMF f df of
    Left err        -> putStrLn err
    Right (fr, lbls) -> do
      print lbls                          -- ["b0","b1*x"]
      print (LA.toList (coefficientsV fr)) -- ≈ [2.0e-2, 2.0]  (y ≈ 2x)
```

R 構文でも同じ結果になります (`parseModel "y ~ x"`)。

## 3. 独自構文 (正本 front-end)

形: `"<応答> <データ変数…> = <右辺式>"`。 左辺で応答とデータ変数を宣言し、 **右辺の自由名
(左辺に無い名前) = 推定パラメータ**です。

演算子の優先順位 (高 → 低): `!` 添字 > `^` > 単項 `-` > `* /` > `+ -`。

| 書き方 | 意味 |
|---|---|
| `b0` | 切片 (定数項) |
| `b1*x` | 連続変数の傾き |
| `b2*log x` | 関数変換 (`log`/`exp`/`sqrt`/`sin`/`cos`/`tan`/`abs`) |
| `b1*x^2` | べき (= `x` の 2 乗) |
| `b*x*z` | 連続×連続 の交互作用 (本物の積) |

`parseFormula`/`prettyFormula` は round-trip します (`parse . pretty == id`)。

## 4. R / patsy 構文 (サブ front-end)

`~` を含む文字列は R 構文として解釈され、 同じ AST に落ちます。

| R 構文 | 意味 |
|---|---|
| `y ~ x` | 切片 (暗黙) + x。 `-1` / `0` で切片除去 |
| `y ~ C(g)` | `g` を categorical (factor) として扱う (★`C()` で明示) |
| `y ~ a:b` | 交互作用のみ / `y ~ a*b` | crossing (= `a + b + a:b`) |
| `y ~ x + I(x**2)` | `I(...)` 内は算術 (`**`/`^` 可) |
| `y ~ poly(x,2)` / `y ~ bs(x,5)` | 多項式 / B-spline 基底 |

> ★data 無しで parse するため、 R 構文では categorical を **`C(g)` で明示**してください
> (列型からの自動推論はしません)。 パラメータ名は内部で合成されます (`_p0`, `_p1`, …)。

## 5. factor・交互作用・基底展開

添字記法 `!` で factor と基底を統一的に書きます。 **factor かどうかは列型でなく
「`!` の右に現れたか」 (使われ方)** で決まります (numeric コードの factor も拾えます)。

```haskell
-- factor 主効果 + factor×連続 (水準別傾き)
parseFormula "y g x = b0 + bg ! g + bs ! g * x"

-- factor×factor (! の左結合連鎖 = 2 次元添字)
parseFormula "y g t = b0 + bg!g + bt!t + bgt!g!t"

-- 基底展開
parseFormula "y x = b0 + bp ! poly(x,2)"     -- x¹, x²
parseFormula "y x = bs ! bspline(x,5)"        -- degree-3 B-spline (knots=quantileKnots 5 x)
```

識別性は **treatment contrast** (切片があれば参照水準 = 昇順第 1 水準を drop して満ランク化)。
B-spline は partition of unity ゆえ切片併用時に先頭基底列を drop します。

## 6. 線形性の検出

パラメータがデータ式の **内側** に現れると非線形と判定され、 `fitLMF` / `linearityCheck` は
`Left` を返します (OLS は適用不可)。

```haskell
linearityCheck (parse "y x = b0 + b1*x + b2*log x") df  -- Right () : 線形
fitLMF          (parse "y x = a*exp(-b*x)")        df   -- Left "非線形: パラメータ 'b' が…"
```

## 7. 検証結果

当てはめ値 ŷ と R² は **parameterization 不変** (contrast や基底の取り方に依らない) という原理を使い、
Python 非依存のオラクルで正しさを確認しています (計画 §3.6.2 の昇格ゲート 4 点)。

| # | 検証 | 結果 |
|---|---|---|
| ① | factor×factor 飽和モデル | ŷ = セル平均・設計行列が満ランク |
| ② | 基底展開 | `poly(x,2)` が二次を厳密再現 / `bspline(x,n)` の ŷ = `fitSpline (BSpline 3)` |
| ③ | parser 堅牢性 | QuickCheck round-trip (`parse . pretty == id`) + golden 優先順位表 |
| ④ | R オラクル | 同一モデルを R/独自 両構文で書いて同 ŷ (5 ケース) + **statsmodels 突合 4/4 ALL PASS** (実機実行済) |

外部 statsmodels / scipy との突合も**実機で実行済 (Phase 47 で 6 OLS + WLS + NLS、 ALL PASS)**:
`bench/python/bench_formula.py` + `bench/python/formula_haskell_ref.json`。
> ★Phase 46 の突合で `y ~ C(g) + C(g):x` の R² 不一致を発見し、 factor×連続で参照水準を誤って
> drop していたバグを修正した (外部オラクルの実価値)。 Phase 47 では `C(g, Sum)` の ŷ/R²、
> WLS 係数 (`smf.wls`)、 NLS パラメータ (`scipy.curve_fit`) も突合済 (ALL PASS)。
> 参照値は再現可能化済 (`cabal run formula-ref-gen` が `formula_haskell_ref.json` を生成)。

```bash
# venv = repo root/.venv (numpy/pandas/statsmodels/scipy 入り)
cabal run formula-ref-gen                                          # Haskell 参照値を再生成
OPENBLAS_NUM_THREADS=1 ../.venv/bin/python bench/python/bench_formula.py
```

テストは `cabal test hanalyze-test` に含まれます。

## 8. Phase 47 拡充機能 (欠損 policy / contrast / weights・offset / 非線形)

線形核 (§1-§7) に加え、 実用回帰のための 4 機能を実装済 (Phase 47、 すべて statsmodels/scipy 突合済)。

### 8.1 欠損 policy (`MissingPolicy`)

`modelFrameWith :: MissingPolicy -> Formula -> DataFrame -> Either String ModelFrame`。
NA 検出・除去・補完を ModelFrame の単一責務点で行う (`modelFrame = modelFrameWith DropRows`)。

- `DropRows` (既定): NA を含む行を全関与列から除外 (後方互換)。
- `Impute ImputeMean` / `Impute ImputeMedian`: 連続説明変数を補完 (応答・factor の NA は別 policy 併用)。
- `TreatAsCategory`: factor 列の NA を独立水準 `"<NA>"` として扱う。
- `ErrorOnMissing`: NA があれば列名+件数つき `Left`。
- `Pairwise`: 線形 OLS では設計行列が成立しないため DropRows に縮退 (相関等の別用途用)。

### 8.2 contrast coding (`ContrastCoding`)

factor 符号化を `C(g, coding)` で指定 (正本・R 両構文)。 無注釈は `Treatment` (既定)。

```text
y g = b0 + bg ! C(g, Sum)          # 正本構文
y ~ C(g, Helmert)                  # R 構文
```

- `Treatment` (R contr.treatment) / `Sum` (sum-to-zero) / `Helmert` / `Polynomial` (直交多項式) / `CustomContrast` (k×(k-1) 行列を API で指定)。
- ŷ/R² は **parameterization 不変** ゆえ contrast の選択で係数の意味だけが変わり当てはめは不変。
- factor×連続 (masked データ列) は full coding で全水準保持 (Phase 46 の罠を踏襲)。

### 8.3 weights / offset = WLS (`fitWLSF`)

`fitWLSF :: WLSConfig -> Formula -> DataFrame -> Either String (FitResult, [Text])`。
statsmodels `smf.wls` に倣い weights/offset は列名で渡す (`WLSConfig {wcWeights, wcOffset}`)。

- weights: `√w` で X/y を行スケールし OLS に帰着 (WLS)。
- offset: 線形では `y − offset` を fit (η への固定加算。 GLM offset は別経路・未対応)。

### 8.4 非線形フィット = NLS (`fitNLS`)

`fitNLS :: Formula -> DataFrame -> [(Text, Double)] -> Either String NLSResult`
(`Hanalyze.Model.Formula.Nonlinear`)。 parse 済 AST を評価関数化し、 SSR を Nelder-Mead で最小化。

```text
y x = a * exp(-b * x)              # §6 では Left (非線形) だが fitNLS で fit 可
```

- 初期値はユーザ必須 (NLS は初期値依存)。 factor 添字は非対応 (線形側で扱う)。
- `scipy.curve_fit` と突合済 (`a*exp(-b*x)` のパラメータ復元)。

### 8.5 random effect = 混合効果モデル (`fitMixedLME` / `fitMixedGLMM`、 Phase 48)

`Hanalyze.Model.Formula.Mixed`。 lme4 流の `(1|g)` (random intercept) / `(x|g)` /
`(1+x|g)` (random slope) を Formula に追加し、 `Hanalyze.Model.GLMM` の一般ランダム
効果フィットへ route する。

```text
y ~ x + (1|g)        # random intercept
y ~ x + (1+x|g)      # random intercept + slope
y ~ x + (0+x|g)      # random slope のみ (intercept 抑制)
```

- `fitMixedLME :: Text -> DataFrame -> Either String (GLMMResultRE, [Text])` (Gaussian LME、 EM)。
- `fitMixedGLMM :: Family -> LinkFn -> Text -> DataFrame -> ...` (Binomial/Logit・Poisson/Log、 Laplace)。
- 結果 `GLMMResultRE` = 固定効果 β + ランダム効果共分散 `G` (r×r) + BLUP (q×r) + 残差分散。
- ★frequentist GLMM ゆえ random 効果に **prior 宣言は不要** (分散 `G` は推定対象。 ベイズ版が
  必要なら HBM DSL を使う)。 ★実装は `Term` を変えず **字句プリパスで `(…|g)` を抽出** する
  (固定効果は既存 `parseModel`/`designMatrixF` 経路をそのまま使用)。
- 現状は **単一 grouping factor** のみ (`(…|g1) + (…|g2)` の複数群は未対応)。
- `statsmodels smf.mixedlm(re_formula="~x").fit(reml=False)` (ML) と突合済: random slope の
  β / 共分散 G / σ² が一致 (`bench/python/bench_formula.py`)。

## 9. 現状の制限 (残り)

- **GLM offset** (Poisson の log-exposure 等): 線形 offset のみ実装、 GLM 経路は未対応。
- **複数 grouping factor** の random effect (`(…|g1) + (…|g2)`): 単一群のみ実装。
- `smooth` (B-spline) の信頼帯 (現状は点推定のみ)。

> 設計の詳細は 内部設計文書 spec: analysis-language §2.1/§2.2/§2.4/§3.6 (非公開) を参照。
