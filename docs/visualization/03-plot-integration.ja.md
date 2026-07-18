# hgg 統合 — `toPlot` / `Plottable` / `module Hanalyze`

> 🌐 [English](03-plot-integration.md) | **日本語**

> 関連: [01-visualization.ja.md](01-visualization.ja.md) (既存 `Hanalyze.Viz.*` の単発プロット) /
> [../regression/01-lm.ja.md](../regression/01-lm.ja.md) (LM) /
> hgg 本体 (`hgg/`、 layer 文法・SVG/PNG backend)。

フィット済みの統計モデルを **`toPlot :: m -> VisualSpec`** で図にし、 hgg の
layer 文法 (`df |>> (layer (scatter ..) <> toPlot fit)`) にそのまま重ねるための連携層。
**系統 A (モデル・アウト型)** の実装 (analyze Phase 46 / plot Phase 15)。

> ⚠ **Experimental + flag 隔離**。 この機能は cabal `flag plot-integration` (既定 **off**) を
> on にしたときのみ build される。 off のままなら analyze は plot 非依存の standalone
> (upstream hanalyze 互換) を保つ。 → [§6 ビルドと依存](#6-ビルドと依存) を先に読むこと。

---

## 1. これは何を解決するか

- **やりたいこと**: `fitLM` / `fitGP` 等の結果を、 散布図に回帰線・信頼帯として重ねたい。
- **方針**: 描画機構は再発明しない。 モデルを `VisualSpec` (hgg の図の通貨) に変換し、
  既存の layer 合成 (`<>`) と DataFrame バインド (`|>>`) に載せる。
- **依存は一方向** `analyze → hgg-core`。 plot は analyze を知らない。

## 2. Quickstart (LM)

```haskell
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Vector              as V
import qualified Numeric.LinearAlgebra    as LA
import           Hgg.Plot.Backend.SVG (saveSVGBound)
import           Hgg.Plot.Frame       ((|>>))
import           Hgg.Plot.Spec        (ColData (..), layer, scatter)
import           Hanalyze.Plot            (lm, (|->), toPlot)

main :: IO ()
main = do
  let xs = [1,2,3,4,5,6,7,8] :: [Double]
      ys = [2.1,3.9,6.2,7.8,10.3,11.7,14.1,16.0]
      df = [ ("x", NumData (V.fromList xs))
           , ("y", NumData (V.fromList ys)) ]
      m  = df |-> lm "x" "y"                                -- ① fit (高レベル動詞)
      plot = df |>> (layer (scatter "x" "y") <> toPlot m)   -- ② 散布図 + 回帰線 + CI band
  saveSVGBound "lm.svg" plot                                 -- ③ SVG 出力
```

- `scatter "x" "y"` は df の列を散布図に。 `toPlot m` は回帰線 + 95% CI band の `VisualSpec`。
- `<>` は `VisualSpec` の Monoid 合成 (新コンビネータ不要)。 `|>>` で df を束ねて `BoundPlot` に。

## 3. `Plottable` protocol

```haskell
class Plottable m where
  toPlot          :: m -> VisualSpec     -- 代表 1 枚 (layer 重畳の主役)
  diagnosticPlots :: m -> [VisualSpec]   -- 診断図の束 (レポート用、 既定 = [toPlot m])
```

「図にできる」 という最終能力のみを表す。 モデルの能力差 (予測・残差) は **中立 protocol**
(`Hanalyze.Model.Core` の `PredictiveModel` / `ResidualModel`) 側に持たせてある (§5)。

### 3.1 `LMModel` (線形モデル)

`FitResult` (数値核) は設計行列 X を保持しないため、 回帰線・CI band を描くのに必要な X を
束ねた描画用の型を別に用意する。

```haskell
lmModel :: LA.Vector Double -> LA.Vector Double -> LMModel   -- (x, y) → fit + X 同梱
```

`toPlot LMModel` = 訓練点を x 昇順に結ぶ回帰線 + `confidenceBand` の 95% 平均応答信頼帯。
`diagnosticPlots` = 回帰線 + 残差 vs fitted。

### 3.2 `GPResult` (ガウス過程)

`GPResult` (`Hanalyze.Model.GP`) は予測 grid・事後平均・credible band を **自己完結** で持つので、
X を別束ねせず **結果型をそのまま** `Plottable` にできる (= `FitResult` 系と異なる形でも protocol が成立)。

```haskell
import Hanalyze.Model.GP (GPModel (..), Kernel (..), fitGP, defaultGPParams)

let gmod = GPModel RBF defaultGPParams
    gres = fitGP gmod trainX trainY grid     -- GPResult
    plot = gdf |>> (layer (scatter "x" "y") <> toPlot gres)
```

`toPlot GPResult` = GP 事後平均の曲線 + `mean ± 2σ` の credible band。

> 💡 **band を見せるコツ**: 訓練点を密にして `optimizeGP` で最適化すると GP がほぼ補間し、
> credible band が極細で見えなくなる。 デモでは **疎な訓練点 + 非最適化 (`defaultGPParams`)** にすると
> 点間・端で band が広がる定番の GP 事後分布図になる。

### 3.3 `GLMMResultRE` (混合効果モデル ─ caterpillar plot)

混合効果モデル (`Hanalyze.Model.GLMM` の `GLMMResultRE`、 random intercept + slope) は、
各 group の **random effect (BLUP) を値で昇順ソート**した **caterpillar plot** で描く。
forest mark (水平棒) で並べ、 0 (= 固定効果からの偏差ゼロ) に参照線を引く。
group 間のばらつき・外れ群を一目で読めるのが GLMM 固有の定番図。

```haskell
import Hanalyze.Plot (glmmF, toPlot, (|->), Fit (..))

-- y ~ x + (1|group) を学習 → (GLMMResultRE, 固定効果係数名)
let Right (re, _) = fitEither (glmmF "y ~ x + (1|group)") df
    plot = noDf |>> toPlot re        -- 第 1 列 (通常 random intercept) の caterpillar
```

`toPlot GLMMResultRE` = random-effect **第 1 列** (通常 intercept) の caterpillar 1 枚。
`diagnosticPlots GLMMResultRE` = 全 r 列 (intercept + 各 slope) の caterpillar list。

> ⚠️ **CI 帯は現状なし (点のみ)**: `GLMMResultRE` は per-group の conditional variance も
> 観測数 `n_j` も格納しておらず (scalar 専用の `glmmBLUPSE` は `GLMMResult` 用で流用不可)、
> BLUP の標準誤差を単体から計算できない。 将来 conditional variance を持たせれば forest の
> 誤差半幅を埋めて帯化できる (forest mark は対称 CI 対応済)。

## 4. `module Hanalyze` (quickstart 出入口)

`import Hanalyze` 一発で中核 (モデル fit・基本統計・検定・効果量・分布・可視化・CSV I/O) が揃う
umbrella。 これ自体は **plot 非依存** (flag 不要)。

```haskell
import Hanalyze     -- Model.{Core,LM,GLM} + Stat.{Summary,Test,Effect,Distribution}
                    -- + Viz.{Core,Scatter,Bar,Histogram} + DataIO.CSV
```

> ⚠ **名前衝突**: GLM の `Family` と `Hanalyze.Stat.Distribution` で `Binomial` / `Poisson` が衝突する。
> umbrella は GLM 優先 (`Poisson :: Family`)。 分布値の `Poisson λ` 等が要るときは
> `Hanalyze.Stat.Distribution` を直接 import する。

## 5. 中立 protocol (portable)

`Hanalyze.Model.Core` の以下は plot 非依存で **upstream cherry-pick 可** (`(hanalyze-portable)`)。

```haskell
class PredictiveModel m where predictAt   :: m -> LA.Matrix Double -> LA.Matrix Double
class ResidualModel   m where residualsOf :: m -> LA.Vector Double
```

LM/GLM/GLMM 共有の `FitResult` に instance がある。 `Plottable` (plot 依存) はこの上に乗る最終能力。

## 6. ビルドと依存

| | flag off (既定) | flag on |
|---|---|---|
| `Hanalyze.Plot` | build されない | build (`hgg-core`/`-svg` 依存) |
| standalone 性 | ✅ plot 非依存・upstream 互換 | analyze → plot-core (一方向) |

```bash
# flag on の統合 build root は cabal.project.plot
cabal build --project-file=cabal.project.plot hanalyze
cabal test  --project-file=cabal.project.plot hanalyze-plot-test
cabal run   --project-file=cabal.project.plot plot-integration-demo

# flag off の standalone 回帰 (portable)
cabal test hanalyze-test
```

## 7. ビューア (compare.html 非依存)

`plot-integration-demo` は HS backend の SVG を **1 枚の自己完結 HTML** に埋め込んだ
`design/plot-integration/viewer.html` を生成する (PS bundle / esbuild 不要、 ブラウザで開くだけ)。
統合図 (LM / GP) + プレーン例 (scatter / line / bar / hist) を並べて目視できる。

個別に SVG/Text が欲しい場合:

```haskell
import Hgg.Plot.Backend.SVG (renderBound, saveSVGBound)  -- BoundPlot → Text / file
import Hgg.Plot.Backend.SVG (renderSVG, saveSVG)         -- inline 列のみの VisualSpec
```

## 8. portability / cherry-pick 規律

- **portable** (`(hanalyze-portable)`、 upstream 候補): 中立 protocol (`PredictiveModel` /
  `ResidualModel`) + umbrella `module Hanalyze` (plot 非依存)。
- **非 portable** (cherry-pick しない): `Hanalyze.Plot.*` 一式 (`Plottable` / `toPlot` / `LMModel` /
  `GPResult` instance)。 `hgg-core` 依存ゆえ flag `plot-integration` 配下に隔離。

## 9. 「同等」 表現の注意 (geom_smooth との差)

- **LM** の CI band は `confidenceBand` (X ベースの平均応答信頼帯、 95%) で、 ggplot
  `geom_smooth(method="lm")` の CI リボンと **概ね同等** (回帰線も最小二乗直線で一致)。
- **GP は別物**: 線は **GP 事後平均** (loess / lm 平滑ではない)、 帯は **mean ± 2σ のベイズ
  credible band** (≈95%、 頻度論 CI ではない)。 → GP を「geom_smooth 同等」 とは **言わない**。
  共通の描画機構 (`regressionLineCI`) を再利用しているだけで、 帯の統計的意味はモデルで異なる。

## 10. 系統 B: stat-in (ggplot 風スタット・イン)

§1-§9 はモデルを先に作って描く **系統 A** (`toPlot`)。 これに対し **系統 B** は ggplot の
`geom_smooth(method="lm")` のように **図の文法の中に stat を書き、 回帰計算を analyze に委譲**します。

`hgg-analyze-bridge` の `Hgg.Plot.Bridge.Stat` で提供 (逆エッジ `plot → analyze` ゆえ
隔離 package。 `hgg-core` は analyze 非依存を維持し循環なし)。

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Hgg.Plot.Backend.SVG (saveSVGWith)
import Hgg.Plot.Bridge.Stat (compileStats, lm, smooth)
import Hgg.Plot.Spec        (ColData(..), Resolver, layer, scatter)
import qualified Data.Vector as V

main :: IO ()
main = do
  let xs = V.fromList [1..30] ; ys = V.fromList (map (\x -> 2*x + 3) [1..30])
      r name | name == "x" = Just (NumData xs)
             | name == "y" = Just (NumData ys)
             | otherwise   = Nothing
      -- 散布図 + 回帰線 + 95% 信頼帯
      spec = layer (scatter "x" "y") <> compileStats r [lm "x" "y"]
  saveSVGWith "lm-stat-in.svg" r spec
```

- `lm "x" "y"` = `parseModel "y ~ x"` + `fitLMF` (= [Formula DSL](../regression/11-formula-dsl.ja.md)) で
  当てはめ + `confidenceBand` で 95% 信頼帯 → plot-core の `line` + `band` 出力。
  **回帰計算は analyze 委譲** (= ggplot `geom_smooth(method="lm")` 相当)。
- `smooth "x" "y" n` = `y ~ bs(x,n)` の B-spline 平滑 **曲線のみ** (帯なし)。
- `compileStats :: Resolver -> [Stat] -> VisualSpec`。 Resolver はバインド/描画時に得るので
  `layer scatter <> compileStats r [...]` の形で重畳する。

> ★signature について: 計画では `compileStats :: Resolver -> VisualSpec -> VisualSpec` (stat を
> VisualSpec に埋込) だったが、 それには plot-core の `MarkKind` 追加 = HS/PS レンダラ + JSON codec の
> parity 改修が必要になる。 → stat タグと解決を bridge に置き `Resolver -> [Stat] -> VisualSpec` とした
> (plot-core 型・レンダラ・PS parity を一切触らない)。

> ★「同等」 の注意: `lm` の帯は `geom_smooth(method="lm")` CI と概ね同等 (中央で細く端で広がる)。
> `smooth` は曲線のみ (帯なし) ゆえ geom_smooth の信頼帯とは異なる (§9 の GAM 規律と同じ)。
