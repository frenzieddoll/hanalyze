# hanalyze-plot

[`hanalyze`](../README.ja.md) と姉妹プロジェクト **hgg** を
繋ぐ**統合層**。 fit 済みの解析モデルを hgg の `VisualSpec` へ
変換する `toPlot` / `Plottable` を提供する 8 module。

依存が他の層と違う点に注意:

- **umbrella package `hanalyze` の上**に乗る (`Fit` / `Wrappers` を
  import するため)。 umbrella → plot 方向にすると package 循環になるので、
  この向きは cabal file にも明記されている。
- sibling repo の `hgg-{core,svg,3d,custom}` に依存する。 このため
  **既定の `cabal.project` には含まれない**。 build には専用の build root
  `cabal.project.plot` を使う。

```bash
cabal build --project-file=cabal.project.plot hanalyze-plot
cabal test  --project-file=cabal.project.plot hanalyze-plot-test
```

> **Vega-Lite で HTML の図・レポートを出したい場合**は
> [`hanalyze-viz`](../hanalyze-viz/README.ja.md) を使う。
> こちらは SVG / PDF / PNG の静的描画が対象。

## 主要 module (全 8 module)

| Module | 役割 |
|---|---|
| `Hanalyze.Plot` | 統合層の入口。 下記 instance 群と `Fit` 系 API (`\|->` / `lm` / `glm` …) をまとめて再輸出する |
| `Plot.Core` | モデル族に依存しない骨格 — `Plottable` / `SingleVarModel` / `MultiVarModel` と grid 評価核 |
| `Plot.Linear` | 線形モデル族の instance (`LMModel` / `GLMModel` / `WeightedLMModel` …) |
| `Plot.Bayes` | ベイズ / HBM 族の instance (`ChainModel` / `ForestSpec` / `PPCSpec` / `DagSpec` / `GLMMResultRE`) + 抽出子 |
| `Plot.ML` | ML / 統計モデル族の instance + 抽出子 |
| `Plot.Robust` | ロバスト回帰・分位点回帰族の instance |
| `Plot.Smooth` | 平滑化・カーネル法族の instance |
| `Plot.Wrappers` | 汎用ラッパ型 (`MultiFit` / `RegModel`) の instance |

中核の型クラスは `Plot.Core` の 1 本だけ:

```haskell
class Plottable m where
  -- | 代表 1 枚の図 (= layer 重畳の主役、 <> で他 layer と合成可)。
  toPlot          :: m -> VisualSpec

  -- | 診断図の束 (= レポート用)。 既定は代表 1 枚のみ。
  diagnosticPlots :: m -> [VisualSpec]
  diagnosticPlots m = [toPlot m]
```

モデル型ごとに `Plottable` の instance を足していく設計なので、 新しいモデルを
描けるようにするのは「instance を 1 本書く」 だけで済む。

## 使い方

```cabal
build-depends: hanalyze-plot
```

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Graphics.Hgg.Frame       ((|>>))
import Graphics.Hgg.Spec        (layer, scatter)
import Graphics.Hgg.Backend.SVG (saveSVGBound)
import Hanalyze.Plot     (toPlot, statModel, grid, (|->), lm)

main :: IO ()
main = do
  let m       = df |-> lm "x" "y"
      lmPlot  = df |>> (layer (scatter "x" "y") <> toPlot m)
  saveSVGBound "lm-scatter-ci.svg" lmPlot
```

`df |-> lm "x" "y"` (fit) → `toPlot` (図化) → `|>>` で他の layer と重ねる、
という 1 本の流れになる。 `toPlot` に渡す前に `statModel m <> grid 200` の
ように**描画オプションを合成**できる (grid 分割数・信頼帯の種類 `bandMode` /
`piMethod`・色 `statColor` 等)。

上の形は demo `plot-integration-demo`
(`hanalyze-demos/demo-plot/PlotIntegrationDemo.hs`) がそのまま使って
いる経路で、 LM / GLM / spline / GP / 分位点回帰などの実例が並んでいる:

```bash
cabal run --project-file=cabal.project.demos plot-integration-demo
```

## テスト

test-suite `hanalyze-plot-test` (`test-plot/Spec.hs`) が、 モデル種別ごとの
`toPlot` 結果の構造・数値を検証する。 元は umbrella 側にあったが、 umbrella の
component がこの package に依存すると循環するため Phase 106.4 で移設した。

## 関連 docs

- 静的描画との統合: [docs/visualization/03-plot-integration.ja.md](../docs/visualization/03-plot-integration.ja.md)
- 可視化の入口: [01-visualization.ja.md](../docs/visualization/01-visualization.ja.md)
- API 一覧 (en): [api-guide/12-plot.md](../docs/api-guide/12-plot.md)

← [repository README](../README.ja.md)
