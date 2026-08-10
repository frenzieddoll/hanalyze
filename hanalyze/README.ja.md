# hanalyze (umbrella package)

分割された 6 層を束ねる **umbrella package**。 これ 1 本を `build-depends` に
書けば、 下位層の **200 module すべてが同じ module 名のまま**使える。

Phase 106 の package 分割で library を 6 層に割ったが、 **利用側の import は
一切変えない**というのが分割の前提だった。 それを担保しているのがこの package の
`reexported-modules` (200 本) で、 `import Hanalyze.Model.LM` は分割前と
同じように書ける。

加えて、 **層をまたぐため下位に置けなかった 4 module** をここで自前実装している。

```
                  hanalyze-core        (Math / Stat-low / Optim / MCMC.Core)
                            |
              +-------------+-------------+
              |                           |
    hanalyze-frame        hanalyze-bayes
    (Data / DataIO)               (HBM DSL / MCMC / VI)
              |                           |
              +-------------+-------------+
                            |
                  hanalyze-models      (ML zoo / Causal / BayesOpt)
                            |
                  hanalyze-design      (DoE)
                            |
                  hanalyze-viz         (Vega-Lite / report)
                            |
                  ★ hanalyze           (本 package・200 module 再輸出)
                        /        \
       hanalyze-cli    hanalyze-plot
```

| 層 | 再輸出 module 数 | README |
|---|---|---|
| `-core` | 44 | [README.ja.md](../hanalyze-core/README.ja.md) |
| `-frame` | 14 | [README.ja.md](../hanalyze-frame/README.ja.md) |
| `-bayes` | 26 | [README.ja.md](../hanalyze-bayes/README.ja.md) |
| `-models` | 67 | [README.ja.md](../hanalyze-models/README.ja.md) |
| `-design` | 30 | [README.ja.md](../hanalyze-design/README.ja.md) |
| `-viz` | 19 | [README.ja.md](../hanalyze-viz/README.ja.md) |
| **計** | **200** | + 自前 4 = 204 |

## 自前実装の 4 module

いずれも「複数の層のモデル型を横断して 1 つの API に束ねる」 ため、
どの下位層にも置けなかったもの。 **hgg には依存しない** (静的描画が
要らない環境でも使える) のが共通の設計方針。

| Module | 役割 |
|---|---|
| `Hanalyze` | quickstart の出入口。 モデル fit・基本統計・可視化・CSV I/O の中核を `import Hanalyze` 1 行で揃える薄い再輸出 module |
| `Hanalyze.Fit` | 統一 fit 演算子 `\|->` と各種 `*Spec` 型。 LM / GLM / GAM / GP / 罰則付き回帰 / SVM / DoE ワークフローまで、 広い範囲を 1 つの動詞に束ねる |
| `Hanalyze.Diagnostics` | fit 済みモデルを「数値として」使う細粒度 API — 点予測・係数要約 (t/z・p 値・95% CI)・bootstrap・平滑項 F 検定 |
| `Hanalyze.Model.Wrappers` | 描画ラッパ型と smart constructor、 および `Fit` 型クラス本体 |

中核は `Fit` の統一動詞:

```haskell
-- Hanalyze/Fit.hs:396-398
infixl 1 |->
(|->) :: (ColumnSource d, Fit spec) => d -> spec -> Fitted spec
d |-> spec = fitWith spec d
```

`spec` を差し替えるだけでモデルを変えられるので、 `df |-> lm "x" "y"` を
`df |-> gp defaultGP "x" "y"` や `df |-> regularized cfg ["x1","x2"] "y"` に
書き換えるだけで別のモデルになる。 進捗表示つきの IO 版 `|->!` もある。

## 使ってみる

install 手順 (GHC / cabal のバージョン、 sibling repo の配置) は
[repository README](../README.ja.md) を参照。

```cabal
build-depends: hanalyze
```

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Hanalyze
import Hanalyze.Fit         (lm, (|->))
import Hanalyze.Diagnostics (modelReport, showReport)
import qualified Data.Text.IO      as TIO

main :: IO ()
main = do
  Right df <- loadAuto "flights.csv"
  let m = df |-> lm "month" "dep_delay"
  TIO.putStrLn (showReport (modelReport m))
```

```
term                    estimate     std.err        stat     p.value   [2.5%, 97.5%]
(Intercept)              -2.1667     13.1762     -0.1644      0.8774   [-38.7495, 34.4162]
x                         6.6667      8.3333      0.8000      0.4685   [-16.4704, 29.8037]
```

> 係数表の項名は現状 `(Intercept)` / `x` 固定で、 渡した列名 (`month`) は
> 反映されない (`Diagnostics.hs:120` / `:187`)。 `LMModel` / `GLMModel` が
> 列名を保持していないため。

`import Hanalyze` だけで CSV 読み込み (`loadAuto`)・記述統計・検定・
散布図 / 棒グラフ / ヒストグラムまで届く。 そこから先 (DoE・HBM・最適化) は
必要な module を個別に import する。

## 依存を絞りたいとき

たとえば「計画表を作るだけ」「CSV を整形するだけ」 なら、 umbrella ではなく
該当層だけを依存に書けば build するコード量を大きく減らせる:

```cabal
build-depends: hanalyze-design   -- DoE の生成だけ
build-depends: hanalyze-frame    -- CSV I/O と dataframe 操作だけ
```

各層で何ができるかは上の表の README を参照。

## テスト

test-suite `hanalyze-test` (`test/Spec.hs`、 hspec-discover) が約 130 本の
`*Spec` module を集める。 ここが library 全体の回帰テストの本体になる。

```bash
cabal test hanalyze-test
```

hgg 統合まわりの診断テストは、 package 循環を避けるため Phase 106.4 で
[`hanalyze-plot`](../hanalyze-plot/README.ja.md) の
`hanalyze-plot-test` へ移設済み。

## 関連 docs

- クイックスタート: [docs/01-quickstart.ja.md](../docs/01-quickstart.ja.md)
- `|->` の使い方: [docs/io/04-fit-api.ja.md](../docs/io/04-fit-api.ja.md)
- PyMC との比較: [docs/02-pymc-comparison.ja.md](../docs/02-pymc-comparison.ja.md)
- できること一覧: [repository README](../README.ja.md)

← [repository README](../README.ja.md)
