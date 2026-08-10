# hanalyze-demos

[`hanalyze`](../README.ja.md) の**デモ / ベンチマーク実行ファイル置き場**。
library は持たず、 **151 本の executable** だけを持つ package。

Phase 106.4 で plot 依存の exe を切り出したのが始まりで、 Phase 109.2 で残りの
demo / bench exe も umbrella から移し、 umbrella を「library + test のみ」 に
戻した。 この package は **opt-in build** で、
既定の `cabal.project` にも `cabal.project.plot` にも含まれない。 専用の
build root を使う:

```bash
cabal build --project-file=cabal.project.demos hanalyze-demos
cabal run   --project-file=cabal.project.demos glmm-demo
cabal run   --project-file=cabal.project.demos posteriordb-radon
```

`cabal.project.demos` は `cabal.project.plot` を `import` した上でこの package を
足す構成なので、 sibling の `hgg-*` も一緒に build される。

## 内訳 (全 151 exe)

`main-is` を `hs-source-dirs` へ解決して実測した内訳。

| ディレクトリ | 本数 | 内容 | 代表 exe |
|---|---|---|---|
| `bench/haskell` | 43 | Haskell 側ベンチマーク (Python 集計器が読む CSV を吐く) | `bench-mcmc` / `bench-bo` / `bench-data-gen` |
| `demo/bayesian` | 37 | HBM DSL・MCMC・変分推論のデモ | `hbm-example` / `plate-notation-demo` |
| `../bench/posteriordb/<NN-name>` | 35 | posteriordb 横断ベンチ (1 モデル = 1 exe) | `posteriordb-radon` / `posteriordb-eight-schools` |
| `demo/doe-optim` | 11 | 実験計画法・最適化 | `optimaldoe-demo` / `materials-moo-demo` |
| `demo/regression` | 9 | 回帰・GP・ロバスト回帰 | `gp-demo` / `robust-gp-demo` |
| `demo/io` | 6 | CSV 読み込み・クリーニング・regrid | `dirty-data-demo` / `preprocess-demo` |
| `demo-plot` | 3 | hgg 統合 (`toPlot`) の実例 | `plot-integration-demo` / `readme-dag-demo` |
| `demo` | 2 | 総合デモ | `glmm-demo` / `integrated-demo` |
| `demo/doe` | 2 | 逐次 RSM・サンプルサイズ設計 | `doe-rsm-samplesize-demo` |
| `demo/visualization` | 2 | Vega-Lite 可視化 | `bar-demo` / `new-sections-demo` |
| `../experiments/phase104-...` | 1 | 調査用の使い捨て probe | `phase104-probe-prof` |

## 共有 module

| Module | 置き場所 | 役割 |
|---|---|---|
| `Common` | `../bench/posteriordb/Common.hs` | posteriordb 35 本が共有。 `summarize` は `arviz.summary` の簡易代替 (mean / sd / 94% HDI / ESS / R-hat / MCSE)。 Python 側の `_common.py` と対 |
| `BenchUtil` | `bench/haskell/BenchUtil.hs` | ベンチ 43 本が共有。 Python 集計器が読む統一 CSV 行 (`BenchRow`) の書き出しと計時 (`timeit` / `timeitIO`) |

> `../bench/posteriordb` と `../experiments` は repo root 側に置いたままで、
> この package からは相対パスで参照している。 package 配下への `git mv` は
> Phase 89 (posteriordb ベンチ) のクローズまで保留する方針。

## どれを動かせばよいか

- **library の使い方を知りたい** → `demo/` 系。 とくに `integrated-demo` と
  `glmm-demo` が入口
- **図の出し方を知りたい** → `demo-plot/` (静的 SVG) と `demo/visualization/`
  (Vega-Lite)
- **HBM / MCMC の書き方を知りたい** → `demo/bayesian/`
- **性能を測りたい** → `bench/haskell/` (CSV 出力)、 他実装との比較は
  `posteriordb-*`

ベンチマークは**並走させず単独で逐次実行**する (同時実行すると測定値が
歪む)。

## 関連 docs

- ベンチマークの回し方と結果: [bench/README.md](../bench/README.md) /
  [bench/RESULTS_tier12.md](../bench/RESULTS_tier12.md)
- PyMC との比較: [docs/02-pymc-comparison.ja.md](../docs/02-pymc-comparison.ja.md)
- Python / R との比較: [docs/comparison/python-r.ja.md](../docs/comparison/python-r.ja.md)
- 可視化: [docs/visualization/01-visualization.ja.md](../docs/visualization/01-visualization.ja.md)
- 静的描画の統合: [visualization/03-plot-integration.ja.md](../docs/visualization/03-plot-integration.ja.md)

← [repository README](../README.ja.md)
