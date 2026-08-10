# hanalyze-cli

[`hanalyze`](../README.ja.md) のコマンドラインフロントエンド。
実行ファイル **`hanalyze`** 1 本だけを提供する package で、 library は持たない。

Phase 106 で umbrella package から分離した。 目的は 2 つ:

- library の作業をするたびに CLI の compile + link が走らないようにする
- **umbrella の公開 API だけに依存する**構成にして、 internal module へ手を
  伸ばしていないことを構造で保証する (`build-depends` は `hanalyze` の
  1 本のみ)

```bash
cabal build hanalyze          # 既定の cabal.project に含まれる
cabal run   hanalyze -- --help
```

## サブコマンド (全 15 個)

### 回帰

| コマンド | 内容 |
|---|---|
| `regress` | 古典 / ベイズ回帰 (LM / GLM / GLMM / GP / HBM)。 **既定のコマンド**で、 サブコマンド名を省くとこれになる |
| `ridge` | 罰則付き回帰 (Ridge / Lasso / Elastic Net) |
| `kernel` | カーネル回帰 / RFF 近似 |
| `spline` | B-spline / 自然三次スプライン回帰 |
| `quantile` | 分位点回帰 (τ 分位・MM-IRLS) |
| `gam` | 一般化加法モデル (加法 B-spline + Ridge) |
| `rf` | ランダムフォレスト回帰 (CART + bagging + 特徴量サブセット) |
| `multireg` | 多出力回帰 (wide CSV・linear / kernel-rbf) |

### データの下見・可視化

| コマンド | 内容 |
|---|---|
| `info` | 列ごとの型と基本統計量を表示 |
| `hist` | ヒストグラム (理論分布の重ね描きも可) |

### 実験計画法

| コマンド | 内容 |
|---|---|
| `doe` | 直交表 (L_n) の生成 |
| `taguchi` | 田口メソッド (SN 比 + 要因効果 + 内側 / 外側配置) |

### データ整形

| コマンド | 内容 |
|---|---|
| `clean` | 列ごとのクリーニング規則を適用 (`StripUnits` / `ParseCurrency` / `ParseDecimalEU` …) |
| `melt` | wide → long 変換 |
| `regrid` | 歯抜けの long 形式データ `[id, z, y]` を共通 grid へ揃える |

> ⚠ `clean` / `melt` / `regrid` は実装済みだが `hanalyze --help` の一覧には
> 載っていない。 各コマンドを引数なしで実行すれば usage が出る
> (例: `hanalyze melt`)。

## 使い方

```bash
# 列ごとの型・基本統計を見る
hanalyze info data.csv

# 単回帰 (サブコマンド省略 = regress)
hanalyze data.csv x y

# 多項式の次数を指定して 90% 信頼区間つき
hanalyze data.tsv "x1 x2" y LM --degree -1 2 -2 3 --ci 0.90

# ポアソン回帰 (log link)
hanalyze data.csv x y GLM -d poisson -l log

# 変量効果つき (LM + --group → LME、 GLM + --group → GLMM)
hanalyze data.csv x y LM --group school

# HTML レポートを書き出す
hanalyze data.csv x y --report report.html --waic
```

`regress` の主なオプション:

| オプション | 内容 |
|---|---|
| `-d, --dist DIST` | 分布 `gaussian` / `binomial` / `poisson` (既定 `gaussian`) |
| `-l, --link LINK` | リンク関数 `identity` / `log` / `logit` / `sqrt` (既定 = 正準リンク) |
| `--degree SPEC` | 多項式次数の指定 (既定 `1`) |
| `--ci [LEVEL]` | 信頼区間 (既定 `0.95`) |
| `--pi [LEVEL]` | 予測区間 (Gaussian のみ・既定 `0.95`) |
| `--group COL` | グループ列 → LME / GLMM |
| `--format FORMAT` | 出力形式 `html` / `png` / `svg` (既定 `html`) |
| `--report [FILE]` | HTML 解析レポートを生成 (既定 `report.html`) |
| `--waic` | WAIC と LOO-CV をレポートに載せる (`--report` が前提) |

各サブコマンド固有のオプションは、 引数なしで実行すると表示される
(例: `hanalyze ridge`)。

## 関連 docs

CLI 専用のページはまだ無く、 各機能の doc の中で `hanalyze <sub>` の実行例が
示されている。

- 回帰の入口: [docs/regression/01-lm.ja.md](../docs/regression/01-lm.ja.md)
- `clean` の規則一覧: [docs/io/01-dirty-data.ja.md](../docs/io/01-dirty-data.ja.md)
- `melt` / `regrid`: [io/02-reshape.ja.md](../docs/io/02-reshape.ja.md) /
  [io/03-regrid.ja.md](../docs/io/03-regrid.ja.md)
- `doe` / `taguchi`: [doe/01-doe.ja.md](../docs/doe/01-doe.ja.md) /
  [doe/02-orthogonal-taguchi.ja.md](../docs/doe/02-orthogonal-taguchi.ja.md)
- `--report` が出す HTML: [visualization/02-report-builder.ja.md](../docs/visualization/02-report-builder.ja.md)

← [repository README](../README.ja.md)
