# GLM-Poisson (posteriordb: `GLM_Poisson_Data-GLM_Poisson_model`)

3 次多項式 Poisson 回帰。BPA (Kéry & Schaub 2011, Ch.3) の個体数カウント
データ (n=40 年)。出典: `stan-dev/posteriordb`
(`posterior_database/models/stan/GLM_Poisson_model.stan`・
`posterior_database/data/data/GLM_Poisson_Data.json.zip`)。

- `reference_posterior_name: null` — posteriordb に公式 reference posterior
  無し。hanalyze vs PyMC の 2 者比較のみ。
- Prior: Stan 原典は `alpha ∈ [-20,20]`・`beta1..3 ∈ [-10,10]` の**暗黙の一様
  事前分布**。`Uniform lo hi` で忠実に移植 (hanalyze の制約変換は現状
  unconstrained 扱いだが、事後質量が境界から十分離れているため実測上
  問題なく収束した — 詳細は `bench/posteriordb/README.md`)。

## ファイル

- `model.py` — PyMC 実装 + 合成ダッシュボード生成 (`py_dashboard_full.svg`・
  `../_common.py` の `make_pymc_dashboard` を使用)。chain 数等はコード中の
  定数 (`chains=4`) で固定。
- `Model.hs` — hanalyze 実装 (cabal exe `posteriordb-glm-poisson`・
  `df |-> hbm` 高レベル API・`dataNamedX`/`dataNamedObs`/`plateForM_`・
  aeson で JSON 読込)。 chain 数等は PyMC 側と同じ定数 (`hbmChains = 4`)
  で揃える (CLI 引数化しない・py/hs でコードの意味を極力合わせる方針)。
  診断図は hgg `dashboardFullOf` で PNG 出力 (rasterific backend・
  `hs_dashboard_full.png`・SVG は param 数×draw 数で肥大するため PNG 採用)。
  事後要約は `../Common.hs` (`summarize`/`printSummary`) = `az.summary` 簡易版。
- `run_pymc_matrix.py` — PyMC の CPU sampler×backend マトリクス
- `data/GLM_Poisson_Data.json` — posteriordb 由来データ (year/C/n)
- `figures/` — `hs_dashboard_full.png`/`py_dashboard_full.svg`
  (詳細は `bench/posteriordb/README.md` 参照)

## 実行方法

```bash
# PyMC 最速 CPU 組み合わせの選定 (+ py_dashboard_full.svg 生成)
bench/venv/bin/python bench/posteriordb/01-glm-poisson/model.py
bench/venv/bin/python bench/posteriordb/01-glm-poisson/run_pymc_matrix.py

# hanalyze (+ hs_dashboard_full.png 生成・az.summary 相当の要約表を標準出力へ)
# hgg 連携につき plot-integration flag が必要。figures/ は事前に
# 存在する前提 (実行時にディレクトリ作成はしない):
cabal build --project-file=cabal.project.plot posteriordb-glm-poisson
cabal run --project-file=cabal.project.plot posteriordb-glm-poisson
```

## 経路確認

`synthVecIR` = `Just` (vecIR 高速経路・データ束縛済みモデルで実測確認)。
**要改善記録は無し** — 正式に数値を記録する。M7
(`bench/haskell/BenchHBMScaling.hs`) の既存コメント「非Gaussian観測ゆえ
fallback」は古い情報だった (`VGPois` 族対応が後に追加されている・本 Phase
で判明・コメント訂正済み)。

## 結果

### 精度 (4 chain・warmup1000+draws1000・独立検証 = arviz)

| パラメータ | hanalyze | pymc (nutpie+numba・最速CPU) |
|---|---|---|
| alpha | 4.284 ± 0.031 | 4.285 ± 0.029 |
| beta1 | 1.247 ± 0.048 | 1.248 ± 0.044 |
| beta2 | 0.0697 ± 0.0249 | 0.070 ± 0.0233 |
| beta3 | -0.230 ± 0.0256 | -0.231 ± 0.023 |

全パラメータ小数第2〜3位まで一致。R-hat = 1.00 (両系統とも)。

> **Phase 90 A11-4② 注記 (2026-07-12)**: F3b (Poisson log-link の
> `log(exp η) → η` 代数簡約) 導入により本モデルの draws は seed 固定でも
> ulp 変化する (dashboard PNG の新 baseline を commit)。 事後は不変で、
> 上表の参照値と MC 誤差内一致を再確認済み (alpha 4.286・beta1 1.244・
> beta2 0.0684・beta3 −0.228)。 詳細は phase md §A11-4②。

### 速度 (4 chain・warmup1000+draws1000・CPU 1コア固定)

PyMC は sampler×backend 7 通りを計測 (blackjax はこのモデルで
`ValueError: cannot select an axis to squeeze` により失敗・bounded Uniform
絡みの可能性・未解明のため参考記録):

| system | wall (ms) | ESS (alpha・arviz計算) | ESS/sec | 対 hanalyze |
|---|---:|---:|---:|---:|
| **hanalyze (Haskell・自作 IR)** | 494.6 | 2459 | **4971.7** | 基準 |
| nutpie + Numba (**PyMC最速CPU**) | 5785.3 | 1563 | 270.2 | 18.4× |
| nutpie + JAX | 8050.6 | 1428 | 177.4 | 28.0× |
| pymc + Numba | 8047.0 | 1934 | 240.3 | 20.7× |
| numpyro | 12626.7 | 1400 | 110.9 | 44.8× |
| pymc + CVM (真の C) | 14981.7 | 1883 | 125.7 | 39.6× |
| pymc + JAX (own NUTS) | 27343.8 | 1682 | 61.5 | 80.8× |
| blackjax | (失敗・後述) | — | — | — |

**hanalyze は PyMC 最速 CPU 組み合わせ (nutpie+Numba) の 18.4 倍**。
Poisson 尤度 (非 Gaussian) でも vecIR 経路が有効に効いている。

### 図 — 両側とも「フルダッシュボード」1 枚に統一

各モデル 2 枚のみ (`hs_dashboard_full.png` / `py_dashboard_full.svg`)。
構成は共通: **上段 2×2 (モデル構造 DAG / forest / PPC / energy) + 下段
param ごと [事後分布 | trace]**。trace は必ず含まれる。

- **Haskell**: hgg の `dashboardFullOf` (1 API 呼び出しで直接 1 枚・
  PNG 出力)。
- **Python**: 新世代 arviz-plots は各図が独立 Figure を返す設計で dashboard
  合成のネイティブ API が無いため、`../_common.py` の `make_pymc_dashboard`
  で各図を PNG 化して `matplotlib.gridspec` で 1 枚に合成する。

### 既知の課題

- **blackjax エラー**: `ValueError: cannot select an axis to squeeze out
  which has size not equal to one, got shape=(4, 2)`。他モデル (radon・
  Phase 88) では blackjax は問題なく動いたため、本モデル特有 (bounded
  `Uniform` の 4 パラメータ構成起因の可能性) と見られるが未解明。
  参考記録として残し、深掘りはしない (本 Phase の主眼ではない)。
- hanalyze の `Uniform` 制約変換は現状 unconstrained 扱い (真の
  logit-on-(lo,hi) 変換は未実装・`Distribution.hs:310` にコメントあり)。
  本モデルでは事後質量が境界 ([-20,20]/[-10,10]) から十分離れているため
  実害なく収束したが、**境界に近い事後を持つ他モデルでは問題化しうる**
  (DSL 機能ギャップとして記録・今回は「要改善」トリガーには該当しなかった)。

全体サマリは `bench/posteriordb/README.md` にも記録。
