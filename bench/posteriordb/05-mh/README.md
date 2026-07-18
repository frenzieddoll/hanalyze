# Mh (posteriordb: `Mh_data-Mh_model`)

Capture-recapture (BPA本 Ch.6・M=385個体の拡張データセット・T=5回のサンプ
リング機会・個体ごとのランダム効果)。出典: `stan-dev/posteriordb`
(`posterior_database/models/stan/Mh_model.stan`・
`posterior_database/data/data/Mh_data.json.zip`)。

- `reference_posterior_name: null` (posteriordb に公式 reference 無し・
  hanalyze vs PyMC の2者比較のみ)。
- Prior: `omega,mean_p ~ Beta(1,1)` (= Uniform(0,1) と数学的に同一・
  「既知の課題」参照)・`sigma ~ Uniform(0,5)`・`eps_raw_i ~ Normal(0,1)`
  (i=1..385、個体ごとの独立な非中心化ランダム効果)。
- 尤度: `eps_i = logit(mean_p) + sigma*eps_raw_i`・`p_i = invlogit(eps_i)`・
  `y_i ~ ZeroInflatedBinomial(T, 1-omega, p_i)` (Stan の
  `log_sum_exp(bernoulli+binomial_logit)` と代数的に厳密一致・
  `ψ=1-omega` は hanalyze の `ZeroInflatedBinomial` 自身の定義に由来)。

## Phase 90 A3 (vecIR ギャップ解消) との関係

このモデルは Phase 89 で「(a) DSL機能ギャップ: vecIR が ZeroInflatedBinomial
尤度を未サポート」としてフォールバック確定・実装ファイル未作成のまま
保留されていた。Phase 90 A3 で `IR.hs` に新規 vecIR family
(`VGZIBinom`/`SDZIBinom`/`VOZIBinom`。 04-low-dim-gauss-mix と共通の
`logSumExp2` 基盤を使用) を追加し、個体ごとの logit-link ランダム効果
(族 gather 経由) を含む形でも高速経路に載せた。

### 副次的に発見・修正した vecIR probe の設計限界

M=385 という「多数の個体ごと latent を持つ階層モデル」で実装を進めたところ、
`synthVecIR` が構造吸収 (`tryGroup`) には成功するのに数値probe
(`vecIRProbeOK`) だけ失敗して `Nothing` になる現象に遭遇した。実測で
根本原因を2つ特定・修正した:

1. **probe 添字の発散**: `vecIRProbeOK` の probe 値は `base + step·i`
   (`i` = 全 latent 通し番号)。 latent 数が多い階層モデルでは高 index の
   latent (例: `eps_raw_384`) の probe 値が発散する (例: `i=387` →
   `0.5+0.07*387≈27.6`)。 `i mod 16` で折り返し、 latent 数が増えても
   probe 値の広がりを一定に保つよう `IR.hs` を修正した (既存の「係数
   取り違え検出」という probe の目的は16通りの相異なる値で十分に保たれる・
   `cabal test` 全1356件PASS・既存モデルの回帰確認済)。
2. **`Uniform` 分布の probe 非安全性 (既存の既知課題が実害化)**:
   01-glm-poisson で既に文書化済みの「hanalyze の `Uniform` は制約変換が
   現状 unconstrained 扱い」という課題が、本モデルでは `mean_p` が
   `logit(mean_p)=log(meanP/(1-meanP))` という**非線形演算に直接使われる**
   ため、probe 時に `mean_p` が `(0,1)` 域外の値を取ると `log` に負数が
   渡り `NaN` が伝播して確実に probe 不一致になる、という形で実害化して
   発見された (01-glm-poisson では境界からの距離が十分で実害なしと記録
   されていたのと対照的)。`omega`/`mean_p` を数学的に同一の `Beta(1,1)`
   に置き換えることで解決 (Beta は hanalyze で実際に (0,1) へ写す変換を
   持つため probe 安全)。

いずれも Phase 90 のスコープである ZeroInflatedBinomial 実装そのものの
バグではなく、**多数の latent を持つ階層モデルで初めて表面化する既存の
設計限界**だった。今後同種のモデル (M が数百規模の random effect モデル)
に他プロジェクトが遭遇した際に再発しうるため、修正は `IR.hs` 側 (全モデル
共通) に入れた。

## ファイル

- `model.py` — PyMC 実装 (`pm.ZeroInflatedBinomial(psi=omega, n=T, p=...)`)。
- `Model.hs` — hanalyze 実装 (`ZeroInflatedBinomial` + `plateI` で個体ごとの
  `eps_raw` を宣言)。
- `run_pymc_matrix.py` — PyMC の CPU sampler×backend マトリクス
- `data/Mh_data.json` — posteriordb 由来データ (`M=385`・`T=5`・`y`)
- `figures/` — `hs_dashboard_full.png`/`py_dashboard_full.svg`
  (★`dashboardOf` = 健全性2x2パネルのみ使用。 `dashboardFullOf` は
  385個体分のtraceパネルを含み52MB超の非実用サイズになったため不採用
  — 「既知の課題」参照)

## 実行方法

```bash
bench/venv/bin/python bench/posteriordb/05-mh/model.py
bench/venv/bin/python bench/posteriordb/05-mh/run_pymc_matrix.py

cabal build --project-file=cabal.project.plot posteriordb-mh
cabal run   --project-file=cabal.project.plot posteriordb-mh
```

**★順次実行必須** (並行実行禁止・OOM対策は `.claude/skills/posteriordb-bench/SKILL.md`
step5 参照)。M=385個体×4chainのNUTSは数十秒かかる (下記「速度」参照)。

## 経路確認

`synthVecIR` = `Just` (データ束縛後のモデルで実測確認・Phase 90 A3 で
新設した `VGZIBinom` family + probe 修正が効いている)。

## 結果

### ★Phase 96 実測 (2026-07-17/18・このマシン・逐次単独・seed1)

runtime `gradPathLabel` = **vecIR** (A2 一括実測とも一致)。root:
`experiments/phase96-mh-reconfirm/` (a5-summary.tsv + 各 log)。

| system | wall (ms) | ess_bulk(ω) | ess/s | r̂(ω) |
|---|---:|---:|---:|---:|
| nutpie+numba (compile 込み) | 6447.2 | 386 | 59.9 | 1.010 |
| nutpie+numba (統一基準 = sampling のみ) | 3094.9-3159.1 | 248 | 78-80 | — |
| **hanalyze (vecIR・A5 `hbmWarmupInitMaxDepth=4`)** | **8950.4** | **429** | **47.9** | **1.005** |
| hanalyze (A5 適用前 fresh) | 9470.1 | 285 | 30.1 | 1.021 |

- **Phase 96 A5**: warmup 初期 (M=I 期) の deep tree (avg depth≈6・ε 鋸歯)
  が warmup の 32% を浪費していたため `hbmWarmupInitMaxDepth = Just 4` を
  適用。warmup evals **−25〜28%** (116-121k → 85-91k)・seed 1/2/3 で
  posterior 統計一致・ess/s の seed 分散も縮小 (30-60 の振れ → 47-51)。
- posterior は旧記録と整合 (omega 0.337±0.077 / mean_p 0.252±0.087 /
  sigma 1.439±0.404・収束やや困難の既知性質も同傾向)。ESS は Phase 100
  以降 essBulk (rank-normalized) 基準になり PyMC と同一指標で比較可能
  (旧記録の Geyer IMSE 注記は新数値には非適用)。
- **残ギャップ = per-eval 単価 2.1×** (53.9µs vs nutpie+numba ≈25.8µs・
  arena/vecIR kernel 3 本で prof 82.5%)。改善レバー (SIMD/FFI kernel 化) は
  **不実施方針** (2026-07-18) — 設計と根拠は
  `docs/dev-notes/ffi-simd-kernel-potential.md` に記録。

以下は Phase 90 起票時の旧記録 (Phase 90-102 の改善以前・**stale**。
速度表の wall/ESS は現状と乖離・精度表の平均値は現在も整合):

### 精度 (4 chain・warmup1000+draws1000・2者比較・reference無し)

| パラメータ | hanalyze | pymc (nutpie+numba・最速CPU) |
|---|---|---|
| omega  | 0.360 ± 0.099 | 0.39 ± 0.20 |
| mean_p | 0.230 ± 0.092 | 0.23 ± 0.11 |
| sigma  | 1.551 ± 0.451 | 1.6 ± 0.6 |

平均値は近いが、**両システムともこのモデルは収束がやや困難**
(r_hat: hanalyze 1.02-1.03・pymc 1.01-1.13。 ess_bulk: hanalyze 34-44・
pymc 24-386 とsampler依存で大きくばらつく)。BPA本 Ch.6 の Mh モデルは
個体ごとのランダム効果が弱識別になりやすい既知の性質を持つ (posteriordb
の分類基準でも「収束成功」リストの中では比較的難しい部類)。

### 速度 (4 chain・warmup1000+draws1000・CPU 1コア固定)

PyMC は sampler×backend 7 通りを計測。ess/sec (収束の質を加味した実効速度)
基準で最速を選定 (blackjax は他モデルと同型のエラーで失敗):

| system | wall (ms) | ESS (omega) | ESS/sec | rhat |
|---|---:|---:|---:|---:|
| nutpie + Numba (**PyMC最速CPU・ess/s基準**) | 14711.7 | 386 | 26.24 | 1.010 |
| numpyro | 19432.3 | 275 | 14.15 | 1.020 |
| nutpie + JAX | 26566.9 | 235 | 8.85 | 1.020 |
| pymc + CVM (真の C) | 30183.3 | 309 | 10.24 | 1.010 |
| pymc + JAX (own NUTS) | 66255.6 | 309 | 4.66 | 1.010 |
| pymc + Numba | 15708.4 | 24 | 1.53 | 1.130 |
| blackjax | (失敗・後述) | — | — | — |
| **hanalyze (Haskell・vecIR・サンプリングのみ)** | **16123.3** | 36-44 (要注記) | ≈2.3 | 1.02-1.03 |

wall clock だけ見ると hanalyze (16123ms) は PyMC最速 (nutpie+numba・
14712ms) とほぼ同等 (1.1倍程度) だが、ESS/sec では PyMC が優位。ただし
hanalyze の ESS は Geyer's IMSE (`Common.hs` 既知の注記) で PyMC の
rank-normalized bulk ESS と算出方法が異なるため、この比較は目安に留める。
M=385個体規模のモデルでの vecIR 実効速度改善は本 Phase の主眼 (ZeroInflated
Binomial を高速経路に載せること) の外にあり、今回は行っていない。

### 図

- **Haskell**: `hs_dashboard_full.png` — DAG は `plateI` の `eps_raw`
  385個体を `ind (385)` の1ボックスに集約表示 (実用的なサイズ)。
- **Python**: `py_dashboard_full.svg` — omega の trace に draw 650-900
  付近で明らかなモード漂流 (0.3付近↔0.9付近) が見える。

### 既知の課題

- 個体ごとランダム効果モデルの弱識別 (上記「結果」参照)。より長い chain・
  非中心化以外のパラメタ化・より informative な prior 等が正攻法だが、
  本 Phase のスコープ外。
- `dashboardFullOf` は M=385 で非実用サイズ (52MB) になる問題を発見。
  `dashboardOf` (健全性パネルのみ) で回避したが、大規模階層モデル向けに
  「param 名を絞った trace パネル」を選べる API が無いのは `Plot/Bayes.hs`
  側の一般的な制約 (本 Phase のスコープ外・将来の改善候補として記録)。
- blackjax エラーは他モデルと同型 (原因未解明・深掘りしない方針)。
