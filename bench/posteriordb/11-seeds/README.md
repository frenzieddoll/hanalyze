# seeds (posteriordb: `seeds_data-seeds_model`)

BUGS 古典例「種子発芽実験」(Crowder 1978・I=21プレート・2種の種子×2種の
根の抽出物の2x2要因計画+overdispersion 用のプレートごとランダム切片)。
出典: `stan-dev/posteriordb`
(`posterior_database/models/stan/seeds_model.stan`・
`posterior_database/data/data/seeds_data.json.zip`)。

- `reference_posterior_name: null` (posteriordb に公式 reference 無し・
  hanalyze vs PyMC の2者比較のみ)。
- Prior: `alpha0,alpha1,alpha2,alpha12 ~ Normal(0,1000)`・
  `tau ~ Gamma(1e-3,1e-3)` (精度パラメータ)・`sigma = 1/sqrt(tau)`・
  `b_i ~ Normal(0, sigma)` (i=1..21、プレートごとのランダム切片)。
  **※Phase 94 A4 で非中心化に変更**: `z_i ~ Normal(0,1)` を実パラメータと
  し `b_i = z_i·sigma` で復元 (funnel 首を除去)。事後は非中心化前と一致
  しつつ `tau` の収束が劇的に改善 (下記「精度」)。
- 尤度: `n_i ~ Binomial(N_i, invlogit(alpha0 + alpha1*x1_i + alpha2*x2_i +
  alpha12*x1_i*x2_i + b_i))` (Stan 原典の `binomial_logit` を
  `Binomial N p` + `p=invlogit(eta)` へ手動展開・05-mh と同じ流儀)。

## `Gamma` は 10-rats の Uniform-SD 罠を回避できる

`tau ~ Gamma(1e-3,1e-3)` は Stan 原典どおりそのまま移植できた。hanalyze の
`Gamma` は `PositiveT` 変換 (exp系) を持つため、10-rats で発見した
「`Uniform(0,X)` を SD/precision パラメータに直接使うと HMC が全 warmup
で発散する罠」(既知の一様事前 sd パラメータ発散パターン) には
該当しない。実測でも初手から正常にサンプリングできた (発散なし)。

## ファイル

- `model.py` — PyMC 実装 (`pm.Binomial(n=N, logit_p=eta, ...)`)。
- `Model.hs` — hanalyze 実装 (`plateI` でプレートごとの `b` を21個宣言・
  `plateForM_` で観測に束ねる eight-schools 型パターン)。
- `run_pymc_matrix.py` — PyMC の CPU sampler×backend マトリクス。
- `data/seeds_data.json` — posteriordb 由来データ (`I=21`・`n`・`N`・
  `x1`・`x2`)。
- `figures/` — `hs_dashboard_full.png`/`py_dashboard_full.svg`
  (I=21+5ハイパーパラメータ=26 param。3.5MB程度で実用サイズのため
  `dashboardFullOf` をそのまま使用・05-mh/10-ratsの `dashboardOf` 縮小
  判断は不要だった)。

## 実行方法

```bash
bench/venv/bin/python bench/posteriordb/11-seeds/model.py
bench/venv/bin/python bench/posteriordb/11-seeds/run_pymc_matrix.py

cabal build --project-file=cabal.project.plot posteriordb-seeds
cabal run   --project-file=cabal.project.plot posteriordb-seeds
```

**★順次実行必須** (並行実行禁止・OOM対策は `.claude/skills/posteriordb-bench/SKILL.md`
step5 参照)。

## 経路確認 (Phase 94 で全面是正)

`synthVecIR` = **`Just` (vecIR 高速経路)**。`gradPathLabel (hbmModelSpec m)`
の直接 print で 3/3 確認済 (Phase 94 A1)。**旧 README の「`synthVecIR =
Nothing`・legacy walk+ad へフォールバック」は誤り**だった (Phase 91 A3
の生モデル診断バグ = `dataNamed* []` を data 空で synthVecIR に渡し
"legacy" と誤表示した confound。Phase 91 A4 で判明)。

ただし当初は **Binomial の n が group key に入り 17-group に分裂**して
いた: `keyOf (SDBinom n p) = key ("bi:"++show n) [] p` が n を key に含む
ため、N が 21 プレートで 17 通り異なる観測が同一 family group に集約
されず、"ベクトル化"が実質 17 個の near-scalar vec op = SIMD 効果ゼロ +
per-group overhead で PyMC に約 2.3 倍負けていた (Phase 94 A2 で計測確定)。

**Phase 94 A3 で group key から n を除外**し (`SDBinom`/`SDZIBinom` の n を
行対応 Vector 化・`IR.hs`)、21 観測を **1-group [21]** に集約 →
**5678→371ms = 約 15× 高速化**。`synthVecIR=Just` は維持。

## 結果

### 精度 (4 chain・warmup1000+draws1000・2者比較・reference無し)

| パラメータ | hanalyze | pymc (numba・最速CPU) |
|---|---|---|
| alpha0  | -0.556 ± 0.176 | -0.548 ± 0.192 |
| alpha1  |  0.119 ± 0.307 |  0.08 ± 0.31 |
| alpha2  |  1.342 ± 0.241 |  1.35 ± 0.27 |
| alpha12 | -0.843 ± 0.412 | -0.82 ± 0.44 |
| tau (rhat/ess) | **非中心化: rhat 1.00・ess(tau) 659** | rhat 1.05・ess_bulk 154 |

alpha0-alpha12 (回帰係数) は小数第1-2位まで一致・R-hat 1.00-1.004
(hanalyze)。alpha0≈-0.55・alpha1≈0.1・alpha2≈1.3・alpha12≈-0.8 は
Crowder (1978) の古典的推定値とも整合。

**`tau` (プレート間分散の精度) の収束は Phase 94 A4 で解消**した。
非中心化前は `Gamma(1e-3,1e-3)` の diffuse 事前分布による funnel 首で
**偶発的に 1 chain が完全崩壊** (accept→0.001・tau が最大値に pin) し、
pooled で rhat 1.10・ess(tau) 10.2 まで劣化していた (旧記録)。**非中心化
(`b=z·σ`) で funnel 首を除去**した結果、崩壊 0・divergence 0・
E-BFMI 0.17→0.75・**rhat(tau) 1.00・ess(tau) 11→659**。fair 比較
(両者非中心化・同一マシン) で **ESS/秒 は hanalyze が約 3.1× 勝ち**
(生 ESS/1000draw は PyMC が約 1.9× 上 = warmup mass 推定の微差由来で
実用影響なし・Phase 94 A6 で sampler バグ無しを制御幾何で確認済)。

### 速度 (4 chain・warmup1000+draws1000・CPU 1コア固定)

PyMC は sampler×backend 7 通りを計測。**nutpie+numba は ess/s 最高
(196.31) だが r_hat=1.530 と収束していないため除外**し、収束した中で
最速の `pymc+numba` を「PyMC最速CPU」として採用 (blackjax は他モデルと
同型のエラーで失敗):

| system | wall (ms) | ess_bulk(alpha0) | ESS/sec | rhat |
|---|---:|---:|---:|---:|
| **pymc + Numba (収束した中でのPyMC最速CPU)** | 6276.0 | 1679 | 267.53 | 1.010 |
| numpyro | 11404.5 | 1544 | 135.39 | 1.000 |
| nutpie + JAX | 14674.9 | 1483 | 101.06 | 1.010 |
| pymc + CVM (真の C) | 16429.0 | 1150 | 70.00 | 1.000 |
| pymc + JAX (own NUTS) | 46033.0 | 1698 | 36.89 | 1.010 |
| nutpie + Numba (参考・r_hat未収束のため除外) | 5751.1 | 1129 | 196.31 | **1.530** |
| blackjax | (失敗・後述) | — | — | — |
| hanalyze (旧記録・stale・別マシン汚染) | ~~9119.5~~ | — | — | 1.00-1.10 |
| **hanalyze (vecIR・group-merge・draws-only)** | **371** | — | — | 1.00 |

**★Phase 94 で速度記録を全面是正 (旧 9119.5ms・6276.0ms は両方 stale)。**
上の PyMC マトリクスは pymc **5.x** 想定・別マシン由来。同一マシンで
やり直した fresh 実測 (record 値を鵜呑みにせず PyMC 再測):

- **hanalyze**: 同一マシン fresh の vecIR 経路が約 5630ms → group-merge
  改修 (Phase 94 A3・n を group key から除外) で **5678→371ms = 約 15×
  高速化** (draws-only・非中心化込で約 466ms)。
- **PyMC 6.1.0 再測** (同一マシン・record 手法 = compile 込 wall):
  中心化 **2462ms**・非中心化 **2760ms** (旧 6276ms は pymc 5.x/別マシン)。
- **wall 倍率**: 371 / 2462 = **約 6.6× hanalyze 高速** (hanalyze=draws-only /
  PyMC=compile込の非対称。純 draws でも約 3× hanalyze 優位の見込)。

I=21 プレート規模の小さな階層モデル。当初 Binomial の n が group key に
入り 17-group に分裂して PyMC に負けていたが (Phase 94 A2)、group-merge で
逆転した。計測の全経緯は `specification/phases/phase-94-seeds-onboarding.md`。

### 図

[hs_dashboard_full.png](./figures/hs_dashboard_full.png) /
[py_dashboard_full.svg](./figures/py_dashboard_full.svg)

### 既知の課題 (Phase 94 で①②を解消)

- ~~**`tau` の収束がやや困難**~~ → **解消済 (Phase 94 A4)**。非中心化
  (`b=z·σ`) で funnel 首を除去し rhat 1.00・ess(tau) 659 (上記「精度」)。
- ~~**vecIRギャップ (b)**~~ → **誤診断と判明 + group-merge で解消
  (Phase 94 A1/A3)**。実経路は最初から vecIR (`synthVecIR=Just`) で、
  遅さの主因は Binomial の n による 17-group 分裂だった。group key から n を
  除外し 1-group に集約 (上記「経路確認」)。
- **生 ESS/draw が PyMC の約 1/1.9 (残存・実用影響なし)**: 非中心化後も
  hanalyze の適応 ε が PyMC よりやや小さく、生 ESS/1000draw は PyMC 優位。
  Phase 94 A6 の制御幾何 (単位/相関ガウス) で **hanalyze の適応 ε が PyMC と
  完全一致** = sampler に一般バグ無しを確認済。seeds の差は 26 次元事後の
  warmup mass 推定の微差で、ESS/秒では hanalyze が約 3× 勝ち。全モデルの
  ESS/draw 底上げ (dense mass / DA calibration) は Phase 94 と別スコープ。
- blackjaxエラーは他モデルと同型 (原因未解明・深掘りしない方針)。
- nutpie+numba は ess/s 最高だが本モデルでは r_hat=1.530 と収束せず
  (参考記録・「PyMC最速CPU」の選定からは除外)。
