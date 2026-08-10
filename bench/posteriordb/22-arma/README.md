# arma (`arma-arma11`)

ARMA(1,1) 時系列 (T=200)。AR成分とMA成分を併せ持つ時系列モデル
(12-arkの純AR・03-garch11の再帰的分散とは異なる構造)。出典:
`stan-dev/posteriordb` (`posterior_database/models/stan/arma11.stan`・
`posterior_database/data/data/arma.json.zip`)。
**`reference_posterior_name: "arma-arma11"`** (posteriordb に公式
reference あり・hanalyze/PyMC/公式referenceの3者比較可能)。★新ファミリ:
AR+MA複合時系列 — Phase 89 で初めて扱うモデル種別。

## Prior・尤度 (Stan原典どおり)

- `mu ~ Normal(0,10)`・`phi ~ Normal(0,2)`・`theta ~ Normal(0,2)`・
  `sigma ~ HalfCauchy(2.5)`
- 逐次再帰: `nu[1] = mu + phi*mu`・`err[1] = y[1]-nu[1]` (err[0]=0とみなす)。
  t=2..T: `nu[t] = mu + phi*y[t-1] + theta*err[t-1]`・`err[t] = y[t]-nu[t]`
- 尤度: `err ~ Normal(0, sigma)`

`err[t]` が `err[t-1]` に依存する逐次再帰 (14-hmm-exampleと同系統) の
ため `observe` を使わず `potential` で尤度を直書きした。 Haskellでは
`Data.List.mapAccumL` で (前時刻のy, 前時刻のerr) を引き回しながらerr列
を構築する。

## ★PyMC実装時に踏んだ罠: `pytensor.scan`の勾配計算バグ2件

1. **`y[1:]`/`y[:-1]` を素のnumpy配列のままscanに渡すとTypeError**:
   `pytensor.scan`のn_steps推定が `TensorVariable cannot be converted
   to Python integer` で失敗した。`pt.as_tensor_variable(y)` で明示的に
   pytensor tensorへ変換してから slicing することで解消。
2. **`pm.logp(pm.Normal.dist(...), ...)` をscan内部の逐次再帰結果に
   適用すると `AttributeError: 'RandomGeneratorVariable' object has no
   attribute 'shape'`**: scanのgradient (pullback) 計算が RandomVariable
   ノードと衝突するバグと見られる (pytensor/pymcのバージョン起因)。
   Normal対数密度を `-0.5*log(2π) - log(sigma) - 0.5*(err/sigma)²` の
   直接手計算に置換することで回避 (RandomVariableノードを一切生成
   しない)。
3. **`mu`/`phi`/`theta` を`step`関数のclosureで直接参照するとscanの
   gradientが壊れる**: `non_sequences=[mu, phi, theta]` として明示的に
   scanへ渡す必要があった (pytensor.scanのベストプラクティス・暗黙の
   closure参照は勾配追跡が不完全になるケースがある)。

いずれもPhase89の他モデルでは初めて遭遇した罠 (14-hmm-exampleは
`pm.Potential`のみでscanの勾配問題には当たらなかった)。

## 経路確認

★Phase 101 A2 (2026-07-17) 以降: `gradPathLabel` = **「ARMA(1,1) 逆向き随伴の
閉形式 (Phase 101)」** (構造化 primitive `ArmaNormal` + `armaAnalyticVG`)。
それ以前は `synthVecIR = Nothing` (legacy walk+ad へのフォールバック) —
逐次再帰 (err[t]がerr[t-1]に依存) が構造的にvecIR対象外 (14-hmm-exampleと同型)。

## 精度 (4 chain・warmup1000+draws1000・独立検証 = arviz・公式referenceと突合)

| パラメータ | hanalyze | pymc (numba・最速CPU) | 公式reference |
|---|---|---|---|
| mu | 0.0068 ± 0.0110 | 0.007 ± 0.0115 | 0.00691 |
| phi | 0.9562 ± 0.0230 | 0.9575 ± 0.0226 | 0.95701 |
| theta | -0.0331 ± 0.0598 | -0.035 ± 0.059 | -0.03370 |
| sigma | 0.1667 ± 0.0089 | 0.167 ± 0.0085 | 0.16648 |

3系統とも全パラメータ小数第3〜4位まで一致。R-hat = 1.00 (hanalyze/pymc
とも)。

## 速度 (4 chain・warmup1000+draws1000・CPU 1コア固定)

PyMC は sampler×backend 7 通りを計測 (blackjax は他モデルと同一の
`ValueError: cannot select an axis to squeeze` により失敗・原因未解明の
ため参考記録):

| system | wall (ms) | ESS (phi・arviz計算) | ESS/sec | 対 hanalyze |
|---|---:|---:|---:|---:|
| **hanalyze (Haskell・legacy walk+ad)** | 13097.1 | (`Common.summarize` 参照) | — | 基準 |
| pymc + Numba (**PyMC最速CPU**) | 7216.9 | 4500 | 623.53 | 0.55× (PyMCが約1.81倍高速) |
| nutpie + JAX | 7825.2 | 5081 | 649.32 | 0.60× |
| numpyro | 8159.2 | 3759 | 460.70 | 0.62× |
| nutpie + Numba | 8932.0 | 5136 | 575.01 | 0.68× |
| pymc + JAX (own NUTS) | 20152.1 | 4395 | 218.09 | 1.54× |
| pymc + CVM (真の C) | 72629.1 | 4618 | 63.58 | 5.55× |
| blackjax | (失敗・後述) | — | — | — |

`synthVecIR=Nothing` (legacy walk+ad) のため、**本モデルでもhanalyze
(13097.1ms) が PyMC最速CPU (pymc+numba・7216.9ms) より遅い** (約1.81倍)
— T=200の逐次再帰 (err[t-1]依存) を毎leapfrogステップでO(T)の残差AD
再計算しており、14-hmm-example (forward algorithm・約4.1倍遅い) や
20-bones (入れ子ループ・約2.0倍遅い) と同系統の「逐次/入れ子構造で
hanalyzeが劣後する」パターンの3例目。

## ★Phase 101 A2 改善後 (2026-07-17・このマシン fresh・4chain warmup1000+draws1000)

尤度を `mapAccumL + potential` 直書きから構造化 primitive **`ArmaNormal`** +
`observeMV` へ移行 (密度は同値・LogpSpec で同値性テスト済)。勾配は
`armaAnalyticVG` = 逆向き 1 パスの閉形式随伴 (ē_t = −e_t/σ² − θ·ē_{t+1}・
AD tape ゼロ)。

| system | wall (ms) | ess_bulk(phi) | ESS/sec |
|---|---:|---:|---:|
| **hanalyze (ArmaNormal 閉形式随伴)** | **376.4** (sampling wall) | 6486.8 | **17,233** |
| hanalyze 改善前 (fresh 同日再測) | 8097.2 | 9335.6 | 1,153 |
| nutpie + JAX (PyMC 最速・compile 込み) | 1539.1 | 5081 | 3,301 |
| numpyro | 1932.2 | 3715 | 1,923 |
| pymc + Numba | 4847.4 | 4261 | 879 |
| nutpie + Numba | 5567.1 | 5136 | 923 |

- **hanalyze が全系統最速: wall 4.1× 勝ち・ESS/sec 5.2× 勝ち** (対 nutpie+jax)。
- sampling wall 統一基準 (compile 除外・Phase 92 B4 と同基準・nutpie 直叩き seed1):
  nutpie+jax sample(tune+draws) = **1219.8ms** (compile 562.4ms)・ess_bulk(phi) 4294
  → それでも hanalyze が **wall 3.2× / ESS/sec 4.9× (17,233 vs 3,521) 勝ち**。
- 上の旧記録表 (13097.1/7216.9ms 等) は**別マシン・改善前**の値 (stale)。
- posterior は公式 reference 一致を維持 (mu 0.0069/phi 0.9565/theta −0.0334/
  sigma 0.1665)。
- 詳細: `specification/phases/phase-101-bones-arma-speed.md`

## 図 — hanalyze側「フルダッシュボード」・PyMC側も同様

`potential`のみで尤度を構成 (observed nodeが無い) ため、両側ともPPC
パネルは空。

[hs_dashboard_full.png](./figures/hs_dashboard_full.png) /
[py_dashboard_full.svg](./figures/py_dashboard_full.svg)

## 既知の課題

- ~~逐次再帰構造でhanalyzeがPyMC最速CPUより約1.81倍遅い~~ →
  **Phase 101 A2 (2026-07-17) で解消**: `ArmaNormal` 閉形式随伴で 21.5× 高速化・
  PyMC 最速 (nutpie+jax) に wall 4.1× / ESS/sec 5.2× 勝ち (上表)。
- PyMC側 `pytensor.scan`のgradient計算バグ2件 (上記「踏んだ罠」参照)。
  RandomVariableノードを経由しない直接手計算+`non_sequences`明示で
  回避済み。
- blackjaxエラーは他モデルと同型 (原因未解明・深掘りしない方針)。
