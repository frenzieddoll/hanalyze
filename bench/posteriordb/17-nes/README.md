# nes (`nes1972-nes`)

ARM本 (Gelman & Hill 2006 Ch.4) の政党支持度回帰。National Election
Studies 1972年調査 (N=1330)。9変数の線形回帰 (切片相当のbeta1込み・
イデオロギー・人種・年齢層3ダミー・教育・性別・収入)。詳細・出典は
`stan-dev/posteriordb` (`posterior_database/models/stan/nes.stan`・
`posterior_database/data/data/nes1972.json.zip`・`gelman2006data` 参照)。
**`reference_posterior_name: "nes1972-nes"`** (posteriordb に公式
reference あり・hanalyze/PyMC/公式referenceの3者比較可能)。

## Prior: Stan原典は暗黙のflat priorのため diffuse な代替を使用

Stan原典に明示的な prior 行は無い (暗黙の flat/improper prior)。
01-glm-poisson/10-rats と同じ流儀で `beta_i ~ Normal(0,1000)`
(UnconstrainedT・境界外初期値の罠なし)・`sigma ~ HalfCauchy(25)`
(10-ratsで確立したSD事前分布の安全パターン・`Uniform(0,X)`は使わない)
という diffuse な代替を hanalyze/PyMC 両方に与えた。

## 経路確認

★Phase 96 A2 (2026-07-17) 実測: runtime `gradPathLabel` = **「Gaussian LM
閉形式ブロック (解析勾配)」** (root:
`experiments/phase96-mh-reconfirm/pathlabels.tsv`)。Phase 91 A4 でも同旨を
確認済み (旧記録は生モデルを `synthVecIR` に渡した誤診断由来)。

以下は Phase 89 起票時の旧記録 (**stale**):

> `synthVecIR = Nothing` (legacy walk+ad へのフォールバック)。9変数の
> 標準的な線形回帰だが `VGGauss` 族単独では表現しきれない構造と見られる。

## 精度 (4 chain・warmup1000+draws1000・独立検証 = arviz・公式referenceと突合)

| パラメータ | hanalyze | pymc (numba・最速CPU) | 公式reference |
|---|---|---|---|
| beta1 (切片) | 1.779 ± 0.422 | 1.8 ± 0.4 | 1.774 |
| beta2 (real_ideo) | 0.486 ± 0.041 | 0.484 ± 0.042 | 0.484 |
| beta3 (race_adj) | -1.109 ± 0.197 | -1.112 ± 0.187 | -1.107 |
| beta4 (age30_44) | -0.190 ± 0.143 | -0.188 ± 0.143 | -0.188 |
| beta5 (age45_64) | -0.050 ± 0.143 | -0.048 ± 0.14 | -0.048 |
| beta6 (age65up) | 0.513 ± 0.184 | 0.509 ± 0.182 | 0.515 |
| beta7 (educ1) | 0.296 ± 0.060 | 0.295 ± 0.061 | 0.297 |
| beta8 (gender) | -0.008 ± 0.107 | -0.009 ± 0.101 | -0.0056 |
| beta9 (income) | 0.160 ± 0.050 | 0.16 ± 0.054 | 0.161 |
| sigma | 1.883 ± 0.036 | 1.883 ± 0.0375 | 1.882 |

3系統とも全パラメータ小数第2〜3位まで一致。R-hat = 1.00 (hanalyze/pymc
とも)。

## 速度 (4 chain・warmup1000+draws1000・CPU 1コア固定)

PyMC は sampler×backend 7 通りを計測 (blackjax は他モデルと同一の
`ValueError: cannot select an axis to squeeze` により失敗・原因未解明の
ため参考記録):

| system | wall (ms) | ESS (beta[0]・arviz計算) | ESS/sec | 対 hanalyze |
|---|---:|---:|---:|---:|
| **hanalyze (Haskell・legacy walk+ad)** | 12430.3 | (`Common.summarize` 参照) | — | 基準 |
| pymc + Numba (**PyMC最速CPU**) | 14450.6 | 2701 | 186.91 | 1.16× |
| nutpie + Numba | 8632.0 | 1141 | 132.18 | 0.69× (r_hat=1.010・僅かに収束劣化のため選定から除外) |
| numpyro | 17841.8 | 2014 | 112.88 | 1.44× |
| pymc + CVM (真の C) | 34670.3 | 2616 | 75.45 | 2.79× |
| nutpie + JAX | 51999.7 | 1203 | 23.13 | 4.18× |
| pymc + JAX (own NUTS) | 153246.7 | 2418 | 15.78 | 12.33× |
| blackjax | (失敗・後述) | — | — | — |

`synthVecIR=Nothing` (legacy walk+ad) にもかかわらず、**hanalyze
(12430.3ms) が PyMC最速CPU (pymc+numba・14450.6ms) の約1.16倍高速**
— N=1330×9変数という中規模データでは legacy経路のO(N)残差AD再計算
コストとGHCネイティブコードの実行効率が概ね拮抗した (02-dogsに近い
パターン)。nutpie+numbaはwall clockではhanalyzeより高速 (8632.0ms) だが
r_hat=1.010と僅かに収束劣化しているため「PyMC最速CPU」の選定からは
除外 (11-seedsと同じ判断基準)。

## 図 — 両側とも「フルダッシュボード」1 枚に統一

`figures/hs_dashboard_full.png` /
`figures/py_dashboard_full.svg`

(図はベンチ実行後に `figures/` へ生成される。リポジトリには含めていない)

## 既知の課題

- 特になし (要改善記録は無し・罠も踏まなかった)。blackjaxエラーのみ
  他モデルと同型 (原因未解明・深掘りしない方針)。
