# bones (`bones_data-bones_model`)

骨年齢の graded response IRT モデル (BUGS 古典例)。nChild=13人の子供・
nInd=34項目 (骨のX線指標)。各項目の困難度カットポイント (gamma) と
識別力 (delta) は**固定データ** (未サンプル)・各子供の能力 theta のみが
latent (13次元と極小)。出典: `stan-dev/posteriordb`
(`posterior_database/models/stan/bones_model.stan`・
`posterior_database/data/data/bones_data.json.zip`)。
**`reference_posterior_name: null`** (posteriordb に公式 reference 無し・
2者比較のみ)。★新ファミリ: 順序ロジット (graded response IRT) — Phase 89
で初めて扱うモデル種別。

## Prior・尤度 (Stan原典どおり)

- `theta[i] ~ Normal(0, 36)` (i=1..13)
- 各項目 j (delta[j]=識別力・gamma[j][1..ncat[j]-1]=カットポイント、
  いずれもデータ):
  `Q[i,j,k] = invlogit(delta[j]*(theta[i]-gamma[j,k]))`・
  `p[i,j,1]=1-Q[i,j,1]`・`p[i,j,k]=Q[i,j,k-1]-Q[i,j,k]`・
  `p[i,j,ncat[j]]=Q[i,j,ncat[j]-1]`
- 尤度: `grade[i,j] != -1` の行だけ `target += log(p[i,j,grade[i,j]])`
  (欠測 `-1` はスキップ)

`observe` を使わず `potential` で尤度を直書きする (14-hmm-example/16-lda
と同系統)。difficulty/discriminationは微分対象ではないデータなので
Haskell 側では素の `[Double]`/`[[Double]]` として closure で渡した。

## ★PyMC側の合成ダッシュボードに罠: PPCパネルが無いモデルでKeyError

`potential` のみで尤度を構成し `observe`/`pm.Normal(...,observed=...)` が
一切無いモデルでは、`idata` に `posterior_predictive`/`observed_data`
groupが存在せず、`bench/posteriordb/_common.py::make_pymc_dashboard` の
`az.plot_ppc_dist(idata)` が `KeyError: 'Could not find node at
posterior_predictive'` で落ちることが判明した (14-hmm-example/16-lda は
`observe`こそ使わないが `dataNamedX`経由の"y"がhanalyze側では便宜上PPC
パネル対象になっていたため気づかなかった潜在バグ)。`_common.py`を
修正し、`KeyError`を捕捉してPPCパネルを空欄 (hanalyze側`dashboardOf`の
「PPCパネルが空」と同じ扱い) にするようにした。全モデル共有ヘルパの
改修のため、今後同様の完全potential-onlyモデルでも自動的に対応する。

## 経路確認

★Phase 101 A3 (2026-07-17) 以降: `gradPathLabel` = **「graded response IRT
解析勾配 (Phase 101)」** (構造化 primitive `GradedResponseIrt` +
`gradedIrtAnalyticVG`)。それ以前は `synthVecIR = Nothing` (legacy walk+ad へ
のフォールバック) — nChild×nInd の入れ子ループ+可変長カテゴリ構造
(`ncat[j]`が2〜5で変動) がvecIR未対応と見られる。

## 精度 (4 chain・warmup1000+draws1000・独立検証 = arviz)

| パラメータ | hanalyze | pymc (numba・最速CPU) |
|---|---|---|
| theta_0 | 0.333 ± 0.197 | 0.33 ± 0.207 |
| theta_6 | 6.475 ± 0.575 | 6.44 ± 0.586 |
| theta_9 | 11.998 ± 0.670 | 11.94 ± 0.67 |
| theta_12 | 16.991 ± 0.749 | 16.96 ± 0.76 |

全13人のtheta推定が両系統で小数第1〜2位まで一致 (13人分の詳細は
`Common.summarize`出力参照)。R-hat = 1.00 (両系統とも)。

## 速度 (4 chain・warmup1000+draws1000・CPU 1コア固定)

PyMC は sampler×backend 7 通りを計測 (blackjax は他モデルと同一の
`ValueError: cannot select an axis to squeeze` により失敗・numpyroは
異常に遅い(763.9秒・入れ子Pythonループでのtracing起因と見られる)・
いずれも原因未解明のため参考記録):

| system | wall (ms) | ESS (theta[0]・arviz計算) | ESS/sec | 対 hanalyze |
|---|---:|---:|---:|---:|
| **hanalyze (Haskell・legacy walk+ad)** | 31272.9 | (`Common.summarize` 参照) | — | 基準 |
| pymc + Numba (**PyMC最速CPU**) | 15660.9 | 6928 | 442.38 | 0.50× (PyMCが約2.0倍高速) |
| nutpie + Numba | 29454.0 | 6151 | 208.83 | 0.94× |
| nutpie + JAX | 30854.0 | 6151 | 199.36 | 0.99× |
| pymc + CVM (真の C) | 32303.4 | 5737 | 177.60 | 1.03× |
| pymc + JAX (own NUTS) | 47082.1 | 6267 | 133.11 | 1.51× |
| numpyro | 763903.8 | 9662 | 12.65 | 24.4× (異常値・参考記録) |
| blackjax | (失敗・後述) | — | — | — |

`synthVecIR=Nothing` (legacy walk+ad) のため、**本モデルでは唯一
hanalyze (31272.9ms) が PyMC最速CPU (pymc+numba・15660.9ms) より遅い**
(約2.0倍) 結果になった — nChild=13×nInd=34の入れ子ループ (最大442項)
+ 可変長カテゴリ分岐を含む`potential`の対数尤度計算が、legacy walk+ad
の残差AD再計算コストとして効いていると見られる (詳細な内訳は未計測)。
Phase89でhanalyzeが劣後した数少ない事例 (06-irt-2plに次ぐ・共に
IRT系モデル)。

## ★Phase 101 A3 改善後 (2026-07-17・このマシン fresh・4chain warmup1000+draws1000)

尤度を `logCatProb + potential` 直書きから構造化 primitive
**`GradedResponseIrt`** + `observeMV` へ移行 (密度は同値・欠測 −1 skip 込で
LogpSpec 同値性テスト済)。勾配は `gradedIrtAnalyticVG` = dQ/dθ = δ·Q(1−Q) の
隣接差の解析勾配 (AD tape ゼロ)。Phase 101 A1 profile では旧 `logCatProb` が
64.8% time / 73.2% alloc を占めていた。

| system | wall (ms) | ess_bulk(theta[0]) | ESS/sec |
|---|---:|---:|---:|
| **hanalyze (GradedResponseIrt 解析勾配)** | **2992.7** (sampling wall) | 5298.8 | **1,771** |
| hanalyze 改善前 (fresh 同日再測) | 18332.4 | 6298.8 | 344 |
| nutpie + JAX (PyMC 最速・compile 込み) | 10774.3 | 6151 | 571 |
| nutpie + Numba | 10915.2 | 6151 | 564 |
| pymc + Numba | 31516.2 | 7944 | 252 |

- **hanalyze が全系統最速: wall 3.6× 勝ち・ESS/sec 3.1× 勝ち** (対 nutpie+jax)。
- sampling wall 統一基準 (compile 除外・Phase 92 B4 と同基準・nutpie 直叩き seed1):
  nutpie+jax sample(tune+draws) = **4308.7ms** (compile 4988.9ms = wall の半分!)・
  ess_bulk(theta[0]) 6077 → それでも hanalyze が **wall 1.44× / ESS/sec 1.26×
  (1,771 vs 1,410) 勝ち**。
- 上の旧記録表 (31272.9/15660.9ms 等) は**別マシン・改善前**の値 (stale)。
  旧記録の「PyMC最速 = pymc+numba」もこのマシンでは成立しない (31516ms と最遅級)。
- posterior は旧記録・PyMC と一致を維持 (theta_0 0.324±0.204・theta_12
  16.959±0.745)。
- 詳細: `specification/phases/phase-101-bones-arma-speed.md`

## 図 — hanalyze側「健全性2x2パネル」・PyMC側は「フルダッシュボード」

`potential`のみで尤度を構成 (observed nodeが無い) ため、両側ともPPC
パネルは空。

[hs_dashboard_full.png](./figures/hs_dashboard_full.png) /
[py_dashboard_full.svg](./figures/py_dashboard_full.svg)

## 既知の課題

- ~~本モデルではhanalyzeがPyMC最速CPUより約2.0倍遅い~~ →
  **Phase 101 A3 (2026-07-17) で解消**: `GradedResponseIrt` 解析勾配で 6.1×
  高速化・PyMC 最速 (nutpie+jax) に wall 3.6× / ESS/sec 3.1× 勝ち (上表)。
- PyMC側 numpyroが異常に遅い (763.9秒): 入れ子Pythonループでの
  tracingコストと見られるが未確認・深掘りしない方針。
- blackjaxエラーは他モデルと同型 (原因未解明・深掘りしない方針)。
