# PyMC サンプラー×勾配バックエンドの全組み合わせベンチマーク (Phase 88)

Phase 84-87 で「PyMC-C」と呼んできた比較対象の実体を検証したところ、
実際は **PyMC 既定 (backend 未指定) = PyTensor の既定 linker = Numba** であり、
真の C/Cython や他のサンプラー実装 (nutpie/blackjax) とは一度も比較していな
かったことが判明した (venv 直接検証・詳細は
`specification/phases/phase-88-pymc-sampler-backend-matrix.md`)。本 doc は
その正確な比較を新規に行った結果 (**既存 `PYMC_BENCHMARK.md` は改変しない**)。

## 計測条件

- 題材: radon (919 obs・相関 varying intercept+slope・Phase 84 flagship)。
- grid = iter [50,100,200,400]・warmup=500・reps=2 (radon 既定)・
  target_accept=0.8・max_treedepth=10。
- CPU 1 コア固定 (`taskset -c 0`)・`OMP/OPENBLAS/MKL_NUM_THREADS=1`・
  `JAX_PLATFORMS=cpu`・`JAX_ENABLE_X64=1`。同日連続実行 (交互 A/B ではなく
  順次実行・環境ドリフトの影響は残る可能性に留意)。
- 環境: `bench/venv` (PyTensor 3.1.2・PyMC 6.1.0・nutpie 0.16.11・
  blackjax 1.5 を本 Phase で追加インストール)。
- **`nuts_sampler` は全ケースで明示指定** (nutpie インストール後の自動選択の
  罠を回避・詳細は phase doc「重大発見」節)。

## 結果 (iter=400・定常状態の代表点)

| system | wall (ms) | ESS | ESS/sec | 対 hanalyze |
|---|---:|---:|---:|---:|
| **hanalyze (Haskell・自作 IR コンパイル)** | 3533 | 400 | **113.2** | 基準 (1.00×) |
| nutpie + JAX 勾配 | 3339 | 379 | 113.5 | 1.00× (実質同着) |
| pymc 既定 NUTS + **C/CVM (真の C)** | 4664 | 486 | 104.2 | 1.09× |
| pymc 既定 NUTS + Numba (**旧「PyMC-C」の実体**) | 5601 | 367 | 65.5 | 1.73× |
| nutpie + Numba (既定) | 4426 | 284 | 64.2 | 1.76× |
| numpyro (NumPyro 自身の NUTS + JAX) | 4831 | 271 | 56.1 | 2.02× |
| pymc 既定 NUTS + JAX 勾配 | 9729 | 397 | 40.8 | 2.77× |
| blackjax | 6153 | 147 | 23.9 | 4.74× |

「対 hanalyze」= hanalyze の ESS/sec ÷ 当該 system の ESS/sec (小さいほど hanalyze
に近い・1.00× = 同着・>1× は hanalyze が優位)。

## 精度 (floor 係数の事後平均・iter=400)

全 system で -0.61 〜 -0.63 の範囲に収まり、MC 誤差内で一致
(hanalyze -0.613・pymc+numba -0.62・pymc+cvm -0.622・pymc+jax -0.615・
numpyro -0.61・nutpie+numba -0.619・nutpie+jax -0.612・blackjax -0.62)。

## 主な発見

1. **hanalyze は「真の C」(CVM) にも僅差で先行し (1.09×)、全組み合わせ中
   最速の nutpie+JAX と実質同着 (1.00×)**。旧来「PyMC-C の 0.35〜0.99×」と
   呼んでいた比較は、実際には PyMC+**Numba** という中位の組み合わせに対する
   ものだった。真の C 相手でも hanalyze が優位という結果は Phase 84-87 時点の
   認識より良い結果である。
2. **tree depth は全 system で ~4.0 に揃っており**、NUTS の適応が収束した
   後の比較として妥当 (幾何の違いによる比較不公平は無い)。
3. **PyMC 既定 NUTS + JAX 勾配 (compile_kwargs={"mode":"jax"}) は最も遅い
   組み合わせの一つ**。NumPyro 自身の NUTS 実装 (numpyro) より PyMC の
   NUTS ループ + JAX 勾配のほうが更に遅く、JAX の速さは「勾配コンパイル」
   だけでなく「NUTS ループ自体の実装」にも依存することを示唆する
   (原因分析は本 Phase の範囲外・追加調査候補)。
4. **blackjax が最も遅い**。デフォルト設定 (warmup adaptation 等) が他と
   異なる可能性があり、追加のチューニング余地がある (本 Phase では既定設定
   のみ計測)。
5. **nutpie は既定で Numba 勾配だが、JAX 勾配へ切り替えると 1.7× 速くなる**
   (4426ms→3339ms)。Rust 実装の NUTS ループ自体が軽量なため、勾配側の
   ボトルネックがより素直に効く。

## 追補: iter=1600 (実運用規模) での再計測 (2026-07-11)

iter=400 は当プロジェクト独自の短め grid (radon が重いため Phase 84 で選択・
外部的な「定番」ではない) であり、**ユーザ指摘により実運用に近い iter=1600
(PyMC 自身の既定 draws=1000 より大きい規模) で改めて全 8 通り (hanalyze 含む)
を計測した**。Phase 87 で iter400→1600 だけで hanalyze 対 PyMC-C 比が
0.80×→0.49-0.68× に動いた前例があり、短い iter での比較は結果を歪めうる
ことが分かっていたための追加計測。

| system | wall (ms) | ESS | ESS/sec | 対 hanalyze |
|---|---:|---:|---:|---:|
| **hanalyze (Haskell・自作 IR)** | 3858 | 1600 | **414.8** | 基準 (1.00×) |
| pymc + Numba (旧「PyMC-C」の実体) | 4548 | 1537 | 337.9 | 1.23× |
| nutpie + JAX | 6800 | 1646 | 242.1 | 1.71× |
| nutpie + Numba (既定) | 6373 | 1260 | 197.7 | 2.10× |
| pymc + C/CVM (真の C) | 7848 | 1540 | 196.2 | 2.11× |
| numpyro | 8991 | 925 | 102.9 | 4.03× |
| pymc + JAX (own NUTS) | 15840 | 1544 | 97.5 | 4.25× |
| blackjax | 8316 | 786 | 94.5 | 4.39× |

**iter=400 とは順位が入れ替わる**: iter=400 では nutpie+JAX (1.00×) と
pymc+CVM (1.09×) が hanalyze と拮抗していたが、**iter=1600 では hanalyze が
全組み合わせ中で最速** (2 位の pymc+Numba に 1.23× 差)。nutpie+JAX・pymc+CVM
はむしろ 1.7〜2.1× まで後退する。これは Phase 87 (sampling 定常コストの
改善・value_and_grad 融合と端点勾配キャッシュ) の効果が短い iter では
warmup 固定費に埋もれて見えにくく、draw 数を伸ばすほど hanalyze の定常
per-draw 効率の高さが表に出るため (Phase 87 doc 参照)。**iter=400 は
hanalyze にとって「不利」でこそあれ「有利」ではなかった** — むしろ短い
iter は JIT 系 (JAX 勾配・nutpie 等) がコンパイル固定費を相対的に
償却しやすい区間であり、長い iter ほど当方の定常最適化の効果が伸びる。
精度は全 system -0.611〜-0.622 で iter=400 同様 MC 誤差内一致。

## 追補2: 収束の独立検証 (2026-07-11・ユーザ懸念への回答)

「モデルが hanalyze 有利になっていないか」「本当に正しい分布に収束しているか
(それっぽいだけで速く見えているのでは)」というユーザ懸念に対し、hanalyze
自身のコードを使わない独立検証を行った。

### モデル定義の照合

`designHBMProgram` の `correlatedRE` (τ_c ~ HalfNormal(5) × LKJ 相関
コレスキー × z ~ N(0,1)、非中心化) と PyMC の
`pm.LKJCholeskyCov(sd_dist=HalfNormal(5))` はソースレベルで数学的に同一
パラメータ化であることを確認。片方に有利な簡略化は見つからなかった。

### 独立検証手順

radon を **4 chain (別 seed) × iter=1600・warmup=500** で実行し、hanalyze の
生の draw を CSV に書き出して **arviz (hanalyze 自身の ess/rhat 実装は不使用)**
で再計算。同条件の PyMC 4-chain 結果と突合した。

| パラメータ | hanalyze (arviz 独立計算) | PyMC (4 chain・同ツール) |
|---|---|---|
| floor | -0.615 ± 0.085 | -0.615 ± 0.087 |
| Intercept | 1.492 ± 0.036 | 1.491 ± 0.036 |
| uranium | 0.742 ± 0.089 | 0.740 ± 0.090 |
| sigma | 0.721 ± 0.018 | 0.721 ± 0.018 |
| tau (切片RE) | 0.128 ± 0.051 | 0.130 ± 0.050 |
| tau (傾きRE) | 0.341 ± 0.121 | 0.339 ± 0.121 |
| 相関係数 | 0.07 ± 0.36 | 0.09 ± 0.36 |

**7 パラメータ全て小数第2〜3位まで一致**。R-hat も全パラメータ 1.00〜1.01
(4 独立チェーンによる真の収束診断)。「それっぽく見えるだけ」ではなく、
同一の事後分布を捉えていることを確認した。

### 副産物の発見: hanalyze 自身の ESS 表示は下限値 (クランプの影響)

上表とは別に、hanalyze 自身の `ess` 関数 (Geyer's Initial Monotone Sequence
Estimator・`src/hanalyze/Analyze/Stat/MCMC.hs`) は `tau = max 1 (...)` で
下限をクランプしており、**理論上 ESS が draw 数 n を超える場合でも n で
頭打ちにして報告する**。今回の radon iter1600 (1 chain, n=1600) で:

| パラメータ | hanalyze 自身の `ess` (クランプあり) | arviz (クランプなし・独立計算・4chain合計n=6400) |
|---|---:|---:|
| floor | 1600 (=n・クランプ発動) | 7379 |
| sigma | 1600 (=n・クランプ発動) | 8046 |
| tau (傾きRE) | 335 (クランプ非発動) | 1305 |

`tau<1` (= ESS>n) は NUTS が良く調整され隣接 draw 間にわずかな負の自己相関
が生じる (「超効率的サンプリング」) ときに起こりうる現象で、fixed effect 系
(floor/uranium/sigma) で発生し correlated RE (tau/相関) では発生しない
(幾何の単純さと対応)。バグではないが、**「hanalyze の ess=1600」という
これまでの表示は真の効率の保守的な下限値**であり、本文執筆時は誤解を招か
ないよう明記する必要がある。

### 参考: PPL 横断ベンチマークの外部知見 (需要ドリブンで次 Phase 化検討)

- PyMC Labs 公式ベンチ (Bradley-Terry 階層モデル・160,420 試合・
  warmup1000+draws1000×4chain): JAX CPU が PyMC/Stan 比 ESS/sec 2.9×・
  JAX GPU vectorized で 11×。事後平均は全手法で実質同一と明記。
  (https://www.pymc-labs.com/blog-posts/pymc-stan-benchmark)
- **posteriordb** (stan-dev 公式・PPL 業界の標準ベンチマーク基盤・120 モデル
  + 参照事後分布・フレームワーク非依存設計): 自作モデルだけでなく業界標準
  モデル群で横断比較する場合の一次候補。(https://github.com/stan-dev/posteriordb)

## 既知の制約・注意点

- iter=50 の wall time は初回コンパイル固定費が支配的で外れ値が出やすい
  (例: pymc+cvm iter=50 で 18971ms)。iter=400 での定常状態比較に加え、
  iter=1600 (実運用規模) でも追加計測済み (上記追補節)。
- reps=2 (radon 既定) ゆえ run-to-run のばらつきは M 系グリッドより大きい。
  厳密な確定には複数日の交互 A/B が望ましいが、本 Phase は探索的な
  一次計測として同日順次実行のみ行った。
- nutpie の `idata.sample_stats` には `tree_depth` キーが無く (`depth` という
  別名)、既存 `_summarize()` は `tree_depth` を明示的に見るため nutpie 系の
  tree_depth 列は `nan` になる (実測値そのものには影響なし・単なる集計欠落)。
- blackjax は PyMC 側のラッパー既定設定のみで計測 (blackjax 本来の
  window adaptation を細かくチューニングすれば改善する可能性がある)。

## 再現方法

```bash
export OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 MKL_NUM_THREADS=1
export JAX_PLATFORMS=cpu JAX_ENABLE_X64=1
PIN="/usr/sbin/taskset -c 0"
PY=bench/venv/bin/python

# pymc 既定 NUTS + Numba (既定)
$PIN $PY bench/python/bench_hbm_scaling.py radon
# pymc 既定 NUTS + 真の C
BENCH_COMPILE_BACKEND=cvm $PIN $PY bench/python/bench_hbm_scaling.py radon
# pymc 既定 NUTS + JAX 勾配
BENCH_COMPILE_BACKEND=jax $PIN $PY bench/python/bench_hbm_scaling.py radon
# numpyro
BENCH_NUTS_SAMPLER=numpyro $PIN $PY bench/python/bench_hbm_scaling.py radon
# nutpie (Numba既定 / JAX)
BENCH_NUTS_SAMPLER=nutpie $PIN $PY bench/python/bench_hbm_scaling.py radon
BENCH_NUTS_SAMPLER=nutpie BENCH_NUTPIE_BACKEND=jax $PIN $PY bench/python/bench_hbm_scaling.py radon
# blackjax
BENCH_NUTS_SAMPLER=blackjax $PIN $PY bench/python/bench_hbm_scaling.py radon

# hanalyze (同日比較用)
$PIN cabal run -v0 -f benches bench-hbm-scaling -- radon
```

CSV 出力: `bench/results/python/hbm_scaling_radon{,_cvm,_jax,_numpyro,_nutpie,
_nutpie_jax,_blackjax}.csv`・`bench/results/haskell/hbm_scaling_radon.csv`。
