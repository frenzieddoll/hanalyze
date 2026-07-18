# lda (posteriordb: `three_men1-ldaK2`) — ⏸ 保留

LDA (Latent Dirichlet Allocation) トピックモデル (K=2固定・V=249語彙・
M=6文書・N=4999語インスタンス)。離散潜在トピック割当を周辺化 (collapsed)
した対数尤度を使う。出典: `stan-dev/posteriordb`
(`posterior_database/models/stan/ldaK2.stan`・
`posterior_database/data/data/three_men1.json.zip`・
`jonasson2021rapid` 参照)。

- `reference_posterior_name: null` (posteriordb に公式 reference 無し・
  hanalyze vs PyMC の2者比較のみ)。
- Prior: `theta[m] ~ Dirichlet(1,1)` (m=1..6・K=2次元simplex)・
  `phi[k] ~ Dirichlet(1,...,1)` (k=1,2・V=249次元simplex)。
- 尤度 (周辺化): `gamma[k] = log(theta[doc[n],k]) + log(phi[k,w[n]])`・
  `target += log_sum_exp(gamma)` (n=1..4999)。posteriordb keywords に
  "multimodal" — トピックは交換可能 (a priori にθ/φのラベルに区別が無い)
  ためラベルスイッチングが起き得る既知の課題 (04-low-dim-gauss-mix と
  同種・対処せず記録のみ)。

## ⏸ 保留の理由: 完走はするが規模に対し実用外に遅い

**実装自体は正常** (`Model.hs` に `ldaModel` として存在・`dirichlet` +
`Hanalyze.Model.HBM.Util.logSumExpA` + `potential` で Stan 原典を
忠実に移植)。13-traffic-accident-nyc (ハングして出力すら出ない) とは
異なり、**本モデルは完走はする** — 中規模probe (1chain・warmup100+
draws100=200 iteration) を実測したところ 325.1秒 (r_hat≈0.99-1.00・
NaN無し・正常収束) だった。

しかし本番設定 (4chain・warmup1000+draws1000=2000 iteration/chain) は
**30分タイムアウトで完走せず** (`EXIT=124`)。中規模probeの実測値
(200 iteration=325.1秒 → 約1.6秒/iteration) から単純外挿すると
2000 iteration/chain ≈ 54分/chain — 他モデルの最遅 (14-hmm-example・
71.7秒) と比べ約45倍遅い規模になる。

原因: theta (M×(K-1)=6次元) + phi (K×(V-1)=496次元) の合計約502次元
という、他の posteriordb ベンチモデルより一桁大きい latent 空間を
`synthVecIR=Nothing` (legacy walk+ad) で扱っており、NUTS の質量行列
適応 (Welford更新・木の深さ探索) のコストが次元数に対して効いている
と見られる (詳細な内訳は未計測・深掘りは本 Phase の主眼外)。

**user確認済み (2026-07-11)**: 13-traffic-accident-nyc と同じ「保留」
区分とし、model.py/run_pymc_matrix.py は作成せず (hanalyze側が実用外と
判明した時点でPyMC比較の意味が無いため)、17-nes へ進む。

## 実測ログ

| 設定 | iteration数 | wall (ms) | 結果 |
|---|---:|---:|---|
| 1chain・warmup3+draws3・V=5 (debug切り分け用) | 6 | 14.1 | 正常 (命名バグ発見・修正) |
| 1chain・warmup3+draws3・V=249 (フル語彙) | 6 | 1601.5 | 正常収束 |
| 1chain・warmup100+draws100・V=249 | 200 | 325120.8 | 正常収束 (r_hat≈0.99-1.00) |
| **4chain・warmup1000+draws1000・V=249 (本番)** | 2000/chain | **30分でtimeout (EXIT=124)** | 未完走 |

## ★実装時に踏んだ罠 (数値の罠ではなく命名バグ)

初回実行 (1chain・warmup3+draws3) で全 `theta_i_j` が `mean=NaN・
ess=0.0・r_hat=NA` という一見「HMC完全凍結」に似た症状が出た。
`dirichlet` helper の実際の命名規則は `(.#)` (= `Hanalyze.Model.HBM.Model.indexed`)
が `"theta" <> "_" <> show i` = `"theta_0"` を生成し、更に `dirichlet`
内部が `"_" <> show k` を付加するため実際の deterministic 名は
`"theta_0_0"` (アンダースコア2箇所)。 `summarize` に渡した名前リストが
`"theta0_0"` (アンダースコア1箇所・doc番号の前が無い) だったため
**存在しないキーを探索して全てNaN扱いになっていた** — 数値的な発散
ではなく単純な文字列不一致だった。V=5への語彙縮小で数秒オーダーの
probeが可能になり、命名を `"theta_" <> show i <> "_" <> show k` に
修正して解消したことで確定した (10-rats等の「Uniform境界外初期値で
凍結」とは別種・混同注意)。

## ファイル

- `model.py` / `run_pymc_matrix.py` — 未作成 (hanalyze側が実用外と判明
  したため、両言語比較の意味が無く見送り。13-traffic-accident-nycと
  同じ判断)。
- `Model.hs` — hanalyze 実装 (完成済み・中規模probeで正常動作確認済み・
  本番規模は未完走のため保留)。
- `data/three_men1.json` — posteriordb 由来データ (`V=249`・`M=6`・
  `N=4999`・`w`/`doc` 1-based)。
- `figures/` — 未生成 (本番実行未完走のため)。

## 経路確認

`synthVecIR = Nothing` (legacy walk+ad)。`dirichlet` の stick-breaking +
K-way `logSumExpA` を使った周辺化混合尤度は vecIR 未対応と見られる
(b) 遅い経路へのフォールバック。

## 既知の課題

- **大規模simplex latent (約502次元) でのNUTS適応コスト**: 本番設定が
  30分で完走せず。根本原因の内訳 (質量行列Welford更新のコストか・木の
  深さ探索のコストか) は未計測。改善には vecIR 側で Dirichlet/simplex
  simplexファミリのサポートが必要と見られる (未確認・Phase90以降の
  スコープ)。
- ラベルスイッチング (トピックの交換可能性) の実害は本番未完走のため
  未確認。
- `dirichlet` helper の命名規則 (`name <> "_" <> show i` を内部で
  さらに付加) は summarize 呼び出し側で見落としやすい罠 — 他モデルで
  `dirichlet`/`(.#)` を組み合わせる際は生成される実際の名前を確認する
  こと。
