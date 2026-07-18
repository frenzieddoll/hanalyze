# HBM / GLMM 関連 demo 解説

> 🌐 English: TODO (Phase 37 完了後に同期)
>
> `Hanalyze.Model.HBM` / `Hanalyze.Model.GLMM` の階層モデル関連 demo を
> 「何をしているか」「何を見るか」 で短く解説したガイド。 各 demo の
> ソースコード自体は `demo/bayesian/` 配下にあります。
>
> モデルの DSL 書き方そのものは
> [`02-probabilistic-model.ja.md`](02-probabilistic-model.ja.md) を、
> 比較指標 (WAIC/LOO) は [`06-model-comparison.ja.md`](06-model-comparison.ja.md)
> を参照。

## `hbm-example` — 階層正規モデル + 4 chain NUTS

ソース: [`demo/bayesian/HBMExample.hs`](../../demo/bayesian/HBMExample.hs)

**目的**: 形式 A (群ごとデータ) の階層正規モデルを NUTS で推論し、 4 chain
の集約レポート (model graph + posterior summary + trace + autocorr + pair) を
HTML で出力する全機能 demo。

**モデル**: `schoolModel :: [[Double]] -> ModelP ()`
- `μ ~ Normal(0, 100)`、 `τ ~ Exponential(0.1)`
- `θ_j ~ Normal(μ, τ)` (3 群)
- `y_ij ~ Normal(θ_j, σ=5)`

**走らせ方**:

```bash
cabal run hbm-example
# → mcmc_report.html          (single chain RWM)
# → mcmc_report_multi.html    (4-chain NUTS, R̂ 含む)
```

**何を見るか**:
- chain ごとの μ、 τ の事後平均が一致しているか (R̂ < 1.01 の確認)
- pair plot で μ-τ に funnel 形状が出ていないか
- ESS が十分か (200 以上)

## `hbm-random-slope` — ランダム切片 vs +ランダム傾き

ソース: [`demo/bayesian/HBMRandomSlopeDemo.hs`](../../demo/bayesian/HBMRandomSlopeDemo.hs)

**目的**: 群によって x の効き方が違うデータに対し、 ランダム切片だけのモデル
(M1: β 共通) と ランダム切片 + ランダム傾き (M2: β_j 群別) を WAIC / LOO で
比較する。

**データ**: 3 群 × 10 観測 = N=30、 真値は
- Group A: α=2.0, β=-0.8 (急な右下り)
- Group B: α=5.0, β=-0.3 (緩やかな右下り)
- Group C: α=8.0, β=+0.2 (わずかに右上り)

**走らせ方**:

```bash
cabal run hbm-random-slope
# → rs_m1.html        (M1 単体レポート)
# → rs_m2.html        (M2 単体レポート)
# → rs_compare.html   (M1 vs M2 並列レポート)
```

**何を見るか**:
- M2 の WAIC / LOO が M1 より小さくなるか (ΔWAIC < 0)
- β_A, β_B, β_C の事後 mean が真値 -0.8 / -0.3 / +0.2 を回復するか
- M1 の β (共通) は群別の真値を平均化してしまい識別不能になる挙動

## `simpson-paradox` — LM / GLMM / HBM の三段比較

ソース: [`demo/bayesian/SimpsonParadoxDemo.hs`](../../demo/bayesian/SimpsonParadoxDemo.hs)

**目的**: シンプソンのパラドックス (群内では負の傾き、 群を無視すると正に
見える) を 3 手法で解析し、 群構造を無視すると誤った結論になることを示す。

**データ**: 3 群 × 10 観測 = N=30、 各群内では `y = α_g − 0.5·x + ノイズ`
(負の傾き)。 ただし群間で `α` と `x` の平均がずれているため、 全体では
正の相関に見える。

**走らせ方**:

```bash
cabal run simpson-paradox
# → simpson_lm.html       (LM、 群無視 → β > 0 の誤った結論)
# → simpson_glmm.html     (GLMM、 ランダム切片 → β < 0 の正しい結論)
# → simpson_hbm.html      (HBM、 完全ベイズ → β < 0 + 95% CI)
# → simpson_compare.html  (3 手法の比較レポート)
```

**何を見るか**:
- LM の slope が +、 GLMM / HBM の slope が − になっているか
- HBM では β の事後 95% CI が 0 を含まず明確に負であるか
- WAIC が GLMM / HBM (群構造あり) で大きく改善するか

## `glmm-demo` — GLMM (LME) 最尤推定

ソース: [`demo/Demo.hs`](../../demo/Demo.hs)
(executable 名 `glmm-demo`)

**目的**: HBM (完全ベイズ) ではなく **古典 GLMM (EM / Laplace)** の使い方を
示す。 高速で fit でき、 モデル探索段階で重宝する。

**データ**: 3 クラス × 5 観測 = N=15、 真値は
`score = 64 + u_school + 2 · hours + ε`、 `u_A=+20, u_B=0, u_C=-20`。
クラス A は少時間で高得点、 C は長時間で低得点なので OLS だと
「勉強時間が増えると成績が下がる」 (シンプソン) と誤読する。

**走らせ方**:

```bash
cabal run glmm-demo
# 標準出力に LME fit 結果 (固定効果 + ランダム効果 + ICC)
```

**何を見るか**:
- 固定効果 `hours` の係数が +2 付近か (真値回復)
- ランダム効果 `u_school` が +20 / 0 / -20 に近いか
- ICC が高い (~0.9) ことから群構造の重要性が読み取れる

**HBM (`simpson-paradox`) との対応**: 同じ手法 (LME) を完全ベイズで書き直すと
HBM パターン 4 形式 A になる。 GLMM は速いが Wald 近似 SE、 HBM は遅いが
厳密な事後分布が得られる (使い分けは
[`docs/principles/glmm.ja.md`](../principles/glmm.ja.md) 末尾参照)。

## `phase37-a0-verify` — doc 内 sample コードの実行確認

ソース: [`demo/bayesian/Phase37A0VerifyDemo.hs`](../../demo/bayesian/Phase37A0VerifyDemo.hs)

**目的**: [`02-probabilistic-model.ja.md`](02-probabilistic-model.ja.md) に
追加した パターン 4 (形式 A/B/C) / 5 (random slope) / 6 (multi-level) /
7 (crossed) / 8 (prior choice) の sample code を 1 つの executable に
集めて、 ビルド + 小規模 NUTS 実行で動作確認する。 doc に貼るコードが本当に
動くことの保証。

**走らせ方**:

```bash
cabal run phase37-a0-verify
# 各モデルを 100 iter / 50 burn-in で 1 回ずつ走らせ、
# 受容率 + 主要パラメータの事後 mean を 1 行で出力
```

**期待される出力**: 全 8 モデルで受容率 > 0.8、 主要パラメータが真値近傍。
例えば `random slope` 行では `beta_1 ≈ -0.80`、 `beta_3 ≈ +0.20` が
回復される。
