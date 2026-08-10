# election88 (`election88-election88_full`)

1988年米大統領選 世論調査の多水準ロジスティック回帰 (Gelman & Hill
2006・N=11566・これまでで最大のN)。★新ファミリ: **5本の独立な階層**
(age/edu/age×edu交互作用/state/regionそれぞれ別々のgroup-level
intercept)。21-radonの単一階層や10-ratsの「同一グループへの二重階層」
とも異なり、5つの互いに独立なグループ添字上の階層という構造。出典:
`stan-dev/posteriordb`
(`posterior_database/models/stan/election88_full.stan`・
`posterior_database/data/data/election88.json.zip`)。
**`reference_posterior_name: null`** (posteriordb に公式 reference
無し・2者比較のみ)。

## Prior・尤度 (Stan原典・sigma_a..eのみhanalyze向けに等価な安全代替)

- `a[age] ~ Normal(0,sigma_a)`・`b[edu] ~ Normal(0,sigma_b)`・
  `c[age_edu] ~ Normal(0,sigma_c)`・`d[state] ~ Normal(0,sigma_d)`・
  `e[region] ~ Normal(0,sigma_e)` (5本の独立階層)
- `beta[1..5] ~ Normal(0,100)` (切片+black+female+v_prev_full+交互作用)
- `sigma_a..e`: Stan原典は`real<lower=0,upper=100>`の暗黙Uniform(0,100)
  だが、01-glm-poisson/10-rats/15-dugongsで確立済みの「Uniform(0,X)を
  SDパラメータに使うとHMCが初手から凍結する罠」に該当するため
  `HalfCauchy(25)`に置換 (10-ratsと同じ代替パターン)
- 尤度: `y_hat[i] = beta1 + beta2*black[i] + beta3*female[i] +
  beta5*female[i]*black[i] + beta4*v_prev_full[i] + a[age[i]] +
  b[edu[i]] + c[age_edu[i]] + d[state[i]] + e[region_full[i]]`・
  `y[i] ~ Bernoulli(invlogit(y_hat[i]))`

hanalyze の `Bernoulli` は確率パラメータ直接指定 (logit link 無し) の
ため、02-dogs/19-surgicalと同じく `p = invlogit(y_hat)` を手計算する。
添字列は微分対象ではない構造的定数なのでclosureで直接渡し、群ごとの
latentは`Data.Vector`経由でO(1)索引する (N=11566と大規模なため必須・
17-nes/21-radonと同じ流儀)。

## 実装状態 (2026-07-12・準備のみ・未実行)

コード/データ準備のみ完了 (`Model.hs`/`model.py`/`run_pymc_matrix.py`・
実データ配置・cabalスタンザ追加)。ビルド・実行・精度/速度比較は未実施。
N=11566はこれまでで最大規模のためlegacy walk+ad経路の場合は実行時間が
長くなる可能性がある (未計測)。

## 経路確認

未確認 (未実行のため)。

## 結果

未計測 (未実行)。
