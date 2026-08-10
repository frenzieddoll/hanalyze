# kilpisjarvi (`kilpisjarvi_mod-kilpisjarvi`)

キルピスヤルヴィ (フィンランド) の気候変動観測データに対する単純な
線形回帰。★新ファミリ: **prior のハイパーパラメータ自体がデータとして
与えられる** (`pmualpha`/`psalpha`/`pmubeta`/`psbeta`)。出典:
`stan-dev/posteriordb`
(`posterior_database/models/stan/kilpisjarvi.stan`・
`posterior_database/data/data/kilpisjarvi_mod.json.zip`)。
**`reference_posterior_name: "kilpisjarvi_mod-kilpisjarvi"`**
(posteriordb に公式referenceあり・3者比較可能)。

## Prior・尤度 (Stan原典・sigmaのみhanalyze向けに置換)

- `alpha ~ Normal(pmualpha, psalpha)`・`beta ~ Normal(pmubeta, psbeta)`
  (ハイパーパラメータ自体がJSONデータから読み込まれる)
- `sigma`: Stan原典に明示的priorが無い (暗黙のflat prior・
  `real<lower=0>`) ため 01-glm-poisson/10-ratsと同じ流儀で
  `HalfCauchy(25)` に置換
- 尤度: `y ~ Normal(alpha + beta*x, sigma)`

## 実装状態 (2026-07-12・コード準備のみ・データ未取得・未実行)

Stan原典の構造のみからコードを作成 (データはダウンロードしていない)。
`Model.hs`/`model.py`/`run_pymc_matrix.py`を作成済み。データ配置・
cabalスタンザ追加・ビルド確認・実行は未実施。

## 経路確認

未確認 (未実行のため)。

## 結果

未計測 (未実行)。
