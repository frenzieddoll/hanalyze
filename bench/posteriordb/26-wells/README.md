# wells (`wells_data-wells_interaction_model`)

バングラデシュ井戸水ヒ素汚染調査「井戸の切替行動」の交互作用ロジス
ティック回帰 (Gelman & Hill 2006)。距離(dist)とヒ素濃度(arsenic)の
交互作用項を含む二項ロジット (切片1+係数3の最も単純な交互作用回帰)。
出典: `stan-dev/posteriordb`
(`posterior_database/models/stan/wells_interaction_model.stan`・
`posterior_database/data/data/wells_data.json.zip`)。
**`reference_posterior_name: null`** (posteriordb に公式 reference
無し・2者比較のみ)。

## Prior・尤度 (Stan原典・暗黙flatのみhanalyze向けに置換)

- `transformed data`: `dist100 = dist/100`・`inter = dist100 * arsenic`
- `alpha`/`beta[1..3]`: Stan原典に明示的priorが無い (暗黙のflat prior)
  ため 01-glm-poisson/10-ratsと同じ流儀で `Normal(0,1000)` に置換
- 尤度: `switched ~ Bernoulli(invlogit(alpha + beta1*dist100 +
  beta2*arsenic + beta3*inter))`

hanalyze の `Bernoulli` は確率パラメータ直接指定 (logit link 無し) の
ため、02-dogs/19-surgicalと同じく `invlogit` を手計算する。

## 実装状態 (2026-07-12・コード準備のみ・データ未取得・未実行)

Stan原典の構造のみからコードを作成 (データはダウンロードしていない)。
`Model.hs`/`model.py`/`run_pymc_matrix.py`を作成済み。データ配置・
cabalスタンザ追加・ビルド確認・実行は未実施。

## 経路確認

未確認 (未実行のため)。

## 結果

未計測 (未実行)。
