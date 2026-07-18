# TODO(モデル名) (posteriordb: `<data_name>-<model_name>`)

TODO: モデルの概要 (何を推定するか・出典・データの形)。出典:
`stan-dev/posteriordb` (`posterior_database/models/stan/<model_name>.stan`・
`posterior_database/data/data/<data_name>.json.zip`)。

- `reference_posterior_name: TODO` — posteriordb に公式 reference posterior が
  あるか確認する (`posterior_database/posteriors/<posterior_name>.json`)。
  null なら hanalyze vs PyMC の2者比較のみ、値があれば3者比較にする。
- Prior: TODO (Stan 原典の prior 構造を記載)。

これは **00-template** (雛形・ダミー値のみ、ビルド対象外)。新モデル着手時は
このディレクトリを `bench/posteriordb/NN-<slug>/` にコピーし、以下を置換する:

- `Model.hs` / `model.py` / `run_pymc_matrix.py` 中の TODO コメント・
  `template_model`/`templateModel`・`data_path`/`dataPath` 等のファイル名
- `data/template_data.json` を実データに差し替え
- `hanalyze.cabal` に `posteriordb-<slug>` executable スタンザを追加
  (`hs-source-dirs: bench/posteriordb/NN-<slug>, bench/posteriordb`)

手順の詳細は実例 `bench/posteriordb/01-glm-poisson/` を参照。

## ファイル

- `model.py` — PyMC 実装 + 合成ダッシュボード生成 (`py_dashboard_full.svg`・
  `../_common.py` の `make_pymc_dashboard` を使用)。
- `Model.hs` — hanalyze 実装 (`df |-> hbm` 高レベル API・
  `dataNamedX`/`dataNamedObs`/`plateForM_`・aeson で JSON 読込)。診断図は
  hgg `dashboardFullOf` で PNG 出力 (rasterific backend)。
- `run_pymc_matrix.py` — PyMC の CPU sampler×backend マトリクス
- `data/template_data.json` — ダミーデータ (実データに差し替える)
- `figures/` — `hs_dashboard_full.png`/`py_dashboard_full.svg` (実行後に生成)

## 実行方法 (NN-<slug>/ にコピーして名前を置換した後)

```bash
bench/venv/bin/python bench/posteriordb/NN-<slug>/model.py
bench/venv/bin/python bench/posteriordb/NN-<slug>/run_pymc_matrix.py

cabal build --project-file=cabal.project.plot posteriordb-<slug>
cabal run   --project-file=cabal.project.plot posteriordb-<slug>
```

## 経路確認 (「要改善」判定)

TODO: `synthVecIR` をこのモデルに直接呼んで `Just`/`Nothing` を確認する
(`cabal repl` 等)。`Nothing` (legacy walk+ad へのフォールバック) なら速度の
数値は記録せず、改善点だけ記録する (DSL機能ギャップ or 遅い経路へのフォール
バックの2種に分類)。

## 結果

TODO: 精度表・速度表・図・既知の課題 (実行後に記載)。
