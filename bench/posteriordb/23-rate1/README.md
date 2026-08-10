# rate1 (`Rate_1_data-Rate_1_model`)

Bayesian Cognitive Modeling (Lee & Wagenmakers 2013) の最も単純な例
「二項比率の推定」(n=10試行・k=5成功)。★新ファミリ: 単一パラメータの
共役Beta-Binomial (これまでで最も単純なモデル・他モデルとの対比用
ベースライン)。出典: `stan-dev/posteriordb`
(`posterior_database/models/stan/Rate_1_model.stan`・
`posterior_database/data/data/Rate_1_data.json.zip`)。
**`reference_posterior_name: null`** (posteriordb に公式 reference
無し・2者比較のみ)。

## Prior・尤度 (Stan原典どおり)

- `theta ~ Beta(1,1)` (= Uniform(0,1) と同一分布)
- 尤度: `k ~ Binomial(n, theta)`

## 実装状態 (2026-07-12・準備のみ・未実行)

コード/データ準備のみ完了 (`Model.hs`/`model.py`/`run_pymc_matrix.py`・
実データ配置・cabalスタンザ追加)。ビルド・実行・精度/速度比較は未実施。

## 経路確認

未確認 (未実行のため)。

## 結果

未計測 (未実行)。
