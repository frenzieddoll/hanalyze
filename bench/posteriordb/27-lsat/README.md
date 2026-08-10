# lsat (`lsat_data-lsat_model`)

LSAT (法科大学院適性試験) T=5問・N=1000人の Rasch (1PL) 型 二値IRT —
Bock & Lieberman (1970) 古典例。★新ファミリ: 全項目共通の単一識別力
(1PL/Rasch) — 06-irt-2pl (項目ごとの識別力・2PL) や 20-bones (graded
response) とは異なるIRTモデル種別。出典: `stan-dev/posteriordb`
(`posterior_database/models/stan/lsat_model.stan`・
`posterior_database/data/data/lsat_data.json.zip`)。
**`reference_posterior_name: null`** (posteriordb に公式 reference
無し・2者比較のみ)。

## Prior・尤度 (Stan原典どおり)

- `alpha[T=5] ~ Normal(0,100)` (項目ごとの困難度)
- `theta[N=1000] ~ Normal(0,1)` (受験者ごとの能力)
- `beta ~ Normal(0,100)` (`real<lower=0>`制約 = 正の切断 =
  `HalfNormal(100)`と厳密に等価・全項目共通の単一識別力)
- 尤度: `r[i,k] ~ Bernoulli(invlogit(beta*theta[i] - alpha[k]))`

## ★データ形式の特殊性: パターン圧縮形式の展開が必要

posteriordbのデータは`N×T`の生の回答行列ではなく、**R=32通りの応答
パターンと各パターンの累積人数(culm)のみ**を保持するパターン圧縮形式。
Stan原典の`transformed data`ブロックがこれをN人分の回答行列に展開する
ロジックを持つため、Haskell/Python側でも同型の`unpackResponses`関数で
再現する必要がある (`culm[i]-culm[i-1]`人が`response[i]`と同じ回答列を
持つ、という展開)。

## 実装状態 (2026-07-12・コード準備のみ・データ未取得・未実行)

Stan原典の構造のみからコードを作成 (データはダウンロードしていない)。
`Model.hs`/`model.py`/`run_pymc_matrix.py`を作成済み (パターン展開
ロジック含む)。データ配置・cabalスタンザ追加・ビルド確認・実行は
未実施。

## 経路確認

未確認 (未実行のため)。

## 結果

未計測 (未実行)。
