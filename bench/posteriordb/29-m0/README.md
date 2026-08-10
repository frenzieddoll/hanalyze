# m0 (`M0_data-M0_model`)

BPA本 (Kéry & Schaub 2011) Ch.6 の最も単純な capture-recapture モデル
M0 (定数の内包確率omega・定数の検出確率p)。05-mhのMhモデルから個体差
ランダム効果を除いたベースライン変種 — 同一ファミリ内の構造比較用。
出典: `stan-dev/posteriordb`
(`posterior_database/models/stan/M0_model.stan`・
`posterior_database/data/data/M0_data.json.zip`)。
**`reference_posterior_name: null`** (posteriordb に公式 reference
無し・2者比較のみ)。

## Prior・尤度 (Stan原典・厳密に等価な ZeroInflatedBinomial 表現)

- `omega ~ Beta(1,1)`・`p ~ Beta(1,1)` (Stan原典は暗黙のUniform(0,1)・
  05-mhと同じ流儀でBeta(1,1)に移植・vecIR probe安全性のため)
- 尤度: 個体ごとの捕獲総数 `s[i] = sum(y[i,:])` に対し
  `s[i] ~ ZeroInflatedBinomial(T, 1-omega, p)`

05-mh/README.mdで導出済みのとおり、Stan原典のif分岐+log_sum_exp構造
(`s[i]>0`なら`bernoulli(1|omega)+binomial(s[i]|T,p)`・`s[i]=0`なら
`log_sum_exp(bernoulli(1|omega)+binomial(0|T,p), bernoulli(0|omega))`)
は`ZeroInflatedBinomial(T,1-omega,p)`と数学的に厳密に一致する
(近似ではない)。

## 実装状態 (2026-07-12・コード準備のみ・データ未取得・未実行)

Stan原典の構造のみからコードを作成 (データはダウンロードしていない)。
`Model.hs`/`model.py`/`run_pymc_matrix.py`を作成済み。データ配置・
cabalスタンザ追加・ビルド確認・実行は未実施。

## 経路確認

未確認 (未実行のため)。 05-mhの`ZeroInflatedBinomial`はvecIR対応済み
(`VGZIBinom`/`SDZIBinom`/`VOZIBinom`・Phase 90 A3) のため、本モデルも
`synthVecIR = Just`になる可能性が高い (未確認)。

## 結果

未計測 (未実行)。
