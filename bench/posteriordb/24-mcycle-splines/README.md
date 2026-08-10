# mcycle-splines (`mcycle_splines-accel_splines`)

オートバイ衝突試験の加速度スプライン回帰 (brms生成モデル・N=133・平均/
分散とも39自由度のthin-plate spline (線形項1+基底関数38) で非線形推定
する不均一分散モデル)。★新ファミリ: スプライン基底回帰 (07-gp-regrの
GPカーネルとは異なり、基底関数計画行列はposteriordbデータに事前計算
済みで提供される)。出典: `stan-dev/posteriordb`
(`posterior_database/models/stan/accel_splines.stan`・
`posterior_database/data/data/mcycle_splines.json.zip`)。
**`reference_posterior_name: null`** (posteriordb に公式 reference
無し・2者比較のみ)。

## Prior・尤度 (Stan原典どおり・一部hanalyze向けに厳密等価変換)

- `Intercept ~ StudentT(3,-13,36)`・`Intercept_sigma ~ StudentT(3,0,10)`
- `bs`/`bs_sigma` (各Ks=1本の線形項係数): Stan原典に明示的priorが無い
  (暗黙のflat prior) ため、01-glm-poisson/10-ratsと同じ流儀で
  `Normal(0,1000)` に置換
- `zs_1_1`/`zs_sigma_1_1` (各38成分) `~ Normal(0,1)`
- `sds_1_1`/`sds_sigma_1_1 ~ half-StudentT(3,0,36)` (下側切断)。hanalyze
  にhalf-StudentT分布が無いため、`HalfCauchy(1)`を安全な初期値を持つ
  代理distributionとしてsampleし、`potential`で真のhalf-StudentT密度
  へ厳密に補正 (14-hmm-exampleのmu2順序制約と同じ「代理+potential
  補正」パターン・近似ではなく数学的に厳密)
- `s_1_1 = sds_1_1 * zs_1_1`・`s_sigma_1_1 = sds_sigma_1_1 * zs_sigma_1_1`
  (non-centered)
- 尤度: `mu[n] = Intercept + Xs[n]·bs + Zs_1_1[n]·s_1_1`・
  `sigma[n] = exp(Intercept_sigma + Xs_sigma[n]·bs_sigma +
  Zs_sigma_1_1[n]·s_sigma_1_1)`・`Y[n] ~ Normal(mu[n], sigma[n])`

計画行列 (`Xs`/`Zs_1_1`/`Xs_sigma`/`Zs_sigma_1_1`) は微分対象ではない
データなのでHaskell側はclosureで直接渡す (20-bonesのgamma/deltaと同じ
流儀)。

## 実装状態 (2026-07-12・準備のみ・未実行)

コード/データ準備のみ完了 (`Model.hs`/`model.py`/`run_pymc_matrix.py`・
実データ配置・cabalスタンザ追加)。ビルド・実行・精度/速度比較は未実施。

## 経路確認

未確認 (未実行のため)。

## 結果

未計測 (未実行)。
