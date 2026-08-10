# hanalyze-bayes

[`hanalyze`](../README.ja.md) の**ベイズ推論層**。 階層ベイズモデル
(HBM) の DSL・MCMC サンプラ群・自動微分・ベイズ的モデル比較を担う。

依存は `hanalyze-core` のみ (dataframe には依存しない)。 `-frame` と
並んで core の直上に位置するので、 **データ表現を持ち込まずに
サンプリング library としてだけ使える**のが特徴。

## 主要 module (全 26 module)

### HBM DSL (`Hanalyze.Model.HBM.*`)

| Module | 役割 |
|---|---|
| `Model.HBM` | 下位 8 module (Util / Distribution / Sampling / Model / Track / Eval / IR / Gradient) を束ねる facade。 通常はこれだけを import する |
| `Model.HBM.Model` | Free monad による多相モデル DSL 記述層 (`sample` / `observe` / `dataNamed*`) |
| `Model.HBM.Distribution` | 多相確率分布 ADT と密度・CDF |
| `Model.HBM.Eval` | log-joint / 尤度インタープリタ + DAG 構築 |
| `Model.HBM.Gradient` / `Model.HBM.IR` / `Model.HBM.VecAD` | AD 勾配コンパイラ層・中間表現・自作 reverse-mode AD (NUTS の per-draw ホット経路) |
| `Model.HBM.Ast` / `Model.HBM.Interp` | dialog DSL の AST + JSON decoder と評価系 |

### サンプラ (`Hanalyze.MCMC.*`)

| Module | 役割 |
|---|---|
| `MCMC.NUTS` | No-U-Turn Sampler — Hoffman & Gelman (2014) Algorithm 3 実装。 主力サンプラ |
| `MCMC.HMC` | Hamiltonian Monte Carlo (AD による exact gradient) |
| `MCMC.MH` / `MCMC.Slice` | Random-Walk Metropolis-Hastings / Slice sampler (Neal 2003) |
| `MCMC.Gibbs` | 共役事前分布向け Gibbs sampler (解析的フル条件付き) |
| `MCMC.SMC` | Tempered target による Sequential Monte Carlo |
| `MCMC.BayesianTest` | Bayesian A/B test — 2 群の平均差を NUTS でサンプルし ROPE / HDI で判定 |
| `MCMC.Progress` | 全 chain 集計の進捗表示 (stderr) |

### 推論・モデル比較 (`Hanalyze.Stat.*`)

| Module | 役割 |
|---|---|
| `Stat.AD` | HMC / NUTS を支える自動微分層 |
| `Stat.VI` | 変分推論 (ADVI) |
| `Stat.BridgeSampling` | Bridge Sampling による周辺尤度 log p(y) 推定 (Meng & Wong 1996) |
| `Stat.BayesFactor` | Bridge Sampling ベースの Bayes factor (Kass & Raftery 1995) |
| `Stat.BayesianModelAveraging` | log marginal を用いた真の BMA |
| `Stat.PosteriorPredictive` | 事前 / 事後予測サンプリング (PyMC の `sample_*_predictive` 相当) |

## 単体で使う

```cabal
-- Chain / 事後統計量は core 側の型なので、直接触るなら core も明示する
build-depends: hanalyze-bayes, hanalyze-core, containers
```

```haskell
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Map.Strict as Map
import Hanalyze.Model.HBM (ModelP, sample, observe, Distribution (..))
import Hanalyze.MCMC.NUTS (nutsPure, defaultNUTSConfig)
import Hanalyze.MCMC.Core (posteriorMean, posteriorSD)

myModel :: ModelP ()
myModel = do
  mu <- sample "mu" (Normal 0 10)
  observe "y" (Normal mu 2) [1.2, 2.3, 3.1, 2.8, 1.9]   -- observe は [Double]

main = do
  -- nutsPure は seed (Word32) を取り、純粋・決定的に Chain を返す
  let chain = nutsPure myModel defaultNUTSConfig (Map.fromList [("mu", 0.0)]) 42
  print (posteriorMean "mu" chain, posteriorSD "mu" chain)
  -- (Just 2.2673259586131507,Just 0.8340460004499451)
```

`Chain` は core の `Hanalyze.MCMC.Core` の型なので、 事後統計量
(`posteriorMean` / `posteriorSD` / `posteriorQuantile`) と診断
(`Hanalyze.Stat.MCMC` の `rhat` / `ess` / `hdi`) は core 側の API で扱える。
HTML レポートや DAG 図が要る場合は `-viz` 層 (`Hanalyze.Viz.*`) を追加する。

## 関連 docs

- 確率モデルの書き方: [docs/bayesian/02-probabilistic-model.ja.md](../docs/bayesian/02-probabilistic-model.ja.md)
- サンプラの選び方: [docs/bayesian/03-mcmc-samplers.ja.md](../docs/bayesian/03-mcmc-samplers.ja.md)
- Gibbs: [docs/bayesian/04-gibbs.ja.md](../docs/bayesian/04-gibbs.ja.md) /
  変分推論: [docs/bayesian/05-vi.ja.md](../docs/bayesian/05-vi.ja.md)
- モデル比較 (WAIC / LOO): [docs/bayesian/06-model-comparison.ja.md](../docs/bayesian/06-model-comparison.ja.md)
- 周辺尤度 / Bayes factor / BMA: [docs/bayesian/07-advanced-marginal-likelihood.ja.md](../docs/bayesian/07-advanced-marginal-likelihood.ja.md)
- Bayesian A/B test: [docs/bayesian/usage-bayesian-ab-test.ja.md](../docs/bayesian/usage-bayesian-ab-test.ja.md)
- 理論: [docs/bayesian/theory-hmc-nuts.ja.md](../docs/bayesian/theory-hmc-nuts.ja.md)

← [repository README](../README.ja.md)
