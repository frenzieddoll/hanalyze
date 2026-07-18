"""arK-arK (posteriordb) — PyMC 実装。

Phase 89: posteriordb 横断ベンチマーク。AR(K) (K次自己回帰) 時系列モデル
(K=5・T=200)。GARCH (03-garch11) と異なり分散ではなく平均のみが過去に
依存するため、全ての y は既知データ — K個のラグ特徴量を使った静的な
線形回帰に帰着する。

Stan 原典 (posteriordb `models/stan/arK.stan`):
  parameters { real alpha; array[K] real beta; real<lower=0> sigma; }
  model {
    alpha ~ normal(0, 10); beta ~ normal(0, 10); sigma ~ cauchy(0, 2.5);
    for (t in (K+1):T) {
      mu = alpha + sum_{k=1}^{K} beta[k]*y[t-k];
      y[t] ~ normal(mu, sigma);
    }
  }

**reference_posterior_name = "arK-arK"** (posteriordb に公式 reference
あり・hanalyze vs PyMC vs 公式referenceの3者比較が可能)。

変数名は hanalyze 側 (Model.hs) に対応させる (Python 流 snake_case):
data_path/read_data/ark_model/figures_dir/main。
"""
import json
import sys
from pathlib import Path

import arviz as az
import matplotlib
import numpy as np
import pymc as pm

sys.path.insert(0, str(Path(__file__).parent.parent))
from _common import make_pymc_dashboard  # noqa: E402 (sys.path 設定の直後)

matplotlib.use("Agg")

data_path = Path(__file__).parent / "data" / "arK.json"
figures_dir = Path(__file__).parent / "figures"


def read_data():
    with open(data_path) as f:
        d = json.load(f)
    return int(d["K"]), np.array(d["y"], dtype=float)


def lag_design(k, y):
    n = len(y)
    obs_idx = np.arange(k, n)
    targets = y[obs_idx]
    lags = np.column_stack([y[obs_idx - lg] for lg in range(1, k + 1)])
    return lags, targets


def ark_model():
    k, y = read_data()
    lags, targets = lag_design(k, y)
    with pm.Model() as m:
        alpha = pm.Normal("alpha", mu=0, sigma=10)
        beta = pm.Normal("beta", mu=0, sigma=10, shape=k)
        sigma = pm.HalfCauchy("sigma", beta=2.5)
        mu = alpha + pm.math.dot(lags, beta)
        pm.Normal("y_obs", mu=mu, sigma=sigma, observed=targets)
    return m


def main():
    m = ark_model()
    with m:
        idata = pm.sample(draws=1000, tune=1000, chains=4, cores=1,
                           nuts_sampler="pymc", random_seed=1,
                           progressbar=False, compute_convergence_checks=False)
        pm.sample_posterior_predictive(idata, extend_inferencedata=True,
                                        progressbar=False, random_seed=1)
    print(az.summary(idata, var_names=["alpha", "beta", "sigma"]).to_string())

    # Phase 89: PyMC 側の合成ダッシュボード (Haskell dashboardFullOf と対)。
    # figures/ は事前に用意されている前提 (Model.hs 側と同じく実行時に
    # ディレクトリを作らない)。
    make_pymc_dashboard(m, idata, "y_obs", figures_dir / "py_dashboard_full.svg")


if __name__ == "__main__":
    main()
