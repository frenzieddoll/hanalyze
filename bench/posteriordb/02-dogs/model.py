"""dogs-dogs (posteriordb) — PyMC 実装。

Phase 89: posteriordb 横断ベンチマーク。Solomon & Wynne (1953) の犬の回避
学習実験 (30 匹 × 25 試行の電気ショック回避、Gelman & Hill 2006 Ch.24 の
ARM 本例) を、累積の回避/被ショック回数を共変量とするロジスティック回帰
でモデル化する。

Stan 原典 (posteriordb `models/stan/dogs.stan`):
  data {
    int n_dogs; int n_trials;
    array[n_dogs, n_trials] int<lower=0,upper=1> y;
  }
  parameters { vector[3] beta; }
  transformed parameters {
    // n_avoid[j,t]/n_shock[j,t] は y (観測データ) のみに依存する累積和
    // (beta 非依存)。 Stan は毎回再計算するが実質は前処理定数。
    p[j,t] = beta[1] + beta[2]*n_avoid[j,t] + beta[3]*n_shock[j,t];
  }
  model {
    beta ~ normal(0, 100);
    y[i,j] ~ bernoulli_logit(p[i,j]);
  }

reference_posterior_name = null (posteriordb に公式 reference posterior 無し)。
本モデルは hanalyze vs PyMC の 2 者比較のみ (3 者比較は不可)。

変数名は hanalyze 側 (`Model.hs`) に対応させる (Python 流 snake_case):
data_path/read_data/dogs_model/figures_dir/main。
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

data_path = Path(__file__).parent / "data" / "dogs_data.json"
figures_dir = Path(__file__).parent / "figures"


def read_data():
    with open(data_path) as f:
        d = json.load(f)
    y = np.array(d["y"], dtype=int)  # shape (n_dogs, n_trials)
    return y

def cumulative_counts(y):
    """n_avoid[j,t]/n_shock[j,t]: beta 非依存の累積和 (Stan transformed
    parameters を前処理として1回だけ計算する — 反復ごとの再計算は不要)。"""
    n_dogs, n_trials = y.shape
    n_avoid = np.zeros((n_dogs, n_trials))
    n_shock = np.zeros((n_dogs, n_trials))
    for t in range(1, n_trials):
        n_avoid[:, t] = n_avoid[:, t - 1] + 1 - y[:, t - 1]
        n_shock[:, t] = n_shock[:, t - 1] + y[:, t - 1]
    return n_avoid, n_shock

def dogs_model():
    y = read_data()
    n_avoid, n_shock = cumulative_counts(y)
    y_flat = y.flatten()
    n_avoid_flat = n_avoid.flatten()
    n_shock_flat = n_shock.flatten()
    with pm.Model() as m:
        beta1 = pm.Normal("beta1", mu=0, sigma=100)
        beta2 = pm.Normal("beta2", mu=0, sigma=100)
        beta3 = pm.Normal("beta3", mu=0, sigma=100)
        logit_p = beta1 + beta2 * n_avoid_flat + beta3 * n_shock_flat
        pm.Bernoulli("y", logit_p=logit_p, observed=y_flat)
    return m

def main():
    m = dogs_model()
    with m:
        idata = pm.sample(draws=1000, tune=1000, chains=4, cores=1,
                           nuts_sampler="pymc", random_seed=1,
                           progressbar=False, compute_convergence_checks=False)
        pm.sample_posterior_predictive(idata, extend_inferencedata=True,
                                        progressbar=False, random_seed=1)
    print(az.summary(idata).to_string())

    # Phase 89: PyMC 側の合成ダッシュボード (Haskell dashboardFullOf と対)。
    make_pymc_dashboard(m, idata, "y", figures_dir / "py_dashboard_full.svg")


if __name__ == "__main__":
    main()
