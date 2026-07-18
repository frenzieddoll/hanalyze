"""irt_2pl-irt_2pl (posteriordb) — PyMC 実装。

Phase 89/90: posteriordb 横断ベンチマーク + Phase 90 A5 (vecIR ギャップ解消:
06-irt-2pl = 独立2ラテント配列を跨ぐ乗算項 a[i]*(theta[j]-b[i]))。

Stan 原典 (posteriordb `models/stan/irt_2pl.stan`。 I=20項目×J=100人):
  parameters { real<lower=0> sigma_theta; vector[J] theta;
               real<lower=0> sigma_a; vector<lower=0>[I] a;
               real mu_b; real<lower=0> sigma_b; vector[I] b; }
  model {
    sigma_theta ~ cauchy(0,2); theta ~ normal(0, sigma_theta);
    sigma_a ~ cauchy(0,2); a ~ lognormal(0, sigma_a);
    mu_b ~ normal(0,5); sigma_b ~ cauchy(0,2); b ~ normal(mu_b, sigma_b);
    for (i in 1:I) y[i] ~ bernoulli_logit(a[i] * (theta - b[i]));
  }

reference_posterior_name = null (posteriordb に公式 reference 無し・
hanalyze vs PyMC の2者比較のみ)。

変数名は hanalyze 側 (Model.hs) に対応させる (Python 流 snake_case):
data_path/read_data/irt_2pl_model/figures_dir/main。
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

data_path = Path(__file__).parent / "data" / "irt_2pl.json"
figures_dir = Path(__file__).parent / "figures"


def read_data():
    with open(data_path) as f:
        d = json.load(f)
    return np.array(d["y"])  # shape (I, J)


def irt_2pl_model():
    y = read_data()
    n_i, n_j = y.shape
    with pm.Model() as m:
        sigma_theta = pm.HalfCauchy("sigma_theta", beta=2)
        theta = pm.Normal("theta", mu=0, sigma=sigma_theta, shape=n_j)
        sigma_a = pm.HalfCauchy("sigma_a", beta=2)
        a = pm.LogNormal("a", mu=0, sigma=sigma_a, shape=n_i)
        mu_b = pm.Normal("mu_b", mu=0, sigma=5)
        sigma_b = pm.HalfCauchy("sigma_b", beta=2)
        b = pm.Normal("b", mu=mu_b, sigma=sigma_b, shape=n_i)
        logit_p = a[:, None] * (theta[None, :] - b[:, None])
        pm.Bernoulli("y", logit_p=logit_p, observed=y)
    return m


def main():
    m = irt_2pl_model()
    with m:
        idata = pm.sample(draws=1000, tune=1000, chains=4, cores=1,
                           nuts_sampler="pymc", random_seed=1,
                           progressbar=False, compute_convergence_checks=False)
        pm.sample_posterior_predictive(idata, extend_inferencedata=True,
                                        progressbar=False, random_seed=1)
    print(az.summary(idata, var_names=["sigma_theta", "sigma_a", "mu_b", "sigma_b"]).to_string())

    # Phase 89: PyMC 側の合成ダッシュボード (Haskell dashboardFullOf と対)。
    # figures/ は事前に用意されている前提 (Model.hs 側と同じく実行時に
    # ディレクトリを作らない)。
    make_pymc_dashboard(m, idata, "y", figures_dir / "py_dashboard_full.svg")


if __name__ == "__main__":
    main()
