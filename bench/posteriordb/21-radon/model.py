"""radon_mn-radon_hierarchical_intercept_noncentered (posteriordb) —
PyMC 実装。

Phase 89: posteriordb 横断ベンチマーク。Gelman ラドン多水準回帰の古典例
(mc-stan.org radon case study)。ミネソタ州 J=85郡・N=919家屋の屋内ラドン
濃度回帰 (郡ごとの varying intercept + 固定傾き2本、non-centered
パラメタ化)。

Stan 原典 (posteriordb `models/stan/radon_hierarchical_intercept_noncentered.stan`):
  parameters { vector[J] alpha_raw; vector[2] beta; real mu_alpha;
               real<lower=0> sigma_alpha; real<lower=0> sigma_y; }
  transformed parameters { alpha = mu_alpha + sigma_alpha * alpha_raw; }
  model {
    sigma_alpha ~ normal(0,1); sigma_y ~ normal(0,1);
    mu_alpha ~ normal(0,10); beta ~ normal(0,10); alpha_raw ~ normal(0,1);
    for (n in 1:N) {
      muj[n] = alpha[county_idx[n]] + log_uppm[n]*beta[1];
      mu[n] = muj[n] + floor_measure[n]*beta[2];
      log_radon[n] ~ normal(mu[n], sigma_y);
    }
  }

reference_posterior_name = null (posteriordb に公式 reference posterior 無し)。
本モデルは hanalyze vs PyMC の2者比較のみ。

変数名は hanalyze 側 (`Model.hs`) に対応させる (Python 流 snake_case):
data_path/read_data/radon_model/figures_dir/main。
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

data_path = Path(__file__).parent / "data" / "radon_mn.json"
figures_dir = Path(__file__).parent / "figures"


def read_data():
    with open(data_path) as f:
        d = json.load(f)
    return dict(
        J=d["J"],
        county_idx=np.array(d["county_idx"]) - 1,  # 0-based
        floor_measure=np.array(d["floor_measure"], dtype=float),
        log_radon=np.array(d["log_radon"], dtype=float),
        log_uppm=np.array(d["log_uppm"], dtype=float),
    )


def radon_model():
    d = read_data()
    with pm.Model() as m:
        mu_alpha = pm.Normal("mu_alpha", mu=0.0, sigma=10.0)
        sigma_alpha = pm.HalfNormal("sigma_alpha", sigma=1.0)
        sigma_y = pm.HalfNormal("sigma_y", sigma=1.0)
        beta1 = pm.Normal("beta1", mu=0.0, sigma=10.0)
        beta2 = pm.Normal("beta2", mu=0.0, sigma=10.0)
        alpha_raw = pm.Normal("alpha_raw", mu=0.0, sigma=1.0, shape=d["J"])
        alpha = pm.Deterministic("alpha", mu_alpha + sigma_alpha * alpha_raw)

        mu = alpha[d["county_idx"]] + d["log_uppm"] * beta1 + d["floor_measure"] * beta2
        pm.Normal("log_radon", mu=mu, sigma=sigma_y, observed=d["log_radon"])
    return m


def main():
    m = radon_model()
    with m:
        idata = pm.sample(draws=1000, tune=1000, chains=4, cores=1,
                           nuts_sampler="pymc", random_seed=1,
                           progressbar=False, compute_convergence_checks=False)
        pm.sample_posterior_predictive(idata, extend_inferencedata=True,
                                        progressbar=False, random_seed=1)
    print(az.summary(idata, var_names=["mu_alpha", "sigma_alpha", "sigma_y", "beta1", "beta2"]).to_string())

    # Phase 89: PyMC 側の合成ダッシュボード (Haskell dashboardFullOf と対)。
    # figures/ は事前に用意されている前提 (Model.hs 側と同じく実行時に
    # ディレクトリを作らない)。
    make_pymc_dashboard(m, idata, "log_radon", figures_dir / "py_dashboard_full.svg")


if __name__ == "__main__":
    main()
