"""eight_schools-eight_schools_noncentered (posteriordb) — PyMC 実装。

Phase 89: posteriordb 横断ベンチマーク。階層モデルの正準例 8-schools
(Rubin 1981・8校の補習授業効果) を non-centered パラメタ化で実装する。

Stan 原典 (posteriordb `models/stan/eight_schools_noncentered.stan`):
  parameters { vector[J] theta_trans; real mu; real<lower=0> tau; }
  transformed parameters { theta = theta_trans * tau + mu; }
  model {
    theta_trans ~ normal(0, 1);
    y ~ normal(theta, sigma);   // sigma は既知データ (観測誤差)
    mu ~ normal(0, 5);
    tau ~ cauchy(0, 5);
  }

reference_posterior_name = "eight_schools-eight_schools_noncentered"
(posteriordb に公式 reference posterior あり・3者比較可能)

変数名は hanalyze 側 (Model.hs) に対応させる (Python 流 snake_case):
data_path/read_data/eight_schools_model/figures_dir/main。
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

data_path = Path(__file__).parent / "data" / "eight_schools.json"
figures_dir = Path(__file__).parent / "figures"


def read_data():
    with open(data_path) as f:
        d = json.load(f)
    return np.array(d["y"]), np.array(d["sigma"])


def eight_schools_model():
    y, sigma = read_data()
    with pm.Model() as m:
        mu = pm.Normal("mu", mu=0, sigma=5)
        tau = pm.HalfCauchy("tau", beta=5)
        eta = pm.Normal("eta", mu=0, sigma=1, shape=len(y))
        theta = pm.Deterministic("theta", mu + tau * eta)
        pm.Normal("y", mu=theta, sigma=sigma, observed=y)
    return m


def main():
    m = eight_schools_model()
    with m:
        idata = pm.sample(draws=1000, tune=1000, chains=4, cores=1,
                           nuts_sampler="pymc", random_seed=1,
                           progressbar=False, compute_convergence_checks=False)
        pm.sample_posterior_predictive(idata, extend_inferencedata=True,
                                        progressbar=False, random_seed=1)
    print(az.summary(idata).to_string())

    # Phase 89: PyMC 側の合成ダッシュボード (Haskell dashboardFullOf と対)。
    # figures/ は事前に用意されている前提 (Model.hs 側と同じく実行時に
    # ディレクトリを作らない)。
    make_pymc_dashboard(m, idata, "y", figures_dir / "py_dashboard_full.svg")


if __name__ == "__main__":
    main()
