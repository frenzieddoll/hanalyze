"""gp_pois_regr-gp_regr (posteriordb) — PyMC 実装。

Phase 89/90: posteriordb 横断ベンチマーク + Phase 90 A2 (vecIR ギャップ解消:
07-gp-regr = GP カーネル + Cholesky分解)。

Stan 原典 (posteriordb `models/stan/gp_regr.stan` — data_name は
`gp_pois_regr` だが実際に走るのは `gp_regr` モデル・Gaussian 尤度):
  parameters { real<lower=0> rho; real<lower=0> alpha; real<lower=0> sigma; }
  model {
    matrix[N,N] cov = gp_exp_quad_cov(x, alpha, rho)
                      + diag_matrix(rep_vector(sigma, N));
    matrix[N,N] L_cov = cholesky_decompose(cov);
    rho ~ gamma(25, 4); alpha ~ normal(0, 2); sigma ~ normal(0, 1);
    y ~ multi_normal_cholesky(rep_vector(0, N), L_cov);
  }

reference_posterior_name = "gp_pois_regr-gp_regr" (posteriordb に公式
reference posterior あり・3者比較可能)。

変数名は hanalyze 側 (Model.hs) に対応させる (Python 流 snake_case):
data_path/read_data/gp_regr_model/figures_dir/main。
"""
import json
import sys
from pathlib import Path

import arviz as az
import matplotlib
import numpy as np
import pymc as pm
import pytensor.tensor as pt

sys.path.insert(0, str(Path(__file__).parent.parent))
from _common import make_pymc_dashboard  # noqa: E402 (sys.path 設定の直後)

matplotlib.use("Agg")

data_path = Path(__file__).parent / "data" / "gp_pois_regr.json"
figures_dir = Path(__file__).parent / "figures"


def read_data():
    with open(data_path) as f:
        d = json.load(f)
    return np.array(d["x"]), np.array(d["y"])


def gp_regr_model():
    x, y = read_data()
    n = len(x)
    with pm.Model() as m:
        rho = pm.Gamma("rho", alpha=25, beta=4)
        alpha = pm.HalfNormal("alpha", sigma=2)
        sigma = pm.HalfNormal("sigma", sigma=1)

        d = x[:, None] - x[None, :]
        cov = alpha**2 * pt.exp(-0.5 * d**2 / rho**2) + sigma * pt.eye(n)
        l_cov = pt.linalg.cholesky(cov)

        pm.MvNormal("y", mu=pt.zeros(n), chol=l_cov, observed=y)
    return m


def main():
    m = gp_regr_model()
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
