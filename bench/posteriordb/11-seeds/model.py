"""seeds_data-seeds_model (posteriordb) — PyMC 実装。

Phase 89: posteriordb 横断ベンチマーク。BUGS 古典例「種子発芽実験」
(Crowder 1978・I=21プレート・2種の種子×2種の根の抽出物の2x2要因計画+
overdispersion 用のプレートごとランダム切片)。

Stan 原典 (posteriordb `models/stan/seeds_model.stan`):
  parameters { real alpha0,alpha1,alpha2,alpha12; real<lower=0> tau;
               vector[I] b; }
  transformed parameters { sigma = 1/sqrt(tau); }
  model {
    alpha0,alpha1,alpha2,alpha12 ~ normal(0, 1000);
    tau ~ gamma(1e-3, 1e-3);
    b ~ normal(0, sigma);
    n ~ binomial_logit(N, alpha0 + alpha1*x1 + alpha2*x2 + alpha12*x1*x2 + b);
  }

reference_posterior_name = null (posteriordb に公式 reference 無し・
hanalyze vs PyMC の2者比較のみ)。

変数名は hanalyze 側 (Model.hs) に対応させる (Python 流 snake_case):
data_path/read_data/seeds_model/figures_dir/main。
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

data_path = Path(__file__).parent / "data" / "seeds_data.json"
figures_dir = Path(__file__).parent / "figures"


def read_data():
    with open(data_path) as f:
        d = json.load(f)
    return (int(d["I"]), np.array(d["n"], dtype=int), np.array(d["N"], dtype=int),
            np.array(d["x1"], dtype=float), np.array(d["x2"], dtype=float))


def seeds_model():
    i, n, ncap, x1, x2 = read_data()
    with pm.Model() as m:
        alpha0 = pm.Normal("alpha0", mu=0, sigma=1000)
        alpha1 = pm.Normal("alpha1", mu=0, sigma=1000)
        alpha2 = pm.Normal("alpha2", mu=0, sigma=1000)
        alpha12 = pm.Normal("alpha12", mu=0, sigma=1000)
        tau = pm.Gamma("tau", alpha=1.0e-3, beta=1.0e-3)
        sigma = 1.0 / pm.math.sqrt(tau)
        b = pm.Normal("b", mu=0, sigma=sigma, shape=i)
        eta = alpha0 + alpha1 * x1 + alpha2 * x2 + alpha12 * x1 * x2 + b
        pm.Binomial("n", n=ncap, logit_p=eta, observed=n)
    return m


def main():
    m = seeds_model()
    with m:
        idata = pm.sample(draws=1000, tune=1000, chains=4, cores=1,
                           nuts_sampler="pymc", random_seed=1,
                           progressbar=False, compute_convergence_checks=False)
        pm.sample_posterior_predictive(idata, extend_inferencedata=True,
                                        progressbar=False, random_seed=1)
    print(az.summary(idata, var_names=["alpha0", "alpha1", "alpha2",
                                        "alpha12", "tau"]).to_string())

    # Phase 89: PyMC 側の合成ダッシュボード (Haskell dashboardFullOf と対)。
    # figures/ は事前に用意されている前提 (Model.hs 側と同じく実行時に
    # ディレクトリを作らない)。
    make_pymc_dashboard(m, idata, "n", figures_dir / "py_dashboard_full.svg")


if __name__ == "__main__":
    main()
