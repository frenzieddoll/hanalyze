"""surgical_data-surgical_model (posteriordb) — PyMC 実装。

Phase 89: posteriordb 横断ベンチマーク。BUGS 古典例「12病院の心臓手術
死亡率」(N=12病院・階層二項ロジット・共変量なしの最も単純な階層形)。

Stan 原典 (posteriordb `models/stan/surgical_model.stan`):
  mu ~ normal(0,1000); sigmasq ~ inv_gamma(0.001,0.001); sigma=sqrt(sigmasq);
  b[i] ~ normal(mu, sigma);  r[i] ~ binomial_logit(n[i], b[i]);

reference_posterior_name = null (posteriordb に公式 reference posterior 無し)。
本モデルは hanalyze vs PyMC の2者比較のみ。

変数名は hanalyze 側 (`Model.hs`) に対応させる (Python 流 snake_case):
data_path/read_data/surgical_model/figures_dir/main。
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

data_path = Path(__file__).parent / "data" / "surgical_data.json"
figures_dir = Path(__file__).parent / "figures"


def read_data():
    with open(data_path) as f:
        d = json.load(f)
    return np.array(d["n"], dtype=int), np.array(d["r"], dtype=int)


def surgical_model():
    n, r = read_data()
    with pm.Model() as m:
        mu = pm.Normal("mu", mu=0.0, sigma=1000.0)
        sigmasq = pm.InverseGamma("sigmasq", alpha=0.001, beta=0.001)
        sigma = pm.Deterministic("sigma", pm.math.sqrt(sigmasq))
        b = pm.Normal("b", mu=mu, sigma=sigma, shape=len(n))
        pm.Deterministic("pop_mean", pm.math.invlogit(mu))
        pm.Binomial("r", n=n, logit_p=b, observed=r)
    return m


def main():
    m = surgical_model()
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
    make_pymc_dashboard(m, idata, "r", figures_dir / "py_dashboard_full.svg")


if __name__ == "__main__":
    main()
