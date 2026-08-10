"""earnings-logearn_height (posteriordb) — PyMC 実装。
log(earn) ~ height の線形回帰。★2026-07-13: コード準備のみ (未実行)。
"""
import json
from pathlib import Path

import numpy as np
import pymc as pm

data_path = Path(__file__).parent / "data" / "earnings.json"


def read_data():
    with open(data_path) as f:
        d = json.load(f)
    return dict(earn=np.array(d["earn"], dtype=float),
                height=np.array(d["height"], dtype=float))


def logearn_height_model():
    d = read_data()
    log_earn = np.log(d["earn"])
    with pm.Model() as m:
        beta1 = pm.Normal("beta1", mu=0.0, sigma=1000.0)
        beta2 = pm.Normal("beta2", mu=0.0, sigma=1000.0)
        sigma = pm.HalfCauchy("sigma", beta=25.0)
        pm.Normal("log_earn", mu=beta1 + beta2 * d["height"], sigma=sigma, observed=log_earn)
    return m


def main():
    m = logearn_height_model()
    with m:
        idata = pm.sample(draws=1000, tune=1000, chains=4, cores=1,
                           nuts_sampler="pymc", random_seed=1,
                           progressbar=False, compute_convergence_checks=False)
    import arviz as az
    print(az.summary(idata).to_string())


if __name__ == "__main__":
    main()
