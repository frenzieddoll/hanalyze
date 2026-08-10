"""earnings-logearn_interaction_z (posteriordb) — PyMC 実装。
log(earn) ~ z(height) + male + z(height)*male (heightを標準化)。
★2026-07-13: コード準備のみ (未実行)。
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
                height=np.array(d["height"], dtype=float),
                male=np.array(d["male"], dtype=float))


def logearn_interaction_z_model():
    d = read_data()
    log_earn = np.log(d["earn"])
    z_height = (d["height"] - d["height"].mean()) / d["height"].std(ddof=1)
    inter = z_height * d["male"]
    with pm.Model() as m:
        beta1 = pm.Normal("beta1", mu=0.0, sigma=1000.0)
        beta2 = pm.Normal("beta2", mu=0.0, sigma=1000.0)
        beta3 = pm.Normal("beta3", mu=0.0, sigma=1000.0)
        beta4 = pm.Normal("beta4", mu=0.0, sigma=1000.0)
        sigma = pm.HalfCauchy("sigma", beta=25.0)
        mu = beta1 + beta2 * z_height + beta3 * d["male"] + beta4 * inter
        pm.Normal("log_earn", mu=mu, sigma=sigma, observed=log_earn)
    return m


def main():
    m = logearn_interaction_z_model()
    with m:
        idata = pm.sample(draws=1000, tune=1000, chains=4, cores=1,
                           nuts_sampler="pymc", random_seed=1,
                           progressbar=False, compute_convergence_checks=False)
    import arviz as az
    print(az.summary(idata).to_string())


if __name__ == "__main__":
    main()
