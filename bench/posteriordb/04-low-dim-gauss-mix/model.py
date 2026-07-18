"""low_dim_gauss_mix-low_dim_gauss_mix (posteriordb) — PyMC 実装。

Phase 89/90: posteriordb 横断ベンチマーク + Phase 90 A3 (vecIR ギャップ解消:
04-low-dim-gauss-mix = 2成分Normal混合)。

Stan 原典 (posteriordb `models/stan/low_dim_gauss_mix.stan`。 N=1000):
  parameters { ordered[2] mu; array[2] real<lower=0> sigma;
               real<lower=0,upper=1> theta; }
  model {
    sigma ~ normal(0, 2); mu ~ normal(0, 2); theta ~ beta(5, 5);
    for (n in 1:N)
      target += log_mix(theta, normal_lpdf(y[n]|mu[1],sigma[1]),
                                normal_lpdf(y[n]|mu[2],sigma[2]));
  }

reference_posterior_name = "low_dim_gauss_mix-low_dim_gauss_mix"
(posteriordb に公式 reference posterior あり・3者比較可能)

PyMC 側は `ordered[2] mu` を `pm.Potential` 等で明示制約せず (hanalyze 側と
条件を揃える・後述「既知の課題」参照)、素の mu1/mu2 で実装する。

変数名は hanalyze 側 (Model.hs) に対応させる (Python 流 snake_case):
data_path/read_data/low_dim_gauss_mix_model/figures_dir/main。
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

data_path = Path(__file__).parent / "data" / "low_dim_gauss_mix.json"
figures_dir = Path(__file__).parent / "figures"


def read_data():
    with open(data_path) as f:
        d = json.load(f)
    return np.array(d["y"])


def low_dim_gauss_mix_model():
    y = read_data()
    with pm.Model() as m:
        mu1 = pm.Normal("mu1", mu=0, sigma=2)
        mu2 = pm.Normal("mu2", mu=0, sigma=2)
        sigma1 = pm.HalfNormal("sigma1", sigma=2)
        sigma2 = pm.HalfNormal("sigma2", sigma=2)
        theta = pm.Beta("theta", alpha=5, beta=5)
        comp1 = pm.Normal.dist(mu=mu1, sigma=sigma1)
        comp2 = pm.Normal.dist(mu=mu2, sigma=sigma2)
        pm.Mixture("y", w=[theta, 1 - theta], comp_dists=[comp1, comp2],
                   observed=y)
    return m


def main():
    m = low_dim_gauss_mix_model()
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
