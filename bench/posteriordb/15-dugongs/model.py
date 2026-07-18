"""dugongs_data-dugongs_model (posteriordb) — PyMC 実装。

Phase 89: posteriordb 横断ベンチマーク。BUGS 古典例「ジュゴンの成長曲線」
(N=27頭・体長 Y と年齢 x の非線形漸近成長曲線回帰)。

Stan 原典 (posteriordb `models/stan/dugongs_model.stan`):
  parameters {
    real alpha; real beta;
    real<lower=.5,upper=1> lambda;
    real<lower=0> tau;
  }
  transformed parameters { sigma = 1/sqrt(tau); U3 = logit(lambda); }
  model {
    m[i] = alpha - beta * pow(lambda, x[i]);
    Y ~ normal(m, sigma);
    alpha ~ normal(0,1000); beta ~ normal(0,1000);
    lambda ~ uniform(.5,1); tau ~ gamma(.0001,.0001);
  }

reference_posterior_name = null (posteriordb に公式 reference posterior 無し)。
本モデルは hanalyze vs PyMC の 2 者比較のみ (3 者比較は不可)。

変数名は hanalyze 側 (`Model.hs`) に対応させる (Python 流 snake_case):
data_path/read_data/dugongs_model/figures_dir/main。
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

data_path = Path(__file__).parent / "data" / "dugongs_data.json"
figures_dir = Path(__file__).parent / "figures"


def read_data():
    with open(data_path) as f:
        d = json.load(f)
    return np.array(d["Y"]), np.array(d["x"])


def dugongs_model():
    y, x = read_data()
    with pm.Model() as m:
        alpha = pm.Normal("alpha", mu=0.0, sigma=1000.0)
        beta = pm.Normal("beta", mu=0.0, sigma=1000.0)
        lam = pm.Uniform("lambda", lower=0.5, upper=1.0)
        tau = pm.Gamma("tau", alpha=0.0001, beta=0.0001)
        sigma = pm.Deterministic("sigma", 1.0 / pm.math.sqrt(tau))
        pm.Deterministic("U3", pm.math.log(lam / (1.0 - lam)))
        mu = alpha - beta * lam**x
        pm.Normal("Y", mu=mu, sigma=sigma, observed=y)
    return m


def main():
    m = dugongs_model()
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
    make_pymc_dashboard(m, idata, "Y", figures_dir / "py_dashboard_full.svg")


if __name__ == "__main__":
    main()
