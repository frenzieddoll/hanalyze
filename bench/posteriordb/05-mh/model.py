"""Mh_data-Mh_model (posteriordb) — PyMC 実装。

Phase 89/90: posteriordb 横断ベンチマーク + Phase 90 A3 (vecIR ギャップ解消:
05-mh = capture-recapture・ZeroInflatedBinomial + 個体ごとのランダム効果)。

Stan 原典 (posteriordb `models/stan/Mh_model.stan`。 BPA本 Ch.6・M=385個体・
T=5回のサンプリング機会):
  parameters { real<lower=0,upper=1> omega; real<lower=0,upper=1> mean_p;
               real<lower=0,upper=5> sigma; vector[M] eps_raw; }
  transformed parameters { vector[M] eps = logit(mean_p) + sigma*eps_raw; }
  model {
    eps_raw ~ normal(0, 1);
    for (i in 1:M) {
      if (y[i] > 0)
        target += bernoulli_lpmf(1|omega) + binomial_logit_lpmf(y[i]|T,eps[i]);
      else
        target += log_sum_exp(bernoulli_lpmf(1|omega)+binomial_logit_lpmf(0|T,eps[i]),
                               bernoulli_lpmf(0|omega));
    }
  }

reference_posterior_name = null (posteriordb に公式 reference 無し・
hanalyze vs PyMC の2者比較のみ)。

PyMC の `pm.ZeroInflatedBinomial(psi, n, p)` は psi=構造的ゼロでない確率
(= Stan の omega と直接対応、hanalyze 側の ψ=1-omega という変換は hanalyze
の `ZeroInflatedBinomial` 自身の定義由来でありPyMC側には不要)。

変数名は hanalyze 側 (Model.hs) に対応させる (Python 流 snake_case):
data_path/read_data/mh_model/figures_dir/main。
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

data_path = Path(__file__).parent / "data" / "Mh_data.json"
figures_dir = Path(__file__).parent / "figures"


def read_data():
    with open(data_path) as f:
        d = json.load(f)
    return np.array(d["y"]), int(d["T"])


def mh_model():
    y, t = read_data()
    m_individuals = len(y)
    with pm.Model() as m:
        omega = pm.Uniform("omega", lower=0, upper=1)
        mean_p = pm.Uniform("mean_p", lower=0, upper=1)
        sigma = pm.Uniform("sigma", lower=0, upper=5)
        eps_raw = pm.Normal("eps_raw", mu=0, sigma=1, shape=m_individuals)
        eps = pt.log(mean_p / (1 - mean_p)) + sigma * eps_raw
        p = pm.math.sigmoid(eps)
        pm.ZeroInflatedBinomial("y", psi=omega, n=t, p=p, observed=y)
    return m


def main():
    m = mh_model()
    with m:
        idata = pm.sample(draws=1000, tune=1000, chains=4, cores=1,
                           nuts_sampler="pymc", random_seed=1,
                           progressbar=False, compute_convergence_checks=False)
        pm.sample_posterior_predictive(idata, extend_inferencedata=True,
                                        progressbar=False, random_seed=1)
    print(az.summary(idata, var_names=["omega", "mean_p", "sigma"]).to_string())

    # Phase 89: PyMC 側の合成ダッシュボード (Haskell dashboardFullOf と対)。
    # figures/ は事前に用意されている前提 (Model.hs 側と同じく実行時に
    # ディレクトリを作らない)。
    make_pymc_dashboard(m, idata, "y", figures_dir / "py_dashboard_full.svg")


if __name__ == "__main__":
    main()
