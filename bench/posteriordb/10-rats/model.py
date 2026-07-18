"""rats_data-rats_model (posteriordb) — PyMC 実装。

Phase 89: posteriordb 横断ベンチマーク。BUGS 古典例「ラットの成長曲線」
(30匹 × 5時点の体重・縦断的階層線形回帰)。ラットごとに独立な切片
alpha[i]・傾き beta[i] を持ち、両方とも部分プーリングされる。

Stan 原典 (posteriordb `models/stan/rats_model.stan`。"Model simplified"版・
sigma_y/sigma_alpha/sigma_beta は真に improper flat prior):
  parameters { array[N] real alpha; array[N] real beta;
               real mu_alpha; real mu_beta;
               real<lower=0> sigma_y; real<lower=0> sigma_alpha; real<lower=0> sigma_beta; }
  model {
    mu_alpha ~ normal(0, 100); mu_beta ~ normal(0, 100);
    alpha ~ normal(mu_alpha, sigma_alpha); beta ~ normal(mu_beta, sigma_beta);
    y[n] ~ normal(alpha[rat[n]] + beta[rat[n]] * (x[n] - xbar), sigma_y);
  }

improper flat prior は hanalyze 側に表現できないため、両言語とも
HalfCauchy(25) (09-eight-schools の tau ~ HalfCauchy(5) と同じ流儀) に
置換する (Model.hs 参照: 当初 Uniform(0,100) で試したが hanalyze 側の
「Uniform の制約変換未実装」罠に当たり sigma_y=0 初期値で尤度が退化・
HMC が全 warmup で発散した。HalfCauchy に切替えて解消)。

reference_posterior_name = null (posteriordb に公式 reference 無し・
hanalyze vs PyMC の2者比較のみ)。

変数名は hanalyze 側 (Model.hs) に対応させる (Python 流 snake_case):
data_path/read_data/rats_model/figures_dir/main。
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

data_path = Path(__file__).parent / "data" / "rats_data.json"
figures_dir = Path(__file__).parent / "figures"


def read_data():
    with open(data_path) as f:
        d = json.load(f)
    rat = np.array(d["rat"], dtype=int) - 1  # 0始まりに変換 (PyMC index)
    x = np.array(d["x"], dtype=float)
    y = np.array(d["y"], dtype=float)
    return int(d["N"]), rat, x, y, float(d["xbar"])


def rats_model():
    n, rat, x, y, xbar = read_data()
    with pm.Model() as m:
        mu_alpha = pm.Normal("mu_alpha", mu=0, sigma=100)
        mu_beta = pm.Normal("mu_beta", mu=0, sigma=100)
        sigma_y = pm.HalfCauchy("sigma_y", beta=25)
        sigma_alpha = pm.HalfCauchy("sigma_alpha", beta=25)
        sigma_beta = pm.HalfCauchy("sigma_beta", beta=25)
        alpha = pm.Normal("alpha", mu=mu_alpha, sigma=sigma_alpha, shape=n)
        beta = pm.Normal("beta", mu=mu_beta, sigma=sigma_beta, shape=n)
        mu = alpha[rat] + beta[rat] * (x - xbar)
        pm.Normal("y", mu=mu, sigma=sigma_y, observed=y)
    return m


def main():
    m = rats_model()
    with m:
        idata = pm.sample(draws=1000, tune=1000, chains=4, cores=1,
                           nuts_sampler="pymc", random_seed=1,
                           progressbar=False, compute_convergence_checks=False)
        pm.sample_posterior_predictive(idata, extend_inferencedata=True,
                                        progressbar=False, random_seed=1)
    print(az.summary(idata, var_names=["mu_alpha", "mu_beta", "sigma_y",
                                        "sigma_alpha", "sigma_beta"]).to_string())

    # Phase 89: PyMC 側の合成ダッシュボード (Haskell dashboardOf と対)。
    # figures/ は事前に用意されている前提 (Model.hs 側と同じく実行時に
    # ディレクトリを作らない)。
    make_pymc_dashboard(m, idata, "y", figures_dir / "py_dashboard_full.svg")


if __name__ == "__main__":
    main()
