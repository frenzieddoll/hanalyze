"""election88-election88_full (posteriordb) — PyMC 実装。

Phase 89: posteriordb 横断ベンチマーク。1988年米大統領選 世論調査の
多水準ロジスティック回帰 (Gelman & Hill 2006・N=11566・これまでで
最大のN)。★新ファミリ: 5本の独立な階層 (age/edu/age×edu交互作用/
state/regionそれぞれ別々のgroup-level intercept)。

Stan 原典 (posteriordb `models/stan/election88_full.stan`):
  y_hat[i] = beta[1] + beta[2]*black[i] + beta[3]*female[i]
           + beta[5]*female[i]*black[i] + beta[4]*v_prev_full[i]
           + a[age[i]] + b[edu[i]] + c[age_edu[i]] + d[state[i]] + e[region_full[i]]
  a~normal(0,sigma_a); b~normal(0,sigma_b); c~normal(0,sigma_c);
  d~normal(0,sigma_d); e~normal(0,sigma_e); beta~normal(0,100);
  sigma_a..e ~ implicit Uniform(0,100) (real<lower=0,upper=100>);
  y ~ bernoulli_logit(y_hat)

sigma_a..e は hanalyze側と揃え HalfCauchy(25) に置換 (10-ratsと同じ理由・
Uniform(0,X)のSD使用はunconstrained初期値の罠がある)。

reference_posterior_name = null (posteriordb に公式 reference posterior 無し)。
本モデルは hanalyze vs PyMC の2者比較のみ。

変数名は hanalyze 側 (`Model.hs`) に対応させる (Python 流 snake_case):
data_path/read_data/election_model/figures_dir/main。
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

data_path = Path(__file__).parent / "data" / "election88.json"
figures_dir = Path(__file__).parent / "figures"


def read_data():
    with open(data_path) as f:
        d = json.load(f)
    return dict(
        n_age=d["n_age"], n_edu=d["n_edu"], n_age_edu=d["n_age_edu"],
        n_state=d["n_state"], n_region_full=d["n_region_full"],
        age=np.array(d["age"]) - 1, age_edu=np.array(d["age_edu"]) - 1,
        black=np.array(d["black"], dtype=float), edu=np.array(d["edu"]) - 1,
        female=np.array(d["female"], dtype=float),
        region_full=np.array(d["region_full"]) - 1, state=np.array(d["state"]) - 1,
        v_prev_full=np.array(d["v_prev_full"], dtype=float),
        y=np.array(d["y"], dtype=int),
    )


def election_model():
    d = read_data()
    with pm.Model() as m:
        sigma_a = pm.HalfCauchy("sigma_a", beta=25.0)
        sigma_b = pm.HalfCauchy("sigma_b", beta=25.0)
        sigma_c = pm.HalfCauchy("sigma_c", beta=25.0)
        sigma_d = pm.HalfCauchy("sigma_d", beta=25.0)
        sigma_e = pm.HalfCauchy("sigma_e", beta=25.0)
        a = pm.Normal("a", mu=0.0, sigma=sigma_a, shape=d["n_age"])
        b = pm.Normal("b", mu=0.0, sigma=sigma_b, shape=d["n_edu"])
        c = pm.Normal("c", mu=0.0, sigma=sigma_c, shape=d["n_age_edu"])
        dd = pm.Normal("d", mu=0.0, sigma=sigma_d, shape=d["n_state"])
        e = pm.Normal("e", mu=0.0, sigma=sigma_e, shape=d["n_region_full"])
        beta1 = pm.Normal("beta1", mu=0.0, sigma=100.0)
        beta2 = pm.Normal("beta2", mu=0.0, sigma=100.0)
        beta3 = pm.Normal("beta3", mu=0.0, sigma=100.0)
        beta4 = pm.Normal("beta4", mu=0.0, sigma=100.0)
        beta5 = pm.Normal("beta5", mu=0.0, sigma=100.0)

        y_hat = (beta1 + beta2 * d["black"] + beta3 * d["female"]
                 + beta5 * d["female"] * d["black"] + beta4 * d["v_prev_full"]
                 + a[d["age"]] + b[d["edu"]] + c[d["age_edu"]]
                 + dd[d["state"]] + e[d["region_full"]])
        pm.Bernoulli("y", logit_p=y_hat, observed=d["y"])
    return m


def main():
    m = election_model()
    with m:
        idata = pm.sample(draws=1000, tune=1000, chains=4, cores=1,
                           nuts_sampler="pymc", random_seed=1,
                           progressbar=False, compute_convergence_checks=False)
        pm.sample_posterior_predictive(idata, extend_inferencedata=True,
                                        progressbar=False, random_seed=1)
    print(az.summary(idata, var_names=["beta1", "beta2", "beta3", "beta4", "beta5",
                                        "sigma_a", "sigma_b", "sigma_c", "sigma_d",
                                        "sigma_e"]).to_string())

    # Phase 89: PyMC 側の合成ダッシュボード (Haskell dashboardFullOf と対)。
    # figures/ は事前に用意されている前提 (Model.hs 側と同じく実行時に
    # ディレクトリを作らない)。
    make_pymc_dashboard(m, idata, "y", figures_dir / "py_dashboard_full.svg")


if __name__ == "__main__":
    main()
