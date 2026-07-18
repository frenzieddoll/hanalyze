"""loss_curves-losscurve_sislob (posteriordb) — PyMC 実装。

Phase 89: posteriordb 横断ベンチマーク。保険数理の損失三角形
(loss reserving)。n_cohort=10 (契約年度)・n_time=10 (経過年)・
n_data=55 (= 10+9+...+1・下三角が未観測の古典的三角形構造)。Weibull型
成長曲線 (`growthmodel_id=1`、データで確認済み) で損失の発展パターンを
モデル化する。

Stan 原典 (posteriordb `models/stan/losscurve_sislob.stan`):
  gf[t] = 1 - exp(-(t/theta)^omega)                      // growth_factor_weibull
  lm[i] = LR[cohort_id[i]] * premium[cohort_id[i]] * gf[t_idx[i]]
  loss[i] ~ normal(lm[i], loss_sd*premium[cohort_id[i]])
  mu_LR ~ normal(0,0.5); sd_LR ~ lognormal(0,0.5); LR ~ lognormal(mu_LR,sd_LR)
  loss_sd ~ lognormal(0,0.7); omega/theta ~ lognormal(0,0.5)

reference_posterior_name = null (posteriordb に公式 reference posterior 無し)。
本モデルは hanalyze vs PyMC の2者比較のみ。

変数名は hanalyze 側 (`Model.hs`) に対応させる (Python 流 snake_case):
data_path/read_data/loss_model/figures_dir/main。
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

data_path = Path(__file__).parent / "data" / "loss_curves.json"
figures_dir = Path(__file__).parent / "figures"


def read_data():
    with open(data_path) as f:
        d = json.load(f)
    return dict(
        n_cohort=d["n_cohort"],
        n_time=d["n_time"],
        cohort_id=np.array(d["cohort_id"]) - 1,  # 0-based
        t_idx=np.array(d["t_idx"]) - 1,  # 0-based
        t_value=np.array(d["t_value"], dtype=float),
        premium=np.array(d["premium"], dtype=float),
        loss=np.array(d["loss"], dtype=float),
    )


def loss_model():
    d = read_data()
    with pm.Model() as m:
        omega = pm.LogNormal("omega", mu=0.0, sigma=0.5)
        theta = pm.LogNormal("theta", mu=0.0, sigma=0.5)
        mu_LR = pm.Normal("mu_LR", mu=0.0, sigma=0.5)
        sd_LR = pm.LogNormal("sd_LR", mu=0.0, sigma=0.5)
        LR = pm.LogNormal("LR", mu=mu_LR, sigma=sd_LR, shape=d["n_cohort"])
        loss_sd = pm.LogNormal("loss_sd", mu=0.0, sigma=0.7)

        gf = 1 - pm.math.exp(-((d["t_value"] / theta) ** omega))
        lm = LR[d["cohort_id"]] * d["premium"][d["cohort_id"]] * gf[d["t_idx"]]
        sd = loss_sd * d["premium"][d["cohort_id"]]
        pm.Normal("loss", mu=lm, sigma=sd, observed=d["loss"])
    return m


def main():
    m = loss_model()
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
    make_pymc_dashboard(m, idata, "loss", figures_dir / "py_dashboard_full.svg")


if __name__ == "__main__":
    main()
