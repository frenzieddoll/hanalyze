"""mcycle_splines-accel_splines (posteriordb) — PyMC 実装。

Phase 89: posteriordb 横断ベンチマーク。オートバイ衝突試験の加速度
スプライン回帰 (brms生成モデル・N=133・平均/分散とも39自由度の
thin-plate spline (bs=1線形項+38基底関数) で非線形推定する不均一分散
モデル)。★新ファミリ: スプライン基底回帰。

Stan 原典 (posteriordb `models/stan/accel_splines.stan`・brms生成):
  mu[n]    = Intercept       + Xs[n]·bs       + Zs_1_1[n]·s_1_1
  sigma[n] = exp(Intercept_sigma + Xs_sigma[n]·bs_sigma + Zs_sigma_1_1[n]·s_sigma_1_1)
  s_1_1 = sds_1_1 * zs_1_1;  s_sigma_1_1 = sds_sigma_1_1 * zs_sigma_1_1  (non-centered)
  Intercept ~ StudentT(3,-13,36);  Intercept_sigma ~ StudentT(3,0,10)
  zs_1_1/zs_sigma_1_1 ~ Normal(0,1) (各38成分)
  sds_1_1/sds_sigma_1_1 ~ half-StudentT(3,0,36) (下側切断)
  bs/bs_sigma: Stan原典に明示的prior無し (暗黙のflat prior・
  hanalyze側と揃え Normal(0,1000) に置換)
  Y ~ Normal(mu, sigma)

reference_posterior_name = null (posteriordb に公式 reference posterior 無し)。
本モデルは hanalyze vs PyMC の2者比較のみ。

変数名は hanalyze 側 (`Model.hs`) に対応させる (Python 流 snake_case):
data_path/read_data/spline_model/figures_dir/main。
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

data_path = Path(__file__).parent / "data" / "mcycle_splines.json"
figures_dir = Path(__file__).parent / "figures"


def read_data():
    with open(data_path) as f:
        d = json.load(f)
    return dict(
        Y=np.array(d["Y"], dtype=float),
        Xs=np.array(d["Xs"], dtype=float),
        Zs_1_1=np.array(d["Zs_1_1"], dtype=float),
        Xs_sigma=np.array(d["Xs_sigma"], dtype=float),
        Zs_sigma_1_1=np.array(d["Zs_sigma_1_1"], dtype=float),
        Ks=d["Ks"],
        Ks_sigma=d["Ks_sigma"],
        knots_1=d["knots_1"],
        knots_sigma_1=d["knots_sigma_1"],
    )


def spline_model():
    d = read_data()
    with pm.Model() as m:
        intercept = pm.StudentT("Intercept", nu=3, mu=-13.0, sigma=36.0)
        intercept_sigma = pm.StudentT("Intercept_sigma", nu=3, mu=0.0, sigma=10.0)
        bs = pm.Normal("bs", mu=0.0, sigma=1000.0, shape=d["Ks"])
        bs_sigma = pm.Normal("bs_sigma", mu=0.0, sigma=1000.0, shape=d["Ks_sigma"])

        sds_1_1 = pm.HalfStudentT("sds_1_1", nu=3, sigma=36.0)
        sds_sigma_1_1 = pm.HalfStudentT("sds_sigma_1_1", nu=3, sigma=36.0)
        zs_1_1 = pm.Normal("zs_1_1", mu=0.0, sigma=1.0, shape=d["knots_1"])
        zs_sigma_1_1 = pm.Normal("zs_sigma_1_1", mu=0.0, sigma=1.0, shape=d["knots_sigma_1"])
        s_1_1 = sds_1_1 * zs_1_1
        s_sigma_1_1 = sds_sigma_1_1 * zs_sigma_1_1

        mu = intercept + d["Xs"] @ bs + d["Zs_1_1"] @ s_1_1
        sigma = pm.math.exp(intercept_sigma + d["Xs_sigma"] @ bs_sigma
                             + d["Zs_sigma_1_1"] @ s_sigma_1_1)
        pm.Normal("Y", mu=mu, sigma=sigma, observed=d["Y"])
    return m


def main():
    m = spline_model()
    with m:
        idata = pm.sample(draws=1000, tune=1000, chains=4, cores=1,
                           nuts_sampler="pymc", random_seed=1,
                           progressbar=False, compute_convergence_checks=False)
        pm.sample_posterior_predictive(idata, extend_inferencedata=True,
                                        progressbar=False, random_seed=1)
    print(az.summary(idata, var_names=["Intercept", "Intercept_sigma",
                                        "sds_1_1", "sds_sigma_1_1"]).to_string())

    # Phase 89: PyMC 側の合成ダッシュボード (Haskell dashboardFullOf と対)。
    # figures/ は事前に用意されている前提 (Model.hs 側と同じく実行時に
    # ディレクトリを作らない)。
    make_pymc_dashboard(m, idata, "Y", figures_dir / "py_dashboard_full.svg")


if __name__ == "__main__":
    main()
