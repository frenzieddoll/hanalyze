"""nes1972-nes (posteriordb) — PyMC 実装。

Phase 89: posteriordb 横断ベンチマーク。ARM本 (Gelman & Hill 2006) Ch.4
の政党支持度回帰 (National Election Studies 1972年調査・N=1330)。9変数の
線形回帰 (イデオロギー・人種・年齢層3ダミー・教育・性別・収入)。

Stan 原典 (posteriordb `models/stan/nes.stan`):
  transformed data {
    age30_44[n] = age_discrete[n]==2; age45_64[n] = age_discrete[n]==3;
    age65up[n]  = age_discrete[n]==4;
  }
  parameters { vector[9] beta; real<lower=0> sigma; }
  model {
    partyid7 ~ normal(beta[1] + beta[2]*real_ideo + beta[3]*race_adj
                     + beta[4]*age30_44 + beta[5]*age45_64 + beta[6]*age65up
                     + beta[7]*educ1 + beta[8]*gender + beta[9]*income, sigma);
  }

Stan 原典に明示的な prior 行は無い (暗黙の flat/improper prior)。hanalyze側
(Model.hs) と揃え beta_i ~ Normal(0,1000)・sigma ~ HalfCauchy(25) という
diffuse な代替を与える。

reference_posterior_name = "nes1972-nes" (posteriordb に公式 reference
あり・hanalyze vs PyMC vs 公式referenceの3者比較可能)。

変数名は hanalyze 側 (`Model.hs`) に対応させる (Python 流 snake_case):
data_path/read_data/nes_model/figures_dir/main。
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

data_path = Path(__file__).parent / "data" / "nes1972.json"
figures_dir = Path(__file__).parent / "figures"


def read_data():
    with open(data_path) as f:
        d = json.load(f)
    age_discrete = np.array(d["age_discrete"])
    return dict(
        partyid7=np.array(d["partyid7"], dtype=float),
        real_ideo=np.array(d["real_ideo"], dtype=float),
        race_adj=np.array(d["race_adj"], dtype=float),
        age30_44=(age_discrete == 2).astype(float),
        age45_64=(age_discrete == 3).astype(float),
        age65up=(age_discrete == 4).astype(float),
        educ1=np.array(d["educ1"], dtype=float),
        gender=np.array(d["gender"], dtype=float),
        income=np.array(d["income"], dtype=float),
    )


def nes_model():
    d = read_data()
    with pm.Model() as m:
        beta = pm.Normal("beta", mu=0.0, sigma=1000.0, shape=9)
        sigma = pm.HalfCauchy("sigma", beta=25.0)
        mu = (beta[0] + beta[1] * d["real_ideo"] + beta[2] * d["race_adj"]
              + beta[3] * d["age30_44"] + beta[4] * d["age45_64"] + beta[5] * d["age65up"]
              + beta[6] * d["educ1"] + beta[7] * d["gender"] + beta[8] * d["income"])
        pm.Normal("partyid7", mu=mu, sigma=sigma, observed=d["partyid7"])
    return m


def main():
    m = nes_model()
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
    make_pymc_dashboard(m, idata, "partyid7", figures_dir / "py_dashboard_full.svg")


if __name__ == "__main__":
    main()
