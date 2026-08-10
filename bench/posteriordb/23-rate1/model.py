"""Rate_1_data-Rate_1_model (posteriordb) — PyMC 実装。

Phase 89: posteriordb 横断ベンチマーク。Bayesian Cognitive Modeling
(Lee & Wagenmakers 2013) の最も単純な例「二項比率の推定」(n=10試行・
k=5成功)。★新ファミリ: 単一パラメータの共役Beta-Binomial。

Stan 原典 (posteriordb `models/stan/Rate_1_model.stan`):
  theta ~ beta(1,1); k ~ binomial(n, theta);

reference_posterior_name = null (posteriordb に公式 reference posterior 無し)。
本モデルは hanalyze vs PyMC の2者比較のみ。

変数名は hanalyze 側 (`Model.hs`) に対応させる (Python 流 snake_case):
data_path/read_data/rate1_model/figures_dir/main。
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

data_path = Path(__file__).parent / "data" / "Rate_1_data.json"
figures_dir = Path(__file__).parent / "figures"


def read_data():
    with open(data_path) as f:
        d = json.load(f)
    return d["n"], d["k"]


def rate1_model():
    n, k = read_data()
    with pm.Model() as m:
        theta = pm.Beta("theta", alpha=1.0, beta=1.0)
        pm.Binomial("k", n=n, p=theta, observed=np.array(k))
    return m


def main():
    m = rate1_model()
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
    make_pymc_dashboard(m, idata, "k", figures_dir / "py_dashboard_full.svg")


if __name__ == "__main__":
    main()
