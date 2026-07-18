"""TODO(<data_name>-<model_name>) (posteriordb) — PyMC 実装。

Phase 89: posteriordb 横断ベンチマーク雛形。00-template は posteriordb-bench
skill の「NN-<slug>/ をコピーして名前を置換」用のスケルトンであり、単体では
実データを持たない (data/ はダミー)。実例は ../01-glm-poisson/model.py を参照。

Stan 原典 (posteriordb `models/stan/<model_name>.stan`):
  TODO: Stan コードを貼り、構造をコメントしておく

reference_posterior_name: TODO (posteriordb の posteriors/<name>.json を確認。
null なら hanalyze vs PyMC の2者比較のみ、値があれば3者比較にする)

変数名は hanalyze 側 (Model.hs) に対応させる (Python 流 snake_case):
data_path/read_data/template_model/figures_dir/main。
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

# TODO: NN-<slug> / データファイル名に置換する。
data_path = Path(__file__).parent / "data" / "template_data.json"
figures_dir = Path(__file__).parent / "figures"


def read_data():
    with open(data_path) as f:
        d = json.load(f)
    return np.array(d["x"]), np.array(d["y"])


def template_model():
    x, y = read_data()
    with pm.Model() as m:
        # TODO: Stan 原典の prior 構造を忠実に移植する。
        alpha = pm.Uniform("alpha", lower=-20, upper=20)
        beta = pm.Uniform("beta", lower=-10, upper=10)
        pm.Normal("y", mu=alpha + beta * x, sigma=1, observed=y)
    return m


def main():
    m = template_model()
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
