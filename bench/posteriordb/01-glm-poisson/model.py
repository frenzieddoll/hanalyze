"""GLM_Poisson_Data-GLM_Poisson_model (posteriordb) — PyMC 実装。

Phase 89: posteriordb 横断ベンチマーク。BPA (Kery & Schaub 2011, Ch.3) の
個体数カウントデータ (n=40年) を3次多項式 Poisson 回帰でモデル化する。

Stan 原典 (posteriordb `models/stan/GLM_Poisson_model.stan`):
  parameters {
    real<lower=-20,upper=20> alpha;
    real<lower=-10,upper=10> beta1/beta2/beta3;
  }
  model {
    // 暗黙の一様事前分布 (Implicit uniform priors)
    log_lambda = alpha + beta1*year + beta2*year^2 + beta3*year^3;
    C ~ poisson_log(log_lambda);
  }

reference_posterior_name = null (posteriordb に公式 reference posterior 無し)。
本モデルは hanalyze vs PyMC の 2 者比較のみ (3 者比較は不可)。

変数名は hanalyze 側 (`Model.hs`) に対応させる (Python 流 snake_case):
data_path/read_data/glm_poisson_model/figures_dir/main。
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

data_path = Path(__file__).parent / "data" / "GLM_Poisson_Data.json"
figures_dir = Path(__file__).parent / "figures"


def read_data():
    with open(data_path) as f:
        d = json.load(f)
    return np.array(d["year"]), np.array(d["C"], dtype=int)

def glm_poisson_model():
    year, c = read_data()
    year_sq = year**2
    year_cb = year_sq * year
    with pm.Model() as m:
        # Stan の bounded implicit uniform prior を Uniform(lo,hi) で忠実に移植。
        # 区間内は密度定数 (勾配0) = 実質 flat prior。 尤度がほぼ全域を支配する
        # 想定 (境界 [-20,20]/[-10,10] は緩い箱に過ぎない)。
        alpha = pm.Uniform("alpha", lower=-20, upper=20)
        beta1 = pm.Uniform("beta1", lower=-10, upper=10)
        beta2 = pm.Uniform("beta2", lower=-10, upper=10)
        beta3 = pm.Uniform("beta3", lower=-10, upper=10)
        log_lambda = alpha + beta1 * year + beta2 * year_sq + beta3 * year_cb
        pm.Poisson("C", mu=pm.math.exp(log_lambda), observed=c)
    return m

def main():
    m = glm_poisson_model()
    with m:
        idata = pm.sample(draws=1000, tune=1000, chains=4, cores=1,
                           nuts_sampler="pymc", random_seed=1,
                           progressbar=False, compute_convergence_checks=False)
        pm.sample_posterior_predictive(idata, extend_inferencedata=True,
                                        progressbar=False, random_seed=1)
    print(az.summary(idata).to_string())

    # Phase 89: PyMC 側の合成ダッシュボード (Haskell dashboardFullOf と対)。
    # 構成: 上段 2x2 (DAG/forest/PPC/energy) + 下段 param ごと[事後分布|trace]。
    # figures/ は事前に用意されている前提 (Model.hs 側と同じく実行時に
    # ディレクトリを作らない)。
    make_pymc_dashboard(m, idata, "C", figures_dir / "py_dashboard_full.svg")


if __name__ == "__main__":
    main()
