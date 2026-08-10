"""kilpisjarvi_mod-kilpisjarvi (posteriordb) — PyMC 実装。

Phase 89: posteriordb 横断ベンチマーク。キルピスヤルヴィ (フィン
ランド) の気候変動観測データに対する単純な線形回帰。★新ファミリ:
prior のハイパーパラメータ自体がデータとして与えられる
(`pmualpha`/`psalpha`/`pmubeta`/`psbeta`)。reference_posterior_name
あり (3者比較可能)。

Stan 原典 (posteriordb `models/stan/kilpisjarvi.stan`):
  data { ... real pmualpha; real psalpha; real pmubeta; real psbeta; ... }
  alpha ~ normal(pmualpha, psalpha);
  beta  ~ normal(pmubeta, psbeta);
  y ~ normal(alpha + beta*x, sigma);
  (sigma に明示的priorなし = 暗黙のflat prior・hanalyze側と揃え
  HalfCauchy(25)に置換)

reference_posterior_name = "kilpisjarvi_mod-kilpisjarvi" (posteriordb
に公式referenceあり・hanalyze vs PyMC vs 公式referenceの3者比較可能)。

★2026-07-12: コード準備のみ (データ未取得・実行は次回)。

変数名は hanalyze 側 (`Model.hs`) に対応させる (Python 流 snake_case):
data_path/read_data/kilpisjarvi_model/figures_dir/main。
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

data_path = Path(__file__).parent / "data" / "kilpisjarvi_mod.json"
figures_dir = Path(__file__).parent / "figures"


def read_data():
    with open(data_path) as f:
        d = json.load(f)
    return dict(
        x=np.array(d["x"], dtype=float), y=np.array(d["y"], dtype=float),
        pmualpha=d["pmualpha"], psalpha=d["psalpha"],
        pmubeta=d["pmubeta"], psbeta=d["psbeta"],
    )


def kilpisjarvi_model():
    d = read_data()
    with pm.Model() as m:
        alpha = pm.Normal("alpha", mu=d["pmualpha"], sigma=d["psalpha"])
        beta = pm.Normal("beta", mu=d["pmubeta"], sigma=d["psbeta"])
        sigma = pm.HalfCauchy("sigma", beta=25.0)
        pm.Normal("y", mu=alpha + beta * d["x"], sigma=sigma, observed=d["y"])
    return m


def main():
    m = kilpisjarvi_model()
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
