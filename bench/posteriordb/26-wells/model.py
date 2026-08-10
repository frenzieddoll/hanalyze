"""wells_data-wells_interaction_model (posteriordb) — PyMC 実装。

Phase 89: posteriordb 横断ベンチマーク。バングラデシュ井戸水ヒ素汚染
調査「井戸の切替行動」の交互作用ロジスティック回帰 (Gelman & Hill 2006)。
距離(dist)とヒ素濃度(arsenic)の交互作用項を含む二項ロジット。

Stan 原典 (posteriordb `models/stan/wells_interaction_model.stan`):
  transformed data { dist100 = dist/100; inter = dist100 .* arsenic; }
  switched ~ bernoulli_logit_glm([dist100, arsenic, inter], alpha, beta);
  (alpha/beta に明示的priorなし = 暗黙のflat prior・hanalyze側と揃え
  Normal(0,1000)に置換)

reference_posterior_name = null (posteriordb に公式 reference posterior 無し)。
本モデルは hanalyze vs PyMC の2者比較のみ。

★2026-07-12: コード準備のみ (データ未取得・実行は次回)。

変数名は hanalyze 側 (`Model.hs`) に対応させる (Python 流 snake_case):
data_path/read_data/wells_model/figures_dir/main。
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

data_path = Path(__file__).parent / "data" / "wells_data.json"
figures_dir = Path(__file__).parent / "figures"


def read_data():
    with open(data_path) as f:
        d = json.load(f)
    return dict(
        switched=np.array(d["switched"], dtype=int),
        dist=np.array(d["dist"], dtype=float),
        arsenic=np.array(d["arsenic"], dtype=float),
    )


def wells_model():
    d = read_data()
    dist100 = d["dist"] / 100.0
    inter = dist100 * d["arsenic"]
    with pm.Model() as m:
        alpha = pm.Normal("alpha", mu=0.0, sigma=1000.0)
        beta1 = pm.Normal("beta1", mu=0.0, sigma=1000.0)
        beta2 = pm.Normal("beta2", mu=0.0, sigma=1000.0)
        beta3 = pm.Normal("beta3", mu=0.0, sigma=1000.0)
        logit = alpha + beta1 * dist100 + beta2 * d["arsenic"] + beta3 * inter
        pm.Bernoulli("switched", logit_p=logit, observed=d["switched"])
    return m


def main():
    m = wells_model()
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
    make_pymc_dashboard(m, idata, "switched", figures_dir / "py_dashboard_full.svg")


if __name__ == "__main__":
    main()
