"""bones_data-bones_model (posteriordb) — PyMC 実装。

Phase 89: posteriordb 横断ベンチマーク。骨年齢の graded response IRT
モデル (BUGS 古典例)。nChild=13人の子供・nInd=34項目 (骨のX線指標)。
各項目の困難度カットポイント (gamma) と識別力 (delta) は固定データ
(未サンプル)・各子供の能力 theta のみが latent。

Stan 原典 (posteriordb `models/stan/bones_model.stan`):
  theta[i] ~ normal(0, 36);
  for each i,j:
    Q[i,j,k] = inv_logit(delta[j]*(theta[i]-gamma[j,k]))  for k=1..(ncat[j]-1)
    p[i,j,1] = 1-Q[i,j,1]; p[i,j,k] = Q[i,j,k-1]-Q[i,j,k]; p[i,j,ncat[j]] = Q[i,j,ncat[j]-1]
    if grade[i,j] != -1: target += log(p[i,j,grade[i,j]])   // 欠測はスキップ

reference_posterior_name = null (posteriordb に公式 reference posterior 無し)。
本モデルは hanalyze vs PyMC の2者比較のみ。

変数名は hanalyze 側 (`Model.hs`) に対応させる (Python 流 snake_case):
data_path/read_data/bones_model/figures_dir/main。
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

data_path = Path(__file__).parent / "data" / "bones_data.json"
figures_dir = Path(__file__).parent / "figures"


def read_data():
    with open(data_path) as f:
        d = json.load(f)
    return dict(ncat=d["ncat"], grade=d["grade"], delta=d["delta"], gamma=d["gamma"])


def bones_model():
    d = read_data()
    ncat, grade, delta, gamma = d["ncat"], d["grade"], d["delta"], d["gamma"]
    n_child = len(grade)
    n_ind = len(ncat)

    with pm.Model() as m:
        theta = pm.Normal("theta", mu=0.0, sigma=36.0, shape=n_child)

        loglik_terms = []
        for i in range(n_child):
            for j in range(n_ind):
                gr = grade[i][j]
                if gr == -1:
                    continue
                nc = ncat[j]
                k_max = nc - 1
                dl = delta[j]
                gm = gamma[j]
                qs = [pm.math.invlogit(dl * (theta[i] - gm[k])) for k in range(k_max)]
                if gr == 1:
                    p = 1 - qs[0]
                elif gr == nc:
                    p = qs[k_max - 1]
                else:
                    p = qs[gr - 2] - qs[gr - 1]
                loglik_terms.append(pm.math.log(p))
        pm.Potential("bones_loglik", pm.math.sum(loglik_terms))
    return m


def main():
    m = bones_model()
    with m:
        idata = pm.sample(draws=1000, tune=1000, chains=4, cores=1,
                           nuts_sampler="pymc", random_seed=1,
                           progressbar=False, compute_convergence_checks=False)
    print(az.summary(idata).to_string())

    # Phase 89: PyMC 側の合成ダッシュボード (Haskell dashboardFullOf と対)。
    # potential のみで尤度を構成 (observed likelihood ノードが無い) ため
    # PPCパネルは無し (_common.make_pymc_dashboard の obs_name=None 相当は
    # 未対応なので、theta の forest/energy のみのダッシュボードにする)。
    # figures/ は事前に用意されている前提 (Model.hs 側と同じく実行時に
    # ディレクトリを作らない)。
    make_pymc_dashboard(m, idata, None, figures_dir / "py_dashboard_full.svg")


if __name__ == "__main__":
    main()
