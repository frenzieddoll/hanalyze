"""hmm_example-hmm_example (posteriordb) — PyMC 実装。

Phase 89: posteriordb 横断ベンチマーク。単純な隠れマルコフモデル (K=2状態・
N=100観測・1次元Gaussian放出)。離散潜在状態は forward algorithm で周辺化
する (PyMC にネイティブの marginalized HMM は無いため `pytensor.scan` で
手動実装・Stan原典の forward algorithm と同型)。

Stan 原典 (posteriordb `models/stan/hmm_example.stan`):
  parameters { simplex[K] theta1; simplex[K] theta2; positive_ordered[K] mu; }
  model {
    mu[1] ~ normal(3,1); mu[2] ~ normal(10,1);
    gamma[1,k] = normal_lpdf(y[1]|mu[k],1);  -- pi0項なし(暗黙一様)
    gamma[t,k] = log_sum_exp_j(gamma[t-1,j] + log(theta[j,k])) + normal_lpdf(y[t]|mu[k],1);
    target += log_sum_exp(gamma[N]);
  }

`positive_ordered[K]` (mu[1]<mu[2]) は `pm.Deterministic` で
`mu2 = mu1 + gap` (gap>0) として表現し、hanalyze側 (Model.hs) と同じ
「gap自身の事前分布をpm.Potentialで打ち消してNormal(10,1)に置換」構成
にする (数学的に厳密・Stan原典と等価)。

**reference_posterior_name = "hmm_example-hmm_example"** (posteriordb
に公式referenceあり・hanalyze vs PyMC vs 公式referenceの3者比較可能)。

変数名は hanalyze 側 (Model.hs) に対応させる (Python 流 snake_case):
data_path/read_data/hmm_model/figures_dir/main。
"""
import json
from pathlib import Path

import arviz as az
import matplotlib
import numpy as np
import pymc as pm
import pytensor
import pytensor.tensor as pt

matplotlib.use("Agg")

data_path = Path(__file__).parent / "data" / "hmm_example.json"
figures_dir = Path(__file__).parent / "figures"


def read_data():
    with open(data_path) as f:
        d = json.load(f)
    return int(d["K"]), np.array(d["y"], dtype=float)


def forward_loglik(mu, trans, y):
    """log-space forward algorithm (Stan原典と同型。pi0項なし=log(1)=0)。"""
    # emit[t, k] = log N(y_t | mu_k, 1)
    emit = -0.5 * (y[:, None] - mu[None, :]) ** 2 - 0.5 * pt.log(2 * np.pi)
    log_trans = pt.log(trans)  # (K, K): log_trans[j, k] = log P(k|j)

    def step(emit_t, alpha_prev, log_trans):
        # alpha_t[k'] = logsumexp_j(alpha_prev[j] + log_trans[j,k']) + emit_t[k']
        return pt.logsumexp(alpha_prev[:, None] + log_trans, axis=0) + emit_t

    alpha0 = emit[0]  # pi0項なし
    alphas, _ = pytensor.scan(
        fn=step,
        sequences=[emit[1:]],
        outputs_info=[alpha0],
        non_sequences=[log_trans],
    )
    alpha_final = alphas[-1]
    return pt.logsumexp(alpha_final)


def hmm_model():
    k, y = read_data()
    with pm.Model() as m:
        mu1 = pm.Normal("mu_1", mu=3, sigma=1)
        gap = pm.HalfNormal("gap", sigma=5)
        mu2 = pm.Deterministic("mu_2", mu1 + gap)
        # gap自身のHalfNormal(5)寄与をpm.Potentialで打ち消しNormal(10,1)に置換
        pm.Potential("mu2_prior",
                     pm.logp(pm.Normal.dist(mu=10, sigma=1), mu2)
                     - pm.logp(pm.HalfNormal.dist(sigma=5), gap))
        mu = pt.stack([mu1, mu2])
        theta1 = pm.Dirichlet("theta1", a=np.ones(k))
        theta2 = pm.Dirichlet("theta2", a=np.ones(k))
        trans = pt.stack([theta1, theta2])
        pm.Potential("hmm_loglik", forward_loglik(mu, trans, y))
    return m


def main():
    m = hmm_model()
    with m:
        idata = pm.sample(draws=1000, tune=1000, chains=4, cores=1,
                           nuts_sampler="nutpie", random_seed=1,
                           progressbar=False, compute_convergence_checks=False)
    print(az.summary(idata, var_names=["mu_1", "mu_2", "gap", "theta1",
                                        "theta2"]).to_string())

    # HMMのようなpotentialベースの尤度はpm.sample_posterior_predictiveの
    # 対象になる観測ノードが無いため、PPCパネルは省略し forest/energy
    # のみ図示する (make_pymc_dashboard は観測ノード前提のため使えない)。
    import matplotlib.pyplot as plt
    az.plot_forest(idata, var_names=["mu_1", "mu_2", "theta1", "theta2"])
    plt.gcf().suptitle("PyMC dashboard: forest (HMM, no PPC — potential-only likelihood)")
    plt.gcf().savefig(figures_dir / "py_dashboard_forest.svg")
    plt.close("all")
    az.plot_energy(idata)
    plt.gcf().suptitle("PyMC dashboard: energy (HMM)")
    plt.gcf().savefig(figures_dir / "py_dashboard_full.svg")


if __name__ == "__main__":
    main()
