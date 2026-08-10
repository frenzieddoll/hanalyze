"""M0_data-M0_model (posteriordb) — PyMC 実装。

Phase 89: posteriordb 横断ベンチマーク。BPA本 (Kéry & Schaub 2011)
Ch.6 の最も単純な capture-recapture モデル M0 (定数の内包確率omega・
定数の検出確率p・05-mhのMhモデルから個体差ランダム効果を除いた
ベースライン変種)。

Stan 原典 (posteriordb `models/stan/M0_model.stan`):
  data { int M; int T; array[M,T] int y; }
  transformed data { s[i] = sum(y[i]); }  // 個体ごとの捕獲総数
  omega/p ~ 暗黙Uniform(0,1);
  for (i in 1:M):
    if (s[i] > 0): target += bernoulli_lpmf(1|omega) + binomial_lpmf(s[i]|T,p)
    else: target += log_sum_exp(bernoulli_lpmf(1|omega)+binomial_lpmf(0|T,p),
                                 bernoulli_lpmf(0|omega))

この尤度構造は ZeroInflatedBinomial(T, 1-omega, p) と数学的に厳密に
一致する (05-mh/README.mdで確立済みの導出)。PyMC側は忠実にStan原典の
if分岐+log_sum_exp構造で実装する (pm.Potentialでper-individualの対数
尤度を直書き)。

reference_posterior_name = null (posteriordb に公式 reference posterior 無し)。
本モデルは hanalyze vs PyMC の2者比較のみ。

★2026-07-12: コード準備のみ (データ未取得・実行は次回)。

変数名は hanalyze 側 (`Model.hs`) に対応させる (Python 流 snake_case):
data_path/read_data/m0_model/figures_dir/main。
"""
import json
import sys
from pathlib import Path

import arviz as az
import matplotlib
import numpy as np
import pymc as pm
import pytensor.tensor as pt

sys.path.insert(0, str(Path(__file__).parent.parent))
from _common import make_pymc_dashboard  # noqa: E402 (sys.path 設定の直後)

matplotlib.use("Agg")

data_path = Path(__file__).parent / "data" / "M0_data.json"
figures_dir = Path(__file__).parent / "figures"


def read_data():
    with open(data_path) as f:
        d = json.load(f)
    y = np.array(d["y"], dtype=int)
    return dict(T=d["T"], s=y.sum(axis=1))


def m0_model():
    d = read_data()
    t = d["T"]
    s = d["s"]
    with pm.Model() as m:
        omega = pm.Beta("omega", alpha=1.0, beta=1.0)
        p = pm.Beta("p", alpha=1.0, beta=1.0)
        binom_logp = pm.logp(pm.Binomial.dist(n=t, p=p), s)
        zero_case = pt.logaddexp(pt.log(omega) + pm.logp(pm.Binomial.dist(n=t, p=p), 0),
                                  pt.log(1 - omega))
        loglik = pt.switch(s > 0, pt.log(omega) + binom_logp, zero_case)
        pm.Potential("m0_loglik", pt.sum(loglik))
    return m


def main():
    m = m0_model()
    with m:
        idata = pm.sample(draws=1000, tune=1000, chains=4, cores=1,
                           nuts_sampler="pymc", random_seed=1,
                           progressbar=False, compute_convergence_checks=False)
    print(az.summary(idata).to_string())

    # m0_loglik は potential のみで尤度を構成 (observed RV が無い) ため
    # PPCパネルは空になる (_common.py 側で対応済み・20-bones/22-armaと同型)。
    make_pymc_dashboard(m, idata, "s", figures_dir / "py_dashboard_full.svg")


if __name__ == "__main__":
    main()
