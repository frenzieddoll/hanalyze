"""arma-arma11 (posteriordb) — PyMC 実装。

Phase 89: posteriordb 横断ベンチマーク。ARMA(1,1) 時系列 (T=200)。
★新ファミリ: AR成分とMA成分を併せ持つ時系列 (12-ark の純AR・03-garch11
の再帰的分散とは異なる構造)。

Stan 原典 (posteriordb `models/stan/arma11.stan`):
  mu ~ normal(0,10); phi ~ normal(0,2); theta ~ normal(0,2);
  sigma ~ cauchy(0,2.5);
  nu[1] = mu + phi*mu; err[1] = y[1]-nu[1];       // err[0]=0 とみなす
  for (t in 2:T) { nu[t] = mu+phi*y[t-1]+theta*err[t-1]; err[t]=y[t]-nu[t]; }
  err ~ normal(0, sigma);

err[t] が err[t-1] に依存する逐次再帰 (14-hmm-example と同系統) のため
`pytensor.scan` で err 列を構築し `pm.Potential` で尤度を加算する。

reference_posterior_name = "arma-arma11" (posteriordb に公式 reference
あり・hanalyze vs PyMC vs 公式referenceの3者比較可能)。

変数名は hanalyze 側 (`Model.hs`) に対応させる (Python 流 snake_case):
data_path/read_data/arma_model/figures_dir/main。
"""
import json
import sys
from pathlib import Path

import arviz as az
import matplotlib
import numpy as np
import pymc as pm
import pytensor
import pytensor.tensor as pt

sys.path.insert(0, str(Path(__file__).parent.parent))
from _common import make_pymc_dashboard  # noqa: E402 (sys.path 設定の直後)

matplotlib.use("Agg")

data_path = Path(__file__).parent / "data" / "arma.json"
figures_dir = Path(__file__).parent / "figures"


def read_data():
    with open(data_path) as f:
        d = json.load(f)
    return np.array(d["y"], dtype=float)


def arma_loglik(mu, phi, theta, sigma, y):
    """逐次再帰 err[t] = y[t] - (mu + phi*y[t-1] + theta*err[t-1]) を
    pytensor.scan で構築し、err ~ Normal(0,sigma) の対数尤度を返す。"""
    y_t = pt.as_tensor_variable(y)
    nu1 = mu + phi * mu
    err1 = y_t[0] - nu1

    def step(y_cur, y_prev, err_prev, mu_, phi_, theta_):
        nu = mu_ + phi_ * y_prev + theta_ * err_prev
        err = y_cur - nu
        return err

    errs_rest, _ = pytensor.scan(
        fn=step,
        sequences=[y_t[1:], y_t[:-1]],
        outputs_info=[err1],
        non_sequences=[mu, phi, theta],
    )
    all_errs = pt.concatenate([err1[None], errs_rest])
    # pm.logp(pm.Normal.dist(...), ...) は scan 経由の勾配計算で
    # RandomGeneratorVariable 関連の AttributeError を起こすため
    # (pytensor/pymcのバージョン起因のバグと見られる)、Normal対数密度を
    # 直接手計算する (RandomVariableノードを一切生成しない)。
    return pt.sum(-0.5 * pt.log(2 * np.pi) - pt.log(sigma) - 0.5 * (all_errs / sigma) ** 2)


def arma_model():
    y = read_data()
    with pm.Model() as m:
        mu = pm.Normal("mu", mu=0.0, sigma=10.0)
        phi = pm.Normal("phi", mu=0.0, sigma=2.0)
        theta = pm.Normal("theta", mu=0.0, sigma=2.0)
        sigma = pm.HalfCauchy("sigma", beta=2.5)
        pm.Potential("arma_loglik", arma_loglik(mu, phi, theta, sigma, y))
    return m


def main():
    m = arma_model()
    with m:
        idata = pm.sample(draws=1000, tune=1000, chains=4, cores=1,
                           nuts_sampler="pymc", random_seed=1,
                           progressbar=False, compute_convergence_checks=False)
    print(az.summary(idata, var_names=["mu", "phi", "theta", "sigma"]).to_string())

    # arma_loglik は potential のみで尤度を構成 (observed RV が無い) ため
    # PPCパネルは空になる (_common.py 側で対応済み・20-bones と同型)。
    make_pymc_dashboard(m, idata, "y", figures_dir / "py_dashboard_full.svg")


if __name__ == "__main__":
    main()
