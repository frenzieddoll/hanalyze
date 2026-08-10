"""lsat_data-lsat_model (posteriordb) — PyMC 実装。

Phase 89: posteriordb 横断ベンチマーク。LSAT (法科大学院適性試験)
T=5問・N=1000人の Rasch (1PL) 型 二値IRT — Bock & Lieberman (1970)
古典例。★新ファミリ: 全項目共通の単一識別力 (1PL/Rasch)。

Stan 原典 (posteriordb `models/stan/lsat_model.stan`):
  data { int N; int R; int T; array[R] int culm; array[R,T] int response; }
  transformed data { r[T,N] を culm/response から展開 (パターン圧縮形式) }
  alpha[T] ~ normal(0,100);  theta[N] ~ normal(0,1);
  beta ~ normal(0,100) (real<lower=0> — 正の切断 = HalfNormal(100));
  for (k in 1:T): r[k] ~ bernoulli_logit(beta*theta - alpha[k]);

reference_posterior_name = null (posteriordb に公式 reference posterior 無し)。
本モデルは hanalyze vs PyMC の2者比較のみ。

★2026-07-12: コード準備のみ (データ未取得・実行は次回)。

変数名は hanalyze 側 (`Model.hs`) に対応させる (Python 流 snake_case):
data_path/read_data/lsat_model/figures_dir/main。
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

data_path = Path(__file__).parent / "data" / "lsat_data.json"
figures_dir = Path(__file__).parent / "figures"


def read_data():
    with open(data_path) as f:
        d = json.load(f)
    return dict(N=d["N"], T=d["T"], culm=d["culm"], response=d["response"])


def unpack_responses(culm, response):
    """culm (累積人数) と response (パターンごとの回答) から N×T の
    回答行列へ展開する (Stan の transformed data と同型ロジック)。"""
    counts = [c - p for c, p in zip(culm, [0] + culm[:-1])]
    rows = []
    for pat, cnt in zip(response, counts):
        rows.extend([pat] * cnt)
    return np.array(rows, dtype=int)


def lsat_model():
    d = read_data()
    r = unpack_responses(d["culm"], d["response"])  # shape (N, T)
    t = d["T"]
    n = d["N"]
    with pm.Model() as m:
        alpha = pm.Normal("alpha", mu=0.0, sigma=100.0, shape=t)
        theta = pm.Normal("theta", mu=0.0, sigma=1.0, shape=n)
        beta = pm.HalfNormal("beta", sigma=100.0)
        logit = beta * theta[:, None] - alpha[None, :]
        pm.Bernoulli("r", logit_p=logit, observed=r)
    return m


def main():
    m = lsat_model()
    with m:
        idata = pm.sample(draws=1000, tune=1000, chains=4, cores=1,
                           nuts_sampler="pymc", random_seed=1,
                           progressbar=False, compute_convergence_checks=False)
        pm.sample_posterior_predictive(idata, extend_inferencedata=True,
                                        progressbar=False, random_seed=1)
    print(az.summary(idata, var_names=["alpha", "beta"]).to_string())

    # Phase 89: PyMC 側の合成ダッシュボード (Haskell dashboardFullOf と対)。
    # figures/ は事前に用意されている前提 (Model.hs 側と同じく実行時に
    # ディレクトリを作らない)。
    make_pymc_dashboard(m, idata, "r", figures_dir / "py_dashboard_full.svg")


if __name__ == "__main__":
    main()
