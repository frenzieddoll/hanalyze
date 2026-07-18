"""traffic_accident_nyc-bym2_offset_only (posteriordb) — PyMC 実装。

Phase 90 A10-4: 13-traffic の PyMC 側比較 (デフォルト sampler のみ・
バックエンド行列は回さない = 2026-07-11 user 指示「大幅な負けが
あるかだけ知りたい」)。BYM2 空間疫学モデル (Morris et al. 2019) —
NYC 交通事故データ (N=1921地域・N_edges=5461隣接ペア)。

Stan 原典 (posteriordb `models/stan/bym2_offset_only.stan`):
  parameters { real beta0; real<lower=0> sigma; real<lower=0,upper=1> rho;
               vector[N] theta; vector[N] phi; }
  transformed parameters {
    convolved_re = sqrt(1-rho)*theta + sqrt(rho/scaling_factor)*phi;
  }
  model {
    y ~ poisson_log(log_E + beta0 + convolved_re*sigma);
    target += -0.5 * dot_self(phi[node1] - phi[node2]);  -- ICAR pairwise
    beta0 ~ normal(0,1); theta ~ normal(0,1); sigma ~ normal(0,1);
    rho ~ beta(0.5,0.5);
    sum(phi) ~ normal(0, 0.001*N);                        -- soft sum-to-zero
  }

★hanalyze 側 (Model.hs) と条件を揃えるため、phi には Haskell 実装と同じ
`Normal(0, 1000)` の diffuse 周辺 prior を与える (Stan 原典は improper
flat・Model.hs 冒頭コメント参照)。ICAR ペナルティとソフトゼロ和制約は
`pm.Potential` で移植する (Haskell 側の `potential` と対)。

reference_posterior_name = null (posteriordb に公式 reference 無し・
2者比較のみ)。

変数名は hanalyze 側 (Model.hs) に対応させる (Python 流 snake_case):
data_path/read_data/bym2_model/figures_dir/main。

実行 (repo root・単独実行 = 他ベンチと並走禁止):
  ~/.virtualenvs/pymc312/bin/python bench/posteriordb/13-traffic-accident-nyc/model.py
"""
import json
import sys
import time
from pathlib import Path

import arviz as az
import matplotlib
import numpy as np
import pymc as pm

sys.path.insert(0, str(Path(__file__).parent.parent))
from _common import make_pymc_dashboard  # noqa: E402 (sys.path 設定の直後)

matplotlib.use("Agg")

data_path = Path(__file__).parent / "data" / "traffic_accident_nyc.json"
figures_dir = Path(__file__).parent / "figures"


def read_data():
    with open(data_path) as f:
        d = json.load(f)
    return (int(d["N"]), np.array(d["node1"], dtype=int) - 1,
            np.array(d["node2"], dtype=int) - 1,
            np.array(d["y"], dtype=int), np.array(d["E"], dtype=float),
            float(d["scaling_factor"]))


def bym2_model():
    n, node1, node2, y, e_offset, scaling_factor = read_data()
    log_e = np.log(e_offset)
    with pm.Model() as m:
        beta0 = pm.Normal("beta0", mu=0, sigma=1)
        sigma = pm.HalfNormal("sigma", sigma=1)
        rho = pm.Beta("rho", alpha=0.5, beta=0.5)
        theta = pm.Normal("theta", mu=0, sigma=1, shape=n)
        phi = pm.Normal("phi", mu=0, sigma=1000, shape=n)  # Model.hs と同じ diffuse 近似
        convolved = (pm.math.sqrt(1 - rho) * theta
                     + pm.math.sqrt(rho / scaling_factor) * phi)
        pm.Poisson("y_obs", mu=pm.math.exp(log_e + beta0 + convolved * sigma),
                   observed=y)
        # ICAR ペア差分ペナルティ (Stan の target += -0.5*dot_self(...))
        pm.Potential("icar", -0.5 * pm.math.sum((phi[node1] - phi[node2]) ** 2))
        # ソフトゼロ和制約 (sum(phi) ~ normal(0, 0.001*N))
        pm.Potential("sum_zero",
                     pm.logp(pm.Normal.dist(mu=0, sigma=0.001 * n),
                             pm.math.sum(phi)))
    return m


def main():
    m = bym2_model()
    with m:
        t0 = time.monotonic()
        idata = pm.sample(draws=1000, tune=1000, chains=4, cores=1,
                          nuts_sampler="pymc", random_seed=1,
                          progressbar=False, compute_convergence_checks=False)
        t1 = time.monotonic()
    print(f"sampling wall = {(t1 - t0) * 1000:.1f} ms "
          "(pm.sample 全体・tune 込み・cores=1)")
    print(az.summary(idata, var_names=["beta0", "sigma", "rho"]).to_string())

    # dashboard は best-effort (venv 再構築で arviz が Phase 89 当時より
    # 新しく、_common 側の互換が未確認のため。失敗しても計測は成立)。
    try:
        with m:
            pm.sample_posterior_predictive(idata, extend_inferencedata=True,
                                           progressbar=False, random_seed=1)
        make_pymc_dashboard(m, idata, "y_obs",
                            figures_dir / "py_dashboard_full.svg")
    except Exception as exc:  # noqa: BLE001 (計測本体を守る)
        print(f"dashboard skipped: {exc}", file=sys.stderr)


if __name__ == "__main__":
    main()
