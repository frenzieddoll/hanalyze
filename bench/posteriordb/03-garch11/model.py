"""garch-garch11 (posteriordb) — PyMC 実装。

Phase 89: posteriordb 横断ベンチマーク。GARCH(1,1) 時系列モデル (T=200)。
分散 sigma[t] がパラメータ依存の逐次再帰 (sigma[t-1] から計算) という、
これまでの2モデル (GLM-Poisson・dogs) とは異なる構造。dogs の累積和は
observed data のみに依存する前処理だったが、本モデルの再帰は alpha0/alpha1/
beta1/mu という **サンプリング対象のパラメータに依存**するため、毎回の
勾配評価で再計算が必要 (前処理不可)。

Stan 原典 (posteriordb `models/stan/garch11.stan`):
  parameters {
    real mu;                                    // 無制約 (暗黙improper flat)
    real<lower=0> alpha0;                       // 下限のみ (暗黙improper half-flat)
    real<lower=0,upper=1> alpha1;                // 暗黙一様事前分布
    real<lower=0,upper=(1-alpha1)> beta1;        // 上限が alpha1 に依存 (定常性条件)
  }
  model {
    sigma[1] = sigma1;  // データ (定数)
    for (t in 2:T)
      sigma[t] = sqrt(alpha0 + alpha1*square(y[t-1]-mu) + beta1*square(sigma[t-1]));
    y ~ normal(mu, sigma);
  }

reference_posterior_name = "garch-garch11" — posteriordb に公式 reference
posterior あり (hanalyze vs PyMC vs 公式referenceの3者比較が可能)。

mu/alpha0 は Stan の暗黙improper flat prior (無制約 real / 下限のみ) を
そのまま移植できない (hanalyze の Uniform は有限区間が必要)。実用上の
proper prior (mu~Normal(0,10)・alpha0~HalfNormal(5)) で代替する
(GLM-Poissonの「Uniform境界で暗黙一様事前分布を移植」とは異なる妥協・
README「既知の課題」に記載)。alpha1/beta1 はStan原典の有界一様事前分布
(beta1の上限が alpha1 に依存する動的境界)を忠実に移植する。

変数名は hanalyze 側 (`Model.hs`) に対応させる (Python 流 snake_case):
data_path/read_data/garch11_model/figures_dir/main。
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

data_path = Path(__file__).parent / "data" / "garch_data.json"
figures_dir = Path(__file__).parent / "figures"


def read_data():
    with open(data_path) as f:
        d = json.load(f)
    return np.array(d["y"]), float(d["sigma1"])

def garch11_model():
    y, sigma1 = read_data()
    with pm.Model() as m:
        mu = pm.Normal("mu", mu=0, sigma=10)
        alpha0 = pm.HalfNormal("alpha0", sigma=5)
        alpha1 = pm.Uniform("alpha1", lower=0, upper=1)
        beta1 = pm.Uniform("beta1", lower=0, upper=1 - alpha1)

        # sigma[t] はパラメータ依存の逐次再帰 (前処理不可・毎回再計算)。
        sigmas = [pt.as_tensor_variable(sigma1)]
        for t in range(1, len(y)):
            prev = sigmas[t - 1]
            sigmas.append(pt.sqrt(alpha0 + alpha1 * (y[t - 1] - mu) ** 2
                                   + beta1 * prev ** 2))
        sigma = pt.stack(sigmas)

        pm.Normal("y", mu=mu, sigma=sigma, observed=y)
    return m

def main():
    m = garch11_model()
    with m:
        idata = pm.sample(draws=1000, tune=1000, chains=4, cores=1,
                           nuts_sampler="pymc", random_seed=1,
                           progressbar=False, compute_convergence_checks=False)
        pm.sample_posterior_predictive(idata, extend_inferencedata=True,
                                        progressbar=False, random_seed=1)
    print(az.summary(idata).to_string())

    # Phase 89: PyMC 側の合成ダッシュボード (Haskell dashboardFullOf と対)。
    make_pymc_dashboard(m, idata, "y", figures_dir / "py_dashboard_full.svg")


if __name__ == "__main__":
    main()
