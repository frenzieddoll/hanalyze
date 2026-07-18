"""mh: PyMC の CPU 限定 sampler×backend マトリクスを回し、最速を選ぶ。

Phase 89 の方針 (phase-88 のマトリクス手法を流用): pymc-own-NUTS×{Numba,CVM,JAX}・
nutpie×{Numba,JAX}・numpyro・blackjax を同条件 (CPU 1コア固定・4chain・
warmup1000+draws1000) で比較し、最速を "PyMC (最速CPU)" として記録する。
"""
import os
import sys
import time
import warnings

os.environ.setdefault("JAX_PLATFORMS", "cpu")
warnings.filterwarnings("ignore")
sys.path.insert(0, os.path.dirname(__file__))

import arviz as az  # noqa: E402 (sys.path 設定の直後)
import pymc as pm  # noqa: E402

from model import mh_model  # noqa: E402

combos = [
    ("pymc+numba", dict(nuts_sampler="pymc")),
    ("pymc+cvm", dict(nuts_sampler="pymc", compile_kwargs={"mode": "cvm"})),
    ("pymc+jax", dict(nuts_sampler="pymc", compile_kwargs={"mode": "jax"})),
    ("numpyro", dict(nuts_sampler="numpyro")),
    ("nutpie+numba", dict(nuts_sampler="nutpie")),
    ("nutpie+jax", dict(nuts_sampler="nutpie", compile_kwargs={"backend": "jax"})),
    ("blackjax", dict(nuts_sampler="blackjax")),
]


def main():
    for label, kwargs in combos:
        m = mh_model()
        t0 = time.perf_counter()
        with m:
            idata = pm.sample(draws=1000, tune=1000, chains=4, cores=1,
                               random_seed=1, progressbar=False,
                               compute_convergence_checks=False, **kwargs)
        ms = 1000.0 * (time.perf_counter() - t0)
        s = az.summary(idata, var_names=["omega"])
        ess = float(s.loc["omega", "ess_bulk"])
        rhat = float(s.loc["omega", "r_hat"])
        print(f"{label:14s} wall={ms:9.1f}ms  ess_bulk(omega)={ess:7.1f}  "
              f"ess/s={ess/(ms/1000):8.2f}  rhat={rhat:.3f}", flush=True)


if __name__ == "__main__":
    main()
