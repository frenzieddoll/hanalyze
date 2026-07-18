"""13-traffic-accident-nyc (BYM2): PyMC の CPU 限定 sampler×backend マトリクスを回し、最速を選ぶ。

Phase 99 A1 (2026-07-14): Phase 90 は「大幅な負けがあるかだけ知りたい」で PyMC デフォルト
(pymc+numba) のみ計測していた。対 PyMC ギャップの正確な基準を得るため、最速 CPU 組み合わせを
このマシンで fresh 再測する ([[feedback-rebench-pymc-this-machine]])。手法は phase-88 の
マトリクス流用: pymc-own-NUTS×{Numba,CVM,JAX}・nutpie×{Numba,JAX}・numpyro・blackjax を同条件
(CPU 1コア固定・4chain・warmup1000+draws1000・seed1) で比較する。

BYM2 は pm.Potential 2 項 (icar / sum_zero) を含むため、backend によっては未対応でエラーに
なり得る。各組み合わせを try/except で隔離し、失敗はスキップして計測を続行する。

実行 (repo root・単独実行 = 他ベンチと並走禁止):
  ~/.virtualenvs/pymc312/bin/python bench/posteriordb/13-traffic-accident-nyc/run_pymc_matrix.py
"""
import os
import sys
import time
import traceback
import warnings

os.environ.setdefault("JAX_PLATFORMS", "cpu")
warnings.filterwarnings("ignore")
sys.path.insert(0, os.path.dirname(__file__))

import arviz as az  # noqa: E402 (sys.path 設定の直後)

from model import bym2_model  # noqa: E402
import pymc as pm  # noqa: E402

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
        try:
            m = bym2_model()
            t0 = time.perf_counter()
            with m:
                idata = pm.sample(draws=1000, tune=1000, chains=4, cores=1,
                                  random_seed=1, progressbar=False,
                                  compute_convergence_checks=False, **kwargs)
            ms = 1000.0 * (time.perf_counter() - t0)
            # ★wall は最優先で単独行出力 (この arviz 版は az.summary が str を返すため
            #   要約整形が失敗し得る。wall を要約と道連れにしない = Phase 99 A1 の教訓)。
            print(f"{label:14s} wall={ms:10.1f}ms", flush=True)
            # 要約は float() で明示変換 (mean/r_hat が str 型)。失敗しても wall は残る。
            try:
                s = az.summary(idata, var_names=["beta0", "sigma", "rho"])
                ess = float(s.loc["rho", "ess_bulk"])
                rhat = float(s.loc["rho", "r_hat"])
                b0 = float(s.loc["beta0", "mean"])
                sg = float(s.loc["sigma", "mean"])
                rh = float(s.loc["rho", "mean"])
                print(f"{'':14s}   ess_bulk(rho)={ess:7.1f}  ess/s={ess/(ms/1000):8.2f}  "
                      f"rhat={rhat:.3f}  beta0={b0:.3f} sigma={sg:.3f} rho={rh:.3f}",
                      flush=True)
            except Exception as sexc:  # noqa: BLE001 (要約失敗でも wall は成立)
                print(f"{'':14s}   summary skipped: {type(sexc).__name__}: {sexc}",
                      flush=True)
        except Exception as exc:  # noqa: BLE001 (1 backend の失敗で全体を止めない)
            print(f"{label:14s} FAILED: {type(exc).__name__}: {exc}", flush=True)
            traceback.print_exc()


if __name__ == "__main__":
    main()
