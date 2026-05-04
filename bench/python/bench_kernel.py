#!/usr/bin/env python3
"""Kernel / GP benchmarks (B2) — Python side.

Run with::

    OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 \
        bench/venv/bin/python bench/python/bench_kernel.py
"""

from __future__ import annotations

import csv
import time
from dataclasses import dataclass
from pathlib import Path

import numpy as np
import pandas as pd
from sklearn.kernel_ridge import KernelRidge
from sklearn.kernel_approximation import RBFSampler
from sklearn.linear_model import Ridge
from sklearn.metrics.pairwise import rbf_kernel
from sklearn.gaussian_process import GaussianProcessRegressor
from sklearn.gaussian_process.kernels import RBF, ConstantKernel as C, WhiteKernel


REPO = Path(__file__).resolve().parents[2]
DATA = REPO / "bench" / "data"
OUT = REPO / "bench" / "results" / "python"
OUT.mkdir(parents=True, exist_ok=True)

H = 1.0
LAM = 1e-3
GAMMA = 1.0 / (2 * H * H)  # rbf_kernel uses gamma = 1/(2h²)


@dataclass
class Row:
    name: str
    time_ms: float
    acc_main: float
    acc_aux: float
    extra: str


def median_time(fn, n_iter: int) -> tuple[float, object]:
    times, last = [], None
    for _ in range(n_iter):
        t0 = time.perf_counter()
        last = fn()
        t1 = time.perf_counter()
        times.append(1000.0 * (t1 - t0))
    times.sort()
    return times[len(times) // 2], last


def load_xy(path: Path):
    df = pd.read_csv(path)
    return df.iloc[:, :-1].to_numpy(), df.iloc[:, -1].to_numpy()


def r2(y, yhat):
    mu = y.mean()
    sst = float(((y - mu) ** 2).sum())
    sse = float(((y - yhat) ** 2).sum())
    return 0.0 if sst == 0 else 1 - sse / sst


def rmse(y, yhat):
    return float(np.sqrt(((y - yhat) ** 2).mean()))


# ---------------------------------------------------------------------------

def bench_gram(path: Path, name: str) -> Row:
    x, _ = load_xy(path)

    def run():
        return rbf_kernel(x, x, gamma=GAMMA)

    ms, _ = median_time(run, 5)
    return Row(name, ms, 0.0, 0.0, f"sklearn.rbf_kernel n={x.shape[0]}")


def bench_kr(path: Path, name: str) -> Row:
    x, y = load_xy(path)

    def run():
        return KernelRidge(alpha=LAM, kernel="rbf", gamma=GAMMA).fit(x, y)

    ms, m = median_time(run, 3)
    yhat = m.predict(x)
    return Row(name, ms, r2(y, yhat), rmse(y, yhat),
               "sklearn.KernelRidge RBF")


def bench_nw(path: Path, name: str) -> Row:
    x, y = load_xy(path)

    def run():
        K = rbf_kernel(x, x, gamma=GAMMA)
        rowsum = K.sum(axis=1)
        rowsum[rowsum == 0] = 1.0
        return (K @ y) / rowsum

    ms, yhat = median_time(run, 3)
    return Row(name, ms, r2(y, yhat), rmse(y, yhat),
               "manual NW via rbf_kernel")


def bench_rff(path: Path, name: str, d: int) -> Row:
    x, y = load_xy(path)
    sampler = RBFSampler(gamma=GAMMA, n_components=d, random_state=0).fit(x)
    phi = sampler.transform(x)

    def run():
        return Ridge(alpha=LAM, fit_intercept=False).fit(phi, y)

    ms, m = median_time(run, 3)
    yhat = m.predict(phi)
    return Row(name, ms, r2(y, yhat), rmse(y, yhat),
               f"sklearn.RBFSampler+Ridge D={d}")


def bench_gp_fit(path: Path, name: str) -> Row:
    x, y = load_xy(path)
    kernel = C(1.0, constant_value_bounds="fixed") \
             * RBF(length_scale=H, length_scale_bounds="fixed") \
             + WhiteKernel(noise_level=0.05, noise_level_bounds="fixed")

    def run():
        gpr = GaussianProcessRegressor(kernel=kernel, optimizer=None,
                                       normalize_y=False)
        gpr.fit(x, y)
        return gpr

    ms, gpr = median_time(run, 3)
    yhat, std = gpr.predict(x, return_std=True)
    return Row(name, ms, r2(y, yhat), rmse(y, yhat),
               "sklearn.GaussianProcessRegressor RBF (HP fixed)")


def bench_gp_opt(path: Path, name: str) -> Row:
    x, y = load_xy(path)
    kernel = C(1.0) * RBF(length_scale=0.5) + WhiteKernel(noise_level=0.05)

    def run():
        gpr = GaussianProcessRegressor(kernel=kernel, normalize_y=False,
                                       n_restarts_optimizer=0)
        gpr.fit(x, y)
        return gpr

    ms, gpr = median_time(run, 2)
    yhat = gpr.predict(x)
    # length scale extracted from optimized kernel
    try:
        ell = float(gpr.kernel_.k1.k2.length_scale)
    except Exception:
        ell = float("nan")
    return Row(name, ms, r2(y, yhat), ell,
               "sklearn GP fit + optimize (L-BFGS-B)")


# ---------------------------------------------------------------------------

def main() -> int:
    rows: list[Row] = [
        bench_gram(DATA / "kernel_n500_p5.csv",  "GramMV_n500_p5"),
        bench_gram(DATA / "kernel_n1000_p5.csv", "GramMV_n1000_p5"),
        bench_gram(DATA / "kernel_n2000_p5.csv", "GramMV_n2000_p5"),
        bench_kr(DATA / "kernel_n500_p5.csv",  "KR_n500_p5"),
        bench_kr(DATA / "kernel_n1000_p5.csv", "KR_n1000_p5"),
        bench_kr(DATA / "kernel_n2000_p5.csv", "KR_n2000_p5"),
        bench_nw(DATA / "kernel_n1000_p5.csv", "NW_n1000_p5"),
        bench_rff(DATA / "kernel_n1000_p5.csv", "RFF_n1000_D256_p5", 256),
        bench_rff(DATA / "kernel_n2000_p5.csv", "RFF_n2000_D256_p5", 256),
        bench_gp_fit(DATA / "kernel_n500_p5.csv",  "GP_fit_n500_p5"),
        bench_gp_fit(DATA / "kernel_n1000_p5.csv", "GP_fit_n1000_p5"),
        bench_gp_opt(DATA / "kernel_n500_p5.csv",  "GP_opt_n500_p5"),
        # GPRobust: no direct sklearn equivalent — skipped.
    ]
    out = OUT / "kernel.csv"
    with open(out, "w", newline="") as fh:
        w = csv.writer(fh)
        w.writerow(["system", "suite", "name",
                    "time_ms", "acc_main", "acc_aux", "extra"])
        for r in rows:
            w.writerow(["python", "kernel", r.name,
                        f"{r.time_ms:.6g}", f"{r.acc_main:.6g}",
                        f"{r.acc_aux:.6g}", r.extra])
    print(f"wrote {len(rows)} rows -> {out}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
