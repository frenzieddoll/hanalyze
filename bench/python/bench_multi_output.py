#!/usr/bin/env python3
"""B12 Multi-output ベンチ — Python 側。

Counterparts of ``bench/haskell/BenchMultiOutput.hs``. Writes
``bench/results/python/multi_output.csv``.
"""
from __future__ import annotations

import csv
import time
from dataclasses import dataclass
from pathlib import Path

import numpy as np


REPO = Path(__file__).resolve().parents[2]
OUT = REPO / "bench" / "results" / "python"
OUT.mkdir(parents=True, exist_ok=True)


@dataclass
class Row:
    name: str
    time_ms: float
    acc_main: float
    acc_aux: float
    extra: str


def median_time(fn, n_iter: int) -> tuple[float, object]:
    times = []
    last = None
    for _ in range(n_iter):
        t0 = time.perf_counter()
        last = fn()
        t1 = time.perf_counter()
        times.append(1000.0 * (t1 - t0))
    times.sort()
    return times[len(times) // 2], last


# ---------------------------------------------------------------------------
# Deterministic data (must match Haskell side).
# ---------------------------------------------------------------------------

def design_x(n: int, p: int) -> np.ndarray:
    i = np.arange(n)[:, None]
    j = np.arange(p)[None, :]
    return (np.sin(i * 0.1 + j * 0.7)
            + 0.3 * np.cos(i * 0.05 + j))


def multi_y(x: np.ndarray, q: int) -> np.ndarray:
    n, p = x.shape
    coefs = np.array([
        [np.sin(j * (k + 1)) for j in range(p)] for k in range(q)
    ])
    y = x @ coefs.T
    bump = np.array([
        [0.05 * np.sin(i * 0.3 + k) for k in range(q)] for i in range(n)
    ])
    return y + bump


# ---------------------------------------------------------------------------

def bench_multi_lm() -> Row:
    from sklearn.linear_model import LinearRegression

    n, p, q = 2000, 10, 5
    x = design_x(n, p)
    y = multi_y(x, q)

    def run():
        m = LinearRegression()
        m.fit(x, y)
        yhat = m.predict(x)
        return float(np.sqrt(np.mean((yhat - y) ** 2)))

    ms, rmse = median_time(run, n_iter=10)
    return Row(
        "MultiLM_n2000_p10_q5", ms, rmse, 0.0,
        f"sklearn LinearRegression multi-Y; RMSE={rmse:.6f}",
    )


def bench_multi_gp() -> Row | None:
    """Independent-GP benchmark: q separate fit() calls. Matches
    hanalyze's @fitMultiGPMV@ which optimises hyperparameters per
    output independently."""
    try:
        from sklearn.gaussian_process import GaussianProcessRegressor
        from sklearn.gaussian_process.kernels import RBF, ConstantKernel
    except ImportError as e:
        print(f"  skip MultiGP: {e}")
        return None

    n, p, q = 200, 3, 3
    x = design_x(n, p)
    y = multi_y(x, q)

    def run():
        s = 0.0
        for k in range(q):
            kern = ConstantKernel() * RBF(length_scale=1.0)
            gp = GaussianProcessRegressor(
                kernel=kern, n_restarts_optimizer=0,
                normalize_y=False, alpha=1e-6,
            )
            gp.fit(x, y[:, k])
            mean = gp.predict(x)
            s += float(mean.sum())
        return s

    ms, _ = median_time(run, n_iter=3)
    return Row(
        "MultiGP_n200_p3_q3", ms, 0.0, 0.0,
        "sklearn GaussianProcessRegressor RBF q=3 outputs (independent GPs)",
    )


def bench_multi_gp_shared_hp() -> Row | None:
    """Shared-HP multi-output GP: sklearn's @fit(X, Y)@ with @Y :: (n, q)@
    optimises kernel hyperparameters once, then solves @α = K⁻¹ Y@ with
    a (q-column) RHS — one Cholesky factor reused across outputs.

    This is sklearn's native multi-output mode. hanalyze's
    @fitMultiGPMV@ does NOT have this mode (yet); each output gets its
    own HP optimization. So this bench exists to document the
    structural advantage sklearn has when shared HP is acceptable."""
    try:
        from sklearn.gaussian_process import GaussianProcessRegressor
        from sklearn.gaussian_process.kernels import RBF, ConstantKernel
    except ImportError as e:
        print(f"  skip MultiGP_sharedHP: {e}")
        return None

    n, p, q = 200, 3, 3
    x = design_x(n, p)
    y = multi_y(x, q)

    def run():
        kern = ConstantKernel() * RBF(length_scale=1.0)
        gp = GaussianProcessRegressor(
            kernel=kern, n_restarts_optimizer=0,
            normalize_y=False, alpha=1e-6,
        )
        gp.fit(x, y)              # Y is (n, q); shared HP across outputs
        mean = gp.predict(x)      # (n, q) prediction
        return float(mean.sum())

    ms, _ = median_time(run, n_iter=3)
    return Row(
        "MultiGP_n200_p3_q3_sharedHP", ms, 0.0, 0.0,
        "sklearn GPR fit(X, Y::(n,q)) — single HP optimization",
    )


# ---------------------------------------------------------------------------

def main():
    rows: list[Row] = []
    for r in (bench_multi_lm(), bench_multi_gp(), bench_multi_gp_shared_hp()):
        if r is not None:
            rows.append(r)
            print(f"  {r.name:<32} {r.time_ms:>10.3f} ms  {r.extra}")

    out = OUT / "multi_output.csv"
    with out.open("w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["system", "suite", "name", "time_ms",
                    "acc_main", "acc_aux", "extra"])
        for r in rows:
            w.writerow(["python", "multi_output", r.name,
                        f"{r.time_ms:.6g}", f"{r.acc_main:.6g}",
                        f"{r.acc_aux:.6g}", r.extra])
    print(f"wrote {len(rows)} rows → {out}")


if __name__ == "__main__":
    main()
