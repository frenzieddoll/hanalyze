#!/usr/bin/env python3
"""B9 Optim+: Constrained / Adam / CMAESFull — Python 側。

Counterparts of ``bench/haskell/BenchOptimPlus.hs``. Writes
``bench/results/python/optim_plus.csv``.
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
# Constrained: scipy SLSQP on minimise (x-1)^2 + (y-2)^2 s.t. x+y=1.
# Closed-form: x*=0, y*=1, f*=2.
# ---------------------------------------------------------------------------

def bench_constrained() -> Row:
    from scipy.optimize import minimize

    def f(xy):
        return (xy[0] - 1) ** 2 + (xy[1] - 2) ** 2

    cons = [{"type": "eq", "fun": lambda xy: xy[0] + xy[1] - 1.0}]

    def run():
        r = minimize(f, [0.0, 0.0], method="SLSQP", constraints=cons,
                     options={"ftol": 1e-12, "maxiter": 200})
        return float(r.x[0]), float(r.x[1]), float(r.fun)

    ms, (x, y, fval) = median_time(run, n_iter=10)
    err = float(np.sqrt(x * x + (y - 1) ** 2))
    return Row(
        "Constrained_Quad2D_eq", ms, err, fval,
        f"scipy SLSQP; x={x:.6e} y={y:.6f} f={fval:.6f} err={err:.6e}",
    )


# ---------------------------------------------------------------------------
# Adam: minimise ‖x‖² in 50D, 1000 iter, lr=0.05. Pure numpy implementation
# (matches what hanalyze does — no torch overhead).
# ---------------------------------------------------------------------------

def bench_adam() -> Row:
    n = 50
    x0 = np.ones(n)

    def run():
        x = x0.copy()
        m = np.zeros(n)
        v = np.zeros(n)
        b1, b2, eps, lr = 0.9, 0.999, 1e-8, 0.05
        for t in range(1, 1001):
            g = 2 * x  # grad of ‖x‖²
            m = b1 * m + (1 - b1) * g
            v = b2 * v + (1 - b2) * g * g
            mh = m / (1 - b1 ** t)
            vh = v / (1 - b2 ** t)
            x -= lr * mh / (np.sqrt(vh) + eps)
        return float(np.sum(x * x))

    ms, fval = median_time(run, n_iter=10)
    return Row(
        "Adam_quad50D_iter1000", ms, fval, 0.0,
        f"numpy Adam lr=0.05 1000 iter; f_final={fval:.6e}",
    )


# ---------------------------------------------------------------------------
# CMAESFull: Rosenbrock 5D with cma library full-rank covariance.
# ---------------------------------------------------------------------------

def bench_cmaes_full() -> Row | None:
    try:
        import cma
    except ImportError as e:
        print(f"  skip CMAES: {e}")
        return None

    def rosen(x):
        return float(sum(100 * (x[i + 1] - x[i] ** 2) ** 2 + (1 - x[i]) ** 2
                         for i in range(len(x) - 1)))

    x0 = [-1.5] * 5

    def run():
        es = cma.CMAEvolutionStrategy(
            x0, 0.5,
            {"verbose": -9, "maxiter": 200, "tolfun": 1e-10,
             "seed": 1},
        )
        es.optimize(rosen)
        return float(es.result.fbest)

    ms, fval = median_time(run, n_iter=3)
    return Row(
        "CMAESFull_Rosenbrock5D_iter200", ms, fval, 0.0,
        f"cma library full-rank σ₀=0.5 maxiter=200; f_final={fval:.6e}",
    )


# ---------------------------------------------------------------------------

def main():
    rows: list[Row] = []
    for r in (bench_constrained(), bench_adam(), bench_cmaes_full()):
        if r is not None:
            rows.append(r)
            print(f"  {r.name:<32} {r.time_ms:>10.3f} ms  {r.extra}")

    out = OUT / "optim_plus.csv"
    with out.open("w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["system", "suite", "name", "time_ms",
                    "acc_main", "acc_aux", "extra"])
        for r in rows:
            w.writerow(["python", "optim_plus", r.name,
                        f"{r.time_ms:.6g}", f"{r.acc_main:.6g}",
                        f"{r.acc_aux:.6g}", r.extra])
    print(f"wrote {len(rows)} rows → {out}")


if __name__ == "__main__":
    main()
