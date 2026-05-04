#!/usr/bin/env python3
"""Bayesian optimization benchmarks (B5) — Python side.

Branin (2D) and Hartmann6 (6D), 5 seeds, 30 evaluations after init.
Compares against ``skopt.gp_minimize``.
"""

from __future__ import annotations

import csv
import time
import warnings
from pathlib import Path
from statistics import median

import numpy as np
from skopt import gp_minimize

warnings.filterwarnings("ignore")


REPO = Path(__file__).resolve().parents[2]
OUT = REPO / "bench" / "results" / "python"
OUT.mkdir(parents=True, exist_ok=True)


def branin(x):
    x1, x2 = x[0], x[1]
    a = 1.0
    b = 5.1 / (4 * np.pi ** 2)
    c = 5.0 / np.pi
    r = 6.0
    s = 10.0
    t = 1.0 / (8 * np.pi)
    return float(a * (x2 - b * x1 ** 2 + c * x1 - r) ** 2
                 + s * (1 - t) * np.cos(x1) + s)


def hartmann6(x):
    alpha = np.array([1.0, 1.2, 3.0, 3.2])
    A = np.array([[10, 3, 17, 3.5, 1.7, 8],
                  [0.05, 10, 17, 0.1, 8, 14],
                  [3, 3.5, 1.7, 10, 17, 8],
                  [17, 8, 0.05, 10, 0.1, 14]])
    P = np.array([[0.1312, 0.1696, 0.5569, 0.0124, 0.8283, 0.5886],
                  [0.2329, 0.4135, 0.8307, 0.3736, 0.1004, 0.9991],
                  [0.2348, 0.1451, 0.3522, 0.2883, 0.3047, 0.6650],
                  [0.4047, 0.8828, 0.8732, 0.5743, 0.1091, 0.0381]])
    x = np.asarray(x)
    inner = (A * (x - P) ** 2).sum(axis=1)
    return float(-np.dot(alpha, np.exp(-inner)))


N_SEEDS = 5
N_INIT = 5
N_TOTAL = 35   # 5 init + 30 BO calls


def run_bo(fn, dims, n_init):
    seeds_t, seeds_y = [], []
    for s in range(N_SEEDS):
        t0 = time.perf_counter()
        res = gp_minimize(fn, dims, n_calls=N_TOTAL, n_initial_points=n_init,
                          random_state=s, n_jobs=1)
        seeds_t.append(1000.0 * (time.perf_counter() - t0))
        seeds_y.append(float(res.fun))
    return median(seeds_t), median(seeds_y)


def main() -> int:
    rows = []
    ms_b, y_b = run_bo(branin, [(-5.0, 10.0), (0.0, 15.0)], 5)
    rows.append({"name": "Branin/BO", "time_ms": ms_b,
                 "acc_main": y_b, "acc_aux": 0.397887,
                 "extra": "skopt.gp_minimize, 5 seeds"})
    ms_h, y_h = run_bo(hartmann6, [(0.0, 1.0)] * 6, 10)
    rows.append({"name": "Hartmann6/BO", "time_ms": ms_h,
                 "acc_main": y_h, "acc_aux": -3.32237,
                 "extra": "skopt.gp_minimize, 5 seeds"})

    out = OUT / "bo.csv"
    with open(out, "w", newline="") as fh:
        w = csv.writer(fh)
        w.writerow(["system", "suite", "name",
                    "time_ms", "acc_main", "acc_aux", "extra"])
        for r in rows:
            w.writerow(["python", "bo", r["name"],
                        f"{r['time_ms']:.6g}", f"{r['acc_main']:.6g}",
                        f"{r['acc_aux']:.6g}", r["extra"]])
    print(f"wrote {len(rows)} rows -> {out}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
