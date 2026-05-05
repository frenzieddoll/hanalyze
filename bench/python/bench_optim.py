#!/usr/bin/env python3
"""Single-objective optimization benchmarks (B3) — Python side.

Compares scipy.optimize, cma, and pyswarms against hanalyze on a fixed
test-function suite. 30 seeds, median time + final f, success rate.
"""

from __future__ import annotations

import csv
import time
import warnings
from dataclasses import dataclass
from pathlib import Path
from statistics import median

import numpy as np
import scipy.optimize as opt

warnings.filterwarnings("ignore")


REPO = Path(__file__).resolve().parents[2]
OUT = REPO / "bench" / "results" / "python"
OUT.mkdir(parents=True, exist_ok=True)


# ---------------------------------------------------------------------------
# Test functions
# ---------------------------------------------------------------------------

def rosenbrock(x):
    x = np.asarray(x)
    return float(np.sum(100 * (x[1:] - x[:-1] ** 2) ** 2 + (1 - x[:-1]) ** 2))


def rastrigin(x):
    x = np.asarray(x)
    return float(10 * len(x) + np.sum(x ** 2 - 10 * np.cos(2 * np.pi * x)))


def sphere(x):
    x = np.asarray(x)
    return float(np.sum(x ** 2))


def ackley(x):
    x = np.asarray(x)
    n = len(x)
    s1 = float(np.sum(x ** 2) / n)
    s2 = float(np.sum(np.cos(2 * np.pi * x)) / n)
    return -20 * np.exp(-0.2 * np.sqrt(s1)) - np.exp(s2) + 20 + np.e


def levy(x):
    x = np.asarray(x)
    w = 1 + (x - 1) / 4
    term1 = float(np.sin(np.pi * w[0]) ** 2)
    term2 = float(np.sum((w[:-1] - 1) ** 2 *
                         (1 + 10 * np.sin(np.pi * w[:-1] + 1) ** 2)))
    term3 = float((w[-1] - 1) ** 2 * (1 + np.sin(2 * np.pi * w[-1]) ** 2))
    return term1 + term2 + term3


def griewank(x):
    x = np.asarray(x)
    s = float(np.sum(x ** 2)) / 4000
    p = float(np.prod(np.cos(x / np.sqrt(np.arange(1, len(x) + 1)))))
    return s - p + 1


def schwefel(x):
    x = np.asarray(x)
    d = len(x)
    return 418.9829 * d - float(np.sum(x * np.sin(np.sqrt(np.abs(x)))))


TESTS = [
    ("Rosenbrock_2D", 2, rosenbrock),
    ("Rosenbrock_10D", 10, rosenbrock),
    ("Rastrigin_10D", 10, rastrigin),
    ("Sphere_30D", 30, sphere),
    ("Ackley_10D", 10, ackley),
    ("Levy_10D", 10, levy),
    ("Griewank_10D", 10, griewank),
    ("Schwefel_5D", 5, schwefel),
]


# ---------------------------------------------------------------------------
# Algorithms
# ---------------------------------------------------------------------------

def algo_nelder(f, d, rng):
    x0 = rng.uniform(-2.0, 2.0, d)
    t0 = time.perf_counter()
    r = opt.minimize(f, x0, method="Nelder-Mead",
                     options={"maxiter": 5000, "xatol": 1e-8, "fatol": 1e-8})
    return float(r.fun), 1000.0 * (time.perf_counter() - t0)


def algo_lbfgs(f, d, rng):
    x0 = rng.uniform(-2.0, 2.0, d)
    t0 = time.perf_counter()
    r = opt.minimize(f, x0, method="L-BFGS-B",
                     options={"maxiter": 1000, "ftol": 1e-10})
    return float(r.fun), 1000.0 * (time.perf_counter() - t0)


def algo_de(f, d, rng):
    bounds = [(-5.0, 5.0)] * d
    t0 = time.perf_counter()
    r = opt.differential_evolution(f, bounds, seed=int(rng.integers(2**31)),
                                   maxiter=200, tol=1e-7, polish=False)
    return float(r.fun), 1000.0 * (time.perf_counter() - t0)


def algo_cmaes(f, d, rng):
    import cma
    x0 = rng.uniform(-2.0, 2.0, d)
    sigma = 0.5
    t0 = time.perf_counter()
    es = cma.CMAEvolutionStrategy(
        x0, sigma,
        {"verbose": -9, "maxiter": 200, "tolfun": 1e-8,
         "seed": int(rng.integers(2**31))},
    )
    es.optimize(f)
    return float(es.result.fbest), 1000.0 * (time.perf_counter() - t0)


def algo_sa(f, d, rng):
    bounds = [(-5.0, 5.0)] * d
    t0 = time.perf_counter()
    r = opt.dual_annealing(f, bounds, seed=int(rng.integers(2**31)),
                           maxiter=200)
    return float(r.fun), 1000.0 * (time.perf_counter() - t0)


def algo_pso(f, d, rng):
    import pyswarms.single.global_best as gb
    bounds = (np.full(d, -5.0), np.full(d, 5.0))
    options = {"c1": 0.5, "c2": 0.3, "w": 0.9}
    optr = gb.GlobalBestPSO(n_particles=30, dimensions=d,
                            options=options, bounds=bounds)
    t0 = time.perf_counter()
    cost, _ = optr.optimize(lambda xs: np.array([f(x) for x in xs]),
                            iters=200, verbose=False)
    return float(cost), 1000.0 * (time.perf_counter() - t0)


ALGOS = [
    ("NelderMead", algo_nelder),
    ("LBFGS",      algo_lbfgs),
    ("DE",         algo_de),
    ("CMAES",      algo_cmaes),
    ("SA",         algo_sa),
    ("PSO",        algo_pso),
]


# ---------------------------------------------------------------------------

N_SEEDS = 30
SUCCESS_THR = 1e-2


def main() -> int:
    rows = []
    for fname, d, f in TESTS:
        for aname, algo in ALGOS:
            fs, ts = [], []
            for s in range(N_SEEDS):
                rng = np.random.default_rng(s + hash(aname + fname) % 1000000)
                try:
                    fv, ms = algo(f, d, rng)
                except Exception as e:
                    fv, ms = float("inf"), 0.0
                fs.append(fv)
                ts.append(ms)
            med_f = median(fs)
            med_t = median(ts)
            succ = sum(1 for v in fs if abs(v) < SUCCESS_THR) / N_SEEDS
            rows.append({
                "name": f"{fname}/{aname}",
                "time_ms": med_t,
                "acc_main": med_f,
                "acc_aux": succ,
                "extra": f"median over {N_SEEDS} seeds",
            })

    out = OUT / "optim.csv"
    with open(out, "w", newline="") as fh:
        w = csv.writer(fh)
        w.writerow(["system", "suite", "name",
                    "time_ms", "acc_main", "acc_aux", "extra"])
        for r in rows:
            w.writerow(["python", "optim", r["name"],
                        f"{r['time_ms']:.6g}", f"{r['acc_main']:.6g}",
                        f"{r['acc_aux']:.6g}", r["extra"]])
    print(f"wrote {len(rows)} rows -> {out}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
