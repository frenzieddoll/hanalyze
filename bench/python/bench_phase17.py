#!/usr/bin/env python3
"""Phase 1-7 features — Python comparison.

Compares hanalyze Phase 1-7 functionality against widely-used
Python libraries. The Haskell side (``bench/haskell/BenchPhase17.hs``)
writes the shared input CSVs (``bench/data/*.csv``) and a results CSV
at ``bench/results/haskell/phase17.csv``. This script reads the same
inputs, runs the Python equivalents, and writes
``bench/results/python/phase17.csv`` so the aggregator can join them
on (suite, name).

Coverage:
- Weibull MLE         → scipy.stats.weibull_min.fit
- One-way MANOVA      → numpy (Wilks Λ + Rao F, same algorithm as Haskell)
- Hotelling T² 1-samp → numpy (same algorithm)
- Lasso λ CV          → sklearn.linear_model.LassoCV
- Ridge λ CV          → sklearn.linear_model.RidgeCV
- LHS                 → scipy.stats.qmc.LatinHypercube
- Halton              → scipy.stats.qmc.Halton

Haskell-only (no direct Python equivalent):
- AugmentDesign (D-optimal sequential)
- DSD (Definitive Screening Design)
- SPC X̄-R chart

Run with::

    OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 \\
        bench/venv/bin/python bench/python/bench_phase17.py
"""

from __future__ import annotations

import csv
import time
from dataclasses import dataclass
from pathlib import Path

import numpy as np
from scipy import stats
from scipy.stats import qmc
from sklearn.linear_model import LassoCV, RidgeCV


REPO = Path(__file__).resolve().parents[2]
DATA = REPO / "bench" / "data"
OUT = REPO / "bench" / "results" / "python"
OUT.mkdir(parents=True, exist_ok=True)


@dataclass
class Row:
    name: str
    time_ms: float
    acc_main: float
    acc_aux: float
    extra: str


def timeit(n_iter: int, fn):
    times = []
    last = None
    for _ in range(n_iter):
        t0 = time.perf_counter()
        last = fn()
        t1 = time.perf_counter()
        times.append((t1 - t0) * 1000.0)
    times.sort()
    return times[len(times) // 2], last


# ---------------------------------------------------------------------------
# Weibull MLE
# ---------------------------------------------------------------------------

def bench_weibull_mle(n, true_k, true_lam):
    csv_path = DATA / f"weibull_n{n}.csv"
    with csv_path.open() as f:
        reader = csv.reader(f)
        next(reader)
        xs = np.array([float(row[0]) for row in reader])

    def fit_once():
        c, _loc, scale = stats.weibull_min.fit(xs, floc=0)
        return c, scale
    median_ms, (k_hat, lam_hat) = timeit(7, fit_once)
    return Row(
        name=f"WeibullMLE_n{n}",
        time_ms=median_ms,
        acc_main=abs(k_hat - true_k) / true_k,
        acc_aux=abs(lam_hat - true_lam) / true_lam,
        extra=f"trueK={true_k} trueLam={true_lam}",
    )


# ---------------------------------------------------------------------------
# MANOVA
# ---------------------------------------------------------------------------

def wilks_lambda_and_pvalue(groups):
    k = len(groups)
    p = groups[0].shape[1]
    ns = np.array([g.shape[0] for g in groups], dtype=float)
    total_n = ns.sum()
    group_means = np.array([g.mean(axis=0) for g in groups])
    overall_mean = (ns[:, None] * group_means).sum(axis=0) / total_n
    B = np.zeros((p, p))
    for n_i, mu_i in zip(ns, group_means):
        diff = mu_i - overall_mean
        B += n_i * np.outer(diff, diff)
    W = np.zeros((p, p))
    for g, mu_i in zip(groups, group_means):
        centered = g - mu_i
        W += centered.T @ centered
    wilks = np.linalg.det(W) / np.linalg.det(W + B)
    q = k - 1
    num_s = p ** 2 * q ** 2 - 4
    den_s = p ** 2 + q ** 2 - 5
    s = np.sqrt(num_s / den_s) if (den_s > 0 and num_s > 0) else 1.0
    m_adj = total_n - 1 - (p + q + 1) / 2
    df1 = p * q
    df2 = m_adj * s - (p * q - 2) / 2
    lam_1s = wilks ** (1 / s)
    F = ((1 - lam_1s) / lam_1s) * (df2 / df1)
    p_value = stats.f.sf(F, df1, df2) if (df2 > 0 and F > 0) else 1.0
    return wilks, p_value


def bench_manova(n_per_group, group_shift):
    csv_path = DATA / f"manova_3grp_n{n_per_group}.csv"
    with csv_path.open() as f:
        reader = csv.reader(f)
        next(reader)
        rows_raw = [(int(r[0]), float(r[1]), float(r[2])) for r in reader]
    n_groups = max(r[0] for r in rows_raw) + 1
    groups = [
        np.array([[r[1], r[2]] for r in rows_raw if r[0] == g])
        for g in range(n_groups)
    ]

    def fit_once():
        return wilks_lambda_and_pvalue(groups)
    median_ms, (wilks, pv) = timeit(7, fit_once)
    return Row(
        name=f"MANOVA_3grp_n{n_per_group}",
        time_ms=median_ms, acc_main=wilks, acc_aux=pv,
        extra=f"groupShift={group_shift} 2vars",
    )


# ---------------------------------------------------------------------------
# Hotelling T² (1-sample)
# ---------------------------------------------------------------------------

def hotelling_t2_1sample(X, mu0):
    n, p = X.shape
    xbar = X.mean(axis=0)
    diff = xbar - mu0
    # sample covariance (n-1 divisor)
    Xc = X - xbar
    S = Xc.T @ Xc / (n - 1)
    Sinv = np.linalg.solve(S, np.eye(p))
    t2 = n * (diff @ Sinv @ diff)
    F = (n - p) / ((n - 1) * p) * t2
    p_value = stats.f.sf(F, p, n - p)
    return t2, p_value


def bench_hotelling(n, shift):
    csv_path = DATA / f"hotelling_n{n}.csv"
    with csv_path.open() as f:
        reader = csv.reader(f)
        next(reader)
        X = np.array([[float(r[0]), float(r[1])] for r in reader])
    mu0 = np.array([0.0, 0.0])

    def fit_once():
        return hotelling_t2_1sample(X, mu0)
    median_ms, (t2, pv) = timeit(7, fit_once)
    return Row(
        name=f"HotellingT2_n{n}",
        time_ms=median_ms, acc_main=t2, acc_aux=pv,
        extra=f"shift={shift} 2vars mu0=(0,0)",
    )


# ---------------------------------------------------------------------------
# Lasso / Ridge CV
# ---------------------------------------------------------------------------

LAMBDA_GRID = [0.001, 0.01, 0.05, 0.1, 0.5, 1.0, 5.0]


def _read_lm_csv(csv_path):
    with csv_path.open() as f:
        reader = csv.reader(f)
        header = next(reader)
        # last col is "y"
        p = len(header) - 1
        rows = [list(map(float, r)) for r in reader]
    arr = np.array(rows)
    X = arr[:, :p]
    y = arr[:, p]
    return X, y


def bench_lasso_cv(n, p):
    csv_path = DATA / f"lm_n{n}_p{p}.csv"
    X, y = _read_lm_csv(csv_path)

    # sklearn LassoCV expects alphas (= our lambdas). cv=5 to match Haskell.
    def fit_once():
        model = LassoCV(alphas=LAMBDA_GRID, cv=5, fit_intercept=False,
                        max_iter=10000)
        model.fit(X, y)
        return model.alpha_
    median_ms, best_alpha = timeit(5, fit_once)
    return Row(
        name=f"LassoCV_n{n}_p{p}",
        time_ms=median_ms, acc_main=best_alpha, acc_aux=0.0,
        extra=f"grid={len(LAMBDA_GRID)} folds=5",
    )


def bench_ridge_cv(n, p):
    csv_path = DATA / f"lm_n{n}_p{p}.csv"
    X, y = _read_lm_csv(csv_path)

    def fit_once():
        model = RidgeCV(alphas=LAMBDA_GRID, cv=5, fit_intercept=False)
        model.fit(X, y)
        return model.alpha_
    median_ms, best_alpha = timeit(5, fit_once)
    return Row(
        name=f"RidgeCV_n{n}_p{p}",
        time_ms=median_ms, acc_main=best_alpha, acc_aux=0.0,
        extra=f"grid={len(LAMBDA_GRID)} folds=5",
    )


# ---------------------------------------------------------------------------
# Space-filling: LHS / Halton (scipy.stats.qmc)
# ---------------------------------------------------------------------------

def _min_pairwise_distance(mat):
    n = mat.shape[0]
    best = float("inf")
    for i in range(n - 1):
        diff = mat[i + 1:] - mat[i]
        d = np.sqrt((diff ** 2).sum(axis=1)).min()
        if d < best:
            best = d
    return best if best < float("inf") else 0.0


def bench_lhs(n, d):
    def gen_once():
        rng = np.random.default_rng(42)
        sampler = qmc.LatinHypercube(d, seed=rng)
        return sampler.random(n)
    median_ms, mat = timeit(5, gen_once)
    return Row(
        name=f"LHS_n{n}_d{d}",
        time_ms=median_ms, acc_main=_min_pairwise_distance(mat),
        acc_aux=0.0, extra="method=LHS",
    )


def bench_halton(n, d):
    def gen_once():
        sampler = qmc.Halton(d, seed=42)
        return sampler.random(n)
    median_ms, mat = timeit(5, gen_once)
    return Row(
        name=f"Halton_n{n}_d{d}",
        time_ms=median_ms, acc_main=_min_pairwise_distance(mat),
        acc_aux=0.0, extra="method=Halton",
    )


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    rows = []

    # Weibull MLE
    for n in [100, 1000, 10000]:
        rows.append(bench_weibull_mle(n, 2.0, 10.0))

    # MANOVA
    for n_per in [30, 100, 500]:
        rows.append(bench_manova(n_per, 1.5))

    # Hotelling T²
    for n in [50, 200, 1000]:
        rows.append(bench_hotelling(n, 0.5))

    # Lasso / Ridge CV
    for n, p in [(200, 10), (500, 20)]:
        rows.append(bench_lasso_cv(n, p))
    for n, p in [(200, 10), (500, 20)]:
        rows.append(bench_ridge_cv(n, p))

    # SpaceFilling
    for n, d in [(50, 2), (200, 3)]:
        rows.append(bench_lhs(n, d))
    for n, d in [(50, 2), (200, 3)]:
        rows.append(bench_halton(n, d))

    # Augment / DSD / SPC: no direct Python equivalent; skip
    # (Haskell rows still appear in haskell phase17.csv for reference.)

    out_path = OUT / "phase17.csv"
    with out_path.open("w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["system", "suite", "name", "time_ms", "acc_main",
                    "acc_aux", "extra"])
        for r in rows:
            w.writerow(["python", "phase17", r.name,
                        f"{r.time_ms:.6g}", f"{r.acc_main:.6g}",
                        f"{r.acc_aux:.6g}", r.extra])
    print(f"✓ {out_path.relative_to(REPO)} written ({len(rows)} rows)")


if __name__ == "__main__":
    main()
