#!/usr/bin/env python3
"""Regression benchmarks (B1) — Python side.

Runs the Python equivalents of every benchmark in
``bench/haskell/BenchRegression.hs`` over the shared CSVs in
``bench/data/``. Writes ``bench/results/python/regression.csv`` with the
unified BenchRow schema.

Run with::

    OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 \
        bench/venv/bin/python bench/python/bench_regression.py
"""

from __future__ import annotations

import csv
import os
import time
from dataclasses import dataclass
from pathlib import Path

import numpy as np
import pandas as pd
from sklearn.linear_model import (
    LinearRegression,
    LogisticRegression,
    PoissonRegressor,
    Ridge,
    Lasso,
    ElasticNet,
)
import statsmodels.api as sm
import statsmodels.formula.api as smf


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


def median_time(fn, n_iter: int) -> tuple[float, object]:
    """Run *fn* `n_iter` times, return (median ms, last result)."""
    times = []
    last = None
    for _ in range(n_iter):
        t0 = time.perf_counter()
        last = fn()
        t1 = time.perf_counter()
        times.append(1000.0 * (t1 - t0))
    times.sort()
    return times[len(times) // 2], last


def load_xy(path: Path) -> tuple[np.ndarray, np.ndarray]:
    df = pd.read_csv(path)
    return df.iloc[:, :-1].to_numpy(), df.iloc[:, -1].to_numpy()


def load_xyg(path: Path) -> tuple[np.ndarray, np.ndarray, np.ndarray]:
    df = pd.read_csv(path)
    g = df["group"].to_numpy()
    y = df["y"].to_numpy()
    x = df.drop(columns=["group", "y"]).to_numpy()
    return x, g, y


def r2(y, yhat):
    mu = y.mean()
    sst = float(((y - mu) ** 2).sum())
    sse = float(((y - yhat) ** 2).sum())
    return 0.0 if sst == 0 else 1 - sse / sst


def rmse(y, yhat):
    return float(np.sqrt(((y - yhat) ** 2).mean()))


# ---------------------------------------------------------------------------
# LM (OLS)
# ---------------------------------------------------------------------------

def bench_lm(path: Path, name: str) -> Row:
    x, y = load_xy(path)

    def run():
        m = LinearRegression(fit_intercept=True).fit(x, y)
        # ensure prediction also forces internal computation
        return m, m.predict(x[:1])

    ms, (m, _) = median_time(run, 5)
    yhat = m.predict(x)
    return Row(name, ms, r2(y, yhat), rmse(y, yhat),
               "sklearn.LinearRegression")


# ---------------------------------------------------------------------------
# GLM Logistic / Poisson
# ---------------------------------------------------------------------------

def bench_logit(path: Path, name: str) -> Row:
    x, y = load_xy(path)

    def run():
        m = LogisticRegression(
            penalty=None, fit_intercept=True, solver="lbfgs",
            max_iter=200, tol=1e-6,
        ).fit(x, y)
        return m

    ms, m = median_time(run, 3)
    p = m.predict_proba(x)[:, 1]
    return Row(name, ms, r2(y, p), rmse(y, p),
               "sklearn.LogisticRegression (lbfgs)")


def bench_poisson(path: Path, name: str) -> Row:
    x, y = load_xy(path)

    def run():
        m = PoissonRegressor(
            alpha=0.0, fit_intercept=True, solver="lbfgs",
            max_iter=200, tol=1e-6,
        ).fit(x, y)
        return m

    ms, m = median_time(run, 3)
    yhat = m.predict(x)
    return Row(name, ms, r2(y, yhat), rmse(y, yhat),
               "sklearn.PoissonRegressor")


# ---------------------------------------------------------------------------
# LME (Gaussian, statsmodels MixedLM)
# ---------------------------------------------------------------------------

def bench_lme(path: Path, name: str) -> Row:
    x, g, y = load_xyg(path)
    # statsmodels MixedLM expects a design including intercept
    cols = [f"x{i}" for i in range(x.shape[1])]
    df = pd.DataFrame(x, columns=cols)
    df["group"] = g
    df["y"] = y
    formula = "y ~ " + " + ".join(cols)

    def run():
        return smf.mixedlm(formula, df, groups=df["group"]).fit(method="lbfgs",
                                                                disp=False)

    ms, fit = median_time(run, 3)
    yhat = fit.fittedvalues.to_numpy()
    icc = float(fit.cov_re.iloc[0, 0]
                / (fit.cov_re.iloc[0, 0] + float(fit.scale)))
    return Row(name, ms, r2(y, yhat), icc, "statsmodels.MixedLM (lbfgs)")


# ---------------------------------------------------------------------------
# Ridge / Lasso / ElasticNet
# ---------------------------------------------------------------------------

def bench_ridge(path: Path, name: str, lam: float) -> Row:
    x, y = load_xy(path)

    def run():
        m = Ridge(alpha=lam, fit_intercept=False, solver="cholesky").fit(x, y)
        return m

    ms, m = median_time(run, 5)
    yhat = m.predict(x)
    return Row(name, ms, r2(y, yhat), rmse(y, yhat),
               f"sklearn.Ridge alpha={lam}")


def bench_lasso(path: Path, name: str, lam: float) -> Row:
    x, y = load_xy(path)

    def run():
        m = Lasso(alpha=lam, fit_intercept=False, max_iter=200,
                  tol=1e-4).fit(x, y)
        return m

    ms, m = median_time(run, 3)
    yhat = m.predict(x)
    return Row(name, ms, r2(y, yhat), rmse(y, yhat),
               f"sklearn.Lasso alpha={lam}")


def bench_en(path: Path, name: str, lam1: float, lam2: float) -> Row:
    x, y = load_xy(path)
    alpha = lam1 + lam2
    l1_ratio = 0.5 if alpha == 0 else lam1 / alpha

    def run():
        m = ElasticNet(alpha=alpha, l1_ratio=l1_ratio,
                       fit_intercept=False, max_iter=200,
                       tol=1e-4).fit(x, y)
        return m

    ms, m = median_time(run, 3)
    yhat = m.predict(x)
    return Row(name, ms, r2(y, yhat), rmse(y, yhat),
               f"sklearn.ElasticNet alpha={alpha} l1_ratio={l1_ratio}")


# ---------------------------------------------------------------------------

def main() -> int:
    rows: list[Row] = [
        bench_lm(DATA / "lm_n1000_p5.csv", "LM_n1000_p5"),
        bench_lm(DATA / "lm_n10000_p50.csv", "LM_n10000_p50"),
        bench_lm(DATA / "lm_n100000_p100.csv", "LM_n100000_p100"),

        bench_logit(DATA / "logistic_n2000_p10.csv",  "GLM_logit_n2000_p10"),
        bench_logit(DATA / "logistic_n10000_p20.csv", "GLM_logit_n10000_p20"),
        bench_poisson(DATA / "poisson_n2000_p10.csv",  "GLM_poisson_n2000_p10"),
        bench_poisson(DATA / "poisson_n10000_p20.csv", "GLM_poisson_n10000_p20"),

        bench_lme(DATA / "glmm_n2000_p5_g20.csv",   "LME_n2000_p5_g20"),
        bench_lme(DATA / "glmm_n10000_p10_g50.csv", "LME_n10000_p10_g50"),

        bench_ridge(DATA / "lm_n1000_p5.csv",   "Ridge_n1000_p5",   1.0),
        bench_ridge(DATA / "lm_n10000_p50.csv", "Ridge_n10000_p50", 1.0),

        bench_lasso(DATA / "lm_n1000_p5.csv",   "Lasso_n1000_p5",   0.05),
        bench_lasso(DATA / "lm_n10000_p50.csv", "Lasso_n10000_p50", 0.05),

        bench_en(DATA / "lm_n1000_p5.csv",   "EN_n1000_p5",   0.05, 0.05),
        bench_en(DATA / "lm_n10000_p50.csv", "EN_n10000_p50", 0.05, 0.05),
    ]

    out_path = OUT / "regression.csv"
    with open(out_path, "w", newline="") as fh:
        w = csv.writer(fh)
        w.writerow(["system", "suite", "name",
                    "time_ms", "acc_main", "acc_aux", "extra"])
        for r in rows:
            w.writerow(["python", "regression", r.name,
                        f"{r.time_ms:.6g}", f"{r.acc_main:.6g}",
                        f"{r.acc_aux:.6g}", r.extra])
    print(f"wrote {len(rows)} rows -> {out_path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
