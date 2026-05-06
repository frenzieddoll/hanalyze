#!/usr/bin/env python3
"""Survival / time-series / quantile / GAM / spline benchmarks (B8) — Python side.

Compares against statsmodels (ARIMA, quantile regression), lifelines
(Cox PH, Kaplan-Meier), pygam (GAM), and scipy.interpolate (PCHIP).

Run with::

    OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 \\
        bench/venv/bin/python bench/python/bench_survts.py

Writes ``bench/results/python/survts.csv``.
"""

from __future__ import annotations

import csv
import random
import time
from dataclasses import dataclass
from pathlib import Path

import numpy as np
import pandas as pd


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
# Synthetic data (deterministic seeds matching Haskell bench)
# ---------------------------------------------------------------------------

def gen_ar1(n: int) -> np.ndarray:
    rng = random.Random(42)
    xs = []
    x = 0.0
    while len(xs) < n:
        z = rng.uniform(-3.0, 3.0)
        x = 0.7 * x + 0.3 * z
        xs.append(x)
    return np.array(xs)


def gen_surv(n: int) -> tuple[np.ndarray, np.ndarray, np.ndarray]:
    rng = random.Random(7)
    rows_x, rows_t, rows_e = [], [], []
    for _ in range(n):
        x1 = rng.uniform(-1.0, 1.0)
        x2 = rng.uniform(-1.0, 1.0)
        u = rng.uniform(0.01, 1.0)
        t = -np.log(u) / np.exp(0.5 * x1 - 0.3 * x2)
        c = rng.uniform(0.0, 1.0)
        ev = 1 if c < 0.7 else 0
        rows_x.append([x1, x2])
        rows_t.append(t)
        rows_e.append(ev)
    return (np.array(rows_x), np.array(rows_t), np.array(rows_e))


# ---------------------------------------------------------------------------
# ARIMA(1,1,1)
# ---------------------------------------------------------------------------

def bench_arima(name: str, n_iter: int = 3) -> Row:
    from statsmodels.tsa.arima.model import ARIMA

    y = gen_ar1(1000)

    def run():
        return ARIMA(y, order=(1, 1, 1)).fit(method_kwargs={"disp": 0})

    ms, fit = median_time(run, n_iter)
    return Row(name, ms, 0, 0,
               "statsmodels.tsa.arima.ARIMA p=1 d=1 q=1 n=1000")


# ---------------------------------------------------------------------------
# Cox PH
# ---------------------------------------------------------------------------

def bench_coxph(name: str, n_iter: int = 5) -> Row:
    from lifelines import CoxPHFitter

    x, t, e = gen_surv(2000)
    df = pd.DataFrame({"x1": x[:, 0], "x2": x[:, 1], "T": t, "E": e})

    def run():
        cph = CoxPHFitter()
        cph.fit(df, duration_col="T", event_col="E")
        return cph

    ms, fit = median_time(run, n_iter)
    b = fit.params_
    return Row(name, ms, float(b["x1"]), float(b["x2"]),
               "lifelines.CoxPHFitter n=2000 p=2 30% censor")


# ---------------------------------------------------------------------------
# Kaplan-Meier
# ---------------------------------------------------------------------------

def bench_km(name: str, n_iter: int = 5) -> Row:
    from lifelines import KaplanMeierFitter

    _, t, e = gen_surv(2000)

    def run():
        km = KaplanMeierFitter()
        km.fit(t, event_observed=e)
        return km

    ms, km = median_time(run, n_iter)
    sf = km.survival_function_["KM_estimate"]
    t_end = float(sf.index[-1])
    s_end = float(sf.iloc[-1])
    return Row(name, ms, t_end, s_end,
               "lifelines.KaplanMeierFitter n=2000")


# ---------------------------------------------------------------------------
# Quantile regression
# ---------------------------------------------------------------------------

def bench_quantile(name: str, n_iter: int = 3) -> Row:
    from statsmodels.regression.quantile_regression import QuantReg

    df = pd.read_csv(DATA / "lm_n10000_p50.csv")
    x = df.iloc[:, :20].to_numpy()
    y = df.iloc[:, -1].to_numpy()
    # Add intercept column.
    X = np.column_stack([np.ones(len(y)), x])

    def run():
        return QuantReg(y, X).fit(q=0.5)

    ms, fit = median_time(run, n_iter)
    return Row(name, ms, 0, 0,
               "statsmodels QuantReg tau=0.5 n=10000 p=20")


# ---------------------------------------------------------------------------
# GAM
# ---------------------------------------------------------------------------

def bench_gam(name: str, n_iter: int = 3) -> Row:
    from pygam import LinearGAM, s

    df = pd.read_csv(DATA / "kernel_n2000_p5.csv")
    x = df.iloc[:, :2].to_numpy()
    y = df.iloc[:, -1].to_numpy()

    def run():
        gam = LinearGAM(s(0, n_splines=8) + s(1, n_splines=8))
        return gam.fit(x, y)

    ms, fit = median_time(run, n_iter)
    return Row(name, ms, 0, 0,
               "pygam.LinearGAM n_splines=8 n=2000 p=2")


# ---------------------------------------------------------------------------
# 1D spline (PCHIP)
# ---------------------------------------------------------------------------

def bench_spline(name: str, n_iter: int = 5) -> Row:
    from scipy.interpolate import PchipInterpolator

    n = 1000
    xs = np.linspace(0, 1, n)
    ys = np.sin(3 * xs) + 0.1 * np.cos(15 * xs)
    qs = np.linspace(0, 1, 5000)

    def run():
        f = PchipInterpolator(xs, ys)
        return float(f(qs).sum())

    ms, total = median_time(run, n_iter)
    return Row(name, ms, total, 0,
               "scipy.interpolate.PchipInterpolator build n=1000 + eval @5000")


# ---------------------------------------------------------------------------

def write_rows(path: Path, rows: list[Row], suite: str = "survts") -> None:
    with path.open("w", newline="") as fh:
        w = csv.writer(fh)
        w.writerow(["system", "suite", "name",
                    "time_ms", "acc_main", "acc_aux", "extra"])
        for r in rows:
            w.writerow(["python", suite, r.name,
                        f"{r.time_ms:.6g}", f"{r.acc_main:.6g}",
                        f"{r.acc_aux:.6g}", r.extra])


def main() -> None:
    rows: list[Row] = []
    for fn, name in [
        (bench_arima,    "ARIMA_n1000_pdq111"),
        (bench_coxph,    "CoxPH_n2000_p2_30pct_censor"),
        (bench_km,       "KM_n2000"),
        (bench_quantile, "Quantile_n10000_p20_tau0.5"),
        (bench_gam,      "GAM_n2000_p2_d3_k5"),
        (bench_spline,   "Spline_PCHIP_n1000"),
    ]:
        print(f"Running {name} …")
        rows.append(fn(name))
    out = OUT / "survts.csv"
    write_rows(out, rows)
    print(f"wrote {len(rows)} rows → {out}")


if __name__ == "__main__":
    main()
