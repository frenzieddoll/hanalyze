#!/usr/bin/env python3
"""B8 残: Holt-Winters / GAM / Spline 補間 — Python 側。

Counterparts of ``bench/haskell/BenchTSExtras.hs``. Writes
``bench/results/python/ts_extras.csv`` with the unified BenchRow schema.
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
# Data generators (must match the Haskell side).
# ---------------------------------------------------------------------------

def seasonal_series(n: int) -> np.ndarray:
    t = np.arange(n)
    return (0.05 * t
            + 2.0 * np.sin(2 * np.pi * t / 12.0)
            + 0.1 * np.sin(t * 0.7))


def smooth_fn(x: np.ndarray) -> np.ndarray:
    return np.sin(2 * x) + 0.5 * x + 0.3 * np.sin(5 * x)


def gam_data(n: int):
    xs = np.linspace(-3.0, 3.0, n)
    ys = smooth_fn(xs)
    return xs, ys


def interp_data(n_knots: int, n_eval: int):
    knots_x = np.linspace(-3.0, 3.0, n_knots)
    knots_y = smooth_fn(knots_x)
    grid = np.linspace(-2.9, 2.9, n_eval)
    return knots_x, knots_y, grid


# ---------------------------------------------------------------------------
# Holt-Winters (statsmodels)
# ---------------------------------------------------------------------------

def bench_hw() -> Row | None:
    try:
        from statsmodels.tsa.holtwinters import ExponentialSmoothing
    except ImportError as e:
        print(f"  skip HW: {e}")
        return None

    y = seasonal_series(500)

    def run():
        # Match the Haskell side: additive trend + additive seasonality.
        # statsmodels fits parameters by MLE; hanalyze uses fixed α=0.3, β=γ=0.1.
        # For an apples-to-apples timing on the *fit* operation, let
        # statsmodels do its default MLE fit (this is the realistic use).
        m = ExponentialSmoothing(
            y, trend="add", seasonal="add", seasonal_periods=12,
            initialization_method="estimated",
        )
        f = m.fit()
        rmse = float(np.sqrt(np.mean((f.fittedvalues - y) ** 2)))
        return rmse

    ms, rmse = median_time(run, n_iter=5)
    return Row(
        "HW_seasonal_n500_p12_additive", ms, rmse, 0.0,
        f"statsmodels ExponentialSmoothing(trend=add,seasonal=add,p=12) MLE fit; "
        f"RMSE={rmse:.6f}",
    )


# ---------------------------------------------------------------------------
# GAM (pygam)
# ---------------------------------------------------------------------------

def bench_gam() -> Row | None:
    try:
        from pygam import LinearGAM, s
    except ImportError as e:
        print(f"  skip GAM: {e}")
        return None

    xs, ys = gam_data(2000)
    X = xs.reshape(-1, 1)

    def run():
        # pygam: cubic B-spline (default) with 10 splines, mild λ.
        gam = LinearGAM(s(0, n_splines=10, spline_order=3, lam=1e-3))
        gam.fit(X, ys)
        yhat = gam.predict(X)
        rmse = float(np.sqrt(np.mean((yhat - ys) ** 2)))
        return rmse

    ms, rmse = median_time(run, n_iter=3)
    return Row(
        "GAM_n2000_splines10_1D", ms, rmse, 0.0,
        f"pygam LinearGAM(s(n_splines=10,order=3,lam=1e-3)); RMSE={rmse:.6f}",
    )


# ---------------------------------------------------------------------------
# Spline interpolation (scipy.interpolate)
# ---------------------------------------------------------------------------

def bench_interp() -> list[Row]:
    from scipy.interpolate import interp1d, CubicSpline, PchipInterpolator

    n_knots, n_eval = 1000, 5000
    kx, ky, grid = interp_data(n_knots, n_eval)
    rows: list[Row] = []

    def run_linear():
        f = interp1d(kx, ky, kind="linear")
        return float(f(grid).sum())

    def run_natural():
        # scipy CubicSpline with bc_type="natural" matches NaturalSpline.
        f = CubicSpline(kx, ky, bc_type="natural")
        return float(f(grid).sum())

    def run_pchip():
        f = PchipInterpolator(kx, ky)
        return float(f(grid).sum())

    for label, fn in [("Linear", run_linear),
                      ("NatSpline", run_natural),
                      ("PCHIP", run_pchip)]:
        ms, _ = median_time(fn, n_iter=10)
        rows.append(Row(
            f"Interp1D_{label}_knots1000_eval5000", ms, 0.0, 0.0,
            f"scipy.interpolate {label} knots=1000 eval=5000",
        ))
    return rows


# ---------------------------------------------------------------------------

def main():
    rows: list[Row] = []
    for r in (bench_hw(), bench_gam()):
        if r is not None:
            rows.append(r)
    rows.extend(bench_interp())

    for r in rows:
        print(f"  {r.name:<42} {r.time_ms:>10.3f} ms  {r.extra}")

    out = OUT / "ts_extras.csv"
    with out.open("w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["system", "suite", "name", "time_ms",
                    "acc_main", "acc_aux", "extra"])
        for r in rows:
            w.writerow(["python", "ts_extras", r.name,
                        f"{r.time_ms:.6g}", f"{r.acc_main:.6g}",
                        f"{r.acc_aux:.6g}", r.extra])
    print(f"wrote {len(rows)} rows → {out}")


if __name__ == "__main__":
    main()
