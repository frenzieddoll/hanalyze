#!/usr/bin/env python3
"""B13 Regrid ベンチ — Python 側。

Equivalent of ``bench/haskell/BenchRegrid.hs``: take the same jagged
long-form CSV, group by id, fit PCHIP per group, evaluate on a common
adaptive grid (peak |dy/dz| concentrated). Writes
``bench/results/python/regrid.csv``.
"""
from __future__ import annotations

import csv
import time
from dataclasses import dataclass
from pathlib import Path

import numpy as np
import pandas as pd
from scipy.interpolate import PchipInterpolator


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


def adaptive_grid(zs: np.ndarray, ys: np.ndarray,
                  n: int, coarse_n: int = 200,
                  eps_ratio: float = 0.05) -> np.ndarray:
    """Replicate Stat.AdaptiveGrid: peak-|dy/dz| concentrated."""
    z_lo, z_hi = float(zs.min()), float(zs.max())
    coarse = np.linspace(z_lo, z_hi, coarse_n)
    # Fit a PCHIP on coarse to estimate |dy/dz|
    f = PchipInterpolator(zs, ys)
    dy = f.derivative()(coarse)
    density = np.abs(dy)
    density = np.maximum(density, eps_ratio * density.max())
    cdf = np.cumsum(density)
    cdf /= cdf[-1]
    # Inverse-CDF lookup at n equally spaced quantiles.
    qs = np.linspace(0, 1, n)
    return np.interp(qs, cdf, coarse)


def regrid_long(df: pd.DataFrame, id_col: str, z_col: str, y_col: str,
                n: int = 30) -> pd.DataFrame:
    """Pandas + scipy synthesis of hanalyze's regridLong."""
    # Determine common z-range as intersection across ids.
    z_lo = df.groupby(id_col)[z_col].min().max()
    z_hi = df.groupby(id_col)[z_col].max().min()

    # Pick a representative id with the most points to drive the
    # adaptive grid (this mirrors what hanalyze does — adaptive grid
    # built from the union of derivatives, but per-id approximation
    # captures the same intent for benchmarking purposes).
    rep_id = df.groupby(id_col).size().idxmax()
    rep = df[df[id_col] == rep_id].sort_values(z_col)
    rep = rep.drop_duplicates(subset=[z_col])
    rep_z = rep[z_col].to_numpy()
    rep_y = rep[y_col].to_numpy()
    # Restrict rep to common range for adaptive grid.
    mask = (rep_z >= z_lo) & (rep_z <= z_hi)
    grid = adaptive_grid(rep_z[mask], rep_y[mask], n)

    # Per-id PCHIP, evaluate on common grid.
    out_rows = []
    for gid, g in df.groupby(id_col):
        g = g.sort_values(z_col).dropna(subset=[z_col, y_col])
        # Drop duplicate z (PCHIP requires strictly increasing).
        g = g.drop_duplicates(subset=[z_col])
        zs, ys = g[z_col].to_numpy(), g[y_col].to_numpy()
        if len(zs) < 2:
            continue
        f = PchipInterpolator(zs, ys, extrapolate=False)
        ys_grid = f(grid)
        for zi, yi in zip(grid, ys_grid):
            out_rows.append({id_col: gid, z_col: zi, y_col: yi})
    return pd.DataFrame(out_rows)


def bench_regrid() -> Row:
    csv_path = REPO / "data" / "io" / "potential_long_jagged.csv"
    df = pd.read_csv(csv_path)

    def run():
        out = regrid_long(df, "name", "z", "y", n=30)
        return len(out)

    ms, n_rows = median_time(run, n_iter=10)
    return Row(
        "Regrid_long_jagged_PCHIP_N30", ms, 0.0, 0.0,
        f"pandas+scipy PCHIP+adaptive N=30 (synth); rows={n_rows}",
    )


def main():
    r = bench_regrid()
    print(f"  {r.name:<32} {r.time_ms:>10.3f} ms  {r.extra}")
    out = OUT / "regrid.csv"
    with out.open("w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["system", "suite", "name", "time_ms",
                    "acc_main", "acc_aux", "extra"])
        w.writerow(["python", "regrid", r.name,
                    f"{r.time_ms:.6g}", f"{r.acc_main:.6g}",
                    f"{r.acc_aux:.6g}", r.extra])
    print(f"wrote 1 row → {out}")


if __name__ == "__main__":
    main()
