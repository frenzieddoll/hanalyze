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


def regrid_long_numpy(arr: np.ndarray, n: int = 30) -> np.ndarray:
    """Numpy-only synthesis (no pandas) for a fairer comparison.

    Input: ``arr`` of shape ``(N, 3)`` with columns ``[id_int, z, y]``;
    ids must be integer-encoded. Output: shape ``(n_ids · n, 3)`` in
    the same column convention. Avoids pandas' Python-loop groupby.
    """
    ids = arr[:, 0].astype(np.int64)
    z   = arr[:, 1]
    y   = arr[:, 2]
    uniq_ids = np.unique(ids)

    # Common z range: intersection across ids.
    per_id_min = np.array([z[ids == i].min() for i in uniq_ids])
    per_id_max = np.array([z[ids == i].max() for i in uniq_ids])
    z_lo = float(per_id_min.max())
    z_hi = float(per_id_max.min())

    # Adaptive grid from the densest id (most points) within [z_lo, z_hi].
    counts = np.array([(ids == i).sum() for i in uniq_ids])
    rep    = int(uniq_ids[np.argmax(counts)])
    rep_mask = ids == rep
    rep_z = z[rep_mask]
    rep_y = y[rep_mask]
    order = np.argsort(rep_z)
    rep_z = rep_z[order]
    rep_y = rep_y[order]
    # Drop duplicate z (PCHIP requires strictly increasing).
    keep = np.r_[True, np.diff(rep_z) > 0]
    rep_z = rep_z[keep]
    rep_y = rep_y[keep]
    in_range = (rep_z >= z_lo) & (rep_z <= z_hi)
    grid = adaptive_grid(rep_z[in_range], rep_y[in_range], n)

    # Per-id PCHIP, evaluate on common grid.
    out_blocks = []
    for i in uniq_ids:
        mask = ids == i
        zi = z[mask]
        yi = y[mask]
        order_i = np.argsort(zi)
        zi = zi[order_i]
        yi = yi[order_i]
        keep_i = np.r_[True, np.diff(zi) > 0]
        zi = zi[keep_i]
        yi = yi[keep_i]
        if len(zi) < 2:
            continue
        f = PchipInterpolator(zi, yi, extrapolate=False)
        ys_grid = f(grid)
        block = np.column_stack([np.full(n, i, dtype=np.float64), grid, ys_grid])
        out_blocks.append(block)
    return np.vstack(out_blocks) if out_blocks else np.empty((0, 3))


def bench_regrid_pandas() -> Row:
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


def bench_regrid_numpy() -> Row:
    """Numpy-only path: avoids pandas groupby/Python-loop overhead.
    Shows the structural ceiling of the operation when comparing
    Python-vectorised work against hanalyze."""
    csv_path = REPO / "data" / "io" / "potential_long_jagged.csv"
    df = pd.read_csv(csv_path)
    # Encode `name` as integer once (this is data prep, not bench scope).
    ids_str   = df["name"].to_numpy()
    uniq, inv = np.unique(ids_str, return_inverse=True)
    arr = np.column_stack([
        inv.astype(np.float64), df["z"].to_numpy(), df["y"].to_numpy()
    ])
    _ = uniq

    def run():
        out = regrid_long_numpy(arr, n=30)
        return len(out)

    ms, n_rows = median_time(run, n_iter=10)
    return Row(
        "Regrid_long_jagged_PCHIP_N30_numpy", ms, 0.0, 0.0,
        f"numpy + scipy PCHIP+adaptive (no pandas); rows={n_rows}",
    )


def main():
    rows = [bench_regrid_pandas(), bench_regrid_numpy()]
    for r in rows:
        print(f"  {r.name:<42} {r.time_ms:>10.3f} ms  {r.extra}")
    out_path = OUT / "regrid.csv"
    with out_path.open("w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["system", "suite", "name", "time_ms",
                    "acc_main", "acc_aux", "extra"])
        for r in rows:
            w.writerow(["python", "regrid", r.name,
                        f"{r.time_ms:.6g}", f"{r.acc_main:.6g}",
                        f"{r.acc_aux:.6g}", r.extra])
    print(f"wrote {len(rows)} rows → {out_path}")


if __name__ == "__main__":
    main()
