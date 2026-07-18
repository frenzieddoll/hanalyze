#!/usr/bin/env python3
"""M5 (パラメタ非線形) の iter 延長ラン — PyMC per-draw 単価の確定用。

通常 grid (50-1600) では PyMC の total が固定費 (compile + tune ~2s) に支配され、
per-draw の線形フィットが R² ~0.13 と不定だった (``HBM_SCALING.md`` 54.11 節の †)。
iter を 25600 まで延ばして draw 部分を固定費より大きくし、 傾きを確定する。
Haskell 側は ``bench-hbm-scaling m5-long`` (同 grid)。 結果は別 CSV
(``hbm_scaling_m5_long.csv``) へ — 通常 bench の CSV は上書きしない。

Run with::

    OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 \\
        ~/.virtualenvs/pymc/bin/python bench/python/bench_hbm_m5_long.py
"""

from __future__ import annotations

import csv

import bench_hbm_scaling as B

ITER_GRID_LONG = [400, 800, 1600, 3200, 6400, 12800, 25600]


def main():
    rows = []
    for draws in ITER_GRID_LONG:
        rows.append(B.bench_m5(draws))
        print(f"M5 iter={draws}: {rows[-1].time_ms:.1f} ms", flush=True)

    out = B.OUT / "hbm_scaling_m5_long.csv"
    with open(out, "w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["system", "suite", "name", "time_ms",
                    "acc_main", "acc_aux", "extra"])
        for r in rows:
            w.writerow(["python", "hbm_scaling", r.name,
                        f"{r.time_ms:.6g}", f"{r.acc_main:.6g}",
                        f"{r.acc_aux:.6g}", r.extra])
    print(f"wrote {len(rows)} rows → {out}")


if __name__ == "__main__":
    main()
