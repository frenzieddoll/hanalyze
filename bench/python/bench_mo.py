#!/usr/bin/env python3
"""Multi-objective optimization benchmarks (B4) — Python side.

Solves ZDT1/2/3 and DTLZ2 (m=3) with pymoo NSGA-II, computes wall time,
exports the Python Pareto set, and aggregates HV/IGD metrics for both
the Haskell- and Python-produced Pareto sets uniformly through pymoo's
indicators.
"""

from __future__ import annotations

import csv
import time
from pathlib import Path

import numpy as np
import pandas as pd
from pymoo.algorithms.moo.nsga2 import NSGA2
from pymoo.optimize import minimize
from pymoo.problems import get_problem
from pymoo.indicators.hv import HV
from pymoo.indicators.igd import IGD


REPO = Path(__file__).resolve().parents[2]
HS_DIR = REPO / "bench" / "results" / "haskell"
PY_DIR = REPO / "bench" / "results" / "python"
PY_DIR.mkdir(parents=True, exist_ok=True)


PROBLEMS = [
    ("ZDT1",     "zdt1", 2, [1.1, 1.1]),
    ("ZDT2",     "zdt2", 2, [1.1, 1.1]),
    ("ZDT3",     "zdt3", 2, [1.1, 1.1]),
    ("DTLZ2_3",  "dtlz2", 3, [1.5, 1.5, 1.5]),
]


def main() -> int:
    rows = []
    summary = []
    for name, pname, m, ref in PROBLEMS:
        if pname.startswith("zdt"):
            problem = get_problem(pname, n_var=30)
        else:
            problem = get_problem(pname, n_var=10, n_obj=3)
        algo = NSGA2(pop_size=100)

        t0 = time.perf_counter()
        res = minimize(problem, algo, ("n_gen", 100), seed=1, verbose=False)
        ms = 1000.0 * (time.perf_counter() - t0)

        py_pareto = np.atleast_2d(res.F)
        # save Python Pareto
        pd.DataFrame(py_pareto, columns=[f"f{i}" for i in range(m)]).to_csv(
            PY_DIR / f"mo_pareto_{name}.csv", index=False
        )

        # compute HV / IGD for both sides
        ref_arr = np.array(ref)
        # reference Pareto front from pymoo (analytic for ZDT/DTLZ)
        try:
            pf_true = problem.pareto_front()
        except Exception:
            pf_true = py_pareto

        hv = HV(ref_point=ref_arr)
        igd = IGD(pf_true)

        py_hv = float(hv(py_pareto))
        py_igd = float(igd(py_pareto))

        # Haskell pareto: load if exists
        hs_path = HS_DIR / f"mo_pareto_{name}.csv"
        if hs_path.exists():
            hs_pareto = pd.read_csv(hs_path).to_numpy()
            hs_hv = float(hv(hs_pareto))
            hs_igd = float(igd(hs_pareto))
        else:
            hs_hv = float("nan")
            hs_igd = float("nan")

        rows.append({
            "name": f"{name}/NSGA-II",
            "time_ms": ms,
            "acc_main": py_hv,
            "acc_aux": py_igd,
            "extra": "pymoo NSGA2 100 gen, HV/IGD via pymoo",
        })
        summary.append({
            "problem": name,
            "hv_hs":  hs_hv,
            "hv_py":  py_hv,
            "igd_hs": hs_igd,
            "igd_py": py_igd,
            "n_pareto_hs": int(hs_pareto.shape[0]) if hs_path.exists() else 0,
            "n_pareto_py": int(py_pareto.shape[0]),
        })

    out = PY_DIR / "mo.csv"
    with open(out, "w", newline="") as fh:
        w = csv.writer(fh)
        w.writerow(["system", "suite", "name",
                    "time_ms", "acc_main", "acc_aux", "extra"])
        for r in rows:
            w.writerow(["python", "mo", r["name"],
                        f"{r['time_ms']:.6g}", f"{r['acc_main']:.6g}",
                        f"{r['acc_aux']:.6g}", r["extra"]])
    print(f"wrote {len(rows)} rows -> {out}")

    # Side-by-side metric summary (Haskell vs Python on common metric)
    summary_df = pd.DataFrame(summary)
    summary_path = REPO / "bench" / "results" / "mo_quality.csv"
    summary_df.to_csv(summary_path, index=False)
    print(f"wrote quality summary -> {summary_path}")
    print("\nQuality summary (HV: bigger is better; IGD: smaller is better):")
    print(summary_df.to_string(index=False))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
