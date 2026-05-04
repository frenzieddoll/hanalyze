#!/usr/bin/env python3
"""Aggregate Haskell + Python benchmark CSVs into a comparison table.

Reads every ``bench/results/{haskell,python}/*.csv`` file with the unified
schema::

    system,suite,name,time_ms,acc_main,acc_aux,extra

and prints a Markdown summary plus a side-by-side speedup column. Designed
to be invoked from the project root::

    bench/venv/bin/python bench/aggregate.py
"""

from __future__ import annotations

import argparse
import glob
import os
import sys

import pandas as pd
from tabulate import tabulate


REPO_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
RESULTS = os.path.join(REPO_ROOT, "bench", "results")


def _read_dir(d: str) -> pd.DataFrame:
    rows = []
    for path in sorted(glob.glob(os.path.join(d, "*.csv"))):
        try:
            rows.append(pd.read_csv(path))
        except Exception as e:
            print(f"warn: cannot read {path}: {e}", file=sys.stderr)
    if not rows:
        return pd.DataFrame(
            columns=[
                "system",
                "suite",
                "name",
                "time_ms",
                "acc_main",
                "acc_aux",
                "extra",
            ]
        )
    return pd.concat(rows, ignore_index=True)


def _merge(haskell: pd.DataFrame, python: pd.DataFrame) -> pd.DataFrame:
    merged = pd.merge(
        haskell,
        python,
        on=["suite", "name"],
        how="outer",
        suffixes=("_hs", "_py"),
    )
    merged["speedup_hs_over_py"] = merged["time_ms_py"] / merged["time_ms_hs"]
    return merged


def main() -> int:
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument(
        "--suite",
        nargs="*",
        default=None,
        help="Restrict to specific suite names (e.g. regression kernel).",
    )
    args = p.parse_args()

    haskell = _read_dir(os.path.join(RESULTS, "haskell"))
    python = _read_dir(os.path.join(RESULTS, "python"))

    if args.suite:
        haskell = haskell[haskell["suite"].isin(args.suite)]
        python = python[python["suite"].isin(args.suite)]

    merged = _merge(haskell, python)
    if merged.empty:
        print("(no benchmark CSVs found under bench/results/)")
        return 0

    cols = [
        "suite",
        "name",
        "time_ms_hs",
        "time_ms_py",
        "speedup_hs_over_py",
        "acc_main_hs",
        "acc_main_py",
    ]
    cols = [c for c in cols if c in merged.columns]
    table = merged[cols].sort_values(["suite", "name"])

    print("# hanalyze vs Python benchmark summary\n")
    print(tabulate(table, headers="keys", tablefmt="github", floatfmt=".4g",
                   showindex=False))
    print()
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
