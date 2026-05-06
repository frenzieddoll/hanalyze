#!/usr/bin/env python3
"""Classical-ML benchmarks (B6) — Python side.

Runs scikit-learn equivalents of every benchmark in
``bench/haskell/BenchML.hs`` over the shared CSVs in ``bench/data/``.
Writes ``bench/results/python/ml.csv`` with the unified BenchRow schema.

Run with::

    OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 \\
        bench/venv/bin/python bench/python/bench_ml.py
"""

from __future__ import annotations

import csv
import time
from dataclasses import dataclass
from pathlib import Path

import numpy as np
import pandas as pd
from sklearn.cluster import KMeans
from sklearn.decomposition import PCA
from sklearn.ensemble import RandomForestRegressor
from sklearn.preprocessing import StandardScaler
from sklearn.tree import DecisionTreeClassifier


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


def load_xy(path: Path) -> tuple[np.ndarray, np.ndarray]:
    df = pd.read_csv(path)
    return df.iloc[:, :-1].to_numpy(), df.iloc[:, -1].to_numpy()


# ---------------------------------------------------------------------------
# PCA
# ---------------------------------------------------------------------------

def bench_pca(path: Path, name: str, k: int, n_iter: int = 5) -> Row:
    x, _ = load_xy(path)
    # Match Haskell's CenterScale (= sklearn's standardised PCA).
    scaler = StandardScaler()

    def run():
        xs = scaler.fit_transform(x)
        m = PCA(n_components=k, svd_solver="full").fit(xs)
        return m

    ms, m = median_time(run, n_iter)
    ratio = float(m.explained_variance_ratio_.sum())
    sigma = float(m.singular_values_.sum())
    return Row(name, ms, ratio, sigma,
               f"sklearn.decomposition.PCA n_components={k} (StandardScaler)")


# ---------------------------------------------------------------------------
# KMeans
# ---------------------------------------------------------------------------

def bench_kmeans(path: Path, name: str, k: int, n_iter: int = 5) -> Row:
    x, _ = load_xy(path)

    def run():
        m = KMeans(n_clusters=k, n_init=10, max_iter=300,
                   tol=1e-4, init="k-means++", random_state=0)
        return m.fit(x)

    ms, m = median_time(run, n_iter)
    return Row(name, ms, float(m.inertia_), float(m.n_iter_),
               f"sklearn.cluster.KMeans k={k} (k-means++)")


# ---------------------------------------------------------------------------
# DecisionTree (binary classification)
# ---------------------------------------------------------------------------

def bench_dt(path: Path, name: str, n_iter: int = 5) -> Row:
    x, y = load_xy(path)
    yi = y.astype(int)

    def run():
        return DecisionTreeClassifier(criterion="gini",
                                      max_depth=None,
                                      random_state=0).fit(x, yi)

    ms, m = median_time(run, n_iter)
    acc = float((m.predict(x) == yi).mean())
    return Row(name, ms, acc, 0.0,
               "sklearn.tree.DecisionTreeClassifier default")


# ---------------------------------------------------------------------------
# RandomForest (binary classification, regressor for 0/1 prob like hanalyze)
# ---------------------------------------------------------------------------

def bench_rf(path: Path, name: str, n_trees: int = 50,
             n_iter: int = 5) -> Row:
    x, y = load_xy(path)
    yi = y.astype(int)

    def run():
        return RandomForestRegressor(n_estimators=n_trees, max_depth=None,
                                     random_state=0).fit(x, y)

    ms, m = median_time(run, n_iter)
    pred = m.predict(x)
    pi = (pred > 0.5).astype(int)
    acc = float((pi == yi).mean())
    return Row(name, ms, acc, 0.0,
               f"sklearn.ensemble.RandomForestRegressor n_trees={n_trees}")


# ---------------------------------------------------------------------------

def write_rows(path: Path, rows: list[Row], suite: str = "ml") -> None:
    with path.open("w", newline="") as fh:
        w = csv.writer(fh)
        w.writerow(["system", "suite", "name",
                    "time_ms", "acc_main", "acc_aux", "extra"])
        for r in rows:
            w.writerow(["python", suite, r.name,
                        f"{r.time_ms:.6g}", f"{r.acc_main:.6g}",
                        f"{r.acc_aux:.6g}", r.extra])


def main() -> None:
    rows: list[Row] = [
        bench_pca(DATA / "lm_n10000_p50.csv",       "PCA_n10000_p50_k5",  5),
        bench_kmeans(DATA / "kernel_n2000_p5.csv",  "KMeans_n2000_p5_k5", 5),
        bench_dt(DATA / "logistic_n2000_p10.csv",  "DT_n2000_p10"),
        bench_rf(DATA / "logistic_n2000_p10.csv",  "RF_n2000_p10_t20", 20),
    ]
    out = OUT / "ml.csv"
    write_rows(out, rows)
    print(f"wrote {len(rows)} rows → {out}")


if __name__ == "__main__":
    main()
