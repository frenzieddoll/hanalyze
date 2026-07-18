"""Phase 9-16 (Tier 1 + Tier 2) 機能の Python 比較ベンチ。

Haskell 側 (`bench-tier12`) が事前に生成した `bench/data/tier12_*.csv` を
読み、 sklearn / scipy で同じ計算を回して time + accuracy を比較記録する。

出力: bench/results/python/tier12.csv (BenchRow schema)
"""

from __future__ import annotations

import csv
import os
import sys
import time
from statistics import median

import numpy as np
from scipy.cluster.hierarchy import linkage
from scipy.stats import friedmanchisquare
from sklearn.cross_decomposition import PLSRegression
from sklearn.discriminant_analysis import (
    LinearDiscriminantAnalysis,
    QuadraticDiscriminantAnalysis,
)
from sklearn.ensemble import RandomForestClassifier
from sklearn.neural_network import MLPRegressor

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))
DATA = os.path.join(ROOT, "bench", "data")
RESULT = os.path.join(ROOT, "bench", "results", "python", "tier12.csv")

HEADER = ["system", "suite", "name", "time_ms", "acc_main", "acc_aux", "extra"]


def time_median(fn, n_iter=5):
    ts = []
    out = None
    for _ in range(n_iter):
        t0 = time.perf_counter()
        out = fn()
        ts.append((time.perf_counter() - t0) * 1000)
    return median(ts), out


def load_csv(path):
    with open(path) as f:
        rdr = csv.reader(f)
        header = next(rdr)
        rows = [[float(v) for v in r] for r in rdr]
    return header, np.array(rows)


# ---------------------------------------------------------------------------
# PLS
# ---------------------------------------------------------------------------


def bench_pls(n, p):
    path = os.path.join(DATA, f"tier12_pls_n{n}_p{p}.csv")
    _, mat = load_csv(path)
    x = mat[:, :p]
    y = mat[:, p]

    def fit():
        m = PLSRegression(n_components=3)
        m.fit(x, y)
        return m.predict(x).ravel()

    t_ms, yhat = time_median(fit, 5)
    rng = y.max() - y.min() + 1e-12
    nrmse = float(np.sqrt(np.mean((yhat - y) ** 2)) / rng)
    return dict(
        system="python", suite="tier12",
        name=f"PLS_n{n}_p{p}",
        time_ms=t_ms, acc_main=nrmse, acc_aux=0,
        extra="k=3",
    )


# ---------------------------------------------------------------------------
# LDA / QDA
# ---------------------------------------------------------------------------


def _lda_xy(n_total, p, k):
    path = os.path.join(DATA, f"tier12_lda_n{n_total}_p{p}_k{k}.csv")
    _, mat = load_csv(path)
    x = mat[:, :p]
    y = mat[:, p].astype(int)
    return x, y


def bench_lda(n_total, p, k):
    x, y = _lda_xy(n_total, p, k)

    def fit():
        m = LinearDiscriminantAnalysis()
        m.fit(x, y)
        return m.predict(x)

    t_ms, preds = time_median(fit, 5)
    acc = float((preds == y).mean())
    return dict(
        system="python", suite="tier12",
        name=f"LDA_n{n_total}_p{p}_k{k}",
        time_ms=t_ms, acc_main=acc, acc_aux=0, extra="",
    )


def bench_qda(n_total, p, k):
    x, y = _lda_xy(n_total, p, k)

    def fit():
        m = QuadraticDiscriminantAnalysis()
        m.fit(x, y)
        return m.predict(x)

    t_ms, preds = time_median(fit, 5)
    acc = float((preds == y).mean())
    return dict(
        system="python", suite="tier12",
        name=f"QDA_n{n_total}_p{p}_k{k}",
        time_ms=t_ms, acc_main=acc, acc_aux=0, extra="",
    )


# ---------------------------------------------------------------------------
# Hierarchical Cluster
# ---------------------------------------------------------------------------


def bench_hcluster(n):
    path = os.path.join(DATA, f"tier12_hc_n{n}.csv")
    _, mat = load_csv(path)

    def fit():
        return linkage(mat, method="ward")

    t_ms, z = time_median(fit, 3)
    last_height = float(z[-1, 2])
    return dict(
        system="python", suite="tier12",
        name=f"HClusterWard_n{n}",
        time_ms=t_ms, acc_main=last_height, acc_aux=0, extra="",
    )


# ---------------------------------------------------------------------------
# Friedman
# ---------------------------------------------------------------------------


def bench_friedman(n):
    path = os.path.join(DATA, f"tier12_friedman_n{n}.csv")
    _, mat = load_csv(path)

    def run():
        return friedmanchisquare(mat[:, 0], mat[:, 1], mat[:, 2])

    t_ms, res = time_median(run, 7)
    return dict(
        system="python", suite="tier12",
        name=f"Friedman_n{n}",
        time_ms=t_ms, acc_main=float(res.statistic), acc_aux=float(res.pvalue),
        extra="",
    )


# ---------------------------------------------------------------------------
# RandomForest Classifier
# ---------------------------------------------------------------------------


def bench_rfc(n_total, p, k):
    x, y = _lda_xy(n_total, p, k)

    def fit():
        m = RandomForestClassifier(n_estimators=50, oob_score=True,
                                   bootstrap=True, random_state=0)
        m.fit(x, y)
        return m

    t_ms, m = time_median(fit, 3)
    return dict(
        system="python", suite="tier12",
        name=f"RFC_n{n_total}_p{p}_k{k}",
        time_ms=t_ms, acc_main=float(m.oob_score_), acc_aux=0,
        extra="trees=50",
    )


# ---------------------------------------------------------------------------
# MLP Regressor
# ---------------------------------------------------------------------------


def bench_mlp(n, p):
    path = os.path.join(DATA, f"tier12_pls_n{n}_p{p}.csv")
    _, mat = load_csv(path)
    x = mat[:, :p]
    y = mat[:, p]

    def fit():
        m = MLPRegressor(hidden_layer_sizes=(16,),
                         max_iter=100, batch_size=16,
                         solver="adam", learning_rate_init=0.01,
                         random_state=0)
        m.fit(x, y)
        return m

    t_ms, m = time_median(fit, 3)
    preds = m.predict(x)
    mse = float(np.mean((preds - y) ** 2))
    return dict(
        system="python", suite="tier12",
        name=f"MLPRegressor_n{n}_p{p}",
        time_ms=t_ms, acc_main=mse, acc_aux=0,
        extra="hidden=16 epochs=100",
    )


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------


def main():
    os.makedirs(os.path.dirname(RESULT), exist_ok=True)
    rows = []

    for n, p in [(100, 10), (500, 10)]:
        rows.append(bench_pls(n, p))
    for nc, p, k in [(30, 5, 3), (100, 5, 3)]:
        rows.append(bench_lda(nc * k, p, k))
        rows.append(bench_qda(nc * k, p, k))
    # Hierarchical uses tier12_pls_n*_p10.csv but only first 5 cols
    for n in [20, 50]:
        rows.append(bench_hcluster(n))
    for n in [10, 30, 100]:
        rows.append(bench_friedman(n))
    for n, p, k in [(90, 5, 3), (300, 5, 3)]:
        rows.append(bench_rfc(n, p, k))
    for n, p in [(100, 10), (500, 10)]:
        rows.append(bench_mlp(n, p))

    with open(RESULT, "w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=HEADER)
        w.writeheader()
        for r in rows:
            w.writerow(r)
    print(f"✓ {RESULT} written ({len(rows)} rows)")


if __name__ == "__main__":
    main()
