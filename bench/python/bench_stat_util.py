#!/usr/bin/env python3
"""B10 Stat util ベンチ — Python 側。

Counterparts of ``bench/haskell/BenchStatUtil.hs``. Writes
``bench/results/python/stat_util.csv``.
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
# Deterministic data generators (must match the Haskell side).
# ---------------------------------------------------------------------------

def synthetic_vec(n: int, offset: int) -> np.ndarray:
    i = np.arange(n)
    return (np.sin((i + offset) * 0.71)
            + 0.4 * np.sin(3 * i + offset))


def shifted(c: float, v: np.ndarray) -> np.ndarray:
    return v + c


# ---------------------------------------------------------------------------

def bench_bootstrap() -> Row:
    from scipy.stats import bootstrap as scipy_boot

    xs = synthetic_vec(1000, 0)
    rng = np.random.default_rng(0)

    def run():
        # scipy bootstrap with B=1000, percentile method, mean statistic.
        r = scipy_boot((xs,), np.mean,
                       n_resamples=1000, confidence_level=0.95,
                       method="percentile", random_state=rng)
        return float(r.confidence_interval.low), float(r.confidence_interval.high)

    ms, (lo, hi) = median_time(run, n_iter=5)
    return Row(
        "Bootstrap_mean_n1000_B1000", ms, hi - lo, lo,
        f"scipy.stats.bootstrap percentile B=1000; [{lo:.6f}, {hi:.6f}]",
    )


def bench_ttest_welch() -> Row:
    from scipy.stats import ttest_ind

    xs = synthetic_vec(500, 0)
    ys = shifted(0.3, synthetic_vec(500, 1000))

    def run():
        r = ttest_ind(xs, ys, equal_var=False)
        return float(r.statistic), float(r.pvalue)

    ms, (t, p) = median_time(run, n_iter=10)
    return Row(
        "Welch_ttest_n500x500", ms, t, p,
        f"scipy.stats.ttest_ind equal_var=False; t={t:.6f} p={p:.6e}",
    )


def bench_mannwhitney() -> Row:
    from scipy.stats import mannwhitneyu

    xs = synthetic_vec(500, 0)
    ys = shifted(0.3, synthetic_vec(500, 1000))

    def run():
        r = mannwhitneyu(xs, ys, alternative="two-sided")
        return float(r.statistic), float(r.pvalue)

    ms, (u, p) = median_time(run, n_iter=10)
    return Row(
        "MannWhitneyU_n500x500", ms, u, p,
        f"scipy.stats.mannwhitneyu; U={u:.0f} p={p:.6e}",
    )


def bench_ks() -> Row:
    # P3 fairness: hanalyze's `kolmogorovSmirnovNormal` tests against
    # @Normal(μ̂, σ̂)@ where the parameters are fitted from the data, i.e.
    # the Lilliefors test (KS with estimated parameters). Previously we
    # compared against `scipy.stats.kstest`, which uses the supplied μ/σ
    # as if known a priori — D values matched but p-values differed by
    # 4 orders of magnitude because the null distributions are different.
    # statsmodels' `lilliefors` is the apples-to-apples comparison.
    from statsmodels.stats.diagnostic import lilliefors

    xs = synthetic_vec(1000, 0)

    def run():
        d, p = lilliefors(xs, dist="norm")
        return float(d), float(p)

    ms, (d, p) = median_time(run, n_iter=10)
    return Row(
        "KS_normal_n1000", ms, d, p,
        f"statsmodels.stats.diagnostic.lilliefors (KS w/ estimated μ̂σ̂); "
        f"D={d:.6f} p={p:.6f}",
    )


def bench_bh() -> Row:
    from statsmodels.stats.multitest import multipletests

    n = 1000
    ps = np.array([
        0.001 + 0.0001 * i if i < 100
        else 0.5 + 0.4 * np.sin(i)
        for i in range(n)
    ])

    def run():
        _, adj, _, _ = multipletests(ps, method="fdr_bh")
        return int(np.sum(adj < 0.05))

    ms, n_sig = median_time(run, n_iter=10)
    return Row(
        "BH_pAdjust_n1000", ms, float(n_sig), 0.0,
        f"statsmodels multipletests fdr_bh; n_sig={n_sig}",
    )


def bench_halton() -> Row | None:
    try:
        from scipy.stats import qmc
    except ImportError as e:
        print(f"  skip Halton: {e}")
        return None

    def run():
        sampler = qmc.Halton(d=5, scramble=False)
        pts = sampler.random(n=10000)
        # Force computation.
        return float(pts.sum())

    ms, _ = median_time(run, n_iter=5)
    return Row(
        "Halton_n10000_d5", ms, 10000.0, 5.0,
        "scipy.stats.qmc.Halton n=10000 d=5",
    )


def bench_auc() -> Row:
    from sklearn.metrics import roc_auc_score, log_loss

    n = 10000
    i = np.arange(n)
    logits = np.sin(i * 0.31)
    probs = 1.0 / (1.0 + np.exp(-logits))
    labels = (logits > 0).astype(int)

    def run():
        a = float(roc_auc_score(labels, probs))
        ll = float(log_loss(labels, probs))
        return a, ll

    ms, (a, ll) = median_time(run, n_iter=10)
    return Row(
        "AUC_LogLoss_n10000", ms, a, ll,
        f"sklearn.metrics roc_auc_score + log_loss; AUC={a:.6f} ll={ll:.6f}",
    )


def bench_kfold() -> Row:
    from sklearn.model_selection import KFold

    def run():
        kf = KFold(n_splits=5, shuffle=True, random_state=0)
        # Force enumeration.
        n_idx = 0
        for train, test in kf.split(np.arange(1000)):
            n_idx += len(train) + len(test)
        return n_idx

    ms, k = median_time(run, n_iter=10)
    return Row(
        "KFold_5_n1000", ms, float(k), 0.0,
        "sklearn.model_selection.KFold(5) on n=1000",
    )


# ---------------------------------------------------------------------------

def main():
    rows: list[Row] = []
    for r in (bench_bootstrap(), bench_ttest_welch(), bench_mannwhitney(),
              bench_ks(), bench_bh(), bench_halton(), bench_auc(),
              bench_kfold()):
        if r is not None:
            rows.append(r)
            print(f"  {r.name:<32} {r.time_ms:>10.3f} ms  {r.extra}")

    out = OUT / "stat_util.csv"
    with out.open("w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["system", "suite", "name", "time_ms",
                    "acc_main", "acc_aux", "extra"])
        for r in rows:
            w.writerow(["python", "stat_util", r.name,
                        f"{r.time_ms:.6g}", f"{r.acc_main:.6g}",
                        f"{r.acc_aux:.6g}", r.extra])
    print(f"wrote {len(rows)} rows → {out}")


if __name__ == "__main__":
    main()
