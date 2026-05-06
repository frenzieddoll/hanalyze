#!/usr/bin/env python3
"""B7 残: Gibbs / ADVI / WAIC ベンチ — Python 側。

Counterparts of ``bench/haskell/BenchMCMCExtras.hs``. Writes
``bench/results/python/mcmc_extras.csv`` with the unified BenchRow schema.

Run with::

    OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 \\
        bench/venv/bin/python bench/python/bench_mcmc_extras.py
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
# Gibbs Beta-Binomial: numpy direct sampling (analytic posterior).
# ---------------------------------------------------------------------------

def bench_gibbs_bb() -> Row:
    """Beta(2,2) prior + Binomial(20, p) with k=12 successes.

    Posterior is exactly Beta(2+12, 2+8) = Beta(14, 10). ``numpy.random.beta``
    is the conjugate sampler — same operation hanalyze's ``betaBinomial``
    performs in its inner loop, just batched.
    """
    rng = np.random.default_rng(0)

    def run():
        # 10000 conjugate draws, matching Haskell side.
        return rng.beta(14.0, 10.0, size=10000)

    ms, samples = median_time(run, n_iter=10)
    mu = float(samples.mean())
    return Row(
        "Gibbs_BetaBinomial_n10000", ms, mu, 0.0,
        f"numpy beta(14,10); analytic E[p]={14/24:.16f}",
    )


# ---------------------------------------------------------------------------
# ADVI on logistic regression. Uses numpyro.infer.SVI (lightest of the
# pymc/blackjax/numpyro trio for this kind of small-model timing).
# ---------------------------------------------------------------------------

def make_logistic_data():
    """Match the Haskell side's deterministic data."""
    n = 60
    xs = np.array([0.1 * i - 3.0 for i in range(n)])
    lin = -0.5 + 1.2 * xs
    probs = 1.0 / (1.0 + np.exp(-lin))
    ys = (probs > 0.5).astype(np.int32)
    return xs, ys


def bench_advi() -> Row | None:
    try:
        import jax
        import jax.numpy as jnp
        import numpyro
        import numpyro.distributions as dist
        from numpyro.infer import SVI, Trace_ELBO
        from numpyro.infer.autoguide import AutoNormal
        from numpyro.optim import Adam
    except ImportError as e:
        print(f"  skip ADVI: {e}")
        return None

    xs, ys = make_logistic_data()
    xs_j = jnp.asarray(xs)
    ys_j = jnp.asarray(ys)

    def model(x, y=None):
        beta0 = numpyro.sample("beta0", dist.Normal(0.0, 5.0))
        beta1 = numpyro.sample("beta1", dist.Normal(0.0, 5.0))
        logits = beta0 + beta1 * x
        numpyro.sample("y", dist.Bernoulli(logits=logits), obs=y)

    guide = AutoNormal(model)
    optimizer = Adam(0.05)
    svi = SVI(model, guide, optimizer, loss=Trace_ELBO(num_particles=5))
    n_iter = 500

    # Warm JIT.
    rng_warm = jax.random.PRNGKey(1)
    state = svi.init(rng_warm, xs_j, ys_j)
    state, _ = jax.lax.scan(lambda s, _: svi.update(s, xs_j, ys_j), state,
                            jnp.arange(2))

    def run():
        rng = jax.random.PRNGKey(0)
        s0 = svi.init(rng, xs_j, ys_j)
        s_final, losses = jax.lax.scan(
            lambda s, _: svi.update(s, xs_j, ys_j), s0,
            jnp.arange(n_iter),
        )
        params = svi.get_params(s_final)
        # AutoNormal stores per-site loc/scale.
        b0 = float(params["beta0_auto_loc"])
        b1 = float(params["beta1_auto_loc"])
        # Block until ready for honest timing.
        jax.block_until_ready(losses)
        return b0, b1, float(losses[-1])

    ms, (b0, b1, last_loss) = median_time(run, n_iter=5)
    return Row(
        "ADVI_logistic_n60_iter500", ms, b1, b0,
        f"numpyro SVI(AutoNormal) Adam(0.05) 500 iter num_particles=5; "
        f"-ELBO={last_loss:.6f} beta0={b0:.6f} beta1={b1:.6f}",
    )


# ---------------------------------------------------------------------------
# WAIC / LOO from a synthetic log-lik matrix. Uses arviz.
# ---------------------------------------------------------------------------

def make_log_lik_mat(s: int = 1000, n: int = 200) -> np.ndarray:
    """Mirrors the Haskell ``makeLogLikMat`` — deterministic, no RNG.

    Shape (S, N) so arviz / scipy expect it.
    """
    i = np.arange(n)
    j = np.arange(s)[:, None]
    base = -0.5 * (i / n - 0.5) ** 2 - 1.0
    return base + 0.05 * np.sin(i + j) + 0.02 * np.cos(3 * i + 7 * j)


def bench_waic() -> Row | None:
    try:
        import arviz as az
    except ImportError as e:
        print(f"  skip WAIC: {e}")
        return None

    ll = make_log_lik_mat(1000, 200)
    # arviz expects a chain×draw×obs cube; treat the 1000 draws as a single chain.
    cube = ll[None, :, :]  # shape (1, 1000, 200)
    # Wrap into an InferenceData to avoid recomputing each call.
    import xarray as xr
    da = xr.DataArray(
        cube, dims=("chain", "draw", "y_dim_0"),
        coords={"chain": [0], "draw": np.arange(1000), "y_dim_0": np.arange(200)},
    )
    idata = az.from_dict(log_likelihood={"y": da})

    def run():
        return az.waic(idata, scale="deviance")

    ms, w = median_time(run, n_iter=5)
    waic_val = float(w.elpd_waic) if hasattr(w, "elpd_waic") else float(w["elpd_waic"])
    waic_se = float(w.se) if hasattr(w, "se") else float(w["se"])
    p_waic = float(w.p_waic) if hasattr(w, "p_waic") else float(w["p_waic"])
    return Row(
        "WAIC_S1000_N200", ms, waic_val, waic_se,
        f"arviz.waic scale=deviance; lppd~{waic_val/-2:.6f} p_waic={p_waic:.6f}",
    )


def bench_loo() -> Row | None:
    try:
        import arviz as az
        import xarray as xr
    except ImportError as e:
        print(f"  skip LOO: {e}")
        return None

    ll = make_log_lik_mat(1000, 200)
    cube = ll[None, :, :]
    da = xr.DataArray(
        cube, dims=("chain", "draw", "y_dim_0"),
        coords={"chain": [0], "draw": np.arange(1000), "y_dim_0": np.arange(200)},
    )
    # arviz.loo requires a posterior group; add a dummy single-param posterior
    # (the LOO computation only uses log_likelihood, so the parameter values
    # are irrelevant — we just need *some* posterior so the InferenceData is
    # well-formed).
    posterior_dummy = {"dummy": np.zeros((1, 1000))}
    idata = az.from_dict(
        posterior=posterior_dummy,
        log_likelihood={"y": da},
    )

    def run():
        return az.loo(idata, scale="deviance")

    ms, l = median_time(run, n_iter=5)
    loo_val = float(l.elpd_loo) if hasattr(l, "elpd_loo") else float(l["elpd_loo"])
    n_bad = 0
    if hasattr(l, "pareto_k"):
        n_bad = int((np.asarray(l.pareto_k) > 0.7).sum())
    return Row(
        "LOO_PSIS_S1000_N200", ms, loo_val, float(n_bad),
        f"arviz.loo scale=deviance; bad_k(>0.7)={n_bad}",
    )


# ---------------------------------------------------------------------------

def main():
    rows: list[Row] = []
    for fn in (bench_gibbs_bb, bench_advi, bench_waic, bench_loo):
        r = fn()
        if r is not None:
            rows.append(r)
            print(f"  {r.name:<32} {r.time_ms:>10.3f} ms  {r.extra}")

    out = OUT / "mcmc_extras.csv"
    with out.open("w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["system", "suite", "name", "time_ms",
                    "acc_main", "acc_aux", "extra"])
        for r in rows:
            w.writerow(["python", "mcmc_extras", r.name,
                        f"{r.time_ms:.6g}", f"{r.acc_main:.6g}",
                        f"{r.acc_aux:.6g}", r.extra])
    print(f"wrote {len(rows)} rows → {out}")


if __name__ == "__main__":
    main()
