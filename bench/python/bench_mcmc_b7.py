#!/usr/bin/env python3
"""MCMC benchmarks (B7) — Python side.

Runs PyMC and blackjax NUTS on the same hierarchical-normal model used
in ``bench/haskell/BenchMCMCB7.hs``. Writes
``bench/results/python/mcmc.csv``.

Model:
    mu      ~ Normal(0, 100)
    tau     ~ Exponential(0.1)
    theta_j ~ Normal(mu, tau)         (j = 1..3)
    y_ij    ~ Normal(theta_j, sigma=5)

Iterations: 500 warmup + 1000 samples (single chain), matching the
Haskell side. Reports per-run wall time and ESS for mu / tau.

Run with::

    OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 \\
        bench/venv/bin/python bench/python/bench_mcmc_b7.py
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


SCHOOL_DATA = [
    [72.0, 68.0, 75.0, 71.0],
    [85.0, 88.0, 82.0, 90.0],
    [61.0, 65.0, 58.0, 63.0],
]
SIGMA_Y = 5.0
N_WARMUP = 500
N_SAMPLES = 1000
SEED = 0


# ---------------------------------------------------------------------------
# PyMC NUTS (default backend)
# ---------------------------------------------------------------------------

def bench_pymc_nuts(name: str) -> Row:
    import pymc as pm
    import arviz as az

    obs = np.array(SCHOOL_DATA)  # (3, 4)
    j = obs.shape[0]
    y_flat = obs.flatten()
    school_idx = np.repeat(np.arange(j), obs.shape[1])

    def run() -> "az.InferenceData":
        with pm.Model():
            mu = pm.Normal("mu", mu=0, sigma=100)
            tau = pm.Exponential("tau", lam=0.1)
            theta = pm.Normal("theta", mu=mu, sigma=tau, shape=j)
            pm.Normal("y", mu=theta[school_idx], sigma=SIGMA_Y,
                      observed=y_flat)
            return pm.sample(
                draws=N_SAMPLES, tune=N_WARMUP, chains=1, cores=1,
                random_seed=SEED, progressbar=False,
                return_inferencedata=True,
            )

    t0 = time.perf_counter()
    idata = run()
    t1 = time.perf_counter()

    summary = az.summary(idata, var_names=["mu", "tau"])
    mu_mean = float(summary.loc["mu", "mean"])
    mu_ess = float(summary.loc["mu", "ess_bulk"])
    tau_ess = float(summary.loc["tau", "ess_bulk"])
    return Row(
        name, 1000.0 * (t1 - t0), mu_mean, mu_ess,
        f"PyMC NUTS draws={N_SAMPLES} tune={N_WARMUP} "
        f"ess(mu)={mu_ess:.1f} ess(tau)={tau_ess:.1f}",
    )


# ---------------------------------------------------------------------------
# Blackjax NUTS (manual log-density)
# ---------------------------------------------------------------------------

def bench_blackjax_nuts(name: str) -> Row:
    import jax
    import jax.numpy as jnp
    import blackjax

    obs = jnp.array(SCHOOL_DATA)
    j = obs.shape[0]
    school_obs = obs  # shape (j, n_per_school)

    # We work in unconstrained space: positive tau via log transform.
    # Parameters are flat: x[0]=mu, x[1]=log_tau, x[2:5]=theta_j
    def logp(x):
        mu = x[0]
        log_tau = x[1]
        theta = x[2:2 + j]
        tau = jnp.exp(log_tau)
        # Priors (Normal/Exp) with Jacobian for log_tau.
        lp_mu = -0.5 * (mu / 100.0) ** 2
        lp_tau = jnp.log(0.1) - 0.1 * tau + log_tau  # +log_tau for Jacobian
        lp_theta = jnp.sum(-0.5 * ((theta - mu) / tau) ** 2 - jnp.log(tau))
        # Likelihood
        lp_y = jnp.sum(
            -0.5 * ((school_obs - theta[:, None]) / SIGMA_Y) ** 2
        )
        return lp_mu + lp_tau + lp_theta + lp_y

    init = jnp.array([73.0, jnp.log(10.0), 71.5, 86.25, 61.75])
    rng_key = jax.random.PRNGKey(SEED)

    # Window adaptation for step size + mass matrix.
    warmup = blackjax.window_adaptation(blackjax.nuts, logp)

    def run():
        rng_warmup, rng_sample = jax.random.split(rng_key)
        (state, params), _ = warmup.run(rng_warmup, init,
                                         num_steps=N_WARMUP)
        kernel = blackjax.nuts(logp, **params).step

        def one_step(state, key):
            new_state, _ = kernel(key, state)
            return new_state, new_state.position

        keys = jax.random.split(rng_sample, N_SAMPLES)
        _, samples = jax.lax.scan(one_step, state, keys)
        return samples

    # Warm JIT first
    _ = run()
    t0 = time.perf_counter()
    samples = run()
    samples = jax.block_until_ready(samples)
    t1 = time.perf_counter()

    mu_samples = np.asarray(samples[:, 0])
    log_tau_samples = np.asarray(samples[:, 1])
    tau_samples = np.exp(log_tau_samples)

    # Naive ESS (autocorr-based via arviz for fair comparison).
    import arviz as az
    mu_ess = float(az.ess(np.asarray(mu_samples)))
    tau_ess = float(az.ess(np.asarray(tau_samples)))
    return Row(
        name, 1000.0 * (t1 - t0), float(mu_samples.mean()), mu_ess,
        f"blackjax NUTS warmup={N_WARMUP} draws={N_SAMPLES} "
        f"ess(mu)={mu_ess:.1f} ess(tau)={tau_ess:.1f}",
    )


# ---------------------------------------------------------------------------

def write_rows(path: Path, rows: list[Row], suite: str = "mcmc") -> None:
    with path.open("w", newline="") as fh:
        w = csv.writer(fh)
        w.writerow(["system", "suite", "name",
                    "time_ms", "acc_main", "acc_aux", "extra"])
        for r in rows:
            w.writerow(["python", suite, r.name,
                        f"{r.time_ms:.6g}", f"{r.acc_main:.6g}",
                        f"{r.acc_aux:.6g}", r.extra])


def main() -> None:
    rows: list[Row] = []
    print("Running PyMC NUTS …")
    rows.append(bench_pymc_nuts("NUTS_8schools_warm500_n1000_pymc"))
    print("Running blackjax NUTS …")
    rows.append(bench_blackjax_nuts("NUTS_8schools_warm500_n1000_blackjax"))
    out = OUT / "mcmc.csv"
    write_rows(out, rows)
    print(f"wrote {len(rows)} rows → {out}")


if __name__ == "__main__":
    main()
