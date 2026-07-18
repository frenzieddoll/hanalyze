"""hmm: nutpie+jax の per-draw 診断採取 (Phase 92 ess/draw 効率調査)。

run_pymc_matrix.py と同条件 (4chain・warmup1000+draws1000・seed1・CPU) で
nutpie を直接 API で回し、以下を分離出力する:
  1. wall 内訳 (model build / compile / sample=tune+draws) — 比較非対称の統一用
  2. post-warmup per-draw 診断: n_steps (leapfrog)・depth・step_size・
     mean_tree_accept・divergences — hanalyze 側 Chain 診断との突き合わせ用
  3. 全パラメータ ess_bulk / ess_tail / rhat
"""
import os
import sys
import time
import warnings

os.environ.setdefault("JAX_PLATFORMS", "cpu")
warnings.filterwarnings("ignore")
sys.path.insert(0, os.path.dirname(__file__))

import arviz as az  # noqa: E402
import numpy as np  # noqa: E402
import nutpie  # noqa: E402

from model import hmm_model  # noqa: E402


def main():
    seed = int(sys.argv[1]) if len(sys.argv) > 1 else 1
    print(f"seed = {seed}")
    t0 = time.perf_counter()
    m = hmm_model()
    t1 = time.perf_counter()
    compiled = nutpie.compile_pymc_model(m, backend="jax")
    t2 = time.perf_counter()
    idata = nutpie.sample(compiled, draws=1000, tune=1000, chains=4,
                          cores=1, seed=seed, progress_bar=False,
                          save_warmup=True)
    t3 = time.perf_counter()

    print(f"wall build   = {1000*(t1-t0):9.1f} ms")
    print(f"wall compile = {1000*(t2-t1):9.1f} ms")
    print(f"wall sample  = {1000*(t3-t2):9.1f} ms (tune+draws)")

    ss = idata.sample_stats
    print("\n== post-warmup per-draw stats (chains x 1000 draws) ==")
    print(f"{'chain':>5} {'mean_nsteps':>11} {'mean_depth':>10} "
          f"{'step_size':>10} {'mean_accept':>11} {'div':>4}")
    for c in range(ss.sizes["chain"]):
        nsteps = ss["n_steps"].isel(chain=c).values
        depth = ss["depth"].isel(chain=c).values
        eps = ss["step_size"].isel(chain=c).values
        acc = ss["mean_tree_accept"].isel(chain=c).values
        div = int(ss["diverging"].isel(chain=c).values.sum())
        print(f"{c:>5} {nsteps.mean():>11.1f} {depth.mean():>10.2f} "
              f"{eps.mean():>10.5f} {acc.mean():>11.3f} {div:>4}")
    nsteps_all = ss["n_steps"].values
    depth_all = ss["depth"].values
    print(f"\ntotal leapfrog (post-warmup) = {int(nsteps_all.sum())}")
    vals, cnts = np.unique(depth_all, return_counts=True)
    print("depth histogram:", {int(v): int(c) for v, c in zip(vals, cnts)})

    if hasattr(idata, "warmup_sample_stats"):
        ws = idata.warmup_sample_stats
        wn = ws["n_steps"].values
        print(f"total leapfrog (warmup)      = {int(wn.sum())}")
        eps_w = ws["step_size"].values
        print("warmup step_size trajectory (chain 0, every 100):")
        print(np.array2string(eps_w[0, ::100], precision=5))

    print("\n== summary (all params) ==")
    s = az.summary(idata, var_names=["mu_1", "mu_2", "gap", "theta1", "theta2"])
    print(s.to_string())
    ess = float(s.loc["mu_1", "ess_bulk"])
    print(f"\ness_bulk(mu_1) = {ess:.1f}  ess/draw = {ess/4000:.4f}")


if __name__ == "__main__":
    main()
