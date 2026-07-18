#!/usr/bin/env python3
"""HBM サンプラ性能スケーリングベンチ — Python (PyMC) 側。

``bench/haskell/BenchHBMScaling.hs`` と同じ M1-M9 (pooled 単回帰 / 階層
random intercept / 階層 ranint+slope / 多変量 X / パラメタ非線形 /
階層×非線形 / Poisson 回帰 / logistic 回帰 / NegBin 回帰) を **同一データ
CSV** (``bench/data/hbm_m{1..9}.csv``) から PyMC NUTS で走らせ、
同じ iter グリッドを warmup 固定で掃く。
引数: 無し=M1-M8 全部 / ``glm``=M7-M9 のみ (``hbm_scaling_glm.csv``) /
``m7-long``/``m8-long``/``m9-long``=延長 grid
(``hbm_scaling_m{7,8,9}_long.csv``)。

  total = (compile + tune 固定費) + (1 draw 単価) * draws

の線形フィットで切片と傾きを分離する。 PyMC は logp を 1 回コンパイルする
固定費があるため切片に乗る (hanalyze は AD ランタイム評価で compile 費なし)。
各 (model, draws) で n 回計測し median。 ESS と平均 tree_depth も記録。

Run with::

    OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 \\
        ~/.virtualenvs/pymc/bin/python bench/python/bench_hbm_scaling.py

NUTS バックエンド切替 (Phase 56.6 追補): ``BENCH_NUTS_SAMPLER=numpyro`` で
PyMC の numpyro (JAX) backend を使う。 結果 CSV は ``_numpyro`` suffix で
既存 (C backend) と分離。 公平比較のため JAX も 1 スレッドに制限して走らせる::

    OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 \\
        XLA_FLAGS="--xla_cpu_multi_thread_eigen=false intra_op_parallelism_threads=1" \\
        BENCH_NUTS_SAMPLER=numpyro \\
        ~/.virtualenvs/pymc/bin/python bench/python/bench_hbm_scaling.py glm
"""

from __future__ import annotations

import csv
import os
import time
from dataclasses import dataclass
from pathlib import Path

import numpy as np

# NUTS サンプラー実装: "pymc" (PyMC 自身の NUTS ループ) / "nutpie" (Rust) /
# "numpyro" (JAX) / "blackjax" (JAX)。
#
# Phase 88: SAMPLER=="pymc" のみ既定で BENCH_NUTS_SAMPLER 未指定を許すが、
# ★nuts_sampler は必ず明示指定する (下記 "重大発見" 参照 — nutpie インストール後は
# nuts_sampler 未指定の pm.sample() が条件次第で黙って nutpie に自動選択される。
# 1354/0 テストとは無関係の Python 側の罠だが、対等ベンチの前提を壊すため
# 全ケースで明示指定に統一する)。
SAMPLER = os.environ.get("BENCH_NUTS_SAMPLER", "pymc")

# Phase 88: SAMPLER=="pymc" のときの勾配コンパイルバックエンド。
# "numba" (既定・pytensor 3.1 の既定 linker=NumbaLinker と同じ) /
# "cvm" (真の C/Cython・PyTensor "c" は内部で "cvm" へ正規化される) /
# "jax" (PyMC 自身の NUTS ループ + JAX 勾配・numpyro=NumPyro 自身の NUTS 実装とは別物)。
# SAMPLER!="pymc" のときは無視 (nutpie/numpyro/blackjax は自身の既定/BENCH_NUTPIE_BACKEND
# に従う)。
COMPILE_BACKEND = os.environ.get("BENCH_COMPILE_BACKEND", "numba")
# Phase 88: nutpie の勾配バックエンド ("numba" 既定 / "jax")。SAMPLER=="nutpie" のみ有効。
NUTPIE_BACKEND = os.environ.get("BENCH_NUTPIE_BACKEND", "numba")

_SUFFIX_PARTS = [SAMPLER] if SAMPLER != "pymc" else []
if SAMPLER == "pymc" and COMPILE_BACKEND != "numba":
    _SUFFIX_PARTS.append(COMPILE_BACKEND)
if SAMPLER == "nutpie" and NUTPIE_BACKEND != "numba":
    _SUFFIX_PARTS.append(NUTPIE_BACKEND)
OUT_SUFFIX = "" if not _SUFFIX_PARTS else "_" + "_".join(_SUFFIX_PARTS)


REPO = Path(__file__).resolve().parents[2]
DATA = REPO / "bench" / "data"
OUT = REPO / "bench" / "results" / "python"
OUT.mkdir(parents=True, exist_ok=True)

ITER_GRID = [50, 100, 200, 400, 800, 1600]
WARMUP = 500
SEED = 42
REPS = 3
TARGET_ACCEPT = 0.8
MAX_TREEDEPTH = 10


@dataclass
class Row:
    name: str
    time_ms: float
    acc_main: float   # 主役パラメタの posterior mean
    acc_aux: float    # 主役パラメタの ess_bulk
    extra: str


def _read_csv(path: Path):
    with open(path) as f:
        r = csv.reader(f)
        header = next(r)
        rows = [[float(c) for c in row] for row in r]
    return header, np.array(rows)


def load_m1():
    _, arr = _read_csv(DATA / "hbm_m1.csv")  # x0,y
    return arr[:, 0], arr[:, 1]


def load_m2():
    _, arr = _read_csv(DATA / "hbm_m2.csv")  # x0,group,y
    return arr[:, 0], arr[:, 1].astype(int), arr[:, 2]


def load_m3():
    _, arr = _read_csv(DATA / "hbm_m3.csv")  # x0,group,y
    return arr[:, 0], arr[:, 1].astype(int), arr[:, 2]


def load_m4():
    _, arr = _read_csv(DATA / "hbm_m4.csv")  # x0..x9,y
    return arr[:, :-1], arr[:, -1]


def load_m5():
    _, arr = _read_csv(DATA / "hbm_m5.csv")  # x0,y
    return arr[:, 0], arr[:, 1]


def load_m6():
    _, arr = _read_csv(DATA / "hbm_m6.csv")  # x0,group,y
    return arr[:, 0], arr[:, 1].astype(int), arr[:, 2]


def load_m7():
    _, arr = _read_csv(DATA / "hbm_m7.csv")  # x0,y (y = 非負整数 count)
    return arr[:, 0], arr[:, 1].astype(int)


def load_m8():
    _, arr = _read_csv(DATA / "hbm_m8.csv")  # x0,y (y = 0/1)
    return arr[:, 0], arr[:, 1].astype(int)


def load_m9():
    _, arr = _read_csv(DATA / "hbm_m9.csv")  # x0,y (y = 非負整数 count)
    return arr[:, 0], arr[:, 1].astype(int)


def load_radon():
    """radon.csv (county 名列があるので _read_csv でなく手読み)。
    列 = county(str), county_idx(int), floor(0/1), log_radon, log_uranium。
    返り値 = (county_idx, floor, log_radon, log_uranium) の numpy 配列。"""
    cidx, floor, lr, lu = [], [], [], []
    with open(DATA / "radon.csv") as f:
        r = csv.reader(f)
        next(r)
        for row in r:
            cidx.append(int(row[1]))
            floor.append(float(row[2]))
            lr.append(float(row[3]))
            lu.append(float(row[4]))
    return (np.array(cidx), np.array(floor), np.array(lr), np.array(lu))


def _sample(build, draws):
    """build() は pm.Model context 内でサンプル前の設定を行い idata を返す。

    Phase 88: nuts_sampler は **常に明示指定** する (nutpie インストール後は
    未指定だと条件次第で黙って nutpie が自動選択されるため・"重大発見" 節参照)。
    """
    import pymc as pm

    def run():
        with pm.Model():
            build()
            kwargs = dict(
                draws=draws, tune=WARMUP, chains=1, cores=1,
                random_seed=SEED, progressbar=False,
                target_accept=TARGET_ACCEPT,
                return_inferencedata=True,
                compute_convergence_checks=False,
                nuts_sampler=SAMPLER,
            )
            if SAMPLER == "pymc":
                kwargs["nuts"] = {"max_treedepth": MAX_TREEDEPTH}
                if COMPILE_BACKEND != "numba":
                    kwargs["compile_kwargs"] = {"mode": COMPILE_BACKEND}
            elif SAMPLER == "nutpie":
                if NUTPIE_BACKEND != "numba":
                    kwargs["compile_kwargs"] = {"backend": NUTPIE_BACKEND}
            # numpyro/blackjax: max_treedepth は各自の既定が 10 で
            # max_treedepth=10 設定と同値ゆえ明示指定しない (従来どおり)。
            return pm.sample(**kwargs)

    times = []
    idata = None
    for _ in range(REPS):
        t0 = time.perf_counter()
        idata = run()
        t1 = time.perf_counter()
        times.append(1000.0 * (t1 - t0))
    return float(np.median(times)), idata


def _summarize(idata, key, label=None):
    import arviz as az
    label = label or key
    s = az.summary(idata, var_names=[key])
    mean = float(s.loc[label, "mean"])
    ess = float(s.loc[label, "ess_bulk"])
    # 平均 tree depth (warmup 後の本サンプル)
    td = np.nan
    if "tree_depth" in idata.sample_stats:
        td = float(np.asarray(idata.sample_stats["tree_depth"]).mean())
    return mean, ess, td


def bench_m1(draws) -> Row:
    import pymc as pm
    x, y = load_m1()

    def build():
        a = pm.Normal("a", mu=0, sigma=10)
        b = pm.Normal("b", mu=0, sigma=10)
        sigma = pm.Exponential("sigma", lam=1.0)
        pm.Normal("y", mu=a + b * x, sigma=sigma, observed=y)

    ms, idata = _sample(build, draws)
    mean, ess, td = _summarize(idata, "b")
    eps = ess / max(1e-9, ms / 1000.0)
    return Row(
        f"M1_pooled_iter{draws}", ms, mean, ess,
        f"iter={draws} warmup={WARMUP} key=b ess={ess:.1f} "
        f"ess_per_sec={eps:.2f} tree_depth={td:.2f} time_ms={ms:.1f}",
    )


def bench_m2(draws) -> Row:
    import pymc as pm
    x, gid, y = load_m2()
    nG = int(gid.max()) + 1

    def build():
        b0 = pm.Normal("beta_0", mu=0, sigma=5)
        b1 = pm.Normal("beta_1", mu=0, sigma=5)
        tau_u = pm.HalfNormal("tau_u", sigma=5)
        u = pm.Normal("u", mu=0, sigma=tau_u, shape=nG)
        sigma = pm.Exponential("sigma", lam=1.0)
        eta = b0 + b1 * x + u[gid]
        pm.Normal("y", mu=eta, sigma=sigma, observed=y)

    ms, idata = _sample(build, draws)
    mean, ess, td = _summarize(idata, "beta_1")
    eps = ess / max(1e-9, ms / 1000.0)
    return Row(
        f"M2_ranint_iter{draws}", ms, mean, ess,
        f"iter={draws} warmup={WARMUP} key=beta_1 ess={ess:.1f} "
        f"ess_per_sec={eps:.2f} tree_depth={td:.2f} time_ms={ms:.1f}",
    )


def bench_m3(draws) -> Row:
    """M3 階層 random intercept+slope (HS 側は per-obs 手書き・同一 prior)。"""
    import pymc as pm
    x, gid, y = load_m3()
    nG = int(gid.max()) + 1

    def build():
        b0 = pm.Normal("beta_0", mu=0, sigma=5)
        b1 = pm.Normal("beta_1", mu=0, sigma=5)
        tau_u = pm.HalfNormal("tau_u", sigma=5)
        tau_v = pm.HalfNormal("tau_v", sigma=5)
        u = pm.Normal("u", mu=0, sigma=tau_u, shape=nG)
        v = pm.Normal("v", mu=0, sigma=tau_v, shape=nG)
        sigma = pm.Exponential("sigma", lam=1.0)
        eta = b0 + b1 * x + u[gid] + v[gid] * x
        pm.Normal("y", mu=eta, sigma=sigma, observed=y)

    ms, idata = _sample(build, draws)
    mean, ess, td = _summarize(idata, "beta_1")
    eps = ess / max(1e-9, ms / 1000.0)
    return Row(
        f"M3_ranslope_iter{draws}", ms, mean, ess,
        f"iter={draws} warmup={WARMUP} key=beta_1 ess={ess:.1f} "
        f"ess_per_sec={eps:.2f} tree_depth={td:.2f} time_ms={ms:.1f}",
    )


def bench_m4(draws) -> Row:
    """M4 多変量 X pooled (p=10 + intercept)。"""
    import pymc as pm
    X, y = load_m4()

    def build():
        beta = pm.Normal("beta", mu=0, sigma=5, shape=X.shape[1] + 1)
        sigma = pm.Exponential("sigma", lam=1.0)
        eta = beta[0] + pm.math.dot(X, beta[1:])
        pm.Normal("y", mu=eta, sigma=sigma, observed=y)

    ms, idata = _sample(build, draws)
    mean, ess, td = _summarize(idata, "beta", label="beta[1]")
    eps = ess / max(1e-9, ms / 1000.0)
    return Row(
        f"M4_multix_iter{draws}", ms, mean, ess,
        f"iter={draws} warmup={WARMUP} key=beta[1] ess={ess:.1f} "
        f"ess_per_sec={eps:.2f} tree_depth={td:.2f} time_ms={ms:.1f}",
    )


def bench_m5(draws) -> Row:
    """M5 パラメタ非線形 y = a·exp(-b·x) + c。"""
    import pymc as pm
    x, y = load_m5()

    def build():
        a = pm.Normal("a", mu=0, sigma=10)
        b = pm.HalfNormal("b", sigma=2)
        c = pm.Normal("c", mu=0, sigma=10)
        sigma = pm.Exponential("sigma", lam=1.0)
        mu = a * pm.math.exp(-b * x) + c
        pm.Normal("y", mu=mu, sigma=sigma, observed=y)

    ms, idata = _sample(build, draws)
    mean, ess, td = _summarize(idata, "b")
    eps = ess / max(1e-9, ms / 1000.0)
    return Row(
        f"M5_nonlin_iter{draws}", ms, mean, ess,
        f"iter={draws} warmup={WARMUP} key=b ess={ess:.1f} "
        f"ess_per_sec={eps:.2f} tree_depth={td:.2f} time_ms={ms:.1f}",
    )


def bench_m6(draws) -> Row:
    """M6 階層 × 非線形 y = a_g·exp(-b·x)。"""
    import pymc as pm
    x, gid, y = load_m6()
    nG = int(gid.max()) + 1

    def build():
        mu_a = pm.Normal("mu_a", mu=0, sigma=10)
        tau_a = pm.HalfNormal("tau_a", sigma=2)
        a = pm.Normal("a", mu=mu_a, sigma=tau_a, shape=nG)
        b = pm.HalfNormal("b", sigma=2)
        sigma = pm.Exponential("sigma", lam=1.0)
        mu = a[gid] * pm.math.exp(-b * x)
        pm.Normal("y", mu=mu, sigma=sigma, observed=y)

    ms, idata = _sample(build, draws)
    mean, ess, td = _summarize(idata, "b")
    eps = ess / max(1e-9, ms / 1000.0)
    return Row(
        f"M6_hier_nonlin_iter{draws}", ms, mean, ess,
        f"iter={draws} warmup={WARMUP} key=b ess={ess:.1f} "
        f"ess_per_sec={eps:.2f} tree_depth={td:.2f} time_ms={ms:.1f}",
    )


def bench_m7(draws) -> Row:
    """M7 Poisson 回帰 (log link)。 HS 側は per-obs 手書き・同一 prior。"""
    import pymc as pm
    x, y = load_m7()

    def build():
        a = pm.Normal("a", mu=0, sigma=5)
        b = pm.Normal("b", mu=0, sigma=5)
        pm.Poisson("y", mu=pm.math.exp(a + b * x), observed=y)

    ms, idata = _sample(build, draws)
    mean, ess, td = _summarize(idata, "b")
    eps = ess / max(1e-9, ms / 1000.0)
    return Row(
        f"M7_pois_iter{draws}", ms, mean, ess,
        f"iter={draws} warmup={WARMUP} key=b ess={ess:.1f} "
        f"ess_per_sec={eps:.2f} tree_depth={td:.2f} time_ms={ms:.1f}",
    )


def bench_m8(draws) -> Row:
    """M8 logistic 回帰 (logit link)。"""
    import pymc as pm
    x, y = load_m8()

    def build():
        a = pm.Normal("a", mu=0, sigma=5)
        b = pm.Normal("b", mu=0, sigma=5)
        pm.Bernoulli("y", logit_p=a + b * x, observed=y)

    ms, idata = _sample(build, draws)
    mean, ess, td = _summarize(idata, "b")
    eps = ess / max(1e-9, ms / 1000.0)
    return Row(
        f"M8_logit_iter{draws}", ms, mean, ess,
        f"iter={draws} warmup={WARMUP} key=b ess={ess:.1f} "
        f"ess_per_sec={eps:.2f} tree_depth={td:.2f} time_ms={ms:.1f}",
    )


def bench_m9(draws) -> Row:
    """M9 NegBin 回帰 (log link・α latent)。 HS 側と同一 prior。

    pm.NegativeBinomial(mu, alpha) と hanalyze の
    ``logDensityObs (NegativeBinomial mu α)`` は同一パラメタ化
    (p = α/(α+μ)・密度 5 点突合 diff ≤ 9e-15 で確認済・Phase 56.6)。
    """
    import pymc as pm
    x, y = load_m9()

    def build():
        a = pm.Normal("a", mu=0, sigma=5)
        b = pm.Normal("b", mu=0, sigma=5)
        alpha = pm.Exponential("alpha", lam=0.5)
        pm.NegativeBinomial("y", mu=pm.math.exp(a + b * x), alpha=alpha,
                            observed=y)

    ms, idata = _sample(build, draws)
    mean, ess, td = _summarize(idata, "b")
    eps = ess / max(1e-9, ms / 1000.0)
    return Row(
        f"M9_negbin_iter{draws}", ms, mean, ess,
        f"iter={draws} warmup={WARMUP} key=b ess={ess:.1f} "
        f"ess_per_sec={eps:.2f} tree_depth={td:.2f} time_ms={ms:.1f}",
    )


def bench_radon(draws) -> Row:
    """Radon (flagship・Phase 84)。 相関 varying intercept+slope (county 群・
    slope on floor)・固定 uranium。 HS 側 designHBMProgram の相関 RE (LKJ・非中心化)
    と同一 prior: beta~N(0,10)・sigma~HalfN(5)・tau~HalfN(5)・LKJ eta=2・z~N(0,1)。
    主役 = beta[1] (floor 係数)。"""
    import pymc as pm
    cidx, floor, log_radon, log_uranium = load_radon()
    nC = int(cidx.max()) + 1

    def build():
        beta = pm.Normal("beta", mu=0, sigma=10, shape=3)   # Intercept, floor, uranium
        sigma = pm.HalfNormal("sigma", sigma=5)
        chol, _, _ = pm.LKJCholeskyCov(
            "chol", n=2, eta=2.0,
            sd_dist=pm.HalfNormal.dist(5.0), compute_corr=True)
        z = pm.Normal("z", 0.0, 1.0, shape=(nC, 2))
        b = z @ chol.T                                       # (nC, 2) 群効果
        mu = (beta[0] + beta[1] * floor + beta[2] * log_uranium
              + b[cidx, 0] + b[cidx, 1] * floor)
        pm.Normal("y", mu=mu, sigma=sigma, observed=log_radon)

    ms, idata = _sample(build, draws)
    mean, ess, td = _summarize(idata, "beta", label="beta[1]")
    eps = ess / max(1e-9, ms / 1000.0)
    return Row(
        f"radon_iter{draws}", ms, mean, ess,
        f"iter={draws} warmup={WARMUP} key=beta[1] ess={ess:.1f} "
        f"ess_per_sec={eps:.2f} tree_depth={td:.2f} time_ms={ms:.1f}",
    )


def bench_eightschools(draws) -> Row:
    """Eight Schools (精度エッジ・Phase 84)。 古典階層正規・funnel の定番。
    非中心化: theta_j = mu + tau*theta_t_j (theta_t~N(0,1))・観測 SE sigma_j 既知。
    mu~N(0,5)・tau~HalfCauchy(5)。 HS 側 eightSchoolsModel と同一 prior・データ。
    主役 = tau (funnel の首)。"""
    import pymc as pm
    y = np.array([28, 8, -3, 7, -1, 1, 18, 12], float)
    sigma = np.array([15, 10, 16, 11, 9, 11, 10, 18], float)

    def build():
        mu = pm.Normal("mu", mu=0, sigma=5)
        tau = pm.HalfCauchy("tau", beta=5)
        tt = pm.Normal("theta_t", mu=0, sigma=1, shape=8)
        theta = mu + tau * tt
        pm.Normal("y", mu=theta, sigma=sigma, observed=y)

    ms, idata = _sample(build, draws)
    mean, ess, td = _summarize(idata, "tau")
    eps = ess / max(1e-9, ms / 1000.0)
    return Row(
        f"eightschools_iter{draws}", ms, mean, ess,
        f"iter={draws} warmup={WARMUP} key=tau ess={ess:.1f} "
        f"ess_per_sec={eps:.2f} tree_depth={td:.2f} time_ms={ms:.1f}",
    )


ITER_GRID_LONG = [400, 800, 1600, 3200, 6400, 12800, 25600]


def _write_rows(rows, out):
    with open(out, "w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["system", "suite", "name", "time_ms",
                    "acc_main", "acc_aux", "extra"])
        for r in rows:
            w.writerow(["python", "hbm_scaling", r.name,
                        f"{r.time_ms:.6g}", f"{r.acc_main:.6g}",
                        f"{r.acc_aux:.6g}", r.extra])
    print(f"wrote {len(rows)} rows → {out}")


def main():
    """引数: 無し=M1-M8 全部 / glm=M7-M9 のみ / radon (Phase 84 flagship) /
    radon1600 (Phase 88・実運用規模 iter=1600 単点) /
    eightschools (Phase 84 精度エッジ) / m7-long / m8-long / m9-long。"""
    import sys
    mode = sys.argv[1] if len(sys.argv) > 1 else "all"

    if mode == "eightschools":
        rows = []
        for draws in ITER_GRID:
            rows.append(bench_eightschools(draws))
            print(f"eightschools iter={draws}: {rows[-1].time_ms:.1f} ms", flush=True)
        _write_rows(rows, OUT / f"hbm_scaling_eightschools{OUT_SUFFIX}.csv")
        return

    if mode == "radon":
        # radon は 919 obs・相関 RE で deep tree ゆえ重い。 HS 側と揃えて
        # reps=2・grid=[50,100,200,400] で回す。
        global REPS
        REPS = 2
        rows = []
        for draws in [50, 100, 200, 400]:
            rows.append(bench_radon(draws))
            print(f"radon iter={draws}: {rows[-1].time_ms:.1f} ms", flush=True)
        _write_rows(rows, OUT / f"hbm_scaling_radon{OUT_SUFFIX}.csv")
        return

    if mode == "radon1600":
        # Phase 88 追補: iter400 は短めの grid で「有利なベンチ」になりうる
        # (Phase 87 で iter400→1600 だけで hanalyze 対 PyMC-C 比が 0.80×→
        # 0.49-0.68× に動いた前例あり)。実運用に近い iter=1600 単点を
        # reps=2 で計測する (draws=1000 が PyMC 既定・1600 はそれより大きい
        # 実運用規模)。
        REPS = 2
        r = bench_radon(1600)
        print(f"radon iter=1600: {r.time_ms:.1f} ms", flush=True)
        _write_rows([r], OUT / f"hbm_scaling_radon1600{OUT_SUFFIX}.csv")
        return

    if mode in ("m7-long", "m8-long", "m9-long"):
        tag, fn = {"m7-long": ("M7", bench_m7),
                   "m8-long": ("M8", bench_m8),
                   "m9-long": ("M9", bench_m9)}[mode]
        rows = []
        for draws in ITER_GRID_LONG:
            rows.append(fn(draws))
            print(f"{tag} iter={draws}: {rows[-1].time_ms:.1f} ms", flush=True)
        _write_rows(rows,
                    OUT / f"hbm_scaling_{mode.split('-')[0]}_long{OUT_SUFFIX}.csv")
        return

    if mode == "glm":
        benches = [("M7", bench_m7), ("M8", bench_m8), ("M9", bench_m9)]
        out = OUT / f"hbm_scaling_glm{OUT_SUFFIX}.csv"
    else:
        benches = [("M1", bench_m1), ("M2", bench_m2), ("M3", bench_m3),
                   ("M4", bench_m4), ("M5", bench_m5), ("M6", bench_m6),
                   ("M7", bench_m7), ("M8", bench_m8)]
        out = OUT / f"hbm_scaling{OUT_SUFFIX}.csv"

    rows = []
    for tag, fn in benches:
        for draws in ITER_GRID:
            rows.append(fn(draws))
            print(f"{tag} iter={draws}: {rows[-1].time_ms:.1f} ms", flush=True)
    _write_rows(rows, out)


if __name__ == "__main__":
    main()
