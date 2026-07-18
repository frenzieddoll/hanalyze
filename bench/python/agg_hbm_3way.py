#!/usr/bin/env python3
"""HBM 3 系ベンチの集約 — Phase 84。

hanalyze (haskell) / PyMC (PyTensor default) / PyMC (NumPyro backend) の 3 系の
結果 CSV を **name で join** し、 速度 (time_ms・ess_per_sec) と精度 (acc_main =
主役パラメタの事後平均) を横並びの Markdown 表にする。 pure-python (pandas 非依存)。

入力 CSV (統一スキーマ `system,suite,name,time_ms,acc_main,acc_aux,extra`):
  - hanalyze      : bench/results/haskell/<stem>.csv
  - PyMC default : bench/results/python/<stem>.csv
  - PyMC numpyro : bench/results/python/<stem>_numpyro.csv

精度基準 = **PyMC default** (確定事項 3)。 hanalyze / numpyro の事後平均が PyMC と
妥当な範囲で一致するかを Δ (絶対差) で示す。 速度は PyMC default 比の speedup。

Run:
  python3 bench/python/agg_hbm_3way.py            # stem=hbm_scaling
  python3 bench/python/agg_hbm_3way.py hbm_scaling_glm
"""
import csv
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
RESULTS = os.path.join(HERE, "..", "results")


def parse_extra(s):
    """`iter=50 warmup=500 key=b ess=49.9 ...` → dict。"""
    d = {}
    for tok in (s or "").split():
        if "=" in tok:
            k, v = tok.split("=", 1)
            d[k] = v
    return d


def read_csv(path):
    """name → row dict。 存在しなければ空。"""
    out = {}
    if not os.path.exists(path):
        return out
    with open(path, newline="") as f:
        for row in csv.DictReader(f):
            row["_extra"] = parse_extra(row.get("extra", ""))
            out[row["name"]] = row
    return out


def fnum(x, default=float("nan")):
    try:
        return float(x)
    except (TypeError, ValueError):
        return default


def fmt(x, nd=2):
    return "-" if x != x else f"{x:.{nd}f}"  # NaN 判定 (x!=x)


def main():
    stem = sys.argv[1] if len(sys.argv) > 1 else "hbm_scaling"
    hs = read_csv(os.path.join(RESULTS, "haskell", f"{stem}.csv"))
    pc = read_csv(os.path.join(RESULTS, "python", f"{stem}.csv"))
    np_ = read_csv(os.path.join(RESULTS, "python", f"{stem}_numpyro.csv"))

    names = sorted(set(hs) | set(pc) | set(np_),
                   key=lambda n: (n.rsplit("_iter", 1)[0],
                                  int(n.rsplit("_iter", 1)[1]) if "_iter" in n else 0))
    if not names:
        print(f"no rows for stem '{stem}' (systems ran?)", file=sys.stderr)
        sys.exit(1)

    print(f"# HBM 3-way benchmark — `{stem}`\n")
    print("速度基準・精度基準 = **PyMC (default)**。 速度 speedup = PyMC_ms / sys_ms "
          "(>1 = 速い)。 精度 Δ = |mean − PyMC_mean| (小さいほど一致)。\n")

    # --- 速度表 ---
    print("## 速度 (time_ms・ess/sec)\n")
    hdr = ["model/iter", "hanalyze_ms", "pymc_ms", "numpyro_ms",
           "speedup_ael", "speedup_npy", "ess/s_ael", "ess/s_pymc", "ess/s_npy"]
    rows = []
    for n in names:
        hm = fnum(hs.get(n, {}).get("time_ms"))
        pm = fnum(pc.get(n, {}).get("time_ms"))
        nm = fnum(np_.get(n, {}).get("time_ms"))
        ea = fnum(hs.get(n, {}).get("_extra", {}).get("ess_per_sec"))
        ep = fnum(pc.get(n, {}).get("_extra", {}).get("ess_per_sec"))
        en = fnum(np_.get(n, {}).get("_extra", {}).get("ess_per_sec"))
        rows.append([n, fmt(hm, 1), fmt(pm, 1), fmt(nm, 1),
                     fmt(pm / hm) if hm == hm and hm else "-",
                     fmt(pm / nm) if nm == nm and nm else "-",
                     fmt(ea), fmt(ep), fmt(en)])
    print_md(hdr, rows)

    # --- 精度表 ---
    print("\n## 精度 (主役パラメタ事後平均・基準 = PyMC)\n")
    hdr2 = ["model/iter", "mean_ael", "mean_pymc", "mean_npy",
            "Δ_ael", "Δ_npy", "ess_ael", "ess_pymc", "ess_npy"]
    rows2 = []
    for n in names:
        ma = fnum(hs.get(n, {}).get("acc_main"))
        mp = fnum(pc.get(n, {}).get("acc_main"))
        mn = fnum(np_.get(n, {}).get("acc_main"))
        ea = fnum(hs.get(n, {}).get("acc_aux"))
        ep = fnum(pc.get(n, {}).get("acc_aux"))
        en = fnum(np_.get(n, {}).get("acc_aux"))
        rows2.append([n, fmt(ma, 4), fmt(mp, 4), fmt(mn, 4),
                      fmt(abs(ma - mp), 4) if ma == ma and mp == mp else "-",
                      fmt(abs(mn - mp), 4) if mn == mn and mp == mp else "-",
                      fmt(ea, 0), fmt(ep, 0), fmt(en, 0)])
    print_md(hdr2, rows2)


def print_md(hdr, rows):
    print("| " + " | ".join(hdr) + " |")
    print("|" + "|".join(["---"] * len(hdr)) + "|")
    for r in rows:
        print("| " + " | ".join(str(c) for c in r) + " |")


if __name__ == "__main__":
    main()
