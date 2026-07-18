#!/usr/bin/env python3
"""Radon (Gelman multilevel) の生 CSV 生成 — Phase 84 flagship 題材。

一次データ = Gelman の radon (Minnesota) を PyMC-examples ミラーから取得し、
標準前処理 (MN 抽出・log_radon・county index・郡レベル log_uranium 併合) して
``bench/data/radon.csv`` に**素の CSV**として焼く。 hanalyze (Haskell) 側と
PyMC/NumPyro (Python) 側が**同一ファイル**を読む (bench 共有 CSV 方式)。

前処理は PyMC の radon チュートリアルと同型 (pandas 非依存・pure csv):
  - srrs2.dat から state=="MN" を抽出、fips = stfips*1000 + cntyfips
  - cty.dat から MN の Uppm を fips で併合
  - idnum で重複除去
  - log_radon = log(activity + 0.1)、log_uranium = log(Uppm)
  - floor は srrs2 の floor 列 (0=basement, 1=first)

出力列: county, county_idx, floor, log_radon, log_uranium
決定的 (RNG なし・実データ)。

Run:
  python3 bench/python/gen_radon_csv.py
"""
import csv
import io
import math
import os
import urllib.request

HERE = os.path.dirname(os.path.abspath(__file__))
OUT = os.path.join(HERE, "..", "data", "radon.csv")
BASE = "https://raw.githubusercontent.com/pymc-devs/pymc-examples/main/examples/data"


def fetch(name):
    with urllib.request.urlopen(f"{BASE}/{name}", timeout=30) as r:
        return r.read().decode("utf-8", errors="replace")


def read_rows(text):
    rows = list(csv.DictReader(io.StringIO(text)))
    # ヘッダ前後の空白を除去
    cleaned = []
    for row in rows:
        cleaned.append({(k.strip() if k else k): (v.strip() if isinstance(v, str) else v)
                        for k, v in row.items()})
    return cleaned


def main():
    srrs = read_rows(fetch("srrs2.dat"))
    cty = read_rows(fetch("cty.dat"))

    # 郡レベル uranium (MN・fips → Uppm)
    uppm = {}
    for r in cty:
        if r.get("st") != "MN":
            continue
        fips = 1000 * int(r["stfips"]) + int(r["ctfips"])
        uppm[fips] = float(r["Uppm"])

    seen = set()
    recs = []
    for r in srrs:
        if r.get("state") != "MN":
            continue
        idnum = r.get("idnum")
        if idnum in seen:
            continue
        seen.add(idnum)
        fips = int(r["stfips"]) * 1000 + int(r["cntyfips"])
        if fips not in uppm:
            continue
        county = r["county"].strip()
        activity = float(r["activity"])
        floor = int(r["floor"])
        recs.append({
            "county": county,
            "floor": floor,
            "log_radon": math.log(activity + 0.1),
            "log_uranium": math.log(uppm[fips]),
        })

    # county → 0-based index (出現順で安定・両側が同じ index を使う)
    counties = []
    idx = {}
    for rec in recs:
        c = rec["county"]
        if c not in idx:
            idx[c] = len(counties)
            counties.append(c)
        rec["county_idx"] = idx[c]

    os.makedirs(os.path.dirname(OUT), exist_ok=True)
    with open(OUT, "w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["county", "county_idx", "floor", "log_radon", "log_uranium"])
        for rec in recs:
            w.writerow([rec["county"], rec["county_idx"], rec["floor"],
                        f"{rec['log_radon']:.6f}", f"{rec['log_uranium']:.6f}"])

    print(f"wrote {os.path.relpath(OUT)}: {len(recs)} obs, {len(counties)} counties")


if __name__ == "__main__":
    main()
