"""hmm: hanalyze の post-warmup draw (CSV) を arviz の rank-normalized
ess_bulk/ess_tail で評価する (Phase 92 ess/draw 効率調査)。

Common.hs の summarize は chain 0 のみ + Geyer IMSE (tau 下限 1 クランプで
n 頭打ち) のため、nutpie 側 (az.summary の ess_bulk・4 chain) と指標が
非対称だった。ここで両者を同一指標に揃える。
"""
import sys
from pathlib import Path

import arviz as az
import numpy as np
import pandas as pd

csv_path = Path(__file__).parent / "hmm_draws_postwarmup.csv"


def main():
    df = pd.read_csv(csv_path)
    params = [c for c in df.columns if c not in ("chain", "draw")]
    n_chain = df["chain"].nunique()
    n_draw = df["draw"].nunique()
    data = {
        p: df.pivot(index="chain", columns="draw", values=p).to_numpy()
        for p in params
    }
    # この venv の arviz は from_dict の署名が旧版と異なるため、
    # (chain, draw) 形状の dict を直接 summary へ渡す (自動変換される)。
    s = az.summary(data)
    print(f"chains={n_chain} draws={n_draw} (total {n_chain*n_draw})")
    print(s.to_string())
    ess = float(s.loc["mu_1", "ess_bulk"])
    print(f"\ness_bulk(mu_1) = {ess:.1f}  ess/draw = {ess/(n_chain*n_draw):.4f}")


if __name__ == "__main__":
    main()
