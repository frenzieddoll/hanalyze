"""Phase 89 posteriordb 横断ベンチマーク: PyMC 側「フルダッシュボード」合成。

Haskell 側 (hgg `dashboardFullOf`) と対にする 1 枚合成図を作る。
`dashboardFullOf` の構成 = 健全性 2x2 (dag/forest/ppc/energy) + param ごと
[事後分布|trace] の行。 新世代 arviz-plots (`PlotCollection`) は各図が独立
Figure を返す設計 (dashboard 合成のネイティブ API は無い) ため、各図を
matplotlib Figure として取得し `imshow` で 1 枚の合成画像に貼り直す。

全モデル (Phase 89 の 8 本) で使い回す前提の共有ヘルパ。
"""
from __future__ import annotations

import io
from pathlib import Path

import matplotlib
import matplotlib.pyplot as plt
import numpy as np


def _fig_to_array(fig) -> np.ndarray:
    """matplotlib Figure を RGBA array へ (imshow 合成用)。"""
    buf = io.BytesIO()
    fig.savefig(buf, format="png", dpi=150, bbox_inches="tight")
    buf.seek(0)
    return plt.imread(buf)


def _graphviz_to_array(digraph) -> np.ndarray:
    """graphviz Digraph → PNG → array。"""
    png_bytes = digraph.pipe(format="png")
    buf = io.BytesIO(png_bytes)
    return plt.imread(buf)


def make_pymc_dashboard(model, idata, obs_name: str, out_path: Path) -> None:
    """Haskell `dashboardFullOf` に対応する PyMC 側の合成ダッシュボードを
    1 枚の SVG として保存する。

    構成: 上段 2x2 (モデル構造 DAG / forest / PPC / energy) +
    下段 (`az.plot_trace_dist` = param ごとの [事後分布|trace] を 1 回で
    まとめて取得)。
    """
    import arviz as az
    import pymc as pm

    matplotlib.use("Agg")

    # --- 上段パネルをそれぞれ個別に生成 → array 化 -------------------------
    g = pm.model_to_graphviz(model)
    g.format = "png"
    dag_arr = _graphviz_to_array(g)

    forest_fig = az.plot_forest(idata).viz["figure"].item()
    forest_arr = _fig_to_array(forest_fig)
    plt.close(forest_fig)

    # potential のみで尤度を構成するモデル (bones 等) は observed RV が無く
    # idata に posterior_predictive/observed_data group が存在しないため
    # plot_ppc_dist が KeyError で落ちる。 その場合は PPC パネルを空欄にする
    # (Haskell 側 dashboardOf の「PPCパネルが空」と同じ扱い・14-hmm-example/
    # 16-lda/20-bones と同型)。
    try:
        ppc_fig = az.plot_ppc_dist(idata).viz["figure"].item()
        ppc_arr = _fig_to_array(ppc_fig)
        plt.close(ppc_fig)
    except KeyError:
        ppc_arr = np.ones((10, 10, 4))

    energy_fig = az.plot_energy(idata).viz["figure"].item()
    energy_arr = _fig_to_array(energy_fig)
    plt.close(energy_fig)

    # --- 下段: param ごと [事後分布|trace] (plot_trace_dist が 1 回で描く) ---
    trace_dist_fig = az.plot_trace_dist(idata).viz["figure"].item()
    trace_dist_arr = _fig_to_array(trace_dist_fig)
    plt.close(trace_dist_fig)

    # --- 合成 (上段 2x2 + 下段 1 枚を縦積み) ---------------------------------
    # 下段の縦横比に合わせ、上段の高さ配分を程よく取る (下段は param 数に
    # 応じて縦に伸びる想定・GridSpec の height_ratios で吸収)。
    h_top = 4.0
    h_bot = trace_dist_arr.shape[0] / trace_dist_arr.shape[1] * 12.8
    fig = plt.figure(figsize=(13, h_top + h_bot))
    gs = fig.add_gridspec(3, 2, height_ratios=[h_top / 2, h_top / 2, h_bot])

    for ax, arr, title in [
        (fig.add_subplot(gs[0, 0]), dag_arr, "model structure (DAG)"),
        (fig.add_subplot(gs[0, 1]), forest_arr, "forest"),
        (fig.add_subplot(gs[1, 0]), ppc_arr, "posterior predictive check"),
        (fig.add_subplot(gs[1, 1]), energy_arr, "energy (BFMI)"),
    ]:
        ax.imshow(arr)
        ax.set_title(title, fontsize=10)
        ax.axis("off")

    ax_bot = fig.add_subplot(gs[2, :])
    ax_bot.imshow(trace_dist_arr)
    ax_bot.axis("off")

    fig.suptitle(f"PyMC dashboard (obs = {obs_name})", fontsize=12)
    fig.savefig(out_path, format="svg", bbox_inches="tight")
    plt.close(fig)
    print(f"wrote {out_path}")
