"""
PyMC リファレンス: Phase 40 plate 記法の正解描画を取得する。

hanalyze (Phase 40) の `Hanalyze.Viz.ModelGraphDot` 出力と比較するため、
同じ 2 モデル (8-schools / nested multi-level) を PyMC で書き、
`pm.model_to_graphviz` で描画する。

実行:
    pip install pymc graphviz
    python3 bench/python/pymc_plate_reference.py

生成物:
    bench/python/pymc-output/8schools.gv          (graphviz source)
    bench/python/pymc-output/8schools.gv.png      (PNG)
    bench/python/pymc-output/multilevel.gv
    bench/python/pymc-output/multilevel.gv.png

PyMC の plate 描画ルール (重要):
    - plate 内の indexed RV (eta_0..eta_7) は **代表 1 ノード** に集約
    - plate 矩形に "school × 8" 等の dim ラベルを下に
    - 観測ノードは灰色塗り
    - hanalyze (Phase 40) はまだ「全 N 個列挙」 段階、 true collapse は
      Phase 40+ の改良タスク
"""
import os
import pathlib
import numpy as np

import pymc as pm

OUTDIR = pathlib.Path(__file__).parent / "pymc-output"
OUTDIR.mkdir(parents=True, exist_ok=True)


def model_eight_schools():
    """8-schools (Gelman et al. 2013) を coords + dims で記述。"""
    ys = np.array([28.0, 8.0, -3.0, 7.0, -1.0, 1.0, 18.0, 12.0])
    sigma_y = np.array([15.0, 10.0, 16.0, 11.0, 9.0, 11.0, 10.0, 18.0])

    with pm.Model(coords={"school": np.arange(8)}) as m:
        mu = pm.Normal("mu", 0.0, 5.0)
        tau = pm.HalfCauchy("tau", 5.0)
        eta = pm.Normal("eta", 0.0, 1.0, dims="school")
        # 非中心化: theta = mu + tau * eta (deterministic 派生量)
        theta = pm.Deterministic("theta", mu + tau * eta, dims="school")
        # 観測 (sigma は school ごとに既知)
        pm.Normal("y", mu=theta, sigma=sigma_y, observed=ys, dims="school")
    return m


def model_multilevel():
    """nested multi-level (school × student)。 plate が入れ子。"""
    J = 3  # schools
    K = 2  # students per school
    ys = np.arange(J * K, dtype=float).reshape(J, K)

    with pm.Model(coords={"school": np.arange(J), "student": np.arange(K)}) as m:
        mu = pm.Normal("mu", 0.0, 5.0)
        tau = pm.HalfNormal("tau", 1.0)
        theta = pm.Normal("theta", mu, tau, dims="school")
        # y は 2 次元 (school × student)、 broadcast で theta を mean に
        pm.Normal(
            "y",
            mu=theta[:, None],  # shape (J, 1) → broadcast to (J, K)
            sigma=1.0,
            observed=ys,
            dims=("school", "student"),
        )
    return m


def render_and_save(model, name):
    graph = pm.model_to_graphviz(model)
    # graphviz は .gv (source) と .png (rendered) の 2 つ書く
    dest = OUTDIR / name
    # render(): source を保存 + 指定形式に描画
    graph.render(filename=str(dest), format="png", cleanup=False)
    print(f"[OK] {name}: {dest}.gv (source) + {dest}.gv.png")


def main():
    print("=== 8-schools ===")
    m1 = model_eight_schools()
    render_and_save(m1, "8schools")

    print("\n=== nested multi-level (school × student) ===")
    m2 = model_multilevel()
    render_and_save(m2, "multilevel")

    print(f"\n出力ディレクトリ: {OUTDIR}")
    print("hanalyze (Phase 40) の DOT 出力と並べて比較:")
    print("  - hanalyze: demo-output/{8schools,multilevel}.dot")
    print("  - pymc    : bench/python/pymc-output/{8schools,multilevel}.gv")


if __name__ == "__main__":
    main()
