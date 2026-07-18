#!/usr/bin/env python3
"""Formula DSL の statsmodels 外部オラクル (Phase 46/15 §3.6 A18 検証点④)。

hanalyze の Formula DSL (fitLMF) が出す当てはめ値 ŷ と決定係数 R² を、
statsmodels.formula.api.ols の同一 formula 結果と突合する。

★検証原理: ŷ と R² は parameterization 不変 (contrast や基底の取り方で係数 β の
  表現は変わるが、 当てはめ値は不変)。 → 係数の付け方の違いに惑わされず正しさを判定できる。

★Haskell 側 ŷ/R² は formula_haskell_ref.json に事前生成済 (同一データ・同一 formula)。
  本スクリプトは statsmodels で同じ formula を fit し、 ref と tol 内一致を確認する。

実行 (要 venv + statsmodels):
    python3 -m venv bench/venv
    bench/venv/bin/pip install numpy pandas statsmodels
    bench/venv/bin/python bench/python/bench_formula.py
"""

import json
import os
import sys

import numpy as np
import pandas as pd
import statsmodels.formula.api as smf

# Haskell 側と完全に同一のデータ (test/Spec.hs rformulaSpec / formula_haskell_ref.json と一致)。
DATA = pd.DataFrame({
    "y": [10, 20, 30, 40, 50, 60, 12, 22, 34, 44, 52, 62],
    "g": ["A", "A", "B", "B", "C", "C", "A", "A", "B", "B", "C", "C"],
    "t": ["P", "Q", "P", "Q", "P", "Q", "P", "Q", "P", "Q", "P", "Q"],
    "x": [1, 2, 3, 4, 5, 6, 1, 2, 3, 4, 5, 6],
})

# ŷ/R² が parameterization 不変で statsmodels と確実に一致する formula。
# Phase 47 A2: contrast (C(g, Sum)) も ŷ 不変ゆえ statsmodels と一致する。
FORMULAS = [
    "y ~ x",
    "y ~ C(g) * C(t)",
    "y ~ C(g) + C(g):x",
    "y ~ x + I(x**2)",
    "y ~ C(g, Sum)",
    "y ~ C(g, Sum) + C(g, Sum):x",
]

# Phase 47 A3 WLS / A4 NLS 用データ (Haskell BenchFormulaRef.hs と完全一致)。
WLS_DATA = pd.DataFrame({
    "y": [2.1, 3.9, 6.2, 7.8, 10.1, 12.2, 13.8, 16.1],
    "x": [1, 2, 3, 4, 5, 6, 7, 8],
    "w": [1, 1, 2, 2, 3, 3, 4, 4],
})

NLS_X = np.array([0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5], dtype=float)
NLS_Y = 3.0 * np.exp(-0.5 * NLS_X)   # a=3, b=0.5 (ノイズなし)

# Phase 48: 混合効果 (random intercept + slope) 用データ (BenchFormulaRef.hs と完全一致)。
MIXED_DATA = pd.DataFrame({
    "y": [3.01, 6.49, 10.01, 13.49, 17.00,
          1.01, 3.49,  6.01,  8.49, 11.00,
          2.51, 5.19,  7.91, 10.59, 13.30,
          1.51, 4.79,  8.11, 11.39, 14.70],
    "x": [0, 1, 2, 3, 4] * 4,
    "g": sum([[k] * 5 for k in ["A", "B", "C", "D"]], []),
})

TOL = 1e-7


def main() -> int:
    here = os.path.dirname(os.path.abspath(__file__))
    ref_path = os.path.join(here, "formula_haskell_ref.json")
    with open(ref_path) as fh:
        ref = json.load(fh)

    all_ok = True
    for formula in FORMULAS:
        res = smf.ols(formula, data=DATA).fit()
        sm_yhat = np.asarray(res.fittedvalues, dtype=float)
        sm_r2 = float(res.rsquared)

        hs = ref.get(formula)
        if hs is None:
            print(f"[SKIP] {formula}: Haskell 参照に無し")
            all_ok = False
            continue
        hs_yhat = np.asarray(hs["yhat"], dtype=float)
        hs_r2 = float(hs["r2"])

        yhat_ok = sm_yhat.shape == hs_yhat.shape and np.allclose(sm_yhat, hs_yhat, atol=TOL)
        r2_ok = abs(sm_r2 - hs_r2) < TOL
        status = "PASS" if (yhat_ok and r2_ok) else "FAIL"
        if not (yhat_ok and r2_ok):
            all_ok = False
        print(f"[{status}] {formula:30s}  ŷ_match={yhat_ok}  "
              f"R²(sm)={sm_r2:.10f}  R²(hs)={hs_r2:.10f}")

    # --- Phase 47 A3: WLS 係数を smf.wls と突合 (係数は同 parameterization ゆえ直接比較可) ---
    wls = ref.get("__wls__")
    if wls is not None:
        sm_wls = smf.wls("y ~ x", data=WLS_DATA, weights=WLS_DATA["w"]).fit()
        sm_coef = np.asarray(sm_wls.params, dtype=float)        # [Intercept, x]
        hs_coef = np.asarray(wls["coef"], dtype=float)
        ok = sm_coef.shape == hs_coef.shape and np.allclose(sm_coef, hs_coef, atol=TOL)
        all_ok = all_ok and ok
        print(f"[{'PASS' if ok else 'FAIL'}] {'WLS (smf.wls)':30s}  "
              f"coef(sm)={sm_coef.round(8).tolist()}  coef(hs)={hs_coef.round(8).tolist()}")

    # --- Phase 47 A4: NLS パラメータを scipy.curve_fit と突合 ---
    nls = ref.get("__nls__")
    if nls is not None:
        from scipy.optimize import curve_fit
        popt, _ = curve_fit(lambda x, a, b: a * np.exp(-b * x), NLS_X, NLS_Y, p0=[1.0, 1.0])
        sm_ab = np.array([popt[0], popt[1]], dtype=float)
        hs_ab = np.array([nls["a"], nls["b"]], dtype=float)
        ok = np.allclose(sm_ab, hs_ab, atol=1e-4)              # NLS は収束 tol
        all_ok = all_ok and ok
        print(f"[{'PASS' if ok else 'FAIL'}] {'NLS (curve_fit)':30s}  "
              f"(a,b)(scipy)={sm_ab.round(6).tolist()}  (a,b)(hs)={hs_ab.round(6).tolist()}")

    # --- Phase 48: mixed-effects (random intercept + slope) を mixedlm と突合 ---
    mixed = ref.get("__mixed__")
    if mixed is not None:
        md = smf.mixedlm("y ~ x", MIXED_DATA, groups=MIXED_DATA["g"], re_formula="~x")
        mr = md.fit(reml=False, method="lbfgs")           # ML (Haskell EM と同じ目的関数)
        sm_beta = np.asarray(mr.fe_params, dtype=float)    # [Intercept, x]
        hs_beta = np.array(mixed["beta"], dtype=float)
        # statsmodels cov_re は絶対データ単位 (re_formula 列順 = [Group(=intercept), x])
        sm_cov = np.asarray(mr.cov_re, dtype=float).reshape(-1)
        hs_cov = np.array(mixed["cov_re"], dtype=float)
        sm_s2 = float(mr.scale)
        hs_s2 = float(mixed["sigma2"])
        beta_ok = np.allclose(sm_beta, hs_beta, atol=1e-3)     # 固定効果は密に一致
        cov_ok = np.allclose(sm_cov, hs_cov, atol=5e-3)        # G は ML 最適化 tol
        s2_ok = abs(sm_s2 - hs_s2) < 5e-3
        ok = beta_ok and cov_ok and s2_ok
        all_ok = all_ok and ok
        print(f"[{'PASS' if ok else 'FAIL'}] {'Mixed LME (smf.mixedlm ML)':30s}  "
              f"β(sm)={sm_beta.round(4).tolist()} β(hs)={hs_beta.round(4).tolist()}")
        print(f"       {'':30s}  "
              f"G(sm)={sm_cov.round(4).tolist()} G(hs)={hs_cov.round(4).tolist()} "
              f"σ²(sm)={round(sm_s2,5)} σ²(hs)={round(hs_s2,5)}")

    print()
    print("RESULT:", "ALL PASS" if all_ok else "SOME FAIL")
    return 0 if all_ok else 1


if __name__ == "__main__":
    sys.exit(main())
