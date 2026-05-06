# hanalyze benchmarks

Speed and accuracy comparison of `hanalyze` against established Python
libraries (`scikit-learn`, `statsmodels`, `cma`, `pyswarms`,
`scikit-optimize`, `pymoo`, `numpyro`, ...).

## Layout

```
bench/
├── README.md            (this file)
├── requirements.txt     Python dependencies
├── venv/                Python virtualenv (gitignored)
├── data/                Shared CSV inputs (fixed seed; both sides read these)
│   └── gen_<scenario>.csv
├── haskell/             Haskell-side bench helpers (data writer etc.)
├── python/              Python comparison scripts
└── results/             Per-bench CSV outputs + final HTML/PNG
    ├── haskell/
    └── python/
```

## How to run

```bash
# 1. Set up Python env (one-off)
python3 -m venv bench/venv
bench/venv/bin/pip install -r bench/requirements.txt

# 2. Generate shared data (fixed-seed, deterministic; not committed to
#    git because lm_n100000_p100.csv > 100 MB exceeds GitHub limits).
cabal run bench-data-gen

# 3. Run Haskell side
cabal bench bench-regression bench-kernel bench-optim bench-mo bench-bo

# 4. Run Python side
OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 \
  bench/venv/bin/python bench/python/bench_regression.py
# (similar for the other phases)

```

## Fairness rules

- `OPENBLAS_NUM_THREADS=1` and `OMP_NUM_THREADS=1` for both sides.
- Same input data (CSV files in `bench/data/`) read by both sides.
- For optimization, identical seeds, budget (max evals), and starting points.
- Reported numbers: Haskell `bench-regression` and `bench-kernel` use
  `tasty-bench` (adaptive iteration, 5% relative stdev target). Other
  Haskell benches use a 5-iteration `timeit` median. Python uses
  `pyperf` (geometric mean of 5 runs × 5 inner loops).

## Status (Phase 1〜13 perf 改善まで完了、2026-05-06)

すべての suite で実装と計測が完了。最新結果は
[`results/SUMMARY.md`](results/SUMMARY.md) を参照。

| Suite | Bench script (Haskell / Python) | tasty-bench 化 |
|---|---|---|
| regression | `bench-regression` / `bench_regression.py` | ✅ |
| kernel | `bench-kernel` / `bench_kernel.py` | ✅ |
| optim | `bench-optim` / `bench_optim.py` | (旧 `timeit` のまま) |
| mo | `bench-mo` / `bench_mo.py` | (旧 `timeit` のまま) |
| bo | `bench-bo` / `bench_bo.py` | (旧 `timeit` のまま) |

詳細レポート / 残課題 / プロファイル分析は:

- [`results/SUMMARY.md`](results/SUMMARY.md) — 最新の Python vs Haskell 比較
- [`results/OPEN_ISSUES.md`](results/OPEN_ISSUES.md) — 残ギャップと FFI 領域
- [`results/perf_profile_findings.md`](results/perf_profile_findings.md) — Phase 11 プロファイル取得結果
- [`results/NSGA_INVESTIGATION.md`](results/NSGA_INVESTIGATION.md) — NSGA-II 調査記録 (歴史)
- [`results/BO_INVESTIGATION.md`](results/BO_INVESTIGATION.md) — BO 調査記録 (歴史)
