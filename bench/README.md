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

# 5. Aggregate
bench/venv/bin/python bench/aggregate.py > bench/results/summary.md
```

## Fairness rules

- `OPENBLAS_NUM_THREADS=1` and `OMP_NUM_THREADS=1` for both sides.
- Same input data (CSV files in `bench/data/`) read by both sides.
- For optimization, identical seeds, budget (max evals), and starting points.
- Reported numbers: Haskell uses `criterion` (median + 95 % CI); Python uses
  `pyperf` (geometric mean of 5 runs × 5 inner loops).

## Status

| Phase | Description | Status |
|---|---|---|
| B0 | Infra (data gen + BenchUtil + aggregate) | done |
| B1 | Classical regression (LM/GLM/GLMM/Ridge/Lasso/EN) | done |
| B2 | Kernel & GP (KR/NW/RFF/GP/GPRobust) | done |
| B3 | Single-objective optimization | done |
| B4 | Multi-objective optimization | done |
| B5 | Bayesian optimization + final report | done |

See [`results/REPORT.md`](results/REPORT.md) for the consolidated
narrative and `results/summary.md` for the auto-generated table.
