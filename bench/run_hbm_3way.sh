#!/usr/bin/env bash
# HBM 3 系ベンチ統合ドライバ — Phase 84。
#
# hanalyze (haskell) / PyMC default (PyTensor) / PyMC numpyro backend の 3 系を
# **同一データ・同一 config・CPU 固定・単スレッド**で回し、 結果 CSV を
# bench/results/{haskell,python}/ に出して 3-way 集約表を表示する。
#
# 使い方 (リポジトリ root から):
#   bench/run_hbm_3way.sh            # 既定 suite (M1-M8 標準 grid)
#   bench/run_hbm_3way.sh glm        # M7-M9 のみ
#   bench/run_hbm_3way.sh <arg>      # BenchHBMScaling / bench_hbm_scaling.py の CLI 引数
#
# 前提: bench/venv (pymc/numpyro/jax/arviz 導入済) と cabal。
set -euo pipefail
cd "$(dirname "$0")/.."

SUITE_ARG="${1:-}"                       # "" | glm | m7-long ...
STEM_ARG="${SUITE_ARG:-hbm_scaling}"     # 集約器へ渡す stem 名 (下で正規化)
PY=bench/venv/bin/python

# --- 公平性: 単スレッド + CPU 固定 (確定事項 4) ---
# XLA の CPU スレッド制御フラグは jax 版で名前が変わり不安定なので使わない。
# 代わりに OS レベルで 1 コアに固定する (taskset・逐次1コア = cores=1 と同義)。
export OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 MKL_NUM_THREADS=1
export JAX_PLATFORMS=cpu                  # GPU 不使用 (確定事項 4)
export JAX_ENABLE_X64=1                   # PyMC/NUTS は f64 前提 (数値一致のため)
# taskset は /usr/sbin に居て PATH 外のことがあるのでフルパスで解決する。
TASKSET_BIN="$(command -v taskset || echo /usr/sbin/taskset)"
if [ -x "$TASKSET_BIN" ]; then PIN="$TASKSET_BIN -c 0"; else PIN=""; fi

echo "==[1/4]== hanalyze (haskell) — データ生成 + サンプリング"
# bench executable は flag benches ガード配下 (既定 off) ゆえ -f benches 必須。
$PIN cabal run -v0 -f benches bench-hbm-scaling -- ${SUITE_ARG}

echo "==[2/4]== PyMC (PyTensor default)"
BENCH_NUTS_SAMPLER=pymc $PIN $PY bench/python/bench_hbm_scaling.py ${SUITE_ARG}

echo "==[3/4]== PyMC (NumPyro backend・CPU)"
BENCH_NUTS_SAMPLER=numpyro $PIN $PY bench/python/bench_hbm_scaling.py ${SUITE_ARG}

echo "==[4/4]== 3-way 集約"
# stem 名: 無引数=hbm_scaling / glm=hbm_scaling_glm / *-long=hbm_scaling_*_long
case "${SUITE_ARG}" in
  "")        STEM=hbm_scaling ;;
  glm)       STEM=hbm_scaling_glm ;;
  *-long)    STEM="hbm_scaling_${SUITE_ARG/-long/_long}" ;;
  *)         STEM="hbm_scaling_${SUITE_ARG}" ;;
esac
python3 bench/python/agg_hbm_3way.py "${STEM}"
