# vecIR バルク FP の SIMD/FFI kernel 化 — 残レバーの記録 (不実施)

2026-07-18 確定: **FFI はやらない方針** (user 判断)。per-eval 残ギャップの
改善レバーとして検討した SIMD/FFI kernel 化は実装せず、本 note に設計と
実測根拠を記録するに留める (将来方針が変わった場合の再開点)。

## 実測済みの現状 (2026-07-17・05-mh・このマシン・逐次単独)

- 05-mh (ZIB 尤度・M=385) の vecIR 経路 per-eval = **53.9µs/eval** vs
  nutpie+numba ≈ **25.8µs/eval** → **~2.1×** の単価差。
  root: `experiments/phase96-mh-reconfirm/` (prof + 統一基準実測)。
- prof 内訳: `$wforwardArenaInto` 43.8% + `$wgradVecIRGoWith` 21.7% +
  `gradValPlan` 17.0% = **arena/vecIR 評価 kernel 3 本で 82.5%**。
- 実体 (`src/Hanalyze/Model/HBM/IR.hs` の `forwardArenaInto` ~L2391):
  vecIR 命令 (VIUn/VIBin/VISum/VIAxpy…) のインタプリタで、命令ごとに
  `f = sBinF op` で引いた関数を **要素ごとに間接呼び出し**する Haskell
  ループ。GHC NCG は SIMD を吐かないため実行はスカラ 1 要素ずつ。
  nutpie 側は numba (LLVM JIT) がモデル全体を融合 native コード化して
  AVX で回している (こちらは構造からの解釈・未計測)。

## やるとしたら何をするか (PoC 設計・不実施)

インタプリタ・IR・NUTS は Haskell のまま、**要素ループ数種だけ C に出す**
外科的 FFI:

1. 命令ミックス実測 (どの命令種 × ベクトル長が評価回数を占めるか) で
   C 化対象を確定。
2. `cbits/arena_kernels.c` に `alc_vbin_mul(dst,a,b,n)` / `alc_vsum(x,n)` /
   融合形 `alc_zib_logp(...)` 等。cabal `c-sources:` + `-O3` (gcc/clang の
   自動ベクトル化。初手は intrinsics 手書き不要)。
3. arena は Storable MVector (= pinned) なので `unsafeWith` → `Ptr Double`、
   ST 内から `unsafeIOToST` + `ccall unsafe`。**短ベクトル命令は Haskell
   ループのまま** (FFI 往復コスト回避の長さ閾値)。
4. gate: 同演算順なら bit 一致狙い。SIMD 縮約 (VISum) は加算順が変わるので
   posterior 統計一致 gate に落とす。`-ffast-math`/`-march=native` は精度/
   可搬性の論点があり既定不使用。

注意点:

- **本丸は超越関数**: ZIB 尤度は `exp`/`log1p`/`log` の 385 要素バルク。
  加減乗除は自動ベクトル化されるが libm 呼び出しはスカラのままなので、
  効かせるには libmvec / SLEEF 等のベクトル数学ライブラリ依存が要る。
- 逆向き sweep (`gradVecIRGoWith`) も同型ループ = 同じ手を展開可能。
- hmatrix→BLAS/LAPACK・hasktorch→libtorch と同じ「構造は Haskell・
  バルク FP は FFI」の標準分業であり、技術的障害は無い。やらないのは
  依存と保守面の方針判断。

## FFI 無しで残っている純 Haskell レバー (同じく不実施・効果未計測)

1. **間接呼び出し除去**: `case op of Add -> addLoop; Mul -> mulLoop; …` と
   命令種ごとに単相の具象ループへ特殊化。数割級の改善はあり得る。
2. **`-fllvm`**: NCG より良いコードを吐く可能性。ただし ST/MVector 逐次
   ループの自動ベクトル化は期待薄 (通説)。
3. GHC SIMD primops (`DoubleX4#` 等) は GHC 9.6.7 + 超越関数非対応で
   実用性低・不採用。

いずれも Haskell の限界ではなく未着手のレバー。ただし SIMD 幅 (4-8 要素/
命令) と超越関数ベクトル化は 1-3 では届かず、nutpie 級の per-eval に
並ぶには FFI か生成コード (モデル融合 JIT) が必要、が 2026-07 時点の結論。
