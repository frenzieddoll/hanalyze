{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-full-laziness -fno-cse #-}
-- | Phase 85.3a: vecIR 融合方式の feasibility spike (計測先行・推測するな計測せよ)。
--
-- 85.1 で radon per-eval ~103µs の ~86% が forward+backward の解釈実行と確定した。
-- 本実装 (IR 融合) に入る前に、 synthetic な elementwise 連鎖 (n=919・二項 7 op =
-- radon の VIBin 48 本×平均 390 セルと同じ per-cell 構造) で 4 方式を実測し、
-- 融合の利得上限と採用方式を決める:
--
--   [A] 現行 forwardArena 相当: 命令ごと全ベクトル走査 + @sBinF op@ の
--       unknown-call ディスパッチ (IR.hs:1367 の実形)
--   [B] A + op 特化ループ: 命令ごと走査は同じだが case op で直接演算
--       (backward VIBin と同形) — unknown-call/boxing の寄与を分離
--   [C] 完全融合 1-pass (手書き): 全連鎖を 1 ループで registers 評価 =
--       compile-time codegen の理論天井 (解釈では到達不能)
--   [D] 要素ごと解釈融合: 要素 j ごとに opcode 列を内側ループで dispatch =
--       「fused interpreter」 方式の現実値 (arena traffic ゼロだが
--       per-node dispatch が要素ごとに走る)
--
-- backward (勾配) 側は [A'] 現行 gradVecIRGo 相当 (forward arena + adj arena +
-- op 特化・現行 backward は既に特化済) と [C'] 完全融合 1-pass (forward 再計算 +
-- registers 逆伝播) の 2 点で床と天井を測る。
--
-- ★この spike は HBM 本体を一切いじらない独立実験。 勾配は全方式で突合し
--   max|Δ| を表示する (正しさの担保)。
module Main where

import           Control.Monad                    (forM_, when)
import           Control.Monad.ST                 (ST, runST)
import qualified Data.Vector                      as BV
import qualified Data.Vector.Storable             as VS
import qualified Data.Vector.Storable.Mutable     as VSM
import qualified Data.Vector.Unboxed              as VU
import           Text.Printf                      (printf)

import           Hanalyze.Model.HBM.IR            (SBin (..), sBinF)

import           BenchUtil                        (timeitTastyIO)

-- ---------------------------------------------------------------------------
-- 設定: acc_0 = x0、 acc_t = acc_{t-1} ⊕_t x_t (t=1..7)、 obj = Σ_j acc_7[j]
-- ---------------------------------------------------------------------------

nObs :: Int
nObs = 919

nOps :: Int
nOps = 7

ops :: [SBin]
ops = [SAddO, SMulO, SSubO, SDivO, SAddO, SMulO, SSubO]

-- | 入力 8 本 (値域 [0.5, 1.5] = div 破綻なし・決定的)。
mkInputs :: Int -> BV.Vector (VS.Vector Double)
mkInputs salt = BV.fromList
  [ VS.generate nObs $ \j ->
      0.5 + fromIntegral ((j * 31 + t * 17 + salt) `mod` 1000) / 1000.0
  | t <- [0 .. nOps] ]

-- ---------------------------------------------------------------------------
-- forward 4 方式
-- ---------------------------------------------------------------------------

-- | [A] 現行 forwardArena 相当: slot ごと全走査 + unknown-call。
fwA :: BV.Vector (VS.Vector Double) -> Double
fwA xs = runST $ do
  ar <- VSM.unsafeNew ((nOps + 1) * nObs)
  let x0 = xs BV.! 0
      copy0 !j | j >= nObs = pure ()
               | otherwise = do
                   VSM.unsafeWrite ar j (x0 `VS.unsafeIndex` j)
                   copy0 (j + 1)
  copy0 0
  forM_ [1 .. nOps] $ \t -> do
    let f  = sBinF (ops !! (t - 1))
        xt = xs BV.! t
        oPrev = (t - 1) * nObs
        oCur  = t * nObs
        go !j | j >= nObs = pure ()
              | otherwise = do
                  a <- VSM.unsafeRead ar (oPrev + j)
                  VSM.unsafeWrite ar (oCur + j) (f a (xt `VS.unsafeIndex` j))
                  go (j + 1)
    go 0
  let oL = nOps * nObs
      sumGo !acc !j | j >= nObs = pure acc
                    | otherwise = do
                        v <- VSM.unsafeRead ar (oL + j)
                        sumGo (acc + v) (j + 1)
  sumGo 0 0

-- | [B] A + op 特化ループ (case op で直接演算・backward VIBin と同形)。
fwB :: BV.Vector (VS.Vector Double) -> Double
fwB xs = runST $ do
  ar <- VSM.unsafeNew ((nOps + 1) * nObs)
  let x0 = xs BV.! 0
      copy0 !j | j >= nObs = pure ()
               | otherwise = do
                   VSM.unsafeWrite ar j (x0 `VS.unsafeIndex` j)
                   copy0 (j + 1)
  copy0 0
  forM_ [1 .. nOps] $ \t -> do
    let xt = xs BV.! t
        oPrev = (t - 1) * nObs
        oCur  = t * nObs
        loopWith f =
          let go !j | j >= nObs = pure ()
                    | otherwise = do
                        a <- VSM.unsafeRead ar (oPrev + j)
                        VSM.unsafeWrite ar (oCur + j)
                          (f a (xt `VS.unsafeIndex` j))
                        go (j + 1)
          in go 0
    case ops !! (t - 1) of
      SAddO -> loopWith (+)
      SSubO -> loopWith (-)
      SMulO -> loopWith (*)
      SDivO -> loopWith (/)
  let oL = nOps * nObs
      sumGo !acc !j | j >= nObs = pure acc
                    | otherwise = do
                        v <- VSM.unsafeRead ar (oL + j)
                        sumGo (acc + v) (j + 1)
  sumGo 0 0

-- | [C] 完全融合 1-pass (手書き・registers のみ) = codegen の理論天井。
fwC :: BV.Vector (VS.Vector Double) -> Double
fwC xs =
  let x0 = xs BV.! 0
      x1 = xs BV.! 1
      x2 = xs BV.! 2
      x3 = xs BV.! 3
      x4 = xs BV.! 4
      x5 = xs BV.! 5
      x6 = xs BV.! 6
      x7 = xs BV.! 7
      go !acc !j
        | j >= nObs = acc
        | otherwise =
            let v1 = x0 `VS.unsafeIndex` j + x1 `VS.unsafeIndex` j
                v2 = v1 * x2 `VS.unsafeIndex` j
                v3 = v2 - x3 `VS.unsafeIndex` j
                v4 = v3 / x4 `VS.unsafeIndex` j
                v5 = v4 + x5 `VS.unsafeIndex` j
                v6 = v5 * x6 `VS.unsafeIndex` j
                v7 = v6 - x7 `VS.unsafeIndex` j
            in go (acc + v7) (j + 1)
  in go 0 0

-- | [D] 要素ごと解釈融合: opcode 列を要素ごとに内側 dispatch (arena なし)。
fwD :: VU.Vector Int -> BV.Vector (VS.Vector Double) -> Double
fwD opCodes xs =
  let go !acc !j
        | j >= nObs = acc
        | otherwise =
            let inner !v !t
                  | t > nOps = v
                  | otherwise =
                      let xtj = (xs BV.! t) `VS.unsafeIndex` j
                          v'  = case opCodes `VU.unsafeIndex` (t - 1) of
                                  0 -> v + xtj
                                  1 -> v - xtj
                                  2 -> v * xtj
                                  _ -> v / xtj
                      in inner v' (t + 1)
                v0 = (xs BV.! 0) `VS.unsafeIndex` j
            in go (acc + inner v0 1) (j + 1)
  in go 0 0

opCode :: SBin -> Int
opCode SAddO = 0
opCode SSubO = 1
opCode SMulO = 2
opCode SDivO = 3

-- ---------------------------------------------------------------------------
-- backward 2 方式 (勾配 = ∂obj/∂x_t 全要素・fw+bw 込みの per-eval)
-- ---------------------------------------------------------------------------

-- | [A'] 現行 gradVecIRGo 相当: forward arena + adj arena (replicate 0) +
--   op 特化 backward。 勾配は gxs ((nOps+1)*nObs) へ加算。
gradA :: BV.Vector (VS.Vector Double) -> VS.Vector Double
gradA xs = runST $ do
  -- forward (B と同じ特化形: 現行 backward は特化済なので forward も B 形で公平に)
  ar <- VSM.unsafeNew ((nOps + 1) * nObs)
  let x0 = xs BV.! 0
      copy0 !j | j >= nObs = pure ()
               | otherwise = do
                   VSM.unsafeWrite ar j (x0 `VS.unsafeIndex` j)
                   copy0 (j + 1)
  copy0 0
  forM_ [1 .. nOps] $ \t -> do
    let xt = xs BV.! t
        oPrev = (t - 1) * nObs
        oCur  = t * nObs
        loopWith f =
          let go !j | j >= nObs = pure ()
                    | otherwise = do
                        a <- VSM.unsafeRead ar (oPrev + j)
                        VSM.unsafeWrite ar (oCur + j)
                          (f a (xt `VS.unsafeIndex` j))
                        go (j + 1)
          in go 0
    case ops !! (t - 1) of
      SAddO -> loopWith (+)
      SSubO -> loopWith (-)
      SMulO -> loopWith (*)
      SDivO -> loopWith (/)
  -- backward
  adj <- VSM.replicate ((nOps + 1) * nObs) 0
  gxs <- VSM.replicate ((nOps + 1) * nObs) 0
  -- obj = Σ acc_7 → adj_7 = 1
  let oL = nOps * nObs
      init1 !j | j >= nObs = pure ()
               | otherwise = VSM.unsafeWrite adj (oL + j) 1 >> init1 (j + 1)
  init1 0
  forM_ [nOps, nOps - 1 .. 1] $ \t -> do
    let xt = xs BV.! t
        oPrev = (t - 1) * nObs
        oCur  = t * nObs
        gOff  = t * nObs
    case ops !! (t - 1) of
      SAddO ->
        let go !j | j >= nObs = pure ()
                  | otherwise = do
                      a <- VSM.unsafeRead adj (oCur + j)
                      VSM.unsafeModify adj (+ a) (oPrev + j)
                      VSM.unsafeModify gxs (+ a) (gOff + j)
                      go (j + 1)
        in go 0
      SSubO ->
        let go !j | j >= nObs = pure ()
                  | otherwise = do
                      a <- VSM.unsafeRead adj (oCur + j)
                      VSM.unsafeModify adj (+ a) (oPrev + j)
                      VSM.unsafeModify gxs (subtract a) (gOff + j)
                      go (j + 1)
        in go 0
      SMulO ->
        let go !j | j >= nObs = pure ()
                  | otherwise = do
                      a  <- VSM.unsafeRead adj (oCur + j)
                      vp <- VSM.unsafeRead ar (oPrev + j)
                      VSM.unsafeModify adj (+ (a * xt `VS.unsafeIndex` j)) (oPrev + j)
                      VSM.unsafeModify gxs (+ (a * vp)) (gOff + j)
                      go (j + 1)
        in go 0
      SDivO ->
        let go !j | j >= nObs = pure ()
                  | otherwise = do
                      a  <- VSM.unsafeRead adj (oCur + j)
                      vp <- VSM.unsafeRead ar (oPrev + j)
                      let x = xt `VS.unsafeIndex` j
                      VSM.unsafeModify adj (+ (a / x)) (oPrev + j)
                      VSM.unsafeModify gxs (+ (negate (a * vp / (x * x)))) (gOff + j)
                      go (j + 1)
        in go 0
  -- acc_0 = x0 → gx_0 = adj_0
  let fin !j | j >= nObs = pure ()
             | otherwise = do
                 a <- VSM.unsafeRead adj j
                 VSM.unsafeModify gxs (+ a) j
                 fin (j + 1)
  fin 0
  VS.unsafeFreeze gxs

-- | [C'] 完全融合 1-pass: 要素ごとに forward re-計算 + registers 逆伝播。
gradC :: BV.Vector (VS.Vector Double) -> VS.Vector Double
gradC xs = runST $ do
  gxs <- VSM.replicate ((nOps + 1) * nObs) 0
  let x0 = xs BV.! 0
      x1 = xs BV.! 1
      x2 = xs BV.! 2
      x3 = xs BV.! 3
      x4 = xs BV.! 4
      x5 = xs BV.! 5
      x6 = xs BV.! 6
      x7 = xs BV.! 7
      go !j
        | j >= nObs = pure ()
        | otherwise = do
            let i0 = x0 `VS.unsafeIndex` j
                i1 = x1 `VS.unsafeIndex` j
                i2 = x2 `VS.unsafeIndex` j
                i3 = x3 `VS.unsafeIndex` j
                i4 = x4 `VS.unsafeIndex` j
                i5 = x5 `VS.unsafeIndex` j
                i6 = x6 `VS.unsafeIndex` j
                i7 = x7 `VS.unsafeIndex` j
                v1 = i0 + i1
                v2 = v1 * i2
                v3 = v2 - i3
                v4 = v3 / i4
                v5 = v4 + i5
                v6 = v5 * i6
                -- v7 = v6 - i7 (obj 側は Σ ゆえ adj_7 = 1)
                a7 = 1 :: Double
                a6 = a7
                a5 = a6 * i6
                a4 = a5
                a3 = a4 / i4
                a2 = a3
                a1 = a2 * i2
                a0 = a1
            VSM.unsafeWrite gxs (7 * nObs + j) (negate a7)
            VSM.unsafeWrite gxs (6 * nObs + j) (a6 * v5)
            VSM.unsafeWrite gxs (5 * nObs + j) a5
            VSM.unsafeWrite gxs (4 * nObs + j) (negate (a4 * v3 / (i4 * i4)))
            VSM.unsafeWrite gxs (3 * nObs + j) (negate a3)
            VSM.unsafeWrite gxs (2 * nObs + j) (a2 * v1)
            VSM.unsafeWrite gxs (1 * nObs + j) a1
            VSM.unsafeWrite gxs (0 * nObs + j) a0
            go (j + 1)
  go 0
  VS.unsafeFreeze gxs

-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "=== Phase 85.3a: vecIR 融合方式 spike (n=919・二項 7 op 連鎖) ==="
  printf "セル数 (bin cells) = %d (radon VIBin 18744 セルの縮小相似形)\n\n"
    (nOps * nObs)
  let opCodes = VU.fromList (map opCode ops)

  -- 正しさ突合 (A' vs C')
  let xs0 = mkInputs 0
      gA  = gradA xs0
      gC  = gradC xs0
      dMax = VS.maximum (VS.zipWith (\a b -> abs (a - b)) gA gC)
      vA = fwA xs0
      vC = fwC xs0
      vD = fwD opCodes xs0
  printf "突合: fwA=%.6f fwC=%.6f fwD=%.6f  grad max|Δ|=%.3e\n\n" vA vC vD dMax
  when (dMax > 1e-12) $ error "gradA と gradC が不一致"

  -- forward 4 方式
  putStrLn "--- forward per-eval (µs・ns/セル) ---"
  let cells = fromIntegral (nOps * nObs) :: Double
      report tag tMs = printf "  %-34s %8.2f µs  %6.2f ns/セル\n"
        (tag :: String) (tMs * 1000) (tMs * 1e6 / cells)
  (tA, _) <- timeitTastyIO id (\i -> pure $! fwA (mkInputsCached i))
  report "[A] 現行 (arena+unknown-call)" tA
  (tB, _) <- timeitTastyIO id (\i -> pure $! fwB (mkInputsCached i))
  report "[B] arena+op特化ループ" tB
  (tC, _) <- timeitTastyIO id (\i -> pure $! fwC (mkInputsCached i))
  report "[C] 完全融合1-pass (天井)" tC
  (tD, _) <- timeitTastyIO id (\i -> pure $! fwD opCodes (mkInputsCached i))
  report "[D] 要素ごと解釈融合" tD

  -- backward (fw+bw)
  putStrLn "\n--- gradient (fw+bw) per-eval (µs・ns/セル) ---"
  (tGA, _) <- timeitTastyIO id
    (\i -> pure $! VS.unsafeIndex (gradA (mkInputsCached i)) (i `mod` nObs))
  report "[A'] 現行 (2 arena+op特化bw)" tGA
  (tGC, _) <- timeitTastyIO id
    (\i -> pure $! VS.unsafeIndex (gradC (mkInputsCached i)) (i `mod` nObs))
  report "[C'] 完全融合1-pass (天井)" tGC

  printf "\n倍率: fw A/C=%.1f×  A/B=%.2f×  A/D=%.1f×  grad A'/C'=%.1f×\n"
    (tA / tC) (tA / tB) (tA / tD) (tGA / tGC)

-- | 入力 16 変種を事前生成 (CSE 回避・生成コストを計測に混ぜない)。
inputsPool :: BV.Vector (BV.Vector (VS.Vector Double))
inputsPool = BV.fromList [ mkInputs s | s <- [0 .. 15] ]
{-# NOINLINE inputsPool #-}

mkInputsCached :: Int -> BV.Vector (VS.Vector Double)
mkInputsCached i = inputsPool BV.! (i `mod` 16)
