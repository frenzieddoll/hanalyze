{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-full-laziness -fno-cse #-}
-- | Phase 85.1: radon 相関モデルの gradVecIR per-eval 内訳プロファイル。
--
-- Phase 84 ベンチで radon (919 obs 相関階層) の per-eval が ~160µs =
-- XLA (numpyro) 比 ~5-10× 遅いと確定した。 本ベンチはその 160µs の内訳を
-- **成分分解で実測**する (推測するな計測せよ):
--
--   full compileGradUV closure per-eval
--   = pc 変換 (unconstrained→constrained・VS.generate)
--   + gradVecIR
--       = forwardArena (arena 確保 'VSM.unsafeNew' + forward 解釈)
--       + guard 検査 ('arenaGuardsOK')
--       + backward ('gradVecIRGo' = adj 確保 'VSM.replicate' + 逆伝播)
--   + 残差 prior の ad ('mPriorGrad'・radon で残るかも本ベンチで確定)
--   + chain rule (constrained→unconstrained・list ベース)
--
-- 各成分は下位経路 (forwardArena 単独・alloc 単独 等) を直接呼んで計測し、
-- 直接測れない成分 (guard・backward 純分) は差分で推定する。 併せて
-- 命令列 mix (命令種別 × 本数 × セル数) を静的に出す = forward/backward の
-- どこが重いか (gather / elementwise / Σ) の見当を付ける。
--
-- 実行: taskset -c 0 で 1 コア固定 (Phase 84 と同条件)。
--   cabal run bench-hbm-vecir-prof --project-file=cabal.project.plot -f benches
module Main where

import           Control.Monad                    (when)
import           Control.Monad.ST                 (runST)
import qualified Data.Map.Strict                  as Map
import qualified Data.Set                         as Set
import qualified Data.Text                        as T
import qualified Data.Vector                      as BV
import qualified Data.Vector.Storable             as VS
import qualified Data.Vector.Storable.Mutable     as VSM
import qualified Data.Vector.Unboxed              as VU
import           Numeric.AD.Mode.Reverse.Double   (grad)
import           Text.Printf                      (printf)

import           Hanalyze.Model.HBM               (ModelP, sampleNames)
import           Hanalyze.Fit                     (designHBMProgram)
import           Hanalyze.Stat.Distribution       (Transform)
import qualified Hanalyze.Model.HBM.Gradient      as G
import qualified Hanalyze.Model.HBM.IR            as IR

import           BenchUtil                        (timeitTastyIO)

-- ---------------------------------------------------------------------------
-- radon モデル (BenchHBMScaling と同一定義・同一 CSV)
-- ---------------------------------------------------------------------------

-- | Radon 生 CSV を読む (BenchHBMScaling.readRadon と同一)。
readRadon :: IO ([[Double]], [Int], [Double], [Double])
readRadon = do
  txt <- readFile "bench/data/radon.csv"
  let recs = map parseRow (drop 1 (lines txt))
      parseRow ln = case splitComma ln of
        (_c : ci : fl : lr : lu : _) ->
          (read ci :: Int, read fl :: Double, read lr :: Double, read lu :: Double)
        _ -> error ("readRadon: 列不足 " ++ ln)
      cidx    = [ c | (c, _, _, _) <- recs ]
      floors  = [ f | (_, f, _, _) <- recs ]
      ys      = [ y | (_, _, y, _) <- recs ]
      designX = [ [1.0, f, u] | (_, f, _, u) <- recs ]
  return (designX, cidx, floors, ys)

splitComma :: String -> [String]
splitComma s = case break (== ',') s of
  (a, ',' : rest) -> a : splitComma rest
  (a, _)          -> [a]

-- | Radon 相関 varying intercept+slope (BenchHBMScaling.radonModel と同一)。
radonModel :: [[Double]] -> [Int] -> [Double] -> [Double] -> ModelP ()
radonModel designX cidx floorCol ys =
  designHBMProgram designX ["(Intercept)", "floor", "uranium"]
                   [(cidx, nCounties, [floorCol])] ys
  where nCounties = if null cidx then 0 else maximum cidx + 1

-- ---------------------------------------------------------------------------
-- 残差 prior の ad (compileGradUV の mPriorGrad と同一構成)
-- ---------------------------------------------------------------------------

-- | 'G.compileGradUV' 内部の @grad (fExcl exclNames)@ を同一式で再現する
-- (radon の per-eval に残差 ad が乗っている場合、 その単独コストを測る)。
residGrad
  :: [[Double]] -> [Int] -> [Double] -> [Double]
  -> [T.Text] -> [Transform] -> Set.Set T.Text
  -> [Double] -> [Double]
residGrad dX cidx fl ys names trans excl = grad f
  where
    f us =
      let paramsC = Map.fromList
            [ (n, G.invTransformF t u) | (n, t, u) <- zip3 names trans us ]
          logJac  = sum [ G.logJacF t u | (t, u) <- zip trans us ]
      in G.logJointExclBlocks excl (radonModel dX cidx fl ys) paramsC + logJac

-- ---------------------------------------------------------------------------
-- 命令列 mix (静的)
-- ---------------------------------------------------------------------------

-- | (命令種別, 本数, 総セル数 = Σ max 1 len)。 forward/backward の作業量の
-- 静的な見当 (gather / elementwise / Σ の比率)。
instrMix :: IR.VecProgram -> [(String, Int, Int)]
instrMix prog =
  let instrs = BV.toList (IR.vpInstrs prog)
      lens   = VU.toList (IR.vpLen prog)
      keyOf ins = case ins of
        IR.VIK{}     -> "VIK   (スカラ定数)"
        IR.VIKV{}    -> "VIKV  (ベクトル定数)"
        IR.VILeafS{} -> "VILeafS (scalar leaf)"
        IR.VILeafV{} -> "VILeafV (vector leaf)"
        IR.VIGath{}  -> "VIGath (gather)"
        IR.VIUn{}    -> "VIUn  (elementwise 単項)"
        IR.VIBin{}   -> "VIBin (elementwise 二項)"
        IR.VISum{}   -> "VISum (Σ 縮約)"
        IR.VIAxpy{}  -> "VIAxpy (a+s·v 融合)"
        IR.VIAxpyC{} -> "VIAxpyC (a+s·const 融合)"
        IR.VISumSqD{} -> "VISumSqD (Σ(x−m)² 融合)"
        IR.VISumSqC{} -> "VISumSqC (Σ(c−m)² 融合)"
        IR.VIMulG{}   -> "VIMulG (s·gather 融合)"
        IR.VIAxpyG{}  -> "VIAxpyG (a+s·gather 融合)"
        IR.VIMulVC{}  -> "VIMulVC (s·v⊙c 融合)"
        IR.VISumSqC2{} -> "VISumSqC2 (Σ(c−m1−m2)² 融合)"
      accum m (ins, l) =
        Map.insertWith (\(c1, e1) (c2, e2) -> (c1 + c2, e1 + e2))
          (keyOf ins) (1 :: Int, max 1 l) m
      mixed = foldl accum Map.empty (zip instrs lens)
  in [ (k, c, e) | (k, (c, e)) <- Map.toList mixed ]

-- ---------------------------------------------------------------------------

usOf :: Double -> Double
usOf ms = ms * 1000

main :: IO ()
main = do
  putStrLn "=== Phase 85.1: radon gradVecIR per-eval 内訳プロファイル ==="
  (dX, cidx, fl, ys) <- readRadon
  let m :: ModelP ()
      m = radonModel dX cidx fl ys
      names  = sampleNames m
      trans  = [ Map.findWithDefault (error "transform missing") n tmap
               | n <- names ]
      tmap   = G.getTransforms m
      nP     = length names
      nObs   = length ys

  -- (a) vecIR 経路の compile (compileGradUV の IR branch と同一手順)
  let (gbs, _) = G.gaussLMBlocksAuto m
  when (not (null gbs)) $
    putStrLn "★注意: gaussLMBlocksAuto が非空 = radon は IR branch でなく hybrid branch"
  case IR.synthVecIR m of
    Nothing -> error "synthVecIR = Nothing (radon が vecIR に乗っていない)"
    Just (gs, fams, sObs) -> do
      let ixOf   = Map.fromList (zip names [0 :: Int ..])
          cvi    = IR.compileVecIR ixOf gs fams
          prog   = IR.cvProg cvi
          famSet = Set.fromList (concat [ ms | (ms, _, _) <- fams ])
          cps    = G.constPriorsOf m famSet
          exclNames = sObs `Set.union` famSet
                      `Set.union` Set.fromList (map fst cps)
          noResid = G.residualFreeOfDensity exclNames m
          transB  = BV.fromList trans
          sz      = IR.vpSize prog
          objOff  = IR.vpOff prog `VU.unsafeIndex` IR.vpObj prog

      -- ---- 静的サマリ ----
      printf "obs=%d  nP=%d  instrs=%d  vpSize(arena セル)=%d  guards=%d\n"
        nObs nP (BV.length (IR.vpInstrs prog)) sz (length (IR.vpGuards prog))
      printf "residual ad (mPriorGrad): %s  (constPriors=%d, excl=%d/%d)\n"
        (if noResid then "なし (noResid)" else "★あり = per-eval に ad が乗る" :: String)
        (length cps) (Set.size exclNames) nP
      putStrLn "\n--- 命令列 mix (種別 / 本数 / 総セル数) ---"
      mapM_ (\(k, c, e) -> printf "  %-24s %5d 本  %8d セル\n" k c e)
            (instrMix prog)

      -- 85.3-ii: 実命令列 dump (superinstruction パターン選定用)
      putStrLn "\n--- 命令列 listing (slot: 命令 [len]) ---"
      BV.imapM_ (\i ins -> do
        let l = IR.vpLen prog `VU.unsafeIndex` i
            s = case ins of
                  IR.VIK v        -> printf "VIK %.4g" v :: String
                  IR.VIKV v       -> printf "VIKV (n=%d)" (VS.length v)
                  IR.VILeafS p    -> printf "VILeafS p%d" p
                  IR.VILeafV p    -> printf "VILeafV p%d" p
                  IR.VIGath p _ n -> printf "VIGath p%d (n=%d)" p n
                  IR.VIUn o x     -> printf "VIUn %s s%d" (show o) x
                  IR.VIBin o x y  -> printf "VIBin %s s%d s%d" (show o) x y
                  IR.VISum x      -> printf "VISum s%d" x
                  IR.VIAxpy a sc v  -> printf "VIAxpy s%d s%d s%d" a sc v
                  IR.VIAxpyC a sc c ->
                    printf "VIAxpyC s%d s%d (n=%d)" a sc (VS.length c)
                  IR.VISumSqD x m -> printf "VISumSqD s%d s%d" x m
                  IR.VISumSqC c m ->
                    printf "VISumSqC (n=%d) s%d" (VS.length c) m
                  IR.VIMulG sc p _ n ->
                    printf "VIMulG s%d gath(p%d,n=%d)" sc p n
                  IR.VIAxpyG a sc p _ n ->
                    printf "VIAxpyG s%d s%d gath(p%d,n=%d)" a sc p n
                  IR.VIMulVC sc v c ->
                    printf "VIMulVC s%d s%d (n=%d)" sc v (VS.length c)
                  IR.VISumSqC2 c m1 m2 ->
                    printf "VISumSqC2 (n=%d) s%d s%d" (VS.length c) m1 m2
        printf "  s%-3d [%4d] %s\n" i l s) (IR.vpInstrs prog)
      printf "  obj=s%d  guards=%s\n" (IR.vpObj prog)
        (show [ sl | (_, sl) <- IR.vpGuards prog ])

      -- ---- 計測点 (16 変種で CSE 回避・guard 域内) ----
      let uvs = BV.fromList
            [ VS.generate nP (\k -> 1e-3 * fromIntegral ((j * 31 + k) `mod` 17))
            | j <- [0 :: Int .. 15] ]
          pcOf uv = VS.generate nP $ \i ->
            G.invTransformF (transB BV.! i) (uv `VS.unsafeIndex` i)
          pcs = BV.map pcOf uvs
          uvAt i = uvs BV.! (i `mod` 16)
          pcAt i = pcs BV.! (i `mod` 16)
      -- pcs を先に強制 (計測に混ぜない)
      mapM_ (\pc -> pure $! VS.sum pc) (BV.toList pcs)
      let v0 = IR.vecIRValue cvi (pcAt 0)
      printf "\nvecIRValue @probe = %.6f (有限であること)\n" v0

      -- ---- per-eval 計測 (tasty-bench 適応・µs) ----
      let gv = G.compileGradUV m names trans
      (tFull, _) <- timeitTastyIO id $ \i ->
        pure $! VS.unsafeIndex (gv (uvAt i)) (i `mod` nP)
      (tPc, _) <- timeitTastyIO id $ \i ->
        pure $! VS.unsafeIndex (pcOf (uvAt i)) (i `mod` nP)
      (tValue, _) <- timeitTastyIO id $ \i ->
        pure $! IR.vecIRValue cvi (pcAt i)
      (tGradIR, _) <- timeitTastyIO id $ \i ->
        pure $! runST (do
          mg <- VSM.replicate nP 0
          ok <- IR.gradVecIR cvi (pcAt i) mg
          if ok then VSM.unsafeRead mg (i `mod` nP) else pure (0 / 0))
      (tForward, _) <- timeitTastyIO id $ \i ->
        pure $! runST (do
          ar <- IR.forwardArena cvi (pcAt i)
          VSM.unsafeRead ar objOff)
      (tFwGuard, _) <- timeitTastyIO id $ \i ->
        pure $! runST (do
          ar <- IR.forwardArena cvi (pcAt i)
          ok <- IR.arenaGuardsOK prog ar
          if ok then VSM.unsafeRead ar objOff else pure (0 / 0))
      (tAllocFw, _) <- timeitTastyIO id $ \i ->
        pure $! runST (do
          ar <- VSM.unsafeNew sz
          VSM.unsafeWrite ar 0 (fromIntegral i :: Double)
          VSM.unsafeRead ar 0)
      (tAllocAdj, _) <- timeitTastyIO id $ \i ->
        pure $! runST (do
          adj <- VSM.replicate sz (0 :: Double)
          VSM.unsafeWrite adj 0 (fromIntegral i)
          VSM.unsafeRead adj (sz - 1))
      tResid <- if noResid
        then pure Nothing
        else do
          let rg = residGrad dX cidx fl ys names trans exclNames
          (t, _) <- timeitTastyIO id $ \i ->
            pure $! sum (rg (VS.toList (uvAt i)))
          pure (Just t)

      -- ---- 内訳表 (µs・差分成分は推定) ----
      let us = usOf
          fwInterp  = tForward - tAllocFw
          guardT    = tFwGuard - tForward
          backward  = tGradIR - tFwGuard - tAllocAdj
          accounted = tPc + tGradIR + maybe 0 id tResid
          chainRest = tFull - accounted
          pct t = 100 * t / tFull
      putStrLn "\n--- per-eval 内訳 (µs・full 比 %) ---"
      printf "full compileGradUV closure      : %9.2f µs (100.0%%)\n" (us tFull)
      printf "├ pc 変換 (VS.generate)         : %9.2f µs (%5.1f%%)\n" (us tPc) (pct tPc)
      printf "├ gradVecIR (fw+guard+bw)       : %9.2f µs (%5.1f%%)\n" (us tGradIR) (pct tGradIR)
      printf "│  ├ forward arena 確保 (New)   : %9.2f µs (%5.1f%%)\n" (us tAllocFw) (pct tAllocFw)
      printf "│  ├ forward 解釈 (差分)        : %9.2f µs (%5.1f%%)\n" (us fwInterp) (pct fwInterp)
      printf "│  ├ guard 検査 (差分)          : %9.2f µs (%5.1f%%)\n" (us guardT) (pct guardT)
      printf "│  ├ adj arena 確保 (replicate) : %9.2f µs (%5.1f%%)\n" (us tAllocAdj) (pct tAllocAdj)
      printf "│  └ backward 逆伝播 (差分)     : %9.2f µs (%5.1f%%)\n" (us backward) (pct backward)
      case tResid of
        Nothing -> printf "├ 残差 prior ad                 : なし (noResid)\n"
        Just t  -> printf "├ 残差 prior ad (grad fExcl)    : %9.2f µs (%5.1f%%)\n" (us t) (pct t)
      printf "└ chain rule + 残り (差分)      : %9.2f µs (%5.1f%%)\n" (us chainRest) (pct chainRest)
      printf "(参考) vecIRValue (値のみ)      : %9.2f µs (%5.1f%%)\n" (us tValue) (pct tValue)
