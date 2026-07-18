{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-full-laziness -fno-cse #-}
-- | Phase 85.6a: warmup 固定費の内訳プロファイル (radon)。
--
-- radon の wall は warmup 500 draw の固定費 (~3.3s = 6.6ms/draw) が支配し、
-- 本サンプリング (~1.4ms/draw) の ~5 倍/draw。 本ドライバは 'nutsStream' の
-- per-iteration callback ('seTreeDepth' = Phase 85.6 追加) で warmup 中の
-- tree depth / ε の推移を採取し、 固定費の主因 (適応初期の深い tree?) を
-- 確定する。 leapfrog 数 ≈ 2^depth で勾配評価回数を近似する。
--
--   cabal run bench-warmup-prof --project-file=cabal.project.plot -f benches
module Main where

import           Data.IORef                       (modifyIORef', newIORef,
                                                   readIORef)
import qualified Data.Map.Strict                  as Map
import qualified Data.Text                        as T
import qualified Data.Vector                      as V
import qualified System.Random.MWC                as MWC
import           System.Environment               (getArgs)
import           Text.Printf                      (printf)

import           Hanalyze.Model.HBM               (ModelP)
import           Hanalyze.Fit                     (designHBMProgram)
import           Hanalyze.MCMC.NUTS               (NUTSConfig (..),
                                                   SampleEvent (..),
                                                   defaultNUTSConfig,
                                                   nutsStream)

-- ---------------------------------------------------------------------------
-- radon モデル (BenchHBMScaling と同一)
-- ---------------------------------------------------------------------------

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

radonModel :: [[Double]] -> [Int] -> [Double] -> [Double] -> ModelP ()
radonModel designX cidx floorCol ys =
  designHBMProgram designX ["(Intercept)", "floor", "uranium"]
                   [(cidx, nCounties, [floorCol])] ys
  where nCounties = if null cidx then 0 else maximum cidx + 1

warmupN, sampleN :: Int
warmupN = 500
sampleN = 100  -- 既定。 第 2 引数で上書き可 (Phase 87.2 profiling 用)

mkConfig :: Int -> NUTSConfig
mkConfig nSamp = defaultNUTSConfig
  { nutsIterations    = nSamp
  , nutsBurnIn        = warmupN
  , nutsStepSize      = 0.1
  , nutsMaxDepth      = 10
  , nutsAdaptStepSize = True
  , nutsTargetAccept  = 0.8
  , nutsAdaptMass     = True
  }

main :: IO ()
main = do
  -- Phase 86 着手時計測: seed 分散を見るため第 1 引数で seed 指定可 (既定 42)。
  args <- getArgs
  let seed = case args of { (s : _) -> read s; [] -> 42 } :: Int
      nSamp = case args of { (_ : n : _) -> read n; _ -> sampleN } :: Int
  printf "=== Phase 85.6a: radon warmup 内訳プロファイル (seed=%d) ===\n" seed
  (dX, cidx, fl, ys) <- readRadon
  let m :: ModelP ()
      m = radonModel dX cidx fl ys
      initP = Map.fromList
        [ ("(Intercept)", 1.3), ("floor", -0.6), ("uranium", 0.7)
        , ("sigma", 0.7)
        , ("tau_g0_0", 0.5), ("tau_g0_1", 0.3), ("Lcorr_g0_u1_0", 0.5) ]
  evRef <- newIORef ([] :: [(Int, Bool, Int, Double, Double)])
  g <- MWC.initialize (V.singleton (fromIntegral seed))
  _ <- nutsStream m (mkConfig nSamp) initP g $ \ev ->
    modifyIORef' evRef
      ((seIter ev, seIsBurnIn ev, seTreeDepth ev, seStepSize ev, seAcceptStat ev) :)
  evs <- fmap reverse (readIORef evRef)

  let leap d = (2 :: Int) ^ d
      warm = [ e | e@(_, True,  _, _, _) <- evs ]
      samp = [ e | e@(_, False, _, _, _) <- evs ]
      -- Stan windows (W=500): init buffer 75・windows 75-450・term 450-500
      seg lo hi = [ (d, eps, al) | (i, _, d, eps, al) <- warm, i >= lo, i < hi ]
      segs = [ ("init buf   [  0, 75)", seg 0 75)
             , ("window 25  [ 75,100)", seg 75 100)
             , ("window 50  [100,150)", seg 100 150)
             , ("window 100 [150,250)", seg 150 250)
             , ("window 200 [250,450)", seg 250 450)
             , ("term buf   [450,500)", seg 450 500) ]
      stats xs =
        let ds = [ d | (d, _, _) <- xs ]
            n  = max 1 (length ds)
            meanD = fromIntegral (sum ds) / fromIntegral n :: Double
            lps   = sum (map leap ds)
            epsL  = case xs of { [] -> 0 / 0; _ -> (\(_, e, _) -> e) (last xs) }
            meanA = sum [ a | (_, _, a) <- xs ] / fromIntegral n
        in (meanD, maximum (0 : ds), lps, epsL, meanA)
  putStrLn "\n--- warmup 区間別 (depth 平均 / 最大 / leapfrog 総数 / 区間末ε / 平均α) ---"
  mapM_ (\(tag, xs) ->
    let (md, mx, lp, eps, ma) = stats xs
    in printf "  %-22s depth=%5.2f max=%2d  leapfrogs=%7d  ε=%.4g  α=%.3f\n"
         (tag :: String) md mx lp eps ma) segs

  let (mdS, mxS, lpS, epsS, maS) = stats [ (d, e, a) | (_, _, d, e, a) <- samp ]
      lpW = sum [ leap d | (_, _, d, _, _) <- warm ]
  printf "\n--- sampling %d draw: depth=%.2f max=%d leapfrogs=%d ε̄=%.4g α=%.3f ---\n"
    nSamp mdS mxS lpS epsS maS
  printf "\nleapfrog 総数: warmup=%d (%.1f/draw)  sampling=%d (%.1f/draw)\n"
    lpW (fromIntegral lpW / fromIntegral warmupN :: Double)
    lpS (fromIntegral lpS / fromIntegral nSamp :: Double)
  printf "warmup 支配率 (grad 評価ベース) = %.1f%%\n"
    (100 * fromIntegral lpW / fromIntegral (lpW + lpS) :: Double)
  -- ε 推移の先頭 20 draw (初期 ε=0.1 の適否)
  putStrLn "\n--- 先頭 20 draw の (depth, ε, α) ---"
  mapM_ (\(i, _, d, eps, al) -> printf "  iter=%3d depth=%2d ε=%.5f α=%.3f\n" i d eps al)
        (take 20 evs)
  -- Phase 87.1: term buffer の DA 収束 trace (最終 window 末 450 の再較正 anchor
  -- から ε がどう動き、 ε̄ がどこに着地するかの診断)。
  putStrLn "\n--- term buffer [445,500) + sampling 先頭 5 の (depth, ε, α) ---"
  mapM_ (\(i, b, d, eps, al) ->
          printf "  iter=%3d %s depth=%2d ε=%.5f α=%.3f\n"
            i (if b then "W" else "S" :: String) d eps al)
        [ e | e@(i, _, _, _, _) <- evs, i >= 445, i < 505 ]
