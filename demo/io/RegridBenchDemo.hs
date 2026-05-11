{-# LANGUAGE OverloadedStrings #-}

-- | Regrid 機能のベンチマークデモ。
--
-- 1. 真の関数 V(z; D) を物理モデル (PotentialGen と同じ) で生成
-- 2. 観測点を歯抜け化 (20% drop + z ズレ ±15 nm) → long-form
-- 3. 3 補間 (Linear / NaturalSpline / PCHIP) × 2 grid (Uniform / Adaptive) で
--    共通 grid に揃える
-- 4. grid 上で真値と比較し RMSE を計算
-- 5. 全結果を 1 つの HTML レポートにまとめて出力
module Main where

import qualified Data.Text             as T
import           Data.Text             (Text)
import           System.Random.MWC     (createSystemRandom, GenIO, uniformR)
import qualified System.Random.MWC.Distributions as MWCD
import           Text.Printf           (printf)
import           Control.Monad         (forM)
import           Data.List             (sort)

import qualified DataFrame             as DX
import qualified Hanalyze.DataIO.Preprocess     as Pp
import qualified Hanalyze.Stat.Interpolate      as Interp
import qualified Hanalyze.Stat.AdaptiveGrid     as AG
import qualified Hanalyze.Viz.ReportBuilder     as RB

-- ---------------------------------------------------------------------------
-- 真の物理モデル (PotentialGen.hs と同じ)
-- ---------------------------------------------------------------------------

projectedRange :: Double -> Double
projectedRange e = 1.5 * (e ** 0.7)

straggle :: Double -> Double
straggle e = 0.4 * projectedRange e

surfaceL, implantK, doseRef, doseAlpha, fixedE :: Double
surfaceL  = 30.0
implantK  = 8.0
doseRef   = 10.0
doseAlpha = 0.26
fixedE    = 100.0

trueV :: Double -> Double -> Double
trueV d z =
  let rp  = projectedRange fixedE
      sg  = straggle fixedE
      amp = implantK * ((d / doseRef) ** doseAlpha)
      surf = 3.5 * exp (negate z / surfaceL)
      well = amp * exp (negate ((z - rp) ** 2) / (2 * sg * sg))
  in surf - well

doses :: [Double]
doses = [6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0]

zRange :: (Double, Double)
zRange = (0, 200)

zPoints :: Int
zPoints = 80

-- ---------------------------------------------------------------------------
-- 歯抜けデータの生成
-- ---------------------------------------------------------------------------

genJaggedRows :: GenIO -> Double -> IO [(Double, Double)]
genJaggedRows gen d = do
  let (zlo, zhi) = zRange
      base       = (zhi - zlo) / fromIntegral (zPoints - 1)
      jitter     = base * 2.5
  zs <- forM [0 .. zPoints - 1] $ \i -> do
    let zb = zlo + fromIntegral i * base
    j <- uniformR (-jitter, jitter) gen
    return (max zlo (min zhi (zb + j)))
  let zsSorted = sort zs
  -- 20% を欠損化
  pts <- forM zsSorted $ \z -> do
    drop' <- uniformR (0, 1 :: Double) gen
    if drop' < 0.20
      then return Nothing
      else do
        eps <- MWCD.normal 0 0.1 gen
        return (Just (z, trueV d z + eps))
  return [p | Just p <- pts]

condId :: Double -> Text
condId d = T.pack (printf "D%.0f" d)

-- ---------------------------------------------------------------------------
-- 補間 + RMSE 計測
-- ---------------------------------------------------------------------------

data Bench = Bench
  { bInterp   :: Interp.InterpKind
  , bGrid     :: AG.GridKind
  , bRMSE     :: Double
  , bNGrid    :: Int
  , bResult   :: Pp.RegridResult
  }

interpName :: Interp.InterpKind -> Text
interpName Interp.Linear        = "Linear"
interpName Interp.NaturalSpline = "NaturalSpline"
interpName Interp.PCHIP         = "PCHIP"

gridName :: AG.GridKind -> Text
gridName AG.Uniform  = "Uniform"
gridName AG.Adaptive = "Adaptive"

runBench :: DX.DataFrame -> Interp.InterpKind -> AG.GridKind -> Bench
runBench df ik gk =
  let opts = Pp.defaultRegridOpts
               { Pp.roInterp      = ik
               , Pp.roGridKind    = gk
               , Pp.roN           = 30
               , Pp.roZBoundsMode = Pp.ZIntersection
               }
      rr   = Pp.regridLong "id" "z" "y" opts df
      -- grid 上の予測 vs 真値
      sqErrs =
        [ let yTrue = trueV (read (drop 1 (T.unpack i)) :: Double) z
              yHat  = f z
          in (yHat - yTrue) ** 2
        | (i, _, f) <- Pp.rrPerIdInterp rr
        , z <- Pp.rrZGrid rr
        ]
      rmse = if null sqErrs then 0
             else sqrt (sum sqErrs / fromIntegral (length sqErrs))
  in Bench ik gk rmse (length (Pp.rrZGrid rr)) rr

-- ---------------------------------------------------------------------------
-- レポート生成
-- ---------------------------------------------------------------------------

mkBenchReport :: [Bench] -> [RB.ReportSection]
mkBenchReport benches =
  let cmpRows = [ [ interpName (bInterp b) <> " / " <> gridName (bGrid b)
                  , T.pack (printf "%.4f" (bRMSE b))
                  , T.pack (show (bNGrid b))
                  ]
                | b <- benches ]
      cmpTable = RB.secTable "RMSE benchmark (vs true V(z; D))"
                   ["Method", "RMSE", "Grid N"] cmpRows
      detailSections =
        [ RB.secInterpolation (irFromBench b)
        | b <- benches ]
  in cmpTable : detailSections

irFromBench :: Bench -> RB.InterpReport
irFromBench b =
  let rr   = bResult b
      perObs   = [ (i, pts) | (i, pts, _) <- Pp.rrPerIdInterp rr ]
      perInterp = [ (i, [(z, f z) | z <- Pp.rrZGrid rr])
                  | (i, _, f) <- Pp.rrPerIdInterp rr ]
      perSummary = [ (Pp.piId s, Pp.piNObserved s
                    , Pp.piZMin s, Pp.piZMax s
                    , Pp.piExtrapBelow s, Pp.piExtrapAbove s
                    , Pp.piResidualMax s)
                   | s <- Pp.rrPerIdStats rr ]
  in RB.InterpReport
       { RB.irTitle         = interpName (bInterp b) <> " / "
                              <> gridName (bGrid b)
                              <> " — RMSE "
                              <> T.pack (printf "%.4f" (bRMSE b))
       , RB.irInterpKind    = interpName (bInterp b)
       , RB.irGridKind      = gridName (bGrid b)
       , RB.irN             = bNGrid b
       , RB.irZBoundsMode   = "intersect"
       , RB.irZMin          = Pp.rrZMin rr
       , RB.irZMax          = Pp.rrZMax rr
       , RB.irPerIdObserved = perObs
       , RB.irPerIdInterpY  = perInterp
       , RB.irGrid          = Pp.rrZGrid rr
       , RB.irDensity       = Pp.rrDensity rr
       , RB.irPerIdSummary  = perSummary
       , RB.irExtraEnabled  = False
       , RB.irPerIdYRange   = []
       }

-- ---------------------------------------------------------------------------
-- main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  gen <- createSystemRandom
  putStrLn "Regrid benchmark — 6 methods × 9 dose levels"
  -- 全 dose の歯抜けデータを 1 つの long DataFrame にまとめる
  perDoseData <- forM doses $ \d -> do
    pts <- genJaggedRows gen d
    return (condId d, pts)
  let allRows = concat
        [ [ (i, z, y) | (z, y) <- pts ]
        | (i, pts) <- perDoseData ]
      ids    = map (\(i,_,_) -> i) allRows
      zs     = map (\(_,z,_) -> z) allRows
      ys     = map (\(_,_,y) -> y) allRows
      df     = DX.insertColumn "y"  (DX.fromList ys)
             $ DX.insertColumn "z"  (DX.fromList zs)
             $ DX.insertColumn "id" (DX.fromList ids)
             $ DX.empty
  printf "  Generated %d rows from %d ids\n" (length allRows) (length doses)
  -- 6 組合せでベンチマーク
  let kinds = [Interp.Linear, Interp.NaturalSpline, Interp.PCHIP]
      grids = [AG.Uniform, AG.Adaptive]
      benches = [ runBench df ik gk | ik <- kinds, gk <- grids ]
  putStrLn "RMSE results (vs true V(z; D)):"
  mapM_ (\b -> printf "  %-15s / %-9s : RMSE = %.4f (N=%d)\n"
                  (T.unpack (interpName (bInterp b)))
                  (T.unpack (gridName (bGrid b)))
                  (bRMSE b)
                  (bNGrid b))
        benches
  let outPath = "trash/regrid_bench.html"
  RB.renderReport outPath
                  (RB.defaultReportConfig "Regrid benchmark — 3 interp × 2 grid")
                  (mkBenchReport benches)
  putStrLn $ "Wrote " ++ outPath
