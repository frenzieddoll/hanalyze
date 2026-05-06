{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-full-laziness -fno-cse #-}
-- | Survival / time-series / quantile / GAM / spline benchmarks (B8).
--
-- Compares hanalyze against statsmodels (ARIMA, quantile regression),
-- lifelines (Cox PH, Kaplan-Meier), pygam (GAM), and scipy.interpolate
-- (1D spline interpolation).
--
-- Outputs the unified BenchRow CSV at @bench/results/haskell/survts.csv@.
module Main where

import qualified Data.Vector             as V
import qualified Numeric.LinearAlgebra   as LA
import           System.Random           (mkStdGen, randomR)

import qualified Model.TimeSeries        as TS
import qualified Model.Survival          as Surv
import qualified Model.Quantile          as QR
import qualified Model.GAM               as GAM
import qualified Stat.Interpolate        as Interp

import           BenchUtil

-- ---------------------------------------------------------------------------
-- Synthetic data generators (deterministic seeds)
-- ---------------------------------------------------------------------------

-- | AR(1) series with phi=0.7 and Gaussian noise.
genAR1 :: Int -> LA.Vector Double
genAR1 n = LA.fromList (go (mkStdGen 42) 0.0 [])
  where
    go _ _ acc | length acc >= n = reverse acc
    go g x acc =
      let (z, g') = randomR (-3.0, 3.0) g
          x'     = 0.7 * x + 0.3 * z
      in go g' x' (x' : acc)

-- | Survival data: exponential time + 30% censoring.
genSurv :: Int -> ([LA.Vector Double], [Surv.SurvSample])
genSurv n =
  let g0 = mkStdGen 7
      (rows, _) = foldr step ([], g0) [1 .. n]
      step _ (acc, g) =
        let (x1, g1) = randomR (-1.0 :: Double, 1.0) g
            (x2, g2) = randomR (-1.0 :: Double, 1.0) g1
            (u,  g3) = randomR (0.01 :: Double, 1.0) g2
            t        = -log u / exp (0.5 * x1 - 0.3 * x2)
            (c,  g4) = randomR (0.0 :: Double, 1.0) g3
            ev       = if c < 0.7 then Surv.Observed else Surv.Censored
        in ((LA.fromList [x1, x2], Surv.SurvSample t ev) : acc, g4)
  in unzip rows

-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  rows <- mconcat <$> sequence
    [ benchARIMA   "ARIMA_n1000_pdq111"
    , benchCoxPH   "CoxPH_n2000_p2_30pct_censor"
    , benchKM      "KM_n2000"
    , benchQuant   "Quantile_n10000_p20_tau0.5"
    , benchGAM     "GAM_n2000_p2_d3_k5"
    , benchSpline  "Spline_PCHIP_n1000"
    ]
  writeRows "bench/results/haskell/survts.csv" rows
  putStrLn $ "wrote " ++ show (length rows)
          ++ " rows → bench/results/haskell/survts.csv"

-- ---------------------------------------------------------------------------
-- ARIMA(1,1,1)
-- ---------------------------------------------------------------------------

benchARIMA :: String -> IO [BenchRow]
benchARIMA name = do
  let y = genAR1 1000
      run :: Int -> IO TS.ARIMAFit
      run _ = pure $! TS.fitARIMA 1 1 1 y
  (ms, fit) <- timeitTastyIO probe run
  return [ BenchRow "haskell" "survts" name ms 0 0
            ("Model.TimeSeries.fitARIMA p=1 d=1 q=1 n=1000") ]
  where
    probe f = LA.sumElements (TS.forecastARIMA f 10)

-- ---------------------------------------------------------------------------
-- Cox PH
-- ---------------------------------------------------------------------------

benchCoxPH :: String -> IO [BenchRow]
benchCoxPH name = do
  let (xs, samples) = genSurv 2000
      run :: Int -> IO Surv.CoxFit
      run _ = pure $! Surv.coxPH xs samples
  (ms, fit) <- timeitTastyIO probe run
  let beta = Surv.coxBeta fit
      b1 = beta LA.! 0
      b2 = beta LA.! 1
  return [ BenchRow "haskell" "survts" name ms b1 b2
            ("Model.Survival.coxPH n=2000 p=2 (Newton-Raphson)") ]
  where
    probe f = LA.sumElements (Surv.coxBeta f)

-- ---------------------------------------------------------------------------
-- Kaplan-Meier
-- ---------------------------------------------------------------------------

benchKM :: String -> IO [BenchRow]
benchKM name = do
  let (_, samples) = genSurv 2000
      run :: Int -> IO Surv.KMResult
      run _ = pure $! Surv.kaplanMeier samples
  (ms, res) <- timeitTastyIO probe run
  let ts   = Surv.kmrTimes res
      surv = Surv.kmrSurvival res
      tEnd = if null ts then 0 else last ts
      sEnd = if null surv then 1 else last surv
  return [ BenchRow "haskell" "survts" name ms tEnd sEnd
            ("Model.Survival.kaplanMeier n=2000") ]
  where
    probe r = sum (Surv.kmrSurvival r)

-- ---------------------------------------------------------------------------
-- Quantile regression (median, tau=0.5)
-- ---------------------------------------------------------------------------

benchQuant :: String -> IO [BenchRow]
benchQuant name = do
  (x, y) <- readCsvXY "bench/data/lm_n10000_p50.csv"
  -- Use first 20 columns for fair comparison with Python.
  let xCut = LA.takeColumns 20 x
      run :: Int -> IO QR.QRFit
      run _ = pure $! QR.fitQuantile 0.5 xCut y
  (ms, fit) <- timeitTastyIO probe run
  return [ BenchRow "haskell" "survts" name ms 0 0
            ("Model.Quantile.fitQuantile tau=0.5 n=10000 p=20") ]
  where
    probe f = LA.sumElements (QR.qfBeta f)

-- ---------------------------------------------------------------------------
-- GAM (degree=3, knots=5, two predictors)
-- ---------------------------------------------------------------------------

benchGAM :: String -> IO [BenchRow]
benchGAM name = do
  (x, y) <- readCsvXY "bench/data/kernel_n2000_p5.csv"
  let cols = LA.toColumns x
      x1   = V.fromList (LA.toList (head cols))
      x2   = V.fromList (LA.toList (cols !! 1))
      yV   = V.fromList (LA.toList y)
      run :: Int -> IO GAM.GAMFit
      run _ = pure $! GAM.fitGAM 3 5 1.0 [x1, x2] yV
  (ms, fit) <- timeitTastyIO probe run
  return [ BenchRow "haskell" "survts" name ms 0 0
            ("Model.GAM.fitGAM degree=3 knots=5 lambda=1.0 n=2000 p=2") ]
  where
    probe f = LA.sumElements (GAM.gamYHat f)

-- ---------------------------------------------------------------------------
-- 1D spline interpolation (PCHIP)
-- ---------------------------------------------------------------------------

benchSpline :: String -> IO [BenchRow]
benchSpline name = do
  let n = 1000
      xs = [fromIntegral i / fromIntegral (n - 1) | i <- [0 .. n - 1]]
      ys = map (\xi -> sin (3 * xi) + 0.1 * cos (15 * xi)) xs
      pts = zip xs ys
      f = Interp.interp1d Interp.PCHIP pts
      -- Evaluate at 5000 query points.
      qs = [fromIntegral i / 4999.0 | i <- [0 .. 4999 :: Int]]
      run :: Int -> IO Double
      run _ = pure $! sum [f q | q <- qs]
  (ms, total) <- timeitTastyIO id run
  return [ BenchRow "haskell" "survts" name ms total 0
            ("Stat.Interpolate.interp1d PCHIP, build n=1000 + eval @5000 pts") ]
