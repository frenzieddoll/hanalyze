{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-full-laziness -fno-cse #-}
-- | B8 残: Holt-Winters / GAM / Spline ベンチ。
--
--   * Holt-Winters seasonal n=500 period=12 (Additive)
--   * GAM n=2000 splines=10 (1D)
--   * Interp1d (Linear / NaturalSpline / PCHIP) on n=1000 grid → eval 5000 pts
--
-- 出力: bench/results/haskell/ts_extras.csv
module Main where

import qualified Data.Vector             as V
import qualified Numeric.LinearAlgebra   as LA

import           Model.TimeSeries        (HWMode (..), holtWinters, hwFitted)
import           Model.GAM               (fitGAM, gamYHat)
import           Stat.Interpolate        (InterpKind (..), interp1d)

import           BenchUtil

-- ---------------------------------------------------------------------------
-- Data generators (deterministic, no RNG).
-- ---------------------------------------------------------------------------

-- Seasonal series of length n with period 12: y_t = trend + sin(2π t/12) + ε
-- where ε is small deterministic noise (sinusoidal with different period).
seasonalSeries :: Int -> LA.Vector Double
seasonalSeries n =
  LA.fromList
    [ 0.05 * fromIntegral t
        + 2.0 * sin (2 * pi * fromIntegral t / 12.0)
        + 0.1 * sin (fromIntegral t * 0.7)
    | t <- [0 .. n - 1] ]

-- 1D smooth-with-bumps function for GAM / interpolation.
smoothFn :: Double -> Double
smoothFn x = sin (2 * x) + 0.5 * x + 0.3 * sin (5 * x)

gamData :: Int -> ([V.Vector Double], V.Vector Double)
gamData n =
  let xs = V.fromList [ -3.0 + 6.0 * fromIntegral i / fromIntegral (n - 1)
                       | i <- [0 .. n - 1] ]
      ys = V.map smoothFn xs
  in ([xs], ys)

-- Returns (knots, fineEvalXs) — scattered knot data + evaluation grid.
interpData :: Int -> Int -> ([(Double, Double)], [Double])
interpData nKnots nEval =
  let knots = [ (xi, smoothFn xi)
              | i <- [0 .. nKnots - 1]
              , let xi = -3.0 + 6.0 * fromIntegral i / fromIntegral (nKnots - 1) ]
      grid  = [ -2.9 + 5.8 * fromIntegral i / fromIntegral (nEval - 1)
              | i <- [0 .. nEval - 1] ]
  in (knots, grid)

-- ---------------------------------------------------------------------------
-- Holt-Winters
-- ---------------------------------------------------------------------------

benchHW :: IO [BenchRow]
benchHW = do
  let !y = seasonalSeries 500
      run :: Int -> IO Double
      run _ = do
        let fit  = holtWinters HWAdditive 12 y
            yhat = hwFitted fit
            r    = yhat - y
            err  = LA.sumElements (LA.cmap (\d -> d * d) r)
        return err
      probe = id
  (ms, e) <- timeitTastyIO probe run
  let n = LA.size y
      rmse = sqrt (e / fromIntegral n)
  return [ BenchRow "haskell" "ts_extras"
            "HW_seasonal_n500_p12_additive" ms rmse 0
            ("Holt-Winters additive period=12 RMSE=" ++ show rmse) ]

-- ---------------------------------------------------------------------------
-- GAM
-- ---------------------------------------------------------------------------

benchGAM :: IO [BenchRow]
benchGAM = do
  let (xss, !y) = gamData 2000
      yLA = LA.fromList (V.toList y)
      run :: Int -> IO Double
      run _ = do
        let fit  = fitGAM 3 10 1e-3 xss y
            yhat = gamYHat fit
            r    = yhat - yLA
            err  = LA.sumElements (LA.cmap (\d -> d * d) r)
        return err
      probe = id
  (ms, e) <- timeitTastyIO probe run
  let n = V.length y
      rmse = sqrt (e / fromIntegral n)
  return [ BenchRow "haskell" "ts_extras"
            "GAM_n2000_splines10_1D" ms rmse 0
            ("GAM degree=3 nKnots=10 λ=1e-3 RMSE=" ++ show rmse) ]

-- ---------------------------------------------------------------------------
-- Spline interpolation: Linear / NaturalSpline / PCHIP each evaluated on 5000 pts
-- ---------------------------------------------------------------------------

benchInterp :: InterpKind -> String -> IO [BenchRow]
benchInterp kind label = do
  let nKnots = 1000
      nEval  = 5000
      (knots, grid) = interpData nKnots nEval
      run :: Int -> IO Double
      run _ = do
        let f = interp1d kind knots
            ys = map f grid
        return (sum ys)
      probe = id
  (ms, _) <- timeitTastyIO probe run
  return [ BenchRow "haskell" "ts_extras"
            ("Interp1D_" ++ label ++ "_knots1000_eval5000") ms 0 0
            (label ++ " interpolation knots=1000 eval=5000") ]

-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  rows <- mconcat <$> sequence
    [ benchHW
    , benchGAM
    , benchInterp Linear        "Linear"
    , benchInterp NaturalSpline "NatSpline"
    , benchInterp PCHIP         "PCHIP"
    ]
  writeRows "bench/results/haskell/ts_extras.csv" rows
  putStrLn $ "wrote " ++ show (length rows)
          ++ " rows → bench/results/haskell/ts_extras.csv"
