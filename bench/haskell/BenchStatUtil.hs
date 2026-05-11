{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-full-laziness -fno-cse #-}
-- | B10 Stat util ベンチ。
--
--   * Bootstrap CI: B=1000 resamples on n=1000 sample mean
--   * Welch's t-test: two samples n=500 each
--   * Mann-Whitney U: two samples n=500 each
--   * Multiple testing (BH): 1000 p-values
--   * Halton sequence: n=10000 d=5
--   * AUC + log-loss: n=10000 binary predictions
--   * k-fold split: 5-fold on n=1000
--
-- 出力: bench/results/haskell/stat_util.csv
module Main where

import qualified Data.Vector             as V
import qualified Data.Vector.Unboxed     as VU
import qualified Numeric.LinearAlgebra   as LA
import qualified System.Random.MWC       as MWC

import           Hanalyze.Stat.Bootstrap          (bootstrapMeanCI)
import           Hanalyze.Stat.Test               (Alternative (..),
                                          tTestWelch, mannWhitneyU,
                                          kolmogorovSmirnovNormal,
                                          TestResult (..))
import           Hanalyze.Stat.MultipleTesting    (benjaminiHochbergV)
import           Hanalyze.Stat.QuasiRandom        (haltonMatrix)
import           Hanalyze.Stat.ClassMetrics       (auc, logLoss)
import           Hanalyze.Stat.CV                 (kFold)

import           BenchUtil

-- ---------------------------------------------------------------------------
-- Deterministic data generators (no RNG dependency on values)
-- ---------------------------------------------------------------------------

-- Sin-of-i deterministic-ish vector ~ N(0, 1) in distribution.
syntheticVec :: Int -> Int -> LA.Vector Double
syntheticVec n offset =
  LA.fromList
    [ sin (fromIntegral (i + offset) * 0.71)
        + 0.4 * sin (fromIntegral (3 * i + offset))
    | i <- [0 .. n - 1] ]

shifted :: Double -> LA.Vector Double -> LA.Vector Double
shifted c v = v + LA.scalar c

-- ---------------------------------------------------------------------------

benchBootstrap :: IO [BenchRow]
benchBootstrap = do
  let xs = syntheticVec 1000 0
      run :: Int -> IO (Double, Double)
      run _ = do
        gen <- MWC.create
        -- P40 (2026-05-07): specialised mean-bootstrap path. Generic
        -- bootstrapCI invokes the statistic per resample (B times)
        -- against a freshly-frozen length-n Vector; this version
        -- uses one (B×n) buffer and one BLAS GEMV for all B means.
        bootstrapMeanCI 1000 0.95 xs gen
      probe (lo, hi) = hi - lo
  (ms, (lo, hi)) <- timeitTastyIO probe run
  return [ BenchRow "haskell" "stat_util"
            "Bootstrap_mean_n1000_B1000" ms (hi - lo) lo
            ("95% CI for mean of n=1000 with B=1000; ["
             ++ show lo ++ ", " ++ show hi ++ "]") ]

benchTTestWelch :: IO [BenchRow]
benchTTestWelch = do
  let xs = syntheticVec 500 0
      ys = shifted 0.3 (syntheticVec 500 1000)
      run :: Int -> IO TestResult
      run _ = return (tTestWelch xs ys TwoSided)
      probe r = trStatistic r
  (ms, r) <- timeitTastyIO probe run
  return [ BenchRow "haskell" "stat_util"
            "Welch_ttest_n500x500" ms (trStatistic r) (trPValue r)
            ("Welch's two-sample t-test n=500+500; t="
             ++ show (trStatistic r) ++ " p=" ++ show (trPValue r)) ]

benchMannWhitney :: IO [BenchRow]
benchMannWhitney = do
  let xs = syntheticVec 500 0
      ys = shifted 0.3 (syntheticVec 500 1000)
      run :: Int -> IO TestResult
      run _ = return (mannWhitneyU xs ys TwoSided)
      probe r = trStatistic r
  (ms, r) <- timeitTastyIO probe run
  return [ BenchRow "haskell" "stat_util"
            "MannWhitneyU_n500x500" ms (trStatistic r) (trPValue r)
            ("Mann-Whitney U n=500+500; U=" ++ show (trStatistic r)
             ++ " p=" ++ show (trPValue r)) ]

benchKS :: IO [BenchRow]
benchKS = do
  let xs = syntheticVec 1000 0
      run :: Int -> IO TestResult
      run _ = return (kolmogorovSmirnovNormal xs)
      probe r = trStatistic r
  (ms, r) <- timeitTastyIO probe run
  return [ BenchRow "haskell" "stat_util"
            "KS_normal_n1000" ms (trStatistic r) (trPValue r)
            ("KS test against Normal(μ̂, σ̂); D="
             ++ show (trStatistic r) ++ " p=" ++ show (trPValue r)) ]

benchBH :: IO [BenchRow]
benchBH = do
  -- Mix of "true null" (uniform) and "alternative" (small) p-values.
  -- P39 (2026-05-07): pre-construct the input as a 'VU.Vector Double'
  -- and call 'benjaminiHochbergV' directly. Matches Python's harness
  -- (which has a pre-built @np.array@ of p-values), so the timer only
  -- captures the BH algorithm itself rather than @[Double]@↔Vector
  -- conversion overhead.
  let n  = 1000
      psV = VU.generate n $ \i ->
              if i < 100 then 0.001 + 0.0001 * fromIntegral i
                         else 0.5 + 0.4 * sin (fromIntegral i)
      run :: Int -> IO (VU.Vector Double)
      run _ = return (benjaminiHochbergV psV)
      probe r = VU.sum r / fromIntegral (VU.length r)
  (ms, adjV) <- timeitTastyIO probe run
  let nSig = VU.length (VU.filter (< 0.05) adjV)
  return [ BenchRow "haskell" "stat_util"
            "BH_pAdjust_n1000" ms (fromIntegral nSig) 0
            ("BH on n=1000 p-values (VU API); significant="
             ++ show nSig) ]

benchHalton :: IO [BenchRow]
benchHalton = do
  -- P41 (2026-05-07): use the flat Matrix API to match Python's
  -- @ndarray@ baseline (scipy returns @(n, d)@ ndarray, summed via
  -- @pts.sum()@). The legacy @[[Double]]@ form added ~1.3 ms of
  -- @n × d@ list-cell + boxed-Double allocation.
  let run :: Int -> IO (LA.Matrix Double)
      run _ = return (haltonMatrix 10000 5)
      -- Force every element via BLAS sumElements (same as np.sum).
      probe = LA.sumElements
  (ms, mat) <- timeitTastyIO probe run
  return [ BenchRow "haskell" "stat_util"
            "Halton_n10000_d5" ms (fromIntegral (LA.rows mat)) 5
            "Halton quasi-random n=10000 d=5 (flat Matrix)" ]

benchAUC :: IO [BenchRow]
benchAUC = do
  let n      = 10000
      -- Deterministic logits from sin(i); labels = (logit > 0).
      logits = [ sin (fromIntegral i * 0.31) | i <- [0 .. n - 1] ]
      probs  = [ 1 / (1 + exp (-z)) | z <- logits ]
      labels = [ if z > 0 then 1 else 0 :: Int | z <- logits ]
      run :: Int -> IO (Double, Double)
      run _ = return (auc labels probs, logLoss labels probs)
      probe = fst
  (ms, (a, ll)) <- timeitTastyIO probe run
  return [ BenchRow "haskell" "stat_util"
            "AUC_LogLoss_n10000" ms a ll
            ("AUC=" ++ show a ++ " logLoss=" ++ show ll) ]

benchKFold :: IO [BenchRow]
benchKFold = do
  let run :: Int -> IO Int
      run _ = do
        gen <- MWC.create
        folds <- kFold 5 1000 gen
        -- Force the full list of fold indices.
        return $! sum [ length (fst f) + length (snd f) | f <- folds ]
      probe = fromIntegral
  (ms, k) <- timeitTastyIO probe run
  return [ BenchRow "haskell" "stat_util"
            "KFold_5_n1000" ms (fromIntegral k) 0
            ("k-fold split: 5 folds on n=1000") ]

-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  rows <- mconcat <$> sequence
    [ benchBootstrap
    , benchTTestWelch
    , benchMannWhitney
    , benchKS
    , benchBH
    , benchHalton
    , benchAUC
    , benchKFold
    ]
  writeRows "bench/results/haskell/stat_util.csv" rows
  putStrLn $ "wrote " ++ show (length rows)
          ++ " rows → bench/results/haskell/stat_util.csv"
