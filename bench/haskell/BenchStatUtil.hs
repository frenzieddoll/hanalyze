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
import qualified Numeric.LinearAlgebra   as LA
import qualified System.Random.MWC       as MWC

import           Stat.Bootstrap          (bootstrapCI, sampleMean)
import           Stat.Test               (Alternative (..),
                                          tTestWelch, mannWhitneyU,
                                          kolmogorovSmirnovNormal,
                                          TestResult (..))
import           Stat.MultipleTesting    (CorrectionMethod (..), pAdjust)
import           Stat.QuasiRandom        (haltonSequence)
import           Stat.ClassMetrics       (auc, logLoss)
import           Stat.CV                 (kFold)

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
        bootstrapCI 1000 0.95 sampleMean xs gen
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
  let n  = 1000
      ps = [ if i < 100 then 0.001 + 0.0001 * fromIntegral i
                        else 0.5 + 0.4 * sin (fromIntegral i)
           | i <- [0 .. n - 1] ]
      run :: Int -> IO [Double]
      run _ = return (pAdjust BenjaminiHochberg ps)
      probe r = sum r / fromIntegral (length r)
  (ms, adj) <- timeitTastyIO probe run
  let nSig = length (filter (< 0.05) adj)
  return [ BenchRow "haskell" "stat_util"
            "BH_pAdjust_n1000" ms (fromIntegral nSig) 0
            ("BH on n=1000 p-values; significant after adj="
             ++ show nSig) ]

benchHalton :: IO [BenchRow]
benchHalton = do
  let run :: Int -> IO [[Double]]
      run _ = return (haltonSequence 10000 5)
      -- Force every element of every point to avoid laziness skipping work.
      probe pts = sum [ s | p <- pts, let s = sum p ]
  (ms, pts) <- timeitTastyIO probe run
  return [ BenchRow "haskell" "stat_util"
            "Halton_n10000_d5" ms (fromIntegral (length pts)) 5
            "Halton quasi-random n=10000 d=5" ]

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
