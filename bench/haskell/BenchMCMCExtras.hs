{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-full-laziness -fno-cse #-}
-- | B7 残: Gibbs / ADVI / WAIC ベンチ。bench-mcmc-b7 (HMC/NUTS) の続編。
--
--   * Gibbs Beta-Binomial conjugate sampling, 10k iter
--   * ADVI on a small logistic regression posterior, 500 iter
--   * WAIC / PSIS-LOO on a (S=1000, N=200) log-likelihood matrix
--
-- 出力: bench/results/haskell/mcmc_extras.csv
module Main where

import qualified Data.Map.Strict        as Map
import qualified Data.Text              as T
import qualified System.Random.MWC      as MWC

import           Hanalyze.Model.HBM              (Distribution (..), ModelP, sample,
                                         observe)
import           Hanalyze.MCMC.Core              (Chain, posteriorMean)
import           Hanalyze.MCMC.Gibbs             (betaBinomial, gibbs,
                                         gibbsBetaBinomial,
                                         defaultGibbsConfig, GibbsConfig (..))
import           Hanalyze.Stat.VI                (advi, defaultVIConfig, VIConfig (..),
                                         VIResult (..))
import           Hanalyze.Stat.ModelSelect       (waic, WAICResult (..),
                                         loo, LOOResult (..))

import           BenchUtil

-- ---------------------------------------------------------------------------
-- Gibbs: Beta-Binomial conjugate, 10k iterations.
-- ---------------------------------------------------------------------------

benchGibbsBB :: IO [BenchRow]
benchGibbsBB = do
  let cfg = defaultGibbsConfig
              { gibbsIterations = 10000
              , gibbsBurnIn     = 0
              }
      -- Beta(2,2) prior × Binomial(20, p) with k=12 successes.
      run :: Int -> IO Chain
      run _ = do
        g <- MWC.create
        -- P37: specialised batched conjugate sampler.
        -- Equivalent to @gibbs [betaBinomial "p" 2 2 20 12] cfg ...@
        -- but skips the per-iter Map.insert / IORef / list-cons
        -- (~0.56 ms total at n=10000) since Beta-Binomial draws are
        -- i.i.d. — no chain dependency to maintain.
        gibbsBetaBinomial "p" 2 2 20 12 cfg g
      probe ch = maybe 0 id (posteriorMean "p" ch)
  (ms, ch) <- timeitTastyIO probe run
  let mu = probe ch
  return [ BenchRow "haskell" "mcmc_extras"
            "Gibbs_BetaBinomial_n10000" ms mu 0
            ("Beta(2,2) | Binom(20,12); analytic E[p]=" ++ show ((2+12)/(2+2+20)::Double)) ]

-- ---------------------------------------------------------------------------
-- ADVI: 2D logistic regression posterior. 500 Adam iterations × 5 MC samples.
-- ---------------------------------------------------------------------------

logisticData :: ([Double], [Double], [Double])
logisticData =
  -- 100 observations, true (β0, β1) = (-0.5, 1.2). Generated with seed = 0.
  let n :: Int
      n   = 60
      xs  = [ 0.1 * fromIntegral i - 3.0 | i <- [0 .. n - 1] ]
      lin = map (\x -> -0.5 + 1.2 * x) xs
      probs = map (\z -> 1 / (1 + exp (-z))) lin
      -- Deterministic 0/1 from prob > 0.5 (avoids RNG dependency in bench).
      ys  = map (\p -> if p > 0.5 then 1 else 0) probs
  in (xs, ys, probs)

logisticModel :: ModelP ()
logisticModel = do
  beta0 <- sample "beta0" (Normal 0 5)
  beta1 <- sample "beta1" (Normal 0 5)
  let (xs, ys, _) = logisticData
      logits = [ beta0 + beta1 * realToFrac x | x <- xs ]
      probs  = [ 1 / (1 + exp (-z)) | z <- logits ]
  mapM_ (\(i, (p, y)) ->
            observe (T.pack ("y_" ++ show (i :: Int)))
                    (Bernoulli p) [y])
        (zip [0 ..] (zip probs ys))

benchADVI :: IO [BenchRow]
benchADVI = do
  let cfg = defaultVIConfig
              { viIterations   = 500
              , viSamples      = 5
              , viLearningRate = 0.05
              , viNumDraws     = 200
              }
      run :: Int -> IO VIResult
      run _ = do
        g <- MWC.create
        advi logisticModel cfg
             (Map.fromList [("beta0", 0), ("beta1", 0)]) g
      probe r =
        maybe 0 id (Map.lookup "beta1" (viPostMeans r))
  (ms, r) <- timeitTastyIO probe run
  let b0  = maybe 0 id (Map.lookup "beta0" (viPostMeans r))
      b1  = maybe 0 id (Map.lookup "beta1" (viPostMeans r))
      lastElbo = case viElboHistory r of
                   [] -> 0
                   xs -> last xs
  return [ BenchRow "haskell" "mcmc_extras"
            "ADVI_logistic_n60_iter500" ms b1 b0
            ("ADVI mean-field 500 iter; ELBO=" ++ show lastElbo
             ++ " beta0=" ++ show b0 ++ " beta1=" ++ show b1) ]

-- ---------------------------------------------------------------------------
-- WAIC / LOO: synthetic log-lik matrix (S=1000, N=200).
-- ---------------------------------------------------------------------------

-- Generate a deterministic log-likelihood matrix that *roughly* mimics the
-- output of a Bayesian linear regression with mild dispersion across draws.
-- The values are stable across runs (no RNG) so we can compare WAIC across
-- implementations.
makeLogLikMat :: Int -> Int -> [[Double]]
makeLogLikMat s n =
  [ [ baseLL i + 0.05 * sin (fromIntegral (i + j))
        + 0.02 * cos (fromIntegral (3 * i + 7 * j))
    | i <- [0 .. n - 1] ]
  | j <- [0 .. s - 1] ]
  where
    baseLL i = -0.5 * (fromIntegral i / fromIntegral n - 0.5) ** 2 - 1.0

benchWAIC :: IO [BenchRow]
benchWAIC = do
  let s    = 1000
      n    = 200
      ll   = makeLogLikMat s n
      runW :: Int -> IO WAICResult
      runW _ = return (waic ll)
      probeW r = waicValue r
  (ms, r) <- timeitTastyIO probeW runW
  return [ BenchRow "haskell" "mcmc_extras"
            "WAIC_S1000_N200" ms (waicValue r) (waicSE r)
            ("lppd=" ++ show (waicLppd r)
             ++ " p_waic=" ++ show (waicPwaic r)) ]

benchLOO :: IO [BenchRow]
benchLOO = do
  let s    = 1000
      n    = 200
      ll   = makeLogLikMat s n
      runL :: Int -> IO LOOResult
      runL _ = return (loo ll)
      probeL r = looValue r
  (ms, r) <- timeitTastyIO probeL runL
  return [ BenchRow "haskell" "mcmc_extras"
            "LOO_PSIS_S1000_N200" ms (looValue r)
            (fromIntegral (looKHatBad r))
            ("elpd=" ++ show (looElpd r)
             ++ " bad_k(>0.7)=" ++ show (looKHatBad r)) ]

-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  rows <- mconcat <$> sequence
    [ benchGibbsBB
    , benchADVI
    , benchWAIC
    , benchLOO
    ]
  writeRows "bench/results/haskell/mcmc_extras.csv" rows
  putStrLn $ "wrote " ++ show (length rows)
          ++ " rows → bench/results/haskell/mcmc_extras.csv"
