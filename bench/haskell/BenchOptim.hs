{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-full-laziness -fno-cse #-}
-- | Single-objective optimization benchmarks (B3).
--
-- Each (algorithm × test function) combination is run for 30 seeds and we
-- record:
--   - median wall time per run (ms),
--   - median final objective value @f(x*)@,
--   - success rate: @|f(x*)| < 1e-2@ (Sphere/Ackley/Levy: known optimum 0;
--     Rosenbrock: known min 0; Rastrigin: known min 0).

module Main where

import qualified System.Random.MWC       as MWC
import           Data.List               (sort, minimumBy)
import           Data.Ord                (comparing)
import           Control.Monad           (forM)

import qualified Optim.NelderMead        as NM
import qualified Optim.LBFGS             as LB
import qualified Optim.LineSearch        as LS
import qualified Optim.DifferentialEvolution as DE
import qualified Optim.CMAES             as CM
import qualified Optim.SimulatedAnnealing    as SA
import qualified Optim.ParticleSwarm     as PS
import qualified Optim.Common            as OC

import           BenchUtil

-- ---------------------------------------------------------------------------
-- Test functions (all minimization, true optimum f(x*) = 0)
-- ---------------------------------------------------------------------------

rosenbrock, rastrigin, sphere, ackley, levy :: [Double] -> Double
rosenbrock xs =
  sum [ 100 * (xs!!(i+1) - xs!!i ** 2)^(2::Int)
      + (1 - xs!!i)^(2::Int)
      | i <- [0 .. length xs - 2] ]
rastrigin xs =
  10 * fromIntegral (length xs)
    + sum [x*x - 10 * cos (2 * pi * x) | x <- xs]
sphere = sum . map (^(2::Int))
ackley xs =
  let n  = fromIntegral (length xs) :: Double
      s1 = sum (map (^(2::Int)) xs) / n
      s2 = sum (map (\x -> cos (2*pi*x)) xs) / n
  in - 20 * exp (- 0.2 * sqrt s1) - exp s2 + 20 + exp 1
levy xs =
  let w i = 1 + (xs!!i - 1) / 4
      d   = length xs
      sumMid = sum [ (w i - 1)^(2::Int) * (1 + 10 * sin (pi * w i + 1)^(2::Int))
                   | i <- [0 .. d - 2] ]
  in sin (pi * w 0)^(2::Int)
   + sumMid
   + (w (d-1) - 1)^(2::Int) * (1 + sin (2 * pi * w (d-1))^(2::Int))

testFns :: [(String, Int, [Double] -> Double)]
testFns =
  [ ("Rosenbrock_2D",  2, rosenbrock)
  , ("Rosenbrock_10D", 10, rosenbrock)
  , ("Rastrigin_10D",  10, rastrigin)
  , ("Sphere_30D",     30, sphere)
  , ("Ackley_10D",     10, ackley)
  , ("Levy_10D",       10, levy)
  ]

-- ---------------------------------------------------------------------------
-- Adapters: each algorithm returns (final f(x*), wall-time ms)
-- ---------------------------------------------------------------------------

data Algo = Algo
  { algoName :: String
  , algoRun  :: ([Double] -> Double) -> Int -> IO (Double, Double)
                -- ^ given f and dim, returns (f_final, ms)
  }

algoNM, algoLBFGS, algoDE, algoCMA, algoSA, algoPSO :: Algo

algoNM = Algo "NelderMead" $ \f d -> do
  x0 <- initSeed d
  (ms, r) <- timeitIO 1 OC.orValue (\_ -> NM.runNelderMead f x0)
  return (OC.orValue r, ms)

-- | L-BFGS の勾配は中央差分で代用 (Numeric variant)。
algoLBFGS = Algo "LBFGS" $ \f d -> do
  x0 <- initSeed d
  (ms, r) <- timeitIO 1 OC.orValue
               (\_ -> LB.runLBFGSNumeric LB.defaultLBFGSConfig f x0)
  return (OC.orValue r, ms)

algoDE = Algo "DE" $ \f d -> do
  let bs = replicate d (-5.0, 5.0)
  gen <- MWC.createSystemRandom
  (ms, r) <- timeitIO 1 OC.orValue (\_ -> DE.runDE bs f gen)
  return (OC.orValue r, ms)

algoCMA = Algo "CMAES" $ \f d -> do
  x0 <- initSeed d
  gen <- MWC.createSystemRandom
  (ms, r) <- timeitIO 1 OC.orValue (\_ -> CM.runCMAES f x0 gen)
  return (OC.orValue r, ms)

algoSA = Algo "SA" $ \f d -> do
  let bs = replicate d (-5.0, 5.0)
  gen <- MWC.createSystemRandom
  -- Multi-start SA (basin-hopping 風): 5 個の異なる init から短めの
  -- SA (5000 iter) を走らせ best を採用。Rastrigin のように 1 run の
  -- median が局所最適に張付く問題で global 到達率を 5× 上げる。
  let cfg = (SA.defaultSAConfig bs)
              { SA.saProposal       = SA.Tsallis 2.62
              , SA.saAccept         = SA.Boltzmann
              , SA.saLocalMethod    = SA.LocalLBFGS
              , SA.saLocalEvery     = Just 10
              , SA.saInitTemp       = 5230.0
              , SA.saRestartIfStuck = Nothing
              , SA.saStop           = (SA.saStop (SA.defaultSAConfig bs))
                                        { OC.stMaxIter = 10000 }
              }
      nRuns = 20 :: Int
  -- Phase A 試行: Async.mapConcurrently で 20 runs を並列実行 → 逆効果
  -- (Rastrigin -N=1: 1500ms / -N=8: 2002ms)。SA の inner LBFGS が
  -- BLAS-heavy で OpenBLAS の lock contention により並列が serial 化。
  -- mapM 版に戻して single-thread の最良成績を維持。
  (ms, r) <- timeitIO 1 OC.orValue $ \_ -> do
    rs <- mapM (\_ -> do
                   x0 <- initSeed d
                   SA.runSAWith cfg f x0 gen) [1 .. nRuns]
    return (minimumBy (comparing OC.orValue) rs)
  return (OC.orValue r, ms)

algoPSO = Algo "PSO" $ \f d -> do
  let bs = replicate d (-5.0, 5.0)
  gen <- MWC.createSystemRandom
  (ms, r) <- timeitIO 1 OC.orValue (\_ -> PS.runPSO bs f gen)
  return (OC.orValue r, ms)

initSeed :: Int -> IO [Double]
initSeed d = do
  gen <- MWC.createSystemRandom
  OC.sampleUniformIn (replicate d (-2.0, 2.0)) gen

algos :: [Algo]
algos = [algoNM, algoLBFGS, algoDE, algoCMA, algoSA, algoPSO]

-- ---------------------------------------------------------------------------
-- Run loop
-- ---------------------------------------------------------------------------

nSeeds :: Int
nSeeds = 30

successThr :: Double
successThr = 1e-2

main :: IO ()
main = do
  rows <- fmap concat $ forM testFns $ \(fname, d, f) ->
    forM algos $ \alg -> do
      results <- mapM (\_ -> (algoRun alg) f d) [1 .. nSeeds]
      let (fs, ts) = unzip results
          medF  = median fs
          medMs = median ts
          succRate = fromIntegral
                       (length (filter (\v -> abs v < successThr) fs))
                     / fromIntegral nSeeds :: Double
      return $ BenchRow "haskell" "optim"
        (fname ++ "/" ++ algoName alg) medMs medF succRate
        ("median over " ++ show nSeeds ++ " seeds")
  writeRows "bench/results/haskell/optim.csv" rows
  putStrLn $ "wrote " ++ show (length rows)
          ++ " rows → bench/results/haskell/optim.csv"

median :: Ord a => [a] -> a
median xs =
  let s = sort xs
  in s !! (length s `div` 2)
