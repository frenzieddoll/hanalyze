{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-full-laziness -fno-cse #-}
-- | Classical-ML benchmarks (B6).
--
-- Compares hanalyze's Model.{PCA, Cluster, DecisionTree, RandomForest}
-- against scikit-learn on shared CSV inputs:
--
--   PCA       : lm_n10000_p50.csv (X only), 5 components
--   KMeans    : kernel_n2000_p5.csv (X only), k=5
--   DT / RF   : logistic_n10000_p20.csv (binary y), p=20
--
-- Outputs the unified BenchRow CSV at @bench/results/haskell/ml.csv@.
module Main where

import qualified Data.Vector             as V
import qualified Numeric.LinearAlgebra   as LA
import qualified System.Random.MWC       as MWC

import qualified Hanalyze.Model.PCA               as PCA
import qualified Hanalyze.Model.Cluster           as Cl
import qualified Hanalyze.Model.DecisionTree      as DT
import qualified Hanalyze.Model.RandomForest      as RF

import           BenchUtil

-- ---------------------------------------------------------------------------
-- Phantom wrappers (defeat CSE across iterations)
-- ---------------------------------------------------------------------------

{-# NOINLINE pcaPhantom #-}
pcaPhantom :: Int -> Int -> LA.Matrix Double -> PCA.PCAResult
pcaPhantom _ k x = PCA.pca PCA.CenterScale (Just k) x

{-# NOINLINE kmeansPhantom #-}
kmeansPhantom :: Int -> Int -> LA.Matrix Double -> MWC.GenIO -> IO Cl.KMeansResult
kmeansPhantom _ k x gen = Cl.kMeans (Cl.defaultKMeansConfig k) x gen

{-# NOINLINE dtPhantom #-}
dtPhantom :: Int -> [[Double]] -> [Int] -> DT.DTree
dtPhantom _ xs ys = DT.fitDT DT.defaultDTConfig xs ys

{-# NOINLINE rfPhantom #-}
rfPhantom :: Int -> [[Double]] -> [Double] -> MWC.GenIO -> IO RF.RandomForest
rfPhantom _ xs ys gen =
  RF.fitRF RF.defaultRFConfig { RF.rfTrees = 20 } xs ys gen

-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  rows <- mconcat <$> sequence
    [ benchPCA      "bench/data/lm_n10000_p50.csv"       "PCA_n10000_p50_k5"  5
    , benchKMeans   "bench/data/kernel_n2000_p5.csv"     "KMeans_n2000_p5_k5" 5
    -- DT/RF use list-based [[Double]] APIs internally; we cap at
    -- n=2000 p=10 so the bench finishes in reasonable time. The Python
    -- side uses the same fixture for fairness.
    , benchDT       "bench/data/logistic_n2000_p10.csv"  "DT_n2000_p10"
    , benchRF       "bench/data/logistic_n2000_p10.csv"  "RF_n2000_p10_t20"
    ]
  writeRows "bench/results/haskell/ml.csv" rows
  putStrLn $ "wrote " ++ show (length rows)
          ++ " rows → bench/results/haskell/ml.csv"

-- ---------------------------------------------------------------------------
-- PCA
-- ---------------------------------------------------------------------------

benchPCA :: FilePath -> String -> Int -> IO [BenchRow]
benchPCA path name k = do
  (x, _y) <- readCsvXY path
  (ms, res) <- timeitTastyIO probe
                 (\i -> return $! pcaPhantom i k x)
  let ratio = LA.sumElements (PCA.pcaExplainedRatio res)
      sigma = LA.sumElements (PCA.pcaSingularValues res)
  return [ BenchRow "haskell" "ml" name ms ratio sigma
            ("Hanalyze.Model.PCA k=" ++ show k ++ " standardized") ]
  where
    probe r = LA.sumElements (PCA.pcaExplainedRatio r)
            + LA.sumElements (PCA.pcaSingularValues r)

-- ---------------------------------------------------------------------------
-- KMeans
-- ---------------------------------------------------------------------------

benchKMeans :: FilePath -> String -> Int -> IO [BenchRow]
benchKMeans path name k = do
  (x, _y) <- readCsvXY path
  gen <- MWC.createSystemRandom
  (ms, res) <- timeitTastyIO probe
                 (\i -> kmeansPhantom i k x gen)
  let inert = Cl.kmrInertia res
      iters = fromIntegral (Cl.kmrIters res)
  return [ BenchRow "haskell" "ml" name ms inert iters
            ("Hanalyze.Model.Cluster.kMeans k=" ++ show k) ]
  where
    probe r = Cl.kmrInertia r

-- ---------------------------------------------------------------------------
-- DecisionTree (classification)
-- ---------------------------------------------------------------------------

benchDT :: FilePath -> String -> IO [BenchRow]
benchDT path name = do
  (x, y) <- readCsvXY path
  let xs   = LA.toLists x
      ys   = map (round :: Double -> Int) (LA.toList y)
  (ms, tree) <- timeitTastyIO probe
                  (\i -> return $! dtPhantom i xs ys)
  let acc = let preds = [ DT.predictDT tree row | row <- xs ]
                hits  = length (filter id (zipWith (==) preds ys))
            in fromIntegral hits / fromIntegral (length ys) :: Double
  return [ BenchRow "haskell" "ml" name ms acc 0
            "Hanalyze.Model.DecisionTree.fitDT default config" ]
  where
    -- Force tree by predicting on the first row.
    probe t = case [ DT.predictDT t [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0] ] of
                (r:_) -> fromIntegral r
                _     -> 0

-- ---------------------------------------------------------------------------
-- RandomForest (regression on binary y; same accuracy metric via threshold 0.5)
-- ---------------------------------------------------------------------------

benchRF :: FilePath -> String -> IO [BenchRow]
benchRF path name = do
  (x, y) <- readCsvXY path
  let xs = LA.toLists x
      ys = LA.toList y
  gen <- MWC.createSystemRandom
  (ms, forest) <- timeitTastyIO probe
                    (\i -> rfPhantom i xs ys gen)
  let preds = map (RF.predictRF forest) xs
      yi    = map (round :: Double -> Int) ys
      pi'   = map (\p -> if p > 0.5 then 1 else 0 :: Int) preds
      hits  = length (filter id (zipWith (==) pi' yi))
      acc   = fromIntegral hits / fromIntegral (length ys) :: Double
  return [ BenchRow "haskell" "ml" name ms acc 0
            "Hanalyze.Model.RandomForest.fitRF (20 trees)" ]
  where
    probe forest = case xs of
      (row:_) -> RF.predictRF forest row
      _       -> 0
      where xs = [[0.0 :: Double | _ <- [0 :: Int .. 19]]]
