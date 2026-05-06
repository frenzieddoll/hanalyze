{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-full-laziness -fno-cse #-}
-- | Focused profile runner for the highest-allocation benchmarks
-- identified by tasty-bench (KernelRidgeMV, gramMatrixMV, GLM_logit).
--
-- Build with profiling:
--
-- > cabal build --enable-profiling --enable-library-profiling bench-profile
--
-- Run for time / cost-center profile:
--
-- > OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 \
-- >   $(cabal list-bin --enable-profiling bench-profile) \
-- >     +RTS -p -RTS <target>
--
-- Run for heap profile (by cost-center):
--
-- > $(cabal list-bin --enable-profiling bench-profile) \
-- >     +RTS -hc -L80 -RTS <target>
-- > hp2ps -e8in -c bench-profile.hp
--
-- targets: kr | gram | glm | lasso | psd
module Main where

import           Control.DeepSeq         (deepseq)
import           Control.Monad           (replicateM_)
import qualified Numeric.LinearAlgebra   as LA
import           System.Environment      (getArgs)

import           Model.Core              (coefficients)
import           Model.GLM               (Family (..), LinkFn (..), fitGLMFull)
import qualified Model.Regularized       as Reg
import           Model.Regularized       (Penalty (..), rfBeta)
import qualified Model.Kernel            as Kn
import qualified Stat.KernelDist         as KD

import           BenchUtil               (readCsvXY)

-- Probe to a Double scalar so the entire result is forced through NF
-- by pulling a numeric field. (Some result types lack an NFData
-- instance so we evaluate the probe value to NF instead.)
runN :: Int -> (a -> Double) -> IO a -> IO ()
runN n force action =
  replicateM_ n $ do
    x <- action
    let s = force x
    s `deepseq` pure ()

main :: IO ()
main = do
  args <- getArgs
  let target = case args of
        (t : _) -> t
        _       -> "kr"
  case target of
    "kr" -> do
      (xKR, yKR) <- readCsvXY "bench/data/kernel_n1000_p5.csv"
      let yMat = LA.asColumn yKR
      runN 30 (LA.sumElements . Kn.krmvAlpha) $
        pure $! Kn.kernelRidgeMV Kn.Gaussian 1.0 1e-3 xKR yMat
    "gram" -> do
      (xKR, _) <- readCsvXY "bench/data/kernel_n1000_p5.csv"
      runN 50 LA.sumElements $
        pure $! Kn.gramMatrixMV Kn.Gaussian 1.0 xKR
    "glm" -> do
      (xL, yL) <- readCsvXY "bench/data/logistic_n10000_p20.csv"
      runN 100 (LA.sumElements . coefficients) $
        pure $! fst (fitGLMFull Binomial Logit xL yL)
    "lasso" -> do
      (xL, yL) <- readCsvXY "bench/data/lm_n10000_p50.csv"
      runN 200 (LA.sumElements . rfBeta) $
        pure $! Reg.fitRegularized (L1 0.1) xL yL
    "psd" -> do
      (xKR, _) <- readCsvXY "bench/data/kernel_n2000_p5.csv"
      runN 30 LA.sumElements $
        pure $! KD.pairwiseSqDist xKR
    _ -> putStrLn $ "unknown target: " ++ target
                 ++ "  (expected: kr | gram | glm | lasso | psd)"
