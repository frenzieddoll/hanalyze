{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-full-laziness -fno-cse #-}
-- | tasty-bench based microbenchmarks for hot paths affected by
-- Phase 1-7 perf optimizations (-O2, StrictData, INLINE).
--
-- Run with:
--
-- > OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 \
-- >   cabal run bench-tasty -- --csv bench/results/tasty.csv
--
-- The CSV output is comparable across builds (same data fixtures and
-- single-thread BLAS). Use @--baseline=path.csv --fail-if-slower=N@ to
-- guard against regressions.
module Main where

import qualified Numeric.LinearAlgebra   as LA
import           Test.Tasty.Bench

import           Model.Core              (coefficients)
import           Model.GLM               (Family (..), LinkFn (..), fitGLMFull)
import qualified Model.Regularized       as Reg
import           Model.Regularized       (Penalty (..), rfBeta)
import qualified Model.Kernel            as Kn
import qualified Stat.Cholesky           as Chol
import qualified Stat.KernelDist         as KD
import           Data.Maybe              (fromMaybe)

import           BenchUtil               (readCsvXY)

makeSpd :: Int -> LA.Matrix Double
makeSpd n =
  let g = LA.build (n, n)
            (\i j -> exp (-(((i - j) * (i - j)) / fromIntegral n)))
  in g + LA.scale 1e-3 (LA.ident n)

main :: IO ()
main = do
  (xLogi, yLogi) <- readCsvXY "bench/data/logistic_n10000_p20.csv"
  (xKR,   yKR)   <- readCsvXY "bench/data/kernel_n1000_p5.csv"
  (xLas,  yLas)  <- readCsvXY "bench/data/lm_n10000_p50.csv"
  let yKRMat   = LA.asColumn yKR
      spd500   = makeSpd 500
      spdRhs   = LA.asColumn (LA.fromList (replicate 500 1.0))
      kdInput2k = LA.fromLists
        [[fromIntegral i + 0.1 * fromIntegral j | j <- [0 .. 4]] | i <- [0 .. 1999]]

  defaultMain
    [ bgroup "regression"
        [ bench "GLM_logit_n10000_p20" $
            nf (\() -> LA.sumElements
                         (coefficients (fst (fitGLMFull Binomial Logit
                                              xLogi yLogi)))) ()
        , bench "Lasso_n10000_p50_lam0.1" $
            nf (\() -> LA.sumElements
                         (rfBeta (Reg.fitRegularized (L1 0.1) xLas yLas))) ()
        ]
    , bgroup "kernel"
        [ bench "KernelRidgeMV_n1000_p5_RBF" $
            nf (\() -> LA.sumElements
                         (Kn.krmvAlpha (Kn.kernelRidgeMV Kn.Gaussian 1.0 1e-3
                                          xKR yKRMat))) ()
        , bench "pairwiseSqDist_n2000_p5" $
            nf (LA.sumElements . KD.pairwiseSqDist) kdInput2k
        , bench "gramMatrixMV_n1000_p5_RBF" $
            nf (\() -> LA.sumElements (Kn.gramMatrixMV Kn.Gaussian 1.0 xKR)) ()
        ]
    , bgroup "cholesky"
        [ bench "cholSolve_n500" $
            nf (\() -> LA.sumElements
                         (fromMaybe (LA.scalar 0)
                                    (Chol.cholSolve spd500 spdRhs))) ()
        ]
    ]
