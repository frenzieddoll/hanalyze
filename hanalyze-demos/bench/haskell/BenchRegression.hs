{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-full-laziness -fno-cse #-}
-- | Regression benchmarks (B1).
--
-- LM, Logistic GLM, Poisson GLM, Gaussian LME (GLMM), Ridge, Lasso,
-- ElasticNet on the shared @bench/data/*.csv@ files. Outputs the unified
-- BenchRow CSV at @bench/results/haskell/regression.csv@.

module Main where

import qualified Data.Vector             as V
import qualified Data.Text               as T
import qualified Numeric.LinearAlgebra   as LA

import           Hanalyze.Model.Core              (FitResult (..))
import           Hanalyze.Model.LM                (fitLMVec)
import           Hanalyze.Model.GLM               (Family (..), LinkFn (..), fitGLMFull)
import qualified Hanalyze.Model.GLMM              as GLMM
import qualified Hanalyze.Model.Regularized       as Reg

import           BenchUtil

-- ---------------------------------------------------------------------------
-- Memoization-defeating wrappers.
--
-- GHC will common-subexpression-eliminate @fitLMVec xWith1 y@ across
-- iterations because the function is pure and the inputs are bound once
-- outside the loop. Each wrapper takes the iteration index as a phantom
-- argument and is marked NOINLINE so the optimizer cannot see through it.
-- ---------------------------------------------------------------------------

{-# NOINLINE fitLMVecPhantom #-}
fitLMVecPhantom :: Int -> LA.Matrix Double -> LA.Vector Double -> FitResult
fitLMVecPhantom _ x y = fitLMVec x y

{-# NOINLINE fitGLMFullPhantom #-}
fitGLMFullPhantom :: Int -> Family -> LinkFn
                  -> LA.Matrix Double -> LA.Vector Double -> FitResult
fitGLMFullPhantom _ fam link x y = fst (fitGLMFull fam link x y)

{-# NOINLINE fitLMEPhantom #-}
fitLMEPhantom :: Int -> LA.Matrix Double -> LA.Vector Double
              -> V.Vector Int -> V.Vector T.Text -> V.Vector Int
              -> GLMM.GLMMResult
fitLMEPhantom _ x y idx labels sizes = GLMM.fitLME x y idx labels sizes

{-# NOINLINE fitRegPhantom #-}
fitRegPhantom :: Int -> Reg.Penalty
              -> LA.Matrix Double -> LA.Vector Double -> Reg.RegFit
-- Match the Python-side bench's @max_iter=200, tol=1e-4@. The previous
-- run used hanalyze's hardcoded @1000 / 1e-7@ which made tol 1000×
-- stricter than sklearn's bench setting and gave an unfair speed
-- comparison.
fitRegPhantom _ pen x y = Reg.fitRegularizedWith 200 1e-4 pen x y

main :: IO ()
main = do
  rows <- mconcat <$> sequence
    [ benchLM "bench/data/lm_n1000_p5.csv"      "LM_n1000_p5"
    , benchLM "bench/data/lm_n10000_p50.csv"    "LM_n10000_p50"
    , benchLM "bench/data/lm_n100000_p100.csv"  "LM_n100000_p100"
    , benchLogistic "bench/data/logistic_n2000_p10.csv"  "GLM_logit_n2000_p10"
    , benchLogistic "bench/data/logistic_n10000_p20.csv" "GLM_logit_n10000_p20"
    , benchPoisson  "bench/data/poisson_n2000_p10.csv"   "GLM_poisson_n2000_p10"
    , benchPoisson  "bench/data/poisson_n10000_p20.csv"  "GLM_poisson_n10000_p20"
    , benchLME "bench/data/glmm_n2000_p5_g20.csv"    "LME_n2000_p5_g20"
    , benchLME "bench/data/glmm_n10000_p10_g50.csv"  "LME_n10000_p10_g50"
    , benchRidge "bench/data/lm_n1000_p5.csv"     "Ridge_n1000_p5"      1.0
    , benchRidge "bench/data/lm_n10000_p50.csv"   "Ridge_n10000_p50"    1.0
    , benchLasso "bench/data/lm_n1000_p5.csv"     "Lasso_n1000_p5"      0.05
    , benchLasso "bench/data/lm_n10000_p50.csv"   "Lasso_n10000_p50"    0.05
    , benchEN    "bench/data/lm_n1000_p5.csv"     "EN_n1000_p5"         0.05 0.05
    , benchEN    "bench/data/lm_n10000_p50.csv"   "EN_n10000_p50"       0.05 0.05
    ]
  writeRows "bench/results/haskell/regression.csv" rows
  putStrLn $ "wrote " ++ show (length rows)
          ++ " rows → bench/results/haskell/regression.csv"

-- ---------------------------------------------------------------------------
-- LM (OLS)
-- ---------------------------------------------------------------------------

benchLM :: FilePath -> String -> IO [BenchRow]
benchLM path name = do
  (x, y) <- readCsvXY path
  let xWith1 = LA.fromBlocks [[ LA.konst 1 (LA.rows x, 1), x ]]
  (ms, fr) <- timeitTastyIO forceFR
                (\i -> return $! fitLMVecPhantom i xWith1 y)
  let yhat = LA.flatten (fitted fr LA.¿ [0])
      r2   = computeR2 y yhat
      rmse = sqrt (LA.sumElements ((y - yhat) ** 2) / fromIntegral (LA.size y))
  return [ BenchRow "haskell" "regression" name ms r2 rmse "fitLM (OLS)" ]

-- ---------------------------------------------------------------------------
-- GLM (Logistic / Poisson, IRLS)
-- ---------------------------------------------------------------------------

benchLogistic :: FilePath -> String -> IO [BenchRow]
benchLogistic = benchGLM Binomial Logit "fitGLM Binomial/Logit"

benchPoisson :: FilePath -> String -> IO [BenchRow]
benchPoisson = benchGLM Poisson Log "fitGLM Poisson/Log"

benchGLM
  :: Family -> LinkFn -> String -> FilePath -> String -> IO [BenchRow]
benchGLM fam link extra path name = do
  (x, y) <- readCsvXY path
  let xWith1 = LA.fromBlocks [[ LA.konst 1 (LA.rows x, 1), x ]]
  (ms, fr) <- timeitTastyIO forceFR
                (\i -> return $! fitGLMFullPhantom i fam link xWith1 y)
  let yhat = LA.flatten (fitted fr LA.¿ [0])
      r2   = computeR2 y yhat
      rmse = sqrt (LA.sumElements ((y - yhat) ** 2) / fromIntegral (LA.size y))
  return [ BenchRow "haskell" "regression" name ms r2 rmse extra ]

-- ---------------------------------------------------------------------------
-- LME (Gaussian, exact EM)
-- ---------------------------------------------------------------------------

benchLME :: FilePath -> String -> IO [BenchRow]
benchLME path name = do
  (x, gIdxs, y) <- readCsvXYG path
  let xWith1 = LA.fromBlocks [[ LA.konst 1 (LA.rows x, 1), x ]]
      uniq   = uniqueInts (V.toList gIdxs)
      labels = V.fromList (map (T.pack . ('g' :) . show) uniq)
      sizes  = V.fromList
        [ length (filter (== g) (V.toList gIdxs)) | g <- uniq ]
  (ms, fit) <- timeitTastyIO (\f -> LA.sumElements (coefficients (GLMM.glmmFixed f)))
                (\i -> return $! fitLMEPhantom i xWith1 y gIdxs labels sizes)
  let yhat = LA.flatten (fitted (GLMM.glmmFixed fit) LA.¿ [0])
      r2   = computeR2 y yhat
  return
    [ BenchRow "haskell" "regression" name ms r2 (GLMM.glmmICC fit)
        "fitLME exact EM" ]
  where
    uniqueInts = foldr (\a acc -> if a `elem` acc then acc else a : acc) []

-- ---------------------------------------------------------------------------
-- Ridge / Lasso / ElasticNet
-- ---------------------------------------------------------------------------

benchRidge :: FilePath -> String -> Double -> IO [BenchRow]
benchRidge = benchPenalty (\lam -> Reg.L2 lam)
                          (\lam -> "fitRidge lambda=" ++ show lam)

benchLasso :: FilePath -> String -> Double -> IO [BenchRow]
benchLasso = benchPenalty (\lam -> Reg.L1 lam)
                          (\lam -> "fitLasso lambda=" ++ show lam ++ " (CD)")

benchEN :: FilePath -> String -> Double -> Double -> IO [BenchRow]
benchEN path name lam1 lam2 = do
  (x, y) <- readCsvXY path
  (ms, fr) <- timeitTastyIO forceReg
                (\i -> return $! fitRegPhantom i (Reg.ElasticNet lam1 lam2) x y)
  let yhat = Reg.predictRegularized fr x
      r2   = computeR2 y yhat
      rmse = sqrt (LA.sumElements ((y - yhat) ** 2) / fromIntegral (LA.size y))
  return [ BenchRow "haskell" "regression" name ms r2 rmse
                     ("fitElasticNet lam1=" ++ show lam1
                      ++ " lam2=" ++ show lam2) ]

benchPenalty
  :: (Double -> Reg.Penalty)
  -> (Double -> String)
  -> FilePath -> String -> Double -> IO [BenchRow]
benchPenalty mkPen mkExtra path name lam = do
  (x, y) <- readCsvXY path
  (ms, fr) <- timeitTastyIO forceReg
                (\i -> return $! fitRegPhantom i (mkPen lam) x y)
  let yhat = Reg.predictRegularized fr x
      r2   = computeR2 y yhat
      rmse = sqrt (LA.sumElements ((y - yhat) ** 2) / fromIntegral (LA.size y))
  return [ BenchRow "haskell" "regression" name ms r2 rmse (mkExtra lam) ]

-- ---------------------------------------------------------------------------

forceFR :: FitResult -> Double
forceFR fr = LA.sumElements (coefficients fr)
           + LA.sumElements (residuals fr)

forceReg :: Reg.RegFit -> Double
forceReg fr = LA.sumElements (Reg.rfBeta fr)
            + LA.sumElements (Reg.rfYHat fr)

computeR2 :: LA.Vector Double -> LA.Vector Double -> Double
computeR2 y yhat =
  let mu  = LA.sumElements y / fromIntegral (LA.size y)
      sst = LA.sumElements ((y - LA.konst mu (LA.size y)) ** 2)
      sse = LA.sumElements ((y - yhat) ** 2)
  in if sst == 0 then 0 else 1 - sse / sst
