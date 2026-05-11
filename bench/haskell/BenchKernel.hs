{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-full-laziness -fno-cse #-}
-- | Kernel / GP benchmarks (B2).

module Main where

import qualified Numeric.LinearAlgebra   as LA
import qualified System.Random.MWC       as MWC
import qualified Data.Vector             as V

import qualified Hanalyze.Model.Kernel            as Kn
import qualified Hanalyze.Model.GP                as GP
import qualified Hanalyze.Model.RFF               as RFF
import qualified Hanalyze.Model.GPRobust          as GPR

import           BenchUtil

-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  rows <- mconcat <$> sequence
    [ benchGram     "bench/data/kernel_n500_p5.csv"  "GramMV_n500_p5"
    , benchGram     "bench/data/kernel_n1000_p5.csv" "GramMV_n1000_p5"
    , benchGram     "bench/data/kernel_n2000_p5.csv" "GramMV_n2000_p5"
    , benchGram     "bench/data/kernel_n4000_p5.csv" "GramMV_n4000_p5"
    , benchKR       "bench/data/kernel_n500_p5.csv"  "KR_n500_p5"
    , benchKR       "bench/data/kernel_n1000_p5.csv" "KR_n1000_p5"
    , benchKR       "bench/data/kernel_n2000_p5.csv" "KR_n2000_p5"
    , benchKR       "bench/data/kernel_n4000_p5.csv" "KR_n4000_p5"
    , benchNW       "bench/data/kernel_n1000_p5.csv" "NW_n1000_p5"
    , benchRFF      "bench/data/kernel_n1000_p5.csv" "RFF_n1000_D256_p5"  256
    , benchRFF      "bench/data/kernel_n2000_p5.csv" "RFF_n2000_D256_p5"  256
    , benchGPFit    "bench/data/kernel_n500_p5.csv"  "GP_fit_n500_p5"
    , benchGPFit    "bench/data/kernel_n1000_p5.csv" "GP_fit_n1000_p5"
    , benchGPFit    "bench/data/kernel_n2000_p5.csv" "GP_fit_n2000_p5"
    , benchGPOpt    "bench/data/kernel_n500_p5.csv"  "GP_opt_n500_p5"
    , benchGPRobust "bench/data/kernel_n500_p5.csv"  "GPRobust_n500_p5"
    ]
  writeRows "bench/results/haskell/kernel.csv" rows
  putStrLn $ "wrote " ++ show (length rows)
          ++ " rows → bench/results/haskell/kernel.csv"

-- ---------------------------------------------------------------------------
-- 共通設定: Gaussian RBF, h = 1.0, λ = 1e-3
h0 :: Double
h0 = 1.0

lam0 :: Double
lam0 = 1e-3

-- ---------------------------------------------------------------------------
-- Gram matrix (BLAS pairwise dist + cmap)
-- ---------------------------------------------------------------------------

{-# NOINLINE gramPhantom #-}
gramPhantom :: Int -> Kn.Kernel -> Double
            -> LA.Matrix Double -> LA.Matrix Double
gramPhantom _ k h x = Kn.gramMatrixMV k h x

benchGram :: FilePath -> String -> IO [BenchRow]
benchGram path name = do
  (x, _) <- readCsvXY path
  (ms, g) <- timeitTastyIO LA.sumElements
               (\i -> return $! gramPhantom i Kn.Gaussian h0 x)
  return [ BenchRow "haskell" "kernel" name ms 0 0
            ("gramMatrixMV BLAS, n=" ++ show (LA.rows g)) ]

-- ---------------------------------------------------------------------------
-- Kernel Ridge fit (multi-input)
-- ---------------------------------------------------------------------------

{-# NOINLINE krPhantom #-}
krPhantom :: Int -> LA.Matrix Double -> LA.Matrix Double -> Kn.KernelRidgeFitMV
krPhantom _ x ym = Kn.kernelRidgeMV Kn.Gaussian h0 lam0 x ym

benchKR :: FilePath -> String -> IO [BenchRow]
benchKR path name = do
  (x, y) <- readCsvXY path
  let yMat = LA.asColumn y
  (ms, fit) <- timeitTastyIO (\f -> LA.sumElements (Kn.krmvAlpha f))
                 (\i -> return $! krPhantom i x yMat)
  let yhat = LA.flatten (Kn.fittedKernelRidgeMV fit LA.¿ [0])
      r2v  = computeR2 y yhat
  return [ BenchRow "haskell" "kernel" name ms r2v
                     (sqrt (LA.sumElements ((y - yhat) ** 2)
                            / fromIntegral (LA.size y)))
                     ("kernelRidgeMV Gaussian h=1 λ=1e-3") ]

-- ---------------------------------------------------------------------------
-- Nadaraya-Watson
-- ---------------------------------------------------------------------------

{-# NOINLINE nwPhantom #-}
nwPhantom :: Int -> LA.Matrix Double -> LA.Matrix Double -> LA.Matrix Double
nwPhantom _ x ym = Kn.nwRegressionMV Kn.Gaussian h0 x ym x

benchNW :: FilePath -> String -> IO [BenchRow]
benchNW path name = do
  (x, y) <- readCsvXY path
  let yMat = LA.asColumn y
  (ms, yhatMat) <- timeitTastyIO LA.sumElements
                     (\i -> return $! nwPhantom i x yMat)
  let yhat = LA.flatten (yhatMat LA.¿ [0])
      r2v  = computeR2 y yhat
  return [ BenchRow "haskell" "kernel" name ms r2v
                     (sqrt (LA.sumElements ((y - yhat) ** 2)
                            / fromIntegral (LA.size y)))
                     "nwRegressionMV Gaussian h=1" ]

-- ---------------------------------------------------------------------------
-- RFF Ridge (multivariate input)
-- ---------------------------------------------------------------------------

{-# NOINLINE rffPhantom #-}
rffPhantom :: Int -> RFF.RFFFeaturesMV -> LA.Matrix Double
           -> LA.Matrix Double -> RFF.RFFRidgeFitMVMO
rffPhantom _ feats x ym = RFF.rffRidgeMVMulti feats x ym lam0

benchRFF :: FilePath -> String -> Int -> IO [BenchRow]
benchRFF path name d = do
  (x, y) <- readCsvXY path
  let ym = LA.asColumn y
      p  = LA.cols x
  gen <- MWC.createSystemRandom
  feats <- RFF.sampleRFFRBFMV p d 1.0 1.0 gen
  (ms, _) <- timeitTastyIO (\f -> LA.sumElements (RFF.rffrmvmWeights f))
                (\i -> return $! rffPhantom i feats x ym)
  let yhatMat = RFF.predictRFFRidgeMVMulti
                  (rffPhantom 0 feats x ym) x
      yhat = LA.flatten (yhatMat LA.¿ [0])
      r2v  = computeR2 y yhat
  return [ BenchRow "haskell" "kernel" name ms r2v
                     (sqrt (LA.sumElements ((y - yhat) ** 2)
                            / fromIntegral (LA.size y)))
                     ("RFFFeaturesMV D=" ++ show d) ]

-- ---------------------------------------------------------------------------
-- GP fit (HP fixed)
-- ---------------------------------------------------------------------------

{-# NOINLINE gpFitPhantom #-}
gpFitPhantom :: Int -> GP.GPModel
             -> LA.Matrix Double -> LA.Vector Double -> LA.Matrix Double
             -> GP.GPResultMV
gpFitPhantom _ mdl x y t = GP.fitGPMV mdl x y t

benchGPFit :: FilePath -> String -> IO [BenchRow]
benchGPFit path name = do
  (x, y) <- readCsvXY path
  let mdl = GP.GPModel GP.RBF (GP.GPParams 1.0 1.0 0.05 1.0 Nothing)
  (ms, res) <- timeitTastyIO (\r -> LA.sumElements (GP.gpmvMean r)
                                + LA.sumElements (GP.gpmvVar r))
                 (\i -> return $! gpFitPhantom i mdl x y x)
  let yhat = GP.gpmvMean res
      r2v  = computeR2 y yhat
  return [ BenchRow "haskell" "kernel" name ms r2v
                     (sqrt (LA.sumElements ((y - yhat) ** 2)
                            / fromIntegral (LA.size y)))
                     "fitGPMV RBF (HP fixed)" ]

-- ---------------------------------------------------------------------------
-- GP HP optimization (L-BFGS over log marginal likelihood)
-- ---------------------------------------------------------------------------

{-# NOINLINE gpOptPhantom #-}
gpOptPhantom :: Int -> LA.Matrix Double -> LA.Vector Double -> GP.GPParams
gpOptPhantom _ x y = GP.optimizeGPMV GP.RBF x y
                       (GP.GPParams 0.5 1.0 0.05 1.0 Nothing)

benchGPOpt :: FilePath -> String -> IO [BenchRow]
benchGPOpt path name = do
  (x, y) <- readCsvXY path
  (ms, p) <- timeitTastyIO (\pr -> GP.gpLengthScale pr + GP.gpSignalVar pr
                              + GP.gpNoiseVar pr)
               (\i -> return $! gpOptPhantom i x y)
  let mdl = GP.GPModel GP.RBF p
      res = GP.fitGPMV mdl x y x
      yhat = GP.gpmvMean res
      r2v  = computeR2 y yhat
  return [ BenchRow "haskell" "kernel" name ms r2v (GP.gpLengthScale p)
                     "optimizeGPMV (L-BFGS / log marginal likelihood)" ]

-- ---------------------------------------------------------------------------
-- GPRobust IRLS (Student-t)
-- ---------------------------------------------------------------------------

{-# NOINLINE gprPhantom #-}
gprPhantom :: Int -> LA.Matrix Double -> LA.Vector Double
           -> GPR.RobustGPFitMV
gprPhantom _ x y = GPR.fitGPRobustMV GP.RBF
                      (GP.GPParams 1.0 1.0 0.05 1.0 Nothing)
                      (GPR.RStudentT 4.0 0.1)
                      x y

benchGPRobust :: FilePath -> String -> IO [BenchRow]
benchGPRobust path name = do
  (x, y) <- readCsvXY path
  (ms, fit) <- timeitTastyIO (\f -> LA.sumElements (GPR.rgpmvAlpha f))
                 (\i -> return $! gprPhantom i x y)
  let (mu, _) = GPR.predictGPRobustMV fit x
      r2v    = computeR2 y mu
  return [ BenchRow "haskell" "kernel" name ms r2v (fromIntegral (GPR.rgpmvIters fit))
                     "fitGPRobustMV StudentT(4, 0.1)" ]

-- ---------------------------------------------------------------------------

computeR2 :: LA.Vector Double -> LA.Vector Double -> Double
computeR2 y yhat =
  let mu  = LA.sumElements y / fromIntegral (LA.size y)
      sst = LA.sumElements ((y - LA.konst mu (LA.size y)) ** 2)
      sse = LA.sumElements ((y - yhat) ** 2)
  in if sst == 0 then 0 else 1 - sse / sst
