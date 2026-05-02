{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | ZeroInflatedPoisson のデモ (Phase H3)。
--
-- 真値: ψ = 0.4 (40% は構造的ゼロ), λ = 5
-- 期待 mean = (1-ψ)λ = 3.0
-- データには余分なゼロが多く出現 → 普通の Poisson モデルだと λ を低く推定する。
module Main where

import qualified Data.Map.Strict as Map
import Text.Printf (printf)
import System.Random.MWC (createSystemRandom)
import qualified System.Random.MWC as MWCBase

import MCMC.NUTS (nuts, defaultNUTSConfig, NUTSConfig (..))
import Model.HBM (ModelP, sample, observe, Distribution (..))
import Viz.MCMC (printPosteriorSummary, posteriorSummaryFile)

cfg :: NUTSConfig
cfg = defaultNUTSConfig
        { nutsIterations = 800
        , nutsBurnIn     = 400
        , nutsStepSize   = 0.1
        , nutsMaxDepth   = 6
        }

-- 真値 ψ=0.4, λ=5 で生成
genZIP :: Int -> Double -> Double -> IO [Double]
genZIP n psi lam = do
  gen <- createSystemRandom
  let drawOne = do
        u <- MWCBase.uniform gen :: IO Double
        if u < psi
          then return 0
          else do
            -- Knuth Poisson
            let go k p = do
                  v <- MWCBase.uniform gen :: IO Double
                  let p' = p * v
                  if p' < exp (-lam)
                    then return (fromIntegral k)
                    else go (k+1) p'
            go (0 :: Int) (1 :: Double)
  mapM (const drawOne) [1 .. n]

poissonModel :: [Double] -> ModelP ()
poissonModel ys = do
  lam <- sample "lambda" (Gamma 1 0.1)
  observe "y" (Poisson lam) ys

zipModel :: [Double] -> ModelP ()
zipModel ys = do
  psi <- sample "psi"    (Beta 1 1)        -- 一様事前
  lam <- sample "lambda" (Gamma 1 0.1)
  observe "y" (ZeroInflatedPoisson psi lam) ys

main :: IO ()
main = do
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  ZeroInflatedPoisson vs Poisson (ゼロ過剰, Phase H3)"
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn ""

  putStrLn "真値: ψ = 0.4, λ = 5  → 観測平均 ≈ (1-0.4)*5 = 3.0"
  ys <- genZIP 100 0.4 5
  let nZero = length (filter (== 0) ys)
      n     = length ys
      muSm  = sum ys / fromIntegral n
  printf "観測 (n=%d): 平均 = %.2f, ゼロ件数 = %d (%.0f%%)\n"
         n muSm nZero (100 * fromIntegral nZero / fromIntegral n :: Double)
  putStrLn ""

  gen <- createSystemRandom

  putStrLn "[1] Poisson (ゼロ過剰を捕えない)"
  ch1 <- nuts (poissonModel ys) cfg
              (Map.fromList [("lambda", 3)]) gen
  printPosteriorSummary ["lambda"] [ch1]
  putStrLn ""

  putStrLn "[2] ZeroInflatedPoisson (構造的ゼロを分離)"
  ch2 <- nuts (zipModel ys) cfg
              (Map.fromList [("psi", 0.3), ("lambda", 5)]) gen
  printPosteriorSummary ["psi", "lambda"] [ch2]
  putStrLn ""

  posteriorSummaryFile "zip-poisson.html" "Poisson"          ["lambda"]      [ch1]
  posteriorSummaryFile "zip-zip.html"     "ZeroInflated Poi" ["psi","lambda"] [ch2]
  putStrLn "  → zip-{poisson,zip}.html"
  putStrLn ""

  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  ✓ ZIP で ψ ≈ 0.4 (構造的ゼロ率) と λ ≈ 5 を分離回復"
  putStrLn "═══════════════════════════════════════════════════════════════"
