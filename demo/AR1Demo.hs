{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | AR(1) 状態空間モデルのデモ (Phase J2)。
--
-- 真値: ϕ=0.7, σ_state=0.5, σ_obs=0.3
-- 真の x_t を AR(1) で生成、ノイズを加えて y_t を観測。
-- ϕ, σ_state, σ_obs を NUTS で同時推定 (x_t は latent ベクトル)。
module Main where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf (printf)
import System.Random.MWC (createSystemRandom)
import qualified System.Random.MWC.Distributions as MWC

import MCMC.NUTS (nuts, defaultNUTSConfig, NUTSConfig (..))
import Model.HBM (ModelP, sample, observe, ar1Latent,
                  Distribution (..), augmentChainWithDeterministic)
import Viz.MCMC (printPosteriorSummary, posteriorSummaryFile)

cfg :: NUTSConfig
cfg = defaultNUTSConfig
        { nutsIterations = 600
        , nutsBurnIn     = 400
        , nutsStepSize   = 0.05
        , nutsMaxDepth   = 7
        }

genData :: Int -> Double -> Double -> Double -> IO ([Double], [Double])
genData nT phi sigSt sigOb = do
  gen <- createSystemRandom
  let stat0 = sigSt / sqrt (1 - phi * phi)
  z0 <- MWC.normal 0 stat0 gen
  let go t prev acc
        | t == nT = return (reverse acc)
        | otherwise = do
            eps <- MWC.normal 0 sigSt gen
            let x = phi * prev + eps
            go (t+1) x (x : acc)
  xs <- go 1 z0 [z0]
  ys <- mapM (\x -> do
                 e <- MWC.normal 0 sigOb gen
                 return (x + e)) xs
  return (xs, ys)

ar1Model :: Int -> [Double] -> ModelP ()
ar1Model nT ys = do
  phi   <- sample "phi"       (Uniform (-0.99) 0.99)
  sigSt <- sample "sig_state" (HalfNormal 1)
  sigOb <- sample "sig_obs"   (HalfNormal 1)
  xs <- ar1Latent "x" nT phi sigSt
  mapM_ (\(t, y) -> observe ("y_" <> tShow t) (Normal (xs !! t) sigOb) [y])
        (zip [0 .. nT - 1] ys)
  where
    tShow :: Int -> Text
    tShow = T.pack . show

main :: IO ()
main = do
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  AR(1) 状態空間モデル (Phase J2)"
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn ""

  let nT = 30
  printf "真値: ϕ=0.7, σ_state=0.5, σ_obs=0.3, 系列長 N=%d\n" nT
  (_, ysObs) <- genData nT 0.7 0.5 0.3
  printf "観測 y の最初 5 点: %s\n"
         (show (take 5 ysObs))
  putStrLn ""

  gen <- createSystemRandom
  let init0 = Map.fromList $
        [(("x_raw" <> T.pack (show t)) :: Text, 0.0 :: Double)
         | t <- [0 .. nT - 1]]
        ++ [("phi", 0.5), ("sig_state", 0.5), ("sig_obs", 0.3)]
  ch0 <- nuts (ar1Model nT ysObs) cfg init0 gen
  let ch = augmentChainWithDeterministic (ar1Model nT ysObs) ch0

  putStrLn "[1] Posterior summary (主要パラメタのみ)"
  printPosteriorSummary ["phi", "sig_state", "sig_obs"] [ch]
  putStrLn ""

  putStrLn "[2] 一部の latent state x_t (派生量)"
  printPosteriorSummary
    [ "x_" <> T.pack (show t) | t <- [0, 5, 10, 15, 20, 25, 29] ]
    [ch]
  putStrLn ""

  posteriorSummaryFile "ar1-summary.html" "AR(1) posterior"
    ["phi", "sig_state", "sig_obs"] [ch]
  putStrLn "  → ar1-summary.html"
  putStrLn ""

  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  ✓ AR(1) latent + 観測モデルで状態空間が動作"
  putStrLn "═══════════════════════════════════════════════════════════════"
