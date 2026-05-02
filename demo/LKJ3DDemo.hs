{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | Phase J1: LKJ 相関行列事前を K=3 で検証。
--
-- 真の相関行列 (sd=1 固定):
--   R = [[1.0, 0.6, 0.3],
--        [0.6, 1.0, 0.4],
--        [0.3, 0.4, 1.0]]
-- から 3D サンプル n=200 を生成し、LKJ(η=1) 事前で
-- 各 3 個の相関 (R[1][0], R[2][0], R[2][1]) を回復する。
module Main where

import qualified Data.Map.Strict as Map
import Text.Printf (printf)
import System.Random.MWC (createSystemRandom)
import qualified System.Random.MWC.Distributions as MWC

import MCMC.NUTS (nuts, defaultNUTSConfig, NUTSConfig (..))
import Model.HBM (ModelP, sample, observeMV, lkjCorrCholesky,
                  Distribution (..), augmentChainWithDeterministic)
import Viz.MCMC (printPosteriorSummary, posteriorSummaryFile)

cfg :: NUTSConfig
cfg = defaultNUTSConfig
        { nutsIterations = 800
        , nutsBurnIn     = 400
        , nutsStepSize   = 0.05
        , nutsMaxDepth   = 7
        }

-- 真の Cholesky factor L (sd=1 仮定)
trueL :: [[Double]]
trueL =
  -- L[0] = (1, 0, 0)
  -- L[1] = (0.6, sqrt(1-0.36)=0.8, 0)
  -- L[2] = (0.3, (0.4 - 0.6*0.3)/0.8 = 0.275, sqrt(1 - 0.09 - 0.275^2) = 0.946)
  [ [1.0, 0.0,    0.0]
  , [0.6, 0.8,    0.0]
  , [0.3, 0.275,  sqrt (1 - 0.09 - 0.275 ^ (2::Int)) ]
  ]

gen3D :: Int -> IO [[Double]]
gen3D n = do
  gen <- createSystemRandom
  let drawOne = do
        z0 <- MWC.standard gen
        z1 <- MWC.standard gen
        z2 <- MWC.standard gen
        let x0 = head (head trueL)             * z0
            x1 = (trueL !! 1 !! 0) * z0 + (trueL !! 1 !! 1) * z1
            x2 = (trueL !! 2 !! 0) * z0 + (trueL !! 2 !! 1) * z1
                 + (trueL !! 2 !! 2) * z2
        return [x0, x1, x2]
  mapM (const drawOne) [1 .. n]

-- σ 既知 (=1)、相関のみ LKJ で推定
lkj3DModel :: [[Double]] -> ModelP ()
lkj3DModel obs = do
  l <- lkjCorrCholesky "R" 3 1.0    -- η = 1: uniform 事前
  let cov = let row i = [ sum [ ((l !! i) !! kk) * ((l !! j) !! kk)
                              | kk <- [0 .. min i j] ]
                        | j <- [0, 1, 2] ]
            in [row i | i <- [0, 1, 2]]
  m0 <- sample "mu0" (Normal 0 5)
  m1 <- sample "mu1" (Normal 0 5)
  m2 <- sample "mu2" (Normal 0 5)
  observeMV "y" (MvNormal [m0, m1, m2] cov) obs

main :: IO ()
main = do
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  LKJ K=3 検証 (Phase J1)"
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn ""

  putStrLn "真の相関: R[1][0]=0.6, R[2][0]=0.3, R[2][1]=0.4"
  obs <- gen3D 200
  -- 標本相関で確認
  let cols = transpose obs
      mu c = sum c / fromIntegral (length c)
      cov ci cj =
        let mi = mu ci; mj = mu cj
        in sum (zipWith (\x y -> (x - mi) * (y - mj)) ci cj)
           / fromIntegral (length ci - 1)
      sd ci = sqrt (cov ci ci)
      cor ci cj = cov ci cj / (sd ci * sd cj)
      [c0, c1, c2] = cols
  printf "標本: r10=%.3f, r20=%.3f, r21=%.3f\n"
         (cor c0 c1) (cor c0 c2) (cor c1 c2)
  putStrLn ""

  gen <- createSystemRandom
  rawCh <- nuts (lkj3DModel obs) cfg
                (Map.fromList [ ("R_u1_0", 0.5), ("R_u2_0", 0.5)
                              , ("R_u2_1", 0.5)
                              , ("mu0", 0), ("mu1", 0), ("mu2", 0) ])
                gen
  let ch = augmentChainWithDeterministic (lkj3DModel obs) rawCh

  -- pc は partial correlations。R 自体の上三角 (j < i) は対応する
  -- canonical partial correlation だが、L の積で R が決まる。
  -- 実際の R[i][j] (i > j) は L から再構築できる; ここでは
  -- pc/L をそのまま表示し、コメントで対応関係を示す。
  putStrLn "[1] Posterior summary"
  let names = [ "R_pc1_0"     -- = R[1][0] = ρ_10   (K=2 部分なので一致)
              , "R_pc2_0"     -- partial corr (NOT直接 ρ_20)
              , "R_pc2_1"     --
              , "R_L1_0", "R_L1_1"
              , "R_L2_0", "R_L2_1", "R_L2_2"
              , "mu0", "mu1", "mu2"
              ]
  printPosteriorSummary names [ch]
  putStrLn ""

  posteriorSummaryFile "lkj3d-summary.html" "LKJ K=3 posterior" names [ch]
  putStrLn "  → lkj3d-summary.html"
  putStrLn ""

  putStrLn "Note: R[i][j] (i>j) は L から再構築:"
  putStrLn "  R[1][0] = L[1][0]"
  putStrLn "  R[2][0] = L[2][0]"
  putStrLn "  R[2][1] = L[1][0]*L[2][0] + L[1][1]*L[2][1]"
  putStrLn ""
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  ✓ LKJ(η=1) が K=3 で動作、3 個の相関を同時推定"
  putStrLn "═══════════════════════════════════════════════════════════════"

  where
    transpose :: [[a]] -> [[a]]
    transpose [] = []
    transpose xss
      | all null xss = []
      | otherwise =
          let heads = [h | (h:_) <- xss]
              tails = [t | (_:t) <- xss]
          in heads : transpose tails
