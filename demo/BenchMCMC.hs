{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | MH / HMC / NUTS のパフォーマンス比較デモ
--
-- ケース 1 (易しい): 独立 2D 正規事後分布
--   μ₁ ~ N(0,5), μ₂ ~ N(0,5)
--   y₁ᵢ | μ₁ ~ N(μ₁,1),  y₂ᵢ | μ₂ ~ N(μ₂,1)
--   → 事後分布の等高線は円形。全手法で効率よく探索できる。
--
-- ケース 2 (難しい): 和制約による強反相関事後分布
--   α ~ N(0,5), β ~ N(0,5)
--   yᵢ | α,β ~ N(α+β, 1)
--   → 事後分布は α+β ≈ ȳ という細長い尾根 (ρ ≈ -0.998)。
--     MH は短軸 (SD≈0.2) にステップを合わせると長軸 (SD≈7) の探索が
--     ランダムウォーク化し ESS が激減する。
--     HMC/NUTS は勾配で尾根に沿って動けるため効率を維持できる。
module Main where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Text.Printf (printf)
import System.Random.MWC (createSystemRandom)

import Model.HBM
import MCMC.Core (Chain (..), chainVals, acceptanceRate, posteriorMean)
import MCMC.MH   (metropolis, MCMCConfig (..))
import MCMC.HMC  (hmc,  HMCConfig (..),  defaultHMCConfig)
import MCMC.NUTS (nuts, NUTSConfig (..), defaultNUTSConfig)
import Stat.Distribution ()
import Stat.MCMC (ess)

-- ---------------------------------------------------------------------------
-- モデル定義
-- ---------------------------------------------------------------------------

-- | ケース 1: 独立 2 パラメータ
easyModel :: [Double] -> [Double] -> ModelP ()
easyModel ys1 ys2 = do
  mu1 <- sample "mu1" (Normal 0 5)
  mu2 <- sample "mu2" (Normal 0 5)
  observe "y1" (Normal mu1 1) ys1
  observe "y2" (Normal mu2 1) ys2

-- | ケース 2: 両パラメータが同じ観測に現れる → 事後分布に強反相関
hardModel :: [Double] -> ModelP ()
hardModel ys = do
  alpha <- sample "mu1" (Normal 0 5)
  beta  <- sample "mu2" (Normal 0 5)
  observe "y" (Normal (alpha + beta) 1) ys

-- ---------------------------------------------------------------------------
-- 合成データ
-- ---------------------------------------------------------------------------

-- ケース 1: 真値 μ₁=2, μ₂=-1, n=20
obsEasy1, obsEasy2 :: [Double]
obsEasy1 = [2.3,1.8,2.1,1.9,2.5,1.7,2.2,2.0,1.6,2.4
           ,2.1,1.8,2.3,2.0,1.9,2.2,1.7,2.5,1.8,2.1]
obsEasy2 = [-0.8,-1.2,-0.9,-1.1,-0.7,-1.3,-1.0,-0.9,-1.2,-1.1
           ,-1.0,-0.8,-1.2,-1.1,-0.9,-1.0,-1.3,-0.7,-1.1,-0.8]

-- ケース 2: 真値 α+β=2, n=20
obsHard :: [Double]
obsHard = [1.5,2.3,1.8,2.1,2.5,1.7,2.2,2.0,1.6,2.4
          ,2.1,1.8,2.3,2.0,1.9,2.2,1.7,2.5,1.8,2.1]

-- ---------------------------------------------------------------------------
-- MCMC 設定
-- ---------------------------------------------------------------------------

nIter, nBurnIn :: Int
nIter   = 5000
nBurnIn = 1000

-- MH (ケース 1): 事後 SD ≈ 0.22 に対してステップ 0.4
mhEasy :: MCMCConfig
mhEasy = MCMCConfig
  { mcmcIterations = nIter
  , mcmcBurnIn     = nBurnIn
  , mcmcStepSizes  = Map.fromList [("mu1", 0.4), ("mu2", 0.4)]
  }

-- MH (ケース 2): 短軸 SD ≈ 0.2 に合わせた小ステップ
--   → 受容率は高いが長軸方向は完全なランダムウォーク
mhHard :: MCMCConfig
mhHard = MCMCConfig
  { mcmcIterations = nIter
  , mcmcBurnIn     = nBurnIn
  , mcmcStepSizes  = Map.fromList [("mu1", 0.1), ("mu2", 0.1)]
  }

-- HMC (ケース 1)
hmcEasy :: HMCConfig
hmcEasy = defaultHMCConfig
  { hmcIterations    = nIter
  , hmcBurnIn        = nBurnIn
  , hmcStepSize      = 0.2
  , hmcLeapfrogSteps = 10
  }

-- HMC (ケース 2): 長軸を踏破するためステップ数を多く
hmcHard :: HMCConfig
hmcHard = defaultHMCConfig
  { hmcIterations    = nIter
  , hmcBurnIn        = nBurnIn
  , hmcStepSize      = 0.05
  , hmcLeapfrogSteps = 50
  }

-- NUTS (ケース 1)
nutsEasy :: NUTSConfig
nutsEasy = defaultNUTSConfig
  { nutsIterations = nIter
  , nutsBurnIn     = nBurnIn
  , nutsStepSize   = 0.2
  }

-- NUTS (ケース 2): U-Turn 判定で軌跡長を自動調整
nutsHard :: NUTSConfig
nutsHard = defaultNUTSConfig
  { nutsIterations = nIter
  , nutsBurnIn     = nBurnIn
  , nutsStepSize   = 0.05
  }

-- ---------------------------------------------------------------------------
-- ユーティリティ
-- ---------------------------------------------------------------------------

getESS :: T.Text -> Chain -> Double
getESS name ch =
  ess (chainVals name ch)

timed :: IO a -> IO (a, Double)
timed action = do
  t0 <- getCurrentTime
  x  <- action
  t1 <- getCurrentTime
  return (x, realToFrac (diffUTCTime t1 t0))

report :: String -> Chain -> Double -> IO ()
report method ch secs = do
  let e1   = getESS "mu1" ch
      e2   = getESS "mu2" ch
      minE = min e1 e2
      m1   = maybe 0 id (posteriorMean "mu1" ch)
      m2   = maybe 0 id (posteriorMean "mu2" ch)
  printf
    "  %-5s | acc=%5.3f | mean(μ₁)=%6.3f mean(μ₂)=%6.3f \
    \| ESS(μ₁)=%5.0f ESS(μ₂)=%5.0f | minESS/s=%6.1f | %5.2fs\n"
    method (acceptanceRate ch) m1 m2 e1 e2 (minE / secs) secs

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

mEasy :: ModelP ()
mEasy = easyModel obsEasy1 obsEasy2

mHard :: ModelP ()
mHard = hardModel obsHard

main :: IO ()
main = do
  gen <- createSystemRandom

  let initP = Map.fromList [("mu1", 0.0 :: Double), ("mu2", 0.0)]

  -- ---- ケース 1: 独立 2D 正規 ----
  let n     = length obsEasy1
      sigPost = 1 / sqrt (fromIntegral n + 1/25 :: Double)

  putStrLn ""
  putStrLn "══════════════════════════════════════════════════════════════════"
  putStrLn " ケース 1: 独立 2D 正規事後分布  (全手法で収束しやすい)"
  printf   "  真値: μ₁≈2.0, μ₂≈-1.0  事後 SD≈%.3f  ρ=0\n" sigPost
  putStrLn "══════════════════════════════════════════════════════════════════"

  (ch1, t1) <- timed $ metropolis mEasy mhEasy  initP gen
  report "MH"   ch1 t1
  (ch2, t2) <- timed $ hmc  mEasy hmcEasy  initP gen
  report "HMC"  ch2 t2
  (ch3, t3) <- timed $ nuts mEasy nutsEasy initP gen
  report "NUTS" ch3 t3

  -- ---- ケース 2: 強反相関 ----
  let ybar     = sum obsHard / fromIntegral (length obsHard)
      n2       = fromIntegral (length obsHard) :: Double
      -- 事後の短軸/長軸 SD を解析的に計算
      -- Λ = [[1/25+n, n],[n, 1/25+n]], Σ = Λ^{-1}
      lam      = 1/25 + n2
      detLam   = lam*lam - n2*n2
      sig11    = lam / detLam
      sig12    = negate n2 / detLam
      rhoPost  = sig12 / sig11
      sdShort  = sqrt (sig11 + sig12)  -- SD of (μ₁-μ₂)/√2
      sdLong   = sqrt (sig11 - sig12)  -- SD of (μ₁+μ₂)/√2

  putStrLn ""
  putStrLn "══════════════════════════════════════════════════════════════════"
  putStrLn " ケース 2: 和制約 α+β≈ȳ  (MH で収束しにくい)"
  printf   "  ȳ=%.2f  事後: 短軸 SD≈%.3f  長軸 SD≈%.2f  ρ≈%.4f\n"
           ybar sdShort sdLong rhoPost
  putStrLn "══════════════════════════════════════════════════════════════════"

  (ch4, t4) <- timed $ metropolis mHard mhHard  initP gen
  report "MH"   ch4 t4
  (ch5, t5) <- timed $ hmc  mHard hmcHard  initP gen
  report "HMC"  ch5 t5
  (ch6, t6) <- timed $ nuts mHard nutsHard initP gen
  report "NUTS" ch6 t6

  putStrLn ""
  putStrLn "凡例: acc=受容率  mean=事後平均  ESS=有効サンプル数  minESS/s=効率"
