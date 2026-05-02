{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
-- | 変分推論 (ADVI) vs NUTS 比較デモ
--
-- 2 つのモデルで VI と NUTS を比較する。
--
-- モデル 1: Beta-Binomial (臨床試験)
--   p_ctrl ~ Beta(1,1),  y_ctrl ~ Binomial(50, p_ctrl),  観測: 18 回復
--   p_trt  ~ Beta(1,1),  y_trt  ~ Binomial(50, p_trt),   観測: 31 回復
--   → 解析解が存在するため精度の検証が可能
--
-- モデル 2: 階層正規モデル (3 校)
--   μ ~ Normal(0,100), τ ~ Exponential(0.1), θ_j ~ Normal(μ,τ)
--   → 強い相関がある事後分布で VI の限界を確認
--
module Main where

import Control.Monad (forM_)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Text.Printf (printf)
import System.Random.MWC (createSystemRandom)

import Model.HBM
import Stat.Distribution ()
import MCMC.Core (chainVals, posteriorMean, posteriorSD)
import MCMC.NUTS (NUTSConfig (..), defaultNUTSConfig, nuts)
import Stat.VI

-- ---------------------------------------------------------------------------
-- モデル 1: Beta-Binomial (臨床試験)
-- ---------------------------------------------------------------------------

nCtrl, kCtrl, nTrt, kTrt :: Int
nCtrl = 50; kCtrl = 18
nTrt  = 50; kTrt  = 31

clinicalModel :: ModelP ()
clinicalModel = do
  pCtrl <- sample "p_ctrl" (Beta 1 1)
  pTrt  <- sample "p_trt"  (Beta 1 1)
  observe "y_ctrl" (Binomial nCtrl pCtrl) [fromIntegral kCtrl]
  observe "y_trt"  (Binomial nTrt  pTrt)  [fromIntegral kTrt]

m1 :: ModelP ()
m1 = clinicalModel

m2 :: ModelP ()
m2 = schoolModelI schoolData

-- 解析解: Beta(1,1) + Binomial → Beta(1+k, 1+n-k)
betaMean :: Int -> Int -> Double
betaMean k n = fromIntegral (1 + k) / fromIntegral (2 + n)

betaSD :: Int -> Int -> Double
betaSD k n =
  let a = fromIntegral (1 + k); b = fromIntegral (1 + n - k); s = a + b
  in sqrt (a * b / (s * s * (s + 1)))

-- ---------------------------------------------------------------------------
-- モデル 2: 階層正規モデル (3 校)
-- ---------------------------------------------------------------------------

sigma :: Double
sigma = 5.0

schoolData :: [[Double]]
schoolData =
  [ [72, 68, 75, 71]
  , [85, 88, 82, 90]
  , [61, 65, 58, 63]
  ]

-- schoolModel を添字付きで作る
schoolModelI :: [[Double]] -> ModelP ()
schoolModelI groupData = do
  mu  <- sample "mu"  (Normal 0 100)
  tau <- sample "tau" (Exponential 0.1)
  forM_ (zip [1::Int ..] groupData) $ \(j, ys) -> do
    theta <- sample (T.pack ("theta_" ++ show j)) (Normal mu tau)
    observe (T.pack ("y_" ++ show j)) (Normal theta (realToFrac sigma)) ys

-- ---------------------------------------------------------------------------
-- ユーティリティ
-- ---------------------------------------------------------------------------

timed :: IO a -> IO (a, Double)
timed action = do
  t0 <- getCurrentTime
  x  <- action
  t1 <- getCurrentTime
  return (x, realToFrac (diffUTCTime t1 t0))

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  gen <- createSystemRandom

  -- ════════════════════════════════════════════════════════════════════════
  putStrLn "=== モデル 1: Beta-Binomial (臨床試験) ==="
  putStrLn "    解析解が存在するモデルで VI の精度を検証する"
  putStrLn ""

  let initP1 = Map.fromList [("p_ctrl", 0.5 :: Double), ("p_trt", 0.5)]

  -- VI
  let viCfg1 = defaultVIConfig
                 { viIterations = 500
                 , viSamples    = 10
                 , viNumDraws   = 5000
                 }
  (viRes1, tVI1) <- timed $ advi m1 viCfg1 initP1 gen

  -- NUTS
  let nutsCfg1 = defaultNUTSConfig
                   { nutsIterations = 2000
                   , nutsBurnIn     = 500
                   , nutsStepSize   = 0.3
                   }
  (nutsC1, tNUTS1) <- timed $ nuts m1 nutsCfg1 initP1 gen

  -- 解析解
  let analCtrlMu = betaMean kCtrl nCtrl;  analCtrlSD = betaSD kCtrl nCtrl
      analTrtMu  = betaMean kTrt  nTrt;   analTrtSD  = betaSD kTrt  nTrt

  let get f p = Map.findWithDefault 0 p (f viRes1)

  printf "  %-12s  %-12s  %-12s  %-12s\n"
    ("" :: String) ("p_ctrl" :: String) ("p_trt" :: String) ("時間" :: String)
  printf "  %-12s  mean=%.4f SD=%.4f  mean=%.4f SD=%.4f  %.3fs\n"
    ("VI" :: String)
    (get viPostMeans "p_ctrl") (get viPostSDs "p_ctrl")
    (get viPostMeans "p_trt")  (get viPostSDs "p_trt")
    tVI1
  printf "  %-12s  mean=%.4f SD=%.4f  mean=%.4f SD=%.4f  %.3fs\n"
    ("NUTS" :: String)
    (maybe 0 id $ posteriorMean "p_ctrl" nutsC1)
    (maybe 0 id $ posteriorSD   "p_ctrl" nutsC1)
    (maybe 0 id $ posteriorMean "p_trt"  nutsC1)
    (maybe 0 id $ posteriorSD   "p_trt"  nutsC1)
    tNUTS1
  printf "  %-12s  mean=%.4f SD=%.4f  mean=%.4f SD=%.4f\n"
    ("解析解" :: String)
    analCtrlMu analCtrlSD analTrtMu analTrtSD
  putStrLn ""

  -- ELBO 収束の表示
  putStrLn "  ELBO 収束 (初期 → 最終):"
  let elboHist = viElboHistory viRes1
      n        = length elboHist
      steps    = [1, n `div` 4, n `div` 2, 3 * n `div` 4, n]
  forM_ steps $ \i ->
    when (i > 0 && i <= n) $
      printf "    iter %4d: ELBO = %.3f\n" i (elboHist !! (i - 1))
  putStrLn ""

  -- P(p_trt > p_ctrl) の推定
  let vDraws1  = viDraws viRes1
      diffVI   = [ Map.findWithDefault 0 "p_trt"  d
                 - Map.findWithDefault 0 "p_ctrl" d | d <- vDraws1 ]
      probVI   = fromIntegral (length (filter (> 0) diffVI)) / fromIntegral (length diffVI) :: Double
      diffNUTS = zipWith (-) (chainVals "p_trt" nutsC1) (chainVals "p_ctrl" nutsC1)
      probNUTS = fromIntegral (length (filter (> 0) diffNUTS)) / fromIntegral (length diffNUTS) :: Double

  printf "  P(p_trt > p_ctrl): VI=%.4f  NUTS=%.4f\n" probVI probNUTS
  putStrLn ""

  -- ════════════════════════════════════════════════════════════════════════
  putStrLn "=== モデル 2: 階層正規モデル (3 校) ==="
  putStrLn "    相関の強い事後分布で VI の近似誤差を確認する"
  putStrLn ""

  let initP2 = Map.fromList
                 [ ("mu", 73.0), ("tau", 10.0)
                 , ("theta_1", 71.5), ("theta_2", 86.25), ("theta_3", 61.75)
                 ]
      names2 = sampleNames m2

  -- VI
  let viCfg2 = defaultVIConfig
                 { viIterations   = 1000
                 , viSamples      = 10
                 , viNumDraws     = 5000
                 , viLearningRate = 0.05
                 }
  (viRes2, tVI2) <- timed $ advi m2 viCfg2 initP2 gen

  -- NUTS
  let nutsCfg2 = defaultNUTSConfig
                   { nutsIterations = 2000
                   , nutsBurnIn     = 500
                   , nutsStepSize   = 0.05
                   }
  (nutsC2, tNUTS2) <- timed $ nuts m2 nutsCfg2 initP2 gen

  putStrLn "  事後サマリー:"
  printf "  %-12s  %8s  %8s  |  %8s  %8s\n"
    ("param" :: String) ("VI 平均" :: String) ("VI SD" :: String)
    ("NUTS 平均" :: String) ("NUTS SD" :: String)
  forM_ names2 $ \p ->
    printf "  %-12s  %8.3f  %8.3f  |  %8.3f  %8.3f\n"
      (T.unpack p)
      (Map.findWithDefault 0 p (viPostMeans viRes2))
      (Map.findWithDefault 0 p (viPostSDs   viRes2))
      (maybe 0 id $ posteriorMean p nutsC2)
      (maybe 0 id $ posteriorSD   p nutsC2)
  putStrLn ""

  printf "  実行時間: VI=%.3fs  NUTS=%.3fs  (VI は NUTS の %.1f 倍速)\n"
    tVI2 tNUTS2 (tNUTS2 / tVI2)
  putStrLn ""
  putStrLn "  注: 平均場 VI は各パラメータ間の相関を無視するため、"
  putStrLn "      階層モデルでは SD を過小評価する傾向がある (過信)"
  putStrLn ""
  putStrLn "完了"

when :: Bool -> IO () -> IO ()
when True  action = action
when False _      = return ()
