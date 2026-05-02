{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | ベイズ A/B テスト (Beta-Binomial モデル)
--
-- 二値アウトカム（例: 新薬投与後の回復）を持つ二群比較。
--
-- モデル:
--   p_ctrl      ~ Beta(1,1)         ← 一様事前分布  (UnitIntervalT → ロジット変換)
--   p_trt       ~ Beta(1,1)
--   y_ctrl      ~ Binomial(n_ctrl,  p_ctrl)
--   y_trt       ~ Binomial(n_trt,   p_trt)
--
-- 解析解 (Beta-Binomial 共役): p|y ~ Beta(1+k, 1+n-k)
--
-- 推論:
--   - 4-chain NUTS でサンプリング
--   - P(p_trt > p_ctrl) をサンプルから推定
--   - HTML レポート生成 (mcmc_report_clinical.html)
--
module Main where

import Control.Monad (forM_)
import qualified Data.Map.Strict as Map
import qualified Data.Text       as T
import Text.Printf (printf)
import System.Random.MWC (createSystemRandom)

import Model.HBM
import MCMC.Core  (Chain (..), chainVals, acceptanceRate, posteriorMean
                  , posteriorQuantile)
import MCMC.NUTS  (NUTSConfig (..), defaultNUTSConfig, nutsChains)
-- import Stat.Distribution (Distribution (..)) -- now from Model.HBM
import Stat.MCMC  (ess, rhat)
import Viz.Core   (openInBrowser)
import Viz.Report (MCMCReport (..), defaultReport, renderReport)

-- ---------------------------------------------------------------------------
-- 合成データ (架空の臨床試験)
-- ---------------------------------------------------------------------------

-- 対照群: 50 人中 18 人が回復  → 真値 p_ctrl ≈ 0.36
nCtrl, kCtrl :: Int
nCtrl = 50
kCtrl = 18

-- 治療群: 50 人中 31 人が回復  → 真値 p_trt  ≈ 0.62
nTrt, kTrt :: Int
nTrt  = 50
kTrt  = 31

-- ---------------------------------------------------------------------------
-- モデル定義
-- ---------------------------------------------------------------------------

clinicalModel :: ModelP ()
clinicalModel = do
  pCtrl <- sample "p_ctrl" (Beta 1 1)
  pTrt  <- sample "p_trt"  (Beta 1 1)
  observe "y_ctrl" (Binomial nCtrl pCtrl) [fromIntegral kCtrl]
  observe "y_trt"  (Binomial nTrt  pTrt)  [fromIntegral kTrt]

-- ---------------------------------------------------------------------------
-- 解析解 (Beta-Binomial 共役)
-- ---------------------------------------------------------------------------

-- Beta(1,1) 事前 + Binomial(n,p) 観測 k → Beta(1+k, 1+n-k) 事後
analyticMean :: Int -> Int -> Double
analyticMean k n = fromIntegral (1 + k) / fromIntegral (2 + n)

analyticSD :: Int -> Int -> Double
analyticSD k n =
  let a = fromIntegral (1 + k)
      b = fromIntegral (1 + n - k)
      s = a + b
  in sqrt (a * b / (s * s * (s + 1)))

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

m :: ModelP ()
m = clinicalModel

main :: IO ()
main = do
  gen <- createSystemRandom
  let names = sampleNames m

  -- ── モデル概要 ────────────────────────────────────────────────────────
  putStrLn "=== Bayesian A/B Test: Clinical Trial ==="
  putStrLn ""
  putStrLn "モデル:"
  putStrLn "  p_ctrl ~ Beta(1,1)"
  putStrLn "  p_trt  ~ Beta(1,1)"
  printf "  y_ctrl ~ Binomial(%d, p_ctrl)  観測: %d/%d 回復\n" nCtrl kCtrl nCtrl
  printf "  y_trt  ~ Binomial(%d, p_trt)   観測: %d/%d 回復\n" nTrt  kTrt  nTrt
  putStrLn ""

  -- ── 解析解 ────────────────────────────────────────────────────────────
  let aCtrlMean = analyticMean kCtrl nCtrl
      aCtrlSD   = analyticSD   kCtrl nCtrl
      aTrtMean  = analyticMean kTrt  nTrt
      aTrtSD    = analyticSD   kTrt  nTrt

  putStrLn "=== 解析解 (Beta-Binomial 共役) ==="
  printf "  p_ctrl | data ~ Beta(%d, %d)  mean=%.4f  SD=%.4f\n"
    (1+kCtrl) (1+nCtrl-kCtrl) aCtrlMean aCtrlSD
  printf "  p_trt  | data ~ Beta(%d, %d)  mean=%.4f  SD=%.4f\n"
    (1+kTrt) (1+nTrt-kTrt) aTrtMean aTrtSD
  putStrLn ""

  -- ── 4-chain NUTS ──────────────────────────────────────────────────────
  putStrLn "=== 4-chain NUTS サンプリング ==="
  let initP = Map.fromList [("p_ctrl", 0.5 :: Double), ("p_trt", 0.5)]
      cfg   = defaultNUTSConfig
                { nutsIterations = 2000
                , nutsBurnIn     = 500
                , nutsStepSize   = 0.3
                }

  chains <- nutsChains m cfg 4 initP gen

  forM_ (zip [1::Int ..] chains) $ \(i, ch) ->
    printf "  chain %d: acceptance=%.3f  p_ctrl=%.4f  p_trt=%.4f\n"
      i (acceptanceRate ch)
      (maybe 0 id $ posteriorMean "p_ctrl" ch)
      (maybe 0 id $ posteriorMean "p_trt"  ch)
  putStrLn ""

  -- ── 事後サマリー ──────────────────────────────────────────────────────
  putStrLn "=== 事後サマリー ==="
  printf "  %-10s  %8s  %8s  %8s  %8s  %8s  %8s  %8s\n"
    ("param"::String) ("mean"::String) ("SD"::String)
    ("2.5%"::String) ("97.5%"::String) ("ESS"::String)
    ("R-hat"::String) ("analytic"::String)
  let allChains = chains
  forM_ names $ \p -> do
    let vals      = concatMap (chainVals p) allChains
        repChain  = head allChains
        get f     = maybe 0 id (f p repChain)
        mean_     = sum vals / fromIntegral (length vals)
        sd_       = sqrt (sum (map (\v -> (v - mean_)^(2::Int)) vals)
                          / fromIntegral (length vals))
        lo        = get (posteriorQuantile 0.025)
        hi        = get (posteriorQuantile 0.975)
        ess_      = ess (chainVals p repChain)
        rhatV     = maybe 0 id (rhat (map (chainVals p) allChains))
        analytic  = if p == "p_ctrl" then aCtrlMean else aTrtMean
    printf "  %-10s  %8.4f  %8.4f  %8.4f  %8.4f  %8.0f  %8.4f  %8.4f\n"
      (T.unpack p) mean_ sd_ lo hi ess_ rhatV analytic
  putStrLn ""

  -- ── 治療効果の推定 ────────────────────────────────────────────────────
  putStrLn "=== 治療効果の推定 ==="
  let ctrlSamples = concatMap (chainVals "p_ctrl") allChains
      trtSamples  = concatMap (chainVals "p_trt")  allChains
      diffs       = zipWith (-) trtSamples ctrlSamples
      probBetter  = fromIntegral (length (filter (> 0) diffs))
                  / fromIntegral (length diffs) :: Double
      meanDiff    = sum diffs / fromIntegral (length diffs)
      sdDiff      = sqrt (sum (map (\d -> (d - meanDiff)^(2::Int)) diffs)
                         / fromIntegral (length diffs))

  printf "  P(p_trt > p_ctrl) = %.4f  (%.1f%%)\n" probBetter (probBetter * 100)
  printf "  E[p_trt - p_ctrl] = %.4f  (SD=%.4f)\n" meanDiff sdDiff
  printf "  95%% CI of差:      [%.4f, %.4f]\n"
    (quantileOf 0.025 diffs) (quantileOf 0.975 diffs)
  putStrLn ""
  printf "  → %s\n" (interpret probBetter :: String)
  putStrLn ""

  -- ── HTML レポート生成 ────────────────────────────────────────────────
  putStrLn "=== HTML レポート生成 ==="
  let graph = buildModelGraph m   -- HBMP: 依存グラフは Track 型で自動抽出
      report = (defaultReport "Bayesian A/B Test — Clinical Trial" (head chains) names)
                 { reportGraph  = Just graph
                 , reportChains = chains
                 , reportPairs  = [("p_ctrl", "p_trt")]
                 , reportMaxLag = 40
                 }
  renderReport "mcmc_report_clinical.html" report
  putStrLn "  mcmc_report_clinical.html を生成しました"
  openInBrowser "mcmc_report_clinical.html"

-- ---------------------------------------------------------------------------
-- ヘルパー
-- ---------------------------------------------------------------------------

quantileOf :: Double -> [Double] -> Double
quantileOf q xs =
  let sorted = foldr insertSorted [] xs
      n      = length sorted
      idx    = min (n - 1) (max 0 (round (q * fromIntegral (n - 1)) :: Int))
  in sorted !! idx
  where
    insertSorted x []     = [x]
    insertSorted x (y:ys)
      | x <= y    = x : y : ys
      | otherwise = y : insertSorted x ys

interpret :: Double -> String
interpret p
  | p >= 0.99 = "非常に強いエビデンス: 治療が有効"
  | p >= 0.95 = "強いエビデンス: 治療が有効"
  | p >= 0.80 = "中程度のエビデンス: 治療が有効傾向"
  | p >= 0.50 = "弱いエビデンス: 治療がやや有効"
  | otherwise = "エビデンスなし: 治療効果は不明瞭"
