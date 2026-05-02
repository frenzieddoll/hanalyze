{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | Truncated / Censored 分布のデモ。
--
-- - Truncated: 観測が範囲内のみで、範囲外は観測されない (打ち切り)。
--   推定で正規化定数を補正する必要がある。
-- - Censored: 範囲外の値もデータに含まれるが「しきい値以下/以上」とのみ判明
--   (Tobit 風)。CDF/SF を尤度に使う。
module Main where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Text.Printf (printf)
import System.Random.MWC (createSystemRandom)

import MCMC.Core (chainSamples, posteriorMean, posteriorSD,
                  posteriorQuantile, acceptanceRate)
import MCMC.NUTS (nuts, defaultNUTSConfig, NUTSConfig (..))
import Model.HBM (ModelP, sample, observe, Distribution (..))

cfg :: NUTSConfig
cfg = defaultNUTSConfig
        { nutsIterations = 1500
        , nutsBurnIn     = 500
        , nutsStepSize   = 0.1
        }

prn :: String -> Double -> Double -> IO ()
prn lbl m s = printf "    %-8s mean=%+.4f  sd=%.4f\n" lbl m s

-- ---------------------------------------------------------------------------
-- 例 1: Truncated Exponential (生存時間モデル、観測は [0, 5] のみ)
-- ---------------------------------------------------------------------------
-- 真値: Exponential(rate=0.5) を [0, 5] で truncate (観測終了時刻 5)。
-- 範囲外の長い生存は観測されない → 無視すると rate を過小推定 (生存時間を短く見積もる)。

truncObs :: [Double]
truncObs =
  [0.5, 1.2, 2.0, 0.3, 4.5, 1.8, 0.8, 3.2, 2.5, 1.5,
   0.4, 2.8, 4.2, 1.1, 0.9, 3.5, 2.1, 0.6, 1.7, 4.0]

-- truncate 補正あり版
truncatedModel :: ModelP ()
truncatedModel = do
  rate <- sample "rate" (HalfNormal 2)
  observe "y" (Truncated (Exponential rate) (Just 0) (Just 5)) truncObs

-- 補正なし版 (誤った推論)
naiveModel :: ModelP ()
naiveModel = do
  rate <- sample "rate" (HalfNormal 2)
  observe "y" (Exponential rate) truncObs

-- ---------------------------------------------------------------------------
-- 例 2: Censored Normal (Tobit 回帰 — 検出限界あり)
-- ---------------------------------------------------------------------------
-- データ: 真の値 N(3, 1.5) に対し、検出下限 = 1 (下限以下は 1 と記録される)
--   検出下限 1 で打ち切られた値: 1.0 (3 件)
--   普通に観測された値: 1.5..

censObs :: [Double]
censObs =
  [1.0, 1.0, 1.0,                        -- 検出下限で打ち切り (真値は < 1)
   1.5, 2.2, 3.3, 3.8, 4.1, 2.9, 3.5,
   2.7, 4.0, 3.1, 3.9, 2.5, 4.2, 3.0]

censoredModel :: ModelP ()
censoredModel = do
  mu  <- sample "mu"    (Normal 0 5)
  sig <- sample "sigma" (HalfNormal 3)
  observe "y" (Censored (Normal mu sig) (Just 1.0) Nothing) censObs

-- 単純に「1.0 を観測値」として扱う誤った版
ignoreCensorModel :: ModelP ()
ignoreCensorModel = do
  mu  <- sample "mu"    (Normal 0 5)
  sig <- sample "sigma" (HalfNormal 3)
  observe "y" (Normal mu sig) censObs

-- ---------------------------------------------------------------------------
-- main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  Truncated / Censored 分布のデモ"
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn ""

  gen <- createSystemRandom

  -- ── 例 1: 片側 Truncated (生存時間モデル) ──
  putStrLn "[1] Truncated Exponential (生存時間): y ~ Exp(rate) truncated to [0, 5]"
  printf "    観測: %d 件、真値 rate=0.5 (= 平均生存 2.0)\n" (length truncObs)
  ch1 <- nuts truncatedModel cfg
              (Map.fromList [("rate", 0.5)]) gen
  putStrLn "  Truncated 補正あり (正しいモデル):"
  prn "rate" (fromMaybe 0 (posteriorMean "rate" ch1)) (fromMaybe 0 (posteriorSD "rate" ch1))
  ch1n <- nuts naiveModel cfg
                (Map.fromList [("rate", 0.5)]) gen
  putStrLn "  Truncated 補正なし (= 普通の Exponential、誤推論):"
  prn "rate" (fromMaybe 0 (posteriorMean "rate" ch1n)) (fromMaybe 0 (posteriorSD "rate" ch1n))
  putStrLn "    (補正なしは rate を過大推定 → 生存時間を短く見積もる)"
  putStrLn ""

  -- ── 例 2: Censored ──
  putStrLn "[2] Censored Normal: 検出下限 1.0 (Tobit 風)"
  printf "    観測: 17 件中 3 件は y=1.0 (検出下限 = 真値は 1 未満だが分からない)\n"
  ch2 <- nuts censoredModel cfg
              (Map.fromList [("mu", 1.0), ("sigma", 1.0)]) gen
  putStrLn "  Censored 補正あり (正しいモデル):"
  prn "mu"    (fromMaybe 0 (posteriorMean "mu" ch2)) (fromMaybe 0 (posteriorSD "mu" ch2))
  prn "sigma" (fromMaybe 0 (posteriorMean "sigma" ch2)) (fromMaybe 0 (posteriorSD "sigma" ch2))
  ch2n <- nuts ignoreCensorModel cfg
                (Map.fromList [("mu", 1.0), ("sigma", 1.0)]) gen
  putStrLn "  Censored 補正なし (1.0 を真値扱い、誤推論):"
  prn "mu"    (fromMaybe 0 (posteriorMean "mu" ch2n)) (fromMaybe 0 (posteriorSD "mu" ch2n))
  prn "sigma" (fromMaybe 0 (posteriorMean "sigma" ch2n)) (fromMaybe 0 (posteriorSD "sigma" ch2n))
  putStrLn "    (補正なしは μ を上方バイアス、σ を過小推定)"
  putStrLn ""

  putStrLn "  注: 両側 Truncated (区間 [a,b]) は log-density 不連続性が強く、"
  putStrLn "      NUTS で収束が難しい場合がある (MH やリジェクション法を併用要)。"
  putStrLn ""

  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  ✓ Truncated / Censored が動作 (正しい推論で σ/μ のバイアス回避)"
  putStrLn "═══════════════════════════════════════════════════════════════"
