{-# LANGUAGE OverloadedStrings #-}
-- | Phase 89 posteriordb 横断ベンチマーク: 全モデルで使い回す Haskell 側
-- 共有ユーティリティ (Python 側 @_common.py@ と対)。
--
-- 'summarize' は @arviz.summary@ の簡易な代替 (mean / sd / 94% HDI / ESS /
-- R-hat / MCSE を 1 表にまとめる)。
--
-- ★Phase 92 B4: ESS 列を arviz 互換の rank-normalized 多 chain **ess_bulk**
-- ('Hanalyze.Stat.MCMC.essBulk') に切替え、mean/sd/HDI も全 chain
-- プールで計算する (= @az.summary@ と同じ土俵)。旧版は chain 0 のみ +
-- Geyer IMSE ('ess'・tau 下限 1 クランプで n 頭打ち) で、PyMC 側の
-- ess_bulk と直接比較できない指標非対称の原因だった (hmm で 766.7 vs
-- 実際は 3143 = 4.1 倍の過小表示・詳細 = phase-92 md B4)。
module Common
  ( ParamSummary (..)
  , summarize
  , printSummary
  , timeSamplingMs
  ) where

import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import qualified Data.Text as T
import Text.Printf (printf)

import Hanalyze.MCMC.Core (Chain, chainVals)
import Hanalyze.Stat.MCMC (essBulk, rhat, hdi)

data ParamSummary = ParamSummary
  { psName :: T.Text
  , psMean :: Double
  , psSd   :: Double
  , psHdiLo, psHdiHi :: Double  -- ^ 94% HDI (全 chain プールの post-warmup draw)。
  , psEss  :: Double            -- ^ 'essBulk' (arviz 互換 rank-normalized・全 chain)。
  , psRhat :: Maybe Double      -- ^ 全 chain の split-R-hat (chain ≥2 が必要)。
  , psMcseMean :: Double        -- ^ 事後平均の モンテカルロ標準誤差 = sd/√ess。
  }

-- | パラメータごとの要約統計 ('az.summary' 相当・全 chain プール)。
summarize :: [T.Text] -> [Chain] -> [ParamSummary]
summarize pars chains = map summarize1 pars
  where
    summarize1 p =
      let allVals = map (chainVals p) chains
          pooled  = concat allVals
          n       = fromIntegral (length pooled) :: Double
          mean_   = sum pooled / n
          sd_     = sqrt (sum [ (x - mean_) ^ (2 :: Int) | x <- pooled ] / (n - 1))
          (lo, hi) = hdi 0.94 pooled
          essV    = essBulk allVals
      in ParamSummary
           { psName = p, psMean = mean_, psSd = sd_
           , psHdiLo = lo, psHdiHi = hi, psEss = essV
           , psRhat = rhat allVals
           , psMcseMean = sd_ / sqrt essV
           }

-- | 表として整形して標準出力へ。
printSummary :: [ParamSummary] -> IO ()
printSummary ps = do
  printf "%-10s %9s %9s %17s %9s %8s %9s\n"
    ("param" :: String) ("mean" :: String) ("sd" :: String)
    ("hdi_3%..hdi_97%" :: String) ("ess_bulk" :: String) ("r_hat" :: String)
    ("mcse_mean" :: String)
  mapM_ printRow ps
  where
    printRow p = printf "%-10s %9.4f %9.4f [%6.3f, %6.3f] %9.1f %8s %9.5f\n"
      (T.unpack (psName p)) (psMean p) (psSd p) (psHdiLo p) (psHdiHi p)
      (psEss p) (maybe "NA" (printf "%.4f") (psRhat p) :: String) (psMcseMean p)

-- | サンプリング**のみ**の壁時計 (ms) を計測する。PyMC 側マトリクス
-- (@run_pymc_matrix.py@) が @t0 = time.perf_counter(); pm.sample(...)@ で
-- サンプリングだけを計測するのに対応させるための共通部品 (2026-07-11
-- 追加・09-eight-schools/07-gp-regr で「GHC起動+コンパイル試行+
-- dashboardFullOf の PNG 生成を含むプロセス全体」を計測してしまい PyMC 側
-- と比較不能だった反省から)。
--
-- @action@ は @df |-> hbm cfg model@ で得た @HBMModel@ の @hbmChainsR@ 等、
-- 遅延評価で未確定のサンプリング結果を渡す。'force' (deepseq) で完全評価
-- してから時刻差を取るので、遅延サンクの一部だけ強制されて計測が不正確に
-- なることはない。戻り値は @(結果, 経過ms)@。
timeSamplingMs :: NFData a => a -> IO (a, Double)
timeSamplingMs result = do
  t0 <- getCurrentTime
  r  <- evaluate (force result)
  t1 <- getCurrentTime
  pure (r, realToFrac (diffUTCTime t1 t0) * 1000)
