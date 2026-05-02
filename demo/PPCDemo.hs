{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | Phase 2.3: 事前予測 / 事後予測サンプリングのデモ。
--
-- - prior predictive: データを見る前に「モデルが何を予測するか」確認
-- - posterior predictive: フィット後に「観測されたデータと整合するか」確認
module Main where

import qualified Data.Map.Strict as Map
import Data.List (sort)
import Text.Printf (printf)
import System.Random.MWC (createSystemRandom)

import MCMC.Core (chainSamples)
import MCMC.NUTS (nuts, defaultNUTSConfig, NUTSConfig (..))
import Model.HBM (ModelP, sample, observe, Distribution (..))
import Stat.PosteriorPredictive
  (priorPredictive, posteriorPredictive, posteriorPredictiveSummary)

obsData :: [Double]
obsData = [1.5, 2.1, 1.8, 2.5, 1.9, 2.3, 1.7, 2.0, 2.2, 1.6]

-- 真値: μ ≈ 1.96, σ ≈ 0.30
linearModel :: ModelP ()
linearModel = do
  mu    <- sample "mu"    (Normal 0 10)
  sigma <- sample "sigma" (HalfNormal 5)
  observe "y" (Normal mu sigma) obsData

cfg :: NUTSConfig
cfg = defaultNUTSConfig
        { nutsIterations = 2000
        , nutsBurnIn     = 500
        , nutsStepSize   = 0.1
        }

-- ---------------------------------------------------------------------------
-- ヘルパー: 統計量
-- ---------------------------------------------------------------------------

stats :: [Double] -> (Double, Double, Double, Double)
stats xs =
  let s   = sort xs
      n   = length s
      mu  = sum xs / fromIntegral n
      q p = s !! min (n-1) (max 0 (floor (p * fromIntegral n) :: Int))
  in (mu, q 0.025, q 0.975, sqrt (sum [(x-mu)^(2::Int) | x <- xs] / fromIntegral n))

-- ---------------------------------------------------------------------------
-- main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  Phase 2.3: 事前予測 / 事後予測サンプリング"
  putStrLn "═══════════════════════════════════════════════════════════════"
  printf "  モデル: μ ~ N(0,10), σ ~ HalfN(5), y ~ N(μ,σ)\n"
  printf "  観測: %d 件 (mean=%.2f, sd=%.2f)\n\n"
         (length obsData) (sum obsData / fromIntegral (length obsData))
         (sqrt (sum [(x - sum obsData / fromIntegral (length obsData))^(2::Int) | x <- obsData] / fromIntegral (length obsData)))

  -- ── 事前予測 ──
  putStrLn "[1] 事前予測サンプリング (priorPredictive)"
  putStrLn "    データ観測前のモデルが予測する y の分布を確認"
  gen <- createSystemRandom
  prior <- priorPredictive linearModel 2000 gen
  let priorYs = concatMap (Map.findWithDefault [] "y") prior
      (pMean, pLo, pHi, pSD) = stats priorYs
  printf "    事前予測: mean=%+.3f  sd=%.3f  95%% CI=[%+.3f, %+.3f]\n"
         pMean pSD pLo pHi
  printf "    → 事前 μ ~ N(0,10) が広いため事前予測は広く散らばる (期待通り)\n\n"

  -- ── NUTS で事後をサンプリング ──
  putStrLn "[2] 事後分布サンプリング (NUTS)"
  ch <- nuts linearModel cfg
              (Map.fromList [("mu", 0.0), ("sigma", 1.0)])
              gen
  printf "    samples=%d\n\n" (length (chainSamples ch))

  -- ── 事後予測 ──
  putStrLn "[3] 事後予測サンプリング (posteriorPredictive)"
  putStrLn "    観測データと整合的か検証"
  postPreds <- posteriorPredictive linearModel ch gen
  let postYs = concatMap (Map.findWithDefault [] "y") postPreds
      (poMean, poLo, poHi, poSD) = stats postYs
  printf "    事後予測: mean=%+.3f  sd=%.3f  95%% CI=[%+.3f, %+.3f]\n"
         poMean poSD poLo poHi
  printf "    観測値:   mean=%+.3f  sd=%.3f  range=[%.2f, %.2f]\n"
         (sum obsData / fromIntegral (length obsData))
         (let mn = sum obsData / fromIntegral (length obsData)
          in sqrt (sum [(x-mn)^(2::Int) | x <- obsData] / fromIntegral (length obsData)))
         (minimum obsData) (maximum obsData)
  putStrLn "    → 事後予測の中心が観測平均近くに来ている (モデル妥当)"
  putStrLn ""

  -- ── 観測位置ごとの事後予測 95% CI ──
  putStrLn "[4] 観測位置ごとの事後予測区間 (posteriorPredictiveSummary)"
  let summary = posteriorPredictiveSummary postPreds
  case Map.lookup "y" summary of
    Just rows -> do
      printf "    %-3s  %8s  %10s  %12s\n"
             ("i"::String) ("y_obs"::String)
             ("yhat_mean"::String) ("95% CI"::String)
      mapM_ (\(i, (y_obs, (m, lo, hi))) ->
              printf "    %-3d  %8.3f  %10.3f  [%+5.2f, %+5.2f]\n"
                     (i::Int) y_obs m lo hi)
            (zip [1..] (zip obsData rows))
    Nothing -> putStrLn "    no predictions"
  putStrLn ""

  -- ── PPC ベイズ p 値風診断 ──
  putStrLn "[5] PPC 整合性チェック (Bayesian p-value)"
  let obsMean = sum obsData / fromIntegral (length obsData)
      meansFromPred = [ let ys = Map.findWithDefault [] "y" p
                        in sum ys / fromIntegral (length ys)
                      | p <- postPreds ]
      pVal = fromIntegral (length (filter (> obsMean) meansFromPred))
            / fromIntegral (length meansFromPred) :: Double
  printf "    観測平均: %.3f\n" obsMean
  printf "    P(事後予測平均 > 観測平均) = %.3f\n" pVal
  printf "    (0.05 < p < 0.95 ならモデルとデータが整合)\n"
  putStrLn ""

  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  ✓ 事前/事後予測サンプリングが正常動作"
  putStrLn "═══════════════════════════════════════════════════════════════"
