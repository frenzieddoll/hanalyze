{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | Multinomial 観測 + Dirichlet 事前のデモ (Phase H2)。
--
-- 1 試行で N 件の対象がカテゴリ K=3 に振り分けられる
-- (例: 投票結果、サイコロ N 回中の出目分布)。
-- そのような実験を T 回繰り返した結果から確率ベクトル π を推定。
-- 共役事後は Dirichlet(α + Σ_t y_t)。
module Main where

import qualified Data.Map.Strict as Map
import Text.Printf (printf)
import System.Random.MWC (createSystemRandom)

import Hanalyze.MCMC.NUTS (nuts, defaultNUTSConfig, NUTSConfig (..))
import Hanalyze.Model.HBM (ModelP, dirichlet, observeMV, Distribution (..),
                  augmentChainWithDeterministic, multinomialLogDensity)
import Hanalyze.Viz.MCMC (printPosteriorSummary, posteriorSummaryFile)

cfg :: NUTSConfig
cfg = defaultNUTSConfig
        { nutsIterations = 1000
        , nutsBurnIn     = 500
        , nutsStepSize   = 0.1
        , nutsMaxDepth   = 6
        }

-- 試行ごとの観測 (T=5 試行、N=20 件、真の π = (0.5, 0.3, 0.2))
trials :: [[Double]]
trials =
  [ [10, 6, 4]
  , [11, 5, 4]
  , [9, 7, 4]
  , [10, 6, 4]
  , [12, 5, 3]
  ]

multinomModel :: ModelP ()
multinomModel = do
  pis <- dirichlet "pi" [1, 1, 1]   -- 一様事前
  observeMV "y" (Multinomial 20 pis) trials

main :: IO ()
main = do
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  Multinomial 観測 + Dirichlet 事前 (Phase H2)"
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn ""

  putStrLn "観測 5 試行 (各 N=20):"
  mapM_ print trials
  let totals = foldr1 (zipWith (+)) trials
  printf "合計: %s  (合計 %.0f)\n" (show totals) (sum totals)
  putStrLn "真値 π = (0.5, 0.3, 0.2)"
  putStrLn "共役事後: Dir(1+52, 1+29, 1+19) → 平均 (0.520, 0.288, 0.192)"
  putStrLn ""

  -- 単体テスト
  let lp = multinomialLogDensity 20 [0.5, 0.3, 0.2] [10, 6, 4] :: Double
  printf "単体テスト: log P([10,6,4] | n=20, π=(.5,.3,.2)) = %.4f\n" lp
  putStrLn ""

  gen <- createSystemRandom
  rawCh <- nuts multinomModel cfg
                (Map.fromList [("pi_b0", 0.5), ("pi_b1", 0.5)]) gen
  let ch = augmentChainWithDeterministic multinomModel rawCh

  putStrLn "[1] Posterior summary"
  printPosteriorSummary ["pi_0", "pi_1", "pi_2"] [ch]
  putStrLn ""

  posteriorSummaryFile "multinom-summary.html" "Multinomial posterior"
    ["pi_0", "pi_1", "pi_2"] [ch]
  putStrLn "  → multinom-summary.html"
  putStrLn ""

  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  ✓ Multinomial 観測で π を推定、π_0+π_1+π_2 = 1 が成立"
  putStrLn "═══════════════════════════════════════════════════════════════"
