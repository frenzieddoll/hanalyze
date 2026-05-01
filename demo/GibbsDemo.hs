{-# LANGUAGE OverloadedStrings #-}
-- | Gibbs サンプリング + モデル比較 (WAIC / LOO-CV) デモ
--
-- モデル: 正規分布の平均推定
--   μ ~ Normal(0, σ_prior)        ← 事前分布
--   yᵢ ~ Normal(μ, σ_lik = 2)    ← 尤度、σ は既知
--   真値: μ = 3.0, n = 20
--
-- セクション 1: Gibbs vs NUTS サンプリング比較
--   - Gibbs: normalNormal 共役アップデートで直接サンプリング
--   - 解析解と ESS/秒で比較
--
-- セクション 2: WAIC によるモデル比較
--   - モデル A: μ ~ Normal(0, 10)  [弱情報事前]
--   - モデル B: μ ~ Normal(5,  1)  [情報事前・真値からずれた仮定]
--
-- セクション 3: PSIS-LOO 診断
--   - 各観測値の Pareto k̂ (< 0.5 良好、> 0.7 要注意)
--
module Main where

import qualified Data.Map.Strict as Map
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Text.Printf (printf)
import System.Random.MWC (createSystemRandom)

import Model.HBM
import Stat.Distribution (Distribution (..))
import MCMC.Core (chainVals, posteriorMean, posteriorSD)
import MCMC.Gibbs (GibbsConfig (..), defaultGibbsConfig, gibbs, normalNormal)
import MCMC.NUTS  (NUTSConfig (..), defaultNUTSConfig, nuts)
import Stat.MCMC  (ess)
import Stat.ModelSelect

-- ---------------------------------------------------------------------------
-- 合成データ  (真値 μ = 3, σ = 2, n = 20)
-- ---------------------------------------------------------------------------

sigLik :: Double
sigLik = 2.0

obsData :: [Double]
obsData =
  [ 3.2, 1.8, 4.1, 2.9, 3.5, 2.3, 4.5, 3.1, 2.7, 3.8
  , 3.3, 2.5, 4.2, 3.0, 2.8, 3.6, 2.4, 4.0, 3.2, 2.9 ]

-- ---------------------------------------------------------------------------
-- モデル定義
-- ---------------------------------------------------------------------------

-- | モデル A: μ ~ Normal(0, 10) — 弱情報事前分布
modelA :: Model ()
modelA = do
  mu <- sample "mu" (Normal 0 10)
  observe "y" (Normal mu sigLik) obsData

-- | モデル B: μ ~ Normal(5, 1) — 情報事前分布 (真値 μ=3 からずれた仮定)
modelB :: Model ()
modelB = do
  mu <- sample "mu" (Normal 5 1)
  observe "y" (Normal mu sigLik) obsData

-- ---------------------------------------------------------------------------
-- 解析解 (Normal-Normal 共役)
-- ---------------------------------------------------------------------------

-- | 解析的事後平均  μ_post = σ_post² × (μ₀/σ₀² + nȳ/σ_lik²)
analyticPosterior :: Double -> Double -> Double -> Double -> (Double, Double)
analyticPosterior mu0 sig0 ybar n =
  let prec0    = 1 / sig0    ^ (2::Int)
      precLik  = 1 / sigLik  ^ (2::Int)
      precPost = prec0 + n * precLik
      sigPost  = sqrt (1 / precPost)
      muPost   = (mu0 * prec0 + n * ybar * precLik) / precPost
  in (muPost, sigPost)

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

  let initP = Map.fromList [("mu", 0.0 :: Double)]
      n     = fromIntegral (length obsData) :: Double
      ybar  = sum obsData / n

  -- ── 1. Gibbs vs NUTS ─────────────────────────────────────────────────────
  putStrLn "=== Section 1: Gibbs vs NUTS (Normal 平均推定) ==="
  putStrLn ""
  printf "  データ: n=%d, ȳ=%.3f, σ_lik=%.1f (既知), 真値 μ=3.0\n"
    (length obsData) ybar sigLik
  putStrLn ""

  -- Gibbs (5000 サンプル)
  let gibbsUpdates = [ normalNormal "mu" 0 10 obsData sigLik ]
      gibbsCfg     = defaultGibbsConfig { gibbsIterations = 5000, gibbsBurnIn = 500 }
  (gibbsCh, tG) <- timed $ gibbs gibbsUpdates gibbsCfg initP gen

  -- NUTS (5000 サンプル)
  let nutsCfg = defaultNUTSConfig { nutsIterations = 5000, nutsBurnIn = 500, nutsStepSize = 0.5 }
  (nutsCh, tN) <- timed $ nuts modelA nutsCfg initP gen

  -- 解析解
  let (muA, sigA) = analyticPosterior 0 10 ybar n

  printf "  %-10s  mean=%7.4f  SD=%7.4f  ESS=%6.0f  ESS/s=%7.1f\n"
    ("Gibbs"   ::String)
    (maybe 0 id $ posteriorMean "mu" gibbsCh)
    (maybe 0 id $ posteriorSD   "mu" gibbsCh)
    (ess (chainVals "mu" gibbsCh))
    (ess (chainVals "mu" gibbsCh) / tG)
  printf "  %-10s  mean=%7.4f  SD=%7.4f  ESS=%6.0f  ESS/s=%7.1f\n"
    ("NUTS"    ::String)
    (maybe 0 id $ posteriorMean "mu" nutsCh)
    (maybe 0 id $ posteriorSD   "mu" nutsCh)
    (ess (chainVals "mu" nutsCh))
    (ess (chainVals "mu" nutsCh) / tN)
  printf "  %-10s  mean=%7.4f  SD=%7.4f\n"
    ("解析解"  ::String) muA sigA
  putStrLn ""
  putStrLn "  → Gibbs は共役モデルで直接サンプリングできるため ESS/s が高い"
  putStrLn ""

  -- ── 2. WAIC モデル比較 ────────────────────────────────────────────────────
  putStrLn "=== Section 2: WAIC モデル比較 ==="
  putStrLn "  モデル A: μ ~ Normal(0, 10)  [弱情報事前: 真値 μ=3 を広くカバー]"
  putStrLn "  モデル B: μ ~ Normal(5,  1)  [情報事前: μ≈5 を強く仮定、真値からずれ]"
  putStrLn ""

  -- モデル A の WAIC: NUTS チェーンから
  let waicA = chainWAIC modelA nutsCh

  -- モデル B を NUTS で推定
  (nutsChB, _) <- timed $ nuts modelB nutsCfg initP gen
  let waicB = chainWAIC modelB nutsChB
      (muB, _) = analyticPosterior 5 1 ybar n

  printf "  %-10s  事後 mean=%.4f (解析=%.4f)  WAIC=%8.3f  lppd=%8.3f  p_waic=%.3f  SE=%.3f\n"
    ("モデル A"::String) (maybe 0 id $ posteriorMean "mu" nutsCh)  muA
    (waicValue waicA) (waicLppd waicA) (waicPwaic waicA) (waicSE waicA)
  printf "  %-10s  事後 mean=%.4f (解析=%.4f)  WAIC=%8.3f  lppd=%8.3f  p_waic=%.3f  SE=%.3f\n"
    ("モデル B"::String) (maybe 0 id $ posteriorMean "mu" nutsChB) muB
    (waicValue waicB) (waicLppd waicB) (waicPwaic waicB) (waicSE waicB)
  putStrLn ""

  let delta = waicValue waicA - waicValue waicB
  printf "  ΔWAIC(A − B) = %.3f\n" delta
  if delta < -2
    then putStrLn "  → モデル A (弱情報事前) の方が良い当てはまり ✓"
    else if delta > 2
      then putStrLn "  → モデル B (情報事前) の方が良い当てはまり"
      else putStrLn "  → 両モデルの差は誤差範囲内"
  putStrLn ""

  -- ── 3. PSIS-LOO 診断 ──────────────────────────────────────────────────────
  putStrLn "=== Section 3: PSIS-LOO 診断 ==="
  putStrLn ""

  let looA = chainLOO modelA nutsCh
      looB = chainLOO modelB nutsChB

  printf "  モデル A: LOO=%.3f  elpd=%.3f  SE=%.3f  k̂>0.7: %d 観測\n"
    (looValue looA) (looElpd looA) (looSE looA) (looKHatBad looA)
  printf "  モデル B: LOO=%.3f  elpd=%.3f  SE=%.3f  k̂>0.7: %d 観測\n"
    (looValue looB) (looElpd looB) (looSE looB) (looKHatBad looB)
  putStrLn ""

  let deltaLOO = looValue looA - looValue looB
  printf "  ΔLOO(A − B) = %.3f\n" deltaLOO
  putStrLn ""

  putStrLn "  Pareto k̂ 診断 (モデル A, 観測値ごと):"
  putStrLn "  k̂ < 0.5: 良好  |  0.5–0.7: 許容  |  > 0.7: LOO が不安定"
  mapM_ (\(i, k) ->
    printf "    obs %2d: k̂=%.3f  %s\n" (i::Int) k (khatLabel k))
    (zip [1..] (looKHat looA))
  putStrLn ""
  putStrLn "完了"

khatLabel :: Double -> String
khatLabel k
  | k < 0.5   = "良好"
  | k < 0.7   = "許容"
  | otherwise = "要注意"
