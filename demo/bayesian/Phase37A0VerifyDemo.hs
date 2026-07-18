{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | Phase 37-A0 doc 拡充の検証用 demo。
--
-- docs/bayesian/02-probabilistic-model.ja.md の追加節
-- (形式 A / B / C / random slope / multi-level / crossed / prior choice)
-- に載せる sample code をそのまま入れて、 build + 小規模 NUTS で実行可能性を
-- 確認する。 doc に貼る code はここから写経する。
--
-- 各モデルは独立に小さい NUTS で 1 回まわす (50 iter / 25 burn) ので、 doc に
-- 載せる code が "本当にコンパイル + 実行できる" ことの証拠になる。
module Main where

import           Control.Monad             (forM, forM_)
import qualified Data.Map.Strict           as Map
import           Data.Maybe                (fromMaybe)
import qualified Data.Text                 as T
import           System.Random.MWC         (createSystemRandom)
import           Text.Printf               (printf)

import           Hanalyze.MCMC.Core        (acceptanceRate, posteriorMean)
import           Hanalyze.MCMC.NUTS        (NUTSConfig (..), defaultNUTSConfig,
                                            nuts)
import           Hanalyze.Model.HBM        (Distribution (..), ModelP,
                                            indexed, nonCenteredNormal,
                                            observe, sample)

-- ===========================================================================
-- 形式 A: 群ごとデータが分かれている (現 Pattern 5)
-- ===========================================================================

-- | μ ~ Normal(0, 10), τ ~ HalfNormal(5),
--   θ_j ~ Normal(μ, τ),  y_ij ~ Normal(θ_j, σ=1)
schoolModelA :: [[Double]] -> ModelP ()
schoolModelA groupData = do
  mu  <- sample "mu"  (Normal 0 10)
  tau <- sample "tau" (HalfNormal 5)
  forM_ (zip [1 :: Int ..] groupData) $ \(j, ys) -> do
    theta <- sample (indexed "theta" j) (Normal mu tau)
    observe (indexed "y" j) (Normal theta 1) ys

groupDataA :: [[Double]]
groupDataA =
  [ [ 1.1, 0.8, 1.3, 1.0 ]
  , [ 4.9, 5.2, 4.7, 5.1 ]
  , [ 9.0, 8.7, 9.3, 8.9 ]
  ]

initA :: Map.Map T.Text Double
initA = Map.fromList
  [ ("mu", 5), ("tau", 3)
  , ("theta_1", 1), ("theta_2", 5), ("theta_3", 9) ]

-- ===========================================================================
-- 形式 B: long-format (各観測が gid を持つ)
-- ===========================================================================

-- | gid と y の縦持ち data を受け、 per-group θ を先に forM で全部展開してから
--   観測する。 同じ gid を持つ観測を集めて 1 度に observe するのが効率的。
schoolModelB :: [Int] -> [Double] -> ModelP ()
schoolModelB gids ys = do
  let nG = maximum gids + 1
  mu  <- sample "mu"  (Normal 0 10)
  tau <- sample "tau" (HalfNormal 5)
  thetas <- forM [0 .. nG - 1] $ \j ->
    sample (indexed "theta" j) (Normal mu tau)
  forM_ [0 .. nG - 1] $ \j -> do
    let ysG = [y | (g, y) <- zip gids ys, g == j]
    observe (indexed "y" j)
            (Normal (thetas !! j) 1) ysG

gidsB :: [Int]
gidsB = [0,0,0,0, 1,1,1,1, 2,2,2,2]

ysB :: [Double]
ysB = [1.1, 0.8, 1.3, 1.0,  4.9, 5.2, 4.7, 5.1,  9.0, 8.7, 9.3, 8.9]

initB :: Map.Map T.Text Double
initB = initA

-- ===========================================================================
-- 形式 C: non-centered グループ (funnel 回避)
-- ===========================================================================

-- | θ_j ~ Normal(μ, τ) を θ_j_raw ~ Normal(0,1), θ_j = μ + τ·θ_j_raw に書き直す。
--   `nonCenteredNormal` がその差し替えを 1 行で提供する。 latent 名は
--   "theta_j_raw" になり、 推論後 augmentChainWithDeterministic で θ_j を復元
--   できる (doc 中では derived 部は割愛しても OK)。
schoolModelC :: [[Double]] -> ModelP ()
schoolModelC groupData = do
  mu  <- sample "mu"  (Normal 0 10)
  tau <- sample "tau" (HalfNormal 5)
  forM_ (zip [1 :: Int ..] groupData) $ \(j, ys) -> do
    theta <- nonCenteredNormal (indexed "theta" j) mu tau
    observe (indexed "y" j) (Normal theta 1) ys

initC :: Map.Map T.Text Double
initC = Map.fromList
  [ ("mu", 5), ("tau", 3)
  , ("theta_1_raw", 0), ("theta_2_raw", 0), ("theta_3_raw", 0) ]

-- ===========================================================================
-- random slope (α_j, β_j 両方を階層化)
-- ===========================================================================

-- | y_ij ~ Normal(α_j + β_j · x_ij, σ),
--   α_j ~ Normal(μ_α, τ_α),  β_j ~ Normal(μ_β, τ_β)。
--   入力は (x, y) のグループごとのペアリスト。
randomSlope :: [[(Double, Double)]] -> ModelP ()
randomSlope groupData = do
  muA  <- sample "mu_alpha"    (Normal 0 10)
  tauA <- sample "tau_alpha"   (HalfNormal 5)
  muB  <- sample "mu_beta"     (Normal 0 5)
  tauB <- sample "tau_beta"    (HalfNormal 5)
  sig  <- sample "sigma"       (Exponential 1)
  forM_ (zip [1 :: Int ..] groupData) $ \(j, pts) -> do
    alpha <- sample (indexed "alpha" j) (Normal muA tauA)
    beta  <- sample (T.pack ("beta_"  ++ show j)) (Normal muB tauB)
    forM_ pts $ \(x, y) ->
      observe (indexed "y" j)
              (Normal (alpha + beta * realToFrac x) sig) [y]

rsData :: [[(Double, Double)]]
rsData =
  [ zip [0.5, 1.0, 1.5, 2.0] [1.6, 1.2, 0.8, 0.4]
  , zip [0.5, 1.0, 1.5, 2.0] [4.85, 4.70, 4.55, 4.40]
  , zip [0.5, 1.0, 1.5, 2.0] [8.10, 8.20, 8.30, 8.40]
  ]

initRS :: Map.Map T.Text Double
initRS = Map.fromList
  [ ("mu_alpha", 5), ("tau_alpha", 3)
  , ("mu_beta", 0),  ("tau_beta", 1)
  , ("sigma", 0.3)
  , ("alpha_1", 2), ("alpha_2", 5), ("alpha_3", 8)
  , ("beta_1", -0.8), ("beta_2", -0.3), ("beta_3", 0.2) ]

-- ===========================================================================
-- multi-level (3-level nested): district → school → students
-- ===========================================================================

-- | 地区 d 内に学校 (d,s) があり、 その中に生徒 (d, s, i) がいる。
--   μ ← Normal(0, 10)
--   τ_d ← HalfNormal(5)               (地区間 SD)
--   τ_s ← HalfNormal(5)               (学校間 SD、 地区共通)
--   δ_d ← Normal(μ, τ_d)              (地区効果)
--   θ_{d,s} ← Normal(δ_d, τ_s)        (学校効果)
--   y_{d,s,i} ← Normal(θ_{d,s}, 1)
--
--   入力: 各地区につき、 学校ごとの観測リストのリスト。
multiLevel :: [[[Double]]] -> ModelP ()
multiLevel byDistrict = do
  mu  <- sample "mu"    (Normal 0 10)
  tD  <- sample "tau_d" (HalfNormal 5)
  tS  <- sample "tau_s" (HalfNormal 5)
  forM_ (zip [1 :: Int ..] byDistrict) $ \(d, schools) -> do
    delta <- sample (indexed "delta" d) (Normal mu tD)
    forM_ (zip [1 :: Int ..] schools) $ \(s, ys) -> do
      theta <- sample (T.pack (concat ["theta_", show d, "_", show s]))
                      (Normal delta tS)
      observe (T.pack (concat ["y_", show d, "_", show s]))
              (Normal theta 1) ys

mlData :: [[[Double]]]
mlData =
  [ [ [1.1, 0.8, 1.3], [1.5, 1.2, 1.7] ]   -- 地区 1 に学校 2 つ
  , [ [4.9, 5.2, 4.7], [5.5, 5.3, 5.7] ]   -- 地区 2 に学校 2 つ
  , [ [8.9, 9.0, 8.7], [8.4, 8.6, 8.5] ]   -- 地区 3 に学校 2 つ
  ]

initML :: Map.Map T.Text Double
initML = Map.fromList $
  [ ("mu", 5), ("tau_d", 3), ("tau_s", 1)
  , ("delta_1", 1), ("delta_2", 5), ("delta_3", 9) ]
  ++ [ (T.pack (concat ["theta_", show d, "_", show s]), v)
     | (d, v) <- [(1::Int, 1.3), (2, 5.4), (3, 8.7)]
     , s <- [1 :: Int .. 2] ]

-- ===========================================================================
-- crossed random effects: school × year
-- ===========================================================================

-- | 学校 s と年度 t が **交差** (どの (s, t) ペアでも観測される)。
--   α_s ← Normal(μ_α, τ_α)
--   γ_t ← Normal(0, τ_γ)
--   y_{s,t,i} ← Normal(α_s + γ_t, σ)
--
--   入力: [(sid, tid, y)] の long-format。
crossed :: Int -> Int -> [(Int, Int, Double)] -> ModelP ()
crossed nS nT obs = do
  muA <- sample "mu_alpha" (Normal 0 10)
  tA  <- sample "tau_a"    (HalfNormal 5)
  tG  <- sample "tau_g"    (HalfNormal 5)
  sig <- sample "sigma"    (Exponential 1)
  alphas <- forM [0 .. nS - 1] $ \s ->
    sample (indexed "alpha" s) (Normal muA tA)
  gammas <- forM [0 .. nT - 1] $ \t ->
    sample (indexed "gamma" t) (Normal 0 tG)
  forM_ obs $ \(s, t, y) ->
    observe (T.pack (concat ["y_", show s, "_", show t]))
            (Normal (alphas !! s + gammas !! t) sig) [y]

crossedObs :: [(Int, Int, Double)]
crossedObs =
  [ (0,0, 1.0), (0,1, 1.3), (0,2, 0.9)
  , (1,0, 4.8), (1,1, 5.2), (1,2, 5.0)
  , (2,0, 8.7), (2,1, 9.1), (2,2, 8.9)
  ]

initCrossed :: Map.Map T.Text Double
initCrossed = Map.fromList
  [ ("mu_alpha", 5), ("tau_a", 3), ("tau_g", 0.3), ("sigma", 0.3)
  , ("alpha_0", 1), ("alpha_1", 5), ("alpha_2", 9)
  , ("gamma_0", 0), ("gamma_1", 0), ("gamma_2", 0) ]

-- ===========================================================================
-- prior choice: HalfNormal vs HalfCauchy on τ
-- ===========================================================================

-- | 弱情報事前 HalfNormal(5) 版 (Gelman 2006 推奨)。
priorHalfNormal :: [[Double]] -> ModelP ()
priorHalfNormal = schoolModelA  -- HalfNormal(5) を使う実装は schoolModelA と同じ

-- | 重い裾 HalfCauchy(2.5) 版。
priorHalfCauchy :: [[Double]] -> ModelP ()
priorHalfCauchy groupData = do
  mu  <- sample "mu"  (Normal 0 10)
  tau <- sample "tau" (HalfCauchy 2.5)
  forM_ (zip [1 :: Int ..] groupData) $ \(j, ys) -> do
    theta <- sample (indexed "theta" j) (Normal mu tau)
    observe (indexed "y" j) (Normal theta 1) ys

-- ===========================================================================
-- 検証 runner
-- ===========================================================================

verify
  :: T.Text
  -> ModelP ()
  -> Map.Map T.Text Double
  -> [T.Text]   -- ^ 確認したい posterior mean パラメータ
  -> IO ()
verify label model initP showVars = do
  gen <- createSystemRandom
  ch  <- nuts model smallCfg initP gen
  printf "[%s] accept=%.2f  " (T.unpack label) (acceptanceRate ch)
  forM_ showVars $ \v ->
    printf " %s=%.2f" (T.unpack v) (fromMaybe (0 :: Double) (posteriorMean v ch))
  putStrLn ""
  where
    smallCfg = defaultNUTSConfig
      { nutsIterations = 100
      , nutsBurnIn     = 50
      , nutsStepSize   = 0.05
      , nutsMaxDepth   = 6
      }

main :: IO ()
main = do
  putStrLn "═══ Phase 37 A0 sample code verification ═══"
  verify "A (per-group data)"   (schoolModelA  groupDataA) initA
         ["mu", "tau", "theta_1", "theta_2", "theta_3"]
  verify "B (long-format)"      (schoolModelB  gidsB ysB)  initB
         ["mu", "tau", "theta_1"]
  verify "C (non-centered)"     (schoolModelC  groupDataA) initC
         ["mu", "tau"]
  verify "random slope"         (randomSlope   rsData)     initRS
         ["mu_alpha", "mu_beta", "beta_1", "beta_3"]
  verify "multi-level (3-lvl)"  (multiLevel    mlData)     initML
         ["mu", "tau_d", "tau_s", "delta_1", "delta_3"]
  verify "crossed (S × T)"      (crossed 3 3 crossedObs)   initCrossed
         ["mu_alpha", "alpha_0", "alpha_2", "gamma_1"]
  verify "prior HalfNormal(5)"  (priorHalfNormal groupDataA) initA
         ["mu", "tau"]
  verify "prior HalfCauchy(2.5)" (priorHalfCauchy groupDataA) initA
         ["mu", "tau"]
  putStrLn "═══ all 8 models compiled + ran ═══"
