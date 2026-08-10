{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-full-laziness -fno-cse #-}
-- | HBM 勾配評価ボトルネック診断 (Phase 53)。
--
-- 仮説 (一次根拠: @HBM.hs:146@ が @Numeric.AD.Mode.Forward.grad@ を使用・
-- ad-4.5.6 ソースが「reverse mode より O(n) 遅い」と明記) =
-- **前進モード AD ゆえ勾配 1 本が latent 数 p 回の関数評価を要する**。
--
-- 本ベンチで決定的に確認する:
--   (1) 1 勾配 (gradADU) / 1 log-joint (logJoint) の時間比 ≈ p か
--   (2) p (群数で latent を増やす) を振ったとき gradADU 時間が p に線形か
--   (3) per-eval が観測数 N_obs に対してどう伸びるか
--
-- これが真なら最適化方針 = reverse-mode AD への切替で勾配を O(1) sweep 化。
module Main where

import           Control.Monad                    (forM_)
import qualified Data.Map.Strict                  as Map
import qualified Data.Text                        as T
import qualified Data.Vector                      as V
import qualified System.Random.MWC                as MWC
import           System.Random.MWC.Distributions  (standard)
import           Text.Printf                      (printf)

import           Hanalyze.Model.HBM
  ( Distribution (..), ModelP, sample, observe
  , glmmRandomIntercept, GlmmFamily (..)
  , sampleNames, getTransforms, gradADU, logJoint )
import           Hanalyze.Stat.Distribution       (fromUnconstrained)

import           BenchUtil

-- ---------------------------------------------------------------------------
-- モデル
-- ---------------------------------------------------------------------------

m1Model :: [Double] -> [Double] -> ModelP ()
m1Model xs ys = do
  a <- sample "a"     (Normal 0 10)
  b <- sample "b"     (Normal 0 10)
  s <- sample "sigma" (Exponential 1)
  forM_ (zip3 [0 :: Int ..] xs ys) $ \(i, x, y) ->
    observe (T.pack ("y_" ++ show i)) (Normal (a + b * realToFrac x) s) [y]

m2Model :: [[Double]] -> [Int] -> [Double] -> ModelP ()
m2Model xRows gids ys = glmmRandomIntercept GlmmGaussian xRows gids ys

-- ---------------------------------------------------------------------------
-- データ生成 (決定的・CSV 不要・内部診断用)
-- ---------------------------------------------------------------------------

normals :: Int -> Int -> IO [Double]
normals seed k = do
  g <- MWC.initialize (V.singleton (fromIntegral seed))
  mapM (const (standard g)) [1 .. k]

genM1Data :: Int -> IO ([Double], [Double])
genM1Data n = do
  xz <- normals 11 n
  ez <- normals 12 n
  let xs = map (* 2.0) xz
      ys = zipWith (\x e -> 2.0 + 1.5 * x + e) xs ez
  return (xs, ys)

-- | nG 群 × perG 観測の random-intercept データ。
genM2Data :: Int -> Int -> IO ([[Double]], [Int], [Double])
genM2Data nG perG = do
  let n = nG * perG
  xz <- normals 21 n
  ez <- normals 22 n
  uz <- normals 23 nG
  let us   = map (* 1.5) uz
      gids = [ i `div` perG | i <- [0 .. n - 1] ]
      xs   = map (* 2.0) xz
      ys   = [ 1.0 + 0.8 * x + (us !! g) + e | (x, g, e) <- zip3 xs gids ez ]
      xRows = [ [1.0, x] | x <- xs ]
  return (xRows, gids, ys)

-- ---------------------------------------------------------------------------
-- 計時ヘルパ: 1 モデルについて logJoint と gradADU の per-eval を測る
-- ---------------------------------------------------------------------------

-- | unconstrained 初期点 (latent 全 0 = 制約空間でも有限) を作り、
--   logJoint(Double) 1 回と gradADU 1 回の per-eval ナノ秒を返す。
profileModel :: String -> ModelP () -> IO ()
profileModel tag m = do
  let names = sampleNames m
      p     = length names
      tmap  = getTransforms m
      trans = [ Map.findWithDefault err n tmap | n <- names ]
      err   = error "transform missing"
      us0   = replicate p (0.0 :: Double)
      -- logJoint を constrained 空間で評価するための params
      paramsC = Map.fromList
        [ (n, fromUnconstrained t u) | (n, t, u) <- zip3 names trans us0 ]

  -- logJoint 1 回 (Double・前進walk 1 本)
  (ljMs, _) <- timeitIO 50 id (\_ -> pure (logJoint m paramsC))
  -- gradADU 1 回 (前進モード grad = p sweep のはず)
  (gMs, _)  <- timeitIO 50 (sum . map abs)
                 (\_ -> pure (gradADU m names trans us0))

  let ratio = gMs / max 1e-12 ljMs
  printf "%-18s p=%-3d | logJoint=%8.4f ms | gradADU=%9.4f ms | ratio=%6.2f (≈p?)\n"
    tag p ljMs gMs ratio

-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "=== Phase 53: HBM 勾配ボトルネック診断 ==="
  putStrLn "仮説: gradADU/logJoint 比 ≈ p なら前進モード AD が O(p) ボトルネック\n"

  -- (1) M1 / M2 の比較
  (x1, y1) <- genM1Data 100
  (xr, g2, y2) <- genM2Data 8 12
  putStrLn "--- (1) M1 (pooled) vs M2 (random intercept) ---"
  profileModel "M1_pooled(100obs)" (m1Model x1 y1)
  profileModel "M2_ranint(8grp)"   (m2Model xr g2 y2)

  -- (2) 群数を振って p を増やす → gradADU が p に線形か
  putStrLn "\n--- (2) 群数↑ で latent p↑: gradADU が p に線形か (obs/群=12 固定) ---"
  forM_ [2, 4, 8, 16, 32] $ \nG -> do
    (xr', g', y') <- genM2Data nG 12
    profileModel (printf "M2_g%d" nG) (m2Model xr' g' y')

  -- (3) 観測数を振る (p=3 固定の M1) → per-eval が N_obs に線形か
  putStrLn "\n--- (3) 観測数↑ (M1 p=3 固定): logJoint/gradADU が N_obs に線形か ---"
  forM_ [50, 100, 200, 400, 800] $ \nObs -> do
    (xs, ys) <- genM1Data nObs
    profileModel (printf "M1_n%d" nObs) (m1Model xs ys)
