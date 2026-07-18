{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | NUTS コストセンタ・プロファイル用の最小実行体 (Phase 53 追加調査)。
--
-- 引数でモデルを選び 1 chain (warmup 500 + 800 draws) を走らせ、
-- @+RTS -p@ で `prof-nuts.prof` を吐かせ per-draw ボトルネックを局在化する:
--   m2 (既定) = 階層 random intercept (glmm helper・高速経路の代表)
--   m3        = random intercept+slope per-obs 手書き (中間: u は REff 昇格・
--               v は dense 列 + v-prior が residual ad walk 残留)
--   m5        = パラメタ非線形 a·exp(-b·x)+c per-obs 手書き (54.8 合成不可 =
--               walk + ad fallback。 Phase 54.9 の本命)
--   m6        = 階層 × 非線形 a_g·exp(-b·x) per-obs 手書き (M5 と同型 fallback・
--               差分 = 階層 prior。 54.9 では差分確認のみ)
--   m7        = Poisson 回帰 exp(a+b·x) per-obs 手書き (非 Gaussian 観測 =
--               高速経路対象外。 Phase 55.1 の支配項確定用)
--   m8        = logistic 回帰 invLogit(a+b·x) per-obs 手書き (同上)
-- モデル定義・DGP は `BenchHBMScaling.hs` と同一 (seed も同じ・CSV は書かない)。
module Main where

import           Control.Monad                    (forM_)
import qualified Data.Map.Strict                  as Map
import qualified Data.Text                        as T
import qualified Data.Vector                      as V
import           System.Environment               (getArgs)
import qualified System.Random.MWC                as MWC
import           System.Random.MWC.Distributions  (standard)

import           Hanalyze.Model.HBM               (Distribution (..), ModelP,
                                                   sample, observe, sampleDist,
                                                   glmmRandomIntercept,
                                                   GlmmFamily (..))
import           Hanalyze.MCMC.Core               (chainVals, posteriorMean)
import           Hanalyze.MCMC.NUTS               (NUTSConfig (..),
                                                   defaultNUTSConfig, nuts)

-- ---------------------------------------------------------------------------
-- DGP (BenchHBMScaling.hs と同一 seed・同一式)
-- ---------------------------------------------------------------------------

normals :: Int -> Int -> IO [Double]
normals seed k = do
  g <- MWC.initialize (V.singleton (fromIntegral seed))
  mapM (const (standard g)) [1 .. k]

nGroups, perGroup :: Int
nGroups  = 8
perGroup = 12

genM2 :: IO ([[Double]], [Int], [Double])
genM2 = do
  let n = nGroups * perGroup
  xz <- normals 21 n
  ez <- normals 22 n
  uz <- normals 23 nGroups
  let us   = map (* 1.5) uz
      gids = [ i `div` perGroup | i <- [0 .. n - 1] ]
      xs   = map (* 2.0) xz
      ys   = [ 1.0 + 0.8 * x + (us !! g) + e | (x, g, e) <- zip3 xs gids ez ]
      xRows = [ [1.0, x] | x <- xs ]
  return (xRows, gids, ys)

genM3 :: IO ([Double], [Int], [Double])
genM3 = do
  let (b0, b1, tauU, tauV, s) = (1.0, 0.8, 1.0, 0.5, 1.0)
      n = nGroups * perGroup
  xz <- normals 31 n
  ez <- normals 32 n
  uz <- normals 33 nGroups
  vz <- normals 34 nGroups
  let us   = map (* tauU) uz
      vs   = map (* tauV) vz
      gids = [ i `div` perGroup | i <- [0 .. n - 1] ]
      xs   = map (* 2.0) xz
      ys   = [ b0 + b1 * x + (us !! g) + (vs !! g) * x + s * e
             | (x, g, e) <- zip3 xs gids ez ]
  return (xs, gids, ys)

genM5 :: IO ([Double], [Double])
genM5 = do
  let (a, b, c, s) = (2.5, 1.2, 0.5, 0.3)
      nM5 = 100 :: Int
  ez <- normals 51 nM5
  let xs = [ 3.0 * (fromIntegral i + 0.5) / fromIntegral nM5
           | i <- [0 .. nM5 - 1] ]
      ys = [ a * exp (negate b * x) + c + s * e | (x, e) <- zip xs ez ]
  return (xs, ys)

genM6 :: IO ([Double], [Int], [Double])
genM6 = do
  let (muA, tauA, b, s) = (2.0, 0.5, 1.0, 0.3)
      n = nGroups * perGroup
  ez <- normals 61 n
  az <- normals 62 nGroups
  let as   = [ muA + tauA * z | z <- az ]
      gids = [ i `div` perGroup | i <- [0 .. n - 1] ]
      xs   = [ 3.0 * (fromIntegral (i `mod` perGroup) + 0.5)
                   / fromIntegral perGroup
             | i <- [0 .. n - 1] ]
      ys   = [ (as !! g) * exp (negate b * x) + s * e
             | (x, g, e) <- zip3 xs gids ez ]
  return (xs, gids, ys)

genM7 :: IO ([Double], [Double])
genM7 = do
  let (a, b) = (0.5, 0.8)
      nM7 = 100 :: Int
  xs <- normals 71 nM7
  g  <- MWC.initialize (V.singleton 72)
  ys <- mapM (\x -> sampleDist (Poisson (exp (a + b * x))) g) xs
  return (xs, ys)

genM8 :: IO ([Double], [Double])
genM8 = do
  let (a, b) = (0.3, 1.2)
      nM8 = 100 :: Int
  xs <- normals 81 nM8
  g  <- MWC.initialize (V.singleton 82)
  ys <- mapM (\x -> sampleDist
                      (Bernoulli (1 / (1 + exp (negate (a + b * x))))) g) xs
  return (xs, ys)

-- ---------------------------------------------------------------------------
-- モデル定義 (BenchHBMScaling.hs と同一)
-- ---------------------------------------------------------------------------

m2Model :: [[Double]] -> [Int] -> [Double] -> ModelP ()
m2Model xRows gids ys = glmmRandomIntercept GlmmGaussian xRows gids ys

m3Model :: [Double] -> [Int] -> [Double] -> ModelP ()
m3Model xs gids ys = do
  let nG = if null gids then 0 else maximum gids + 1
  b0 <- sample "beta_0" (Normal 0 5)
  b1 <- sample "beta_1" (Normal 0 5)
  tu <- sample "tau_u"  (HalfNormal 5)
  tv <- sample "tau_v"  (HalfNormal 5)
  us <- mapM (\j -> sample (T.pack ("u_" ++ show j)) (Normal 0 tu)) [0 .. nG - 1]
  vs <- mapM (\j -> sample (T.pack ("v_" ++ show j)) (Normal 0 tv)) [0 .. nG - 1]
  s  <- sample "sigma" (Exponential 1)
  forM_ (zip3 [0 :: Int ..] (zip xs gids) ys) $ \(i, (x, g), y) ->
    observe (T.pack ("y_" ++ show i))
      (Normal (b0 + b1 * realToFrac x + us !! g + (vs !! g) * realToFrac x) s) [y]

m6Model :: [Double] -> [Int] -> [Double] -> ModelP ()
m6Model xs gids ys = do
  let nG = if null gids then 0 else maximum gids + 1
  muA  <- sample "mu_a"  (Normal 0 10)
  tauA <- sample "tau_a" (HalfNormal 2)
  as   <- mapM (\j -> sample (T.pack ("a_" ++ show j)) (Normal muA tauA))
               [0 .. nG - 1]
  b    <- sample "b" (HalfNormal 2)
  s    <- sample "sigma" (Exponential 1)
  forM_ (zip3 [0 :: Int ..] (zip xs gids) ys) $ \(i, (x, g), y) ->
    observe (T.pack ("y_" ++ show i))
      (Normal ((as !! g) * exp (negate b * realToFrac x)) s) [y]

m5Model :: [Double] -> [Double] -> ModelP ()
m5Model xs ys = do
  a <- sample "a" (Normal 0 10)
  b <- sample "b" (HalfNormal 2)
  c <- sample "c" (Normal 0 10)
  s <- sample "sigma" (Exponential 1)
  forM_ (zip3 [0 :: Int ..] xs ys) $ \(i, x, y) ->
    observe (T.pack ("y_" ++ show i))
      (Normal (a * exp (negate b * realToFrac x) + c) s) [y]

m7Model :: [Double] -> [Double] -> ModelP ()
m7Model xs ys = do
  a <- sample "a" (Normal 0 5)
  b <- sample "b" (Normal 0 5)
  forM_ (zip3 [0 :: Int ..] xs ys) $ \(i, x, y) ->
    observe (T.pack ("y_" ++ show i))
      (Poisson (exp (a + b * realToFrac x))) [y]

m8Model :: [Double] -> [Double] -> ModelP ()
m8Model xs ys = do
  a <- sample "a" (Normal 0 5)
  b <- sample "b" (Normal 0 5)
  forM_ (zip3 [0 :: Int ..] xs ys) $ \(i, x, y) ->
    observe (T.pack ("y_" ++ show i))
      (Bernoulli (1 / (1 + exp (negate (a + b * realToFrac x))))) [y]

-- ---------------------------------------------------------------------------
-- 実行 (warmup 500 + 800 draws・seed 42・BenchHBMScaling の mkConfig と同一)
-- ---------------------------------------------------------------------------

profConfig :: NUTSConfig
profConfig = defaultNUTSConfig
  { nutsIterations = 800, nutsBurnIn = 500
  , nutsStepSize = 0.1, nutsMaxDepth = 10
  , nutsAdaptStepSize = True, nutsTargetAccept = 0.8, nutsAdaptMass = True }

groupNames :: String -> [T.Text]
groupNames pre = [ T.pack (pre ++ show j) | j <- [0 .. nGroups - 1] ]

runProf :: ModelP () -> Map.Map T.Text Double -> [T.Text] -> IO ()
runProf mdl initP names = do
  gen <- MWC.initialize (V.singleton 42)
  ch <- nuts mdl profConfig initP gen
  -- 全 chain を force (プロファイル対象を確実に評価)
  let s = sum [ maybe 0 id (posteriorMean p ch) | p <- names ]
          + sum (map (\p -> sum (chainVals p ch)) names) * 0
  print (s :: Double)

main :: IO ()
main = do
  args <- getArgs
  let which = case args of { (w : _) -> w; [] -> "m2" }
  case which of
    "m3" -> do
      (xs, gids, ys) <- genM3
      let initP = Map.fromList $
            [ ("beta_0", 1.0), ("beta_1", 0.8), ("tau_u", 1.0), ("tau_v", 0.5)
            , ("sigma", 1.0) ]
            ++ [ (u, 0.0) | u <- groupNames "u_" ++ groupNames "v_" ]
          names = ["beta_0", "beta_1", "tau_u", "tau_v", "sigma"]
                  ++ groupNames "u_" ++ groupNames "v_"
      runProf (m3Model xs gids ys) initP names
    "m6" -> do
      (xs, gids, ys) <- genM6
      let initP = Map.fromList $
            [ ("mu_a", 2.0), ("tau_a", 0.5), ("b", 1.0), ("sigma", 0.3) ]
            ++ [ (a, 2.0) | a <- groupNames "a_" ]
          names = ["mu_a", "tau_a", "b", "sigma"] ++ groupNames "a_"
      runProf (m6Model xs gids ys) initP names
    "m5" -> do
      (xs, ys) <- genM5
      let initP = Map.fromList
            [("a", 2.5), ("b", 1.2), ("c", 0.5), ("sigma", 0.3)]
          names = ["a", "b", "c", "sigma"]
      runProf (m5Model xs ys) initP names
    "m7" -> do
      (xs, ys) <- genM7
      runProf (m7Model xs ys)
        (Map.fromList [("a", 0.5), ("b", 0.8)]) ["a", "b"]
    "m8" -> do
      (xs, ys) <- genM8
      runProf (m8Model xs ys)
        (Map.fromList [("a", 0.3), ("b", 1.2)]) ["a", "b"]
    _ -> do
      (xRows, gids, ys) <- genM2
      let initP = Map.fromList $
            [ ("beta_0", 1.0), ("beta_1", 0.8), ("tau_u", 1.5), ("sigma", 1.0) ]
            ++ [ (u, 0.0) | u <- groupNames "u_" ]
          names = ["beta_0", "beta_1", "tau_u", "sigma"] ++ groupNames "u_"
      runProf (m2Model xRows gids ys) initP names
