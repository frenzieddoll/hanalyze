{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-full-laziness -fno-cse #-}
-- | HBM サンプラ性能スケーリングベンチ (hanalyze NUTS vs PyMC)。
--
-- 目的: HBM (NUTS) の wall-time が post-warmup サンプル数 @iter@ に対して
-- 線形 (O(iter)) に伸びるか、 また per-sample 単価が PyMC と比べてどうかを
-- 計測する。 warmup を固定し本サンプル数だけ掃くことで
--   total = (warmup 固定費) + (1 サンプル単価) * iter
-- の線形フィットで切片 (固定費) と傾き (単価) を分離する。
--
-- モデル階層 (簡単→複雑):
--   M1 pooled 単回帰        y_i ~ N(a + b·x_i, σ)
--   M2 階層 random intercept y_ij ~ N(β0 + β1·x_ij + u_{g(i)}, σ),
--                            u_j ~ N(0, τ_u)
--   M3 階層 random intercept+slope (Phase 54.8 後拡張)
--                            y_ij ~ N(β0 + β1·x + u_g + v_g·x, σ)
--   M4 多変量 X pooled       y_i ~ N(β0 + Σ_{k=1..10} β_k·x_ik, σ)
--   M5 パラメタ非線形        y_i ~ N(a·exp(-b·x_i) + c, σ)
--   M6 組合せ (階層×非線形)   y_ij ~ N(a_g·exp(-b·x_ij), σ), a_g ~ N(μ_a, τ_a)
--   M7 Poisson 回帰 (Phase 55) y_i ~ Poisson(exp(a + b·x_i))
--   M8 logistic 回帰 (Phase 55) y_i ~ Bernoulli(invLogit(a + b·x_i))
--   M9 NegBin 回帰 (Phase 56.6) y_i ~ NegBin(exp(a + b·x_i), α)
--
-- M3-M9 は **per-obs scalar observe の手書き** (汎用 authoring) で組む:
-- M3/M4 は affine ゆえ Phase 54.8 の自動 ObserveLM 合成が乗り、 M5/M6 は
-- パラメタ非線形ゆえ合成不可 = walk+ad fallback。 M7/M8 は非 Gaussian 観測
-- ゆえ高速経路対象外 (Phase 55 改善前 baseline)。 M9 は 56.5 の IR 吸収
-- (lgamma 項含む) の per-draw 効果測定用。 「汎用に書いてどこまで
-- 速いか」 を正直に測る構成。
--
-- データは決定的 DGP で生成し @bench/data/hbm_m{1..9}.csv@ に書き出す
-- (Python 側 @bench_hbm_scaling.py@ が同じ CSV を読む = 公平比較)。
-- 結果は unified BenchRow CSV @bench/results/haskell/hbm_scaling.csv@ へ。
-- 引数: 無し=M1-M8 全部 / @glm@=M7-M9 のみ (hbm_scaling_glm.csv) /
-- @m5-long@/@m7-long@/@m8-long@/@m9-long@=延長 grid (hbm_scaling_<m>_long.csv)。
module Main where

import           Control.Monad                    (forM_)
import           System.Environment               (getArgs)
import qualified Data.Map.Strict                  as Map
import qualified Data.Text                        as T
import qualified Data.Vector                      as V
import qualified System.Random.MWC                as MWC
import           System.Random.MWC.Distributions  (standard)
import           Text.Printf                      (printf)
import           System.IO                        (withFile, IOMode (..),
                                                   hPutStrLn, hSetBuffering,
                                                   BufferMode (..))

import           Hanalyze.Model.HBM               (Distribution (..), ModelP,
                                                   sample, observe, sampleDist,
                                                   glmmRandomIntercept,
                                                   GlmmFamily (..))
-- Distribution(..) は HalfCauchy 等 全コンストラクタを含む (eight schools で使用)。
import           Hanalyze.MCMC.Core               (Chain, chainAccepted,
                                                   chainTreeDepths,
                                                   chainTotal, chainVals,
                                                   posteriorMean)
import           Hanalyze.MCMC.NUTS               (NUTSConfig (..),
                                                   defaultNUTSConfig, nuts)
import           Hanalyze.Stat.MCMC               (ess)
import           Hanalyze.Fit                     (designHBMProgram)

import           BenchUtil

-- ---------------------------------------------------------------------------
-- 共通設定
-- ---------------------------------------------------------------------------

iterGrid :: [Int]
iterGrid = [50, 100, 200, 400, 800, 1600]

warmupFixed :: Int
warmupFixed = 500

timingReps :: Int
timingReps = 5

-- 真値 (DGP)
m1True :: (Double, Double, Double)   -- (a, b, sigma)
m1True = (2.0, 1.5, 1.0)

m2True :: (Double, Double, Double, Double)  -- (beta0, beta1, tau_u, sigma)
m2True = (1.0, 0.8, 1.5, 1.0)

nM1 :: Int
nM1 = 100

nGroupsM2, perGroupM2 :: Int
nGroupsM2  = 8
perGroupM2 = 12     -- 計 96 観測

-- M3: (beta0, beta1, tau_u, tau_v, sigma)。 群構成は M2 と同じ 8×12。
m3True :: (Double, Double, Double, Double, Double)
m3True = (1.0, 0.8, 1.0, 0.5, 1.0)

-- M4: 多変量 X (p=10 + intercept)。 (intercept:betas, sigma)。
m4BetaTrue :: [Double]
m4BetaTrue = [1.0, 1.2, -0.8, 0.5, 0.3, -0.2, 0.7, -0.4, 0.6, -0.5, 0.1]

m4SigmaTrue :: Double
m4SigmaTrue = 1.0

nM4, pM4 :: Int
nM4 = 200
pM4 = 10

-- M5: y = a·exp(-b·x) + c。 (a, b, c, sigma)。
m5True :: (Double, Double, Double, Double)
m5True = (2.5, 1.2, 0.5, 0.3)

nM5 :: Int
nM5 = 100

-- M6: y_ij = a_g·exp(-b·x) (a_g ~ N(mu_a, tau_a))。 (mu_a, tau_a, b, sigma)。
m6True :: (Double, Double, Double, Double)
m6True = (2.0, 0.5, 1.0, 0.3)

-- M7: y ~ Poisson(exp(a + b·x))。 (a, b)。 x ~ N(0,1) で λ ∈ おおよそ
-- [0.1, 20] (Knuth サンプラ・logFactorial とも十分な範囲)。
m7True :: (Double, Double)
m7True = (0.5, 0.8)

nM7 :: Int
nM7 = 100

-- M8: y ~ Bernoulli(invLogit(a + b·x))。 (a, b)。
m8True :: (Double, Double)
m8True = (0.3, 1.2)

nM8 :: Int
nM8 = 100

-- M9: y ~ NegBin(exp(a + b·x), α)。 (a, b, alpha)。 x ~ N(0,1) で
-- μ ∈ おおよそ [0.15, 18] (M7 と同レンジ)。 α は過分散が見える 1.5
-- (整数回避は lgammaApprox 境界 FD 罠の慣例に合わせる・56.1 記録)。
m9True :: (Double, Double, Double)
m9True = (0.5, 0.8, 1.5)

nM9 :: Int
nM9 = 100

-- ---------------------------------------------------------------------------
-- 決定的データ生成
-- ---------------------------------------------------------------------------

-- | seed 固定の N(0,1) 列。
normals :: Int -> Int -> IO [Double]
normals seed k = do
  g <- MWC.initialize (V.singleton (fromIntegral seed))
  mapM (const (standard g)) [1 .. k]

-- | M1: x ~ N(0,2), y = a + b·x + N(0,σ)。 (x, y) を返し CSV も書く。
genM1 :: IO ([Double], [Double])
genM1 = do
  let (a, b, s) = m1True
  xz <- normals 11 nM1
  ez <- normals 12 nM1
  let xs = map (* 2.0) xz
      ys = zipWith (\x e -> a + b * x + s * e) xs ez
  writeCsv "bench/data/hbm_m1.csv" "x0,y"
    [ printf "%.6f,%.6f" x y | (x, y) <- zip xs ys ]
  return (xs, ys)

-- | M2: 8 群 × 12。 x ~ N(0,2), u_g ~ N(0,τ_u), y = β0+β1·x+u_g+N(0,σ)。
--   (xRows [[1,x]], gids, ys) を返し CSV (x0,group,y) も書く。
genM2 :: IO ([[Double]], [Int], [Double])
genM2 = do
  let (b0, b1, tauU, s) = m2True
      n = nGroupsM2 * perGroupM2
  xz <- normals 21 n
  ez <- normals 22 n
  uz <- normals 23 nGroupsM2
  let us   = map (* tauU) uz
      gids = [ i `div` perGroupM2 | i <- [0 .. n - 1] ]
      xs   = map (* 2.0) xz
      ys   = [ b0 + b1 * x + (us !! g) + s * e
             | (x, g, e) <- zip3 xs gids ez ]
      xRows = [ [1.0, x] | x <- xs ]
  writeCsv "bench/data/hbm_m2.csv" "x0,group,y"
    [ printf "%.6f,%d,%.6f" x g y | (x, g, y) <- zip3 xs gids ys ]
  return (xRows, gids, ys)

-- | M3: 8 群 × 12。 y = β0 + β1·x + u_g + v_g·x + N(0,σ)。
--   (xs, gids, ys) を返し CSV (x0,group,y) も書く。
genM3 :: IO ([Double], [Int], [Double])
genM3 = do
  let (b0, b1, tauU, tauV, s) = m3True
      n = nGroupsM2 * perGroupM2
  xz <- normals 31 n
  ez <- normals 32 n
  uz <- normals 33 nGroupsM2
  vz <- normals 34 nGroupsM2
  let us   = map (* tauU) uz
      vs   = map (* tauV) vz
      gids = [ i `div` perGroupM2 | i <- [0 .. n - 1] ]
      xs   = map (* 2.0) xz
      ys   = [ b0 + b1 * x + (us !! g) + (vs !! g) * x + s * e
             | (x, g, e) <- zip3 xs gids ez ]
  writeCsv "bench/data/hbm_m3.csv" "x0,group,y"
    [ printf "%.6f,%d,%.6f" x g y | (x, g, y) <- zip3 xs gids ys ]
  return (xs, gids, ys)

-- | M4: n=200, p=10。 y = β0 + Σ β_k·x_k + N(0,σ)。
--   (xRows (p 列・intercept 含まず), ys) を返し CSV (x0..x9,y) も書く。
genM4 :: IO ([[Double]], [Double])
genM4 = do
  xz <- normals 41 (nM4 * pM4)
  ez <- normals 42 nM4
  let xRows = [ take pM4 (drop (i * pM4) xz) | i <- [0 .. nM4 - 1] ]
      (b0 : bks) = m4BetaTrue
      ys = [ b0 + sum (zipWith (*) bks xr) + m4SigmaTrue * e
           | (xr, e) <- zip xRows ez ]
      hdr = concat [ "x" ++ show k ++ "," | k <- [0 .. pM4 - 1] ] ++ "y"
  writeCsv "bench/data/hbm_m4.csv" hdr
    [ concat [ printf "%.6f," x | x <- xr ] ++ printf "%.6f" y
    | (xr, y) <- zip xRows ys ]
  return (xRows, ys)

-- | M5: x = [0,3) 等間隔グリッド。 y = a·exp(-b·x) + c + N(0,σ)。
genM5 :: IO ([Double], [Double])
genM5 = do
  let (a, b, c, s) = m5True
  ez <- normals 51 nM5
  let xs = [ 3.0 * (fromIntegral i + 0.5) / fromIntegral nM5
           | i <- [0 .. nM5 - 1] ]
      ys = [ a * exp (negate b * x) + c + s * e | (x, e) <- zip xs ez ]
  writeCsv "bench/data/hbm_m5.csv" "x0,y"
    [ printf "%.6f,%.6f" x y | (x, y) <- zip xs ys ]
  return (xs, ys)

-- | M6: 8 群 × 12・x は群内 [0,3) グリッド。 y = a_g·exp(-b·x) + N(0,σ)。
genM6 :: IO ([Double], [Int], [Double])
genM6 = do
  let (muA, tauA, b, s) = m6True
      n = nGroupsM2 * perGroupM2
  ez <- normals 61 n
  az <- normals 62 nGroupsM2
  let as   = [ muA + tauA * z | z <- az ]
      gids = [ i `div` perGroupM2 | i <- [0 .. n - 1] ]
      xs   = [ 3.0 * (fromIntegral (i `mod` perGroupM2) + 0.5)
                   / fromIntegral perGroupM2
             | i <- [0 .. n - 1] ]
      ys   = [ (as !! g) * exp (negate b * x) + s * e
             | (x, g, e) <- zip3 xs gids ez ]
  writeCsv "bench/data/hbm_m6.csv" "x0,group,y"
    [ printf "%.6f,%d,%.6f" x g y | (x, g, y) <- zip3 xs gids ys ]
  return (xs, gids, ys)

-- | M7: x ~ N(0,1)。 y ~ Poisson(exp(a + b·x)) (sampleDist で決定的生成)。
genM7 :: IO ([Double], [Double])
genM7 = do
  let (a, b) = m7True
  xs <- normals 71 nM7
  g  <- MWC.initialize (V.singleton 72)
  ys <- mapM (\x -> sampleDist (Poisson (exp (a + b * x))) g) xs
  writeCsv "bench/data/hbm_m7.csv" "x0,y"
    [ printf "%.6f,%.0f" x y | (x, y) <- zip xs ys ]
  return (xs, ys)

-- | M8: x ~ N(0,1)。 y ~ Bernoulli(invLogit(a + b·x))。
genM8 :: IO ([Double], [Double])
genM8 = do
  let (a, b) = m8True
  xs <- normals 81 nM8
  g  <- MWC.initialize (V.singleton 82)
  ys <- mapM (\x -> sampleDist
                      (Bernoulli (1 / (1 + exp (negate (a + b * x))))) g) xs
  writeCsv "bench/data/hbm_m8.csv" "x0,y"
    [ printf "%.6f,%.0f" x y | (x, y) <- zip xs ys ]
  return (xs, ys)

-- | M9: x ~ N(0,1)。 y ~ NegBin(exp(a + b·x), α) (sampleDist で決定的生成)。
genM9 :: IO ([Double], [Double])
genM9 = do
  let (a, b, al) = m9True
  xs <- normals 91 nM9
  g  <- MWC.initialize (V.singleton 92)
  ys <- mapM (\x -> sampleDist
                      (NegativeBinomial (exp (a + b * x)) al) g) xs
  writeCsv "bench/data/hbm_m9.csv" "x0,y"
    [ printf "%.6f,%.0f" x y | (x, y) <- zip xs ys ]
  return (xs, ys)

writeCsv :: FilePath -> String -> [String] -> IO ()
writeCsv path hdr rows = withFile path WriteMode $ \h -> do
  hSetBuffering h LineBuffering
  hPutStrLn h hdr
  mapM_ (hPutStrLn h) rows

-- | Radon 生 CSV を読む (Python 側と同一ファイル)。 列 =
--   county(str), county_idx(int), floor(0/1), log_radon, log_uranium。
--   返り値 = (designX=[[1,floor,uranium]], county_idx, floor 列, log_radon)。
readRadon :: IO ([[Double]], [Int], [Double], [Double])
readRadon = do
  txt <- readFile "bench/data/radon.csv"
  let recs = map parseRow (drop 1 (lines txt))
      parseRow ln = case splitComma ln of
        (_c : ci : fl : lr : lu : _) ->
          (read ci :: Int, read fl :: Double, read lr :: Double, read lu :: Double)
        _ -> error ("readRadon: 列不足 " ++ ln)
      cidx    = [ c | (c, _, _, _) <- recs ]
      floors  = [ f | (_, f, _, _) <- recs ]
      ys      = [ y | (_, _, y, _) <- recs ]
      designX = [ [1.0, f, u] | (_, f, _, u) <- recs ]
  return (designX, cidx, floors, ys)

-- | 単純な comma split (Data.List.Split 非依存)。
splitComma :: String -> [String]
splitComma s = case break (== ',') s of
  (a, ',' : rest) -> a : splitComma rest
  (a, _)          -> [a]

-- ---------------------------------------------------------------------------
-- モデル定義
-- ---------------------------------------------------------------------------

-- | M1 pooled 単回帰。 Distribution はスカラ専用ゆえ観測は 1 点ずつ展開。
m1Model :: [Double] -> [Double] -> ModelP ()
m1Model xs ys = do
  a <- sample "a"     (Normal 0 10)
  b <- sample "b"     (Normal 0 10)
  s <- sample "sigma" (Exponential 1)
  forM_ (zip3 [0 :: Int ..] xs ys) $ \(i, x, y) ->
    observe (T.pack ("y_" ++ show i)) (Normal (a + b * realToFrac x) s) [y]

-- | M2 階層 random intercept。 既存 helper をそのまま使う
--   (latent: beta_0, beta_1, tau_u, u_0..u_{nG-1}, sigma)。
m2Model :: [[Double]] -> [Int] -> [Double] -> ModelP ()
m2Model xRows gids ys = glmmRandomIntercept GlmmGaussian xRows gids ys

-- | M3 階層 random intercept+slope (per-obs 手書き)。 u_g は係数 1 ゆえ
--   54.8 合成で REff gather、 v_g は係数 x ゆえ dense 列 + prior は
--   residual ad walk に残る (中間ケース)。
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

-- | M4 多変量 X pooled (per-obs 手書き)。 全 affine ゆえ 54.8 合成で
--   全 latent が dense β 列 + 定数 prior = 完全解析経路。
m4Model :: [[Double]] -> [Double] -> ModelP ()
m4Model xRows ys = do
  bs <- mapM (\k -> sample (T.pack ("beta_" ++ show k)) (Normal 0 5))
             [0 .. pM4]
  s  <- sample "sigma" (Exponential 1)
  let (b0 : bks) = bs
  forM_ (zip3 [0 :: Int ..] xRows ys) $ \(i, xr, y) ->
    observe (T.pack ("y_" ++ show i))
      (Normal (b0 + sum (zipWith (\b x -> b * realToFrac x) bks xr)) s) [y]

-- | M5 パラメタ非線形 (per-obs 手書き)。 μ = a·exp(-b·x) + c は非 affine ゆえ
--   54.8 合成不可 → walk + ad fallback (汎用経路の弱点を正直に測る)。
m5Model :: [Double] -> [Double] -> ModelP ()
m5Model xs ys = do
  a <- sample "a" (Normal 0 10)
  b <- sample "b" (HalfNormal 2)
  c <- sample "c" (Normal 0 10)
  s <- sample "sigma" (Exponential 1)
  forM_ (zip3 [0 :: Int ..] xs ys) $ \(i, x, y) ->
    observe (T.pack ("y_" ++ show i))
      (Normal (a * exp (negate b * realToFrac x) + c) s) [y]

-- | M6 階層 × 非線形 (per-obs 手書き)。 a_g·exp(-b·x) も非 affine → fallback。
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

-- | M7 Poisson 回帰 (per-obs 手書き・log link)。
--   ★旧コメント「非Gaussian観測ゆえ高速経路対象外」は古い情報 (Phase 89 で
--   訂正): `VGPois` 族対応が後に追加され、現在は vecIR (a) 経路に乗る
--   (`synthVecIR` = Just・`bench/posteriordb/glm-poisson` で実測確認)。
m7Model :: [Double] -> [Double] -> ModelP ()
m7Model xs ys = do
  a <- sample "a" (Normal 0 5)
  b <- sample "b" (Normal 0 5)
  forM_ (zip3 [0 :: Int ..] xs ys) $ \(i, x, y) ->
    observe (T.pack ("y_" ++ show i))
      (Poisson (exp (a + b * realToFrac x))) [y]

-- | M8 logistic 回帰 (per-obs 手書き・logit link)。 M7 と同じく fallback。
m8Model :: [Double] -> [Double] -> ModelP ()
m8Model xs ys = do
  a <- sample "a" (Normal 0 5)
  b <- sample "b" (Normal 0 5)
  forM_ (zip3 [0 :: Int ..] xs ys) $ \(i, x, y) ->
    observe (T.pack ("y_" ++ show i))
      (Bernoulli (1 / (1 + exp (negate (a + b * realToFrac x))))) [y]

-- | M9 NegBin 回帰 (per-obs 手書き・log link・α latent)。 Phase 56.5 で
--   IR 吸収 (lgammaΓ(k+α) は SLgammaO elementwise・Γ(k+1) は compile 時定数)。
m9Model :: [Double] -> [Double] -> ModelP ()
m9Model xs ys = do
  a  <- sample "a"     (Normal 0 5)
  b  <- sample "b"     (Normal 0 5)
  al <- sample "alpha" (Exponential 0.5)
  forM_ (zip3 [0 :: Int ..] xs ys) $ \(i, x, y) ->
    observe (T.pack ("y_" ++ show i))
      (NegativeBinomial (exp (a + b * realToFrac x)) al) [y]

-- | Radon 相関 varying intercept+slope (flagship・Phase 84)。 固定効果 =
--   (Intercept)+floor+uranium、 ランダム効果 = county 群の相関 (切片+floor 傾き)。
--   designHBMProgram の相関 RE branch (非中心化・LKJ・Phase 80.2b) に載る。
--   Python 側 bench_radon と同一 prior・同一データ (radon.csv)。
radonModel :: [[Double]] -> [Int] -> [Double] -> [Double] -> ModelP ()
radonModel designX cidx floorCol ys =
  designHBMProgram designX ["(Intercept)", "floor", "uranium"]
                   [(cidx, nCounties, [floorCol])] ys
  where nCounties = if null cidx then 0 else maximum cidx + 1

-- | Eight Schools (精度エッジ・Phase 84)。 古典的階層正規・funnel の定番。
--   非中心化パラメタ化: θ_j = μ + τ·θ̃_j (θ̃_j ~ N(0,1))・観測 SE σ_j は既知。
--   μ~N(0,5)・τ~HalfCauchy(5)。 Python 側 bench_eightschools と同一 prior・データ。
--   主役 = τ (funnel の首・サンプラ品質が出る)。
eightSchoolsModel :: ModelP ()
eightSchoolsModel = do
  let ys     = [28, 8, -3, 7, -1, 1, 18, 12] :: [Double]
      sigmas = [15, 10, 16, 11, 9, 11, 10, 18] :: [Double]
  mu  <- sample "mu"  (Normal 0 5)
  tau <- sample "tau" (HalfCauchy 5)
  tts <- mapM (\j -> sample (T.pack ("theta_t_" ++ show j)) (Normal 0 1)) [0 .. 7]
  forM_ (zip3 [0 :: Int ..] (zip ys sigmas) tts) $ \(i, (y, s), tt) ->
    observe (T.pack ("y_" ++ show i)) (Normal (mu + tau * tt) (realToFrac s)) [y]

-- ---------------------------------------------------------------------------
-- NUTS 実行 (warmup 固定・iter 可変)
-- ---------------------------------------------------------------------------

mkConfig :: Int -> NUTSConfig
mkConfig iters = defaultNUTSConfig
  { nutsIterations    = iters
  , nutsBurnIn        = warmupFixed
  , nutsStepSize      = 0.1
  , nutsMaxDepth      = 10
  , nutsAdaptStepSize = True
  , nutsTargetAccept  = 0.8
  , nutsAdaptMass     = True
  }

acceptRate :: Chain -> Double
acceptRate ch =
  fromIntegral (chainAccepted ch) / max 1 (fromIntegral (chainTotal ch))

probeChain :: [T.Text] -> Chain -> Double
probeChain names ch =
  sum [ maybe 0 id (posteriorMean p ch) | p <- names ]

-- | 1 (モデル, iter) について timingReps 回計測 (index-seed で CSE 回避)、
--   返り値の chain (seed 42) から ESS/posterior mean を取る。
runBench
  :: String              -- ^ モデル名タグ (M1_pooled / M2_ranint)
  -> ModelP ()           -- ^ モデル (init は別途・seed は gen 側)
  -> Map.Map T.Text Double  -- ^ init params
  -> [T.Text]               -- ^ probe 用全パラメタ名
  -> T.Text                 -- ^ 主役パラメタ (ESS 報告対象: slope)
  -> Int                    -- ^ iter
  -> IO BenchRow
runBench = runBenchReps timingReps

-- | 'runBench' の reps 可変版。 重いモデル (radon 等) は少ない reps で回す。
runBenchReps
  :: Int -> String -> ModelP () -> Map.Map T.Text Double
  -> [T.Text] -> T.Text -> Int -> IO BenchRow
runBenchReps reps tag mdl initP allNames keyParam iters = do
  let cfg = mkConfig iters
      run :: Int -> IO Chain
      run i = do
        g <- MWC.initialize (V.singleton (fromIntegral (42 + i)))
        nuts mdl cfg initP g
  (ms, ch) <- timeitIO reps (probeChain allNames) run
  let keyEss   = ess (chainVals keyParam ch)
      keyMean  = maybe 0 id (posteriorMean keyParam ch)
      acc      = acceptRate ch
      essPerSec = keyEss / max 1e-9 (ms / 1000.0)
      name     = tag ++ "_iter" ++ show iters
      -- Phase 85.3: per-draw tree depth の平均 (PyMC の tree_depth と直接比較)
      depths   = chainTreeDepths ch
      meanDepth = if null depths then 0
                  else fromIntegral (sum depths)
                       / fromIntegral (length depths) :: Double
      extra    = printf ("iter=%d warmup=%d key=%s ess=%.1f ess_per_sec=%.2f "
                         ++ "accept=%.3f tree_depth=%.2f time_ms=%.1f")
                   iters warmupFixed (T.unpack keyParam)
                   keyEss essPerSec acc meanDepth ms
  return (BenchRow "haskell" "hbm_scaling" name ms keyMean keyEss extra)

-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["m5-long"] -> do
      (x5, y5) <- genM5
      mainLong "M5_nonlin" (m5Model x5 y5)
        (Map.fromList [("a", 2.5), ("b", 1.2), ("c", 0.5), ("sigma", 0.3)])
        ["a", "b", "c", "sigma"] "m5"
    ["m7-long"] -> do
      (x7, y7) <- genM7
      mainLong "M7_pois" (m7Model x7 y7)
        (Map.fromList [("a", 0.5), ("b", 0.8)]) ["a", "b"] "m7"
    ["m8-long"] -> do
      (x8, y8) <- genM8
      mainLong "M8_logit" (m8Model x8 y8)
        (Map.fromList [("a", 0.3), ("b", 1.2)]) ["a", "b"] "m8"
    ["m9-long"] -> do
      (x9, y9) <- genM9
      mainLong "M9_negbin" (m9Model x9 y9)
        (Map.fromList [("a", 0.5), ("b", 0.8), ("alpha", 1.5)])
        ["a", "b", "alpha"] "m9"
    ["glm"]          -> mainGlm
    ["radon"]        -> mainRadon
    ["radon1600"]    -> mainRadon1600
    ["eightschools"] -> mainEight
    _                -> mainAll

-- | Eight Schools を通常 grid で掃く (引数 @eightschools@)。 小モデルゆえ軽い。
mainEight :: IO ()
mainEight = do
  let eightInit = Map.fromList [("mu", 4.0), ("tau", 3.0)]
      allNames  = ["mu", "tau"]
  rows <- mapM
    (runBench "eightschools" eightSchoolsModel eightInit allNames "tau")
    iterGrid
  writeRows "bench/results/haskell/hbm_scaling_eightschools.csv" rows
  putStrLn $ "wrote " ++ show (length rows)
          ++ " rows → bench/results/haskell/hbm_scaling_eightschools.csv"

-- | Radon (flagship) を通常 grid で掃く (引数 @radon@)。 別 CSV
--   @hbm_scaling_radon.csv@ へ (M 系 CSV を上書きしない)。 相関 RE の
--   正値制約 latent (sigma/tau/LKJ の Beta) だけ init を与え、 z/beta は 0 既定。
-- radon は 919 obs・相関 RE で 1 サンプルあたり deep tree (~max depth 10) ゆえ
-- 重い。 reps=2・短め grid で回す (Python 側 radonGrid と一致させる)。
radonGrid :: [Int]
radonGrid = [50, 100, 200, 400]

radonReps :: Int
radonReps = 2

mainRadon :: IO ()
mainRadon = do
  (designX, cidx, floorCol, ys) <- readRadon
  let radonInit = Map.fromList
        [ ("(Intercept)", 1.3), ("floor", -0.6), ("uranium", 0.7)
        , ("sigma", 0.7)
        , ("tau_g0_0", 0.5), ("tau_g0_1", 0.3), ("Lcorr_g0_u1_0", 0.5) ]
      allNames = ["(Intercept)", "floor", "uranium", "sigma"]
  rows <- mapM
    (\it -> do
        r <- runBenchReps radonReps "radon"
               (radonModel designX cidx floorCol ys)
               radonInit allNames "floor" it
        putStrLn $ "  radon iter=" ++ show it ++ ": "
                 ++ show (round (brTimeMs r) :: Int) ++ " ms"
        pure r)
    radonGrid
  writeRows "bench/results/haskell/hbm_scaling_radon.csv" rows
  putStrLn $ "wrote " ++ show (length rows)
          ++ " rows → bench/results/haskell/hbm_scaling_radon.csv"

-- | Phase 88 追補: radon を実運用規模 iter=1600 単点で計測する (引数
--   @radon1600@)。 iter400 は短めの grid で「有利なベンチ」になりうる
--   (Phase 87 で iter400→1600 だけで hanalyze 対 PyMC-C 比が 0.80×→
--   0.49-0.68× に動いた前例あり)。 別 CSV @hbm_scaling_radon1600.csv@ へ。
mainRadon1600 :: IO ()
mainRadon1600 = do
  (designX, cidx, floorCol, ys) <- readRadon
  let radonInit = Map.fromList
        [ ("(Intercept)", 1.3), ("floor", -0.6), ("uranium", 0.7)
        , ("sigma", 0.7)
        , ("tau_g0_0", 0.5), ("tau_g0_1", 0.3), ("Lcorr_g0_u1_0", 0.5) ]
      allNames = ["(Intercept)", "floor", "uranium", "sigma"]
  r <- runBenchReps radonReps "radon"
         (radonModel designX cidx floorCol ys)
         radonInit allNames "floor" 1600
  putStrLn $ "  radon iter=1600: " ++ show (round (brTimeMs r) :: Int) ++ " ms"
  writeRows "bench/results/haskell/hbm_scaling_radon1600.csv" [r]
  putStrLn "wrote 1 rows → bench/results/haskell/hbm_scaling_radon1600.csv"

-- | 1 モデルだけを延長 grid で掃く (引数 @m5-long@/@m7-long@/@m8-long@)。
--
-- 通常 grid (50-1600) では PyMC 側 total が固定費 (compile+tune ~2s) に支配され
-- per-draw 線形フィットが R² ~0.13 と不定のままだった (M5・54.11)。 iter を
-- 25600 まで延ばし draw 部分を固定費より大きくして傾きを確定する (Python 側 =
-- @bench_hbm_scaling.py <m>-long@・同 grid)。 結果は別 CSV
-- (@hbm_scaling_<m>_long.csv@) へ (通常 bench の CSV は上書きしない)。
iterGridLong :: [Int]
iterGridLong = [400, 800, 1600, 3200, 6400, 12800, 25600]

mainLong
  :: String -> ModelP () -> Map.Map T.Text Double -> [T.Text] -> String -> IO ()
mainLong tag mdl initP names short = do
  rows <- mapM (runBench tag mdl initP names "b") iterGridLong
  let out = "bench/results/haskell/hbm_scaling_" ++ short ++ "_long.csv"
  writeRows out rows
  putStrLn $ "wrote " ++ show (length rows) ++ " rows → " ++ out

-- | M7-M9 (GLM 系) だけ通常 grid で掃く (引数 @glm@・Phase 55.1 baseline 用、
--   M9 は Phase 56.6 追加)。 M1-M6 の既存 CSV を上書きしないよう別 CSV へ。
mainGlm :: IO ()
mainGlm = do
  (x7, y7) <- genM7
  (x8, y8) <- genM8
  (x9, y9) <- genM9
  m7Rows <- mapM
    (runBench "M7_pois" (m7Model x7 y7)
       (Map.fromList [("a", 0.5), ("b", 0.8)]) ["a", "b"] "b")
    iterGrid
  m8Rows <- mapM
    (runBench "M8_logit" (m8Model x8 y8)
       (Map.fromList [("a", 0.3), ("b", 1.2)]) ["a", "b"] "b")
    iterGrid
  m9Rows <- mapM
    (runBench "M9_negbin" (m9Model x9 y9)
       (Map.fromList [("a", 0.5), ("b", 0.8), ("alpha", 1.5)])
       ["a", "b", "alpha"] "b")
    iterGrid
  let rows = m7Rows ++ m8Rows ++ m9Rows
  writeRows "bench/results/haskell/hbm_scaling_glm.csv" rows
  putStrLn $ "wrote " ++ show (length rows)
          ++ " rows → bench/results/haskell/hbm_scaling_glm.csv"

mainAll :: IO ()
mainAll = do
  (x1, y1)          <- genM1
  (xRows, gids, y2) <- genM2
  (x3, g3, y3)      <- genM3
  (xR4, y4)         <- genM4
  (x5, y5)          <- genM5
  (x6, g6, y6)      <- genM6
  (x7, y7)          <- genM7
  (x8, y8)          <- genM8

  let groupNames pre = [ T.pack (pre ++ show j) | j <- [0 .. nGroupsM2 - 1] ]
      m1Init = Map.fromList [("a", 2.0), ("b", 1.5), ("sigma", 1.0)]
      m2Init = Map.fromList $
        [ ("beta_0", 1.0), ("beta_1", 0.8), ("tau_u", 1.5), ("sigma", 1.0) ]
        ++ [ (u, 0.0) | u <- groupNames "u_" ]
      m3Init = Map.fromList $
        [ ("beta_0", 1.0), ("beta_1", 0.8), ("tau_u", 1.0), ("tau_v", 0.5)
        , ("sigma", 1.0) ]
        ++ [ (u, 0.0) | u <- groupNames "u_" ++ groupNames "v_" ]
      m4Init = Map.fromList $
        [ (T.pack ("beta_" ++ show k), 0.0) | k <- [0 .. pM4] ]
        ++ [("sigma", 1.0)]
      m5Init = Map.fromList
        [("a", 2.5), ("b", 1.2), ("c", 0.5), ("sigma", 0.3)]
      m6Init = Map.fromList $
        [ ("mu_a", 2.0), ("tau_a", 0.5), ("b", 1.0), ("sigma", 0.3) ]
        ++ [ (a, 2.0) | a <- groupNames "a_" ]
      m1AllNames = ["a", "b", "sigma"]
      m2AllNames = ["beta_0", "beta_1", "tau_u", "sigma"] ++ groupNames "u_"
      m3AllNames = ["beta_0", "beta_1", "tau_u", "tau_v", "sigma"]
                   ++ groupNames "u_" ++ groupNames "v_"
      m4AllNames = [ T.pack ("beta_" ++ show k) | k <- [0 .. pM4] ] ++ ["sigma"]
      m5AllNames = ["a", "b", "c", "sigma"]
      m6AllNames = ["mu_a", "tau_a", "b", "sigma"] ++ groupNames "a_"

  m1Rows <- mapM
    (runBench "M1_pooled" (m1Model x1 y1) m1Init m1AllNames "b")
    iterGrid
  m2Rows <- mapM
    (runBench "M2_ranint" (m2Model xRows gids y2) m2Init m2AllNames "beta_1")
    iterGrid
  m3Rows <- mapM
    (runBench "M3_ranslope" (m3Model x3 g3 y3) m3Init m3AllNames "beta_1")
    iterGrid
  m4Rows <- mapM
    (runBench "M4_multix" (m4Model xR4 y4) m4Init m4AllNames "beta_1")
    iterGrid
  m5Rows <- mapM
    (runBench "M5_nonlin" (m5Model x5 y5) m5Init m5AllNames "b")
    iterGrid
  m6Rows <- mapM
    (runBench "M6_hier_nonlin" (m6Model x6 g6 y6) m6Init m6AllNames "b")
    iterGrid
  m7Rows <- mapM
    (runBench "M7_pois" (m7Model x7 y7)
       (Map.fromList [("a", 0.5), ("b", 0.8)]) ["a", "b"] "b")
    iterGrid
  m8Rows <- mapM
    (runBench "M8_logit" (m8Model x8 y8)
       (Map.fromList [("a", 0.3), ("b", 1.2)]) ["a", "b"] "b")
    iterGrid

  let rows = m1Rows ++ m2Rows ++ m3Rows ++ m4Rows ++ m5Rows ++ m6Rows
          ++ m7Rows ++ m8Rows
  writeRows "bench/results/haskell/hbm_scaling.csv" rows
  putStrLn $ "wrote " ++ show (length rows)
          ++ " rows → bench/results/haskell/hbm_scaling.csv"
