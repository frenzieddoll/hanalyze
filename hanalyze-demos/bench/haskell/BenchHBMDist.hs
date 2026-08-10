{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-full-laziness -fno-cse #-}
-- | Phase 56.6 bench: 観測分布ごとの per-call 勾配 A/B (bench-hbm-het の一般化)。
--
-- 56.3-56.5 で IR 吸収された各分布の canonical 回帰形 (n=100・test/Spec.hs の
-- synthVecIR test と同形) について、 同一 unconstrained 全勾配を
--
--   (a) HBM.gradADU      — 実経路 (IR 吸収・NUTS が払う値)
--   (b) RevD.grad (walk) — 旧 fallback 相当 (モデル全体を ad で毎回 walk)
--
-- で計測し 1 表にする。 各モデルは計測前に synthVecIR = Just (吸収) と
-- 相対誤差 (IR vs walk) を確認してから走らせる。
--
-- ⚠ ここで出るのは **per-call の改善倍率のみ** (per-draw への波及は M 系
-- bench でしか測れない・M9_negbin 以外は未計測)。 「PyMC 同等」 等の比較は
-- この表からは言えない。
--
-- 引数: 無し=全 family / family 名の列挙 (例: @negbin gamma@) = その family のみ。
-- 結果: bench/results/haskell/hbm_dist_grad_ab.csv
module Main where

import           Control.Monad                  (forM, unless)
import qualified Data.Map.Strict                as Map
import qualified Data.Text                      as T
import           System.Environment             (getArgs)
import           Text.Printf                    (printf)

import qualified Numeric.AD.Mode.Reverse.Double as RevD

import           Hanalyze.Model.HBM             (Distribution (..), Model,
                                                 ModelP,
                                                 sample, observe, gradADU,
                                                 sampleNames, getTransforms,
                                                 logJoint, invTransformF,
                                                 logJacF, synthVecIR)

import           BenchUtil                      (BenchRow (..), timeitIO,
                                                 writeRows)

-- ===========================================================================
-- 決定的データ (乱数不要の固定系列・n=100)
-- ===========================================================================

nObs :: Int
nObs = 100

-- 説明変数: [-1, 1) 等間隔グリッド。
xsD :: [Double]
xsD = [ 2 * fromIntegral i / fromIntegral nObs - 1 | i <- [0 .. nObs - 1] ]

-- 決定的 pseudo-noise (sin 系列・[-1,1])。
wiggle :: [Double]
wiggle = [ sin (fromIntegral (7 * i :: Int)) | i <- [0 .. nObs - 1] ]

-- 実数値 (非有界): 位置-尺度系 (Gauss/StudentT/Cauchy/Logistic/Gumbel/LogN の log 前)。
ysReal :: [Double]
ysReal = [ 1.2 + 0.5 * x + 0.4 * w | (x, w) <- zip xsD wiggle ]

-- 正値: Expo/Weibull/LogNormal/Gamma。
ysPos :: [Double]
ysPos = [ exp (0.3 * x + 0.5 * w) | (x, w) <- zip xsD wiggle ]

-- (0,1): Beta。
ysUnit :: [Double]
ysUnit = [ 0.5 + 0.35 * w | w <- wiggle ]

-- 非負整数 count: Poisson/NegBin。
ysCount :: [Double]
ysCount = [ fromIntegral (max 0 (round (2 + 1.5 * x + 1.2 * w) :: Int))
          | (x, w) <- zip xsD wiggle ]

-- 0/1: Bernoulli。
ysBin01 :: [Double]
ysBin01 = [ if w > 0 then 1 else 0 | w <- wiggle ]

-- 0..10: Binomial(10)。
ysBinom :: [Double]
ysBinom = [ fromIntegral (min 10 (max 0 (round (5 + 3 * x + 2 * w) :: Int)))
          | (x, w) <- zip xsD wiggle ]

-- 1..: Geometric (試行回数パラメタ化)。
ysGeom :: [Double]
ysGeom = [ fromIntegral (max 1 (round (2 + 1.5 * w) :: Int)) | w <- wiggle ]

-- ===========================================================================
-- canonical 回帰形 (test/Spec.hs の synthVecIR test と同形・n=100 化)
-- ===========================================================================

-- | per-obs 手書きの共通骨格: 観測分布だけ差し替える (x は AD 型に持ち上げて渡す)。
perObs :: (Floating a, Ord a)
       => (a -> Distribution a) -> [Double] -> Model a ()
perObs mk ys =
  mapM_ (\(i, (x, y)) ->
           observe (T.pack ("y_" ++ show (i :: Int)))
             (mk (realToFrac x)) [y])
        (zip [0 ..] (zip xsD ys))

invLogit :: Floating a => a -> a
invLogit e = 1 / (1 + exp (negate e))

-- | family 名 + モデル + unconstrained 初期点 (sampleNames 順)。
--   ModelP は rank-2 ゆえタプル格納不可 → data でラップ (Phase 51 の既知罠)。
data Fam = Fam String (ModelP ()) [Double]

mGauss, mPois, mBern, mStudentT, mCauchy, mLogistic, mGumbel,
  mExpo, mWeibull, mLogNormal, mGamma, mBeta, mBinomial, mGeometric,
  mNegBin :: ModelP ()

mGauss = do
  a <- sample "a" (Normal 0 10)
  b <- sample "b" (Normal 0 10)
  s <- sample "sigma" (Exponential 1)
  perObs (\x -> Normal (a + b * x) s) ysReal

mPois = do
  a <- sample "a" (Normal 0 5)
  b <- sample "b" (Normal 0 5)
  perObs (\x -> Poisson (exp (a + b * x))) ysCount

mBern = do
  a <- sample "a" (Normal 0 5)
  b <- sample "b" (Normal 0 5)
  perObs (\x -> Bernoulli (invLogit (a + b * x))) ysBin01

-- 56.3 (ν=SC 定数)
mStudentT = do
  a <- sample "a" (Normal 0 10)
  b <- sample "b" (Normal 0 10)
  s <- sample "sigma" (Exponential 1)
  perObs (\x -> StudentT 4 (a + b * x) s) ysReal

mCauchy = do
  a <- sample "a" (Normal 0 10)
  b <- sample "b" (Normal 0 10)
  g <- sample "gamma" (Exponential 1)
  perObs (\x -> Cauchy (a + b * x) g) ysReal

mLogistic = do
  a <- sample "a" (Normal 0 10)
  b <- sample "b" (Normal 0 10)
  s <- sample "s" (Exponential 1)
  perObs (\x -> Logistic (a + b * x) s) ysReal

mGumbel = do
  a <- sample "a" (Normal 0 10)
  b <- sample "b" (Normal 0 10)
  be <- sample "beta" (Exponential 1)
  perObs (\x -> Gumbel (a + b * x) be) ysReal

-- 56.4 (rate=exp(η))
mExpo = do
  a <- sample "a" (Normal 0 5)
  b <- sample "b" (Normal 0 5)
  perObs (\x -> Exponential (exp (a + b * x))) ysPos

-- 56.4 (k latent, λ=exp(η))
mWeibull = do
  k <- sample "k" (Exponential 1)
  a <- sample "a" (Normal 0 5)
  b <- sample "b" (Normal 0 5)
  perObs (\x -> Weibull k (exp (a + b * x))) ysPos

-- 56.4 (Gaussian ノード再利用)
mLogNormal = do
  a <- sample "a" (Normal 0 5)
  b <- sample "b" (Normal 0 5)
  s <- sample "sigma" (Exponential 1)
  perObs (\x -> LogNormal (a + b * x) s) ysPos

-- 56.4 (α latent, rate=exp(η))
mGamma = do
  al <- sample "alpha" (Exponential 1)
  a  <- sample "a" (Normal 0 5)
  b  <- sample "b" (Normal 0 5)
  perObs (\x -> Gamma al (exp (a + b * x))) ysPos

-- 56.4 (α=μφ, β=(1-μ)φ・φ は整数回避 = lgamma FD 罠の慣例)
mBeta = do
  a  <- sample "a" (Normal 0 5)
  b  <- sample "b" (Normal 0 5)
  ph <- sample "phi" (Exponential 0.5)
  perObs (\x -> let mu = invLogit (a + b * x)
                  in Beta (mu * ph) ((1 - mu) * ph)) ysUnit

-- 56.5 (n=10 定数)
mBinomial = do
  a <- sample "a" (Normal 0 5)
  b <- sample "b" (Normal 0 5)
  perObs (\x -> Binomial 10 (invLogit (a + b * x))) ysBinom

-- 56.5 (p=invLogit(η))
mGeometric = do
  a <- sample "a" (Normal 0 5)
  b <- sample "b" (Normal 0 5)
  perObs (\x -> Geometric (invLogit (a + b * x))) ysGeom

-- 56.5 (μ=exp(η), α latent)
mNegBin = do
  a  <- sample "a" (Normal 0 5)
  b  <- sample "b" (Normal 0 5)
  al <- sample "alpha" (Exponential 0.5)
  perObs (\x -> NegativeBinomial (exp (a + b * x)) al) ysCount

families :: [Fam]
families =
  [ Fam "gauss"     mGauss     [1.0, 0.4, log 0.8]
  , Fam "pois"      mPois      [0.3, 0.4]
  , Fam "bern"      mBern      [0.2, 0.5]
  , Fam "studentt"  mStudentT  [1.2, 0.4, log 0.6]
  , Fam "cauchy"    mCauchy    [1.2, 0.4, log 0.5]
  , Fam "logistic"  mLogistic  [1.2, 0.4, log 0.5]
  , Fam "gumbel"    mGumbel    [1.2, 0.4, log 0.6]
  , Fam "expo"      mExpo      [0.2, -0.3]
  , Fam "weibull"   mWeibull   [log 1.3, 0.2, -0.3]
  , Fam "lognormal" mLogNormal [0.1, 0.3, log 0.7]
  , Fam "gamma"     mGamma     [log 1.6, 0.2, -0.3]
  , Fam "beta"      mBeta      [0.3, -0.2, log 3.3]
  , Fam "binomial"  mBinomial  [0.2, 0.6]
  , Fam "geometric" mGeometric [-0.2, 0.5]
  , Fam "negbin"    mNegBin    [0.4, 0.3, log 1.7]
  ]

-- ===========================================================================
-- 計測 (bench-hbm-het と同一手順)
-- ===========================================================================

-- | 1 family の per-call A/B。 計測前に (1) IR 吸収を確認 (吸収されなければ
--   FALLBACK 行として速度比 1 を返す) (2) 相対誤差を検証する。
runFamily :: Fam -> IO BenchRow
runFamily (Fam tag mdl uvs) = do
  let names = sampleNames mdl
      tmap  = getTransforms mdl
      trans = [ tmap Map.! nm | nm <- names ]
      absorbed = case synthVecIR mdl of
                   Just (_, fams, _) -> null fams   -- 残余 family 無し = 全吸収
                   Nothing           -> False
      gIR = gradADU mdl names trans
      gAD uv = RevD.grad
                 (\uv' -> logJoint mdl
                            (Map.fromList
                               (zip names (zipWith invTransformF trans uv')))
                          + sum (zipWith logJacF trans uv'))
                 uv
      relErr = maximum [ abs (x - y) / (1 + abs y)
                       | (x, y) <- zip (gIR uvs) (gAD uvs) ]
  unless (relErr < 1e-8) $
    fail (tag ++ ": relErr IR vs ad-full = " ++ show relErr ++ " (>1e-8)")
  -- 1 計測 = batch 回の勾配呼出 (µs 級なので)。 入力を毎回微小摂動して
  -- CSE/共有を防ぐ (1e-12 は数値に実質影響しない)。
  let batch = 1000 :: Int
      runBatch g i = pure $! sum
        [ sum (g (map (+ (1e-12 * fromIntegral (i * batch + j))) uvs))
        | j <- [1 .. batch] ]
  (msIR, _) <- timeitIO 7 id (runBatch gIR)
  (msAD, _) <- timeitIO 7 id (runBatch gAD)
  let pcIR = msIR / fromIntegral batch
      pcAD = msAD / fromIntegral batch
      sp   = pcAD / pcIR
  printf "%-10s %s  IR %.5f ms/call  walk %.5f ms/call  x%.1f  (relErr %.1e)\n"
    tag (if absorbed then "[IR]      " else "[FALLBACK]" :: String)
    pcIR pcAD sp relErr
  return (BenchRow "haskell" "hbm_dist" tag pcIR pcAD sp
            (printf ("absorbed=%s relErr=%.2e n=%d batch=%d "
                     ++ "per-call only (per-draw 波及は未計測)")
               (show absorbed) relErr nObs batch))

main :: IO ()
main = do
  args <- getArgs
  let sel = if null args then families
            else [ f | f@(Fam tag _ _) <- families, tag `elem` args ]
  putStrLn "family     path        per-call gradient A/B (IR 吸収 vs 全体 ad walk)"
  rows <- forM sel runFamily
  writeRows "bench/results/haskell/hbm_dist_grad_ab.csv" rows
  putStrLn $ "wrote " ++ show (length rows)
          ++ " rows → bench/results/haskell/hbm_dist_grad_ab.csv"
