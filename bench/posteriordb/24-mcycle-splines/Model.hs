{-# LANGUAGE OverloadedStrings #-}
-- | mcycle_splines-accel_splines (posteriordb) — hanalyze (ModelP) 実装。
--
-- Phase 89: posteriordb 横断ベンチマーク。オートバイ衝突試験の加速度
-- スプライン回帰 (brms生成モデル・N=133・平均/分散とも39自由度の
-- thin-plate spline (bs=1線形項+38基底関数) で非線形推定する不均一分散
-- モデル)。★新ファミリ: スプライン基底回帰 (これまでのGPカーネル(07)とは
-- 異なり、基底関数行列はposteriordbデータに事前計算済みで提供される)。
--
-- Stan 原典 (posteriordb `models/stan/accel_splines.stan`・brms生成):
--   mu[n]    = Intercept       + Xs[n]·bs       + Zs_1_1[n]·s_1_1
--   sigma[n] = exp(Intercept_sigma + Xs_sigma[n]·bs_sigma + Zs_sigma_1_1[n]·s_sigma_1_1)
--   s_1_1 = sds_1_1 * zs_1_1;  s_sigma_1_1 = sds_sigma_1_1 * zs_sigma_1_1  (non-centered)
--   Intercept ~ StudentT(3,-13,36);  Intercept_sigma ~ StudentT(3,0,10)
--   zs_1_1/zs_sigma_1_1 ~ Normal(0,1) (各38成分)
--   sds_1_1/sds_sigma_1_1 ~ half-StudentT(3,0,36) (下側切断)
--   bs/bs_sigma: Stan原典に明示的prior無し (暗黙のflat prior)
--   Y ~ Normal(mu, sigma)
--
-- hanalyze に half-StudentT 分布は無いため、`HalfCauchy` を安全な
-- unconstrained初期値を持つ「代理distribution」としてsampleし、
-- `potential` で真の half-StudentT(3,0,36) 密度へ厳密に補正する
-- (14-hmm-exampleのmu2順序制約と同じ「代理+potential補正で厳密に
-- 等価な分布を作る」パターン)。bs/bs_sigma の暗黙flat priorは
-- 01-glm-poisson/10-rats と同じ流儀で Normal(0,1000) に置換。
--
-- Xs/Zs_1_1/Xs_sigma/Zs_sigma_1_1 (基底関数計画行列) は微分対象では
-- ないデータなので closure で直接渡す (20-bonesのgamma/deltaと同じ
-- 流儀)。
--
-- reference_posterior_name = null (posteriordb に公式 reference posterior 無し)。
--
-- ビルド: cabal build --project-file=cabal.project.plot posteriordb-mcycle-splines
module Main (main) where

import Data.Aeson (FromJSON (..), withObject, (.:), eitherDecodeFileStrict)
import qualified Data.Text as T
import qualified Data.Vector as V
import Text.Printf (printf)

import Hanalyze.Model.HBM (ModelP, Distribution (..), sample, observe,
                                    potential, logDensity,
                                    plateI, plateForM_, (.#))
import Hanalyze.Model.HBM.IR (synthVecIR)
import Hanalyze.Plot (HBMConfig (..), defaultHBM, hbm, (|->),
                              dashboardOf, hbmChainsR)
import Graphics.Hgg.Spec (ColData (..))
import Graphics.Hgg.Frame (BoundPlot, (|>>))
import Graphics.Hgg.Backend.Rasterific (savePNGBound)

import Common (summarize, printSummary, timeSamplingMs)

-- | posteriordb の @mcycle_splines.json@ 形状 (brms生成)。
data SplineData = SplineData
  { spY            :: [Double]
  , spXs           :: [[Double]]  -- N x Ks (Ks=1)
  , spZs1          :: [[Double]]  -- N x knots_1 (=38)
  , spXsSigma      :: [[Double]]  -- N x Ks_sigma (=1)
  , spZsSigma1     :: [[Double]]  -- N x knots_sigma_1 (=38)
  , spKnots1       :: Int
  , spKnotsSigma1  :: Int
  , spKs           :: Int
  , spKsSigma      :: Int
  }

instance FromJSON SplineData where
  parseJSON = withObject "SplineData" $ \v ->
    SplineData <$> v .: "Y" <*> v .: "Xs" <*> v .: "Zs_1_1"
               <*> v .: "Xs_sigma" <*> v .: "Zs_sigma_1_1"
               <*> v .: "knots_1" <*> v .: "knots_sigma_1"
               <*> v .: "Ks" <*> v .: "Ks_sigma"

noDf :: [(T.Text, ColData)]
noDf = []

dataPath :: FilePath
dataPath = "bench/posteriordb/24-mcycle-splines/data/mcycle_splines.json"

figuresDir :: FilePath
figuresDir = "bench/posteriordb/24-mcycle-splines/figures"

readData :: IO SplineData
readData = either fail pure =<< eitherDecodeFileStrict dataPath

-- | データ行 (定数) と latent ベクトルの内積 (realToFrac 経由)。
dotConst :: Floating a => [a] -> [Double] -> a
dotConst coefs row = sum (zipWith (\c x -> c * realToFrac x) coefs row)

-- | half-StudentT(nu,0,sigma) の厳密な log density (下側切断・2倍)。
halfStudentTLogDensity :: (Floating a, Ord a) => a -> a -> a -> a -> a
halfStudentTLogDensity nu mu sigma x = log 2 + logDensity (StudentT nu mu sigma) x

-- | thin-plate spline (mean+heteroscedastic sigma) 回帰 (Stan 原典と
-- 同一構造)。 計画行列は微分対象ではないデータなので closure で直接渡す。
splineModel :: Int -> Int -> Int -> Int
            -> [[Double]] -> [[Double]] -> [[Double]] -> [[Double]] -> [Double]
            -> ModelP ()
splineModel ks ksSigma knots1 knotsSigma1 xsRows zs1Rows xsSigmaRows zsSigma1Rows ys = do
  intercept      <- sample "Intercept"       (StudentT 3 (-13) 36)
  interceptSigma <- sample "Intercept_sigma" (StudentT 3 0 10)
  bs      <- plateI "bs"      ks      $ \i -> sample ("bs"      .# i) (Normal 0 1000)
  bsSigma <- plateI "bsSigma" ksSigma $ \i -> sample ("bsSigma" .# i) (Normal 0 1000)

  sds1Raw <- sample "sds_1_1_raw" (HalfCauchy 1)
  potential "sds_1_1_correction"
    (halfStudentTLogDensity 3 0 36 sds1Raw - logDensity (HalfCauchy 1) sds1Raw)
  sdsSigma1Raw <- sample "sds_sigma_1_1_raw" (HalfCauchy 1)
  potential "sds_sigma_1_1_correction"
    (halfStudentTLogDensity 3 0 36 sdsSigma1Raw - logDensity (HalfCauchy 1) sdsSigma1Raw)

  zs1      <- plateI "zs1"      knots1      $ \k -> sample ("zs1"      .# k) (Normal 0 1)
  zsSigma1 <- plateI "zsSigma1" knotsSigma1 $ \k -> sample ("zsSigma1" .# k) (Normal 0 1)
  let s1      = map (* sds1Raw) zs1
      sSigma1 = map (* sdsSigma1Raw) zsSigma1

  plateForM_ "obs" (zip5' xsRows zs1Rows xsSigmaRows zsSigma1Rows ys)
    $ \(xsRow, zs1Row, xsSigmaRow, zsSigma1Row, yVal) ->
        let mu       = intercept + dotConst bs xsRow + dotConst s1 zs1Row
            sigmaLin = interceptSigma + dotConst bsSigma xsSigmaRow
                     + dotConst sSigma1 zsSigma1Row
            sigma    = exp sigmaLin
        in observe "Y" (Normal mu sigma) [yVal]
  where
    zip5' (a : as) (b : bs') (c : cs) (d : ds) (e : es) =
      (a, b, c, d, e) : zip5' as bs' cs ds es
    zip5' _ _ _ _ _ = []

main :: IO ()
main = do
  d <- readData
  let df = [ ("Y", NumData (V.fromList (spY d))) ] :: [(T.Text, ColData)]
      cfg = defaultHBM { hbmChains = 4, hbmSamples = 1000
                        , hbmWarmup = 1000, hbmSeed = Just 1 }
      model :: ModelP ()
      model = splineModel (spKs d) (spKsSigma d) (spKnots1 d) (spKnotsSigma1 d)
                           (spXs d) (spZs1 d) (spXsSigma d) (spZsSigma1 d) (spY d)
      m = df |-> hbm cfg model

  putStrLn $ "synthVecIR = " ++
    (case synthVecIR model of
       Just _  -> "Just (vecIR高速経路)"
       Nothing -> "Nothing (legacy walk+ad)")

  (_, samplingMs) <- timeSamplingMs (hbmChainsR m)
  printf "sampling wall = %.1f ms (draws only, no dashboard/startup)\n" samplingMs

  -- zs1/zsSigma1 (各38latent) を含むため dashboardFullOf でなく
  -- dashboardOf (健全性2x2パネルのみ・05-mh/10-ratsと同じ判断)。
  savePNGBound (figuresDir ++ "/hs_dashboard_full.png") $
    (noDf |>> dashboardOf m "Y" :: BoundPlot)

  printSummary $ summarize
    ["Intercept", "Intercept_sigma", "sds_1_1_raw", "sds_sigma_1_1_raw"]
    (hbmChainsR m)
