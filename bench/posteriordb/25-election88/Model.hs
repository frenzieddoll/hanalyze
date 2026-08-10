{-# LANGUAGE OverloadedStrings #-}
-- | election88-election88_full (posteriordb) — hanalyze (ModelP) 実装。
--
-- Phase 89: posteriordb 横断ベンチマーク。1988年米大統領選 世論調査の
-- 多水準ロジスティック回帰 (Gelman & Hill 2006・N=11566・これまでで
-- 最大のN)。★新ファミリ: **5本の独立な階層** (age/edu/age×edu交互作用/
-- state/regionそれぞれ別々のgroup-level intercept)。21-radonの単一階層
-- や10-ratsの「同一グループへの二重階層」とも異なり、5つの**互いに
-- 独立なグループ添字**上の階層という構造。
--
-- Stan 原典 (posteriordb `models/stan/election88_full.stan`):
--   y_hat[i] = beta[1] + beta[2]*black[i] + beta[3]*female[i]
--            + beta[5]*female[i]*black[i] + beta[4]*v_prev_full[i]
--            + a[age[i]] + b[edu[i]] + c[age_edu[i]] + d[state[i]] + e[region_full[i]]
--   a~normal(0,sigma_a); b~normal(0,sigma_b); c~normal(0,sigma_c);
--   d~normal(0,sigma_d); e~normal(0,sigma_e); beta~normal(0,100);
--   sigma_a..e ~ implicit Uniform(0,100) (real<lower=0,upper=100>);
--   y ~ bernoulli_logit(y_hat)
--
-- ★既知の罠を先回りして回避: `sigma_a..e ~ Uniform(0,100)`は
-- 01-glm-poisson/10-rats/15-dugongsで確立済みの「Uniform(0,X)をSD
-- パラメータに使うとHMCが初手から凍結する罠」に該当するため、
-- `HalfCauchy(25)`に置換 (10-ratsと同じ代替パターン)。
--
-- hanalyze の `Bernoulli` は確率パラメータ直接指定 (logit link 無し) の
-- ため、02-dogs/19-surgicalと同じく `p = invlogit(y_hat)` を手計算する。
-- 添字列 (age/edu/age_edu/state/region_full) は微分対象ではない構造的
-- 定数なので closure で直接渡し、群ごとの latent は `Data.Vector`
-- 経由でO(1)索引する (N=11566と大規模なため必須・17-nes/21-radonと
-- 同じ流儀)。
--
-- reference_posterior_name = null (posteriordb に公式 reference posterior 無し)。
--
-- ビルド: cabal build --project-file=cabal.project.plot posteriordb-election88
module Main (main) where

import Data.Aeson (FromJSON (..), withObject, (.:), eitherDecodeFileStrict)
import qualified Data.Text as T
import qualified Data.Vector as V
import Text.Printf (printf)

import Hanalyze.Model.HBM (ModelP, Distribution (..), sample, observe,
                                    dataNamedObs, plateI, plateForM_, (.#))
import Hanalyze.Model.HBM.IR (synthVecIR)
import Hanalyze.Plot (HBMConfig (..), defaultHBM, hbm, (|->),
                              dashboardOf, hbmChainsR)
import Graphics.Hgg.Spec (ColData (..))
import Graphics.Hgg.Frame (BoundPlot, (|>>))
import Graphics.Hgg.Backend.Rasterific (savePNGBound)

import Common (summarize, printSummary, timeSamplingMs)

-- | posteriordb の @election88.json@ 形状 (N=11566・添字は1-based)。
data ElectionData = ElectionData
  { elN         :: Int
  , elNAge      :: Int
  , elNEdu      :: Int
  , elNAgeEdu   :: Int
  , elNState    :: Int
  , elNRegion   :: Int
  , elAge       :: [Int]
  , elAgeEdu    :: [Int]
  , elBlack     :: [Double]
  , elEdu       :: [Int]
  , elFemale    :: [Double]
  , elRegion    :: [Int]
  , elState     :: [Int]
  , elVPrev     :: [Double]
  , elY         :: [Int]
  }

instance FromJSON ElectionData where
  parseJSON = withObject "ElectionData" $ \v ->
    ElectionData <$> v .: "N" <*> v .: "n_age" <*> v .: "n_edu"
                 <*> v .: "n_age_edu" <*> v .: "n_state" <*> v .: "n_region_full"
                 <*> v .: "age" <*> v .: "age_edu" <*> v .: "black"
                 <*> v .: "edu" <*> v .: "female" <*> v .: "region_full"
                 <*> v .: "state" <*> v .: "v_prev_full" <*> v .: "y"

noDf :: [(T.Text, ColData)]
noDf = []

dataPath :: FilePath
dataPath = "bench/posteriordb/25-election88/data/election88.json"

figuresDir :: FilePath
figuresDir = "bench/posteriordb/25-election88/figures"

readData :: IO ElectionData
readData = either fail pure =<< eitherDecodeFileStrict dataPath

-- | 5本の独立階層を持つ多水準ロジスティック回帰 (Stan 原典と同一構造)。
-- 添字列は微分対象ではない構造的定数なので closure で直接渡す。
electionModel :: Int -> Int -> Int -> Int -> Int
              -> [Int] -> [Int] -> [Double] -> [Int] -> [Double] -> [Int] -> [Int] -> [Double]
              -> ModelP ()
electionModel nAge nEdu nAgeEdu nState nRegion
              ageIdx ageEduIdx blackD eduIdx femaleD regionIdx stateIdx vPrevD = do
  sigmaA <- sample "sigma_a" (HalfCauchy 25)
  sigmaB <- sample "sigma_b" (HalfCauchy 25)
  sigmaC <- sample "sigma_c" (HalfCauchy 25)
  sigmaD <- sample "sigma_d" (HalfCauchy 25)
  sigmaE <- sample "sigma_e" (HalfCauchy 25)
  aRaw <- plateI "age"    nAge    $ \i -> sample ("a" .# i) (Normal 0 sigmaA)
  bRaw <- plateI "edu"    nEdu    $ \i -> sample ("b" .# i) (Normal 0 sigmaB)
  cRaw <- plateI "ageEdu" nAgeEdu $ \i -> sample ("c" .# i) (Normal 0 sigmaC)
  dRaw <- plateI "state"  nState  $ \i -> sample ("d" .# i) (Normal 0 sigmaD)
  eRaw <- plateI "region" nRegion $ \i -> sample ("e" .# i) (Normal 0 sigmaE)
  let aV = V.fromList aRaw
      bV = V.fromList bRaw
      cV = V.fromList cRaw
      dV = V.fromList dRaw
      eV = V.fromList eRaw
  beta1 <- sample "beta1" (Normal 0 100)
  beta2 <- sample "beta2" (Normal 0 100)
  beta3 <- sample "beta3" (Normal 0 100)
  beta4 <- sample "beta4" (Normal 0 100)
  beta5 <- sample "beta5" (Normal 0 100)
  ys <- dataNamedObs "y" []
  plateForM_ "obs" (zip9' ageIdx ageEduIdx blackD eduIdx femaleD regionIdx stateIdx vPrevD ys)
    $ \(ageI, ageEduI, blkD, eduI, femD, regI, stI, vprevD, yVal) ->
        let blk   = realToFrac blkD
            fem   = realToFrac femD
            vprev = realToFrac vprevD
            yHat = beta1 + beta2 * blk + beta3 * fem + beta5 * fem * blk + beta4 * vprev
                 + (aV V.! (ageI - 1)) + (bV V.! (eduI - 1)) + (cV V.! (ageEduI - 1))
                 + (dV V.! (stI - 1)) + (eV V.! (regI - 1))
            p = 1 / (1 + exp (negate yHat))
        in observe "y" (Bernoulli p) [yVal]
  where
    zip9' (a1 : as) (b1 : bs) (c1 : cs) (d1 : ds) (e1 : es) (f1 : fs) (g1 : gs) (h1 : hs) (i1 : is_) =
      (a1, b1, c1, d1, e1, f1, g1, h1, i1) : zip9' as bs cs ds es fs gs hs is_
    zip9' _ _ _ _ _ _ _ _ _ = []

main :: IO ()
main = do
  d <- readData
  let df = [ ("y", NumData (V.fromList (map fromIntegral (elY d)))) ] :: [(T.Text, ColData)]
      cfg = defaultHBM { hbmChains = 4, hbmSamples = 1000
                        , hbmWarmup = 1000, hbmSeed = Just 1 }
      model :: ModelP ()
      model = electionModel (elNAge d) (elNEdu d) (elNAgeEdu d) (elNState d) (elNRegion d)
                             (elAge d) (elAgeEdu d) (elBlack d) (elEdu d) (elFemale d)
                             (elRegion d) (elState d) (elVPrev d)
      m = df |-> hbm cfg model

  putStrLn $ "synthVecIR = " ++
    (case synthVecIR model of
       Just _  -> "Just (vecIR高速経路)"
       Nothing -> "Nothing (legacy walk+ad)")

  (_, samplingMs) <- timeSamplingMs (hbmChainsR m)
  printf "sampling wall = %.1f ms (draws only, no dashboard/startup)\n" samplingMs

  -- 5階層分のlatent (計80群) + N=11566の観測を含むため dashboardFullOf
  -- でなく dashboardOf (健全性2x2パネルのみ・05-mh/10-ratsと同じ判断)。
  savePNGBound (figuresDir ++ "/hs_dashboard_full.png") $
    (noDf |>> dashboardOf m "y" :: BoundPlot)

  printSummary $ summarize
    ["beta1", "beta2", "beta3", "beta4", "beta5",
     "sigma_a", "sigma_b", "sigma_c", "sigma_d", "sigma_e"]
    (hbmChainsR m)
