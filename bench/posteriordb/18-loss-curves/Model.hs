{-# LANGUAGE OverloadedStrings #-}
-- | loss_curves-losscurve_sislob (posteriordb) — hanalyze (ModelP) 実装。
--
-- Phase 89: posteriordb 横断ベンチマーク。保険数理の損失三角形
-- (loss reserving)。n_cohort=10 (契約年度)・n_time=10 (経過年)・
-- n_data=55 (= 10+9+...+1・下三角が未観測の古典的三角形構造)。
-- Weibull 型成長曲線 (`growthmodel_id=1`、データで確認済み) で
-- 損失の発展パターンをモデル化する。
--
-- Stan 原典 (posteriordb `models/stan/losscurve_sislob.stan`):
--   gf[t] = 1 - exp(-(t/theta)^omega)                      // growth_factor_weibull
--   lm[i] = LR[cohort_id[i]] * premium[cohort_id[i]] * gf[t_idx[i]]
--   loss[i] ~ normal(lm[i], loss_sd*premium[cohort_id[i]])
--   mu_LR ~ normal(0,0.5); sd_LR ~ lognormal(0,0.5); LR ~ lognormal(mu_LR,sd_LR)
--   loss_sd ~ lognormal(0,0.7); omega/theta ~ lognormal(0,0.5)
--
-- 全 prior が LogNormal/Normal (unconstrained or PositiveT) のため、
-- 01-glm-poisson/10-rats 系列で確立した「Uniform境界外初期値の罠」は
-- 該当しない (LogNormalはPositiveT変換を持つ・安全)。
--
-- reference_posterior_name = null (posteriordb に公式 reference posterior 無し)。
--
-- ビルド: cabal build --project-file=cabal.project.plot posteriordb-loss-curves
module Main (main) where

import Data.Aeson (FromJSON (..), withObject, (.:), eitherDecodeFileStrict)
import qualified Data.Text as T
import qualified Data.Vector as V
import Text.Printf (printf)

import Hanalyze.Model.HBM (ModelP, Distribution (..), sample, observe,
                                    dataNamedX, dataNamedObs, plateI, plateForM_, (.#))
import Hanalyze.Model.HBM (gradPathLabel)
import Hanalyze.Plot (hbmModelSpec)
import Hanalyze.Plot (HBMConfig (..), defaultHBM, hbm, (|->),
                              dashboardFullOf, hbmChainsR)
import Graphics.Hgg.Spec (ColData (..))
import Graphics.Hgg.Frame (BoundPlot, (|>>))
import Graphics.Hgg.Backend.Rasterific (savePNGBound)

import Common (summarize, printSummary, timeSamplingMs)

-- | posteriordb の @loss_curves.json@ 形状
-- ({"n_data":55,"n_time":10,"n_cohort":10,"cohort_id":[...],"t_idx":[...],
--   "t_value":[...],"premium":[...],"loss":[...]}・1-based 添字)。
data LossData = LossData
  { nCohort  :: Int
  , nTime    :: Int
  , cohortId :: [Int]
  , tIdx     :: [Int]
  , tValue   :: [Double]
  , premium  :: [Double]
  , loss     :: [Double]
  }

instance FromJSON LossData where
  parseJSON = withObject "LossData" $ \v ->
    LossData <$> v .: "n_cohort" <*> v .: "n_time" <*> v .: "cohort_id"
             <*> v .: "t_idx" <*> v .: "t_value" <*> v .: "premium" <*> v .: "loss"

noDf :: [(T.Text, ColData)]
noDf = []

dataPath :: FilePath
dataPath = "bench/posteriordb/18-loss-curves/data/loss_curves.json"

figuresDir :: FilePath
figuresDir = "bench/posteriordb/18-loss-curves/figures"

readData :: IO LossData
readData = either fail pure =<< eitherDecodeFileStrict dataPath

-- | Weibull成長曲線による損失三角形モデル (Stan 原典と同一構造・
-- growthmodel_id=1 固定)。 premium/t_value はコホート/経過年ごとの
-- 固定定数として df 経由で束縛 ('dataNamedX')・cohort_id/t_idx は
-- 微分対象ではない構造的添字なので素の @[Int]@ をクロージャで直接渡す
-- (05-mh/16-lda と同じ流儀)。
lossModel :: Int -> [Int] -> [Int] -> ModelP ()
lossModel nT cids tidxs = do
  omega  <- sample "omega"   (LogNormal 0 0.5)
  theta  <- sample "theta"   (LogNormal 0 0.5)
  muLR   <- sample "mu_LR"   (Normal 0 0.5)
  sdLR   <- sample "sd_LR"   (LogNormal 0 0.5)
  lrs    <- plateI "cohort" 10 $ \i -> sample ("LR" .# i) (LogNormal muLR sdLR)
  lossSd <- sample "loss_sd" (LogNormal 0 0.7)
  tValues  <- dataNamedX   "t_value" []
  premiums <- dataNamedX   "premium" []
  losses   <- dataNamedObs "loss"    []
  let tValueV  = V.fromList tValues
      premiumV = V.fromList premiums
      lrV      = V.fromList lrs
      gfV = V.generate nT $ \i ->
              let tv = tValueV V.! i
              in 1 - exp (negate ((tv / theta) ** omega))
  plateForM_ "obs" (zip3 cids tidxs losses) $ \(cid, tidx, lossVal) ->
    let lm = (lrV V.! (cid - 1)) * (premiumV V.! (cid - 1)) * (gfV V.! (tidx - 1))
        sd = lossSd * (premiumV V.! (cid - 1))
    in observe "loss" (Normal lm sd) [lossVal]

main :: IO ()
main = do
  d <- readData
  let df = [ ("t_value", NumData (V.fromList (tValue d)))
           , ("premium", NumData (V.fromList (premium d)))
           , ("loss",    NumData (V.fromList (loss d)))
           ] :: [(T.Text, ColData)]
      -- PyMC 側 (model.py) と同じ設定を定数で揃える。
      cfg = defaultHBM { hbmChains = 4, hbmSamples = 1000
                        , hbmWarmup = 1000, hbmSeed = Just 1 }
      model :: ModelP ()
      model = lossModel (nTime d) (cohortId d) (tIdx d)
      m = df |-> hbm cfg model

  -- 勾配経路 = compileGradUV が実際に選ぶ経路 (束縛済 hbmModelSpec で判定・
  -- Phase 91 A4: 生モデルを synthVecIR に渡すと data 空で誤表示するため差替)。
  putStrLn $ "勾配経路 = " ++ gradPathLabel (hbmModelSpec m)

  (_, samplingMs) <- timeSamplingMs (hbmChainsR m)
  printf "sampling wall = %.1f ms (draws only, no dashboard/startup)\n" samplingMs

  savePNGBound (figuresDir ++ "/hs_dashboard_full.png") $
    (noDf |>> dashboardFullOf m "loss" :: BoundPlot)

  printSummary $ summarize
    (["omega", "theta", "mu_LR", "sd_LR", "loss_sd"] ++
     [ "LR_" <> T.pack (show i) | i <- [0 .. nCohort d - 1] ])
    (hbmChainsR m)
