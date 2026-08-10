{-# LANGUAGE OverloadedStrings #-}
-- | earnings-logearn_logheight_male (posteriordb) — hanalyze (ModelP) 実装。
-- log(earn) ~ log(height) + male の線形回帰。
-- ★2026-07-13: コード準備のみ (データ未取得・cabal buildのみ確認)。
module Main (main) where

import Data.Aeson (FromJSON (..), withObject, (.:), eitherDecodeFileStrict)
import qualified Data.Text as T
import qualified Data.Vector as V
import Text.Printf (printf)

import Hanalyze.Model.HBM (ModelP, Distribution (..), sample, observe,
                                    dataNamedX, dataNamedObs, plateForM_)
import Hanalyze.Model.HBM.IR (synthVecIR)
import Hanalyze.Plot (HBMConfig (..), defaultHBM, hbm, (|->),
                              dashboardFullOf, hbmChainsR)
import Graphics.Hgg.Spec (ColData (..))
import Graphics.Hgg.Frame (BoundPlot, (|>>))
import Graphics.Hgg.Backend.Rasterific (savePNGBound)

import Common (summarize, printSummary, timeSamplingMs)

data EData = EData { eEarn :: [Double], eHeight :: [Double], eMale :: [Double] }

instance FromJSON EData where
  parseJSON = withObject "EData" $ \v ->
    EData <$> v .: "earn" <*> v .: "height" <*> v .: "male"

noDf :: [(T.Text, ColData)]
noDf = []

dataPath :: FilePath
dataPath = "bench/posteriordb/36-logearn-logheight-male/data/earnings.json"

figuresDir :: FilePath
figuresDir = "bench/posteriordb/36-logearn-logheight-male/figures"

readData :: IO EData
readData = either fail pure =<< eitherDecodeFileStrict dataPath

logEarnLogheightMaleModel :: ModelP ()
logEarnLogheightMaleModel = do
  beta1 <- sample "beta1" (Normal 0 1000)
  beta2 <- sample "beta2" (Normal 0 1000)
  beta3 <- sample "beta3" (Normal 0 1000)
  sigma <- sample "sigma" (HalfCauchy 25)
  logHeights <- dataNamedX "log_height" []
  males      <- dataNamedX "male"       []
  logEarn    <- dataNamedObs "log_earn" []
  plateForM_ "obs" (zip3 logHeights males logEarn) $ \(lh, mm, le) ->
    observe "log_earn" (Normal (beta1 + beta2 * lh + beta3 * mm) sigma) [le]

main :: IO ()
main = do
  d <- readData
  let logEarn   = map log (eEarn d)
      logHeight = map log (eHeight d)
      df = [ ("log_height", NumData (V.fromList logHeight))
           , ("male",       NumData (V.fromList (eMale d)))
           , ("log_earn",   NumData (V.fromList logEarn))
           ] :: [(T.Text, ColData)]
      cfg = defaultHBM { hbmChains = 4, hbmSamples = 1000
                        , hbmWarmup = 1000, hbmSeed = Just 1 }
      m = df |-> hbm cfg logEarnLogheightMaleModel

  putStrLn $ "synthVecIR = " ++
    (case synthVecIR logEarnLogheightMaleModel of
       Just _  -> "Just (vecIR高速経路)"
       Nothing -> "Nothing (legacy walk+ad)")

  (_, samplingMs) <- timeSamplingMs (hbmChainsR m)
  printf "sampling wall = %.1f ms (draws only, no dashboard/startup)\n" samplingMs

  savePNGBound (figuresDir ++ "/hs_dashboard_full.png") $
    (noDf |>> dashboardFullOf m "log_earn" :: BoundPlot)

  printSummary $ summarize ["beta1", "beta2", "beta3", "sigma"] (hbmChainsR m)
