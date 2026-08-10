{-# LANGUAGE OverloadedStrings #-}
-- | earnings-logearn_height (posteriordb) — hanalyze (ModelP) 実装。
-- log(earn) ~ height の線形回帰。
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

data EData = EData { eEarn :: [Double], eHeight :: [Double] }

instance FromJSON EData where
  parseJSON = withObject "EData" $ \v ->
    EData <$> v .: "earn" <*> v .: "height"

noDf :: [(T.Text, ColData)]
noDf = []

dataPath :: FilePath
dataPath = "bench/posteriordb/32-logearn-height/data/earnings.json"

figuresDir :: FilePath
figuresDir = "bench/posteriordb/32-logearn-height/figures"

readData :: IO EData
readData = either fail pure =<< eitherDecodeFileStrict dataPath

logEarnHeightModel :: ModelP ()
logEarnHeightModel = do
  beta1 <- sample "beta1" (Normal 0 1000)
  beta2 <- sample "beta2" (Normal 0 1000)
  sigma <- sample "sigma" (HalfCauchy 25)
  heights <- dataNamedX "height" []
  logEarn <- dataNamedObs "log_earn" []
  plateForM_ "obs" (zip heights logEarn) $ \(h, le) ->
    observe "log_earn" (Normal (beta1 + beta2 * h) sigma) [le]

main :: IO ()
main = do
  d <- readData
  let logEarn = map log (eEarn d)
      df = [ ("height",   NumData (V.fromList (eHeight d)))
           , ("log_earn", NumData (V.fromList logEarn))
           ] :: [(T.Text, ColData)]
      cfg = defaultHBM { hbmChains = 4, hbmSamples = 1000
                        , hbmWarmup = 1000, hbmSeed = Just 1 }
      m = df |-> hbm cfg logEarnHeightModel

  putStrLn $ "synthVecIR = " ++
    (case synthVecIR logEarnHeightModel of
       Just _  -> "Just (vecIR高速経路)"
       Nothing -> "Nothing (legacy walk+ad)")

  (_, samplingMs) <- timeSamplingMs (hbmChainsR m)
  printf "sampling wall = %.1f ms (draws only, no dashboard/startup)\n" samplingMs

  savePNGBound (figuresDir ++ "/hs_dashboard_full.png") $
    (noDf |>> dashboardFullOf m "log_earn" :: BoundPlot)

  printSummary $ summarize ["beta1", "beta2", "sigma"] (hbmChainsR m)
