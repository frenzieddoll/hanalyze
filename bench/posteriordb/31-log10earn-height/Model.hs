{-# LANGUAGE OverloadedStrings #-}
-- | earnings-log10earn_height (posteriordb) — hanalyze (ModelP) 実装。
-- log10(earn) ~ height の線形回帰。log10変換はreadData後にHaskell側で
-- 適用 (Stanのtransformed dataと同型)。
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
dataPath = "bench/posteriordb/31-log10earn-height/data/earnings.json"

figuresDir :: FilePath
figuresDir = "bench/posteriordb/31-log10earn-height/figures"

readData :: IO EData
readData = either fail pure =<< eitherDecodeFileStrict dataPath

log10EarnHeightModel :: ModelP ()
log10EarnHeightModel = do
  beta1 <- sample "beta1" (Normal 0 1000)
  beta2 <- sample "beta2" (Normal 0 1000)
  sigma <- sample "sigma" (HalfCauchy 25)
  heights   <- dataNamedX "height" []
  log10Earn <- dataNamedObs "log10_earn" []
  plateForM_ "obs" (zip heights log10Earn) $ \(h, le) ->
    observe "log10_earn" (Normal (beta1 + beta2 * h) sigma) [le]

main :: IO ()
main = do
  d <- readData
  let log10Earn = map (logBase 10) (eEarn d)
      df = [ ("height",     NumData (V.fromList (eHeight d)))
           , ("log10_earn", NumData (V.fromList log10Earn))
           ] :: [(T.Text, ColData)]
      cfg = defaultHBM { hbmChains = 4, hbmSamples = 1000
                        , hbmWarmup = 1000, hbmSeed = Just 1 }
      m = df |-> hbm cfg log10EarnHeightModel

  putStrLn $ "synthVecIR = " ++
    (case synthVecIR log10EarnHeightModel of
       Just _  -> "Just (vecIR高速経路)"
       Nothing -> "Nothing (legacy walk+ad)")

  (_, samplingMs) <- timeSamplingMs (hbmChainsR m)
  printf "sampling wall = %.1f ms (draws only, no dashboard/startup)\n" samplingMs

  savePNGBound (figuresDir ++ "/hs_dashboard_full.png") $
    (noDf |>> dashboardFullOf m "log10_earn" :: BoundPlot)

  printSummary $ summarize ["beta1", "beta2", "sigma"] (hbmChainsR m)
