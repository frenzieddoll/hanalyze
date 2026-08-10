{-# LANGUAGE OverloadedStrings #-}
-- | earnings-logearn_interaction (posteriordb) — hanalyze (ModelP) 実装。
-- log(earn) ~ height + male + height*male の交互作用回帰。
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
dataPath = "bench/posteriordb/34-logearn-interaction/data/earnings.json"

figuresDir :: FilePath
figuresDir = "bench/posteriordb/34-logearn-interaction/figures"

readData :: IO EData
readData = either fail pure =<< eitherDecodeFileStrict dataPath

logEarnInteractionModel :: ModelP ()
logEarnInteractionModel = do
  beta1 <- sample "beta1" (Normal 0 1000)
  beta2 <- sample "beta2" (Normal 0 1000)
  beta3 <- sample "beta3" (Normal 0 1000)
  beta4 <- sample "beta4" (Normal 0 1000)
  sigma <- sample "sigma" (HalfCauchy 25)
  heights <- dataNamedX "height" []
  males   <- dataNamedX "male"   []
  inters  <- dataNamedX "inter"  []
  logEarn <- dataNamedObs "log_earn" []
  plateForM_ "obs" (zip4' heights males inters logEarn) $ \(h, mm, it, le) ->
    observe "log_earn" (Normal (beta1 + beta2 * h + beta3 * mm + beta4 * it) sigma) [le]
  where
    zip4' (a : as) (b : bs) (c : cs) (d : ds) = (a, b, c, d) : zip4' as bs cs ds
    zip4' _ _ _ _ = []

main :: IO ()
main = do
  d <- readData
  let logEarn = map log (eEarn d)
      inter   = zipWith (*) (eHeight d) (eMale d)
      df = [ ("height",   NumData (V.fromList (eHeight d)))
           , ("male",     NumData (V.fromList (eMale d)))
           , ("inter",    NumData (V.fromList inter))
           , ("log_earn", NumData (V.fromList logEarn))
           ] :: [(T.Text, ColData)]
      cfg = defaultHBM { hbmChains = 4, hbmSamples = 1000
                        , hbmWarmup = 1000, hbmSeed = Just 1 }
      m = df |-> hbm cfg logEarnInteractionModel

  putStrLn $ "synthVecIR = " ++
    (case synthVecIR logEarnInteractionModel of
       Just _  -> "Just (vecIR高速経路)"
       Nothing -> "Nothing (legacy walk+ad)")

  (_, samplingMs) <- timeSamplingMs (hbmChainsR m)
  printf "sampling wall = %.1f ms (draws only, no dashboard/startup)\n" samplingMs

  savePNGBound (figuresDir ++ "/hs_dashboard_full.png") $
    (noDf |>> dashboardFullOf m "log_earn" :: BoundPlot)

  printSummary $ summarize ["beta1", "beta2", "beta3", "beta4", "sigma"] (hbmChainsR m)
