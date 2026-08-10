{-# LANGUAGE OverloadedStrings #-}
-- | earnings-earn_height (posteriordb) — hanalyze (ModelP) 実装。
-- 単純な線形回帰 (earn ~ height)。beta/sigma に明示的priorなし (暗黙flat)
-- のため Normal(0,1000)/HalfCauchy(25) に置換。
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
dataPath = "bench/posteriordb/30-earn-height/data/earnings.json"

figuresDir :: FilePath
figuresDir = "bench/posteriordb/30-earn-height/figures"

readData :: IO EData
readData = either fail pure =<< eitherDecodeFileStrict dataPath

earnHeightModel :: ModelP ()
earnHeightModel = do
  beta1 <- sample "beta1" (Normal 0 1000)
  beta2 <- sample "beta2" (Normal 0 1000)
  sigma <- sample "sigma" (HalfCauchy 25)
  heights <- dataNamedX "height" []
  earns   <- dataNamedObs "earn" []
  plateForM_ "obs" (zip heights earns) $ \(h, e) ->
    observe "earn" (Normal (beta1 + beta2 * h) sigma) [e]

main :: IO ()
main = do
  d <- readData
  let df = [ ("height", NumData (V.fromList (eHeight d)))
           , ("earn",   NumData (V.fromList (eEarn d)))
           ] :: [(T.Text, ColData)]
      cfg = defaultHBM { hbmChains = 4, hbmSamples = 1000
                        , hbmWarmup = 1000, hbmSeed = Just 1 }
      m = df |-> hbm cfg earnHeightModel

  putStrLn $ "synthVecIR = " ++
    (case synthVecIR earnHeightModel of
       Just _  -> "Just (vecIR高速経路)"
       Nothing -> "Nothing (legacy walk+ad)")

  (_, samplingMs) <- timeSamplingMs (hbmChainsR m)
  printf "sampling wall = %.1f ms (draws only, no dashboard/startup)\n" samplingMs

  savePNGBound (figuresDir ++ "/hs_dashboard_full.png") $
    (noDf |>> dashboardFullOf m "earn" :: BoundPlot)

  printSummary $ summarize ["beta1", "beta2", "sigma"] (hbmChainsR m)
