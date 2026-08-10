{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
-- | 'Hanalyze.Data.Wrangle' のテスト (Phase 67)。
--   dplyr summarise/mutate/group_by 相当の出力一致を確認する。
module Hanalyze.Data.WrangleSpec (spec) where

import Test.Hspec
import           Data.Text (Text)
import qualified DataFrame.Internal.Column    as DF
import qualified DataFrame.Internal.DataFrame  as DF
import qualified DataFrame.Operations.Core     as DF
import qualified DataFrame.Operators           as DF
import qualified DataFrame.Internal.Column as DFC
import           DataFrame.Operators ((|>))
import Hanalyze.Data.Wrangle

near :: Double -> Double -> Bool
near a b = abs (a - b) < 1e-9

-- g = c(a,a,b,b,b), x = c(1,2,3,5,7)
df :: DF.DataFrame
df = DF.fromNamedColumns
  [ ("g", DF.fromList (["a","a","b","b","b"] :: [Text]))
  , ("x", DF.fromList ([1,2,3,5,7] :: [Double])) ]

col :: forall a. DFC.Columnable a => Text -> DF.DataFrame -> [a]
col n = DF.columnAsList (DF.col @a n)

spec :: Spec
spec = describe "Hanalyze.Data.Wrangle" $ do

  describe "summarise (ungrouped・1 行)" $ do
    let r = df |> summarise [ "mean" =: meanOf "x"
                            , "q75"  =: quantileOf 0.75 "x"
                            , "n"    =: nOf ]
    it "1 行" $ fst (DF.dimensions r) `shouldBe` 1
    it "mean(x) = 3.6" $ head (col @Double "mean" r) `shouldSatisfy` near 3.6
    it "q75(x) = 5"    $ head (col @Double "q75" r)  `shouldSatisfy` near 5
    it "n = 5 (Int 列)" $ col @Int "n" r `shouldBe` [5]

  describe "groupBy + summarise (キー昇順)" $ do
    let r = summarise [ "mean" =: meanOf "x", "n" =: nOf ] (groupBy ["g"] df)
    it "2 群" $ fst (DF.dimensions r) `shouldBe` 2
    it "キー昇順 a,b" $ col @Text "g" r `shouldBe` ["a","b"]
    it "群 mean = [1.5, 5]" $
      (col @Double "mean" r) `shouldSatisfy` (\ms -> near (ms!!0) 1.5 && near (ms!!1) 5)
    it "群 n = [2,3]" $ col @Int "n" r `shouldBe` [2,3]

  describe "mutate (元列温存 + 新列)" $ do
    let r = df |> mutate [ "rank" =: minRankOf "x", "lag1" =: lagOf 1 "x" ]
    it "元列 x 温存" $ col @Double "x" r `shouldBe` [1,2,3,5,7]
    it "rank = 1..5" $ col @(Maybe Double) "rank" r
        `shouldBe` [Just 1, Just 2, Just 3, Just 4, Just 5]
    it "lag1 先頭 NA" $ col @(Maybe Double) "lag1" r
        `shouldBe` [Nothing, Just 1, Just 2, Just 3, Just 5]
