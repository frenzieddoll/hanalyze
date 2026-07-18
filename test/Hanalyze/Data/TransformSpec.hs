{-# LANGUAGE OverloadedStrings #-}
-- | 'Hanalyze.Data.Transform' のテスト (Phase 66)。
--   dplyr/base R の対応関数と数値・配置一致を確認する。 参照値は R 4.x。
module Hanalyze.Data.TransformSpec (spec) where

import Test.Hspec
import Data.Ord (Down (..))
import qualified Hanalyze.Data.Transform as T

spec :: Spec
spec = describe "Hanalyze.Data.Transform" $ do

  describe "順位 (dplyr ranking)" $ do
    -- x <- c(1, 5, 5, 17, 22)
    let x = [1, 5, 5, 17, 22] :: [Int]
    it "minRank (1,2,2,4,5)"     $ T.minRank x   `shouldBe` [1,2,2,4,5]
    it "denseRank (1,2,2,3,4)"   $ T.denseRank x `shouldBe` [1,2,2,3,4]
    it "rowNumber (1,2,3,4,5)"   $ T.rowNumber x `shouldBe` [1,2,3,4,5]
    it "percentRank (0,.25,.25,.75,1)" $
      T.percentRank x `shouldBe` [0, 0.25, 0.25, 0.75, 1]
    it "cumeDist (.2,.6,.6,.8,1)" $
      T.cumeDist x `shouldBe` [0.2, 0.6, 0.6, 0.8, 1.0]
    it "minRank desc (= Down): 5,3,3,2,1" $
      T.minRank (map Down x) `shouldBe` [5,3,3,2,1]

  describe "順位 NA 保持 (R x=c(1,5,5,17,22,NA))" $ do
    let xs = [Just 1, Just 5, Just 5, Just 17, Just 22, Nothing] :: [Maybe Int]
    it "minRankNA" $
      T.minRankNA xs `shouldBe` [Just 1, Just 2, Just 2, Just 4, Just 5, Nothing]
    it "denseRankNA" $
      T.denseRankNA xs `shouldBe` [Just 1, Just 2, Just 2, Just 3, Just 4, Nothing]
    it "percentRankNA" $
      T.percentRankNA xs `shouldBe` [Just 0, Just 0.25, Just 0.25, Just 0.75, Just 1, Nothing]

  describe "オフセット" $ do
    -- x <- c(2, 5, 11, 11, 19, 35)
    let x = [2, 5, 11, 11, 19, 35] :: [Int]
    it "lag 1 (先頭 default)" $
      T.lag 1 (-1) x `shouldBe` [-1, 2, 5, 11, 11, 19]
    it "lead 1 (末尾 default)" $
      T.lead 1 (-1) x `shouldBe` [5, 11, 11, 19, 35, -1]
    it "lag 2" $ T.lag 2 0 x `shouldBe` [0, 0, 2, 5, 11, 11]
    it "lag n>=length = 全 default" $ T.lag 10 0 x `shouldBe` [0,0,0,0,0,0]

  describe "累積" $ do
    it "cumsum 1:10" $ T.cumsum [1..10 :: Int] `shouldBe` [1,3,6,10,15,21,28,36,45,55]
    it "cumprod"  $ T.cumprod [1,2,3,4 :: Int] `shouldBe` [1,2,6,24]
    it "cummin"   $ T.cummin [5,3,4,1,2 :: Int] `shouldBe` [5,3,3,1,1]
    it "cummax"   $ T.cummax [1,3,2,5,4 :: Int] `shouldBe` [1,3,3,5,5]
    it "cummean"  $ T.cummean [2,4,6] `shouldBe` [2,3,4]

  describe "区間化 (cut・right=TRUE)" $ do
    -- x <- c(1,2,5,10,15,20); cut(x, breaks=c(0,5,10,15,20))
    --   → (0,5] (0,5] (0,5] (5,10] (10,15] (15,20]  = bin 1,1,1,2,3,4
    let x = [1,2,5,10,15,20]
    it "cut bin index" $
      T.cut [0,5,10,15,20] x `shouldBe` [Just 1, Just 1, Just 1, Just 2, Just 3, Just 4]
    it "範囲外 → Nothing" $
      T.cut [0,5,10,15,20] [-10, 5, 10, 30] `shouldBe` [Nothing, Just 1, Just 2, Nothing]
    it "ラベル付き" $
      T.cutLabels (["sm","md","lg","xl"] :: [String]) [0,5,10,15,20] x
        `shouldBe` [Just "sm", Just "sm", Just "sm", Just "md", Just "lg", Just "xl"]

  describe "連続識別子 (consecutive_id)" $ do
    -- x <- c("a","a","a","b","c","c","d","e","a","a","b","b")
    it "値が変わるたび +1" $
      T.consecutiveId (["a","a","a","b","c","c","d","e","a","a","b","b"] :: [String])
        `shouldBe` [1,1,1,2,3,3,4,5,6,6,7,7]
