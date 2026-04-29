{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec
import DataFrame.Core
import qualified Data.Vector as V

main :: IO ()
main = hspec $ do
  describe "DataFrame.Core" $ do
    it "mkDataFrame stores columns" $ do
      let df = mkDataFrame [("x", NumericCol (V.fromList [1,2,3]))]
      columnNames df `shouldBe` ["x"]

    it "getNumeric retrieves numeric column" $ do
      let v  = V.fromList [1.0, 2.0, 3.0]
          df = mkDataFrame [("x", NumericCol v)]
      getNumeric "x" df `shouldBe` Just v

    it "numRows counts rows" $ do
      let df = mkDataFrame [("x", NumericCol (V.fromList [1,2,3]))]
      numRows df `shouldBe` 3
