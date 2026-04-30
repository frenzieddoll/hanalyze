{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec
import DataFrame.Core
import Model.GLMM
import Model.GLM (Family (..), LinkFn (..))
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

  describe "Model.GLMM" $ do
    -- Dataset: 3 groups × 4 obs, strong between-group signal, weak within-group noise.
    -- True: β₀≈5, β₁≈0, u_A≈2, u_B≈0, u_C≈-2, σ²_u≈4, σ²≈small → ICC≈high.
    let xs  = V.fromList [1,2,3,4, 1,2,3,4, 1,2,3,4 :: Double]
        ys  = V.fromList [7.1,6.9,7.0,7.0, 5.0,4.9,5.1,5.0, 3.0,2.9,3.1,3.0]
        gs  = V.fromList ["A","A","A","A","B","B","B","B","C","C","C","C"]
        df  = mkDataFrame
                [ ("x",     NumericCol xs)
                , ("y",     NumericCol ys)
                , ("group", TextCol    gs) ]
        res = fitLMEDataFrame [("x", 1)] "group" "y" df

    it "returns Just for valid input" $
      res `shouldSatisfy` (\r -> case r of { Just _ -> True; Nothing -> False })

    it "ICC is in [0, 1]" $
      maybe (expectationFailure "expected Just") (\r ->
        glmmICC r `shouldSatisfy` (\v -> v >= 0 && v <= 1)) res

    it "ICC is high for strongly grouped data" $
      maybe (expectationFailure "expected Just") (\r ->
        glmmICC r `shouldSatisfy` (> 0.9)) res

    it "random variance is positive" $
      maybe (expectationFailure "expected Just") (\r ->
        glmmRandVar r `shouldSatisfy` (> 0)) res

    it "residual variance is positive" $
      maybe (expectationFailure "expected Just") (\r ->
        glmmResidVar r `shouldSatisfy` (> 0)) res

    it "BLUP count equals number of groups" $
      maybe (expectationFailure "expected Just") (\r ->
        V.length (glmmBLUPs r) `shouldBe` 3) res

    it "group labels are sorted" $
      case res of
        Just r  -> glmmGroups r `shouldBe` V.fromList ["A","B","C"]
        Nothing -> expectationFailure "expected Just"

    it "returns Nothing for missing column" $
      fitLMEDataFrame [("x", 1)] "group" "missing" df
        `shouldSatisfy` (\r -> case r of { Nothing -> True; Just _ -> False })

  describe "Model.GLMM (non-Gaussian)" $ do
    -- Binomial GLMM: 3 hospitals, binary outcome (treatment success)
    -- Strong hospital effect; within each hospital, dose → higher success rate.
    -- True: u_A ≈ +1, u_B ≈ 0, u_C ≈ -1  (on logit scale)
    let doseV  = V.fromList [1,2,3,4,5, 1,2,3,4,5, 1,2,3,4,5 :: Double]
        succV  = V.fromList [1,1,1,1,1, 1,1,0,1,0, 0,0,0,1,0 :: Double]
        hospV  = V.fromList ["A","A","A","A","A","B","B","B","B","B","C","C","C","C","C"]
        dfBin  = mkDataFrame
                   [ ("dose",     NumericCol doseV)
                   , ("success",  NumericCol succV)
                   , ("hospital", TextCol    hospV) ]
        resBin = fitGLMMDataFrame Binomial Logit [("dose", 1)] "hospital" "success" dfBin

    it "Binomial GLMM returns Just" $
      resBin `shouldSatisfy` (\r -> case r of { Just _ -> True; Nothing -> False })

    it "Binomial ICC in [0, 1]" $
      maybe (expectationFailure "expected Just") (\r ->
        glmmICC r `shouldSatisfy` (\v -> v >= 0 && v <= 1)) resBin

    it "Binomial σ²_u is positive" $
      maybe (expectationFailure "expected Just") (\r ->
        glmmRandVar r `shouldSatisfy` (> 0)) resBin

    -- Poisson GLMM: 3 regions, count outcome (events per month)
    -- True: β₀ on log scale ≈ 2 (≈7 events baseline), u differs by region.
    let timeV   = V.fromList [1,2,3,4,5, 1,2,3,4,5, 1,2,3,4,5 :: Double]
        countV  = V.fromList [15,18,22,20,25, 7,9,8,10,11, 2,3,2,4,3 :: Double]
        regionV = V.fromList ["X","X","X","X","X","Y","Y","Y","Y","Y","Z","Z","Z","Z","Z"]
        dfPois  = mkDataFrame
                    [ ("time",   NumericCol timeV)
                    , ("count",  NumericCol countV)
                    , ("region", TextCol    regionV) ]
        resPois = fitGLMMDataFrame Poisson Log [("time", 1)] "region" "count" dfPois

    it "Poisson GLMM returns Just" $
      resPois `shouldSatisfy` (\r -> case r of { Just _ -> True; Nothing -> False })

    it "Poisson σ²_u is positive" $
      maybe (expectationFailure "expected Just") (\r ->
        glmmRandVar r `shouldSatisfy` (> 0)) resPois

    it "Poisson ICC in [0, 1]" $
      maybe (expectationFailure "expected Just") (\r ->
        glmmICC r `shouldSatisfy` (\v -> v >= 0 && v <= 1)) resPois
