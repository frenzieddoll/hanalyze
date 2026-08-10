{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Model.KNNSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Hanalyze.Model.Formula
import Hanalyze.Model.Formula.Frame
import Hanalyze.Model.Formula.Design
import Hanalyze.Model.Formula.RFormula
import Hanalyze.Model.Formula.Nonlinear
import Hanalyze.Model.Formula.Mixed
import Hanalyze.Model.GLMM
import Hanalyze.Model.GLM (Family (..), LinkFn (..))
import Hanalyze.Stat.Distribution (Transform)
import Data.List (sort, nub)
import Control.Monad (forM, forM_)
import System.IO.Temp (withSystemTempFile)
import System.IO     (hPutStr, hClose)
import           Hanalyze.Model.HBM.Ast (Expr (..), Lit (..), DoStmt (..), Err)
import           Data.IORef         (newIORef, readIORef, modifyIORef')
import qualified Numeric.LinearAlgebra as LA
import qualified Data.Vector.Unboxed        as VU
import qualified Data.Map.Strict as M
import qualified Hanalyze.Model.KNN            as KNN
import qualified Data.Map.Strict    as M
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Model.KNN (Phase 34-A4)" $ do
    let xTrain = LA.fromLists [[fromIntegral i] | i <- [0 :: Int .. 9]]
        yTrainR = VU.fromList [fromIntegral i * 2 | i <- [0 :: Int .. 9]]
        yTrainC = VU.fromList ([0,0,0,0,0,1,1,1,1,1] :: [Int])
        xTest   = LA.fromLists [[2.0], [7.5]]
    it "fitKNNR + predictKNNR: k=3 で局所平均" $ do
      let knn = KNN.fitKNNR 3 xTrain yTrainR
          ys  = KNN.predictKNNR knn xTest
      VU.length ys `shouldBe` 2
      -- x=2 → 近傍 {1,2,3} の y= {2,4,6} → 平均 4
      abs (ys VU.! 0 - 4.0) `shouldSatisfy` (< 1e-9)
    it "fitKNNC + predictKNNC: 分類で多数決" $ do
      let knn = KNN.fitKNNC 3 xTrain yTrainC
          ys  = KNN.predictKNNC knn xTest
      ys VU.! 0 `shouldBe` 0
      ys VU.! 1 `shouldBe` 1
    it "fitKNNC: knnCClasses は sorted unique" $ do
      let knn = KNN.fitKNNC 3 xTrain yTrainC
      KNN.knnCClasses knn `shouldBe` [0, 1]
    it "predictKNNCProbs: 確率は和 1" $ do
      let knn = KNN.fitKNNC 3 xTrain yTrainC
          ps  = KNN.predictKNNCProbs knn xTest
      and [ abs (sum (M.elems m) - 1.0) < 1e-10 | m <- ps ]
        `shouldBe` True
