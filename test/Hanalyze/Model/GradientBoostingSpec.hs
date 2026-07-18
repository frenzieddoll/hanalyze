{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Model.GradientBoostingSpec (spec) where

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
import qualified Hanalyze.Model.GradientBoosting as GB
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Model.GradientBoosting (Phase 34-A1)" $ do
    let xs = [[fromIntegral i / 5 - 1] | i <- [0 :: Int .. 19]]
        xMat = LA.fromLists xs
        ys = VU.fromList [2 * (fromIntegral i / 5 - 1) + 1
                         | i <- [0 :: Int .. 19]]
        -- 二値分類: x < 0 → 0, x >= 0 → 1
        yCls = VU.fromList ([0,0,0,0,0,0,0,0,0,0,
                              1,1,1,1,1,1,1,1,1,1] :: [Int])
    it "fitGBRegressor: 線形データに収束 (MSE 低)" $ do
      let cfg = GB.defaultGBM { GB.gbNRounds = 100
                                   , GB.gbMaxDepth = 3
                                   , GB.gbLearnRate = 0.1 }
          gb  = GB.fitGBRegressor cfg xMat ys
          yhat = GB.predictGBR gb xMat
          mse  = VU.sum (VU.zipWith (\a b -> (a - b)^(2::Int)) yhat ys)
                   / fromIntegral (VU.length ys)
      mse `shouldSatisfy` (< 0.05)
    it "fitGBRegressor: gbrTrees の長さ = gbNRounds" $ do
      let cfg = GB.defaultGBM { GB.gbNRounds = 20 }
          gb  = GB.fitGBRegressor cfg xMat ys
      length (GB.gbrTrees gb) `shouldBe` 20
    it "fitGBClassifier: 線形分離可能データで訓練精度 100%" $ do
      let cfg = GB.defaultGBM { GB.gbNRounds = 50
                                   , GB.gbMaxDepth = 2 }
          gb  = GB.fitGBClassifier cfg xMat yCls
          yhat = GB.predictGBC gb xMat
          correct = VU.length (VU.filter id
                       (VU.zipWith (==) yhat yCls))
      correct `shouldBe` VU.length yCls
    it "predictGBCProbs: 全行 [0,1] 範囲" $ do
      let cfg = GB.defaultGBM { GB.gbNRounds = 20 }
          gb  = GB.fitGBClassifier cfg xMat yCls
          ps  = GB.predictGBCProbs gb xMat
      VU.all (\p -> p >= 0 && p <= 1) ps `shouldBe` True
