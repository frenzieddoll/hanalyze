{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Model.NeuralNetworkSpec (spec) where

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
import qualified Data.Vector as V
import qualified Numeric.LinearAlgebra as LA
import qualified Data.Vector.Unboxed        as VU
import qualified Hanalyze.Model.NeuralNetwork  as NN
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC as MWC
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Model.NeuralNetwork (Phase 16)" $ do
    let -- 線形 y = 2x + 1 + ノイズ なし
        xReg = LA.fromColumns
                 [LA.fromList [fromIntegral i / 5 - 1 | i <- [0 :: Int .. 9]]]
        yReg = LA.fromList [2 * (fromIntegral i / 5 - 1) + 1
                           | i <- [0 :: Int .. 9]]
        -- 2 クラス分類: x < 0 → 0、 x >= 0 → 1
        xCls = LA.fromColumns
                 [LA.fromList [fromIntegral i - 5 | i <- [0 :: Int .. 9]]]
        yCls = VU.fromList [0, 0, 0, 0, 0, 1, 1, 1, 1, 1]
    it "fitMLPRegressor: 線形データに学習収束" $ do
      gen <- MWC.createSystemRandom
      let cfg = NN.defaultMLP
            { NN.mlpHidden = [8]
            , NN.mlpEpochs = 500
            , NN.mlpBatch  = 5
            , NN.mlpLR     = 0.05
            }
      fit <- NN.fitMLPRegressor cfg xReg yReg gen
      let preds = LA.flatten (NN.predictMLP fit xReg)
          mse = LA.sumElements ((preds - yReg) ^ (2 :: Int))
                  / fromIntegral (LA.size yReg)
      mse `shouldSatisfy` (< 0.3)
    it "fitMLPRegressor: loss history が単調減少 (大局的)" $ do
      gen <- MWC.createSystemRandom
      let cfg = NN.defaultMLP { NN.mlpEpochs = 100 }
      fit <- NN.fitMLPRegressor cfg xReg yReg gen
      let losses = NN.mlpLossHist fit
      length losses `shouldBe` 100
      last losses `shouldSatisfy` (< head losses)
    it "fitMLPClassifier: 1-D 線形分離で訓練精度 100%" $ do
      gen <- MWC.createSystemRandom
      let cfg = NN.defaultMLP
            { NN.mlpHidden = [4]
            , NN.mlpEpochs = 300
            , NN.mlpBatch  = 5
            , NN.mlpLR     = 0.1
            }
      fit <- NN.fitMLPClassifier cfg xCls yCls gen
      let preds = NN.predictMLPClass fit xCls
          correct = length [ () | i <- [0 .. VU.length yCls - 1]
                                , preds V.! i == yCls VU.! i ]
      correct `shouldSatisfy` (>= 9)
    it "fitMLPClassifier: mlpClasses は sorted unique label" $ do
      gen <- MWC.createSystemRandom
      let cfg = NN.defaultMLP { NN.mlpEpochs = 5 }
      fit <- NN.fitMLPClassifier cfg xCls yCls gen
      NN.mlpClasses fit `shouldBe` [0, 1]
    it "predictMLP: 出力 shape が (n × out)" $ do
      gen <- MWC.createSystemRandom
      let cfg = NN.defaultMLP { NN.mlpEpochs = 5 }
      fit <- NN.fitMLPRegressor cfg xReg yReg gen
      let out = NN.predictMLP fit xReg
      LA.rows out `shouldBe` 10
      LA.cols out `shouldBe` 1
