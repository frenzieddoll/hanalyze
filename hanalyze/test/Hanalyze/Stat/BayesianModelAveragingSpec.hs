{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Stat.BayesianModelAveragingSpec (spec) where

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
import qualified Data.Map.Strict as M
import qualified System.Random.MWC as MWC
import qualified Data.ByteString   as BS
import qualified System.Random.MWC as MWC
import qualified Hanalyze.MCMC.NUTS as NUTS
import qualified Hanalyze.Stat.BridgeSampling as BS
import qualified Hanalyze.Stat.BayesianModelAveraging as BMA
import qualified Hanalyze.Model.HBM as HBM
import qualified Data.Map.Strict    as M
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Stat.BayesianModelAveraging (Phase 29-A4)" $ do
    it "bayesianModelAveraging: 同じ log marginal で uniform weights、 prior 省略" $ do
      let r = BMA.bayesianModelAveraging [-10, -10, -10] Nothing
      BMA.bmaWeights r `shouldBe` [1/3, 1/3, 1/3]
    it "bayesianModelAveraging: 大きい log marginal が高い weight、 softmax stable" $ do
      let r = BMA.bayesianModelAveraging [-100, -98, -102] Nothing
      -- 真ん中 (-98) が最大、 隣接で 1 / e^2 ≈ 0.135 倍
      let ws = BMA.bmaWeights r
      sum ws `shouldSatisfy` (\s -> abs (s - 1.0) < 1e-9)
      (ws !! 1) `shouldSatisfy` (> ws !! 0)
      (ws !! 1) `shouldSatisfy` (> ws !! 2)
    it "averagePredictions: weighted sum of vectors" $ do
      let r = BMA.bayesianModelAveraging [log 0.7, log 0.3] Nothing
          v1 = LA.fromList [1, 2, 3]
          v2 = LA.fromList [4, 5, 6]
          avg = BMA.averagePredictions r [v1, v2]
      -- 0.7·[1,2,3] + 0.3·[4,5,6] = [1.9, 2.9, 3.9]
      LA.toList avg `shouldSatisfy`
        (\xs -> length xs == 3 && all (\(a,b) -> abs (a - b) < 1e-9)
                                     (zip xs [1.9, 2.9, 3.9]))
    it "BMA 統合: Bridge 経由の log marginal で 2 モデル比較" $ do
      let m0 :: HBM.ModelP ()
          m0 = do
            mu <- HBM.sample "mu" (HBM.Normal 0 1)
            HBM.observe "y" (HBM.Normal mu 1) (replicate 10 5.0)
          m1 :: HBM.ModelP ()
          m1 = do
            mu <- HBM.sample "mu" (HBM.Normal 0 10)
            HBM.observe "y" (HBM.Normal mu 1) (replicate 10 5.0)
          nutsCfg = NUTS.defaultNUTSConfig
            { NUTS.nutsIterations = 2000
            , NUTS.nutsBurnIn     = 500
            , NUTS.nutsAdaptStepSize = True
            }
      gen0 <- MWC.create
      ch0 <- NUTS.nuts m0 nutsCfg (M.fromList [("mu", 5)]) gen0
      gen1 <- MWC.create
      ch1 <- NUTS.nuts m1 nutsCfg (M.fromList [("mu", 5)]) gen1
      gen2 <- MWC.create
      r0 <- BS.bridgeSampling m0 BS.defaultBridgeConfig ch0 gen2
      gen3 <- MWC.create
      r1 <- BS.bridgeSampling m1 BS.defaultBridgeConfig ch1 gen3
      let bma = BMA.bayesianModelAveraging
            [BS.brLogMarginal r0, BS.brLogMarginal r1] Nothing
      -- M_1 (= 弱 prior) が dominant weight (> 0.9 想定)
      (BMA.bmaWeights bma !! 1) `shouldSatisfy` (> 0.9)
