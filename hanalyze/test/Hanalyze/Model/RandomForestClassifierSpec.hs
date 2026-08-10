{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Model.RandomForestClassifierSpec (spec) where

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
import qualified Hanalyze.Model.RandomForestClassifier as RFC
import qualified Data.Vector.Unboxed        as VU
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC as MWC
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Model.RandomForestClassifier (Phase 13.5)" $ do
    it "fitRFClassifier: 線形分離可能 2 クラスで OOB error 低い" $ do
      gen <- MWC.createSystemRandom
      let n = 60
          half = n `div` 2
          x = LA.fromColumns
                [ LA.fromList
                    (replicate half 0 ++ replicate half 1)
                , LA.fromList
                    (replicate half 0 ++ replicate half 1)
                ]
          y = VU.fromList (replicate half 0 ++ replicate half 1)
      fit <- RFC.fitRFClassifier
               (RFC.defaultRFCConfig { RFC.rfcNTrees = 30 })
               x y gen
      RFC.rfcOOBError fit `shouldSatisfy` (< 0.1)
      RFC.rfcClasses fit `shouldBe` [0, 1]
    it "predictRFClassifier: 訓練データで自己分類精度高い" $ do
      gen <- MWC.createSystemRandom
      let x = LA.fromColumns [LA.fromList [0,0,0,1,1,1,2,2,2]]
          y = VU.fromList    [0,0,0,1,1,1,2,2,2]
      fit <- RFC.fitRFClassifier
               (RFC.defaultRFCConfig { RFC.rfcNTrees = 20 })
               x y gen
      let pred_ = RFC.predictRFClassifier fit x
          correct = length [ () | i <- [0 .. VU.length y - 1]
                                , pred_ V.! i == y VU.! i ]
      correct `shouldSatisfy` (>= 8)
    it "permutation importance: ノイズ列 < 真の列" $ do
      gen <- MWC.createSystemRandom
      let n = 80
          half = n `div` 2
          xCol1 = LA.fromList (replicate half 0 ++ replicate half 1)
          xCol2 = LA.fromList [ sin (fromIntegral i) | i <- [0 .. n - 1] ]
          x = LA.fromColumns [xCol1, xCol2]
          y = VU.fromList (replicate half 0 ++ replicate half 1)
      fit <- RFC.fitRFClassifier
               (RFC.defaultRFCConfig { RFC.rfcNTrees = 30 })
               x y gen
      let imp = RFC.rfcImportance fit
      LA.atIndex imp 0 `shouldSatisfy` (>= LA.atIndex imp 1)
