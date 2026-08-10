{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Model.NaiveBayesSpec (spec) where

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
import qualified Hanalyze.Model.NaiveBayes     as NB
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Model.NaiveBayes (Phase 34-A5)" $ do
    -- Gaussian: 2 つの正規分布
    let xG = LA.fromLists $
               [[-2.0, -2.0], [-1.5, -1.8], [-2.1, -1.9], [-1.9, -2.2]] ++
               [[ 2.0,  2.0], [ 1.8,  2.1], [ 2.2,  1.9], [ 1.7,  2.3]]
        yG = VU.fromList ([0,0,0,0, 1,1,1,1] :: [Int])
    it "fitGNB + predictNB: 2 ガウス分離で訓練精度 100%" $ do
      let nb = NB.NBGaussian (NB.fitGNB xG yG)
          ys = NB.predictNB nb xG
          correct = VU.length (VU.filter id (VU.zipWith (==) ys yG))
      correct `shouldBe` VU.length yG
    it "fitGNB: gnbMeans の長さ = クラス数" $ do
      let nb = NB.fitGNB xG yG
      length (NB.gnbMeans nb) `shouldBe` 2
      NB.gnbClasses nb `shouldBe` [0, 1]
    it "predictNBLogProbs: 正規化済 (exp 和 = 1)" $ do
      let nb = NB.NBGaussian (NB.fitGNB xG yG)
          lps = NB.predictNBLogProbs nb xG
      and [ abs (sum [ exp z | z <- row ] - 1) < 1e-10 | row <- lps ]
        `shouldBe` True
    it "fitMNB + predictNB: カウント特徴 (Multinomial)" $ do
      -- クラス 0 は feature 0 中心、 クラス 1 は feature 1 中心
      let xM = LA.fromLists [[5,1,0],[6,0,1],[4,2,0],
                              [0,1,5],[1,0,6],[0,2,4]]
          yM = VU.fromList ([0,0,0, 1,1,1] :: [Int])
          nb = NB.NBMultinomial (NB.fitMNB 1.0 xM yM)
          ys = NB.predictNB nb xM
          correct = VU.length (VU.filter id (VU.zipWith (==) ys yM))
      correct `shouldBe` VU.length yM
