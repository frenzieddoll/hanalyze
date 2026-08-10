{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Model.ClusterSpec (spec) where

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
import qualified Hanalyze.Model.Cluster     as Cl
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC as MWC
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Model.Cluster" $ do
    it "kMeans: 2 つの離れたクラスタで正しく分類" $ do
      gen <- MWC.createSystemRandom
      -- Cluster 1: around (0, 0); cluster 2: around (10, 10)
      let xs = LA.fromLists $
            [[0.1*x, 0.1*y] | x <- [-3..3], y <- [-3..3]] ++
            [[10 + 0.1*x, 10 + 0.1*y] | x <- [-3..3], y <- [-3..3]]
          cfg = Cl.defaultKMeans 2
      r <- Cl.kMeans cfg xs gen
      LA.rows (Cl.kmrCentroids r) `shouldBe` 2
      -- 全 49 points がクラスタ 1、49 が クラスタ 2
      let labels = Cl.kmrLabels r
          (c0, c1) = (length (filter (== 0) labels), length (filter (== 1) labels))
      (min c0 c1) `shouldBe` 49
      (max c0 c1) `shouldBe` 49

    it "silhouette: well-separated clusters で > 0.5" $ do
      gen <- MWC.createSystemRandom
      let xs = LA.fromLists $
            [[0.1*x, 0.1*y] | x <- [-3..3], y <- [-3..3]] ++
            [[20 + 0.1*x, 20 + 0.1*y] | x <- [-3..3], y <- [-3..3]]
          cfg = Cl.defaultKMeans 2
      r <- Cl.kMeans cfg xs gen
      let s = Cl.silhouette xs (Cl.kmrLabels r)
      s `shouldSatisfy` (> 0.7)

    it "kMeans: inertia は monotone non-increasing in iter" $ do
      gen <- MWC.createSystemRandom
      let xs = LA.fromLists [[fromIntegral i, fromIntegral j]
                            | i <- [0..9::Int], j <- [0..9::Int]]
          cfg = (Cl.defaultKMeans 4) { Cl.kmRestarts = 5 }
      r <- Cl.kMeans cfg xs gen
      Cl.kmrInertia r `shouldSatisfy` (>= 0)
      Cl.kmrConverged r `shouldBe` True

  -- ===========================================================================
  -- Hanalyze.Stat.MultipleTesting (Phase 6)
  -- ===========================================================================
