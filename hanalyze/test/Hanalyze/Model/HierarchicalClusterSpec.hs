{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Model.HierarchicalClusterSpec (spec) where

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
import qualified Hanalyze.Model.HierarchicalCluster as HC
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Model.HierarchicalCluster (Phase 12)" $ do
    let blob = LA.fromLists
                 [ [0,0], [0.1,0], [0,0.1]    -- cluster A
                 , [5,5], [5.1,5], [5,5.1]    -- cluster B
                 ]
    it "fitHierarchical Ward: 6 サンプルで 5 マージ" $ do
      let fit = HC.fitHierarchical HC.Ward blob
      length (HC.hcMerges fit) `shouldBe` 5
      length (HC.hcHeights fit) `shouldBe` 5
    it "cutTree K=2 で 2 ブロブが分離" $ do
      let fit  = HC.fitHierarchical HC.Ward blob
          ids  = HC.cutTree fit 2
          half = V.length (V.filter (== V.head ids) ids)
      half `shouldBe` 3
    it "Single linkage でも 5 マージ系列" $ do
      let fit = HC.fitHierarchical HC.Single blob
      length (HC.hcMerges fit) `shouldBe` 5
    it "Complete linkage 高さは Single 以上" $ do
      let fS = HC.fitHierarchical HC.Single blob
          fC = HC.fitHierarchical HC.Complete blob
      last (HC.hcHeights fC) `shouldSatisfy` (>= last (HC.hcHeights fS) - 1e-9)
