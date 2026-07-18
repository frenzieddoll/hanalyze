{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Model.DAGSpec (spec) where

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
import qualified Hanalyze.Model.DAG                   as DAG
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Model.DAG (Phase 36-LiNGAM 共通基盤)" $ do
    it "fromBMatrix + topoSort: 3 ノード DAG の順序" $ do
      let b = LA.fromLists [[0, 0, 0]
                           ,[0.8, 0, 0]
                           ,[0.4, 0.6, 0]]
          g = DAG.fromBMatrix 0.05 b
      DAG.topoSort g `shouldBe` Just [0, 1, 2]
    it "isAcyclic: DAG は True、 cycle 入りなら False" $ do
      let acyc = DAG.fromBMatrix 0.05
                   (LA.fromLists [[0,0,0]
                                 ,[0.5,0,0]
                                 ,[0,0.5,0]])
          cyc  = DAG.fromBMatrix 0.05
                   (LA.fromLists [[0,0.5,0]
                                 ,[0,0,0.5]
                                 ,[0.5,0,0]])
      DAG.isAcyclic acyc `shouldBe` True
      DAG.isAcyclic cyc  `shouldBe` False
    it "dagEdges: エッジ数が非零要素数と一致" $ do
      let b = LA.fromLists [[0,0,0],[0.5,0,0],[0.3,0.4,0]]
          g = DAG.fromBMatrix 0.05 b
      length (DAG.dagEdges g) `shouldBe` 3
