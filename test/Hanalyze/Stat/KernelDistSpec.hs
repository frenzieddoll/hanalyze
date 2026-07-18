{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Stat.KernelDistSpec (spec) where

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
import qualified Hanalyze.Stat.KernelDist   as KD
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Stat.KernelDist" $ do
    let xs = LA.fromLists [[0, 0], [3, 4], [1, 1]] :: LA.Matrix Double
        d  = KD.pairwiseSqDist xs
        -- naive reference
        naive m =
          let rows = LA.toRows m
              n    = length rows
          in (n LA.>< n)
               [ let r = rows !! i - rows !! j in r `LA.dot` r
               | i <- [0 .. n - 1], j <- [0 .. n - 1] ]
        ref = naive xs

    it "returns an n x n matrix with zero diagonal" $ do
      LA.rows d `shouldBe` 3
      LA.cols d `shouldBe` 3
      LA.toList (LA.takeDiag d) `shouldBe` [0, 0, 0]

    it "matches the naive reference within 1e-9" $
      LA.norm_Inf (d - ref) < 1e-9 `shouldBe` True

    it "pairwiseSqDistXY matches reference for cross-matrix" $ do
      let ys  = LA.fromLists [[0, 0], [1, 0]] :: LA.Matrix Double
          dXY = KD.pairwiseSqDistXY xs ys
          rxs = LA.toRows xs
          rys = LA.toRows ys
          ref' = (LA.rows xs LA.>< LA.rows ys)
                   [ let r = rxs !! i - rys !! j in r `LA.dot` r
                   | i <- [0 .. LA.rows xs - 1]
                   , j <- [0 .. LA.rows ys - 1] ]
      LA.norm_Inf (dXY - ref') < 1e-9 `shouldBe` True
