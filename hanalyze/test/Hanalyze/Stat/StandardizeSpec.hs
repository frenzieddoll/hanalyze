{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Stat.StandardizeSpec (spec) where

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
import qualified Hanalyze.Stat.Standardize  as Std
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Stat.Standardize" $ do
    let xMat = LA.fromLists [[1, 100], [2, 200], [3, 300], [4, 400], [5, 500]] :: LA.Matrix Double
        s    = Std.fitStandardizer xMat
    it "fit: 各列の μ が一致" $
      Std.stMu s `shouldSatisfy`
        (\ms -> length ms == 2
              && abs (ms !! 0 - 3) < 1e-9
              && abs (ms !! 1 - 300) < 1e-9)
    it "fit: 各列の σ が不偏分散の平方根 (n-1 正規化)" $
      Std.stSd s `shouldSatisfy`
        (\ss -> length ss == 2
              && abs (ss !! 0 - sqrt 2.5) < 1e-9
              && abs (ss !! 1 - sqrt 25000) < 1e-9)
    it "apply 後は各列 mean≈0, std≈1" $ do
      let x' = Std.applyStandardizer s xMat
          c0 = LA.toColumns x' !! 0
          c1 = LA.toColumns x' !! 1
          mn v = LA.sumElements v / fromIntegral (LA.size v)
      abs (mn c0) `shouldSatisfy` (< 1e-9)
      abs (mn c1) `shouldSatisfy` (< 1e-9)
    it "unapply で元の値に戻る" $ do
      let x'  = Std.applyStandardizer s xMat
          x'' = Std.unapplyStandardizer s x'
          d   = LA.norm_2 (xMat - x'') :: Double
      d `shouldSatisfy` (< 1e-9)
    it "定数列 (std=0) は std=1 にフォールバック (中央化のみ)" $ do
      let constMat = LA.fromLists [[7, 1], [7, 2], [7, 3]] :: LA.Matrix Double
          s2      = Std.fitStandardizer constMat
      abs (Std.stSd s2 !! 0 - 1.0) `shouldSatisfy` (< 1e-12)
      let x' = Std.applyStandardizer s2 constMat
          c0 = LA.toColumns x' !! 0
      abs (LA.sumElements c0) `shouldSatisfy` (< 1e-9)
