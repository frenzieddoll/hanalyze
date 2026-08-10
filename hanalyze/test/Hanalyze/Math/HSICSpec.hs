{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Math.HSICSpec (spec) where

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
import qualified Hanalyze.Stat.QuasiRandom  as QR
import qualified Hanalyze.Math.HSIC                   as HSIC
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Math.HSIC" $ do
    it "完全従属 (X = Y) で独立データより HSIC が大きい" $ do
      let halton b k = QR.radicalInverse b k - 0.5
          xs = LA.asColumn (LA.fromList [halton 2 i | i <- [1..200]])
          ys = LA.asColumn (LA.fromList [halton 7 i | i <- [1..200]])
          dep = HSIC.hsicBiased xs xs    -- 同データ = 完全従属
          ind = HSIC.hsicBiased xs ys    -- 異独立データ
      dep `shouldSatisfy` (> ind)
      ind `shouldSatisfy` (< 0.05)

    it "medianBandwidth は正値" $ do
      let xs = LA.asColumn (LA.fromList [QR.radicalInverse 2 i | i <- [1..50]])
      HSIC.medianBandwidth xs `shouldSatisfy` (> 0)
