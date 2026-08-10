{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Stat.InterpolateSpec (spec) where

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
import qualified Hanalyze.Stat.Interpolate  as Interp
import qualified Hanalyze.Stat.Interpret       as Interp
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Stat.Interpolate" $ do
    let pts = [(0,0), (1,1), (2,4), (3,9), (4,16)]   -- y = x^2
    it "Linear: 観測点で原値を厳密に再現" $ do
      let f = Interp.interp1d Interp.Linear pts
      mapM_ (\(x, y) -> abs (f x - y) `shouldSatisfy` (< 1e-12)) pts

    it "NaturalSpline: 観測点で原値を厳密に再現、中間点で線形より精度高い" $ do
      let fl = Interp.interp1d Interp.Linear pts
          fs = Interp.interp1d Interp.NaturalSpline pts
          true x = x * x
          xMid = 1.5
          errL = abs (fl xMid - true xMid)
          errS = abs (fs xMid - true xMid)
      mapM_ (\(x, y) -> abs (fs x - y) `shouldSatisfy` (< 1e-9)) pts
      errS `shouldSatisfy` (< errL)

    it "PCHIP: 単調データ ([0,1,2,4,8,16]) で出力も単調" $ do
      let mp = [(0,0), (1,1), (2,2), (3,4), (4,8), (5,16)]
          fp = Interp.interp1d Interp.PCHIP mp
          ys = map fp [0, 0.1 .. 5]
      and (zipWith (<=) ys (tail ys)) `shouldBe` True

    it "PCHIP: 観測点で原値を厳密に再現" $ do
      let fp = Interp.interp1d Interp.PCHIP pts
      mapM_ (\(x, y) -> abs (fp x - y) `shouldSatisfy` (< 1e-9)) pts
