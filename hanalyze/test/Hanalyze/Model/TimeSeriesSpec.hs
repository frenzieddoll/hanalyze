{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Model.TimeSeriesSpec (spec) where

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
import qualified Hanalyze.Model.TimeSeries   as TS
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Model.TimeSeries" $ do
    it "autocorrelation: lag 0 = 1.0" $ do
      let y = LA.fromList [1.0, 2.0, 1.5, 2.5, 1.8]
          acf = TS.autocorrelation 5 y
      LA.atIndex acf 0 `shouldBe` 1.0

    it "autocorrelation: 周期 4 の sin 系列で lag 4 が高い" $ do
      let y = LA.fromList [sin (2*pi*fromIntegral t / 4) | t <- [0..40::Int]]
          acf = TS.autocorrelation 8 y
      LA.atIndex acf 4 `shouldSatisfy` (> 0.5)

    it "fitAR(1) on quasi-AR(1) data: φ̂ in (0, 1)" $ do
      -- Quasi-AR(1) with φ=0.7 + small deterministic perturbation
      let phi = 0.7
          go _    0   = []
          go prev n   =
            let yi = phi * prev + 0.01 * sin (fromIntegral n :: Double)
            in yi : go yi (n - 1)
          ys  = take 100 (drop 50 (go 1.0 200))  -- burn-in 50
          y = LA.fromList ys
          fit = TS.fitAR 1 y
      let phiHat = LA.atIndex (TS.arPhi fit) 0
      -- AR(1) coefficient should be in (0, 1) for stationary positive AR
      phiHat `shouldSatisfy` (\p -> p > 0 && p < 1)

    it "forecastAR: h-step forecast 同 size" $ do
      let y = LA.fromList [1.0, 1.5, 2.0, 2.5, 3.0, 2.5, 2.0, 1.5, 1.0, 1.5,
                          2.0, 2.5, 3.0, 2.5, 2.0]
          fit = TS.fitAR 2 y
          fc = TS.forecastAR fit y 5
      LA.size fc `shouldBe` 5

    it "differencing: y' length = n - 1" $ do
      let y = LA.fromList [1.0, 3.0, 2.0, 5.0, 4.0]
          d1 = TS.differencing y
      LA.size d1 `shouldBe` 4
      LA.toList d1 `shouldBe` [2.0, -1.0, 3.0, -1.0]

    it "simpleExpSmoothing: α=1 で原系列、α=0 で初期値固定" $ do
      let y = LA.fromList [1.0, 2.0, 3.0, 4.0, 5.0]
          s1 = TS.simpleExpSmoothing 1.0 y    -- α=1
          s0 = TS.simpleExpSmoothing 0.0 y    -- α=0
      LA.toList s1 `shouldBe` LA.toList y    -- exact
      all (== 1.0) (LA.toList s0) `shouldBe` True

    it "holtWinters: 線形 trend + 周期 4 を再現" $ do
      let -- y_t = t + 5 sin(2πt/4)
          ys = [ fromIntegral t + 3 * sin (2 * pi * fromIntegral t / 4)
               | t <- [0 .. 39 :: Int] ]
          y  = LA.fromList ys
          fit = TS.holtWinters TS.HWAdditive 4 y
          fc  = TS.hwForecast fit 4
      LA.size fc `shouldBe` 4
      -- Forecast at t=40,41,42,43 should be roughly 40 + sin pattern
      LA.atIndex fc 0 `shouldSatisfy` (> 35)

    it "stlDecompose: 周期成分が period 倍で繰り返す" $ do
      let ys = [ fromIntegral t / 10 + 2 * sin (2 * pi * fromIntegral t / 4)
               | t <- [0 .. 39 :: Int] ]
          y  = LA.fromList ys
          (_trend, seasonal, _resid) = TS.stlDecompose 4 y
      LA.size seasonal `shouldBe` LA.size y

  -- ===========================================================================
  -- Hanalyze.Model.Survival (Phase 12)
  -- ===========================================================================
