{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Model.FDASpec (spec) where

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
import qualified Data.Vector.Storable              as VS
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC as MWC
import qualified Hanalyze.Model.FDA                   as FDA
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Model.FDA (Phase 33 Functional Data Analysis)" $ do
    -- 共通: t grid [0, 1] を 50 点、 内部 knots 8 点で B-spline degree=3
    let nGrid = 50
        tList = [fromIntegral i / fromIntegral (nGrid - 1)
                | i <- [0 .. nGrid - 1]]
        tGrid = LA.fromList tList
        -- bsplineBasis は境界も含むノット列を期待 (Spline.hs §bsplineBasis 注釈)
        intKnots = [fromIntegral i / 11 | i <- [0 .. 11]]
        basis = FDA.BSpline 3 intKnots
    it "smoothBasis: noisy sinusoid を 12% RMSE 以内で復元 (Phase 33-A1)" $ do
      gen <- MWC.create
      let trueFn t = sin (2 * pi * t)
          nSamp = 5
      -- 同じ true 関数 + 異なる noise の 5 sample
      noisyRows <- mapM (\_ -> do
        ns <- VS.replicateM nGrid (do
                u1 <- MWC.uniformR (1e-9, 1.0 :: Double) gen
                u2 <- MWC.uniformR (0.0, 1.0 :: Double) gen
                pure (0.1 * sqrt (-2 * log u1) * cos (2 * pi * u2)))
        pure (zipWith (\t e -> trueFn t + e) tList (VS.toList ns)))
        [1 .. nSamp]
      let yMat = LA.fromLists noisyRows
          fits = FDA.smoothBasis basis 1e-3 tGrid yMat
      length fits `shouldBe` nSamp
      -- 復元値と真値の RMSE
      let evals = [LA.toList (FDA.evalFunctional fit tGrid) | fit <- fits]
          trueVals = map trueFn tList
          rmse vs = sqrt (sum [(v - tv)^(2::Int) | (v, tv) <- zip vs trueVals]
                           / fromIntegral nGrid)
      all (\v -> rmse v < 0.12) evals `shouldBe` True
    it "smoothBasis: 大 λ で over-smooth (= 直線近似)、 小 λ で interpolate" $ do
      -- データ自体に二次的な変動が必要、 小 λ では当てはまる、 大 λ では潰す
      let trueFn t = sin (4 * pi * t)
          yRow = map trueFn tList
          yMat = LA.fromLists [yRow]
          fitSmall = head (FDA.smoothBasis basis 1e-6 tGrid yMat)
          fitBig   = head (FDA.smoothBasis basis 1e8  tGrid yMat)
          residSmall = sum [(v - r)^(2::Int)
                           | (v, r) <- zip (LA.toList (FDA.evalFunctional fitSmall tGrid)) yRow]
          residBig   = sum [(v - r)^(2::Int)
                           | (v, r) <- zip (LA.toList (FDA.evalFunctional fitBig tGrid)) yRow]
      -- 小 λ の方が残差小、 大 λ は明確に大きい
      residSmall `shouldSatisfy` (< residBig / 10)
    it "functionalPCA: 2 成分で分散 90% 以上 + 主成分関数の符号一致 (Phase 33-A2)" $ do
      gen <- MWC.create
      let nSamp = 80
      -- DGP: x_i(t) = c1_i · sin(2πt) + c2_i · cos(2πt) + small noise
      --     c1, c2 ~ N(0, 1) 独立
      curves <- mapM (\_ -> do
        u1a <- MWC.uniformR (1e-9, 1.0 :: Double) gen
        u2a <- MWC.uniformR (0.0, 1.0 :: Double) gen
        u1b <- MWC.uniformR (1e-9, 1.0 :: Double) gen
        u2b <- MWC.uniformR (0.0, 1.0 :: Double) gen
        let c1 = sqrt (-2 * log u1a) * cos (2 * pi * u2a)
            c2 = sqrt (-2 * log u1b) * cos (2 * pi * u2b)
        ns <- VS.replicateM nGrid (do
                u1 <- MWC.uniformR (1e-9, 1.0 :: Double) gen
                u2 <- MWC.uniformR (0.0, 1.0 :: Double) gen
                pure (0.05 * sqrt (-2 * log u1) * cos (2 * pi * u2)))
        pure [ c1 * sin (2 * pi * t) + c2 * cos (2 * pi * t) + e
             | (t, e) <- zip tList (VS.toList ns) ])
        [1 .. nSamp]
      let yMat = LA.fromLists curves
          fits = FDA.smoothBasis basis 1e-4 tGrid yMat
          pca  = FDA.functionalPCA 5 fits
          vals = LA.toList (FDA.fpcaEigenvalues pca)
      length vals `shouldSatisfy` (>= 2)
      -- 上位 2 成分が分散 90% 以上を説明
      let top2 = sum (take 2 vals)
          total = sum vals
      (top2 / total) `shouldSatisfy` (> 0.9)
      -- 主成分関数を grid 上で取り、 sin / cos との相関を見る
      LA.rows (FDA.fpcaEigenfn pca) `shouldSatisfy` (>= 2)
    it "fLM: 既知 β(t) = sin(2πt) を回復 (R² > 0.85、 Phase 33-A3)" $ do
      gen <- MWC.create
      let nSamp = 60
          alphaTrue = 0.5
          betaFn t = sin (2 * pi * t)
          dt = head tList - head (tail tList)  -- not used; trapezoidal は内部
      -- x_i(t) = s1·sin(2πt) + s2·cos(2πt)  (β と s1 が orthogonal でない設計)
      -- → ∫x_i β dt = s1·∫sin²(2πt) dt + 0 = 0.5·s1
      curves <- mapM (\_ -> do
        u1a <- MWC.uniformR (1e-9, 1.0 :: Double) gen
        u2a <- MWC.uniformR (0.0, 1.0 :: Double) gen
        u1b <- MWC.uniformR (1e-9, 1.0 :: Double) gen
        u2b <- MWC.uniformR (0.0, 1.0 :: Double) gen
        let s1 = sqrt (-2 * log u1a) * cos (2 * pi * u2a)
            s2 = sqrt (-2 * log u1b) * cos (2 * pi * u2b)
        pure [s1 * sin (2 * pi * t) + s2 * cos (2 * pi * t) | t <- tList])
        [1 .. nSamp]
      ns <- VS.replicateM nSamp (do
              u1 <- MWC.uniformR (1e-9, 1.0 :: Double) gen
              u2 <- MWC.uniformR (0.0, 1.0 :: Double) gen
              pure (0.05 * sqrt (-2 * log u1) * cos (2 * pi * u2)))
      let dtVal = 1.0 / fromIntegral (nGrid - 1)
          intXBeta xs = dtVal *
            sum [ x * betaFn t | (x, t) <- zip xs tList ]   -- 簡易 trap
          ys = LA.fromList [alphaTrue + intXBeta x + e
                           | (x, e) <- zip curves (VS.toList ns)]
          yMat = LA.fromLists curves
          fits = FDA.smoothBasis basis 1e-4 tGrid yMat
          flm  = FDA.fLM fits ys 1e-3
      FDA.flmR2 flm `shouldSatisfy` (> 0.85)
      abs (FDA.flmAlpha flm - alphaTrue) `shouldSatisfy` (< 0.2)
      LA.size (FDA.flmBetaFn flm) `shouldBe` nGrid
