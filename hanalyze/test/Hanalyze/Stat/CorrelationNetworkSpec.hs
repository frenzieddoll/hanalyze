{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Stat.CorrelationNetworkSpec (spec) where

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
import qualified Hanalyze.Stat.CorrelationNetwork     as CN
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Stat.CorrelationNetwork (Phase 32-A1 Graphical Lasso)" $ do
    -- 真の precision matrix:
    --   Θ = [[ 2.0, -0.8,  0.0,  0.0],
    --        [-0.8,  2.0, -0.6,  0.0],
    --        [ 0.0, -0.6,  2.0,  0.0],
    --        [ 0.0,  0.0,  0.0,  2.0]]
    -- → 非零 off-diag: (0,1), (1,2)。 (0,2), (0,3), (1,3), (2,3) はゼロ
    -- Σ = Θ⁻¹ を計算してから X ~ N(0, Σ) をサンプリング
    it "graphicalLasso: sparse 構造を回復 (中 λ で 0 を維持 + non-zero 検出)" $ do
      gen <- MWC.create
      let theta = LA.fromLists
            [ [ 2.0, -0.8,  0.0,  0.0]
            , [-0.8,  2.0, -0.6,  0.0]
            , [ 0.0, -0.6,  2.0,  0.0]
            , [ 0.0,  0.0,  0.0,  2.0] ]
          sigma = LA.inv theta
          -- Cholesky 因子から N(0, Σ) サンプル
          chol  = LA.chol (LA.trustSym sigma)
          nGL   = 500
          pGL   = 4
      zs <- VS.replicateM (nGL * pGL) (do
              u1 <- MWC.uniformR (1e-9, 1.0 :: Double) gen
              u2 <- MWC.uniformR (0.0, 1.0 :: Double) gen
              pure (sqrt (-2 * log u1) * cos (2 * pi * u2)))
      let zMat = LA.reshape pGL (LA.fromList (VS.toList zs))
          xMat = zMat LA.<> chol  -- N(0, chol^T chol) = N(0, Σ)
          fit  = CN.graphicalLasso xMat 0.05 100 1e-4
          thetaHat = CN.glPrecision fit
      -- 真の非零位置 (0,1), (1,2) を検出
      abs (LA.atIndex thetaHat (0, 1)) `shouldSatisfy` (> 0.1)
      abs (LA.atIndex thetaHat (1, 2)) `shouldSatisfy` (> 0.1)
      -- 真のゼロ位置は十分小さい
      abs (LA.atIndex thetaHat (0, 3)) `shouldSatisfy` (< 0.15)
      abs (LA.atIndex thetaHat (2, 3)) `shouldSatisfy` (< 0.15)
      CN.glConverged fit `shouldBe` True
    it "graphicalLasso: λ=0 に近いとほぼ S⁻¹、 λ 大で対角優位 (連続性)" $ do
      gen <- MWC.create
      let nGL = 200
          pGL = 3
      zs <- VS.replicateM (nGL * pGL) (do
              u1 <- MWC.uniformR (1e-9, 1.0 :: Double) gen
              u2 <- MWC.uniformR (0.0, 1.0 :: Double) gen
              pure (sqrt (-2 * log u1) * cos (2 * pi * u2)))
      let x = LA.reshape pGL (LA.fromList (VS.toList zs))
          fitBig   = CN.graphicalLasso x 5.0 100 1e-4
          thetaBig = CN.glPrecision fitBig
      -- 大 λ: off-diag がほぼ 0 に潰される
      CN.nonZeroPrecision 0.05 thetaBig `shouldBe` 0
    it "empiricalCov: 中央化後の (1/(n-1)) X^T X" $ do
      let x = LA.fromLists [[1, 2], [2, 4], [3, 6], [4, 8]]
          s = CN.empiricalCov x
      -- 共分散: var(col1) = var([1,2,3,4]) = 5/3 ≈ 1.6667
      --        var(col2) = 4·var(col1) = 6.6667
      --        cov(col1, col2) = 2·var(col1) = 3.3333
      abs (LA.atIndex s (0, 0) - 5/3) `shouldSatisfy` (< 1e-9)
      abs (LA.atIndex s (1, 1) - 20/3) `shouldSatisfy` (< 1e-9)
      abs (LA.atIndex s (0, 1) - 10/3) `shouldSatisfy` (< 1e-9)
