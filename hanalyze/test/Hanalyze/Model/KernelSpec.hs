{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Model.KernelSpec (spec) where

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
import qualified Hanalyze.Model.KernelRegression      as Kn
import qualified Hanalyze.Model.GP          as GP
import qualified Hanalyze.Model.GPRobust    as GPR
import qualified Hanalyze.Model.GP        as GP
import qualified Hanalyze.Model.GPRobust  as GPR
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Model.KernelRegression multi-input (MV)" $ do
    -- 2D regression target: y = sin(x1) + 0.5 cos(x2)
    let n     = 60
        h     = 0.5
        lam   = 1e-4
        f x1 x2 = sin x1 + 0.5 * cos x2
        xs    = LA.fromLists
                  [ [ fromIntegral i / 10
                    , fromIntegral (n - i) / 10
                    ]
                  | i <- [0 .. n - 1] ]
        ys    = LA.asColumn $ LA.fromList
                  [ f (xs `LA.atIndex` (i, 0)) (xs `LA.atIndex` (i, 1))
                  | i <- [0 .. n - 1] ]
        fit   = Kn.kernelRidgeMV Kn.Gaussian h lam xs ys
        yhat  = Kn.fittedKernelRidgeMV fit
        ssErr = LA.sumElements ((ys - yhat) ** 2)
        ssTot = let muY = LA.sumElements ys / fromIntegral n
                in LA.sumElements ((ys - LA.konst muY (n, 1)) ** 2)
        r2    = 1 - ssErr / ssTot

    it "achieves R² > 0.95 on a 2D smooth target" $
      r2 > 0.95 `shouldBe` True

    it "predict at training points equals fitted" $ do
      let p = Kn.predictKernelRidgeMV fit xs
      LA.norm_Inf (p - yhat) < 1e-9 `shouldBe` True

    it "gramMatrixMV matches kernelFromSqDist by element" $ do
      let xS  = LA.fromLists [[0.0, 0.0], [1.0, 0.0], [0.5, 0.5]] :: LA.Matrix Double
          gMV = Kn.gramMatrixMV Kn.Gaussian 1.0 xS
          rs  = LA.toRows xS
          ref = LA.fromLists
                  [ [ let d = rs !! i - rs !! j
                          s = (d `LA.dot` d) / (1.0 * 1.0)
                      in Kn.kernelFromSqDist Kn.Gaussian s
                    | j <- [0 .. 2] ]
                  | i <- [0 .. 2] ]
      LA.norm_Inf (gMV - ref) < 1e-12 `shouldBe` True

    it "Hanalyze.Model.GP MV: 1D input matches legacy 1D fitGP within 1e-6" $ do
      let xL  = [fromIntegral i / 5 | i <- [0 .. 19 :: Int]]
          yL  = map sin xL
          tL  = [0.5, 1.5, 2.5]
          mdl = GP.GPModel GP.RBF (GP.GPParams 1.0 1.0 0.05 1.0 Nothing)
          legacy = GP.fitGP mdl xL yL tL
          xMV = LA.fromLists (map (:[]) xL) :: LA.Matrix Double
          yMV = LA.fromList yL
          tMV = LA.fromLists (map (:[]) tL) :: LA.Matrix Double
          mv  = GP.fitGPMV mdl xMV yMV tMV
          dMu = LA.norm_Inf
                  (GP.gpmvMean mv - LA.fromList (GP.gpMean legacy))
          dVr = LA.norm_Inf
                  (GP.gpmvVar  mv - LA.fromList (GP.gpVar  legacy))
      (dMu < 1e-6) `shouldBe` True
      (dVr < 1e-6) `shouldBe` True

    it "Hanalyze.Model.GP MV: 2D RBF reaches R² > 0.95 with optimized HP" $ do
      let nN  = 50
          gx  = [(fromIntegral i / 10, fromIntegral (nN - i) / 10)
                | i <- [0 .. nN - 1 :: Int]]
          ftn (x1, x2) = sin x1 + 0.5 * cos x2
          xMV = LA.fromLists [ [a, b] | (a, b) <- gx ] :: LA.Matrix Double
          yMV = LA.fromList [ ftn p | p <- gx ]
          p0  = GP.GPParams 1.0 1.0 0.01 1.0 Nothing
          po  = GP.optimizeGPMV GP.RBF xMV yMV p0
          mdl = GP.GPModel GP.RBF po
          res = GP.fitGPMV mdl xMV yMV xMV
          mu  = GP.gpmvMean res
          y   = yMV
          ss  = LA.sumElements ((y - mu) ** 2)
          mY  = LA.sumElements y / fromIntegral nN
          st  = LA.sumElements ((y - LA.konst mY nN) ** 2)
          r2  = 1 - ss / st
      (r2 > 0.95) `shouldBe` True

    it "Hanalyze.Model.GPRobust MV: 1D input matches legacy fitGPRobust" $ do
      let xL  = [fromIntegral i / 5 | i <- [0 .. 14 :: Int]]
          yL  = map sin xL
          tL  = [0.5, 1.5, 2.5]
          ker = GP.RBF
          ps  = GP.GPParams 1.0 1.0 0.05 1.0 Nothing
          lik = GPR.RGaussian 0.1
          legFit = GPR.fitGPRobust ker ps lik xL yL
          legacy = GPR.predictGPRobust legFit tL
          legM   = LA.fromList (map fst legacy)
          xMV    = LA.fromLists (map (:[]) xL) :: LA.Matrix Double
          yMV    = LA.fromList yL
          tMV    = LA.fromLists (map (:[]) tL) :: LA.Matrix Double
          mvFit  = GPR.fitGPRobustMV ker ps lik xMV yMV
          (mvM, _) = GPR.predictGPRobustMV mvFit tMV
      LA.norm_Inf (mvM - legM) < 1e-6 `shouldBe` True

    it "MV gramMatrix on a single-column input agrees with kernelFromSqDist" $ do
      let xs1 = LA.fromLists [[fromIntegral i / 5] | i <- [0 .. 19 :: Int]]
                  :: LA.Matrix Double
          gMV2 = Kn.gramMatrixMV Kn.Gaussian 0.4 xs1
          ref  = LA.fromLists
                   [ [ let xi = xs1 `LA.atIndex` (i, 0)
                           xj = xs1 `LA.atIndex` (j, 0)
                           d  = xi - xj
                       in Kn.kernelFromSqDist Kn.Gaussian (d * d / (0.4 * 0.4))
                     | j <- [0 .. 19] ]
                   | i <- [0 .. 19] ]
      LA.norm_Inf (gMV2 - ref) < 1e-12 `shouldBe` True
