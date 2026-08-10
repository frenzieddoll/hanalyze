{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Model.Formula.WLSSpec (spec) where

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
import qualified DataFrame.Internal.Column    as DX
import qualified DataFrame.Internal.DataFrame  as DX
import qualified Hanalyze.Model.Core        as Core
import qualified Hanalyze.MCMC.Core as Core
import SpecHelper

spec :: Spec
spec = do
  describe "weights / offset = WLS (Phase 47 A3)" $ do
    let frm s = case parseFormula s of Right f -> f; Left e -> error e
        xs = [1,2,3,4,5,6,7,8] :: [Double]
        ys = [2.1,3.9,6.2,7.8,10.1,12.2,13.8,16.1] :: [Double]
        ws = [1,1,2,2,3,3,4,4] :: [Double]
        df = DX.fromNamedColumns
               [ ("y", DX.fromList ys), ("x", DX.fromList xs), ("w", DX.fromList ws) ]
        f  = frm "y x = b0 + b1*x"
        coefOf = LA.toList . Core.coefficientsV
        close a b = and (zipWith (\p q -> abs (p - q) < 1e-8) a b)

    it "w≡1 (重みなし) は OLS = fitLMF と一致" $ do
      let Right (a, _) = fitWLSF defaultWLS f df
          Right (b, _) = fitLMF f df
      close (coefOf a) (coefOf b) `shouldBe` True

    it "WLS 係数 = 閉形式 (XᵀWX)⁻¹XᵀWy (hmatrix 直計算オラクル)" $ do
      let Right (r, _) = fitWLSF defaultWLS { wcWeights = Just "w" } f df
          x    = LA.fromLists [ [1, xi] | xi <- xs ]
          w    = LA.diag (LA.fromList ws)
          y    = LA.asColumn (LA.fromList ys)
          beta = LA.flatten (LA.inv (LA.tr x LA.<> w LA.<> x) LA.<> LA.tr x LA.<> w LA.<> y)
      close (coefOf r) (LA.toList beta) `shouldBe` True

    it "offset: y を offset z 付きで fit = (y−z) を素の fit と一致" $ do
      let zs  = [0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0] :: [Double]
          dfO = DX.fromNamedColumns
                  [ ("y", DX.fromList ys), ("x", DX.fromList xs), ("z", DX.fromList zs) ]
          dfM = DX.fromNamedColumns
                  [ ("y", DX.fromList (zipWith (-) ys zs)), ("x", DX.fromList xs) ]
          Right (ro, _) = fitWLSF defaultWLS { wcOffset = Just "z" } f dfO
          Right (rm, _) = fitLMF f dfM
      close (coefOf ro) (coefOf rm) `shouldBe` True

  -- ----------------------------------------------------------------------------
  -- A4 (Phase 47) 非線形フィット NLS
  -- ----------------------------------------------------------------------------
