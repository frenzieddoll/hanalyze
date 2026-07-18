{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Model.Formula.ContrastSpec (spec) where

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
import qualified Data.Text   as T
import qualified Numeric.LinearAlgebra as LA
import qualified DataFrame.Internal.Column    as DX
import qualified DataFrame.Internal.DataFrame  as DX
import qualified Hanalyze.Model.Core        as Core
import qualified Hanalyze.MCMC.Core as Core
import SpecHelper

spec :: Spec
spec = do
  describe "Contrast coding (Phase 47 A2)" $ do
    let frm s = case parseFormula s of Right f -> f; Left e -> error e
        gs = ["A","A","A","B","B","B","C","C","C"] :: [T.Text]
        xs = [1,2,3, 1,2,3, 1,2,3] :: [Double]
        ys = [10,11,12, 20,22,24, 5,6,7] :: [Double]
        df = DX.fromNamedColumns
               [ ("y", DX.fromList ys), ("g", DX.fromList gs), ("x", DX.fromList xs) ]
        fitOf s = case fitLMF (frm s) df of Right (fr, _) -> fr; Left e -> error e
        yhat    = Core.fittedList
        close a b = and (zipWith (\p q -> abs (p - q) < 1e-9) a b)

    describe "parameterization 不変 (ŷ/R² は contrast 非依存 = Python 非依存オラクル)" $ do
      let trt = fitOf "y g = b0 + bg ! g"
          sm  = fitOf "y g = b0 + bg ! C(g, Sum)"
          hel = fitOf "y g = b0 + bg ! C(g, Helmert)"
          pol = fitOf "y g = b0 + bg ! C(g, Poly)"
      it "Treatment と Sum で ŷ 一致" $ close (yhat trt) (yhat sm) `shouldBe` True
      it "Treatment と Helmert で ŷ 一致" $ close (yhat trt) (yhat hel) `shouldBe` True
      it "Treatment と Polynomial で ŷ 一致" $ close (yhat trt) (yhat pol) `shouldBe` True
      it "Treatment と Sum で R² 一致" $
        abs (Core.rSquared1 trt - Core.rSquared1 sm) < 1e-9 `shouldBe` True
      it "ŷ = 群平均 (主効果のみ)" $ do
        let mean zs = sum zs / fromIntegral (length zs)
            gm = [ mean [v | (g', v) <- zip gs ys, g' == g] | g <- gs ]
        close (yhat trt) gm `shouldBe` True

    describe "列数 / 識別性" $ do
      let dmOf s = case modelFrame (frm s) df >>= designMatrixF (frm s) of
                     Right (x, _) -> x; Left e -> error e
      it "切片あり主効果 (Sum) = 切片1 + contrast(k-1)=2 = 3 列" $
        LA.cols (dmOf "y g = b0 + bg ! C(g, Sum)") `shouldBe` 3
      it "Sum は満ランク" $
        LA.rank (dmOf "y g = b0 + bg ! C(g, Sum)") `shouldBe` 3

    describe "contrastMatrix (構造)" $ do
      it "Treatment k=3: 参照行 (水準0) = [0,0]" $
        LA.toLists (contrastMatrix Treatment 3) `shouldBe` [[0,0],[1,0],[0,1]]
      it "Sum k=3: 最終行 = [-1,-1] (sum-to-zero)" $
        LA.toLists (contrastMatrix Sum 3) `shouldBe` [[1,0],[0,1],[-1,-1]]
      it "Polynomial k=3: 列直交 (QᵀQ = I)" $ do
        let m = contrastMatrix Polynomial 3
            g = LA.tr m LA.<> m
        and [ abs (g `LA.atIndex` (i, j) - (if i == j then 1 else 0)) < 1e-9
            | i <- [0, 1], j <- [0, 1] ] `shouldBe` True

    describe "factor×連続 (masked 列は full coding ゆえ contrast 非依存・ŷ 不変)" $ do
      let trt = fitOf "y g x = b0 + bg ! g + bx ! g * x"
          sm  = fitOf "y g x = b0 + bg ! C(g, Sum) + bx ! C(g, Sum) * x"
      it "Treatment と Sum で ŷ 一致" $ close (yhat trt) (yhat sm) `shouldBe` True

    describe "R front-end C(g, Sum) は正本構文と等価" $ do
      let nat  = fitOf "y g = b0 + bg ! C(g, Sum)"
          rfit = case parseModel "y ~ C(g, Sum)" >>= \f -> fitLMF f df of
                   Right (fr, _) -> fr; Left e -> error e
      it "ŷ 一致" $ close (yhat nat) (yhat rfit) `shouldBe` True

  -- ----------------------------------------------------------------------------
  -- A3 (Phase 47) weights / offset = WLS
  -- ----------------------------------------------------------------------------
