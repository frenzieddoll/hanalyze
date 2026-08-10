{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Model.LM.DiagnosticsSpec (spec) where

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
import qualified Hanalyze.Model.LM             as LM
import qualified Hanalyze.Model.LM.Diagnostics as LMD
import SpecHelper

spec :: Spec
spec = do
  -- Phase 82: 飽和 fit (df = n−p <= 0) で CI/PI 帯が Student-t 例外を出さず、
  -- 幅ゼロ帯 (lo=hi=ŷ) に潰れること。 profiler が飽和モデルで落ちないための保証。
  describe "LM CI/PI band: 飽和 (df<=0) ガード (Phase 82)" $ do
    let xSat = LA.fromColumns [ LA.konst 1.0 2, LA.fromList [1.0, 2.0] ]  -- 2×2 = 2 param
        ySat = LA.fromList [3.0, 5.0]
        fitS = LM.fitLMVec xSat ySat                                     -- df = 2 − 2 = 0
    it "confidenceBandAt: df=0 は例外なし・幅ゼロ (lo=hi)" $ do
      let b = LM.confidenceBandAt xSat fitS 0.95 xSat
      LM.lowerBound b `shouldBe` LM.upperBound b
    it "predictionBandAt: df=0 は例外なし・幅ゼロ (lo=hi)" $ do
      let b = LM.predictionBandAt xSat fitS 0.95 xSat
      LM.lowerBound b `shouldBe` LM.upperBound b
    it "df>0 では通常どおり幅を持つ (回帰・帯が線でない)" $ do
      let x3 = LA.fromColumns [ LA.konst 1.0 3, LA.fromList [1.0, 2.0, 3.0] ]
          y3 = LA.fromList [3.0, 5.2, 6.9]
          b  = LM.confidenceBandAt x3 (LM.fitLMVec x3 y3) 0.95 x3
      LM.lowerBound b `shouldNotBe` LM.upperBound b

  describe "Hanalyze.Model.LM.Diagnostics (vs statsmodels OLS)" $ do
    let xRaw = [1.0, 2, 3, 4, 5, 6, 7, 8, 9, 10]
        yRaw = [3.7, 5.2, 6.8, 8.5, 9.9, 11.3, 13.1, 14.4, 15.8, 17.2]
        x    = LA.fromColumns
                 [ LA.konst 1.0 (length xRaw)
                 , LA.fromList xRaw
                 ]
        y    = LA.fromList yRaw
        fit  = LM.fitLMVec x y

        approx tol a b = abs (a - b) < tol
        approxV tol expected actual =
          length expected == LA.size actual &&
          and (zipWith (approx tol) expected (LA.toList actual))

    it "ciTValue: 95% / df=8 ≈ 2.306" $
      LMD.ciTValue 0.95 8 `shouldSatisfy` approx 1e-3 2.306

    it "lmStdErrors matches statsmodels (intercept, slope)" $
      LMD.lmStdErrors x fit `shouldSatisfy`
        approxV 1e-5 [0.09602188, 0.01547533]

    it "lmCoefStats t / p match statsmodels" $ do
      let cs = LMD.lmCoefStats x fit
      length cs `shouldBe` 2
      LMD.csTValue (head cs)    `shouldSatisfy` approx 1e-3 23.8834
      LMD.csTValue (cs !! 1)    `shouldSatisfy` approx 1e-3 97.4768
      LMD.csPValue (head cs)    `shouldSatisfy` approx 1e-9 1.0062e-8
      LMD.csPValue (cs !! 1)    `shouldSatisfy` approx 1e-13 1.3699e-13

    it "lmFStatistic matches statsmodels (F, p, df1, df2)" $ do
      let fs = head (LMD.lmFStatistic x fit)
      LMD.fsValue fs  `shouldSatisfy` approx 1e-1 9501.7193
      LMD.fsPValue fs `shouldSatisfy` approx 1e-13 1.3699e-13
      LMD.fsDf1 fs `shouldBe` 1
      LMD.fsDf2 fs `shouldBe` 8

    it "lmInformationCriteria (R lm() convention, k = p + 1 with σ)" $ do
      let ic = LMD.lmInformationCriteria fit
      LMD.icLogLik ic `shouldSatisfy` approx 1e-5 6.547424
      LMD.icAIC    ic `shouldSatisfy` approx 1e-5 (-7.094848)
      LMD.icBIC    ic `shouldSatisfy` approx 1e-5 (-6.187093)

    it "hatDiagonal matches statsmodels leverage" $
      LMD.hatDiagonal x `shouldSatisfy`
        approxV 1e-6
          [ 0.34545455, 0.24848485, 0.17575758, 0.12727273, 0.10303030
          , 0.10303030, 0.12727273, 0.17575758, 0.24848485, 0.34545455 ]

    it "standardizedResiduals match statsmodels (resid_studentized_internal)" $
      LMD.standardizedResiduals x fit `shouldSatisfy`
        approxV 1e-5
          [ -0.89534127, -0.90521501, -0.14722564,  1.31539084,  0.48257654
          , -0.33234045,  1.88308584,  0.30394970, -0.57197652, -1.56684722 ]

    it "cooksDistance matches statsmodels" $
      LMD.cooksDistance x fit `shouldSatisfy`
        approxV 1e-5
          [ 0.21154283, 0.13546767, 0.00231098, 0.12616429, 0.01337487
          , 0.00634342, 0.25856339, 0.00984992, 0.05408646, 0.64784992 ]

    it "predictorStdDevs: intercept col=0, x col≈3.0277" $ do
      let sds = LMD.predictorStdDevs x
      LA.size sds `shouldBe` 2
      (LA.toList sds !! 0) `shouldSatisfy` approx 1e-12 0
      (LA.toList sds !! 1) `shouldSatisfy` approx 1e-6 3.027650

  -- ─────────────────────────────────────────────────────────────────────
