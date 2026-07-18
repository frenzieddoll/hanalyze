{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Design.Custom.GoldenSpec (spec) where

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
import qualified Hanalyze.Stat.ClassMetrics as CM
import qualified Hanalyze.Design.Optimal       as OPT
import qualified Hanalyze.Design.Custom.Factor     as CF
import qualified Hanalyze.Design.Custom.Model      as CM
import qualified Hanalyze.Design.Custom.Constraint as CC
import qualified Hanalyze.Design.Custom.Coordinate as CX
import qualified Hanalyze.Design.Custom.Compare    as CCMP
import SpecHelper

spec :: Spec
spec = do
  describe "Custom Design ゴールデン例題 (Phase 24-9)" $ do
    -- Example 1: 2 因子 2nd-order RSM (full quadratic、 p_terms = 6)
    it "golden ex1: 2 factor + 2nd-order RSM、 D-eff > 0、 全 grid 内" $ do
      let f1g = CF.Factor "x1" (CF.Continuous (-1) 1) CF.Controllable
          f2g = CF.Factor "x2" (CF.Continuous (-1) 1) CF.Controllable
          modelRSM = CM.Model
            [ CM.TIntercept
            , CM.TMain "x1", CM.TMain "x2"
            , CM.TInter ["x1","x2"]
            , CM.TPower "x1" 2, CM.TPower "x2" 2
            ] CM.NCoded
          spec = CX.CustomDesignSpec
            { CX.cdsFactors     = [f1g, f2g]
            , CX.cdsModel       = modelRSM
            , CX.cdsConstraints = []
            , CX.cdsNRuns       = 12
            , CX.cdsCriterion   = OPT.DOpt
            , CX.cdsBudget      = CX.defaultBudget
            , CX.cdsSeed        = Just 42
            , CX.cdsInitial     = Nothing

            , CX.cdsDJConvention = False
            }
      r <- CX.coordinateExchange spec
      case r of
        Left e -> expectationFailure (T.unpack e)
        Right cd -> do
          LA.rows (CX.cdMatrix cd) `shouldBe` 12
          LA.cols (CX.cdMatrix cd) `shouldBe` 2
          let vs = concat (LA.toLists (CX.cdMatrix cd))
          all (\v -> v >= -1.0001 && v <= 1.0001) vs `shouldBe` True
          -- D 値 > 0 (非特異)
          let dval = - CX.crCriterionValue (CX.cdReport cd)
          dval `shouldSatisfy` (> 0)
          -- D-eff via Compare も 0 より大きい
          let dc = CCMP.compareDesigns [("rsm", cd)]
          (CCMP.dcEffTable dc `LA.atIndex` (0, 0)) `shouldSatisfy` (> 0.4)
    -- Example 2: 1 連続 + 1 categorical(3) + main + interaction
    it "golden ex2: 1 cont + 1 cat(3) + main+int model、 各 cat level 出現" $ do
      let fc = CF.Factor "x"  (CF.Continuous (-1) 1)            CF.Controllable
          fk = CF.Factor "k"  (CF.Categorical ["A","B","C"])    CF.Controllable
          model = CM.Model
            [ CM.TIntercept
            , CM.TMain "x", CM.TMain "k"
            , CM.TInter ["x","k"]
            ] CM.NCoded
          spec = CX.CustomDesignSpec
            { CX.cdsFactors     = [fc, fk]
            , CX.cdsModel       = model
            , CX.cdsConstraints = []
            , CX.cdsNRuns       = 12
            , CX.cdsCriterion   = OPT.DOpt
            , CX.cdsBudget      = CX.defaultBudget
            , CX.cdsSeed        = Just 100
            , CX.cdsInitial     = Nothing

            , CX.cdsDJConvention = False
            }
      r <- CX.coordinateExchange spec
      case r of
        Left e -> expectationFailure (T.unpack e)
        Right cd -> do
          LA.rows (CX.cdMatrix cd) `shouldBe` 12
          let kCol = LA.toList (LA.flatten (LA.subMatrix (0, 1) (12, 1) (CX.cdMatrix cd)))
              ks   = map (round :: Double -> Int) kCol
          all (`elem` [0, 1, 2]) ks `shouldBe` True
          -- D-opt なら全 3 level が出現するはず (= balanced 設計の必要条件)
          length (filter (== 0) ks) `shouldSatisfy` (> 0)
          length (filter (== 1) ks) `shouldSatisfy` (> 0)
          length (filter (== 2) ks) `shouldSatisfy` (> 0)
    -- Example 3: LinearIneq 制約 + 2 factor main
    it "golden ex3: LinearIneq (x1+x2 <= 0.5) で全 row が制約満足" $ do
      let f1g = CF.Factor "x1" (CF.Continuous (-1) 1) CF.Controllable
          f2g = CF.Factor "x2" (CF.Continuous (-1) 1) CF.Controllable
          modelMain = CM.Model
            [CM.TIntercept, CM.TMain "x1", CM.TMain "x2"] CM.NCoded
          spec = CX.CustomDesignSpec
            { CX.cdsFactors     = [f1g, f2g]
            , CX.cdsModel       = modelMain
            , CX.cdsConstraints =
                [CC.LinearIneq [("x1", 1), ("x2", 1)] CC.CLeq 0.5]
            , CX.cdsNRuns       = 8
            , CX.cdsCriterion   = OPT.DOpt
            , CX.cdsBudget      = CX.defaultBudget
            , CX.cdsSeed        = Just 21
            , CX.cdsInitial     = Nothing

            , CX.cdsDJConvention = False
            }
      r <- CX.coordinateExchange spec
      case r of
        Left e -> expectationFailure (T.unpack e)
        Right cd -> do
          LA.rows (CX.cdMatrix cd) `shouldBe` 8
          let rows = LA.toLists (CX.cdMatrix cd)
              ok [a, b] = a + b <= 0.5 + 1e-6
              ok _      = False
          all ok rows `shouldBe` True
          -- D 値 > 0 (非特異)
          let dval = - CX.crCriterionValue (CX.cdReport cd)
          dval `shouldSatisfy` (> 0)
