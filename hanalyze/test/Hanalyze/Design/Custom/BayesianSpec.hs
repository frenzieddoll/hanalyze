{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Design.Custom.BayesianSpec (spec) where

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
import qualified Hanalyze.Design.Custom.Coordinate as CX
import qualified Hanalyze.Design.Custom.Compare    as CCMP
import qualified Hanalyze.Design.Custom.Bayesian   as CB
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Design.Custom.Bayesian (Phase 26 BayesianD)" $ do
    let f1b = CF.Factor "x1" (CF.Continuous (-1) 1) CF.Controllable
        f2b = CF.Factor "x2" (CF.Continuous (-1) 1) CF.Controllable
        modelRSM = CM.Model
          [ CM.TIntercept, CM.TMain "x1", CM.TMain "x2"
          , CM.TInter ["x1","x2"]
          , CM.TPower "x1" 2, CM.TPower "x2" 2
          ] CM.NCoded
    it "priorPrecisionDefault: intercept / 主効果 = 0、 2fi / 二乗 = τ²" $ do
      let pp = CB.priorPrecisionDefault [f1b, f2b] modelRSM 1.5
          km = LA.fromLists (CB.precisionToMatrix pp)
      LA.rows km `shouldBe` 6
      LA.cols km `shouldBe` 6
      -- 対角: [0 (intercept), 0 (x1), 0 (x2), 1.5 (x1*x2), 1.5 (x1²), 1.5 (x2²)]
      LA.toList (LA.takeDiag km) `shouldBe` [0, 0, 0, 1.5, 1.5, 1.5]
    it "bayesianDValueM: K = 0 で classic D に縮退" $ do
      let pp = CB.PriorPrecision (LA.konst 0 (3, 3))
          x  = LA.fromLists [[1,-1,-1],[1,1,-1],[1,-1,1],[1,1,1]]
      -- |X'X| = 64
      CB.bayesianDValueM pp x `shouldBe` 64
    it "bayesianDValueM: K > 0 で det(X'X + K) > det(X'X)" $ do
      let x  = LA.fromLists [[1,-1,-1],[1,1,-1],[1,-1,1],[1,1,1]]
          k0 = CB.PriorPrecision (LA.konst 0 (3, 3))
          k1 = CB.PriorPrecision (LA.diagl [0, 1, 1])
      let d0 = CB.bayesianDValueM k0 x
          d1 = CB.bayesianDValueM k1 x
      d1 `shouldSatisfy` (> d0)
    it "OptCriterion.BayesianD: coordinateExchange で動作 (連続 2 因子)" $ do
      let pp = CB.priorPrecisionDefault [f1b, f2b] modelRSM 1.0
          spec = CX.CustomDesignSpec
            { CX.cdsFactors = [f1b, f2b]
            , CX.cdsModel   = modelRSM
            , CX.cdsConstraints = []
            , CX.cdsNRuns   = 10
            , CX.cdsCriterion = OPT.BayesianD (CB.precisionToMatrix pp)
            , CX.cdsBudget    = CX.defaultBudget
                { CX.dbRestarts = 3, CX.dbMaxIter = 20 }
            , CX.cdsSeed      = Just 77
            , CX.cdsInitial   = Nothing

            , CX.cdsDJConvention = False
            }
      r <- CX.coordinateExchange spec
      case r of
        Left e -> expectationFailure (T.unpack e)
        Right cd -> do
          LA.rows (CX.cdMatrix cd) `shouldBe` 10
          -- criterion 値が finite + 0 より小さい (= - det > 0)
          CX.crCriterionValue (CX.cdReport cd) `shouldSatisfy` (< 0)
    it "Phase 28-12a djTransformColumns: paper §2.2 例 (x²→x²-0.5、 x³→(x³-0.85x)/0.6) と一致" $ do
      let fx  = CF.Factor "x" (CF.Continuous (-1) 1) CF.Controllable
          m   = CM.Model
                  [CM.TIntercept, CM.TMain "x", CM.TPower "x" 2, CM.TPower "x" 3]
                  CM.NCoded
          -- 候補集合: {-1, -0.5, 0, 0.5, 1} (paper §2.2 の前提)
          cand = LA.fromLists [[-1],[-0.5],[0],[0.5],[1]]
          -- 同じ点を「設計」 として渡す (full grid)
          design = cand
          nearly a b = abs (a - b) < 1e-9
      case CM.expandDesignMatrix [fx] m design of
        Left e -> expectationFailure (T.unpack e)
        Right x0 ->
          case CB.djTransformColumns [fx] m cand x0 of
            Left e -> expectationFailure (T.unpack e)
            Right xT -> do
              -- 列順: [Intercept, x, x², x³] → 変換後: [1, x, x²−0.5, (x³−0.85x)/0.6]
              LA.cols xT `shouldBe` 4
              -- 行 0: x = -1 → [1, -1, 1-0.5=0.5, (-1+0.85)/0.6 = -0.25]
              (xT `LA.atIndex` (0, 2)) `shouldSatisfy` nearly 0.5
              (xT `LA.atIndex` (0, 3)) `shouldSatisfy` nearly (-0.25)
              -- 行 1: x = -0.5 → [1, -0.5, 0.25-0.5=-0.25, (-0.125+0.425)/0.6 = 0.5]
              (xT `LA.atIndex` (1, 2)) `shouldSatisfy` nearly (-0.25)
              (xT `LA.atIndex` (1, 3)) `shouldSatisfy` nearly 0.5
              -- 行 2: x = 0 → [1, 0, -0.5, 0]
              (xT `LA.atIndex` (2, 2)) `shouldSatisfy` nearly (-0.5)
              (xT `LA.atIndex` (2, 3)) `shouldSatisfy` nearly 0
              -- 行 3: x = 0.5 → [1, 0.5, -0.25, -0.5]
              (xT `LA.atIndex` (3, 2)) `shouldSatisfy` nearly (-0.25)
              (xT `LA.atIndex` (3, 3)) `shouldSatisfy` nearly (-0.5)
              -- 行 4: x = 1 → [1, 1, 0.5, 0.25]
              (xT `LA.atIndex` (4, 2)) `shouldSatisfy` nearly 0.5
              (xT `LA.atIndex` (4, 3)) `shouldSatisfy` nearly 0.25
              -- primary 列 (0, 1) はそのまま
              (xT `LA.atIndex` (3, 0)) `shouldSatisfy` nearly 1
              (xT `LA.atIndex` (3, 1)) `shouldSatisfy` nearly 0.5
    it "Phase 28-12a djFitTransform: primary/potential 分類が defaultClassifier と整合" $ do
      let fx = CF.Factor "x" (CF.Continuous (-1) 1) CF.Controllable
          fy = CF.Factor "y" (CF.Continuous (-1) 1) CF.Controllable
          m  = CM.Model
                 [ CM.TIntercept, CM.TMain "x", CM.TMain "y"
                 , CM.TInter ["x","y"], CM.TPower "x" 2 ]
                 CM.NCoded
          cand = LA.fromLists [[a,b] | a <- [-1, 0, 1], b <- [-1, 0, 1]]
      case CB.djFitTransform [fx, fy] m cand of
        Left e -> expectationFailure (T.unpack e)
        Right t -> do
          -- expand 後の列: [Intercept(0), x(1), y(2), x:y(3), x²(4)]
          -- primary: 0,1,2、 potential: 3,4
          CB.djtPrimaryIdx t   `shouldBe` [0, 1, 2]
          CB.djtPotentialIdx t `shouldBe` [3, 4]
    it "Compound 重み正規化: 負を 0、 合計 = 1" $ do
      let ws  = [(0.7, OPT.DOpt), (0.5, OPT.AOpt), (-0.1, OPT.IOpt)]
          ws' = CCMP.normalizeCompoundWeights ws
          total = sum (map fst ws')
      abs (total - 1.0) `shouldSatisfy` (< 1e-9)
      (fst (ws' !! 2)) `shouldBe` 0
    it "Compound 重み合計 ≤ 0 は no-op" $ do
      let ws = [(-1, OPT.DOpt), (-2, OPT.AOpt)]
      CCMP.normalizeCompoundWeights ws `shouldBe` ws
