{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Design.Custom.CoordinateSpec (spec) where

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
import qualified Data.Vector.Unboxed        as VU
import qualified Hanalyze.Stat.ClassMetrics as CM
import qualified Hanalyze.Design.Optimal       as OPT
import qualified Hanalyze.Design.Custom.Factor     as CF
import qualified Hanalyze.Design.Custom.Model      as CM
import qualified Hanalyze.Design.Custom.Constraint as CC
import qualified Hanalyze.Design.Custom.Coordinate as CX
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Design.Custom.Coordinate (Phase 24-3 Coordinate Exchange)" $ do
    let fx1 = CF.Factor "x1" (CF.Continuous (-1) 1) CF.Controllable
        fx2 = CF.Factor "x2" (CF.Continuous (-1) 1) CF.Controllable
        fCat = CF.Factor "cat" (CF.Categorical ["A","B"]) CF.Controllable
        modelMain2 = CM.Model
          [CM.TIntercept, CM.TMain "x1", CM.TMain "x2"] CM.NCoded
        budget = CX.defaultBudget
                   { CX.dbRestarts = 3
                   , CX.dbMaxIter  = 30
                   , CX.dbCxStepGrid = 11
                   }
        baseSpec = CX.CustomDesignSpec
          { CX.cdsFactors     = [fx1, fx2]
          , CX.cdsModel       = modelMain2
          , CX.cdsConstraints = []
          , CX.cdsNRuns       = 6
          , CX.cdsCriterion   = OPT.DOpt
          , CX.cdsBudget      = budget
          , CX.cdsSeed        = Just 42
          , CX.cdsInitial     = Nothing

          , CX.cdsDJConvention = False
          }
    it "gridForBudget: 21 点 grid は [-1, 1] の等間隔" $ do
      let g = CX.gridForBudget CX.defaultBudget
      VU.length g `shouldBe` 21
      g VU.! 0  `shouldBe` (-1)
      g VU.! 20 `shouldBe` 1
      g VU.! 10 `shouldBe` 0
    it "factorGrid: Continuous は dbCxStepGrid 点 / Categorical は [0..K-1]" $ do
      let gCont = CX.factorGrid CX.defaultBudget fx1
          gCat  = CX.factorGrid CX.defaultBudget fCat  -- K=2
      VU.length gCont `shouldBe` 21
      VU.toList  gCat `shouldBe` [0, 1]
    it "dbRestarts < 1 は Left" $ do
      let spec = baseSpec { CX.cdsBudget = budget { CX.dbRestarts = 0 } }
      r <- CX.coordinateExchange spec
      case r of
        Left _  -> pure ()
        Right _ -> expectationFailure "expected Left"
    it "D-opt: 連続 2 因子 main-effect モデルで factorial 2² 設計に近い |det| を得る" $ do
      r <- CX.coordinateExchange baseSpec
      case r of
        Left e  -> expectationFailure (T.unpack e)
        Right cd -> do
          -- 戻り値の整合
          LA.rows (CX.cdMatrix cd) `shouldBe` 6
          LA.cols (CX.cdMatrix cd) `shouldBe` 2
          -- raw matrix は [-1, 1] grid 内
          let vs = concat (LA.toLists (CX.cdMatrix cd))
          all (\v -> v >= -1.0001 && v <= 1.0001) vs `shouldBe` True
          -- D 値 = −critValue (最小化方向)
          let dval = - (CX.crCriterionValue (CX.cdReport cd))
          dval `shouldSatisfy` (> 0)
          -- 6 点で main+intercept (3 列) なら |X'X| は概ね 6^3 = 216 付近が上限
          -- (各列ノルム² ≤ 6、 直交時に等号)。 0.7 倍 = 151.2 以上を要求。
          dval `shouldSatisfy` (> 150)
    it "D-opt: ランダム単 start より multi-start が同等以上の D 値" $ do
      let spec1 = baseSpec { CX.cdsBudget = budget { CX.dbRestarts = 1 }
                           , CX.cdsSeed = Just 7 }
          spec5 = baseSpec { CX.cdsBudget = budget { CX.dbRestarts = 5 }
                           , CX.cdsSeed = Just 7 }
      r1 <- CX.coordinateExchange spec1
      r5 <- CX.coordinateExchange spec5
      case (r1, r5) of
        (Right cd1, Right cd5) -> do
          let d1 = - CX.crCriterionValue (CX.cdReport cd1)
              d5 = - CX.crCriterionValue (CX.cdReport cd5)
          (d5 + 1e-9) `shouldSatisfy` (>= d1)
        _ -> expectationFailure "both runs should return Right"
    it "critValueM (DOpt): D 値が古典 dValue と一致" $ do
      let x = LA.fromLists [[1, -1, -1], [1, 1, -1], [1, -1, 1], [1, 1, 1]]
          c = CX.critValueM OPT.DOpt x
      c `shouldBe` (-64)   -- |X'X| = 4 * 4 * 4 = 64
    -- Phase 24-4 hybrid: continuous + categorical 混合
    it "hybrid: x1 (連続) + cat (K=2) の D-opt 設計、 cat 列は 0/1 のみ" $ do
      let modelMix = CM.Model
            [CM.TIntercept, CM.TMain "x1", CM.TMain "cat"] CM.NCoded
          spec = baseSpec
            { CX.cdsFactors = [fx1, fCat]
            , CX.cdsModel   = modelMix
            , CX.cdsNRuns   = 8
            , CX.cdsSeed    = Just 100
            }
      r <- CX.coordinateExchange spec
      case r of
        Left e -> expectationFailure (T.unpack e)
        Right cd -> do
          let m = CX.cdMatrix cd
          LA.rows m `shouldBe` 8
          LA.cols m `shouldBe` 2
          -- cat 列 (index 1) は integer level index ∈ {0, 1}
          let catCol = LA.toList (LA.flatten (LA.subMatrix (0, 1) (8, 1) m))
          all (`elem` [0, 1]) (map round catCol :: [Int]) `shouldBe` True
          all (\v -> abs (v - fromIntegral (round v :: Int)) < 1e-9) catCol
            `shouldBe` True
          -- 両 level が少なくとも 1 回ずつ出現 (= balanced に近い)
          (0 `elem` map round catCol :: Bool) `shouldBe` True
          (1 `elem` (map round catCol :: [Int])) `shouldBe` True
          -- x1 列は [-1, 1]
          let x1Col = LA.toList (LA.flatten (LA.subMatrix (0, 0) (8, 1) m))
          all (\v -> v >= -1.0001 && v <= 1.0001) x1Col `shouldBe` True
          -- D > 0 (full rank)
          let dval = - CX.crCriterionValue (CX.cdReport cd)
          dval `shouldSatisfy` (> 0)
    -- Phase 24-5 制約統合
    it "LinearIneq (x1 + x2 <= 1): 全 row が制約を満たす" $ do
      let modelMain2 = CM.Model
            [CM.TIntercept, CM.TMain "x1", CM.TMain "x2"] CM.NCoded
          spec = baseSpec
            { CX.cdsFactors = [fx1, fx2]
            , CX.cdsModel   = modelMain2
            , CX.cdsConstraints =
                [CC.LinearIneq [("x1", 1), ("x2", 1)] CC.CLeq 1]
            , CX.cdsNRuns   = 6
            , CX.cdsSeed    = Just 21
            }
      r <- CX.coordinateExchange spec
      case r of
        Left e -> expectationFailure (T.unpack e)
        Right cd -> do
          let rows = LA.toLists (CX.cdMatrix cd)
              ok [a, b] = a + b <= 1 + 1e-6
              ok _ = False
          all ok rows `shouldBe` True
    it "Forbidden (cat=A, x1=1): 該当組合せが出現しない" $ do
      let modelMix = CM.Model
            [CM.TIntercept, CM.TMain "x1", CM.TMain "cat"] CM.NCoded
          spec = baseSpec
            { CX.cdsFactors = [fx1, fCat]
            , CX.cdsModel   = modelMix
            , CX.cdsConstraints =
                [CC.Forbidden [("cat", CC.FVText "A"), ("x1", CC.FVDouble 1)]]
            , CX.cdsNRuns   = 8
            , CX.cdsSeed    = Just 33
            }
      r <- CX.coordinateExchange spec
      case r of
        Left e -> expectationFailure (T.unpack e)
        Right cd -> do
          let rows = LA.toLists (CX.cdMatrix cd)
              bad [x1, c]
                | round c == (0 :: Int) && abs (x1 - 1) < 1e-9 = True
                | otherwise = False
              bad _ = False
          any bad rows `shouldBe` False
    it "実現不能制約 (x1 >= 2): Left" $ do
      let modelMain1 = CM.Model
            [CM.TIntercept, CM.TMain "x1"] CM.NCoded
          spec = baseSpec
            { CX.cdsFactors = [fx1]
            , CX.cdsModel   = modelMain1
            , CX.cdsConstraints =
                [CC.LinearIneq [("x1", 1)] CC.CGeq 2]   -- x1 >= 2 は [-1,1] grid で実現不能
            , CX.cdsNRuns   = 4
            , CX.cdsSeed    = Just 1
            }
      r <- CX.coordinateExchange spec
      case r of
        Left _  -> pure ()
        Right _ -> expectationFailure "expected Left for infeasible constraint"

    it "hybrid: 3 水準 categorical のみで level 全て出現可能" $ do
      let fCat3 = CF.Factor "k" (CF.Categorical ["A","B","C"]) CF.Controllable
          modelCat = CM.Model
            [CM.TIntercept, CM.TMain "k"] CM.NCoded
          spec = baseSpec
            { CX.cdsFactors = [fCat3]
            , CX.cdsModel   = modelCat
            , CX.cdsNRuns   = 9
            , CX.cdsSeed    = Just 11
            }
      r <- CX.coordinateExchange spec
      case r of
        Left e -> expectationFailure (T.unpack e)
        Right cd -> do
          let col = LA.toList (LA.flatten (CX.cdMatrix cd))
              idxs = map round col :: [Int]
          all (`elem` [0, 1, 2]) idxs `shouldBe` True
          -- D-opt なら 9 runs で各 level 3 回ずつが理論最適。
          -- 3 level 全部出現するはず。
          length (filter (== 0) idxs) `shouldSatisfy` (> 0)
          length (filter (== 1) idxs) `shouldSatisfy` (> 0)
          length (filter (== 2) idxs) `shouldSatisfy` (> 0)

  -- Phase 78.M M1: IO→ST pure 化。同一 seed で純粋版が IO 版とビット一致
  -- (アルゴリズム不変の回帰保証) + 純粋版の参照透過性を検証。
  describe "Phase 78.M M1: coordinateExchangePure (seed 決定的 pure)" $ do
    let fx1 = CF.Factor "x1" (CF.Continuous (-1) 1) CF.Controllable
        fx2 = CF.Factor "x2" (CF.Continuous (-1) 1) CF.Controllable
        fCat3 = CF.Factor "k" (CF.Categorical ["A","B","C"]) CF.Controllable
        budget = CX.defaultBudget
                   { CX.dbRestarts = 3, CX.dbMaxIter = 30, CX.dbCxStepGrid = 11 }
        mkSpec fs md nruns seed = CX.CustomDesignSpec
          { CX.cdsFactors = fs, CX.cdsModel = md, CX.cdsConstraints = []
          , CX.cdsNRuns = nruns, CX.cdsCriterion = OPT.DOpt, CX.cdsBudget = budget
          , CX.cdsSeed = seed, CX.cdsInitial = Nothing, CX.cdsDJConvention = False }
        model2 = CM.Model [CM.TIntercept, CM.TMain "x1", CM.TMain "x2"] CM.NCoded
        modelCat = CM.Model [CM.TIntercept, CM.TMain "k"] CM.NCoded

    it "純粋版は IO 版と同 seed でビット一致 (連続 2 因子・RSM)" $ do
      let spec = mkSpec [fx1, fx2] model2 6 (Just 42)
      io <- CX.coordinateExchange spec
      let pu = CX.coordinateExchangePure spec
      case (io, pu) of
        (Right cdIO, Right cdPu) -> do
          LA.toLists (CX.cdMatrix cdIO) `shouldBe` LA.toLists (CX.cdMatrix cdPu)
          CX.crCriterionValue (CX.cdReport cdIO)
            `shouldBe` CX.crCriterionValue (CX.cdReport cdPu)
        _ -> expectationFailure "expected Right from both IO and pure"

    it "純粋版は IO 版と同 seed でビット一致 (categorical 因子)" $ do
      let spec = mkSpec [fCat3] modelCat 9 (Just 11)
      io <- CX.coordinateExchange spec
      case (io, CX.coordinateExchangePure spec) of
        (Right cdIO, Right cdPu) ->
          LA.toLists (CX.cdMatrix cdIO) `shouldBe` LA.toLists (CX.cdMatrix cdPu)
        _ -> expectationFailure "expected Right from both IO and pure"

    it "純粋版は参照透過 (同 spec を 2 回呼んで同結果)" $ do
      let spec = mkSpec [fx1, fx2] model2 6 (Just 7)
      case (CX.coordinateExchangePure spec, CX.coordinateExchangePure spec) of
        (Right a, Right b) ->
          LA.toLists (CX.cdMatrix a) `shouldBe` LA.toLists (CX.cdMatrix b)
        _ -> expectationFailure "expected Right from both pure calls"

    it "seed が Nothing でも純粋版は全域 (defaultPureSeed で決定的)" $ do
      let spec = mkSpec [fx1, fx2] model2 6 Nothing
      case (CX.coordinateExchangePure spec, CX.coordinateExchangePure spec) of
        (Right a, Right b) ->
          LA.toLists (CX.cdMatrix a) `shouldBe` LA.toLists (CX.cdMatrix b)
        _ -> expectationFailure "expected Right (total) even with Nothing seed"
