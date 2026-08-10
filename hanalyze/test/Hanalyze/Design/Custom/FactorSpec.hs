{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Design.Custom.FactorSpec (spec) where

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
import qualified Hanalyze.Design.Custom.Factor     as CF
import qualified Hanalyze.Design.Custom.Model      as CM
import qualified Hanalyze.Design.Custom.Constraint as CC
import qualified Data.Map.Strict as M
import qualified Data.Map.Strict    as M
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Design.Custom.Factor / Model / Constraint (Phase 24-1 skeleton)" $ do
    let f1 = CF.Factor "x1" (CF.Continuous (-1) 1)   CF.Controllable
        f2 = CF.Factor "x2" (CF.Continuous (-1) 1)   CF.Controllable
        fCat = CF.Factor "cat" (CF.Categorical ["A","B","C"]) CF.Controllable
        factors = [f1, f2]
        raw = LA.fromLists [[-1,-1],[1,-1],[-1,1],[1,1]]
        modelMain = CM.Model [CM.TIntercept, CM.TMain "x1", CM.TMain "x2"] CM.NCoded
        modelFull = CM.Model
          [CM.TIntercept, CM.TMain "x1", CM.TMain "x2"
          , CM.TInter ["x1","x2"], CM.TPower "x1" 2] CM.NCoded
    it "Factor.factorIsContinuous / factorDimension" $ do
      CF.factorIsContinuous f1   `shouldBe` True
      CF.factorIsContinuous fCat `shouldBe` False
      CF.factorDimension f1   `shouldBe` 1
      CF.factorDimension fCat `shouldBe` 2  -- treatment coding: levels - 1
    it "expandDesignMatrix: 主効果のみ 2² で 4×3 (intercept + x1 + x2)" $ do
      case CM.expandDesignMatrix factors modelMain raw of
        Left e  -> expectationFailure (T.unpack e)
        Right m -> do
          LA.rows m `shouldBe` 4
          LA.cols m `shouldBe` 3
          LA.toLists m `shouldBe`
            [[1,-1,-1],[1,1,-1],[1,-1,1],[1,1,1]]
    it "expandDesignMatrix: 交互作用 + 二乗を含むフルモデル" $ do
      case CM.expandDesignMatrix factors modelFull raw of
        Left e  -> expectationFailure (T.unpack e)
        Right m -> do
          LA.cols m `shouldBe` 5
          -- 各 row の最後の 2 列を確認: x1*x2、 x1^2
          let rows = LA.toLists m
              lastTwo = map (drop 3) rows
          lastTwo `shouldBe` [[1,1],[-1,1],[-1,1],[1,1]]
    it "expandDesignMatrix: 列数不一致は Left" $ do
      case CM.expandDesignMatrix factors modelMain (LA.fromLists [[1]]) of
        Left _  -> pure ()
        Right _ -> expectationFailure "expected Left"
    it "expandDesignMatrix: 未知因子参照は Left" $ do
      let m = CM.Model [CM.TMain "x9"] CM.NCoded
      case CM.expandDesignMatrix factors m raw of
        Left _  -> pure ()
        Right _ -> expectationFailure "expected Left"
    it "expandDesignMatrix: Categorical TMain は treatment coding で K-1 列 (Phase 24-2)" $ do
      let fs = [fCat]
          m  = CM.Model [CM.TIntercept, CM.TMain "cat"] CM.NCoded
          rawCat = LA.fromLists [[0],[1],[2],[0],[1]]
      case CM.expandDesignMatrix fs m rawCat of
        Left e  -> expectationFailure (T.unpack e)
        Right d -> do
          LA.rows d `shouldBe` 5
          LA.cols d `shouldBe` 3  -- intercept + (3-1) treatment cols
          LA.toLists d `shouldBe`
            [ [1, 0, 0]  -- ref (A)
            , [1, 1, 0]  -- B
            , [1, 0, 1]  -- C
            , [1, 0, 0]
            , [1, 1, 0]
            ]
    it "expandDesignMatrix: Categorical × 連続 TInter は (K-1) 列" $ do
      let fs = [f1, fCat]
          m  = CM.Model [CM.TInter ["x1","cat"]] CM.NCoded
          rawMix = LA.fromLists [[-1, 0], [1, 1], [-1, 2], [1, 0]]
      case CM.expandDesignMatrix fs m rawMix of
        Left e  -> expectationFailure (T.unpack e)
        Right d -> do
          LA.cols d `shouldBe` 2
          LA.toLists d `shouldBe`
            [ [(-1) * 0, (-1) * 0]   -- cat = A → 両 indicator 0
            , [1 * 1,    1 * 0]      -- cat = B
            , [(-1) * 0, (-1) * 1]   -- cat = C
            , [1 * 0,    1 * 0]      -- cat = A
            ]
    it "expandDesignMatrix: Categorical raw が非整数なら Left" $ do
      let fs = [fCat]
          m  = CM.Model [CM.TMain "cat"] CM.NCoded
      case CM.expandDesignMatrix fs m (LA.fromLists [[0.5]]) of
        Left _  -> pure ()
        Right _ -> expectationFailure "expected Left for non-integer level index"
    it "expandDesignMatrix: Categorical raw が範囲外なら Left" $ do
      let fs = [fCat]
          m  = CM.Model [CM.TMain "cat"] CM.NCoded
      case CM.expandDesignMatrix fs m (LA.fromLists [[3]]) of
        Left _  -> pure ()
        Right _ -> expectationFailure "expected Left for out-of-range index"
    it "expandDesignMatrix: TPower を Categorical に適用すると Left" $ do
      let fs = [fCat]
          m  = CM.Model [CM.TPower "cat" 2] CM.NCoded
      case CM.expandDesignMatrix fs m (LA.fromLists [[0]]) of
        Left _  -> pure ()
        Right _ -> expectationFailure "expected Left for TPower on categorical"
    it "Phase 28-1 TNested: A within B で K_B × (K_A - 1) 列、 indicator I[B=b]·I[A=a]" $ do
      -- B = {b0,b1} (2 levels)、 A = {a0,a1,a2} (3 levels) → 2 × 2 = 4 列
      let fA = CF.Factor "A" (CF.Categorical ["a0","a1","a2"]) CF.Controllable
          fB = CF.Factor "B" (CF.Categorical ["b0","b1"])      CF.Controllable
          fs = [fA, fB]
          m  = CM.Model [CM.TNested "A" "B"] CM.NCoded
          -- 6 行: 全 (B, A) 組合せ + B=0,A=0 / B=1,A=2 を 1 行ずつ
          raw = LA.fromLists
            [[0, 0]  -- A=a0, B=b0 → all 4 cols = 0 (a=0 は reference)
            ,[1, 0]  -- A=a1, B=b0 → col (b0, a1) = 1
            ,[2, 0]  -- A=a2, B=b0 → col (b0, a2) = 1
            ,[0, 1]  -- A=a0, B=b1 → all = 0
            ,[1, 1]  -- A=a1, B=b1 → col (b1, a1) = 1
            ,[2, 1]  -- A=a2, B=b1 → col (b1, a2) = 1
            ]
      case CM.expandDesignMatrix fs m raw of
        Left e -> expectationFailure (T.unpack e)
        Right x -> do
          LA.rows x `shouldBe` 6
          LA.cols x `shouldBe` 4    -- K_B(2) × (K_A-1)(2) = 4
          -- 列順 (b, a) = (0,1), (0,2), (1,1), (1,2)
          LA.toLists x `shouldBe`
            [[0,0,0,0]   -- A=0, B=0
            ,[1,0,0,0]   -- A=1, B=0 → col (0,1) = 1
            ,[0,1,0,0]   -- A=2, B=0 → col (0,2) = 1
            ,[0,0,0,0]   -- A=0, B=1
            ,[0,0,1,0]   -- A=1, B=1 → col (1,1) = 1
            ,[0,0,0,1]   -- A=2, B=1 → col (1,2) = 1
            ]
    it "modelNumColumns: Categorical を含むモデルで K-1 を考慮" $ do
      let m = CM.Model [CM.TIntercept, CM.TMain "x1", CM.TMain "cat"
                       , CM.TInter ["x1","cat"]] CM.NCoded
      -- 1 (intercept) + 1 (x1) + 2 (cat K-1) + 2 (x1 * cat K-1) = 6
      CM.modelNumColumns [f1, fCat] m `shouldBe` 6
    it "Constraint.LinearIneq 評価" $ do
      let row = CC.compileRowFromFactors ["x1","x2"] [0.4, 0.4]
          c   = CC.LinearIneq [("x1", 1), ("x2", 1)] CC.CLeq 1
      CC.checkRowAgainst row c `shouldBe` True
      let row2 = CC.compileRowFromFactors ["x1","x2"] [0.7, 0.7]
      CC.checkRowAgainst row2 c `shouldBe` False
    it "Constraint.Forbidden + Conditional 評価" $ do
      let row = M.fromList [("cat", CC.FVText "A"), ("temp", CC.FVDouble 90)]
          forb = CC.Forbidden [("cat", CC.FVText "A"), ("temp", CC.FVDouble 90)]
          cond = CC.Conditional
                    (CC.GuardEq "cat" (CC.FVText "A"))
                    [CC.RangeBound "temp" 0 80]
      CC.checkRowAgainst row forb `shouldBe` False  -- forbidden 一致 = 違反
      CC.checkRowAgainst row cond `shouldBe` False  -- cat=A の時 temp ≤ 80 を要求、 90 は違反
