{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Design.ConstraintSpec (spec) where

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
import qualified Hanalyze.Design.Optimal       as OPT
import qualified Hanalyze.Design.Constraint    as DCons
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Design.Constraint (Phase 23-b)" $ do
    let cands =
          [ [1, x1, x2] | x1 <- [-1, 0, 1], x2 <- [-1, 0, 1] ]
    it "checkRow: LinearConstraint x1 + x2 <= 1 を判定" $ do
      let c = DCons.LinearConstraint [0, 1, 1] DCons.CLeq 1
      DCons.checkRow [c] [1, -1, -1] `shouldBe` True
      DCons.checkRow [c] [1,  1,  1] `shouldBe` False
      DCons.checkRow [c] [1,  0,  1] `shouldBe` True
    it "checkRow: ForbiddenCombination で完全一致を弾く" $ do
      let c = DCons.ForbiddenCombination [1, 1, 1]
      DCons.checkRow [c] [1, 1, 1] `shouldBe` False
      DCons.checkRow [c] [1, 0, 1] `shouldBe` True
    it "checkRow: 浮動小数 tolerance (1e-9) で ForbiddenCombination 判定" $ do
      let c = DCons.ForbiddenCombination [1, 1]
      DCons.checkRow [c] [1, 1 + 1e-11] `shouldBe` False  -- tol 内 = 一致 = 違反
      DCons.checkRow [c] [1, 1 + 1e-6 ] `shouldBe` True   -- tol 外 = OK
    it "checkDesign: 違反 row index を列挙" $ do
      let c    = DCons.LinearConstraint [0, 1, 1] DCons.CLeq 1
          mat  = LA.fromLists [[1,-1,-1],[1,1,1],[1,0,1],[1,1,0.5]]
      DCons.checkDesign [c] mat `shouldBe` [1, 3]
    it "filterCandidates: x1 + x2 <= 1 で 9 候補 → 8 候補 (排除: (1,1))" $ do
      let c   = DCons.LinearConstraint [0, 1, 1] DCons.CLeq 1
          fc  = DCons.filterCandidates [c] cands
      length fc `shouldBe` 8
    it "filterCandidates 後の候補で dOptimal が動作 (5 行抽出)" $ do
      let c   = DCons.LinearConstraint [0, 1, 1] DCons.CLeq 1
          fc  = DCons.filterCandidates [c] cands
          (idxs, des) = OPT.dOptimal fc 5 42
      length idxs `shouldBe` 5
      length des `shouldBe` 5
    it "より厳しい制約 x1 + x2 <= 0 で 9 候補 → 6 候補に縮約" $ do
      -- 排除: (0,1)=1、 (1,0)=1、 (1,1)=2 の 3 件
      let c   = DCons.LinearConstraint [0, 1, 1] DCons.CLeq 0
          fc  = DCons.filterCandidates [c] cands
      length fc `shouldBe` 6
    it "CEq / CGeq 判定" $ do
      let cEq  = DCons.LinearConstraint [0, 1, 1] DCons.CEq  0
          cGeq = DCons.LinearConstraint [0, 1, 1] DCons.CGeq 1
      DCons.checkRow [cEq]  [1, -1, 1] `shouldBe` True
      DCons.checkRow [cEq]  [1,  1, 1] `shouldBe` False
      DCons.checkRow [cGeq] [1,  1, 1] `shouldBe` True
      DCons.checkRow [cGeq] [1, -1, 0] `shouldBe` False
