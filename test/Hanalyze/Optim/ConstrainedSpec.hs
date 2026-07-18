{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Optim.ConstrainedSpec (spec) where

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
import qualified Hanalyze.Optim.Constrained as Con
import qualified Hanalyze.Optim.Common      as OC
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Optim.Constrained" $ do
    it "Augmented Lagrangian: min x1²+x2² s.t. x1+x2=1 → (0.5, 0.5)" $ do
      let f xs = (head xs)^(2::Int) + (xs !! 1)^(2::Int)
          cs = Con.ConstraintSet
                 { Con.csEq   = [\xs -> head xs + xs !! 1 - 1]
                 , Con.csIneq = []
                 }
      (r, viol) <- Con.runAugmentedLagrangian
                     Con.defaultConstrainedConfig f cs [0, 0]
      let [x1, x2] = OC.orBest r
      abs (x1 - 0.5) `shouldSatisfy` (< 0.05)
      abs (x2 - 0.5) `shouldSatisfy` (< 0.05)
      viol `shouldSatisfy` (< 1e-3)

    it "Augmented Lagrangian: 不等式 x ≥ 1 (h(x)=1-x≤0) で min x²" $ do
      let f xs  = (head xs)^(2::Int)
          cs   = Con.ConstraintSet
                 { Con.csEq   = []
                 , Con.csIneq = [\xs -> 1 - head xs]    -- 1 - x ≤ 0 ⇔ x ≥ 1
                 }
      (r, viol) <- Con.runAugmentedLagrangian
                     Con.defaultConstrainedConfig f cs [0]
      let [x] = OC.orBest r
      x `shouldSatisfy` (\v -> v >= 1 - 0.01)
      viol `shouldSatisfy` (< 1e-3)

    it "Penalty method: 等式 x1+x2=1 (簡易版)" $ do
      let f xs = (head xs)^(2::Int) + (xs !! 1)^(2::Int)
          cs = Con.ConstraintSet
                 { Con.csEq   = [\xs -> head xs + xs !! 1 - 1]
                 , Con.csIneq = []
                 }
      (r, _) <- Con.penaltyMethod Con.defaultConstrainedConfig f cs [0, 0]
      let [x1, x2] = OC.orBest r
      abs (x1 + x2 - 1) `shouldSatisfy` (< 0.05)

    it "boxToIneq: bounds [(1,5)] を不等式 2 本に展開、x=3 で両方満たす" $ do
      let ineqs = Con.boxToIneq [(1.0, 5.0)]
      length ineqs `shouldBe` 2
      all (\h -> h [3.0] <= 0) ineqs `shouldBe` True
      any (\h -> h [0.0] > 0) ineqs `shouldBe` True   -- 下限違反
      any (\h -> h [6.0] > 0) ineqs `shouldBe` True   -- 上限違反

  -- ===========================================================================
  -- box 制約 (Bounds) 統一インターフェース (Hanalyze.Optim.Common)
  -- ===========================================================================
