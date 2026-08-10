{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Stat.CholeskySpec (spec) where

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
import qualified Hanalyze.Stat.Cholesky     as Chol
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Stat.Cholesky" $ do
    let aSPD = LA.fromLists [[4, 2, 1], [2, 5, 3], [1, 3, 6]]
                 :: LA.Matrix Double
        b    = LA.asColumn (LA.fromList [1.0, 2.0, 3.0])

    it "cholSolve agrees with LA.<\\> on a 3x3 SPD system (1e-9)" $ do
      let xC = Chol.cholSolve aSPD b
          xR = aSPD LA.<\> b
      case xC of
        Nothing -> expectationFailure "cholSolve returned Nothing on SPD"
        Just xc -> LA.norm_Inf (xc - xR) < 1e-9 `shouldBe` True

    it "cholSolveJitter falls back gracefully on a singular matrix" $ do
      let aSing = LA.fromLists [[1, 0, 0], [0, 0, 0], [0, 0, 1]]
                    :: LA.Matrix Double
          bSing = LA.asColumn (LA.fromList [1.0, 0.0, 1.0])
          x = Chol.cholSolveJitter aSing bSing
      LA.rows x `shouldBe` 3   -- did not crash; whatever LSQ gives is fine

    it "cholFactor returns Just for SPD and Nothing for non-SPD" $ do
      Chol.cholFactor aSPD `shouldSatisfy` (\m -> case m of
                                                    Just _  -> True
                                                    Nothing -> False)
      let aNeg = LA.fromLists [[1, 2], [2, 1]] :: LA.Matrix Double
                  -- eigenvalues 3 and -1 → not SPD
      Chol.cholFactor aNeg `shouldBe` Nothing
