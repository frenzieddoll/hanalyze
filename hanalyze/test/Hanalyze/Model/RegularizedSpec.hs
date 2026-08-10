{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Model.RegularizedSpec (spec) where

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
import qualified System.Random.MWC as MWC
import qualified Hanalyze.Model.Regularized as Reg
import qualified System.Random.MWC as MWC
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Model.Regularized.selectLambdaCV (Phase 4.4)" $ do
    -- 合成データ: y = 2 x_1 + 1 x_2 + noise (x_3..x_5 は無関係)
    let synthData :: IO (LA.Matrix Double, LA.Vector Double)
        synthData = do
          gen <- MWC.create
          let n = 50; p = 5
          xs <- LA.fromLists <$> sequence
                  [ sequence [ MWC.uniformR (-1, 1) gen | _ <- [1..p] ]
                  | _ <- [1..n] ]
          let trueB = LA.fromList [2.0, 1.0, 0.0, 0.0, 0.0] :: LA.Vector Double
              y0 = xs LA.#> trueB
          noise <- LA.fromList <$> sequence
                     [ MWC.uniformR (-0.1, 0.1) gen | _ <- [1..n] ]
          pure (xs, y0 + noise)

    it "Ridge: 全 λ で CV MSE が finite、 best λ が grid 内" $ do
      (xs, y) <- synthData
      let lambdas = [0.001, 0.01, 0.1, 1.0, 10.0]
      gen <- MWC.create
      sel <- Reg.selectLambdaCV 5 Reg.KindRidge lambdas xs y gen
      Reg.lsLambdas sel `shouldBe` lambdas
      length (Reg.lsCVScores sel) `shouldBe` length lambdas
      all (\v -> not (isNaN v) && not (isInfinite v) && v >= 0)
        (Reg.lsCVScores sel) `shouldBe` True
      Reg.lsBestLambda sel `shouldSatisfy` (`elem` lambdas)
      Reg.lsOneSeLambda sel `shouldSatisfy` (>= Reg.lsBestLambda sel)
      Reg.lsKind sel `shouldBe` Reg.KindRidge

    it "Lasso: best λ は grid 内、 各 fold で MSE finite" $ do
      (xs, y) <- synthData
      let lambdas = [0.001, 0.01, 0.05, 0.1, 0.5]
      gen <- MWC.create
      sel <- Reg.selectLambdaCV 5 Reg.KindLasso lambdas xs y gen
      Reg.lsBestLambda sel `shouldSatisfy` (`elem` lambdas)
      length (Reg.lsCVScores sel) `shouldBe` length lambdas
      all (\v -> not (isNaN v) && not (isInfinite v) && v >= 0)
        (Reg.lsCVScores sel) `shouldBe` True

    it "ElasticNet α=0.5: PenaltyKind が結果に反映される" $ do
      (xs, y) <- synthData
      let lambdas = [0.01, 0.1, 1.0]
      gen <- MWC.create
      sel <- Reg.selectLambdaCV 5 (Reg.KindElasticNet 0.5) lambdas xs y gen
      Reg.lsKind sel `shouldBe` Reg.KindElasticNet 0.5
      length (Reg.lsCVScores sel) `shouldBe` length lambdas

    it "1-SE rule: lsOneSeLambda ≥ lsBestLambda" $ do
      (xs, y) <- synthData
      gen <- MWC.create
      sel <- Reg.selectLambdaCV 5 Reg.KindRidge
                                [0.001, 0.01, 0.1, 1.0, 10.0, 100.0]
                                xs y gen
      Reg.lsOneSeLambda sel `shouldSatisfy` (>= Reg.lsBestLambda sel)
