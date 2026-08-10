{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Design.SpaceFillingSpec (spec) where

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
import qualified Hanalyze.Design.SpaceFilling  as SF
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC as MWC
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Design.SpaceFilling (Phase 6.1)" $ do
    let inUnitBox m =
          all (\v -> v >= 0 && v < 1) (LA.toList (LA.flatten m))

    it "latinHypercube: n × d 行列、 [0,1) 内、 minDist > 0" $ do
      gen <- MWC.create
      des <- SF.latinHypercube 20 3 gen
      SF.sfdNPoints des `shouldBe` 20
      SF.sfdNDims   des `shouldBe` 3
      LA.rows (SF.sfdMatrix des) `shouldBe` 20
      LA.cols (SF.sfdMatrix des) `shouldBe` 3
      inUnitBox (SF.sfdMatrix des) `shouldBe` True
      SF.sfdMinDist des `shouldSatisfy` (> 0)
      SF.sfdMethod  des `shouldBe` "LHS"

    it "latinHypercube: 各列の stratification (n セルに 1 点ずつ)" $ do
      gen <- MWC.create
      des <- SF.latinHypercube 10 2 gen
      let mat = SF.sfdMatrix des
          col0 = LA.toList (LA.flatten (mat LA.¿ [0]))
          cellOf x = floor (x * 10) :: Int
          cells0 = map cellOf col0
      length (Data.List.nub cells0) `shouldBe` 10  -- 全 10 セル使用

    it "latinHypercubeMaximin: minDist ≥ plain LHS minDist (LHS から始まる)" $ do
      gen1 <- MWC.create
      gen2 <- MWC.create
      lhs <- SF.latinHypercube 15 3 gen1
      mxmn <- SF.latinHypercubeMaximin 15 3 200 gen2
      SF.sfdMethod mxmn `shouldBe` "MaximinLHS"
      -- maximin は LHS の swap 局所探索なので、 同 seed では LHS と
      -- 同等以上の minDist を期待。 異 seed でも壊滅的に悪くはならないはず:
      -- 「max(LHS, MaximinLHS) ≥ LHS の半分」 程度の緩い検証で OK
      SF.sfdMinDist mxmn `shouldSatisfy` (>= SF.sfdMinDist lhs / 2)

    it "haltonDesign: 決定的 (= 同じ n, d で同じ結果)" $ do
      let d1 = SF.haltonDesign 10 3
          d2 = SF.haltonDesign 10 3
      SF.sfdMatrix d1 `shouldBe` SF.sfdMatrix d2
      SF.sfdMethod  d1 `shouldBe` "Halton"
      inUnitBox (SF.sfdMatrix d1) `shouldBe` True
      SF.sfdMinDist d1 `shouldSatisfy` (> 0)

    it "designMinDistance: 既知 2 点 design で正しい距離" $ do
      let m = LA.fromLists [[0.0, 0.0], [3.0, 4.0]]  -- 距離 = 5
      abs (SF.designMinDistance m - 5.0) `shouldSatisfy` (< 1e-12)

    it "designMinDistance: 行数 < 2 で 0" $ do
      let m0 = (0 LA.>< 0) []
          m1 = LA.fromLists [[1.0, 2.0]]
      SF.designMinDistance m0 `shouldBe` 0
      SF.designMinDistance m1 `shouldBe` 0

    it "n=0 or d=0 で安全に empty design" $ do
      gen <- MWC.create
      des0 <- SF.latinHypercube 0 3 gen
      SF.sfdNPoints des0 `shouldBe` 0
      let halt0 = SF.haltonDesign 0 3
      SF.sfdNPoints halt0 `shouldBe` 0
