{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Design.SequentialSpec (spec) where

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
import qualified Hanalyze.Design.Sequential    as Seq
import qualified Hanalyze.Design.RSM           as RSMd
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Design.Sequential (Phase 7.2)" $ do
    it "steepestAscent: ascent 方向 = +b / |b|" $ do
      let sa = Seq.steepestAscent True [0, 0] [3, 4] 1.0 5
          d = LA.toList (Seq.sarDirection sa)
      -- (3, 4) の単位ベクトル = (0.6, 0.8)
      abs (d !! 0 - 0.6) `shouldSatisfy` (< 1e-12)
      abs (d !! 1 - 0.8) `shouldSatisfy` (< 1e-12)
      length (Seq.sarStepPoints sa) `shouldBe` 6  -- nSteps + 1
      Seq.sarStepPoints sa !! 0 `shouldBe` [0, 0]
      -- step 1 = center + 1 * (0.6, 0.8) = (0.6, 0.8)
      abs (Seq.sarStepPoints sa !! 1 !! 0 - 0.6) `shouldSatisfy` (< 1e-12)

    it "steepestAscent: descent 方向 = -b / |b|" $ do
      let sa = Seq.steepestAscent False [10, 10] [1, 0] 0.5 3
      Seq.sarMaximize sa `shouldBe` False
      let d = LA.toList (Seq.sarDirection sa)
      -- (1, 0) の単位ベクトル = (1, 0)、 descent なので -1, 0
      abs (d !! 0 - (-1.0)) `shouldSatisfy` (< 1e-12)
      abs (d !! 1) `shouldSatisfy` (< 1e-12)
      -- step 2 = (10, 10) + 2 * 0.5 * (-1, 0) = (9, 10)
      abs (Seq.sarStepPoints sa !! 2 !! 0 - 9.0) `shouldSatisfy` (< 1e-12)
      abs (Seq.sarStepPoints sa !! 2 !! 1 - 10.0) `shouldSatisfy` (< 1e-12)

    it "steepestAscent: |b| = 0 なら方向 0、 全点 center" $ do
      let sa = Seq.steepestAscent True [5, 5, 5] [0, 0, 0] 1.0 3
          d = LA.toList (Seq.sarDirection sa)
      all (== 0) d `shouldBe` True
      all (== [5,5,5]) (Seq.sarStepPoints sa) `shouldBe` True

    it "steepestAscentFromQuad: QuadFit から第一階係数を抽出" $ do
      -- 2 因子の簡単な response surface y = 1 + 2*x1 + 3*x2 + ...
      let xs = [[-1,-1], [1,-1], [-1,1], [1,1], [0,0]]
          ys = [ 1 + 2*x1 + 3*x2 | [x1, x2] <- xs ]
          qf = RSMd.fitQuadratic xs ys
          sa = Seq.steepestAscentFromQuad True [0, 0] qf 0.1 4
      -- 第一階係数 (2, 3) → 単位ベクトル (2, 3)/sqrt(13)
      let expectedX = 2 / sqrt 13
          expectedY = 3 / sqrt 13
          d = LA.toList (Seq.sarDirection sa)
      abs (d !! 0 - expectedX) `shouldSatisfy` (< 0.05)  -- numerical fit tolerance
      abs (d !! 1 - expectedY) `shouldSatisfy` (< 0.05)

    it "sequentialCCD: 新中心 + span で coded / real 両方返す" $ do
      let res = Seq.sequentialCCD [10, 20] 2.0 2 RSMd.CCF 1
      Seq.sccdCenter res `shouldBe` [10, 20]
      Seq.sccdSpan res   `shouldBe` 2.0
      -- coded は -1, 0, +1 の組合せ (FaceCentered なので axial も ±1)
      length (Seq.sccdCoded res) `shouldBe` length (Seq.sccdReal res)
      -- real の各行 = center + span * coded
      let checkRow (coded, real) =
            real == zipWith (\c x -> c + 2.0 * x) [10, 20] coded
      all checkRow (zip (Seq.sccdCoded res) (Seq.sccdReal res)) `shouldBe` True

    it "sequentialCCD: factorial part は ±span 周辺" $ do
      let res = Seq.sequentialCCD [50, 50] 5.0 2 RSMd.CCF 1
          realRows = Seq.sccdReal res
      -- 角点は (45,45), (55,45), (45,55), (55,55) を含むはず
      [45, 45] `elem` realRows `shouldBe` True
      [55, 55] `elem` realRows `shouldBe` True
