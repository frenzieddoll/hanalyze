{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Math.HungarianSpec (spec) where

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
import qualified Data.Vector.Unboxed        as VU
import qualified Hanalyze.Math.Hungarian              as Hung
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Math.Hungarian" $ do
    it "4×4 既知最適: 対角コスト最小割当を当てる" $ do
      -- C[i,j] = 0 (i == j)、 それ以外は 1。 最適は assignment[i] = i、 cost 0。
      let c = LA.build (4, 4) (\i j ->
            if (round i :: Int) == (round j :: Int) then 0.0 else 1.0)
          a = Hung.hungarianMin c
          total = sum [ LA.atIndex c (i, a VU.! i) | i <- [0 .. 3] ]
      VU.toList a `shouldBe` [0, 1, 2, 3]
      total `shouldBe` 0.0

    it "順列が必要な行列でも全単射な最適解を返す" $ do
      -- 既知最適: assignment = [3, 2, 1, 0]、 cost 4。
      let rows = [ [9, 9, 9, 1]
                 , [9, 9, 1, 9]
                 , [9, 1, 9, 9]
                 , [1, 9, 9, 9]
                 ]
          c = LA.fromLists rows :: LA.Matrix Double
          a = Hung.hungarianMin c
          total = sum [ LA.atIndex c (i, a VU.! i) | i <- [0 .. 3] ]
      VU.toList a `shouldBe` [3, 2, 1, 0]
      total `shouldBe` 4.0
      -- 全単射 (重複なし)
      length (VU.toList a) `shouldBe` length (nub (VU.toList a))

    it "1×1 と空行列の境界" $ do
      VU.toList (Hung.hungarianMin (LA.fromLists [[42.0]])) `shouldBe` [0]
      VU.toList (Hung.hungarianMin ((0 LA.>< 0) [])) `shouldBe` []
