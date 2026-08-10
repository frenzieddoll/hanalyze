{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Stat.MDSSpec (spec) where

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
import qualified Hanalyze.Stat.MDS             as MDS
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Stat.MDS (Phase 34-A3)" $ do
    -- 2-D 配列 → 距離行列 → 2-D 再構成で距離保存を確認
    let pts2d = LA.fromLists [[0, 0], [1, 0], [0, 1], [1, 1], [2, 0.5]]
        d     = MDS.euclideanDist pts2d
    it "mdsClassical: 2-D データを 2-D 復元すると距離が保たれる" $ do
      let emb = MDS.mdsClassical d 2
          d2  = MDS.euclideanDist emb
          err = LA.maxElement (LA.cmap abs (d - d2))
      err `shouldSatisfy` (< 1e-8)
    it "mdsClassical: 出力 shape は (n × k)" $ do
      let emb = MDS.mdsClassical d 2
      LA.rows emb `shouldBe` 5
      LA.cols emb `shouldBe` 2
    it "euclideanDist: 対角は 0、 対称" $ do
      let n = 5
      and [ abs (LA.atIndex d (i, i)) < 1e-10 | i <- [0 .. n - 1] ]
        `shouldBe` True
      and [ abs (LA.atIndex d (i, j) - LA.atIndex d (j, i)) < 1e-10
          | i <- [0 .. n - 1], j <- [0 .. n - 1] ]
        `shouldBe` True
    it "mdsSammon: stress が古典 MDS より同等以下" $ do
      let embC = MDS.mdsClassical d 2
          embS = MDS.mdsSammon MDS.defaultSammonConfig d 2
          sC   = MDS.sammonStress d embC
          sS   = MDS.sammonStress d embS
      sS `shouldSatisfy` (<= sC + 1e-8)
