{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Model.LiNGAM.BootstrapSpec (spec) where

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
import qualified Hanalyze.Stat.QuasiRandom  as QR
import qualified Hanalyze.Model.LiNGAM.Bootstrap      as LNGB
import qualified Hanalyze.Model.DAG                   as DAG
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Model.LiNGAM.Bootstrap (Phase 36 A2)" $ do
    let mkDataN n =
          let halton b k = QR.radicalInverse b k - 0.5
              e0s = [halton 2 i | i <- [1..n]]
              e1s = [halton 3 i | i <- [1..n]]
              e2s = [halton 5 i | i <- [1..n]]
              x0s = e0s
              x1s = zipWith (\a b -> 0.8 * a + b) x0s e1s
              x2s = zipWith3 (\a b c -> 0.4 * a + 0.6 * b + c) x0s x1s e2s
          in LA.fromColumns [LA.fromList x0s, LA.fromList x1s, LA.fromList x2s]
    it "fitBootstrapLiNGAM: 真エッジの出現頻度が高 (>= 0.7)、 偽エッジは低" $ do
      let cfg = LNGB.defaultBootstrapConfig { LNGB.bcNumBootstraps = 30 }
      res <- LNGB.fitBootstrapLiNGAM cfg (mkDataN 400)
      -- 真エッジ: x1 ← x0、 x2 ← x0、 x2 ← x1
      let probMat = LNGB.brEdgeProbability res
      LA.atIndex probMat (1, 0) `shouldSatisfy` (>= 0.7)
      LA.atIndex probMat (2, 1) `shouldSatisfy` (>= 0.7)
      -- 偽エッジ: 逆方向 x0 ← x1 等は低頻度
      LA.atIndex probMat (0, 1) `shouldSatisfy` (<= 0.3)
    it "confidenceDAG: 高頻度エッジのみ採用、 acyclic 維持" $ do
      let cfg = LNGB.defaultBootstrapConfig { LNGB.bcNumBootstraps = 30 }
      res <- LNGB.fitBootstrapLiNGAM cfg (mkDataN 400)
      let g = LNGB.confidenceDAG 0.7 0.8 res
      DAG.isAcyclic g `shouldBe` True
    it "signConsistency: 真エッジは符号合致率高い" $ do
      let cfg = LNGB.defaultBootstrapConfig { LNGB.bcNumBootstraps = 30 }
      res <- LNGB.fitBootstrapLiNGAM cfg (mkDataN 400)
      let sigMat = LNGB.brSignConsistency res
      -- 真エッジは符号合致率 1 に近い
      LA.atIndex sigMat (1, 0) `shouldSatisfy` (>= 0.9)
      LA.atIndex sigMat (2, 1) `shouldSatisfy` (>= 0.9)
