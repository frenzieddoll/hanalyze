{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Model.PCASpec (spec) where

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
import qualified Hanalyze.Model.PCA         as PCA
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Model.PCA" $ do
    it "PCA on rank-1 matrix: 1st component explains ~100% var" $ do
      let -- Rank-1: each row = scalar × [1, 2, 3]
          ks = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
          xs = LA.fromLists [[k, 2 * k, 3 * k] | k <- ks]
          r  = PCA.pca PCA.Center Nothing xs
      head (LA.toList (PCA.pcaExplainedRatio r))
        `shouldSatisfy` (> 0.999)

    it "pcaTransform + pcaInverse は Center mode で全成分なら復元 ≈ x" $ do
      let xs = LA.fromLists [[1, 2, 3], [4, 5, 6], [7, 8, 9], [2, 1, 0]]
          r  = PCA.pca PCA.Center Nothing xs
          scores = PCA.pcaTransform r xs
          recon  = PCA.pcaInverse r scores
          diff   = LA.norm_2 (LA.flatten (xs - recon))
      diff `shouldSatisfy` (< 1e-9)

    it "pcaCumExplained は monotone increasing で max ≤ 1" $ do
      let xs = LA.fromLists [[k, 2*k+1, k*k]
                            | k <- [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0]]
          r  = PCA.pca PCA.Center Nothing xs
          cum = LA.toList (PCA.pcaCumExplained r)
      last cum `shouldSatisfy` (<= 1.0001)
      and (zipWith (<=) cum (tail cum)) `shouldBe` True

    it "CenterScale で各列の SD が 1 に正規化される" $ do
      let xs = LA.fromLists [[1, 100, 0.1], [2, 200, 0.2],
                             [3, 300, 0.3], [4, 400, 0.4]]
          r  = PCA.pca PCA.CenterScale Nothing xs
      -- SD は元データの per-col SD で保存される
      LA.size (PCA.pcaScale r) `shouldBe` 3
      all (> 0) (LA.toList (PCA.pcaScale r)) `shouldBe` True

  -- ===========================================================================
  -- Hanalyze.Stat.ClassMetrics (Phase 3)
  -- ===========================================================================
