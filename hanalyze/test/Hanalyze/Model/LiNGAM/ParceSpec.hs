{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Model.LiNGAM.ParceSpec (spec) where

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
import qualified Hanalyze.Model.LiNGAM.Parce          as LNGPa
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Model.LiNGAM.Parce (Phase 36 A7)" $ do
    -- 同 SEM データで ParceLiNGAM が DirectLiNGAM と同等の結果を出すか確認
    let mkData n =
          let halton b k = QR.radicalInverse b k - 0.5
              e0s = [halton 2 i | i <- [1..n]]
              e1s = [halton 3 i | i <- [1..n]]
              e2s = [halton 5 i | i <- [1..n]]
              x0s = e0s
              x1s = zipWith (\a b -> 0.8 * a + b) x0s e1s
              x2s = zipWith3 (\a b c -> 0.4 * a + 0.6 * b + c) x0s x1s e2s
          in LA.fromColumns [LA.fromList x0s, LA.fromList x1s, LA.fromList x2s]
    it "fitParceLiNGAM: 潜在交絡なしのデータで causal order を [0,1,2] と推定" $ do
      let x = mkData 500
          fit = LNGPa.fitParceLiNGAM LNGPa.defaultParceConfig x
      LNGPa.pcOrder fit `shouldBe` [0, 1, 2]
      LNGPa.pcUnresolvedGroup fit `shouldBe` []

    -- 潜在交絡: hidden h が x0, x1 を駆動、 x2 は両者の sink
    --   h ~ 非ガウシアン、 x0 = 0.7 h + e0、 x1 = 0.6 h + e1、 x2 = 0.5 x0 + 0.4 x1 + e2
    --   観測 X = [x0, x1, x2] のみ。 v0.2 は x2 を sink と同定、 x0/x1 は
    --   独立性が満たされないため unresolved group に入る想定。
    let mkLatent n =
          let halton b k = QR.radicalInverse b k - 0.5
              hs  = [halton 2 i | i <- [1..n]]
              e0s = [halton 3 i | i <- [1..n]]
              e1s = [halton 5 i | i <- [1..n]]
              e2s = [halton 7 i | i <- [1..n]]
              x0s = zipWith (\h e -> 0.7 * h + e) hs e0s
              x1s = zipWith (\h e -> 0.6 * h + e) hs e1s
              x2s = zipWith3 (\a b c -> 0.5 * a + 0.4 * b + c) x0s x1s e2s
          in LA.fromColumns [LA.fromList x0s, LA.fromList x1s, LA.fromList x2s]
    it "fitParceLiNGAM v0.2: 潜在交絡シナリオで unresolved group に x0/x1 が残る" $ do
      let x   = mkLatent 500
          fit = LNGPa.fitParceLiNGAM LNGPa.defaultParceConfig x
      -- x2 は sink として確定 (末尾)
      last (LNGPa.pcOrder fit) `shouldBe` 2
      -- x0, x1 は順序確定できず unresolved group に入る (順序問わず)
      sort (LNGPa.pcUnresolvedGroup fit) `shouldBe` [0, 1]
