{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Design.DSDSpec (spec) where

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
import qualified Data.Text   as T
import qualified Numeric.LinearAlgebra as LA
import qualified Hanalyze.Design.DSD           as DSD
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Design.DSD (Phase 6.2)" $ do
    it "sanity: k < 2 は Left" $ do
      case DSD.dsdDesign 0 of
        Left _  -> pure ()
        Right _ -> expectationFailure "expected Left for k=0"
      case DSD.dsdDesign 1 of
        Left _  -> pure ()
        Right _ -> expectationFailure "expected Left for k=1"

    it "k=4: verified DSD (Jones-Nachtsheim conference matrix 由来)" $
      case DSD.dsdDesign 4 of
        Left e  -> expectationFailure (T.unpack e)
        Right r -> do
          DSD.dsdNFactors r `shouldBe` 4
          DSD.dsdNRuns r    `shouldBe` 9   -- = 2*4 + 1
          DSD.dsdHasOptimal r `shouldBe` True
          LA.rows (DSD.dsdMatrix r) `shouldBe` 9
          LA.cols (DSD.dsdMatrix r) `shouldBe` 4

    it "k=4: row 0 (center) は全 0" $
      case DSD.dsdDesign 4 of
        Left e  -> expectationFailure (T.unpack e)
        Right r -> do
          let row0 = LA.toList (LA.flatten (LA.tr (DSD.dsdMatrix r) LA.¿ [0]))
          row0 `shouldBe` replicate 4 0.0

    it "k=4: 各 row (center 除く) は値域 {-1, 0, +1}" $
      case DSD.dsdDesign 4 of
        Left e  -> expectationFailure (T.unpack e)
        Right r ->
          all (\v -> v == -1 || v == 0 || v == 1)
              (LA.toList (LA.flatten (DSD.dsdMatrix r))) `shouldBe` True

    it "k=4: 各 row (center 除く) に 0 が ちょうど 1 個" $
      case DSD.dsdDesign 4 of
        Left e  -> expectationFailure (T.unpack e)
        Right r -> do
          let rs = LA.toLists (DSD.dsdMatrix r)
              nonCenter = tail rs  -- skip row 0
              zeroCount row = length (filter (== 0) row)
          all (\row -> zeroCount row == 1) nonCenter `shouldBe` True

    it "k=4: foldover 構造 (row i+k = -row i for i in 1..k)" $
      case DSD.dsdDesign 4 of
        Left e  -> expectationFailure (T.unpack e)
        Right r -> do
          let rs = LA.toLists (DSD.dsdMatrix r)
              -- rows 1..4 と rows 5..8 が foldover ペア
              forward  = take 4 (drop 1 rs)
              backward = take 4 (drop 5 rs)
              negated  = map (map negate) forward
          backward `shouldBe` negated

    it "k=4: conference matrix 検証 C·Cᵀ = (n-1)·I (verified DSD のみ)" $
      case DSD.dsdDesign 4 of
        Left e  -> expectationFailure (T.unpack e)
        Right r -> do
          -- conference matrix = rows 1..4 of dsdMatrix
          let cMat = (DSD.dsdMatrix r) LA.? [1, 2, 3, 4]
              prod = cMat LA.<> LA.tr cMat
              -- (n-1) * I_4 should be diag(3, 3, 3, 3)
              expected = LA.scale 3 (LA.ident 4)
              diff = prod - expected
          LA.maxElement (LA.cmap abs diff) `shouldSatisfy` (< 1e-12)

    it "k=6: structural DSD (Hadamard-like 近似)" $
      case DSD.dsdDesign 6 of
        Left e  -> expectationFailure (T.unpack e)
        Right r -> do
          DSD.dsdNFactors r `shouldBe` 6
          DSD.dsdNRuns r    `shouldBe` 13  -- = 2*6 + 1
          DSD.dsdHasOptimal r `shouldBe` False  -- conference matrix では無い
          let rs = LA.toLists (DSD.dsdMatrix r)
          -- center + 2k=12 rows
          length rs `shouldBe` 13
          -- 各非中心行に 0 が 1 個
          all (\row -> length (filter (== 0) row) == 1) (tail rs) `shouldBe` True

    it "structural DSD の foldover 構造維持" $
      case DSD.dsdDesign 8 of
        Left e  -> expectationFailure (T.unpack e)
        Right r -> do
          DSD.dsdNRuns r `shouldBe` 17
          let rs = LA.toLists (DSD.dsdMatrix r)
              forward  = take 8 (drop 1 rs)
              backward = take 8 (drop 9 rs)
          backward `shouldBe` map (map negate) forward
