{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Stat.GroupComparisonSpec (spec) where

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
import qualified Data.Vector as V
import qualified Data.Text   as T
import qualified Hanalyze.Stat.GroupComparison as GC
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Stat.GroupComparison.goodVsBad (Phase 4.2)" $ do
    it "sanity: 空変数 / 空ラベル / 長さ mismatch は Left" $ do
      case GC.goodVsBad [] (V.fromList [True, False]) of
        Left _ -> pure ()
        Right _ -> expectationFailure "expected Left for empty vars"
      case GC.goodVsBad [("a", V.fromList [1.0, 2.0])] V.empty of
        Left _ -> pure ()
        Right _ -> expectationFailure "expected Left for empty labels"
      case GC.goodVsBad
             [("a", V.fromList [1.0, 2.0, 3.0])]
             (V.fromList [True, False]) of
        Left _ -> pure ()
        Right _ -> expectationFailure "expected Left for length mismatch"

    it "sanity: 群サイズ < 2 は Left" $
      case GC.goodVsBad
             [("a", V.fromList [1.0, 2.0, 3.0])]
             (V.fromList [True, False, False]) of  -- nG=1
        Left _ -> pure ()
        Right _ -> expectationFailure "expected Left for nG<2"

    it "差が無いデータでは effect ≈ 0、 p-value 高い" $ do
      let labels = V.fromList (concat (replicate 5 [True, False]))
          -- 両群同じ分布 (微小ばらつきあり)
          vals   = V.fromList [1.0, 1.1, 0.9, 1.0, 1.1, 1.0, 1.0, 0.9, 1.1, 1.0]
      case GC.goodVsBad [("x", vals)] labels of
        Left e -> expectationFailure (T.unpack e)
        Right [r] -> do
          abs (GC.gcrEffect r) `shouldSatisfy` (< 1.0)  -- 小さい effect
          GC.gcrPValue r `shouldSatisfy` (> 0.1)        -- 有意ではない
        Right _ -> expectationFailure "expected single result"

    it "大きな差のデータでは |effect| が大きい、 p-value 小さい" $ do
      let labels = V.fromList (replicate 10 True ++ replicate 10 False)
          vals   = V.fromList ([1, 1, 2, 2, 1, 1, 2, 2, 1, 2]
                            ++ [10, 11, 12, 10, 11, 12, 10, 11, 12, 11])
      case GC.goodVsBad [("x", vals)] labels of
        Left e -> expectationFailure (T.unpack e)
        Right [r] -> do
          abs (GC.gcrEffect r) `shouldSatisfy` (> 3.0)  -- huge effect
          GC.gcrPValue r `shouldSatisfy` (< 1e-6)
          GC.gcrMeanG r `shouldSatisfy` (\v -> v < 2.0)
          GC.gcrMeanB r `shouldSatisfy` (\v -> v > 9.0)
          GC.gcrNG r `shouldBe` 10
          GC.gcrNB r `shouldBe` 10
        Right _ -> expectationFailure "expected single result"

    it "複数変数の場合、 効果量絶対値降順でソート" $ do
      let labels = V.fromList (replicate 10 True ++ replicate 10 False)
          -- v1: large diff (Good ≈ 1、 Bad ≈ 10)
          v1 = V.fromList ([1, 1.1, 0.9, 1, 1.1, 1, 0.9, 1, 1.1, 1]
                        ++ [10, 10.1, 9.9, 10, 10.1, 10, 9.9, 10, 10.1, 10])
          -- v2: medium diff (Good ≈ 1、 Bad ≈ 3)
          v2 = V.fromList ([1, 1.1, 0.9, 1, 1.1, 1, 0.9, 1, 1.1, 1]
                        ++ [3, 3.1, 2.9, 3, 3.1, 3, 2.9, 3, 3.1, 3])
          -- v3: no diff (両群同じ分布)
          v3 = V.fromList ([1, 1.1, 0.9, 1, 1.1, 1, 0.9, 1, 1.1, 1]
                        ++ [1, 1.1, 0.9, 1, 1.1, 1, 0.9, 1, 1.1, 1])
      case GC.goodVsBad [("v3", v3), ("v1", v1), ("v2", v2)] labels of
        Left e -> expectationFailure (T.unpack e)
        Right rs -> do
          length rs `shouldBe` 3
          -- 並び順は v1 → v2 → v3
          map GC.gcrVarName rs `shouldBe` ["v1", "v2", "v3"]
