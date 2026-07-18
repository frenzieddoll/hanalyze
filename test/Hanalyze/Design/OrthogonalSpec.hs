{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Design.OrthogonalSpec (spec) where

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
import qualified Hanalyze.Design.Orthogonal as OA
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Design.Orthogonal" $ do
    it "L4 has 4 runs and 3 columns" $ do
      OA.oaRuns OA.l4    `shouldBe` 4
      OA.oaFactors OA.l4 `shouldBe` 3
      length (OA.oaTable OA.l4) `shouldBe` 4

    it "L8 has 8 runs and 7 columns" $ do
      OA.oaRuns OA.l8    `shouldBe` 8
      OA.oaFactors OA.l8 `shouldBe` 7

    it "L9 has 9 runs and 4 columns at 3 levels each" $ do
      OA.oaRuns OA.l9    `shouldBe` 9
      OA.oaFactors OA.l9 `shouldBe` 4
      OA.oaLevels OA.l9  `shouldBe` [3, 3, 3, 3]

    it "L18 has 18 runs and 8 columns (1×2 + 7×3)" $ do
      OA.oaRuns OA.l18    `shouldBe` 18
      OA.oaLevels OA.l18  `shouldBe` 2 : replicate 7 3

    it "L27 has 27 runs and 13 columns at 3 levels each" $ do
      OA.oaRuns OA.l27    `shouldBe` 27
      OA.oaFactors OA.l27 `shouldBe` 13
      OA.oaLevels OA.l27  `shouldBe` replicate 13 3

    it "L27 columns are balanced (each level appears 9 times)" $ do
      let table = OA.oaTable OA.l27
          colJ j = [ row !! j | row <- table ]
      mapM_ (\j -> mapM_ (\l ->
        length (filter (== l) (colJ j)) `shouldBe` 9) [1,2,3]) [0 .. 12]

    it "L27 column pairs are strength-2 orthogonal (each of 9 combos appears 3 times)" $ do
      let table = OA.oaTable OA.l27
          colJ j = [ row !! j | row <- table ]
          pairCount j1 j2 a b =
            length (filter id (zipWith (\x y -> x == a && y == b)
                                       (colJ j1) (colJ j2)))
      mapM_ (\(j1, j2) -> mapM_ (\(a, b) ->
        pairCount j1 j2 a b `shouldBe` 3)
        [ (a, b) | a <- [1,2,3], b <- [1,2,3] ])
        [ (j1, j2) | j1 <- [0 .. 12], j2 <- [j1 + 1 .. 12] ]

    it "lookupOA finds L27" $
      OA.oaName <$> OA.lookupOA "L27" `shouldBe` Just "L27(3^13)"

    it "L8 columns are balanced (each level appears 4 times)" $ do
      let table = OA.oaTable OA.l8
          colJ j = [ row !! j | row <- table ]
      mapM_ (\j -> do
        let cs = colJ j
        length (filter (== 1) cs) `shouldBe` 4
        length (filter (== 2) cs) `shouldBe` 4) [0 .. 6]

    it "L8 column pairs are pairwise orthogonal" $ do
      let table = OA.oaTable OA.l8
          colJ j = [ row !! j | row <- table ]
          pairCount j1 j2 a b =
            length (filter id (zipWith (\x y -> x == a && y == b)
                                       (colJ j1) (colJ j2)))
      -- For 2-level orthogonality: each pair (1,1)/(1,2)/(2,1)/(2,2) must appear equally
      mapM_ (\(j1, j2) -> do
        pairCount j1 j2 1 1 `shouldBe` 2
        pairCount j1 j2 1 2 `shouldBe` 2
        pairCount j1 j2 2 1 `shouldBe` 2
        pairCount j1 j2 2 2 `shouldBe` 2)
        [(0,1),(0,2),(0,3),(1,2),(1,3),(2,3)]

    it "lookupOA finds standard arrays case-insensitively" $ do
      OA.oaName <$> OA.lookupOA "L9"   `shouldBe` Just "L9(3^4)"
      OA.oaName <$> OA.lookupOA "l9"   `shouldBe` Just "L9(3^4)"
      OA.oaName <$> OA.lookupOA "L99"  `shouldBe` Nothing

    it "assignFactors fills levels correctly for L4" $ do
      let specs = [ OA.FactorSpec "A" [OA.LText "lo", OA.LText "hi"]
                  , OA.FactorSpec "B" [OA.LNumeric 0,   OA.LNumeric 1]
                  ]
      case OA.assignFactors OA.l4 specs of
        Right ad -> do
          length (OA.adRows ad) `shouldBe` 4
          map length (OA.adRows ad) `shouldBe` [2, 2, 2, 2]
        Left e -> expectationFailure (show e)

    it "assignFactors rejects too many factors" $ do
      let specs = replicate 5 (OA.FactorSpec "X" [OA.LNumeric 1, OA.LNumeric 2])
      OA.assignFactors OA.l4 specs `shouldSatisfy`
        \r -> case r of { Left _ -> True; Right _ -> False }

    it "assignFactors rejects level count mismatch" $ do
      let specs = [ OA.FactorSpec "A" [OA.LText "x"] ]   -- only 1 level, L4 needs 2
      OA.assignFactors OA.l4 specs `shouldSatisfy`
        \r -> case r of { Left _ -> True; Right _ -> False }

  -- ─────────────────────────────────────────────────────────────────────

  describe "Hanalyze.Design.Orthogonal.listArraysWithSize" $ do
    it "returns one entry per standard array" $
      length OA.listArraysWithSize `shouldBe` length OA.standardArrays

    it "L9 entry exposes runs / factors / levels" $ do
      let l9meta = head [ m | m <- OA.listArraysWithSize
                            , OA.omName m == OA.oaName OA.l9 ]
      OA.omRuns l9meta    `shouldBe` 9
      OA.omFactors l9meta `shouldBe` 4
      OA.omLevels l9meta  `shouldBe` [3, 3, 3, 3]

  -- ─────────────────────────────────────────────────────────────────────
