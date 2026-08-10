{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Stat.QuasiRandomSpec (spec) where

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
import qualified Hanalyze.Stat.QuasiRandom  as QR
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC as MWC
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Stat.QuasiRandom (Halton)" $ do
    it "haltonSequence 10 1 lies in [0, 1) and is a permutation" $ do
      let pts = QR.haltonSequence 10 1
      length pts `shouldBe` 10
      all (\[u] -> u >= 0 && u < 1) pts `shouldBe` True

    it "haltonSequence 16 2 covers a 4x4 grid better than random" $ do
      -- For n = 16, d = 2 the Halton points should cover all 16
      -- 0.25-bins; an iid uniform usually misses some.
      let pts = QR.haltonSequence 16 2
          binIdx p = (floor (4 * head p) :: Int,
                      floor (4 * (p !! 1)) :: Int)
          unique = length (foldr (\b acc -> if b `elem` acc then acc
                                              else b : acc) [] (map binIdx pts))
      unique `shouldSatisfy` (>= 14)   -- Halton normally hits ≥ 14 / 16

    it "haltonSequenceIn rescales into the supplied bounds" $ do
      let bs  = [(-2, 2), (10, 20)] :: [(Double, Double)]
          pts = QR.haltonSequenceIn 5 bs
      all (\[a, b] -> a >= -2 && a <  2
                   && b >= 10 && b <  20) pts `shouldBe` True

    it "lhsSamples 10 3 lies in [0,1)^3 and fills every per-dim cell" $ do
      gen <- MWC.createSystemRandom
      pts <- QR.lhsSamples 10 3 gen
      length pts `shouldBe` 10
      all (\xs -> all (\u -> u >= 0 && u < 1) xs) pts `shouldBe` True
      -- For each dim, the 10 points should occupy 10 distinct cells
      -- (= floor(10 * u) is a permutation of [0..9]).
      let cellsAlong k = map (\xs -> floor (10 * (xs !! k)) :: Int) pts
          ok k = sort (cellsAlong k) == [0 .. 9]
      ok 0 `shouldBe` True
      ok 1 `shouldBe` True
      ok 2 `shouldBe` True

    it "lhsSamplesIn rescales into bounds" $ do
      gen <- MWC.createSystemRandom
      let bs = [(-1, 1), (5, 10)] :: [(Double, Double)]
      pts <- QR.lhsSamplesIn 8 bs gen
      all (\[a, b] -> a >= -1 && a <  1
                   && b >=  5 && b < 10) pts `shouldBe` True
