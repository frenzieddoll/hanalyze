{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Stat.InterpretSpec (spec) where

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
import qualified Hanalyze.Stat.Interpolate  as Interp
import qualified Hanalyze.Stat.Interpret       as Interp
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC as MWC
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Stat.Interpret" $ do
    it "permutationImportance: 重要 feature が高 importance" $ do
      gen <- MWC.createSystemRandom
      -- y = x_0 のみに依存、x_1 は無関係
      let xs = [[fromIntegral i, fromIntegral (i * i)]
               | i <- [1 .. 20 :: Int]]
          ys = [head row | row <- xs]
          predict zs = [head row | row <- zs]
          score yt yp =
            let mse = sum [(yt !! i - yp !! i) ^ (2 :: Int)
                          | i <- [0 .. length yt - 1]]
            in negate mse  -- higher (= 0) is better
          cfg = (Interp.defaultPermutationConfig)
                  { Interp.pcNRepeats = 5 }
      r <- Interp.permutationImportance cfg predict score xs ys gen
      let imps = Interp.piMeanImportance r
      -- 0番目 feature の importance が高いはず (重要)
      head imps `shouldSatisfy` (> 0.5 * (imps !! 1))

    it "partialDependence: y = x[0] で PDP が grid に追従" $ do
      let xs = [[fromIntegral i, 0.0] | i <- [1..10::Int]]
          predict zs = [head row | row <- zs]
          grid = [1.0, 5.0, 10.0]
          pdp = Interp.partialDependence predict xs 0 grid
      -- 各 grid 点での PD は値そのもの (= grid 値)
      Interp.pdpMeanPredict pdp `shouldBe` grid

    it "icePlot: 全 sample について curve を返す" $ do
      let xs = [[1.0, 2.0], [3.0, 4.0], [5.0, 6.0]]
          predict zs = [sum row | row <- zs]
          grid = [0.0, 1.0, 2.0]
          ice = Interp.icePlot predict xs 0 grid
      length (Interp.iceCurves ice) `shouldBe` 3
      length (Interp.iceFeatureValues ice) `shouldBe` 3
      -- iceMean should equal partial dependence
      length (Interp.iceMean ice) `shouldBe` 3

  -- ─────────────────────────────────────────────────────────────────────
