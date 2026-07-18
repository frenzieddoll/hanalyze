{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Stat.AdaptiveGridSpec (spec) where

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
import qualified Hanalyze.Stat.AdaptiveGrid as AG
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Stat.AdaptiveGrid" $ do
    it "uniformGrid: 端点を含み等間隔" $ do
      let g = AG.uniformGrid 5 0 4
      g `shouldBe` [0, 1, 2, 3, 4]

    it "Adaptive: 急激な変化付近に grid が集中する (step 関数)" $ do
      -- y は z=0..1 でほぼ平坦、z=1..2 で急激に変化、z=2..3 で再び平坦
      let pts1 = [(z, if z < 1 then 0 else if z < 2 then 10*(z-1) else 10) | z <- [0, 0.1 .. 3]]
          spec = (AG.defaultGridSpec 30) { AG.gsKind = AG.Adaptive }
          g    = AG.makeGrid [pts1] (0, 3) spec
          -- 中央領域 [1, 2] にある grid 点数 vs 端領域 [0,1] の grid 点数
          midN = length (filter (\z -> z >= 1 && z <= 2) g)
          edgeN = length (filter (\z -> z < 1) g)
      length g `shouldBe` 30
      midN `shouldSatisfy` (> edgeN)

    it "Uniform: 端点 + 等間隔" $ do
      let g = AG.makeGrid [] (0, 1) ((AG.defaultGridSpec 6) { AG.gsKind = AG.Uniform })
      length g `shouldBe` 6
      head g `shouldBe` 0
      last g `shouldBe` 1

    it "N < 10 で adaptive 指定でも uniform にフォールバック" $ do
      let pts1 = [(z, sin z) | z <- [0, 0.1 .. 3]]
          spec = (AG.defaultGridSpec 5) { AG.gsKind = AG.Adaptive }
          g    = AG.makeGrid [pts1] (0, 3) spec
          gU   = AG.uniformGrid 5 0 3
      g `shouldBe` gU
