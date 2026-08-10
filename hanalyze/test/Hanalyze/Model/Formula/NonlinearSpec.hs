{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Model.Formula.NonlinearSpec (spec) where

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
import qualified DataFrame.Internal.Column    as DX
import qualified DataFrame.Internal.DataFrame  as DX
import SpecHelper

spec :: Spec
spec = do
  describe "非線形フィット NLS (Phase 47 A4)" $ do
    let frm s = case parseFormula s of Right f -> f; Left e -> error e
        xs = [0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5] :: [Double]
        a0 = 3.0; b0 = 0.5
        ys = map (\x -> a0 * exp (negate b0 * x)) xs    -- ノイズなし → 完全 recovery 可
        df = DX.fromNamedColumns [ ("y", DX.fromList ys), ("x", DX.fromList xs) ]
        f  = frm "y x = a * exp(-b * x)"
        lookupP k pm = maybe (0/0) id (lookup k pm)

    it "a*exp(-b*x) のパラメータ復元 (ノイズなし、 init a=1 b=1)" $ do
      let Right r = fitNLS f df [("a",1),("b",1)]
          pm = nlsParams r
      (abs (lookupP "a" pm - a0) < 1e-2 && abs (lookupP "b" pm - b0) < 1e-2)
        `shouldBe` True
    it "ノイズなしで SSR ≈ 0 かつ収束" $ do
      let Right r = fitNLS f df [("a",1),("b",1)]
      (nlsSSR r < 1e-6 && nlsConverged r) `shouldBe` True
    it "初期値が無いパラメータは Left" $
      fitNLS f df [("a",1)] `shouldSatisfy` isLeftE
    it "factor 添字は NLS 非対応 (Left)" $ do
      let dfg = DX.fromNamedColumns
                  [ ("y", DX.fromList ([1,2,3,4]::[Double]))
                  , ("g", DX.fromList (["A","B","A","B"]::[T.Text])) ]
      fitNLS (frm "y g = a * bg ! g") dfg [("a",1),("bg",1)] `shouldSatisfy` isLeftE

  -- ----------------------------------------------------------------------------
  -- A18 R/patsy front-end — 同 AST へ + クロス front-end 等価 (検証点④ の Python 非依存半分)
  -- ----------------------------------------------------------------------------
