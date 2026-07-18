{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Model.LiNGAM.PairwiseSpec (spec) where

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
import qualified Hanalyze.Model.LiNGAM.Pairwise       as LNGP
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Model.LiNGAM.Pairwise (Phase 36 A3)" $ do
    -- x → y、 noise は uniform 由来非ガウシアン
    let mkPair n =
          let halton b k = QR.radicalInverse b k - 0.5
              xs = LA.fromList [halton 2 i | i <- [1..n]]
              es = LA.fromList [halton 3 i | i <- [1..n]]
              ys = LA.scale 0.8 xs + es
          in (xs, ys)
    it "x → y を XtoY と判定" $ do
      let (x, y) = mkPair 500
          r = LNGP.pairwiseLiNGAM 0 x y
      LNGP.prDirection r `shouldBe` LNGP.XtoY
    it "y → x を YtoX と判定 (入力順を逆転)" $ do
      let (x, y) = mkPair 500
          r = LNGP.pairwiseLiNGAM 0 y x
      LNGP.prDirection r `shouldBe` LNGP.YtoX
