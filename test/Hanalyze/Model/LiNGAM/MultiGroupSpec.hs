{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Model.LiNGAM.MultiGroupSpec (spec) where

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
import qualified Hanalyze.Model.LiNGAM.MultiGroup     as LNGM
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Model.LiNGAM.MultiGroup (Phase 36 A6)" $ do
    -- 共通構造 x0 → x1 → x2 を 2 群分作成 (係数は群ごとに微小差)
    let mkGroup n c1 c2 =
          let halton b k = QR.radicalInverse b k - 0.5
              e0s = [halton 2 i | i <- [1..n]]
              e1s = [halton 3 i | i <- [1..n]]
              e2s = [halton 5 i | i <- [1..n]]
              x0s = e0s
              x1s = zipWith (\a b -> c1 * a + b) x0s e1s
              x2s = zipWith3 (\a b c -> c2 * b + c) x0s x1s e2s
          in LA.fromColumns [LA.fromList x0s, LA.fromList x1s, LA.fromList x2s]
    it "2 群の共通 causal order を [0,1,2] と推定" $ do
      let g1 = mkGroup 400 0.8 0.6
          g2 = mkGroup 400 0.7 0.5
          fit = LNGM.fitMultiGroupLiNGAM LNGM.defaultMultiGroupConfig [g1, g2]
      LNGM.mgCommonOrder fit `shouldBe` [0, 1, 2]
