{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Model.LiNGAM.VARSpec (spec) where

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
import qualified Hanalyze.Model.LiNGAM.VAR            as LNGV
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Model.LiNGAM.VAR (Phase 36 A5)" $ do
    -- VAR(1) + contemporaneous LiNGAM の合成データ
    -- Y_t = A · Y_{t-1} + u_t、 u_t は 2 変数 (x0 → x1) の LiNGAM 構造
    let mkSeries n =
          let halton b k = QR.radicalInverse b k - 0.5
              e0s = [halton 2 i | i <- [1..n]]
              e1s = [halton 3 i | i <- [1..n]]
              -- u_t (同時刻): u0 = e0、 u1 = 0.7 u0 + e1
              u0s = e0s
              u1s = zipWith (\a b -> 0.7 * a + b) u0s e1s
              -- VAR(1): Y_t = 0.3 Y_{t-1} + u_t (両変数とも自己回帰)
              go [_, _]    _    _    acc = reverse acc
              go (u0:u1:rest) y0Prev y1Prev acc =
                let y0 = 0.3 * y0Prev + u0
                    y1 = 0.3 * y1Prev + u1
                in go rest y0 y1 ((y0, y1) : acc)
              go _ _ _ acc = reverse acc
              pairs = go (interleave u0s u1s) 0 0 []
              ys = take (n - 1) (map fst pairs)
              y1ss = take (n - 1) (map snd pairs)
              interleave (a:as) (b:bs) = a : b : interleave as bs
              interleave xs _          = xs
          in LA.fromColumns [LA.fromList ys, LA.fromList y1ss]
    it "fitVARLiNGAM: 同時刻 contemporaneous order を推定" $ do
      let y = mkSeries 500
          fit = LNGV.fitVARLiNGAM LNGV.defaultVARLiNGAMConfig y
      LNGV.vlContempOrder fit `shouldBe` [0, 1]
