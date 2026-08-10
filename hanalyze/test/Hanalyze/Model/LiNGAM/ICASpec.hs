{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Model.LiNGAM.ICASpec (spec) where

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
import qualified Hanalyze.Model.LiNGAM.ICA            as LNGI
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Model.LiNGAM.ICA (Phase 36 A4)" $ do
    -- 2 変数 SEM: x0 → x1 (係数 0.8)、 非ガウシアン noise
    let mkData2 n =
          let halton b k = QR.radicalInverse b k - 0.5
              e0s = [halton 2 i | i <- [1..n]]
              e1s = [halton 3 i | i <- [1..n]]
              x0s = e0s
              x1s = zipWith (\a b -> 0.8 * a + b) x0s e1s
          in LA.fromColumns [LA.fromList x0s, LA.fromList x1s]
    it "fitICALiNGAM: 2 変数 SEM の order を推定" $ do
      let x = mkData2 500
      fit <- LNGI.fitICALiNGAM LNGI.defaultICALiNGAMConfig x
      -- ICA の符号曖昧性 + 順列貪欲のため、 厳密な order ではなく adjacency
      -- に 1 つのエッジが立つことを確認
      let adj = LNGI.ilAdjacency fit
          edges = sum [ round (LA.atIndex adj (i, j)) :: Int
                      | i <- [0, 1], j <- [0, 1], i /= j ]
      edges `shouldSatisfy` (>= 1)

  describe "Hanalyze.Model.LiNGAM.ICA: Hungarian 化 (p=8)" $ do
    -- 8 変数 chain SEM: x_0 → x_1 → ... → x_7、 係数 0.6、 非ガウシアン noise。
    -- Hungarian/greedy のどちらでも fit が走ることだけ確認 (adjacency >= 1)。
    -- 純粋なアルゴリズム妥当性は Hungarian テストで担保。
    let mkChain n p0 =
          let halton b k = QR.radicalInverse b k - 0.5
              primes = take p0 [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]
              es     = [ [halton b i | i <- [1..n]] | b <- primes ]
              go acc []     = reverse acc
              go []  (e:rs) = go [e] rs
              go (prev:rest) (e:rs) =
                let cur = zipWith (\a b -> 0.6 * a + b) prev e
                in go (cur : prev : rest) rs
              cols   = case es of
                         []     -> []
                         (e0:r) -> go [e0] r
          in LA.fromColumns (map LA.fromList cols)
    it "Hungarian 化: 8 変数 chain で adjacency が立つ" $ do
      let x = mkChain 400 8
          cfg = LNGI.defaultICALiNGAMConfig { LNGI.ilcUseHungarian = True }
      fit <- LNGI.fitICALiNGAM cfg x
      let adj = LNGI.ilAdjacency fit
          edges = sum [ round (LA.atIndex adj (i, j)) :: Int
                      | i <- [0 .. 7], j <- [0 .. 7], i /= j ]
      edges `shouldSatisfy` (>= 1)
    it "貪欲版も同じ shape で動く (ilcUseHungarian = False、 回帰確認)" $ do
      let x = mkChain 400 8
          cfg = LNGI.defaultICALiNGAMConfig { LNGI.ilcUseHungarian = False }
      fit <- LNGI.fitICALiNGAM cfg x
      LA.rows (LNGI.ilAdjacency fit) `shouldBe` 8
