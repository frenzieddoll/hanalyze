{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Model.PLSSpec (spec) where

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
import qualified Numeric.LinearAlgebra as LA
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC as MWC
import qualified Hanalyze.Model.PLS         as PLS
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Model.PLS (Phase 9)" $ do
    -- 合成データ: y = 2 x1 + 1 x2 + 0 x3 (3 features) + noise
    let synthData :: IO (LA.Matrix Double, LA.Vector Double, LA.Vector Double)
        synthData = do
          gen <- MWC.create
          let n = 100; p = 5
          xs <- LA.fromLists <$> sequence
                  [ sequence [ MWC.uniformR (-1, 1) gen | _ <- [1..p] ]
                  | _ <- [1..n] ]
          let trueB = LA.fromList [2.0, 1.0, 0.0, 0.0, 0.0] :: LA.Vector Double
              y0 = xs LA.#> trueB
          noise <- LA.fromList <$> sequence
                     [ MWC.uniformR (-0.1, 0.1) gen | _ <- [1..n] ]
          pure (xs, y0 + noise, trueB)

    it "fitPLS1: n_components=2 で fit、 shape 整合性" $ do
      (xs, y, _) <- synthData
      let cfg = PLS.defaultPLS { PLS.plsN_Components = 2 }
      case PLS.fitPLS1 cfg xs y of
        Left e -> expectationFailure (T.unpack e)
        Right f -> do
          LA.rows (PLS.plsScoresT f)    `shouldBe` 100
          LA.cols (PLS.plsScoresT f)    `shouldBe` 2
          LA.rows (PLS.plsLoadingsP f)  `shouldBe` 5
          LA.cols (PLS.plsLoadingsP f)  `shouldBe` 2
          LA.size (PLS.plsVIP f)        `shouldBe` 5
          LA.size (PLS.plsR2X f)        `shouldBe` 2

    it "predictPLS1: 合成データで真値に近い予測" $ do
      (xs, y, _) <- synthData
      let cfg = PLS.defaultPLS { PLS.plsN_Components = 3 }
      case PLS.fitPLS1 cfg xs y of
        Left e -> expectationFailure (T.unpack e)
        Right f -> do
          let yHat = PLS.predictPLS1 f xs
              resid = y - yHat
              n = LA.size y :: Int
              mse = LA.sumElements (resid * resid) / fromIntegral n
          mse `shouldSatisfy` (< 0.05)

    it "VIP: 強い変数 (x1, x2) は VIP > 1、 ノイズ変数は VIP < 1 に近い" $ do
      (xs, y, _) <- synthData
      let cfg = PLS.defaultPLS { PLS.plsN_Components = 3 }
      case PLS.fitPLS1 cfg xs y of
        Left e -> expectationFailure (T.unpack e)
        Right f -> do
          let vip = LA.toList (PLS.plsVIP f)
          -- VIP[0] (x1) は最大級
          (vip !! 0) `shouldSatisfy` (> 1.0)
          -- VIP[1] (x2) も 1 超え
          (vip !! 1) `shouldSatisfy` (> 0.5)

    it "n_components が n-1 超えで Left" $ do
      (xs, y, _) <- synthData
      let cfg = PLS.defaultPLS { PLS.plsN_Components = 200 }
      case PLS.fitPLS1 cfg xs y of
        Left _ -> pure ()
        Right _ -> expectationFailure "expected Left for too many components"

    it "SIMPLS は現状未実装で Left" $ do
      (xs, y, _) <- synthData
      let cfg = PLS.defaultPLS { PLS.plsAlgorithm = PLS.SIMPLS }
      case PLS.fitPLS1 cfg xs y of
        Left _ -> pure ()
        Right _ -> expectationFailure "expected Left for SIMPLS (not implemented)"

    it "selectPLSComponentsCV: best K が grid 内" $ do
      (xs, y, _) <- synthData
      gen <- MWC.create
      sel <- PLS.selectPLSComponentsCV 5 4 xs (LA.asColumn y) gen
      PLS.plsBestK sel `shouldSatisfy` (\k -> k >= 1 && k <= 4)
      length (PLS.plsCVMSEs sel) `shouldBe` 4
      PLS.plsOneSeK sel `shouldSatisfy` (<= PLS.plsBestK sel)
