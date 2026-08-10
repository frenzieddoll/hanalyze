{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Model.ReliabilitySpec (spec) where

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
import qualified Hanalyze.Model.Reliability    as Rel
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Model.Reliability.fitArrhenius (Phase 2.5)" $ do
    it "sanity: 空入力は Left" $
      case Rel.fitArrhenius [] of
        Left _  -> pure ()
        Right _ -> expectationFailure "expected Left for empty"

    it "sanity: 1 温度のみは Left" $
      case Rel.fitArrhenius [(300.0, [1000, 1100, 950])] of
        Left _  -> pure ()
        Right _ -> expectationFailure "expected Left for single temperature"

    it "sanity: 非正の温度 / 寿命は Left" $ do
      case Rel.fitArrhenius [(300.0, [1000]), (350.0, [-500])] of
        Left _  -> pure ()
        Right _ -> expectationFailure "expected Left for negative lifetime"
      case Rel.fitArrhenius [(0.0, [1000]), (350.0, [500])] of
        Left _  -> pure ()
        Right _ -> expectationFailure "expected Left for T=0"

    it "正確データでパラメータが復元される (真 A=1e-6 s, Ea=0.7 eV)" $ do
      let trueA  = 1e-6
          trueEa = 0.7    -- eV
          predict t = trueA * exp (trueEa / Rel.kBoltzmann / t)
          temps  = [298.15, 323.15, 348.15, 373.15, 398.15]  -- K
          input  = [ (t, [predict t]) | t <- temps ]
      case Rel.fitArrhenius input of
        Left e  -> expectationFailure (T.unpack e)
        Right f -> do
          -- 完全データなので E_a がぴったり一致
          abs (Rel.afEa f - trueEa) `shouldSatisfy` (< 1e-9)
          -- A は exp(b0) で recover; 数値誤差は < 0.1%
          abs (Rel.afA f - trueA) / trueA `shouldSatisfy` (< 1e-6)
          Rel.afN f `shouldBe` length temps

    it "ノイズあり 5 温度 × 3 replicate で Ea が真値の ±15% に入る" $ do
      let trueA  = 1e-8
          trueEa = 0.5    -- eV
          predict t = trueA * exp (trueEa / Rel.kBoltzmann / t)
          temps  = [298.15, 323.15, 348.15, 373.15, 398.15]
          -- 乗法ノイズ: ±10% を確定的に振る
          noises = [0.95, 1.0, 1.05]
          input  = [ (t, [predict t * f | f <- noises]) | t <- temps ]
      case Rel.fitArrhenius input of
        Left e  -> expectationFailure (T.unpack e)
        Right f -> do
          abs (Rel.afEa f - trueEa) / trueEa `shouldSatisfy` (< 0.15)
          Rel.afN f `shouldBe` length temps * length noises

    it "accelerationFactor: 試験温度の方が高ければ AF > 1" $ do
      let trueA  = 1e-6
          trueEa = 0.7
          predict t = trueA * exp (trueEa / Rel.kBoltzmann / t)
          input = [ (t, [predict t]) | t <- [298.15, 348.15, 398.15] ]
      case Rel.fitArrhenius input of
        Left e  -> expectationFailure (T.unpack e)
        Right f -> do
          -- 試験 125°C で使用 25°C
          let af = Rel.accelerationFactor f 298.15 398.15
          af `shouldSatisfy` (> 1)
          -- 同じ温度なら AF = 1
          abs (Rel.accelerationFactor f 350 350 - 1) `shouldSatisfy` (< 1e-12)

  describe "Hanalyze.Model.Reliability.fitEyring (Phase 2.6)" $ do
    it "sanity: 空入力は Left" $
      case Rel.fitEyring [] of
        Left _  -> pure ()
        Right _ -> expectationFailure "expected Left"

    it "sanity: 3 (T, S) ペア未満は Left" $
      case Rel.fitEyring [(300.0, 1.0, [1000, 1100]), (350.0, 1.0, [500])] of
        Left _  -> pure ()
        Right _ -> expectationFailure "expected Left for < 3 distinct (T, S)"

    it "正確データでパラメータ復元 (A=1e-6, Ea=0.7 eV, B=0.5)" $ do
      let trueA  = 1e-6
          trueEa = 0.7
          trueB  = 0.5
          predict t s = trueA / t * exp (trueEa / Rel.kBoltzmann / t) * exp (trueB * s)
          tsPairs = [(t, s) | t <- [298.15, 348.15, 398.15], s <- [0.5, 1.0, 1.5]]
          input = [ (t, s, [predict t s]) | (t, s) <- tsPairs ]
      case Rel.fitEyring input of
        Left e  -> expectationFailure (T.unpack e)
        Right f -> do
          abs (Rel.efEa f - trueEa) `shouldSatisfy` (< 1e-9)
          abs (Rel.efB  f - trueB)  `shouldSatisfy` (< 1e-9)
          abs (Rel.efA  f - trueA) / trueA `shouldSatisfy` (< 1e-6)
          Rel.efN f `shouldBe` length tsPairs

  describe "Hanalyze.Model.Reliability.fitInversePower (Phase 2.6)" $ do
    it "sanity: 空入力 / 1 stress / 非正値は Left" $ do
      case Rel.fitInversePower [] of
        Left _ -> pure ()
        Right _ -> expectationFailure "expected Left for empty"
      case Rel.fitInversePower [(10.0, [1000, 1100])] of
        Left _ -> pure ()
        Right _ -> expectationFailure "expected Left for single stress"
      case Rel.fitInversePower [(10.0, [1000]), (20.0, [-100])] of
        Left _ -> pure ()
        Right _ -> expectationFailure "expected Left for negative lifetime"

    it "正確データでパラメータ復元 (A=1e6, n=2)" $ do
      let trueA = 1e6
          trueN = 2.0
          predict s = trueA * s ** (- trueN)
          stresses = [10, 20, 50, 100, 200]
          input = [ (s, [predict s]) | s <- stresses ]
      case Rel.fitInversePower input of
        Left e  -> expectationFailure (T.unpack e)
        Right f -> do
          abs (Rel.ipfN f - trueN) `shouldSatisfy` (< 1e-9)
          abs (Rel.ipfA f - trueA) / trueA `shouldSatisfy` (< 1e-6)
          Rel.ipfNobs f `shouldBe` length stresses

    it "ノイズあり ±5% で n が真値の ±10% 内" $ do
      let trueA = 1e6
          trueN = 2.0
          predict s = trueA * s ** (- trueN)
          stresses = [10, 20, 50, 100, 200]
          noises = [0.95, 1.0, 1.05]
          input = [ (s, [predict s * f | f <- noises]) | s <- stresses ]
      case Rel.fitInversePower input of
        Left e  -> expectationFailure (T.unpack e)
        Right f -> abs (Rel.ipfN f - trueN) / trueN `shouldSatisfy` (< 0.10)
