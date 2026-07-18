{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Model.WeibullSpec (spec) where

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
import qualified Data.Vector as V
import qualified Data.Text   as T
import qualified Hanalyze.Model.Weibull        as Wei
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Model.Weibull.fitWeibullMLE (Phase 2.2)" $ do
    let -- generate Weibull(k, λ) samples deterministically via inverse CDF
        -- x_i = λ · (−log(1 − u_i))^(1/k), u_i uniform on (0, 1)
        invCDFWeibull k lam u = lam * (- log (1 - u)) ** (1 / k)
        uniforms n = [ (fromIntegral i - 0.5) / fromIntegral n | i <- [1..n] ]

    it "sanity: 入力が空または非正なら Left" $ do
      case Wei.fitWeibullMLE V.empty of
        Left _  -> pure ()
        Right _ -> expectationFailure "expected Left for empty"
      case Wei.fitWeibullMLE (V.fromList [1.0, -2.0, 3.0]) of
        Left _  -> pure ()
        Right _ -> expectationFailure "expected Left for negative input"
      case Wei.fitWeibullMLE (V.fromList [1.0]) of
        Left _  -> pure ()
        Right _ -> expectationFailure "expected Left for n=1"
      case Wei.fitWeibullMLE (V.fromList [2.0, 2.0, 2.0]) of
        Left _  -> pure ()
        Right _ -> expectationFailure "expected Left for constant data"

    it "k=2, λ=10 の合成データから MLE が真値の ±10% に入る" $ do
      let trueK = 2.0
          trueL = 10.0
          xs    = V.fromList [ invCDFWeibull trueK trueL u | u <- uniforms 200 ]
      case Wei.fitWeibullMLE xs of
        Left e  -> expectationFailure (T.unpack e)
        Right f -> do
          Wei.wfShape f `shouldSatisfy` (\k -> abs (k - trueK) / trueK < 0.10)
          Wei.wfScale f `shouldSatisfy` (\l -> abs (l - trueL) / trueL < 0.10)
          Wei.wfRObs  f `shouldBe` V.length xs
          Wei.wfN     f `shouldBe` V.length xs

    it "k=1 (指数分布相当)、 λ=5 の合成データでも収束" $ do
      let trueK = 1.0
          trueL = 5.0
          xs    = V.fromList [ invCDFWeibull trueK trueL u | u <- uniforms 300 ]
      case Wei.fitWeibullMLE xs of
        Left e  -> expectationFailure (T.unpack e)
        Right f -> do
          Wei.wfShape f `shouldSatisfy` (\k -> abs (k - trueK) / trueK < 0.10)
          Wei.wfScale f `shouldSatisfy` (\l -> abs (l - trueL) / trueL < 0.10)

    it "bxLife: 真値 (k=2, λ=10) で B_10 ≈ λ · (−ln 0.9)^(1/k)" $ do
      let trueK = 2.0
          trueL = 10.0
          xs    = V.fromList [ invCDFWeibull trueK trueL u | u <- uniforms 200 ]
          expectedB10 = trueL * (- log 0.9) ** (1 / trueK)
      case Wei.fitWeibullMLE xs of
        Left e  -> expectationFailure (T.unpack e)
        Right f -> Wei.bxLife 0.10 f
                     `shouldSatisfy` (\b -> abs (b - expectedB10) / expectedB10 < 0.15)

    it "weibullParameterSE: 両 SE > 0 で finite" $ do
      let trueK = 2.0
          trueL = 10.0
          xs    = V.fromList [ invCDFWeibull trueK trueL u | u <- uniforms 200 ]
      case Wei.fitWeibullMLE xs of
        Left e  -> expectationFailure (T.unpack e)
        Right f -> do
          let (seK, seL) = Wei.weibullParameterSE f
          seK `shouldSatisfy` (\v -> v > 0 && not (isNaN v) && not (isInfinite v))
          seL `shouldSatisfy` (\v -> v > 0 && not (isNaN v) && not (isInfinite v))

  describe "Hanalyze.Model.Weibull.fitWeibullCensored (Phase 2.3)" $ do
    let invCDFWeibull k lam u = lam * (- log (1 - u)) ** (1 / k)
        uniforms n = [ (fromIntegral i - 0.5) / fromIntegral n | i <- [1..n] ]

    it "sanity: 時間と delta の長さ mismatch は Left" $
      case Wei.fitWeibullCensored
             (V.fromList [1.0, 2.0, 3.0])
             (V.fromList [True, False]) of
        Left  _ -> pure ()
        Right _ -> expectationFailure "expected Left for length mismatch"

    it "sanity: failure 数が 2 未満は Left" $
      case Wei.fitWeibullCensored
             (V.fromList [1.0, 2.0, 3.0])
             (V.fromList [True, False, False]) of
        Left  _ -> pure ()
        Right _ -> expectationFailure "expected Left for r < 2"

    it "全 failure (打ち切り無し) は fitWeibullMLE と同一結果" $ do
      let trueK = 2.0
          trueL = 10.0
          xs = V.fromList [ invCDFWeibull trueK trueL u | u <- uniforms 100 ]
          ds = V.replicate (V.length xs) True
      case (Wei.fitWeibullMLE xs, Wei.fitWeibullCensored xs ds) of
        (Right f1, Right f2) -> do
          abs (Wei.wfShape f1 - Wei.wfShape f2) `shouldSatisfy` (< 1e-8)
          abs (Wei.wfScale f1 - Wei.wfScale f2) `shouldSatisfy` (< 1e-8)
        _ -> expectationFailure "both fits should succeed"

    it "Type-II 打ち切り (大きい時間を打ち切り) で λ̂ が真値に近い" $ do
      let trueK = 2.0
          trueL = 10.0
          -- 200 サンプル中、 上位 30% を打ち切りに
          allXs = [ invCDFWeibull trueK trueL u | u <- uniforms 200 ]
          sortedXs = V.fromList (sort allXs)
          cutoff = V.length sortedXs * 70 `div` 100
          deltas = V.generate (V.length sortedXs) (\i -> i < cutoff)
          -- 打ち切られた点は打ち切り時刻 = cutoff 時刻
          censorTime = sortedXs V.! (cutoff - 1)
          xs = V.imap (\i x -> if i < cutoff then x else censorTime) sortedXs
      case Wei.fitWeibullCensored xs deltas of
        Left e  -> expectationFailure (T.unpack e)
        Right f -> do
          Wei.wfRObs f `shouldBe` cutoff
          Wei.wfN    f `shouldBe` V.length xs
          -- 打ち切り 30% の loss は MLE バイアス小、 ±20% 内に収まる
          Wei.wfShape f `shouldSatisfy` (\k -> abs (k - trueK) / trueK < 0.20)
          Wei.wfScale f `shouldSatisfy` (\l -> abs (l - trueL) / trueL < 0.20)

    it "打ち切り混在: failure < n だが r ≥ 2 で fit 成立" $ do
      -- 故障時間 [1, 2, 3, 4]、 打ち切り時間 [5, 5] (右打ち切り)
      let xs     = V.fromList [1.0, 2.0, 3.0, 4.0, 5.0, 5.0]
          deltas = V.fromList [True, True, True, True, False, False]
      case Wei.fitWeibullCensored xs deltas of
        Left e  -> expectationFailure (T.unpack e)
        Right f -> do
          Wei.wfRObs f `shouldBe` 4
          Wei.wfN    f `shouldBe` 6
          Wei.wfShape f `shouldSatisfy` (> 0)
          Wei.wfScale f `shouldSatisfy` (> 0)

  describe "Hanalyze.Model.Weibull.bxLifeCI + covariance (Phase 2.4)" $ do
    let invCDFWeibull k lam u = lam * (- log (1 - u)) ** (1 / k)
        uniforms n = [ (fromIntegral i - 0.5) / fromIntegral n | i <- [1..n] ]
        synthFit = case Wei.fitWeibullMLE
                         (V.fromList [ invCDFWeibull 2.0 10.0 u | u <- uniforms 200 ]) of
                     Right f -> f
                     Left  e -> error (T.unpack e)

    it "weibullParameterCovariance: var > 0、 cov is finite" $ do
      let (vK, cKL, vL) = Wei.weibullParameterCovariance synthFit
      vK  `shouldSatisfy` (> 0)
      vL  `shouldSatisfy` (> 0)
      cKL `shouldSatisfy` (\v -> not (isNaN v) && not (isInfinite v))

    it "weibullParameterSE と Covariance の対角 sqrt が一致" $ do
      let (vK, _, vL) = Wei.weibullParameterCovariance synthFit
          (sK, sL)    = Wei.weibullParameterSE synthFit
      abs (sK - sqrt vK) `shouldSatisfy` (< 1e-12)
      abs (sL - sqrt vL) `shouldSatisfy` (< 1e-12)

    it "bxLifeCI: estimate は bxLife と一致、 lower ≤ estimate ≤ upper" $ do
      let (est, lo, hi) = Wei.bxLifeCI 0.10 0.05 synthFit
          bp = Wei.bxLife 0.10 synthFit
      abs (est - bp) `shouldSatisfy` (< 1e-12)
      lo `shouldSatisfy` (<= est)
      est `shouldSatisfy` (<= hi)
      lo `shouldSatisfy` (>= 0)  -- lifetimes are non-negative

    it "bxLifeCI: 95% CI が 99% CI より狭い" $ do
      let (_, lo95, hi95) = Wei.bxLifeCI 0.10 0.05 synthFit
          (_, lo99, hi99) = Wei.bxLifeCI 0.10 0.01 synthFit
      (hi95 - lo95) `shouldSatisfy` (< hi99 - lo99)

    it "bxLifeCI: 真値 B_10 が 95% CI に入る (200 サンプル)" $ do
      let trueB10 = 10.0 * (- log 0.9) ** (1 / 2.0)
          (_, lo, hi) = Wei.bxLifeCI 0.10 0.05 synthFit
      trueB10 `shouldSatisfy` (\b -> b >= lo && b <= hi)
