{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Model.SurvivalSpec (spec) where

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
import qualified Hanalyze.Model.Survival     as Surv
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Model.Survival" $ do
    it "kaplanMeier: 全 event observed で S(t) は単調減少" $ do
      let samples = [ Surv.SurvSample t Surv.Observed | t <- [1, 2, 3, 4, 5] ]
          km = Surv.kaplanMeier samples
      length (Surv.kmrTimes km) `shouldBe` 5
      let ss = Surv.kmrSurvival km
      and (zipWith (>=) ss (tail ss)) `shouldBe` True

    it "kaplanMeier: S(t) は前向き累積積の実値と一致 (逆順バグの回帰)" $ do
      -- n=5・各時刻 1 event: S = [4/5, 3/4, 2/3, 1/2, 0] の累積。
      -- 旧実装は右から積んで最終 factor 0 が全時点を 0 に潰していた。
      let samples = [ Surv.SurvSample t Surv.Observed | t <- [1, 2, 3, 4, 5] ]
          ss = Surv.kmrSurvival (Surv.kaplanMeier samples)
          expected = [0.8, 0.6, 0.4, 0.2, 0.0]
      and (zipWith (\a b -> abs (a - b) < 1e-9) ss expected) `shouldBe` True

    it "kaplanMeier: censored data でも非負の生存確率" $ do
      let samples = [ Surv.SurvSample 1 Surv.Observed
                    , Surv.SurvSample 2 Surv.Censored
                    , Surv.SurvSample 3 Surv.Observed
                    , Surv.SurvSample 4 Surv.Observed
                    , Surv.SurvSample 5 Surv.Censored
                    ]
          km = Surv.kaplanMeier samples
      all (>= 0) (Surv.kmrSurvival km) `shouldBe` True
      all (<= 1) (Surv.kmrSurvival km) `shouldBe` True

    it "nelsonAalen: 累積ハザードは monotone non-decreasing" $ do
      let samples = [ Surv.SurvSample t Surv.Observed | t <- [1, 2, 3, 4, 5] ]
          na = Surv.nelsonAalen samples
          h = Surv.narCumHazard na
      and (zipWith (<=) h (tail h)) `shouldBe` True

    it "logRankTest: 同一分布で p > 0.05" $ do
      let g1 = [ Surv.SurvSample t Surv.Observed | t <- [1, 2, 3, 4, 5] ]
          g2 = [ Surv.SurvSample t Surv.Observed | t <- [1, 2, 3, 4, 5] ]
          lr = Surv.logRankTest [g1, g2]
      Surv.lrPValue lr `shouldSatisfy` (> 0.05)

    it "logRankTest: 異なる分布で p < 0.05" $ do
      let g1 = [ Surv.SurvSample t Surv.Observed | t <- [1, 2, 3, 4, 5] ]
          g2 = [ Surv.SurvSample t Surv.Observed | t <- [10, 11, 12, 13, 14] ]
          lr = Surv.logRankTest [g1, g2]
      Surv.lrPValue lr `shouldSatisfy` (< 0.05)

    it "coxPH: 共変量と event 時間に強い相関で β > 0" $ do
      -- x が大きい個体が早く event を起こす想定の合成データ
      let n = 30
          xs = [ LA.fromList [fromIntegral i / fromIntegral n :: Double]
               | i <- [1 .. n] ]
          times = [ 30 - i | i <- [1 .. n] ]   -- x 大きいほど early
          samples = [ Surv.SurvSample (fromIntegral t) Surv.Observed
                    | t <- times ]
          fit = Surv.coxPH xs samples
      LA.atIndex (Surv.coxBeta fit) 0 `shouldSatisfy` (> 0)

  -- ===========================================================================
  -- Hanalyze.Stat.Interpret (Phase 13)
  -- ===========================================================================
