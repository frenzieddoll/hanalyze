{-# LANGUAGE OverloadedStrings #-}
-- | 'Hanalyze.Stat.Descriptive' のテスト。
--   R の @mean@/@median@/@var@/@sd@/@IQR@/@quantile(type=7)@ と数値一致を確認する
--   (Phase 65)。 参照値は R 4.x で算出。
module Hanalyze.Stat.DescriptiveSpec (spec) where

import Test.Hspec
import qualified Data.Vector.Storable as VS
import qualified Hanalyze.Stat.Descriptive as D

-- 近似一致
near :: Double -> Double -> Bool
near a b = abs (a - b) < 1e-9

spec :: Spec
spec = describe "Hanalyze.Stat.Descriptive" $ do
  -- x <- c(1,2,3,5,7,11,13); R: mean=6, median=5, var=22.66667, sd=4.760953, IQR=7
  let x  = VS.fromList [1,2,3,5,7,11,13] :: VS.Vector Double
      xl = [1,2,3,5,7,11,13] :: [Double]

  describe "中心" $ do
    it "mean" $ D.mean x `shouldSatisfy` near 6
    it "median (奇数長)" $ D.median x `shouldSatisfy` near 5
    it "median (偶数長 = 中央 2 点平均)" $
      D.median (VS.fromList [1,2,3,4] :: VS.Vector Double) `shouldSatisfy` near 2.5

  describe "散布 (R var/sd = n-1)" $ do
    -- R: var(x) = 21 (= 126/6・偏差平方和 126), sd = 4.582576
    it "variance (不偏・n-1)" $ D.variance x `shouldSatisfy` near 21
    it "sd" $ D.sd x `shouldSatisfy` near (sqrt 21)
    it "range'" $ D.range' x `shouldSatisfy` near 12

  describe "分位点 (R type-7)" $ do
    -- R quantile(x, c(0,.25,.5,.75,.95,1), type=7):
    --   0% 1 / 25% 2.5 / 50% 5 / 75% 9 / 95% 12.4 / 100% 13
    it "p=0   → 最小" $ D.quantile 0    x `shouldSatisfy` near 1
    it "p=.25 → 2.5"  $ D.quantile 0.25 x `shouldSatisfy` near 2.5
    it "p=.5  → median" $ D.quantile 0.5  x `shouldSatisfy` near 5
    it "p=.75 → 9"    $ D.quantile 0.75 x `shouldSatisfy` near 9
    it "p=.95 → 12.4" $ D.quantile 0.95 x `shouldSatisfy` near 12.4
    it "p=1   → 最大" $ D.quantile 1    x `shouldSatisfy` near 13
    it "IQR = q75 - q25 = 6.5" $ D.iqr x `shouldSatisfy` near 6.5
    it "percentile 95 = quantile 0.95" $
      D.percentile 95 x `shouldSatisfy` near (D.quantile 0.95 x)

  describe "[Double] wrapper" $ do
    it "meanL ≡ mean" $ D.meanL xl `shouldSatisfy` near (D.mean x)
    it "quantileL ≡ quantile" $ D.quantileL 0.95 xl `shouldSatisfy` near 12.4

  describe "空・単一要素" $ do
    let e = VS.empty :: VS.Vector Double
    it "mean 空 = NaN" $ isNaN (D.mean e) `shouldBe` True
    it "variance 単一 = NaN" $ isNaN (D.variance (VS.fromList [3])) `shouldBe` True
    it "quantile 単一 = その値" $ D.quantile 0.7 (VS.fromList [42]) `shouldSatisfy` near 42
