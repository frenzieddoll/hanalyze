{-# LANGUAGE OverloadedStrings #-}
-- | 部分従属 (PDP) / ICE 純粋エンジンのテスト (Phase 75.27)。
--   既知の predict 関数で PDP の解析値・ICE 本数・grid 端点・中心化を検証する。
module Hanalyze.Model.PartialDependenceSpec (spec) where

import           Test.Hspec
import qualified Numeric.LinearAlgebra as LA
import           Hanalyze.Model.PartialDependence

-- 3 行 × 2 列の小さな訓練行列。 列 0 = {0,1,2}, 列 1 = {10,20,30}。
trainX :: LA.Matrix Double
trainX = LA.fromLists [ [0, 10], [1, 20], [2, 30] ]

spec :: Spec
spec =
  describe "Hanalyze.Model.PartialDependence" $ do

    it "線形 predict f = 2*x_j: PDP は grid をそのまま 2 倍 (他列に非依存)" $ do
      let predict m = [ 2 * (row LA.! 0) | row <- LA.toRows m ]   -- 列 0 のみ使う
          r = partialDependence trainX predict 0 3
      pdpGrid r `shouldBe` [0, 1, 2]
      pdpMean r `shouldBe` [0, 2, 4]

    it "加法 f = 3*x0 + 5*x1: 特徴 0 の PDP 傾きは 3, 切片は 5*mean(x1)" $ do
      let predict m = [ 3 * (row LA.! 0) + 5 * (row LA.! 1) | row <- LA.toRows m ]
          r = partialDependence trainX predict 0 3
          -- x1 平均 = 20 → 定数寄与 100。 PDP = 3*grid + 100。
      pdpMean r `shouldBe` [100, 103, 106]

    it "ICE: 曲線本数 = 行数、 各曲線の長さ = grid 数" $ do
      let predict m = [ row LA.! 0 + row LA.! 1 | row <- LA.toRows m ]
          r = partialDependence trainX predict 0 4
      length (pdpIce r) `shouldBe` 3
      map length (pdpIce r) `shouldBe` [4, 4, 4]

    it "grid 端点は注目列の観測 min/max" $ do
      let predict m = replicate (LA.rows m) 0
          r = partialDependence trainX predict 1 5   -- 列 1 = {10,20,30}
      head (pdpGrid r) `shouldBe` 10
      last (pdpGrid r) `shouldBe` 30

    it "PDP = ICE 群の各 grid 列平均に一致" $ do
      let predict m = [ 2 * (row LA.! 0) + (row LA.! 1) | row <- LA.toRows m ]
          r = partialDependence trainX predict 0 3
          colMeans = map (\k -> sum [ c !! k | c <- pdpIce r ] / 3) [0, 1, 2]
      pdpMean r `shouldBe` colMeans

    it "centerICE: 各 ICE 曲線が左端で 0 になる" $ do
      let predict m = [ 3 * (row LA.! 0) + (row LA.! 1) | row <- LA.toRows m ]
          r  = centerICE (partialDependence trainX predict 0 3)
      map head (pdpIce r) `shouldBe` [0, 0, 0]
      -- 中心化後の PDP も左端 0。
      head (pdpMean r) `shouldBe` 0

    it "空行列・列外 index は空結果" $ do
      let predict m = replicate (LA.rows m) 0
      partialDependence (LA.fromLists []) predict 0 5 `shouldBe` PDPResult [] [] []
      partialDependence trainX predict 9 5 `shouldBe` PDPResult [] [] []
