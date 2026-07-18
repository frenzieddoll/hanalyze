{-# LANGUAGE OverloadedStrings #-}
module Hanalyze.Model.SVMSpec (spec) where

import Test.Hspec
import qualified Data.Vector.Unboxed   as VU
import qualified Numeric.LinearAlgebra as LA
import           Hanalyze.Model.SVM
import           Hanalyze.Model.Kernel (Kernel (..), KernelParams (..), defaultKernelParams)

-- 線形分離可能な 2 クラス (左下 vs 右上)。
linData :: (LA.Matrix Double, VU.Vector Int)
linData =
  ( LA.fromLists [[0,0],[0,1],[1,0],[4,4],[4,5],[5,4]]
  , VU.fromList  [0,0,0,1,1,1] )

-- 同心 (XOR 風)・線形非分離: 内側 1 クラス・外側 1 クラス。
ringData :: (LA.Matrix Double, VU.Vector Int)
ringData =
  ( LA.fromLists [ [0,0],[0.3,0.2],[-0.2,0.1]            -- 内側 (class 0)
                 , [3,0],[-3,0],[0,3],[0,-3],[2.1,2.1],[-2.1,-2.1] ]  -- 外側 (class 1)
  , VU.fromList  [0,0,0, 1,1,1,1,1,1] )

trainAcc :: SVM -> LA.Matrix Double -> VU.Vector Int -> Double
trainAcc m x y =
  let pred = predictSVM m x
      n    = VU.length y
      ok   = length [ () | i <- [0 .. n - 1], pred VU.! i == y VU.! i ]
  in fromIntegral ok / fromIntegral n

-- γ=1/(2ℓ²) ゆえ γ を ℓ に変換する (共有 Kernel は ℓ ベース・Phase 75.15)。
rbfParams :: Double -> KernelParams
rbfParams gamma = defaultKernelParams { kpLengthScale = sqrt (1 / (2 * gamma)) }

spec :: Spec
spec = describe "Hanalyze.Model.SVM (Phase 75.11/75.15)" $ do
  let (xl, yl) = linData
      (xr, yr) = ringData

  it "Linear カーネル: 線形分離データを訓練精度 100%" $ do
    let m = fitSVM defaultSVM { svmKernel = Linear } xl yl
    trainAcc m xl yl `shouldBe` 1.0

  it "RBF カーネル: 同心 (線形非分離) を訓練精度 100%" $ do
    let m = fitSVM defaultSVM
              { svmKernel = RBF, svmParams = rbfParams 0.5, svmC = 10 } xr yr
    trainAcc m xr yr `shouldBe` 1.0

  it "サポートベクタはスパース (α>0 の点 < 全点数)" $ do
    let m = fitSVM defaultSVM
              { svmKernel = RBF, svmParams = rbfParams 0.5 } xr yr
    numSupportVectors m `shouldSatisfy` (< LA.rows xr)
    numSupportVectors m `shouldSatisfy` (> 0)

  it "決定的 (同入力で 2 回 fit して SV 数一致)" $ do
    let cfg = defaultSVM { svmKernel = RBF, svmParams = rbfParams 0.5 }
        m1  = fitSVM cfg xr yr
        m2  = fitSVM cfg xr yr
    numSupportVectors m1 `shouldBe` numSupportVectors m2

  it "多クラス (one-vs-rest): 3 クラスを訓練上で正しく分類" $ do
    let x3 = LA.fromLists [[0,0],[0.2,0.1],[5,0],[5,0.3],[0,5],[0.1,5]]
        y3 = VU.fromList [0,0,1,1,2,2]
        mm = fitSVMMulti defaultSVM
               { svmKernel = RBF, svmParams = rbfParams 0.3, svmC = 10 } x3 y3
        pr = predictSVMMulti mm x3
    VU.toList pr `shouldBe` VU.toList y3

  -- Phase 75.20: k-fold CV グリッド自動調律 (tuneSVM)。
  describe "tuneSVM (CV グリッド自動最適化・決定的)" $ do
    -- 同心 2 クラスを十分なサンプルで生成 (CV に必要な点数を確保)。
    let ringBig =
          ( LA.fromLists $ inner ++ outer
          , VU.fromList (replicate (length inner) 0 ++ replicate (length outer) 1) )
        inner = [ [0.3 * cos t, 0.3 * sin t] | t <- angles ]
        outer = [ [3.0 * cos t, 3.0 * sin t] | t <- angles ]
        angles = [ 2 * pi * fromIntegral i / 12 | i <- [0 .. 11 :: Int] ]
        (xb, yb) = ringBig
        grid = defaultSVMTuneGrid { svmtCs = [1, 10], svmtLengths = [0.5, 1, 2], svmtFolds = 3 }

    it "決定的: 同入力で 2 回呼んで同じ config/score" $ do
      let (c1, s1) = tuneSVM defaultSVM grid xb yb
          (c2, s2) = tuneSVM defaultSVM grid xb yb
      svmC c1 `shouldBe` svmC c2
      kpLengthScale (svmParams c1) `shouldBe` kpLengthScale (svmParams c2)
      s1 `shouldBe` s2

    it "非線形 (同心) で CV accuracy が高い (> 0.8) 構成を選ぶ" $ do
      let (_best, score) = tuneSVM defaultSVM grid xb yb
      score `shouldSatisfy` (> 0.8)

    it "選ばれた config で全データ再学習すると訓練精度 100%" $ do
      let (best, _) = tuneSVM defaultSVM grid xb yb
          m = fitSVMMulti best xb yb
          pr = predictSVMMulti m xb
      VU.toList pr `shouldBe` VU.toList yb
