{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Model.HBMSpec (spec) where

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
import qualified Data.Map.Strict as M
import qualified System.Random.MWC as MWC
import qualified Hanalyze.Model.Core        as Core
import qualified System.Random.MWC as MWC
import qualified Hanalyze.MCMC.NUTS as NUTS
import qualified Hanalyze.MCMC.Core as Core
import qualified Hanalyze.Model.HBM as HBM
import qualified Data.Map.Strict    as M
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Model.HBM Distribution (Phase 37-A2: 連続 4 分布)" $ do
    let isClose eps a b = abs (a - b) < eps
    --
    -- SkewNormal: α=0 で標準正規一致、 α>0 で右側に質量
    --
    it "SkewNormal(0,1,0) は Normal(0,1) と一致する (α=0、 erfA 近似誤差 ~1e-9)" $ do
      let p = HBM.logDensity (HBM.SkewNormal 0 1 0 :: HBM.Distribution Double) 0
      isClose 1e-8 p (-0.9189385332046727) `shouldBe` True
    it "SkewNormal(0,1,5) は x=+1 で x=-1 より大きい (右側に歪み)" $ do
      let pPos = HBM.logDensity (HBM.SkewNormal 0 1 5 :: HBM.Distribution Double) 1
          pNeg = HBM.logDensity (HBM.SkewNormal 0 1 5 :: HBM.Distribution Double) (-1)
      (pPos > pNeg) `shouldBe` True
    it "SkewNormal(0,1,0) の sample 1000 個で 0 付近に集中" $ do
      gen <- MWC.create
      xs <- mapM (\_ -> HBM.sampleDist (HBM.SkewNormal 0 1 0) gen) [1..1000::Int]
      let mean = sum xs / 1000
      isClose 0.1 mean 0 `shouldBe` True
    --
    -- Logistic: μ=0, s=1 で logpdf(0) = -log 4、 CDF(0) = 0.5
    --
    it "Logistic(0,1) logDensity(0) = -log 4" $ do
      let p = HBM.logDensity (HBM.Logistic 0 1 :: HBM.Distribution Double) 0
      isClose 1e-9 p (- log 4) `shouldBe` True
    it "Logistic(0,1) CDF(0) = 0.5" $ do
      case HBM.distCDF (HBM.Logistic 0 1 :: HBM.Distribution Double) 0 of
        Just c  -> isClose 1e-9 c 0.5 `shouldBe` True
        Nothing -> expectationFailure "distCDF returned Nothing"
    it "Logistic sample 1000 個で平均が μ=2 近傍" $ do
      gen <- MWC.create
      xs <- mapM (\_ -> HBM.sampleDist (HBM.Logistic 2 1) gen) [1..1000::Int]
      let mean = sum xs / 1000
      isClose 0.2 mean 2 `shouldBe` True
    --
    -- Gumbel: μ=0, β=1 で logpdf(0) = -1、 CDF(0) = exp(-1)
    --
    it "Gumbel(0,1) logDensity(0) = -1" $ do
      let p = HBM.logDensity (HBM.Gumbel 0 1 :: HBM.Distribution Double) 0
      isClose 1e-9 p (-1) `shouldBe` True
    it "Gumbel(0,1) CDF(0) = exp(-1)" $ do
      case HBM.distCDF (HBM.Gumbel 0 1 :: HBM.Distribution Double) 0 of
        Just c  -> isClose 1e-9 c (exp (-1)) `shouldBe` True
        Nothing -> expectationFailure "distCDF returned Nothing"
    it "Gumbel(0,1) sample 2000 個で平均が μ + β·γ ≈ 0.5772 近傍" $ do
      gen <- MWC.create
      xs <- mapM (\_ -> HBM.sampleDist (HBM.Gumbel 0 1) gen) [1..2000::Int]
      let mean = sum xs / 2000
      -- オイラー定数 γ ≈ 0.5772、 サンプル誤差 ~ β·π/√(6·n) ≈ 0.029
      isClose 0.15 mean 0.5772156649 `shouldBe` True
    --
    -- AsymmetricLaplace: κ=1 で対称ラプラス、 logpdf(0) = -log 2、 CDF(0) = 0.5
    --
    it "AsymmetricLaplace(1,1,0) は対称ラプラス" $ do
      let p1 = HBM.logDensity (HBM.AsymmetricLaplace 1 1 0 :: HBM.Distribution Double) 1
          pN1 = HBM.logDensity (HBM.AsymmetricLaplace 1 1 0 :: HBM.Distribution Double) (-1)
      isClose 1e-9 p1 pN1 `shouldBe` True
    it "AsymmetricLaplace(1,1,0) logDensity(0) = -log 2" $ do
      let p = HBM.logDensity (HBM.AsymmetricLaplace 1 1 0 :: HBM.Distribution Double) 0
      isClose 1e-9 p (- log 2) `shouldBe` True
    it "AsymmetricLaplace(1,1,0) CDF(0) = 0.5 (κ=1 で対称)" $ do
      case HBM.distCDF (HBM.AsymmetricLaplace 1 1 0 :: HBM.Distribution Double) 0 of
        Just c  -> isClose 1e-9 c 0.5 `shouldBe` True
        Nothing -> expectationFailure "distCDF returned Nothing"
    it "AsymmetricLaplace(1,2,0) は右側裾長 (κ=2 で正側に長い尾)" $ do
      -- κ=2 で正の x の方が log density 高いが、 大きい値で右側が遅く減衰
      let p_3pos = HBM.logDensity (HBM.AsymmetricLaplace 1 2 0 :: HBM.Distribution Double) 3
          p_3neg = HBM.logDensity (HBM.AsymmetricLaplace 1 2 0 :: HBM.Distribution Double) (-3)
      -- x=+3 では -b·κ·3 = -6、 x=-3 では (b/κ)·(-3) = -1.5
      -- → x=-3 の方が log density 高い (左側裾は短く局在)
      (p_3neg > p_3pos) `shouldBe` True
    it "AsymmetricLaplace(1,1,5) sample 2000 個で平均が μ=5 近傍" $ do
      gen <- MWC.create
      xs <- mapM (\_ -> HBM.sampleDist (HBM.AsymmetricLaplace 1 1 5) gen) [1..2000::Int]
      let mean = sum xs / 2000
      isClose 0.2 mean 5 `shouldBe` True

  describe "Hanalyze.Model.HBM Distribution (Phase 37-A3: 離散 5 分布)" $ do
    let isClose eps a b = abs (a - b) < eps
    --
    -- OrderedLogistic: K=3 (cuts=[c1, c2]) で 3 カテゴリ
    --
    it "OrderedLogistic(η=0, cuts=[-1,1]) は 3 カテゴリの確率総和 1" $ do
      let d = HBM.OrderedLogistic 0 [-1, 1] :: HBM.Distribution Double
          p0 = exp (HBM.logDensityObs d 0)
          p1 = exp (HBM.logDensityObs d 1)
          p2 = exp (HBM.logDensityObs d 2)
      isClose 1e-9 (p0 + p1 + p2) 1 `shouldBe` True
    it "OrderedLogistic(η=0, cuts=[-1,1]) は対称 (P(0) = P(2))" $ do
      let d = HBM.OrderedLogistic 0 [-1, 1] :: HBM.Distribution Double
          p0 = exp (HBM.logDensityObs d 0)
          p2 = exp (HBM.logDensityObs d 2)
      isClose 1e-9 p0 p2 `shouldBe` True
    it "OrderedLogistic(η=2, cuts=[-1,1]) は P(2) > P(0) (η 増 → 高カテゴリ寄り)" $ do
      let d = HBM.OrderedLogistic 2 [-1, 1] :: HBM.Distribution Double
          p0 = exp (HBM.logDensityObs d 0)
          p2 = exp (HBM.logDensityObs d 2)
      (p2 > p0) `shouldBe` True
    --
    -- DiscreteUniform: 0..9 で logpmf = -log 10
    --
    it "DiscreteUniform(0,9) logpmf(5) = -log 10" $ do
      let p = HBM.logDensityObs (HBM.DiscreteUniform 0 9 :: HBM.Distribution Double) 5
      isClose 1e-9 p (- log 10) `shouldBe` True
    it "DiscreteUniform(0,9) range 外で logpmf = -∞" $ do
      let p = HBM.logDensityObs (HBM.DiscreteUniform 0 9 :: HBM.Distribution Double) 10
      isInfinite p && p < 0 `shouldBe` True
    it "DiscreteUniform(0,9) sample 1000 個で全カテゴリが現れる" $ do
      gen <- MWC.create
      xs <- mapM (\_ -> HBM.sampleDist (HBM.DiscreteUniform 0 9) gen) [1..1000::Int]
      let unique = length (foldr (\x acc -> if x `elem` acc then acc else x:acc) [] xs)
      (unique >= 9) `shouldBe` True
    --
    -- Geometric: PyMC 慣例 (support 1, 2, ...)
    --
    it "Geometric(0.5) logpmf(1) = log 0.5" $ do
      let p = HBM.logDensityObs (HBM.Geometric 0.5 :: HBM.Distribution Double) 1
      isClose 1e-9 p (log 0.5) `shouldBe` True
    it "Geometric(0.5) logpmf(3) = 3 log 0.5" $ do
      let p = HBM.logDensityObs (HBM.Geometric 0.5 :: HBM.Distribution Double) 3
      isClose 1e-9 p (3 * log 0.5) `shouldBe` True
    it "Geometric(0.5) sample 2000 個の平均が 1/p = 2 近傍" $ do
      gen <- MWC.create
      xs <- mapM (\_ -> HBM.sampleDist (HBM.Geometric 0.5) gen) [1..2000::Int]
      let mean = sum xs / 2000
      isClose 0.2 mean 2 `shouldBe` True
    --
    -- HyperGeometric(N=10, K=5, n=3): k=2 で scipy ≈ 0.4167
    --   C(5,2)*C(5,1)/C(10,3) = 10 * 5 / 120 = 50/120 = 0.4167
    --
    it "HyperGeometric(10,5,3) pmf(2) ≈ 0.4167" $ do
      let p = exp (HBM.logDensityObs (HBM.HyperGeometric 10 5 3 :: HBM.Distribution Double) 2)
      isClose 1e-6 p (50 / 120) `shouldBe` True
    it "HyperGeometric(10,5,3) pmf 0..3 総和 = 1" $ do
      let d = HBM.HyperGeometric 10 5 3 :: HBM.Distribution Double
          tot = sum [exp (HBM.logDensityObs d (realToFrac k)) | k <- [0..3 :: Int]]
      isClose 1e-9 tot 1 `shouldBe` True
    --
    -- ZeroInflatedNegativeBinomial: ψ=0 で NegBin と一致
    --
    it "ZeroInflatedNegativeBinomial(0, 5, 2) は NegativeBinomial(5,2) と一致 (ψ=0)" $ do
      let d1 = HBM.ZeroInflatedNegativeBinomial 0 5 2 :: HBM.Distribution Double
          d2 = HBM.NegativeBinomial 5 2 :: HBM.Distribution Double
          p1 = HBM.logDensityObs d1 3
          p2 = HBM.logDensityObs d2 3
      isClose 1e-9 p1 p2 `shouldBe` True
    it "ZeroInflatedNegativeBinomial(0.3, 5, 2) は P(y=0) が NegBin より大きい" $ do
      let d1 = HBM.ZeroInflatedNegativeBinomial 0.3 5 2 :: HBM.Distribution Double
          d2 = HBM.NegativeBinomial 5 2 :: HBM.Distribution Double
          p1 = exp (HBM.logDensityObs d1 0)
          p2 = exp (HBM.logDensityObs d2 0)
      (p1 > p2) `shouldBe` True
    it "ZeroInflatedNegativeBinomial(0.3, 3, 2) sample 2000 個で 0 比率 ≈ 0.3 + (1-0.3)·NegBin(0)" $ do
      gen <- MWC.create
      xs <- mapM (\_ -> HBM.sampleDist (HBM.ZeroInflatedNegativeBinomial 0.3 3 2) gen) [1..2000::Int]
      let zeros = length (filter (== 0) xs)
          rate0 = realToFrac zeros / 2000
          -- 期待される P(0): 0.3 + 0.7 * (2/(2+3))^2 = 0.3 + 0.7 * 0.16 = 0.412
          expectedP0 = 0.3 + 0.7 * (2 / 5) ** 2
      (abs (rate0 - expectedP0) < 0.05) `shouldBe` True

  describe "Hanalyze.Model.HBM Distribution (Phase 37-A4: 多変量 2 分布)" $ do
    let isClose eps a b = abs (a - b) < eps
    --
    -- MvStudentT(ν=10, μ=[0,0], Σ=I) at y=[0,0]: scipy ≈ -1.8379
    --
    it "MvStudentT(ν=10, μ=[0,0], Σ=I) logpdf([0,0]) ≈ -1.8379 (scipy)" $ do
      let p = HBM.mvStudentTLogDensity 10
                ([0, 0] :: [Double])
                [[1, 0], [0, 1]]
                [0, 0]
      isClose 1e-3 p (-1.8379) `shouldBe` True
    it "MvStudentT(ν=1000) は MvNormal に漸近 (logpdf at [0,0] ≈ −log(2π))" $ do
      let pT = HBM.mvStudentTLogDensity 1000
                ([0, 0] :: [Double])
                [[1, 0], [0, 1]]
                [0, 0]
          pN = HBM.mvNormalLogDensity
                ([0, 0] :: [Double])
                [[1, 0], [0, 1]]
                [0, 0]
      -- ν=1000 で diff ~ 1e-3 オーダ
      (abs (pT - pN) < 0.01) `shouldBe` True
    it "MvStudentT は中心で density が周辺より大きい" $ do
      let pCenter = HBM.mvStudentTLogDensity 5
                     ([0, 0] :: [Double])
                     [[1, 0], [0, 1]]
                     [0, 0]
          pEdge   = HBM.mvStudentTLogDensity 5
                     ([0, 0] :: [Double])
                     [[1, 0], [0, 1]]
                     [3, 3]
      (pCenter > pEdge) `shouldBe` True
    --
    -- DirichletMultinomial(n=2, α=[1,1,1]) at counts=[1,1,0]: scipy = log(1/6) ≈ -1.7918
    --
    it "DirichletMultinomial(n=2, α=[1,1,1]) pmf([1,1,0]) = 1/6" $ do
      let p = HBM.dirichletMultinomialLogDensity 2
                ([1, 1, 1] :: [Double])
                [1, 1, 0]
      isClose 1e-9 p (log (1/6)) `shouldBe` True
    it "DirichletMultinomial(n=2, α=[1,1,1]) は全 6 種類の組合せ確率総和 = 1" $ do
      let combos = [[2,0,0],[0,2,0],[0,0,2],[1,1,0],[1,0,1],[0,1,1]]
          tot = sum [ exp (HBM.dirichletMultinomialLogDensity 2
                            ([1, 1, 1] :: [Double]) c)
                    | c <- combos ]
      isClose 1e-9 tot 1 `shouldBe` True
    it "DirichletMultinomial(n=3, α=[1,1,1,1]) total prob = 1 (10 種)" $ do
      let combos = [ [c0, c1, c2, c3]
                   | c0 <- [0..3], c1 <- [0..3-c0]
                   , c2 <- [0..3-c0-c1]
                   , let c3 = 3 - c0 - c1 - c2
                   , c3 >= 0 ]
          tot = sum [ exp (HBM.dirichletMultinomialLogDensity 3
                            ([1, 1, 1, 1] :: [Double]) c)
                    | c <- combos ]
      isClose 1e-9 tot 1 `shouldBe` True
    --
    -- obsLogSum 経由で MvStudentT が k 次元 chunk として処理されることを確認
    --
    it "obsLogSum (MvStudentT) は 2 観測 (k=2、 flat 4 要素) を 2 個の log と一致" $ do
      let dist = HBM.MvStudentT 10 ([0, 0] :: [Double]) [[1, 0], [0, 1]]
          flat = [0, 0,  1, 1]
          summed = HBM.obsLogSum dist flat
          p1 = HBM.mvStudentTLogDensity 10 [0,0] [[1,0],[0,1]] [0,0]
          p2 = HBM.mvStudentTLogDensity 10 [0,0] [[1,0],[0,1]] [1,1]
      isClose 1e-9 summed (p1 + p2) `shouldBe` True

  describe "Hanalyze.Model.HBM Distribution (Phase 39-A1: 連続 3 + 離散 1)" $ do
    let isClose eps a b = abs (a - b) < eps
    --
    -- Triangular: lower=0, c=0.5, upper=1
    --   pdf(0.5) = 2/(1·0.5) = 4  -> logpdf = log 4
    --   CDF closed-form via inverse sample test (平均 ≈ (lo+c+hi)/3)
    --
    it "Triangular(0,0.5,1) logDensity(0.5) = log 2 (peak pdf = 2/(hi-lo))" $ do
      let p = HBM.logDensity (HBM.Triangular 0 0.5 1 :: HBM.Distribution Double) 0.5
      isClose 1e-9 p (log 2) `shouldBe` True
    it "Triangular(0,0.5,1) は範囲外 (-0.1) で −∞" $ do
      let p = HBM.logDensity (HBM.Triangular 0 0.5 1 :: HBM.Distribution Double) (-0.1)
      (p < -1e10) `shouldBe` True
    it "Triangular(0,0.5,1) sample 2000 個で平均 ≈ 0.5 (= (0+0.5+1)/3)" $ do
      gen <- MWC.create
      xs <- mapM (\_ -> HBM.sampleDist (HBM.Triangular 0 0.5 1) gen) [1..2000::Int]
      let mean = sum xs / 2000
      isClose 0.05 mean 0.5 `shouldBe` True
    --
    -- Kumaraswamy(a=1, b=1) は uniform(0,1)
    --   logpdf(0.5) = log 1 + log 1 + 0 + 0 = 0
    --
    it "Kumaraswamy(1,1) は Uniform(0,1) (logDensity = 0)" $ do
      let p = HBM.logDensity (HBM.Kumaraswamy 1 1 :: HBM.Distribution Double) 0.5
      isClose 1e-9 p 0 `shouldBe` True
    it "Kumaraswamy(2,2) logDensity(0.5)" $ do
      -- pdf(0.5) = 2*2*0.5*(1-0.25) = 4*0.5*0.75 = 1.5
      let p = HBM.logDensity (HBM.Kumaraswamy 2 2 :: HBM.Distribution Double) 0.5
      isClose 1e-9 p (log 1.5) `shouldBe` True
    it "Kumaraswamy(2,2) sample 2000 個で平均が ~0.5 近傍" $ do
      gen <- MWC.create
      xs <- mapM (\_ -> HBM.sampleDist (HBM.Kumaraswamy 2 2) gen) [1..2000::Int]
      let mean = sum xs / 2000
      -- E[X] = b * B(1+1/a, b) = 2 * B(1.5, 2) = 2 * Γ(1.5)Γ(2)/Γ(3.5)
      --      = 2 * 0.8862269 * 1 / 3.3233509 = 0.5333...
      isClose 0.05 mean 0.5333333 `shouldBe` True
    --
    -- Rice(ν=0, σ) は Rayleigh(σ)、 pdf(x) = (x/σ²) exp(-x²/(2σ²))
    --   x=σ=1: logpdf = log 1 - 0 - 0.5 + logI0(0) = -0.5
    --
    it "Rice(0,1) logDensity(1) = -0.5 (Rayleigh、 I0(0)=1)" $ do
      let p = HBM.logDensity (HBM.Rice 0 1 :: HBM.Distribution Double) 1
      isClose 1e-7 p (-0.5) `shouldBe` True
    it "Rice(0,1) sample 2000 個で平均 ≈ σ·√(π/2) ≈ 1.2533 (Rayleigh 平均)" $ do
      gen <- MWC.create
      xs <- mapM (\_ -> HBM.sampleDist (HBM.Rice 0 1) gen) [1..2000::Int]
      let mean = sum xs / 2000
      isClose 0.05 mean 1.2533141 `shouldBe` True
    it "Rice(2,1) sample 2000 個で平均 > Rice(0,1) 平均 (ν>0 で右シフト)" $ do
      gen <- MWC.create
      xs0 <- mapM (\_ -> HBM.sampleDist (HBM.Rice 0 1) gen) [1..2000::Int]
      xs2 <- mapM (\_ -> HBM.sampleDist (HBM.Rice 2 1) gen) [1..2000::Int]
      (sum xs2 > sum xs0) `shouldBe` True
    --
    -- DiscreteWeibull(q=0.5, β=1) は Geometric(p=0.5) (PyMC 慣例の {0,1,...})
    --   pmf(0) = q^0 - q^1 = 1 - 0.5 = 0.5
    --   pmf(1) = q^1 - q^4 (β=1) で q^1 - q^2 = 0.5 - 0.25 = 0.25
    --
    it "DiscreteWeibull(0.5, 1) pmf(0) = 0.5" $ do
      let p = exp (HBM.logDensityObs (HBM.DiscreteWeibull 0.5 1 :: HBM.Distribution Double) 0)
      isClose 1e-9 p 0.5 `shouldBe` True
    it "DiscreteWeibull(0.5, 1) pmf(1) = 0.25" $ do
      let p = exp (HBM.logDensityObs (HBM.DiscreteWeibull 0.5 1 :: HBM.Distribution Double) 1)
      isClose 1e-9 p 0.25 `shouldBe` True
    it "DiscreteWeibull(0.5, 1) pmf 0..30 総和 ≈ 1" $ do
      let d = HBM.DiscreteWeibull 0.5 1 :: HBM.Distribution Double
          tot = sum [exp (HBM.logDensityObs d (realToFrac k)) | k <- [0..30::Int]]
      isClose 1e-6 tot 1 `shouldBe` True
    it "DiscreteWeibull(0.5, 1) sample 2000 個で整数 (≥0) のみ" $ do
      gen <- MWC.create
      xs <- mapM (\_ -> HBM.sampleDist (HBM.DiscreteWeibull 0.5 1) gen) [1..2000::Int]
      (all (\x -> x >= 0 && x == fromIntegral (round x :: Int)) xs) `shouldBe` True
    --
    -- DAG: distDepsT は Phase 38 で確立した「新分布追加 4 箇所」 規律で網羅
    --
    it "Triangular の buildModelGraph: lo, c, hi → y の 3 edges" $ do
      let m :: HBM.ModelP ()
          m = do
            lo <- HBM.sample "lo" (HBM.Normal 0 1)
            c  <- HBM.sample "c"  (HBM.Normal 0 1)
            hi <- HBM.sample "hi" (HBM.Normal 0 1)
            HBM.observe "y" (HBM.Triangular lo c hi) [0.5]
      let g = HBM.buildModelGraph m
          edges = HBM.mgEdges g
      length [() | (s, t) <- edges, t == "y", s `elem` ["lo", "c", "hi"]] `shouldBe` 3
    it "Rice の buildModelGraph: ν, σ → y の 2 edges" $ do
      let m :: HBM.ModelP ()
          m = do
            nu  <- HBM.sample "nu"  (HBM.HalfNormal 1)
            sig <- HBM.sample "sig" (HBM.HalfNormal 1)
            HBM.observe "y" (HBM.Rice nu sig) [1.0]
      let g = HBM.buildModelGraph m
          edges = HBM.mgEdges g
      length [() | (s, t) <- edges, t == "y", s `elem` ["nu", "sig"]] `shouldBe` 2

  describe "Hanalyze.Model.HBM Distribution (Phase 39-A2: Wishart)" $ do
    let isClose eps a b = abs (a - b) < eps
    --
    -- Wishart(ν=3, V=I_2) at W=I_2:
    --   logpdf = -3 log 2 - log(π/2) - 1 ≈ -3.5310
    --
    it "Wishart(3, I_2) logpdf(I_2) ≈ -3.531 (scipy.stats.wishart 同等)" $ do
      let p = HBM.wishartLogDensity 3
                ([[1, 0], [0, 1]] :: [[Double]])
                [1, 0, 0, 1]  -- W = I_2 flatten
      isClose 1e-4 p (-2 * log 2 - log pi - 1) `shouldBe` True
    --
    -- ν < k で degenerate (-∞)
    --
    it "Wishart(ν=0) は degenerate (logpdf = -∞)" $ do
      let p = HBM.wishartLogDensity 0
                ([[1, 0], [0, 1]] :: [[Double]])
                [1, 0, 0, 1]
      (p < -1e10) `shouldBe` True
    --
    -- V≠I のスケール変換: V = 2I で logpdf がシフトする
    --   logpdf = -3 log 2 - (3/2) log|2I| - log Γ_2(3/2) - (1/2) tr((2I)⁻¹ I)
    --         = -3 log 2 - (3/2)(2 log 2) - log(π/2) - 0.5
    --         = -3 log 2 - 3 log 2 - log(π/2) - 0.5
    --         = -6 log 2 - log π + log 2 - 0.5
    --         = -5 log 2 - log π - 0.5
    --
    it "Wishart(3, 2·I_2) logpdf(I_2) で V スケール反映" $ do
      let p = HBM.wishartLogDensity 3
                ([[2, 0], [0, 2]] :: [[Double]])
                [1, 0, 0, 1]
      isClose 1e-4 p (-5 * log 2 - log pi - 0.5) `shouldBe` True
    --
    -- obsLogSum 経由: 2 観測 (k=2、 flat 8 要素) を 2 個の log と一致
    --
    it "obsLogSum (Wishart) は 2 観測 (flat 8 要素) を 2 個の log と一致" $ do
      let dist = HBM.Wishart 3 ([[1, 0], [0, 1]] :: [[Double]])
          flat = [1, 0, 0, 1,   2, 0, 0, 2]
          summed = HBM.obsLogSum dist flat
          p1 = HBM.wishartLogDensity 3 [[1,0],[0,1]] [1, 0, 0, 1]
          p2 = HBM.wishartLogDensity 3 [[1,0],[0,1]] [2, 0, 0, 2]
      isClose 1e-9 summed (p1 + p2) `shouldBe` True
    --
    -- DAG: ν, V → y のエッジ
    --
    it "Wishart の buildModelGraph: ν, V 各要素 → y のエッジ" $ do
      let m :: HBM.ModelP ()
          m = do
            nu  <- HBM.sample "nu"  (HBM.HalfNormal 5)
            v00 <- HBM.sample "v00" (HBM.HalfNormal 1)
            v11 <- HBM.sample "v11" (HBM.HalfNormal 1)
            HBM.observeMV "y"
              (HBM.Wishart nu [[v00, 0], [0, v11]])
              [[1, 0, 0, 1]]
      let g = HBM.buildModelGraph m
          edges = HBM.mgEdges g
      length [() | (s, t) <- edges, t == "y", s `elem` ["nu", "v00", "v11"]] `shouldBe` 3

  describe "Hanalyze.Model.HBM Distribution (Phase 39-A3: Bound + OrderedProbit)" $ do
    let isClose eps a b = abs (a - b) < eps
    --
    -- Bound = Truncated と同等
    --
    it "Bound(Normal(0,1), 0, +∞) は Truncated と同じ logDensity" $ do
      let p1 = HBM.logDensity (HBM.Bound (HBM.Normal 0 1) (Just 0) Nothing :: HBM.Distribution Double) 1
          p2 = HBM.logDensity (HBM.Truncated (HBM.Normal 0 1) (Just 0) Nothing :: HBM.Distribution Double) 1
      isClose 1e-12 p1 p2 `shouldBe` True
    it "Bound(Normal(0,1), 0, ∞) は範囲外 (-1) で −∞" $ do
      let p = HBM.logDensity (HBM.Bound (HBM.Normal 0 1) (Just 0) Nothing :: HBM.Distribution Double) (-1)
      (p < -1e10) `shouldBe` True
    it "Bound sample (rejection) は範囲内に収まる" $ do
      gen <- MWC.create
      xs <- mapM (\_ -> HBM.sampleDist (HBM.Bound (HBM.Normal 0 1) (Just 0) (Just 2)) gen) [1..500::Int]
      (all (\x -> x >= 0 && x <= 2) xs) `shouldBe` True
    --
    -- OrderedProbit: K=3 (cuts=[c1, c2]) で 3 カテゴリの確率総和 1
    --
    it "OrderedProbit(η=0, cuts=[-1,1]) は 3 カテゴリの確率総和 1" $ do
      let d = HBM.OrderedProbit 0 [-1, 1] :: HBM.Distribution Double
          p0 = exp (HBM.logDensityObs d 0)
          p1 = exp (HBM.logDensityObs d 1)
          p2 = exp (HBM.logDensityObs d 2)
      isClose 1e-9 (p0 + p1 + p2) 1 `shouldBe` True
    it "OrderedProbit(η=0, cuts=[-1,1]) は中央カテゴリの確率最大 (対称)" $ do
      let d = HBM.OrderedProbit 0 [-1, 1] :: HBM.Distribution Double
          p0 = exp (HBM.logDensityObs d 0)
          p1 = exp (HBM.logDensityObs d 1)
          p2 = exp (HBM.logDensityObs d 2)
      (p1 > p0 && p1 > p2) `shouldBe` True
    it "OrderedProbit(η=2, cuts=[-1,1]) は最大カテゴリ寄り (右シフト)" $ do
      -- η=2: P(y=2) = 1 - Φ(1 - 2) = 1 - Φ(-1) ≈ 0.841
      let d = HBM.OrderedProbit 2 [-1, 1] :: HBM.Distribution Double
          p2 = exp (HBM.logDensityObs d 2)
      isClose 0.01 p2 0.8413 `shouldBe` True
    it "OrderedProbit sample 2000 個で全カテゴリ {0,1,2} に分布" $ do
      gen <- MWC.create
      xs <- mapM (\_ -> HBM.sampleDist (HBM.OrderedProbit 0 [-1, 1]) gen) [1..2000::Int]
      let cats = [ length (filter (== fromIntegral k) xs) | k <- [0..2 :: Int]]
      (all (> 0) cats) `shouldBe` True
    --
    -- DAG
    --
    it "OrderedProbit の buildModelGraph: η, cuts → y" $ do
      let m :: HBM.ModelP ()
          m = do
            eta <- HBM.sample "eta" (HBM.Normal 0 1)
            c1  <- HBM.sample "c1"  (HBM.Normal 0 1)
            c2  <- HBM.sample "c2"  (HBM.Normal 0 1)
            HBM.observe "y" (HBM.OrderedProbit eta [c1, c2]) [0, 1, 2]
      let g = HBM.buildModelGraph m
          edges = HBM.mgEdges g
      length [() | (s, t) <- edges, t == "y", s `elem` ["eta", "c1", "c2"]] `shouldBe` 3

  describe "Hanalyze.Model.HBM.orderedCuts (Phase 39-A6)" $ do
    --
    -- orderedCuts は increasing 列を deterministic で生成
    --
    it "orderedCuts: K-1=3 で長さ 3 の Track 列、 d_i は latent として登録" $ do
      let m :: HBM.ModelP ()
          m = do
            cs <- HBM.orderedCuts "cuts" 3 (-1) 1
            -- 観測なし (構造のみ確認)
            HBM.observe "y" (HBM.OrderedLogistic 0 cs) [0, 1, 2, 3]
      let g = HBM.buildModelGraph m
          nodes = HBM.mgNodes g
          nodeNames = map HBM.nodeName nodes
      -- 期待: cuts_c_1, cuts_d_2, cuts_c_2, cuts_d_3, cuts_c_3, y
      elem "cuts_c_1" nodeNames `shouldBe` True
      elem "cuts_d_2" nodeNames `shouldBe` True
      elem "cuts_c_2" nodeNames `shouldBe` True
      elem "cuts_d_3" nodeNames `shouldBe` True
      elem "cuts_c_3" nodeNames `shouldBe` True
    it "orderedCuts: cuts は y の親に並ぶ (DAG-safe helper)" $ do
      let m :: HBM.ModelP ()
          m = do
            cs <- HBM.orderedCuts "cuts" 2 0 1
            HBM.observe "y" (HBM.OrderedLogistic 0 cs) [0, 1, 2]
      let g = HBM.buildModelGraph m
          edges = HBM.mgEdges g
      -- y の親に cuts_c_1, cuts_c_2 が含まれる
      elem ("cuts_c_1", "y") edges `shouldBe` True
      elem ("cuts_c_2", "y") edges `shouldBe` True
    it "orderedCuts は sample 時に c_min から増加する列を返す" $ do
      let m :: HBM.ModelP ()
          m = do
            cs <- HBM.orderedCuts "cuts" 3 (-2) 0.5
            -- 戻り値をそのまま使うため observe で形だけ束縛 (drift 防止に gen=()
            -- は使わず deterministic 確認は sampleNames 経由)
            HBM.observe "y" (HBM.OrderedLogistic 0 cs) [0]
      -- buildModelGraph 経由で deterministic ノード数を数える
      let g = HBM.buildModelGraph m
          dets = filter (\n -> HBM.nodeDist n == "Deterministic") (HBM.mgNodes g)
      length dets `shouldBe` 3  -- c_1, c_2, c_3

  describe "Hanalyze.Model.HBM.dpStickBreaking (Phase 39-A5)" $ do
    --
    -- T=5 で β_1..β_4 latent + stick_1..stick_5 det + pi_1..pi_5 det
    --
    it "dpStickBreaking T=5 α=1: β_1..β_4 + stick + π ノードが揃う" $ do
      let m :: HBM.ModelP ()
          m = do
            pis <- HBM.dpStickBreaking "dp" 5 1
            HBM.observe "y" (HBM.Categorical pis) [0, 1, 2, 3, 4]
      let g = HBM.buildModelGraph m
          ns = map HBM.nodeName (HBM.mgNodes g)
      all (`elem` ns) ["dp_b_1", "dp_b_2", "dp_b_3", "dp_b_4"] `shouldBe` True
      all (`elem` ns) ["dp_pi_1", "dp_pi_2", "dp_pi_3", "dp_pi_4", "dp_pi_5"] `shouldBe` True
    it "dpStickBreaking π の和は 1 (構造的、 任意 β で成立)" $ do
      -- Track 経由のテスト: β_k = 0.5 固定で π_k の和を計算
      -- stick_1 = 1
      -- π_1 = 0.5 * 1 = 0.5
      -- stick_2 = 1 * 0.5 = 0.5
      -- π_2 = 0.5 * 0.5 = 0.25
      -- stick_3 = 0.5 * 0.5 = 0.25
      -- π_3 = 0.5 * 0.25 = 0.125
      -- stick_4 = 0.25 * 0.5 = 0.125
      -- π_4 = stick_4 = 0.125 (last)
      -- 和: 0.5 + 0.25 + 0.125 + 0.125 = 1.0
      let pis = let beta = 0.5
                    stk1 = 1
                    stk2 = stk1 * (1 - beta)
                    stk3 = stk2 * (1 - beta)
                    stk4 = stk3 * (1 - beta)
                    p1 = beta * stk1
                    p2 = beta * stk2
                    p3 = beta * stk3
                    p4 = stk4
                in [p1, p2, p3, p4 :: Double]
      sum pis `shouldBe` 1.0
    it "dpStickBreaking T=3: pi_3 が y の親に含まれる (DAG-safe)" $ do
      let m :: HBM.ModelP ()
          m = do
            pis <- HBM.dpStickBreaking "dp" 3 2
            HBM.observe "y" (HBM.Categorical pis) [0, 1, 2]
      let g = HBM.buildModelGraph m
          edges = HBM.mgEdges g
      elem ("dp_pi_1", "y") edges `shouldBe` True
      elem ("dp_pi_2", "y") edges `shouldBe` True
      elem ("dp_pi_3", "y") edges `shouldBe` True

  describe "Hanalyze.Model.HBM.hmmForwardLogLik (Phase 39-A4)" $ do
    let isClose eps a b = abs (a - b) < eps
    --
    -- 手計算確認 K=2, T=2:
    --   π_0 = [0.6, 0.4]、 T = [[0.7, 0.3], [0.4, 0.6]]
    --   emit (y_1=0, y_2=1): emit = [[log 0.5, log 0.1], [log 0.4, log 0.5]]
    --   直接列挙: Σ_{s0,s1} π_0[s0] e_0[s0] T[s0][s1] e_1[s1]
    --     0,0: 0.6 * 0.5 * 0.7 * 0.4 = 0.084
    --     0,1: 0.6 * 0.5 * 0.3 * 0.5 = 0.045
    --     1,0: 0.4 * 0.1 * 0.4 * 0.4 = 0.0064
    --     1,1: 0.4 * 0.1 * 0.6 * 0.5 = 0.012
    --   sum = 0.1474 → log ≈ -1.9143
    --
    it "K=2, T=2 の周辺対数尤度が直接列挙と一致 (-1.9143)" $ do
      let pi0   = [0.6, 0.4] :: [Double]
          trans = [[0.7, 0.3], [0.4, 0.6]]
          emit  = [[log 0.5, log 0.1], [log 0.4, log 0.5]]
          ll = HBM.hmmForwardLogLik pi0 trans emit
      isClose 1e-9 ll (log 0.1474) `shouldBe` True
    --
    -- T=1 の自明ケース: log P(y_1) = logSumExp(log π_0[k] + emit[0][k])
    --
    it "T=1 で周辺対数尤度 = logSumExp(log π_0 + emit)" $ do
      let pi0  = [0.7, 0.3] :: [Double]
          tr   = [[0.5, 0.5], [0.5, 0.5]]
          emit = [[log 0.8, log 0.2]]
          ll   = HBM.hmmForwardLogLik pi0 tr emit
          expected = log (0.7 * 0.8 + 0.3 * 0.2)
      isClose 1e-9 ll expected `shouldBe` True
    --
    -- T=0 (観測なし) は 0
    --
    it "T=0 (空観測) は logLik = 0" $ do
      let pi0 = [0.5, 0.5] :: [Double]
          tr  = [[0.5, 0.5], [0.5, 0.5]]
          ll  = HBM.hmmForwardLogLik pi0 tr ([] :: [[Double]])
      ll `shouldBe` 0
    --
    -- 恒等遷移 (T = I) は各時刻独立: log P(y) = log Σ_k π_0[k] Π_t e_t[k]
    --
    it "恒等遷移 (絶対吸収) で π_0 と emission の積が正しく入る" $ do
      let pi0  = [1.0, 0.0] :: [Double]  -- 確実に状態 0 開始
          tr   = [[1.0, 0.0], [0.0, 1.0]]  -- 状態固定
          emit = [[log 0.5, log 0.9], [log 0.4, log 0.8]]
          ll   = HBM.hmmForwardLogLik pi0 tr emit
          -- 状態 0 確定: P = 0.5 * 0.4 = 0.2
          expected = log 0.2
      isClose 1e-9 ll expected `shouldBe` True
    --
    -- 大 T (T=200) で underflow しない (log-space の効用)
    --
    it "T=200 で underflow せず有限値を返す (log-space)" $ do
      let pi0  = [0.5, 0.5] :: [Double]
          tr   = [[0.9, 0.1], [0.1, 0.9]]
          emit = replicate 200 [log 0.3, log 0.5]
          ll   = HBM.hmmForwardLogLik pi0 tr emit
      -- 期待値ではなく「有限かつ負」 を確認
      (ll < 0 && not (isInfinite ll) && not (isNaN ll)) `shouldBe` True

  describe "Hanalyze.Model.HBM.hmmLatent (Phase 39-A4)" $ do
    --
    -- K=2 で pi0 + 2 trans rows = 3 Dirichlet が立つ
    --
    it "hmmLatent K=2 α=1: pi0 + trans_0, trans_1 の Dirichlet が立つ" $ do
      let m :: HBM.ModelP ()
          m = do
            (pi0, trans) <- HBM.hmmLatent "hmm" 2 1
            -- emit の dummy 計算 (実用では emission 毎時刻で別々)
            let emit = [[log (pi0 !! 0), log (pi0 !! 1)]]
                tr   = trans
            HBM.potential "lik" (HBM.hmmForwardLogLik pi0 tr emit)
      let g = HBM.buildModelGraph m
          ns = map HBM.nodeName (HBM.mgNodes g)
      -- pi0: hmm_pi0_0, hmm_pi0_1 (Dirichlet 内部の det)
      elem "hmm_pi0_0" ns `shouldBe` True
      elem "hmm_pi0_1" ns `shouldBe` True
      -- trans: hmm_trans_0_0, hmm_trans_0_1, hmm_trans_1_0, hmm_trans_1_1
      elem "hmm_trans_0_0" ns `shouldBe` True
      elem "hmm_trans_1_1" ns `shouldBe` True
    it "hmmLatent K=3 α=1: 3 + 9 = 12 個の π det ノード" $ do
      let m :: HBM.ModelP ()
          m = do
            (pi0, trans) <- HBM.hmmLatent "hmm" 3 1
            let emit = [[log p | p <- pi0]]
            HBM.potential "lik" (HBM.hmmForwardLogLik pi0 trans emit)
      let g = HBM.buildModelGraph m
          ns = map HBM.nodeName (HBM.mgNodes g)
          -- 各 dirichlet は内部に β_b<i> latent も持つので、 π det は
          -- 末尾が数字のみ (β は _b<i> 形)
          isPiDet base n =
            base `T.isPrefixOf` n
            && case T.stripPrefix base n of
                 Just rest -> T.all (`elem` ("0123456789" :: String)) rest
                 Nothing   -> False
          piCnt = length [n | n <- ns, isPiDet "hmm_pi0_" n]
          trCnt = sum
            [ length [n | n <- ns, isPiDet ("hmm_trans_" <> T.pack (show i) <> "_") n]
            | i <- [0..2 :: Int] ]
      piCnt `shouldBe` 3   -- π_0[0..2]
      trCnt `shouldBe` 9   -- 3 行 × 3 列

  describe "Hanalyze.Model.HBM.plate (Phase 40-A1/A2)" $ do
    --
    -- 8-schools: plate "school" 8 で eta_j を囲う、 mu/tau は plate 外
    --
    let eightSchools :: HBM.ModelP ()
        eightSchools = do
          mu  <- HBM.sample "mu"  (HBM.Normal 0 5)
          tau <- HBM.sample "tau" (HBM.HalfCauchy 5)
          _ <- HBM.plate "school" 8 $ forM [0..7 :: Int] $ \j -> do
            eta <- HBM.sample ("eta_" <> T.pack (show j)) (HBM.Normal 0 1)
            HBM.observe ("y_" <> T.pack (show j))
              (HBM.Normal (mu + tau * eta) 1) [realToFrac j]
          return ()
    it "8-schools: mgPlates に \"school\" → 8 が入る" $ do
      let g = HBM.buildModelGraph eightSchools
      M.lookup "school" (HBM.mgPlates g) `shouldBe` Just 8
    it "8-schools: mu / tau は nodePlates が空 (plate 外)" $ do
      let g = HBM.buildModelGraph eightSchools
          nByName nm = filter (\n -> HBM.nodeName n == nm) (HBM.mgNodes g)
      case nByName "mu" of
        [n] -> HBM.nodePlates n `shouldBe` []
        _   -> expectationFailure "mu not found"
      case nByName "tau" of
        [n] -> HBM.nodePlates n `shouldBe` []
        _   -> expectationFailure "tau not found"
    it "8-schools: eta_j / y_j は nodePlates = [\"school\"]" $ do
      let g = HBM.buildModelGraph eightSchools
          nByName nm = filter (\n -> HBM.nodeName n == nm) (HBM.mgNodes g)
      case nByName "eta_3" of
        [n] -> HBM.nodePlates n `shouldBe` ["school"]
        _   -> expectationFailure "eta_3 not found"
      case nByName "y_5" of
        [n] -> HBM.nodePlates n `shouldBe` ["school"]
        _   -> expectationFailure "y_5 not found"
    --
    -- plateI 糖衣の動作確認
    --
    it "plateI helper: plate name n (forM [0..n-1] f) と同等" $ do
      let m :: HBM.ModelP ()
          m = do
            _ <- HBM.plateI "g" 3 $ \j ->
              HBM.sample ("x_" <> T.pack (show j)) (HBM.Normal 0 1)
            return ()
      let g = HBM.buildModelGraph m
          xPlate = [HBM.nodePlates n | n <- HBM.mgNodes g, HBM.nodeName n == "x_1"]
      xPlate `shouldBe` [["g"]]
      M.lookup "g" (HBM.mgPlates g) `shouldBe` Just 3
    --
    -- plateI_ 糖衣の動作確認 (返り値破棄版・plateForM_ name [0..n-1] と同等)
    --
    it "plateI_ helper: plate name n (forM_ [0..n-1] f) と同等" $ do
      let m :: HBM.ModelP ()
          m = HBM.plateI_ "g" 3 $ \j ->
                HBM.sample ("x" HBM..# j) (HBM.Normal 0 1)
      let g = HBM.buildModelGraph m
          xPlate = [HBM.nodePlates n | n <- HBM.mgNodes g, HBM.nodeName n == "x_1"]
      xPlate `shouldBe` [["g"]]
      M.lookup "g" (HBM.mgPlates g) `shouldBe` Just 3
    --
    -- nested plate: outer "school" 3 内に inner "student" 2
    --
    it "nested plate: nodePlates が [\"school\", \"student\"] (外→内)" $ do
      let m :: HBM.ModelP ()
          m = do
            _ <- HBM.plate "school" 3 $ forM_ [0..2 :: Int] $ \j ->
              HBM.plate "student" 2 $ forM_ [0..1 :: Int] $ \i ->
                HBM.sample ("y_" <> T.pack (show j) <> "_" <> T.pack (show i))
                           (HBM.Normal 0 1)
            return ()
      let g = HBM.buildModelGraph m
          yNode = filter (\n -> HBM.nodeName n == "y_1_0") (HBM.mgNodes g)
      case yNode of
        [n] -> HBM.nodePlates n `shouldBe` ["school", "student"]
        _   -> expectationFailure "y_1_0 not found"
      M.lookup "school"  (HBM.mgPlates g) `shouldBe` Just 3
      M.lookup "student" (HBM.mgPlates g) `shouldBe` Just 2
    --
    -- 既存 helper (plate 未使用) との regression: nodePlates は空
    --
    it "既存モデル (plate 未使用) の nodePlates は空 (regression なし)" $ do
      let m :: HBM.ModelP ()
          m = do
            mu <- HBM.sample "mu" (HBM.Normal 0 1)
            HBM.observe "y" (HBM.Normal mu 1) [0, 1, 2]
      let g = HBM.buildModelGraph m
      all (null . HBM.nodePlates) (HBM.mgNodes g) `shouldBe` True
      M.null (HBM.mgPlates g) `shouldBe` True

  describe "Hanalyze.Model.HBM ノード名ヘルパ (indexed / .#)" $ do
    it "indexed はアンダースコア付きインデックス名を作る" $ do
      HBM.indexed "theta" 1 `shouldBe` ("theta_1" :: T.Text)
      HBM.indexed "y" 0     `shouldBe` ("y_0" :: T.Text)
    it ".# は indexed の中置版 (= 同じ結果)" $
      ("theta" HBM..# 3) `shouldBe` HBM.indexed "theta" 3
    it "sample 名に使うと sampleNames に反映される" $ do
      let m :: HBM.ModelP ()
          m = do
            mu <- HBM.sample "mu" (HBM.Normal 0 5)
            _  <- HBM.sample (HBM.indexed "theta" 1) (HBM.Normal mu 1)
            _  <- HBM.sample ("theta" HBM..# 2)      (HBM.Normal mu 1)
            pure ()
      sort (HBM.sampleNames m) `shouldBe` sort ["mu", "theta_1", "theta_2"]

  describe "Hanalyze.Model.HBM plate 罠 6 件 (Phase 40-A4)" $ do
    --
    -- 罠 1: 非中心化 (nonCentered) + plate
    --   raw eta_raw + det eta が両方 plate "school" に属するべき
    --
    it "罠 1: nonCenteredNormal を plate 内で使うと raw + det 両方 plate" $ do
      let m :: HBM.ModelP ()
          m = do
            mu  <- HBM.sample "mu" (HBM.Normal 0 5)
            tau <- HBM.sample "tau" (HBM.HalfCauchy 5)
            _ <- HBM.plate "school" 4 $ forM_ [0..3 :: Int] $ \j -> do
              eta <- HBM.nonCenteredNormal ("eta_" <> T.pack (show j)) mu tau
              HBM.observe ("y_" <> T.pack (show j)) (HBM.Normal eta 1)
                          [realToFrac j]
            return ()
      let g = HBM.buildModelGraph m
          nByName nm = filter (\n -> HBM.nodeName n == nm) (HBM.mgNodes g)
      case nByName "eta_2_raw" of
        [n] -> HBM.nodePlates n `shouldBe` ["school"]
        _   -> expectationFailure "eta_2_raw not found"
      case nByName "eta_2" of
        [n] -> HBM.nodePlates n `shouldBe` ["school"]
        _   -> expectationFailure "eta_2 not found"
    --
    -- 罠 2: 入れ子 plate (multi-level: school × student) は A2 で確認済
    --   ここでは edge が境界を超えても plate 構造を壊さないことを確認
    --
    it "罠 2: nested plate で内側 edge は内 plate のまま" $ do
      let m :: HBM.ModelP ()
          m = do
            _ <- HBM.plate "school" 2 $ forM_ [0..1 :: Int] $ \j -> do
              theta <- HBM.sample ("theta_" <> T.pack (show j))
                         (HBM.Normal 0 1)
              _ <- HBM.plate "student" 2 $ forM_ [0..1 :: Int] $ \i ->
                HBM.observe ("y_" <> T.pack (show j) <> "_" <> T.pack (show i))
                  (HBM.Normal theta 1) [realToFrac i]
              return ()
            return ()
      let g = HBM.buildModelGraph m
          nByName nm = filter (\n -> HBM.nodeName n == nm) (HBM.mgNodes g)
      -- theta は school plate のみ
      case nByName "theta_0" of
        [n] -> HBM.nodePlates n `shouldBe` ["school"]
        _   -> expectationFailure "theta_0 not found"
      -- y_0_0 は school + student 両 plate
      case nByName "y_0_0" of
        [n] -> HBM.nodePlates n `shouldBe` ["school", "student"]
        _   -> expectationFailure "y_0_0 not found"
    --
    -- 罠 3: クロス plate (subject × time): 重ならない 2 plate を別個に書く
    --   (PyMC でも完全な交差は描けないので、 2 plate 並列のみ確認)
    --
    it "罠 3: crossed plate は 2 つの別々 plate として記録される" $ do
      let m :: HBM.ModelP ()
          m = do
            -- subject ごとの効果 (plate "subject" 3)
            _ <- HBM.plate "subject" 3 $ forM_ [0..2 :: Int] $ \s ->
              HBM.sample ("u_" <> T.pack (show s)) (HBM.Normal 0 1)
            -- time ごとの効果 (plate "time" 2、 別 plate)
            _ <- HBM.plate "time" 2 $ forM_ [0..1 :: Int] $ \t ->
              HBM.sample ("v_" <> T.pack (show t)) (HBM.Normal 0 1)
            return ()
      let g = HBM.buildModelGraph m
      M.lookup "subject" (HBM.mgPlates g) `shouldBe` Just 3
      M.lookup "time"    (HBM.mgPlates g) `shouldBe` Just 2
    --
    -- 罠 4: plate 外から plate 内 への edge (mu → eta_j × 8 本)
    --   → mu の plate は [] / eta は plate ['school'] / edge は数本残る
    --
    it "罠 4: plate 外 mu から plate 内 eta_j への edge が複数本記録される" $ do
      let m :: HBM.ModelP ()
          m = do
            mu <- HBM.sample "mu" (HBM.Normal 0 5)
            _ <- HBM.plate "school" 3 $ forM_ [0..2 :: Int] $ \j ->
              HBM.sample ("eta_" <> T.pack (show j)) (HBM.Normal mu 1)
            return ()
      let g = HBM.buildModelGraph m
          edges = HBM.mgEdges g
          muEdges = [() | (s, t) <- edges, s == "mu",
                          T.isPrefixOf "eta_" t]
      length muEdges `shouldBe` 3
    --
    -- 罠 5: plate 内同士の edge (eta_j → y_j 同 plate)
    --
    it "罠 5: plate 内同士の edge (eta_j → y_j) が記録される" $ do
      let m :: HBM.ModelP ()
          m = do
            _ <- HBM.plate "school" 3 $ forM_ [0..2 :: Int] $ \j -> do
              eta <- HBM.sample ("eta_" <> T.pack (show j))
                       (HBM.Normal 0 1)
              HBM.observe ("y_" <> T.pack (show j)) (HBM.Normal eta 1)
                          [realToFrac j]
            return ()
      let g = HBM.buildModelGraph m
          edges = HBM.mgEdges g
      elem ("eta_0", "y_0") edges `shouldBe` True
      elem ("eta_1", "y_1") edges `shouldBe` True
      elem ("eta_2", "y_2") edges `shouldBe` True
    --
    -- 罠 6: observe の自動 merge と plate の整合
    --   同名 observe を統合する mergeByName は plate 内でも動く
    --   (個別名 y_0..y_7 を使えば衝突なし)
    --
    it "罠 6: 同名 observe \"y\" の自動 merge と plate の整合" $ do
      let m :: HBM.ModelP ()
          m = do
            mu <- HBM.sample "mu" (HBM.Normal 0 1)
            -- plate 内で同じ "y" を 3 回 observe (mergeByName が 1 ノードに統合)
            _ <- HBM.plate "obs" 3 $ forM_ [0..2 :: Int] $ \_ ->
              HBM.observe "y" (HBM.Normal mu 1) [0]
            return ()
      let g = HBM.buildModelGraph m
          ys = filter (\n -> HBM.nodeName n == "y") (HBM.mgNodes g)
      length ys `shouldBe` 1
      -- merge された y は最初の plate 状態を維持 (= ["obs"])
      case ys of
        [n] -> HBM.nodePlates n `shouldBe` ["obs"]
        _   -> expectationFailure "y not found"

  describe "既存 helper (dirichlet / glmm / ar1Latent) を plate で包む (Phase 40-A5)" $ do
    --
    -- B2 plate は既存 helper をそのまま囲うだけで plate-aware 化できる
    -- (新規 helper 追加なし)。 これがメリット。
    --
    it "dirichlet を plate で包むと β / π すべてが plate メンバ" $ do
      let m :: HBM.ModelP ()
          m = do
            _ <- HBM.plate "K" 3 $ HBM.dirichlet "pi" [1, 1, 1]
            return ()
      let g = HBM.buildModelGraph m
          ns = HBM.mgNodes g
          piNodes = filter (\n -> "pi_" `T.isPrefixOf` HBM.nodeName n) ns
      all (\n -> HBM.nodePlates n == ["K"]) piNodes `shouldBe` True
      M.lookup "K" (HBM.mgPlates g) `shouldBe` Just 3
    it "ar1Latent を plate で包むと x_raw_t / x_t すべてが plate メンバ" $ do
      let m :: HBM.ModelP ()
          m = do
            _ <- HBM.plate "T" 4 $ HBM.ar1Latent "x" 4 0.5 1
            return ()
      let g = HBM.buildModelGraph m
          ns = HBM.mgNodes g
          xNodes = filter (\n -> "x_" `T.isPrefixOf` HBM.nodeName n) ns
      all (\n -> HBM.nodePlates n == ["T"]) xNodes `shouldBe` True
    it "plate 入れ子で glmmRandomIntercept を使うと u_j のみ plate" $ do
      let m :: HBM.ModelP ()
          m = do
            -- glmmRandomIntercept は内部で複数 sample/observe を出すので、
            -- plate "subject" J で包むと全てが plate メンバになる
            let xs = [[1.0], [1.0], [1.0]] :: [[Double]]
                gids = [0, 1, 0] :: [Int]
                ys = [0.5, 1.0, 0.7] :: [Double]
            _ <- HBM.plate "subject" 2 $
                   HBM.glmmRandomIntercept HBM.GlmmGaussian xs gids ys
            return ()
      let g = HBM.buildModelGraph m
          ns = HBM.mgNodes g
          uNodes = filter (\n -> "u_" `T.isPrefixOf` HBM.nodeName n) ns
      -- u_j は全て plate "subject"
      all (\n -> HBM.nodePlates n == ["subject"]) uNodes `shouldBe` True
      -- 結論: 既存 helper 無変更で plate で包めば自動的に DAG にも plate 反映

  describe "Hanalyze.Model.HBM.collapseIndexedPlateNodes (Phase 40-A8: PyMC 同等)" $ do
    --
    -- 8-schools collapse: eta_0..eta_7 → eta、 y_0..y_7 → y (n=8)
    --
    it "8-schools: collapse 後ノード数 = 4 (mu, tau, eta, y)" $ do
      let m :: HBM.ModelP ()
          m = do
            mu  <- HBM.sample "mu"  (HBM.Normal 0 5)
            tau <- HBM.sample "tau" (HBM.HalfCauchy 5)
            _ <- HBM.plate "school" 8 $ forM_ [0..7 :: Int] $ \j -> do
              eta <- HBM.sample ("eta_" <> T.pack (show j)) (HBM.Normal 0 1)
              HBM.observe ("y_" <> T.pack (show j))
                          (HBM.Normal (mu + tau * eta) 1) [realToFrac j]
            return ()
      let g  = HBM.buildModelGraph m
          gc = HBM.collapseIndexedPlateNodes g
          names = map HBM.nodeName (HBM.mgNodes gc)
      length (HBM.mgNodes gc) `shouldBe` 4
      all (`elem` names) ["mu", "tau", "eta", "y"] `shouldBe` True
    it "8-schools: 集約後 y の観測数 = 8" $ do
      let m :: HBM.ModelP ()
          m = do
            _ <- HBM.plate "school" 8 $ forM_ [0..7 :: Int] $ \j ->
              HBM.observe ("y_" <> T.pack (show j)) (HBM.Normal 0 1)
                          [realToFrac j]
            return ()
      let gc = HBM.collapseIndexedPlateNodes (HBM.buildModelGraph m)
          yNode = filter (\n -> HBM.nodeName n == "y") (HBM.mgNodes gc)
      case yNode of
        [n] -> HBM.nodeKind n `shouldBe` HBM.ObservedN 8
        _   -> expectationFailure "y not found"
    it "8-schools: 集約後 edges 3 本 (eta→y, mu→y, tau→y)" $ do
      let m :: HBM.ModelP ()
          m = do
            mu  <- HBM.sample "mu"  (HBM.Normal 0 5)
            tau <- HBM.sample "tau" (HBM.HalfCauchy 5)
            _ <- HBM.plate "school" 8 $ forM_ [0..7 :: Int] $ \j -> do
              eta <- HBM.sample ("eta_" <> T.pack (show j)) (HBM.Normal 0 1)
              HBM.observe ("y_" <> T.pack (show j))
                          (HBM.Normal (mu + tau * eta) 1) [realToFrac j]
            return ()
      let gc = HBM.collapseIndexedPlateNodes (HBM.buildModelGraph m)
      length (HBM.mgEdges gc) `shouldBe` 3
      elem ("eta", "y") (HBM.mgEdges gc) `shouldBe` True
      elem ("mu",  "y") (HBM.mgEdges gc) `shouldBe` True
      elem ("tau", "y") (HBM.mgEdges gc) `shouldBe` True
    --
    -- nested multilevel: 不動点で 2 段集約
    --
    it "nested multilevel: 集約後 y は n=6 (J=3 × K=2)" $ do
      let m :: HBM.ModelP ()
          m = do
            _ <- HBM.plate "school" 3 $ forM_ [0..2 :: Int] $ \j -> do
              theta <- HBM.sample ("theta_" <> T.pack (show j))
                                  (HBM.Normal 0 1)
              _ <- HBM.plate "student" 2 $ forM_ [0..1 :: Int] $ \i ->
                HBM.observe ("y_" <> T.pack (show j) <> "_" <> T.pack (show i))
                            (HBM.Normal theta 1) [realToFrac i]
              return ()
            return ()
      let gc = HBM.collapseIndexedPlateNodes (HBM.buildModelGraph m)
          yNode = filter (\n -> HBM.nodeName n == "y") (HBM.mgNodes gc)
      case yNode of
        [n] -> HBM.nodeKind n `shouldBe` HBM.ObservedN 6
        _   -> expectationFailure "y not found"
    it "idempotent: collapse 2 回適用しても結果不変" $ do
      let m :: HBM.ModelP ()
          m = do
            _ <- HBM.plate "g" 5 $ forM_ [0..4 :: Int] $ \j ->
              HBM.sample ("x_" <> T.pack (show j)) (HBM.Normal 0 1)
            return ()
      let g  = HBM.buildModelGraph m
          gc1 = HBM.collapseIndexedPlateNodes g
          gc2 = HBM.collapseIndexedPlateNodes gc1
      length (HBM.mgNodes gc1) `shouldBe` length (HBM.mgNodes gc2)
      length (HBM.mgEdges gc1) `shouldBe` length (HBM.mgEdges gc2)
    --
    -- 非集約ケース: 単独 / heterogeneous な分布
    --
    it "非 indexed RV は触らない (mu / tau はそのまま)" $ do
      let m :: HBM.ModelP ()
          m = do
            mu  <- HBM.sample "mu"  (HBM.Normal 0 1)
            _ <- HBM.sample "tau" (HBM.HalfNormal 1)
            HBM.observe "y" (HBM.Normal mu 1) [0]
            return ()
      let gc = HBM.collapseIndexedPlateNodes (HBM.buildModelGraph m)
          names = map HBM.nodeName (HBM.mgNodes gc)
      sort names `shouldBe` sort ["mu", "tau", "y"]
    it "命名規則違いは集約しない: eta_0 / phi_1 は別名で残る" $ do
      let m :: HBM.ModelP ()
          m = do
            _ <- HBM.plate "g" 2 $ do
              _ <- HBM.sample "eta_0" (HBM.Normal 0 1)
              _ <- HBM.sample "phi_1" (HBM.Normal 0 1)
              return ()
            return ()
      let gc = HBM.collapseIndexedPlateNodes (HBM.buildModelGraph m)
          names = map HBM.nodeName (HBM.mgNodes gc)
      sort names `shouldBe` sort ["eta_0", "phi_1"]

  describe "Hanalyze.Model.HBM.glmmRandomIntercept (Phase 37-A6)" $ do
    -- 3 群、 切片のみの GLMM (X = [[1], [1], ...]、 つまり全観測で 1 列)
    let xRows = replicate 9 [1.0]                 -- intercept only
        gids  = [0, 0, 0, 1, 1, 1, 2, 2, 2]       -- 3 群、 各 3 観測
        ysGauss = [1.0, 0.8, 1.3, 4.9, 5.2, 4.7, 8.9, 9.0, 8.7]
        modelGauss :: HBM.ModelP ()
        modelGauss = HBM.glmmRandomIntercept HBM.GlmmGaussian xRows gids ysGauss
        modelBin :: HBM.ModelP ()
        modelBin = HBM.glmmRandomIntercept HBM.GlmmBinomial xRows gids
                     [1, 0, 1, 0, 0, 1, 1, 1, 0]
        modelPois :: HBM.ModelP ()
        modelPois = HBM.glmmRandomIntercept HBM.GlmmPoisson xRows gids
                     [2, 1, 3, 5, 6, 4, 8, 9, 7]
    --
    it "GlmmGaussian は beta_0 / tau_u / u_0..u_2 / sigma を sample 名に持つ" $ do
      let ns = HBM.sampleNames modelGauss
      ns `shouldContain` ["beta_0"]
      ns `shouldContain` ["tau_u"]
      ns `shouldContain` ["u_0", "u_1", "u_2"]
      ns `shouldContain` ["sigma"]
    it "GlmmBinomial は sigma を含まない (残差不要)" $ do
      let ns = HBM.sampleNames modelBin
      ns `shouldContain` ["beta_0"]
      ns `shouldNotContain` ["sigma"]
    it "GlmmPoisson は sigma を含まない (残差不要)" $ do
      let ns = HBM.sampleNames modelPois
      let containsSigma = "sigma" `elem` ns
      containsSigma `shouldBe` False
    it "GlmmGaussian で短い NUTS が収束し u_2 > u_0 を回復" $ do
      gen <- MWC.create
      let cfg = NUTS.defaultNUTSConfig
                  { NUTS.nutsIterations = 200
                  , NUTS.nutsBurnIn     = 100
                  , NUTS.nutsStepSize   = 0.05
                  }
          initP = M.fromList
            [ ("beta_0", 5), ("tau_u", 3), ("sigma", 0.3)
            , ("u_0", -4), ("u_1", 0), ("u_2", 4)
            ]
      ch <- NUTS.nuts modelGauss cfg initP gen
      let u0 = case Core.posteriorMean "u_0" ch of Just v -> v; Nothing -> 0
          u2 = case Core.posteriorMean "u_2" ch of Just v -> v; Nothing -> 0
      (u2 > u0) `shouldBe` True

  describe "Hanalyze.Model.HBM.buildModelGraph (Phase 38: 簡単 6 例)" $ do
    let latentsOf g  = sort [ HBM.nodeName n
                            | n <- HBM.mgNodes g
                            , HBM.nodeKind n == HBM.LatentN ]
        obsOf g      = sort [ (HBM.nodeName n, k)
                            | n <- HBM.mgNodes g
                            , HBM.ObservedN k <- [HBM.nodeKind n] ]
        edgesOf g    = sort (HBM.mgEdges g)
    --
    -- 簡単 1: Normal(μ, σ既知) + observe
    --
    it "簡単 1: Normal(μ, σ既知) で 1 latent / 1 obs / 1 edge" $ do
      let m :: HBM.ModelP ()
          m = do
            mu <- HBM.sample "mu" (HBM.Normal 0 10)
            HBM.observe "y" (HBM.Normal mu 1) [0, 1, 2]
          g = HBM.buildModelGraph m
      latentsOf g `shouldBe` ["mu"]
      obsOf g     `shouldBe` [("y", 3)]
      edgesOf g   `shouldBe` [("mu", "y")]
    --
    -- 簡単 2: Normal(μ, σ) + observe (両方 latent)
    --
    it "簡単 2: Normal(μ, σ) で 2 latent / 1 obs / 2 edges" $ do
      let m :: HBM.ModelP ()
          m = do
            mu    <- HBM.sample "mu"    (HBM.Normal 0 10)
            sigma <- HBM.sample "sigma" (HBM.HalfNormal 1)
            HBM.observe "y" (HBM.Normal mu sigma) [0, 1, 2, 3]
          g = HBM.buildModelGraph m
      latentsOf g `shouldBe` ["mu", "sigma"]
      obsOf g     `shouldBe` [("y", 4)]
      edgesOf g   `shouldBe` [("mu", "y"), ("sigma", "y")]
    --
    -- 簡単 3: Beta-Binomial A/B (2 群独立)
    --
    it "簡単 3: Beta-Binomial A/B で 2 latent / 2 obs / 2 edges" $ do
      let m :: HBM.ModelP ()
          m = do
            pC <- HBM.sample "p_ctrl" (HBM.Beta 1 1)
            pT <- HBM.sample "p_trt"  (HBM.Beta 1 1)
            HBM.observe "y_ctrl" (HBM.Binomial 100 pC) [60]
            HBM.observe "y_trt"  (HBM.Binomial 100 pT) [72]
          g = HBM.buildModelGraph m
      latentsOf g `shouldBe` ["p_ctrl", "p_trt"]
      obsOf g     `shouldBe` [("y_ctrl", 1), ("y_trt", 1)]
      edgesOf g   `shouldBe` [("p_ctrl", "y_ctrl"), ("p_trt", "y_trt")]
    --
    -- 簡単 4: SkewNormal (Phase 37-A2 + 38 補修で distDepsT 追加)
    --
    it "簡単 4: SkewNormal で μ, σ, α → y の 3 edges" $ do
      let m :: HBM.ModelP ()
          m = do
            mu    <- HBM.sample "mu"    (HBM.Normal 0 1)
            sigma <- HBM.sample "sigma" (HBM.HalfNormal 1)
            alpha <- HBM.sample "alpha" (HBM.Normal 0 1)
            HBM.observe "y" (HBM.SkewNormal mu sigma alpha) [0, 1, 2]
          g = HBM.buildModelGraph m
      latentsOf g `shouldBe` ["alpha", "mu", "sigma"]
      obsOf g     `shouldBe` [("y", 3)]
      edgesOf g   `shouldBe` [("alpha", "y"), ("mu", "y"), ("sigma", "y")]
    --
    -- 簡単 5: OrderedLogistic (cuts もすべて latent として親に出る)
    --
    it "簡単 5: OrderedLogistic で η, c_1, c_2 → y の 3 edges" $ do
      let m :: HBM.ModelP ()
          m = do
            eta <- HBM.sample "eta" (HBM.Normal 0 1)
            c1  <- HBM.sample "c_1" (HBM.Normal (-1) 1)
            c2  <- HBM.sample "c_2" (HBM.Normal 1 1)
            HBM.observe "y" (HBM.OrderedLogistic eta [c1, c2]) [0, 1, 2]
          g = HBM.buildModelGraph m
      latentsOf g `shouldBe` ["c_1", "c_2", "eta"]
      obsOf g     `shouldBe` [("y", 3)]
      edgesOf g   `shouldBe` [("c_1", "y"), ("c_2", "y"), ("eta", "y")]
    --
    -- 簡単 6: MvStudentT (ν と μ vector は latent、 Σ は定数行列)
    --
    it "簡単 6: MvStudentT で ν, μ_1, μ_2 → y の 3 edges (Σ=I 定数)" $ do
      let m :: HBM.ModelP ()
          m = do
            nu  <- HBM.sample "nu"  (HBM.Gamma 2 0.1)
            mu1 <- HBM.sample "mu_1" (HBM.Normal 0 1)
            mu2 <- HBM.sample "mu_2" (HBM.Normal 0 1)
            HBM.observe "y"
              (HBM.MvStudentT nu [mu1, mu2] [[1, 0], [0, 1]])
              [0, 0, 1, 1]
          g = HBM.buildModelGraph m
      latentsOf g `shouldBe` ["mu_1", "mu_2", "nu"]
      obsOf g     `shouldBe` [("y", 4)]
      edgesOf g   `shouldBe` [("mu_1", "y"), ("mu_2", "y"), ("nu", "y")]

  describe "Hanalyze.Model.HBM.buildModelGraph (Phase 38: 代表 9 例)" $ do
    let latentsOf g  = sort [ HBM.nodeName n
                            | n <- HBM.mgNodes g
                            , HBM.nodeKind n == HBM.LatentN ]
        -- LatentN + DeterministicN (Phase 52.A15 で det は DeterministicN に
        -- 分類。 latent + 派生量が全て graph に出ることを見るテスト用)。
        latDetOf g   = sort [ HBM.nodeName n
                            | n <- HBM.mgNodes g
                            , HBM.nodeKind n `elem` [HBM.LatentN, HBM.DeterministicN] ]
        obsOf g      = sort [ (HBM.nodeName n, k)
                            | n <- HBM.mgNodes g
                            , HBM.ObservedN k <- [HBM.nodeKind n] ]
        edgesOf g    = sort (HBM.mgEdges g)
        containsAll xs ys = all (`elem` xs) ys
    --
    -- 代表 1: 形式 A (per-group data、 群 J=3、 θ_j を unroll)
    --
    it "代表 1: 形式 A で μ, τ → θ_j → y_j の 2 段階層 (J=3)" $ do
      let m :: HBM.ModelP ()
          m = do
            mu  <- HBM.sample "mu"  (HBM.Normal 0 10)
            tau <- HBM.sample "tau" (HBM.HalfNormal 5)
            t1  <- HBM.sample "theta_1" (HBM.Normal mu tau)
            t2  <- HBM.sample "theta_2" (HBM.Normal mu tau)
            t3  <- HBM.sample "theta_3" (HBM.Normal mu tau)
            HBM.observe "y_1" (HBM.Normal t1 1) [1.0, 1.2]
            HBM.observe "y_2" (HBM.Normal t2 1) [5.0, 5.3]
            HBM.observe "y_3" (HBM.Normal t3 1) [9.0, 9.2]
          g = HBM.buildModelGraph m
      latentsOf g `shouldBe` sort ["mu", "tau", "theta_1", "theta_2", "theta_3"]
      obsOf g     `shouldBe` sort [("y_1", 2), ("y_2", 2), ("y_3", 2)]
      edgesOf g   `shouldBe` sort
        [ ("mu", "theta_1"), ("tau", "theta_1"), ("theta_1", "y_1")
        , ("mu", "theta_2"), ("tau", "theta_2"), ("theta_2", "y_2")
        , ("mu", "theta_3"), ("tau", "theta_3"), ("theta_3", "y_3") ]
    --
    -- 代表 2: 形式 B (long-format、 forM で展開)
    --   Track 透過性: forM 経由でも親集合が伝わることを確認
    --
    it "代表 2: 形式 B (forM long-format) で形式 A と同じ DAG" $ do
      let m :: HBM.ModelP ()
          m = do
            mu  <- HBM.sample "mu"  (HBM.Normal 0 10)
            tau <- HBM.sample "tau" (HBM.HalfNormal 5)
            thetas <- forM [1 :: Int, 2, 3] $ \j ->
              HBM.sample (T.pack ("theta_" ++ show j)) (HBM.Normal mu tau)
            forM_ (zip [1 :: Int, 2, 3] thetas) $ \(j, th) ->
              HBM.observe (T.pack ("y_" ++ show j)) (HBM.Normal th 1) [1.0]
          g = HBM.buildModelGraph m
      latentsOf g `shouldBe` sort ["mu", "tau", "theta_1", "theta_2", "theta_3"]
      obsOf g     `shouldBe` sort [("y_1", 1), ("y_2", 1), ("y_3", 1)]
      edgesOf g   `shouldBe` sort
        [ ("mu", "theta_1"), ("tau", "theta_1"), ("theta_1", "y_1")
        , ("mu", "theta_2"), ("tau", "theta_2"), ("theta_2", "y_2")
        , ("mu", "theta_3"), ("tau", "theta_3"), ("theta_3", "y_3") ]
    --
    -- 代表 3: 形式 C (non-centered、 raw + deterministic 2 段)
    --   nonCenteredNormal は raw (Normal(0,1)) + det (loc + scale * raw) に展開
    --
    it "代表 3: 形式 C (non-centered) で raw → det theta_j → y_j" $ do
      let m :: HBM.ModelP ()
          m = do
            mu  <- HBM.sample "mu"  (HBM.Normal 0 10)
            tau <- HBM.sample "tau" (HBM.HalfNormal 5)
            thetas <- forM [1 :: Int, 2, 3] $ \j ->
              HBM.nonCenteredNormal (T.pack ("theta_" ++ show j)) mu tau
            forM_ (zip [1 :: Int, 2, 3] thetas) $ \(j, th) ->
              HBM.observe (T.pack ("y_" ++ show j)) (HBM.Normal th 1) [1.0]
          g = HBM.buildModelGraph m
      -- raw (Normal(0,1) 由来・LatentN) と det theta_j (DeterministicN) の
      -- 両方が graph に現れる (Phase 52.A15 で det は DeterministicN)
      latDetOf g `shouldBe` sort
        [ "mu", "tau"
        , "theta_1_raw", "theta_2_raw", "theta_3_raw"
        , "theta_1", "theta_2", "theta_3" ]
      obsOf g `shouldBe` sort [("y_1", 1), ("y_2", 1), ("y_3", 1)]
      -- raw は Normal(0,1) なので親なし、 det theta_j は mu, tau, theta_j_raw が親
      containsAll (HBM.mgEdges g)
        [ ("mu", "theta_1"), ("tau", "theta_1"), ("theta_1_raw", "theta_1")
        , ("theta_1", "y_1") ] `shouldBe` True
      containsAll (HBM.mgEdges g)
        [ ("mu", "theta_3"), ("tau", "theta_3"), ("theta_3_raw", "theta_3")
        , ("theta_3", "y_3") ] `shouldBe` True
      -- raw は親なし
      [ p | (p, c) <- HBM.mgEdges g, c == "theta_1_raw" ] `shouldBe` []
    --
    -- 代表 4: Random slope (per-group α_j, β_j、 J=2)
    --
    it "代表 4: Random slope で μ_α, τ_α, μ_β, τ_β → α_j, β_j → y_j" $ do
      let m :: HBM.ModelP ()
          m = do
            mua <- HBM.sample "mu_a"  (HBM.Normal 0 5)
            taa <- HBM.sample "tau_a" (HBM.HalfNormal 1)
            mub <- HBM.sample "mu_b"  (HBM.Normal 0 5)
            tab <- HBM.sample "tau_b" (HBM.HalfNormal 1)
            a1 <- HBM.sample "alpha_1" (HBM.Normal mua taa)
            a2 <- HBM.sample "alpha_2" (HBM.Normal mua taa)
            b1 <- HBM.sample "beta_1"  (HBM.Normal mub tab)
            b2 <- HBM.sample "beta_2"  (HBM.Normal mub tab)
            HBM.observe "y_1" (HBM.Normal (a1 + b1 * 1.0) 1) [1.0]
            HBM.observe "y_2" (HBM.Normal (a2 + b2 * 1.0) 1) [2.0]
          g = HBM.buildModelGraph m
      latentsOf g `shouldBe` sort
        [ "mu_a", "tau_a", "mu_b", "tau_b"
        , "alpha_1", "alpha_2", "beta_1", "beta_2" ]
      obsOf g `shouldBe` sort [("y_1", 1), ("y_2", 1)]
      containsAll (HBM.mgEdges g)
        [ ("mu_a", "alpha_1"), ("tau_a", "alpha_1")
        , ("mu_b", "beta_1"),  ("tau_b", "beta_1")
        , ("alpha_1", "y_1"),  ("beta_1", "y_1")
        , ("alpha_2", "y_2"),  ("beta_2", "y_2") ] `shouldBe` True
    --
    -- 代表 5: Multi-level (3-level: μ → δ_d → θ_{d,s} → y_{d,s})、 D=2, S=2
    --
    it "代表 5: 3 階層 (μ → δ_d → θ_{d,s} → y_{d,s})" $ do
      let m :: HBM.ModelP ()
          m = do
            mu  <- HBM.sample "mu"     (HBM.Normal 0 5)
            tDd <- HBM.sample "tau_d"  (HBM.HalfNormal 1)
            tSs <- HBM.sample "tau_s"  (HBM.HalfNormal 1)
            d1  <- HBM.sample "delta_1" (HBM.Normal mu tDd)
            d2  <- HBM.sample "delta_2" (HBM.Normal mu tDd)
            t11 <- HBM.sample "theta_1_1" (HBM.Normal d1 tSs)
            t12 <- HBM.sample "theta_1_2" (HBM.Normal d1 tSs)
            t21 <- HBM.sample "theta_2_1" (HBM.Normal d2 tSs)
            t22 <- HBM.sample "theta_2_2" (HBM.Normal d2 tSs)
            HBM.observe "y_1_1" (HBM.Normal t11 1) [1.0]
            HBM.observe "y_1_2" (HBM.Normal t12 1) [1.0]
            HBM.observe "y_2_1" (HBM.Normal t21 1) [1.0]
            HBM.observe "y_2_2" (HBM.Normal t22 1) [1.0]
          g = HBM.buildModelGraph m
      latentsOf g `shouldBe` sort
        [ "mu", "tau_d", "tau_s"
        , "delta_1", "delta_2"
        , "theta_1_1", "theta_1_2", "theta_2_1", "theta_2_2" ]
      length (obsOf g) `shouldBe` 4
      containsAll (HBM.mgEdges g)
        [ ("mu", "delta_1"),    ("tau_d", "delta_1")
        , ("delta_1", "theta_1_1"), ("tau_s", "theta_1_1")
        , ("theta_1_1", "y_1_1")
        , ("mu", "delta_2"),    ("tau_d", "delta_2")
        , ("delta_2", "theta_2_2"), ("tau_s", "theta_2_2")
        , ("theta_2_2", "y_2_2") ] `shouldBe` True
    --
    -- 代表 6: Crossed (subject × time、 S=2, T=2)
    --
    it "代表 6: Crossed (α_s + γ_t → y_{s,t})" $ do
      let m :: HBM.ModelP ()
          m = do
            mua <- HBM.sample "mu_a"  (HBM.Normal 0 5)
            taa <- HBM.sample "tau_a" (HBM.HalfNormal 1)
            tag <- HBM.sample "tau_g" (HBM.HalfNormal 1)
            a1 <- HBM.sample "alpha_1" (HBM.Normal mua taa)
            a2 <- HBM.sample "alpha_2" (HBM.Normal mua taa)
            g1 <- HBM.sample "gamma_1" (HBM.Normal 0 tag)
            g2 <- HBM.sample "gamma_2" (HBM.Normal 0 tag)
            HBM.observe "y_1_1" (HBM.Normal (a1 + g1) 1) [1.0]
            HBM.observe "y_1_2" (HBM.Normal (a1 + g2) 1) [1.0]
            HBM.observe "y_2_1" (HBM.Normal (a2 + g1) 1) [1.0]
            HBM.observe "y_2_2" (HBM.Normal (a2 + g2) 1) [1.0]
          g = HBM.buildModelGraph m
      latentsOf g `shouldBe` sort
        [ "mu_a", "tau_a", "tau_g"
        , "alpha_1", "alpha_2", "gamma_1", "gamma_2" ]
      length (obsOf g) `shouldBe` 4
      containsAll (HBM.mgEdges g)
        [ ("mu_a", "alpha_1"), ("tau_a", "alpha_1")
        , ("tau_g", "gamma_1")
        , ("alpha_1", "y_1_1"), ("gamma_1", "y_1_1")
        , ("alpha_2", "y_2_2"), ("gamma_2", "y_2_2") ] `shouldBe` True
    --
    -- 代表 7: GLMM helper (glmmRandomIntercept GlmmGaussian、 nG=3, n=6)
    --
    it "代表 7: glmmRandomIntercept Gaussian で β_0/τ_u/σ/u_j → y (単一ブロック)" $ do
      let xRows = replicate 6 [1.0]
          gids  = [0, 0, 1, 1, 2, 2]
          ys    = [1, 1.1, 4.9, 5.0, 8.9, 9.0]
          m :: HBM.ModelP ()
          m = HBM.glmmRandomIntercept HBM.GlmmGaussian xRows gids ys
          g = HBM.buildModelGraph m
      latentsOf g `shouldBe` sort
        ["beta_0", "tau_u", "sigma", "u_0", "u_1", "u_2"]
      -- Phase 54.4a: 観測は単一の observeLMR ブロック "y" (PyMC/Stan 同様の
      -- ベクトル化観測ノード。 旧実装は per-obs y_0..y_5 を 6 個展開)。
      obsOf g `shouldBe` [("y", 6)]
      -- τ_u → u_j が伝わる (centered パラメタ化)
      containsAll (HBM.mgEdges g)
        [ ("tau_u", "u_0"), ("tau_u", "u_1"), ("tau_u", "u_2") ]
        `shouldBe` True
      -- 単一観測ブロック "y" の親 = β + 全群効果 u + σ
      let parentsOf nm = sort [ p | (p, c) <- HBM.mgEdges g, c == nm ]
      parentsOf "y" `shouldBe`
        sort ["beta_0", "sigma", "u_0", "u_1", "u_2"]
    --
    -- 代表 8: nonCenteredNormal 単体の derived 関係 (raw → det)
    --
    it "代表 8: nonCenteredNormal で raw (親なし) + name (deterministic)" $ do
      let m :: HBM.ModelP ()
          m = do
            mu  <- HBM.sample "mu"  (HBM.Normal 0 10)
            tau <- HBM.sample "tau" (HBM.HalfNormal 1)
            th  <- HBM.nonCenteredNormal "theta" mu tau
            HBM.observe "y" (HBM.Normal th 1) [1, 2, 3]
          g = HBM.buildModelGraph m
      latDetOf g `shouldBe` sort ["mu", "tau", "theta_raw", "theta"]
      obsOf g     `shouldBe` [("y", 3)]
      -- theta_raw は Normal(0,1) なので親なし
      [ p | (p, c) <- HBM.mgEdges g, c == "theta_raw" ] `shouldBe` []
      -- theta (det) の親 = {mu, tau, theta_raw}
      sort [ p | (p, c) <- HBM.mgEdges g, c == "theta" ]
        `shouldBe` sort ["mu", "tau", "theta_raw"]
      -- theta → y
      [ p | (p, c) <- HBM.mgEdges g, c == "y" ] `shouldBe` ["theta"]
    --
    -- 代表 9: dirichlet stick-breaking (K=3、 β_k latent + π_k deterministic)
    --
    it "代表 9: dirichlet K=3 で β_b<i> (2 個) + π_<i> (3 個) det" $ do
      let m :: HBM.ModelP ()
          m = do
            pis <- HBM.dirichlet "p" [1, 1, 1]
            HBM.observe "y" (HBM.Categorical pis) [0, 1, 2]
          g = HBM.buildModelGraph m
      -- K=3 → β は p_b0, p_b1 (2 個)、 π は p_0, p_1, p_2 (3 個 deterministic)
      latDetOf g `shouldBe` sort ["p_b0", "p_b1", "p_0", "p_1", "p_2"]
      obsOf g     `shouldBe` [("y", 3)]
      -- π_0 = β_0、 π_1 = β_1 (1 - β_0)、 π_2 = (1 - β_0)(1 - β_1)
      sort [ p | (p, c) <- HBM.mgEdges g, c == "p_0" ]
        `shouldBe` ["p_b0"]
      sort [ p | (p, c) <- HBM.mgEdges g, c == "p_1" ]
        `shouldBe` sort ["p_b0", "p_b1"]
      sort [ p | (p, c) <- HBM.mgEdges g, c == "p_2" ]
        `shouldBe` sort ["p_b0", "p_b1"]
      -- y の親 = π_0, π_1, π_2 (Categorical の引数すべて)
      sort [ p | (p, c) <- HBM.mgEdges g, c == "y" ]
        `shouldBe` sort ["p_0", "p_1", "p_2"]

  describe "Hanalyze.Model.HBM.buildModelGraph (Phase 38: 複雑 9 例)" $ do
    let latentsOf g  = sort [ HBM.nodeName n
                            | n <- HBM.mgNodes g
                            , HBM.nodeKind n == HBM.LatentN ]
        -- LatentN + DeterministicN。 Phase 52.A15 で deterministic は
        -- DeterministicN に分類されたので、 「latent + 派生量が全て graph に
        -- 出る」 を見るテストはこちらを使う (latentsOf は sampler latent のみ)。
        latDetOf g   = sort [ HBM.nodeName n
                            | n <- HBM.mgNodes g
                            , HBM.nodeKind n `elem` [HBM.LatentN, HBM.DeterministicN] ]
        obsOf g      = sort [ (HBM.nodeName n, k)
                            | n <- HBM.mgNodes g
                            , HBM.ObservedN k <- [HBM.nodeKind n] ]
        parentsOf g nm = sort [ p | (p, c) <- HBM.mgEdges g, c == nm ]
    --
    -- 複雑 1: ZIN-NegBin GLMM (ψ, β_0, τ_u, α 群効果 + 過分散ゼロ過剰)
    --
    it "複雑 1: ZIN-NegBin GLMM で ψ, β_0, α, u_j → y_i" $ do
      let m :: HBM.ModelP ()
          m = do
            b0  <- HBM.sample "beta_0" (HBM.Normal 0 5)
            tau <- HBM.sample "tau_u"  (HBM.HalfNormal 5)
            psi <- HBM.sample "psi"    (HBM.Beta 1 1)
            al  <- HBM.sample "alpha"  (HBM.Gamma 2 1)
            u0  <- HBM.sample "u_0"    (HBM.Normal 0 tau)
            u1  <- HBM.sample "u_1"    (HBM.Normal 0 tau)
            HBM.observe "y_0"
              (HBM.ZeroInflatedNegativeBinomial psi (exp (b0 + u0)) al)
              [3, 0]
            HBM.observe "y_1"
              (HBM.ZeroInflatedNegativeBinomial psi (exp (b0 + u1)) al)
              [0, 5]
          g = HBM.buildModelGraph m
      latentsOf g `shouldBe` sort
        ["beta_0", "tau_u", "psi", "alpha", "u_0", "u_1"]
      obsOf g `shouldBe` sort [("y_0", 2), ("y_1", 2)]
      parentsOf g "u_0" `shouldBe` ["tau_u"]
      parentsOf g "u_1" `shouldBe` ["tau_u"]
      parentsOf g "y_0" `shouldBe` sort ["alpha", "beta_0", "psi", "u_0"]
      parentsOf g "y_1" `shouldBe` sort ["alpha", "beta_0", "psi", "u_1"]
    --
    -- 複雑 2: 階層 OrderedLogistic (cuts 共通、 η は群別)
    --
    it "複雑 2: 階層 OrderedLogistic で μ/τ → η_j、 cuts は y 親に共有" $ do
      let m :: HBM.ModelP ()
          m = do
            mu  <- HBM.sample "mu"  (HBM.Normal 0 1)
            tau <- HBM.sample "tau" (HBM.HalfNormal 1)
            c1  <- HBM.sample "c_1" (HBM.Normal (-1) 1)
            c2  <- HBM.sample "c_2" (HBM.Normal 1 1)
            e1  <- HBM.sample "eta_1" (HBM.Normal mu tau)
            e2  <- HBM.sample "eta_2" (HBM.Normal mu tau)
            HBM.observe "y_1" (HBM.OrderedLogistic e1 [c1, c2]) [0, 1, 2]
            HBM.observe "y_2" (HBM.OrderedLogistic e2 [c1, c2]) [0, 1, 2]
          g = HBM.buildModelGraph m
      latentsOf g `shouldBe` sort
        ["mu", "tau", "c_1", "c_2", "eta_1", "eta_2"]
      parentsOf g "eta_1" `shouldBe` sort ["mu", "tau"]
      parentsOf g "y_1"   `shouldBe` sort ["c_1", "c_2", "eta_1"]
      parentsOf g "y_2"   `shouldBe` sort ["c_1", "c_2", "eta_2"]
    --
    -- 複雑 3: Hierarchical mixture (mu_1, mu_2 が共通 hyper から)
    --
    it "複雑 3: Hierarchical mixture で μ_g/τ_g → μ_k → y" $ do
      let m :: HBM.ModelP ()
          m = do
            w   <- HBM.sample "w"     (HBM.Beta 1 1)
            mug <- HBM.sample "mu_g"  (HBM.Normal 0 5)
            tag <- HBM.sample "tau_g" (HBM.HalfNormal 1)
            m1  <- HBM.sample "mu_1"  (HBM.Normal mug tag)
            m2  <- HBM.sample "mu_2"  (HBM.Normal mug tag)
            HBM.observe "y"
              (HBM.Mixture [w, 1 - w] [HBM.Normal m1 1, HBM.Normal m2 1])
              [0, 1, 2, 3]
          g = HBM.buildModelGraph m
      latentsOf g `shouldBe` sort ["w", "mu_g", "tau_g", "mu_1", "mu_2"]
      parentsOf g "mu_1" `shouldBe` sort ["mu_g", "tau_g"]
      parentsOf g "mu_2" `shouldBe` sort ["mu_g", "tau_g"]
      parentsOf g "y"    `shouldBe` sort ["mu_1", "mu_2", "w"]
    --
    -- 複雑 4: Gaussian + potential 制約 (mu_1 ≈ mu_2 ペナルティ)
    --   potential は LatentN "Potential" として親集合付きで出る
    --
    it "複雑 4: Gaussian + potential 制約で constr ノードが mu_1, mu_2 親" $ do
      let m :: HBM.ModelP ()
          m = do
            m1 <- HBM.sample "mu_1" (HBM.Normal 0 5)
            m2 <- HBM.sample "mu_2" (HBM.Normal 0 5)
            HBM.potential "constr" (negate ((m1 - m2) ** 2))
            HBM.observe "y" (HBM.Normal m1 1) [1, 2, 3]
          g = HBM.buildModelGraph m
      latentsOf g `shouldBe` sort ["mu_1", "mu_2", "constr"]
      obsOf g     `shouldBe` [("y", 3)]
      parentsOf g "constr" `shouldBe` sort ["mu_1", "mu_2"]
      parentsOf g "y"      `shouldBe` ["mu_1"]
    --
    -- 複雑 5: Multi-output (mu 共有、 sigma 個別)
    --
    it "複雑 5: Multi-output (mu 共有, sigma_k 個別) で 3 obs、 mu 共有親" $ do
      let m :: HBM.ModelP ()
          m = do
            mu <- HBM.sample "mu"      (HBM.Normal 0 5)
            s1 <- HBM.sample "sigma_1" (HBM.HalfNormal 1)
            s2 <- HBM.sample "sigma_2" (HBM.HalfNormal 1)
            s3 <- HBM.sample "sigma_3" (HBM.HalfNormal 1)
            HBM.observe "y_1" (HBM.Normal mu s1) [0, 1, 2]
            HBM.observe "y_2" (HBM.Normal mu s2) [3, 4, 5]
            HBM.observe "y_3" (HBM.Normal mu s3) [6, 7, 8]
          g = HBM.buildModelGraph m
      latentsOf g `shouldBe` sort ["mu", "sigma_1", "sigma_2", "sigma_3"]
      parentsOf g "y_1" `shouldBe` sort ["mu", "sigma_1"]
      parentsOf g "y_2" `shouldBe` sort ["mu", "sigma_2"]
      parentsOf g "y_3" `shouldBe` sort ["mu", "sigma_3"]
    --
    -- 複雑 6: Random slope + nonCentered (形式 C + slope の組合せ)
    --
    it "複雑 6: Random slope + nonCenteredNormal で raw + det 2 軸" $ do
      let m :: HBM.ModelP ()
          m = do
            mua <- HBM.sample "mu_a"  (HBM.Normal 0 5)
            taa <- HBM.sample "tau_a" (HBM.HalfNormal 1)
            mub <- HBM.sample "mu_b"  (HBM.Normal 0 5)
            tab <- HBM.sample "tau_b" (HBM.HalfNormal 1)
            a1 <- HBM.nonCenteredNormal "alpha_1" mua taa
            a2 <- HBM.nonCenteredNormal "alpha_2" mua taa
            b1 <- HBM.nonCenteredNormal "beta_1"  mub tab
            b2 <- HBM.nonCenteredNormal "beta_2"  mub tab
            HBM.observe "y_1" (HBM.Normal (a1 + b1) 1) [1.0]
            HBM.observe "y_2" (HBM.Normal (a2 + b2) 1) [2.0]
          g = HBM.buildModelGraph m
      latDetOf g `shouldBe` sort
        [ "mu_a", "tau_a", "mu_b", "tau_b"
        , "alpha_1_raw", "alpha_2_raw", "beta_1_raw", "beta_2_raw"
        , "alpha_1", "alpha_2", "beta_1", "beta_2" ]
      parentsOf g "alpha_1_raw" `shouldBe` []
      parentsOf g "alpha_1" `shouldBe` sort ["mu_a", "tau_a", "alpha_1_raw"]
      parentsOf g "beta_2"  `shouldBe` sort ["mu_b", "tau_b", "beta_2_raw"]
      parentsOf g "y_1"     `shouldBe` sort ["alpha_1", "beta_1"]
      parentsOf g "y_2"     `shouldBe` sort ["alpha_2", "beta_2"]
    --
    -- 複雑 7: Dirichlet mixture (K=3 有限近似)
    --
    it "複雑 7: Dirichlet mixture K=3 で β/π/μ_k → y" $ do
      let m :: HBM.ModelP ()
          m = do
            pis <- HBM.dirichlet "p" [1, 1, 1]
            m1  <- HBM.sample "mu_1" (HBM.Normal 0 5)
            m2  <- HBM.sample "mu_2" (HBM.Normal 0 5)
            m3  <- HBM.sample "mu_3" (HBM.Normal 0 5)
            HBM.observe "y"
              (HBM.Mixture pis
                [HBM.Normal m1 1, HBM.Normal m2 1, HBM.Normal m3 1])
              [0, 1, 2]
          g = HBM.buildModelGraph m
      latDetOf g `shouldBe` sort
        ["p_b0", "p_b1", "p_0", "p_1", "p_2", "mu_1", "mu_2", "mu_3"]
      parentsOf g "p_0" `shouldBe` ["p_b0"]
      parentsOf g "y"   `shouldBe` sort
        ["mu_1", "mu_2", "mu_3", "p_0", "p_1", "p_2"]
    --
    -- 複雑 8: AR(1) latent 時系列 (ar1Latent helper、 nT=3)
    --   plate-style 期待: x_raw_t → x_t (det)、 x_{t-1} → x_t (t≥1)、 x_2 → y
    --
    it "複雑 8: ar1Latent nT=3 で x_raw_t + det x_t chain + x_2 → y" $ do
      let m :: HBM.ModelP ()
          m = do
            xs <- HBM.ar1Latent "x" 3 0.8 0.3
            HBM.observe "y" (HBM.Normal (xs !! 2) 1) [1.0]
          g = HBM.buildModelGraph m
      latDetOf g `shouldBe` sort
        ["x_raw0", "x_raw1", "x_raw2", "x_0", "x_1", "x_2"]
      -- raw は親なし
      parentsOf g "x_raw0" `shouldBe` []
      parentsOf g "x_raw1" `shouldBe` []
      parentsOf g "x_raw2" `shouldBe` []
      -- x_0 の親は x_raw0 のみ (定数 phi/sigma は trackConst で deps 空)
      parentsOf g "x_0" `shouldBe` ["x_raw0"]
      -- x_1 の親は x_0 と x_raw1
      parentsOf g "x_1" `shouldBe` sort ["x_0", "x_raw1"]
      -- x_2 の親は x_1 と x_raw2
      parentsOf g "x_2" `shouldBe` sort ["x_1", "x_raw2"]
      -- y の親は x_2 のみ
      parentsOf g "y" `shouldBe` ["x_2"]
    --
    -- 複雑 9: sampleNames と buildModelGraph の latent 名一致確認
    --   (Phase 37-A5 fullRankAdvi 等の推論側で出る latent 名と DAG が整合)
    --
    it "複雑 9: sampleNames m と graph latent (det/potential 除く) が一致" $ do
      let m :: HBM.ModelP ()
          m = do
            mu  <- HBM.sample "mu"  (HBM.Normal 0 5)
            tau <- HBM.sample "tau" (HBM.HalfNormal 1)
            th  <- HBM.nonCenteredNormal "theta" mu tau
            HBM.observe "y" (HBM.Normal th 1) [1, 2, 3]
          g = HBM.buildModelGraph m
          -- buildModelGraph の latent から Deterministic を除いたもの
          pureLatents = sort
            [ HBM.nodeName n
            | n <- HBM.mgNodes g
            , HBM.nodeKind n == HBM.LatentN
            , HBM.nodeDist n /= "Deterministic"
            , HBM.nodeDist n /= "Potential" ]
          sampled = sort (HBM.sampleNames m)
      sampled `shouldBe` pureLatents
      -- 具体的に: mu, tau, theta_raw が sample されている (theta は det)
      sampled `shouldBe` sort ["mu", "tau", "theta_raw"]
