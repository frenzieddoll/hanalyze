{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Model.HBM.LogpSpec (spec) where

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
import Data.List (sort, nub, mapAccumL, zip4)
import Control.Monad (forM, forM_)
import System.IO.Temp (withSystemTempFile)
import System.IO     (hPutStr, hClose)
import           Hanalyze.Model.HBM.Ast (Expr (..), Lit (..), DoStmt (..), Err)
import           Data.IORef         (newIORef, readIORef, modifyIORef')
import qualified Data.Text   as T
import qualified Numeric.AD.Mode.Reverse.Double as RevD
import qualified Data.Map.Strict as M
import qualified Data.Set        as Set
import qualified Hanalyze.Model.HBM as HBM
import           Hanalyze.Fit (designHBMProgram)
import qualified Data.Map.Strict    as M
import SpecHelper

spec :: Spec
spec = do
  describe "Phase 53: gradADU 勾配の正しさ (中心差分 ground truth)" $ do
    let centralGrad m names trans us =
          let h = 1e-5
              f vs = HBM.logJointUnconstrained m names trans
                       (M.fromList (zip names vs))
              bump i d = [ if j == i then u + d else u
                         | (j, u) <- zip [(0 :: Int) ..] us ]
          in [ (f (bump i h) - f (bump i (-h))) / (2 * h)
             | i <- [0 .. length us - 1] ]
        closeVec tol a b =
          length a == length b &&
          and [ abs (x - y) <= tol * (1 + abs y) | (x, y) <- zip a b ]

    it "M1 pooled 回帰 (latent 3: Normal×2 + Exp): gradADU ≈ 中心差分" $ do
      let xs = [-1.0, -0.4, 0.2, 0.8, 1.5] :: [Double]
          ys = [0.3, 1.1, 2.4, 3.0, 4.2] :: [Double]
          m :: HBM.ModelP ()
          m = do
            a <- HBM.sample "a"     (HBM.Normal 0 10)
            b <- HBM.sample "b"     (HBM.Normal 0 10)
            s <- HBM.sample "sigma" (HBM.Exponential 1)
            mapM_ (\(i, (x, y)) ->
                     HBM.observe (T.pack ("y_" ++ show (i :: Int)))
                       (HBM.Normal (a + b * realToFrac x) s) [y])
                  (zip [0 ..] (zip xs ys))
          names = HBM.sampleNames m
          tmap  = HBM.getTransforms m
          trans = [ tmap M.! n | n <- names ]
          us    = [0.3, -0.2, 0.1]
      closeVec 1e-4 (HBM.gradADU m names trans us) (centralGrad m names trans us)
        `shouldBe` True

    it "M2 random intercept GLMM (latent 6: HalfNormal/Exp 含む): gradADU ≈ 中心差分" $ do
      let xRows = [ [1.0, -0.5], [1.0, 0.3], [1.0, 1.2]
                  , [1.0, -0.8], [1.0, 0.6], [1.0, 0.1] ]
          gids  = [0, 0, 0, 1, 1, 1]
          ys    = [0.2, 0.9, 1.8, 0.5, 1.3, 1.0] :: [Double]
          m :: HBM.ModelP ()
          m = HBM.glmmRandomIntercept HBM.GlmmGaussian xRows gids ys
          names = HBM.sampleNames m
          tmap  = HBM.getTransforms m
          trans = [ tmap M.! n | n <- names ]
          us    = take (length names) [0.1, -0.2, 0.15, 0.05, -0.1, 0.2, 0.3, 0.0]
      closeVec 1e-4 (HBM.gradADU m names trans us) (centralGrad m names trans us)
        `shouldBe` True

    it "GP-RBF (MvNormalGpRBF・Phase 95 B-dsl 閉形式随伴): gradADU ≈ 中心差分" $ do
      -- gp_pois_regr データ (N=11)。 gradADU → compileGradUV → gpRBFAnalyticVG
      -- (Cholesky を AD tape に載せない閉形式随伴) が真の密度の中心差分と一致するか。
      let xs = [-10,-8,-6,-4,-2,0,2,4,6,8,10] :: [Double]
          ys = [ 4.75906, 1.59423, 2.99548, 5.27501, 1.66472, 2.24347
               , 2.8914, 4.08681, 4.60588, 0.802364, 3.92136 ] :: [Double]
          m :: HBM.ModelP ()
          m = do
            rho   <- HBM.sample "rho"   (HBM.Gamma 25 4)
            alpha <- HBM.sample "alpha" (HBM.HalfNormal 2)
            sigma <- HBM.sample "sigma" (HBM.HalfNormal 1)
            HBM.observeMV "y"
              (HBM.MvNormalGpRBF (map realToFrac xs) alpha rho sigma) [ys]
          names = HBM.sampleNames m
          tmap  = HBM.getTransforms m
          trans = [ tmap M.! n | n <- names ]
          us    = [1.8, 0.88, 0.59]   -- exp(u) ≈ posterior 近傍 (ρ6,α2.4,σ1.8)
      closeVec 1e-4 (HBM.gradADU m names trans us) (centralGrad m names trans us)
        `shouldBe` True

    it "HMM (HmmForwardNormal・Phase 92 A2 forward-backward 閉形式随伴): gradADU ≈ 中心差分" $ do
      -- 14-hmm-example と同構造 (mu_2=mu_1+gap の順序制約 + dirichlet 遷移行)。
      -- gradADU → gradValPlan → hmmAnalyticVG (forward-backward・AD tape ゼロ) が
      -- 真の密度 (obsLogSum = hmmForwardLogLik) の中心差分と一致するか。
      let ys = [0.5, 3.2, 9.8, 10.1, 2.7, 9.5, 0.1, 10.4] :: [Double]
          kk = 2 :: Int
          m :: HBM.ModelP ()
          m = do
            mu1 <- HBM.sample "mu_1" (HBM.Normal 3 1)
            gap <- HBM.sample "gap" (HBM.HalfNormal 5)
            mu2 <- HBM.deterministic "mu_2" (mu1 + gap)
            HBM.potential "mu2_prior"
              (HBM.logDensity (HBM.Normal 10 1) mu2
                 - HBM.logDensity (HBM.HalfNormal 5) gap)
            th1 <- HBM.dirichlet "theta1" (replicate kk 1)
            th2 <- HBM.dirichlet "theta2" (replicate kk 1)
            HBM.observeMV "y_seq"
              (HBM.HmmForwardNormal (replicate kk 1) [th1, th2] [mu1, mu2] 1) [ys]
          names = HBM.sampleNames m
          tmap  = HBM.getTransforms m
          trans = [ tmap M.! n | n <- names ]
          us    = [0.4, 0.9, 0.3, -0.5]   -- mu_1, gap(log), theta1_b0(logit), theta2_b0(logit)
      closeVec 1e-4 (HBM.gradADU m names trans us) (centralGrad m names trans us)
        `shouldBe` True

    it "HmmForwardNormal 密度 = 従来 potential (hmmForwardLogLik) 書きと一致 (Phase 92 A2)" $ do
      -- 同一パラメータ点で logJointUnconstrained が旧書き方と一致 (移行の同値性)。
      let ys = [0.5, 3.2, 9.8, 10.1, 2.7, 9.5, 0.1, 10.4] :: [Double]
          kk = 2 :: Int
          mNew :: HBM.ModelP ()
          mNew = do
            mu1 <- HBM.sample "mu_1" (HBM.Normal 3 1)
            gap <- HBM.sample "gap" (HBM.HalfNormal 5)
            _mu2 <- HBM.deterministic "mu_2" (mu1 + gap)
            th1 <- HBM.dirichlet "theta1" (replicate kk 1)
            th2 <- HBM.dirichlet "theta2" (replicate kk 1)
            HBM.observeMV "y_seq"
              (HBM.HmmForwardNormal (replicate kk 1) [th1, th2] [mu1, mu1 + gap] 1) [ys]
          mOld :: HBM.ModelP ()
          mOld = do
            mu1 <- HBM.sample "mu_1" (HBM.Normal 3 1)
            gap <- HBM.sample "gap" (HBM.HalfNormal 5)
            _mu2 <- HBM.deterministic "mu_2" (mu1 + gap)
            th1 <- HBM.dirichlet "theta1" (replicate kk 1)
            th2 <- HBM.dirichlet "theta2" (replicate kk 1)
            let emit = [ [ HBM.logDensity (HBM.Normal mu 1) (realToFrac y)
                         | mu <- [mu1, mu1 + gap] ] | y <- ys ]
            HBM.potential "hmm_loglik"
              (HBM.hmmForwardLogLik (replicate kk 1) [th1, th2] emit)
          names = HBM.sampleNames mNew
          tmap  = HBM.getTransforms mNew
          trans = [ tmap M.! n | n <- names ]
          us    = M.fromList (zip names ([0.4, 0.9, 0.3, -0.5] :: [Double]))
      abs (HBM.logJointUnconstrained mNew names trans us
             - HBM.logJointUnconstrained mOld names trans us)
        `shouldSatisfy` (< 1e-10)

    it "ARMA(1,1) (ArmaNormal・Phase 101 A2 逆向き随伴の閉形式): gradADU ≈ 中心差分" $ do
      -- 22-arma と同構造 (μ/φ/θ Normal prior + σ HalfCauchy)。gradADU →
      -- gradValPlan → armaAnalyticVG (逆向き随伴再帰・AD tape ゼロ) が
      -- 真の密度 (obsLogSum = err 再帰) の中心差分と一致するか。
      let ys = [0.5, 0.8, 0.2, -0.4, 0.9, 1.3, 0.1, -0.7, 0.4, 0.6] :: [Double]
          m :: HBM.ModelP ()
          m = do
            mu    <- HBM.sample "mu"    (HBM.Normal 0 10)
            phi   <- HBM.sample "phi"   (HBM.Normal 0 2)
            theta <- HBM.sample "theta" (HBM.Normal 0 2)
            sigma <- HBM.sample "sigma" (HBM.HalfCauchy 2.5)
            HBM.observeMV "y_seq" (HBM.ArmaNormal mu phi theta sigma) [ys]
          names = HBM.sampleNames m
          tmap  = HBM.getTransforms m
          trans = [ tmap M.! n | n <- names ]
          us    = [0.3, 0.7, -0.2, -0.4]   -- mu, phi, theta, sigma(log)
      closeVec 1e-4 (HBM.gradADU m names trans us) (centralGrad m names trans us)
        `shouldBe` True

    it "ArmaNormal 密度 = 従来 mapAccumL + potential 書きと一致 (Phase 101 A2)" $ do
      -- 同一パラメータ点で logJointUnconstrained が旧書き方と一致 (移行の同値性)。
      let ys = [0.5, 0.8, 0.2, -0.4, 0.9, 1.3, 0.1, -0.7, 0.4, 0.6] :: [Double]
          mNew :: HBM.ModelP ()
          mNew = do
            mu    <- HBM.sample "mu"    (HBM.Normal 0 10)
            phi   <- HBM.sample "phi"   (HBM.Normal 0 2)
            theta <- HBM.sample "theta" (HBM.Normal 0 2)
            sigma <- HBM.sample "sigma" (HBM.HalfCauchy 2.5)
            HBM.observeMV "y_seq" (HBM.ArmaNormal mu phi theta sigma) [ys]
          mOld :: HBM.ModelP ()
          mOld = do
            mu    <- HBM.sample "mu"    (HBM.Normal 0 10)
            phi   <- HBM.sample "phi"   (HBM.Normal 0 2)
            theta <- HBM.sample "theta" (HBM.Normal 0 2)
            sigma <- HBM.sample "sigma" (HBM.HalfCauchy 2.5)
            let (y1 : rest) = map realToFrac ys
                e1 = y1 - (mu + phi * mu)
                step (prevY, prevErr) yt =
                  let err = yt - (mu + phi * prevY + theta * prevErr)
                  in ((yt, err), err)
                errs = e1 : snd (mapAccumL step (y1, e1) rest)
            HBM.potential "arma_loglik"
              (sum [ HBM.logDensity (HBM.Normal 0 sigma) e | e <- errs ])
          names = HBM.sampleNames mNew
          tmap  = HBM.getTransforms mNew
          trans = [ tmap M.! n | n <- names ]
          us    = M.fromList (zip names ([0.3, 0.7, -0.2, -0.4] :: [Double]))
      abs (HBM.logJointUnconstrained mNew names trans us
             - HBM.logJointUnconstrained mOld names trans us)
        `shouldSatisfy` (< 1e-10)

    it "graded response IRT (GradedResponseIrt・Phase 101 A3 解析勾配): gradADU ≈ 中心差分" $ do
      -- 20-bones と同構造 (theta のみ latent・delta/gamma/ncat は定数 data・
      -- 欠測 −1 スキップ込)。gradADU → gradValPlan → gradedIrtAnalyticVG
      -- (dQ/dθ = δ·Q(1−Q) の隣接差・AD tape ゼロ) が真の密度の中心差分と一致するか。
      let ncats  = [3, 2] :: [Int]
          deltas = [1.2, 0.7] :: [Double]
          gammas = [[-0.5, 0.8], [0.1]] :: [[Double]]
          grades = [1, 2, 3, -1, 2, 1] :: [Double]   -- 3 child × 2 item 行優先 (−1 = 欠測)
          m :: HBM.ModelP ()
          m = do
            ths <- mapM (\i -> HBM.sample (T.pack ("theta_" ++ show (i :: Int)))
                                 (HBM.Normal 0 6)) [0 .. 2]
            HBM.observeMV "grades" (HBM.GradedResponseIrt ths ncats deltas gammas) [grades]
          names = HBM.sampleNames m
          tmap  = HBM.getTransforms m
          trans = [ tmap M.! n | n <- names ]
          us    = [0.4, -0.8, 1.1]
      closeVec 1e-4 (HBM.gradADU m names trans us) (centralGrad m names trans us)
        `shouldBe` True

    it "GradedResponseIrt 密度 = 従来 logCatProb + potential 書きと一致 (Phase 101 A3)" $ do
      -- 同一パラメータ点で logJointUnconstrained が旧書き方と一致 (移行の同値性)。
      let ncats  = [3, 2] :: [Int]
          deltas = [1.2, 0.7] :: [Double]
          gammas = [[-0.5, 0.8], [0.1]] :: [[Double]]
          grades = [1, 2, 3, -1, 2, 1] :: [Double]
          logCatProb th nc dl gm gr =
            let kMax = nc - 1
                qs = [ 1 / (1 + exp (negate (realToFrac dl * (th - realToFrac (gm !! (kk - 1))))))
                     | kk <- [1 .. kMax :: Int] ]
                ps = [ if k == 1 then 1 - head qs
                       else if k == nc then qs !! (kMax - 1)
                       else (qs !! (k - 2)) - (qs !! (k - 1))
                     | k <- [1 .. nc] ]
            in log (ps !! (gr - 1))
          mNew :: HBM.ModelP ()
          mNew = do
            ths <- mapM (\i -> HBM.sample (T.pack ("theta_" ++ show (i :: Int)))
                                  (HBM.Normal 0 6)) [0 .. 2]
            HBM.observeMV "grades" (HBM.GradedResponseIrt ths ncats deltas gammas) [grades]
          mOld :: HBM.ModelP ()
          mOld = do
            ths <- mapM (\i -> HBM.sample (T.pack ("theta_" ++ show (i :: Int)))
                                  (HBM.Normal 0 6)) [0 .. 2]
            let rows = [ take 2 grades, take 2 (drop 2 grades), drop 4 grades ]
                terms = [ logCatProb th nc dl gm (round gr)
                        | (th, row) <- zip ths rows
                        , (nc, dl, gm, gr) <- zip4 ncats deltas gammas row
                        , gr /= -1 ]
            HBM.potential "bones_loglik" (sum terms)
          names = HBM.sampleNames mNew
          tmap  = HBM.getTransforms mNew
          trans = [ tmap M.! n | n <- names ]
          us    = M.fromList (zip names ([0.4, -0.8, 1.1] :: [Double]))
      abs (HBM.logJointUnconstrained mNew names trans us
             - HBM.logJointUnconstrained mOld names trans us)
        `shouldSatisfy` (< 1e-10)

    it "irt-2pl 型 (LogNormal-latent-scale 解析勾配・Phase 98 A3): gradADU ≈ 中心差分" $ do
      -- Bernoulli-logit 積尤度 (a_i·theta_j) で vecIR 経路に載り、a_i~LogNormal(0,σ_a)
      -- (σ_a latent) が 'gradLogNormalIx' で解析勾配に載る (reverse-AD tape 全廃)。
      -- theta は Normal 族で arena 吸収・σ 群は constPrior。真の密度の中心差分と一致確認。
      let ys = [1,0,1, 0,1,0] :: [Double]   -- 2 item × 3 person の (i,j) 行優先
          m :: HBM.ModelP ()
          m = do
            sigTheta <- HBM.sample "sigma_theta" (HBM.HalfCauchy 2)
            th <- mapM (\j -> HBM.sample (T.pack ("theta_" ++ show (j :: Int)))
                                (HBM.Normal 0 sigTheta)) [0 .. 2]
            sigA <- HBM.sample "sigma_a" (HBM.HalfCauchy 2)
            as <- mapM (\i -> HBM.sample (T.pack ("a_" ++ show (i :: Int)))
                                (HBM.LogNormal 0 sigA)) [0 .. 1]
            mapM_ (\((i, j), y) ->
                     let logit = (as !! i) * (th !! j)
                     in HBM.observe (T.pack ("y_" ++ show i ++ "_" ++ show j))
                          (HBM.Bernoulli (1 / (1 + exp (negate logit)))) [y])
                  (zip [ (i, j) | i <- [0 .. 1], j <- [0 .. 2] ] ys)
          names = HBM.sampleNames m
          tmap  = HBM.getTransforms m
          trans = [ tmap M.! n | n <- names ]
          us    = [0.2, 0.3, -0.1, 0.25, 0.15, 0.4, -0.2]  -- 7 latent (σθ,θ0-2,σa,a0-1)
      closeVec 1e-4 (HBM.gradADU m names trans us) (centralGrad m names trans us)
        `shouldBe` True

  -- Phase 54.1: 構造化線形予測子 observe (ObserveLM)。 設計行列 X と β 名を
  -- 分離保持する観測ブロックが、 per-obs observe を N 回呼ぶのと数値等価かを担保。
  -- (54.2 で Gaussian-恒等リンクの suff-stat collapse に乗せる前提テスト)

  describe "Phase 54.1: ObserveLM が per-obs observe と数値等価" $ do
    let centralGrad m names trans us =
          let h = 1e-5
              f vs = HBM.logJointUnconstrained m names trans
                       (M.fromList (zip names vs))
              bump i d = [ if j == i then u + d else u
                         | (j, u) <- zip [(0 :: Int) ..] us ]
          in [ (f (bump i h) - f (bump i (-h))) / (2 * h)
             | i <- [0 .. length us - 1] ]
        closeVec tol a b =
          length a == length b &&
          and [ abs (x - y) <= tol * (1 + abs y) | (x, y) <- zip a b ]
        xs = [-1.0, -0.4, 0.2, 0.8, 1.5] :: [Double]
        designX = [ [1.0, x] | x <- xs ]   -- intercept + slope

    it "Gaussian-identity: logJoint/gradADU が per-obs observe と一致 + 中心差分" $ do
      let ys = [0.3, 1.1, 2.4, 3.0, 4.2] :: [Double]
          mObs, mLM :: HBM.ModelP ()
          mObs = do
            a <- HBM.sample "a"     (HBM.Normal 0 10)
            b <- HBM.sample "b"     (HBM.Normal 0 10)
            s <- HBM.sample "sigma" (HBM.Exponential 1)
            mapM_ (\(i, (x, y)) ->
                     HBM.observe (T.pack ("y_" ++ show (i :: Int)))
                       (HBM.Normal (a + b * realToFrac x) s) [y])
                  (zip [0 ..] (zip xs ys))
          mLM = do
            _ <- HBM.sample "a"     (HBM.Normal 0 10)
            _ <- HBM.sample "b"     (HBM.Normal 0 10)
            _ <- HBM.sample "sigma" (HBM.Exponential 1)
            HBM.observeLM "y" ["a", "b"] designX (HBM.LMGaussian "sigma") ys
          names = HBM.sampleNames mLM
          tmap  = HBM.getTransforms mLM
          trans = [ tmap M.! n | n <- names ]
          us    = [0.3, -0.2, 0.1]
          -- Phase 60.7: TrackTag (非標準クラス) が defaulting を止めるので明示
          ps    = M.fromList [("a", 0.4), ("b", 1.5), ("sigma", 0.6)]
                    :: M.Map T.Text Double
      -- sampleNames は ObserveLM が latent を増やさず一致
      names `shouldBe` ["a", "b", "sigma"]
      -- logJoint が per-obs と一致
      closeVec 1e-9 [HBM.logJoint mLM ps] [HBM.logJoint mObs ps] `shouldBe` True
      -- gradADU が per-obs と一致
      closeVec 1e-7 (HBM.gradADU mLM names trans us)
                    (HBM.gradADU mObs names trans us) `shouldBe` True
      -- gradADU が中心差分 ground truth と一致
      closeVec 1e-4 (HBM.gradADU mLM names trans us)
                    (centralGrad mLM names trans us) `shouldBe` True

    it "Poisson (log link): ObserveLM が per-obs observe と logJoint 一致" $ do
      let ys = [1.0, 0.0, 3.0, 2.0, 5.0] :: [Double]
          mObs, mLM :: HBM.ModelP ()
          mObs = do
            a <- HBM.sample "a" (HBM.Normal 0 10)
            b <- HBM.sample "b" (HBM.Normal 0 10)
            mapM_ (\(i, (x, y)) ->
                     HBM.observe (T.pack ("y_" ++ show (i :: Int)))
                       (HBM.Poisson (exp (a + b * realToFrac x))) [y])
                  (zip [0 ..] (zip xs ys))
          mLM = do
            _ <- HBM.sample "a" (HBM.Normal 0 10)
            _ <- HBM.sample "b" (HBM.Normal 0 10)
            HBM.observeLM "y" ["a", "b"] designX HBM.LMPoisson ys
          ps = M.fromList [("a", 0.2), ("b", 0.5)] :: M.Map T.Text Double
      closeVec 1e-9 [HBM.logJoint mLM ps] [HBM.logJoint mObs ps] `shouldBe` True

    it "Bernoulli (logit link): ObserveLM が per-obs observe と logJoint 一致" $ do
      let ys = [1.0, 0.0, 1.0, 1.0, 0.0] :: [Double]
          logistic z = 1 / (1 + exp (negate z))
          mObs, mLM :: HBM.ModelP ()
          mObs = do
            a <- HBM.sample "a" (HBM.Normal 0 10)
            b <- HBM.sample "b" (HBM.Normal 0 10)
            mapM_ (\(i, (x, y)) ->
                     HBM.observe (T.pack ("y_" ++ show (i :: Int)))
                       (HBM.Bernoulli (logistic (a + b * realToFrac x))) [y])
                  (zip [0 ..] (zip xs ys))
          mLM = do
            _ <- HBM.sample "a" (HBM.Normal 0 10)
            _ <- HBM.sample "b" (HBM.Normal 0 10)
            HBM.observeLM "y" ["a", "b"] designX HBM.LMBernoulli ys
          ps = M.fromList [("a", -0.3), ("b", 0.8)] :: M.Map T.Text Double
      closeVec 1e-9 [HBM.logJoint mLM ps] [HBM.logJoint mObs ps] `shouldBe` True

    it "extractDeps: ObserveLM は観測 1 ノード・親 = β + σ" $ do
      let ys = [0.3, 1.1, 2.4, 3.0, 4.2] :: [Double]
          mLM :: HBM.ModelP ()
          mLM = do
            _ <- HBM.sample "a"     (HBM.Normal 0 10)
            _ <- HBM.sample "b"     (HBM.Normal 0 10)
            _ <- HBM.sample "sigma" (HBM.Exponential 1)
            HBM.observeLM "y" ["a", "b"] designX (HBM.LMGaussian "sigma") ys
          (nodes, _) = HBM.extractDeps mLM
          yNode = head [ n | n <- nodes, HBM.nodeName n == "y" ]
      HBM.nodeKind yNode `shouldBe` HBM.ObservedN (length ys)
      HBM.nodeDeps yNode `shouldBe` Set.fromList ["a", "b", "sigma"]

    it "observeLMR (REff gather): random intercept が per-obs observe と一致 + 中心差分" $ do
      -- random intercept: η_i = a + b·x_i + u_{g(i)}、 g = [0,0,1,1,0]
      let ys   = [0.3, 1.1, 2.4, 3.0, 4.2] :: [Double]
          gids = [0, 0, 1, 1, 0] :: [Int]
          mObs, mLMR :: HBM.ModelP ()
          mObs = do
            a  <- HBM.sample "a"     (HBM.Normal 0 10)
            b  <- HBM.sample "b"     (HBM.Normal 0 10)
            tu <- HBM.sample "tau_u" (HBM.HalfNormal 5)
            u0 <- HBM.sample "u_0"   (HBM.Normal 0 tu)
            u1 <- HBM.sample "u_1"   (HBM.Normal 0 tu)
            s  <- HBM.sample "sigma" (HBM.Exponential 1)
            let us = [u0, u1]
            mapM_ (\(i, (x, g, y)) ->
                     HBM.observe (T.pack ("y_" ++ show (i :: Int)))
                       (HBM.Normal (a + b * realToFrac x + us !! g) s) [y])
                  (zip [0 ..] (zip3 xs gids ys))
          mLMR = do
            _ <- HBM.sample "a"     (HBM.Normal 0 10)
            _ <- HBM.sample "b"     (HBM.Normal 0 10)
            tu <- HBM.sample "tau_u" (HBM.HalfNormal 5)
            _ <- HBM.sample "u_0"   (HBM.Normal 0 tu)
            _ <- HBM.sample "u_1"   (HBM.Normal 0 tu)
            _ <- HBM.sample "sigma" (HBM.Exponential 1)
            HBM.observeLMR "y" ["a", "b"] designX [HBM.REff ["u_0", "u_1"] gids Nothing Nothing Nothing]
              (HBM.LMGaussian "sigma") ys
          names = HBM.sampleNames mLMR
          tmap  = HBM.getTransforms mLMR
          trans = [ tmap M.! n | n <- names ]
          us    = [0.3, -0.2, 0.1, 0.4, -0.3, 0.0]
          ps    = M.fromList
            [ ("a", 0.4), ("b", 1.5), ("tau_u", 0.7)
            , ("u_0", 0.2), ("u_1", -0.3), ("sigma", 0.6) ]
                    :: M.Map T.Text Double
      names `shouldBe` ["a", "b", "tau_u", "u_0", "u_1", "sigma"]
      -- logJoint が per-obs と一致
      closeVec 1e-9 [HBM.logJoint mLMR ps] [HBM.logJoint mObs ps] `shouldBe` True
      -- gradADU (ハイブリッド vec-tape) が per-obs ad と一致
      closeVec 1e-7 (HBM.gradADU mLMR names trans us)
                    (HBM.gradADU mObs names trans us) `shouldBe` True
      -- gradADU が中心差分 ground truth と一致
      closeVec 1e-4 (HBM.gradADU mLMR names trans us)
                    (centralGrad mLMR names trans us) `shouldBe` True
      -- extractDeps: 親 = β + u + σ
      let (nodes, _) = HBM.extractDeps mLMR
          yNode = head [ n | n <- nodes, HBM.nodeName n == "y" ]
      HBM.nodeDeps yNode `shouldBe`
        Set.fromList ["a", "b", "u_0", "u_1", "sigma"]

    it "reNormal/at (Phase 54.4c): 解析 prior 勾配が ad 経路・中心差分と一致" $ do
      -- 第一級ランダム効果 (reNormal/at) で組んだモデル mNew は u-prior 勾配を
      -- 解析計算 + u_j Sample を ad から除外する。 文字列 REff (Nothing) で組んだ
      -- mAd は prior を従来 ad で計算する。 両者は数値的に等価でなければならない。
      let ys   = [0.3, 1.1, 2.4, 3.0, 4.2] :: [Double]
          gids = [0, 0, 1, 1, 0] :: [Int]
          mAd, mNew :: HBM.ModelP ()
          mAd = do
            _  <- HBM.sample "a"     (HBM.Normal 0 10)
            _  <- HBM.sample "b"     (HBM.Normal 0 10)
            tu <- HBM.sample "tau_u" (HBM.HalfNormal 5)
            _  <- HBM.sample "u_0"   (HBM.Normal 0 tu)
            _  <- HBM.sample "u_1"   (HBM.Normal 0 tu)
            _  <- HBM.sample "sigma" (HBM.Exponential 1)
            HBM.observeLMR "y" ["a", "b"] designX [HBM.REff ["u_0", "u_1"] gids Nothing Nothing Nothing]
              (HBM.LMGaussian "sigma") ys
          mNew = do
            _   <- HBM.sample "a"     (HBM.Normal 0 10)
            _   <- HBM.sample "b"     (HBM.Normal 0 10)
            tau <- HBM.sample "tau_u" (HBM.HalfNormal 5)
            u   <- HBM.reNormal "u" 2 "tau_u" tau
            _   <- HBM.sample "sigma" (HBM.Exponential 1)
            HBM.observeNormalLM "y" designX ["a", "b"] [u `HBM.at` gids] "sigma" ys
          names = HBM.sampleNames mNew
          tmap  = HBM.getTransforms mNew
          trans = [ tmap M.! n | n <- names ]
          us    = [0.3, -0.2, 0.1, 0.4, -0.3, 0.0]
      -- mAd の prior には tau に依存する u_j prior が ad で入る。 mNew では u_j prior
      -- を解析計算するが、 値は同一なので両勾配は一致するはず。
      names `shouldBe` ["a", "b", "tau_u", "u_0", "u_1", "sigma"]
      closeVec 1e-7 (HBM.gradADU mNew names trans us)
                    (HBM.gradADU mAd names trans us) `shouldBe` True
      closeVec 1e-4 (HBM.gradADU mNew names trans us)
                    (centralGrad mNew names trans us) `shouldBe` True

    it "compileLogPU (Phase 54.4d): 値評価が logJointUnconstrained と一致 (3 経路)" $ do
      -- (1) 解析経路 (reNormal/at = Just scale) / (2) Nothing REff (LM vec のみ) /
      -- (3) Gaussian LM 無し (scalar fallback)。 いずれも従来 walk 評価と一致すること。
      let ys   = [0.3, 1.1, 2.4, 3.0, 4.2] :: [Double]
          gids = [0, 0, 1, 1, 0] :: [Int]
          mAna, mPlain, mScalar :: HBM.ModelP ()
          mAna = do
            _   <- HBM.sample "a"     (HBM.Normal 0 10)
            _   <- HBM.sample "b"     (HBM.Normal 0 10)
            tau <- HBM.sample "tau_u" (HBM.HalfNormal 5)
            u   <- HBM.reNormal "u" 2 "tau_u" tau
            _   <- HBM.sample "sigma" (HBM.Exponential 1)
            HBM.observeNormalLM "y" designX ["a", "b"] [u `HBM.at` gids] "sigma" ys
          mPlain = do
            _  <- HBM.sample "a"     (HBM.Normal 0 10)
            _  <- HBM.sample "b"     (HBM.Normal 0 10)
            tu <- HBM.sample "tau_u" (HBM.HalfNormal 5)
            _  <- HBM.sample "u_0"   (HBM.Normal 0 tu)
            _  <- HBM.sample "u_1"   (HBM.Normal 0 tu)
            _  <- HBM.sample "sigma" (HBM.Exponential 1)
            HBM.observeLMR "y" ["a", "b"] designX [HBM.REff ["u_0", "u_1"] gids Nothing Nothing Nothing]
              (HBM.LMGaussian "sigma") ys
          mScalar = do
            mu <- HBM.sample "mu" (HBM.Normal 0 10)
            s  <- HBM.sample "s"  (HBM.Exponential 1)
            HBM.observe "y" (HBM.Normal mu s) ys
          checkEq :: HBM.ModelP () -> [Double] -> Expectation
          checkEq mdl uvals = do
            let nms  = HBM.sampleNames mdl
                tmap = HBM.getTransforms mdl
                trs  = [ tmap M.! n | n <- nms ]
                pU   = M.fromList (zip nms uvals)
            HBM.compileLogPU mdl nms trs uvals
              `shouldSatisfy` (\v -> abs (v - HBM.logJointUnconstrained mdl nms trs pU) < 1e-9)
      checkEq mAna    [0.3, -0.2, 0.1, 0.4, -0.3, 0.0]
      checkEq mPlain  [0.3, -0.2, 0.1, 0.4, -0.3, 0.0]
      checkEq mScalar [0.7, -0.1]

    it "54.4e fallback: LM ブロック + scalar observe 混在 (residual 非空) で ad/中心差分一致" $ do
      -- β/σ は定数パラメタ prior (解析勾配) だが scalar observe "z" が residual に
      -- 残るので ad 経路も併用される。 値・勾配とも従来 walk / 中心差分と一致すること。
      let ys = [0.3, 1.1, 2.4, 3.0, 4.2] :: [Double]
          mMix :: HBM.ModelP ()
          mMix = do
            a <- HBM.sample "a"     (HBM.Normal 0 10)
            _ <- HBM.sample "b"     (HBM.Normal 0 10)
            _ <- HBM.sample "sigma" (HBM.Exponential 1)
            HBM.observe "z" (HBM.Normal a 2) [0.5, 0.9]   -- residual に残る scalar observe
            HBM.observeLM "y" ["a", "b"] designX (HBM.LMGaussian "sigma") ys
          names = HBM.sampleNames mMix
          tmap  = HBM.getTransforms mMix
          trans = [ tmap M.! n | n <- names ]
          us    = [0.3, -0.2, 0.1]
          pU    = M.fromList (zip names us)
      names `shouldBe` ["a", "b", "sigma"]
      abs (HBM.compileLogPU mMix names trans us
           - HBM.logJointUnconstrained mMix names trans pU) < 1e-9 `shouldBe` True
      closeVec 1e-4 (HBM.gradADU mMix names trans us)
                    (centralGrad mMix names trans us) `shouldBe` True

    it "synthGaussLMBlocks (Phase 54.8): M1 per-obs 手書きが自動合成され値/勾配一致" $ do
      -- per-obs scalar observe 手書きの pooled 回帰 (bench M1 形)。 affine 追跡で
      -- ObserveLM ブロックに自動合成され、 全 Observe が吸収されること +
      -- 値/勾配が従来 walk・中心差分と一致すること。
      let ys = [0.3, 1.1, 2.4, 3.0, 4.2] :: [Double]
          mM1 :: HBM.ModelP ()
          mM1 = do
            a <- HBM.sample "a"     (HBM.Normal 0 10)
            b <- HBM.sample "b"     (HBM.Normal 0 10)
            s <- HBM.sample "sigma" (HBM.Exponential 1)
            mapM_ (\(i, (x, y)) ->
                     HBM.observe (T.pack ("y_" ++ show (i :: Int)))
                       (HBM.Normal (a + b * realToFrac x) s) [y])
                  (zip [0 ..] (zip xs ys))
          (blocks, absorbed) = HBM.synthGaussLMBlocks mM1
          names = HBM.sampleNames mM1
          tmap  = HBM.getTransforms mM1
          trans = [ tmap M.! n | n <- names ]
          us    = [0.3, -0.2, 0.1]
          pU    = M.fromList (zip names us)
      [ (bs, xs', re, sn, ys') | (_, bs, xs', re, sn, ys') <- blocks ]
        `shouldBe` [ (["a", "b"], designX, [], "sigma", ys) ]
      Set.toList absorbed
        `shouldBe` [ T.pack ("y_" ++ show i) | i <- [0 .. 4 :: Int] ]
      abs (HBM.compileLogPU mM1 names trans us
           - HBM.logJointUnconstrained mM1 names trans pU) < 1e-9 `shouldBe` True
      closeVec 1e-4 (HBM.gradADU mM1 names trans us)
                    (centralGrad mM1 names trans us) `shouldBe` True

    it "synthGaussLMBlocks (Phase 54.8): 階層 per-obs 手書きで one-hot 族が REff gather 化" $ do
      -- m2Scalar 形: 係数常 1 の u_0/u_1 (prior = Normal(0, tau_u)) が dense 列で
      -- なく REff gather (Just tau_u = 解析 prior 経路) に昇格すること。
      let gids = [0, 0, 1, 1, 0] :: [Int]
          ys   = [0.3, 1.1, 2.4, 3.0, 4.2] :: [Double]
          mH :: HBM.ModelP ()
          mH = do
            a   <- HBM.sample "a"     (HBM.Normal 0 10)
            b   <- HBM.sample "b"     (HBM.Normal 0 10)
            tau <- HBM.sample "tau_u" (HBM.HalfNormal 5)
            uvs <- mapM (\j -> HBM.sample (T.pack ("u_" ++ show (j :: Int)))
                                          (HBM.Normal 0 tau)) [0, 1]
            s   <- HBM.sample "sigma" (HBM.Exponential 1)
            mapM_ (\(i, ((x, g), y)) ->
                     HBM.observe (T.pack ("y_" ++ show (i :: Int)))
                       (HBM.Normal (a + b * realToFrac x + uvs !! g) s) [y])
                  (zip [0 ..] (zip (zip xs gids) ys))
          (blocks, _) = HBM.synthGaussLMBlocks mH
          names = HBM.sampleNames mH
          tmap  = HBM.getTransforms mH
          trans = [ tmap M.! n | n <- names ]
          uv    = [0.3, -0.2, 0.1, 0.4, -0.3, 0.0]
          pU    = M.fromList (zip names uv)
      [ (bs, re) | (_, bs, _, re, _, _) <- blocks ]
        `shouldBe` [ (["a", "b"], [HBM.REff ["u_0", "u_1"] gids (Just "tau_u") Nothing Nothing]) ]
      abs (HBM.compileLogPU mH names trans uv
           - HBM.logJointUnconstrained mH names trans pU) < 1e-9 `shouldBe` True
      closeVec 1e-4 (HBM.gradADU mH names trans uv)
                    (centralGrad mH names trans uv) `shouldBe` True

    it "synthGaussLMBlocks (Phase 54.8): 値依存分岐は合成せず fallback (値/勾配は従来経路)" $ do
      -- μ に値依存分岐 (if a > 0) を含むモデル。 AffV の Ord poison →
      -- try/force 捕捉で合成全体が fallback し、 値/勾配は従来 ad 経路で正しいこと。
      let mBr :: HBM.ModelP ()
          mBr = do
            a <- HBM.sample "a" (HBM.Normal 0 1)
            s <- HBM.sample "s" (HBM.Exponential 1)
            let mu = if a > 0 then a else negate a
            HBM.observe "y" (HBM.Normal mu s) [0.5, 1.0]
          (blocks, absorbed) = HBM.synthGaussLMBlocks mBr
          names = HBM.sampleNames mBr
          tmap  = HBM.getTransforms mBr
          trans = [ tmap M.! n | n <- names ]
          uv    = [0.7, -0.1]
          pU    = M.fromList (zip names uv)
      null blocks `shouldBe` True
      Set.null absorbed `shouldBe` True
      abs (HBM.compileLogPU mBr names trans uv
           - HBM.logJointUnconstrained mBr names trans pU) < 1e-9 `shouldBe` True
      closeVec 1e-4 (HBM.gradADU mBr names trans uv)
                    (centralGrad mBr names trans uv) `shouldBe` True

    it "synthGaussLMBlocks (Phase 54.10): random slope (係数付き) も u/v 二重族で REff 化" $ do
      -- M3 形: u_g (係数 1) と v_g (係数 x_i) が、 ともに REff gather に昇格する
      -- こと (v 族は per-row 重み = x・u 族は重み Nothing)。 β (b0/b1) は dense
      -- 列のまま。 値/勾配は従来 walk・中心差分と一致。
      let gids = [0, 0, 1, 1, 0] :: [Int]
          ys   = [0.3, 1.1, 2.4, 3.0, 4.2] :: [Double]
          mRS :: HBM.ModelP ()
          mRS = do
            b0  <- HBM.sample "b0"    (HBM.Normal 0 5)
            b1  <- HBM.sample "b1"    (HBM.Normal 0 5)
            tu  <- HBM.sample "tau_u" (HBM.HalfNormal 5)
            tv  <- HBM.sample "tau_v" (HBM.HalfNormal 5)
            uvs <- mapM (\j -> HBM.sample (T.pack ("u_" ++ show (j :: Int)))
                                          (HBM.Normal 0 tu)) [0, 1]
            vvs <- mapM (\j -> HBM.sample (T.pack ("v_" ++ show (j :: Int)))
                                          (HBM.Normal 0 tv)) [0, 1]
            s   <- HBM.sample "sigma" (HBM.Exponential 1)
            mapM_ (\(i, ((x, g), y)) ->
                     HBM.observe (T.pack ("y_" ++ show (i :: Int)))
                       (HBM.Normal (b0 + b1 * realToFrac x + uvs !! g
                                    + (vvs !! g) * realToFrac x) s) [y])
                  (zip [0 ..] (zip (zip xs gids) ys))
          (blocks, absorbed) = HBM.synthGaussLMBlocks mRS
          names = HBM.sampleNames mRS
          tmap  = HBM.getTransforms mRS
          trans = [ tmap M.! n | n <- names ]
          uv    = [0.3, -0.2, 0.1, 0.4, -0.3, 0.2, 0.15, -0.25, 0.0]
          pU    = M.fromList (zip names uv)
      [ (bs, re) | (_, bs, _, re, _, _) <- blocks ]
        `shouldBe` [ (["b0", "b1"],
                      [ HBM.REff ["u_0", "u_1"] gids (Just "tau_u") Nothing Nothing
                      , HBM.REff ["v_0", "v_1"] gids (Just "tau_v") (Just xs) Nothing ]) ]
      Set.size absorbed `shouldBe` 5
      abs (HBM.compileLogPU mRS names trans uv
           - HBM.logJointUnconstrained mRS names trans pU) < 1e-9 `shouldBe` True
      closeVec 1e-4 (HBM.gradADU mRS names trans uv)
                    (centralGrad mRS names trans uv) `shouldBe` True

    it "observeLMR (Phase 54.10): 明示の重み付き REff が per-obs 手書きと値一致" $ do
      -- 重み付き gather (η_i += w_i·v_{g_i}) を Nothing スケール (汎用 walk 経路)
      -- で明示構築し、 同じモデルの per-obs 手書きと logJointUnconstrained が
      -- 一致すること (lmReffEta の重み対応の直接検証)。
      let gids = [0, 0, 1, 1, 0] :: [Int]
          ys   = [0.3, 1.1, 2.4, 3.0, 4.2] :: [Double]
          mW, mHand :: HBM.ModelP ()
          mW = do
            _  <- HBM.sample "a"     (HBM.Normal 0 10)
            _  <- HBM.sample "b"     (HBM.Normal 0 10)
            tv <- HBM.sample "tau_v" (HBM.HalfNormal 5)
            _  <- HBM.sample "v_0"   (HBM.Normal 0 tv)
            _  <- HBM.sample "v_1"   (HBM.Normal 0 tv)
            _  <- HBM.sample "sigma" (HBM.Exponential 1)
            HBM.observeLMR "y" ["a", "b"] designX
              [HBM.REff ["v_0", "v_1"] gids Nothing (Just xs) Nothing]
              (HBM.LMGaussian "sigma") ys
          mHand = do
            a  <- HBM.sample "a"     (HBM.Normal 0 10)
            b  <- HBM.sample "b"     (HBM.Normal 0 10)
            tv <- HBM.sample "tau_v" (HBM.HalfNormal 5)
            vvs <- mapM (\j -> HBM.sample (T.pack ("v_" ++ show (j :: Int)))
                                          (HBM.Normal 0 tv)) [0, 1]
            s  <- HBM.sample "sigma" (HBM.Exponential 1)
            mapM_ (\(i, ((x, g), y)) ->
                     HBM.observe (T.pack ("y_" ++ show (i :: Int)))
                       (HBM.Normal (a + b * realToFrac x
                                    + (vvs !! g) * realToFrac x) s) [y])
                  (zip [0 ..] (zip (zip xs gids) ys))
          names = HBM.sampleNames mW
          tmap  = HBM.getTransforms mW
          trans = [ tmap M.! n | n <- names ]
          uv    = [0.3, -0.2, 0.1, 0.4, -0.3, 0.0]
          pU    = M.fromList (zip names uv)
      names `shouldBe` HBM.sampleNames mHand
      abs (HBM.logJointUnconstrained mW names trans pU
           - HBM.logJointUnconstrained mHand names trans pU) < 1e-9 `shouldBe` True
      abs (HBM.compileLogPU mW names trans uv
           - HBM.logJointUnconstrained mW names trans pU) < 1e-9 `shouldBe` True
      closeVec 1e-4 (HBM.gradADU mW names trans uv)
                    (centralGrad mW names trans uv) `shouldBe` True

    -- Phase 54.11 共通: 従来 ad の unconstrained 勾配 (fFull 相当) を参照値に。
    let adGradRef :: HBM.ModelP () -> [T.Text] -> [Transform] -> [Double] -> [Double]
        adGradRef mm nms trs uvs =
          RevD.grad
            (\uv' -> HBM.logJoint mm
                       (M.fromList (zip nms (zipWith HBM.invTransformF trs uv')))
                     + sum (zipWith HBM.logJacF trs uv'))
            uvs

    it "synthVecIR (Phase 54.11): M5 形 (非線形 μ) がベクトル式 IR 化され値/勾配一致" $ do
      -- μ_i = a·exp(-b·x_i) + c (bench M5 形)。 affine 合成 (54.8) は不成立、
      -- ベクトル式 IR が全 Observe を吸収し、 値/勾配が従来 ad・中心差分と一致。
      let ys = [2.1, 1.7, 1.4, 1.2, 1.0] :: [Double]
          m5 :: HBM.ModelP ()
          m5 = do
            a <- HBM.sample "a"     (HBM.Normal 0 10)
            b <- HBM.sample "b"     (HBM.HalfNormal 2)
            c <- HBM.sample "c"     (HBM.Normal 0 10)
            s <- HBM.sample "sigma" (HBM.Exponential 1)
            mapM_ (\(i, (x, y)) ->
                     HBM.observe (T.pack ("y_" ++ show (i :: Int)))
                       (HBM.Normal (a * exp (negate b * realToFrac x) + c) s) [y])
                  (zip [0 ..] (zip xs ys))
          names = HBM.sampleNames m5
          tmap  = HBM.getTransforms m5
          trans = [ tmap M.! n | n <- names ]
          uvs   = [0.8, log 0.9, 0.3, log 0.4]
          pU    = M.fromList (zip names uvs)
      null (fst (HBM.synthGaussLMBlocks m5)) `shouldBe` True
      case HBM.synthVecIR m5 of
        Nothing -> expectationFailure "synthVecIR: expected Just"
        Just (gs, fams, sObs) -> do
          length gs `shouldBe` 1
          length fams `shouldBe` 0
          Set.toList sObs `shouldBe`
            [ T.pack ("y_" ++ show i) | i <- [0 .. 4 :: Int] ]
      abs (HBM.compileLogPU m5 names trans uvs
           - HBM.logJointUnconstrained m5 names trans pU) < 1e-9 `shouldBe` True
      closeVec 1e-9 (HBM.gradADU m5 names trans uvs)
                    (adGradRef m5 names trans uvs) `shouldBe` True
      closeVec 1e-4 (HBM.gradADU m5 names trans uvs)
                    (centralGrad m5 names trans uvs) `shouldBe` True

    it "synthVecIR (Phase 54.11): M6 形 (階層×非線形) で族 prior も IR に乗り値/勾配一致" $ do
      -- μ_i = a_{g(i)}·exp(-b·x_i)、 a_g ~ Normal(μ_a, τ_a) (bench M6 形)。
      -- a_g 族が gather + ベクトル化 prior として IR に乗ること。
      let gids = [0, 0, 1, 1, 0] :: [Int]
          ys   = [2.1, 1.7, 1.4, 1.2, 1.0] :: [Double]
          m6 :: HBM.ModelP ()
          m6 = do
            muA  <- HBM.sample "mu_a"  (HBM.Normal 0 10)
            tauA <- HBM.sample "tau_a" (HBM.HalfNormal 2)
            as   <- mapM (\j -> HBM.sample (T.pack ("a_" ++ show (j :: Int)))
                                           (HBM.Normal muA tauA)) [0, 1]
            b    <- HBM.sample "b"     (HBM.HalfNormal 2)
            s    <- HBM.sample "sigma" (HBM.Exponential 1)
            mapM_ (\(i, ((x, g), y)) ->
                     HBM.observe (T.pack ("y_" ++ show (i :: Int)))
                       (HBM.Normal ((as !! g) * exp (negate b * realToFrac x)) s) [y])
                  (zip [0 ..] (zip (zip xs gids) ys))
          names = HBM.sampleNames m6
          tmap  = HBM.getTransforms m6
          trans = [ tmap M.! n | n <- names ]
          uvs   = [1.5, log 0.6, 1.8, 1.6, log 0.9, log 0.4]
          pU    = M.fromList (zip names uvs)
      case HBM.synthVecIR m6 of
        Nothing -> expectationFailure "synthVecIR: expected Just"
        Just (gs, fams, _) -> do
          length gs `shouldBe` 1
          [ ms | (ms, _, _) <- fams ] `shouldBe` [["a_0", "a_1"]]
      abs (HBM.compileLogPU m6 names trans uvs
           - HBM.logJointUnconstrained m6 names trans pU) < 1e-9 `shouldBe` True
      closeVec 1e-9 (HBM.gradADU m6 names trans uvs)
                    (adGradRef m6 names trans uvs) `shouldBe` True
      closeVec 1e-4 (HBM.gradADU m6 names trans uvs)
                    (centralGrad m6 names trans uvs) `shouldBe` True

    it "synthVecIR (Phase 54.11): 値依存分岐 (非線形) は合成せず fallback" $ do
      -- μ に値依存分岐を含む非線形モデル。 SExp の Ord poison → try/force 捕捉で
      -- IR 合成全体が fallback し、 値/勾配は従来 ad 経路で正しいこと。
      let mBr :: HBM.ModelP ()
          mBr = do
            a <- HBM.sample "a" (HBM.Normal 0 1)
            s <- HBM.sample "s" (HBM.Exponential 1)
            let mu = if a > 0 then exp a else negate a
            HBM.observe "y" (HBM.Normal mu s) [0.5, 1.0]
          names = HBM.sampleNames mBr
          tmap  = HBM.getTransforms mBr
          trans = [ tmap M.! n | n <- names ]
          uvs   = [0.7, -0.1]
          pU    = M.fromList (zip names uvs)
      (case HBM.synthVecIR mBr of Nothing -> True; Just _ -> False)
        `shouldBe` True
      abs (HBM.compileLogPU mBr names trans uvs
           - HBM.logJointUnconstrained mBr names trans pU) < 1e-9 `shouldBe` True
      closeVec 1e-4 (HBM.gradADU mBr names trans uvs)
                    (centralGrad mBr names trans uvs) `shouldBe` True

    it "synthVecIR (Phase 54.11→55.2): 式形混在 σ グループも指紋サブグループ化で全吸収" $ do
      -- σ グループ s1 は同型非線形・s2 は行ごとに式の形が違う。 Phase 55.2 の
      -- (σ名, μ式形指紋) サブグループ化により z_0/z_1 も**それぞれ独立の
      -- グループとして吸収**される (54.11 時点では s2 丸ごと residual だった)。
      let mMix :: HBM.ModelP ()
          mMix = do
            a  <- HBM.sample "a"  (HBM.Normal 0 10)
            b  <- HBM.sample "b"  (HBM.HalfNormal 2)
            s1 <- HBM.sample "s1" (HBM.Exponential 1)
            s2 <- HBM.sample "s2" (HBM.Exponential 1)
            mapM_ (\(i, (x, y)) ->
                     HBM.observe (T.pack ("y1_" ++ show (i :: Int)))
                       (HBM.Normal (a * exp (negate b * realToFrac x)) s1) [y])
                  (zip [0 ..] (zip xs [2.1, 1.7, 1.4, 1.2, 1.0 :: Double]))
            HBM.observe "z_0" (HBM.Normal (exp a) s2) [1.3]
            HBM.observe "z_1" (HBM.Normal (a * a) s2) [0.9]
          names = HBM.sampleNames mMix
          tmap  = HBM.getTransforms mMix
          trans = [ tmap M.! n | n <- names ]
          uvs   = [0.5, log 0.8, log 0.6, log 0.7]
          pU    = M.fromList (zip names uvs)
      case HBM.synthVecIR mMix of
        Nothing -> expectationFailure "synthVecIR: expected Just"
        Just (gs, _, sObs) -> do
          length gs `shouldBe` 3
          Set.member "y1_0" sObs `shouldBe` True
          Set.member "z_0" sObs `shouldBe` True
          Set.member "z_1" sObs `shouldBe` True
      abs (HBM.compileLogPU mMix names trans uvs
           - HBM.logJointUnconstrained mMix names trans pU) < 1e-9 `shouldBe` True
      closeVec 1e-9 (HBM.gradADU mMix names trans uvs)
                    (adGradRef mMix names trans uvs) `shouldBe` True
      closeVec 1e-4 (HBM.gradADU mMix names trans uvs)
                    (centralGrad mMix names trans uvs) `shouldBe` True

    it "synthVecIR (Phase 55.2): 同一 σ 下 2 形 (複数行ずつ) を両方吸収 + 非対応分布は residual" $ do
      -- 同一 σ 下に exp a 群と a*a 群 (各 2 行) → 形指紋で 2 グループに割れて
      -- 両方吸収。 AsymmetricLaplace 観測は対象外で residual walk に残る
      -- (部分吸収の継続。 55.4 時点は StudentT だったが 56.3 で吸収対象化)。
      let mShapes :: HBM.ModelP ()
          mShapes = do
            a <- HBM.sample "a" (HBM.Normal 0 10)
            s <- HBM.sample "s" (HBM.Exponential 1)
            mapM_ (\(i, y) ->
                     HBM.observe (T.pack ("p_" ++ show (i :: Int)))
                       (HBM.Normal (exp a) s) [y])
                  (zip [0 ..] [1.3, 1.1 :: Double])
            mapM_ (\(i, y) ->
                     HBM.observe (T.pack ("q_" ++ show (i :: Int)))
                       (HBM.Normal (a * a) s) [y])
                  (zip [0 ..] [0.9, 0.7 :: Double])
            HBM.observe "r" (HBM.AsymmetricLaplace s 1 a) [0.2]
          names = HBM.sampleNames mShapes
          tmap  = HBM.getTransforms mShapes
          trans = [ tmap M.! n | n <- names ]
          uvs   = [0.6, log 0.5]
          pU    = M.fromList (zip names uvs)
      case HBM.synthVecIR mShapes of
        Nothing -> expectationFailure "synthVecIR: expected Just"
        Just (gs, fams, sObs) -> do
          length gs `shouldBe` 2
          length fams `shouldBe` 0
          Set.toList sObs `shouldBe` ["p_0", "p_1", "q_0", "q_1"]
      abs (HBM.compileLogPU mShapes names trans uvs
           - HBM.logJointUnconstrained mShapes names trans pU) < 1e-9 `shouldBe` True
      closeVec 1e-9 (HBM.gradADU mShapes names trans uvs)
                    (adGradRef mShapes names trans uvs) `shouldBe` True
      closeVec 1e-4 (HBM.gradADU mShapes names trans uvs)
                    (centralGrad mShapes names trans uvs) `shouldBe` True

    it "synthVecIR (Phase 80.2): 非中心化相関 RE μ(L×z) が (a) 経路に載り値/勾配一致" $ do
      -- 2 群・k=2 (切片 + 傾き 1)・群あたり 3 obs = 6 obs。 非中心化:
      --   b_g^0 = τ0·z0_g,  b_g^1 = τ1·(pc·z0_g + √(1-pc²)·z1_g),  pc = 2u-1
      --   μ_i   = β + b_{g(i)}^0 + b_{g(i)}^1·w_i,  y_i ~ Normal(μ_i, σ)
      -- μ に latent×latent 積 (τ·z, L·z) が入る非 affine 形。 Phase 80.2 で probe を
      -- ドメイン対応化する前は、 pcu (Beta ∈ (0,1)) が固定 probe 点 base=1.3 で
      -- 域外 (pcu=1.74 → √(1-pc²)=NaN) となり誤 fallback していた。 unify/gather/
      -- 族 prior は元から成立しており、 IR は数値的に忠実。
      let gids = [0,0,0,1,1,1] :: [Int]
          ws   = [-1.0, 0.0, 1.0, -0.5, 0.5, 1.5] :: [Double]
          ys   = [0.2, 0.5, 1.1, -0.3, 0.4, 1.2]  :: [Double]
          m :: HBM.ModelP ()
          m = do
            beta <- HBM.sample "beta"  (HBM.Normal 0 10)
            sig  <- HBM.sample "sigma" (HBM.HalfNormal 5)
            tau0 <- HBM.sample "tau0"  (HBM.HalfNormal 5)
            tau1 <- HBM.sample "tau1"  (HBM.HalfNormal 5)
            u    <- HBM.sample "pcu"   (HBM.Beta 2 2)
            let pc  = 2 * u - 1
                l11 = sqrt (1 - pc * pc)
            z0 <- mapM (\g -> HBM.sample (T.pack ("z0_" ++ show (g :: Int)))
                                         (HBM.Normal 0 1)) [0, 1]
            z1 <- mapM (\g -> HBM.sample (T.pack ("z1_" ++ show (g :: Int)))
                                         (HBM.Normal 0 1)) [0, 1]
            let b0 g = tau0 * (z0 !! g)
                b1 g = tau1 * (pc * (z0 !! g) + l11 * (z1 !! g))
                mu i = beta + b0 (gids !! i) + b1 (gids !! i) * realToFrac (ws !! i)
            mapM_ (\i -> HBM.observe (T.pack ("y_" ++ show i))
                           (HBM.Normal (mu i) sig) [ys !! i])
                  [0 .. 5 :: Int]
          names = HBM.sampleNames m
          tmap  = HBM.getTransforms m
          trans = [ tmap M.! n | n <- names ]
          -- unconstrained probe (変換で Beta→(0,1)・HalfNormal→(0,∞) に写り NaN 無し)。
          uvs   = [ 0.15 + 0.07 * fromIntegral i | i <- [0 .. length names - 1] ]
          pU    = M.fromList (zip names uvs)
      -- (b) affine 合成には載らない (latent×latent) が、 (a) vecIR には載る。
      null (fst (HBM.synthGaussLMBlocks m)) `shouldBe` True
      case HBM.synthVecIR m of
        Nothing -> expectationFailure "synthVecIR: expected Just (probe ドメイン対応後)"
        Just (gs, fams, sObs) -> do
          length gs `shouldBe` 1
          Set.fromList [ ms | (ms, _, _) <- fams ]
            `shouldBe` Set.fromList [["z0_0", "z0_1"], ["z1_0", "z1_1"]]
          Set.toList sObs `shouldBe` [ T.pack ("y_" ++ show i) | i <- [0 .. 5 :: Int] ]
      -- 値: compiled IR ≈ 従来 walk。
      abs (HBM.compileLogPU m names trans uvs
           - HBM.logJointUnconstrained m names trans pU) < 1e-9 `shouldBe` True
      -- 勾配: IR ≈ 従来 ad (1e-9) ≈ 中心差分 (finite-diff 1e-6)。
      closeVec 1e-9 (HBM.gradADU m names trans uvs)
                    (adGradRef m names trans uvs) `shouldBe` True
      closeVec 1e-6 (HBM.gradADU m names trans uvs)
                    (centralGrad m names trans uvs) `shouldBe` True

    it "synthVecIR (Phase 80.2b): 実 designHBMProgram (相関傾き) が非中心化 (a) に載る" $ do
      -- Phase 80.2b: Fit.hs:designHBMProgram の相関傾き branch を非中心化へ改修した後、
      -- 実コードが吐く ModelP が (b) affine には載らず (a) vecIR に載ることを担保。
      -- 2 群・k=2 (切片 + 傾き 1)・群あたり 3 obs = 6 obs。 spike (Phase 80.2) と
      -- 同型だが、 lkjCorrCholesky / deterministic b を経由する **実コード経路**を検証。
      let gids    = [0,0,0,1,1,1] :: [Int]
          ws      = [-1.0, 0.0, 1.0, -0.5, 0.5, 1.5] :: [Double]
          ys      = [0.2, 0.5, 1.1, -0.3, 0.4, 1.2]  :: [Double]
          designX = [ [1.0, w] | w <- ws ]           -- (Intercept) + temp
          betaNames = ["(Intercept)", "temp"]
          res     = [(gids, 2, [ws])]                -- 1 RE 群・傾き列 = temp
          m :: HBM.ModelP ()
          m       = designHBMProgram designX betaNames res ys
          names   = HBM.sampleNames m
          tmap    = HBM.getTransforms m
          trans   = [ tmap M.! n | n <- names ]
          -- unconstrained probe (変換で Beta→(0,1)/HalfNormal→(0,∞) に写り NaN 無し)。
          uvs     = [ 0.1 + 0.05 * fromIntegral i | i <- [0 .. length names - 1] ]
          pU      = M.fromList (zip names uvs)
      -- (b) affine 合成には載らない (latent×latent = τ·L·z)。
      null (fst (HBM.synthGaussLMBlocks m)) `shouldBe` True
      -- (a) vecIR に載る (6 観測)。
      case HBM.synthVecIR m of
        Nothing -> expectationFailure "synthVecIR: expected Just (非中心化相関 RE が (a) に載る)"
        Just (_gs, _fams, sObs) -> Set.size sObs `shouldBe` 6
      -- 値: compiled IR ≈ 従来 walk。
      abs (HBM.compileLogPU m names trans uvs
           - HBM.logJointUnconstrained m names trans pU) < 1e-9 `shouldBe` True
      -- 勾配: IR ≈ 従来 ad (1e-9) ≈ 中心差分 (finite-diff 1e-6)。
      closeVec 1e-9 (HBM.gradADU m names trans uvs)
                    (adGradRef m names trans uvs) `shouldBe` True
      closeVec 1e-6 (HBM.gradADU m names trans uvs)
                    (centralGrad m names trans uvs) `shouldBe` True

    it "synthVecIR (Phase 55.3): σ がスカラ式 (定数倍 2·s) でも吸収され値/勾配一致" $ do
      -- σ = 2*s は 54.11 の「σ = 単一 latent」 条件を満たさず residual 落ち
      -- していた形。 55.3 で σ 位置が任意 SExp に拡張され吸収される。
      let mScale :: HBM.ModelP ()
          mScale = do
            a <- HBM.sample "a" (HBM.Normal 0 10)
            b <- HBM.sample "b" (HBM.HalfNormal 2)
            s <- HBM.sample "s" (HBM.Exponential 1)
            mapM_ (\(i, (x, y)) ->
                     HBM.observe (T.pack ("y_" ++ show (i :: Int)))
                       (HBM.Normal (a * exp (negate b * realToFrac x)) (2 * s))
                       [y])
                  (zip [0 ..] (zip xs [2.1, 1.7, 1.4, 1.2, 1.0 :: Double]))
          names = HBM.sampleNames mScale
          tmap  = HBM.getTransforms mScale
          trans = [ tmap M.! n | n <- names ]
          uvs   = [0.5, log 0.7, log 0.6]
          pU    = M.fromList (zip names uvs)
      null (fst (HBM.synthGaussLMBlocks mScale)) `shouldBe` True
      case HBM.synthVecIR mScale of
        Nothing -> expectationFailure "synthVecIR: expected Just"
        Just (gs, _, sObs) -> do
          length gs `shouldBe` 1
          Set.size sObs `shouldBe` 5
      abs (HBM.compileLogPU mScale names trans uvs
           - HBM.logJointUnconstrained mScale names trans pU) < 1e-9 `shouldBe` True
      closeVec 1e-9 (HBM.gradADU mScale names trans uvs)
                    (adGradRef mScale names trans uvs) `shouldBe` True
      closeVec 1e-4 (HBM.gradADU mScale names trans uvs)
                    (centralGrad mScale names trans uvs) `shouldBe` True

    it "synthVecIR (Phase 55.3): heteroscedastic σ_i = exp(g0+g1·z_i) がベクトル密度で吸収" $ do
      -- σ が行依存 (z_i はデータ定数)。 名前付き σ 指紋で 1 グループに揃い、
      -- UC 列を含む σ IR → ベクトル版密度 -Σlogσ_i - Σr_i²/(2σ_i²) (値/tape 両方)。
      let zs = [0.2, -0.5, 1.0, 0.4, -1.2] :: [Double]
          mHet :: HBM.ModelP ()
          mHet = do
            a  <- HBM.sample "a"  (HBM.Normal 0 10)
            g0 <- HBM.sample "g0" (HBM.Normal 0 2)
            g1 <- HBM.sample "g1" (HBM.Normal 0 2)
            mapM_ (\(i, (z, y)) ->
                     HBM.observe (T.pack ("y_" ++ show (i :: Int)))
                       (HBM.Normal a (exp (g0 + g1 * realToFrac z))) [y])
                  (zip [0 ..] (zip zs [1.4, 0.8, 1.9, 1.1, 0.5 :: Double]))
          names = HBM.sampleNames mHet
          tmap  = HBM.getTransforms mHet
          trans = [ tmap M.! n | n <- names ]
          uvs   = [0.9, -0.3, 0.4]
          pU    = M.fromList (zip names uvs)
      null (fst (HBM.synthGaussLMBlocks mHet)) `shouldBe` True
      case HBM.synthVecIR mHet of
        Nothing -> expectationFailure "synthVecIR: expected Just"
        Just (gs, fams, sObs) -> do
          length gs `shouldBe` 1
          length fams `shouldBe` 0
          Set.size sObs `shouldBe` 5
      abs (HBM.compileLogPU mHet names trans uvs
           - HBM.logJointUnconstrained mHet names trans pU) < 1e-9 `shouldBe` True
      closeVec 1e-9 (HBM.gradADU mHet names trans uvs)
                    (adGradRef mHet names trans uvs) `shouldBe` True
      closeVec 1e-4 (HBM.gradADU mHet names trans uvs)
                    (centralGrad mHet names trans uvs) `shouldBe` True

    it "synthVecIR (Phase 55.4): M7 形 (Poisson 回帰 log link) が IR 吸収され値/勾配一致" $ do
      -- y_i ~ Poisson(exp(a + b·x_i))。 非 Gaussian 観測の IR 化 (本丸)。
      -- Σlog y_i! は compile 時前計算 (勾配に寄与しない)。
      let ysP = [1, 0, 3, 2, 5] :: [Double]
          m7 :: HBM.ModelP ()
          m7 = do
            a <- HBM.sample "a" (HBM.Normal 0 5)
            b <- HBM.sample "b" (HBM.Normal 0 5)
            mapM_ (\(i, (x, y)) ->
                     HBM.observe (T.pack ("y_" ++ show (i :: Int)))
                       (HBM.Poisson (exp (a + b * realToFrac x))) [y])
                  (zip [0 ..] (zip xs ysP))
          names = HBM.sampleNames m7
          tmap  = HBM.getTransforms m7
          trans = [ tmap M.! n | n <- names ]
          uvs   = [0.4, 0.7]
          pU    = M.fromList (zip names uvs)
      null (fst (HBM.synthGaussLMBlocks m7)) `shouldBe` True
      case HBM.synthVecIR m7 of
        Nothing -> expectationFailure "synthVecIR: expected Just"
        Just (gs, fams, sObs) -> do
          length gs `shouldBe` 1
          length fams `shouldBe` 0
          Set.size sObs `shouldBe` 5
      abs (HBM.compileLogPU m7 names trans uvs
           - HBM.logJointUnconstrained m7 names trans pU) < 1e-9 `shouldBe` True
      closeVec 1e-9 (HBM.gradADU m7 names trans uvs)
                    (adGradRef m7 names trans uvs) `shouldBe` True
      closeVec 1e-4 (HBM.gradADU m7 names trans uvs)
                    (centralGrad m7 names trans uvs) `shouldBe` True

    it "synthVecIR (Phase 55.4): M8 形 (logistic 回帰) が IR 吸収され値/勾配一致" $ do
      -- y_i ~ Bernoulli(invLogit(a + b·x_i))。 y ∈ {0,1} は定数係数化。
      let ysB = [1, 0, 1, 1, 0] :: [Double]
          m8 :: HBM.ModelP ()
          m8 = do
            a <- HBM.sample "a" (HBM.Normal 0 5)
            b <- HBM.sample "b" (HBM.Normal 0 5)
            mapM_ (\(i, (x, y)) ->
                     HBM.observe (T.pack ("y_" ++ show (i :: Int)))
                       (HBM.Bernoulli
                          (1 / (1 + exp (negate (a + b * realToFrac x))))) [y])
                  (zip [0 ..] (zip xs ysB))
          names = HBM.sampleNames m8
          tmap  = HBM.getTransforms m8
          trans = [ tmap M.! n | n <- names ]
          uvs   = [0.2, 1.1]
          pU    = M.fromList (zip names uvs)
      case HBM.synthVecIR m8 of
        Nothing -> expectationFailure "synthVecIR: expected Just"
        Just (gs, fams, sObs) -> do
          length gs `shouldBe` 1
          length fams `shouldBe` 0
          Set.size sObs `shouldBe` 5
      abs (HBM.compileLogPU m8 names trans uvs
           - HBM.logJointUnconstrained m8 names trans pU) < 1e-9 `shouldBe` True
      closeVec 1e-9 (HBM.gradADU m8 names trans uvs)
                    (adGradRef m8 names trans uvs) `shouldBe` True
      closeVec 1e-4 (HBM.gradADU m8 names trans uvs)
                    (centralGrad m8 names trans uvs) `shouldBe` True

    it "synthVecIR (Phase 55.4): 階層 GLM 形 (Poisson + 群 intercept) で族 prior も IR に乗る" $ do
      -- y_i ~ Poisson(exp(b0 + u_{g(i)}))、 u_g ~ Normal(0, τ)。 λ 式中の
      -- 族 gather + 族 prior が既存機構のまま乗ること (M6 の Poisson 版)。
      let gids = [0, 0, 1, 1, 0] :: [Int]
          ysP  = [2, 1, 4, 3, 2] :: [Double]
          mG :: HBM.ModelP ()
          mG = do
            b0  <- HBM.sample "b0"  (HBM.Normal 0 5)
            tau <- HBM.sample "tau" (HBM.HalfNormal 2)
            us  <- mapM (\j -> HBM.sample (T.pack ("u_" ++ show (j :: Int)))
                                          (HBM.Normal 0 tau)) [0, 1]
            mapM_ (\(i, (g, y)) ->
                     HBM.observe (T.pack ("y_" ++ show (i :: Int)))
                       (HBM.Poisson (exp (b0 + us !! g))) [y])
                  (zip [0 ..] (zip gids ysP))
          names = HBM.sampleNames mG
          tmap  = HBM.getTransforms mG
          trans = [ tmap M.! n | n <- names ]
          uvs   = [0.5, log 0.8, 0.3, -0.2]
          pU    = M.fromList (zip names uvs)
      case HBM.synthVecIR mG of
        Nothing -> expectationFailure "synthVecIR: expected Just"
        Just (gs, fams, sObs) -> do
          length gs `shouldBe` 1
          [ ms | (ms, _, _) <- fams ] `shouldBe` [["u_0", "u_1"]]
          Set.size sObs `shouldBe` 5
      abs (HBM.compileLogPU mG names trans uvs
           - HBM.logJointUnconstrained mG names trans pU) < 1e-9 `shouldBe` True
      closeVec 1e-9 (HBM.gradADU mG names trans uvs)
                    (adGradRef mG names trans uvs) `shouldBe` True
      closeVec 1e-4 (HBM.gradADU mG names trans uvs)
                    (centralGrad mG names trans uvs) `shouldBe` True

    it "synthVecIR (Phase 55.4): 非対応分布 (AsymmetricLaplace obs のみ) は従来どおり fallback" $ do
      -- 55.4 時点は StudentT で確認していたが 56.3 で吸収対象になったため、
      -- 引き続き非対応の AsymmetricLaplace に差し替え。
      let mT :: HBM.ModelP ()
          mT = do
            a <- HBM.sample "a" (HBM.Normal 0 5)
            s <- HBM.sample "s" (HBM.Exponential 1)
            HBM.observe "y" (HBM.AsymmetricLaplace s 1 a) [0.5, 1.0, -0.2]
          names = HBM.sampleNames mT
          tmap  = HBM.getTransforms mT
          trans = [ tmap M.! n | n <- names ]
          uvs   = [0.3, -0.1]
          pU    = M.fromList (zip names uvs)
      (case HBM.synthVecIR mT of Nothing -> True; Just _ -> False)
        `shouldBe` True
      abs (HBM.compileLogPU mT names trans uvs
           - HBM.logJointUnconstrained mT names trans pU) < 1e-9 `shouldBe` True
      closeVec 1e-4 (HBM.gradADU mT names trans uvs)
                    (centralGrad mT names trans uvs) `shouldBe` True

    it "digamma (Phase 56.1): 既知値 + 漸化式 + lgammaApprox 中心差分一致" $ do
      let gammaE = 0.5772156649015329 :: Double
      -- 既知値: ψ(1) = -γ, ψ(1/2) = -γ - 2 ln 2
      abs (HBM.digamma 1.0 - negate gammaE) < 1e-9 `shouldBe` True
      abs (HBM.digamma 0.5 - (negate gammaE - 2 * log 2)) < 1e-9 `shouldBe` True
      -- 漸化式 ψ(x+1) = ψ(x) + 1/x (構成上ほぼ厳密)
      mapM_ (\x -> abs (HBM.digamma (x + 1) - HBM.digamma x - 1 / x) < 1e-12
                     `shouldBe` True)
            [0.3, 1.7, 5.5, 20.0 :: Double]
      -- 実際に使う lgammaApprox の数値微分 (中心差分 h=1e-5) と 1e-8 一致。
      -- ⚠ x は整数を避ける: x±h が lgammaApprox の再帰段数境界 (x+k=12) を
      -- 跨ぐと打切り誤差ジャンプ ~1e-9 が /2h 増幅され FD 自体が壊れる
      -- (lgammaApprox の性質・digamma の問題ではない)。
      let h = 1e-5
          cd x = (HBM.lgammaApprox (x + h) - HBM.lgammaApprox (x - h)) / (2 * h)
      mapM_ (\x -> abs (cd x - HBM.digamma x) < 1e-8 `shouldBe` True)
            [0.7, 2.3, 8.1, 15.3, 40.2 :: Double]

    it "synthVecIR (Phase 56.3): StudentT (ν=SC) robust 回帰形が IR 吸収され値/勾配一致" $ do
      -- y_i ~ StudentT(4, a + b·x_i, σ)。 ν=SC 定数 → lgamma 項は compile 時
      -- 定数化され密度は初等演算のみ (外れ値 8.0 入りの robust 回帰形)。
      let ysT = [2.1, 1.7, 1.4, 1.2, 8.0] :: [Double]
          mSt :: HBM.ModelP ()
          mSt = do
            a <- HBM.sample "a"     (HBM.Normal 0 10)
            b <- HBM.sample "b"     (HBM.Normal 0 10)
            s <- HBM.sample "sigma" (HBM.Exponential 1)
            mapM_ (\(i, (x, y)) ->
                     HBM.observe (T.pack ("y_" ++ show (i :: Int)))
                       (HBM.StudentT 4 (a + b * realToFrac x) s) [y])
                  (zip [0 ..] (zip xs ysT))
          names = HBM.sampleNames mSt
          tmap  = HBM.getTransforms mSt
          trans = [ tmap M.! n | n <- names ]
          uvs   = [1.4, -0.6, log 0.5]
          pU    = M.fromList (zip names uvs)
      case HBM.synthVecIR mSt of
        Nothing -> expectationFailure "synthVecIR: expected Just"
        Just (gs, fams, sObs) -> do
          length gs `shouldBe` 1
          length fams `shouldBe` 0
          Set.size sObs `shouldBe` 5
      abs (HBM.compileLogPU mSt names trans uvs
           - HBM.logJointUnconstrained mSt names trans pU) < 1e-9 `shouldBe` True
      closeVec 1e-9 (HBM.gradADU mSt names trans uvs)
                    (adGradRef mSt names trans uvs) `shouldBe` True
      closeVec 1e-4 (HBM.gradADU mSt names trans uvs)
                    (centralGrad mSt names trans uvs) `shouldBe` True

    it "synthVecIR (Phase 56.3): StudentT の ν latent は fallback (計画 scope どおり)" $ do
      -- ν を latent にすると lgamma(ν) 項が定数化できず収集対象外 →
      -- IR 合成されず従来 ad 経路で値/勾配が正しいこと。
      let mNu :: HBM.ModelP ()
          mNu = do
            nu <- HBM.sample "nu" (HBM.Exponential 0.1)
            a  <- HBM.sample "a"  (HBM.Normal 0 10)
            s  <- HBM.sample "s"  (HBM.Exponential 1)
            HBM.observe "y" (HBM.StudentT nu a s) [0.5, 1.0, -0.2]
          names = HBM.sampleNames mNu
          tmap  = HBM.getTransforms mNu
          trans = [ tmap M.! n | n <- names ]
          uvs   = [log 4, 0.3, log 0.7]
          pU    = M.fromList (zip names uvs)
      (case HBM.synthVecIR mNu of Nothing -> True; Just _ -> False)
        `shouldBe` True
      abs (HBM.compileLogPU mNu names trans uvs
           - HBM.logJointUnconstrained mNu names trans pU) < 1e-9 `shouldBe` True
      closeVec 1e-4 (HBM.gradADU mNu names trans uvs)
                    (centralGrad mNu names trans uvs) `shouldBe` True

    it "synthVecIR (Phase 56.3): Cauchy robust 回帰形が IR 吸収され値/勾配一致" $ do
      -- y_i ~ Cauchy(a + b·x_i, γ)。 logp = -n·logπ - Σlogγ - Σlog(1+z²)。
      let ysC = [2.1, 1.7, 1.4, 1.2, 8.0] :: [Double]
          mC :: HBM.ModelP ()
          mC = do
            a <- HBM.sample "a" (HBM.Normal 0 10)
            b <- HBM.sample "b" (HBM.Normal 0 10)
            g <- HBM.sample "gamma" (HBM.Exponential 1)
            mapM_ (\(i, (x, y)) ->
                     HBM.observe (T.pack ("y_" ++ show (i :: Int)))
                       (HBM.Cauchy (a + b * realToFrac x) g) [y])
                  (zip [0 ..] (zip xs ysC))
          names = HBM.sampleNames mC
          tmap  = HBM.getTransforms mC
          trans = [ tmap M.! n | n <- names ]
          uvs   = [1.4, -0.6, log 0.5]
          pU    = M.fromList (zip names uvs)
      case HBM.synthVecIR mC of
        Nothing -> expectationFailure "synthVecIR: expected Just"
        Just (gs, fams, sObs) -> do
          length gs `shouldBe` 1
          length fams `shouldBe` 0
          Set.size sObs `shouldBe` 5
      abs (HBM.compileLogPU mC names trans uvs
           - HBM.logJointUnconstrained mC names trans pU) < 1e-9 `shouldBe` True
      closeVec 1e-9 (HBM.gradADU mC names trans uvs)
                    (adGradRef mC names trans uvs) `shouldBe` True
      closeVec 1e-4 (HBM.gradADU mC names trans uvs)
                    (centralGrad mC names trans uvs) `shouldBe` True

    it "synthVecIR (Phase 56.3): Logistic 回帰形が IR 吸収され値/勾配一致" $ do
      -- y_i ~ Logistic(a + b·x_i, s)。 logp = -Σz - Σlog s - 2·Σlog(1+exp(-z))。
      let ysL = [2.1, 1.7, 1.4, 1.2, 1.0] :: [Double]
          mL :: HBM.ModelP ()
          mL = do
            a <- HBM.sample "a" (HBM.Normal 0 10)
            b <- HBM.sample "b" (HBM.Normal 0 10)
            s <- HBM.sample "s" (HBM.Exponential 1)
            mapM_ (\(i, (x, y)) ->
                     HBM.observe (T.pack ("y_" ++ show (i :: Int)))
                       (HBM.Logistic (a + b * realToFrac x) s) [y])
                  (zip [0 ..] (zip xs ysL))
          names = HBM.sampleNames mL
          tmap  = HBM.getTransforms mL
          trans = [ tmap M.! n | n <- names ]
          uvs   = [1.4, -0.6, log 0.5]
          pU    = M.fromList (zip names uvs)
      case HBM.synthVecIR mL of
        Nothing -> expectationFailure "synthVecIR: expected Just"
        Just (gs, fams, sObs) -> do
          length gs `shouldBe` 1
          length fams `shouldBe` 0
          Set.size sObs `shouldBe` 5
      abs (HBM.compileLogPU mL names trans uvs
           - HBM.logJointUnconstrained mL names trans pU) < 1e-9 `shouldBe` True
      closeVec 1e-9 (HBM.gradADU mL names trans uvs)
                    (adGradRef mL names trans uvs) `shouldBe` True
      closeVec 1e-4 (HBM.gradADU mL names trans uvs)
                    (centralGrad mL names trans uvs) `shouldBe` True

    it "synthVecIR (Phase 56.3): Logistic heteroscedastic s_i = exp(g0+g1·x_i) もベクトル密度で吸収" $ do
      -- scale 行依存 → Σlog s_i のベクトル分岐 ('sumLogScale' の RUSum 側) カバー。
      let ysL = [2.1, 1.7, 1.4, 1.2, 1.0] :: [Double]
          mLh :: HBM.ModelP ()
          mLh = do
            a  <- HBM.sample "a"  (HBM.Normal 0 10)
            g0 <- HBM.sample "g0" (HBM.Normal 0 2)
            g1 <- HBM.sample "g1" (HBM.Normal 0 2)
            mapM_ (\(i, (x, y)) ->
                     HBM.observe (T.pack ("y_" ++ show (i :: Int)))
                       (HBM.Logistic a (exp (g0 + g1 * realToFrac x))) [y])
                  (zip [0 ..] (zip xs ysL))
          names = HBM.sampleNames mLh
          tmap  = HBM.getTransforms mLh
          trans = [ tmap M.! n | n <- names ]
          uvs   = [1.4, -0.3, 0.2]
          pU    = M.fromList (zip names uvs)
      case HBM.synthVecIR mLh of
        Nothing -> expectationFailure "synthVecIR: expected Just"
        Just (gs, fams, sObs) -> do
          length gs `shouldBe` 1
          length fams `shouldBe` 0
          Set.size sObs `shouldBe` 5
      abs (HBM.compileLogPU mLh names trans uvs
           - HBM.logJointUnconstrained mLh names trans pU) < 1e-9 `shouldBe` True
      closeVec 1e-9 (HBM.gradADU mLh names trans uvs)
                    (adGradRef mLh names trans uvs) `shouldBe` True
      closeVec 1e-4 (HBM.gradADU mLh names trans uvs)
                    (centralGrad mLh names trans uvs) `shouldBe` True

    it "synthVecIR (Phase 56.3): Gumbel 回帰形が IR 吸収され値/勾配一致" $ do
      -- y_i ~ Gumbel(a + b·x_i, β)。 logp = -Σlogβ - Σz - Σexp(-z) (極値回帰形)。
      let ysG = [2.5, 2.0, 1.8, 1.5, 3.2] :: [Double]
          mGu :: HBM.ModelP ()
          mGu = do
            a <- HBM.sample "a" (HBM.Normal 0 10)
            b <- HBM.sample "b" (HBM.Normal 0 10)
            be <- HBM.sample "beta" (HBM.Exponential 1)
            mapM_ (\(i, (x, y)) ->
                     HBM.observe (T.pack ("y_" ++ show (i :: Int)))
                       (HBM.Gumbel (a + b * realToFrac x) be) [y])
                  (zip [0 ..] (zip xs ysG))
          names = HBM.sampleNames mGu
          tmap  = HBM.getTransforms mGu
          trans = [ tmap M.! n | n <- names ]
          uvs   = [1.8, -0.4, log 0.6]
          pU    = M.fromList (zip names uvs)
      case HBM.synthVecIR mGu of
        Nothing -> expectationFailure "synthVecIR: expected Just"
        Just (gs, fams, sObs) -> do
          length gs `shouldBe` 1
          length fams `shouldBe` 0
          Set.size sObs `shouldBe` 5
      abs (HBM.compileLogPU mGu names trans uvs
           - HBM.logJointUnconstrained mGu names trans pU) < 1e-9 `shouldBe` True
      closeVec 1e-9 (HBM.gradADU mGu names trans uvs)
                    (adGradRef mGu names trans uvs) `shouldBe` True
      closeVec 1e-4 (HBM.gradADU mGu names trans uvs)
                    (centralGrad mGu names trans uvs) `shouldBe` True

    it "synthVecIR (Phase 56.4): Exponential 生存形 (rate=exp(η)) が IR 吸収され値/勾配一致" $ do
      -- y_i ~ Exponential(exp(a + b·x_i))。 logp = Σlog rate - Σ rate·y。
      let ysE = [0.8, 1.5, 0.3, 2.1, 0.6] :: [Double]
          mE :: HBM.ModelP ()
          mE = do
            a <- HBM.sample "a" (HBM.Normal 0 5)
            b <- HBM.sample "b" (HBM.Normal 0 5)
            mapM_ (\(i, (x, y)) ->
                     HBM.observe (T.pack ("y_" ++ show (i :: Int)))
                       (HBM.Exponential (exp (a + b * realToFrac x))) [y])
                  (zip [0 ..] (zip xs ysE))
          names = HBM.sampleNames mE
          tmap  = HBM.getTransforms mE
          trans = [ tmap M.! n | n <- names ]
          uvs   = [0.4, -0.3]
          pU    = M.fromList (zip names uvs)
      case HBM.synthVecIR mE of
        Nothing -> expectationFailure "synthVecIR: expected Just"
        Just (gs, fams, sObs) -> do
          length gs `shouldBe` 1
          length fams `shouldBe` 0
          Set.size sObs `shouldBe` 5
      abs (HBM.compileLogPU mE names trans uvs
           - HBM.logJointUnconstrained mE names trans pU) < 1e-9 `shouldBe` True
      closeVec 1e-9 (HBM.gradADU mE names trans uvs)
                    (adGradRef mE names trans uvs) `shouldBe` True
      closeVec 1e-4 (HBM.gradADU mE names trans uvs)
                    (centralGrad mE names trans uvs) `shouldBe` True

    it "synthVecIR (Phase 56.4): 定義域外 y (負値) を含むグループは収集拒否で fallback" $ do
      -- Exponential 観測に y < 0 が混在 → グループ丸ごと吸収しない
      -- (walk の -∞ 縮退をそのまま残す安全方向・55.4 の Poisson と同じ規律)。
      let mNeg :: HBM.ModelP ()
          mNeg = do
            a <- HBM.sample "a" (HBM.Normal 0 5)
            HBM.observe "y" (HBM.Exponential (exp a)) [0.8, -0.5, 1.2]
      (case HBM.synthVecIR mNeg of Nothing -> True; Just _ -> False)
        `shouldBe` True

    it "synthVecIR (Phase 56.4): Weibull 生存形 (k latent, λ=exp(η)) が IR 吸収され値/勾配一致" $ do
      -- y_i ~ Weibull(k, exp(a + b·x_i))。 (y/λ)^k = exp(k·(log y - log λ)) の
      -- 初等化で k も latent のまま吸収 (lgamma 不要)。
      let ysW = [0.8, 1.5, 0.3, 2.1, 0.6] :: [Double]
          mW :: HBM.ModelP ()
          mW = do
            k <- HBM.sample "k" (HBM.Exponential 1)
            a <- HBM.sample "a" (HBM.Normal 0 5)
            b <- HBM.sample "b" (HBM.Normal 0 5)
            mapM_ (\(i, (x, y)) ->
                     HBM.observe (T.pack ("y_" ++ show (i :: Int)))
                       (HBM.Weibull k (exp (a + b * realToFrac x))) [y])
                  (zip [0 ..] (zip xs ysW))
          names = HBM.sampleNames mW
          tmap  = HBM.getTransforms mW
          trans = [ tmap M.! n | n <- names ]
          uvs   = [log 1.3, 0.4, -0.3]
          pU    = M.fromList (zip names uvs)
      case HBM.synthVecIR mW of
        Nothing -> expectationFailure "synthVecIR: expected Just"
        Just (gs, fams, sObs) -> do
          length gs `shouldBe` 1
          length fams `shouldBe` 0
          Set.size sObs `shouldBe` 5
      abs (HBM.compileLogPU mW names trans uvs
           - HBM.logJointUnconstrained mW names trans pU) < 1e-9 `shouldBe` True
      closeVec 1e-9 (HBM.gradADU mW names trans uvs)
                    (adGradRef mW names trans uvs) `shouldBe` True
      closeVec 1e-4 (HBM.gradADU mW names trans uvs)
                    (centralGrad mW names trans uvs) `shouldBe` True

    it "synthVecIR (Phase 56.4): LogNormal 回帰形が Gaussian ノード再利用で IR 吸収され値/勾配一致" $ do
      -- y_i ~ LogNormal(a + b·x_i, σ)。 log y 前計算 → VOGauss densityIR
      -- 再利用 + 定数 -Σlog y (新密度ノード無し・計画どおり)。
      let ysLn = [0.8, 1.5, 0.3, 2.1, 0.6] :: [Double]
          mLn :: HBM.ModelP ()
          mLn = do
            a <- HBM.sample "a" (HBM.Normal 0 5)
            b <- HBM.sample "b" (HBM.Normal 0 5)
            s <- HBM.sample "sigma" (HBM.Exponential 1)
            mapM_ (\(i, (x, y)) ->
                     HBM.observe (T.pack ("y_" ++ show (i :: Int)))
                       (HBM.LogNormal (a + b * realToFrac x) s) [y])
                  (zip [0 ..] (zip xs ysLn))
          names = HBM.sampleNames mLn
          tmap  = HBM.getTransforms mLn
          trans = [ tmap M.! n | n <- names ]
          uvs   = [0.2, -0.4, log 0.7]
          pU    = M.fromList (zip names uvs)
      case HBM.synthVecIR mLn of
        Nothing -> expectationFailure "synthVecIR: expected Just"
        Just (gs, fams, sObs) -> do
          length gs `shouldBe` 1
          length fams `shouldBe` 0
          Set.size sObs `shouldBe` 5
      abs (HBM.compileLogPU mLn names trans uvs
           - HBM.logJointUnconstrained mLn names trans pU) < 1e-9 `shouldBe` True
      closeVec 1e-9 (HBM.gradADU mLn names trans uvs)
                    (adGradRef mLn names trans uvs) `shouldBe` True
      closeVec 1e-4 (HBM.gradADU mLn names trans uvs)
                    (centralGrad mLn names trans uvs) `shouldBe` True

    it "synthVecIR (Phase 56.4): Gamma 回帰形 (α latent, rate=exp(η)) が IR 吸収され値/勾配一致" $ do
      -- y_i ~ Gamma(α, exp(a + b·x_i))。 lgammaΓ(α) は SLgammaO
      -- (値 lgammaApprox / 導関数 digamma) — α latent の勾配も記号微分で自動。
      let ysG = [0.8, 1.5, 0.3, 2.1, 0.6] :: [Double]
          mGa :: HBM.ModelP ()
          mGa = do
            al <- HBM.sample "alpha" (HBM.Exponential 1)
            a  <- HBM.sample "a" (HBM.Normal 0 5)
            b  <- HBM.sample "b" (HBM.Normal 0 5)
            mapM_ (\(i, (x, y)) ->
                     HBM.observe (T.pack ("y_" ++ show (i :: Int)))
                       (HBM.Gamma al (exp (a + b * realToFrac x))) [y])
                  (zip [0 ..] (zip xs ysG))
          names = HBM.sampleNames mGa
          tmap  = HBM.getTransforms mGa
          trans = [ tmap M.! n | n <- names ]
          uvs   = [log 1.6, 0.4, -0.3]
          pU    = M.fromList (zip names uvs)
      case HBM.synthVecIR mGa of
        Nothing -> expectationFailure "synthVecIR: expected Just"
        Just (gs, fams, sObs) -> do
          length gs `shouldBe` 1
          length fams `shouldBe` 0
          Set.size sObs `shouldBe` 5
      abs (HBM.compileLogPU mGa names trans uvs
           - HBM.logJointUnconstrained mGa names trans pU) < 1e-9 `shouldBe` True
      closeVec 1e-9 (HBM.gradADU mGa names trans uvs)
                    (adGradRef mGa names trans uvs) `shouldBe` True
      closeVec 1e-4 (HBM.gradADU mGa names trans uvs)
                    (centralGrad mGa names trans uvs) `shouldBe` True

    it "synthVecIR (Phase 56.4): Beta 回帰形 (α=μφ, β=(1-μ)φ, μ=invLogit(η)) が IR 吸収され値/勾配一致" $ do
      -- y_i ~ Beta(μ_i·φ, (1-μ_i)·φ)。 両パラメタとも行依存式 + lgammaΓ 3 項。
      -- ⚠ φ は整数を避ける (φ=3.0 だと α+β の lgammaApprox 再帰が x+k=12 整数
      -- 境界に乗り中心差分が壊れる・56.1 で記録済みの FD 罠を実測で再確認)。
      let ysB = [0.3, 0.6, 0.4, 0.7, 0.5] :: [Double]
          mBe :: HBM.ModelP ()
          mBe = do
            a  <- HBM.sample "a" (HBM.Normal 0 5)
            b  <- HBM.sample "b" (HBM.Normal 0 5)
            ph <- HBM.sample "phi" (HBM.Exponential 0.5)
            mapM_ (\(i, (x, y)) ->
                     let muI = 1 / (1 + exp (negate (a + b * realToFrac x)))
                     in HBM.observe (T.pack ("y_" ++ show (i :: Int)))
                          (HBM.Beta (muI * ph) ((1 - muI) * ph)) [y])
                  (zip [0 ..] (zip xs ysB))
          names = HBM.sampleNames mBe
          tmap  = HBM.getTransforms mBe
          trans = [ tmap M.! n | n <- names ]
          uvs   = [0.4, -0.3, log 3.3]
          pU    = M.fromList (zip names uvs)
      case HBM.synthVecIR mBe of
        Nothing -> expectationFailure "synthVecIR: expected Just"
        Just (gs, fams, sObs) -> do
          length gs `shouldBe` 1
          length fams `shouldBe` 0
          Set.size sObs `shouldBe` 5
      abs (HBM.compileLogPU mBe names trans uvs
           - HBM.logJointUnconstrained mBe names trans pU) < 1e-9 `shouldBe` True
      closeVec 1e-9 (HBM.gradADU mBe names trans uvs)
                    (adGradRef mBe names trans uvs) `shouldBe` True
      closeVec 1e-4 (HBM.gradADU mBe names trans uvs)
                    (centralGrad mBe names trans uvs) `shouldBe` True

    it "synthVecIR (Phase 56.5): Binomial グループ形 (n=10, p=invLogit(η)) が IR 吸収され値/勾配一致" $ do
      -- y_i ~ Binomial(10, invLogit(a + b·x_i))。 Bernoulli の 0/1 係数を
      -- k/n-k に一般化 + ΣlogC(n,k) は compile 時定数。
      let ysK = [3, 7, 5, 8, 2] :: [Double]
          mBi :: HBM.ModelP ()
          mBi = do
            a <- HBM.sample "a" (HBM.Normal 0 5)
            b <- HBM.sample "b" (HBM.Normal 0 5)
            mapM_ (\(i, (x, y)) ->
                     HBM.observe (T.pack ("y_" ++ show (i :: Int)))
                       (HBM.Binomial 10
                          (1 / (1 + exp (negate (a + b * realToFrac x))))) [y])
                  (zip [0 ..] (zip xs ysK))
          names = HBM.sampleNames mBi
          tmap  = HBM.getTransforms mBi
          trans = [ tmap M.! n | n <- names ]
          uvs   = [0.3, 0.8]
          pU    = M.fromList (zip names uvs)
      case HBM.synthVecIR mBi of
        Nothing -> expectationFailure "synthVecIR: expected Just"
        Just (gs, fams, sObs) -> do
          length gs `shouldBe` 1
          length fams `shouldBe` 0
          Set.size sObs `shouldBe` 5
      abs (HBM.compileLogPU mBi names trans uvs
           - HBM.logJointUnconstrained mBi names trans pU) < 1e-9 `shouldBe` True
      closeVec 1e-9 (HBM.gradADU mBi names trans uvs)
                    (adGradRef mBi names trans uvs) `shouldBe` True
      closeVec 1e-4 (HBM.gradADU mBi names trans uvs)
                    (centralGrad mBi names trans uvs) `shouldBe` True

    it "synthVecIR (Phase 94): 行ごとに n が異なる Binomial が 1 group に merge され値/勾配一致" $ do
      -- Phase 94 の核心。 seeds (11-seeds) 型 = 各行の試行数 n_i が異なる。
      -- 修正前は n を group key に含めていたため n 別に分裂 (ここでは 5 行全て
      -- 相異なる n → 5 group)。 n を行対応 Vector 化し key から除外することで
      -- 1 group に merge される (= per-eval の 17→1 group 化と同じ改修)。
      let nsB  = [10, 12, 8, 15, 6] :: [Double]   -- 全て相異なる n
          ysK  = [3, 7, 5, 8, 2]  :: [Double]     -- 0 ≤ y_i ≤ n_i
          mBn :: HBM.ModelP ()
          mBn = do
            a <- HBM.sample "a" (HBM.Normal 0 5)
            b <- HBM.sample "b" (HBM.Normal 0 5)
            mapM_ (\(i, (x, (nn, y))) ->
                     HBM.observe (T.pack ("y_" ++ show (i :: Int)))
                       (HBM.Binomial (round nn)
                          (1 / (1 + exp (negate (a + b * realToFrac x))))) [y])
                  (zip [0 ..] (zip xs (zip nsB ysK)))
          names = HBM.sampleNames mBn
          tmap  = HBM.getTransforms mBn
          trans = [ tmap M.! n | n <- names ]
          uvs   = [0.3, 0.8]
          pU    = M.fromList (zip names uvs)
      case HBM.synthVecIR mBn of
        Nothing -> expectationFailure "synthVecIR: expected Just"
        Just (gs, fams, sObs) -> do
          length gs   `shouldBe` 1   -- ★Phase 94: n 別分裂が解消され 1 group
          length fams `shouldBe` 0
          Set.size sObs `shouldBe` 5
      abs (HBM.compileLogPU mBn names trans uvs
           - HBM.logJointUnconstrained mBn names trans pU) < 1e-9 `shouldBe` True
      closeVec 1e-9 (HBM.gradADU mBn names trans uvs)
                    (adGradRef mBn names trans uvs) `shouldBe` True
      closeVec 1e-4 (HBM.gradADU mBn names trans uvs)
                    (centralGrad mBn names trans uvs) `shouldBe` True

    it "synthVecIR (Phase 56.5): Geometric 回帰形 (p=invLogit(η)) が IR 吸収され値/勾配一致" $ do
      -- y_i ~ Geometric(invLogit(a + b·x_i))。 logp = Σ(k-1)·log(1-p) + Σlog p。
      let ysG = [2, 1, 4, 3, 1] :: [Double]
          mGe :: HBM.ModelP ()
          mGe = do
            a <- HBM.sample "a" (HBM.Normal 0 5)
            b <- HBM.sample "b" (HBM.Normal 0 5)
            mapM_ (\(i, (x, y)) ->
                     HBM.observe (T.pack ("y_" ++ show (i :: Int)))
                       (HBM.Geometric
                          (1 / (1 + exp (negate (a + b * realToFrac x))))) [y])
                  (zip [0 ..] (zip xs ysG))
          names = HBM.sampleNames mGe
          tmap  = HBM.getTransforms mGe
          trans = [ tmap M.! n | n <- names ]
          uvs   = [-0.2, 0.5]
          pU    = M.fromList (zip names uvs)
      case HBM.synthVecIR mGe of
        Nothing -> expectationFailure "synthVecIR: expected Just"
        Just (gs, fams, sObs) -> do
          length gs `shouldBe` 1
          length fams `shouldBe` 0
          Set.size sObs `shouldBe` 5
      abs (HBM.compileLogPU mGe names trans uvs
           - HBM.logJointUnconstrained mGe names trans pU) < 1e-9 `shouldBe` True
      closeVec 1e-9 (HBM.gradADU mGe names trans uvs)
                    (adGradRef mGe names trans uvs) `shouldBe` True
      closeVec 1e-4 (HBM.gradADU mGe names trans uvs)
                    (centralGrad mGe names trans uvs) `shouldBe` True

    it "synthVecIR (Phase 56.5): NegBin 回帰形 (μ=exp(η), α latent) が IR 吸収され値/勾配一致" $ do
      -- y_i ~ NegativeBinomial(exp(a + b·x_i), α)。 lgammaΓ(k_i+α) は SLgammaO の
      -- elementwise 適用・lgammaΓ(k_i+1) は compile 時定数 (56.5 本命)。
      -- ⚠ α は整数を避ける (lgammaApprox x+k=12 境界の FD 罠)。
      let ysN = [2, 0, 5, 3, 1] :: [Double]
          mNb :: HBM.ModelP ()
          mNb = do
            a  <- HBM.sample "a" (HBM.Normal 0 5)
            b  <- HBM.sample "b" (HBM.Normal 0 5)
            al <- HBM.sample "alpha" (HBM.Exponential 0.5)
            mapM_ (\(i, (x, y)) ->
                     HBM.observe (T.pack ("y_" ++ show (i :: Int)))
                       (HBM.NegativeBinomial
                          (exp (a + b * realToFrac x)) al) [y])
                  (zip [0 ..] (zip xs ysN))
          names = HBM.sampleNames mNb
          tmap  = HBM.getTransforms mNb
          trans = [ tmap M.! n | n <- names ]
          uvs   = [0.6, -0.3, log 2.3]
          pU    = M.fromList (zip names uvs)
      case HBM.synthVecIR mNb of
        Nothing -> expectationFailure "synthVecIR: expected Just"
        Just (gs, fams, sObs) -> do
          length gs `shouldBe` 1
          length fams `shouldBe` 0
          Set.size sObs `shouldBe` 5
      abs (HBM.compileLogPU mNb names trans uvs
           - HBM.logJointUnconstrained mNb names trans pU) < 1e-9 `shouldBe` True
      closeVec 1e-9 (HBM.gradADU mNb names trans uvs)
                    (adGradRef mNb names trans uvs) `shouldBe` True
      closeVec 1e-4 (HBM.gradADU mNb names trans uvs)
                    (centralGrad mNb names trans uvs) `shouldBe` True

    it "synthVecIR (Phase 94): 行ごとに n が異なる ZeroInflatedBinomial が 1 group に merge され値/勾配一致" $ do
      -- Phase 94 で SDZIBinom も n を行対応 Vector 化 (SDBinom と同型)。
      -- y=0 行を含めて branch0 (仮想 y=0 密度 = 行ごとの n を使う) を発火させる。
      let nsB  = [10, 12, 8, 15, 6] :: [Double]   -- 全て相異なる n
          ysZ  = [0, 7, 0, 8, 2]  :: [Double]     -- y=0 行 2 つで branch0 を経由
          mZb :: HBM.ModelP ()
          mZb = do
            a <- HBM.sample "a" (HBM.Normal 0 5)
            b <- HBM.sample "b" (HBM.Normal 0 5)
            c <- HBM.sample "c" (HBM.Normal 0 5)
            mapM_ (\(i, (x, (nn, y))) ->
                     HBM.observe (T.pack ("y_" ++ show (i :: Int)))
                       (HBM.ZeroInflatedBinomial (round nn)
                          (1 / (1 + exp (negate c)))                     -- ψ = invLogit(c)
                          (1 / (1 + exp (negate (a + b * realToFrac x))))) [y])
                  (zip [0 ..] (zip xs (zip nsB ysZ)))
          names = HBM.sampleNames mZb
          tmap  = HBM.getTransforms mZb
          trans = [ tmap M.! n | n <- names ]
          uvs   = [0.3, 0.8, -0.5]
          pU    = M.fromList (zip names uvs)
      case HBM.synthVecIR mZb of
        Nothing -> expectationFailure "synthVecIR: expected Just"
        Just (gs, fams, sObs) -> do
          length gs   `shouldBe` 1   -- ★Phase 94: n 別分裂が解消され 1 group
          length fams `shouldBe` 0
          Set.size sObs `shouldBe` 5
      abs (HBM.compileLogPU mZb names trans uvs
           - HBM.logJointUnconstrained mZb names trans pU) < 1e-9 `shouldBe` True
      closeVec 1e-9 (HBM.gradADU mZb names trans uvs)
                    (adGradRef mZb names trans uvs) `shouldBe` True
      closeVec 1e-4 (HBM.gradADU mZb names trans uvs)
                    (centralGrad mZb names trans uvs) `shouldBe` True

    it "synthVecIR (Phase 90 A10): raw potential (同型 Σ チェーン) が VGPot 吸収され値/勾配一致" $ do
      -- BYM2 ミニ形 (13-traffic の縮小同型): Poisson 尤度 + icar ペア差分
      -- potential (12 項 ≥ potSumThreshold) + ソフトゼロ和 potential (内部
      -- Σφ = 10 項)。 両 potential が USum ベクトル化で VGPot に吸収され
      -- 残差 ad ゼロ。 値/勾配が従来 ad・中心差分と一致。
      let nA = 10 :: Int
          edges = [ (i, i + 1) | i <- [0 .. 8] ] ++ [(0, 5), (2, 7), (4, 9)]
          ysB = [1, 0, 2, 1, 3, 0, 1, 2, 1, 0] :: [Double]
          mB :: HBM.ModelP ()
          mB = do
            b0 <- HBM.sample "b0" (HBM.Normal 0 1)
            sg <- HBM.sample "sg" (HBM.HalfNormal 1)
            phis <- mapM (\i -> HBM.sample (T.pack ("phi_" ++ show (i :: Int)))
                                           (HBM.Normal 0 10)) [0 .. nA - 1]
            mapM_ (\(i, y) ->
                     HBM.observe (T.pack ("y_" ++ show (i :: Int)))
                       (HBM.Poisson (exp (b0 + (phis !! i) * sg))) [y])
                  (zip [0 ..] ysB)
            HBM.potential "icar" (negate 0.5 * sum
              [ (phis !! a - phis !! b) * (phis !! a - phis !! b)
              | (a, b) <- edges ])
            HBM.potential "szero"
              (HBM.logDensity (HBM.Normal 0 0.01) (sum phis))
          names = HBM.sampleNames mB
          tmap  = HBM.getTransforms mB
          trans = [ tmap M.! n | n <- names ]
          uvs   = [ 0.05 + 0.03 * fromIntegral i
                  | i <- [0 .. length names - 1] ]
          pU    = M.fromList (zip names uvs)
      case HBM.synthVecIR mB of
        Nothing -> expectationFailure "synthVecIR: expected Just"
        Just (gs, _fams, sObs) -> do
          length gs `shouldBe` 3            -- Poisson 群 + VGPot ×2
          ("icar" `Set.member` sObs) `shouldBe` True
          ("szero" `Set.member` sObs) `shouldBe` True
      abs (HBM.compileLogPU mB names trans uvs
           - HBM.logJointUnconstrained mB names trans pU) < 1e-9 `shouldBe` True
      closeVec 1e-9 (HBM.gradADU mB names trans uvs)
                    (adGradRef mB names trans uvs) `shouldBe` True
      closeVec 1e-4 (HBM.gradADU mB names trans uvs)
                    (centralGrad mB names trans uvs) `shouldBe` True

    it "synthVecIR (Phase 90 A10): unify 不能な大チェーン potential は残差 ad に残り値/勾配一致" $ do
      -- 項の形が交互に違う (φa·φb と (φa-φb)²) 12 項チェーン → unifyManyD
      -- 失敗 → その potential は吸収されず残差 ad が担う (安全方向)。
      -- Poisson 群は吸収されたまま・二重計上/脱落なく値/勾配一致。
      let nA = 10 :: Int
          edges = [ (i, i + 1) | i <- [0 .. 8] ] ++ [(0, 5), (2, 7), (4, 9)]
          ysB = [1, 0, 2, 1, 3, 0, 1, 2, 1, 0] :: [Double]
          mB :: HBM.ModelP ()
          mB = do
            b0 <- HBM.sample "b0" (HBM.Normal 0 1)
            sg <- HBM.sample "sg" (HBM.HalfNormal 1)
            phis <- mapM (\i -> HBM.sample (T.pack ("phi_" ++ show (i :: Int)))
                                           (HBM.Normal 0 10)) [0 .. nA - 1]
            mapM_ (\(i, y) ->
                     HBM.observe (T.pack ("y_" ++ show (i :: Int)))
                       (HBM.Poisson (exp (b0 + (phis !! i) * sg))) [y])
                  (zip [0 ..] ysB)
            HBM.potential "mixpot" (negate 0.5 * sum
              [ if even k
                  then (phis !! a) * (phis !! b)
                  else (phis !! a - phis !! b) * (phis !! a - phis !! b)
              | (k, (a, b)) <- zip [0 :: Int ..] edges ])
          names = HBM.sampleNames mB
          tmap  = HBM.getTransforms mB
          trans = [ tmap M.! n | n <- names ]
          uvs   = [ 0.05 + 0.03 * fromIntegral i
                  | i <- [0 .. length names - 1] ]
          pU    = M.fromList (zip names uvs)
      case HBM.synthVecIR mB of
        Nothing -> expectationFailure "synthVecIR: expected Just"
        Just (gs, _fams, sObs) -> do
          length gs `shouldBe` 1            -- Poisson 群のみ (VGPot 無し)
          ("mixpot" `Set.member` sObs) `shouldBe` False
      abs (HBM.compileLogPU mB names trans uvs
           - HBM.logJointUnconstrained mB names trans pU) < 1e-9 `shouldBe` True
      closeVec 1e-9 (HBM.gradADU mB names trans uvs)
                    (adGradRef mB names trans uvs) `shouldBe` True
      closeVec 1e-4 (HBM.gradADU mB names trans uvs)
                    (centralGrad mB names trans uvs) `shouldBe` True
