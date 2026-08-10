{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Model.HBM.InterpSpec (spec) where

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
import qualified Data.Map.Strict as M
import qualified Hanalyze.Model.HBM as HBM
import qualified Hanalyze.Model.HBM.Interp as HI
import qualified Data.Map.Strict    as M
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Model.HBM.Interp categorical 列 (Phase 41)" $ do
    it "colDoubles: Numeric はそのまま、 Factor は code を Double 化" $ do
      HI.colDoubles (HI.Numeric [1.0, 2.5]) `shouldBe` [1.0, 2.5]
      HI.colDoubles (HI.Factor ["a", "b"] [0, 1, 0]) `shouldBe` [0.0, 1.0, 0.0]
    it "colLevels: Factor は level 辞書、 Numeric は Nothing" $ do
      HI.colLevels (HI.Factor ["a", "b"] [0, 1]) `shouldBe` Just ["a", "b"]
      HI.colLevels (HI.Numeric [1.0]) `shouldBe` Nothing
    it "groupSuffixFor: 安全な level は可読 suffix '_versicolor'" $
      HI.groupSuffixFor (Just (HI.Factor ["setosa", "versicolor"] [0, 1])) 1
        `shouldBe` "_versicolor"
    it "groupSuffixFor: 不安全な level (空白/記号) は数値 code に fallback" $
      HI.groupSuffixFor (Just (HI.Factor ["A/B", "C D"] [0, 1])) 0 `shouldBe` "_0"
    it "groupSuffixFor: 数値先頭 level も識別子規則違反で fallback" $
      HI.groupSuffixFor (Just (HI.Factor ["1cat", "2dog"] [0, 1])) 0 `shouldBe` "_0"
    it "groupSuffixFor: Numeric / Nothing は従来の数値 suffix" $ do
      HI.groupSuffixFor (Just (HI.Numeric [0, 1, 2])) 2 `shouldBe` "_2"
      HI.groupSuffixFor Nothing 3 `shouldBe` "_3"
    it "groupSuffixFor: 範囲外 code は数値 fallback" $
      HI.groupSuffixFor (Just (HI.Factor ["a"] [0])) 5 `shouldBe` "_5"

  describe "Hanalyze.Model.HBM.Interp categorical observe (Phase 41.5)" $ do
    -- 2 値応答列 outcome = yes/no を出現順 code (yes=0, no=1) で持つ factor。
    let dmF = M.fromList [("outcome", HI.Factor ["yes", "no"] [0, 1, 0, 1])]
        -- do { p <- Beta 1 1; observe "y" (Bernoulli p) 'outcome' }
        beta11 = EApp (EApp (EVar "Beta") (ELit (LNumber 1))) (ELit (LNumber 1))
        bern   = EApp (EVar "Bernoulli") (EVar "p")
        observeOn col = EApp (EApp (EApp (EVar "observe") (ELit (LText "y"))) bern) (ECol col)
        modelOn col = EDo [ DoBind "p" beta11, DoExpr (observeOn col) ]
                          (EApp (EVar "pure") (ELit (LNumber 0)))
        isRight' = either (const False) (const True)
    it "factor 観測列の値は code として渡る (= 2 値 0/1)" $
      HI.lookupDoubles "outcome" dmF `shouldBe` [0.0, 1.0, 0.0, 1.0]
    it "validateAst は factor 列の Bernoulli observe を受理する" $
      HI.validateAst [] (modelOn "outcome") dmF `shouldSatisfy` isRight'
    it "validateAst は存在しない観測列は弾く (factor も例外でない)" $
      HI.validateAst [] (modelOn "nope") dmF `shouldSatisfy` (not . isRight')

  describe "Hanalyze.Model.HBM.Interp 多値 categorical 応答 (Phase 42)" $ do
    let emptyEnv = M.empty :: HI.EnvA Double
        emptyDM  = M.empty :: HI.DataMap
        evalD e  = HI.evalDist emptyEnv emptyDM Nothing e :: Err (HBM.Distribution Double)
        evalV e  = HI.evalValue emptyEnv emptyDM Nothing e :: Err (HI.Value Double)
        num d    = ELit (LNumber d)
        -- Categorical [0.2, 0.3, 0.5]
        catE     = EApp (EVar "Categorical") (EList [num 0.2, num 0.3, num 0.5])
        -- OrderedLogistic 0.0 [-1, 1]
        olE      = EApp (EApp (EVar "OrderedLogistic") (num 0.0))
                        (EList [ENeg (num 1), num 1])
        isRight' = either (const False) (const True)
    it "evalValue: EList を VList に評価する (Phase 42 list 引数の土台)" $
      (case evalV (EList [num 1, num 2, num 3]) of
         Right (HI.VList xs) -> length xs == 3
         _                   -> False) `shouldBe` True
    it "asList: VList は要素列を返し、 非 list はエラー" $ do
      isRight' (HI.asList (HI.VList [HI.VNum (1 :: Double), HI.VNum 2])) `shouldBe` True
      isRight' (HI.asList (HI.VNum (1 :: Double))) `shouldBe` False
    it "evalDist: Categorical [probs] を構築する" $
      show (evalD catE)
        `shouldBe` show (Right (HBM.Categorical [0.2, 0.3, 0.5])
                          :: Err (HBM.Distribution Double))
    it "evalDist: OrderedLogistic eta [cuts] を構築する" $
      show (evalD olE)
        `shouldBe` show (Right (HBM.OrderedLogistic 0.0 [-1, 1])
                          :: Err (HBM.Distribution Double))
    it "evalDist: Categorical の probs に非リスト (スカラ) を渡すとエラー" $
      evalD (EApp (EVar "Categorical") (num 0.5)) `shouldSatisfy` (not . isRight')

    -- 3 水準 factor 応答列 grade = A/B/C を出現順 code (A=0,B=1,C=2) で観測。
    let dm3 = M.fromList [("grade", HI.Factor ["A", "B", "C"] [0, 1, 2, 0, 1, 2])]
        normal01 = EApp (EApp (EVar "Normal") (num 0)) (num 1)
        olObs    = EApp (EApp (EVar "OrderedLogistic") (EVar "eta"))
                        (EList [ENeg (num 1), num 1])
        observeOL col = EApp (EApp (EApp (EVar "observe") (ELit (LText "y"))) olObs) (ECol col)
        modelOL col = EDo [ DoBind "eta" normal01, DoExpr (observeOL col) ]
                          (EApp (EVar "pure") (num 0))
    it "validateAst: 3 水準 factor 列の OrderedLogistic observe を受理する" $
      HI.validateAst [] (modelOL "grade") dm3 `shouldSatisfy` isRight'

  describe "Hanalyze.Model.HBM.Interp list 値 latent (Phase 43)" $ do
    let num d    = ELit (LNumber d)
        isRight' = either (const False) (const True)
        -- 全ノード名 (kind 問わず): combinator 内部 latent が実際に Model に
        -- 登録されたかの証明に使う。
        allNames g = sort [ HBM.nodeName n | n <- HBM.mgNodes g ]
        -- 3 水準 factor 応答列。
        dm = M.fromList
          [ ("grade",  HI.Factor ["A", "B", "C"] [0, 1, 2, 0, 1, 2])
          , ("choice", HI.Factor ["x", "y", "z"] [0, 1, 2, 0, 1, 2])
          ]
        -- orderedCuts "cut" 2 (-2) 1
        ocE = EApp (EApp (EApp (EApp (EVar "orderedCuts") (ELit (LText "cut")))
                              (num 2)) (ENeg (num 2))) (num 1)
        -- dirichlet "pi" [1, 1, 1]
        dirE = EApp (EApp (EVar "dirichlet") (ELit (LText "pi")))
                    (EList [num 1, num 1, num 1])
        normal05 = EApp (EApp (EVar "Normal") (num 0)) (num 5)
        olD = EApp (EApp (EVar "OrderedLogistic") (EVar "alpha")) (EVar "cuts")
        catD = EApp (EVar "Categorical") (EVar "probs")
        observeE nm dist col =
          EApp (EApp (EApp (EVar "observe") (ELit (LText nm))) dist) (ECol col)
        -- alpha <- Normal 0 5; cuts <- orderedCuts "cut" 2 (-2) 1;
        -- observe "y" (OrderedLogistic alpha cuts) 'grade'
        modelCuts = EDo [ DoBind "alpha" normal05
                        , DoBind "cuts" ocE
                        , DoExpr (observeE "y" olD "grade") ]
                        (EApp (EVar "pure") (num 0))
        -- probs <- dirichlet "pi" [1,1,1]; observe "z" (Categorical probs) 'choice'
        modelDir  = EDo [ DoBind "probs" dirE
                        , DoExpr (observeE "z" catD "choice") ]
                        (EApp (EVar "pure") (num 0))

    -- Phase 43.1: latent cuts つき OrderedLogistic
    it "validateAst: cuts <- orderedCuts の list 値 bind を受理する" $
      HI.validateAst [] modelCuts dm `shouldSatisfy` isRight'
    it "interpStmts: orderedCuts が Model モナドで走り内部 latent cut_d_2 が出る" $ do
      let Right stmts = HI.validateAst [] modelCuts dm
          g = HBM.buildModelGraph (HI.interpStmts [] dm stmts)
      -- orderedCuts "cut" 2 …: cut_c_1/cut_c_2 (det) + cut_d_2 (HalfNormal latent)。
      -- cut_d_2 の存在が combinator 実行の証明 (固定リテラルでは出ない)。
      ("cut_d_2" `elem` allNames g) `shouldBe` True
      ("alpha"   `elem` allNames g) `shouldBe` True
    it "validateAst: orderedCuts のカット数が非リテラル (変数) ならエラー" $ do
      let bad = EApp (EApp (EApp (EApp (EVar "orderedCuts") (ELit (LText "cut")))
                              (EVar "k")) (ENeg (num 2))) (num 1)
          m = EDo [ DoBind "cuts" bad, DoExpr (observeE "y" olD "grade") ]
                  (EApp (EVar "pure") (num 0))
      HI.validateAst [] m dm `shouldSatisfy` (not . isRight')

    -- Phase 43.2: Dirichlet prior つき Categorical
    it "validateAst: probs <- dirichlet の list 値 bind を受理する" $
      HI.validateAst [] modelDir dm `shouldSatisfy` isRight'
    it "interpStmts: dirichlet が Model モナドで走り内部 latent pi_b0/pi_b1 が出る" $ do
      let Right stmts = HI.validateAst [] modelDir dm
          g = HBM.buildModelGraph (HI.interpStmts [] dm stmts)
      -- dirichlet "pi" [1,1,1]: stick-breaking で pi_b0/pi_b1 (Beta latent) +
      -- pi_0/pi_1/pi_2 (det π)。 pi_b0 の存在が combinator 実行の証明。
      ("pi_b0" `elem` allNames g) `shouldBe` True
      ("pi_b1" `elem` allNames g) `shouldBe` True
    it "validateAst: dirichlet の α が長さ 1 ([..]) ならエラー" $ do
      let bad = EApp (EApp (EVar "dirichlet") (ELit (LText "pi"))) (EList [num 1])
          m = EDo [ DoBind "probs" bad, DoExpr (observeE "z" catD "choice") ]
                  (EApp (EVar "pure") (num 0))
      HI.validateAst [] m dm `shouldSatisfy` (not . isRight')

    -- Phase 43.3: softmax 多項ロジット
    let evalV2 e = HI.evalValue (M.empty :: HI.EnvA Double) (M.empty :: HI.DataMap) Nothing e
        smx es   = EApp (EVar "softmax") (EList es)
        nums r   = case r of Right (HI.VList xs) -> [ x | HI.VNum x <- xs ]; _ -> []
        catSmx   = EApp (EVar "Categorical") (smx [num 0, EVar "b1", EVar "b2"])
        mlModel  = EDo [ DoBind "b1" normal05, DoBind "b2" normal05
                       , DoExpr (observeE "y" catSmx "grade") ]
                       (EApp (EVar "pure") (num 0))
    it "softmax: 確率に正規化される (総和 ≈ 1)" $
      (abs (sum (nums (evalV2 (smx [num 0, num 1, num 2]))) - 1) < 1e-9) `shouldBe` True
    it "softmax: 一様入力 [0,0,0] は各クラス 1/3" $
      all (\x -> abs (x - 1/3) < 1e-9) (nums (evalV2 (smx [num 0, num 0, num 0]))) `shouldBe` True
    it "softmax: 大きな入力でも overflow しない (安定版 [1000,1000] → [0.5,0.5])" $
      nums (evalV2 (smx [num 1000, num 1000]))
        `shouldSatisfy` (\ys -> length ys == 2 && all (\y -> abs (y - 0.5) < 1e-9) ys)
    it "softmax: 空リスト ([]) はエラー" $
      isRight' (evalV2 (smx [])) `shouldBe` False
    it "validateAst: Categorical (softmax [0,b1,b2]) 多項ロジットを受理する" $
      HI.validateAst [] mlModel dm `shouldSatisfy` isRight'
    it "interpStmts: softmax 多項ロジットが Model を構築し b1/b2 が latent に出る" $ do
      let Right stmts = HI.validateAst [] mlModel dm
          g = HBM.buildModelGraph (HI.interpStmts [] dm stmts)
      (("b1" `elem` allNames g) && ("b2" `elem` allNames g)) `shouldBe` True

    -- WAIC/PPC 再評価: combinator 由来 latent vector を posterior sample から
    -- 再構築する (= NaN fallback でなく実分布が組める)。
    it "computeObsDists: orderedCuts の cuts を sample (cut_d_2) から再構築" $ do
      let Right stmts = HI.validateAst [] modelCuts dm
          -- posterior 1 sample: sampled latent のみ (alpha/beta/cut_d_2)。
          sample = M.fromList [("alpha", 0.5), ("beta", 1.0), ("cut_d_2", 2.0)]
          dists  = concat (concatMap HI.odsDists (HI.computeObsDists [] stmts dm [sample]))
          isOL d = case d of
            HBM.OrderedLogistic _ cs -> length cs == 2 && all (not . isNaN) cs
            _                        -> False
      (not (null dists) && all isOL dists) `shouldBe` True
    it "computeObsDists: dirichlet の probs を stick-breaking 再構築 (Σπ≈1)" $ do
      let Right stmts = HI.validateAst [] modelDir dm
          -- b0=0.3, b1=0.5 → π=[0.3, 0.35, 0.35]、 総和 1。
          sample = M.fromList [("pi_b0", 0.3), ("pi_b1", 0.5)]
          dists  = concat (concatMap HI.odsDists (HI.computeObsDists [] stmts dm [sample]))
          isCat d = case d of
            HBM.Categorical ps -> length ps == 3 && abs (sum ps - 1) < 1e-9
            _                  -> False
      (not (null dists) && all isCat dists) `shouldBe` True

  describe "Hanalyze.Model.HBM.Interp MvNormal multi-column observe (Phase 44)" $ do
    let num d    = ELit (LNumber d)
        isRight' = either (const False) (const True)
        allNames g = sort [ HBM.nodeName n | n <- HBM.mgNodes g ]
        -- 2 列の連続観測 (y1, y2)。 行長は 4 で揃える。
        dm = M.fromList
          [ ("y1", HI.Numeric [1.0, 2.0, 3.0, 4.0])
          , ("y2", HI.Numeric [1.5, 2.5, 2.0, 3.5]) ]
        emptyEnv = M.empty :: HI.EnvA Double
        evalD e  = HI.evalDist emptyEnv dm Nothing e :: Err (HBM.Distribution Double)
        normal10 = EApp (EApp (EVar "Normal") (num 0)) (num 10)
        -- MvNormal [mu1, mu2] [[1, 0.5], [0.5, 1]]
        mvE muRow = EApp (EApp (EVar "MvNormal") muRow)
                         (EList [ EList [num 1, num 0.5], EList [num 0.5, num 1] ])
        mvLit = mvE (EList [num 0, num 0])
        -- observeMV "y" (MvNormal …) ['y1', 'y2']
        observeMvE dist colE = EApp (EApp (EApp (EVar "observeMV") (ELit (LText "y"))) dist) colE
        cols2 = EList [ECol "y1", ECol "y2"]
        -- latent μ + 固定 cov の最短モデル
        mvModel = EDo [ DoBind "mu1" normal10, DoBind "mu2" normal10
                      , DoExpr (observeMvE (mvE (EList [EVar "mu1", EVar "mu2"])) cols2) ]
                      (EApp (EVar "pure") (num 0))

    -- Phase 44.2: evalDist MvNormal
    it "evalDist: MvNormal [mu] [[cov]] を構築する" $
      show (evalD mvLit)
        `shouldBe` show (Right (HBM.MvNormal [0, 0] [[1, 0.5], [0.5, 1]])
                          :: Err (HBM.Distribution Double))
    it "evalDist: 非正方 cov はエラー (2×3)" $
      evalD (EApp (EApp (EVar "MvNormal") (EList [num 0, num 0]))
                  (EList [ EList [num 1, num 0, num 0], EList [num 0, num 1, num 0] ]))
        `shouldSatisfy` (not . isRight')
    it "evalDist: μ 長と cov 次元の不一致はエラー (μ=3, cov=2×2)" $
      evalD (mvE (EList [num 0, num 0, num 0])) `shouldSatisfy` (not . isRight')

    -- Phase 44.1: observeMV 機構
    it "asMatrix: VList-of-VList を [[a]] に落とす + 行長不一致はエラー" $ do
      isRight' (HI.asMatrix (HI.VList [ HI.VList [HI.VNum (1::Double), HI.VNum 2]
                                      , HI.VList [HI.VNum 3, HI.VNum 4] ])) `shouldBe` True
      isRight' (HI.asMatrix (HI.VList [ HI.VList [HI.VNum (1::Double)]
                                      , HI.VList [HI.VNum 3, HI.VNum 4] ])) `shouldBe` False
    it "validateAst: latent μ + 固定 cov の observeMV を受理する" $
      HI.validateAst [] mvModel dm `shouldSatisfy` isRight'
    it "interpStmts: observeMV が Model を構築し latent mu1/mu2 が出る" $ do
      let Right stmts = HI.validateAst [] mvModel dm
          g = HBM.buildModelGraph (HI.interpStmts [] dm stmts)
      (("mu1" `elem` allNames g) && ("mu2" `elem` allNames g)) `shouldBe` True
    it "validateAst: observeMV の観測列が 1 列ならエラー (scalar observe を使うべき)" $ do
      let m = EDo [ DoBind "mu1" normal10
                  , DoExpr (observeMvE (mvE (EList [EVar "mu1"])) (EList [ECol "y1"])) ]
                  (EApp (EVar "pure") (num 0))
      HI.validateAst [] m dm `shouldSatisfy` (not . isRight')
    it "validateAst: observeMV の観測列が欠落しているとエラー" $ do
      let m = EDo [ DoBind "mu1" normal10, DoBind "mu2" normal10
                  , DoExpr (observeMvE (mvE (EList [EVar "mu1", EVar "mu2"]))
                                       (EList [ECol "y1", ECol "nope"])) ]
                  (EApp (EVar "pure") (num 0))
      HI.validateAst [] m dm `shouldSatisfy` (not . isRight')
    it "validateAst: observeMV に scalar 分布 (Normal) を渡すと親切エラー" $ do
      let m = EDo [ DoBind "mu1" normal10
                  , DoExpr (observeMvE normal10 cols2) ]
                  (EApp (EVar "pure") (num 0))
      HI.validateAst [] m dm `shouldSatisfy` (not . isRight')

    -- Phase 44.3: MvNormalChol 分布 + lkjCorrCholesky 行列値 bind (latent Σ)
    let halfN1   = EApp (EVar "HalfNormal") (num 1)
        -- lkjCorrCholesky "L" 2 2.0
        lkjE k   = EApp (EApp (EApp (EVar "lkjCorrCholesky") (ELit (LText "L")))
                              (num (fromIntegral (k :: Int)))) (num 2.0)
        -- MvNormalChol [mu1,mu2] [s1,s2] L
        mvcE muRow sigRow lRef =
          EApp (EApp (EApp (EVar "MvNormalChol") muRow) sigRow) lRef
        -- L <- lkjCorrCholesky "L" 2 2.0; s1/s2 <- HalfNormal 1;
        -- mu1/mu2 <- Normal 0 10; observeMV "y" (MvNormalChol [mu1,mu2] [s1,s2] L) ['y1','y2']
        mvcModel = EDo [ DoBind "L"   (lkjE 2)
                       , DoBind "s1"  halfN1, DoBind "s2"  halfN1
                       , DoBind "mu1" normal10, DoBind "mu2" normal10
                       , DoExpr (observeMvE
                           (mvcE (EList [EVar "mu1", EVar "mu2"])
                                 (EList [EVar "s1", EVar "s2"]) (EVar "L")) cols2) ]
                       (EApp (EVar "pure") (num 0))
    -- 相関 Cholesky L (ρ=0.5) + σ=[2,3] → M=diag σ·L、 Σ=M Mᵀ=[[4,3],[3,9]]。
    it "mvNormalCholLogDensity: full-Σ 版 (mvNormalLogDensity) と Σ=M Mᵀ で数値一致" $ do
      let lCorr = [[1, 0], [0.5, sqrt 0.75]] :: [[Double]]
          sig   = [2, 3] :: [Double]
          mu    = [0, 0] :: [Double]
          y     = [1, 2] :: [Double]
          cov   = [[4, 3], [3, 9]] :: [[Double]]   -- = (diag σ·L)(diag σ·L)ᵀ
          a = HBM.mvNormalCholLogDensity mu sig lCorr y
          b = HBM.mvNormalLogDensity mu cov y
      abs (a - b) < 1e-9 `shouldBe` True
    it "evalDist: MvNormalChol [mu] [sigma] [[L]] を構築する" $
      show (evalD (mvcE (EList [num 0, num 0]) (EList [num 1, num 1])
                        (EList [EList [num 1, num 0], EList [num 0, num 1]])))
        `shouldBe` show (Right (HBM.MvNormalChol [0,0] [1,1] [[1,0],[0,1]])
                          :: Err (HBM.Distribution Double))
    it "validateAst: lkjCorrCholesky bind + MvNormalChol observe を受理する" $
      HI.validateAst [] mvcModel dm `shouldSatisfy` isRight'
    it "interpStmts: lkjCorrCholesky が Model で走り内部 latent L_u1_0 が出る" $ do
      let Right stmts = HI.validateAst [] mvcModel dm
          g = HBM.buildModelGraph (HI.interpStmts [] dm stmts)
      -- lkjCorrCholesky "L" 2 …: L_u1_0 (Beta latent) + L_pc1_0 (det)。
      -- L_u1_0 の存在が combinator 実行の証明 (固定リテラルでは出ない)。
      (("L_u1_0" `elem` allNames g) && ("mu1" `elem` allNames g)) `shouldBe` True
    it "validateAst: lkjCorrCholesky の次元が 1 (k<2) ならエラー" $ do
      let m = EDo [ DoBind "L" (lkjE 1)
                  , DoBind "mu1" normal10, DoBind "mu2" normal10
                  , DoExpr (observeMvE
                      (mvcE (EList [EVar "mu1", EVar "mu2"])
                            (EList [num 1, num 1]) (EVar "L")) cols2) ]
                  (EApp (EVar "pure") (num 0))
      HI.validateAst [] m dm `shouldSatisfy` (not . isRight')
    it "evalDist: MvNormalChol の σ 長と μ 長の不一致はエラー" $
      evalD (mvcE (EList [num 0, num 0]) (EList [num 1])
                  (EList [EList [num 1, num 0], EList [num 0, num 1]]))
        `shouldSatisfy` (not . isRight')

    -- Phase 44.4: WAIC/PPC 再評価 (observeMV 専用並行経路)
    let finiteAll = all (all (\x -> not (isNaN x || isInfinite x)))
    it "reconstructMatrixComb: L_u1_0=0.5 (pc=0) から単位 Cholesky を再構築" $ do
      let sm = M.fromList [("L_u1_0", 0.5 :: Double)]   -- pc = 2*0.5-1 = 0
          Just l = HI.reconstructMatrixComb sm (HI.LkjCholSpec "L" 2)
      (length l == 2 && abs ((l !! 0 !! 0) - 1) < 1e-9 && abs (l !! 1 !! 0) < 1e-9
        && abs ((l !! 1 !! 1) - 1) < 1e-9) `shouldBe` True
    it "reconstructMatrixComb: pc=0.5 → L=[[1,0],[0.5,√0.75]] (相関 0.5)" $ do
      let sm = M.fromList [("L_u1_0", 0.75 :: Double)]  -- pc = 2*0.75-1 = 0.5
          Just l = HI.reconstructMatrixComb sm (HI.LkjCholSpec "L" 2)
      (abs (l !! 1 !! 0 - 0.5) < 1e-9 && abs (l !! 1 !! 1 - sqrt 0.75) < 1e-9) `shouldBe` True
    it "computeMvObsDists: literal cov observeMV を sample から MvNormal に再評価" $ do
      let Right stmts = HI.validateAst [] mvModel dm
          sample = M.fromList [("mu1", 0.0), ("mu2", 0.0)]
          sets = HI.computeMvObsDists [] stmts dm [sample]
          isMv d = case d of HBM.MvNormal mu _ -> length mu == 2; _ -> False
      (not (null sets) && all isMv (concat (HI.mvodsDists (head sets)))) `shouldBe` True
    it "pointwiseLogLikMv: literal cov の log-lik が非空・有限" $ do
      let Right stmts = HI.validateAst [] mvModel dm
          sample = M.fromList [("mu1", 0.0), ("mu2", 0.0)]
          ll = HI.pointwiseLogLikMv (HI.computeMvObsDists [] stmts dm [sample])
      (not (null ll) && not (null (head ll)) && finiteAll ll) `shouldBe` True
    it "computeMvObsDists+LogLik: latent Σ を L_u1_0 から MvNormalChol に再評価 (有限)" $ do
      let Right stmts = HI.validateAst [] mvcModel dm
          sample = M.fromList [ ("mu1", 0.0), ("mu2", 0.0), ("s1", 1.0), ("s2", 1.0)
                              , ("L_u1_0", 0.75) ]
          sets = HI.computeMvObsDists [] stmts dm [sample]
          isChol d = case d of HBM.MvNormalChol mu _ _ -> length mu == 2; _ -> False
          ll = HI.pointwiseLogLikMv sets
      (not (null sets) && all isChol (concat (HI.mvodsDists (head sets)))
        && finiteAll ll) `shouldBe` True

  describe "Hanalyze.Model.HBM.Interp Mixture 分布リスト引数 (Phase 45)" $ do
    let num d    = ELit (LNumber d)
        isRight' = either (const False) (const True)
        allNames g = sort [ HBM.nodeName n | n <- HBM.mgNodes g ]
        -- 二峰性の連続観測列 (混合の主用途)。
        dm = M.fromList [("value", HI.Numeric [1.0, 1.5, 8.0, 9.0])]
        emptyEnv = M.empty :: HI.EnvA Double
        evalD e  = HI.evalDist emptyEnv dm Nothing e :: Err (HBM.Distribution Double)
        normalE m s = EApp (EApp (EVar "Normal") m) s
        catE probs  = EApp (EVar "Categorical") (EList probs)
        -- Mixture weights comps
        mixE w c = EApp (EApp (EVar "Mixture") w) c
        -- Mixture [0.3, 0.7] [Normal 0 1, Normal 5 1]
        mixLit = mixE (EList [num 0.3, num 0.7])
                      (EList [normalE (num 0) (num 1), normalE (num 5) (num 1)])

    -- Phase 45.1: evalDist Mixture 節 (literal weights + 再帰 evalDist)
    it "evalDist: Mixture [w1,w2] [Normal,Normal] を構築する" $
      show (evalD mixLit)
        `shouldBe` show (Right (HBM.Mixture [0.3, 0.7] [HBM.Normal 0 1, HBM.Normal 5 1])
                          :: Err (HBM.Distribution Double))
    it "evalDist: 第 2 引数の各成分を再帰 evalDist する (Categorical 成分のネスト)" $
      show (evalD (mixE (EList [num 0.5, num 0.5])
                        (EList [catE [num 0.2, num 0.8], catE [num 0.6, num 0.4]])))
        `shouldBe` show (Right (HBM.Mixture [0.5, 0.5]
                                 [HBM.Categorical [0.2, 0.8], HBM.Categorical [0.6, 0.4]])
                          :: Err (HBM.Distribution Double))
    it "evalDist: 成分分布が空 (Mixture [] []) はエラー" $
      evalD (mixE (EList []) (EList [])) `shouldSatisfy` (not . isRight')
    it "evalDist: 重み数 ≠ 成分数 (2 vs 1) はエラー" $
      evalD (mixE (EList [num 0.3, num 0.7])
                  (EList [normalE (num 0) (num 1)])) `shouldSatisfy` (not . isRight')
    it "logDensity: 構築した Mixture は有限な対数密度を返す" $ do
      let Right d = evalD mixLit
          ld = HBM.logDensity d (4.0 :: Double)
      (not (isNaN ld || isInfinite ld)) `shouldBe` True

    -- literal weights + latent component mean の最短モデル
    let normal010 = normalE (num 0) (num 10)
        halfN2    = EApp (EVar "HalfNormal") (num 2)
        observeE nm dist col =
          EApp (EApp (EApp (EVar "observe") (ELit (LText nm))) dist) (ECol col)
        -- Mixture [0.3,0.7] [Normal mu1 sigma, Normal mu2 sigma]
        mixD = mixE (EList [num 0.3, num 0.7])
                    (EList [ normalE (EVar "mu1") (EVar "sigma")
                           , normalE (EVar "mu2") (EVar "sigma") ])
        -- mu1/mu2 <- Normal 0 10; sigma <- HalfNormal 2;
        -- observe "y" (Mixture …) 'value'
        mixModel = EDo [ DoBind "mu1" normal010, DoBind "mu2" normal010
                       , DoBind "sigma" halfN2
                       , DoExpr (observeE "y" mixD "value") ]
                       (EApp (EVar "pure") (num 0))
    it "validateAst: literal weights Mixture observe を受理する" $
      HI.validateAst [] mixModel dm `shouldSatisfy` isRight'
    it "interpStmts: Mixture observe が Model を構築し latent mu1/mu2 が出る" $ do
      let Right stmts = HI.validateAst [] mixModel dm
          g = HBM.buildModelGraph (HI.interpStmts [] dm stmts)
      (("mu1" `elem` allNames g) && ("mu2" `elem` allNames g)) `shouldBe` True
    it "computeObsDists+LogLik: literal weights Mixture を sample から再評価 (有限)" $ do
      let Right stmts = HI.validateAst [] mixModel dm
          sample = M.fromList [("mu1", 1.0), ("mu2", 8.0), ("sigma", 1.0)]
          sets = HI.computeObsDists [] stmts dm [sample]
          ll = HI.pointwiseLogLik sets
      (not (null sets) && not (null ll) && not (null (head ll))
        && all (all (\x -> not (isNaN x || isInfinite x))) ll) `shouldBe` True

    -- Phase 45.2: Dirichlet latent weights 経路 (Phase 43 dirichlet bind を流用)。
    -- pi <- dirichlet "pi" [1,1]; mu1/mu2 <- Normal 0 10; sigma <- HalfNormal 2;
    -- observe "y" (Mixture pi [Normal mu1 sigma, Normal mu2 sigma]) 'value'
    let dirE = EApp (EApp (EVar "dirichlet") (ELit (LText "pi"))) (EList [num 1, num 1])
        mixDirD = mixE (EVar "pi")
                       (EList [ normalE (EVar "mu1") (EVar "sigma")
                              , normalE (EVar "mu2") (EVar "sigma") ])
        mixDirModel = EDo [ DoBind "pi" dirE
                          , DoBind "mu1" normal010, DoBind "mu2" normal010
                          , DoBind "sigma" halfN2
                          , DoExpr (observeE "y" mixDirD "value") ]
                          (EApp (EVar "pure") (num 0))
    it "validateAst: Dirichlet latent 重み Mixture observe を受理する" $
      HI.validateAst [] mixDirModel dm `shouldSatisfy` isRight'
    it "interpStmts: dirichlet が走り内部 latent pi_b0 + mu1/mu2 が出る" $ do
      let Right stmts = HI.validateAst [] mixDirModel dm
          g = HBM.buildModelGraph (HI.interpStmts [] dm stmts)
      (("pi_b0" `elem` allNames g) && ("mu1" `elem` allNames g)) `shouldBe` True
    it "computeObsDists+LogLik: 重み pi を pi_b0 から再構築し Mixture 再評価 (有限)" $ do
      -- combinator 内部 latent pi_b0 から reconstructComb が π を復元し、
      -- Mixture の重みとして解決される (新規再構築コード不要 = scalar 経路流用)。
      let Right stmts = HI.validateAst [] mixDirModel dm
          sample = M.fromList [("pi_b0", 0.5), ("mu1", 1.0), ("mu2", 8.0), ("sigma", 1.0)]
          sets = HI.computeObsDists [] stmts dm [sample]
          ll = HI.pointwiseLogLik sets
          isMix d = case d of HBM.Mixture ws _ -> length ws == 2; _ -> False
      (not (null sets) && all isMix (concat (HI.odsDists (head sets)))
        && not (null ll) && all (all (\x -> not (isNaN x || isInfinite x))) ll) `shouldBe` True

-- ============================================================================
-- Hanalyze.Model.Formula (Phase 46/15 §3.6 A15) — parser / AST / round-trip
-- ============================================================================
