{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Viz.ModelGraphSpec (spec) where

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
import qualified Hanalyze.Viz.ModelGraph    as VMG
import qualified Hanalyze.Viz.ModelGraphDot as VMGD
import qualified Hanalyze.Model.HBM as HBM
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Viz.ModelGraph + ModelGraphDot (Phase 40-A3)" $ do
    let m :: HBM.ModelP ()
        m = do
          mu <- HBM.sample "mu" (HBM.Normal 0 5)
          _ <- HBM.plate "g" 4 $ forM [0..3 :: Int] $ \j ->
            HBM.sample ("x_" <> T.pack (show j)) (HBM.Normal mu 1)
          return ()
    --
    -- Mermaid plate 出力
    --
    it "buildMermaid: plate 内ノードが subgraph で囲まれる" $ do
      let g = HBM.buildModelGraph m
          src = VMG.buildMermaid g
      T.isInfixOf "subgraph plate_g[\"g × 4\"]" src `shouldBe` True
      T.isInfixOf "end" src `shouldBe` True
    it "buildMermaid: plate 外ノード mu は subgraph の外" $ do
      let g = HBM.buildModelGraph m
          src = VMG.buildMermaid g
          -- "subgraph plate_g" の前に mu が現れるか後か
          beforeSub = T.takeWhile (\_ -> True) (T.takeWhile (/= 's') src)
              -- 大雑把: mu は src の前半に出現
          muIdx = T.length (fst (T.breakOn "mu" src))
          subIdx = T.length (fst (T.breakOn "subgraph plate_g" src))
      (muIdx < subIdx) `shouldBe` True
      -- beforeSub だけ使う体裁を維持
      T.length beforeSub `shouldSatisfy` (>= 0)
    --
    -- Graphviz DOT 出力
    --
    it "renderModelGraphDot: 出力が digraph G { で始まり } で終わる" $ do
      let g = HBM.buildModelGraph m
          src = VMGD.renderModelGraphDot g
      T.isPrefixOf "digraph G {" src `shouldBe` True
      T.isSuffixOf "}\n" src `shouldBe` True
    it "renderModelGraphDot: plate が cluster_g { label=\"g × 4\" } で囲まれる" $ do
      let g = HBM.buildModelGraph m
          src = VMGD.renderModelGraphDot g
      T.isInfixOf "subgraph cluster_g {" src `shouldBe` True
      T.isInfixOf "label=\"g × 4\";" src `shouldBe` True
      T.isInfixOf "labelloc=\"b\";" src `shouldBe` True
    it "renderModelGraphDot: edge は arrow (->) で出力" $ do
      let g = HBM.buildModelGraph m
          src = VMGD.renderModelGraphDot g
      T.isInfixOf "mu -> x_0;" src `shouldBe` True
    --
    -- DeterministicN ノード (Phase 59.2 回帰: mkNodeLine が LatentN/ObservedN
    -- のみで non-exhaustive crash していた。 request/255 §1 の最小再現)
    --
    it "renderModelGraphDot: deterministic ノードで crash せず box 形状で出力" $ do
      let dm :: HBM.ModelP ()
          dm = do
            a  <- HBM.sample "a" (HBM.Normal 0 1)
            mu <- HBM.deterministic "mu" (2 * a)
            HBM.observe "y" (HBM.Normal mu 1) [0.5]
      let g = HBM.buildModelGraph dm
          src = VMGD.renderModelGraphDot g
      T.isInfixOf "mu [label=\"mu\\nDeterministic\", shape=box];" src `shouldBe` True
      T.isInfixOf "a -> mu;" src `shouldBe` True
      T.isInfixOf "mu -> y;" src `shouldBe` True
    --
    -- Phase 60.4: データ slot (dataNamed/dataNamedIx) の DAG ノード化
    -- (pm.Data 相当・既定 ON)。 label/attrs 両 case + 回帰 test を同時に
    -- (Phase 59.2 の non-exhaustive crash の教訓)。
    --
    it "renderModelGraphDot: data slot が角丸灰色 box + x→mu エッジ (60.4)" $ do
      let xm :: HBM.ModelP ()
          xm = do
            xs  <- HBM.dataNamed    "x" [1, 2, 3]
            ys  <- HBM.dataNamedObs "y" [2, 4, 6]
            _gs <- HBM.dataNamedIx  "g" [0, 1, 0]
            b   <- HBM.sample "b" (HBM.Normal 0 1)
            mu  <- HBM.deterministic "mu" (b * head xs)
            HBM.observe "y" (HBM.Normal mu 1) ys
      let g = HBM.buildModelGraph xm
          src = VMGD.renderModelGraphDot g
      -- ノード種: x/g は DataN、 y は observe に吸収され ObservedN
      -- (dataNamedObs "y" + observe "y" の docs 慣例・PyMC で observed RV が
      -- data 容器を内包するのと同型)
      [ HBM.nodeKind n | n <- HBM.mgNodes g, HBM.nodeName n == "x" ]
        `shouldBe` [HBM.DataN 3]
      [ HBM.nodeKind n | n <- HBM.mgNodes g, HBM.nodeName n == "g" ]
        `shouldBe` [HBM.DataN 3]
      [ HBM.nodeKind n | n <- HBM.mgNodes g, HBM.nodeName n == "y" ]
        `shouldBe` [HBM.ObservedN 3]
      -- エッジ: x→mu (dataNamed の dep タグ)・mu→y。 g ([Int]) はエッジなし
      HBM.mgEdges g `shouldSatisfy` (("x", "mu") `elem`)
      HBM.mgEdges g `shouldSatisfy` (("mu", "y") `elem`)
      [ e | e@(f, _) <- HBM.mgEdges g, f == "g" ] `shouldBe` []
      -- DOT: 角丸灰色 box (PyMC ConstantData 流)・crash しない
      T.isInfixOf "x [label=\"x\\n(n=3)\", shape=box, style=\"rounded,filled\", fillcolor=lightgray];" src
        `shouldBe` True
      T.isInfixOf "x -> mu;" src `shouldBe` True

    it "dataNamedIx + (!!!) で slot→利用先エッジが出る (60.7)" $ do
      let im :: HBM.ModelP ()
          im = do
            gs <- HBM.dataNamedIx  "g" [0, 1, 0]
            ys <- HBM.dataNamedObs "yv" [1, 2, 3]
            m0 <- HBM.sample "m0" (HBM.Normal 0 5)
            m1 <- HBM.sample "m1" (HBM.Normal 0 5)
            s  <- HBM.sample "s" (HBM.HalfNormal 1)
            let ms = [m0, m1]
            HBM.plateForM_ "obs" (zip gs ys) $ \(g, y) -> do
              mu <- HBM.deterministic "mu" (ms HBM.!!! g)
              HBM.observe "y" (HBM.Normal mu s) [y]
      let g = HBM.buildModelGraph im
      -- (!!!) の Track 解釈で g→mu エッジ (PyMC の b0[gid] 同型)
      HBM.mgEdges g `shouldSatisfy` (("g", "mu") `elem`)
      -- index 由来でない親 (m0/m1) も従来通り
      HBM.mgEdges g `shouldSatisfy` (("m0", "mu") `elem`)

    -- Phase 62: REff 経路 (atIx = ObserveLM 構造化ブロック) でも slot→観測
    -- ノードのエッジが出る (60.7 では「既知の制限」 だった残り半分)。
    it "dataNamedIx + reNormal/atIx + observeLMR で slot→obs エッジが出る (62)" $ do
      let rm :: HBM.ModelP ()
          rm = do
            gids <- HBM.dataNamedIx "g" [0, 1, 0, 1]
            a    <- HBM.sample "a" (HBM.Normal 0 5)
            tau  <- HBM.sample "tau_u" (HBM.HalfNormal 1)
            u    <- HBM.reNormal "u" 2 "tau_u" tau
            _    <- pure a
            HBM.observeNormalLM "y" [[1], [1], [1], [1]] ["a"]
              [u `HBM.atIx` gids] "sigma" [1, 2, 3, 4]
          rg = HBM.buildModelGraph rm
      -- slot g → 観測ブロック y のエッジ (lmParents が slot 名を運ぶ)
      HBM.mgEdges rg `shouldSatisfy` (("g", "y") `elem`)
      -- 従来の親 (β=a / u_j / σ 名は sample が無くとも親集合に入る) も不変
      HBM.mgEdges rg `shouldSatisfy` (("a", "y") `elem`)
      HBM.mgEdges rg `shouldSatisfy` (("u_0", "y") `elem`)

    it "at ([Int] 経路) は従来どおり slot エッジなし (62 非影響確認)" $ do
      let rm :: HBM.ModelP ()
          rm = do
            a   <- HBM.sample "a" (HBM.Normal 0 5)
            tau <- HBM.sample "tau_u" (HBM.HalfNormal 1)
            u   <- HBM.reNormal "u" 2 "tau_u" tau
            _   <- pure a
            HBM.observeNormalLM "y" [[1], [1], [1], [1]] ["a"]
              [u `HBM.at` [0, 1, 0, 1]] "sigma" [1, 2, 3, 4]
          rg = HBM.buildModelGraph rm
      [ e | e@(f, _) <- HBM.mgEdges rg, f == "g" ] `shouldBe` []
      HBM.mgEdges rg `shouldSatisfy` (("u_1", "y") `elem`)

    -- Phase 63.1: dataNamedObs の slot は observe の生 ys と値一致で
    -- obs→slot エッジが張られ obs の子になる (PyMC make_compute_graph の
    -- obs→y 同型。 従来はエッジゼロで source rank に浮遊し x と被っていた)。
    it "dataNamedObs slot に obs→slot エッジが出る (63.1・構造化 observe)" $ do
      let om :: HBM.ModelP ()
          om = do
            xs <- HBM.dataNamed    "x"  [1, 2, 3]
            ys <- HBM.dataNamedObs "yv" [2, 4, 6]
            b  <- HBM.sample "b" (HBM.Normal 0 1)
            mu <- HBM.deterministic "mu" (b * head xs)
            HBM.observe "y" (HBM.Normal mu 1) ys
      let g = HBM.buildModelGraph om
      -- obs→slot (y が親・yv が子 = yv は y の下に描かれる)
      HBM.mgEdges g `shouldSatisfy` (("y", "yv") `elem`)
      -- x slot は従来どおり x→mu のみ (値が異なるので obs からのエッジなし)
      HBM.mgEdges g `shouldSatisfy` (("x", "mu") `elem`)
      [ e | e@(_, t) <- HBM.mgEdges g, t == "x" ] `shouldBe` []

    it "per-point loop の observe も連結 ys で slot match する (63.1)" $ do
      let lm :: HBM.ModelP ()
          lm = do
            xs <- HBM.dataNamed    "x"  [1, 2, 3]
            ys <- HBM.dataNamedObs "yv" [2, 4, 6]
            b  <- HBM.sample "b" (HBM.Normal 0 1)
            s  <- HBM.sample "s" (HBM.HalfNormal 1)
            HBM.plateForM_ "obs" (zip xs ys) $ \(x, y) -> do
              mu <- HBM.deterministic "mu" (b * x)
              HBM.observe "y" (HBM.Normal mu s) [y]
      let g = HBM.buildModelGraph lm
      HBM.mgEdges g `shouldSatisfy` (("y", "yv") `elem`)

    it "dataNamedObs と observe が同名なら自己ループを張らない (63.1)" $ do
      let sm :: HBM.ModelP ()
          sm = do
            ys <- HBM.dataNamedObs "y" [2, 4, 6]
            mu <- HBM.sample "mu" (HBM.Normal 0 1)
            HBM.observe "y" (HBM.Normal mu 1) ys
      let g = HBM.buildModelGraph sm
      [ e | e@(f, t) <- HBM.mgEdges g, f == t ] `shouldBe` []

    it "data slot がデータ長 = plate サイズの一意 match で plate に入る (60.6 追補)" $ do
      let pm :: HBM.ModelP ()
          pm = do
            xs <- HBM.dataNamed    "x" [1, 2, 3]
            ys <- HBM.dataNamedObs "yv" [2, 4, 6]
            b  <- HBM.sample "b" (HBM.Normal 0 1)
            s  <- HBM.sample "s" (HBM.HalfNormal 1)
            HBM.plateForM_ "obs" (zip xs ys) $ \(x, y) -> do
              mu <- HBM.deterministic "mu" (b * x)
              HBM.observe "y" (HBM.Normal mu s) [y]
      let g = HBM.buildModelGraph pm
          platesOf nm = [ HBM.nodePlates n | n <- HBM.mgNodes g
                                           , HBM.nodeName n == nm ]
      -- 宣言は plate 外だが n=3 = plate "obs" (3) の一意 match → obs 内
      platesOf "x"  `shouldBe` [["obs"]]
      platesOf "yv" `shouldBe` [["obs"]]
      -- latent は従来通り plate 外
      platesOf "b" `shouldBe` [[]]

    it "data 長と一致する plate が複数なら据え置き (60.6 追補・曖昧 match)" $ do
      let am :: HBM.ModelP ()
          am = do
            xs <- HBM.dataNamed "x" [1, 2]
            b  <- HBM.sample "b" (HBM.Normal 0 1)
            _  <- HBM.plateForM "p1" [0, 1 :: Int] $ \j ->
                    HBM.sample ("a" HBM..# j) (HBM.Normal b 1)
            _  <- HBM.plateForM "p2" [0, 1 :: Int] $ \j ->
                    HBM.sample ("c" HBM..# j) (HBM.Normal b 1)
            HBM.observe "y" (HBM.Normal (head xs) 1) [0.5]
      let g = HBM.buildModelGraph am
      [ HBM.nodePlates n | n <- HBM.mgNodes g, HBM.nodeName n == "x" ]
        `shouldBe` [[]]

    it "describeModel: data slot が [data] 行で出る (60.4)" $ do
      let xm :: HBM.ModelP ()
          xm = do
            xs <- HBM.dataNamed "x" [1, 2, 3]
            b  <- HBM.sample "b" (HBM.Normal 0 1)
            HBM.observe "y" (HBM.Normal (b * head xs) 1) [0.5]
      T.isInfixOf "[data]" (HBM.describeModel xm) `shouldBe` True

    --
    -- Nested plate: cluster の入れ子
    --
    it "renderModelGraphDot: nested plate で cluster が入れ子" $ do
      let nm :: HBM.ModelP ()
          nm = do
            _ <- HBM.plate "school" 2 $ forM_ [0..1 :: Int] $ \j ->
                   HBM.plate "student" 2 $ forM_ [0..1 :: Int] $ \i ->
                     HBM.sample ("y_" <> T.pack (show j) <> "_" <> T.pack (show i))
                                (HBM.Normal 0 1)
            return ()
      let g = HBM.buildModelGraph nm
          src = VMGD.renderModelGraphDot g
          -- "school" cluster の中に "student" cluster
          schoolIdx = T.length (fst (T.breakOn "cluster_school" src))
          studentIdx = T.length (fst (T.breakOn "cluster_student" src))
      (schoolIdx < studentIdx) `shouldBe` True
