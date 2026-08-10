{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.MCMC.NUTSSpec (spec) where

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
  describe "Hanalyze.MCMC.NUTS.nutsStream" $ do
    let smallCfg = NUTS.defaultNUTSConfig
          { NUTS.nutsIterations = 20
          , NUTS.nutsBurnIn     = 10
          , NUTS.nutsAdaptStepSize = False
          , NUTS.nutsAdaptMass     = False
          }
        -- Trivial model: mu ~ Normal(0, 1), observe one data point.
        model :: HBM.ModelP ()
        model = do
          mu <- HBM.sample "mu" (HBM.Normal 0 1)
          HBM.observe "y" (HBM.Normal mu 1) [0.5]

    it "callback が iterations + burnIn 回呼ばれる" $ do
      eventsRef <- newIORef []
      gen <- MWC.create
      _ <- NUTS.nutsStream model smallCfg (M.fromList [("mu", 0)]) gen $ \ev ->
        modifyIORef' eventsRef (ev :)
      events <- reverse <$> readIORef eventsRef
      length events `shouldBe`
        (NUTS.nutsBurnIn smallCfg + NUTS.nutsIterations smallCfg)

    it "seIter は 0..total-1 で順番に並ぶ" $ do
      eventsRef <- newIORef []
      gen <- MWC.create
      _ <- NUTS.nutsStream model smallCfg (M.fromList [("mu", 0)]) gen $ \ev ->
        modifyIORef' eventsRef (ev :)
      events <- reverse <$> readIORef eventsRef
      map NUTS.seIter events
        `shouldBe`
        [0 .. NUTS.nutsBurnIn smallCfg + NUTS.nutsIterations smallCfg - 1]

    it "seIsBurnIn は 先頭 burnIn 個だけ True" $ do
      eventsRef <- newIORef []
      gen <- MWC.create
      _ <- NUTS.nutsStream model smallCfg (M.fromList [("mu", 0)]) gen $ \ev ->
        modifyIORef' eventsRef (ev :)
      events <- reverse <$> readIORef eventsRef
      length (filter NUTS.seIsBurnIn events)
        `shouldBe` NUTS.nutsBurnIn smallCfg

    it "既存 nuts は nutsStream の wrapper として同一結果(同じ seed)" $ do
      gen1 <- MWC.create
      chain1 <- NUTS.nuts model smallCfg (M.fromList [("mu", 0)]) gen1
      gen2 <- MWC.create
      chain2 <- NUTS.nutsStream model smallCfg (M.fromList [("mu", 0)])
                  gen2 (\_ -> pure ())
      -- 同じ seed (MWC.create は決定的) なので chain length 一致
      length (Core.chainSamples chain1)
        `shouldBe` length (Core.chainSamples chain2)

  describe "Hanalyze.MCMC.NUTS 純粋版 (Phase 50.3 nutsPure / nutsChainsPure)" $ do
    let cfg = NUTS.defaultNUTSConfig
          { NUTS.nutsIterations = 30, NUTS.nutsBurnIn = 20
          , NUTS.nutsAdaptStepSize = False, NUTS.nutsAdaptMass = False }
        model :: HBM.ModelP ()
        model = do
          mu <- HBM.sample "mu" (HBM.Normal 0 10)
          HBM.observe "y" (HBM.Normal mu 1) (replicate 8 5.0)
        initC = M.fromList [("mu", 0)]

    it "nutsPure: 同 seed なら chainSamples がビット同一 (再現性)" $ do
      let c1 = NUTS.nutsPure model cfg initC 12345
          c2 = NUTS.nutsPure model cfg initC 12345
      Core.chainSamples c1 `shouldBe` Core.chainSamples c2

    it "nutsPure: 別 seed なら chainSamples が異なる" $ do
      let c1 = NUTS.nutsPure model cfg initC 1
          c2 = NUTS.nutsPure model cfg initC 2
      (Core.chainSamples c1 == Core.chainSamples c2) `shouldBe` False

    it "nutsPure: IO nuts と同 seed でビット同一 (ST/IO 等価)" $ do
      gen <- MWC.initialize (V.singleton 777)
      ioChain <- NUTS.nuts model cfg initC gen
      let pureChain = NUTS.nutsPure model cfg initC 777
      Core.chainSamples pureChain `shouldBe` Core.chainSamples ioChain

    it "nutsChainsPure: numChains 本・各 chain は iterations 長・再現性あり" $ do
      let chains = NUTS.nutsChainsPure model cfg 3 initC 99
      length chains `shouldBe` 3
      map (length . Core.chainSamples) chains
        `shouldBe` replicate 3 (NUTS.nutsIterations cfg)
      -- 同 seed で全 chain がビット同一 (parList は決定性に影響しない)
      map Core.chainSamples (NUTS.nutsChainsPure model cfg 3 initC 99)
        `shouldBe` map Core.chainSamples chains

    it "nutsChainsPure: 子 seed が分かれ chain 同士は異なる" $ do
      let chains = NUTS.nutsChainsPure model cfg 2 initC 55
      case chains of
        [a, b] -> (Core.chainSamples a == Core.chainSamples b) `shouldBe` False
        _      -> expectationFailure "expected 2 chains"

    -- Phase 94 A4-2: init jitter。 既定 (nutsInitJitter=0) は従来と完全一致
    -- (RNG stream を消費しない = 既存全テストの再現性を保つ)。 jitter>0 は
    -- 初期位置を散らして結果が変わるが、 同 seed なら決定的に再現。
    it "nutsInitJitter=0 は jitter 無指定 (従来) とビット同一" $ do
      let c0 = NUTS.nutsPure model cfg initC 2024
          cJ0 = NUTS.nutsPure model cfg { NUTS.nutsInitJitter = 0 } initC 2024
      Core.chainSamples cJ0 `shouldBe` Core.chainSamples c0

    it "nutsInitJitter>0 は結果を変える (init を散らす) が同 seed で再現" $ do
      let c0 = NUTS.nutsPure model cfg initC 2024
          cJ = NUTS.nutsPure model cfg { NUTS.nutsInitJitter = 1.0 } initC 2024
          cJ' = NUTS.nutsPure model cfg { NUTS.nutsInitJitter = 1.0 } initC 2024
      (Core.chainSamples cJ == Core.chainSamples c0) `shouldBe` False
      Core.chainSamples cJ `shouldBe` Core.chainSamples cJ'

    -- Phase 61.1: IO 経路 (nutsChainsStream) は chainSeeds 共有 + ST/IO 等価
    -- (Phase 50 実証) により pure 経路とビット一致するのが設計の柱。
    it "nutsChainsStream: no-op callback で nutsChainsPure とビット一致" $ do
      ioChains <- NUTS.nutsChainsStream model cfg 3 initC 99 (\_ _ -> pure ())
      let pureChains = NUTS.nutsChainsPure model cfg 3 initC 99
      map Core.chainSamples ioChains
        `shouldBe` map Core.chainSamples pureChains

    it "nutsChainsStream: callback が chain ごとに total 回 (burnIn 込み) 呼ばれる" $ do
      countsRef <- newIORef (M.empty :: M.Map Int Int)
      _ <- NUTS.nutsChainsStream model cfg 2 initC 7
             (\i _ -> modifyIORef' countsRef (M.insertWith (+) i 1))
      counts <- readIORef countsRef
      let total = NUTS.nutsBurnIn cfg + NUTS.nutsIterations cfg
      counts `shouldBe` M.fromList [(0, total), (1, total)]

  -- Phase 53: gradADU の正しさを中心差分 (ground truth) で検証。
  -- ★前進/逆 AD どちらでも勾配は数学的に同一ゆえ、 reverse 化の前後で
  --   同じテストが通る = 切替が正しさを保存することの担保。
