{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | Phase 103: HBM 事後要約の一発 API (hbmSummary / hbmSummaryNames) の spec。
--
-- fixture は A1 probe (experiments/phase103-hbm-summary-df-helpers/) と同型の
-- 決定的 fit (hbmModelPure = 既定 seed 42)。固定する配線:
--   * hbmSummary = posteriorSummary (latent+deterministic 名) (augment 済 chain)
--     の手繋ぎ直呼びと完全一致 (一発 API が既存経路の糖衣であること)
--   * deterministic 派生量 (mu) が既定で要約に含まれる (A1 確定の設計)
--   * deterministicNames は宣言順・runDeterministics の key 集合と一致
module Hanalyze.Model.HBMSummarySpec (spec) where

import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import qualified DataFrame.Internal.DataFrame  as DX
import Test.Hspec

import qualified Hanalyze.Data.Wrangle as W
import Hanalyze.DataIO.Preprocess (readMaybeDoubleColumn)
import Hanalyze.MCMC.Core (chainVals)
import qualified Hanalyze.Model.HBM as HBM
import Hanalyze.Model.Wrappers (HBMModel (..), HBMConfig (..), defaultHBM,
                                       hbmModelPure, hbmSummary, hbmSummaryNames,
                                       hbmSummaryDf, hbmDrawsDf)
import Hanalyze.Stat.Summary (SummaryRow (..), posteriorSummary)

-- | A1 probe と同型の最小モデル (latent 2 + deterministic 1)。
model :: HBM.ModelP ()
model = do
  xs <- HBM.dataNamed    "x" [1, 2, 3]
  ys <- HBM.dataNamedObs "y" [2, 4, 6]
  b  <- HBM.sample "b" (HBM.Normal 0 1)
  s  <- HBM.sample "s" (HBM.HalfNormal 1)
  mu <- HBM.deterministic "mu" (b * head xs)
  HBM.observe "y" (HBM.Normal mu s) ys

-- | deterministic 無しモデル (augment 迂回パスの確認用)。
modelNoDet :: HBM.ModelP ()
modelNoDet = do
  ys <- HBM.dataNamedObs "y" [2, 4, 6]
  b  <- HBM.sample "b" (HBM.Normal 0 1)
  HBM.observe "y" (HBM.Normal b 1) ys

-- | 決定的 fit (seed 42 既定・test 速度優先の小 iteration)。
fitted :: HBMModel
fitted = hbmModelPure (defaultHBM { hbmChains = 2, hbmWarmup = 100, hbmSamples = 100 })
                      model []

rowKey :: SummaryRow -> (String, Double, Double, Double, Double, Double, Maybe Double)
rowKey r = (show (srName r), srMean r, srSD r, srHdiLo r, srHdiHi r, srEssV r, srRhat r)

spec :: Spec
spec = do
  describe "hbmSummaryNames (Phase 103)" $ do
    it "latent 宣言順 → deterministic 宣言順の連結" $
      hbmSummaryNames fitted `shouldBe` ["b", "s", "mu"]

    it "deterministicNames = runDeterministics の key 集合 (順序のみ宣言順)" $ do
      let spec' :: HBM.ModelP ()
          spec' = hbmModelSpec fitted
          ks    = Map.keys (HBM.runDeterministics spec' (Map.fromList [("b", 1), ("s", 1)]))
      HBM.deterministicNames spec' `shouldMatchList` ks

  describe "hbmSummary (Phase 103)" $ do
    it "posteriorSummary 直呼び (augment 手繋ぎ) と完全一致" $ do
      let spec' :: HBM.ModelP ()
          spec'  = hbmModelSpec fitted
          names  = HBM.sampleNames spec' ++ HBM.deterministicNames spec'
          chains = map (HBM.augmentChainWithDeterministic spec') (hbmChainsR fitted)
          direct = posteriorSummary names chains
      map rowKey (hbmSummary fitted) `shouldBe` map rowKey direct

    it "deterministic 派生量 mu が既定で要約に含まれ有限値を持つ" $ do
      let rows = hbmSummary fitted
          mus  = [r | r <- rows, srName r == "mu"]
      length mus `shouldBe` 1
      all (\r -> not (isNaN (srMean r))) mus `shouldBe` True

    it "multi-chain fit では r_hat が Just" $
      all (\r -> srRhat r /= Nothing) (hbmSummary fitted) `shouldBe` True

    it "deterministic 無しモデルでも成立 (augment 迂回パス)" $ do
      let m    = hbmModelPure (defaultHBM { hbmChains = 1, hbmWarmup = 50, hbmSamples = 50 })
                              modelNoDet []
          rows = hbmSummary m
      map srName rows `shouldBe` ["b"]
      -- 単 chain は r_hat 無し (posteriorSummary の既存挙動に一致)
      map srRhat rows `shouldBe` [Nothing]

  describe "hbmSummaryDf (Phase 103)" $ do
    it "multi-chain: 列 = param/mean/sd/hdi_lo/hdi_hi/ess_bulk/r_hat・行 = 全パラメタ" $ do
      let df = hbmSummaryDf fitted
      DX.columnNames df `shouldBe`
        ["param", "mean", "sd", "hdi_lo", "hdi_hi", "ess_bulk", "r_hat"]
      let Just means = readMaybeDoubleColumn "mean" df
      catMaybes means `shouldBe` map srMean (hbmSummary fitted)

    it "単 chain: r_hat 列が付かない (printPosteriorSummary の列規約と同じ)" $ do
      let m  = hbmModelPure (defaultHBM { hbmChains = 1, hbmWarmup = 50, hbmSamples = 50 })
                            modelNoDet []
          df = hbmSummaryDf m
      DX.columnNames df `shouldBe`
        ["param", "mean", "sd", "hdi_lo", "hdi_hi", "ess_bulk"]

  describe "hbmDrawsDf (Phase 103)" $ do
    it "1 パラメタ = 1 列 (deterministic 込み)・全 chain 連結の draw 数" $ do
      let df = hbmDrawsDf fitted
      DX.columnNames df `shouldBe` ["b", "s", "mu"]
      let Just mus = readMaybeDoubleColumn "mu" df
      length (catMaybes mus) `shouldBe` 200   -- 2 chain x 100 draw

    it "draw 値 = augment 済 chain の chainVals 連結 (chain 順)" $ do
      let spec' :: HBM.ModelP ()
          spec'  = hbmModelSpec fitted
          direct = concatMap (chainVals "mu")
                     (map (HBM.augmentChainWithDeterministic spec') (hbmChainsR fitted))
          Just mus = readMaybeDoubleColumn "mu" (hbmDrawsDf fitted)
      catMaybes mus `shouldBe` direct

    it "Wrangle summarise (meanOf) がそのまま効く" $ do
      let df = hbmDrawsDf fitted
          out = W.summarise ["m" W.=: W.meanOf "mu"] df
          Just [Just m] = readMaybeDoubleColumn "m" out
          Just mus = readMaybeDoubleColumn "mu" df
          xs = catMaybes mus
      abs (m - sum xs / fromIntegral (length xs)) `shouldSatisfy` (< 1e-9)
