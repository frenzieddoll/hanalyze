{-# LANGUAGE OverloadedStrings #-}
-- | Hanalyze.MCMC.Progress の純粋部 (formatProgress) のテスト (Phase 61.2)。
module Hanalyze.MCMC.ProgressSpec (spec) where

import Test.Hspec
import Hanalyze.MCMC.Progress

spec :: Spec
spec = do
  describe "Hanalyze.MCMC.Progress.formatProgress" $ do
    it "warmup 中: 計画 md の例どおりの 1 行" $
      formatProgress (ProgressSnapshot 4 2 3400 8000 True 12 380.0)
        `shouldBe` "chains 2/4 done | draw 3400/8000 (warmup) | div 12 | 380.0 it/s"

    it "warmup 後: (warmup) タグが消える" $
      formatProgress (ProgressSnapshot 4 4 8000 8000 False 0 95.25)
        `shouldBe` "chains 4/4 done | draw 8000/8000 | div 0 | 95.2 it/s"
