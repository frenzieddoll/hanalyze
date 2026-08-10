{-# LANGUAGE OverloadedStrings #-}
-- | Hanalyze.Stat.Summary の spec (Phase 100: 表示系 ESS の essBulk 統一)。
--
-- posteriorSummary の srEssV が旧 pooled 'ess' ではなく chain 構造を渡した
-- 'essBulk' (arviz ess_bulk 互換・MCMCSpec の golden で検証済) と一致する
-- 配線を固定する。データは MCMCSpec と同じ決定的 LCG+AR(1)。
module Hanalyze.Stat.SummarySpec (spec) where

import qualified Data.Map.Strict as Map
import Test.Hspec

import Hanalyze.MCMC.Core    (Chain (..))
import Hanalyze.Stat.MCMC    (essBulk)
import Hanalyze.Stat.Summary (SummaryRow (..), posteriorSummary)

-- | glibc 系数の LCG (mod 2^31)・[-0.5, 0.5) 一様。Integer 演算なので厳密。
lcg :: Int -> Int -> [Double]
lcg seed n = take n (map toU (drop 1 (iterate step (fromIntegral seed))))
  where
    step x = (1103515245 * x + 12345) `mod` (2 ^ (31 :: Int)) :: Integer
    toU x  = fromIntegral x / 2 ^ (31 :: Int) - 0.5

-- | AR(1): y_i = phi*y_{i-1} + u_i (y_0 起点 0)。
ar1 :: Int -> Int -> Double -> [Double]
ar1 seed n phi = drop 1 (scanl (\prev u -> phi * prev + u) 0 (lcg seed n))

mkChain :: [Double] -> Chain
mkChain vs = Chain
  { chainSamples     = [Map.singleton "x" v | v <- vs]
  , chainAccepted    = 0
  , chainTotal       = 0
  , chainEnergy      = []
  , chainDivergences = []
  , chainTreeDepths  = []
  }

relClose :: Double -> Double -> Double -> Bool
relClose tol expected actual = abs (actual - expected) <= tol * abs expected

spec :: Spec
spec = do
  describe "posteriorSummary の ESS (Phase 100: essBulk 統一)" $ do
    it "多 chain: srEssV = essBulk perChain (arviz golden 84.428・旧 pooled ess 86.804 ではない)" $ do
      let perChain = [ar1 (c + 1) 300 0.9 | c <- [0 .. 3 :: Int]]
          [row]    = posteriorSummary ["x"] (map mkChain perChain)
      srEssV row `shouldBe` essBulk perChain
      srEssV row `shouldSatisfy` relClose 1e-6 84.42798772749184

    it "単一 chain でも essBulk (split 2 sub-chain) を返す" $ do
      let vs    = ar1 42 300 0.5
          [row] = posteriorSummary ["x"] [mkChain vs]
      srEssV row `shouldBe` essBulk [vs]
      srRhat row `shouldBe` Nothing
