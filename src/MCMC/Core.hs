{-# LANGUAGE OverloadedStrings #-}
-- | MCMC 共通型と事後統計量。
-- サンプラーに依存しないため、MCMC を単独ライブラリとして使う際の基盤となります。
module MCMC.Core
  ( -- * チェーン型
    Chain (..)
    -- * 事後統計量
  , acceptanceRate
  , posteriorMean
  , posteriorSD
  , posteriorQuantile
  , chainVals
    -- * ユーティリティ
  , spawnGen
  ) where

import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Word (Word32)
import qualified Data.Vector as V
import System.Random.MWC (GenIO, uniform, initialize)

-- ---------------------------------------------------------------------------
-- Chain
-- ---------------------------------------------------------------------------

-- | MCMC チェーン。バーンイン後サンプルのみを保持する。
data Chain = Chain
  { chainSamples  :: [Map.Map Text Double]  -- ^ バーンイン後サンプル (描画順)
  , chainAccepted :: Int                    -- ^ 採択数 (バーンイン含む)
  , chainTotal    :: Int                    -- ^ 提案総数 (バーンイン含む)
  , chainEnergy   :: [Double]
    -- ^ 各反復の Hamiltonian エネルギー H = -log p(θ) + 0.5|p|² (バーンイン後)。
    --   HMC/NUTS のみ意味を持つ; MH/Gibbs などは空リスト。BFMI / energy plot 用。
  , chainDivergences :: [Int]
    -- ^ NUTS で divergent transition が起きた反復の 0-origin index 列
    --   (バーンイン後)。Stan 同様 |H_proposal - H_initial| > 1000 を判定基準
    --   とする。多ければ事後分布が病的で、reparameterization が必要。
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- Summary statistics
-- ---------------------------------------------------------------------------

-- | 受容率 (バーンイン込み)。
acceptanceRate :: Chain -> Double
acceptanceRate ch =
  fromIntegral (chainAccepted ch) / fromIntegral (chainTotal ch)

-- | 指定パラメータの事後平均。存在しない場合は Nothing。
posteriorMean :: Text -> Chain -> Maybe Double
posteriorMean name ch =
  let vals = chainVals name ch
  in if null vals then Nothing
     else Just (sum vals / fromIntegral (length vals))

-- | 指定パラメータの事後標準偏差。
posteriorSD :: Text -> Chain -> Maybe Double
posteriorSD name ch =
  case posteriorMean name ch of
    Nothing -> Nothing
    Just mu ->
      let vals = chainVals name ch
      in if null vals then Nothing
         else Just (sqrt (sum (map (\x -> (x - mu) ^ (2 :: Int)) vals)
                         / fromIntegral (length vals)))

-- | 経験分位点 (0 ≤ p ≤ 1)。
posteriorQuantile :: Double -> Text -> Chain -> Maybe Double
posteriorQuantile p name ch =
  let vals = sort (chainVals name ch)
      n    = length vals
  in if null vals then Nothing
     else
       let idx = min (n - 1) (floor (p * fromIntegral n) :: Int)
       in Just (vals !! idx)

-- | チェーンから指定パラメータのサンプル列を取り出す。rhat 等に渡す用途。
chainVals :: Text -> Chain -> [Double]
chainVals name ch = [v | Just v <- map (Map.lookup name) (chainSamples ch)]

-- ---------------------------------------------------------------------------
-- Utility
-- ---------------------------------------------------------------------------

-- | 基底 GenIO から独立した子 GenIO を生成する。
-- 並列チェーンで各チェーンに異なるシードを与えるために使う。
spawnGen :: GenIO -> IO GenIO
spawnGen base = do
  seed <- uniform base :: IO Word32
  initialize (V.singleton seed)
