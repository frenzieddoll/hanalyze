{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | Prior- and posterior-predictive sampling (analogous to PyMC's
-- @sample_prior_predictive@ / @sample_posterior_predictive@).
--
-- @
-- import Stat.PosteriorPredictive
--
-- chain <- nuts model cfg initP gen
-- ppc   <- posteriorPredictive model chain gen
-- -- ppc :: [Map Text [Double]]   -- predicted observations per sample
-- @
module Stat.PosteriorPredictive
  ( -- * 事後予測サンプリング (chain ベース)
    posteriorPredictive
  , posteriorPredictiveSummary
    -- * 事前予測サンプリング (chain 不要)
  , priorPredictive
    -- * 事前サンプリング (latent も)
  , samplePrior
  ) where

import Control.Monad (replicateM)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.List (sort)
import System.Random.MWC (GenIO)

import MCMC.Core (Chain (..))
import Model.HBM
  ( ModelP, sampleDist, runObserveDists, priorList )

-- ---------------------------------------------------------------------------
-- 事後予測サンプリング
-- ---------------------------------------------------------------------------

-- | 事後分布から各観測ノードに対する事後予測サンプルを生成する。
--
-- アルゴリズム:
--   1. チェーンの各サンプル (latent 値) を取り出す
--   2. その latent 値で 'runObserveDists' を呼び、各 observe ノードの
--      条件付き分布を得る
--   3. その分布から元観測数と同じだけ新しい y 値をサンプリング
--
-- 戻り値の長さは 'chainSamples' の長さ。各要素は観測ノードごとに
-- 「元データと同じ長さの新しい予測値リスト」を持つ Map。
posteriorPredictive
  :: forall r. ModelP r
  -> Chain
  -> GenIO
  -> IO [Map Text [Double]]
posteriorPredictive m chain gen =
  mapM (\ps -> genFromObserves m ps gen) (chainSamples chain)

-- | 観測ごとの事後予測サンプル統計 (mean, 95% CI) を計算する。
--
-- 戻り値: 観測名 → 各観測位置 i に対する (mean, 2.5%, 97.5%) のリスト
posteriorPredictiveSummary
  :: [Map Text [Double]]                           -- posteriorPredictive の出力
  -> Map Text [(Double, Double, Double)]
posteriorPredictiveSummary preds =
  let names = case preds of
                []    -> []
                (m:_) -> Map.keys m
  in Map.fromList
       [ (n, summarizePerObs (perSamplePerObs n preds)) | n <- names ]
  where
    -- 観測 n: 各サンプルの観測 i 番目を集めて [[Double]] (列ごと)
    perSamplePerObs :: Text -> [Map Text [Double]] -> [[Double]]
    perSamplePerObs nm samples =
      transpose (map (Map.findWithDefault [] nm) samples)

    summarizePerObs :: [[Double]] -> [(Double, Double, Double)]
    summarizePerObs cols = map oneObs cols
      where
        oneObs xs =
          let s   = sort xs
              n   = length s
              mu  = if n == 0 then 0 else sum xs / fromIntegral n
              q p = if n == 0 then 0
                              else s !! min (n - 1) (max 0 (floor (p * fromIntegral n) :: Int))
          in (mu, q 0.025, q 0.975)

    transpose :: [[a]] -> [[a]]
    transpose [] = []
    transpose xss
      | all null xss = []
      | otherwise =
          let heads = [h | (h:_) <- xss]
              tails = [t | (_:t) <- xss]
          in heads : transpose tails

-- ---------------------------------------------------------------------------
-- 事前予測サンプリング (チェーン不要)
-- ---------------------------------------------------------------------------

-- | 事前分布から N 個の予測サンプルを生成する。
-- データを観測する前に「モデルがどんな観測値を予測するか」を確認するのに使う。
priorPredictive
  :: forall r. ModelP r
  -> Int        -- ^ サンプル数 N
  -> GenIO
  -> IO [Map Text [Double]]
priorPredictive m n gen = replicateM n $ do
  ps <- samplePrior m gen
  genFromObserves m ps gen

-- | モデルの全 latent 変数を事前分布から 1 セット引く。
--
-- 注: 'priorList' は placeholder=0 で構造を取り出すが、ここでは値を順次
-- サンプリングして Map に貯める。下流の latent (μ, σ → θ ~ Normal(μ,σ))
-- に対しては正しい連鎖サンプリングが必要だが、現状の 'priorList' は
-- 個別の事前のみを返す (μ, σ は固定 prior, θ は依存)。
--
-- 簡易実装: 各 latent を独立に事前から引く。階層モデルでは PyMC の
-- sample_prior_predictive と一致しないが、軽量な事前確認には十分。
-- (将来的には 'extractDeps' で順序付けて値を流し込む実装に拡張可能)
samplePrior :: forall r. ModelP r -> GenIO -> IO (Map Text Double)
samplePrior m gen = do
  let priors = priorList m   -- [(name, Distribution Double)] (placeholder=0 走査)
  vals <- mapM (\(_, d) -> sampleDist d gen) priors
  return (Map.fromList (zip (map fst priors) vals))

-- ---------------------------------------------------------------------------
-- 内部: 与えられた latent 値で観測を生成
-- ---------------------------------------------------------------------------

-- 各 observe ノードについて、元データの個数だけ新しいサンプルを生成。
genFromObserves
  :: forall r. ModelP r
  -> Map Text Double
  -> GenIO
  -> IO (Map Text [Double])
genFromObserves m ps gen = do
  let observes = runObserveDists m ps   -- [(name, Distribution Double, [Double])]
  newGroups <- mapM
    (\(nm, d, ys) -> do
        let nObs = length ys
        newYs <- replicateM nObs (sampleDist d gen)
        return (nm, newYs))
    observes
  -- 同名 observe が複数ある場合はリスト連結
  return $ Map.fromListWith (++) newGroups
