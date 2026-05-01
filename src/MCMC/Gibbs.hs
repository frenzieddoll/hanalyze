{-# LANGUAGE OverloadedStrings #-}
-- | Gibbs サンプラー — 共役事前分布の解析的完全条件付きサンプリング。
--
-- 各 GibbsUpdate は 1 パラメータを完全条件付き分布から直接サンプリングするため、
-- Metropolis 棄却ステップが不要でサンプルはすべて採択される。
-- 非共役パラメータが混在する場合は MH と組み合わせること。
--
-- @
-- let updates = [ betaBinomial "p" 1 1 50 18 ]
--     cfg     = defaultGibbsConfig { gibbsIterations = 5000 }
-- chain <- gibbs updates cfg initParams gen
-- @
module MCMC.Gibbs
  ( -- * 共役アップデートブロック
    GibbsUpdate
  , normalNormal
  , betaBinomial
  , gammaPoisson
    -- * サンプラー
  , GibbsConfig (..)
  , defaultGibbsConfig
  , gibbs
  , gibbsChains
  ) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (foldM, replicateM, when)
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import System.Random.MWC (GenIO)
import System.Random.MWC.Distributions (gamma, normal)

import MCMC.Core (Chain (..), spawnGen)

-- ---------------------------------------------------------------------------
-- 型
-- ---------------------------------------------------------------------------

type Params = Map.Map Text Double

-- | Gibbs 更新ブロック: 現在のパラメータ一式を受け取り、
-- 担当パラメータの新しい値を完全条件付き分布から 1 つサンプリングして返す。
type GibbsUpdate = Params -> GenIO -> IO (Text, Double)

-- ---------------------------------------------------------------------------
-- 共役アップデートの構築
-- ---------------------------------------------------------------------------

-- | Normal 事前分布 × Normal 尤度 (既知 σ) の共役アップデート。
--
-- 事前: μ ~ Normal(μ₀, σ₀)
-- 尤度: yᵢ ~ Normal(μ, σ_lik)  — σ_lik は既知
-- 事後: μ|y ~ Normal(μ_post, σ_post) where
--   1/σ_post² = 1/σ₀² + n/σ_lik²
--   μ_post    = σ_post² × (μ₀/σ₀² + Σyᵢ/σ_lik²)
normalNormal
  :: Text    -- ^ パラメータ名
  -> Double  -- ^ 事前平均 μ₀
  -> Double  -- ^ 事前 SD σ₀
  -> [Double] -- ^ 観測値
  -> Double  -- ^ 既知の尤度 SD σ_lik
  -> GibbsUpdate
normalNormal paramName mu0 sig0 ys sigLik _ps gen = do
  let n        = fromIntegral (length ys) :: Double
      ybar     = if n == 0 then 0 else sum ys / n
      prec0    = 1 / sig0    ^ (2::Int)
      precLik  = 1 / sigLik  ^ (2::Int)
      precPost = prec0 + n * precLik
      sigPost  = sqrt (1 / precPost)
      muPost   = (mu0 * prec0 + n * ybar * precLik) / precPost
  val <- normal muPost sigPost gen
  return (paramName, val)

-- | Beta 事前分布 × Binomial 尤度の共役アップデート。
--
-- 事前: p ~ Beta(α, β)
-- 尤度: k ~ Binomial(n, p)
-- 事後: p|k ~ Beta(α + k, β + n − k)
betaBinomial
  :: Text    -- ^ パラメータ名
  -> Double  -- ^ 事前 α
  -> Double  -- ^ 事前 β
  -> Int     -- ^ 試行数 n
  -> Int     -- ^ 成功数 k
  -> GibbsUpdate
betaBinomial paramName alpha0 beta0 n k _ps gen = do
  val <- sampleBeta (alpha0 + fromIntegral k)
                    (beta0  + fromIntegral (n - k))
                    gen
  return (paramName, val)

-- | Gamma 事前分布 × Poisson 尤度の共役アップデート。
--
-- 事前: λ ~ Gamma(α, rate=β)  [rate パラメータ化]
-- 尤度: yᵢ ~ Poisson(λ)
-- 事後: λ|y ~ Gamma(α + Σyᵢ, rate=β + n)
gammaPoisson
  :: Text    -- ^ パラメータ名
  -> Double  -- ^ 事前 shape α
  -> Double  -- ^ 事前 rate β
  -> [Double] -- ^ 観測値
  -> GibbsUpdate
gammaPoisson paramName alpha0 beta0 ys _ps gen = do
  let n     = fromIntegral (length ys) :: Double
      aPost = alpha0 + sum ys
      bPost = beta0 + n           -- rate パラメータ
  val <- gamma aPost (1 / bPost) gen   -- mwc-random は scale = 1/rate
  return (paramName, val)

-- ---------------------------------------------------------------------------
-- Beta サンプリング補助 (mwc-random 0.15 には beta がないため自前実装)
-- ---------------------------------------------------------------------------

-- | Beta(a, b) から 1 サンプル。
-- X ~ Gamma(a,1), Y ~ Gamma(b,1) → X/(X+Y) ~ Beta(a,b)
sampleBeta :: Double -> Double -> GenIO -> IO Double
sampleBeta a b gen = do
  x <- gamma a 1 gen
  y <- gamma b 1 gen
  return (x / (x + y))

-- ---------------------------------------------------------------------------
-- Gibbs サンプラー
-- ---------------------------------------------------------------------------

data GibbsConfig = GibbsConfig
  { gibbsIterations :: Int
  , gibbsBurnIn     :: Int
  } deriving (Show)

defaultGibbsConfig :: GibbsConfig
defaultGibbsConfig = GibbsConfig
  { gibbsIterations = 2000
  , gibbsBurnIn     = 500
  }

-- | Gibbs サンプラー。
-- updates を 1 イテレーションごとに順番に適用してすべてのパラメータを更新する。
-- Gibbs ステップはすべて採択されるため chainAccepted / chainTotal は全ステップ数になる。
gibbs :: [GibbsUpdate] -> GibbsConfig -> Params -> GenIO -> IO Chain
gibbs updates cfg initP gen = do
  let total = gibbsBurnIn cfg + gibbsIterations cfg
      nUpd  = length updates

  samplesRef  <- newIORef []
  acceptedRef <- newIORef (0 :: Int)

  let step current = foldM applyOne current updates
        where
          applyOne ps upd = do
            (name, val) <- upd ps gen
            return (Map.insert name val ps)

  let loop 0 current = return current
      loop i current = do
        next <- step current
        modifyIORef' acceptedRef (+ nUpd)
        when (i <= gibbsIterations cfg) $
          modifyIORef' samplesRef (next :)
        loop (i - 1) next

  _ <- loop total initP
  samples  <- fmap reverse (readIORef samplesRef)
  accepted <- readIORef acceptedRef
  return Chain
    { chainSamples  = samples
    , chainAccepted = accepted
    , chainTotal    = total * nUpd
    }

-- | Gibbs を numChains 本並列実行する (+RTS -N で CPU 並列)。
gibbsChains :: [GibbsUpdate] -> GibbsConfig -> Int -> Params -> GenIO -> IO [Chain]
gibbsChains updates cfg numChains initP baseGen = do
  gens <- replicateM numChains (spawnGen baseGen)
  mapConcurrently (\g -> gibbs updates cfg initP g) gens
