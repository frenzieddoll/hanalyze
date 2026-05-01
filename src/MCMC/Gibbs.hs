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
    -- * HBM DSL 統合: 共役自動検出
  , gibbsFromModel
    -- * ハイブリッド Gibbs+MH サンプラー
  , gibbsMH
  , gibbsMHChains
  ) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (foldM, replicateM, when)
import Data.IORef
import Data.List (nub)
import Data.Maybe (listToMaybe)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import System.Random.MWC (GenIO, uniform)
import System.Random.MWC.Distributions (gamma, normal)

import MCMC.Core (Chain (..), spawnGen)
import Model.HBM (Model, collectNodes, NodeInfo (..), NodeRole (..), logJoint,
                  runObserveDists)
import Stat.Distribution (Distribution (..))

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

-- ---------------------------------------------------------------------------
-- HBM DSL 統合: 共役構造の自動検出
-- ---------------------------------------------------------------------------

-- 分布パラメータをリスト化 (変化検出用)
distParams :: Distribution -> [Double]
distParams (Normal mu sig)    = [mu, sig]
distParams (Binomial n p)     = [fromIntegral n, p]
distParams (Poisson lam)      = [lam]
distParams (Exponential r)    = [r]
distParams (Gamma a b)        = [a, b]
distParams (Beta a b)         = [a, b]

-- 各潜在変数が Observe ノードのどの (obsIndex, slotIndex) に影響するかを検出。
-- 摂動法: 変数を 1 にセットして分布パラメータの変化を確認する。
detectObsDeps :: Model a -> [Text] -> Map.Map Text [(Int, Int)]
detectObsDeps m latNames =
  let baseline = map (\(_, d, _) -> distParams d) (runObserveDists m Map.empty)
      perturb v = map (\(_, d, _) -> distParams d)
                      (runObserveDists m (Map.singleton v 1.0))
  in Map.fromList
      [ (v, nub
              [ (oi, si)
              | let pp = perturb v
              , (oi, (bp, pp')) <- zip [0..] (zip baseline pp)
              , (si, (bv, pv))  <- zip [0..] (zip bp pp')
              , bv /= pv
              ])
      | v <- latNames
      ]

-- | HBM モデルの構造を解析し、共役 GibbsUpdate を自動構築する。
--
-- 検出できる共役ペア:
--   * @Gamma(α,β)@ 事前 + @Poisson(λ)@ 尤度  → gammaPoisson
--   * @Beta(α,β)@  事前 + @Binomial(n,p)@ 尤度 → betaBinomial
--   * @Normal(μ₀,σ₀)@ 事前 + @Normal(μ,σ)@ 尤度 → normalNormal (σ は動的参照)
--
-- 戻り値: (自動構築した GibbsUpdate リスト, Gibbs 非対応で MH が必要なパラメータ名リスト)
gibbsFromModel :: Model a -> ([GibbsUpdate], [Text])
gibbsFromModel m =
  let nodes    = collectNodes m
      latents  = [ (nodeName n, nodeDist n)
                 | n <- nodes, isLatent (nodeRole n) ]
      latNames = map fst latents
      -- Observe ノードのみを 0-indexed リスト化
      obsList  = [ (i, d, xs)
                 | (i, NodeInfo _ d (Observed xs)) <- zip [0..] (filter isObs nodes) ]
      deps     = detectObsDeps m latNames

      isLatent Latent       = True
      isLatent (Observed _) = False
      isObs (NodeInfo _ _ (Observed _)) = True
      isObs _               = False

      obsAt i = listToMaybe [ (d, xs) | (j, d, xs) <- obsList, i == j ]

      buildUpd (v, priorD) =
        let vDeps = Map.findWithDefault [] v deps
        in case (priorD, vDeps) of

          -- ── Gamma(α,β) 事前 + Poisson(λ) 尤度 ──────────────────────────
          (Gamma a b, [(obsIdx, 0)]) ->
            case obsAt obsIdx of
              Just (Poisson _, xs) -> Just (gammaPoisson v a b xs)
              _                    -> Nothing

          -- ── Beta(α,β) 事前 + Binomial(n,p) 尤度 ─────────────────────────
          -- slot 1 = p (Binomial n p)
          (Beta a b, [(obsIdx, 1)]) ->
            case obsAt obsIdx of
              Just (Binomial nPerObs _, xs) ->
                let k = round (sum xs) :: Int
                    n = nPerObs * length xs   -- 合計試行数
                in Just (betaBinomial v a b n k)
              _ -> Nothing

          -- ── Normal(μ₀,σ₀) 事前 + Normal(μ,σ) 尤度 (slot 0 = mean) ──────
          (Normal mu0 sig0, [(obsIdx, 0)]) ->
            case obsAt obsIdx of
              Just (Normal _ _, xs) ->
                -- σ (slot 1) を制御している他の潜在変数を探す
                let sigmaVar = listToMaybe
                      [ w | (w, wDeps) <- Map.toList deps
                      , any (\(oi, si) -> oi == obsIdx && si == 1) wDeps
                      , w /= v
                      ]
                in Just $ \ps gen ->
                  let sigLik = maybe 1.0 (\sv -> Map.findWithDefault 1.0 sv ps) sigmaVar
                  in normalNormal v mu0 sig0 xs sigLik ps gen
              _ -> Nothing

          _ -> Nothing

      results   = map buildUpd latents
      updates   = [ u | Just u  <- results ]
      remaining = [ fst p | (p, Nothing) <- zip latents results ]
  in (updates, remaining)

-- ---------------------------------------------------------------------------
-- ハイブリッド Gibbs+MH サンプラー
-- ---------------------------------------------------------------------------

-- 1 イテレーション: 共役パラメータを Gibbs で更新し、残りを MH で更新する
hybridStep
  :: [GibbsUpdate]
  -> [Text]
  -> Map.Map Text Double   -- ^ MH ステップサイズ
  -> Model a
  -> Params -> GenIO
  -> IO (Params, Bool)
hybridStep gibbsUpds mhNames mhSteps model current gen = do
  -- Gibbs フェーズ (全提案が採択される)
  afterGibbs <- foldM (\ps upd -> do
    (name, val) <- upd ps gen
    return (Map.insert name val ps)) current gibbsUpds
  -- MH フェーズ (残りパラメータ)
  if null mhNames
    then return (afterGibbs, True)
    else do
      proposed <- foldM (\ps n -> do
        let s  = Map.findWithDefault 1.0 n mhSteps
            cv = Map.findWithDefault 0.0 n ps
        eps <- normal 0 s gen
        return (Map.insert n (cv + eps) ps)) afterGibbs mhNames
      let logA = logJoint model proposed - logJoint model afterGibbs
      u <- uniform gen
      let accepted = log (u :: Double) < logA
      return (if accepted then proposed else afterGibbs, accepted)

-- | HBM モデルに対するハイブリッド Gibbs+MH サンプラー。
--
-- 共役パラメータは自動検出して Gibbs サンプリング (棄却なし) を行い、
-- 残りのパラメータは Random Walk MH で更新する。
--
-- @mhSteps@ は MH が担当するパラメータのステップサイズ。
-- キーにないパラメータはデフォルト 1.0 を使用。
gibbsMH
  :: Model a
  -> GibbsConfig
  -> Map.Map Text Double   -- ^ MH ステップサイズ
  -> Params
  -> GenIO
  -> IO Chain
gibbsMH model cfg mhSteps initP gen = do
  let (gibbsUpds, mhNames) = gibbsFromModel model
      total = gibbsBurnIn cfg + gibbsIterations cfg
  samplesRef  <- newIORef []
  acceptedRef <- newIORef (0 :: Int)
  let loop 0 current = return current
      loop i current = do
        (next, acc) <- hybridStep gibbsUpds mhNames mhSteps model current gen
        when acc $ modifyIORef' acceptedRef (+1)
        when (i <= gibbsIterations cfg) $
          modifyIORef' samplesRef (next :)
        loop (i - 1) next
  _ <- loop total initP
  samples  <- fmap reverse (readIORef samplesRef)
  accepted <- readIORef acceptedRef
  return Chain
    { chainSamples  = samples
    , chainAccepted = accepted
    , chainTotal    = total
    }

-- | ハイブリッド Gibbs+MH を numChains 本並列実行する。
gibbsMHChains
  :: Model a
  -> GibbsConfig
  -> Map.Map Text Double
  -> Int
  -> Params
  -> GenIO
  -> IO [Chain]
gibbsMHChains model cfg mhSteps numChains initP baseGen = do
  gens <- replicateM numChains (spawnGen baseGen)
  mapConcurrently (\g -> gibbsMH model cfg mhSteps initP g) gens
