{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | Gibbs サンプラー — 共役事前分布の解析的完全条件付きサンプリング。
--
-- 各 GibbsUpdate は 1 パラメータを完全条件付き分布から直接サンプリングするため、
-- Metropolis 棄却ステップが不要でサンプルはすべて採択される。
-- 非共役パラメータが混在する場合は MH と組み合わせる ('gibbsMH')。
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
import Data.Map.Strict (Map)
import Data.Text (Text)
import System.Random.MWC (GenIO, uniform)
import System.Random.MWC.Distributions (gamma, normal)

import MCMC.Core (Chain (..), spawnGen)
import Model.HBM (ModelP, Params, Distribution (..),
                  Node (..), NodeKind (..), collectNodes,
                  logJoint, runObserveDists, priorList)

-- ---------------------------------------------------------------------------
-- 型
-- ---------------------------------------------------------------------------

-- | Gibbs 更新ブロック: 現在のパラメータ一式を受け取り、
-- 担当パラメータの新しい値を完全条件付き分布から 1 つサンプリングして返す。
type GibbsUpdate = Params -> GenIO -> IO (Text, Double)

-- ---------------------------------------------------------------------------
-- 共役アップデート (モデル非依存)
-- ---------------------------------------------------------------------------

-- | Normal 事前 × Normal 尤度 (既知 σ) の共役アップデート。
normalNormal
  :: Text -> Double -> Double -> [Double] -> Double -> GibbsUpdate
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

-- | Beta 事前 × Binomial 尤度の共役アップデート。
betaBinomial
  :: Text -> Double -> Double -> Int -> Int -> GibbsUpdate
betaBinomial paramName alpha0 beta0 n k _ps gen = do
  val <- sampleBeta (alpha0 + fromIntegral k)
                    (beta0  + fromIntegral (n - k))
                    gen
  return (paramName, val)

-- | Gamma 事前 × Poisson 尤度の共役アップデート (rate パラメータ化)。
gammaPoisson
  :: Text -> Double -> Double -> [Double] -> GibbsUpdate
gammaPoisson paramName alpha0 beta0 ys _ps gen = do
  let n     = fromIntegral (length ys) :: Double
      aPost = alpha0 + sum ys
      bPost = beta0 + n
  val <- gamma aPost (1 / bPost) gen
  return (paramName, val)

-- | Beta(a, b) サンプル (mwc-random に Beta がないため X/(X+Y) 公式で実装)。
sampleBeta :: Double -> Double -> GenIO -> IO Double
sampleBeta a b gen = do
  x <- gamma a 1 gen
  y <- gamma b 1 gen
  return (x / (x + y))

-- ---------------------------------------------------------------------------
-- Gibbs サンプラー (汎用ランナー、モデル非依存)
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

-- | updates を 1 イテレーションごとに順番に適用する。
-- Gibbs ステップはすべて採択されるため chainAccepted は全 (updates × iter) になる。
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

gibbsChains :: [GibbsUpdate] -> GibbsConfig -> Int -> Params -> GenIO -> IO [Chain]
gibbsChains updates cfg numChains initP baseGen = do
  gens <- replicateM numChains (spawnGen baseGen)
  mapConcurrently (\g -> gibbs updates cfg initP g) gens

-- ---------------------------------------------------------------------------
-- HBM DSL 統合: 共役構造の自動検出
-- ---------------------------------------------------------------------------

distParams :: Distribution Double -> [Double]
distParams (Normal mu sig)    = [mu, sig]
distParams (Binomial n p)     = [fromIntegral n, p]
distParams (Poisson lam)      = [lam]
distParams (Exponential r)    = [r]
distParams (Gamma a b)        = [a, b]
distParams (Beta a b)         = [a, b]
distParams (Uniform lo hi)    = [lo, hi]
distParams (StudentT df mu s) = [df, mu, s]
distParams (Cauchy loc s)     = [loc, s]
distParams (HalfNormal s)     = [s]
distParams (HalfCauchy s)     = [s]
distParams (LogNormal mu s)   = [mu, s]
distParams (Bernoulli p)      = [p]
distParams (Categorical ps)   = ps
distParams (Mixture ws _)     = ws  -- 共役検出には使えない (重みのみ)
distParams (Truncated _ _ _)  = []  -- 共役検出対象外
distParams (Censored  _ _ _)  = []  -- 共役検出対象外
distParams MvNormal{}         = []  -- 共役検出対象外 (観測専用)

-- 各潜在変数が Observe ノードのどの (obsIndex, slotIndex) に影響するかを検出。
detectObsDeps :: ModelP r -> [Text] -> Map Text [(Int, Int)]
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
--
--   * @Gamma(α,β)@ + @Poisson(λ)@   → 'gammaPoisson'
--   * @Beta(α,β)@  + @Binomial(n,p)@ → 'betaBinomial'
--   * @Normal(μ₀,σ₀)@ + @Normal(μ,σ)@ → 'normalNormal'
--
-- 戻り値: (自動構築した GibbsUpdate リスト, MH が必要な残りパラメータ名)。
gibbsFromModel :: ModelP r -> ([GibbsUpdate], [Text])
gibbsFromModel m =
  let nodes    = collectNodes m
      latNames = [ nodeName n | n <- nodes, nodeKind n == LatentN ]
      priorMap = Map.fromList (priorList m)
      obsList  = runObserveDists m Map.empty
      indexedObs = zip [0 :: Int ..] obsList
      deps     = detectObsDeps m latNames

      obsAt i = listToMaybe [ (d, xs) | (j, (_, d, xs)) <- indexedObs, i == j ]

      buildUpd v =
        let priorD = Map.findWithDefault (Normal 0 1) v priorMap
            vDeps  = Map.findWithDefault [] v deps
        in case (priorD, vDeps) of
          (Gamma a b, [(obsIdx, 0)]) ->
            case obsAt obsIdx of
              Just (Poisson _, xs) -> Just (gammaPoisson v a b xs)
              _                    -> Nothing

          (Beta a b, [(obsIdx, 1)]) ->
            case obsAt obsIdx of
              Just (Binomial nPerObs _, xs) ->
                let k = round (sum xs) :: Int
                    n = nPerObs * length xs
                in Just (betaBinomial v a b n k)
              _ -> Nothing

          (Normal mu0 sig0, [(obsIdx, 0)]) ->
            case obsAt obsIdx of
              Just (Normal _ _, xs) ->
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

      results   = map buildUpd latNames
      updates   = [ u | Just u  <- results ]
      remaining = [ v | (v, Nothing) <- zip latNames results ]
  in (updates, remaining)

-- ---------------------------------------------------------------------------
-- ハイブリッド Gibbs+MH
-- ---------------------------------------------------------------------------

hybridStep
  :: [GibbsUpdate]
  -> [Text]
  -> Map Text Double
  -> ModelP r
  -> Params -> GenIO
  -> IO (Params, Bool)
hybridStep gibbsUpds mhNames mhSteps model current gen = do
  afterGibbs <- foldM (\ps upd -> do
    (name, val) <- upd ps gen
    return (Map.insert name val ps)) current gibbsUpds
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

-- | 共役パラメータを Gibbs、残りを Random Walk MH で更新するハイブリッド。
gibbsMH
  :: ModelP r
  -> GibbsConfig
  -> Map Text Double   -- ^ MH ステップサイズ
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

gibbsMHChains
  :: ModelP r
  -> GibbsConfig
  -> Map Text Double
  -> Int
  -> Params
  -> GenIO
  -> IO [Chain]
gibbsMHChains model cfg mhSteps numChains initP baseGen = do
  gens <- replicateM numChains (spawnGen baseGen)
  mapConcurrently (\g -> gibbsMH model cfg mhSteps initP g) gens
