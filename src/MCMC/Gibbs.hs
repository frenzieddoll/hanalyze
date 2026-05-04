{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | Gibbs sampler — analytic full-conditional sampling for conjugate priors.
--
-- Each 'GibbsUpdate' draws a single parameter directly from its full
-- conditional distribution, so no Metropolis rejection step is needed and
-- every sample is accepted. When non-conjugate parameters are mixed in,
-- combine with Metropolis-Hastings ('gibbsMH').
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

-- | A Gibbs update block. Receives the current parameter set and returns
-- a single fresh @(name, value)@ sampled from the assigned parameter's
-- full conditional distribution.
type GibbsUpdate = Params -> GenIO -> IO (Text, Double)

-- ---------------------------------------------------------------------------
-- 共役アップデート (モデル非依存)
-- ---------------------------------------------------------------------------

-- | Conjugate update for a Normal prior × Normal likelihood with known
-- @σ@.
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

-- | Conjugate update for a Beta prior × Binomial likelihood.
betaBinomial
  :: Text -> Double -> Double -> Int -> Int -> GibbsUpdate
betaBinomial paramName alpha0 beta0 n k _ps gen = do
  val <- sampleBeta (alpha0 + fromIntegral k)
                    (beta0  + fromIntegral (n - k))
                    gen
  return (paramName, val)

-- | Conjugate update for a Gamma prior × Poisson likelihood
-- (rate parameterization).
gammaPoisson
  :: Text -> Double -> Double -> [Double] -> GibbsUpdate
gammaPoisson paramName alpha0 beta0 ys _ps gen = do
  let n     = fromIntegral (length ys) :: Double
      aPost = alpha0 + sum ys
      bPost = beta0 + n
  val <- gamma aPost (1 / bPost) gen
  return (paramName, val)

-- | Sample @Beta(a, b)@. Implemented as @X / (X + Y)@ with
-- @X ~ Gamma(a)@, @Y ~ Gamma(b)@, since @mwc-random@ has no Beta sampler.
sampleBeta :: Double -> Double -> GenIO -> IO Double
sampleBeta a b gen = do
  x <- gamma a 1 gen
  y <- gamma b 1 gen
  return (x / (x + y))

-- ---------------------------------------------------------------------------
-- Gibbs サンプラー (汎用ランナー、モデル非依存)
-- ---------------------------------------------------------------------------

-- | Gibbs configuration.
data GibbsConfig = GibbsConfig
  { gibbsIterations :: Int   -- ^ Total iterations (burn-in included).
  , gibbsBurnIn     :: Int   -- ^ Burn-in iterations to discard.
  } deriving (Show)

-- | Default configuration: 2000 iterations, 500 burn-in.
defaultGibbsConfig :: GibbsConfig
defaultGibbsConfig = GibbsConfig
  { gibbsIterations = 2000
  , gibbsBurnIn     = 500
  }

-- | Apply each update in @updates@ once per iteration, in order. Every
-- Gibbs step is accepted by construction, so @chainAccepted@ equals
-- @(length updates) × iterations@.
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
    , chainEnergy   = []
    , chainDivergences = []
    }

-- | Run 'gibbs' on @numChains@ parallel chains.
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
distParams (NegativeBinomial mu a) = [mu, a]
distParams (Multinomial _ ps)      = ps
distParams (ZeroInflatedPoisson psi lam)  = [psi, lam]
distParams (ZeroInflatedBinomial _ psi p) = [psi, p]
distParams (InverseGamma a b)             = [a, b]
distParams (Weibull k l)                  = [k, l]
distParams (Pareto a xm)                  = [a, xm]
distParams (BetaBinomial _ a b)           = [a, b]
distParams (VonMises mu k)                = [mu, k]

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

-- | Inspect an HBM model's structure and synthesise the conjugate
-- 'GibbsUpdate' steps automatically.
--
-- Detected conjugate pairs:
--
--   * @Gamma(α,β)@   + @Poisson(λ)@    → 'gammaPoisson'
--   * @Beta(α,β)@    + @Binomial(n,p)@ → 'betaBinomial'
--   * @Normal(μ₀,σ₀)@ + @Normal(μ,σ)@  → 'normalNormal'
--
-- Returns @(updates, remaining)@: the synthesised updates and the names
-- of parameters that still need an MH step.
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

-- | Hybrid sampler: Gibbs-update conjugate parameters and use Random-Walk
-- Metropolis on the rest.
gibbsMH
  :: ModelP r
  -> GibbsConfig
  -> Map Text Double   -- ^ MH step size per non-conjugate parameter.
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
    , chainEnergy   = []
    , chainDivergences = []
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
