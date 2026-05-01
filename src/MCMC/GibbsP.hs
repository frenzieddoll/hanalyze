{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | 多相 HBM DSL ('Model.HBMP') 用の Gibbs サンプラー。
--
-- 'MCMC.Gibbs' の HBM 専用 'gibbsFromModel' / 'gibbsMH' を HBMP 化したもの。
-- 共役検出ロジックは同じだが、HBMP の多相 'Distribution' を Double 特殊化して
-- パラメータ値を取り出す。
--
-- 共役更新ブロック ('normalNormal' / 'betaBinomial' / 'gammaPoisson') と
-- 'gibbs' / 'gibbsChains' ランナーはモデル非依存なので 'MCMC.Gibbs' のものを
-- そのまま再利用できる。
--
-- @
-- import Model.HBMP
-- import MCMC.GibbsP
--
-- chain <- gibbsMHP myModel defaultGibbsConfig
--                   Map.empty (Map.fromList [(\"mu\",0),(\"sigma\",1)]) gen
-- @
module MCMC.GibbsP
  ( -- * 自動構築
    gibbsFromModelP
    -- * ハイブリッド Gibbs+MH
  , gibbsMHP
  , gibbsMHPChains
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
import System.Random.MWC.Distributions (normal)

import MCMC.Core   (Chain (..), spawnGen)
import MCMC.Gibbs  (GibbsConfig (..), GibbsUpdate,
                    normalNormal, betaBinomial, gammaPoisson)
import Model.HBMP  (ModelP, Distribution (..),
                    Node (..), NodeKind (..), collectNodes,
                    logJoint, runObserveDists, priorList)

type Params = Map Text Double

-- ---------------------------------------------------------------------------
-- 共役検出
-- ---------------------------------------------------------------------------

-- | HBMP の Double 特殊化分布から数値パラメータを抽出する。
distParamsP :: Distribution Double -> [Double]
distParamsP (Normal mu sig)    = [mu, sig]
distParamsP (Binomial n p)     = [fromIntegral n, p]
distParamsP (Poisson lam)      = [lam]
distParamsP (Exponential r)    = [r]
distParamsP (Gamma a b)        = [a, b]
distParamsP (Beta a b)         = [a, b]

-- 各潜在変数が Observe ノードのどの (obsIndex, slotIndex) に影響するかを検出。
-- 摂動法: 変数を 1 にセットして観測分布パラメータの変化を見る。
detectObsDepsP :: ModelP r -> [Text] -> Map Text [(Int, Int)]
detectObsDepsP m latNames =
  let baseline = map (\(_, d, _) -> distParamsP d) (runObserveDists m Map.empty)
      perturb v = map (\(_, d, _) -> distParamsP d)
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

-- | HBMP モデルの構造を解析し、共役 GibbsUpdate を自動構築する。
--
-- 検出できる共役ペア (HBM 版と同じ):
--
--   * @Gamma(α,β)@ 事前 + @Poisson(λ)@ 尤度  → 'gammaPoisson'
--   * @Beta(α,β)@  事前 + @Binomial(n,p)@ 尤度 → 'betaBinomial'
--   * @Normal(μ₀,σ₀)@ 事前 + @Normal(μ,σ)@ 尤度 → 'normalNormal'
--
-- 戻り値: (自動構築した GibbsUpdate リスト, MH が必要な残りパラメータ名)。
gibbsFromModelP :: ModelP r -> ([GibbsUpdate], [Text])
gibbsFromModelP m =
  let nodes    = collectNodes m
      latNames = [ nodeName n | n <- nodes, nodeKind n == LatentN ]
      priorMap = priorDistMap m
      obsList  = runObserveDists m Map.empty
      indexedObs = zip [0 :: Int ..] obsList
      deps     = detectObsDepsP m latNames

      obsAt i = listToMaybe [ (d, xs) | (j, (_, d, xs)) <- indexedObs, i == j ]

      buildUpd v =
        let priorD = Map.findWithDefault (Normal 0 1) v priorMap
            vDeps  = Map.findWithDefault [] v deps
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
                    n = nPerObs * length xs
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

      results   = map buildUpd latNames
      updates   = [ u | Just u  <- results ]
      remaining = [ v | (v, Nothing) <- zip latNames results ]
  in (updates, remaining)

-- 各潜在変数の事前分布を Double 特殊化して取得する。
priorDistMap :: ModelP r -> Map Text (Distribution Double)
priorDistMap m = Map.fromList (priorList m)

-- ---------------------------------------------------------------------------
-- ハイブリッド Gibbs+MH (HBMP 版)
-- ---------------------------------------------------------------------------

hybridStep
  :: [GibbsUpdate]
  -> [Text]
  -> Map Text Double         -- ^ MH ステップサイズ
  -> ModelP r                -- ^ logJoint 用
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

-- | HBMP モデルに対するハイブリッド Gibbs+MH サンプラー。
--
-- 共役パラメータは自動検出して Gibbs (棄却なし)、残りは Random Walk MH。
gibbsMHP
  :: ModelP r
  -> GibbsConfig
  -> Map Text Double         -- ^ MH ステップサイズ
  -> Params
  -> GenIO
  -> IO Chain
gibbsMHP model cfg mhSteps initP gen = do
  let (gibbsUpds, mhNames) = gibbsFromModelP model
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

-- | gibbsMHP を numChains 本並列実行する。
gibbsMHPChains
  :: ModelP r
  -> GibbsConfig
  -> Map Text Double
  -> Int
  -> Params
  -> GenIO
  -> IO [Chain]
gibbsMHPChains model cfg mhSteps numChains initP baseGen = do
  gens <- replicateM numChains (spawnGen baseGen)
  mapConcurrently (\g -> gibbsMHP model cfg mhSteps initP g) gens
