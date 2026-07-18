-- |
-- Module      : Hanalyze.MCMC.NUTS
-- Description : No-U-Turn Sampler (NUTS) — Hoffman & Gelman (2014) Algorithm 3 実装
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- No-U-Turn Sampler (NUTS).
--
-- Implements Hoffman & Gelman (2014) Algorithm 3, with Nesterov dual
-- averaging for step-size adaptation (Stan's strategy). Gradients are
-- exact, computed via 'Numeric.AD.Mode.Reverse.Double' (Phase 53: reverse モードで
-- 勾配を latent 数非依存の ~1 sweep に。 旧 forward は O(p) だった).
--
-- Constrained parameters (@PositiveT@, @UnitIntervalT@) are detected
-- automatically from the prior distribution.
--
-- @
-- import Hanalyze.Model.HBM
-- import Hanalyze.MCMC.NUTS
--
-- chain <- nuts myModel defaultNUTSConfig
--                (Map.fromList [("mu",0),("sigma",1)]) gen
-- @
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Hanalyze.MCMC.NUTS
  ( NUTSConfig (..)
  , defaultNUTSConfig
  , nuts
  , nutsStream
  , nutsChains
  , nutsPure
  , nutsChainsPure
  , nutsChainsStream
  , chainSeeds
  , SampleEvent (..)
  ) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (foldM, replicateM, when)
import Control.Monad.ST (ST, runST)
import Control.Parallel.Strategies (parList, rdeepseq, using)
import Data.Primitive.MutVar
import Data.Word (Word32)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import System.Random.MWC (Gen, GenIO, uniform, initialize)
import Control.Monad.Primitive (PrimMonad, PrimState, RealWorld)
import System.Random.MWC.Distributions (standard)

import Hanalyze.MCMC.Core (Chain (..), spawnGen)
import Hanalyze.MCMC.HMC  (kineticMVS, leapfrogWithMVS)
import Hanalyze.Model.HBM (ModelP, Params, sampleNames, getTransforms,
                  compileGradUV, compileGradValUVM, compileLogPUV)
import Hanalyze.Stat.Distribution (toUnconstrained, fromUnconstrained)

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

-- | NUTS configuration.
data NUTSConfig = NUTSConfig
  { nutsIterations    :: Int     -- ^ Post-burn-in draws to keep (the loop runs
                                 --   @nutsBurnIn + nutsIterations@ total).
  , nutsBurnIn        :: Int     -- ^ Burn-in iterations to discard.
  , nutsStepSize      :: Double  -- ^ Initial leapfrog step size @ε@.
  , nutsMaxDepth      :: Int     -- ^ Maximum tree depth (typically 10).
  , nutsAdaptStepSize :: Bool    -- ^ Enable Nesterov dual-averaging step-size adaptation.
  , nutsTargetAccept  :: Double  -- ^ Target acceptance rate (0.8 typical, 0.95 for hard problems).
  , nutsWarmupInitMaxDepth :: Maybe Int
                                 -- ^ Phase 85.6: 質量行列の**初回更新前** (init
                                 --   buffer + 第 1 window・M=I 期間) に適用する
                                 --   tree depth 上限 (opt-in・既定 'Nothing' =
                                 --   無効)。 M=I では幾何が合わず dual averaging
                                 --   の ε 鋸歯で depth 7-10 の木を掘り radon 実測
                                 --   で warmup leapfrog の 68% を浪費するため、
                                 --   'Just 6' 等で抑制できる。 ただし参照実装
                                 --   (Stan/PyMC) に無いヒューリスティックゆえ
                                 --   既定 OFF — 原理側の対策は 'nutsInitEpsSearch'。
                                 --   'nutsAdaptMass' が False のときは不適用。
  , nutsInitEpsSearch :: Bool    -- ^ Phase 85.6c/86: Stan (Hoffman–Gelman
                                 --   Algorithm 4) の ε 倍加探索を (i) サンプリング
                                 --   開始前と (ii) 質量行列の各 window 末更新直後
                                 --   (Phase 86・Stan adapt_diag_e_nuts の
                                 --   init_stepsize+restart と同順) に行う (既定
                                 --   True)。 DA anchor (μ = log 10ε) が幾何と
                                 --   乖離すると ε が鋸歯振動して深い木を掘るため、
                                 --   ε を 1 step leapfrog の受容率 ~1/2 になる値へ
                                 --   都度較正する (Stan と同じ標準機構)。
                                 --   'nutsAdaptStepSize' が True のときのみ有効。
  , nutsAdaptMass     :: Bool    -- ^ Enable diagonal mass-matrix adaptation (B11).
                                 --   Stan-style multi-window: init buffer (15% /
                                 --   ≥75 iter, M=I) → doubling windows
                                 --   25→50→100→200→… (M updated + dual avg
                                 --   restarted at each window end) → term buffer
                                 --   (10% / ≥50 iter, M frozen, ε converges).
                                 --   Recommended for posteriors with strongly
                                 --   varying scales across parameters.
  , nutsInitJitter    :: Double  -- ^ Phase 94 A4-2: 各 chain の初期位置 (unconstrained)
                                 --   に加える一様 jitter 半幅 (PyMC jitter+adapt_diag
                                 --   相当)。 chain ごとに独立に @U(-j, +j)@ を各成分へ
                                 --   加算し、 funnel 首での whole-chain 崩壊 (全 chain
                                 --   同一 init 由来) を減らす。 @0@ = 無操作 (= 従来
                                 --   挙動・単一 chain 再現性テスト非影響)。 多 chain
                                 --   経路 ('hbmNutsConfig') で 1.0 を設定。
  } deriving (Show)

-- | Default NUTS configuration: 2000 post-burn-in draws, 500 burn-in
-- (2500 total), @ε = 0.1@,
-- max depth 10, dual averaging enabled, target acceptance 0.8,
-- diagonal mass-matrix adaptation off (opt-in via 'nutsAdaptMass').
defaultNUTSConfig :: NUTSConfig
defaultNUTSConfig = NUTSConfig
  { nutsIterations    = 2000
  , nutsBurnIn        = 500
  , nutsStepSize      = 0.1
  , nutsMaxDepth      = 10
  , nutsAdaptStepSize = True
  , nutsTargetAccept  = 0.8
  , nutsWarmupInitMaxDepth = Nothing
  , nutsInitEpsSearch = True
  , nutsAdaptMass     = False
  , nutsInitJitter    = 0.0
  }

-- ---------------------------------------------------------------------------
-- Dual averaging
-- ---------------------------------------------------------------------------

-- | Internal state for Nesterov's dual-averaging step-size adaptation.
data DualAvgState = DualAvgState
  { daLogEps     :: Double   -- ^ Current @log ε@ used for sampling.
  , daLogEpsBar  :: Double   -- ^ Running smoothed @log ε̄@ (post-adaptation value).
  , daH          :: Double   -- ^ Running average of (target − accept-stat).
  , daMu         :: Double   -- ^ Anchor @μ = log(10 ε₀)@.
  , daM          :: Int      -- ^ Iteration counter.
  }

-- | Initialize 'DualAvgState' from an initial step size @ε₀@.
initDualAvg :: Double -> DualAvgState
initDualAvg eps0 = DualAvgState
  { daLogEps    = log eps0
  , daLogEpsBar = log eps0
  , daH         = 0.0
  , daMu        = log (10 * eps0)
  , daM         = 0
  }

-- | Phase 85.6c/86: Stan (@base_hmc::init_stepsize@) 準拠の ε 探索。
-- 与えられた ε を起点に、 1 step leapfrog の受容比が 0.8 を跨ぐまで倍加/半減
-- する (運動量は試行ごとに再サンプル)。 dual averaging の anchor μ = log(10 ε₀)
-- が幾何に合った値になり、 ε 鋸歯振動 (radon で depth 7-10 の深掘り) を防ぐ。
-- ★Hoffman–Gelman 2014 Alg.4 (起点 1.0・閾値 1/2・運動量 1 本固定) でなく
-- Stan 実装 (起点 = 現在 ε・閾値 0.8・毎試行再サンプル) に合わせる —
-- window 末の再較正 (Phase 86) では既適応の ε 近傍から保守的に探す必要がある
-- (起点 1.0/閾値 0.5 は radon 実測で新 M 下の trajectory が支えない大きな ε を
-- 返し、 次 window の深掘りを招いた)。 非有限 (発散) は比 −∞ 扱い = 半減方向。
-- 反復と ε は安全側に有界。
findReasonableEpsilon
  :: PrimMonad m
  => (VS.Vector Double -> VS.Vector Double)   -- ^ gradFn (−∇ logπ・NUTS と同じ向き)
  -> (VS.Vector Double -> Double)             -- ^ logπ (unconstrained)
  -> VS.Vector Double                          -- ^ M⁻¹ 対角
  -> Double                                    -- ^ 探索起点 ε (現在の nominal ε)
  -> VS.Vector Double                          -- ^ 初期位置 θ (unconstrained)
  -> Gen (PrimState m)
  -> m Double
findReasonableEpsilon gradFn logPiFn mInv eps0 theta gen = do
    dH0 <- trial epsInit
    let dir = if dH0 > thresh then 1 else -1 :: Int
    loop dir epsInit (50 :: Int)
  where
    epsInit = max 1e-10 (min 1e7 eps0)
    thresh  = log 0.8
    -- 1 step leapfrog の log 受容比 (Stan と同じく運動量を都度引き直す)。
    trial eps = do
      r0 <- sampleMomentum mInv gen
      let h0        = negate (logPiFn theta) + kineticMVS mInv r0
          (th', r') = leapfrogWithMVS gradFn mInv eps 1 theta r0
          h'        = negate (logPiFn th') + kineticMVS mInv r'
      pure (if isNaN h' || isInfinite h' then (-1) / 0 else h0 - h')
    loop dir !eps !k
      | k <= 0 = pure eps
      | otherwise = do
          dH <- trial eps
          let keepGoing = if dir == 1 then dH > thresh else dH < thresh
              eps'      = if dir == 1 then eps * 2 else eps / 2
          if not keepGoing then pure eps
          else if eps' > 1e7 || eps' < 1e-10 then pure eps
          else loop dir eps' (k - 1)

-- | Apply one dual-averaging update given the target acceptance @δ@ and
-- the observed acceptance statistic @α@ for the iteration.
updateDualAvg :: Double -> Double -> DualAvgState -> DualAvgState
updateDualAvg delta alpha da =
  let m      = daM da + 1
      gamma  = 0.05
      t0     = 10.0
      kappa  = 0.75
      hNew   = (1 - 1 / (fromIntegral m + t0)) * daH da
             + (1 / (fromIntegral m + t0)) * (delta - alpha)
      logEps = daMu da - sqrt (fromIntegral m) / gamma * hNew
      logEpsClip = max (-7) (min 5 logEps)
      logEpsBar = (fromIntegral m ** (-kappa)) * logEpsClip
                + (1 - fromIntegral m ** (-kappa)) * daLogEpsBar da
  in da { daLogEps = logEpsClip, daLogEpsBar = logEpsBar, daH = hNew, daM = m }

-- ---------------------------------------------------------------------------
-- 内部ツリー
-- ---------------------------------------------------------------------------

-- | Internal NUTS tree node. All position/momentum are 'VS.Vector
-- Double' rather than 'Params' (= @Map@) / @[Double]@: the
-- @doubleTree@ recursion creates up to @2¹⁰@ intermediate trees per
-- iteration, and the previous Map / list representation paid an
-- order-of-magnitude in allocation that swamped the actual leapfrog
-- arithmetic.
data NUTSTree = NUTSTree
  { ntThMinus :: VS.Vector Double
  , ntRMinus  :: VS.Vector Double
  , ntGMinus  :: VS.Vector Double
    -- ^ Phase 87.2b: minus 端点の ∇U = −∇logπ (leapfrog 勾配キャッシュ)。
    --   同方向の次の葉が始点勾配を再計算せずに済む (Stan の z_.g と同じ)。
  , ntThPlus  :: VS.Vector Double
  , ntRPlus   :: VS.Vector Double
  , ntGPlus   :: VS.Vector Double
    -- ^ Phase 87.2b: plus 端点の ∇U (同上)。
  , ntThPrime :: VS.Vector Double
  , ntN       :: Int
  , ntS       :: Bool
  , ntDiv     :: Bool
    -- ^ サブツリー中で divergent (|ΔH| > deltaMax) が発生したか
  , ntASum    :: !Double
    -- ^ Phase 87.2: Σ min(1, exp(H0 − H_leaf)) — Stan の accept_stat 蓄積。
    --   dual averaging はこの平均 ᾱ を学習する (旧: 1-step probe = 毎 draw
    --   余分な leapfrog+エネルギー評価を払う非標準の独自実装だった)。
  , ntANum    :: !Int
    -- ^ Phase 87.2: ᾱ の分母 (サブツリーの葉数・棄却葉も含む)。
  }

deltaMax :: Double
deltaMax = 1000.0

-- | U-turn check on Storable Vectors. @(θ⁺ − θ⁻) · r⁻ < 0@ or
-- @(θ⁺ − θ⁻) · r⁺ < 0@ ⇒ trajectory has begun to retrace itself.
--
-- Phase 90 A11-4①: 旧実装は @delta@ の共有 binding で stream fusion が切れ
-- delta ベクトルを毎回実体化していた (prof 実測: nuts_uturn が総 alloc の
-- 23.5%)。 2 つの内積を単一パス・確保なしで融合する。 加算順序は旧
-- 'VS.sum' (左畳み込み) と同一 = ビット同一。
uTurnVS
  :: VS.Vector Double -> VS.Vector Double
  -> VS.Vector Double -> VS.Vector Double -> Bool
uTurnVS thMinus rMinus thPlus rPlus = go 0 0 0
  where
    !n = VS.length thMinus
    go !d1 !d2 !j
      | j >= n    = d1 < 0 || d2 < 0
      | otherwise =
          let d = thPlus `VS.unsafeIndex` j - thMinus `VS.unsafeIndex` j
          in go (d1 + d * (rMinus `VS.unsafeIndex` j))
                (d2 + d * (rPlus  `VS.unsafeIndex` j))
                (j + 1)
{-# INLINE uTurnVS #-}

-- | Sample momentum @r ~ N(0, M)@ from the diagonal mass matrix
-- represented as @M⁻¹@. Per coordinate: @r_i = z / sqrt(M⁻¹_i)@,
-- @z ~ N(0,1)@. Storable-vector tight loop, no list allocation.
sampleMomentum :: PrimMonad m => VS.Vector Double -> Gen (PrimState m) -> m (VS.Vector Double)
sampleMomentum mInv gen = do
  let n = VS.length mInv
  VS.generateM n $ \i -> do
    z <- standard gen
    return (z / sqrt (mInv `VS.unsafeIndex` i))
{-# INLINE sampleMomentum #-}

-- ---------------------------------------------------------------------------
-- ツリービルダー
-- ---------------------------------------------------------------------------

buildTree
  :: forall m. PrimMonad m
  => (VS.Vector Double -> m (Double, VS.Vector Double))
     -- ^ 融合評価 (Phase 87.2b): θ ↦ (logπ(θ), ∇U(θ) = −∇logπ(θ))。 Phase 90
     --   A11-4①: chain 閉包に確保した arena/adj を再利用するため monadic。
  -> VS.Vector Double                         -- ^ Diagonal M⁻¹.
  -> Double                                   -- ^ Step size @ε@.
  -> VS.Vector Double                         -- ^ Position.
  -> VS.Vector Double                         -- ^ Momentum.
  -> VS.Vector Double                         -- ^ ∇U at position (キャッシュ)。
  -> Double                                   -- ^ @log u@ slice.
  -> Double                                   -- ^ 初期エネルギー @H0@ (ᾱ 用)。
  -> Int                                      -- ^ Direction (±1).
  -> Int                                      -- ^ Recursion depth.
  -> Gen (PrimState m)
  -> m NUTSTree
-- Phase 54.7b: PrimMonad 多相 (Phase 50) は SPECIALIZE が無いと dictionary 渡しで
-- mwc の uniform/standard が unbox されない (prof 実測で RNG 系 11%/alloc 25%)。
-- IO / ST の両具体型に特殊化して Phase 50 以前の機械語品質に戻す。
{-# SPECIALIZE buildTree
  :: (VS.Vector Double -> IO (Double, VS.Vector Double))
  -> VS.Vector Double -> Double -> VS.Vector Double -> VS.Vector Double
  -> VS.Vector Double
  -> Double -> Double -> Int -> Int -> Gen RealWorld -> IO NUTSTree #-}
{-# SPECIALIZE buildTree
  :: (VS.Vector Double -> ST s (Double, VS.Vector Double))
  -> VS.Vector Double -> Double -> VS.Vector Double -> VS.Vector Double
  -> VS.Vector Double
  -> Double -> Double -> Int -> Int -> Gen s -> ST s NUTSTree #-}
buildTree gradValU mInv eps theta r gU logU h0 dir depth gen
  | depth == 0 = do
      -- Phase 87.2b: 1-step leapfrog を融合評価でインライン化。 始点勾配は
      -- 端点キャッシュ (gU) を使い、 終点は (logπ, ∇U) を 1 回の融合評価で
      -- 取得 (旧: 葉ごとに grad 2 回 + logπ 1 回 = 始点勾配の再計算と
      -- エネルギー用 forward の重複を払っていた)。
      let !epsD    = fromIntegral dir * eps
          !halfEps = 0.5 * epsD
          rHalf  = {-# SCC "nuts_leapfrog_kick1" #-}
                   VS.zipWith (\ri gi -> ri - halfEps * gi) r gU
          theta' = {-# SCC "nuts_leapfrog_drift" #-}
                   VS.zipWith3 (\ti m_inv ri -> ti + epsD * m_inv * ri)
                               theta mInv rHalf
      (v', g') <- {-# SCC "nuts_gradval" #-} gradValU theta'
      let r'     = {-# SCC "nuts_leapfrog_kick2" #-}
                   VS.zipWith (\ri gi -> ri - halfEps * gi) rHalf g'
          h'  = {-# SCC "nuts_energy" #-} (negate v' + kineticMVS mInv r')
          n'  = if logU <= -h' then 1 else 0
          s'  = logU < deltaMax - h'
          divergent = not s'
          -- Phase 87.2: Stan の accept_stat = min(1, exp(H0 − H')) を葉ごとに
          -- 蓄積 (非有限は 0 = 棄却扱い)。
          a'  = let d = h0 - h'
                in if isNaN d then 0 else min 1 (exp (min 0 d))
      return NUTSTree
        { ntThMinus = theta', ntRMinus = r', ntGMinus = g'
        , ntThPlus  = theta', ntRPlus  = r', ntGPlus  = g'
        , ntThPrime = theta', ntN = n', ntS = s'
        , ntDiv = divergent
        , ntASum = a', ntANum = 1
        }
  | otherwise = do
      t1 <- buildTree gradValU mInv eps theta r gU logU h0 dir (depth - 1) gen
      if not (ntS t1) then return t1
      else do
        let (th0, r0, g0) = if dir == -1
              then (ntThMinus t1, ntRMinus t1, ntGMinus t1)
              else (ntThPlus  t1, ntRPlus  t1, ntGPlus  t1)
        t2 <- buildTree gradValU mInv eps th0 r0 g0 logU h0 dir (depth - 1) gen
        let n1 = ntN t1; n2 = ntN t2
        thPrime' <-
          if n1 == 0 then return (ntThPrime t2)
          else if n2 == 0 then return (ntThPrime t1)
          else do
            u <- {-# SCC "nuts_rng_uniform" #-} (uniform gen :: m Double)
            return $ if u < min 1.0 (fromIntegral n2 / fromIntegral n1)
                     then ntThPrime t2
                     else ntThPrime t1
        let (minus', rMinus', gMinus', plus', rPlus', gPlus') = if dir == -1
              then (ntThMinus t2, ntRMinus t2, ntGMinus t2,
                    ntThPlus t1, ntRPlus t1, ntGPlus t1)
              else (ntThMinus t1, ntRMinus t1, ntGMinus t1,
                    ntThPlus t2, ntRPlus t2, ntGPlus t2)
            s' = ntS t2 && not ({-# SCC "nuts_uturn" #-} uTurnVS minus' rMinus' plus' rPlus')
        return NUTSTree
          { ntThMinus = minus', ntRMinus = rMinus', ntGMinus = gMinus'
          , ntThPlus  = plus',  ntRPlus  = rPlus',  ntGPlus  = gPlus'
          , ntThPrime = thPrime', ntN = n1 + n2, ntS = s'
          , ntDiv = ntDiv t1 || ntDiv t2
          , ntASum = ntASum t1 + ntASum t2
          , ntANum = ntANum t1 + ntANum t2
          }

-- ---------------------------------------------------------------------------
-- Streaming hook
-- ---------------------------------------------------------------------------

-- | Per-iteration sample event emitted by 'nutsStream'.
--
-- Used by callers that want to observe MCMC progress as it happens
-- (e.g. live trace plots, real-time R-hat / ESS updates over the wire).
-- The callback receives one event per iteration of the outer loop,
-- including burn-in iterations (distinguished by 'seIsBurnIn').
--
-- The 'seParams' values are in the **constrained** parameter space,
-- matching the convention used in 'chainSamples'. Burn-in events are
-- /not/ included in 'chainSamples', but are still streamed via the
-- callback so the UI can show warmup progress and adaptation.
data SampleEvent = SampleEvent
  { seIter      :: !Int      -- ^ 0-based iteration index (burn-in inclusive).
                              --   Ranges over @[0 .. nutsBurnIn + nutsIterations - 1]@.
  , seIsBurnIn  :: !Bool     -- ^ True if @seIter < nutsBurnIn@.
  , seParams    :: !Params   -- ^ Current sample (constrained space).
  , seEnergy    :: !Double   -- ^ Hamiltonian H0 at the start of this iteration.
  , seDivergent :: !Bool     -- ^ Whether this iteration's trajectory diverged.
  , seAccepted  :: !Bool     -- ^ Whether the proposal was accepted
                              --   (@proposedU /= currentU@).
  , seStepSize  :: !Double   -- ^ Current ε (after this iteration's adaptation).
  , seTreeDepth :: !Int      -- ^ Phase 85.6: この draw で実行された doubling 回数
                              --   (leapfrog 数 ≈ 2^depth・warmup 固定費の診断用)。
  , seAcceptStat :: !Double  -- ^ Phase 87.1: この draw の mean accept-stat α
                              --   (dual averaging が target と比較する統計・
                              --   'seAccepted' の bool とは別物)。ε̄ 収束診断用。
  }

-- ---------------------------------------------------------------------------
-- NUTS サンプラー
-- ---------------------------------------------------------------------------

-- | NUTS sampler for a polymorphic HBM model ('ModelP').
-- 軌道長は U-Turn 判定で自動決定。
--
-- This is a thin wrapper around 'nutsStream' with a no-op callback.
-- Use 'nutsStream' directly if you want per-iteration progress
-- (e.g. for live UI updates over a WebSocket / SSE channel).
nuts :: PrimMonad m => ModelP r -> NUTSConfig -> Params -> Gen (PrimState m) -> m Chain
{-# SPECIALIZE nuts :: ModelP r -> NUTSConfig -> Params -> Gen RealWorld -> IO Chain #-}
{-# SPECIALIZE nuts :: ModelP r -> NUTSConfig -> Params -> Gen s -> ST s Chain #-}
nuts m cfg initC gen = nutsStream m cfg initC gen (\_ -> pure ())

-- | NUTS sampler with a per-iteration callback. Identical to 'nuts'
-- semantically; in addition, calls @onSample event@ once per outer
-- loop iteration (burn-in inclusive). The callback runs synchronously
-- inside the sampler loop, so it should return quickly (push events to
-- a queue rather than do IO of unbounded latency).
nutsStream :: forall r m. PrimMonad m
           => ModelP r -> NUTSConfig -> Params -> Gen (PrimState m)
           -> (SampleEvent -> m ())
           -> m Chain
{-# SPECIALIZE nutsStream
  :: ModelP r -> NUTSConfig -> Params -> Gen RealWorld
  -> (SampleEvent -> IO ()) -> IO Chain #-}
{-# SPECIALIZE nutsStream
  :: ModelP r -> NUTSConfig -> Params -> Gen s
  -> (SampleEvent -> ST s ()) -> ST s Chain #-}
nutsStream m cfg initC gen onSample = do
  let names      = sampleNames m
      trMap      = getTransforms m
      transList  = [Map.findWithDefault errT n trMap | n <- names]
      errT       = error "nuts: missing transform"

      -- Initial unconstrained position as a Storable Vector. The hot
      -- loop never touches 'Params' (= Map); we only convert at the
      -- boundary to record samples.
      initUV0 :: VS.Vector Double
      initUV0 = VS.fromList
        [ toUnconstrained t (Map.findWithDefault 0 n initC)
        | (n, t) <- zip names transList ]

      total   = nutsBurnIn cfg + nutsIterations cfg
      doAdapt = nutsAdaptStepSize cfg && nutsBurnIn cfg > 0

      -- Vector-native log target density. Phase 54.4d/54.6: エネルギー評価も
      -- 'compileLogPUV' で静的部分 (名前→index 解決込み) を 1 度だけ前処理した
      -- compiled closure を全 tree node で再利用する (旧: 毎回
      -- 'logJointUnconstrained' の Free walk + per-obs スカラ logDensityObs)。
      logPiFn :: VS.Vector Double -> Double
      logPiFn = compileLogPUV m names transList

      -- Vector-native gradient. Phase 54.4b/54.6: モデル構造は draw 間で不変
      -- ゆえ 'compileGradUV' で静的部分を **1 度だけ**前処理し、 返った
      -- vector-native クロージャを全 leapfrog で再利用する (VS↔list 変換なし)。
      gradV :: VS.Vector Double -> VS.Vector Double
      gradV = compileGradUV m names transList
      gradFn :: VS.Vector Double -> VS.Vector Double
      gradFn uv = VS.map negate (gradV uv)

      toConstrained :: VS.Vector Double -> Params
      toConstrained uv = Map.fromList
        [ (n, fromUnconstrained t (uv `VS.unsafeIndex` i))
        | (i, (n, t)) <- zip [0..] (zip names transList) ]

  -- Phase 87.2b: 値+勾配の融合評価 (JAX value_and_grad 相当)。 tree の葉が
  -- leapfrog 最終勾配とエネルギーを同一点で二重評価していた重複を除去。
  -- Phase 90 A11-4①: 'compileGradValUVM' は forward/随伴 arena を **この chain
  -- 閉包生成時に 1 度だけ**確保して全 leapfrog で再利用する (per-call 34k×2
  -- セル確保 + GC churn を除去)。 chain ごとに別 'nutsStream' 呼出 = 別バッファ
  -- ゆえ chain 横断並列 ('nutsChainsPure'/'nutsChainsStream') と非干渉。
  -- Phase 94 A4-2: 各 chain の初期位置に一様 jitter (funnel 首の whole-chain 崩壊対策)。
  -- j=0 なら initUV0 をそのまま (従来挙動)。 gen は chain 固有ゆえ chain ごと独立。
  initUV <- let j = nutsInitJitter cfg
            in if j <= 0 then pure initUV0
               else VS.mapM (\x -> do u <- uniform gen
                                      pure (x + (u * 2 - 1) * j)) initUV0
  gradValV <- compileGradValUVM m names transList
  let gradValU :: VS.Vector Double -> m (Double, VS.Vector Double)
      gradValU uv = do
        (v, g) <- gradValV uv
        pure (v, VS.map negate g)

  samplesRef    <- newMutVar []
  energyRef     <- newMutVar ([] :: [Double])
  divergenceRef <- newMutVar ([] :: [Int])
  depthRef      <- newMutVar ([] :: [Int])   -- Phase 85.3: per-draw tree depth
  acceptedRef   <- newMutVar (0 :: Int)
  -- Phase 85.6c: 初期 ε の較正 (Stan Algorithm 4・doAdapt 時のみ)。
  eps0 <- if doAdapt && nutsInitEpsSearch cfg
            then findReasonableEpsilon gradFn logPiFn
                   (VS.replicate (length names) 1.0) (nutsStepSize cfg) initUV gen
            else pure (nutsStepSize cfg)
  daRef         <- newMutVar (initDualAvg eps0)

  -- B11: Stan-style multi-window diagonal mass-matrix adaptation.
  --
  -- Schedule (warmup W):
  --   * init buffer  (max 75 / W÷7 iters): step-size adapt only, M = I
  --   * window phase: doubling windows 25 → 50 → 100 → 200 → ...
  --       At the end of each window: update M⁻¹ from window's
  --       Welford-accumulated diagonal variance, restart dual averaging.
  --   * term buffer  (max 50 / W÷10 iters): M frozen, step-size adapt
  --       continues to converge ε under the final geometry.
  let nParams       = length names
      adaptM        = nutsAdaptMass cfg && nutsBurnIn cfg > 0
      (windowEnds, initBuf, _termBuf) = stanWindows (nutsBurnIn cfg)
      windowPhaseEnd = if null windowEnds then 0 else last windowEnds
  mInvRef     <- newMutVar (VS.replicate nParams 1.0)
  welfordRef  <- newMutVar (emptyWelford nParams)

  let step :: VS.Vector Double -> Double -> Int -> VS.Vector Double
           -> m (VS.Vector Double, Double, Double, Bool, Int)
      step mInv eps maxDep currentU = do
        -- r ~ N(0, M)  ⇔  r_i = sqrt(M_ii) * z = z / sqrt(M⁻¹_ii)
        r0 <- {-# SCC "nuts_sampleMomentum" #-} sampleMomentum mInv gen
        u0 <- {-# SCC "nuts_rng_uniform" #-} (uniform gen :: m Double)
        -- Phase 87.2b: 始点の (logπ, ∇U) を融合評価 1 回で取得。 値は H0 に、
        -- 勾配は両方向の最初の葉の始点キャッシュに使う。
        (v0, gU0) <- {-# SCC "nuts_gradval0" #-} gradValU currentU
        let h0   = negate v0 + kineticMVS mInv r0
            logU = log u0 - h0
        let tree0 = NUTSTree
              { ntThMinus = currentU, ntRMinus = r0, ntGMinus = gU0
              , ntThPlus  = currentU, ntRPlus  = r0, ntGPlus  = gU0
              , ntThPrime = currentU, ntN = 1, ntS = True
              , ntDiv = False
              , ntASum = 0, ntANum = 0
              }
        let doubleTree tree j =
              if not (ntS tree) then return tree
              else do
                u <- {-# SCC "nuts_rng_uniform" #-} (uniform gen :: m Double)
                let dir = if u < 0.5 then -1 else 1 :: Int
                    (th0, r0', g0') = if dir == -1
                      then (ntThMinus tree, ntRMinus tree, ntGMinus tree)
                      else (ntThPlus  tree, ntRPlus  tree, ntGPlus  tree)
                subtree <- {-# SCC "nuts_buildTree" #-}
                  buildTree gradValU mInv eps th0 r0' g0' logU h0 dir j gen
                let n1 = ntN tree; n2 = ntN subtree
                thPrime' <-
                  if not (ntS subtree) || n2 == 0
                  then return (ntThPrime tree)
                  else do
                    u2 <- {-# SCC "nuts_rng_uniform" #-} (uniform gen :: m Double)
                    return $ if u2 < min 1.0 (fromIntegral n2 / fromIntegral n1)
                             then ntThPrime subtree
                             else ntThPrime tree
                let (minus', rMinus', gMinus', plus', rPlus', gPlus') = if dir == -1
                      then (ntThMinus subtree, ntRMinus subtree, ntGMinus subtree,
                            ntThPlus  tree,    ntRPlus  tree,    ntGPlus  tree)
                      else (ntThMinus tree,    ntRMinus tree,    ntGMinus tree,
                            ntThPlus  subtree, ntRPlus  subtree, ntGPlus  subtree)
                    s' = ntS subtree && not ({-# SCC "nuts_uturn" #-} uTurnVS minus' rMinus' plus' rPlus')
                return NUTSTree
                  { ntThMinus = minus', ntRMinus = rMinus', ntGMinus = gMinus'
                  , ntThPlus  = plus',  ntRPlus  = rPlus',  ntGPlus  = gPlus'
                  , ntThPrime = thPrime', ntN = n1 + n2, ntS = s'
                  , ntDiv = ntDiv tree || ntDiv subtree
                  , ntASum = ntASum tree + ntASum subtree
                  , ntANum = ntANum tree + ntANum subtree
                  }
        -- Phase 85.3: 実行された doubling 回数 = tree depth (PyMC の
        -- tree_depth 相当・leapfrog 数 ≈ 2^depth) を数える。
        let doubleTreeD (tree, !dep) j =
              if not (ntS tree) then return (tree, dep)
              else do
                t' <- doubleTree tree j
                return (t', dep + 1 :: Int)
        (finalTree, treeDepth) <-
          foldM doubleTreeD (tree0, 0) [0 .. maxDep - 1]
        -- Phase 87.2: alpha は Stan の accept_stat = tree 全葉の
        -- min(1, exp(H0−H')) 平均 (buildTree で蓄積)。 旧 1-step probe
        -- (毎 draw 余分な leapfrog + エネルギー評価・非標準の独自実装) を廃止。
        let proposedU = ntThPrime finalTree
            alpha     = if ntANum finalTree > 0
                          then ntASum finalTree / fromIntegral (ntANum finalTree)
                          else 0
        when (proposedU /= currentU) $ modifyMutVar' acceptedRef (+1)
        return (proposedU, alpha, h0, ntDiv finalTree, treeDepth)

  let loop 0 currentU _eps = return currentU
      loop i currentU eps = do
        mInv <- readMutVar mInvRef
        let isBurnIn   = i > nutsIterations cfg
            -- iteration index from start (1-based); total counts down.
            iterIdx    = total - i + 1
            -- Phase 85.6: M 初回更新前 (M=I) は深い木を掘らない (init 期の
            -- draw は捨てる区間・radon で warmup leapfrog の 68% を占めた)。
            firstMUpd  = case windowEnds of { (w : _) -> w; [] -> 0 }
            maxDep
              | adaptM && isBurnIn && iterIdx <= firstMUpd
              , Just cap <- nutsWarmupInitMaxDepth cfg =
                  min (nutsMaxDepth cfg) cap
              | otherwise = nutsMaxDepth cfg
            -- Inside the window phase: collect samples for Welford.
            inWindowPhase = adaptM && isBurnIn
                            && iterIdx > initBuf
                            && iterIdx <= windowPhaseEnd
            -- This iteration ends a window: update M, restart DA.
            isWindowEnd   = adaptM && isBurnIn && iterIdx `elem` windowEnds
        (nextU, alpha, h0, divergent, treeDepth) <- step mInv eps maxDep currentU
        when inWindowPhase $
          modifyMutVar' welfordRef (\w -> {-# SCC "nuts_welford" #-} welfordAddVS w nextU)
        -- Phase 86: window 末に M を更新したら、 Stan (adapt_diag_e_nuts の
        -- init_stepsize + restart) と同じく**新 metric の下で ε を再較正**
        -- (Algorithm 4) して DA を restart する。 旧実装は鋸歯振動中の瞬間値
        -- ε を anchor (μ = log 10ε) にしており、 M 更新直後に ε が幾何と桁で
        -- 乖離すると次 window 丸ごと深掘りする (radon seed=1 実測で window
        -- [150,250) が depth 9.9・101k leapfrog = warmup 全体の 76%)。
        recalEps <- if isWindowEnd
          then do
            w <- readMutVar welfordRef
            -- Reset Welford for the next window (window-local variance).
            writeMutVar welfordRef (emptyWelford nParams)
            if wN w >= 5  -- need a few samples to be meaningful
              then do
                let mInv' = welfordMInvVS w
                writeMutVar mInvRef mInv'
                if doAdapt && nutsInitEpsSearch cfg
                  then
                    -- Phase 87.1: **最終 window 末 (term buffer 直前) は restart
                    -- しない** (M のみ更新・DA 継続 = PyMC の連続 DA と同じ挙動)。
                    -- restart すると DA が m=1 の暴れ期からやり直しになり、 50
                    -- draw の term buffer では ε̄ が鋸歯の暴れを拾って過小に着地
                    -- する (radon 実測: ε 振動 0.035-1.37・ε̄=0.18・sampling α
                    -- 0.95/depth 5。 PyMC は restart なしで振動 0.14-0.50・
                    -- ε 0.23-0.34・depth 4/α 0.80-0.88)。 中間 window 末の
                    -- recal+restart (Phase 86・爆発対策) は維持 — この時点まで
                    -- に M はほぼ収束しており継続 DA の ε がそのまま通用する。
                    if iterIdx == windowPhaseEnd
                      then pure Nothing
                      else do
                        epsNew <- findReasonableEpsilon gradFn logPiFn mInv' eps nextU gen
                        writeMutVar daRef (initDualAvg epsNew)
                        pure (Just epsNew)
                  else do
                    -- 旧挙動 (opt-out 時): 現 ε anchor で restart。
                    writeMutVar daRef (initDualAvg eps)
                    pure Nothing
              else pure Nothing
          else pure Nothing
        eps' <- case recalEps of
          -- Stan と同じく restart 直後はこの draw の accept 統計を学習しない
          -- (旧 metric 下の α で較正済 anchor を汚さない)。
          Just epsNew -> pure epsNew
          Nothing
            | doAdapt && isBurnIn -> do
                da <- readMutVar daRef
                let da' = {-# SCC "nuts_dualavg" #-} updateDualAvg (nutsTargetAccept cfg) alpha da
                writeMutVar daRef da'
                return (exp (daLogEps da'))
            | otherwise -> do
                da <- readMutVar daRef
                let epsBar = if doAdapt && not isBurnIn && i == nutsIterations cfg
                             then exp (daLogEpsBar da)
                             else eps
                return epsBar
        let nextParams = {-# SCC "nuts_toConstrained" #-} toConstrained nextU
        if not isBurnIn
          then do
            modifyMutVar' samplesRef (nextParams :)
            modifyMutVar' energyRef  (h0 :)
            modifyMutVar' depthRef   (treeDepth :)
            when divergent $
              modifyMutVar' divergenceRef
                ((nutsIterations cfg - i) :)
          else return ()
        -- Phase 9.1a: per-iteration callback for streaming UIs.
        -- 0-based iter index running 0 .. total-1; isBurnIn for first nutsBurnIn.
        onSample SampleEvent
          { seIter      = total - i
          , seIsBurnIn  = isBurnIn
          , seParams    = nextParams
          , seEnergy    = h0
          , seDivergent = divergent
          , seAccepted  = nextU /= currentU
          , seStepSize  = eps'
          , seTreeDepth = treeDepth
          , seAcceptStat = alpha
          }
        loop (i - 1) nextU eps'

  _ <- loop total initUV eps0
  samples  <- fmap reverse (readMutVar samplesRef)
  energies <- fmap reverse (readMutVar energyRef)
  divs     <- fmap reverse (readMutVar divergenceRef)
  depths   <- fmap reverse (readMutVar depthRef)
  accepted <- readMutVar acceptedRef
  return Chain
    { chainSamples     = samples
    , chainAccepted    = accepted
    , chainTotal       = total
    , chainEnergy      = energies
    , chainDivergences = divs
    , chainTreeDepths  = depths
    }

-- ---------------------------------------------------------------------------
-- B11: Mass-matrix adaptation helpers
-- ---------------------------------------------------------------------------

-- | Welford online accumulator for diagonal sample variance.
--
-- Per-coordinate one-pass mean / M2; variance = M2 / (n − 1).
-- Used by Stan-style window adaptation to estimate posterior variance
-- without keeping the raw samples around.
-- | Plain (non-record) constructor: @Welford n mean m2@. Kept positional
-- because the @m2@ field is only ever pattern-matched, never read via a
-- selector (record syntax would generate an unused-binding warning).
-- | Storable-Vector Welford. The previous list-based form allocated
-- four @[Double]@ vectors per add (warmup ~500 iters × 4 cells = 10K
-- list cells per fit) and was hot during the mass-matrix adaptation
-- window phase.
data Welford = Welford !Int !(VS.Vector Double) !(VS.Vector Double)

wN :: Welford -> Int
wN (Welford n _ _) = n

emptyWelford :: Int -> Welford
emptyWelford p = Welford 0 (VS.replicate p 0) (VS.replicate p 0)

welfordAddVS :: Welford -> VS.Vector Double -> Welford
welfordAddVS (Welford n mean m2) x =
  let !n'   = n + 1
      !nD   = fromIntegral n' :: Double
      !d    = VS.zipWith (-) x mean
      !mean' = VS.zipWith (\me di -> me + di / nD) mean d
      !d2   = VS.zipWith (-) x mean'
      !m2'  = VS.zipWith3 (\m2i d1 d22 -> m2i + d1 * d22) m2 d d2
  in Welford n' mean' m2'

-- | Stan-regularised diagonal @M⁻¹@ from a Welford accumulator.
--
-- @σ̂² = (n / (n+5)) · sample_var + 1e-3 · (5 / (n+5))@.
-- The 1e-3 shrinkage target keeps the estimator non-degenerate when
-- @n@ is tiny; for moderate @n@ it reduces to the sample variance.
--
-- /Convention/: following Stan/blackjax, @M⁻¹@ stores the posterior
-- covariance directly (so @M⁻¹_ii = σ̂²_i@). With kinetic energy
-- @½ rᵀ M⁻¹ r@ and @r ~ N(0, M)@, this gives a per-leapfrog position
-- step @ε · σ̂_i@ in absolute units (i.e. @ε@ in posterior-sd units),
-- which is what NUTS needs for tree depth ~ @1/ε@.
welfordMInvVS :: Welford -> VS.Vector Double
welfordMInvVS (Welford n _ m2)
  | n < 2     = VS.replicate (VS.length m2) 1.0
  | otherwise =
      let nD     = fromIntegral n :: Double
          k      = 5.0 :: Double
          weight = nD / (nD + k)
          target = 1e-3
      in VS.map (\v -> let raw = v / (nD - 1)
                       in max 1e-12 (weight * raw + (1 - weight) * target))
                m2

-- | Stan-style adaptation schedule for a warmup of @W@ iterations.
--
-- Returns @(windowEndIters, initBuffer, termBuffer)@ where
-- @windowEndIters@ are 1-based iteration indices at which to update the
-- mass matrix, and @initBuffer@ / @termBuffer@ are the no-update
-- prefix / suffix lengths (Stan defaults: 15% / 10%, with floors of 75
-- and 50 iters respectively). Windows double in size starting from 25;
-- the last window absorbs any remainder.
--
-- For @W = 500@: @initBuffer = 75@, @termBuffer = 50@, middle = 375,
-- windows = @[100, 150, 250, 450]@.
stanWindows :: Int -> ([Int], Int, Int)
stanWindows w
  | w < 50    = ([], w, 0)
  | otherwise =
      let initB  = max 75 (w `div` 7)
          termB  = max 50 (w `div` 10)
          midLen = w - initB - termB
      in if midLen < 25
         then ([], w, 0)
         else (genW (initB + 1) midLen 25, initB, termB)
  where
    genW _     0    _     = []
    genW start rest wsize
      | wsize * 2 > rest =
          -- Next doubled window wouldn't fit; absorb the remainder.
          [start + rest - 1]
      | otherwise =
          let endIter = start + wsize - 1
          in endIter : genW (endIter + 1) (rest - wsize) (wsize * 2)

-- | Run 'nuts' on @numChains@ parallel chains.
nutsChains :: ModelP r -> NUTSConfig -> Int -> Params -> GenIO -> IO [Chain]
nutsChains m cfg numChains initC baseGen = do
  gens <- replicateM numChains (spawnGen baseGen)
  mapConcurrently (\g -> nuts m cfg initC g) gens

-- ---------------------------------------------------------------------------
-- Phase 50: 純粋 (ST + seed) ラッパ
--
-- 'nuts' を 'ST' で走らせ 'runST' で閉じることで、 **seed → 確定 'Chain'** の
-- 純粋関数にする (同 seed → ビット同一・IO 不要)。 mwc は 'PrimMonad' 汎用ゆえ
-- ロジックは 50.2 で一般化した 'nuts' をそのまま使う。
-- ---------------------------------------------------------------------------

-- | 純粋・決定的な単一 NUTS chain。 同じ @seed@ なら必ず同じ 'Chain' を返す。
nutsPure :: ModelP r -> NUTSConfig -> Params -> Word32 -> Chain
nutsPure m cfg initC seed =
  runST (initialize (V.singleton seed) >>= nuts m cfg initC)

-- | 親 @seed@ から chain ごとの child seed 列を純粋に導出する (Phase 61.1 で
-- 'nutsChainsPure' から抽出)。 pure 経路と IO 経路 ('nutsChainsStream') が
-- **同じ seed 列**を共有することで両経路のビット一致を保証する (複製すると drift)。
chainSeeds :: Word32 -> Int -> [Word32]
chainSeeds seed numChains = runST $ do
  g <- initialize (V.singleton seed)
  replicateM numChains (uniform g)

-- | 純粋・決定的な multi-chain。 親 @seed@ から子 seed を純粋に導出 (各 chain は
-- 別 'runST') し、 chain 横断を @parList rdeepseq@ で**最初から**並列評価する
-- (純粋性と並列性は直交。 @+RTS -N@ でマルチコア。 結果は spark/コア数に依らずビット同一)。
nutsChainsPure :: ModelP r -> NUTSConfig -> Int -> Params -> Word32 -> [Chain]
nutsChainsPure m cfg numChains initC seed =
  let chains = [ nutsPure m cfg initC s | s <- chainSeeds seed numChains ]
  in chains `using` parList rdeepseq

-- | 'nutsChainsPure' の IO 版 (Phase 61.1): 同じ child seed 規約
-- ('chainSeeds') で chain ごとに 'nutsStream' を回し、 chain index 付き
-- callback で進捗を観測できるようにする。 chain 横断は 'mapConcurrently'
-- (既存 'nutsChains' と同様・実 OS スレッド並列には @-threaded +RTS -N@)。
--
-- mwc の 'PrimMonad' 汎用性 + Phase 50 で実証済の ST/IO ビット同一により、
-- no-op callback なら結果は @nutsChainsPure m cfg n initC seed@ と
-- **ビット一致**する (回帰テストで固定)。
nutsChainsStream :: ModelP r -> NUTSConfig -> Int -> Params -> Word32
                 -> (Int -> SampleEvent -> IO ())
                 -> IO [Chain]
nutsChainsStream m cfg numChains initC seed onSample =
  mapConcurrently
    (\(i, s) -> do
        g <- initialize (V.singleton s)
        nutsStream m cfg initC g (onSample i))
    (zip [0 ..] (chainSeeds seed numChains))
