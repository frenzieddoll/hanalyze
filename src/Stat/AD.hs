{-# LANGUAGE RankNTypes #-}
-- | 自動微分 (AD) を用いた正確な勾配計算と HMC 統合。
--
-- Numeric.AD (ekmett/ad) による逆モード AD で勾配を計算する。
-- 数値微分 (中心差分) より正確で、パラメータ数が少ない場合 (< 100) は
-- 同程度の速度で動作する。
--
-- == 使い方
--
-- ユーザーは log p(θ, y) を \"Floating 多相関数\" として記述する。
-- 観測値は固定 Double として @realToFrac@ で型変換する:
--
-- @
-- import Stat.AD
-- import Stat.Distribution (Transform (..))
--
-- -- θ = [mu, sigma]
-- myLogJoint :: [Double] -> LogJointF
-- myLogJoint obs [mu, sigma] =
--   logNormalF 0 10 mu                          -- prior: μ ~ N(0,10)
--   + logExpF 1 sigma                           -- prior: σ ~ Exp(1)
--   + sum [ logNormalObsF y mu sigma | y <- obs ] -- lik
--
-- chain <- hmcAD (myLogJoint myData)
--                [UnconstrainedT, PositiveT]
--                defaultHMCConfig
--                ["mu","sigma"]
--                (Map.fromList [("mu",0),("sigma",1)])
--                gen
-- @
module Stat.AD
  ( -- * 多相対数密度関数 (log-joint 記述用)
    LogJointF
  , logNormalF
  , logNormalObsF
  , logExpF
  , logGammaF
  , logBetaF
  , logPoissonObsF
  , logBernoulliObsF
    -- * AD 勾配計算
  , gradAD
  , gradADU
    -- * HMC AD 版
  , hmcAD
  , hmcADChains
  ) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (forM, replicateM)
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Numeric.AD.Mode.Forward (grad)
import System.Random.MWC (GenIO, uniform)
import System.Random.MWC.Distributions (standard)

import MCMC.Core (Chain (..), spawnGen)
import MCMC.HMC (HMCConfig (..), leapfrogWith, kinetic)
import Stat.Distribution (Transform (..), toUnconstrained, fromUnconstrained)

type Params = Map.Map Text Double

-- | Floating 多相の log-joint 関数の型エイリアス。
-- @[a]@ は制約付きパラメータベクトル (constrained space)。
type LogJointF = forall a. Floating a => [a] -> a

-- ---------------------------------------------------------------------------
-- 多相対数密度関数
-- ---------------------------------------------------------------------------

-- | log N(x; μ₀, σ₀) — μ₀, σ₀ は固定 Double ハイパーパラメータ。
logNormalF :: Floating a => Double -> Double -> a -> a
logNormalF mu0 sig0 x =
  let mu  = realToFrac mu0
      sig = realToFrac sig0
  in negate (0.5 * log (2 * pi)) - log sig - 0.5 * ((x - mu) / sig) ^ (2::Int)

-- | log N(y_obs; μ, σ) — y_obs は固定観測値 Double、μ と σ は AD 型。
logNormalObsF :: Floating a => Double -> a -> a -> a
logNormalObsF y_obs mu sig =
  let y = realToFrac y_obs
  in negate (0.5 * log (2 * pi)) - log sig - 0.5 * ((y - mu) / sig) ^ (2::Int)

-- | log Exp(x; rate) — rate は固定 Double。
logExpF :: Floating a => Double -> a -> a
logExpF rate0 x =
  let r = realToFrac rate0
  in log r - r * x

-- | log Gamma(x; shape, rate) — shape と rate は固定 Double。
-- log p(x) = (α-1)log x − βx + α log β − log Γ(α)
-- log Γ(α) は Stirling 近似 (α を固定定数として計算)。
logGammaF :: Floating a => Double -> Double -> a -> a
logGammaF shape0 rate0 x =
  let a   = realToFrac shape0
      b   = realToFrac rate0
      lgA = realToFrac (stirlingLogGamma shape0)
  in (a - 1) * log x - b * x + a * log b - lgA

-- | log Beta(x; α, β) — α, β は固定 Double。
-- log p(x) = (α-1)log x + (β-1)log(1-x) − log B(α,β)
logBetaF :: Floating a => Double -> Double -> a -> a
logBetaF alpha0 beta0 x =
  let a   = realToFrac alpha0
      b   = realToFrac beta0
      lbB = realToFrac (stirlingLogGamma alpha0 + stirlingLogGamma beta0
                        - stirlingLogGamma (alpha0 + beta0))
  in (a - 1) * log x + (b - 1) * log (1 - x) - lbB

-- | log Poisson(k | λ) — k は固定観測値 (整数として丸める)、λ は AD 型。
logPoissonObsF :: Floating a => Double -> a -> a
logPoissonObsF y_obs lam =
  let k  = fromIntegral (round y_obs :: Int) :: Double
      lf = realToFrac (logFactorial (round y_obs :: Int))
  in realToFrac k * log lam - lam - lf

-- | log Bernoulli(y | p) — y は固定 0/1 観測値、p は AD 型。
logBernoulliObsF :: Floating a => Double -> a -> a
logBernoulliObsF y_obs p
  | y_obs > 0.5 = log p
  | otherwise   = log (1 - p)

-- ---------------------------------------------------------------------------
-- AD 勾配計算
-- ---------------------------------------------------------------------------

-- | constrained 空間の log-joint の勾配を AD で計算する。
--
-- @
-- gradAD logJoint [1.0, 0.5]  -- [∂/∂θ₁, ∂/∂θ₂]
-- @
gradAD :: LogJointF -> [Double] -> [Double]
gradAD f xs = grad f xs

-- | unconstrained 空間の log-joint (制約変換 + Jacobian 込み) の AD 勾配。
--
-- 内部で各パラメータに制約変換を適用してから 'gradAD' を呼ぶ。
gradADU :: LogJointF -> [Transform] -> [Double] -> [Double]
gradADU logJointC transforms us =
  grad (logJointUF transforms logJointC) us

-- ---------------------------------------------------------------------------
-- 制約変換 (Floating 多相版)
-- ---------------------------------------------------------------------------

-- | unconstrained パラメータ → constrained パラメータ (Floating 多相)。
invTransformF :: Floating a => Transform -> a -> a
invTransformF UnconstrainedT u = u
invTransformF PositiveT      u = exp u
invTransformF UnitIntervalT  u = 1 / (1 + exp (-u))  -- sigmoid

-- | log |∂θ/∂u| — Jacobian 対数行列式成分 (Floating 多相)。
logJacF :: Floating a => Transform -> a -> a
logJacF UnconstrainedT _ = 0
logJacF PositiveT      u = u                     -- log(exp u) = u
logJacF UnitIntervalT  u =
  let p = 1 / (1 + exp (-u))
  in log p + log (1 - p)                         -- log σ(u)(1−σ(u))

-- | unconstrained 空間での log-joint (制約変換 + Jacobian 補正込み)。
logJointUF :: Floating a => [Transform] -> LogJointF -> [a] -> a
logJointUF transforms logJointC us =
  let thetas = zipWith invTransformF transforms us
      logJac  = sum (zipWith logJacF transforms us)
  in logJointC thetas + logJac

-- ---------------------------------------------------------------------------
-- HMC AD 版サンプラー
-- ---------------------------------------------------------------------------

-- | AD 勾配を使った HMC サンプラー。
--
-- 既存の 'MCMC.HMC.hmc' と同じアルゴリズムだが、勾配を 'Numeric.AD.grad'
-- で正確に計算する。ユーザーは log-joint を 'LogJointF' 形式で記述する。
hmcAD
  :: LogJointF    -- ^ log p(θ, y) の Floating 多相関数 (constrained 空間)
  -> [Transform]  -- ^ 各パラメータの制約種別 (パラメータ名リストと同じ順)
  -> HMCConfig
  -> [Text]       -- ^ パラメータ名リスト (初期値 Map のキーと対応)
  -> Params       -- ^ 初期値 (constrained 空間)
  -> GenIO
  -> IO Chain
hmcAD logJointC transforms cfg names initC gen = do
  let total   = hmcBurnIn cfg + hmcIterations cfg
      -- Unconstrained log-joint
      logJU u = logJointUF transforms logJointC
                  [Map.findWithDefault 0 n u | n <- names]
      -- AD gradient function: leapfrogWith の規約は ∇U = -∇logπ なので符号を反転
      gradFn ns paramsU =
        let xs = [Map.findWithDefault 0 n paramsU | n <- ns]
        in map negate (grad (logJointUF transforms logJointC) xs)
      -- Initial unconstrained params
      initU = Map.fromList
        [ (n, toUnconstrained t v)
        | (n, t) <- zip names transforms
        , Just v <- [Map.lookup n initC]
        ]

  samplesRef  <- newIORef []
  acceptedRef <- newIORef (0 :: Int)

  let step currentU = do
        r <- forM names (\_ -> standard gen)
        let (proposedU, rFinal) =
              leapfrogWith gradFn names
                           (hmcStepSize cfg) (hmcLeapfrogSteps cfg)
                           currentU r
            logAlpha = (logJU proposedU - kinetic rFinal)
                     - (logJU currentU  - kinetic r)
        u <- uniform gen
        if log (u :: Double) < logAlpha
          then do modifyIORef' acceptedRef (+1); return proposedU
          else return currentU

  let loop 0 currentU = return currentU
      loop i currentU = do
        nextU <- step currentU
        when (i <= hmcIterations cfg) $
          modifyIORef' samplesRef
            (Map.fromList
               [ (n, fromUnconstrained t (Map.findWithDefault 0 n nextU))
               | (n, t) <- zip names transforms
               ] :)
        loop (i - 1) nextU

  _ <- loop total initU
  samples  <- fmap reverse (readIORef samplesRef)
  accepted <- readIORef acceptedRef
  return Chain
    { chainSamples  = samples
    , chainAccepted = accepted
    , chainTotal    = total
    , chainEnergy   = []
    }
  where
    when True  action = action
    when False _      = return ()

-- | hmcAD を numChains 本並列実行する。
hmcADChains
  :: LogJointF
  -> [Transform]
  -> HMCConfig
  -> Int
  -> [Text]
  -> Params
  -> GenIO
  -> IO [Chain]
hmcADChains logJointC transforms cfg numChains names initC baseGen = do
  gens <- replicateM numChains (spawnGen baseGen)
  mapConcurrently (\g -> hmcAD logJointC transforms cfg names initC g) gens

-- ---------------------------------------------------------------------------
-- 数値ユーティリティ
-- ---------------------------------------------------------------------------

-- Stirling 近似による log Γ(z) — z は固定 Double ハイパーパラメータ用
stirlingLogGamma :: Double -> Double
stirlingLogGamma z
  | z < 0.5   = log pi - log (sin (pi * z)) - stirlingLogGamma (1 - z)
  | z < 12    = stirlingLogGamma (z + 1) - log z
  | otherwise = (z - 0.5) * log z - z + 0.5 * log (2 * pi)
                + 1/(12*z) - 1/(360*z^(3::Int))

logFactorial :: Int -> Double
logFactorial n = sum (map log [2 .. fromIntegral n])
