{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImpredicativeTypes #-}
-- | Polymorphic Hierarchical Bayesian Model (HBM) DSL.
--
-- A free-monad embedded language for probabilistic programs. The
-- continuation type is left polymorphic so that a single model term can
-- be reinterpreted as:
--
--   * a structural inspector (parameter / observation graph),
--   * a log-joint density,
--   * an automatically-differentiated log-joint
--     (via @Numeric.AD.Mode.Forward@),
--   * a dependency tracker (the 'Track' interpretation, used by
--     @Hanalyze.Viz.ModelGraph@ to build a Mermaid DAG).
--
-- See @docs/bayesian/02-probabilistic-model.md@ for an extended
-- introduction.
--
-- @
-- data ModelF a next
--   = Sample  Text (Distribution a) (a -> next)
--   | Observe Text (Distribution a) [Double] next
--   deriving Functor
-- @
--
-- ユーザーは @forall a. (Floating a, Ord a) => Model a r@ という
-- 「型に多相なモデル」を一度だけ書き、解釈時に @a@ を選ぶことで
-- 同じモデルから複数の解釈 (サンプリング・log joint・AD 勾配・依存抽出)
-- を取り出せる。
--
-- == 使い方
--
-- @
-- import Hanalyze.Model.HBM
--
-- myModel :: ModelP ()
-- myModel = do
--   mu    <- sample "mu"    (Normal 0 10)
--   sigma <- sample "sigma" (Exponential 1)
--   observe "y" (Normal mu sigma) [1.5, 2.0, 1.8]
--
-- -- 異なる解釈:
-- logVal = logJoint myModel (Map.fromList [("mu",1),("sigma",2)])  -- 数値評価
-- gVec   = gradAD myModel ["mu","sigma"] [1, 2]                    -- AD 勾配
-- deps   = extractDeps myModel                                      -- 依存関係
-- @
module Hanalyze.Model.HBM
  ( -- * Polymorphic distributions
    Distribution (..)
  , distName
  , logDensity
  , logDensityObs
  , sampleDist
  , distCDF
  , logCDF
  , logSF
    -- * Polymorphic model DSL
  , Free (..)
  , liftF
  , ModelF (..)
  , Model
  , ModelP
  , sample
  , observe
  , observeMV
  , observeColumns
  , potential
  , deterministic
  , runDeterministics
  , augmentChainWithDeterministic
  , nonCenteredNormal
  , dirichlet
  , dataNamed
  , withData
  , mvNormalLatent
  , mvNormalLogDensity
  , multinomialLogDensity
  , lkjCorrCholesky
  , ar1Latent
    -- * Structural inspection
  , Node (..)
  , NodeKind (..)
  , collectNodes
  , sampleNames
  , extractDeps
    -- * Type aliases
  , Params
    -- * Interpreters
  , logJoint
  , logPrior
  , logLikelihood
  , perObsLogLiks
  , runObserveDists
  , priorList
  , describeModel
    -- * Model graph (visualization)
  , ModelGraph (..)
  , buildModelGraph
    -- * AD gradient
  , gradAD
  , gradADU
    -- * Constraint transforms (for HMC)
  , getTransforms
  , logJointUnconstrained
  , invTransformF
  , logJacF
    -- * Dependency-tracking interpretation
  , Track (..)
  , trackVar
  , trackConst
  ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import Numeric.AD.Mode.Forward (grad)
import qualified System.Random.MWC as MWCBase
import qualified System.Random.MWC.Distributions as MWC
import System.Random.MWC (GenIO)

import Hanalyze.Stat.Distribution (Transform (..))
import Hanalyze.MCMC.Core (Chain (..))

-- ---------------------------------------------------------------------------
-- @Free@ monad (再実装。Hanalyze.Model.HBM のものとは型が違うので別途定義)
-- ---------------------------------------------------------------------------

data Free f a = Pure a | Free (f (Free f a))

instance Functor f => Functor (Free f) where
  fmap g (Pure a) = Pure (g a)
  fmap g (Free x) = Free (fmap (fmap g) x)

instance Functor f => Applicative (Free f) where
  pure = Pure
  Pure g <*> x  = fmap g x
  Free fg <*> x = Free (fmap (<*> x) fg)

instance Functor f => Monad (Free f) where
  return = pure
  Pure a >>= g = g a
  Free x >>= g = Free (fmap (>>= g) x)

liftF :: Functor f => f a -> Free f a
liftF fa = Free (fmap Pure fa)

-- ---------------------------------------------------------------------------
-- 多相分布
-- ---------------------------------------------------------------------------

-- | A probability distribution polymorphic in its value type @a@.
--
-- @a@ ranges over @Double@ (sampling and density), @Reverse s Double@
-- (AD-based gradient), @Track@ (dependency tracking) and so on.
data Distribution a
  = Normal      a a       -- ^ Normal(μ, σ)
  | Exponential a         -- ^ Exp(rate)
  | Gamma       a a       -- ^ Gamma(shape, rate)
  | Beta        a a       -- ^ Beta(α, β)
  | Poisson     a         -- ^ Poisson(λ)
  | Binomial    Int a     -- ^ Binomial(n, p)
  | Uniform     a a       -- ^ Uniform(low, high)
  | StudentT    a a a     -- ^ StudentT(ν degrees of freedom, μ location, σ scale)
  | Cauchy      a a       -- ^ Cauchy(x₀ location, γ scale)
  | HalfNormal  a         -- ^ HalfNormal(σ) — support: x ≥ 0
  | HalfCauchy  a         -- ^ HalfCauchy(γ scale) — support: x ≥ 0
  | LogNormal   a a       -- ^ LogNormal(μ log-mean, σ log-sd) — support: x > 0
  | Bernoulli   a         -- ^ Bernoulli(p) — observed: 0 or 1
  | Categorical [a]       -- ^ Categorical(probs) — observed: 0..K-1
  | Mixture [a] [Distribution a]
    -- ^ @Mixture(weights, components)@ —
    --   @log p(x) = logSumExp(log w_k + log p_k(x))@.
    --   Weights need only be positive; they are auto-normalized.
  | Truncated (Distribution a) (Maybe a) (Maybe a)
    -- ^ @Truncated(d, lo, hi)@: restrict the support of @d@ to
    --   @[lo, hi]@. Out-of-range observations get @-∞@.
    --   'Nothing' bounds mean @-∞ / +∞@. Only base distributions with a
    --   CDF (Normal / Exponential / LogNormal / Uniform) are supported.
  | Censored  (Distribution a) (Maybe a) (Maybe a)
    -- ^ @Censored(d, lo, hi)@: censor @y ≤ lo@ on the left and
    --   @y ≥ hi@ on the right. When @y_i@ equals a threshold the CDF/SF
    --   is used. Useful for Tobit-style models. Only CDF-supporting
    --   base distributions.
  | MvNormal [a] [[a]]
    -- ^ @MvNormal(μ, Σ)@: multivariate normal (observation-only).
    --   @μ@ is a length-@k@ mean vector, @Σ@ is the @k×k@
    --   symmetric-positive-definite covariance. Pass @k@-vector
    --   observations through 'observeMV'. Density is computed via
    --   Cholesky. /Not supported/ as a latent ('sample' returns 0
    --   density).
  | NegativeBinomial a a
    -- ^ @NegativeBinomial(μ, α)@ (PyMC parameterization).
    --   @mean = μ@, @var = μ + μ²/α@ (Poisson in the limit
    --   @α → ∞@). Likelihood for over-dispersed count data;
    --   observations are non-negative integers.
  | Multinomial Int [a]
    -- ^ @Multinomial(n, [p_0, …, p_{K-1}])@ (observation-only).
    --   @n@ is the trial count and @p@ the probability vector.
    --   Observations are @K@-dimensional count vectors summing to @n@,
    --   passed via 'observeMV'.
  | ZeroInflatedPoisson a a
    -- ^ @ZeroInflatedPoisson(ψ, λ)@: zero-inflated Poisson.
    --   @ψ ∈ [0, 1]@ is the structural-zero probability.
    --   @P(0) = ψ + (1-ψ) e^{-λ}@,
    --   @P(k>0) = (1-ψ) λ^k e^{-λ} / k!@.
  | ZeroInflatedBinomial Int a a
    -- ^ @ZeroInflatedBinomial(n, ψ, p)@: zero-inflated binomial.
    --   @P(0) = ψ + (1-ψ) (1-p)^n@,
    --   @P(k>0) = (1-ψ) C(n,k) p^k (1-p)^{n-k}@.
  | InverseGamma a a
    -- ^ @InverseGamma(α, β)@. Support @x > 0@. If
    --   @X ~ InverseGamma(α, β)@ then @1/X ~ Gamma(α, β)@ (rate
    --   parameterization). Common conjugate prior on variance
    --   (@mean = β/(α−1)@, finite when @α > 1@).
  | Weibull a a
    -- ^ @Weibull(k shape, λ scale)@: a standard survival distribution.
    --   Support @x > 0@. @pdf = (k/λ) (x/λ)^{k-1} exp(-(x/λ)^k)@.
    --   With @k = 1@ this is @Exponential(rate = 1/λ)@.
  | Pareto a a
    -- ^ @Pareto(α shape, x_m scale)@: heavy-tailed power law.
    --   Support @x ≥ x_m > 0@. @pdf = α x_m^α / x^{α+1}@.
    --   Mean @= α x_m / (α-1)@ when @α > 1@.
  | BetaBinomial Int a a
    -- ^ @BetaBinomial(n, α, β)@ overdispersed binomial
    --   (observation-only).
    --   @P(k) = C(n, k) B(k+α, n-k+β) / B(α, β)@. With @α = β = 1@
    --   this is uniform on @{0, …, n}@; large @α/β@ tends to a
    --   binomial.
  | VonMises a a
    -- ^ @VonMises(μ location, κ concentration)@: distribution on the
    --   circle @(-π, π]@.
    --   @pdf = exp(κ cos(x − μ)) / (2π I_0(κ))@.
    --   @κ → 0@ approaches uniform; @κ → ∞@ approaches
    --   @Normal(μ, 1/√κ)@.
  deriving (Show, Functor)

-- | Display name of a distribution constructor (e.g. @\"Normal\"@).
distName :: Distribution a -> Text
distName Normal{}      = "Normal"
distName Exponential{} = "Exponential"
distName Gamma{}       = "Gamma"
distName Beta{}        = "Beta"
distName Poisson{}     = "Poisson"
distName Binomial{}    = "Binomial"
distName Uniform{}     = "Uniform"
distName StudentT{}    = "StudentT"
distName Cauchy{}      = "Cauchy"
distName HalfNormal{}  = "HalfNormal"
distName HalfCauchy{}  = "HalfCauchy"
distName LogNormal{}   = "LogNormal"
distName Bernoulli{}   = "Bernoulli"
distName Categorical{} = "Categorical"
distName Mixture{}     = "Mixture"
distName Truncated{}   = "Truncated"
distName Censored{}    = "Censored"
distName MvNormal{}    = "MvNormal"
distName NegativeBinomial{} = "NegativeBinomial"
distName Multinomial{}          = "Multinomial"
distName ZeroInflatedPoisson{}  = "ZeroInflatedPoisson"
distName ZeroInflatedBinomial{} = "ZeroInflatedBinomial"
distName InverseGamma{}         = "InverseGamma"
distName Weibull{}              = "Weibull"
distName Pareto{}               = "Pareto"
distName BetaBinomial{}         = "BetaBinomial"
distName VonMises{}             = "VonMises"

-- | Log prior density at a sample value of type @a@.
logDensity :: (Floating a, Ord a) => Distribution a -> a -> a
logDensity (Normal mu sig) x
  | sig <= 0  = negInf
  | otherwise = -0.5 * log (2 * pi) - log sig
              - 0.5 * ((x - mu) / sig) ^ (2::Int)
logDensity (Exponential rate) x
  | x < 0 || rate <= 0 = negInf
  | otherwise          = log rate - rate * x
logDensity (Gamma shape rate) x
  | x <= 0 || shape <= 0 || rate <= 0 = negInf
  | otherwise =
      (shape - 1) * log x - rate * x
      + shape * log rate - lgammaApprox shape
logDensity (Beta alpha beta) x
  | x <= 0 || x >= 1 || alpha <= 0 || beta <= 0 = negInf
  | otherwise =
      (alpha - 1) * log x + (beta - 1) * log (1 - x)
      - (lgammaApprox alpha + lgammaApprox beta - lgammaApprox (alpha + beta))
logDensity (Poisson lam) x
  | lam <= 0 = negInf
  | x  < 0   = negInf
  | otherwise =
      -- x はサンプル値なので連続として扱う (整数化はしない)
      x * log lam - lam
logDensity (Binomial _ p) _
  | p <= 0 || p >= 1 = negInf
  | otherwise        = 0  -- サンプル時は使わない (構造のみ)
logDensity (Uniform lo hi) x
  | hi <= lo            = negInf
  | x  < lo || x  > hi  = negInf
  | otherwise           = -log (hi - lo)
logDensity (StudentT df mu sig) x
  | df <= 0 || sig <= 0 = negInf
  | otherwise =
      let z = (x - mu) / sig
      in lgammaApprox ((df + 1) / 2)
       - lgammaApprox (df / 2)
       - 0.5 * log (df * pi)
       - log sig
       - ((df + 1) / 2) * log (1 + z * z / df)
logDensity (Cauchy loc sc) x
  | sc <= 0   = negInf
  | otherwise =
      let z = (x - loc) / sc
      in -log pi - log sc - log (1 + z * z)
logDensity (HalfNormal sig) x
  | sig <= 0 = negInf
  | x < 0    = negInf
  | otherwise =
      0.5 * log 2 - 0.5 * log pi - log sig
      - 0.5 * (x / sig) ^ (2::Int)
logDensity (HalfCauchy sc) x
  | sc <= 0 = negInf
  | x < 0   = negInf
  | otherwise =
      log 2 - log pi - log sc - log (1 + (x / sc) ^ (2::Int))
logDensity (LogNormal mu sig) x
  | sig <= 0 = negInf
  | x  <= 0  = negInf
  | otherwise =
      let lx = log x
      in -0.5 * log (2 * pi) - log sig - lx
         - 0.5 * ((lx - mu) / sig) ^ (2::Int)
logDensity (Bernoulli p) _
  | p <= 0 || p >= 1 = negInf
  | otherwise        = 0  -- 構造のみ (離散なので連続 prior 評価には使わない)
logDensity (Categorical _) _ = 0  -- 同上
logDensity (Mixture ws comps) x
  | null ws || length ws /= length comps = negInf
  | otherwise =
      let total      = sum ws
          logTotal   = log total
          -- log(w_k / Σw) + log p_k(x)
          logTerms   = zipWith (\w d -> log w - logTotal + logDensity d x) ws comps
      in logSumExpA logTerms
logDensity (Truncated d mLo mHi) x =
  -- 範囲外なら 0 (=> log で −∞)
  let outOfRange = case (mLo, mHi) of
        (Just lo, _      ) | x < lo  -> True
        (_,       Just hi) | x > hi  -> True
        _                            -> False
  in if outOfRange
       then negInf
       else logDensity d x - logCDFInterval d mLo mHi
logDensity (Censored d _ _) x =
  -- prior 評価では通常の密度を使う (打ち切りは観測時のみ意味を持つ)
  logDensity d x
logDensity MvNormal{} _ = 0  -- observation-only: latent としては使わない
logDensity Multinomial{} _ = 0  -- observation-only
logDensity (InverseGamma alpha beta) x
  | alpha <= 0 || beta <= 0 || x <= 0 = negInf
  | otherwise =
      alpha * log beta - lgammaApprox alpha
      - (alpha + 1) * log x - beta / x
logDensity (Weibull kShape lam) x
  | kShape <= 0 || lam <= 0 || x <= 0 = negInf
  | otherwise =
      log kShape - log lam
      + (kShape - 1) * (log x - log lam)
      - (x / lam) ** kShape
logDensity (Pareto alpha xm) x
  | alpha <= 0 || xm <= 0 || x < xm = negInf
  | otherwise =
      log alpha + alpha * log xm - (alpha + 1) * log x
logDensity BetaBinomial{} _ = 0  -- 観測専用 (離散)
logDensity (VonMises mu kappa) x
  | kappa <= 0 = negInf
  | otherwise =
      kappa * cos (x - mu)
      - log (2 * pi)
      - logBesselI0 kappa
logDensity (ZeroInflatedPoisson psi lam) x
  | psi < 0 || psi > 1 || lam <= 0 || x < 0 = negInf
  | x == 0 =
      -- log(ψ + (1-ψ) e^{-λ})
      logSumExpA [log psi, log (1 - psi) - lam]
  | otherwise =
      -- log(1-ψ) + Poisson logpmf
      log (1 - psi) + x * log lam - lam - lgammaApprox (x + 1)
logDensity (ZeroInflatedBinomial n psi p) x
  | psi < 0 || psi > 1 || p <= 0 || p >= 1 || x < 0 = negInf
  | otherwise =
      let nA   = realToFrac (fromIntegral n :: Double)
          -- log(C(n,k)) = lgamma(n+1) - lgamma(k+1) - lgamma(n-k+1) (多相)
          logC = lgammaApprox (nA + 1)
               - lgammaApprox (x + 1)
               - lgammaApprox (nA - x + 1)
      in if x == 0
           then logSumExpA [log psi
                           , log (1 - psi) + nA * log (1 - p)]
           else log (1 - psi)
                + logC + x * log p + (nA - x) * log (1 - p)
logDensity (NegativeBinomial mu alpha) x
  | mu <= 0 || alpha <= 0 || x < 0 = negInf
  | otherwise =
      let p = alpha / (alpha + mu)        -- success prob
      in lgammaApprox (x + alpha)
       - lgammaApprox alpha
       - lgammaApprox (x + 1)
       + alpha * log p
       + x * log (1 - p)

-- | Log likelihood density at an observation (a fixed @Double@).
-- Observations are passed as @[Double]@, so this uses only the
-- @Floating a@ constraint.
logDensityObs :: forall a. (Floating a, Ord a) => Distribution a -> Double -> a
logDensityObs (Normal mu sig) y
  | sig <= 0  = negInf
  | otherwise =
      let yA = realToFrac y :: a
      in -0.5 * log (2 * pi) - log sig - 0.5 * ((yA - mu) / sig) ^ (2::Int)
logDensityObs (Exponential rate) y
  | y < 0      = negInf
  | rate <= 0  = negInf
  | otherwise  = log rate - rate * (realToFrac y :: a)
logDensityObs (Gamma shape rate) y
  | y <= 0     = negInf
  | shape <= 0 || rate <= 0 = negInf
  | otherwise  =
      let yA = realToFrac y :: a
      in (shape - 1) * log yA - rate * yA
         + shape * log rate - lgammaApprox shape
logDensityObs (Beta alpha beta) y
  | y <= 0 || y >= 1 || alpha <= 0 || beta <= 0 = negInf
  | otherwise =
      let yA = realToFrac y :: a
      in (alpha - 1) * log yA + (beta - 1) * log (1 - yA)
         - (lgammaApprox alpha + lgammaApprox beta - lgammaApprox (alpha + beta))
logDensityObs (Poisson lam) y
  | lam <= 0 = negInf
  | y < 0    = negInf
  | otherwise =
      let kA   = realToFrac y :: a
          kInt = round y :: Int
          logFactK = realToFrac (logFactorial kInt) :: a
      in kA * log lam - lam - logFactK
logDensityObs (Binomial n p) y
  | p <= 0 || p >= 1 = negInf
  | otherwise =
      let k    = round y :: Int
          kA   = realToFrac y :: a
          nA   = realToFrac (fromIntegral n :: Double) :: a
          logC = realToFrac (logBinomCoeff n k) :: a
      in logC + kA * log p + (nA - kA) * log (1 - p)
logDensityObs (Uniform lo hi) y
  | hi <= lo  = negInf
  | otherwise =
      let yA = realToFrac y :: a
      in if yA < lo || yA > hi then negInf else -log (hi - lo)
logDensityObs (StudentT df mu sig) y
  | df <= 0 || sig <= 0 = negInf
  | otherwise =
      let yA = realToFrac y :: a
          z  = (yA - mu) / sig
      in lgammaApprox ((df + 1) / 2)
       - lgammaApprox (df / 2)
       - 0.5 * log (df * pi)
       - log sig
       - ((df + 1) / 2) * log (1 + z * z / df)
logDensityObs (Cauchy loc sc) y
  | sc <= 0   = negInf
  | otherwise =
      let yA = realToFrac y :: a
          z  = (yA - loc) / sc
      in -log pi - log sc - log (1 + z * z)
logDensityObs (HalfNormal sig) y
  | sig <= 0 = negInf
  | y  < 0   = negInf
  | otherwise =
      let yA = realToFrac y :: a
      in 0.5 * log 2 - 0.5 * log pi - log sig
       - 0.5 * (yA / sig) ^ (2::Int)
logDensityObs (HalfCauchy sc) y
  | sc <= 0 = negInf
  | y  < 0  = negInf
  | otherwise =
      let yA = realToFrac y :: a
      in log 2 - log pi - log sc - log (1 + (yA / sc) ^ (2::Int))
logDensityObs (LogNormal mu sig) y
  | sig <= 0 = negInf
  | y  <= 0  = negInf
  | otherwise =
      let yA = realToFrac y :: a
          lx = log yA
      in -0.5 * log (2 * pi) - log sig - lx
       - 0.5 * ((lx - mu) / sig) ^ (2::Int)
logDensityObs (Bernoulli p) y
  | p <= 0 || p >= 1 = negInf
  | otherwise =
      let k = round y :: Int
      in case k of
           1 -> log p
           0 -> log (1 - p)
           _ -> negInf
logDensityObs (Categorical probs) y =
  let k    = round y :: Int
      n    = length probs
  in if k < 0 || k >= n
       then negInf
       else
         -- log p_k - log(Σ p_i)  (probs を正規化)
         let pk     = probs !! k
             total  = sum probs
         in if pk <= 0 || total <= 0
              then negInf
              else log pk - log total
logDensityObs (Mixture ws comps) y
  | null ws || length ws /= length comps = negInf
  | otherwise =
      let total    = sum ws
          logTotal = log total
          logTerms = zipWith (\w d -> log w - logTotal + logDensityObs d y) ws comps
      in logSumExpA logTerms
logDensityObs (Truncated d mLo mHi) y =
  let yA = realToFrac y :: a
      outOfRange = case (mLo, mHi) of
        (Just lo, _      ) | yA < lo  -> True
        (_,       Just hi) | yA > hi  -> True
        _                             -> False
  in if outOfRange
       then negInf
       else logDensityObs d y - logCDFInterval d mLo mHi
logDensityObs (Censored d mLo mHi) y =
  -- 観測値 y が境界 lo / hi に等しい場合は左/右打ち切り尤度
  let yA = realToFrac y :: a
      eps = 1e-9 :: a
      isAt v target = abs (v - target) < eps
  in case (mLo, mHi) of
       (Just lo, _) | yA <= lo || isAt yA lo -> logCDF d lo                -- 左打ち切り
       (_, Just hi) | yA >= hi || isAt yA hi -> logSF  d hi                -- 右打ち切り
       _                                     -> logDensityObs d y          -- 通常観測
logDensityObs MvNormal{} _ = 0
  -- スカラー観測経路では使わない (chunk して 'mvNormalLogDensity' を呼ぶ obsLogSum 経由)
logDensityObs Multinomial{} _ = 0
  -- スカラー観測経路では使わない (k 次元 chunk で multinomialLogDensity を呼ぶ)
logDensityObs (InverseGamma alpha beta) y
  | alpha <= 0 || beta <= 0 || y <= 0 = negInf
  | otherwise =
      let yA = realToFrac y :: a
      in alpha * log beta - lgammaApprox alpha
       - (alpha + 1) * log yA - beta / yA
logDensityObs (Weibull kShape lam) y
  | kShape <= 0 || lam <= 0 || y <= 0 = negInf
  | otherwise =
      let yA = realToFrac y :: a
      in log kShape - log lam
       + (kShape - 1) * (log yA - log lam)
       - (yA / lam) ** kShape
logDensityObs (Pareto alpha xm) y
  | alpha <= 0 || xm <= 0 = negInf
  | otherwise =
      let yA = realToFrac y :: a
      in if yA < xm
           then negInf
           else log alpha + alpha * log xm - (alpha + 1) * log yA
logDensityObs (BetaBinomial n alpha beta) y
  | alpha <= 0 || beta <= 0 || y < 0 = negInf
  | otherwise =
      let yA   = realToFrac y :: a
          nA   = realToFrac (fromIntegral n :: Double) :: a
          k    = round y :: Int
          logC = realToFrac (logBinomCoeff n k) :: a
      in logC
       + lgammaApprox (yA + alpha)
       + lgammaApprox (nA - yA + beta)
       - lgammaApprox (nA + alpha + beta)
       - (lgammaApprox alpha + lgammaApprox beta - lgammaApprox (alpha + beta))
logDensityObs (VonMises mu kappa) y
  | kappa <= 0 = negInf
  | otherwise =
      let yA = realToFrac y :: a
      in kappa * cos (yA - mu) - log (2 * pi) - logBesselI0 kappa
logDensityObs (ZeroInflatedPoisson psi lam) y
  | psi < 0 || psi > 1 || lam <= 0 || y < 0 = negInf
  | y == 0 =
      logSumExpA [log psi, log (1 - psi) - lam]
  | otherwise =
      let kA       = realToFrac y :: a
          kInt     = round y :: Int
          logFactK = realToFrac (logFactorial kInt) :: a
      in log (1 - psi) + kA * log lam - lam - logFactK
logDensityObs (ZeroInflatedBinomial n psi p) y
  | psi < 0 || psi > 1 || p <= 0 || p >= 1 || y < 0 = negInf
  | otherwise =
      let kA   = realToFrac y :: a
          k    = round y :: Int
          nA   = realToFrac (fromIntegral n :: Double) :: a
          logC = realToFrac (logBinomCoeff n k) :: a
      in if y == 0
           then logSumExpA [log psi
                           , log (1 - psi) + nA * log (1 - p)]
           else log (1 - psi)
                + logC + kA * log p + (nA - kA) * log (1 - p)
logDensityObs (NegativeBinomial mu alpha) y
  | mu <= 0 || alpha <= 0 || y < 0 = negInf
  | otherwise =
      let kA = realToFrac y :: a
          p  = alpha / (alpha + mu)
      in lgammaApprox (kA + alpha)
       - lgammaApprox alpha
       - lgammaApprox (kA + 1)
       + alpha * log p
       + kA * log (1 - p)

-- | Sum of log likelihoods over a list of observations. For ordinary
-- distributions one observation contributes one scalar log-density.
-- For 'MvNormal' (which expects @k@-vectors), the flattened @[Double]@
-- is chunked into length-@k@ groups before evaluation.
obsLogSum :: forall a. (Floating a, Ord a) => Distribution a -> [Double] -> a
obsLogSum (MvNormal mu cov) ys =
  let k       = length mu
      chunks  = chunksOf k ys
  in sum [ mvNormalLogDensity mu cov (map realToFrac yv :: [a])
         | yv <- chunks ]
obsLogSum (Multinomial n probs) ys =
  let k      = length probs
      chunks = chunksOf k ys
  in sum [ multinomialLogDensity n probs yv | yv <- chunks ]
obsLogSum d ys = sum [ logDensityObs d y | y <- ys ]

-- | Log probability of a single multinomial observation (a @K@-vector
-- of counts).
--   log P(k_1, …, k_K) = log n!/Π k_i! + Σ k_i log p_i
multinomialLogDensity :: forall a. (Floating a, Ord a)
                      => Int -> [a] -> [Double] -> a
multinomialLogDensity n probs counts
  | length probs /= length counts = negInf
  | sum (map round counts :: [Int]) /= n = negInf
  | any (< 0) counts                = negInf
  | any (\p -> p <= 0) probs        = negInf
  | otherwise =
      let logFactN = realToFrac (logFactorial n) :: a
          logFactSum = sum [ realToFrac (logFactorial (round c :: Int)) :: a
                           | c <- counts ]
          dotPart = sum (zipWith (\c p -> realToFrac c * log p) counts probs)
      in logFactN - logFactSum + dotPart

-- | Log density of an 'MvNormal' at a single @k@-vector observation.
--   log p(y) = -k/2 log(2π) - 0.5 log|Σ| - 0.5 (y-μ)ᵀ Σ⁻¹ (y-μ)
--   Σ⁻¹ と log|Σ| は Cholesky 分解 Σ = L Lᵀ から計算。
mvNormalLogDensity :: forall a. (Floating a, Ord a) => [a] -> [[a]] -> [a] -> a
mvNormalLogDensity mu cov yObs
  | length mu == 0           = 0
  | length yObs /= length mu = negInf
  | otherwise =
      case choleskyL cov of
        Nothing -> negInf
        Just l  ->
          let k      = length mu
              kA     = fromIntegral k :: a
              d      = zipWith (-) yObs mu
              z      = forwardSub l d           -- L z = d
              quad   = sum (map (\zi -> zi * zi) z)
              logDet = 2 * sum [ log ((l !! i) !! i) | i <- [0 .. k - 1] ]
          in -0.5 * kA * log (2 * pi) - 0.5 * logDet - 0.5 * quad

-- | リストを長さ @n@ ごとに分割。最後が短ければそのまま (本実装では使わない想定)。
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (h, t) = splitAt n xs in h : chunksOf n t

-- | 対称正定値行列 Σ の Cholesky 下三角分解 L (Σ = L Lᵀ)。
-- 行列は行リスト @[[a]]@ で、l[i] は長さ @i+1@ の下三角行 ([L[i][0]..L[i][i]])。
-- 対角が非正になれば @Nothing@。
choleskyL :: forall a. (Floating a, Ord a) => [[a]] -> Maybe [[a]]
choleskyL a0 =
  let n = length a0
      step :: Int -> [[a]] -> Maybe [[a]]
      step i prev
        | i == n = Just prev
        | otherwise =
            let row = a0 !! i
                buildCol :: Int -> [a] -> Maybe [a]
                buildCol j cur
                  | j > i  = Just cur
                  | j == i =
                      let s  = sum (map (\v -> v * v) cur)
                          d2 = (row !! i) - s
                      in if d2 <= 0
                           then Nothing
                           else buildCol (j + 1) (cur ++ [sqrt d2])
                  | otherwise =
                      let lj  = prev !! j           -- 長さ j+1
                          s   = sum (zipWith (*) cur lj)
                          ljj = lj !! j
                      in if ljj == 0
                           then Nothing
                           else buildCol (j + 1) (cur ++ [((row !! j) - s) / ljj])
            in case buildCol 0 [] of
                 Nothing -> Nothing
                 Just nr -> step (i + 1) (prev ++ [nr])
  in step 0 []

-- | 下三角系 L z = b の前進代入 (L は @choleskyL@ 形式、長さ各 i+1)。
forwardSub :: forall a. Floating a => [[a]] -> [a] -> [a]
forwardSub l b =
  let n   = length b
      go :: Int -> [a] -> [a]
      go i acc
        | i == n = acc
        | otherwise =
            let lrow = l !! i              -- 長さ i+1
                lii  = lrow !! i
                lpre = take i lrow         -- L[i][0..i-1]
                bi   = b !! i
                s    = sum (zipWith (*) lpre acc)
                zi   = (bi - s) / lii
            in go (i + 1) (acc ++ [zi])
  in go 0 []

negInf :: Floating a => a
negInf = -1/0

-- | 多相 log-sum-exp。AD でも Track でも使えるよう Floating + Ord で書く。
-- @logSumExpA xs = log (Σ exp x)@ を最大値シフトで安定化。
logSumExpA :: (Floating a, Ord a) => [a] -> a
logSumExpA []  = negInf
logSumExpA [x] = x
logSumExpA xs  =
  let m = maximum xs
  in m + log (sum (map (\x -> exp (x - m)) xs))

-- ---------------------------------------------------------------------------
-- 多相 CDF / log-CDF (Truncated / Censored 用)
-- ---------------------------------------------------------------------------

-- | 多相 erf 近似 (Abramowitz & Stegun 7.1.26)。誤差 < 1.5e-7。
-- AD でも Track でも動く。
erfA :: (Floating a, Ord a) => a -> a
erfA x =
  let p   = 0.3275911
      a1  = 0.254829592
      a2  = -0.284496736
      a3  = 1.421413741
      a4  = -1.453152027
      a5  = 1.061405429
      sgn = if x < 0 then -1 else 1
      ax  = abs x
      t   = 1 / (1 + p * ax)
      poly = a1*t + a2*t*t + a3*t*t*t + a4*t*t*t*t + a5*t*t*t*t*t
  in sgn * (1 - poly * exp (- ax * ax))

-- | 標準正規 CDF Φ(x)。
phiCdfA :: (Floating a, Ord a) => a -> a
phiCdfA x = 0.5 * (1 + erfA (x / sqrt 2))

-- | CDF @F(x) = P(Y ≤ x)@ of a 'Distribution'. Returns 'Nothing' for
-- distributions that do not have a closed-form CDF in this library.
distCDF :: (Floating a, Ord a) => Distribution a -> a -> Maybe a
distCDF (Normal mu sig) x
  | sig <= 0  = Nothing
  | otherwise = Just (phiCdfA ((x - mu) / sig))
distCDF (Exponential rate) x
  | rate <= 0 = Nothing
  | x <= 0    = Just 0
  | otherwise = Just (1 - exp (-rate * x))
distCDF (LogNormal mu sig) x
  | sig <= 0 || x <= 0 = Nothing
  | otherwise = Just (phiCdfA ((log x - mu) / sig))
distCDF (Uniform lo hi) x
  | hi <= lo  = Nothing
  | x <= lo   = Just 0
  | x >= hi   = Just 1
  | otherwise = Just ((x - lo) / (hi - lo))
distCDF (HalfNormal sig) x
  | sig <= 0 = Nothing
  | x <= 0   = Just 0
  | otherwise = Just (erfA (x / (sig * sqrt 2)))
distCDF (HalfCauchy sc) x
  | sc <= 0 = Nothing
  | x <= 0  = Just 0
  | otherwise = Just (2 * atan (x / sc) / pi)
distCDF (Cauchy loc sc) x
  | sc <= 0   = Nothing
  | otherwise = Just (0.5 + atan ((x - loc) / sc) / pi)
distCDF (Gamma shape rate) x
  | shape <= 0 || rate <= 0 = Nothing
  | x <= 0                  = Just 0
  | otherwise               = Just (incGammaPA shape (rate * x))
distCDF (Beta a b) x
  | a <= 0 || b <= 0 = Nothing
  | x <= 0           = Just 0
  | x >= 1           = Just 1
  | otherwise        = Just (incBetaA x a b)
distCDF (StudentT df mu sig) x
  | df <= 0 || sig <= 0 = Nothing
  | otherwise =
      let z     = (x - mu) / sig
          -- F_t(z; df) = 1 - 0.5 * I(df/(df+z²); df/2, 1/2)   (z >= 0)
          --            =     0.5 * I(df/(df+z²); df/2, 1/2)   (z <  0)
          ratio = df / (df + z * z)
          ix    = incBetaA ratio (df / 2) 0.5
      in Just (if z >= 0 then 1 - 0.5 * ix else 0.5 * ix)
distCDF _ _ = Nothing  -- 他の分布 (離散・Mixture・Truncated 内の Truncated 等) は未対応

-- | @log F(x)@. Computed as @log(F)@ directly to avoid loss of
-- precision near the tails where @F@ approaches 0 or 1.
logCDF :: (Floating a, Ord a) => Distribution a -> a -> a
logCDF d x = case distCDF d x of
  Nothing -> negInf
  Just c | c <= 0    -> negInf
         | c >= 1    -> 0
         | otherwise -> log c

-- | Log of the right-tail survival function @log(1 − F(x))@.
logSF :: (Floating a, Ord a) => Distribution a -> a -> a
logSF d x = case distCDF d x of
  Nothing -> negInf
  Just c | c <= 0    -> 0
         | c >= 1    -> negInf
         | otherwise -> log (1 - c)

-- ---------------------------------------------------------------------------
-- 不完全ガンマ関数 P(a, x) = γ(a, x) / Γ(a)  (Numerical Recipes 6.2)
-- ---------------------------------------------------------------------------

-- | 正則化された下側不完全ガンマ関数 P(a, x) = γ(a, x) / Γ(a) ∈ [0, 1]。
-- これは Gamma(shape=a, rate=1) の CDF F(x)。
incGammaPA :: (Floating a, Ord a) => a -> a -> a
incGammaPA a x
  | x <= 0 || a <= 0 = 0
  | x < a + 1        = igammSer a x          -- 級数展開で P(a,x)
  | otherwise        = 1 - igammCF a x        -- 連分数で Q(a,x)、P = 1 - Q

-- 級数展開: P(a, x) = e^{-x} x^a / Γ(a) * Σ x^n / (a(a+1)...(a+n))
igammSer :: forall a. (Floating a, Ord a) => a -> a -> a
igammSer a x = sumSer * exp (-x + a * log x - lgammaApprox a)
  where
    -- 反復: term_{n+1} = term_n * x / (a + n + 1)
    sumSer = go (0 :: Int) (1 / a) (1 / a)
    eps :: a
    eps    = 1e-13
    maxIt  = 200 :: Int
    go n term acc
      | n >= maxIt           = acc
      | abs term < abs acc * eps = acc
      | otherwise =
          let n'    = n + 1
              term' = term * x / (a + fromIntegral n')
              acc'  = acc + term'
          in go n' term' acc'

-- 連分数 (Lentz 法): Q(a, x) = e^{-x} x^a / Γ(a) * CF
-- CF = 1/(x+1-a - 1·(1-a)/(x+3-a - 2·(2-a)/(...))
igammCF :: forall a. (Floating a, Ord a) => a -> a -> a
igammCF a x = exp (-x + a * log x - lgammaApprox a) * h
  where
    fpmin, eps :: a
    fpmin = 1e-300
    eps   = 1e-13
    maxIt = 200 :: Int
    -- modified Lentz's method
    b0    = x + 1 - a
    c0    = 1 / fpmin
    d0    = 1 / b0
    h     = goCF (1 :: Int) b0 c0 d0 d0
    goCF i b c d hh
      | i > maxIt              = hh
      | abs (del - 1) < eps    = hh'
      | otherwise              = goCF (i + 1) b' c'' d''' hh'
      where
        an   = -fromIntegral i * (fromIntegral i - a)
        b'   = b + 2
        d'   = b' + an * d
        d''  = if abs d' < fpmin then fpmin else d'
        c'   = b' + an / c
        c''  = if abs c' < fpmin then fpmin else c'
        d''' = 1 / d''
        del  = d''' * c''
        hh'  = hh * del
    _ = c0  -- 未使用ダミー (修正された Lentz 法の起動値: 別経路)

-- ---------------------------------------------------------------------------
-- 正則化された不完全ベータ関数 I_x(a, b) = B(x; a, b) / B(a, b)
-- ---------------------------------------------------------------------------

-- | 正則化された不完全ベータ関数 I_x(a, b) ∈ [0, 1]。
-- これは Beta(a, b) の CDF F(x)。
-- StudentT の CDF にも内部で使用。
incBetaA :: (Floating a, Ord a) => a -> a -> a -> a
incBetaA x a b
  | x <= 0    = 0
  | x >= 1    = 1
  | otherwise =
      -- 対数ベータ正規化定数
      let bt = exp ( lgammaApprox (a + b)
                   - lgammaApprox a
                   - lgammaApprox b
                   + a * log x
                   + b * log (1 - x))
      in if x < (a + 1) / (a + b + 2)
           then bt * betaCFA x a b / a
           else 1 - bt * betaCFA (1 - x) b a / b

-- 連分数 (modified Lentz, Numerical Recipes §6.4)
betaCFA :: forall a. (Floating a, Ord a) => a -> a -> a -> a
betaCFA x a b = iterate' (1 :: Int) 1 d0 h0
  where
    fpmin, eps :: a
    fpmin = 1e-300
    eps   = 1e-13
    maxIt = 200 :: Int
    qab = a + b
    qap = a + 1
    qam = a - 1
    capLent v = if abs v < fpmin then fpmin else v
    d0 = 1 / capLent (1 - qab * x / qap)
    h0 = d0

    iterate' m c d h
      | m > maxIt          = h
      | abs (del - 1) < eps = hO
      | otherwise          = iterate' (m + 1) cO dO hO
      where
        mD  = fromIntegral m :: a
        -- 偶数項: aa_2m = m(b-m)x / ((qam+2m)(a+2m))
        aaE = mD * (b - mD) * x / ((qam + 2 * mD) * (a + 2 * mD))
        dE  = 1 / capLent (1 + aaE * d)
        cE  = capLent (1 + aaE / c)
        hE  = h * dE * cE
        -- 奇数項: aa_2m+1 = -(a+m)(qab+m)x / ((a+2m)(qap+2m))
        aaO = -(a + mD) * (qab + mD) * x / ((a + 2 * mD) * (qap + 2 * mD))
        dO  = 1 / capLent (1 + aaO * dE)
        cO  = capLent (1 + aaO / cE)
        del = dO * cO
        hO  = hE * del

-- | log(F(hi) − F(lo)) — Truncated の正規化定数。
logCDFInterval :: (Floating a, Ord a) => Distribution a -> Maybe a -> Maybe a -> a
logCDFInterval d mLo mHi = case (mLo, mHi) of
  (Nothing, Nothing) -> 0  -- log(1)
  (Just lo, Nothing) -> logSF d lo
  (Nothing, Just hi) -> logCDF d hi
  (Just lo, Just hi) ->
    case (distCDF d lo, distCDF d hi) of
      (Just cl, Just ch)
        | ch <= cl  -> negInf
        | otherwise -> log (ch - cl)
      _ -> negInf

-- ---------------------------------------------------------------------------
-- 分布からのサンプリング (事前/事後予測用)
-- ---------------------------------------------------------------------------

-- | Draw a single sample from a 'Distribution Double'.
-- 事前予測サンプリング、事後予測サンプリング、観測値の生成に使う。
--
-- mwc-random が直接提供しない分布はここで実装する (Cauchy, HalfCauchy, etc.)。
sampleDist :: Distribution Double -> GenIO -> IO Double
sampleDist (Normal mu sig) gen = MWC.normal mu sig gen
sampleDist (Exponential rate) gen = do
  u <- MWCBase.uniform gen :: IO Double
  return (-log u / rate)
sampleDist (Gamma shape rate) gen =
  -- mwc-random の gamma は scale パラメタ化なので 1/rate を渡す
  MWC.gamma shape (1 / rate) gen
sampleDist (Beta a b) gen = do
  x <- MWC.gamma a 1 gen
  y <- MWC.gamma b 1 gen
  return (x / (x + y))
sampleDist (Poisson lam) gen = samplePoissonKnuth lam gen
sampleDist (Binomial n p) gen = do
  -- n 回のベルヌーイ試行
  let go 0 acc = return acc
      go k acc = do
        u <- MWCBase.uniform gen :: IO Double
        go (k - 1) (if u < p then acc + 1 else acc)
  fmap fromIntegral (go n (0 :: Int))
sampleDist (Uniform lo hi) gen = do
  u <- MWCBase.uniform gen :: IO Double
  return (lo + u * (hi - lo))
sampleDist (StudentT df mu sig) gen = do
  -- t = mu + sig * Normal(0,1) / sqrt(Chi2(df) / df)
  z    <- MWC.standard gen
  chi2 <- MWC.gamma (df / 2) 2 gen   -- Chi2(df) = Gamma(df/2, scale=2)
  return (mu + sig * z / sqrt (chi2 / df))
sampleDist (Cauchy loc sc) gen = do
  u <- MWCBase.uniform gen :: IO Double
  return (loc + sc * tan (pi * (u - 0.5)))
sampleDist (HalfNormal sig) gen = do
  z <- MWC.standard gen
  return (abs (sig * z))
sampleDist (HalfCauchy sc) gen = do
  u <- MWCBase.uniform gen :: IO Double
  return (sc * abs (tan (pi * (u - 0.5))))
sampleDist (LogNormal mu sig) gen = do
  z <- MWC.standard gen
  return (exp (mu + sig * z))
sampleDist (Bernoulli p) gen = do
  u <- MWCBase.uniform gen :: IO Double
  return (if u < p then 1.0 else 0.0)
sampleDist (Categorical probs) gen = do
  u <- MWCBase.uniform gen :: IO Double
  let total = sum probs
      go _   []     = fromIntegral (length probs - 1)
      go acc (p:ps) =
        let acc' = acc + p / total
        in if u < acc' then 0 else 1 + go acc' ps
  return (go 0 probs)
sampleDist (Mixture ws comps) gen
  | null ws || length ws /= length comps = return (0/0)  -- NaN: 不正
  | otherwise = do
      -- 1) 重みに比例して成分 k を選ぶ
      u <- MWCBase.uniform gen :: IO Double
      let total = sum ws
          pickIdx _ [] = length ws - 1
          pickIdx acc (w:rest) =
            let acc' = acc + w / total
            in if u < acc' then 0 else 1 + pickIdx acc' rest
          k = pickIdx 0 ws
      -- 2) 選んだ成分からサンプリング
      sampleDist (comps !! k) gen
sampleDist (Truncated d mLo mHi) gen =
  -- 単純なリジェクション・サンプリング (範囲が極めて狭いと収束遅い)
  let inRange y = case (mLo, mHi) of
        (Just lo, _      ) | y < lo  -> False
        (_,       Just hi) | y > hi  -> False
        _                            -> True
      tryOnce maxAttempts
        | maxAttempts <= 0 = return (0/0)  -- 諦め
        | otherwise = do
            y <- sampleDist d gen
            if inRange y then return y else tryOnce (maxAttempts - 1)
  in tryOnce (10000 :: Int)
sampleDist MvNormal{} _ =
  error "MvNormal: observation-only — 'sample' 経由でのドローは未対応"
sampleDist Multinomial{} _ =
  error "Multinomial: observation-only — 'sample' 経由でのドローは未対応"
sampleDist (InverseGamma alpha beta) gen = do
  -- 1 / Gamma(α, rate=β) = 1 / Gamma(α, scale=1/β)
  y <- MWC.gamma alpha (1 / beta) gen
  return (1 / y)
sampleDist (Weibull kShape lam) gen = do
  -- 逆 CDF 法: x = λ (-log(1-u))^(1/k)
  u <- MWCBase.uniform gen :: IO Double
  return (lam * ((-log (1 - u)) ** (1 / kShape)))
sampleDist (Pareto alpha xm) gen = do
  -- 逆 CDF 法: x = x_m / u^(1/α)
  u <- MWCBase.uniform gen :: IO Double
  return (xm / (u ** (1 / alpha)))
sampleDist (BetaBinomial n alpha beta) gen = do
  -- p ~ Beta(α, β); k ~ Binomial(n, p)
  p <- sampleDist (Beta alpha beta) gen
  sampleDist (Binomial n p) gen
sampleDist (VonMises mu kappa) gen = do
  -- Best-Fisher の rejection sampler
  let a = 1 + sqrt (1 + 4 * kappa * kappa)
      b = (a - sqrt (2 * a)) / (2 * kappa)
      r = (1 + b * b) / (2 * b)
      tryOnce = do
        u1 <- MWCBase.uniform gen :: IO Double
        let z = cos (pi * u1)
            f = (1 + r * z) / (r + z)
            c = kappa * (r - f)
        u2 <- MWCBase.uniform gen :: IO Double
        if c * (2 - c) - u2 > 0 || log (c / u2) + 1 - c >= 0
          then do
            u3 <- MWCBase.uniform gen :: IO Double
            let sign = if u3 - 0.5 < 0 then (-1.0) else 1.0
            return (mu + sign * acos f)
          else tryOnce
  tryOnce
sampleDist (ZeroInflatedPoisson psi lam) gen = do
  u <- MWCBase.uniform gen :: IO Double
  if u < psi
    then return 0
    else samplePoissonKnuth lam gen
sampleDist (ZeroInflatedBinomial n psi p) gen = do
  u <- MWCBase.uniform gen :: IO Double
  if u < psi
    then return 0
    else sampleDist (Binomial n p) gen
sampleDist (NegativeBinomial mu alpha) gen = do
  -- Gamma-Poisson mixture: λ ~ Gamma(α, β=α/μ); X ~ Poisson(λ)
  lam <- MWC.gamma alpha (mu / alpha) gen
  samplePoissonKnuth lam gen
sampleDist (Censored d _ _) gen =
  -- 元分布から普通にサンプリング (打ち切りは「観測過程」の話で生成側ではない)
  sampleDist d gen

-- | Knuth のアルゴリズムで Poisson(λ) サンプル。λ < 30 程度なら十分高速。
samplePoissonKnuth :: Double -> GenIO -> IO Double
samplePoissonKnuth lam gen = do
  let l = exp (-lam)
      go k p = do
        u <- MWCBase.uniform gen :: IO Double
        let p' = p * u
        if p' < l
          then return (fromIntegral k)
          else go (k + 1) p'
  go 0 (1.0 :: Double)

-- ---------------------------------------------------------------------------
-- 多相モデル (@Free@ monad)
-- ---------------------------------------------------------------------------

-- | DSL のプリミティブ。継続が @a -> next@ なので任意の @a@ を流せる。
--
-- 'Potential' は PyMC の @pm.Potential@ 相当で、任意の log-prob 項を
-- log-joint に加える。ソフト制約・カスタム尤度・正則化項などに使える。
data ModelF a next
  = Sample  Text (Distribution a) (a -> next)
  | Observe Text (Distribution a) [Double] next
  | Potential Text a next
    -- ^ 名前付きの ad-hoc な log-prob 項。値 @a@ がそのまま log-joint に加算される。
  | Deterministic Text a (a -> next)
    -- ^ 名前付きの派生量 (PyMC `pm.Deterministic`)。log-joint には寄与せず、
    --   サンプルごとに値を保存する。継続には値そのものを通すので、その後の
    --   モデル中でも参照可能。
  | Data Text [Double] ([Double] -> next)
    -- ^ 名前付き観測データプレースホルダ (PyMC `pm.Data`)。
    --   モデル内でデータを保持し、`withData` で外部から差し替え可能。
    --   観測値を直接 `observe` に渡す代わりに、`dataNamed` で受け取って
    --   `observe` に渡すと、後でデータ差し替えができる。
  deriving Functor

type Model a = Free (ModelF a)

-- | Type alias for the polymorphic model DSL.
-- @ModelP r = forall a. (Floating a, Ord a) => Model a r@
type ModelP r = forall a. (Floating a, Ord a) => Model a r

sample :: Text -> Distribution a -> Model a a
sample n d = liftF (Sample n d id)

observe :: Text -> Distribution a -> [Double] -> Model a ()
observe n d ys = liftF (Observe n d ys ())

-- | Multivariate observation (for 'MvNormal'). Each observation is a
-- length-@k@ vector; pass them as a list @[[Double]]@.
-- 内部的には @concat@ で flatten され、評価時に Distribution の次元 k で chunk される。
observeMV :: Text -> Distribution a -> [[Double]] -> Model a ()
observeMV n d obss = liftF (Observe n d (concat obss) ())

-- | Multi-output observation helper. Takes @q@ pairs of
-- @observe (prefix <> \"_\" <> j) dist_j ys_j@ を順に発行する。
--
-- 多出力回帰の尤度を 1 行で書きたいときに使う:
--
-- @
-- observeColumns \"y\" [(Normal mu_j sigma_j, ysCol j) | j <- [0 .. q - 1]]
-- @
observeColumns :: Text -> [(Distribution a, [Double])] -> Model a ()
observeColumns prefix pairs =
  mapM_ (\(j, (d, ys)) ->
           observe (prefix <> "_" <> T.pack (show (j :: Int))) d ys)
        (zip [0..] pairs)

-- | Add an arbitrary log-probability term to the model (analogous to
-- PyMC's @pm.Potential@).
--
-- 通常のサンプリング/観測では表せない log-density 寄与を入れるのに使う。
-- 典型用途:
--
--   * **ソフト制約**: @potential \"order\" (if mu1 < mu2 then 0 else (-1e10))@
--   * **カスタム尤度**: 既存 'Distribution' で表せない尤度項
--   * **正則化**: ベイズ的な正則化 (e.g. ridge: @-0.5 * lambda * sum (map (^2) betas)@)
--
-- @Potential@ の値は 'logJoint' と 'logPrior' に加算される
-- ('logLikelihood' には含まれない — これらは @observe@ 専用)。
potential :: Text -> a -> Model a ()
potential nm v = liftF (Potential nm v ())

-- | 派生量を名前付きで保存する (PyMC `pm.Deterministic` 相当)。
--
-- log-joint には寄与しないが、各 posterior サンプルごとに値が記録され
-- 'augmentChainWithDeterministic' で Chain に注入できる。
--
-- 例:
--
-- > tau <- deterministic "tau" (1 / (sigma * sigma))
deterministic :: Text -> a -> Model a a
deterministic nm v = liftF (Deterministic nm v id)

-- | 名前付きデータプレースホルダを宣言する (PyMC `pm.Data` 相当)。
-- 既定値 @ys@ を持ち、後で 'withData' により差し替え可能。
--
-- 典型的な使い方:
--
-- > model = do
-- >   y <- dataNamed "y" trainData
-- >   mu <- sample "mu" (Normal 0 5)
-- >   observe "y" (Normal mu 1) y
--
-- そして @withData \"y\" testData model@ で同じ構造で別データを使う。
dataNamed :: Text -> [Double] -> Model a [Double]
dataNamed n ys = liftF (Data n ys id)

-- | Replace a named data block in the model. If no match exists the
-- model is returned unchanged.
-- 同じ名前が複数回出現する場合は全箇所で差し替わる。
--
-- 型シグネチャは @Model a r@ なので、ユーザーが @ModelP r@ から呼ぶ場合
-- そのまま多相的に使える (各 @a@ で個別に適用される)。
withData :: forall r. Text -> [Double] -> ModelP r -> ModelP r
withData n new m = mPoly
  where
    -- 戻り値を多相モデルとして再構築。各 @a@ 個別に元の m を走査する。
    mPoly :: forall a. (Floating a, Ord a) => Model a r
    mPoly = go m
      where
        go :: Model a r -> Model a r
        go (Pure r) = Pure r
        go (Free f) = Free (case f of
          Data n' ys k
            | n == n'   -> Data n' new (\d -> go (k d))
            | otherwise -> Data n' ys  (\d -> go (k d))
          Sample nm d k        -> Sample nm d (\v -> go (k v))
          Observe nm d ys nx   -> Observe nm d ys (go nx)
          Potential nm v nx    -> Potential nm v (go nx)
          Deterministic nm v k -> Deterministic nm v (\v' -> go (k v')))

-- | Latent multivariate-normal vector (analogous to PyMC's
-- @pm.MvNormal@ used as a latent).
--
-- 非中心化パラメタ化 + Cholesky 分解で実装:
--
--   z_i ~ Normal(0, 1)  (i = 0..K-1, 独立な latent)
--   x   = μ + L z       (L = Cholesky(Σ))
--
-- 各 z_i は通常の latent として NUTS が探索し、x は派生量として
-- Chain に記録される。共分散行列が他の latent に依存する形でも
-- 動作する (choleskyL は @(Floating a, Ord a)@ 多相)。
--
-- 共分散が非正定値のときは μ をそのまま返す (NUTS 探索中の不正領域
-- に対する graceful fallback)。
--
-- 戻り値: K 次元 latent ベクトル @[a]@ (μ + L z)。
-- Chain には @<name>_z<i>@ (raw latent) と @<name>_<i>@ (派生量) を保存。
mvNormalLatent :: forall a. (Floating a, Ord a)
               => Text -> [a] -> [[a]] -> Model a [a]
mvNormalLatent name muVec covMatrix = do
  let k = length muVec
  zs <- mapM (\i -> sample (name <> "_z" <> T.pack (show i)) (Normal 0 1))
             [0 .. k - 1]
  let xs = case choleskyL covMatrix of
        Just l  -> [ (muVec !! i) +
                       sum [ ((l !! i) !! j) * (zs !! j)
                           | j <- [0 .. i] ]
                   | i <- [0 .. k - 1] ]
        Nothing -> muVec      -- non-PD のフォールバック
  mapM
    (\(i, x) -> deterministic (name <> "_" <> T.pack (show i)) x)
    (zip [0 :: Int ..] xs)

-- | LKJ 相関行列の Cholesky factor (PyMC @LKJCholeskyCov@ 相当)。
--
-- LKJ(η) 事前: p(R) ∝ |R|^(η-1)。η = 1 で uniform、η > 1 で I に集中。
--
-- 実装は canonical partial correlations (CPC) 法:
--   z_ij ~ scaled Beta(α_i, α_i) on (-1, 1),  α_i = η + (K - i - 1) / 2
--     (i = 1..K-1, j = 0..i-1)
--
-- 各 z_ij は @<name>_pc<i>_<j>@ (Beta latent in (0,1)、内部で 2u-1 に変換)
-- として保存。Cholesky factor の各要素は派生量 @<name>_L<i>_<j>@。
--
-- 戻り値: K×K 下三角行列 L (R = L Lᵀ となる相関の Cholesky)。
-- 対角は √(1 - Σ z_{i,k}²)、対角下は z_ij × √(Π_{k<j}(1-z_{i,k}²))。
lkjCorrCholesky :: forall a. (Floating a, Ord a)
                => Text -> Int -> a -> Model a [[a]]
lkjCorrCholesky name k eta
  | k < 2     = error "lkjCorrCholesky: dimension must be >= 2"
  | otherwise = do
      -- 各 (i, j) で 1 <= j < i <= K-1 の partial correlation を sample
      let pcIndices = [(i, j) | i <- [1 .. k - 1], j <- [0 .. i - 1]]
      pcs <- mapM
        (\(i, j) -> do
            let alpha = eta + fromIntegral (k - i - 1) / 2
                tag   = T.pack (show i) <> "_" <> T.pack (show j)
            u <- sample (name <> "_u" <> tag) (Beta alpha alpha)
            deterministic (name <> "_pc" <> tag) (2 * u - 1))
        pcIndices
      -- (i,j) → z_ij マップ
      let pcMap = zip pcIndices pcs
          lookupPC i j = head [v | ((ii, jj), v) <- pcMap, ii == i, jj == j]
      -- Cholesky factor を構築 (下三角)
      let lRow i =
            [ if j > i then 0
              else if i == 0 && j == 0 then 1
              else if j == i  -- 対角
                   then sqrt (1 - sum [ let z = lookupPC i kk
                                        in z * z | kk <- [0 .. i - 1] ])
              else            -- 対角下 j < i
                let z       = lookupPC i j
                    factor2 = product [ let z' = lookupPC i kk
                                        in 1 - z' * z' | kk <- [0 .. j - 1] ]
                in z * sqrt factor2
            | j <- [0 .. k - 1] ]
          lMat = [lRow i | i <- [0 .. k - 1]]
      -- L 各要素を deterministic として保存
      _ <- mapM
        (\(i, j) ->
          deterministic (name <> "_L" <> T.pack (show i) <> "_" <> T.pack (show j))
                        ((lMat !! i) !! j))
        [(i, j) | i <- [0 .. k - 1], j <- [0 .. i]]
      return lMat

-- | AR(1) latent 時系列 (PyMC `pm.AR1` 相当)。
--
-- 状態方程式:  x_t = ϕ x_{t−1} + ε_t,   ε_t ~ Normal(0, σ)
-- 初期分布:    x_0 ~ Normal(0, σ / √(1 − ϕ²))   (定常分布、|ϕ| < 1 なら有限)
--
-- 引数 @phi@ は AR 係数、@sigma@ は innovation の sd。N 個の latent
-- 状態 x_0 .. x_{N-1} を非中心化パラメタ化で sample する:
--
--   raw_t ~ Normal(0, 1)
--   x_t = phi * x_{t-1} + sigma * raw_t       (t > 0)
--   x_0 = (sigma / √(1 - ϕ²)) * raw_0
--
-- 戻り値: x_0 .. x_{N-1} の latent 値リスト ([a])。各 raw_t は
-- @<name>_raw<t>@、x_t 自体は派生量 @<name>_<t>@ として保存。
--
-- |ϕ| ≥ 1 のフォールバック: 初期 sd を sigma に置き換える。
ar1Latent :: forall a. (Floating a, Ord a)
          => Text -> Int -> a -> a -> Model a [a]
ar1Latent name nT phi sigma
  | nT < 1 = error "ar1Latent: length must be >= 1"
  | otherwise = do
      raws <- mapM
        (\t -> sample (name <> "_raw" <> T.pack (show t)) (Normal 0 1))
        [0 .. nT - 1]
      let phi2     = phi * phi
          stat     = if phi2 < 1
                       then sigma / sqrt (1 - phi2)
                       else sigma   -- フォールバック
          x0       = stat * head raws
          xs       = scanl
                       (\xPrev (rt, _) -> phi * xPrev + sigma * rt)
                       x0
                       (zip (tail raws) [(1 :: Int) ..])
      _ <- mapM
        (\(t, x) -> deterministic
                       (name <> "_" <> T.pack (show t)) x)
        (zip [0 :: Int ..] xs)
      return xs

-- | 非中心化 (non-centered) 正規分布。
--
-- @x ~ Normal(loc, scale)@ を直接サンプリングする代わりに、
--
-- > raw <- sample (name <> "_raw") (Normal 0 1)
-- > deterministic name (loc + scale * raw)
--
-- に展開する。loc / scale が他の latent に依存するとき、centered
-- パラメタ化は HMC の posterior が病的になりやすいので、それを
-- 緩和するヘルパ。Neal's funnel が代表例。
--
-- 戻り値は constrained な値 @loc + scale * raw@。Chain には
-- @<name>_raw@ (latent) と @<name>@ (derived) の両方が保存される。
nonCenteredNormal :: Num a => Text -> a -> a -> Model a a
nonCenteredNormal name loc scale = do
  raw <- sample (name <> "_raw") (Normal 0 1)
  deterministic name (loc + scale * raw)

-- | Dirichlet distribution (analogous to PyMC's @pm.Dirichlet@), expanded
-- via stick-breaking
-- latent ベクトル。
--
-- 引数:
--   * @name@   : ベース名。展開後は @<name>_b<i>@ (i=0..K-2) が Beta 由来の
--                棒折り変数、@<name>_<i>@ (i=0..K-1) が deterministic で
--                記録された π 成分。
--   * @alphas@ : 集中度ベクトル α = (α_1,...,α_K)。長さ K ≥ 2。
--
-- アルゴリズム:
--   k = 1..K-1 で β_k ~ Beta(α_k, Σ_{j>k} α_j) を sample する。
--   π_1 = β_1,  π_k = β_k Π_{j<k} (1 − β_j),  π_K = Π_{j<K} (1 − β_j)
--
-- これは π ~ Dirichlet(α) と厳密に等価なので、追加の Jacobian 補正は不要。
-- HMC/NUTS では β_k が UnitIntervalT (logit) で自動的に
-- (0,1) ↔ ℝ 変換されるので、シンプレックス制約は満たされる。
dirichlet :: forall a. (Floating a, Ord a) => Text -> [a] -> Model a [a]
dirichlet name alphas = do
  let k = length alphas
  if k < 2
    then error "dirichlet: 長さ 2 未満のベクトルは未対応"
    else do
      let -- α_k+1..K の累積和 (右から)。長さ K (最後の要素は 0)
          tailSums = scanr (+) 0 alphas
      -- β_0..β_{K-2} を sample
      betas <- mapM
        (\i -> sample (name <> "_b" <> T.pack (show i))
                      (Beta (alphas !! i) (tailSums !! (i + 1))))
        [0 .. k - 2]
      -- 残り棒の累積積 prods[i] = Π_{j<i} (1 - β_j),  prods[0] = 1
      let prods = scanl (\acc b -> acc * (1 - b)) (1 :: a) betas
          -- π_i = β_i * prods[i] for i < K-1, π_{K-1} = prods[K-1]
          pis = [ if i < length betas
                    then (betas !! i) * (prods !! i)
                    else prods !! i
                | i <- [0 .. k - 1] ]
      -- 各 π_i を deterministic として保存し戻り値にも返す
      mapM (\(i, p) ->
              deterministic (name <> "_" <> T.pack (show i)) p)
           (zip [0 :: Int ..] pis)

-- ---------------------------------------------------------------------------
-- 構造検査
-- ---------------------------------------------------------------------------

data NodeKind = LatentN | ObservedN Int  deriving (Show, Eq)

data Node = Node
  { nodeName :: Text
  , nodeKind :: NodeKind
  , nodeDist :: Text         -- 分布名 (e.g. "Normal")
  , nodeDeps :: Set Text     -- 直接の親 (依存変数)
  } deriving (Show)

-- | Walk the model with placeholder zeros and collect 'Node' metadata.
-- 依存関係 ('nodeDeps') は 'extractDeps' を使うこと (placeholder 走査では取れない)。
collectNodes :: forall r. ModelP r -> [Node]
collectNodes m = go m []
  where
    go :: Model Double r -> [Node] -> [Node]
    go (Pure _) acc = reverse acc
    go (Free (Sample n d k)) acc =
      go (k 0) (Node n LatentN (distName d) Set.empty : acc)
    go (Free (Observe n d ys next)) acc =
      go next (Node n (ObservedN (length ys)) (distName d) Set.empty : acc)
    go (Free (Potential _ _ next)) acc = go next acc   -- Node 表示には含めない
    go (Free (Deterministic _ v k)) acc = go (k v) acc
    go (Free (Data _ ys k)) acc = go (k ys) acc

sampleNames :: ModelP r -> [Text]
sampleNames m = [nodeName n | n <- collectNodes m, nodeKind n == LatentN]

-- ---------------------------------------------------------------------------
-- 評価インタープリタ
-- ---------------------------------------------------------------------------

-- | Polymorphic interpreter that computes the log-joint
-- @log p(θ, y)@.
-- 引数 @a@ を @Double@ にすると数値評価、@Reverse s Double@ にすると AD 評価が可能。
logJoint :: (Floating a, Ord a) => Model a r -> Map Text a -> a
logJoint model params = go model 0
  where
    go (Pure _) acc = acc
    go (Free (Sample n d k)) acc =
      case Map.lookup n params of
        Nothing  -> negInf
        Just v   ->
          let lp = logDensity d v
          in go (k v) (acc + lp)
    go (Free (Observe _ d ys next)) acc =
      let ll = obsLogSum d ys
      in go next (acc + ll)
    go (Free (Potential _ v next)) acc = go next (acc + v)
    go (Free (Deterministic _ v k)) acc = go (k v) acc
    go (Free (Data _ ys k)) acc = go (k ys) acc

-- | log p(θ) のみ (prior 部分)。
logPrior :: (Floating a, Ord a) => Model a r -> Map Text a -> a
logPrior model params = go model 0
  where
    go (Pure _) acc = acc
    go (Free (Sample n d k)) acc =
      case Map.lookup n params of
        Nothing -> negInf
        Just v  -> go (k v) (acc + logDensity d v)
    go (Free (Observe _ _ _ next)) acc = go next acc
    go (Free (Potential _ v next)) acc = go next (acc + v)
    go (Free (Deterministic _ v k)) acc = go (k v) acc
    go (Free (Data _ ys k)) acc = go (k ys) acc

-- | log p(y | θ) のみ (likelihood 部分)。
logLikelihood :: (Floating a, Ord a) => Model a r -> Map Text a -> a
logLikelihood model params = go model 0
  where
    go (Pure _) acc = acc
    go (Free (Sample n _ k)) acc =
      case Map.lookup n params of
        Nothing -> go (k 0) acc
        Just v  -> go (k v) acc
    go (Free (Observe _ d ys next)) acc =
      let ll = obsLogSum d ys
      in go next (acc + ll)
    go (Free (Potential _ _ next)) acc = go next acc   -- Potential は事前項とみなす
    go (Free (Deterministic _ v k)) acc = go (k v) acc
    go (Free (Data _ ys k)) acc = go (k ys) acc

-- | For each observe node, return its distribution evaluated at the
-- current parameter values together with the observed data.
-- Gibbs サンプラーが共役構造を検出する際に、潜在変数の現在値に対する
-- 観測分布のパラメータを得るために使う (Double 特殊化版)。
--
-- 例: @y ~ Normal(mu, sigma)@ で @ps = {mu=2, sigma=0.5}@ を渡すと
-- @[(\"y\", Normal 2 0.5, [...])]@ を返す。
runObserveDists :: Model Double r
                -> Map Text Double
                -> [(Text, Distribution Double, [Double])]
runObserveDists (Pure _) _ = []
runObserveDists (Free (Sample n _ k)) ps =
  runObserveDists (k (Map.findWithDefault 0 n ps)) ps
runObserveDists (Free (Observe n d ys next)) ps =
  (n, d, ys) : runObserveDists next ps
runObserveDists (Free (Potential _ _ next)) ps =
  runObserveDists next ps
runObserveDists (Free (Deterministic _ v k)) ps =
  runObserveDists (k v) ps
runObserveDists (Free (Data _ ys k)) ps =
  runObserveDists (k ys) ps

-- | For each sample node, return @(name, prior distribution)@ in the
-- @Double@-specialized form.
-- Gibbs サンプラーの共役検出で「この潜在変数の事前は Gamma か Beta か」を
-- 判定するために使う。継続値はプレースホルダ 0 を流す。
priorList :: Model Double r -> [(Text, Distribution Double)]
priorList (Pure _) = []
priorList (Free (Sample n d k)) = (n, d) : priorList (k 0)
priorList (Free (Observe _ _ _ next)) = priorList next
priorList (Free (Potential _ _ next)) = priorList next
priorList (Free (Deterministic _ v k)) = priorList (k v)
priorList (Free (Data _ ys k)) = priorList (k ys)

-- ---------------------------------------------------------------------------
-- 互換 API
-- ---------------------------------------------------------------------------

-- | パラメータ名 → 値 のマップ (constrained 空間)。
type Params = Map Text Double

-- | Per-observation log-likelihood (used by WAIC / LOO-CV).
-- 各 Observe ノードのすべての観測値の logDensity を平坦リストで返す。
perObsLogLiks :: forall r. ModelP r -> Params -> [Double]
perObsLogLiks m params = go m []
  where
    go :: Model Double r -> [Double] -> [Double]
    go (Pure _) acc = reverse acc
    go (Free (Sample n _ k)) acc =
      go (k (Map.findWithDefault 0 n params)) acc
    go (Free (Observe _ d ys next)) acc =
      let lls = case d of
            MvNormal mu cov ->
              let k = length mu
              in [ mvNormalLogDensity mu cov (map realToFrac yv :: [Double])
                 | yv <- chunksOf k ys ]
            Multinomial nn pp ->
              let k = length pp
              in [ multinomialLogDensity nn pp yv | yv <- chunksOf k ys ]
            _ -> [ logDensityObs d y | y <- ys ]
      in go next (reverse lls ++ acc)
    go (Free (Potential _ _ next)) acc = go next acc
    go (Free (Deterministic _ v k)) acc = go (k v) acc
    go (Free (Data _ ys k)) acc = go (k ys) acc

-- | Evaluate every 'Deterministic' node and return the resulting
-- derived-quantity @Map@.
--
-- @params@ は latent 変数 (sample) の値を表す Map。Deterministic は
-- それらから導出される量で、ここでは Double 特殊化で評価する。
runDeterministics :: forall r. ModelP r -> Params -> Map Text Double
runDeterministics m params = go m Map.empty
  where
    go :: Model Double r -> Map Text Double -> Map Text Double
    go (Pure _) acc = acc
    go (Free (Sample n _ k)) acc =
      go (k (Map.findWithDefault 0 n params)) acc
    go (Free (Observe _ _ _ next)) acc = go next acc
    go (Free (Potential _ _ next)) acc = go next acc
    go (Free (Deterministic n v k)) acc =
      go (k v) (Map.insert n v acc)
    go (Free (Data _ ys k)) acc = go (k ys) acc

-- | Evaluate 'runDeterministics' on every posterior sample and
-- 結果を 'chainSamples' の Map にマージした新しい Chain を返す。
-- これにより @chainVals@ / @posteriorSummary@ などのヘルパで派生量を
-- そのまま参照できる。
augmentChainWithDeterministic :: ModelP r -> Chain -> Chain
augmentChainWithDeterministic m ch =
  let aug ps = Map.union (runDeterministics m ps) ps
  in ch { chainSamples = map aug (chainSamples ch) }

-- | Human-readable summary of the model structure (no inference is run).
describeModel :: ModelP r -> Text
describeModel m = T.unlines (header : map fmtNode (collectNodes m))
  where
    header = "Model nodes:"
    fmtNode n = case nodeKind n of
      LatentN     -> "  [latent]   " <> nodeName n <> " ~ " <> nodeDist n
      ObservedN k -> "  [observed] " <> nodeName n <> " ~ " <> nodeDist n
                  <> "  (n=" <> T.pack (show k) <> ")"

-- | DAG representation of the model. Edges are derived automatically by
-- 'extractDeps'.
data ModelGraph = ModelGraph
  { mgNodes :: [Node]
  , mgEdges :: [(Text, Text)]   -- (parent, child)
  } deriving (Show)

-- | 多相モデルから DAG を自動構築する (Track 型による依存追跡)。
--
-- 同じ名前で複数登場する Observe ノード (例: 回帰モデルで観測点ごとに
-- @observe \"y\"@ を発行する場合) は 1 つに統合される。観測数の合計と
-- 親変数集合の和をマージし、エッジも重複排除する。
buildModelGraph :: ModelP r -> ModelGraph
buildModelGraph m =
  let rawNodes = extractDeps m
      merged   = mergeByName rawNodes
      edges    = Set.toList $ Set.fromList
                   [ (parent, nodeName n)
                   | n <- merged
                   , parent <- Set.toList (nodeDeps n) ]
  in ModelGraph merged edges
  where
    -- 同名ノードを統合: ObservedN n1 + ObservedN n2 → ObservedN (n1+n2)
    -- LatentN は最初の出現を残す。deps は和集合。
    mergeByName ns = mergeGo ns Map.empty []
    mergeGo [] _ acc = reverse acc
    mergeGo (n:ns) seen acc =
      let nm = nodeName n
      in case Map.lookup nm seen of
           Nothing -> mergeGo ns (Map.insert nm n seen) (n : acc)
           Just prev ->
             let merged' = Node
                   { nodeName = nm
                   , nodeKind = case (nodeKind prev, nodeKind n) of
                       (ObservedN a, ObservedN b) -> ObservedN (a + b)
                       (k, _)                     -> k
                   , nodeDist = nodeDist prev
                   , nodeDeps = nodeDeps prev <> nodeDeps n
                   }
                 acc' = map (\x -> if nodeName x == nm then merged' else x) acc
             in mergeGo ns (Map.insert nm merged' seen) acc'

-- ---------------------------------------------------------------------------
-- AD 勾配
-- ---------------------------------------------------------------------------

-- | AD で勾配を計算する。@names@ の順で各パラメータに対する偏微分を返す。
gradAD :: ModelP r -> [Text] -> [Double] -> [Double]
gradAD m names xs0 = grad f xs0
  where
    f xs =
      let params = Map.fromList (zip names xs)
      in logJoint m params

-- | unconstrained 空間で AD 勾配を計算する (HMC 用)。
-- 各パラメータに制約変換を適用し、Jacobian 補正項込みの log-joint を微分する。
gradADU :: ModelP r -> [Text] -> [Transform] -> [Double] -> [Double]
gradADU m names trans us0 = grad f us0
  where
    f us =
      let paramsC = Map.fromList
            [ (n, invTransformF t u)
            | (n, t, u) <- zip3 names trans us ]
          logJac  = sum
            [ logJacF t u
            | (t, u) <- zip trans us ]
      in logJoint m paramsC + logJac

-- ---------------------------------------------------------------------------
-- 制約変換 (Floating 多相版)
-- ---------------------------------------------------------------------------

-- | unconstrained → constrained 変換 (Floating 多相)。
--
-- > UnconstrainedT: θ = u
-- > PositiveT:      θ = exp(u)
-- > UnitIntervalT:  θ = sigmoid(u) = 1/(1+exp(-u))
invTransformF :: Floating a => Transform -> a -> a
invTransformF UnconstrainedT u = u
invTransformF PositiveT      u = exp u
invTransformF UnitIntervalT  u = 1 / (1 + exp (-u))

-- | log |∂θ/∂u| — Jacobian 行列式の対数 (Floating 多相)。
logJacF :: Floating a => Transform -> a -> a
logJacF UnconstrainedT _ = 0
logJacF PositiveT      u = u                       -- log(exp u) = u
logJacF UnitIntervalT  u =
  let p = 1 / (1 + exp (-u))
  in log p + log (1 - p)                           -- log σ(u)(1-σ(u))

-- | 各 latent 変数の事前分布から制約変換を自動検出する。
getTransforms :: ModelP r -> Map Text Transform
getTransforms m = Map.fromList
  [ (nodeName n, transformFor (nodeDist n))
  | n <- collectNodes m
  , nodeKind n == LatentN
  ]
  where
    transformFor "Normal"      = UnconstrainedT
    transformFor "Exponential" = PositiveT
    transformFor "Gamma"       = PositiveT
    transformFor "Beta"        = UnitIntervalT
    transformFor "StudentT"    = UnconstrainedT
    transformFor "Cauchy"      = UnconstrainedT
    transformFor "HalfNormal"  = PositiveT
    transformFor "HalfCauchy"  = PositiveT
    transformFor "LogNormal"   = PositiveT  -- support: x>0 (log は AD 安全)
    transformFor "Uniform"     = UnconstrainedT  -- 注: 真の制約変換は logit-on-(lo,hi) だが現状は未実装
    transformFor "Bernoulli"   = UnitIntervalT   -- p ∈ (0,1)
    transformFor "Categorical" = UnconstrainedT  -- ベクトル制約は未対応 (Dirichlet で別途)
    transformFor "Mixture"     = UnconstrainedT  -- 混合分布の潜在は通常 unconstrained
    transformFor "Truncated"   = UnconstrainedT  -- 簡易: 範囲制約は logDensity 内で扱う
    transformFor "Censored"    = UnconstrainedT
    transformFor "MvNormal"    = UnconstrainedT  -- observation-only
    transformFor "InverseGamma" = PositiveT
    transformFor "Weibull"     = PositiveT
    transformFor "Pareto"      = PositiveT
    transformFor "BetaBinomial" = UnitIntervalT
    transformFor "VonMises"    = UnconstrainedT  -- 角度 (-π, π]
    transformFor _             = UnconstrainedT

-- | unconstrained 空間における log-joint (Jacobian 補正込み)。
-- Jacobian 補正で確率密度の積分を保存する。
logJointUnconstrained :: forall a r. (Floating a, Ord a)
                      => Model a r
                      -> [Text]      -- ^ パラメータ順序
                      -> [Transform] -- ^ 各パラメータの変換種別
                      -> Map Text a  -- ^ unconstrained パラメータ値
                      -> a
logJointUnconstrained m names trans paramsU =
  let paramsC = Map.fromList
        [ (n, invTransformF t (Map.findWithDefault 0 n paramsU))
        | (n, t) <- zip names trans ]
      logJac  = sum
        [ logJacF t (Map.findWithDefault 0 n paramsU)
        | (n, t) <- zip names trans ]
  in logJoint m paramsC + logJac

-- ---------------------------------------------------------------------------
-- 依存追跡型 Track
-- ---------------------------------------------------------------------------

-- | Floating 演算を通して「この値はどの変数に依存するか」を伝播する型。
--
-- @ModelP@ をこの型で特殊化することで、各 Observe ノードが
-- どの latent 変数に依存しているか自動抽出できる。
data Track = Track
  { trackVal  :: !Double
  , trackDeps :: !(Set Text)
  } deriving (Show, Eq)

-- | 変数として登場する Track (deps に自分の名前を入れる)。
trackVar :: Text -> Double -> Track
trackVar n v = Track v (Set.singleton n)

-- | 定数として扱う Track (deps なし)。
trackConst :: Double -> Track
trackConst v = Track v Set.empty

-- 自然な順序関係 (Double の比較を使う)
instance Ord Track where
  compare a b = compare (trackVal a) (trackVal b)

-- Floating の階段
instance Num Track where
  fromInteger n = trackConst (fromInteger n)
  Track a sa + Track b sb = Track (a + b) (sa <> sb)
  Track a sa - Track b sb = Track (a - b) (sa <> sb)
  Track a sa * Track b sb = Track (a * b) (sa <> sb)
  abs    (Track a sa) = Track (abs a) sa
  signum (Track a sa) = Track (signum a) sa
  negate (Track a sa) = Track (negate a) sa

instance Fractional Track where
  fromRational r = trackConst (fromRational r)
  Track a sa / Track b sb = Track (a / b) (sa <> sb)

instance Floating Track where
  pi             = trackConst pi
  exp   (Track a sa) = Track (exp   a) sa
  log   (Track a sa) = Track (log   a) sa
  sin   (Track a sa) = Track (sin   a) sa
  cos   (Track a sa) = Track (cos   a) sa
  tan   (Track a sa) = Track (tan   a) sa
  asin  (Track a sa) = Track (asin  a) sa
  acos  (Track a sa) = Track (acos  a) sa
  atan  (Track a sa) = Track (atan  a) sa
  sinh  (Track a sa) = Track (sinh  a) sa
  cosh  (Track a sa) = Track (cosh  a) sa
  tanh  (Track a sa) = Track (tanh  a) sa
  asinh (Track a sa) = Track (asinh a) sa
  acosh (Track a sa) = Track (acosh a) sa
  atanh (Track a sa) = Track (atanh a) sa
  sqrt  (Track a sa) = Track (sqrt  a) sa
  Track a sa ** Track b sb = Track (a ** b) (sa <> sb)
  logBase (Track a sa) (Track b sb) = Track (logBase a b) (sa <> sb)

instance Real Track where
  toRational = toRational . trackVal

instance RealFrac Track where
  properFraction (Track a sa) = let (i, f) = properFraction a in (i, Track f sa)

-- | モデルを Track 型で実行し、各ノードの依存関係を抽出する。
--
-- Sample n: その変数自体は @{n}@ に依存する (自己依存)。
-- Observe n: 分布のパラメータに含まれる latent 変数の集合を deps とする。
extractDeps :: forall r. ModelP r -> [Node]
extractDeps m = go m []
  where
    go :: Model Track r -> [Node] -> [Node]
    go (Pure _) acc = reverse acc
    go (Free (Sample n d k)) acc =
      let parentDeps = distDepsT d
          node = Node n LatentN (distName d) parentDeps
          v    = trackVar n 1.0  -- 1 にすると log/exp が安全
      in go (k v) (node : acc)
    go (Free (Observe n d ys next)) acc =
      let parentDeps = distDepsT d
          node = Node n (ObservedN (length ys)) (distName d) parentDeps
      in go next (node : acc)
    go (Free (Potential nm v next)) acc =
      -- Potential も DAG 上は「依存を持つ無形ノード」として可視化
      let parentDeps = trackDeps v
          node = Node nm LatentN "Potential" parentDeps
      in go next (node : acc)
    go (Free (Deterministic nm v k)) acc =
      -- Deterministic も親 latent からの導出関係を保存
      let parentDeps = trackDeps v
          node = Node nm LatentN "Deterministic" parentDeps
      in go (k v) (node : acc)
    go (Free (Data _ ys k)) acc =
      -- Data はデータプレースホルダ。継続には [Double] をそのまま渡す。
      go (k ys) acc

-- | Distribution Track に含まれる依存変数集合を取り出す。
distDepsT :: Distribution Track -> Set Text
distDepsT (Normal mu sig)    = trackDeps mu <> trackDeps sig
distDepsT (Exponential r)    = trackDeps r
distDepsT (Gamma s r)        = trackDeps s <> trackDeps r
distDepsT (Beta a b)         = trackDeps a <> trackDeps b
distDepsT (Poisson lam)      = trackDeps lam
distDepsT (Binomial _ p)     = trackDeps p
distDepsT (Uniform lo hi)    = trackDeps lo <> trackDeps hi
distDepsT (StudentT df mu s) = trackDeps df <> trackDeps mu <> trackDeps s
distDepsT (Cauchy loc s)     = trackDeps loc <> trackDeps s
distDepsT (HalfNormal s)     = trackDeps s
distDepsT (HalfCauchy s)     = trackDeps s
distDepsT (LogNormal mu s)   = trackDeps mu <> trackDeps s
distDepsT (Bernoulli p)      = trackDeps p
distDepsT (Categorical ps)   = mconcat (map trackDeps ps)
distDepsT (Mixture ws ds)    = mconcat (map trackDeps ws) <> mconcat (map distDepsT ds)
distDepsT (Truncated d mLo mHi) =
  distDepsT d <> maybe mempty trackDeps mLo <> maybe mempty trackDeps mHi
distDepsT (Censored  d mLo mHi) =
  distDepsT d <> maybe mempty trackDeps mLo <> maybe mempty trackDeps mHi
distDepsT (MvNormal mus covRows) =
  mconcat (map trackDeps mus)
    <> mconcat (concatMap (map trackDeps) covRows)
distDepsT (NegativeBinomial mu alpha) = trackDeps mu <> trackDeps alpha
distDepsT (Multinomial _ ps) = mconcat (map trackDeps ps)
distDepsT (ZeroInflatedPoisson psi lam) = trackDeps psi <> trackDeps lam
distDepsT (ZeroInflatedBinomial _ psi p) = trackDeps psi <> trackDeps p
distDepsT (InverseGamma a b) = trackDeps a <> trackDeps b
distDepsT (Weibull k l)      = trackDeps k <> trackDeps l
distDepsT (Pareto a xm)      = trackDeps a <> trackDeps xm
distDepsT (BetaBinomial _ a b) = trackDeps a <> trackDeps b
distDepsT (VonMises mu k)    = trackDeps mu <> trackDeps k

-- | Track でモデルを評価する (log joint も依存集合付きで計算)。
runTrack :: forall r. ModelP r -> Map Text Track -> Track
runTrack m params = logJoint (m :: Model Track r) params

-- ---------------------------------------------------------------------------
-- 数値ユーティリティ
-- ---------------------------------------------------------------------------

-- | log Γ(z) の Stirling 近似 (z > 0)。AD でも Track でも使える多相版。
lgammaApprox :: (Floating a, Ord a) => a -> a
lgammaApprox z
  | z < 12    = lgammaApprox (z + 1) - log z
  | otherwise = (z - 0.5) * log z - z + 0.5 * log (2 * pi)
              + 1 / (12 * z) - 1 / (360 * z ^ (3::Int))

logFactorial :: Int -> Double
logFactorial n
  | n <= 1    = 0
  | otherwise = sum (map log [2 .. fromIntegral n])

logBinomCoeff :: Int -> Int -> Double
logBinomCoeff n k = logFactorial n - logFactorial k - logFactorial (n - k)

-- | log I_0(x) — 修正 Bessel 関数 (第一種・order 0) の対数。VonMises 用。
-- 小 x: 級数 I_0(x) = Σ (x/2)^(2k) / (k!)² (k = 0..)
-- 大 x: 漸近展開 I_0(x) ≈ exp(x) / √(2πx) × [1 + 1/(8x) + 9/(128x²) + …]
-- AD/Track 互換のため (Floating a, Ord a) 多相。
logBesselI0 :: (Floating a, Ord a) => a -> a
logBesselI0 x
  | x < 0     = logBesselI0 (-x)  -- 偶関数
  | x < 3.75  =
      -- Abramowitz & Stegun 9.8.1: 多項式近似 (誤差 < 1.6e-7)
      let t = (x / 3.75) ^ (2::Int)
          i0 = 1 + t * (3.5156229 + t * (3.0899424 + t * (1.2067492
             + t * (0.2659732 + t * (0.0360768 + t * 0.0045813)))))
      in log i0
  | otherwise =
      -- Abramowitz & Stegun 9.8.2: 漸近 (誤差 < 1.9e-7)
      let t = 3.75 / x
          poly = 0.39894228 + t * (0.01328592 + t * (0.00225319
               + t * (-0.00157565 + t * (0.00916281 + t * (-0.02057706
               + t * (0.02635537 + t * (-0.01647633 + t * 0.00392377)))))))
      in x - 0.5 * log x + log poly
