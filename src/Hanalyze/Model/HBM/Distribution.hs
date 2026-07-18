{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Hanalyze.Model.HBM.Distribution
-- Description : HBM の多相確率分布 ADT と密度・CDF
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- HBM の多相確率分布 ADT と密度・CDF。
--
-- 'Distribution' は値型 @a@ に多相な確率分布。 @a@ は @Double@ (サンプリング・
-- 密度)、 @Reverse s Double@ (AD 勾配)、 @Track@ (依存追跡) を渡せる。 本モジュール
-- は型・名前・**事前密度** 'logDensity'・多変量密度・閉形式 CDF を提供し、 純粋
-- leaf 'Hanalyze.Model.HBM.Util' のみに依存する。
--
-- ★観測尤度 'logDensityObs' / 'obsLogSum' は **含めない** (Eval 層へ残置。
-- Distribution→Eval の cycle を避けるため・request/254)。
--
-- Phase 58.3 で 'Hanalyze.Model.HBM' から責務分離して抽出。 数値は 1 bit 不変。
module Hanalyze.Model.HBM.Distribution
  ( Distribution (..)
  , distName
  , nameToTransform
  , distToTransform
  , logDensity
  , logDensityRD
  , logDensityObs
  , obsLogSum
  , multinomialLogDensity
  , mvNormalLogDensity
  , mvNormalCholLogDensity
  , mvStudentTLogDensity
  , dirichletMultinomialLogDensity
  , wishartLogDensity
  , erfA
  , phiCdfA
  , distCDF
  , logCDF
  , logSF
  , logCDFInterval
  ) where

import Data.List (mapAccumL, zip4)
import Data.Text (Text)
-- Phase 92 B3: 'logDensityRD' (AD 定数正規化項の畳み込み) 用。 多相 'logDensity'
-- 本体は AD 非依存のまま。
import Data.Reflection (Reifies)
import qualified Numeric.AD.Internal.Reverse.Double as ADRD
import Hanalyze.Model.HBM.Util
import Hanalyze.Stat.Distribution (Transform (..))

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
  | MvNormalChol [a] [a] [[a]]
    -- ^ @MvNormalChol(μ, σ, L)@: multivariate normal parameterized by a
    --   scale vector @σ@ (length @k@) and a /correlation/ Cholesky factor
    --   @L@ (lower-triangular @k×k@, typically from 'lkjCorrCholesky').
    --   The covariance is @Σ = (diag σ · L)(diag σ · L)ᵀ@. The density
    --   uses the scaled Cholesky @M = diag σ · L@ directly (forward
    --   substitution, no re-decomposition) — numerically the most stable
    --   parameterization (Stan's @multi_normal_cholesky@ idiom).
    --   Observation-only; pass @k@-vectors via 'observeMV'.
  | MvNormalGpRBF [a] a a a
    -- ^ Phase 95 B-dsl: @MvNormalGpRBF(x, α, ρ, σ)@ — zero-mean GP 回帰尤度
    --   専用の多変量正規 (observation-only)。 共分散は RBF (exp-quad) カーネル
    --   @Σ_ij = α² exp(-0.5 (x_i-x_j)²/ρ²) + [i=j](1e-10 + σ)@ で内部構築する。
    --   汎用 'MvNormal' と密度は同値だが、 カーネルの役割 (x/α/ρ/σ) を型で明示
    --   保持することで、 勾配コンパイラ ('gpRBFAnalyticVG') が **Cholesky を AD
    --   tape に載せない閉形式随伴** (@∂Σ/∂α=2K'/α@・@∂Σ/∂ρ=K'∘d²/ρ³@・
    --   @∂Σ/∂σ=I@) を使える。 @x@ は共変量 data (定数)、 α/ρ/σ は latent。
    --   観測は length-@k@ ベクトルを 'observeMV' で渡す (μ=0 固定)。
  | HmmForwardNormal [a] [[a]] [a] a
    -- ^ Phase 92 A2: @HmmForwardNormal(π_0, trans, μs, σ)@ — Normal emission の
    --   隠れマルコフモデル周辺尤度 (observation-only)。 観測列 y_{1..T} 全体を
    --   1 つの多変量観測として 'observeMV' で渡す (@observeMV nm d [ys]@)。
    --   密度は @'hmmForwardLogLik' π_0 trans emit@ (emit[t][k] =
    --   Normal(μs[k], σ) の logpdf(y_t)) と同値。 状態役割 (π_0/遷移行/emission
    --   平均/σ) を型で明示保持することで、 勾配コンパイラ ('hmmAnalyticVG') が
    --   **forward-backward の閉形式随伴** (∂logL/∂emit = γ_t・∂logL/∂T_ij = ξ
    --   集計・AD tape ゼロ) を使える。 π_0 は非正規化可 (log 空間で加算されるのみ)。
  | ArmaNormal a a a a
    -- ^ Phase 101 A2: @ArmaNormal(μ, φ, θ, σ)@ — ARMA(1,1) の条件付き尤度
    --   (observation-only)。 観測列 y_{1..T} 全体を 1 つの多変量観測として
    --   'observeMV' で渡す (@observeMV nm d [ys]@)。 密度は Stan 原典 arma11 の
    --   err 逐次再帰 (@err_1 = y_1 − (μ+φμ)@・@err_t = y_t − μ − φ·y_{t−1} −
    --   θ·err_{t−1}@・@err_t ~ Normal(0, σ)@) と同値。 役割 (μ/φ/θ/σ) を型で
    --   明示保持することで、 勾配コンパイラ ('armaAnalyticVG') が **逆向き
    --   1 パスの閉形式随伴** (@ē_t = −e_t/σ² − θ·ē_{t+1}@ の線形随伴再帰・
    --   AD tape ゼロ) を使える。
  | GradedResponseIrt [a] [Int] [Double] [[Double]]
    -- ^ Phase 101 A3: @GradedResponseIrt(θs, ncats, δs, γs)@ — graded response
    --   IRT (順序ロジット・BUGS bones) の尤度 (observation-only)。 @θs@ =
    --   受験者能力 (latent・唯一の param 側)、 @ncats[j]@/@δs[j]@/@γs[j][k]@ =
    --   項目 j のカテゴリ数/識別力/カットポイント (**定数データ**)。 観測は
    --   grade 行列 (nChild×nItem 行優先・1-based カテゴリ・欠測 = −1) を
    --   'observeMV' で 1 観測として渡す (@observeMV nm d [grades]@)。
    --   密度は @Q_k = invlogit(δ(θ−γ_k))@ の隣接差 p のカテゴリ対数確率と
    --   同値。 θ_i (スカラ) 毎に独立なため、 勾配コンパイラ
    --   ('gradedIrtAnalyticVG') が **解析勾配** (@dQ/dθ = δ·Q(1−Q)@ の差分・
    --   AD tape ゼロ) を使える。
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
  | SkewNormal a a a
    -- ^ @SkewNormal(μ location, σ scale, α shape)@ (Phase 37-A2).
    --   @pdf = (2/σ) φ((x−μ)/σ) Φ(α(x−μ)/σ)@.
    --   @α = 0@ で標準正規。 @α > 0@ で右側に歪み、 @α < 0@ で左側。
    --   Sample は Henze 1986: @δ = α/√(1+α²)@,
    --   @X = μ + σ(δ |U₀| + √(1−δ²) U₁)@ with i.i.d. @U_i ~ N(0,1)@.
  | Logistic a a
    -- ^ @Logistic(μ location, s scale)@ (Phase 37-A2).
    --   @pdf = e^{−z} / (s(1+e^{−z})²)@ with @z = (x−μ)/s@.
    --   平均 @μ@、 分散 @s²π²/3@。 closed-form CDF あり。
  | Gumbel a a
    -- ^ @Gumbel(μ location, β scale)@ (Phase 37-A2、 最大値型極値分布)。
    --   @pdf = (1/β) exp(−z − e^{−z})@ with @z = (x−μ)/β@.
    --   平均 @μ + βγ@ (γ ≈ 0.5772 オイラー定数)、 分散 @β²π²/6@。
    --   closed-form CDF: @F(x) = exp(−exp(−z))@.
  | AsymmetricLaplace a a a
    -- ^ @AsymmetricLaplace(b scale > 0, κ asymmetry > 0, μ location)@
    --   (Phase 37-A2、 PyMC parameterization、 分位点回帰の尤度)。
    --   @pdf = b/(κ+1/κ) · exp(−b·κ·(x−μ))@ for @x ≥ μ@、
    --   @pdf = b/(κ+1/κ) · exp(b/κ·(x−μ))@ for @x < μ@。
    --   @κ = 1@ で対称ラプラス、 @κ > 1@ で右側裾長。
  | OrderedLogistic a [a]
    -- ^ @OrderedLogistic(η linear predictor, cuts = [c₁, …, c_{K-1}])@
    --   (Phase 37-A3、 順序ロジット回帰)。
    --   観測 @y ∈ {0, …, K-1}@、
    --   @P(y=k) = σ(c_{k+1} − η) − σ(c_k − η)@ with
    --   @σ(x) = 1/(1+e^{-x})@, @c_0 = −∞, c_K = +∞@.
    --   cuts は **increasing** 列、 入力側で確保すること。
    --   observation-only。
  | DiscreteUniform Int Int
    -- ^ @DiscreteUniform(lo, hi)@ (Phase 37-A3、 包含両端)。
    --   @pmf = 1/(hi-lo+1)@ for @lo ≤ y ≤ hi@。 observation-only。
  | Geometric a
    -- ^ @Geometric(p)@ (Phase 37-A3、 PyMC 慣例 = 初回成功までの試行回数)。
    --   support @y = 1, 2, 3, …@、 @pmf = (1−p)^{y-1} p@。
    --   observation-only。
  | HyperGeometric Int Int Int
    -- ^ @HyperGeometric(N total, K successes, n draws)@ (Phase 37-A3、
    --   非復元抽出の成功数)。
    --   @pmf = C(K, y) C(N-K, n-y) / C(N, n)@、
    --   support @max(0, n+K-N) ≤ y ≤ min(n, K)@。 observation-only。
  | ZeroInflatedNegativeBinomial a a a
    -- ^ @ZeroInflatedNegativeBinomial(ψ, μ, α)@ (Phase 37-A3、 過分散ゼロ過剰)。
    --   @P(0) = ψ + (1-ψ) (α/(α+μ))^α@、
    --   @P(k>0) = (1-ψ) · NegBin(k | μ, α)@。
  | MvStudentT a [a] [[a]]
    -- ^ @MvStudentT(ν, μ, Σ)@ (Phase 37-A4、 ロバスト多変量)。
    --   @ν > 0@ 自由度、 @μ@ は @k@ 次元平均、 @Σ@ は @k×k@ SPD scale matrix。
    --   観測 (observation-only)、 @y :: [Double]@ は flatten された
    --   @k@ ベクトル列 (@observeMV@ で渡す)。
    --   @ν → ∞@ で MvNormal に収束。
  | DirichletMultinomial Int [a]
    -- ^ @DirichletMultinomial(n trials, α concentration K-vector)@
    --   (Phase 37-A4、 過分散 multinomial)。
    --   観測 y は @K@ 次元 counts、 @Σ yᵢ = n@。
    --   @logpmf = log Γ(α₀) − log Γ(α₀+n)
    --           + Σ [log Γ(yᵢ+αᵢ) − log Γ(αᵢ)]
    --           + log n! − Σ log yᵢ!@、 @α₀ = Σαᵢ@.
    --   observation-only。
  | Triangular a a a
    -- ^ @Triangular(lower, c mode, upper)@ (Phase 39-A1、 弱情報事前)。
    --   Support @[lower, upper]@、 @lower ≤ c ≤ upper@。
    --   @pdf = 2(x-lower)/((upper-lower)(c-lower))@ for @lower ≤ x ≤ c@、
    --   @pdf = 2(upper-x)/((upper-lower)(upper-c))@ for @c < x ≤ upper@。
    --   closed-form CDF / 逆 CDF sample。
  | Kumaraswamy a a
    -- ^ @Kumaraswamy(a, b)@ (Phase 39-A1、 Beta 代替、 closed-form CDF)。
    --   Support @(0, 1)@、 @pdf = a·b·x^{a-1}(1-x^a)^{b-1}@。
    --   CDF @= 1 - (1-x^a)^b@、 sample @x = (1-(1-u)^{1/b})^{1/a}@。
  | Rice a a
    -- ^ @Rice(ν, σ)@ (Phase 39-A1、 MRI / Rayleigh 拡張)。
    --   Support @x ≥ 0@、 @ν ≥ 0@、 @σ > 0@。
    --   @pdf = (x/σ²) exp(-(x²+ν²)/(2σ²)) I_0(xν/σ²)@、
    --   @ν = 0@ で Rayleigh(σ)。 @logBesselI0@ で評価。
    --   sample: @X = √(Y₁² + Y₂²)@ with @Y₁ ~ N(ν, σ²), Y₂ ~ N(0, σ²)@。
  | DiscreteWeibull a a
    -- ^ @DiscreteWeibull(q, β)@ (Phase 39-A1、 整数 Weibull)。
    --   Support @{0, 1, 2, …}@、 @0 < q < 1, β > 0@。
    --   @P(X ≤ k) = 1 - q^{(k+1)^β}@、
    --   @pmf(k) = q^{k^β} - q^{(k+1)^β}@。 observation-only。
    --   sample: @k = ⌈(log(1-u)/log q)^{1/β}⌉ - 1@。
  | Wishart a [[a]]
    -- ^ @Wishart(ν degrees, V scale matrix)@ (Phase 39-A2、 共分散プライアの直接表現)。
    --   @ν > k-1@、 @V@ は @k×k@ SPD scale matrix。
    --   観測 (observation-only)、 @k×k@ 観測行列 W を flatten で渡す
    --   (長さ @k²@、 row-major)。 @observeMV@ で渡す想定。
    --   @logpdf(W) = -(νk/2) log 2 - (ν/2) log|V| - log Γ_k(ν/2)
    --              + ((ν-k-1)/2) log|W| - (1/2) tr(V⁻¹ W)@、
    --   @log Γ_k(z) = (k(k-1)/4) log π + Σ_{i=1}^k log Γ((z+1-i)/2)@。
  | Bound (Distribution a) (Maybe a) (Maybe a)
    -- ^ @Bound(d, lo, hi)@ (Phase 39-A3、 PyMC 互換)。
    --   @d@ の支持を @[lo, hi]@ に制限する。 'Truncated' とほぼ同義
    --   (実装も委譲)。 'Nothing' は @-∞ / +∞@。
    --   違いは語用論のみ: PyMC では prior 寄りで Bound、 観測寄りで
    --   Truncated を使う慣例があるため API として並べた。
  | OrderedProbit a [a]
    -- ^ @OrderedProbit(η linear predictor, cuts = [c₁, …, c_{K-1}])@
    --   (Phase 39-A3、 順序プロビット回帰)。
    --   @P(y=k) = Φ(c_{k+1} − η) − Φ(c_k − η)@ with
    --   @c_0 = −∞, c_K = +∞@、 Φ は標準正規 CDF (@phiCdfA@)。
    --   cuts は increasing 列、 入力側で確保。 observation-only。
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
distName MvNormalChol{} = "MvNormalChol"
distName MvNormalGpRBF{} = "MvNormalGpRBF"
distName HmmForwardNormal{} = "HmmForwardNormal"
distName ArmaNormal{} = "ArmaNormal"
distName GradedResponseIrt{} = "GradedResponseIrt"
distName NegativeBinomial{} = "NegativeBinomial"
distName Multinomial{}          = "Multinomial"
distName ZeroInflatedPoisson{}  = "ZeroInflatedPoisson"
distName ZeroInflatedBinomial{} = "ZeroInflatedBinomial"
distName InverseGamma{}         = "InverseGamma"
distName Weibull{}              = "Weibull"
distName Pareto{}               = "Pareto"
distName BetaBinomial{}         = "BetaBinomial"
distName VonMises{}             = "VonMises"
distName SkewNormal{}           = "SkewNormal"
distName Logistic{}             = "Logistic"
distName Gumbel{}               = "Gumbel"
distName AsymmetricLaplace{}    = "AsymmetricLaplace"
distName OrderedLogistic{}      = "OrderedLogistic"
distName DiscreteUniform{}      = "DiscreteUniform"
distName Geometric{}            = "Geometric"
distName HyperGeometric{}       = "HyperGeometric"
distName ZeroInflatedNegativeBinomial{} = "ZeroInflatedNegativeBinomial"
distName MvStudentT{}           = "MvStudentT"
distName DirichletMultinomial{} = "DirichletMultinomial"
distName Triangular{}           = "Triangular"
distName Kumaraswamy{}          = "Kumaraswamy"
distName Rice{}                 = "Rice"
distName DiscreteWeibull{}      = "DiscreteWeibull"
distName Wishart{}              = "Wishart"
distName Bound{}                = "Bound"
distName OrderedProbit{}        = "OrderedProbit"

-- | 分布名 → NUTS が探索する **unconstrained 変換種別**。 latent の制約付き台
-- (正値・単位区間) を実数空間へ写す種別を返す。
--
-- ★これが分布→変換の **唯一の表**。 'getTransforms'
-- (@Gradient@・node walk 版) も本関数へ委譲する。 分布を latent 化して台が
-- 変わる場合はここを更新する (1 箇所)。 未列挙は保守的に 'UnconstrainedT'。
nameToTransform :: Text -> Transform
nameToTransform "Exponential"  = PositiveT
nameToTransform "Gamma"        = PositiveT
nameToTransform "HalfNormal"   = PositiveT
nameToTransform "HalfCauchy"   = PositiveT
nameToTransform "LogNormal"    = PositiveT     -- support: x>0 (log は AD 安全)
nameToTransform "InverseGamma" = PositiveT
nameToTransform "Weibull"      = PositiveT
nameToTransform "Pareto"       = PositiveT
nameToTransform "Beta"         = UnitIntervalT
nameToTransform "Bernoulli"    = UnitIntervalT -- p ∈ (0,1)
nameToTransform "BetaBinomial" = UnitIntervalT
nameToTransform _              = UnconstrainedT -- Normal/StudentT/Cauchy/Uniform 等
-- 注: Uniform の真の制約変換は logit-on-(lo,hi) だが現状未実装 (unconstrained 扱い)。

-- | 分布 (ADT) → unconstrained 変換種別。 'nameToTransform' の値レベル版。
distToTransform :: Distribution a -> Transform
distToTransform = nameToTransform . distName

-- | Log probability of a single multinomial observation (a @K@-vector
-- of counts).
--   log P(k_1, …, k_K) = log n!/Π k_i! + Σ k_i log p_i
{-# INLINABLE multinomialLogDensity #-}
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
{-# INLINABLE mvNormalLogDensity #-}
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

-- | 'MvNormalChol' の 1 観測 (k-vector) の log density (Phase 44)。
--   scale vector @σ@ と /相関/ Cholesky 因子 @L@ から scaled Cholesky
--   @M = diag σ · L@ (= @M_ij = σ_i · L_ij@) を直接構成し、 共分散
--   @Σ = M Mᵀ@ を /再分解せず/ 評価する:
--     @log p(y) = -k/2 log(2π) - Σ log M_ii - 0.5 |z|²@、 @M z = (y-μ)@ を
--   前進代入で解く。 @log|Σ| = 2 Σ log M_ii@ なので密度の @-0.5 log|Σ|@ は
--   @-Σ log M_ii@。 'mvNormalLogDensity' (full Σ → choleskyL) と @Σ = M Mᵀ@ で
--   数値一致する。 Stan の @multi_normal_cholesky@ と同じ idiom。
{-# INLINABLE mvNormalCholLogDensity #-}
mvNormalCholLogDensity :: forall a. (Floating a, Ord a) => [a] -> [a] -> [[a]] -> [a] -> a
mvNormalCholLogDensity mu sigma l yObs
  | k == 0                                       = 0
  | length yObs /= k || length sigma /= k        = negInf
  | length l /= k || any ((/= k) . length) l     = negInf
  | otherwise =
      let m      = [ [ (sigma !! i) * ((l !! i) !! j) | j <- [0 .. k - 1] ]
                   | i <- [0 .. k - 1] ]
          kA     = fromIntegral k :: a
          d      = zipWith (-) yObs mu
          z      = forwardSub m d           -- M z = d (M 下三角)
          quad   = sum (map (\zi -> zi * zi) z)
          logDet = sum [ log ((m !! i) !! i) | i <- [0 .. k - 1] ]  -- = 0.5 log|Σ|
      in -0.5 * kA * log (2 * pi) - logDet - 0.5 * quad
  where k = length mu

-- | MvStudentT(ν, μ, Σ) の 1 観測 (k-vector) の log density (Phase 37-A4)。
--   @logpdf(y) = log Γ((ν+k)/2) − log Γ(ν/2) − (k/2) log(νπ) − (1/2) log|Σ|
--              − ((ν+k)/2) log(1 + m²/ν)@、
--   @m² = (y−μ)ᵀ Σ⁻¹ (y−μ)@ を Cholesky で評価。
{-# INLINABLE mvStudentTLogDensity #-}
mvStudentTLogDensity :: forall a. (Floating a, Ord a)
                     => a -> [a] -> [[a]] -> [a] -> a
mvStudentTLogDensity nu mu cov yObs
  | nu <= 0                   = negInf
  | length mu == 0            = 0
  | length yObs /= length mu  = negInf
  | otherwise =
      case choleskyL cov of
        Nothing -> negInf
        Just l  ->
          let k      = length mu
              kA     = fromIntegral k :: a
              d      = zipWith (-) yObs mu
              z      = forwardSub l d
              quad   = sum (map (\zi -> zi * zi) z)
              logDet = 2 * sum [ log ((l !! i) !! i) | i <- [0 .. k - 1] ]
          in lgammaApprox ((nu + kA) / 2)
           - lgammaApprox (nu / 2)
           - 0.5 * kA * log (nu * pi)
           - 0.5 * logDet
           - 0.5 * (nu + kA) * log (1 + quad / nu)

-- | DirichletMultinomial(n, α) の 1 観測 (K-vector counts) の log pmf
--   (Phase 37-A4)。
--   @logpmf = log Γ(α₀) − log Γ(α₀+n) + Σ [log Γ(yᵢ+αᵢ) − log Γ(αᵢ)]
--           + log n! − Σ log yᵢ!@、 @α₀ = Σ αᵢ@.
{-# INLINABLE dirichletMultinomialLogDensity #-}
dirichletMultinomialLogDensity :: forall a. (Floating a, Ord a)
                               => Int -> [a] -> [Double] -> a
dirichletMultinomialLogDensity n alpha counts
  | length alpha /= length counts = negInf
  | sum (map round counts :: [Int]) /= n = negInf
  | any (< 0) counts = negInf
  | any (\al -> al <= 0) alpha = negInf
  | otherwise =
      let nA       = realToFrac (fromIntegral n :: Double) :: a
          a0       = sum alpha
          logFactN = realToFrac (logFactorial n) :: a
          logFactSum = sum
            [ realToFrac (logFactorial (round c :: Int)) :: a | c <- counts ]
          term = sum
            [ lgammaApprox (realToFrac c + ai)  -- yᵢ + αᵢ
              - lgammaApprox ai
            | (c, ai) <- zip counts alpha
            ]
      in lgammaApprox a0
       - lgammaApprox (a0 + nA)
       + term
       + logFactN
       - logFactSum

-- | Wishart(ν, V) の 1 観測 (k×k 行列を flatten した長さ k² の列) の log density
--   (Phase 39-A2)。
--   @logpdf(W) = -(νk/2) log 2 - (ν/2) log|V| - log Γ_k(ν/2)
--              + ((ν-k-1)/2) log|W| - (1/2) tr(V⁻¹ W)@、
--   @log Γ_k(z) = (k(k-1)/4) log π + Σ_{i=1}^k log Γ((z+1-i)/2)@。
--   V / W の Cholesky で log determinant と tr(V⁻¹ W) を評価。
{-# INLINABLE wishartLogDensity #-}
wishartLogDensity :: forall a. (Floating a, Ord a)
                  => a -> [[a]] -> [a] -> a
wishartLogDensity nu vRows wFlat
  | nu <= fromIntegral (k - 1) = negInf
  | length wFlat /= k * k      = negInf
  | otherwise =
      case (choleskyL vRows, choleskyL wRows) of
        (Just lV, Just lW) ->
          let logDetV = 2 * sum [ log ((lV !! i) !! i) | i <- [0 .. k - 1] ]
              logDetW = 2 * sum [ log ((lW !! i) !! i) | i <- [0 .. k - 1] ]
              -- tr(V⁻¹ W) を列ごとに solve V z_j = w_j で計算
              wCols   = [ [ (wRows !! i) !! j | i <- [0 .. k - 1] ]
                        | j <- [0 .. k - 1] ]
              solveV b =
                let y = forwardSub lV b
                    x = backSubLT lV y     -- Lᵀ x = y
                in x
              traceVW = sum [ solveV (wCols !! j) !! j
                            | j <- [0 .. k - 1] ]
              kA      = fromIntegral k :: a
              -- log Γ_k(ν/2)
              logMvGam =
                (kA * (kA - 1) / 4) * log pi
                + sum [ lgammaApprox ((nu + 1 - fromIntegral i) / 2)
                      | i <- [1 .. k] ]
          in -(nu * kA / 2) * log 2
           - (nu / 2) * logDetV
           - logMvGam
           + ((nu - kA - 1) / 2) * logDetW
           - 0.5 * traceVW
        _ -> negInf
  where
    k     = length vRows
    wRows = chunksOf k wFlat

-- ---------------------------------------------------------------------------
-- 多相 CDF / log-CDF (Truncated / Censored 用)
-- ---------------------------------------------------------------------------

-- | 多相 erf 近似 (Abramowitz & Stegun 7.1.26)。誤差 < 1.5e-7。
-- AD でも Track でも動く。
{-# INLINABLE erfA #-}
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
{-# INLINABLE phiCdfA #-}
phiCdfA :: (Floating a, Ord a) => a -> a
phiCdfA x = 0.5 * (1 + erfA (x / sqrt 2))

-- | CDF @F(x) = P(Y ≤ x)@ of a 'Distribution'. Returns 'Nothing' for
-- distributions that do not have a closed-form CDF in this library.
{-# INLINABLE distCDF #-}
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
distCDF (Logistic mu s) x
  | s <= 0    = Nothing
  | otherwise = Just (1 / (1 + exp (-((x - mu) / s))))
distCDF (Gumbel mu beta) x
  | beta <= 0 = Nothing
  | otherwise = Just (exp (- exp (-((x - mu) / beta))))
distCDF (AsymmetricLaplace b kappa mu) x
  | b <= 0 || kappa <= 0 = Nothing
  | otherwise =
      let k2  = kappa * kappa
          pc  = k2 / (1 + k2)  -- F(μ)
          d   = x - mu
      in if d < 0
           then Just (pc * exp ((b / kappa) * d))
           else Just (1 - (1 - pc) * exp (- b * kappa * d))
distCDF _ _ = Nothing  -- SkewNormal / 離散・Mixture・Truncated 内 Truncated 等は未対応

-- | @log F(x)@. Computed as @log(F)@ directly to avoid loss of
-- precision near the tails where @F@ approaches 0 or 1.
{-# INLINABLE logCDF #-}
logCDF :: (Floating a, Ord a) => Distribution a -> a -> a
logCDF d x = case distCDF d x of
  Nothing -> negInf
  Just c | c <= 0    -> negInf
         | c >= 1    -> 0
         | otherwise -> log c

-- | Log of the right-tail survival function @log(1 − F(x))@.
{-# INLINABLE logSF #-}
logSF :: (Floating a, Ord a) => Distribution a -> a -> a
logSF d x = case distCDF d x of
  Nothing -> negInf
  Just c | c <= 0    -> 0
         | c >= 1    -> negInf
         | otherwise -> log (1 - c)

-- | log(F(hi) − F(lo)) — Truncated の正規化定数。
{-# INLINABLE logCDFInterval #-}
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
-- 多相 log 密度 (事前 logDensity + 観測 logDensityObs/obsLogSum)
-- ---------------------------------------------------------------------------
-- Phase 58.6: 元 HBM.hs の「事前 log 密度」節 (logDensity は 58.3 で AD 勾配と
-- 同居のため残置していたが、 logJoint/logPrior が参照するため Eval 抽出 (58.6c) で
-- back-edge になる。 密度は本来 Distribution の責務 (Phase 58 計画の module sketch)
-- ゆえここへ集約する。 INLINABLE は AD 経路の cross-module inlining 維持のため保持。

-- | Log prior density at a sample value of type @a@.
{-# INLINABLE logDensity #-}
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
logDensity MvNormalChol{} _ = 0  -- observation-only
logDensity MvNormalGpRBF{} _ = 0  -- observation-only (Phase 95 B-dsl)
logDensity HmmForwardNormal{} _ = 0  -- observation-only (Phase 92 A2)
logDensity ArmaNormal{} _ = 0  -- observation-only (Phase 101 A2)
logDensity GradedResponseIrt{} _ = 0  -- observation-only (Phase 101 A3)
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
logDensity (SkewNormal mu sig alpha) x
  | sig <= 0  = negInf
  | otherwise =
      let z      = (x - mu) / sig
          logPhi = -0.5 * log (2 * pi) - 0.5 * z * z
          -- log Φ(αz) を phiCdfA 経由で。 引数が大きく負だと数値的に困るが、
          -- phiCdfA は erfA ベースなので clip して log を取る
          cdfArg = phiCdfA (alpha * z)
          -- 数値下限 1e-300 程度に防御
          logCdf = log (max cdfArg 1e-300)
      in log 2 - log sig + logPhi + logCdf
logDensity (Logistic mu s) x
  | s <= 0    = negInf
  | otherwise =
      let z = (x - mu) / s
      in -z - log s - 2 * log (1 + exp (-z))
logDensity (Gumbel mu beta) x
  | beta <= 0 = negInf
  | otherwise =
      let z = (x - mu) / beta
      in -log beta - z - exp (-z)
logDensity (AsymmetricLaplace b kappa mu) x
  | b <= 0 || kappa <= 0 = negInf
  | otherwise =
      let logNorm = log b - log (kappa + 1 / kappa)
          d       = x - mu
      in if d >= 0
           then logNorm - b * kappa * d
           else logNorm + (b / kappa) * d
-- 離散分布は構造のみ (observation-only の意味で logDensity は使われない)
logDensity OrderedLogistic{} _      = 0
logDensity DiscreteUniform{} _      = 0
logDensity (Geometric p) _
  | p <= 0 || p >= 1 = negInf
  | otherwise        = 0
logDensity HyperGeometric{} _       = 0
logDensity (ZeroInflatedNegativeBinomial psi mu alpha) _
  | psi < 0 || psi > 1 || mu <= 0 || alpha <= 0 = negInf
  | otherwise = 0
logDensity MvStudentT{} _ = 0          -- observation-only
logDensity DirichletMultinomial{} _ = 0  -- observation-only
logDensity (Triangular lo c hi) x
  | hi <= lo || c < lo || c > hi = negInf
  | x < lo || x > hi             = negInf
  | x <= c =
      log 2 + log (x - lo)
      - log (hi - lo) - log (c - lo)
  | otherwise =
      log 2 + log (hi - x)
      - log (hi - lo) - log (hi - c)
logDensity (Kumaraswamy a b) x
  | a <= 0 || b <= 0 || x <= 0 || x >= 1 = negInf
  | otherwise =
      let xa = x ** a
      in log a + log b + (a - 1) * log x + (b - 1) * log (1 - xa)
logDensity (Rice nu sig) x
  | sig <= 0 || nu < 0 || x < 0 = negInf
  | otherwise =
      let s2 = sig * sig
          z  = x * nu / s2
      in log x - 2 * log sig - (x * x + nu * nu) / (2 * s2)
         + logBesselI0 z
logDensity DiscreteWeibull{} _ = 0   -- 離散: structure only
logDensity Wishart{} _ = 0           -- observation-only (k×k 行列観測)
logDensity (Bound d mLo mHi) x = logDensity (Truncated d mLo mHi) x
logDensity OrderedProbit{} _ = 0     -- observation-only (離散)

-- | 'logDensity' の AD ('ADRD.ReverseDouble') 特化版 (Phase 92 B3)。
-- hyperparameter が**定数** ('ADRD.Zero' / 'ADRD.Lift' = tape 由来でない) の
-- lgamma 正規化項を Double で 1 発計算して 'ADRD.Lift' で戻す。 'ADRD.Lift'
-- 同士の AD 演算は @Lift (f b c)@ (同一の Double 演算列・tape 追記なし) なので
-- 結果は generic 'logDensity' と **bit-identical**、 定数に勾配は流れないので
-- 微分も不変。 hyperparameter が tape 変数 (階層 prior) なら generic へ
-- fallback し勾配は AD がそのまま構成する。
--
-- 動機 (hmm reduced prof): Dirichlet(1,…,1) = 棒折り Beta(1,1) の定数濃度
-- lgamma が AD walk 上で毎 eval Stirling recurrence (z<12 の梯子 ~11 段 ×
-- lgamma 3 呼び出し) を boxed 'ADRD.Lift' で歩いていた (550,480 entries =
-- 70 call/eval・time 6.6%/alloc 14.4%)。 対象は lgamma を持つ定数 prior 3 種
-- (Beta / Gamma / StudentT の ν) のみ・折り畳み式の結合順は generic 実装と
-- 完全一致させてある (bit 一致の根拠)。
-- ※ 'lgammaApprox' への RULES 書き換えは過負荷関数 + 辞書引数で発火せず断念
--    (2026-07-17 実測)、 呼び出し点注入 ('logPriorWith') 方式にした。
logDensityRD
  :: forall s. Reifies s ADRD.Tape
  => Distribution (ADRD.ReverseDouble s) -> ADRD.ReverseDouble s
  -> ADRD.ReverseDouble s
logDensityRD d x = case d of
  Beta a b
    | Just a' <- constRD a, Just b' <- constRD b
    , not (x <= 0 || x >= 1 || a' <= 0 || b' <= 0) ->
        (a - 1) * log x + (b - 1) * log (1 - x)
          - ADRD.Lift (lgammaApprox a' + lgammaApprox b' - lgammaApprox (a' + b'))
  Gamma sh ra
    | Just sh' <- constRD sh, Just ra' <- constRD ra
    , not (x <= 0 || sh' <= 0 || ra' <= 0) ->
        (sh - 1) * log x - ra * x
          + ADRD.Lift (sh' * log ra') - ADRD.Lift (lgammaApprox sh')
  StudentT df mu sig
    | Just df' <- constRD df
    , not (df' <= 0 || sig <= 0) ->
        let z = (x - mu) / sig
        in ADRD.Lift (lgammaApprox ((df' + 1) / 2) - lgammaApprox (df' / 2)
                        - 0.5 * log (df' * pi))
           - log sig
           - ((df + 1) / 2) * log (1 + z * z / df)
  _ -> logDensity d x
  where
    -- Zero/Lift = tape に乗らない定数 (Lift 同士の演算は Lift に閉じる)
    constRD :: ADRD.ReverseDouble s -> Maybe Double
    constRD ADRD.Zero             = Just 0
    constRD (ADRD.Lift v)         = Just v
    constRD ADRD.ReverseDouble{}  = Nothing

-- | Log likelihood density at an observation (a fixed @Double@).
-- Observations are passed as @[Double]@, so this uses only the
-- @Floating a@ constraint.
-- Phase 58.6c: ObserveLM 評価 (lmObsLogLiks) と logJoint が AD で微分しながら呼ぶ
-- ホット経路。 58.6a で本体から移したため cross-module になった。 INLINABLE で
-- 境界跨ぎ inline を維持 (M1/M2 の +25% 劣化を解消・58.6 bench 実測)。
{-# INLINABLE logDensityObs #-}
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
logDensityObs MvNormalChol{} _ = 0
logDensityObs MvNormalGpRBF{} _ = 0  -- Phase 95 B-dsl: obsLogSum 経由 (下と同じ)
logDensityObs HmmForwardNormal{} _ = 0  -- Phase 92 A2: obsLogSum 経由 (下と同じ)
logDensityObs ArmaNormal{} _ = 0  -- Phase 101 A2: obsLogSum 経由 (下と同じ)
logDensityObs GradedResponseIrt{} _ = 0  -- Phase 101 A3: obsLogSum 経由 (下と同じ)
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
logDensityObs (SkewNormal mu sig alpha) y
  | sig <= 0 = negInf
  | otherwise =
      let yA     = realToFrac y :: a
          z      = (yA - mu) / sig
          logPhi = -0.5 * log (2 * pi) - 0.5 * z * z
          cdfArg = phiCdfA (alpha * z)
          logCdf = log (max cdfArg 1e-300)
      in log 2 - log sig + logPhi + logCdf
logDensityObs (Logistic mu s) y
  | s <= 0 = negInf
  | otherwise =
      let yA = realToFrac y :: a
          z  = (yA - mu) / s
      in -z - log s - 2 * log (1 + exp (-z))
logDensityObs (Gumbel mu beta) y
  | beta <= 0 = negInf
  | otherwise =
      let yA = realToFrac y :: a
          z  = (yA - mu) / beta
      in -log beta - z - exp (-z)
logDensityObs (AsymmetricLaplace b kappa mu) y
  | b <= 0 || kappa <= 0 = negInf
  | otherwise =
      let yA      = realToFrac y :: a
          logNorm = log b - log (kappa + 1 / kappa)
          d       = yA - mu
      in if d >= 0
           then logNorm - b * kappa * d
           else logNorm + (b / kappa) * d
logDensityObs (OrderedLogistic eta cuts) y
  | null cuts                 = negInf
  | k < 0 || k > kMax         = negInf
  | otherwise =
      -- σ(c_{k+1} − η) − σ(c_k − η)、 c_0 = −∞、 c_K = +∞
      let sigm x  = 1 / (1 + exp (-x))
          kMax_a  = kMax  -- 上限カテゴリ index
          probHi
            | k == kMax_a = 1
            | otherwise   = sigm (cuts !! k - eta)
          probLo
            | k == 0    = 0
            | otherwise = sigm (cuts !! (k - 1) - eta)
          pK = probHi - probLo
      in if pK <= 0 then negInf else log pK
  where
    k    = round y :: Int
    kMax = length cuts
logDensityObs (DiscreteUniform lo hi) y
  | hi < lo                = negInf
  | yI < lo || yI > hi     = negInf
  | otherwise              = -log (realToFrac (hi - lo + 1) :: a)
  where
    yI = round y :: Int
logDensityObs (Geometric p) y
  | p <= 0 || p >= 1 = negInf
  | yI < 1           = negInf
  | otherwise =
      let kA = realToFrac y :: a
      in (kA - 1) * log (1 - p) + log p
  where
    yI = round y :: Int
logDensityObs (HyperGeometric nN kK nDraw) y
  | nN <= 0 || kK < 0 || kK > nN || nDraw < 0 || nDraw > nN = negInf
  | yI < max 0 (nDraw + kK - nN) || yI > min nDraw kK       = negInf
  | otherwise =
      let lc = realToFrac (logBinomCoeff kK yI
                         + logBinomCoeff (nN - kK) (nDraw - yI)
                         - logBinomCoeff nN nDraw) :: a
      in lc
  where
    yI = round y :: Int
logDensityObs (ZeroInflatedNegativeBinomial psi mu alpha) y
  | psi < 0 || psi > 1 || mu <= 0 || alpha <= 0 || y < 0 = negInf
  | y == 0 =
      -- log(ψ + (1-ψ) (α/(α+μ))^α)
      let p0NB = alpha * (log alpha - log (alpha + mu))
      in logSumExpA [log psi, log (1 - psi) + p0NB]
  | otherwise =
      let kA = realToFrac y :: a
          p  = alpha / (alpha + mu)
          logNB = lgammaApprox (kA + alpha)
                - lgammaApprox alpha
                - lgammaApprox (kA + 1)
                + alpha * log p
                + kA * log (1 - p)
      in log (1 - psi) + logNB
logDensityObs MvStudentT{} _ = 0
  -- スカラー観測経路では使わない (k chunk で mvStudentTLogDensity 経由)
logDensityObs DirichletMultinomial{} _ = 0
  -- スカラー観測経路では使わない (K chunk で dirichletMultinomialLogDensity 経由)
logDensityObs (Triangular lo c hi) y
  | hi <= lo || c < lo || c > hi = negInf
  | otherwise =
      let yA = realToFrac y :: a
      in if yA < lo || yA > hi
           then negInf
           else if yA <= c
             then log 2 + log (yA - lo)
                  - log (hi - lo) - log (c - lo)
             else log 2 + log (hi - yA)
                  - log (hi - lo) - log (hi - c)
logDensityObs (Kumaraswamy a b) y
  | a <= 0 || b <= 0 = negInf
  | otherwise =
      let yA = realToFrac y :: a
      in if yA <= 0 || yA >= 1
           then negInf
           else let xa = yA ** a
                in log a + log b + (a - 1) * log yA + (b - 1) * log (1 - xa)
logDensityObs (Rice nu sig) y
  | sig <= 0 || nu < 0 || y < 0 = negInf
  | otherwise =
      let yA = realToFrac y :: a
          s2 = sig * sig
          z  = yA * nu / s2
      in log yA - 2 * log sig - (yA * yA + nu * nu) / (2 * s2)
         + logBesselI0 z
logDensityObs Wishart{} _ = 0
  -- スカラー観測経路では使わない (k² chunk で wishartLogDensity 経由)
logDensityObs (Bound d mLo mHi) y = logDensityObs (Truncated d mLo mHi) y
logDensityObs (OrderedProbit eta cuts) y
  | null cuts                 = negInf
  | k < 0 || k > kMax         = negInf
  | otherwise =
      let probHi
            | k == kMax = 1
            | otherwise = phiCdfA (cuts !! k - eta)
          probLo
            | k == 0    = 0
            | otherwise = phiCdfA (cuts !! (k - 1) - eta)
          pK = probHi - probLo
      in if pK <= 0 then negInf else log pK
  where
    k    = round y :: Int
    kMax = length cuts
logDensityObs (DiscreteWeibull q beta) y
  | y < 0 = negInf
  | otherwise =
      -- q は (0,1)、 β > 0
      -- pmf(k) = q^(k^β) - q^((k+1)^β)
      let qVal :: a
          qVal = q
          bVal :: a
          bVal = beta
      in if qVal <= 0 || qVal >= 1 || bVal <= 0
           then negInf
           else
             let kI    = round y :: Int
                 kA    = realToFrac (fromIntegral kI :: Double) :: a
                 logQ  = log qVal
                 -- log(q^(k^β) - q^((k+1)^β))
                 --   = log q^(k^β) + log(1 - q^((k+1)^β - k^β))
                 -- 安定化: a1 = (k+1)^β - k^β > 0 (β>0)
                 pk    = kA ** bVal
                 pk1   = (kA + 1) ** bVal
                 diffP = pk1 - pk
                 -- log(1 - q^diffP) = log(1 - exp(diffP * logQ))
                 -- diffP * logQ <= 0
                 expArg = diffP * logQ
                 log1mE = log (1 - exp expArg)
             in pk * logQ + log1mE

-- | Sum of log likelihoods over a list of observations. For ordinary
-- distributions one observation contributes one scalar log-density.
-- For 'MvNormal' (which expects @k@-vectors), the flattened @[Double]@
-- is chunked into length-@k@ groups before evaluation.
-- Phase 58.6c: logJoint/logLikelihood の Observe 分岐が AD で呼ぶ。 cross-module
-- inline 維持のため INLINABLE。
{-# INLINABLE obsLogSum #-}
obsLogSum :: forall a. (Floating a, Ord a) => Distribution a -> [Double] -> a
obsLogSum (MvNormal mu cov) ys =
  let k       = length mu
      chunks  = chunksOf k ys
  in sum [ mvNormalLogDensity mu cov (map realToFrac yv :: [a])
         | yv <- chunks ]
obsLogSum (MvNormalGpRBF xs alpha rho sigma) ys =
  -- Phase 95 B-dsl: zero-mean・cov = RBF カーネル + (1e-10 + σ)·I。 値は汎用
  -- 'MvNormal' 経路と同値 (ホット勾配のみ 'gpRBFAnalyticVG' で閉形式化)。
  let k       = length xs
      cov     = gpRBFCovList xs alpha rho sigma
      mu      = replicate k 0
      chunks  = chunksOf k ys
  in sum [ mvNormalLogDensity mu cov (map realToFrac yv :: [a])
         | yv <- chunks ]
obsLogSum (GradedResponseIrt thetas ncats deltas gammas) ys =
  -- Phase 101 A3: grade 行列 (nChild×nItem 行優先・欠測 −1) 全体を 1 観測として
  -- 評価。 値は従来の @logCatProb + potential@ 書きと同値
  -- (ホット勾配のみ 'gradedIrtAnalyticVG' で閉形式化)。
  let nItem = length ncats
      rows  = chunksOf nItem ys
      logCatP th nc dl gm gr =
        let kMax = nc - 1
            qs = [ 1 / (1 + exp (negate (realToFrac dl * (th - realToFrac (gm !! (kk - 1))))))
                 | kk <- [1 .. kMax] ]
            ps = [ if k == 1 then 1 - head qs
                   else if k == nc then qs !! (kMax - 1)
                   else (qs !! (k - 2)) - (qs !! (k - 1))
                 | k <- [1 .. nc] ]
        in log (ps !! (gr - 1))
  in sum [ logCatP th nc dl gm (round gr)
         | (th, row) <- zip thetas rows
         , (nc, dl, gm, gr) <- zip4 ncats deltas gammas row
         , gr /= -1 ]
obsLogSum (ArmaNormal mu phi theta sg) ys =
  -- Phase 101 A2: 観測列全体 (長さ T) を 1 観測として err 逐次再帰で評価。
  -- 値は従来の @mapAccumL + potential@ 書きと同値
  -- (ホット勾配のみ 'armaAnalyticVG' で閉形式化)。
  case ys of
    [] -> 0
    (y1 : rest) ->
      let e1 = realToFrac y1 - (mu + phi * mu)
          step (prevY, prevErr) yt =
            let err = realToFrac yt - (mu + phi * realToFrac prevY + theta * prevErr)
            in ((yt, err), err)
          errs = e1 : snd (mapAccumL step (y1, e1) rest)
      in sum [ logDensity (Normal 0 sg) e | e <- errs ]
obsLogSum (HmmForwardNormal pi0 trans mus sg) ys =
  -- Phase 92 A2: 観測列全体 (長さ T) を 1 観測として forward algorithm で周辺化。
  -- 値は従来の @potential nm (hmmForwardLogLik pi0 trans emit)@ 書きと同値
  -- (ホット勾配のみ 'hmmAnalyticVG' で閉形式化)。
  let emit = [ [ logDensity (Normal mu sg) (realToFrac y) | mu <- mus ] | y <- ys ]
  in hmmForwardLogLik pi0 trans emit
obsLogSum (Multinomial n probs) ys =
  let k      = length probs
      chunks = chunksOf k ys
  in sum [ multinomialLogDensity n probs yv | yv <- chunks ]
obsLogSum (MvNormalChol mu sigma l) ys =
  let k      = length mu
      chunks = chunksOf k ys
  in sum [ mvNormalCholLogDensity mu sigma l (map realToFrac yv :: [a])
         | yv <- chunks ]
obsLogSum (MvStudentT nu mu cov) ys =
  let k      = length mu
      chunks = chunksOf k ys
  in sum [ mvStudentTLogDensity nu mu cov (map realToFrac yv :: [a])
         | yv <- chunks ]
obsLogSum (DirichletMultinomial n alpha) ys =
  let k      = length alpha
      chunks = chunksOf k ys
  in sum [ dirichletMultinomialLogDensity n alpha yv | yv <- chunks ]
obsLogSum (Wishart nu vRows) ys =
  let k       = length vRows
      chunks  = chunksOf (k * k) ys
  in sum [ wishartLogDensity nu vRows (map realToFrac yv :: [a])
         | yv <- chunks ]
obsLogSum d ys = sum [ logDensityObs d y | y <- ys ]
