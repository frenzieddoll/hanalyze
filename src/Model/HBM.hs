{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | 多相版 HBM DSL。
--
-- 現行 'Model.HBM' は @Sample Text Distribution (Double -> next)@ という形で
-- 継続の値型が @Double@ に固定されている。これは AD ('Numeric.AD') にも
-- 依存追跡型 ('Track') にも対応できないという根本的な制約をもたらす。
--
-- 'Model.HBM' では継続を多相化:
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
-- import Model.HBM
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
module Model.HBM
  ( -- * 多相分布
    Distribution (..)
  , distName
  , logDensity
  , logDensityObs
  , sampleDist
    -- * 多相モデル
  , Model
  , ModelP
  , sample
  , observe
  , potential
    -- * 構造検査
  , Node (..)
  , NodeKind (..)
  , collectNodes
  , sampleNames
  , extractDeps
    -- * 型エイリアス
  , Params
    -- * インタープリタ
  , logJoint
  , logPrior
  , logLikelihood
  , perObsLogLiks
  , runObserveDists
  , priorList
  , describeModel
    -- * モデルグラフ (可視化用)
  , ModelGraph (..)
  , buildModelGraph
    -- * AD 勾配
  , gradAD
  , gradADU
    -- * 制約変換 (HMC 用)
  , getTransforms
  , logJointUnconstrained
  , invTransformF
  , logJacF
    -- * 依存追跡型
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

import Stat.Distribution (Transform (..))

-- ---------------------------------------------------------------------------
-- Free monad (再実装。Model.HBM のものとは型が違うので別途定義)
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

-- | 値の型 @a@ に多相な確率分布。
-- @a@ には @Double@、@Reverse s Double@ (AD)、@Track@ (依存追跡) などが入る。
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
    -- ^ Mixture(weights, components) — log p(x) = logsumexp(log w_k + log p_k(x))
    -- 重みは正の値で渡し、内部で自動正規化される。
  deriving (Show, Functor)

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

-- | サンプル値 (型 @a@) に対する事前分布の対数密度。
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

-- | 観測値 (Double 定数) に対する尤度の対数密度。
-- 観測は @[Double]@ で渡されるので、@Floating a@ 制約のみで計算可能。
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
-- 分布からのサンプリング (事前/事後予測用)
-- ---------------------------------------------------------------------------

-- | 'Distribution Double' から 1 サンプル生成する。
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
-- 多相モデル (Free monad)
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
  deriving Functor

type Model a = Free (ModelF a)

-- | 多相モデルの型エイリアス。
-- @ModelP r = forall a. (Floating a, Ord a) => Model a r@
type ModelP r = forall a. (Floating a, Ord a) => Model a r

sample :: Text -> Distribution a -> Model a a
sample n d = liftF (Sample n d id)

observe :: Text -> Distribution a -> [Double] -> Model a ()
observe n d ys = liftF (Observe n d ys ())

-- | 任意の log-prob 項をモデルに加える (PyMC `pm.Potential` 相当)。
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

-- | placeholder 値 0 でモデルを走査し、ノード情報を集める。
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

sampleNames :: ModelP r -> [Text]
sampleNames m = [nodeName n | n <- collectNodes m, nodeKind n == LatentN]

-- ---------------------------------------------------------------------------
-- 評価インタープリタ
-- ---------------------------------------------------------------------------

-- | log p(θ, y) を計算する多相インタープリタ。
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
      let ll = sum [ logDensityObs d y | y <- ys ]
      in go next (acc + ll)
    go (Free (Potential _ v next)) acc = go next (acc + v)

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
      let ll = sum [ logDensityObs d y | y <- ys ]
      in go next (acc + ll)
    go (Free (Potential _ _ next)) acc = go next acc   -- Potential は事前項とみなす

-- | 各 Observe ノードの「現在のパラメータ値で評価した分布」と観測値を取得する。
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

-- | 各 Sample ノードの (名前, 事前分布) を Double 特殊化で取得する。
-- Gibbs サンプラーの共役検出で「この潜在変数の事前は Gamma か Beta か」を
-- 判定するために使う。継続値はプレースホルダ 0 を流す。
priorList :: Model Double r -> [(Text, Distribution Double)]
priorList (Pure _) = []
priorList (Free (Sample n d k)) = (n, d) : priorList (k 0)
priorList (Free (Observe _ _ _ next)) = priorList next
priorList (Free (Potential _ _ next)) = priorList next

-- ---------------------------------------------------------------------------
-- 互換 API
-- ---------------------------------------------------------------------------

-- | パラメータ名 → 値 のマップ (constrained 空間)。
type Params = Map Text Double

-- | 観測値ごとの対数尤度 (WAIC / LOO 用)。
-- 各 Observe ノードのすべての観測値の logDensity を平坦リストで返す。
perObsLogLiks :: forall r. ModelP r -> Params -> [Double]
perObsLogLiks m params = go m []
  where
    go :: Model Double r -> [Double] -> [Double]
    go (Pure _) acc = reverse acc
    go (Free (Sample n _ k)) acc =
      go (k (Map.findWithDefault 0 n params)) acc
    go (Free (Observe _ d ys next)) acc =
      let lls = [ logDensityObs d y | y <- ys ]
      in go next (reverse lls ++ acc)
    go (Free (Potential _ _ next)) acc = go next acc

-- | モデル構造の人間向け要約 (推論は実行しない)。
describeModel :: ModelP r -> Text
describeModel m = T.unlines (header : map fmtNode (collectNodes m))
  where
    header = "Model nodes:"
    fmtNode n = case nodeKind n of
      LatentN     -> "  [latent]   " <> nodeName n <> " ~ " <> nodeDist n
      ObservedN k -> "  [observed] " <> nodeName n <> " ~ " <> nodeDist n
                  <> "  (n=" <> T.pack (show k) <> ")"

-- | DAG 用のモデルグラフ。エッジは 'extractDeps' で自動抽出される。
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
