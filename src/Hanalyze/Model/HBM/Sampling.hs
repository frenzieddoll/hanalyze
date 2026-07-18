{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Hanalyze.Model.HBM.Sampling
-- Description : HBM の分布サンプリング (事前/事後予測用)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Phase 58.4: 分布からのサンプリング (事前/事後予測用) を分離。
--
-- 'Distribution' / 'HBM.Util' の上層。 PrimMonad + mwc-random に依存し、
-- mwc-random が直接提供しない分布 (Cauchy, HalfCauchy, Weibull, …) は
-- 逆 CDF 法 / rejection でここに実装する。 NUTS の per-draw 経路には乗らず
-- (事前/事後予測のみ)、 性能ホットではない。
module Hanalyze.Model.HBM.Sampling
  ( sampleDist
  , sampleMvDist
  , sampleObsRep
  ) where

import Control.Monad (replicateM)
import Data.List (zip4)
import Control.Monad.Primitive (PrimMonad, PrimState)
import qualified System.Random.MWC as MWCBase
import qualified System.Random.MWC.Distributions as MWC
import System.Random.MWC (Gen)

import Hanalyze.Model.HBM.Util (choleskyL, chunksOf, gpRBFCovList)
import Hanalyze.Model.HBM.Distribution (Distribution (..), phiCdfA)

-- ---------------------------------------------------------------------------
-- 分布からのサンプリング (事前/事後予測用)
-- ---------------------------------------------------------------------------

-- | Draw a single sample from a 'Distribution Double'.
-- 事前予測サンプリング、事後予測サンプリング、観測値の生成に使う。
--
-- mwc-random が直接提供しない分布はここで実装する (Cauchy, HalfCauchy, etc.)。
sampleDist :: forall m. PrimMonad m => Distribution Double -> Gen (PrimState m) -> m Double
sampleDist (Normal mu sig) gen = MWC.normal mu sig gen
sampleDist (Exponential rate) gen = do
  u <- MWCBase.uniform gen :: m Double
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
        u <- MWCBase.uniform gen :: m Double
        go (k - 1) (if u < p then acc + 1 else acc)
  fmap fromIntegral (go n (0 :: Int))
sampleDist (Uniform lo hi) gen = do
  u <- MWCBase.uniform gen :: m Double
  return (lo + u * (hi - lo))
sampleDist (StudentT df mu sig) gen = do
  -- t = mu + sig * Normal(0,1) / sqrt(Chi2(df) / df)
  z    <- MWC.standard gen
  chi2 <- MWC.gamma (df / 2) 2 gen   -- Chi2(df) = Gamma(df/2, scale=2)
  return (mu + sig * z / sqrt (chi2 / df))
sampleDist (Cauchy loc sc) gen = do
  u <- MWCBase.uniform gen :: m Double
  return (loc + sc * tan (pi * (u - 0.5)))
sampleDist (HalfNormal sig) gen = do
  z <- MWC.standard gen
  return (abs (sig * z))
sampleDist (HalfCauchy sc) gen = do
  u <- MWCBase.uniform gen :: m Double
  return (sc * abs (tan (pi * (u - 0.5))))
sampleDist (LogNormal mu sig) gen = do
  z <- MWC.standard gen
  return (exp (mu + sig * z))
sampleDist (Bernoulli p) gen = do
  u <- MWCBase.uniform gen :: m Double
  return (if u < p then 1.0 else 0.0)
sampleDist (Categorical probs) gen = do
  u <- MWCBase.uniform gen :: m Double
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
      u <- MWCBase.uniform gen :: m Double
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
sampleDist MvNormalChol{} _ =
  error "MvNormalChol: observation-only — 'sample' 経由でのドローは未対応"
sampleDist MvNormalGpRBF{} _ =
  error "MvNormalGpRBF: observation-only — 'sample' 経由でのドローは未対応"
sampleDist HmmForwardNormal{} _ =
  error "HmmForwardNormal: observation-only — 'sample' 経由でのドローは未対応"
sampleDist ArmaNormal{} _ =
  error "ArmaNormal: observation-only — 'sample' 経由でのドローは未対応"
sampleDist GradedResponseIrt{} _ =
  error "GradedResponseIrt: observation-only — 'sample' 経由でのドローは未対応"
sampleDist Multinomial{} _ =
  error "Multinomial: observation-only — 'sample' 経由でのドローは未対応"
sampleDist (InverseGamma alpha beta) gen = do
  -- 1 / Gamma(α, rate=β) = 1 / Gamma(α, scale=1/β)
  y <- MWC.gamma alpha (1 / beta) gen
  return (1 / y)
sampleDist (Weibull kShape lam) gen = do
  -- 逆 CDF 法: x = λ (-log(1-u))^(1/k)
  u <- MWCBase.uniform gen :: m Double
  return (lam * ((-log (1 - u)) ** (1 / kShape)))
sampleDist (Pareto alpha xm) gen = do
  -- 逆 CDF 法: x = x_m / u^(1/α)
  u <- MWCBase.uniform gen :: m Double
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
        u1 <- MWCBase.uniform gen :: m Double
        let z = cos (pi * u1)
            f = (1 + r * z) / (r + z)
            c = kappa * (r - f)
        u2 <- MWCBase.uniform gen :: m Double
        if c * (2 - c) - u2 > 0 || log (c / u2) + 1 - c >= 0
          then do
            u3 <- MWCBase.uniform gen :: m Double
            let sign = if u3 - 0.5 < 0 then (-1.0) else 1.0
            return (mu + sign * acos f)
          else tryOnce
  tryOnce
sampleDist (ZeroInflatedPoisson psi lam) gen = do
  u <- MWCBase.uniform gen :: m Double
  if u < psi
    then return 0
    else samplePoissonKnuth lam gen
sampleDist (ZeroInflatedBinomial n psi p) gen = do
  u <- MWCBase.uniform gen :: m Double
  if u < psi
    then return 0
    else sampleDist (Binomial n p) gen
sampleDist (SkewNormal mu sig alpha) gen = do
  -- Henze 1986: δ = α/√(1+α²), X = μ + σ(δ|U₀| + √(1-δ²)U₁)
  let delta = alpha / sqrt (1 + alpha * alpha)
  u0 <- MWC.standard gen
  u1 <- MWC.standard gen
  return (mu + sig * (delta * abs u0 + sqrt (1 - delta * delta) * u1))
sampleDist (Logistic mu s) gen = do
  -- 逆 CDF: X = μ + s · log(u/(1-u))
  u <- MWCBase.uniform gen :: m Double
  return (mu + s * log (u / (1 - u)))
sampleDist (Gumbel mu beta) gen = do
  -- 逆 CDF: X = μ - β · log(-log u)
  u <- MWCBase.uniform gen :: m Double
  return (mu - beta * log (- log u))
sampleDist (AsymmetricLaplace b kappa mu) gen = do
  -- 逆 CDF。 pc = κ²/(1+κ²)、 U < pc なら左尾、 そうでなければ右尾
  u <- MWCBase.uniform gen :: m Double
  let k2 = kappa * kappa
      pc = k2 / (1 + k2)
  if u < pc
    then return (mu + (kappa / b) * log (u / pc))
    else return (mu - (1 / (b * kappa)) * log ((1 - u) / (1 - pc)))
sampleDist (OrderedLogistic eta cuts) gen = do
  -- η と cuts から各カテゴリの確率を計算して Categorical で sample
  let sigm x = 1 / (1 + exp (-x))
      probs  = catProbs cuts
      catProbs []           = [1]
      catProbs (c:rest)     = sigm (c - eta) : restProbs (sigm (c - eta)) rest
      restProbs _    []         = [1 - sigm (last cuts - eta)]
      restProbs prev (c:rest)   =
        let cur = sigm (c - eta)
        in (cur - prev) : restProbs cur rest
  u <- MWCBase.uniform gen :: m Double
  let go acc k []     = realToFrac (k - 1 :: Int)
      go acc k (p:ps) =
        let acc' = acc + p
        in if u < acc' then realToFrac k else go acc' (k + 1) ps
  return (go 0 (0 :: Int) probs)
sampleDist (DiscreteUniform lo hi) gen = do
  u <- MWCBase.uniform gen :: m Double
  let span_ = hi - lo + 1
      k     = lo + floor (u * realToFrac span_)
      kClip = min hi k  -- u が 1 のとき span_ になるのを防ぐ
  return (realToFrac kClip)
sampleDist (Geometric p) gen = do
  -- 逆 CDF: X = ceil(log U / log(1-p))、 PyMC convention で support から 1
  u <- MWCBase.uniform gen :: m Double
  let lq = log (1 - p)
      x  = ceiling (log u / lq) :: Int
  return (realToFrac (max 1 x))
sampleDist (HyperGeometric nN kK nDraw) gen = do
  -- 単純な urn sampling: 各引きで残り成功/失敗の割合から二項
  let loop remN remK remDraw acc
        | remDraw <= 0 = return (realToFrac (acc :: Int))
        | otherwise = do
            u <- MWCBase.uniform gen :: m Double
            let pSucc = realToFrac remK / realToFrac remN :: Double
                pick  = if u < pSucc then 1 else 0
            loop (remN - 1) (remK - pick) (remDraw - 1) (acc + pick)
  loop nN kK nDraw 0
sampleDist (ZeroInflatedNegativeBinomial psi mu alpha) gen = do
  u <- MWCBase.uniform gen :: m Double
  if u < psi
    then return 0
    else sampleDist (NegativeBinomial mu alpha) gen
sampleDist MvStudentT{} _ =
  error "MvStudentT: observation-only — 'sample' 経由でのドローは未対応 (latent helper を別途用意予定)"
sampleDist DirichletMultinomial{} _ =
  error "DirichletMultinomial: observation-only — 'sample' 経由でのドローは未対応"
sampleDist (NegativeBinomial mu alpha) gen = do
  -- Gamma-Poisson mixture: λ ~ Gamma(α, β=α/μ); X ~ Poisson(λ)
  lam <- MWC.gamma alpha (mu / alpha) gen
  samplePoissonKnuth lam gen
sampleDist (Censored d _ _) gen =
  -- 元分布から普通にサンプリング (打ち切りは「観測過程」の話で生成側ではない)
  sampleDist d gen
sampleDist (Triangular lo c hi) gen = do
  -- 逆 CDF。 Fc = (c-lo)/(hi-lo) 未満なら左側、 以上なら右側
  u <- MWCBase.uniform gen :: m Double
  let fc = (c - lo) / (hi - lo)
  if u < fc
    then return (lo + sqrt (u * (hi - lo) * (c - lo)))
    else return (hi - sqrt ((1 - u) * (hi - lo) * (hi - c)))
sampleDist (Kumaraswamy a b) gen = do
  -- 逆 CDF: x = (1 - (1-u)^{1/b})^{1/a}
  u <- MWCBase.uniform gen :: m Double
  return ((1 - (1 - u) ** (1 / b)) ** (1 / a))
sampleDist (Rice nu sig) gen = do
  -- Y1 ~ N(ν, σ), Y2 ~ N(0, σ); X = sqrt(Y1²+Y2²)
  y1 <- MWC.normal nu sig gen
  y2 <- MWC.normal 0  sig gen
  return (sqrt (y1 * y1 + y2 * y2))
sampleDist Wishart{} _ =
  error "Wishart: observation-only — 'sample' 経由でのドローは未対応 (Bartlett decomp の latent helper を別途用意予定)"
sampleDist (Bound d mLo mHi) gen =
  -- Bound は Truncated とほぼ同じ。 sample は rejection。
  sampleDist (Truncated d mLo mHi) gen
sampleDist (OrderedProbit eta cuts) gen = do
  -- η と cuts から各カテゴリの確率を計算して Categorical で sample
  let probs  = catProbs cuts
      catProbs []        = [1]
      catProbs (c:rest)  = phiCdfA (c - eta) : restProbs (phiCdfA (c - eta)) rest
      restProbs _    []         = [1 - phiCdfA (last cuts - eta)]
      restProbs prev (c:rest)   =
        let cur = phiCdfA (c - eta)
        in (cur - prev) : restProbs cur rest
  u <- MWCBase.uniform gen :: m Double
  let go acc k []     = realToFrac (k - 1 :: Int)
      go acc k (p:ps) =
        let acc' = acc + p
        in if u < acc' then realToFrac k else go acc' (k + 1) ps
  return (go 0 (0 :: Int) probs)
sampleDist (DiscreteWeibull q beta) gen = do
  -- 逆 CDF: k = ceil((log(1-u)/log q)^{1/β}) - 1
  u <- MWCBase.uniform gen :: m Double
  let r  = log (1 - u) / log q
      k0 = ceiling (r ** (1 / beta)) - 1 :: Int
      k  = max 0 k0
  return (fromIntegral k)

-- | 多変量分布から 1 観測 (k-vector) を draw する (Phase 44、 PPC 用)。
-- 'sampleDist' (スカラ観測専用 'error') と別経路。 @y = μ + C z@ で z ~ N(0,1)、
-- C は MvNormal なら @choleskyL Σ@、 MvNormalChol なら scaled Cholesky
-- @M = diag σ · L@ (再分解不要)。 対応外の多変量分布は空リスト (= worker 側で
-- graceful にスキップ。 本 Phase は MvNormal/MvNormalChol に集中)。
sampleMvDist :: forall m. PrimMonad m => Distribution Double -> Gen (PrimState m) -> m [Double]
sampleMvDist (MvNormal mu cov) gen = do
  let k = length mu
  zs <- replicateM k (MWC.standard gen)
  pure $ case choleskyL cov of
    Just c  -> [ (mu !! i) + sum [ ((c !! i) !! j) * (zs !! j) | j <- [0 .. i] ]
               | i <- [0 .. k - 1] ]
    Nothing -> mu
sampleMvDist (MvNormalChol mu sigma l) gen = do
  let k = length mu
      m = [ [ (sigma !! i) * ((l !! i) !! j) | j <- [0 .. k - 1] ] | i <- [0 .. k - 1] ]
  zs <- replicateM k (MWC.standard gen)
  pure [ (mu !! i) + sum [ ((m !! i) !! j) * (zs !! j) | j <- [0 .. i] ]
       | i <- [0 .. k - 1] ]
sampleMvDist (MvNormalGpRBF xs alpha rho sigma) gen =    -- Phase 95 B-dsl: cov 展開して MvNormal と同じ
  sampleMvDist (MvNormal (replicate (length xs) 0) (gpRBFCovList xs alpha rho sigma)) gen
sampleMvDist _ _ = pure []

-- | 1 observe ノード分の複製データ (y_rep) をまとめて draw する (Phase 90 A2)。
-- PPC ('sampleYRep'/'epredPIAtHeld' 等) はこれまで観測分布を問わず ys の要素
-- ごとに 'sampleDist' (スカラ専用) を呼んでいたため、 多変量分布 (MvNormal/
-- MvNormalChol、 ys = 1 つの k-vector 観測がフラット化された形) では即座に
-- @error@ していた (07-gp-regr で実測発覚)。 ここで次元 k ごとに ys をチャンク
-- し 'sampleMvDist' に委譲する。 Multinomial は 'sampleMvDist' が未対応の
-- ままなので (本 Phase の対象外)、 従来どおり 'sampleDist' に委ねる。
sampleObsRep :: forall m. PrimMonad m
             => Gen (PrimState m) -> Distribution Double -> [Double] -> m [Double]
sampleObsRep gen d@(MvNormal mu _) ys =
  concat <$> mapM (const (sampleMvDist d gen)) (chunksOf (length mu) ys)
sampleObsRep gen d@(MvNormalChol mu _ _) ys =
  concat <$> mapM (const (sampleMvDist d gen)) (chunksOf (length mu) ys)
sampleObsRep gen d@(MvNormalGpRBF xs _ _ _) ys =   -- Phase 95 B-dsl
  concat <$> mapM (const (sampleMvDist d gen)) (chunksOf (length xs) ys)
sampleObsRep gen (HmmForwardNormal pi0 trans mus sg) ys = do
  -- Phase 92 A2 (PPC): 状態列を π_0/遷移行列から draw → Normal(μ_s, σ) で emission。
  -- 観測列全体 = 1 観測なので T = length ys の系列を 1 本生成する。
  let kk = length pi0
      pick ws = do                       -- 重み ws (非正規化可) からカテゴリを 1 つ draw
        let s = sum ws
        u <- (* s) <$> MWCBase.uniform gen
        let go i acc (w:rest) | null rest || u <= acc + w = pure i
                              | otherwise                 = go (i + 1) (acc + w) rest
            go i _ []                                     = pure (max 0 (i - 1))
        go 0 0 ws
      stepState s = pick (if s < length trans then trans !! s else replicate kk 1)
      emitAt s = do
        z <- MWC.standard gen
        pure ((if s < length mus then mus !! s else 0) + sg * z)
      go' _ 0 acc = pure (reverse acc)
      go' s n acc = do
        y <- emitAt s
        s' <- stepState s
        go' s' (n - 1 :: Int) (y : acc)
  s0 <- pick pi0
  go' s0 (length ys) []
sampleObsRep gen (ArmaNormal mu phi theta sg) ys = do
  -- Phase 101 A2 (PPC): err_t ~ Normal(0, σ) を draw し、y を前向き再帰で生成
  -- (y_1 = μ+φμ+e_1・y_t = μ + φ·y_{t−1} + θ·e_{t−1} + e_t)。
  -- 観測列全体 = 1 観測なので T = length ys の系列を 1 本生成する。
  let drawE = (sg *) <$> MWC.standard gen
      go' _ _ 0 acc = pure (reverse acc)
      go' prevY prevE n acc = do
        e <- drawE
        let y = mu + phi * prevY + theta * prevE + e
        go' y e (n - 1 :: Int) (y : acc)
  case length ys of
    0 -> pure []
    t -> do
      e1 <- drawE
      let y1 = mu + phi * mu + e1
      go' y1 e1 (t - 1) [y1]
sampleObsRep gen (GradedResponseIrt thetas ncats deltas gammas) ys = do
  -- Phase 101 A3 (PPC): 各 (child, item) の p ベクトルからカテゴリを draw。
  -- 欠測 (−1) 位置は −1 のまま返す (観測の欠測パターンを保存)。
  let nItem = length ncats
      rows  = chunksOf nItem ys
      catPs th nc dl gm =
        let kMax = nc - 1
            qs = [ 1 / (1 + exp (negate (dl * (th - gm !! (kk - 1)))))
                 | kk <- [1 .. kMax] ]
        in [ if k == 1 then 1 - head qs
             else if k == nc then qs !! (kMax - 1)
             else (qs !! (k - 2)) - (qs !! (k - 1))
           | k <- [1 .. nc] ]
      pickCat ps = do
        u <- MWCBase.uniform gen
        let go k acc (w:rest) | null rest || u <= acc + w = pure k
                              | otherwise                 = go (k + 1) (acc + w) rest
            go k _ []                                     = pure (max 1 (k - 1))
        go 1 0 ps
      drawRow th row =
        sequence [ if gr == -1 then pure (-1)
                   else fromIntegral <$> pickCat (catPs th nc dl gm)
                 | (nc, dl, gm, gr) <- zip4 ncats deltas gammas row ]
  concat <$> sequence [ drawRow th row | (th, row) <- zip thetas rows ]
sampleObsRep gen d ys = mapM (const (sampleDist d gen)) ys

-- | Knuth のアルゴリズムで Poisson(λ) サンプル。λ < 30 程度なら十分高速。
samplePoissonKnuth :: forall m. PrimMonad m => Double -> Gen (PrimState m) -> m Double
samplePoissonKnuth lam gen = do
  let l = exp (-lam)
      go k p = do
        u <- MWCBase.uniform gen :: m Double
        let p' = p * u
        if p' < l
          then return (fromIntegral k)
          else go (k + 1) p'
  go 0 (1.0 :: Double)
