{-# LANGUAGE OverloadedStrings #-}
module Stat.Distribution
  ( Distribution (..)
  , density
  , logDensity
  , isContinuous
  , supportRange
  , distributionName
  , parseDistribution
    -- * 制約変換 (HMC/NUTS の unconstrained サンプリング用)
  , Transform (..)
  , distTransform
  , toUnconstrained
  , fromUnconstrained
  , logJacobianAdj
  ) where

import Data.Text (Text)
import qualified Data.Text as T

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

data Distribution
  = Normal     Double Double   -- μ σ
  | Binomial   Int    Double   -- n p
  | Poisson    Double          -- λ
  | Exponential Double         -- λ (rate)
  | Gamma      Double Double   -- α (shape) β (rate)
  | Beta       Double Double   -- α β
  deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Density / PMF
-- ---------------------------------------------------------------------------

density :: Distribution -> Double -> Double
density (Normal mu sig) x
  | sig <= 0  = 0
  | otherwise = exp (negate ((x - mu)^(2::Int) / (2 * sig^(2::Int))))
              / (sig * sqrt (2 * pi))

density (Binomial n p) x
  | p < 0 || p > 1      = 0
  | x < 0 || x > fromIntegral n = 0
  | otherwise =
      let k = round x :: Int
      in fromIntegral (choose n k) * p ^ k * (1 - p) ^ (n - k)

density (Poisson lam) x
  | lam <= 0  = 0
  | x < 0     = 0
  | otherwise =
      let k = round x :: Int
      in exp (negate lam) * lam ^ k / fromIntegral (factorial k)

density (Exponential lam) x
  | lam <= 0 = 0
  | x < 0    = 0
  | otherwise = lam * exp (negate lam * x)

density (Gamma alpha beta_) x
  | alpha <= 0 || beta_ <= 0 = 0
  | x <= 0                    = 0
  | otherwise =
      beta_ ** alpha * x ** (alpha - 1) * exp (negate beta_ * x)
      / gammaFn alpha

density (Beta alpha beta_) x
  | alpha <= 0 || beta_ <= 0 = 0
  | x <= 0 || x >= 1         = 0
  | otherwise =
      x ** (alpha - 1) * (1 - x) ** (beta_ - 1)
      / betaFn alpha beta_

logDensity :: Distribution -> Double -> Double
logDensity d x =
  let p = density d x
  in if p <= 0 then -1/0 else log p

-- ---------------------------------------------------------------------------
-- Properties
-- ---------------------------------------------------------------------------

isContinuous :: Distribution -> Bool
isContinuous (Binomial  _ _) = False
isContinuous (Poisson   _  ) = False
isContinuous _               = True

-- | Suggested x-axis range for plotting.
-- Continuous: mean ± k*sd; discrete: [0, mean + k*sd].
supportRange :: Distribution -> (Double, Double)
supportRange (Normal mu sig)      = (mu - 4*sig,     mu + 4*sig)
supportRange (Binomial n _)       = (0, fromIntegral n)
supportRange (Poisson lam)        = (0, max 20 (lam + 4 * sqrt lam))
supportRange (Exponential lam)    = (0, 6 / lam)
supportRange (Gamma alpha beta_)  = let m = alpha / beta_
                                        s = sqrt (alpha / (beta_*beta_))
                                    in (0, m + 4*s)
supportRange (Beta _ _)           = (0, 1)

distributionName :: Distribution -> Text
distributionName (Normal     mu sig ) = "Normal(" <> fmt mu <> ", " <> fmt sig <> ")"
distributionName (Binomial   n  p   ) = "Binomial(" <> T.pack (show n) <> ", " <> fmt p <> ")"
distributionName (Poisson    lam    ) = "Poisson(" <> fmt lam <> ")"
distributionName (Exponential lam   ) = "Exponential(" <> fmt lam <> ")"
distributionName (Gamma  a b        ) = "Gamma(" <> fmt a <> ", " <> fmt b <> ")"
distributionName (Beta   a b        ) = "Beta(" <> fmt a <> ", " <> fmt b <> ")"

fmt :: Double -> Text
fmt v = T.pack (show (fromIntegral (round (v * 100) :: Int) / 100.0 :: Double))

-- | Parse "normal", "binomial", "poisson", "exponential", "gamma", "beta".
parseDistribution :: String -> [Double] -> Either String Distribution
parseDistribution name params = case map toLowerAscii name of
  "normal"      -> case params of
    [mu, sig] | sig > 0  -> Right (Normal mu sig)
    [_, sig]             -> Left ("Normal: σ must be > 0, got " ++ show sig)
    _                    -> Left "Normal requires params: mean sd"
  "binomial"    -> case params of
    [n, p] | p >= 0, p <= 1, n >= 1 ->
      Right (Binomial (round n) p)
    _ -> Left "Binomial requires params: n p  (n≥1, 0≤p≤1)"
  "poisson"     -> case params of
    [lam] | lam > 0 -> Right (Poisson lam)
    _               -> Left "Poisson requires params: lambda (>0)"
  "exponential" -> case params of
    [lam] | lam > 0 -> Right (Exponential lam)
    _               -> Left "Exponential requires params: rate (>0)"
  "gamma"       -> case params of
    [a, b] | a > 0, b > 0 -> Right (Gamma a b)
    _                      -> Left "Gamma requires params: shape rate (both >0)"
  "beta"        -> case params of
    [a, b] | a > 0, b > 0 -> Right (Beta a b)
    _                      -> Left "Beta requires params: alpha beta (both >0)"
  other -> Left ("Unknown distribution: " ++ other
              ++ ". Available: normal, binomial, poisson, exponential, gamma, beta")

-- ---------------------------------------------------------------------------
-- 制約変換
-- ---------------------------------------------------------------------------

-- | パラメータの定義域に対応する変換の種類。
-- HMC/NUTS は unconstrained 空間 (ℝ) でリープフロッグを行い、
-- サンプルを constrained 空間に戻すことで支持域外への逸脱を防ぐ。
data Transform
  = UnconstrainedT   -- ^ (-∞, ∞): 恒等変換 (Normal の平均など)
  | PositiveT        -- ^ (0, ∞): 対数変換  θ = exp(u)
  | UnitIntervalT    -- ^ (0, 1): ロジット変換 θ = sigmoid(u)
  deriving (Show, Eq)

-- | 事前分布の型から変換を自動判定する。
distTransform :: Distribution -> Transform
distTransform (Normal _ _)    = UnconstrainedT
distTransform (Exponential _) = PositiveT
distTransform (Gamma _ _)     = PositiveT
distTransform (Beta _ _)      = UnitIntervalT
distTransform (Binomial _ _)  = UnconstrainedT  -- 離散; HMC/NUTS 非推奨
distTransform (Poisson _)     = UnconstrainedT  -- 離散; HMC/NUTS 非推奨

-- | θ_constrained → u_unconstrained
toUnconstrained :: Transform -> Double -> Double
toUnconstrained UnconstrainedT x = x
toUnconstrained PositiveT      x = log x
toUnconstrained UnitIntervalT  x = log x - log (1 - x)  -- logit

-- | u_unconstrained → θ_constrained
fromUnconstrained :: Transform -> Double -> Double
fromUnconstrained UnconstrainedT u = u
fromUnconstrained PositiveT      u = exp u
fromUnconstrained UnitIntervalT  u = 1 / (1 + exp (-u))  -- sigmoid

-- | log |dθ/du|: logJoint に加算する Jacobian 補正項。
-- PositiveT:     θ = exp(u)          → log|J| = u
-- UnitIntervalT: θ = sigmoid(u)      → log|J| = log σ(u) + log(1-σ(u))
logJacobianAdj :: Transform -> Double -> Double
logJacobianAdj UnconstrainedT _ = 0
logJacobianAdj PositiveT      u = u
logJacobianAdj UnitIntervalT  u =
  let s = 1 / (1 + exp (-u))
  in log s + log (1 - s)

toLowerAscii :: Char -> Char
toLowerAscii c
  | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
  | otherwise             = c

-- ---------------------------------------------------------------------------
-- Math helpers
-- ---------------------------------------------------------------------------

factorial :: Int -> Int
factorial n = product [1 .. n]

choose :: Int -> Int -> Int
choose n k
  | k < 0 || k > n = 0
  | k == 0 || k == n = 1
  | otherwise = choose (n-1) (k-1) + choose (n-1) k

-- Lanczos approximation for Γ(z), z > 0
gammaFn :: Double -> Double
gammaFn z
  | z < 0.5   = pi / (sin (pi * z) * gammaFn (1 - z))
  | otherwise =
      let z'  = z - 1
          x   = lanczosC !! 0
              + sum [ lanczosC !! i / (z' + fromIntegral i)
                    | i <- [1 .. length lanczosC - 1] ]
          t   = z' + fromIntegral (length lanczosC) - 0.5
      in sqrt (2*pi) * t ** (z' + 0.5) * exp (negate t) * x

lanczosC :: [Double]
lanczosC =
  [ 0.99999999999980993
  , 676.5203681218851
  , -1259.1392167224028
  , 771.32342877765313
  , -176.61502916214059
  , 12.507343278686905
  , -0.13857109526572012
  , 9.9843695780195716e-6
  , 1.5056327351493116e-7
  ]

betaFn :: Double -> Double -> Double
betaFn a b = gammaFn a * gammaFn b / gammaFn (a + b)
