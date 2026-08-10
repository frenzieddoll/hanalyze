{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Hanalyze.Model.AFT
-- Description : Accelerated Failure Time (AFT) パラメトリック生存モデル
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Accelerated Failure Time (AFT) parametric survival model.
--
-- AFT は寿命 T の対数を共変量の線形関数として表現する:
--
-- @
-- log T_i = X_i β + σ ε_i
-- @
--
-- ε の分布で family が決まる:
--
--   * 'AFTWeibull'    : ε ~ Gumbel  (生存解析の Weibull AFT)
--   * 'AFTLogNormal'  : ε ~ Normal(0, 1)
--   * 'AFTLogLogistic': ε ~ Logistic(0, 1)
--   * 'AFTExponential': Weibull with σ = 1 を固定
--
-- 右側打ち切り (right censoring) 対応。 推定は対数尤度の最大化を
-- Nelder-Mead で行う (純粋関数のため runIdentity 経由)。
--
-- API:
--
-- > fitAFT     :: AFTDistribution -> Matrix Double -> Vector Double
-- >            -> Vector Bool -> IO (Either Text AFTFit)
-- > predictAFT :: AFTFit -> Matrix Double -> Vector Double  -- 期待寿命
module Hanalyze.Model.AFT
  ( AFTDistribution (..)
  , AFTFit (..)
  , fitAFT
  , predictAFT
  , logS          -- ^ 標準化誤差 z の log 生存関数 (= 生存曲線描画に使用・Phase 68 A5)
  ) where

import qualified Data.Vector                       as V
import qualified Numeric.LinearAlgebra             as LA
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import qualified Statistics.Distribution           as SD
import qualified Statistics.Distribution.Normal    as ND

import           Hanalyze.Optim.NelderMead         (runNelderMeadWith, defaultNMConfig,
                                                    NMConfig (..))
import           Hanalyze.Optim.Common             (OptimResult (..), StopCriteria (..))

-- ===========================================================================
-- 型
-- ===========================================================================

data AFTDistribution
  = AFTWeibull
  | AFTLogNormal
  | AFTLogLogistic
  | AFTExponential
  deriving (Show, Eq)

data AFTFit = AFTFit
  { aftBeta         :: !(LA.Vector Double)
  , aftScale        :: !Double            -- ^ scale parameter σ
  , aftLogLik       :: !Double
  , aftDistribution :: !AFTDistribution
  , aftIters        :: !Int
  } deriving (Show)

-- ===========================================================================
-- fit
-- ===========================================================================

-- | AFT モデルを MLE で fit する。
--   X: n × p 共変量、 t: n 観測時間 (> 0)、 delta: n failure indicator
--   (True = 観測、 False = 右側打ち切り)。
fitAFT
  :: AFTDistribution
  -> LA.Matrix Double
  -> LA.Vector Double
  -> V.Vector Bool
  -> IO (Either Text AFTFit)
fitAFT dist x t delta
  | LA.rows x /= LA.size t || LA.rows x /= V.length delta =
      pure (Left "fitAFT: input dimensions mismatch")
  | LA.size t == 0 =
      pure (Left "fitAFT: empty input")
  | V.any (<= 0) (V.fromList (LA.toList t)) =
      pure (Left "fitAFT: t must be > 0")
  | otherwise = do
      let p   = LA.cols x
          -- intercept-only start: β_0 = mean(log t), β_j = 0 (j ≥ 1)
          logT = LA.cmap log t
          beta0 =
            let mu = LA.sumElements logT / fromIntegral (LA.size logT)
            in if p == 0
                 then []
                 else mu : replicate (p - 1) 0
          -- log σ を最後に追加 (Exponential では 0 固定)
          x0 = case dist of
                 AFTExponential -> beta0
                 _              -> beta0 ++ [0]   -- log σ = 0  → σ = 1 として開始
          obj params =
            let (betaPart, logSigma) = case dist of
                  AFTExponential -> (params, 0)
                  _              -> (init params, last params)
                sigma = exp logSigma
                betaV = LA.fromList betaPart
            in negate (logLikAFT dist x t delta betaV sigma)
          cfg = defaultNMConfig
            { nmStop = StopCriteria
                { stMaxIter = 2000
                , stTolFun  = 1e-8
                , stTolX    = 1e-8
                }
            }
      res <- runNelderMeadWith cfg obj x0
      let xs = orBest res
          (betaPart, sigma) = case dist of
            AFTExponential -> (xs, 1)
            _              -> (init xs, exp (last xs))
          betaV = LA.fromList betaPart
          ll = logLikAFT dist x t delta betaV sigma
      pure (Right AFTFit
              { aftBeta         = betaV
              , aftScale        = sigma
              , aftLogLik       = ll
              , aftDistribution = dist
              , aftIters        = orIters res
              })

-- | 期待寿命の予測 E[T | X] = exp(X β + σ² / 2) -- log-normal の場合
--   Weibull AFT: E[T] = exp(X β) · Γ(1 + σ)
--   LogLogistic: E[T] = exp(X β) · π σ / sin(π σ) (σ < 1)
--   Exponential: E[T] = exp(X β)
predictAFT :: AFTFit -> LA.Matrix Double -> LA.Vector Double
predictAFT fit xNew =
  let linPred = xNew LA.#> aftBeta fit
      sigma   = aftScale fit
      adjust  = case aftDistribution fit of
        AFTWeibull     -> gammaApprox (1 + sigma)
        AFTLogNormal   -> exp (sigma * sigma / 2)
        AFTLogLogistic ->
          if sigma < 1 && sigma > 0
            then pi * sigma / sin (pi * sigma)
            else 1 / 0   -- 平均が発散
        AFTExponential -> 1
  in LA.cmap (\lp -> exp lp * adjust) linPred

-- ===========================================================================
-- 内部 helpers
-- ===========================================================================

-- | 対数尤度。 censored は log S(t)、 observed は log f(t)。
logLikAFT
  :: AFTDistribution
  -> LA.Matrix Double -> LA.Vector Double -> V.Vector Bool
  -> LA.Vector Double -> Double
  -> Double
logLikAFT dist x t delta beta sigma
  | sigma <= 0 = -1e15
  | otherwise =
      let n = LA.rows x
          eta = x LA.#> beta             -- length n
          logT = LA.cmap log t           -- length n
          zs = LA.cmap (/ sigma) (logT - eta)
      in sum
           [ let z   = LA.atIndex zs i
                 lt  = LA.atIndex logT i
                 obs = delta V.! i
             in if obs
                  then logPDF dist sigma lt z
                  else logS  dist z
           | i <- [0 .. n - 1] ]

-- | log f(t)  =  log f_ε(z) − log σ − log t
logPDF :: AFTDistribution -> Double -> Double -> Double -> Double
logPDF dist sigma logT z =
  let body = case dist of
        AFTWeibull     -> z - exp z
        AFTExponential -> z - exp z
        AFTLogNormal   -> -0.5 * z * z - 0.5 * log (2 * pi)
        AFTLogLogistic -> z - 2 * log1p (exp z)
  in body - log (max 1e-300 sigma) - logT

-- | log S(t)  =  log S_ε(z)
logS :: AFTDistribution -> Double -> Double
logS dist z = case dist of
  AFTWeibull     -> -exp z
  AFTExponential -> -exp z
  AFTLogNormal   -> log (max 1e-300 (1 - SD.cumulative ND.standard z))
  AFTLogLogistic -> -log1p (exp z)

log1p :: Double -> Double
log1p x
  | abs x < 1e-4 = x - x * x / 2 + x * x * x / 3
  | otherwise    = log (1 + x)

-- | Stirling 近似による Γ(x) (x > 0)。 AFT の平均補正で使うだけなので簡易版。
gammaApprox :: Double -> Double
gammaApprox x
  | x <= 0 = 1 / 0
  | x < 1  = gammaApprox (x + 1) / x
  | otherwise =
      let n = floor (x - 1) :: Int
          frac = x - fromIntegral n - 1
          base = gammaStirling (1 + frac)
      in base * fromIntegral (product [1 .. n])
  where
    gammaStirling y =
      sqrt (2 * pi / y) * (y / exp 1) ** y
      * (1 + 1/(12*y) + 1/(288*y*y))
