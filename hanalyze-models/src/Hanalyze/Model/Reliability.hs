{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Hanalyze.Model.Reliability
-- Description : 信頼性解析 — 加速寿命試験モデル群 (Arrhenius / Eyring / Inverse Power Law)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: 信頼性解析: 加速寿命試験のモデル群。
--
-- ストレス変数 (温度 / 電圧 / 湿度等) と寿命の関係を回帰し、 使用条件下での
-- 寿命予測や加速係数を計算する。
--
-- 提供するモデル:
--
--   - 'fitArrhenius' — 温度ストレス: @t = A · exp(Ea / (k_B · T))@
--   - 'fitEyring' — 温度 + 1 ストレス: 半導体 EM 等
--   - 'fitInversePower' — 電圧 / 機械応力: @t = A · S^(-n)@
--
-- いずれも対数寿命を線形モデルとして fit する (古典的アプローチ)。
-- 寿命分布の指定が必要な場合は 'Hanalyze.Model.Weibull' の MLE 結果を
-- 入力として渡すバリアント (本モジュールの提供外、 別フェーズで検討)。
--
-- [English]: Reliability analysis: a family of accelerated life-testing
-- models.
--
-- Regresses the relationship between stress variables (temperature \/
-- voltage \/ humidity, etc.) and lifetime, computing lifetime predictions
-- under use conditions and acceleration factors.
--
-- Models provided:
--
--   - 'fitArrhenius' — temperature stress: @t = A · exp(Ea / (k_B · T))@
--   - 'fitEyring' — temperature + 1 stress: semiconductor EM etc.
--   - 'fitInversePower' — voltage \/ mechanical stress: @t = A · S^(-n)@
--
-- All of these fit log-lifetime as a linear model (the classical approach).
-- A variant that takes MLE results from 'Hanalyze.Model.Weibull' as
-- input, for cases where the lifetime distribution must be specified, is
-- out of scope for this module (to be considered in a separate phase).
module Hanalyze.Model.Reliability
  ( -- * Arrhenius
    ArrheniusFit (..)
  , fitArrhenius
  , accelerationFactor
    -- * Eyring
  , EyringFit (..)
  , fitEyring
    -- * Inverse Power Law
  , InversePowerFit (..)
  , fitInversePower
    -- * 共通定数
  , kBoltzmann
  ) where

import qualified Numeric.LinearAlgebra as LA
import           Data.Text (Text)

-- ===========================================================================
-- 共通定数
-- ===========================================================================

-- | [日本語]: Boltzmann 定数 (eV/K)。 Arrhenius / Eyring で温度ストレスに使う。
--   [English]: The Boltzmann constant (eV\/K). Used for temperature stress
--   in Arrhenius \/ Eyring.
kBoltzmann :: Double
kBoltzmann = 8.617333262145e-5

-- ===========================================================================
-- Arrhenius モデル
-- ===========================================================================

-- | [日本語]: Arrhenius fit: @t = A · exp(Ea / (k_B · T))@
--   [English]: Arrhenius fit: @t = A · exp(Ea / (k_B · T))@.
data ArrheniusFit = ArrheniusFit
  { afA      :: !Double  -- ^ [日本語]: 前指数因子 A。 [English]: The pre-exponential factor A.
  , afEa     :: !Double  -- ^ [日本語]: 活性化エネルギー Ea (eV)。 [English]: The activation energy Ea (eV).
  , afLogLik :: !Double  -- ^ [日本語]: 対数尤度 (Gaussian residual 仮定)。 [English]: The log-likelihood (assuming Gaussian residuals).
  , afN      :: !Int     -- ^ [日本語]: 観測 (温度 × 寿命) 数。 [English]: The number of observations (temperature × lifetime).
  } deriving (Show)

-- | [日本語]: Arrhenius モデルの fit。
--
-- 入力: @[(temperature_K, [lifetimes])]@ の対、 温度ごとに複数寿命を観測。
-- 解法: log t = log A + Ea/k_B · (1/T) を OLS で解く (= 線形回帰)。
-- 戻り値: @A@ と @Ea (eV)@ の点推定、 log-likelihood (Gaussian residual 仮定)。
--
-- 失敗条件:
--
--   - 入力が空または全観測 0 個 → Left
--   - 温度水準が 1 種類しかない (= 傾き決定不能) → Left
--   - 任意の温度 ≤ 0 や寿命 ≤ 0 → Left (log 取得不能)
--
--   [English]: Fits the Arrhenius model.
--
-- Input: pairs of @[(temperature_K, [lifetimes])]@, with multiple lifetimes
-- observed per temperature.
-- Method: solves log t = log A + Ea/k_B · (1/T) via OLS (= linear
-- regression).
-- Returns: point estimates of @A@ and @Ea (eV)@, plus the log-likelihood
-- (assuming Gaussian residuals).
--
-- Failure conditions:
--
--   - Input is empty or has zero observations overall → Left
--   - Only one distinct temperature level (= slope cannot be determined) →
--     Left
--   - Any temperature ≤ 0 or lifetime ≤ 0 → Left (log cannot be taken)
fitArrhenius :: [(Double, [Double])] -> Either Text ArrheniusFit
fitArrhenius input = do
  () <- if null input then Left "fitArrhenius: empty input" else Right ()
  let allPairs =
        [ (t, life)
        | (t, lives) <- input
        , life <- lives
        ]
  () <- if null allPairs
          then Left "fitArrhenius: no lifetime observations across all temperatures"
          else Right ()
  () <- if any (\(t, l) -> t <= 0 || l <= 0) allPairs
          then Left "fitArrhenius: temperatures and lifetimes must all be > 0"
          else Right ()
  let distinctTemps = length (nubByDouble (map fst allPairs))
  () <- if distinctTemps < 2
          then Left "fitArrhenius: need at least 2 distinct temperatures"
          else Right ()
  -- (x, y) = (1/T, log t)
  let xs = map (\(t, _) -> 1 / t) allPairs
      ys = map (\(_, l) -> log l) allPairs
      n  = length allPairs
      meanX = sum xs / fromIntegral n
      meanY = sum ys / fromIntegral n
      sxx = sum [ (x - meanX) ** 2 | x <- xs ]
      sxy = sum [ (x - meanX) * (y - meanY) | (x, y) <- zip xs ys ]
  () <- if sxx <= 0
          then Left "fitArrhenius: zero variance in 1/T (numerical issue)"
          else Right ()
  let b1     = sxy / sxx                     -- slope = Ea / k_B
      b0     = meanY - b1 * meanX            -- intercept = log A
      a      = exp b0
      ea     = b1 * kBoltzmann
      yHat   = [ b0 + b1 * x | x <- xs ]
      sse    = sum [ (y - yh) ** 2 | (y, yh) <- zip ys yHat ]
      sigma2 = if n > 2 then sse / fromIntegral (n - 2) else sse / fromIntegral n
      ll     = -0.5 * fromIntegral n * (log (2 * pi * sigma2) + 1)
  Right ArrheniusFit
    { afA      = a
    , afEa     = ea
    , afLogLik = ll
    , afN      = n
    }

-- | [日本語]: 重複除去 (浮動小数点許容なし、 完全一致のみ)。
--   [English]: Deduplication (no floating-point tolerance, exact match
--   only).
nubByDouble :: [Double] -> [Double]
nubByDouble = go []
  where
    go acc []     = reverse acc
    go acc (x:xs) | x `elem` acc = go acc xs
                  | otherwise    = go (x : acc) xs

-- | [日本語]: 加速係数 AF = exp(Ea/k_B · (1/T_use - 1/T_test))
--   [English]: The acceleration factor AF = exp(Ea/k_B · (1/T_use -
--   1/T_test)).
accelerationFactor :: ArrheniusFit -> Double -> Double -> Double
accelerationFactor fit tUse tTest =
  exp (afEa fit / kBoltzmann * (1/tUse - 1/tTest))

-- ===========================================================================
-- Eyring モデル (Phase 2.6)
-- ===========================================================================

-- | [日本語]: Eyring fit: @t = A · T^(-1) · exp(Ea / (k_B · T)) · exp(B · S)@
-- (温度 T と 1 ストレス変数 S)
--   [English]: Eyring fit: @t = A · T^(-1) · exp(Ea / (k_B · T)) ·
--   exp(B · S)@ (temperature T and 1 stress variable S).
data EyringFit = EyringFit
  { efA      :: !Double
  , efEa     :: !Double
  , efB      :: !Double  -- ストレス係数
  , efLogLik :: !Double
  , efN      :: !Int
  } deriving (Show)

-- | [日本語]: Eyring モデルの fit。
--
-- モデル: @t · T = A · exp(Ea / (k_B · T)) · exp(B · S)@
-- 等価に: @log t = log A − log T + Ea/(k_B · T) + B · S@
--
-- 入力: @[(temperature_K, stress, [lifetimes])]@。 各 (T, S) 組合せで複数寿命可。
-- 解法: y = log t + log T を (1/T, S) の 2 変量 OLS で fit (intercept 含む)。
--   β0 = log A、 β1 = Ea / k_B、 β2 = B
--
--   [English]: Fits the Eyring model.
--
-- Model: @t · T = A · exp(Ea / (k_B · T)) · exp(B · S)@
-- Equivalently: @log t = log A − log T + Ea/(k_B · T) + B · S@
--
-- Input: @[(temperature_K, stress, [lifetimes])]@. Multiple lifetimes are
-- allowed per (T, S) combination.
-- Method: fits y = log t + log T via bivariate OLS on (1/T, S) (including
-- an intercept).
--   β0 = log A, β1 = Ea / k_B, β2 = B
fitEyring :: [(Double, Double, [Double])] -> Either Text EyringFit
fitEyring input = do
  () <- if null input then Left "fitEyring: empty input" else Right ()
  let pairs =
        [ (t, s, life)
        | (t, s, lives) <- input
        , life <- lives
        ]
  () <- if null pairs
          then Left "fitEyring: no lifetime observations"
          else Right ()
  () <- if any (\(t, _, l) -> t <= 0 || l <= 0) pairs
          then Left "fitEyring: temperatures and lifetimes must be > 0"
          else Right ()
  let distinctTS = nubByPair [ (t, s) | (t, s, _) <- pairs ]
  () <- if length distinctTS < 3
          then Left "fitEyring: need at least 3 distinct (T, S) combinations"
          else Right ()
  let xRows = [ [1, 1 / t, s] | (t, s, _) <- pairs ]
      ys    = [ log l + log t | (t, _, l) <- pairs ]
      xMat  = LA.fromLists xRows :: LA.Matrix Double
      yVec  = LA.fromList ys     :: LA.Vector Double
      -- normal equations: β = (XᵀX)⁻¹ Xᵀy
      xt    = LA.tr xMat
      xtx   = xt LA.<> xMat
      xty   = xt LA.#> yVec
  betaList <- case LA.linearSolve xtx (LA.asColumn xty) of
    Just m  -> Right (LA.toList (LA.flatten m))
    Nothing -> Left "fitEyring: design matrix is singular (collinear T/S?)"
  case betaList of
    [b0, b1, b2] -> do
      let n      = length pairs
          a      = exp b0
          ea     = b1 * kBoltzmann
          bCoef  = b2
          yHat   = LA.toList (xMat LA.#> LA.fromList [b0, b1, b2])
          sse    = sum [ (y - yh) ** 2 | (y, yh) <- zip ys yHat ]
          dof    = max 1 (n - 3)
          sigma2 = sse / fromIntegral dof
          ll     = -0.5 * fromIntegral n * (log (2 * pi * sigma2) + 1)
      Right EyringFit
        { efA      = a
        , efEa     = ea
        , efB      = bCoef
        , efLogLik = ll
        , efN      = n
        }
    _ -> Left "fitEyring: linearSolve returned unexpected length"

-- | [日本語]: (T, S) ペアの重複除去。
--   [English]: Deduplicates (T, S) pairs.
nubByPair :: [(Double, Double)] -> [(Double, Double)]
nubByPair = go []
  where
    go acc []     = reverse acc
    go acc (p:ps) | p `elem` acc = go acc ps
                  | otherwise    = go (p : acc) ps

-- ===========================================================================
-- Inverse Power Law モデル (Phase 2.6)
-- ===========================================================================

-- | [日本語]: Inverse Power Law fit: @t = A · S^(-n)@
--   [English]: Inverse Power Law fit: @t = A · S^(-n)@.
data InversePowerFit = InversePowerFit
  { ipfA      :: !Double
  , ipfN      :: !Double  -- パワー指数
  , ipfLogLik :: !Double
  , ipfNobs   :: !Int
  } deriving (Show)

-- | [日本語]: Inverse Power Law モデルの fit。
--
-- モデル: @t = A · S^(-n)@
-- log 変換: @log t = log A − n · log S@
--
-- 入力: @[(stress, [lifetimes])]@。 stress > 0、 lifetime > 0 必須。
-- 解法: y = log t を log S の単変量 OLS で fit。 傾き = -n。
--
--   [English]: Fits the Inverse Power Law model.
--
-- Model: @t = A · S^(-n)@
-- Log transform: @log t = log A − n · log S@
--
-- Input: @[(stress, [lifetimes])]@. Requires stress > 0 and lifetime > 0.
-- Method: fits y = log t via univariate OLS on log S. Slope = -n.
fitInversePower :: [(Double, [Double])] -> Either Text InversePowerFit
fitInversePower input = do
  () <- if null input then Left "fitInversePower: empty input" else Right ()
  let pairs =
        [ (s, life)
        | (s, lives) <- input
        , life <- lives
        ]
  () <- if null pairs
          then Left "fitInversePower: no lifetime observations"
          else Right ()
  () <- if any (\(s, l) -> s <= 0 || l <= 0) pairs
          then Left "fitInversePower: stress and lifetimes must be > 0"
          else Right ()
  let distinctS = length (nubByDouble (map fst pairs))
  () <- if distinctS < 2
          then Left "fitInversePower: need at least 2 distinct stress levels"
          else Right ()
  let xs = map (\(s, _) -> log s) pairs
      ys = map (\(_, l) -> log l) pairs
      n  = length pairs
      meanX = sum xs / fromIntegral n
      meanY = sum ys / fromIntegral n
      sxx = sum [ (x - meanX) ** 2 | x <- xs ]
      sxy = sum [ (x - meanX) * (y - meanY) | (x, y) <- zip xs ys ]
  () <- if sxx <= 0
          then Left "fitInversePower: zero variance in log S"
          else Right ()
  let slope  = sxy / sxx           -- = -n
      b0     = meanY - slope * meanX
      a      = exp b0
      nExp   = - slope
      yHat   = [ b0 + slope * x | x <- xs ]
      sse    = sum [ (y - yh) ** 2 | (y, yh) <- zip ys yHat ]
      sigma2 = if n > 2 then sse / fromIntegral (n - 2) else sse / fromIntegral n
      ll     = -0.5 * fromIntegral n * (log (2 * pi * sigma2) + 1)
  Right InversePowerFit
    { ipfA      = a
    , ipfN      = nExp
    , ipfLogLik = ll
    , ipfNobs   = n
    }
