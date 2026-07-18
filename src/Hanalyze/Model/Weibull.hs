{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : Hanalyze.Model.Weibull
-- Description : Weibull 分布の最尤推定・B_x 寿命・Wald 標準誤差 (信頼性/故障時間解析の中核)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Weibull 分布の最尤推定 + B_x 寿命 + Wald SE。
--
-- 信頼性 / 故障時間解析の中核。 半導体 / 材料分野の加速試験データ解析に使う。
-- 加速モデル (Arrhenius / Eyring / Inverse Power Law) は
-- @Hanalyze.Model.Reliability@ で別途扱う。
--
-- Weibull(k, λ) の確率密度 / 生存関数:
--
-- > f(x) = (k/λ) (x/λ)^(k-1) exp(-(x/λ)^k)        for x > 0
-- > S(x) = exp(-(x/λ)^k)
--
-- 形状 k と尺度 λ は両方とも正。 k < 1 は故障率低下 (初期不良)、 k = 1 は
-- 指数分布、 k > 1 は故障率上昇 (摩耗故障)。
module Hanalyze.Model.Weibull
  ( -- * 結果型
    WeibullFit (..)
    -- * MLE fit
  , fitWeibullMLE
  , fitWeibullCensored
    -- * 派生量
  , bxLife
  , bxLifeCI
  , weibullParameterSE
  , weibullParameterCovariance
    -- * 数値ユーティリティ
  , quantileNormal
  ) where

import           Data.Text     (Text)
import           Data.Vector   (Vector)
import qualified Data.Vector   as V

-- ===========================================================================
-- 型定義
-- ===========================================================================

-- | Weibull MLE 結果。
data WeibullFit = WeibullFit
  { wfShape   :: !Double           -- ^ k (形状パラメータ、 > 0)
  , wfScale   :: !Double           -- ^ λ (尺度パラメータ、 > 0)
  , wfLogLik  :: !Double           -- ^ 対数尤度の MLE 値
  , wfN       :: !Int              -- ^ 観測総数 (打ち切り含む)
  , wfRObs    :: !Int              -- ^ 観測 failure 数 (打ち切り除く)
  , wfFisher  :: !(Double, Double, Double)
    -- ^ Fisher 情報行列 2x2 を上三角 (I_kk, I_kλ, I_λλ) で保持。
    --   Wald SE 計算で逆行列を取る。
  } deriving (Show)

-- ===========================================================================
-- 内部ヘルパ
-- ===========================================================================

-- | 観測値リストの sanity check (全て正で非空)。
validatePositive :: Vector Double -> Either Text ()
validatePositive xs
  | V.null xs           = Left "fitWeibull: empty observation series"
  | V.any (<= 0) xs     = Left "fitWeibull: all observations must be positive"
  | otherwise           = Right ()

-- | A(k) = Σ x_i^k log x_i (failures のみ加算する版は censored 用)。
weightedLog :: Double -> Vector Double -> Double
weightedLog k xs = V.sum (V.map (\x -> x ** k * log x) xs)

-- | B(k) = Σ x_i^k。 censored 含む場合は加算範囲を呼び出し側で制御する。
sumPow :: Double -> Vector Double -> Double
sumPow k xs = V.sum (V.map (** k) xs)

-- | g(k) = A(k)/B(k) − (1/r)·Σ_{failures} log x − 1/k = 0
--   r = failure 数。 単調増加なので bisection で root を取れる。
scoreG :: Double -> Vector Double -> Vector Double -> Int -> Double
scoreG k allXs failuresXs r =
  let bk = sumPow k allXs
      ak = weightedLog k allXs
      meanLogFail = V.sum (V.map log failuresXs) / fromIntegral r
  in ak / bk - meanLogFail - 1 / k

-- | 単調増加関数の root を bisection で。 区間 [lo, hi] で g(lo) < 0 < g(hi) を仮定。
bisect
  :: (Double -> Double)  -- 単調増加 g
  -> Double              -- lo
  -> Double              -- hi
  -> Double              -- 許容誤差
  -> Int                 -- 最大反復
  -> Either Text Double
bisect g lo0 hi0 tol maxIter = go lo0 hi0 0
  where
    go !lo !hi !i
      | i >= maxIter             = Left "Weibull MLE: bisection did not converge"
      | (hi - lo) < tol          = Right ((lo + hi) / 2)
      | otherwise =
          let mid = (lo + hi) / 2
              gm  = g mid
          in if gm > 0
               then go lo mid (i + 1)
               else go mid hi (i + 1)

-- | 区間を「拡張 + 縮小」 でブラケットを取る。
--   関数 g は単調増加。 g(start_lo) ≥ 0 や g(start_hi) ≤ 0 の場合は範囲を広げる。
findBracket
  :: (Double -> Double)
  -> Double  -- 初期 lo (>0)
  -> Double  -- 初期 hi
  -> Int     -- 最大拡張回数
  -> Either Text (Double, Double)
findBracket g lo0 hi0 maxExp = go lo0 hi0 0
  where
    go !lo !hi !i
      | i >= maxExp = Left "Weibull MLE: failed to bracket root"
      | otherwise =
          let glo = g lo
              ghi = g hi
          in if glo <= 0 && ghi >= 0
               then Right (lo, hi)
               else if glo > 0  -- root より大きすぎる
                      then go (lo / 4) hi (i + 1)
                      else if ghi < 0  -- root より小さすぎる
                             then go lo (hi * 4) (i + 1)
                             else Right (lo, hi)

-- | 全観測 failure 仮定で MLE を解く中核ロジック。
--   xs (failure 時間) + xsAll (全観測; censored 含む) を分けるのは Phase 2.3 用。
solveWeibull
  :: Vector Double  -- failures (時間)
  -> Vector Double  -- 全観測 (失敗 + 打ち切り)
  -> Int            -- failure 数 r
  -> Either Text WeibullFit
solveWeibull failuresXs allXs r = do
  let g k = scoreG k allXs failuresXs r
  (lo, hi) <- findBracket g 0.1 10.0 30
  k        <- bisect g lo hi 1e-10 200
  let bk     = sumPow k allXs
      lam    = (bk / fromIntegral r) ** (1 / k)
      -- log-likelihood at MLE (failures contribution + censored survival)
      n      = V.length allXs
      sumLogFailures = V.sum (V.map log failuresXs)
      sumScaled = V.sum (V.map (\x -> (x / lam) ** k) allXs)
      ll     = fromIntegral r * (log k - k * log lam)
             + (k - 1) * sumLogFailures
             - sumScaled
      -- 観測 Fisher 情報 (uncensored 公式; censored ではバイアスあり)
      -- I_kk ≈ r / k^2 + Σ (x/λ)^k (log(x/λ))^2
      -- I_λλ ≈ k^2 · (Σ (x/λ)^k) / λ^2 − r k / λ^2  ... 簡素化:
      -- 厳密 expected information を Phase 2.4 で詰める。 ここでは
      -- observed information (負 Hessian) の対角成分を返す。
      iKK   = fromIntegral r / (k * k)
            + V.sum (V.map (\x -> (x / lam) ** k * (log (x / lam))**2) allXs)
      iLL   = (k * k / (lam * lam)) * V.sum (V.map (\x -> (x / lam) ** k) allXs)
            - fromIntegral r * k / (lam * lam) + 2 * k * fromIntegral r / (lam * lam)
            -- 教科書: I_λλ = r·k² / λ²  (uncensored at MLE は Σ (x/λ)^k = r)
            -- censored の場合は上の Σ がそのまま入る。
      iKL   = V.sum (V.map (\x -> (x / lam) ** k * log (x / lam)) allXs)
            * (k / lam)
            - fromIntegral r / lam
  pure WeibullFit
    { wfShape   = k
    , wfScale   = lam
    , wfLogLik  = ll
    , wfN       = n
    , wfRObs    = r
    , wfFisher  = (iKK, iKL, iLL)
    }

-- ===========================================================================
-- 公開関数
-- ===========================================================================

-- | Weibull MLE (打ち切り無し)。
--
-- 入力: 全て観測済の故障時間 (> 0)。
-- 解法: score equation @1/k = A(k)/B(k) − (1/n)·Σ log x@ を 1D bisection で
--       解き、 λ = (Σ x^k / n)^(1/k)。
fitWeibullMLE :: Vector Double -> Either Text WeibullFit
fitWeibullMLE xs = do
  _ <- validatePositive xs
  if V.length xs < 2
    then Left "fitWeibullMLE: need at least 2 observations"
    else
      let logs = V.map log xs
          maxL = V.maximum logs
          meanL = V.sum logs / fromIntegral (V.length xs)
      in if abs (maxL - meanL) < 1e-12
           then Left "fitWeibullMLE: data is constant (degenerate)"
           else solveWeibull xs xs (V.length xs)

-- | Weibull MLE (右打ち切り対応)。
--
-- 第 2 引数の @True@ = failure observed、 @False@ = right-censored。
-- 同じ score equation @1/k = A_all(k)/B_all(k) − (1/r)·Σ_{δ=1} log x@ を解くが、
-- @A@, @B@ は 全観測 (failure + 打ち切り) で加算し、 log-sum は failure のみ。
-- @r@ は failure 数。
fitWeibullCensored :: Vector Double -> Vector Bool -> Either Text WeibullFit
fitWeibullCensored xs deltas = do
  _ <- validatePositive xs
  if V.length xs /= V.length deltas
    then Left "fitWeibullCensored: times and delta indicators differ in length"
    else
      let failuresXs = V.ifilter (\i _ -> deltas V.! i) xs
          r = V.length failuresXs
      in if r < 2
           then Left "fitWeibullCensored: need at least 2 observed failures"
           else
             let logsFail = V.map log failuresXs
                 maxL  = V.maximum logsFail
                 meanL = V.sum logsFail / fromIntegral r
             in if abs (maxL - meanL) < 1e-12
                  then Left "fitWeibullCensored: failure data is constant (degenerate)"
                  else solveWeibull failuresXs xs r

-- | B_p 寿命: F^{-1}(p) = λ · (−ln(1−p))^(1/k)。
--
-- 典型用途: @bxLife 0.10 fit@ → B_10 (10%故障時間)、
--           @bxLife 0.50 fit@ → B_50 (中央寿命)。
bxLife :: Double -> WeibullFit -> Double
bxLife p _ | p <= 0 || p >= 1 = error "bxLife: probability must be in (0, 1)"
bxLife p fit =
  let k   = wfShape fit
      lam = wfScale fit
  in lam * (- log (1 - p)) ** (1 / k)

-- | (k_SE, λ_SE) — Fisher 情報行列の逆行列の対角の平方根。
--
-- 2x2 逆行列: var(k) = I_λλ / det、 var(λ) = I_kk / det、 det = I_kk·I_λλ − I_kλ²
weibullParameterSE :: WeibullFit -> (Double, Double)
weibullParameterSE fit =
  let (vK, _, vL) = weibullParameterCovariance fit
  in (sqrt (max 0 vK), sqrt (max 0 vL))

-- | (Var(k), Cov(k, λ), Var(λ))。 Fisher 情報行列の 2x2 逆行列。
--   非正定値の場合は (0, 0, 0) を返す (canvas 側で警告するための signal)。
weibullParameterCovariance :: WeibullFit -> (Double, Double, Double)
weibullParameterCovariance fit =
  let (iKK, iKL, iLL) = wfFisher fit
      det = iKK * iLL - iKL * iKL
  in if det <= 0
       then (0, 0, 0)
       else (iLL / det, -iKL / det, iKK / det)

-- | B_p 寿命の Wald 信頼区間 (delta method)。
--
-- @bxLifeCI p α fit@ で「故障時間が確率 p に達する時刻」 の
-- 信頼度 @1 − α@ 信頼区間 (例: α = 0.05 で 95% CI) を返す。
--
-- delta method:
--
-- > Var(B_p) ≈ (∂B_p/∂k)² Var(k) + (∂B_p/∂λ)² Var(λ) + 2 (∂B_p/∂k)(∂B_p/∂λ) Cov(k,λ)
-- > ∂B_p/∂λ = B_p / λ
-- > ∂B_p/∂k = −B_p · log(−log(1−p)) / k²
--
-- 戻り値: @(estimate, lower, upper)@。 lower は max(0, ...) で 0 にクリップ
-- (寿命は非負)。 共分散が非正定値で SE 計算不能の場合は @(estimate, estimate, estimate)@。
--
-- 注: α は両側で考えるので 95% CI なら z = 1.96 を内部使用。
bxLifeCI :: Double -> Double -> WeibullFit -> (Double, Double, Double)
bxLifeCI p alpha fit =
  let bp     = bxLife p fit
      k      = wfShape fit
      lam    = wfScale fit
      (vK, cKL, vL) = weibullParameterCovariance fit
      logArg = log (- log (1 - p))
      dbdL   = bp / lam
      dbdK   = - bp * logArg / (k * k)
      varBp  = dbdK * dbdK * vK + dbdL * dbdL * vL + 2 * dbdK * dbdL * cKL
      seBp   = if varBp > 0 then sqrt varBp else 0
      z      = quantileNormal (1 - alpha / 2)
      lo     = max 0 (bp - z * seBp)
      hi     = bp + z * seBp
  in (bp, lo, hi)

-- | 標準正規分布の分位点 (近似)。 95% CI で z = 1.959964…。
--   Acklam 高精度近似 (12 桁) を採用。
quantileNormal :: Double -> Double
quantileNormal q
  | q <= 0 || q >= 1 = error "quantileNormal: q must be in (0, 1)"
  | q < pLow = let qn = sqrt (-2 * log q) in
      (((((cN1 * qn + cN2) * qn + cN3) * qn + cN4) * qn + cN5) * qn + cN6)
      / ((((dN1 * qn + dN2) * qn + dN3) * qn + dN4) * qn + 1)
  | q <= pHigh = let qn = q - 0.5; r = qn * qn in
      ((((((aN1 * r + aN2) * r + aN3) * r + aN4) * r + aN5) * r + aN6) * qn)
      / (((((bN1 * r + bN2) * r + bN3) * r + bN4) * r + bN5) * r + 1)
  | otherwise = let qn = sqrt (-2 * log (1 - q)) in
      negate $
      (((((cN1 * qn + cN2) * qn + cN3) * qn + cN4) * qn + cN5) * qn + cN6)
      / ((((dN1 * qn + dN2) * qn + dN3) * qn + dN4) * qn + 1)
  where
    pLow  = 0.02425
    pHigh = 1 - pLow
    aN1 = -3.969683028665376e1; aN2 =  2.209460984245205e2
    aN3 = -2.759285104469687e2; aN4 =  1.383577518672690e2
    aN5 = -3.066479806614716e1; aN6 =  2.506628277459239e0
    bN1 = -5.447609879822406e1; bN2 =  1.615858368580409e2
    bN3 = -1.556989798598866e2; bN4 =  6.680131188771972e1
    bN5 = -1.328068155288572e1
    cN1 = -7.784894002430293e-3; cN2 = -3.223964580411365e-1
    cN3 = -2.400758277161838e0;  cN4 = -2.549732539343734e0
    cN5 =  4.374664141464968e0;  cN6 =  2.938163982698783e0
    dN1 =  7.784695709041462e-3; dN2 =  3.224671290700398e-1
    dN3 =  2.445134137142996e0;  dN4 =  3.754408661907416e0
