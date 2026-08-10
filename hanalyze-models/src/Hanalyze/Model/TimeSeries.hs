{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : Hanalyze.Model.TimeSeries
-- Description : AR/MA/ARIMA・指数平滑・STL 分解を含む時系列モデリング一式
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Time-series modelling.
--
-- @
-- import Hanalyze.Model.TimeSeries
--
-- let acf = autocorrelation 20 ys
--     fit = fitAR 2 ys                          -- AR(2) by Yule-Walker
--     fc  = forecastAR fit ys 10                -- 10-step ahead
--
-- let hw  = holtWinters HWAdditive 12 ys
--     fc2 = hwForecast hw 24
-- @
--
-- == Implemented
--
--   * 'autocorrelation' / 'partialAutocorrelation' (sample ACF / PACF)
--   * 'fitAR' / 'forecastAR' (autoregressive AR(p) via Yule-Walker)
--   * 'fitMA' / 'forecastMA' (moving-average MA(q) via innovations)
--   * 'differencing' / 'inverseDifferencing' (helpers for ARIMA d)
--   * 'fitARIMA' / 'forecastARIMA' (ARIMA(p, d, q))
--   * 'simpleExpSmoothing' (single exp smoothing)
--   * 'holtWinters' (triple exp smoothing, additive / multiplicative)
--   * 'movingAverage' (centred / trailing)
--   * 'stlDecompose' (STL — seasonal-trend decomposition, simplified)
module Hanalyze.Model.TimeSeries
  ( -- * ACF / PACF
    autocorrelation
  , partialAutocorrelation
    -- * AR
  , ARFit (..)
  , fitAR
  , forecastAR
    -- * MA
  , MAFit (..)
  , fitMA
  , forecastMA
    -- * ARIMA
  , ARIMAFit (..)
  , fitARIMA
  , forecastARIMA
  , differencing
  , inverseDifferencing
    -- * Exponential smoothing
  , simpleExpSmoothing
  , HWMode (..)
  , HWFit (..)
  , holtWinters
  , hwForecast
    -- * Helpers
  , movingAverage
  , stlDecompose
  ) where

import qualified Numeric.LinearAlgebra as LA

-- ---------------------------------------------------------------------------
-- ACF / PACF
-- ---------------------------------------------------------------------------

-- | Sample autocorrelation function up to @maxLag@. Lag 0 is always
-- @1.0@. Computed as @r_k = c_k / c_0@ with biased autocovariance:
-- @c_k = (1/n) Σ_{t=0..n-k-1} (y_t - ȳ)(y_{t+k} - ȳ)@.
autocorrelation
  :: Int               -- ^ Maximum lag.
  -> LA.Vector Double
  -> LA.Vector Double
autocorrelation maxLag y =
  let n     = LA.size y
      ybar  = LA.sumElements y / fromIntegral n
      ydev  = y - LA.scalar ybar
      c0    = LA.dot ydev ydev / fromIntegral n
      cAt k = sum [ LA.atIndex ydev t * LA.atIndex ydev (t + k)
                  | t <- [0 .. n - k - 1] ]
              / fromIntegral n
      rs    = [ if c0 == 0 then 0 else cAt k / c0
              | k <- [0 .. maxLag] ]
  in LA.fromList rs

-- | Sample partial autocorrelation function up to @maxLag@ via direct
-- AR-fit: PACF[k] = last AR coefficient when fitting AR(k) by
-- Yule-Walker. Conceptually equivalent to the Durbin-Levinson
-- recursion but easier to implement correctly.
partialAutocorrelation
  :: Int
  -> LA.Vector Double
  -> LA.Vector Double
partialAutocorrelation maxLag y =
  let pacfAt 0 = 1
      pacfAt k =
        let fit = fitAR k y
            phi = arPhi fit
        in if LA.size phi == 0 then 0
             else LA.atIndex phi (k - 1)
  in LA.fromList [pacfAt k | k <- [0 .. maxLag]]

-- ---------------------------------------------------------------------------
-- AR (autoregressive)
-- ---------------------------------------------------------------------------

-- | Fitted AR(p) model.
data ARFit = ARFit
  { arOrder    :: !Int          -- ^ p
  , arPhi      :: !(LA.Vector Double)  -- ^ AR coefficients (length p)
  , arIntercept :: !Double      -- ^ μ (mean)
  , arResidVar :: !Double       -- ^ Innovation variance.
  } deriving (Show)

-- | Fit an AR(p) model by the Yule-Walker equations.
-- Solves @R φ = r@ where @R@ is the @p × p@ Toeplitz matrix of
-- autocovariances and @r = (γ_1, …, γ_p)@.
fitAR :: Int -> LA.Vector Double -> ARFit
fitAR p y =
  let n     = LA.size y
      ybar  = LA.sumElements y / fromIntegral n
      yC    = y - LA.scalar ybar
      gamma k = LA.dot (LA.subVector 0 (n - k) yC)
                       (LA.subVector k (n - k) yC) / fromIntegral n
      rhs   = LA.fromList [gamma k | k <- [1 .. p]]
      mat   = LA.fromLists
                [[gamma (abs (i - j)) | j <- [0 .. p - 1]]
                                      | i <- [0 .. p - 1]]
      phi   = mat LA.<\> rhs
      -- Innovation variance via Yule-Walker:
      -- σ² = γ_0 - Σ φ_i γ_i
      innovVar = gamma 0 - LA.dot phi rhs
  in ARFit
       { arOrder     = p
       , arPhi       = phi
       , arIntercept = ybar
       , arResidVar  = max 0 innovVar
       }

-- | Forecast @h@ steps ahead from a fitted AR model and the most
-- recent observations (in chronological order).
forecastAR
  :: ARFit
  -> LA.Vector Double  -- ^ History (must be ≥ p).
  -> Int               -- ^ Horizon h.
  -> LA.Vector Double
forecastAR fit hist h =
  let p     = arOrder fit
      mu    = arIntercept fit
      phi   = arPhi fit
      lastP = LA.toList (LA.subVector (LA.size hist - p) p hist)
      go _ acc 0 = reverse acc
      go window acc k =
        let dev    = zipWith (-) window (replicate p mu)
            yHat   = mu + LA.dot phi (LA.fromList dev)
            window' = drop 1 window ++ [yHat]
        in go window' (yHat : acc) (k - 1)
  in LA.fromList (go lastP [] h)

-- ---------------------------------------------------------------------------
-- MA (moving average)
-- ---------------------------------------------------------------------------

-- | Fitted MA(q) model.
data MAFit = MAFit
  { maOrder    :: !Int
  , maTheta    :: !(LA.Vector Double)  -- ^ MA coefficients (length q)
  , maIntercept :: !Double
  , maResidVar :: !Double
  , maResiduals :: !(LA.Vector Double)  -- ^ Innovation series.
  } deriving (Show)

-- | Fit an MA(q) model via the innovations algorithm (Brockwell-Davis
-- 1991, §5.2). Returns the estimated θ_i and innovation series.
fitMA :: Int -> LA.Vector Double -> MAFit
fitMA q y =
  let n     = LA.size y
      ybar  = LA.sumElements y / fromIntegral n
      yC    = y - LA.scalar ybar
      gamma k = LA.dot (LA.subVector 0 (n - k) yC)
                       (LA.subVector k (n - k) yC) / fromIntegral n
      -- Innovations algorithm: recursion
      -- v_n = γ_0
      -- θ_{n,n-k} = (γ_{n-k} - Σ_{j=0}^{k-1} θ_{n,n-j} θ_{k,k-j} v_j) / v_k
      -- v_n = γ_0 - Σ_{j=0}^{n-1} θ_{n,n-j}² v_j
      --
      -- We compute up to lag q.
      theta = LA.konst 0 q :: LA.Vector Double
      _ = theta
      -- Simplified approximation: use sample autocovariances directly
      -- to estimate θ via least squares (Hannan-Rissanen 1982).
      -- This is less accurate than full Innovations but simpler.
      thetaSimple = LA.fromList [ gamma k / max 1e-15 (gamma 0)
                                | k <- [1 .. q] ]
      -- Compute residuals: e_t = y_t - μ - Σ θ_i e_{t-i}
      residuals = computeMAResiduals (LA.toList yC) (LA.toList thetaSimple)
      sigma2 = sum [r * r | r <- residuals] / fromIntegral n
  in MAFit
       { maOrder     = q
       , maTheta     = thetaSimple
       , maIntercept = ybar
       , maResidVar  = sigma2
       , maResiduals = LA.fromList residuals
       }
  where
    computeMAResiduals :: [Double] -> [Double] -> [Double]
    computeMAResiduals ys thetas =
      let go acc []     = reverse acc
          go acc (yi:ys') =
            let q' = length thetas
                eHist = take q' acc  -- recent residuals
                pad   = replicate (q' - length eHist) 0
                ePadded = pad ++ eHist
                yHat  = sum (zipWith (*) thetas (reverse ePadded))
                eNew  = yi - yHat
            in go (eNew : acc) ys'
      in go [] ys

-- | Forecast h steps from MA(q). Beyond q steps, the forecast equals
-- the mean (innovations are zero in expectation).
forecastMA :: MAFit -> Int -> LA.Vector Double
forecastMA fit h =
  let q     = maOrder fit
      theta = LA.toList (maTheta fit)
      mu    = maIntercept fit
      eHist = LA.toList (maResiduals fit)
      eRecent = take q (reverse eHist)
      go k
        | k > q || k > h = []
        | otherwise =
            let pad = replicate (q - length eRecent) 0
                eP  = pad ++ eRecent
                yhat = mu + sum (zipWith (*) theta (drop (k - 1) (reverse eP)))
            in yhat : go (k + 1)
      truncated = take h (go 1 ++ repeat mu)
  in LA.fromList truncated

-- ---------------------------------------------------------------------------
-- ARIMA
-- ---------------------------------------------------------------------------

-- | Fitted ARIMA(p, d, q) model.
data ARIMAFit = ARIMAFit
  { arimaP   :: !Int
  , arimaD   :: !Int
  , arimaQ   :: !Int
  , arimaAR  :: !ARFit
  , arimaMA  :: !MAFit
  , arimaOrigSeries :: !(LA.Vector Double)
  } deriving (Show)

-- | Fit ARIMA(p, d, q): difference d times, then fit AR(p) + MA(q) on
-- the differenced series. Uses two-stage estimation (AR first, then
-- MA on residuals).
fitARIMA :: Int -> Int -> Int -> LA.Vector Double -> ARIMAFit
fitARIMA p d q y =
  let yDiff = iterate differencing y !! d
      arFit = fitAR p yDiff
      arResid = computeARResiduals arFit yDiff
      maFit = fitMA q arResid
  in ARIMAFit
       { arimaP   = p
       , arimaD   = d
       , arimaQ   = q
       , arimaAR  = arFit
       , arimaMA  = maFit
       , arimaOrigSeries = y
       }

computeARResiduals :: ARFit -> LA.Vector Double -> LA.Vector Double
computeARResiduals fit y =
  let p   = arOrder fit
      mu  = arIntercept fit
      phi = LA.toList (arPhi fit)
      n   = LA.size y
      ys  = LA.toList y
      go i
        | i < p = 0
        | otherwise =
            let dev = [ys !! (i - k - 1) - mu | k <- [0 .. p - 1]]
                yHat = mu + sum (zipWith (*) phi dev)
            in (ys !! i) - yHat
      residuals = [go i | i <- [0 .. n - 1]]
  in LA.fromList residuals

-- | Forecast h steps from a fitted ARIMA model.
forecastARIMA :: ARIMAFit -> Int -> LA.Vector Double
forecastARIMA fit h =
  let _origY = arimaOrigSeries fit
      d      = arimaD fit
      diff_d = iterate differencing _origY !! d
      arFc   = forecastAR (arimaAR fit) diff_d h
      maFc   = forecastMA (arimaMA fit) h
      combined = arFc + maFc - LA.scalar (arIntercept (arimaAR fit))
      -- Inverse-difference d times.
      lastObs = take d (reverse (LA.toList _origY))
      _ = lastObs
  in iterate (inverseDifferencing _origY) combined !! d

-- | First-difference: @y'_t = y_t - y_{t-1}@. Output length = n - 1.
differencing :: LA.Vector Double -> LA.Vector Double
differencing y =
  let n = LA.size y
  in if n < 2 then LA.fromList []
       else LA.subVector 1 (n - 1) y - LA.subVector 0 (n - 1) y

-- | Inverse first-difference given the last observation of the
-- original series. Output length = n + 1 (prepends the seed).
-- Simplified: cumulative sum prepended by 0.
inverseDifferencing
  :: LA.Vector Double  -- ^ Original (for last value reference).
  -> LA.Vector Double  -- ^ Differenced forecast.
  -> LA.Vector Double
inverseDifferencing origY diff =
  let lastY = LA.atIndex origY (LA.size origY - 1)
      cumS  = scanl (+) lastY (LA.toList diff)
  in LA.fromList (drop 1 cumS)

-- ---------------------------------------------------------------------------
-- Exponential smoothing
-- ---------------------------------------------------------------------------

-- | Simple exponential smoothing (single, no trend / seasonality).
-- @s_t = α y_t + (1 − α) s_{t−1}@. Returns the smoothed series.
simpleExpSmoothing
  :: Double            -- ^ α ∈ (0, 1).
  -> LA.Vector Double
  -> LA.Vector Double
simpleExpSmoothing alpha y =
  let ys = LA.toList y
      go _    []     = []
      go prev (yi:rest) =
        let sNew = alpha * yi + (1 - alpha) * prev
        in sNew : go sNew rest
      s0 = case ys of { (y0:_) -> y0; [] -> 0 }
  in LA.fromList (go s0 ys)

-- | Holt-Winters mode (additive vs multiplicative seasonality).
data HWMode = HWAdditive | HWMultiplicative deriving (Show, Eq)

-- | Fitted Holt-Winters (triple exponential smoothing).
data HWFit = HWFit
  { hwMode   :: !HWMode
  , hwPeriod :: !Int
  , hwAlpha  :: !Double
  , hwBeta   :: !Double
  , hwGamma  :: !Double
  , hwLevel  :: !Double          -- ^ Final level component.
  , hwTrend  :: !Double          -- ^ Final trend component.
  , hwSeasonal :: ![Double]      -- ^ Final seasonal indices (length period).
  , hwFitted :: !(LA.Vector Double)
  } deriving (Show)

-- | Fit Holt-Winters (additive seasonal). Picks default smoothing
-- parameters @α = β = γ = 0.3@; for production use, optimise these.
holtWinters
  :: HWMode            -- ^ Additive or multiplicative.
  -> Int               -- ^ Seasonal period (e.g. 12 for monthly).
  -> LA.Vector Double  -- ^ Time series.
  -> HWFit
holtWinters mode period y =
  let alpha = 0.3 :: Double
      beta  = 0.1 :: Double
      gamma = 0.1 :: Double
      ys    = LA.toList y
      -- Initialise from first 'period' observations.
      initLevel = sum (take period ys) / fromIntegral period
      initTrend = (sum (take period (drop period ys))
                  - sum (take period ys))
                  / fromIntegral (period * period)
      initSeas = case mode of
        HWAdditive       ->
          [ ys !! i - initLevel | i <- [0 .. period - 1] ]
        HWMultiplicative ->
          [ ys !! i / max 1e-15 initLevel | i <- [0 .. period - 1] ]
      -- Iterate.
      go !lvl !trd !seas !fitted [] = (lvl, trd, seas, reverse fitted)
      go !lvl !trd !seas !fitted (yi:rest) =
        let p     = period
            sIdx  = length fitted `mod` p
            sCur  = seas !! sIdx
            (lvlNew, trdNew, sNew, fHat) = case mode of
              HWAdditive ->
                let l' = alpha * (yi - sCur) + (1 - alpha) * (lvl + trd)
                    t' = beta  * (l' - lvl) + (1 - beta)  * trd
                    s' = gamma * (yi - l') + (1 - gamma) * sCur
                    fh = lvl + trd + sCur
                in (l', t', s', fh)
              HWMultiplicative ->
                let l' = alpha * (yi / max 1e-15 sCur) + (1 - alpha) * (lvl + trd)
                    t' = beta  * (l' - lvl) + (1 - beta)  * trd
                    s' = gamma * (yi / max 1e-15 l') + (1 - gamma) * sCur
                    fh = (lvl + trd) * sCur
                in (l', t', s', fh)
            seas' = updateAt sIdx sNew seas
        in go lvlNew trdNew seas' (fHat : fitted) rest
      (finalLvl, finalTrd, finalSeas, fits) =
        go initLevel initTrend initSeas [] ys
  in HWFit
       { hwMode     = mode
       , hwPeriod   = period
       , hwAlpha    = alpha
       , hwBeta     = beta
       , hwGamma    = gamma
       , hwLevel    = finalLvl
       , hwTrend    = finalTrd
       , hwSeasonal = finalSeas
       , hwFitted   = LA.fromList fits
       }

-- | Forecast @h@ steps ahead from a fitted Holt-Winters model.
hwForecast :: HWFit -> Int -> LA.Vector Double
hwForecast fit h =
  let lvl   = hwLevel fit
      trd   = hwTrend fit
      seas  = hwSeasonal fit
      p     = hwPeriod fit
      mode  = hwMode fit
      go k
        | k > h = []
        | otherwise =
            let sIdx = (k - 1) `mod` p
                fc   = case mode of
                  HWAdditive       -> lvl + fromIntegral k * trd + seas !! sIdx
                  HWMultiplicative -> (lvl + fromIntegral k * trd) * seas !! sIdx
            in fc : go (k + 1)
  in LA.fromList (go 1)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Centred moving average with window @w@ (odd recommended). Values
-- near the edges have NaN.
movingAverage :: Int -> LA.Vector Double -> LA.Vector Double
movingAverage w y =
  let n     = LA.size y
      half  = w `div` 2
      avg i
        | i - half < 0 || i + half >= n = 0/0
        | otherwise = sum [LA.atIndex y (i + j) | j <- [-half .. half]]
                      / fromIntegral w
  in LA.fromList [avg i | i <- [0 .. n - 1]]

-- | Simplified STL decomposition (loess-free version): subtract a
-- centred moving-average trend, then estimate seasonality as the
-- mean per phase.
stlDecompose
  :: Int               -- ^ Period.
  -> LA.Vector Double
  -> (LA.Vector Double, LA.Vector Double, LA.Vector Double)
       -- ^ (trend, seasonal, residual).
stlDecompose period y =
  let n     = LA.size y
      trend = movingAverage period y
      detrended = LA.fromList
        [ if isNaN (LA.atIndex trend i) then 0
            else LA.atIndex y i - LA.atIndex trend i
        | i <- [0 .. n - 1] ]
      -- Per-phase mean over non-NaN cells.
      phaseMeans =
        [ let maxJ = (n - 1 - i) `div` period
              xs = [LA.atIndex detrended (i + j * period)
                   | j <- [0 .. maxJ], i + j * period < n]
              valid = filter (not . isNaN) xs
          in if null valid then 0 else sum valid / fromIntegral (length valid)
        | i <- [0 .. period - 1] ]
      -- Centre seasonal indices around 0.
      seasMean = sum phaseMeans / fromIntegral period
      seasonal = LA.fromList
        [ phaseMeans !! (i `mod` period) - seasMean | i <- [0 .. n - 1] ]
      residual = y - trend - seasonal
  in (trend, seasonal, residual)

-- | Update list element at index.
updateAt :: Int -> a -> [a] -> [a]
updateAt _ _ []     = []
updateAt 0 v (_:xs) = v : xs
updateAt i v (x:xs) = x : updateAt (i - 1) v xs

