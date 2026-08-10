-- |
-- Module      : Hanalyze.Model.Robust
-- Description : IRLS による Huber / Tukey biweight ロバスト回帰 (M-estimator)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: IRLS による Huber / Tukey biweight ロバスト回帰 (M-estimator)。
--
-- 外れ値を含むデータに対する線形回帰。 OLS の二乗損失を bounded influence
-- 関数 (Huber / Tukey biweight) に置き換え、 Iteratively Reweighted Least
-- Squares で β を求める。 JMP "Fit Model > Personality: Robust Fit"、
-- R `MASS::rlm` 相当。
--
-- ## アルゴリズム
--
-- 1. β を OLS で初期化
-- 2. 残差 @r_i = y_i - x_i^T β@ を計算
-- 3. ロバストスケール推定 @σ̂ = MAD(r) / 0.6745@
-- 4. 影響関数から重み @w_i@ を計算 ('huberWeight' / 'tukeyWeight')
-- 5. 加重 LS で β を更新: @β ← (X^T W X)^{-1} X^T W y@
-- 6. 収束まで 2-5 を繰り返す
--
-- ## 推定子の選択
--
-- - __Huber__ (@k=1.345@、 95% 効率): 線形 + 線形クリップ、 滑らか、 標準
-- - __Tukey biweight__ (@c=4.685@、 95% 効率): 完全棄却閾値付き、 外れ値の
--   影響を 0 に落とす、 だが多峰目的関数 (OLS 初期化が重要)
--
-- Reference:
--   Huber (1964) "Robust estimation of a location parameter".
--   Tukey (1977) biweight、 Rousseeuw-Leroy (1987) 教科書。
--
-- [English]: Huber \/ Tukey biweight robust regression (M-estimator) via IRLS.
--
-- Linear regression for data containing outliers. Replaces OLS's squared
-- loss with a bounded-influence function (Huber \/ Tukey biweight) and
-- solves for β with Iteratively Reweighted Least Squares. Equivalent to
-- JMP's "Fit Model > Personality: Robust Fit" or R's @MASS::rlm@.
--
-- ## Algorithm
--
-- 1. Initialize β with OLS
-- 2. Compute residuals @r_i = y_i - x_i^T β@
-- 3. Estimate robust scale @σ̂ = MAD(r) / 0.6745@
-- 4. Compute weights @w_i@ from the influence function ('huberWeight' \/ 'tukeyWeight')
-- 5. Update β via weighted LS: @β ← (X^T W X)^{-1} X^T W y@
-- 6. Repeat 2-5 until convergence
--
-- ## Choice of estimator
--
-- - __Huber__ (@k=1.345@, 95% efficiency): linear + linear clipping, smooth, standard
-- - __Tukey biweight__ (@c=4.685@, 95% efficiency): has a hard rejection
--   threshold, drives the influence of outliers to 0, but has a multimodal
--   objective function (OLS initialization matters)
--
-- Reference:
--   Huber (1964) "Robust estimation of a location parameter".
--   Tukey (1977) biweight, Rousseeuw-Leroy (1987) textbook.
module Hanalyze.Model.Robust
  ( RobustEstimator (..)
  , RobustFit (..)
  , defaultHuberK
  , defaultTukeyC
  , fitRobustLM
  , huberWeight
  , tukeyWeight
  , psiFn
  , psiDerivFn
  , robustCovBeta
  ) where

import qualified Numeric.LinearAlgebra as LA
import           Data.List             (sort)

-- ---------------------------------------------------------------------------
-- 型
-- ---------------------------------------------------------------------------

-- | [日本語]: M-estimator の選択。 LTS (Least Trimmed Squares) は非凸組合せ
--   最適化なので、 別途候補として今後の対応課題とする (regression-advanced 系
--   の後続検討 §RR3 参照)。
--   [English]: Choice of M-estimator. LTS (Least Trimmed Squares) is a
--   non-convex combinatorial optimization problem, so it remains a future
--   candidate for later work (see the follow-up discussion in the
--   regression-advanced material, §RR3).
data RobustEstimator
  = Huber !Double  -- ^ [日本語]: @k@ (= 1.345 で 95% 効率、 = 'defaultHuberK')。 [English]: @k@ (= 1.345 for 95% efficiency, = 'defaultHuberK').
  | Tukey !Double  -- ^ [日本語]: @c@ (= 4.685 で 95% 効率、 = 'defaultTukeyC')。 [English]: @c@ (= 4.685 for 95% efficiency, = 'defaultTukeyC').
  deriving (Show, Eq)

data RobustFit = RobustFit
  { rfCoef       :: !(LA.Vector Double)   -- ^ [日本語]: 係数 β̂。 [English]: Coefficients β̂.
  , rfScale      :: !Double                -- ^ [日本語]: ロバストスケール σ̂ (MAD-based)。 [English]: Robust scale σ̂ (MAD-based).
  , rfWeights    :: !(LA.Vector Double)   -- ^ [日本語]: 最終 IRLS 重み (≤ 1)。 [English]: Final IRLS weights (≤ 1).
  , rfFitted     :: !(LA.Vector Double)   -- ^ [日本語]: ŷ = Xβ̂。 [English]: ŷ = Xβ̂.
  , rfResiduals  :: !(LA.Vector Double)   -- ^ [日本語]: y - ŷ。 [English]: y - ŷ.
  , rfIterations :: !Int                   -- ^ [日本語]: IRLS 反復回数。 [English]: Number of IRLS iterations.
  , rfConverged  :: !Bool                  -- ^ [日本語]: tol 内収束したか。 [English]: Whether it converged within tol.
  , rfEstimator  :: !RobustEstimator       -- ^ [日本語]: 使用した estimator。 [English]: The estimator used.
  } deriving (Show)

-- | [日本語]: Huber の標準値 (95% Gaussian 効率): @k = 1.345@。
--   [English]: Huber's standard value (95% Gaussian efficiency): @k = 1.345@.
defaultHuberK :: Double
defaultHuberK = 1.345

-- | [日本語]: Tukey biweight の標準値 (95% Gaussian 効率): @c = 4.685@。
--   [English]: Tukey biweight's standard value (95% Gaussian efficiency): @c = 4.685@.
defaultTukeyC :: Double
defaultTukeyC = 4.685

-- ---------------------------------------------------------------------------
-- 重み関数 (= ψ(u)/u where ψ is the influence function)
-- ---------------------------------------------------------------------------

-- | [日本語]: Huber 重み: @w(u) = 1@ if @|u| ≤ k@、 @k/|u|@ otherwise。
--   ここで @u = r / σ@ (標準化残差)。
--   [English]: Huber weight: @w(u) = 1@ if @|u| ≤ k@, @k/|u|@ otherwise.
--   Here @u = r / σ@ (the standardized residual).
huberWeight :: Double -> Double -> Double
huberWeight k u
  | absU <= k = 1
  | absU == 0 = 1
  | otherwise = k / absU
  where absU = abs u

-- | [日本語]: Tukey biweight 重み: @w(u) = (1 - (u/c)²)²@ if @|u| ≤ c@、 @0@ otherwise。
--   [English]: Tukey biweight weight: @w(u) = (1 - (u/c)²)²@ if @|u| ≤ c@, @0@ otherwise.
tukeyWeight :: Double -> Double -> Double
tukeyWeight c u
  | absU >= c = 0
  | otherwise = let t = u / c
                    s = 1 - t * t
                in s * s
  where absU = abs u

-- ---------------------------------------------------------------------------
-- 影響関数 ψ とその導関数 ψ' (M 推定量の漸近共分散に使う)
-- ψ(u) = w(u)·u (重み × 標準化残差)。
-- ---------------------------------------------------------------------------

-- | [日本語]: 影響関数 @ψ(u) = w(u)·u@ (= 標準化残差に重みを掛けたスコア)。
--   Huber: @u@ (|u|≤k) / @k·sign u@ (それ以外)。 Tukey: @u(1-(u/c)²)²@ (|u|≤c) / 0。
--   [English]: Influence function @ψ(u) = w(u)·u@ (= the score obtained by
--   weighting the standardized residual). Huber: @u@ (|u|≤k) / @k·sign u@
--   (otherwise). Tukey: @u(1-(u/c)²)²@ (|u|≤c) / 0.
psiFn :: RobustEstimator -> Double -> Double
psiFn (Huber k) u = huberWeight k u * u
psiFn (Tukey c) u = tukeyWeight c u * u

-- | [日本語]: ψ の導関数 @ψ'(u)@ (M 推定量サンドイッチ分散の分母項)。
--   Huber: @1@ (|u|≤k) / @0@。 Tukey: @(1-(u/c)²)(1-5(u/c)²)@ (|u|≤c) / 0。
--   [English]: Derivative of ψ, @ψ'(u)@ (the denominator term of the
--   M-estimator's sandwich variance). Huber: @1@ (|u|≤k) / @0@. Tukey:
--   @(1-(u/c)²)(1-5(u/c)²)@ (|u|≤c) / 0.
psiDerivFn :: RobustEstimator -> Double -> Double
psiDerivFn (Huber k) u = if abs u <= k then 1 else 0
psiDerivFn (Tukey c) u
  | abs u >= c = 0
  | otherwise  = let t2 = (u / c) * (u / c)
                 in (1 - t2) * (1 - 5 * t2)

-- ---------------------------------------------------------------------------
-- M 推定量の漸近共分散 (サンドイッチ・statsmodels RLM cov="H1")
-- ---------------------------------------------------------------------------

-- | [日本語]: M 推定量 β̂ の漸近共分散行列。 statsmodels @RLM@ 既定 (cov="H1") に一致:
--
-- @
-- u_i   = r_i / σ̂                       (標準化残差)
-- m     = mean ψ'(u_i)
-- K     = 1 + (p\/n)·Var(ψ')\/m²         (自由度補正)
-- cov   = K²·(σ̂²·Σψ(u_i)²\/(n−p))\/m² · (XᵀX)⁻¹
-- @
--
-- SE は @sqrt (diag cov)@、 β̂±z·SE が Wald 信頼区間 (RLM は正規分布で z)。
--   [English]: The asymptotic covariance matrix of the M-estimator β̂.
--   Matches statsmodels @RLM@'s default (cov="H1"):
--
-- @
-- u_i   = r_i / σ̂                       (standardized residual)
-- m     = mean ψ'(u_i)
-- K     = 1 + (p\/n)·Var(ψ')\/m²         (degrees-of-freedom correction)
-- cov   = K²·(σ̂²·Σψ(u_i)²\/(n−p))\/m² · (XᵀX)⁻¹
-- @
--
-- The SE is @sqrt (diag cov)@; β̂±z·SE gives the Wald confidence interval
-- (RLM uses z from the normal distribution).
robustCovBeta
  :: RobustEstimator       -- ^ [日本語]: 使用した estimator (ψ/ψ' を決める)。 [English]: The estimator used (determines ψ\/ψ').
  -> Double                -- ^ [日本語]: ロバストスケール σ̂ ('rfScale')。 [English]: The robust scale σ̂ ('rfScale').
  -> LA.Vector Double      -- ^ [日本語]: 残差 r = y − ŷ ('rfResiduals')。 [English]: Residuals r = y − ŷ ('rfResiduals').
  -> LA.Matrix Double      -- ^ [日本語]: 設計行列 X (intercept 列付き)。 [English]: The design matrix X (with an intercept column).
  -> LA.Matrix Double      -- ^ [日本語]: β̂ の共分散 (p × p)。 [English]: The covariance of β̂ (p × p).
robustCovBeta est scale resid x =
  let n      = LA.rows x
      p      = LA.cols x
      u      = LA.cmap (/ scale) resid
      pderiv = LA.cmap (psiDerivFn est) u
      m      = meanV pderiv
      varpp  = meanV (LA.cmap (\v -> (v - m) * (v - m)) pderiv)   -- 母分散 (ddof=0)
      kcorr  = 1 + (fromIntegral p / fromIntegral n) * varpp / (m * m)
      sspsi  = LA.sumElements (LA.cmap (\v -> let pv = psiFn est v in pv * pv) u)
      xtxInv = LA.inv (LA.tr x LA.<> x)
      factor = kcorr * kcorr
               * (sspsi * scale * scale / fromIntegral (n - p)) / (m * m)
  in LA.scale factor xtxInv
  where
    meanV v = LA.sumElements v / fromIntegral (LA.size v)

-- ---------------------------------------------------------------------------
-- IRLS
-- ---------------------------------------------------------------------------

-- | [日本語]: M-estimator IRLS で線形回帰を fit。
--
-- @X@ は @n × p@ (intercept 列は呼び出し側で付加)、 @y@ は長さ @n@。
-- @maxIter@ デフォルト 50、 @tol@ デフォルト 1e-6。
--   [English]: Fits a linear regression via M-estimator IRLS.
--
-- @X@ is @n × p@ (the caller appends the intercept column); @y@ has length
-- @n@. @maxIter@ defaults to 50, @tol@ defaults to 1e-6.
fitRobustLM
  :: RobustEstimator
  -> LA.Matrix Double      -- ^ [日本語]: X。 [English]: X.
  -> LA.Vector Double      -- ^ [日本語]: y。 [English]: y.
  -> Int                   -- ^ [日本語]: max IRLS iterations。 [English]: Maximum number of IRLS iterations.
  -> Double                -- ^ [日本語]: @|Δβ|₂@ に対する許容誤差。 [English]: Tolerance on @|Δβ|₂@.
  -> RobustFit
fitRobustLM est x y maxIter tol =
  let -- 初期 β: OLS
      beta0 = LA.flatten (x LA.<\> LA.asColumn y)
      step beta =
        let yHat   = x LA.#> beta
            resid  = y - yHat
            sigma  = madScale resid
            sigma' = if sigma < 1e-12 then 1e-12 else sigma
            uVec   = LA.cmap (/ sigma') resid
            wVec   = case est of
                       Huber k -> LA.cmap (huberWeight k) uVec
                       Tukey c -> LA.cmap (tukeyWeight c) uVec
            -- 加重 LS: β ← (X^T W X)^{-1} X^T W y
            wDiag  = wVec
            xtWx   = LA.tr x LA.<> (x * LA.asColumn wDiag)
            xtWy   = LA.tr x LA.#> (wDiag * y)
            betaN  = LA.flatten (xtWx LA.<\> LA.asColumn xtWy)
        in (betaN, sigma', wVec)
      loop !k !beta
        | k >= maxIter = (beta, k, False)
        | otherwise    =
            let (betaN, _, _) = step beta
                diff = LA.norm_2 (betaN - beta)
            in if diff < tol
                 then (betaN, k + 1, True)
                 else loop (k + 1) betaN
      (betaFinal, iters, converged) = loop 0 beta0
      yHatF  = x LA.#> betaFinal
      residF = y - yHatF
      sigmaF = max 1e-12 (madScale residF)
      uF     = LA.cmap (/ sigmaF) residF
      wF     = case est of
                 Huber k -> LA.cmap (huberWeight k) uF
                 Tukey c -> LA.cmap (tukeyWeight c) uF
  in RobustFit
       { rfCoef       = betaFinal
       , rfScale      = sigmaF
       , rfWeights    = wF
       , rfFitted     = yHatF
       , rfResiduals  = residF
       , rfIterations = iters
       , rfConverged  = converged
       , rfEstimator  = est
       }

-- ---------------------------------------------------------------------------
-- ロバストスケール (Median Absolute Deviation)
-- ---------------------------------------------------------------------------

-- | [日本語]: MAD ベースのロバストスケール推定:
-- @σ̂ = median(|r_i - median(r)|) / 0.6745@ (Gaussian 整合性)。
-- ロバストスケール σ̂ = median(|r|) / Φ⁻¹(0.75)。 残差 r は intercept で中心化済
-- ゆえ __中心 0__ で MAD を取る (= statsmodels RLM の @mad(resid, center=0)@ と一致。
-- median 中心化は二重中心化になり scale が過小になる)。 定数は Φ⁻¹(0.75)=0.674489…。
--   [English]: MAD-based robust scale estimate:
-- @σ̂ = median(|r_i - median(r)|) / 0.6745@ (for Gaussian consistency).
-- The robust scale σ̂ = median(|r|) / Φ⁻¹(0.75). Since the residuals r are
-- already centered by the intercept, the MAD is taken __around 0__ (matching
-- statsmodels RLM's @mad(resid, center=0)@; centering on the median again
-- would double-center and understate the scale). The constant is
-- Φ⁻¹(0.75)=0.674489….
madScale :: LA.Vector Double -> Double
madScale v =
  let dev = map abs (LA.toList v)       -- 中心 0 (statsmodels RLM 準拠)
      mad = medianList dev
  in mad / 0.6744897501960817

medianList :: [Double] -> Double
medianList [] = 0
medianList xs =
  let s = sort xs
      n = length s
  in if odd n
       then s !! (n `div` 2)
       else 0.5 * (s !! (n `div` 2 - 1) + s !! (n `div` 2))
