-- |
-- Module      : Hanalyze.Model.Robust
-- Description : IRLS による Huber / Tukey biweight ロバスト回帰 (M-estimator)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Robust regression M-estimators via IRLS (Phase 31-A5)。
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
-- - **Huber** (@k=1.345@、 95% 効率): 線形 + 線形クリップ、 滑らか、 標準
-- - **Tukey biweight** (@c=4.685@、 95% 効率): 完全棄却閾値付き、 外れ値の
--   影響を 0 に落とす、 だが多峰目的関数 (OLS 初期化が重要)
--
-- Reference:
--   Huber (1964) "Robust estimation of a location parameter".
--   Tukey (1977) biweight、 Rousseeuw-Leroy (1987) 教科書。
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

-- | M-estimator の選択。 LTS (Least Trimmed Squares) は非凸組合せ最適化なので
-- 別 Phase 候補 (phase-NN-regression-advanced.md §RR3 参照)。
data RobustEstimator
  = Huber !Double  -- ^ @k@ (= 1.345 で 95% 効率、 = 'defaultHuberK')
  | Tukey !Double  -- ^ @c@ (= 4.685 で 95% 効率、 = 'defaultTukeyC')
  deriving (Show, Eq)

data RobustFit = RobustFit
  { rfCoef       :: !(LA.Vector Double)   -- ^ 係数 β̂
  , rfScale      :: !Double                -- ^ ロバストスケール σ̂ (MAD-based)
  , rfWeights    :: !(LA.Vector Double)   -- ^ 最終 IRLS 重み (≤ 1)
  , rfFitted     :: !(LA.Vector Double)   -- ^ ŷ = Xβ̂
  , rfResiduals  :: !(LA.Vector Double)   -- ^ y - ŷ
  , rfIterations :: !Int                   -- ^ IRLS 反復回数
  , rfConverged  :: !Bool                  -- ^ tol 内収束したか
  , rfEstimator  :: !RobustEstimator       -- ^ 使用した estimator
  } deriving (Show)

-- | Huber の標準値 (95% Gaussian 効率): @k = 1.345@
defaultHuberK :: Double
defaultHuberK = 1.345

-- | Tukey biweight の標準値 (95% Gaussian 効率): @c = 4.685@
defaultTukeyC :: Double
defaultTukeyC = 4.685

-- ---------------------------------------------------------------------------
-- 重み関数 (= ψ(u)/u where ψ is the influence function)
-- ---------------------------------------------------------------------------

-- | Huber 重み: @w(u) = 1@ if @|u| ≤ k@、 @k/|u|@ otherwise。
-- ここで @u = r / σ@ (標準化残差)。
huberWeight :: Double -> Double -> Double
huberWeight k u
  | absU <= k = 1
  | absU == 0 = 1
  | otherwise = k / absU
  where absU = abs u

-- | Tukey biweight 重み: @w(u) = (1 - (u/c)²)²@ if @|u| ≤ c@、 @0@ otherwise。
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

-- | 影響関数 @ψ(u) = w(u)·u@ (= 標準化残差に重みを掛けたスコア)。
--   Huber: @u@ (|u|≤k) / @k·sign u@ (それ以外)。 Tukey: @u(1-(u/c)²)²@ (|u|≤c) / 0。
psiFn :: RobustEstimator -> Double -> Double
psiFn (Huber k) u = huberWeight k u * u
psiFn (Tukey c) u = tukeyWeight c u * u

-- | ψ の導関数 @ψ'(u)@ (M 推定量サンドイッチ分散の分母項)。
--   Huber: @1@ (|u|≤k) / @0@。 Tukey: @(1-(u/c)²)(1-5(u/c)²)@ (|u|≤c) / 0。
psiDerivFn :: RobustEstimator -> Double -> Double
psiDerivFn (Huber k) u = if abs u <= k then 1 else 0
psiDerivFn (Tukey c) u
  | abs u >= c = 0
  | otherwise  = let t2 = (u / c) * (u / c)
                 in (1 - t2) * (1 - 5 * t2)

-- ---------------------------------------------------------------------------
-- M 推定量の漸近共分散 (サンドイッチ・statsmodels RLM cov="H1")
-- ---------------------------------------------------------------------------

-- | M 推定量 β̂ の漸近共分散行列。 statsmodels @RLM@ 既定 (cov="H1") に一致:
--
-- @
-- u_i   = r_i / σ̂                       (標準化残差)
-- m     = mean ψ'(u_i)
-- K     = 1 + (p\/n)·Var(ψ')\/m²         (自由度補正)
-- cov   = K²·(σ̂²·Σψ(u_i)²\/(n−p))\/m² · (XᵀX)⁻¹
-- @
--
-- SE は @sqrt (diag cov)@、 β̂±z·SE が Wald 信頼区間 (RLM は正規分布で z)。
robustCovBeta
  :: RobustEstimator       -- ^ 使用した estimator (ψ/ψ' を決める)。
  -> Double                -- ^ ロバストスケール σ̂ ('rfScale')。
  -> LA.Vector Double      -- ^ 残差 r = y − ŷ ('rfResiduals')。
  -> LA.Matrix Double      -- ^ 設計行列 X (intercept 列付き)。
  -> LA.Matrix Double      -- ^ β̂ の共分散 (p × p)。
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

-- | M-estimator IRLS で線形回帰を fit。
--
-- @X@ は @n × p@ (intercept 列は呼び出し側で付加)、 @y@ は長さ @n@。
-- @maxIter@ デフォルト 50、 @tol@ デフォルト 1e-6。
fitRobustLM
  :: RobustEstimator
  -> LA.Matrix Double      -- ^ X
  -> LA.Vector Double      -- ^ y
  -> Int                   -- ^ max IRLS iterations
  -> Double                -- ^ tolerance on @|Δβ|₂@
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

-- | MAD ベースのロバストスケール推定:
-- @σ̂ = median(|r_i - median(r)|) / 0.6745@ (Gaussian 整合性)。
-- | ロバストスケール σ̂ = median(|r|) / Φ⁻¹(0.75)。 残差 r は intercept で中心化済
-- ゆえ **中心 0** で MAD を取る (= statsmodels RLM の @mad(resid, center=0)@ と一致。
-- median 中心化は二重中心化になり scale が過小になる)。 定数は Φ⁻¹(0.75)=0.674489…。
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
