{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hanalyze.Diagnostics
-- Description : plot 非依存の回帰モデル係数診断 (点予測・係数要約・bootstrap・平滑項 F 検定)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- 回帰モデルの係数診断 (plot 非依存層)。
--
-- fit 済モデルを「数値として」 使う細粒度 API: 点予測・係数ベクトル・係数要約。
-- 'Hanalyze.Plot' (cabal flag @plot-integration@ 配下) が描画
-- ('VisualSpec' 化) を担うのに対し、 本モジュールは **hgg に依存しない**
-- 係数統計 (t\/z・p 値・95% CI) のみを切り出したもの。 非ゲート (常時 build) なので
-- 'df |-> spec' / 'coefSummary' が plot フラグ無しで使える。
-- 'Hanalyze.Plot' は本モジュールを import し従来の名前で再 export する。
module Hanalyze.Diagnostics
  ( -- * モデル API 層 (描画と独立: predict / describe / coefficients)
    Coef (..)
  , ModelAPI (..)
  , coefSummaryFromCov
  , lmCoefCov
    -- * 統一係数サマリ (t\/z・p 値・95% CI)
  , CoefRow (..)
  , HasCoefSummary (..)
  , coefRowsLM
  , coefRowsZ
  , designCoefNames
    -- * bootstrap 係数サマリ (quantile / penalized)
  , HasCoefBoot (..)
  , coefSummaryBoot
  , bootCoefRows
  , resampleRows
    -- * 平滑項単位の近似有意性 (mgcv 流 edf + 近似 F) — Phase 72.2
  , TermRow (..)
  , HasTermSummary (..)
  , termSummary
  , gamTermRows
    -- * 統一玄関 (.summary() 風) — Phase 72.3
  , ModelReport (..)
  , HasReport (..)
  , modelReport
  , showReport
  ) where

import           Data.List             (sort)
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Vector           as V
import           Data.Word             (Word32)
import           Numeric               (showFFloat)
import           Control.Monad         (replicateM)
import           Control.Monad.ST      (runST)
import           System.Random.MWC     (initialize, uniformR)
import qualified Numeric.LinearAlgebra as LA

import qualified Statistics.Distribution        as SD
import           Statistics.Distribution.Normal (standard)
import qualified Statistics.Distribution.FDistribution as FD

import           Hanalyze.Model.Wrappers
import           Hanalyze.Model.Core   (FitResult, coefficientsV, residualsV, fittedV)
import           Hanalyze.Model.GAM    (GAMFit (..))
import           Hanalyze.Model.Spline (SplineFit (..))
import           Hanalyze.Model.GLM    (linkFnOf)
import           Hanalyze.Model.GP     (GPResult)
import           Hanalyze.Model.LM.Diagnostics (CoefStats (..), lmCoefStats, ciTValue)
import           Hanalyze.Model.LM     (designMatrix)
import           Hanalyze.Model.Quantile (QRFit (..), fitQuantile)
import           Hanalyze.Model.Robust (RobustFit (..), robustCovBeta)
import           Hanalyze.Model.Weibull (quantileNormal)
import           Hanalyze.Model.Formula (Formula (..))

-- ===========================================================================
-- モデル API 層 (描画と独立: predict / describe / coefficients)
--
-- 'Plottable' (図にする) とは別の細粒度 class (god class 回避、 Core.hs §2.3 方針)。
-- fit 済モデルを「数値として」 使う: 点予測・係数・要約。 Phase 16 §3 D。
-- ===========================================================================

-- | 係数 1 つの要約 (名前・推定値・標準誤差・95% Wald CI)。
data Coef = Coef
  { coefName  :: Text
  , coefValue :: Double
  , coefSE    :: Double
  , coefCI    :: (Double, Double)
  } deriving (Show, Eq)

-- | fit 済モデルを描画と独立に使う細粒度 API。
class ModelAPI m where
  -- | 係数ベクトル (intercept 含む)。
  modelCoefficients :: m -> [Double]
  -- | 単一説明変数 x での点予測 (μ スケール。 GLM は逆リンク後)。
  predictPoint      :: m -> Double -> Double
  -- | 各係数の要約 (推定値 + SE + 95% Wald CI)。
  describeModel     :: m -> [Coef]

-- | 係数共分散 Cov と β から要約を作る (SE = √diag, 95% CI = β ± 1.96·SE)。
coefSummaryFromCov :: LA.Matrix Double -> LA.Vector Double -> [Text] -> [Coef]
coefSummaryFromCov cov beta names =
  [ Coef nm b se (b - 1.96 * se, b + 1.96 * se)
  | (nm, b, se) <- zip3 names (LA.toList beta)
                       (map (sqrt . max 0) (LA.toList (LA.takeDiag cov))) ]

-- | LM の係数共分散 = σ̂²·(XᵀX)⁻¹  (σ̂² = RSS/(n−p))。
lmCoefCov :: LA.Matrix Double -> FitResult -> LA.Matrix Double
lmCoefCov x res =
  let r      = residualsV res
      n      = LA.rows x
      p      = LA.cols x
      sigma2 = (r LA.<.> r) / fromIntegral (max 1 (n - p))
  in LA.scale sigma2 (LA.inv (LA.tr x LA.<> x))

instance ModelAPI LMModel where
  modelCoefficients = LA.toList . coefficientsV . lmResult
  predictPoint m x  =
    let b = coefficientsV (lmResult m)
    in LA.atIndex b 0 + (if LA.size b > 1 then LA.atIndex b 1 * x else 0)
  describeModel m   =
    coefSummaryFromCov (lmCoefCov (lmDesign m) (lmResult m))
                       (coefficientsV (lmResult m)) ["(Intercept)", "x"]

instance ModelAPI GLMModel where
  modelCoefficients = LA.toList . coefficientsV . glmResult
  predictPoint m x  =
    let b            = coefficientsV (glmResult m)
        (_, gInv, _) = linkFnOf (glmLink m)
        eta          = LA.atIndex b 0 + (if LA.size b > 1 then LA.atIndex b 1 * x else 0)
    in gInv eta
  describeModel m   =
    coefSummaryFromCov (glmSigma m) (coefficientsV (glmResult m)) ["(Intercept)", "x"]

-- ===========================================================================
-- 統一係数サマリ (CoefRow / HasCoefSummary) — Phase 70.D
--
-- 'Coef' (推定値 + SE + 95% CI) では回帰の係数表に不足する **検定統計量 (t / z) と
-- p 値** を加えた 1 行型。 OLS 系 (LM / 重回帰 / WLS) は t 分布 (df = n − p)、
-- GLM / RLM は正規 (z) で推論する (= statsmodels @OLS.summary()@ は t、 @GLM@ /
-- @RLM@ は z。 ★statsmodels 突合済 Phase 70.D)。
-- ===========================================================================

-- | 統一係数サマリの 1 行。 検定統計量 'crStat' は OLS 系で t 値、 GLM\/RLM で z 値。
data CoefRow = CoefRow
  { crTerm     :: !Text              -- ^ 係数名 (@\"(Intercept)\"@ / 変数名)。
  , crEstimate :: !Double            -- ^ 点推定 β̂。
  , crStdErr   :: !Double            -- ^ 標準誤差 SE。
  , crStat     :: !Double            -- ^ Wald 統計量 β̂\/SE (t or z)。
  , crPValue   :: !Double            -- ^ 両側 p 値。
  , crCI95     :: !(Double, Double)  -- ^ 95% 信頼区間。
  } deriving (Show, Eq)

-- | fit 済モデルの統一係数サマリ。 @coefSummary model@ で全係数の表を得る。
class HasCoefSummary m where
  coefSummary :: m -> [CoefRow]

-- | OLS 経路の係数行 (t 分布・df = n − p)。 'lmCoefStats' (SE\/t\/p) を再利用し、
--   95% CI は @β̂ ± t_{0.975, df}·SE@。 WLS は √w スケール設計を渡せば正しい。
coefRowsLM :: [Text] -> LA.Matrix Double -> FitResult -> [CoefRow]
coefRowsLM names x res =
  let stats = lmCoefStats x res
      betas = LA.toList (coefficientsV res)
      df    = LA.rows x - LA.cols x
      tc    = ciTValue 0.95 df
  in [ CoefRow nm b (csSE s) (csTValue s) (csPValue s)
               (b - tc * csSE s, b + tc * csSE s)
     | (nm, b, s) <- zip3 names betas stats ]

-- | z 経路の係数行 (正規・GLM\/RLM)。 共分散 Cov から SE = √diag、 z = β̂\/SE、
--   両側 p = @2·(1 − Φ(|z|))@、 95% CI = @β̂ ± z_{0.975}·SE@。
coefRowsZ :: [Text] -> LA.Vector Double -> LA.Matrix Double -> [CoefRow]
coefRowsZ names beta cov =
  let ses = map (sqrt . max 0) (LA.toList (LA.takeDiag cov))
      zc  = quantileNormal 0.975
  in [ CoefRow nm b se z (2 * SD.complCumulative standard (abs z))
               (b - zc * se, b + zc * se)
     | (nm, b, se) <- zip3 names (LA.toList beta) ses
     , let z = if se == 0 then 0 else b / se ]

-- | 設計列数に合わせた係数名 (@\"(Intercept)\" : 変数名@)。 加法数値モデル
--   ('additiveFormula' \/ 単純 @y ~ x1 + x2@) では列数が @1 + |dvars|@ と一致するので
--   変数名をそのまま使う。 factor\/交互作用で列数が増える場合は総称名へフォールバック。
designCoefNames :: Int -> [Text] -> [Text]
designCoefNames p dvars
  | length dvars == p - 1 = "(Intercept)" : dvars
  | otherwise             = "(Intercept)" : [ "x" <> T.pack (show i) | i <- [1 .. p - 1] ]

instance HasCoefSummary LMModel where
  coefSummary m = coefRowsLM ["(Intercept)", "x"] (lmDesign m) (lmResult m)

instance HasCoefSummary MultiLMModel where
  coefSummary m =
    coefRowsLM (designCoefNames (LA.cols (mlmDesign m)) (formDataVars (mlmFormula m)))
               (mlmDesign m) (mlmResult m)

instance HasCoefSummary WeightedLMModel where
  coefSummary m =
    let inner = wlmInner m
    in coefRowsLM ["(Intercept)", "x"] (lmDesign inner) (lmResult inner)

instance HasCoefSummary GLMModel where
  coefSummary m =
    coefRowsZ ["(Intercept)", "x"] (coefficientsV (glmResult m)) (glmSigma m)

instance HasCoefSummary MultiGLMModel where
  coefSummary m =
    coefRowsZ (designCoefNames (LA.cols (mglmSigma m)) (formDataVars (mglmFormula m)))
              (coefficientsV (mglmResult m)) (mglmSigma m)

instance HasCoefSummary RobustModel where
  coefSummary m =
    let fit = rmFit m
        xd  = designMatrix (V.fromList (LA.toList (rmXraw m)))
        cov = robustCovBeta (rfEstimator fit) (rfScale fit) (rfResiduals fit) xd
    in coefRowsZ ["(Intercept)", "x"] (rfCoef fit) cov

instance HasCoefSummary MultiRobustModel where
  coefSummary m =
    let fit = mrmFit m
        cov = robustCovBeta (rfEstimator fit) (rfScale fit) (rfResiduals fit) (mrmDesign m)
    in coefRowsZ (designCoefNames (LA.cols (mrmDesign m)) (formDataVars (mrmFormula m)))
                 (rfCoef fit) cov

-- ===========================================================================
-- bootstrap 係数サマリ (HasCoefBoot) — Phase 72.1
--
-- 解析的 SE を持たないモデル (分位点回帰) や、 罰則化で解析 SE が定義し難い
-- モデル (Lasso / Ridge 等) に対し、 **case (行) bootstrap** で係数の
-- 不確実性を要約する。 返り値は 'coefSummary' と同型の '[CoefRow]' だが、
--
--   * 'crStdErr' = B 回再標本化した係数の標本 SD
--   * 'crStat'   = β̂ \/ SE_boot
--   * 'crPValue' = percentile 法の両側 p 値 (符号反転割合の 2 倍)
--   * 'crCI95'   = percentile 区間 (numpy.percentile type-7 線形補間)
--
-- seed 固定 + ST + MWC なので **純粋・再現可能**。
-- ===========================================================================

-- | bootstrap 係数サマリを持つモデル。 @coefSummaryBoot seed B model@ で
--   B 回の case bootstrap による係数表を得る。
class HasCoefBoot m where
  -- | @coefSummaryBoot seed B model@: 乱数 seed と replicate 回数 B からサマリ。
  coefSummaryBoot :: Word32 -> Int -> m -> [CoefRow]

-- | seed → B → n から、 B 個の「@n@ 個の @[0, n)@ 一様乱数 index リスト」 を作る。
--   case (行) bootstrap の再標本化 index。 ST + MWC で純粋・seed 固定で再現可能。
resampleRows :: Word32 -> Int -> Int -> [[Int]]
resampleRows seed b n
  | n <= 0 || b <= 0 = []
  | otherwise = runST $ do
      g <- initialize (V.singleton seed)
      replicateM b (replicateM n (uniformR (0, n - 1) g))

-- | numpy.percentile (type-7・線形補間) 互換。 @q@ は 0〜100。
percentileT7 :: [Double] -> Double -> Double
percentileT7 xs q =
  let sorted = sort xs
      n      = length sorted
  in case n of
       0 -> 0 / 0   -- 空は NaN (呼び元は非空を保証)
       1 -> head sorted
       _ -> let rank = q / 100 * fromIntegral (n - 1)
                lo   = floor rank :: Int
                hi   = min (n - 1) (lo + 1)
                frac = rank - fromIntegral lo
            in (sorted !! lo) * (1 - frac) + (sorted !! hi) * frac

clamp01 :: Double -> Double
clamp01 = max 0 . min 1

-- | (係数名, 点推定 β̂, B 個の replicate β) から係数表を作る。
--   各係数 j について SE = 標本 SD (n−1 除算・要素 1 未満は 0)、
--   p 値 = @clamp01 (2 · min(#{<0}\/B, #{>0}\/B))@、 CI = percentile [2.5, 97.5]。
bootCoefRows
  :: [Text]      -- ^ 係数名。
  -> [Double]    -- ^ 点推定 β̂ (長さ = 係数数)。
  -> [[Double]]  -- ^ B 個の replicate β (各長さ = 係数数)。
  -> [CoefRow]
bootCoefRows names point reps =
  [ let ests   = [ rep !! j | rep <- reps ]
        bN     = length ests
        meanE  = sum ests / fromIntegral (max 1 bN)
        var    = if bN < 2 then 0
                 else sum [ (e - meanE) ^ (2 :: Int) | e <- ests ]
                        / fromIntegral (bN - 1)
        se     = sqrt var
        stat   = if se == 0 then 0 else est / se
        ci     = if null ests then (est, est)
                 else (percentileT7 ests 2.5, percentileT7 ests 97.5)
        nNeg   = length (filter (< 0) ests)
        nPos   = length (filter (> 0) ests)
        pv     = if bN == 0 then 1
                 else clamp01 (2 * min (fromIntegral nNeg / fromIntegral bN)
                                       (fromIntegral nPos / fromIntegral bN))
    in CoefRow nm est se stat pv ci
  | (j, nm, est) <- zip3 [0 ..] names point ]

-- τ ラベルを @ (τ=0.50) @ 形式で作る。
tauLabel :: Double -> Text
tauLabel tau = " (τ=" <> T.pack (showFFloat (Just 2) tau "") <> ")"

-- QuantileModel 系の共通 bootstrap: 設計行列 X (intercept 列込み)・応答 y・
-- τ リスト・係数名 (intercept 込み) から、 τ ごとに行 (係数 × τ) を並べた表。
quantileBootRows
  :: Word32 -> Int
  -> LA.Matrix Double          -- ^ 設計行列 X (= [1, x..])。
  -> LA.Vector Double          -- ^ 応答 y。
  -> [Double]                  -- ^ τ リスト。
  -> [(Double, LA.Vector Double)]  -- ^ (τ, 点推定 qfBeta)。
  -> [Text]                    -- ^ 係数名 (intercept 込み)。
  -> [CoefRow]
quantileBootRows seed b xMat y taus pointBetas names =
  let n        = LA.rows xMat
      idxSets  = resampleRows seed b n
      -- 各 replicate で τ ごとの β を計算: replBetas !! r !! tIdx = [β...]
      replBetas =
        [ let xr = xMat LA.? idxs
              yr = LA.fromList [ y `LA.atIndex` i | i <- idxs ]
          in [ LA.toList (qfBeta (fitQuantile t xr yr)) | t <- taus ]
        | idxs <- idxSets ]
  in concat
       [ let repsForTau = [ rb !! tIdx | rb <- replBetas ]
             pt         = LA.toList beta
             namesTau   = [ nm <> tauLabel tau | nm <- names ]
         in bootCoefRows namesTau pt repsForTau
       | (tIdx, (tau, beta)) <- zip [0 ..] pointBetas ]

instance HasCoefBoot QuantileModel where
  coefSummaryBoot seed b m =
    let taus  = map fst (qmFits m)
        x     = qmXraw m
        xMat  = designMatrix (V.fromList (LA.toList x))
        -- y を head fit の qfYHat + qfResid から復元。
        (_, fit0) = head (qmFits m)
        y     = qfYHat fit0 + qfResid fit0
        pts   = [ (t, qfBeta f) | (t, f) <- qmFits m ]
    in quantileBootRows seed b xMat y taus pts ["(Intercept)", "x"]

instance HasCoefBoot MultiQuantileModel where
  coefSummaryBoot seed b m =
    let taus  = mqmTaus m
        xMat  = mqmX m
        (_, fit0) = head (mqmFits m)
        y     = qfYHat fit0 + qfResid fit0
        pts   = [ (t, qfBeta f) | (t, f) <- mqmFits m ]
        names = "(Intercept)" : mqmNames m
    in quantileBootRows seed b xMat y taus pts names

-- ===========================================================================
-- 平滑項単位の近似有意性 (TermRow / HasTermSummary) — Phase 72.2
--
-- 回帰の係数表 ('CoefRow') が「基底係数 1 つ 1 つ」 を並べるのに対し、 平滑項
-- (GAM / spline の @s(x)@) は **項全体** の有意性を見たい。 mgcv @summary.gam@
-- は項ごとに **有効自由度 edf** と **近似 F 検定** (Wood 2013 の rank-r 擬似逆
-- Wald 統計量) を報告する。 本セクションはそれに倣う。
--
--   * GAM (罰則付き ridge) は **近似**: 項 j の edf = sub-trace、 F は
--     rank-r 擬似逆 Wald。 r = round(edf_j) を 1..m_j にクランプ。
--   * Spline (罰則なし OLS) は **厳密** nested F (曲線 vs 定数)。
-- ===========================================================================

-- | 平滑項 1 つの近似有意性。 'teEdf' は有効自由度、 'teStat' は近似 F、
--   'tePValue' は上側 @F(r, dfRes)@ の確率。
--   注: フィールド prefix は @te@ (term)。 @tr@ は 'Hanalyze.Stat.Test'
--   の @TestResult@ が占有しており、 plot umbrella での再 export 衝突を避けるため。
data TermRow = TermRow
  { teTerm   :: !Text    -- ^ 平滑項名 (@\"s(x)\"@ / @\"s(<name>)\"@)。
  , teEdf    :: !Double  -- ^ 有効自由度 edf。
  , teStat   :: !Double  -- ^ 近似 F 統計量。
  , tePValue :: !Double  -- ^ 上側確率 (近似 p 値)。
  } deriving (Show, Eq)

-- | fit 済の平滑モデルの「項単位」 近似有意性。 @termSummary model@ で各平滑項の
--   edf + 近似 F + p 値の表を得る。
class HasTermSummary m where
  termSummary :: m -> [TermRow]

-- | GAM の項単位サマリ (mgcv 流 edf + rank-r 擬似逆 Wald 近似 F)。
--   名前は呼び出し側が与える (@length = length gamBetas@)。
--
--   設計列レイアウト: intercept=0、 項 j は @starts!!j .. starts!!j+mSizes!!j-1@。
--   ★基底の再評価は不要 — 'GAMFit' に格納済の @gamBetas \/ gamCov \/ gamEdf \/
--   gamLambda \/ gamResid@ のみから算出する。
gamTermRows :: GAMFit -> [Text] -> [TermRow]
gamTermRows fit names =
  let resid  = gamResid fit
      n      = LA.size resid
      rss    = resid LA.<.> resid
      dfRes  = fromIntegral n - gamEdf fit
      phi    = if dfRes > 1e-9 then rss / dfRes else rss
      cov    = gamCov fit                     -- Vβ = (XᵀX+λP)⁻¹·φ̂
      p      = LA.rows cov
      lhsInv = LA.scale (1 / phi) cov         -- (XᵀX+λP)⁻¹
      lhs    = LA.scale phi (LA.inv cov)      -- XᵀX+λP
      pen    = LA.diag (LA.fromList (0 : replicate (p - 1) (gamLambda fit)))
      xtx    = lhs - pen                       -- XᵀX
      fMat   = lhsInv LA.<> xtx                -- edf 行列 (XᵀX+λP)⁻¹ XᵀX
      fDiag  = LA.toList (LA.takeDiag fMat)
      betas  = gamBetas fit
      mSizes = map LA.size betas
      starts = scanl (+) 1 mSizes              -- intercept は index 0
      dfResI = max 1 (round dfRes) :: Int
  in [ termRowFromBlock nm (betas !! j) vBlock edfJ dfResI
     | (j, nm) <- zip [0 ..] names
     , let cols  = [ starts !! j .. starts !! j + mSizes !! j - 1 ]
           edfJ  = sum [ fDiag !! k | k <- cols ]
           vBlock = (cov LA.¿ cols) LA.? cols ]   -- cols×cols 共分散ブロック

-- | 1 項の rank-r 擬似逆 Wald 統計量から 'TermRow' を作る (mgcv Wood 2013 流)。
--   r = clamp (round edf) 1 (dim β_j)。 V_j を対称固有分解し上位 r 固有対で
--   Vr⁻ = Σ_{i≤r} (u_i u_iᵀ)/λ_i。 Tr = β_jᵀ Vr⁻ β_j、 F = Tr / r。
termRowFromBlock :: Text -> LA.Vector Double -> LA.Matrix Double -> Double -> Int -> TermRow
termRowFromBlock nm beta vBlock edf dfResI =
  let mj   = LA.size beta
      r    = max 1 (min mj (round edf))
      (evals, evecs) = LA.eigSH (LA.trustSym vBlock)  -- 降順固有値・列が固有ベクトル
      evalL = LA.toList evals
      cols  = LA.toColumns evecs
      -- 上位 r 固有対で擬似逆 Vr⁻ = Σ (u uᵀ)/λ  (λ≤0 は除外)
      vrInv = sum [ LA.scale (1 / lam) (LA.outer u u)
                  | (i, (lam, u)) <- zip [0 :: Int ..] (zip evalL cols)
                  , i < r, lam > 1e-12 ]
      vrInvM = if null [ () | (i, lam) <- zip [0 ..] evalL, i < r, lam > 1e-12 ]
                 then LA.konst 0 (mj, mj)
                 else vrInv
      tr    = beta LA.<.> (vrInvM LA.#> beta)
      fstat = if r > 0 then tr / fromIntegral r else 0
      pv    = SD.complCumulative (FD.fDistribution r dfResI) fstat
  in TermRow nm edf fstat pv

instance HasTermSummary GAMModel where
  -- gamModel は名前を持たないので "s(x)" 固定 (項は 1 つ)。
  termSummary m = gamTermRows (gamFit m) ["s(x)"]

instance HasTermSummary GAMModelN where
  termSummary m =
    gamTermRows (gamNFit m) (map (\nm -> "s(" <> nm <> ")") (gamNNames m))

instance HasTermSummary SplineModel where
  -- 罰則なし OLS ゆえ厳密 nested F (spline 曲線 vs 定数)。 基底は intercept 列を
  -- 含む (B-spline は partition-of-unity で定数を張る・自然3次は明示の "1" 列)
  -- ので、 定数 null モデルは 1 パラメータ。 df_num = p1−1、 dfRes = n−p1。
  termSummary m =
    let fit   = splFit m
        res   = sfResult fit
        resid = residualsV res
        yhat  = fittedV res
        y     = yhat + resid            -- 観測値の復元 (y = ŷ + r)
        n     = LA.size resid
        rss1  = resid LA.<.> resid
        yMean = LA.sumElements y / fromIntegral (max 1 n)
        rss0  = LA.sumElements (LA.cmap (\v -> (v - yMean) ^ (2 :: Int)) y)
        p1    = LA.size (sfBeta fit)
        dfNum = max 1 (p1 - 1)
        dfRes = max 1 (n - p1)
        fstat = if rss1 <= 1e-300 || dfRes <= 0 then 0
                else ((rss0 - rss1) / fromIntegral dfNum)
                       / (rss1 / fromIntegral dfRes)
        pv    = SD.complCumulative (FD.fDistribution dfNum dfRes) fstat
    in [ TermRow "s(x)" (fromIntegral dfNum) fstat pv ]

-- ===========================================================================
-- 統一玄関 (.summary() 風) — Phase 72.3
--
-- 既存の 'coefSummary' (Wald・'HasCoefSummary')・'coefSummaryBoot' (bootstrap・
-- 'HasCoefBoot')・'termSummary' (項有意性・'HasTermSummary') を **1 つのタグ付き
-- 直和** 'ModelReport' でラップし、 モデル型ごとに適切な診断へディスパッチする
-- 玄関。 statsmodels の @.summary()@ に相当する「とりあえずこれを呼べば要約が出る」
-- 入口を提供する (どの診断が該当するかをモデル型側が知っている)。
-- ===========================================================================

-- | モデル要約レポート。 係数表 (Wald も bootstrap も同じ箱) / 平滑項有意性 /
--   該当なし (理由文付き) のタグ付き直和。
data ModelReport
  = CoefReport [CoefRow]   -- ^ 係数表 (Wald 'coefSummary' か bootstrap 'coefSummaryBoot')。
  | TermReport [TermRow]   -- ^ GAM\/spline の平滑項有意性 'termSummary'。
  | NoReport   Text        -- ^ 係数診断が非該当 (理由文)。
  deriving (Show, Eq)

-- | fit 済モデルの統一要約玄関。 @modelReport model@ で型に応じた 'ModelReport' を得る。
class HasReport m where
  modelReport :: m -> ModelReport

-- 4 桁固定の数値整形 (符号付き)。
fmt4 :: Double -> String
fmt4 x = showFFloat (Just 4) x ""

-- 右詰めパディング (width 12)。
padR :: Int -> String -> Text
padR w s = T.pack (replicate (max 0 (w - length s)) ' ' <> s)

-- 左詰めパディング (width w)。
padL :: Int -> String -> Text
padL w s = T.pack (s <> replicate (max 0 (w - length s)) ' ')

-- | 'ModelReport' を @.summary()@ 風のテキスト表へ整形する。
--
--   * 'CoefReport': ヘッダ @term \/ estimate \/ std.err \/ stat \/ p.value \/ [2.5%, 97.5%]@
--     + 各 'CoefRow' を固定幅で整列 (4 桁)。
--   * 'TermReport': ヘッダ @term \/ edf \/ F \/ p.value@ + 各 'TermRow'。
--   * 'NoReport': 理由文をそのまま返す。
showReport :: ModelReport -> Text
showReport (NoReport msg) = msg
showReport (CoefReport rows) =
  T.unlines (header : map row rows)
  where
    header = padL 20 "term" <> padR 12 "estimate" <> padR 12 "std.err"
               <> padR 12 "stat" <> padR 12 "p.value" <> "   " <> "[2.5%, 97.5%]"
    row (CoefRow tm est se st pv (lo, hi)) =
      padL 20 (T.unpack tm) <> padR 12 (fmt4 est) <> padR 12 (fmt4 se)
        <> padR 12 (fmt4 st) <> padR 12 (fmt4 pv)
        <> "   [" <> T.pack (fmt4 lo) <> ", " <> T.pack (fmt4 hi) <> "]"
showReport (TermReport rows) =
  T.unlines (header : map row rows)
  where
    header = padL 20 "term" <> padR 12 "edf" <> padR 12 "F" <> padR 12 "p.value"
    row (TermRow tm edf st pv) =
      padL 20 (T.unpack tm) <> padR 12 (fmt4 edf) <> padR 12 (fmt4 st)
        <> padR 12 (fmt4 pv)

-- --- ディスパッチ instance ---------------------------------------------------

-- Wald 係数表 (HasCoefSummary 経由)
instance HasReport LMModel        where modelReport m = CoefReport (coefSummary m)
instance HasReport MultiLMModel   where modelReport m = CoefReport (coefSummary m)
instance HasReport WeightedLMModel where modelReport m = CoefReport (coefSummary m)
instance HasReport GLMModel       where modelReport m = CoefReport (coefSummary m)
instance HasReport MultiGLMModel  where modelReport m = CoefReport (coefSummary m)
instance HasReport RobustModel    where modelReport m = CoefReport (coefSummary m)
instance HasReport MultiRobustModel where modelReport m = CoefReport (coefSummary m)

-- bootstrap 係数表 (HasCoefBoot 経由・既定 seed=42 / B=2000)
instance HasReport QuantileModel where
  modelReport m = CoefReport (coefSummaryBoot 42 2000 m)
instance HasReport MultiQuantileModel where
  modelReport m = CoefReport (coefSummaryBoot 42 2000 m)

-- 平滑項有意性 (HasTermSummary 経由)
instance HasReport GAMModel    where modelReport m = TermReport (termSummary m)
instance HasReport GAMModelN   where modelReport m = TermReport (termSummary m)
instance HasReport SplineModel where modelReport m = TermReport (termSummary m)

-- GP 系: 線形係数を持たない → 非該当
gpNoReport :: ModelReport
gpNoReport = NoReport
  "ガウス過程は線形係数を持たない (ハイパーパラメータのみ)。係数診断は非該当。"

instance HasReport GPResult     where modelReport _ = gpNoReport
instance HasReport GPRegModel   where modelReport _ = gpNoReport
instance HasReport GPRegModelN  where modelReport _ = gpNoReport
