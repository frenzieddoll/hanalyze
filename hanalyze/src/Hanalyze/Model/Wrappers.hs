{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Hanalyze.Model.Wrappers
-- Description : hgg に依存しないフィット済みモデルの描画ラッパ型と smart constructor
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: 描画ラッパ型 + smart constructor の plot 非依存層。
--
-- 別パッケージ @hanalyze-plot@ の 'Hanalyze.Plot'
-- (@cabal build --project-file=cabal.project.plot@ で build) が描画
-- (@VisualSpec@ 化・@Plottable@ instance) を担うのに対し、 本モジュールは
-- そこから __hgg に依存しない__ 部分 (フィット済みモデルを束ねた
-- 描画ラッパ型と、 それを組み立てる smart constructor) のみを切り出したもの。
-- 非ゲート (常時 build) なので 'Graphics.Hgg.*' を一切 import しない。
-- 'Hanalyze.Plot' は本モジュールを import し従来の名前で再 export する。
--
-- [English]: Plot-independent layer of rendering wrapper types + smart
-- constructors.
--
-- Whereas 'Hanalyze.Plot' (in the separate @hanalyze-plot@
-- package, built via @cabal build --project-file=cabal.project.plot@)
-- handles rendering (turning results into a @VisualSpec@, the @Plottable@
-- instances), this module carves out only the __hgg-independent__
-- part: the rendering wrapper types that bundle a fitted model,
-- and the smart constructors that assemble them. It's ungated (always
-- built), so it never imports 'Graphics.Hgg.*'. 'Hanalyze.Plot'
-- imports this module and re-exports under the traditional names.
module Hanalyze.Model.Wrappers
  ( -- * 多変量 effect plot のラッパ + smart ctor
    AlongSpec (..)
  , along
  , HoldAgg (..)
  , MultiLMModel (..)
  , multiLMModel
  , multiLMModelF
  , MultiGLMModel (..)
  , multiGLMModel
  , multiGLMModelF
  , MultiRobustModel (..)
  , multiRobustModelF
  , additiveFormula
  , PLSModel (..)
  , plsModel
  , selectOutput
    -- * 帯モード / 予測区間セレクタ
  , BandMode (..)
  , PIMethod (..)
    -- * 応答曲面オプション
  , SurfaceOpts (..)
  , defaultSurfaceOpts
    -- * 単回帰系の描画ラッパ + smart ctor
  , LMModel (..)
  , lmModel
  , GLMModel (..)
  , glmModel
  , SplineModel (..)
  , splineModel
  , GAMModel (..)
  , gamModel
  , GAMModelN (..)
  , RobustModel (..)
  , robustModel
  , QuantileModel (..)
  , quantileModel
  , MultiQuantileModel (..)
    -- * MCMC チェーン ラッパ
  , ChainModel (..)
  , chainModel
    -- * HBM 学習
  , HBMConfig (..)
  , defaultHBM
  , HBMModel (..)
  , bindCols
  , bindIxCols
  , hbmModel
  , hbmInitPoint
  , hbmModelPure
  , hbmModelPureWith
  , hbmModelIO
  , hbmModelIOWith
    -- * HBM 事後要約 (Phase 103)
  , hbmSummaryNames
  , hbmSummary
  , printHBMSummary
  , hbmSummaryDf
  , hbmDrawsDf
    -- * 時系列予測 ラッパ
  , ForecastModel (..)
  , forecastModel
    -- * 統一 fit API
  , Fit (..)
  , LiNGAMFitted (..)
  , reqColV
  , reqColsM
    -- * WLS / 標準化 / 群別フィット ラッパ
  , WeightedLMModel (..)
  , StandardizedModel (..)
  , GroupedFit (..)
    -- * カーネル回帰 ラッパ
  , GPMethod (..)
  , HyperStrategy (..)
  , GPRegModel (..)
  , GPRegModelN (..)
    -- * 罰則回帰 ラッパ
  , RegMethod (..)
  , RegModel (..)
  , regPredict
  ) where

import           Data.Maybe            (fromMaybe)
import qualified Data.Map.Strict       as Map
import           Data.Word             (Word32)
import qualified Data.Vector           as V
import           System.Random.MWC     (createSystemRandom, initialize)

import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified DataFrame.Internal.Column    as DX
import qualified DataFrame.Internal.DataFrame  as DX
import qualified Numeric.LinearAlgebra as LA

import           Hanalyze.Data.ColumnSource     (ColumnSource (..))
import           Hanalyze.Model.Formula         (Formula (..), Term (..), BinOp (..))
import           Hanalyze.Model.Formula.Frame   (ModelFrame (..), VarRole (..),
                                                 modelFrame)
import           Hanalyze.Model.Formula.Design  (designMatrixF, responseVec,
                                                 fitLMF)
import           Hanalyze.Model.Formula.RFormula (parseModel)

import           Hanalyze.Model.Core   (FitResult, coefficientsV)
import           Hanalyze.Model.GLM    ( Family (..), LinkFn (..)
                                       , fitGLMFull )
import           Hanalyze.Model.GP     (Kernel (..), GPParams (..))
import           Hanalyze.Model.LM     ( designMatrix, fitLMVec )
import           Hanalyze.Model.Spline ( SplineKind (..), SplineFit (..)
                                       , fitSpline )
import           Hanalyze.Model.GAM    (GAMFit (..), fitGAM)
import           Hanalyze.Model.Robust ( RobustEstimator, RobustFit (..)
                                       , fitRobustLM )
import           Hanalyze.Model.MultiLM (MultiFit (..))
import           Hanalyze.Model.Quantile (QRFit (..), fitQuantile)
import           Hanalyze.MCMC.Core     (Chain (..), chainVals)
import           Hanalyze.MCMC.NUTS     ( nutsChains, nutsChainsPure, nutsChainsStream
                                       , NUTSConfig (..), defaultNUTSConfig )
import           Hanalyze.MCMC.Progress (newProgressRenderer)
import           Hanalyze.Model.HBM     ( ModelP, withData, withDataIx, getTransforms
                                               , sampleNames, deterministicNames
                                               , augmentChainWithDeterministic )
import           Hanalyze.Stat.Summary  (SummaryRow (..), posteriorSummary)
import           Hanalyze.Viz.MCMC      (printPosteriorSummary)
import           Hanalyze.Stat.Distribution (Transform (..))
import           Hanalyze.Model.TimeSeries (ARFit (..), fitAR)
import           Hanalyze.Stat.Standardize (Standardizer (..))
import           Hanalyze.Model.PLS (PLSFit (..), fitPLS, predictPLS, PLSConfig (..))
import           Hanalyze.Model.Regularized (RegFit (..))

-- ===========================================================================
-- 多変量 effect plot の along / HoldAgg
-- ===========================================================================

-- | [日本語]: 多変量 effect plot で「動かす変数」 (along)。 @statModelMulti@ の必須引数。
--   型で単/多変量を分離し along 忘れをコンパイル時に弾く (§3 確定設計)。
--   [English]: The "variable to vary" (along) in a multivariate effect plot;
--   a required argument of @statModelMulti@. Separating single\/multivariate
--   by type catches a forgotten along at compile time (confirmed design in
--   §3).
newtype AlongSpec = AlongSpec Text

-- | [日本語]: along 変数を指定する。 @statModelMulti m (along \"x1\")@。
--   [English]: Specifies the along variable. @statModelMulti m (along \"x1\")@.
along :: Text -> AlongSpec
along = AlongSpec

-- | [日本語]: 多変量 effect で along 以外の説明変数をどう固定するか (既定 'Mean')。
--
--   * 'Mean' \/ 'Median' = 連続変数の集約 (factor 列は自動で 'Mode' に振替)。
--   * 'Mode' = 最頻 (連続は丸め最頻、 factor は最頻水準)。
--   * 'Reference' = factor の参照水準 (昇順先頭。 連続は 'Mean' に振替)。
--   * 'Marginalize' = 固定せず観測分布で周辺化 (PDP\/AME。 全観測行 × grid で重く、
--     band は提供しない = 曲線のみ)。
--   * 'Fixed' = 明示指定 (部分指定可。 指定の無い変数は 'Mean')。
--   [English]: How to hold the explanatory variables other than along fixed
--   in a multivariate effect (default 'Mean').
--
--   * 'Mean' \/ 'Median' = aggregation of continuous variables (factor
--     columns automatically switch to 'Mode').
--   * 'Mode' = most frequent (rounded mode for continuous, most frequent
--     level for factors).
--   * 'Reference' = the factor's reference level (first in ascending order;
--     continuous switches to 'Mean').
--   * 'Marginalize' = not fixed, marginalized over the observed distribution
--     (PDP\/AME; heavy — all observed rows × grid — and provides no band,
--     curve only).
--   * 'Fixed' = explicit values (partial specification allowed; unspecified
--     variables use 'Mean').
data HoldAgg
  = Mean
  | Median
  | Mode
  | Reference
  | Marginalize
  | Fixed [(Text, Double)]
  deriving (Eq, Show)

-- ===========================================================================
-- 帯モード / 予測区間セレクタ
-- ===========================================================================

-- | [日本語]: 帯モードセレクタ。 出す帯を 1 つの値で選ぶ (@bandMode@ で指定):
--
--   * @BandOff@  = 帯なし (曲線のみ)。
--   * @BandCI@   = 信頼区間のみ (平均 E[y|x] の不確実性・__既定__)。
--   * @BandPI@   = 予測区間のみ (新規観測 1 点が入る区間・σ̂² を含むぶん広い)。
--   * @BandCIPI@ = CI + PI を入れ子で重ねる (外=PI 薄・内=CI 濃・ファンチャート)。
--
-- PI 非提供モデル (Robust\/GAM\/Quantile\/非 Gaussian GLM\/effect plot) では
-- @BandPI@\/@BandCIPI@ は CI へフォールバックする (@svGridPI@ = 'Nothing')。
--   [English]: Band mode selector. Choose which band to draw with a single
--   value (specified via @bandMode@):
--
--   * @BandOff@  = no band (curve only).
--   * @BandCI@   = confidence interval only (uncertainty of the mean
--     E[y|x] — __default__).
--   * @BandPI@   = prediction interval only (interval containing one new
--     observation; wider since it includes σ̂²).
--   * @BandCIPI@ = CI + PI overlaid nested (outer = PI, faint; inner = CI,
--     bold; a "fan chart").
--
-- For models that don't provide a PI (Robust\/GAM\/Quantile\/non-Gaussian
-- GLM\/effect plot), @BandPI@\/@BandCIPI@ fall back to the CI
-- (@svGridPI@ = 'Nothing').
data BandMode = BandOff | BandCI | BandPI | BandCIPI
  deriving (Eq, Show)

-- | [日本語]: 帯の算出法 (@piMethod@ で指定)。 @PIClosedForm@ = 閉形式 (既定)、
--   @PIBootstrap seed draws@ = case-resampling ブートストラップ (seed 決定的・draws 回)。
--   [English]: How the band is computed (specified via @piMethod@).
--   @PIClosedForm@ = closed form (default); @PIBootstrap seed draws@ =
--   case-resampling bootstrap (deterministic seed, draws repetitions).
data PIMethod = PIClosedForm | PIBootstrap !Word32 !Int
  deriving (Eq, Show)

-- ===========================================================================
-- 応答曲面オプション
-- ===========================================================================

-- | [日本語]: @surfaceOf@ / @surfaceGrid@ のオプション。
--   [English]: Options for @surfaceOf@ \/ @surfaceGrid@.
data SurfaceOpts = SurfaceOpts
  { soN      :: Int                     -- ^ [日本語]: 各軸の grid 点数 (既定 40)。 [English]: Grid points per axis (default 40).
  , soHoldAt :: HoldAgg                 -- ^ [日本語]: 他変数の固定方式 (既定 'Mean')。 [English]: How other variables are held fixed (default 'Mean').
  , soXRange :: Maybe (Double, Double)  -- ^ [日本語]: v1 範囲 (既定 = 観測 min\/max)。 [English]: Range of v1 (default = observed min\/max).
  , soYRange :: Maybe (Double, Double)  -- ^ [日本語]: v2 範囲 (既定 = 観測 min\/max)。 [English]: Range of v2 (default = observed min\/max).
  }

defaultSurfaceOpts :: SurfaceOpts
defaultSurfaceOpts = SurfaceOpts
  { soN = 40, soHoldAt = Mean, soXRange = Nothing, soYRange = Nothing }

-- ===========================================================================
-- 多変量モデル型 (effect plot 用、 新規 fit)
-- ===========================================================================

-- | [日本語]: formula + @DataFrame@ で fit した多変量線形モデル (effect plot 用)。
--   [English]: A multivariate linear model fit with a formula + @DataFrame@
--   (for effect plots).
data MultiLMModel = MultiLMModel
  { mlmFormula :: Formula           -- ^ [日本語]: R\/独自 formula (評価点設計行列の組み立てに保持)。 [English]: R-style\/custom formula (kept for building the evaluation-point design matrix).
  , mlmFrame   :: ModelFrame        -- ^ [日本語]: 訓練 frame (along range + 他変数集約元)。 [English]: Training frame (source of the along range + aggregation of other variables).
  , mlmDesign  :: LA.Matrix Double  -- ^ [日本語]: 訓練設計行列 X (@confidenceBandAt@ の分散核)。 [English]: Training design matrix X (variance kernel for @confidenceBandAt@).
  , mlmResult  :: FitResult         -- ^ [日本語]: OLS 結果。 [English]: OLS result.
  }

-- | [日本語]: formula 文字列 (例 @\"y ~ x1 + x2 + x3\"@) と @DataFrame@ から多変量 LM を組む。
--   [English]: Builds a multivariate LM from a formula string (e.g.
--   @\"y ~ x1 + x2 + x3\"@) and a @DataFrame@.
multiLMModel :: Text -> DX.DataFrame -> Either String MultiLMModel
multiLMModel fml df = parseModel fml >>= \f -> multiLMModelF f df

-- | [日本語]: 既に組み上げた 'Formula' (parse 済 or 'additiveFormula' で直接合成) から多変量 LM を
--   組む。 重回帰 spec (@lmMulti@) が parse を経ずに使う経路。
--   [English]: Builds a multivariate LM from an already-assembled 'Formula'
--   (either parsed or directly composed via 'additiveFormula'). The path
--   used by the multiple-regression spec (@lmMulti@) that skips parsing.
multiLMModelF :: Formula -> DX.DataFrame -> Either String MultiLMModel
multiLMModelF f df = do
  mf         <- modelFrame f df
  (x, _)     <- designMatrixF f mf
  (res, _)   <- fitLMF f df
  Right MultiLMModel { mlmFormula = f, mlmFrame = mf, mlmDesign = x, mlmResult = res }

-- | [日本語]: formula + @DataFrame@ で fit した多変量 GLM (effect plot 用)。 帯は μ スケールで非対称。
--   [English]: A multivariate GLM fit with a formula + @DataFrame@ (for
--   effect plots). The band is asymmetric on the μ scale.
data MultiGLMModel = MultiGLMModel
  { mglmFormula :: Formula           -- ^ [日本語]: formula (評価点設計行列に保持)。 [English]: Formula (kept for the evaluation-point design matrix).
  , mglmFrame   :: ModelFrame        -- ^ [日本語]: 訓練 frame。 [English]: Training frame.
  , mglmResult  :: FitResult         -- ^ [日本語]: 'fitGLMFull' の結果 (β\/μ̂)。 [English]: Result of 'fitGLMFull' (β\/μ̂).
  , mglmSigma   :: LA.Matrix Double  -- ^ [日本語]: 逆 Fisher 情報 Σ (CI 用)。 [English]: Inverse Fisher information Σ (for the CI).
  , mglmFamily  :: Family            -- ^ [日本語]: 分布族。 [English]: Distribution family.
  , mglmLink    :: LinkFn            -- ^ [日本語]: リンク関数。 [English]: Link function.
  }

-- | [日本語]: family\/link + formula 文字列 + @DataFrame@ から多変量 GLM を組む。
--   [English]: Builds a multivariate GLM from a family\/link + formula
--   string + @DataFrame@.
multiGLMModel :: Family -> LinkFn -> Text -> DX.DataFrame -> Either String MultiGLMModel
multiGLMModel family link fml df =
  parseModel fml >>= \f -> multiGLMModelF family link f df

-- | [日本語]: 既に組み上げた 'Formula' から多変量 GLM を組む (重回帰 spec @glmMulti@ 用)。
--   [English]: Builds a multivariate GLM from an already-assembled 'Formula'
--   (for the multiple-regression spec @glmMulti@).
multiGLMModelF :: Family -> LinkFn -> Formula -> DX.DataFrame -> Either String MultiGLMModel
multiGLMModelF family link f df = do
  mf     <- modelFrame f df
  (x, _) <- designMatrixF f mf
  yv     <- responseVec mf
  let y            = LA.fromList (V.toList yv)
      (res, sigma) = fitGLMFull family link x y
  Right MultiGLMModel { mglmFormula = f, mglmFrame = mf, mglmResult = res
                      , mglmSigma = sigma, mglmFamily = family, mglmLink = link }

-- ===========================================================================
-- 列名リスト → 加法線形 Formula AST (パース無し直接合成) — Phase 70.D
--
-- 重回帰 (multiple regression) は formula DSL とは別概念: 説明変数の列名リストから
-- 設計行列 @[1, x1, …, xp]@ を作るだけ。 これを文字列を介さず 'Formula' AST に直接
-- 組み立て、 既存の 'multiLMModelF' / 'designMatrixF' / effect plot 機構をそのまま使う
-- (= @parseModel "y ~ x1 + … + xp"@ と同一 AST。 パラメータ名 @_p0.._pp@ も同じ規約)。
-- ===========================================================================

-- | [日本語]: 応答列 @y@ と説明変数列名 @[x1,…,xp]@ から加法線形 'Formula' を直接合成する。
--   RHS = @_p0 + _p1*x1 + … + _pp*xp@ (切片 + 各変数の主効果)。 数値列前提
--   (factor / 交互作用 / 変換が要るなら formula 版 @lmF@ を使う)。
--   [English]: Directly composes an additive linear 'Formula' from a
--   response column @y@ and explanatory variable column names
--   @[x1,…,xp]@. RHS = @_p0 + _p1*x1 + … + _pp*xp@ (intercept + main effect
--   of each variable). Assumes numeric columns (use the formula version
--   @lmF@ if factors \/ interactions \/ transforms are needed).
additiveFormula :: Text -> [Text] -> Formula
additiveFormula y xs = Formula
  { formResponse = y
  , formDataVars = xs
  , formRHS      = foldl1 (Bin Add)
      (Ref "_p0" : zipWith (\i x -> Bin Mul (Ref ("_p" <> tshowInt i)) (Ref x))
                           [1 :: Int ..] xs) }
  where tshowInt = T.pack . show

-- | [日本語]: formula + @DataFrame@ で fit した多変量ロバスト回帰 (effect plot 用)。
--   [English]: A multivariate robust regression fit with a formula +
--   @DataFrame@ (for effect plots).
data MultiRobustModel = MultiRobustModel
  { mrmEstimator :: RobustEstimator   -- ^ [日本語]: Huber k or Tukey c。 [English]: Huber k or Tukey c.
  , mrmFormula   :: Formula           -- ^ [日本語]: 評価点設計行列の組み立てに保持。 [English]: Kept for building the evaluation-point design matrix.
  , mrmFrame     :: ModelFrame        -- ^ [日本語]: 訓練 frame (along range + 他変数集約元)。 [English]: Training frame (source of the along range + aggregation of other variables).
  , mrmDesign    :: LA.Matrix Double  -- ^ [日本語]: 訓練設計行列 X (サンドイッチ共分散の核)。 [English]: Training design matrix X (kernel of the sandwich covariance).
  , mrmFit       :: RobustFit         -- ^ [日本語]: 'fitRobustLM' の結果 (β̂ / 重み / スケール)。 [English]: Result of 'fitRobustLM' (β̂ \/ weights \/ scale).
  }

-- | [日本語]: 'Formula' + @DataFrame@ から多変量ロバスト回帰を組む (重回帰 spec @robustMulti@ 用)。
--   [English]: Builds a multivariate robust regression from a 'Formula' +
--   @DataFrame@ (for the multiple-regression spec @robustMulti@).
multiRobustModelF :: RobustEstimator -> Formula -> DX.DataFrame
                  -> Either String MultiRobustModel
multiRobustModelF est f df = do
  mf     <- modelFrame f df
  (x, _) <- designMatrixF f mf
  yv     <- responseVec mf
  let y   = LA.fromList (V.toList yv)
      fit = fitRobustLM est x y 50 1e-6
  Right MultiRobustModel { mrmEstimator = est, mrmFormula = f, mrmFrame = mf
                         , mrmDesign = x, mrmFit = fit }

-- | [日本語]: PLS の effect plot 用ラッパ。 'PLSFit' は列名/'ModelFrame' を
-- 持たないので、 @statModelMulti@ (along/holdAt/byVar) を効かせるために訓練 frame と
-- 列順・出力選択を保持する。 'MultiLMModel' と同型 (frame-carrying wrapper)。
--   [English]: Effect-plot wrapper for PLS. Since 'PLSFit' doesn't carry
--   column names\/a 'ModelFrame', this holds the training frame, column
--   order, and output selection so that @statModelMulti@
--   (along\/holdAt\/byVar) can be used. Same shape as 'MultiLMModel' (a
--   frame-carrying wrapper).
data PLSModel = PLSModel
  { plsmFit    :: !PLSFit       -- ^ [日本語]: 学習済 PLS。 [English]: The trained PLS.
  , plsmFrame  :: !ModelFrame   -- ^ [日本語]: 訓練 frame (X 列 = 'RoleContinuous'・along range/hold の元)。 [English]: Training frame (X columns = 'RoleContinuous'; source of the along range\/hold).
  , plsmXNames :: ![Text]       -- ^ [日本語]: X 列名 ('predictPLS' へ渡す列順)。 [English]: X column names (order passed to 'predictPLS').
  , plsmYNames :: ![Text]       -- ^ [日本語]: Y 出力名 (出力セレクタ 'selectOutput' 用)。 [English]: Y output names (for the output selector 'selectOutput').
  , plsmOutIdx :: !Int          -- ^ [日本語]: effect plot に描く Y 出力列 index (既定 0)。 [English]: Y output column index to plot in the effect plot (default 0).
  }

-- | [日本語]: 列名指定で PLS effect plot 用モデルを組む。 @plsModel cfg xcols ycols df@。
--   学習は 'fitPLS'、 frame は X 列を 'RoleContinuous' として手組み (応答ダミー)。
--   [English]: Builds a model for PLS effect plots by specifying column
--   names. @plsModel cfg xcols ycols df@. Trained via 'fitPLS'; the frame is
--   hand-assembled with X columns as 'RoleContinuous' (dummy response).
plsModel :: ColumnSource d
         => PLSConfig -> [Text] -> [Text] -> d -> Either String PLSModel
plsModel cfg xcols ycols d = do
  x <- reqColsM xcols d
  y <- reqColsM ycols d
  fit <- either (Left . T.unpack) Right (fitPLS cfg x y)
  let n        = LA.rows x
      xColVecs = [ V.fromList (LA.toList c) | c <- LA.toColumns x ]
      -- 応答ダミーを先頭に (慣例: 応答が先頭・PLS の mvEvalFrame は応答を読まない)。
      roles    = ("__pls_resp", RoleResponse (V.replicate n 0))
               : [ (nm, RoleContinuous v) | (nm, v) <- zip xcols xColVecs ]
      mf = ModelFrame { mfRoles = roles, mfParams = [], mfNRows = n }
  Right PLSModel { plsmFit = fit, plsmFrame = mf, plsmXNames = xcols
                 , plsmYNames = ycols, plsmOutIdx = 0 }

-- | [日本語]: 描く Y 出力列を名前で選ぶ (多出力 PLS 用・既定は第 0 出力)。
--   @statModelMulti (selectOutput \"y2\" m) (along \"x1\")@。 名前が無ければ無変更。
--   [English]: Selects, by name, which Y output column to plot (for
--   multi-output PLS; default is output 0).
--   @statModelMulti (selectOutput \"y2\" m) (along \"x1\")@. Leaves unchanged
--   if the name isn't found.
selectOutput :: Text -> PLSModel -> PLSModel
selectOutput yname m =
  case lookup yname (zip (plsmYNames m) [0 ..]) of
    Just i  -> m { plsmOutIdx = i }
    Nothing -> m

-- ===========================================================================
-- 線形モデル (描画可能)
-- ===========================================================================

-- | [日本語]: X を束ねた描画可能な単回帰モデル。
--   [English]: A plottable simple regression model bundling X.
data LMModel = LMModel
  { lmDesign :: LA.Matrix Double  -- ^ [日本語]: 設計行列 X @n × p@ (intercept 列含む)。 [English]: Design matrix X, @n × p@ (including the intercept column).
  , lmResult :: FitResult         -- ^ [日本語]: 'fitLMVec' の結果 (β / ŷ / 残差 / R²)。 [English]: Result of 'fitLMVec' (β \/ ŷ \/ residuals \/ R²).
  , lmXraw   :: LA.Vector Double  -- ^ [日本語]: 散布図 x 軸の生 predictor @n@ (単回帰の x)。 [English]: Raw predictor for the scatter plot's x axis, length @n@ (the x of the simple regression).
  }

-- | [日本語]: 単回帰 @(x, y)@ から 'LMModel' を組む。 設計行列は @[1, x]@、 fit は 'fitLMVec'。
--   [English]: Builds an 'LMModel' from simple regression @(x, y)@. The
--   design matrix is @[1, x]@, fit via 'fitLMVec'.
lmModel :: LA.Vector Double -> LA.Vector Double -> LMModel
lmModel xs ys =
  let dm  = designMatrix (V.fromList (LA.toList xs))  -- designMatrix は boxed Vector 入力
      res = fitLMVec dm ys
  in LMModel { lmDesign = dm, lmResult = res, lmXraw = xs }

-- | [日本語]: X と family/link を束ねた描画可能な単回帰 GLM。
--   [English]: A plottable simple-regression GLM bundling X and the
--   family\/link.
data GLMModel = GLMModel
  { glmDesign :: LA.Matrix Double  -- ^ [日本語]: 設計行列 X @n × p@ (intercept 列含む)。 [English]: Design matrix X, @n × p@ (including the intercept column).
  , glmResult :: FitResult         -- ^ [日本語]: 'fitGLMFull' の結果 (β / μ̂ / 残差)。 [English]: Result of 'fitGLMFull' (β \/ μ̂ \/ residuals).
  , glmSigma  :: LA.Matrix Double  -- ^ [日本語]: 逆 Fisher 情報 Σ=(XᵀWX)⁻¹ (CI 用)。 [English]: Inverse Fisher information Σ=(XᵀWX)⁻¹ (for the CI).
  , glmFamily :: Family            -- ^ [日本語]: 分布族 (帯の意味付けに保持)。 [English]: Distribution family (kept for interpreting the band).
  , glmLink   :: LinkFn            -- ^ [日本語]: リンク関数 (μ スケールへの逆変換に必要)。 [English]: Link function (needed for the inverse transform to the μ scale).
  , glmXraw   :: LA.Vector Double  -- ^ [日本語]: 散布図 x 軸の生 predictor @n@ (単回帰の x)。 [English]: Raw predictor for the scatter plot's x axis, length @n@ (the x of the simple regression).
  }

-- | [日本語]: 単回帰 @(x, y)@ と family/link から 'GLMModel' を組む。 設計行列は @[1, x]@、
-- fit は 'fitGLMFull' (FitResult と逆 Fisher 情報 Σ の両方を返す)。
--   [English]: Builds a 'GLMModel' from simple regression @(x, y)@ and a
--   family\/link. The design matrix is @[1, x]@, fit via 'fitGLMFull'
--   (which returns both the FitResult and the inverse Fisher information Σ).
glmModel :: Family -> LinkFn -> LA.Vector Double -> LA.Vector Double -> GLMModel
glmModel family link xs ys =
  let dm           = designMatrix (V.fromList (LA.toList xs))
      (res, sigma) = fitGLMFull family link dm ys
  in GLMModel { glmDesign = dm, glmResult = res, glmSigma = sigma
              , glmFamily = family, glmLink = link, glmXraw = xs }

-- | [日本語]: X (生 predictor) を束ねた描画可能なスプライン回帰モデル。
--
-- 'SplineFit' は基底行列を保持しないので、 @confidenceBand@ / 散布図用に生 x を別途
-- 束ねる (= LMModel と同型の「描画可能なモデル」 化)。
--   [English]: A plottable spline regression model bundling X (the raw
--   predictor).
--
-- Since 'SplineFit' doesn't keep the basis matrix, the raw x is bundled
-- separately for @confidenceBand@ \/ the scatter plot (turning it into a
-- "plottable model" of the same shape as LMModel).
data SplineModel = SplineModel
  { splFit  :: SplineFit          -- ^ [日本語]: 'fitSpline' の結果 (basis 係数 + 線形核)。 [English]: Result of 'fitSpline' (basis coefficients + linear kernel).
  , splXraw :: LA.Vector Double   -- ^ [日本語]: 散布図 x 軸の生 predictor @n@。 [English]: Raw predictor for the scatter plot's x axis, length @n@.
  }

-- | [日本語]: @(x, y)@ と spline 種別・ノットから 'SplineModel' を組む。
--   [English]: Builds a 'SplineModel' from @(x, y)@ and the spline
--   kind\/knots.
splineModel
  :: SplineKind        -- ^ [日本語]: B-spline (次数) or 自然 3 次スプライン。 [English]: B-spline (with degree) or natural cubic spline.
  -> [Double]          -- ^ [日本語]: 内部ノット (境界含む)。 [English]: Interior knots (including boundaries).
  -> LA.Vector Double  -- ^ [日本語]: 説明変数 x。 [English]: Explanatory variable x.
  -> LA.Vector Double  -- ^ [日本語]: 応答 y。 [English]: Response y.
  -> SplineModel
splineModel kind knots xs ys =
  let xsV = V.fromList (LA.toList xs)
      ysV = V.fromList (LA.toList ys)
      fit = fitSpline kind knots xsV ysV
  in SplineModel { splFit = fit, splXraw = xs }

-- | [日本語]: X (単一 predictor の生 x) を束ねた描画可能な単変量 GAM。
--   [English]: A plottable univariate GAM bundling X (the raw single
--   predictor).
data GAMModel = GAMModel
  { gamFit  :: GAMFit             -- ^ [日本語]: 'fitGAM' の結果 (基底係数 + fitted)。 [English]: Result of 'fitGAM' (basis coefficients + fitted values).
  , gamXraw :: LA.Vector Double   -- ^ [日本語]: 散布図 x 軸の生 predictor @n@ (単変量の x)。 [English]: Raw predictor for the scatter plot's x axis, length @n@ (the univariate x).
  }

-- | [日本語]: 単変量 @(x, y)@ から 'GAMModel' を組む。 内部で 1 特徴の 'fitGAM' を呼ぶ。
--   [English]: Builds a 'GAMModel' from a univariate @(x, y)@. Internally
--   calls the single-feature 'fitGAM'.
gamModel
  :: Int               -- ^ [日本語]: B-spline 次数 (3 = cubic 推奨)。 [English]: B-spline degree (3 = cubic, recommended).
  -> Int               -- ^ [日本語]: 内部ノット数 (例 5)。 [English]: Number of interior knots (e.g. 5).
  -> Double            -- ^ [日本語]: ridge 罰則 λ (0 で無効)。 [English]: Ridge penalty λ (0 disables it).
  -> LA.Vector Double  -- ^ [日本語]: 説明変数 x。 [English]: Explanatory variable x.
  -> LA.Vector Double  -- ^ [日本語]: 応答 y。 [English]: Response y.
  -> GAMModel
gamModel degree nKnots lambda xs ys =
  let xsV = V.fromList (LA.toList xs)
      ysV = V.fromList (LA.toList ys)
      fit = fitGAM degree nKnots lambda [xsV] ysV
  in GAMModel { gamFit = fit, gamXraw = xs }

-- | [日本語]: df|-> 由来の (多予測子) GAM。 第1予測子を描画軸にする。
--   [English]: A (multi-predictor) GAM originating from @df |->@. The first
--   predictor is used as the plotting axis.
data GAMModelN = GAMModelN
  { gamNFit   :: GAMFit              -- ^ [日本語]: @fitGAMAuto@ の結果。 [English]: Result of @fitGAMAuto@.
  , gamNXraws :: [LA.Vector Double]  -- ^ [日本語]: 予測子ごとの訓練 x (列名順)。 [English]: Training x per predictor (in column-name order).
  , gamNNames :: [Text]             -- ^ [日本語]: 予測子名 (列名順)。 [English]: Predictor names (in column-name order).
  }

-- | [日本語]: X (生 predictor) を束ねた描画可能な単回帰ロバストモデル。
--   [English]: A plottable simple-regression robust model bundling X (the
--   raw predictor).
data RobustModel = RobustModel
  { rmFit  :: RobustFit           -- ^ [日本語]: 'fitRobustLM' の結果 (β̂ / ŷ / 重み)。 [English]: Result of 'fitRobustLM' (β̂ \/ ŷ \/ weights).
  , rmXraw :: LA.Vector Double    -- ^ [日本語]: 散布図 x 軸の生 predictor @n@ (単回帰の x)。 [English]: Raw predictor for the scatter plot's x axis, length @n@ (the x of the simple regression).
  }

-- | [日本語]: 単回帰 @(x, y)@ と estimator から 'RobustModel' を組む。 設計行列は @[1, x]@、
-- fit は 'fitRobustLM' (max 50 iter / tol 1e-6)。
--   [English]: Builds a 'RobustModel' from simple regression @(x, y)@ and an
--   estimator. The design matrix is @[1, x]@, fit via 'fitRobustLM' (max 50
--   iterations \/ tolerance 1e-6).
robustModel
  :: RobustEstimator   -- ^ [日本語]: Huber k or Tukey c。 [English]: Huber k or Tukey c.
  -> LA.Vector Double  -- ^ [日本語]: 説明変数 x。 [English]: Explanatory variable x.
  -> LA.Vector Double  -- ^ [日本語]: 応答 y。 [English]: Response y.
  -> RobustModel
robustModel est xs ys =
  let dm  = designMatrix (V.fromList (LA.toList xs))
      fit = fitRobustLM est dm ys 50 1e-6
  in RobustModel { rmFit = fit, rmXraw = xs }

-- | [日本語]: X と複数 τ の fit を束ねた描画可能な分位点回帰モデル。
--   [English]: A plottable quantile regression model bundling X and fits for
--   multiple τ.
data QuantileModel = QuantileModel
  { qmFits :: [(Double, QRFit)]   -- ^ [日本語]: (τ, その fit) の並び (τ 昇順を推奨)。 [English]: List of (τ, its fit) pairs (ascending τ order recommended).
  , qmXraw :: LA.Vector Double    -- ^ [日本語]: 散布図 x 軸の生 predictor @n@ (単回帰の x)。 [English]: Raw predictor for the scatter plot's x axis, length @n@ (the x of the simple regression).
  }

-- | [日本語]: 単回帰 @(x, y)@ と分位水準 τ のリストから 'QuantileModel' を組む。 設計行列は @[1, x]@、
-- 各 τ を 'fitQuantile' で fit。
--   [English]: Builds a 'QuantileModel' from simple regression @(x, y)@ and
--   a list of quantile levels τ. The design matrix is @[1, x]@; each τ is
--   fit via 'fitQuantile'.
quantileModel
  :: [Double]          -- ^ [日本語]: 分位水準 τ ∈ (0,1) のリスト (例 [0.1, 0.5, 0.9])。 [English]: List of quantile levels τ ∈ (0,1) (e.g. [0.1, 0.5, 0.9]).
  -> LA.Vector Double  -- ^ [日本語]: 説明変数 x。 [English]: Explanatory variable x.
  -> LA.Vector Double  -- ^ [日本語]: 応答 y。 [English]: Response y.
  -> QuantileModel
quantileModel taus xs ys =
  let dm   = designMatrix (V.fromList (LA.toList xs))
      fits = [ (t, fitQuantile t dm ys) | t <- taus ]
  in QuantileModel { qmFits = fits, qmXraw = xs }

-- | [日本語]: 多変量 (重回帰) 分位点回帰の結果。 設計行列 @[1, x₁..xₚ]@ に各 τ で 'fitQuantile' を
--   当てた fit 群を保持する。 係数は @qfBeta@ ('mqmFits' の各 'QRFit') で取り出す
--   (分位点回帰は SE を持たないため @coefSummary@ は非対応・単変量 @quantile@ と一貫)。
--   [English]: Result of multivariate (multiple-regression) quantile
--   regression. Holds the fits obtained by applying 'fitQuantile' at each τ
--   to the design matrix @[1, x₁..xₚ]@. Coefficients are extracted via
--   @qfBeta@ (each 'QRFit' in 'mqmFits') — since quantile regression has no
--   SE, @coefSummary@ isn't supported, consistent with the univariate
--   @quantile@.
data MultiQuantileModel = MultiQuantileModel
  { mqmTaus  :: ![Double]            -- ^ [日本語]: 分位水準 τ の並び。 [English]: List of quantile levels τ.
  , mqmNames :: ![Text]             -- ^ [日本語]: 予測子名 (intercept を除く・設計行列の 2 列目以降と対応)。 [English]: Predictor names (excluding the intercept; correspond to columns 2 onward of the design matrix).
  , mqmFits  :: ![(Double, QRFit)]   -- ^ [日本語]: 各 τ の fit (係数 'qfBeta' = @[β₀, β₁, …, βₚ]@)。 [English]: Fit for each τ (coefficients 'qfBeta' = @[β₀, β₁, …, βₚ]@).
  , mqmX     :: !(LA.Matrix Double)  -- ^ [日本語]: 設計行列 @[1, x₁, …, xₚ]@ (effect plot の評価元)。 [English]: Design matrix @[1, x₁, …, xₚ]@ (source for effect-plot evaluation).
  }

-- ===========================================================================
-- MCMC チェーン (描画可能)
-- ===========================================================================

-- | [日本語]: 1 パラメータを選んだ描画可能な MCMC チェーン。
--   [English]: A plottable MCMC chain with one parameter selected.
data ChainModel = ChainModel
  { cmChain :: Chain   -- ^ [日本語]: サンプラ出力 (post-burn-in)。 [English]: Sampler output (post-burn-in).
  , cmParam :: Text    -- ^ [日本語]: 描画対象のパラメータ名。 [English]: Name of the parameter to plot.
  }

-- | [日本語]: パラメータ名と 'Chain' から 'ChainModel' を組む。
--   [English]: Builds a 'ChainModel' from a parameter name and a 'Chain'.
chainModel :: Text -> Chain -> ChainModel
chainModel name ch = ChainModel { cmChain = ch, cmParam = name }

-- ===========================================================================
-- HBM (ベイズ確率プログラム) の学習 — Phase 49 A1
-- ===========================================================================

-- | [日本語]: HBM 学習の設定。 NUTS の chain 数 / 本サンプル数 / warmup を保持する
-- (brms 既定 = 4 chains × 1000 draws + 1000 warmup に相当)。 'hbmSeed' は
-- 純粋化 (将来の ST 版 @hbmModelPure seed …@) の継ぎ目として今から署名に持つ。
--   [English]: Configuration for HBM training. Holds NUTS's chain count \/
--   sample count \/ warmup (equivalent to brms's default: 4 chains × 1000
--   draws + 1000 warmup). 'hbmSeed' is carried in the signature now as a
--   seam for future purification (a future ST version,
--   @hbmModelPure seed …@).
data HBMConfig = HBMConfig
  { hbmChains    :: !Int            -- ^ [日本語]: chain 数 (既定 4)。 [English]: Number of chains (default 4).
  , hbmSamples   :: !Int            -- ^ [日本語]: 本サンプル数 = post-warmup draws (既定 1000)。 [English]: Number of samples = post-warmup draws (default 1000).
  , hbmWarmup    :: !Int            -- ^ [日本語]: warmup / burn-in (既定 1000)。 [English]: Warmup \/ burn-in (default 1000).
  , hbmSeed      :: !(Maybe Word32) -- ^ [日本語]: 乱数シード (現状は IO 内で消費・将来 ST の継ぎ目)。 [English]: Random seed (currently consumed inside IO; a future seam for ST).
  , hbmAdaptMass :: !Bool           -- ^ [日本語]: 対角質量行列の適応 (既定 True・brms/PyMC 同様)。
                                     --   a/b と s のようにスケールが大きく異なる posterior で
                                     --   収束 (特に scale param) に必須。 OFF だと s が未収束に
                                     --   なりやすい (計測で確認済)。
                                     --   [English]: Diagonal mass matrix adaptation (default True,
                                     --   as in brms\/PyMC). Essential for convergence (especially
                                     --   of the scale param) in posteriors where scales differ
                                     --   greatly, such as a\/b vs. s. When OFF, s tends not to
                                     --   converge (confirmed by measurement).
  , hbmWarmupInitMaxDepth :: !(Maybe Int)
                                     -- ^ [日本語]: 'nutsWarmupInitMaxDepth' の pass-through
                                     --   (opt-in・既定 'Nothing' = 無効)。 質量行列の初回更新前
                                     --   (M=I 期間) の tree depth 上限。 warmup 初期の ε 鋸歯で
                                     --   deep tree を掘る浪費 (05-mh 実測で warmup evals が
                                     --   nutpie 比 1.92×) を 'Just' 6 等で抑制する。 参照実装に
                                     --   無いヒューリスティックゆえ既定 OFF (NUTS.hs 側と同判断)。
                                     --   [English]: A pass-through for 'nutsWarmupInitMaxDepth'
                                     --   (opt-in; default 'Nothing' = disabled). An upper bound on
                                     --   tree depth during the period before the mass matrix's
                                     --   first update (the M=I period). Suppresses the waste of
                                     --   digging deep trees during early-warmup ε sawtoothing
                                     --   (measured on 05-mh: warmup evals were 1.92× nutpie's)
                                     --   with something like 'Just' 6. Since this heuristic isn't
                                     --   in the reference implementation, it defaults to OFF
                                     --   (matching the decision on the NUTS.hs side).
  } deriving (Show, Eq)

-- | [日本語]: 既定の HBM 設定: 4 chains × 1000 draws + 1000 warmup (brms 既定相当)。 質量行列適応 ON。
--   [English]: Default HBM configuration: 4 chains × 1000 draws + 1000
--   warmup (equivalent to brms's default). Mass matrix adaptation ON.
defaultHBM :: HBMConfig
defaultHBM = HBMConfig
  { hbmChains    = 4
  , hbmSamples   = 1000
  , hbmWarmup    = 1000
  , hbmSeed      = Nothing
  , hbmAdaptMass = True
  , hbmWarmupInitMaxDepth = Nothing
  }

-- | [日本語]: 学習済 HBM モデル。 data placeholder を bind したモデル本体 (@hbmModelSpec@)、
-- posterior draws ('hbmChainsR' = chain 群)、 bind 済みデータ ('hbmData') を保持する。
-- ★ 抽出子 (@epred@ 等) はここから純粋に図を組む (= @df |>>@ と整合)。
--   [English]: A trained HBM model. Holds the model body with data
--   placeholders bound (@hbmModelSpec@), the posterior draws ('hbmChainsR' =
--   the chains), and the bound data ('hbmData').
--   Note: extractors (e.g. @epred@) build plots purely from this (consistent
--   with @df |>>@).
data HBMModel = HBMModel
  { hbmModelSpec :: ModelP ()          -- ^ [日本語]: data を bind 済みのモデル (epred 評価でも使う)。 [English]: The model with data already bound (also used for epred evaluation).
  , hbmChainsR   :: ![Chain]           -- ^ [日本語]: posterior draws (chain 群)。 [English]: Posterior draws (the chains).
  , hbmData      :: ![(Text, [Double])] -- ^ [日本語]: bind 済みデータ列 (列名 → 値)。 [English]: Bound data columns (column name → values).
  , hbmFactorLevels :: ![(Text, [Text])]
    -- ^ [日本語]: @dataNamedIx@ slot に Text factor 列を bind した場合の
    --   sort 順 levels (slot 名 → levels)。 コード i = levels !! i で、
    --   indexed パラメータ (b0_2 等) がどの群かを引ける。 数値列 bind は空。
    --   [English]: When a Text factor column is bound to a @dataNamedIx@
    --   slot, this holds the sorted levels (slot name → levels). Code i =
    --   levels !! i lets you look up which group an indexed parameter
    --   (e.g. b0_2) belongs to. Empty for numeric-column binds.
  }

-- | [日本語]: 列名→値の組をモデル中の data placeholder に順に 'withData' で bind する。
-- 明示再帰 (foldr ではなく) なのは、 'ModelP' が rank-2 多相 ('forall a.') ゆえ
-- ImpredicativeTypes 下の foldr では accumulator が単相化してしまうため。
--   [English]: Binds column-name → value pairs in order to the model's data
--   placeholders via 'withData'. Explicit recursion (rather than foldr) is
--   used because 'ModelP' is rank-2 polymorphic ('forall a.'), and under
--   ImpredicativeTypes a foldr would cause the accumulator to be
--   monomorphized.
bindCols :: [(Text, [Double])] -> ModelP r -> ModelP r
bindCols []             m = m
bindCols ((n, vs):rest) m = bindCols rest (withData n vs m)

-- | [日本語]: 'bindCols' の @DataIx@ 版: 列名→index 列を 'withDataIx' で bind。
--   [English]: The @DataIx@ version of 'bindCols': binds column-name →
--   index-column pairs via 'withDataIx'.
bindIxCols :: [(Text, [Int])] -> ModelP r -> ModelP r
bindIxCols []             m = m
bindIxCols ((n, is):rest) m = bindIxCols rest (withDataIx n is m)

-- | [日本語]: 確率プログラム 'ModelP' を NUTS で学習し 'HBMModel' にする (MCMC ゆえ IO)。
--
-- 列名で 'withData' を畳み込み、 モデル中の placeholder (@dataNamed@ / observe の
-- 参照名) を df 由来の実データに差し替える (PyMC @set_data@ 同型)。 chain は既存
-- 'nutsChains' が並列実行 (実 OS スレッド並列には @-threaded +RTS -N@ が要る)。
--
-- 当面の入口は @[(Text,[Double])]@ (列名→値)。 'ColumnSource' 一般化
-- (Map/DataFrame/assoc 疎結合) は別 Phase (データ API)。
--   [English]: Trains a probabilistic program 'ModelP' via NUTS into an
--   'HBMModel' (IO, since this is MCMC).
--
-- Folds 'withData' over the column names, replacing the model's
-- placeholders (the reference names of @dataNamed@ \/ observe) with real
-- data sourced from the df (the same shape as PyMC's @set_data@). Chains
-- are run in parallel via the existing 'nutsChains' (actual OS-thread
-- parallelism requires @-threaded +RTS -N@).
--
-- The entry point for now is @[(Text,[Double])]@ (column name → values).
-- Generalizing 'ColumnSource' (loosely coupled Map\/DataFrame\/assoc) is a
-- separate Phase (the data API).
hbmModel :: HBMConfig -> ModelP () -> [(Text, [Double])] -> IO HBMModel
hbmModel cfg model dat = do
  let bound :: ModelP ()
      bound = bindCols dat model
      initC = hbmInitPoint bound
      ncfg  = hbmNutsConfig cfg
  gen <- case hbmSeed cfg of
           Nothing -> createSystemRandom
           Just w  -> initialize (V.singleton w)
  chains <- nutsChains bound ncfg (hbmChains cfg) initC gen
  pure HBMModel
    { hbmModelSpec = bound
    , hbmChainsR   = chains
    , hbmData      = dat
    , hbmFactorLevels = []
    }

-- | [日本語]: NUTS の初期点 (制約空間)。 positive 制約 (σ 等) を 0 で初期化すると @log 0 = -∞@ で
-- 初手から全 proposal が divergence する (実測 2026-06-05)。 @getTransforms@ で制約を検出し
-- PositiveT→1 / UnitIntervalT→0.5 / 他→0 で初期化する。
--   [English]: NUTS's initial point (in constrained space). Initializing a
--   positive-constrained parameter (e.g. σ) at 0 causes @log 0 = -∞@, making
--   every proposal diverge from the very first step (measured 2026-06-05).
--   Detects constraints via @getTransforms@ and initializes: PositiveT→1,
--   UnitIntervalT→0.5, others→0.
hbmInitPoint :: ModelP () -> Map.Map Text Double
hbmInitPoint bound = Map.map initFor (getTransforms bound)
  where
    initFor PositiveT      = 1.0
    initFor UnitIntervalT  = 0.5
    initFor UnconstrainedT = 0.0

-- | [日本語]: 'HBMConfig' → 'NUTSConfig'。 NUTS は @total = burnIn + iterations@ を回し iterations 本
-- だけ保持するので、 iterations に本サンプル数・burnIn に warmup を割り当てる。
--   [English]: 'HBMConfig' → 'NUTSConfig'. NUTS runs
--   @total = burnIn + iterations@ and keeps only iterations of them, so the
--   sample count is assigned to iterations and warmup to burnIn.
hbmNutsConfig :: HBMConfig -> NUTSConfig
hbmNutsConfig cfg = defaultNUTSConfig
  { nutsIterations = hbmSamples cfg
  , nutsBurnIn     = hbmWarmup cfg
  , nutsAdaptMass  = hbmAdaptMass cfg
  , nutsWarmupInitMaxDepth = hbmWarmupInitMaxDepth cfg
  -- Phase 94 A4-2: init jitter ('nutsInitJitter') は opt-in の infra として持つが
  -- **blanket default にはしない** (=0)。 seeds の funnel は非中心化で解消済で
  -- jitter 不要。 一律 jitter=1.0 は相関 RE 小モデル (WorkflowSpec ranSlope) の
  -- warmup を散らして傾き回復を壊す退化が実測されたため (§A4-2)。 funnel 型で
  -- 明示的に効かせたい呼び出し側が個別に nutsInitJitter を上げる。
  }

-- | [日本語]: 純粋・決定的な HBM 学習。 'hbmModel' の ST/seed 版で IO を持たない。
-- @nutsChainsPure@ (chain 横断を spark 並列・seed で再現可能) を使う。 @hbmSeed@ が
-- 'Nothing' のときは固定既定 seed (42) を用いる (純粋・決定的を保証する設計判断)。
--   [English]: Pure, deterministic HBM training. An ST\/seed version of
--   'hbmModel' with no IO. Uses @nutsChainsPure@ (spark-parallel across
--   chains, reproducible via seed). When @hbmSeed@ is 'Nothing', a fixed
--   default seed (42) is used (a design decision to guarantee pure,
--   deterministic behavior).
hbmModelPure :: HBMConfig -> ModelP () -> [(Text, [Double])] -> HBMModel
hbmModelPure cfg model dat = hbmModelPureWith cfg model dat [] []

-- | [日本語]: 'hbmModelPure' の拡張形: @DataIx@ slot の index 列と
-- Text factor levels も bind する。 'df |-> hbm' ('Fit' instance) が
-- @resolveIxSlots@ で解決した結果を渡す主経路。
--   [English]: An extended form of 'hbmModelPure': also binds @DataIx@
--   slots' index columns and Text factor levels. The main path used when
--   @df |-> hbm@ (the 'Fit' instance) passes results resolved by
--   @resolveIxSlots@.
hbmModelPureWith :: HBMConfig -> ModelP () -> [(Text, [Double])]
                 -> [(Text, [Int])] -> [(Text, [Text])] -> HBMModel
hbmModelPureWith cfg model dat ixDat levels =
  let bound :: ModelP ()
      bound  = bindIxCols ixDat (bindCols dat model)
      initC  = hbmInitPoint bound
      ncfg   = hbmNutsConfig cfg
      seed   = fromMaybe 42 (hbmSeed cfg)
      chains = nutsChainsPure bound ncfg (hbmChains cfg) initC seed
  in HBMModel
       { hbmModelSpec = bound
       , hbmChainsR   = chains
       , hbmData      = dat
       , hbmFactorLevels = levels
       }

-- | [日本語]: 'hbmModelPure' の IO 版: stderr に進捗を表示しながら学習する。
-- bind + seed 規約は 'hbmModelPureWith' と同一・chain ごとの seed は
-- @chainSeeds@ 共有 ('nutsChainsStream') ゆえ、 結果は同 cfg の
-- 'hbmModelPure' と__ビット一致__する (test-plot で固定)。
--   [English]: The IO version of 'hbmModelPure': trains while showing
--   progress on stderr. The bind + seed convention is identical to
--   'hbmModelPureWith'; since per-chain seeds are shared via @chainSeeds@
--   ('nutsChainsStream'), the result is __bit-identical__ to 'hbmModelPure'
--   with the same cfg (pinned down in test-plot).
hbmModelIO :: HBMConfig -> ModelP () -> [(Text, [Double])] -> IO HBMModel
hbmModelIO cfg model dat = hbmModelIOWith cfg model dat [] []

-- | [日本語]: 'hbmModelPureWith' の IO + 進捗表示版。 @(|->!)@ の
-- HBM 経路 ('fitIO') が @resolveIxSlots@ の解決結果を渡す主経路。
--   [English]: The IO + progress-display version of 'hbmModelPureWith'. The
--   main path used when the HBM path of @(|->!)@ ('fitIO') passes results
--   resolved by @resolveIxSlots@.
hbmModelIOWith :: HBMConfig -> ModelP () -> [(Text, [Double])]
               -> [(Text, [Int])] -> [(Text, [Text])] -> IO HBMModel
hbmModelIOWith cfg model dat ixDat levels = do
  let bound :: ModelP ()
      bound    = bindIxCols ixDat (bindCols dat model)
      initC    = hbmInitPoint bound
      ncfg     = hbmNutsConfig cfg
      seed     = fromMaybe 42 (hbmSeed cfg)
      perChain = hbmWarmup cfg + hbmSamples cfg
  (onSample, finish) <- newProgressRenderer (hbmChains cfg) perChain
  chains <- nutsChainsStream bound ncfg (hbmChains cfg) initC seed onSample
  finish
  pure HBMModel
    { hbmModelSpec = bound
    , hbmChainsR   = chains
    , hbmData      = dat
    , hbmFactorLevels = levels
    }

-- ===========================================================================
-- HBM 事後要約 (Phase 103)
-- ===========================================================================

-- | [日本語]: 要約対象のパラメタ名 (latent 宣言順 → deterministic 宣言順の連結)。
-- deterministic 派生量を既定で含めるのは PyMC/arviz の @az.summary@ が
-- Deterministic を含むのと同型。
--   [English]: Parameter names to summarize (declaration order of latent
--   variables followed by declaration order of deterministic ones).
--   Including deterministic derived quantities by default matches how
--   PyMC\/arviz's @az.summary@ includes Deterministic.
hbmSummaryNames :: HBMModel -> [Text]
hbmSummaryNames m = sampleNames spec ++ deterministicNames spec
  where spec :: ModelP ()
        spec = hbmModelSpec m

-- | [日本語]: deterministic 派生量を注入済みの chain 群。派生量が無いモデルでは
-- augment (全 draw の再評価) を省いて素の chain を返す。
--   [English]: The chains with deterministic derived quantities injected.
--   For models with no derived quantities, skips the augment step
--   (re-evaluating every draw) and returns the plain chains.
hbmAugmentedChains :: HBMModel -> [Chain]
hbmAugmentedChains m
  | null (deterministicNames spec) = hbmChainsR m
  | otherwise = map (augmentChainWithDeterministic spec) (hbmChainsR m)
  where spec :: ModelP ()
        spec = hbmModelSpec m

-- | [日本語]: 学習済 HBM の事後要約表 (@az.summary@ 相当・純粋)。
-- mean / sd / HDI / ess_bulk (+ multi-chain 時 r_hat) を latent +
-- deterministic の全パラメタについて返す。
--   [English]: Posterior summary table of a trained HBM (equivalent to
--   @az.summary@; pure). Returns mean \/ sd \/ HDI \/ ess_bulk (+ r_hat for
--   multi-chain) for all latent and deterministic parameters.
hbmSummary :: HBMModel -> [SummaryRow]
hbmSummary m = posteriorSummary (hbmSummaryNames m) (hbmAugmentedChains m)

-- | [日本語]: 'hbmSummary' をコンソール表として表示する。
--   [English]: Displays 'hbmSummary' as a console table.
printHBMSummary :: HBMModel -> IO ()
printHBMSummary m = printPosteriorSummary (hbmSummaryNames m) (hbmAugmentedChains m)

-- | [日本語]: 'hbmSummary' の DataFrame 化。列 = param / mean / sd / hdi_lo / hdi_hi /
-- ess_bulk (+ multi-chain 時のみ r_hat = 'printPosteriorSummary' の列規約と同じ)。
--   [English]: Turns 'hbmSummary' into a DataFrame. Columns = param \/ mean \/
--   sd \/ hdi_lo \/ hdi_hi \/ ess_bulk (+ r_hat only for multi-chain — the
--   same column convention as 'printPosteriorSummary').
hbmSummaryDf :: HBMModel -> DX.DataFrame
hbmSummaryDf m =
  let rows  = hbmSummary m
      multi = any (\r -> case srRhat r of Just _ -> True; _ -> False) rows
      base  =
        [ ("param",    DX.fromList (map srName  rows))
        , ("mean",     DX.fromList (map srMean  rows))
        , ("sd",       DX.fromList (map srSD    rows))
        , ("hdi_lo",   DX.fromList (map srHdiLo rows))
        , ("hdi_hi",   DX.fromList (map srHdiHi rows))
        , ("ess_bulk", DX.fromList (map srEssV  rows))
        ]
      rh    = [ ("r_hat", DX.fromList (map (fromMaybe (0 / 0) . srRhat) rows))
              | multi ]
  in DX.fromNamedColumns (base ++ rh)

-- | [日本語]: 事後 draw の DataFrame 化 (1 パラメタ = 1 列・全 chain を chain 順に連結、
-- deterministic 派生量込み)。'Hanalyze.Data.Wrangle' の
-- @summarise@ / @groupBy@ 等で自由集計する入口。
--   [English]: Turns the posterior draws into a DataFrame (one parameter =
--   one column, all chains concatenated in chain order, including
--   deterministic derived quantities). An entry point for free-form
--   aggregation via 'Hanalyze.Data.Wrangle''s @summarise@ \/
--   @groupBy@ etc.
hbmDrawsDf :: HBMModel -> DX.DataFrame
hbmDrawsDf m =
  let chains = hbmAugmentedChains m
  in DX.fromNamedColumns
       [ (n, DX.fromList (concatMap (chainVals n) chains))
       | n <- hbmSummaryNames m ]

-- ===========================================================================
-- 時系列予測 (描画可能)
-- ===========================================================================

-- | [日本語]: 履歴系列と AR fit・予測地平を束ねた描画可能な時系列予測モデル。
--   [English]: A plottable time-series forecast model bundling the history
--   series, the AR fit, and the forecast horizon.
data ForecastModel = ForecastModel
  { fmFit     :: ARFit             -- ^ [日本語]: 'fitAR' の結果。 [English]: Result of 'fitAR'.
  , fmHistory :: LA.Vector Double  -- ^ [日本語]: 観測系列 (時系列順)。 [English]: Observed series (in time order).
  , fmHorizon :: Int               -- ^ [日本語]: 予測地平 h。 [English]: Forecast horizon h.
  }

-- | [日本語]: 系列・AR 次数・地平から 'ForecastModel' を組む ('fitAR' で fit)。
--   [English]: Builds a 'ForecastModel' from the series, AR order, and
--   horizon (fit via 'fitAR').
forecastModel
  :: Int               -- ^ [日本語]: AR 次数 p。 [English]: AR order p.
  -> Int               -- ^ [日本語]: 予測地平 h。 [English]: Forecast horizon h.
  -> LA.Vector Double  -- ^ [日本語]: 観測系列 (時系列順)。 [English]: Observed series (in time order).
  -> ForecastModel
forecastModel order horizon series =
  ForecastModel { fmFit = fitAR order series, fmHistory = series
                , fmHorizon = horizon }

-- ===========================================================================
-- データ源 → モデル を当てはめる統一型クラス。
-- ===========================================================================

-- | [日本語]: データ源 → モデル を当てはめる統一型クラス。
--
-- @fitWith@ / @(|->)@ は __pure だが total ではない__ (列欠落・parse 失敗は
-- 'error')。 検証パイプライン用に total な 'fitEither' を併設する
-- (既定の @fitWith@ は 'fitEither' を 'error' で潰したもの)。
--   [English]: A unified type class for fitting a data source → a model.
--
-- @fitWith@ \/ @(|->)@ are __pure but not total__ (missing columns \/ parse
-- failures raise 'error'). A total 'fitEither' is provided alongside for
-- validation pipelines (the default @fitWith@ is 'fitEither' collapsed via
-- 'error').
class Fit spec where
  -- | [日本語]: この spec を当てはめた結果のモデル型。
  --   [English]: The resulting model type when this spec is fitted.
  type Fitted spec
  -- | [日本語]: 当てはめ (pure・失敗は 'error')。 既定実装は 'fitEither' 経由。
  --   [English]: Fit (pure; failure raises 'error'). The default
  --   implementation goes through 'fitEither'.
  fitWith   :: ColumnSource d => spec -> d -> Fitted spec
  fitWith spec d = either error id (fitEither spec d)
  -- | [日本語]: 当てはめ (total・失敗は 'Left')。
  --   [English]: Fit (total; failure is 'Left').
  fitEither :: ColumnSource d => spec -> d -> Either String (Fitted spec)
  -- | [日本語]: 当てはめ (IO・進捗表示など副作用つき学習)。 既定 =
  -- @pure . fitWith@ で純粋 spec は挙動不変 (失敗の error 意味論も @(|->)@
  -- と同じ)。 学習が重い spec (@HBMSpec@) だけ override して進捗を出す。
  --   [English]: Fit (IO; training with side effects such as progress
  --   display). Default = @pure . fitWith@, so pure specs are unchanged in
  --   behavior (the error semantics of failure also match @(|->)@). Only
  --   specs with heavy training (@HBMSpec@) override this to show progress.
  fitIO     :: ColumnSource d => spec -> d -> IO (Fitted spec)
  fitIO spec d = pure (fitWith spec d)
  -- | [日本語]: 透過標準化ラッパ (@standardized@ / @standardizedY@) が __標準化対象とする予測子列名__
  -- 既定は @[]@ = 「被せる意味の無い spec」
  -- であり、 @standardized@ を付けても 'fitEither' が 'Left' で誤用を弾く。
  -- 距離ベース (kNN) や線形 (整形目的) の spec だけが実列名を返す。 内部標準化済
  -- (@GPSpec@ \/ @RegSpec@ \/ @PCASpec@ \/ @PLSSpec@) と木系は二重標準化\/無意味回避で
  -- 既定 @[]@ のまま (= ラッパ拒否)。
  --   [English]: The __predictor column names to standardize__ for the
  --   transparent standardization wrapper (@standardized@ \/
  --   @standardizedY@). The default is @[]@ ("a spec for which wrapping is
  --   meaningless"), so even if @standardized@ is applied, 'fitEither'
  --   rejects the misuse with 'Left'. Only distance-based (kNN) or linear
  --   (for shaping purposes) specs return real column names. Specs that are
  --   already internally standardized (@GPSpec@ \/ @RegSpec@ \/ @PCASpec@ \/
  --   @PLSSpec@) and tree-based ones stay at the default @[]@ to avoid
  --   double standardization \/ meaninglessness (i.e. the wrapper is
  --   rejected).
  predictorCols :: spec -> [Text]
  predictorCols _ = []
  -- | [日本語]: 透過標準化ラッパが y も標準化する (@standardizedY@) 際の__応答列名__
  -- 既定 'Nothing'。 __連続応答の回帰 spec のみ__ @Just@ を返す。
  -- 分類 (クラスラベル) や family\/link でスケールが拘束される GLM は標準化が不正ゆえ
  -- 'Nothing' のまま (= @standardizedY@ を付けると 'fitEither' が 'Left')。
  --   [English]: The __response column name__ used when the transparent
  --   standardization wrapper also standardizes y (@standardizedY@).
  --   Default 'Nothing'. __Only regression specs with a continuous response__
  --   return @Just@. Classification (class labels) and GLMs
  --   whose scale is constrained by the family\/link keep 'Nothing', since
  --   standardizing them is invalid (i.e. applying @standardizedY@ makes
  --   'fitEither' return 'Left').
  responseCol :: spec -> Maybe Text
  responseCol _ = Nothing

-- | [日本語]: 因果探索 (LiNGAM) の高レベル @df |->@ 結果ラッパ。 各 LiNGAM fit 型は
--   変数名を持たないため、 学習した fit @a@ に__変数名__ (@df |->@ が渡した列名) を添える。
--   @Plottable@ (@Hanalyze.Plot.ML@) が @lfNames@ を DAG ノード名に使う
--   (無ければ @x0..@ フォールバック)。 侵襲的な per-fit-型 names フィールド追加を避ける汎用ラッパ。
--   [English]: A high-level @df |->@ result wrapper for causal discovery
--   (LiNGAM). Since each LiNGAM fit type carries no variable names, this
--   attaches the __variable names__ (the column names passed by @df |->@)
--   to the trained fit @a@. @Plottable@ (@Hanalyze.Plot.ML@) uses
--   @lfNames@ as DAG node names (falling back to @x0..@ if absent). A
--   generic wrapper that avoids invasively adding a names field to each
--   per-fit type.
data LiNGAMFitted a = LiNGAMFitted
  { lfFit   :: !a        -- ^ [日本語]: 各 variant の fit 結果 (@DirectLiNGAMFit@ 等) [English]: The fit result of each variant (e.g. @DirectLiNGAMFit@).
  , lfNames :: ![Text]   -- ^ [日本語]: 変数名 (行列の列順 = fit の変数 index 順) [English]: Variable names (matrix column order = fit's variable index order).
  } deriving (Show)

-- | [日本語]: 列名で数値列を引き 'LA.Vector' 化 (無ければ 'Left')。 二変量近道の素経路。
--   [English]: Looks up a numeric column by name and turns it into an
--   'LA.Vector' (or 'Left' if absent). The plain path for the bivariate
--   shortcut.
reqColV :: ColumnSource d => Text -> d -> Either String (LA.Vector Double)
reqColV n d = case lookupCol n d of
  Just xs -> Right (LA.fromList xs)
  Nothing -> Left ("ColumnSource: 列が見つかりません: " <> T.unpack n)

-- | [日本語]: 複数の列名から @n × p@ 行列を組む (各列名 = 1 変数 = 行列の 1 列・行=標本)。
--   行列入力モデル (PCA \/ PLS \/ …) を列名 spec で高レベル化する際の素経路。
--   列が 1 つも無い / 長さ不揃いは 'Left'。
--   [English]: Builds an @n × p@ matrix from multiple column names (each
--   column name = 1 variable = 1 matrix column; rows = samples). The plain
--   path used when giving matrix-input models (PCA \/ PLS \/ …) a
--   high-level column-name spec. 'Left' if there are no columns at all \/
--   the lengths don't match.
reqColsM :: ColumnSource d => [Text] -> d -> Either String (LA.Matrix Double)
reqColsM [] _ = Left "ColumnSource: 列名が空です (1 列以上必要)"
reqColsM ns d = do
  cols <- mapM (`reqColV` d) ns
  let lens = map LA.size cols
  if all (== head lens) lens
    then Right (LA.fromColumns cols)
    else Left ("reqColsM: 列の長さが不揃いです: " <> show lens)

-- ===========================================================================
-- WLS / 透過標準化 / 群別フィット の結果型
-- ===========================================================================

-- | [日本語]: WLS の結果。 内側 'LMModel' は __√w スケール設計行列__ ('lmDesign'=X_w) と
--   その OLS 結果 ('lmResult')、 __元の x__ ('lmXraw') を保持する (grid 経路で正しい
--   WLS CI を出すための容れ物)。 weighted R² 算出用に__重み__ と__元 y__ も保持する。
--   [English]: Result of WLS. The inner 'LMModel' holds the
--   __√w-scaled design matrix__ ('lmDesign'=X_w), its OLS result
--   ('lmResult'), and the __original x__ ('lmXraw') — a container for
--   producing a correct WLS CI on the grid path. Also holds the
--   __weights__ and __original y__ for computing weighted R².
data WeightedLMModel = WeightedLMModel
  { wlmInner   :: !LMModel    -- ^ [日本語]: √w スケール設計・OLS 結果・元 x。 [English]: √w-scaled design, OLS result, original x.
  , wlmWeights :: ![Double]   -- ^ [日本語]: 重み w (元の行順)。 [English]: Weights w (in original row order).
  , wlmY       :: ![Double]   -- ^ [日本語]: 元の応答 y (weighted R² 算出用)。 [English]: Original response y (for computing weighted R²).
  }

-- | [日本語]: 透過標準化の結果。 内側モデル (標準化空間で学習) と逆変換に要る (μ,σ) を保持する。
--   @SingleVarModel@ / @Plottable@ instance がこれを使い元スケール軸で描く。
--   [English]: Result of transparent standardization. Holds the inner model
--   (trained in standardized space) and the (μ,σ) needed for the inverse
--   transform. The @SingleVarModel@ \/ @Plottable@ instance uses this to
--   plot on the original-scale axes.
data StandardizedModel m = StandardizedModel
  { smInner :: !m                            -- ^ [日本語]: 標準化空間で fit した内側モデル。 [English]: Inner model fit in standardized space.
  , smXStd  :: !Standardizer                 -- ^ [日本語]: 予測子列の (μ,σ)。'Stat.Standardize'。 [English]: (μ,σ) of the predictor columns. See 'Stat.Standardize'.
  , smYStd  :: !(Maybe (Double, Double))     -- ^ [日本語]: 応答 y の (μ,σ)。@standardizedY@ 時のみ。 [English]: (μ,σ) of the response y. Only when @standardizedY@ is used.
  , smTrain :: !(Maybe ([Double], [Double])) -- ^ [日本語]: 元スケール訓練 (x,y)。単変量散布図用 (予測子 1 列時のみ)。 [English]: Original-scale training (x,y). For the univariate scatter plot (only when there is a single predictor column).
  }

-- | [日本語]: 群別フィットの結果。 各群ラベル → その群の 'Fitted spec' を保持する
--   __実結果型__ ('HBMModel' 同族・@ModelSpec@ ではない)。 @groupModels@ で取り出す。
--   [English]: Result of a per-group fit. Holds each group label →  its
--   group's 'Fitted spec' as an __actual result type__ (in the same family
--   as 'HBMModel', not a @ModelSpec@). Extracted via @groupModels@.
newtype GroupedFit spec = GroupedFit { gfGroups :: [(Text, Fitted spec)] }

-- ===========================================================================
-- カーネル回帰 (描画可能)
-- ===========================================================================

-- | [日本語]: カーネル回帰の 4 象限。 @seed@/@D@ は RFF 近似コンストラクタにだけ載る。
-- KRR 象限は 'Krr'/'KrrRff' (Kernel Ridge Regression・線形罰則回帰の @Ridge@ と区別)。
--   [English]: The 4 quadrants of kernel regression. @seed@\/@D@ appear only
--   on the RFF-approximation constructors. The KRR quadrants are
--   'Krr'\/'KrrRff' (Kernel Ridge Regression, distinct from linear
--   penalized regression's @Ridge@).
data GPMethod
  = Gp                       -- ^ [日本語]: 厳密 GP   (分布あり・事後分散→帯)。 [English]: Exact GP (has a distribution; posterior variance → band).
  | Krr                      -- ^ [日本語]: 厳密 KRR  (点・KRR ≡ GP 事後平均)。 [English]: Exact KRR (point estimate; KRR ≡ GP posterior mean).
  | GpRff  !Int !Word32      -- ^ [日本語]: RFF 近似 GP  (@D@ 特徴次元, seed)。 [English]: RFF-approximated GP (@D@ feature dimension, seed).
  | KrrRff !Int !Word32      -- ^ [日本語]: RFF 近似 KRR (@D@ 特徴次元, seed)。 [English]: RFF-approximated KRR (@D@ feature dimension, seed).
  deriving (Eq, Show)

-- | [日本語]: ハイパラの「決め方 + (固定時のみ) 値」を一箇所に集約 (役割重複ゆえ別フィールドは持たない)。
--   [English]: Gathers the hyperparameter's "how it's decided + (only when
--   fixed) value" into one place (no separate field, since that would
--   duplicate the role).
data HyperStrategy
  = FixedHyper GPParams   -- ^ [日本語]: 固定: この 'GPParams' を使う (最適化しない)。 [English]: Fixed: use this 'GPParams' (no optimization).
  | AutoMarginalLik       -- ^ [日本語]: 周辺尤度で自動 ('GP.optimizeGP'・初期値はデータ駆動)。 [English]: Automatic via marginal likelihood ('GP.optimizeGP'; data-driven initial values).
  | AutoCV                -- ^ [日本語]: LOOCV (PRESS) で自動 ('GP.autoCVHyperGP'・初期値は同上)。 [English]: Automatic via LOOCV (PRESS) ('GP.autoCVHyperGP'; same initial values).

-- | [日本語]: 当てはめ済の統合カーネル回帰モデル。 象限 + 解決済ハイパラ + 予測子を保持する。
-- 予測子 'gprPredict' は @grid x → (μ̂, Maybe 事後分散)@: 分布あり象限 (Gp/GpRff) は
-- @Just 分散@、 点象限 (Ridge/RidgeRff) は @Nothing@ (= 帯なし)。 @SingleVarModel@ /
-- @Plottable@ instance は E2。
--   [English]: A fitted unified kernel regression model. Holds the
--   quadrant + resolved hyperparameters + predictor. The predictor
--   'gprPredict' takes @grid x → (μ̂, Maybe posterior variance)@: quadrants
--   with a distribution (Gp\/GpRff) give @Just variance@, point quadrants
--   (Ridge\/RidgeRff) give @Nothing@ (= no band). The @SingleVarModel@ \/
--   @Plottable@ instance is E2.
data GPRegModel = GPRegModel
  { gprMethod  :: !GPMethod                                  -- ^ [日本語]: Periodic フォールバック後の象限。 [English]: The quadrant after any Periodic fallback.
  , gprKernel  :: !Kernel                                    -- ^ [日本語]: カーネル種 (Periodic は不変)。 [English]: Kernel kind (unchanged for Periodic).
  , gprParams  :: !GPParams                                  -- ^ [日本語]: 解決済ハイパラ。 [English]: Resolved hyperparameters.
  , gprXraw    :: !(LA.Vector Double)                        -- ^ [日本語]: 訓練 x (svRange / 散布図)。 [English]: Training x (svRange \/ scatter plot).
  , gprY       :: !(LA.Vector Double)                        -- ^ [日本語]: 訓練 y。 [English]: Training y.
  , gprPredict :: !([Double] -> ([Double], Maybe [Double]))  -- ^ [日本語]: grid x → (μ̂, Maybe 事後分散)。 [English]: grid x → (μ̂, Maybe posterior variance).
  }

-- | [日本語]: 当てはめ済の多変量カーネル回帰モデル。 予測子 'gprnPredict' は評価行列 (@m × p@) を
-- 取り @(μ̂, Maybe 事後分散)@ を返す (分布あり象限のみ Just)。
--   [English]: A fitted multivariate kernel regression model. The predictor
--   'gprnPredict' takes an evaluation matrix (@m × p@) and returns
--   @(μ̂, Maybe posterior variance)@ (Just only for quadrants with a
--   distribution).
data GPRegModelN = GPRegModelN
  { gprnMethod  :: !GPMethod
  , gprnKernel  :: !Kernel
  , gprnParams  :: !GPParams
  , gprnXraws   :: ![LA.Vector Double]                           -- ^ [日本語]: 予測子ごとの訓練 x (列名順)。 [English]: Training x per predictor (in column-name order).
  , gprnNames   :: ![Text]                                       -- ^ [日本語]: 予測子名 (列名順)。 [English]: Predictor names (in column-name order).
  , gprnYraw    :: !(LA.Vector Double)                           -- ^ [日本語]: 訓練応答 y (profiler 実測点の重ね用)。 [English]: Training response y (for overlaying the profiler's observed points).
  , gprnPredict :: !(LA.Matrix Double -> ([Double], Maybe [Double])) -- ^ [日本語]: testX (m×p) → (μ̂, Maybe 分散)。 [English]: testX (m×p) → (μ̂, Maybe variance).
  }

-- ===========================================================================
-- 罰則付き回帰 (描画可能)
-- ===========================================================================

-- | [日本語]: 罰則の種類 (実装済み全 7 種)。 追加パラメータも型に載せる。
--   [English]: Kinds of penalty (all 7 implemented kinds). Extra parameters
--   are carried in the type too.
data RegMethod
  = Ridge                      -- ^ [日本語]: L2。 [English]: L2.
  | Lasso                      -- ^ [日本語]: L1。 [English]: L1.
  | ElasticNet    !Double      -- ^ [日本語]: α = L1 比 (0..1)。 [English]: α = L1 ratio (0..1).
  | MCP           !Double      -- ^ [日本語]: γ concavity (推奨 ≥3)。 [English]: γ concavity (recommended ≥3).
  | SCAD          !Double      -- ^ [日本語]: a (推奨 3.7)。 [English]: a (recommended 3.7).
  | AdaptiveLasso !Double      -- ^ [日本語]: OLS pilot weight 指数 γ。 [English]: OLS pilot weight exponent γ.
  | GroupLasso    ![Int]       -- ^ [日本語]: 各列の群 ID (列名順・長さ = 列数)。 [English]: Group ID for each column (in column-name order, length = column count).
  deriving (Eq, Show)

-- | [日本語]: 当てはめ済の罰則回帰モデル。 係数は __元スケール__ (intercept + 特徴ごと)。
--   [English]: A fitted penalized regression model. Coefficients are on the
--   __original scale__ (intercept + per feature).
data RegModel = RegModel
  { rmgMethod    :: !RegMethod
  , rmgNames     :: ![Text]                       -- ^ [日本語]: 説明変数名 (列順)。 [English]: Explanatory variable names (in column order).
  , rmgLambda    :: !Double                        -- ^ [日本語]: 選択された λ。 [English]: The selected λ.
  , rmgIntercept :: !Double                        -- ^ [日本語]: β₀ (元スケール)。 [English]: β₀ (original scale).
  , rmgCoefs     :: ![Double]                      -- ^ [日本語]: β (元スケール・特徴ごと・長さ = 列数)。 [English]: β (original scale, per feature, length = column count).
  , rmgFitStd    :: !RegFit                        -- ^ [日本語]: 標準化空間の fit (診断用)。 [English]: The fit in standardized space (for diagnostics).
  , rmgCVPath    :: !(Maybe ([Double], [Double]))  -- ^ [日本語]: (λ grid, CV/LOOCV スコア)・自動選択時のみ。 [English]: (λ grid, CV\/LOOCV score); only when auto-selected.
  , rmgXraw      :: !(LA.Matrix Double)            -- ^ [日本語]: 生設計行列 (特徴のみ・intercept 列なし)。bootstrap refit 用。 [English]: Raw design matrix (features only, no intercept column). For bootstrap refit.
  , rmgYraw      :: !(LA.Vector Double)            -- ^ [日本語]: 生応答 y。bootstrap refit 用。 [English]: Raw response y. For bootstrap refit.
  }

-- | [日本語]: 新規データ (各行が p 次元の特徴ベクトル) での予測 @ŷ = β₀ + Σ βⱼ xⱼ@。
--   [English]: Prediction on new data (each row a p-dimensional feature
--   vector), @ŷ = β₀ + Σ βⱼ xⱼ@.
regPredict :: RegModel -> [[Double]] -> [Double]
regPredict m rows = [ rmgIntercept m + sum (zipWith (*) (rmgCoefs m) r) | r <- rows ]
