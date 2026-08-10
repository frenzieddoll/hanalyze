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
-- 描画ラッパ型 + smart constructor の plot 非依存層。
--
-- 'Hanalyze.Plot' (cabal flag @plot-integration@ 配下) が描画
-- ('VisualSpec' 化・'Plottable' instance) を担うのに対し、 本モジュールは
-- そこから **hgg に依存しない** 部分 (フィット済みモデルを束ねた
-- 描画ラッパ型と、 それを組み立てる smart constructor) のみを切り出したもの。
-- 非ゲート (常時 build) なので 'Graphics.Hgg.*' を一切 import しない。
-- 'Hanalyze.Plot' は本モジュールを import し従来の名前で再 export する。
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

-- | 多変量 effect plot で「動かす変数」 (along)。 'statModelMulti' の必須引数。
-- 型で単/多変量を分離し along 忘れをコンパイル時に弾く (§3 確定設計)。
newtype AlongSpec = AlongSpec Text

-- | along 変数を指定する。 @statModelMulti m (along \"x1\")@。
along :: Text -> AlongSpec
along = AlongSpec

-- | 多変量 effect で along 以外の説明変数をどう固定するか (既定 'Mean')。
--
--   * 'Mean' \/ 'Median' = 連続変数の集約 (factor 列は自動で 'Mode' に振替)。
--   * 'Mode' = 最頻 (連続は丸め最頻、 factor は最頻水準)。
--   * 'Reference' = factor の参照水準 (昇順先頭。 連続は 'Mean' に振替)。
--   * 'Marginalize' = 固定せず観測分布で周辺化 (PDP\/AME。 全観測行 × grid で重く、
--     band は提供しない = 曲線のみ)。
--   * 'Fixed' = 明示指定 (部分指定可。 指定の無い変数は 'Mean')。
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

-- | 帯モードセレクタ (Phase 70.F)。 出す帯を 1 つの値で選ぶ ('bandMode' で指定):
--
--   * @BandOff@  = 帯なし (曲線のみ)。
--   * @BandCI@   = 信頼区間のみ (平均 E[y|x] の不確実性・**既定**)。
--   * @BandPI@   = 予測区間のみ (新規観測 1 点が入る区間・σ̂² を含むぶん広い)。
--   * @BandCIPI@ = CI + PI を入れ子で重ねる (外=PI 薄・内=CI 濃・ファンチャート)。
--
-- PI 非提供モデル (Robust\/GAM\/Quantile\/非 Gaussian GLM\/effect plot) では
-- @BandPI@\/@BandCIPI@ は CI へフォールバックする ('svGridPI' = 'Nothing')。
data BandMode = BandOff | BandCI | BandPI | BandCIPI
  deriving (Eq, Show)

-- | 帯の算出法 (Phase 70.H・'piMethod' で指定)。 @PIClosedForm@ = 閉形式 (既定)、
--   @PIBootstrap seed draws@ = case-resampling ブートストラップ (seed 決定的・draws 回)。
data PIMethod = PIClosedForm | PIBootstrap !Word32 !Int
  deriving (Eq, Show)

-- ===========================================================================
-- 応答曲面オプション
-- ===========================================================================

-- | 'surfaceOf' / 'surfaceGrid' のオプション。
data SurfaceOpts = SurfaceOpts
  { soN      :: Int                     -- ^ 各軸の grid 点数 (既定 40)。
  , soHoldAt :: HoldAgg                 -- ^ 他変数の固定方式 (既定 'Mean')。
  , soXRange :: Maybe (Double, Double)  -- ^ v1 範囲 (既定 = 観測 min\/max)。
  , soYRange :: Maybe (Double, Double)  -- ^ v2 範囲 (既定 = 観測 min\/max)。
  }

defaultSurfaceOpts :: SurfaceOpts
defaultSurfaceOpts = SurfaceOpts
  { soN = 40, soHoldAt = Mean, soXRange = Nothing, soYRange = Nothing }

-- ===========================================================================
-- 多変量モデル型 (effect plot 用、 新規 fit)
-- ===========================================================================

-- | formula + 'DataFrame' で fit した多変量線形モデル (effect plot 用)。
data MultiLMModel = MultiLMModel
  { mlmFormula :: Formula           -- ^ R\/独自 formula (評価点設計行列の組み立てに保持)。
  , mlmFrame   :: ModelFrame        -- ^ 訓練 frame (along range + 他変数集約元)。
  , mlmDesign  :: LA.Matrix Double  -- ^ 訓練設計行列 X ('confidenceBandAt' の分散核)。
  , mlmResult  :: FitResult         -- ^ OLS 結果。
  }

-- | formula 文字列 (例 @\"y ~ x1 + x2 + x3\"@) と 'DataFrame' から多変量 LM を組む。
multiLMModel :: Text -> DX.DataFrame -> Either String MultiLMModel
multiLMModel fml df = parseModel fml >>= \f -> multiLMModelF f df

-- | 既に組み上げた 'Formula' (parse 済 or 'additiveFormula' で直接合成) から多変量 LM を
--   組む。 重回帰 spec ('lmMulti') が parse を経ずに使う経路 (Phase 70.D)。
multiLMModelF :: Formula -> DX.DataFrame -> Either String MultiLMModel
multiLMModelF f df = do
  mf         <- modelFrame f df
  (x, _)     <- designMatrixF f mf
  (res, _)   <- fitLMF f df
  Right MultiLMModel { mlmFormula = f, mlmFrame = mf, mlmDesign = x, mlmResult = res }

-- | formula + 'DataFrame' で fit した多変量 GLM (effect plot 用)。 帯は μ スケールで非対称。
data MultiGLMModel = MultiGLMModel
  { mglmFormula :: Formula           -- ^ formula (評価点設計行列に保持)。
  , mglmFrame   :: ModelFrame        -- ^ 訓練 frame。
  , mglmResult  :: FitResult         -- ^ 'fitGLMFull' の結果 (β\/μ̂)。
  , mglmSigma   :: LA.Matrix Double  -- ^ 逆 Fisher 情報 Σ (CI 用)。
  , mglmFamily  :: Family            -- ^ 分布族。
  , mglmLink    :: LinkFn            -- ^ リンク関数。
  }

-- | family\/link + formula 文字列 + 'DataFrame' から多変量 GLM を組む。
multiGLMModel :: Family -> LinkFn -> Text -> DX.DataFrame -> Either String MultiGLMModel
multiGLMModel family link fml df =
  parseModel fml >>= \f -> multiGLMModelF family link f df

-- | 既に組み上げた 'Formula' から多変量 GLM を組む (重回帰 spec 'glmMulti' 用・Phase 70.D)。
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

-- | 応答列 @y@ と説明変数列名 @[x1,…,xp]@ から加法線形 'Formula' を直接合成する。
--   RHS = @_p0 + _p1*x1 + … + _pp*xp@ (切片 + 各変数の主効果)。 数値列前提
--   (factor / 交互作用 / 変換が要るなら formula 版 'lmF' を使う)。
additiveFormula :: Text -> [Text] -> Formula
additiveFormula y xs = Formula
  { formResponse = y
  , formDataVars = xs
  , formRHS      = foldl1 (Bin Add)
      (Ref "_p0" : zipWith (\i x -> Bin Mul (Ref ("_p" <> tshowInt i)) (Ref x))
                           [1 :: Int ..] xs) }
  where tshowInt = T.pack . show

-- | formula + 'DataFrame' で fit した多変量ロバスト回帰 (effect plot 用)。
data MultiRobustModel = MultiRobustModel
  { mrmEstimator :: RobustEstimator   -- ^ Huber k or Tukey c。
  , mrmFormula   :: Formula           -- ^ 評価点設計行列の組み立てに保持。
  , mrmFrame     :: ModelFrame        -- ^ 訓練 frame (along range + 他変数集約元)。
  , mrmDesign    :: LA.Matrix Double  -- ^ 訓練設計行列 X (サンドイッチ共分散の核)。
  , mrmFit       :: RobustFit         -- ^ 'fitRobustLM' の結果 (β̂ / 重み / スケール)。
  }

-- | 'Formula' + 'DataFrame' から多変量ロバスト回帰を組む (重回帰 spec 'robustMulti' 用)。
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

-- | PLS の effect plot 用ラッパ (Phase 70.B2)。 'PLSFit' は列名/'ModelFrame' を
-- 持たないので、 'statModelMulti' (along/holdAt/byVar) を効かせるために訓練 frame と
-- 列順・出力選択を保持する。 'MultiLMModel' と同型 (frame-carrying wrapper)。
data PLSModel = PLSModel
  { plsmFit    :: !PLSFit       -- ^ 学習済 PLS。
  , plsmFrame  :: !ModelFrame   -- ^ 訓練 frame (X 列 = 'RoleContinuous'・along range/hold の元)。
  , plsmXNames :: ![Text]       -- ^ X 列名 ('predictPLS' へ渡す列順)。
  , plsmYNames :: ![Text]       -- ^ Y 出力名 (出力セレクタ 'selectOutput' 用)。
  , plsmOutIdx :: !Int          -- ^ effect plot に描く Y 出力列 index (既定 0)。
  }

-- | 列名指定で PLS effect plot 用モデルを組む。 @plsModel cfg xcols ycols df@。
--   学習は 'fitPLS'、 frame は X 列を 'RoleContinuous' として手組み (応答ダミー)。
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

-- | 描く Y 出力列を名前で選ぶ (多出力 PLS 用・既定は第 0 出力)。
--   @statModelMulti (selectOutput \"y2\" m) (along \"x1\")@。 名前が無ければ無変更。
selectOutput :: Text -> PLSModel -> PLSModel
selectOutput yname m =
  case lookup yname (zip (plsmYNames m) [0 ..]) of
    Just i  -> m { plsmOutIdx = i }
    Nothing -> m

-- ===========================================================================
-- 線形モデル (描画可能)
-- ===========================================================================

-- | X を束ねた描画可能な単回帰モデル。
data LMModel = LMModel
  { lmDesign :: LA.Matrix Double  -- ^ 設計行列 X @n × p@ (intercept 列含む)。
  , lmResult :: FitResult         -- ^ 'fitLMVec' の結果 (β / ŷ / 残差 / R²)。
  , lmXraw   :: LA.Vector Double  -- ^ 散布図 x 軸の生 predictor @n@ (単回帰の x)。
  }

-- | 単回帰 @(x, y)@ から 'LMModel' を組む。 設計行列は @[1, x]@、 fit は 'fitLMVec'。
lmModel :: LA.Vector Double -> LA.Vector Double -> LMModel
lmModel xs ys =
  let dm  = designMatrix (V.fromList (LA.toList xs))  -- designMatrix は boxed Vector 入力
      res = fitLMVec dm ys
  in LMModel { lmDesign = dm, lmResult = res, lmXraw = xs }

-- | X と family/link を束ねた描画可能な単回帰 GLM。
data GLMModel = GLMModel
  { glmDesign :: LA.Matrix Double  -- ^ 設計行列 X @n × p@ (intercept 列含む)。
  , glmResult :: FitResult         -- ^ 'fitGLMFull' の結果 (β / μ̂ / 残差)。
  , glmSigma  :: LA.Matrix Double  -- ^ 逆 Fisher 情報 Σ=(XᵀWX)⁻¹ (CI 用)。
  , glmFamily :: Family            -- ^ 分布族 (帯の意味付けに保持)。
  , glmLink   :: LinkFn            -- ^ リンク関数 (μ スケールへの逆変換に必要)。
  , glmXraw   :: LA.Vector Double  -- ^ 散布図 x 軸の生 predictor @n@ (単回帰の x)。
  }

-- | 単回帰 @(x, y)@ と family/link から 'GLMModel' を組む。 設計行列は @[1, x]@、
-- fit は 'fitGLMFull' (FitResult と逆 Fisher 情報 Σ の両方を返す)。
glmModel :: Family -> LinkFn -> LA.Vector Double -> LA.Vector Double -> GLMModel
glmModel family link xs ys =
  let dm           = designMatrix (V.fromList (LA.toList xs))
      (res, sigma) = fitGLMFull family link dm ys
  in GLMModel { glmDesign = dm, glmResult = res, glmSigma = sigma
              , glmFamily = family, glmLink = link, glmXraw = xs }

-- | X (生 predictor) を束ねた描画可能なスプライン回帰モデル。
--
-- 'SplineFit' は基底行列を保持しないので、 'confidenceBand' / 散布図用に生 x を別途
-- 束ねる (= LMModel と同型の「描画可能なモデル」 化)。
data SplineModel = SplineModel
  { splFit  :: SplineFit          -- ^ 'fitSpline' の結果 (basis 係数 + 線形核)。
  , splXraw :: LA.Vector Double   -- ^ 散布図 x 軸の生 predictor @n@。
  }

-- | @(x, y)@ と spline 種別・ノットから 'SplineModel' を組む。
splineModel
  :: SplineKind        -- ^ B-spline (次数) or 自然 3 次スプライン。
  -> [Double]          -- ^ 内部ノット (境界含む)。
  -> LA.Vector Double  -- ^ 説明変数 x。
  -> LA.Vector Double  -- ^ 応答 y。
  -> SplineModel
splineModel kind knots xs ys =
  let xsV = V.fromList (LA.toList xs)
      ysV = V.fromList (LA.toList ys)
      fit = fitSpline kind knots xsV ysV
  in SplineModel { splFit = fit, splXraw = xs }

-- | X (単一 predictor の生 x) を束ねた描画可能な単変量 GAM。
data GAMModel = GAMModel
  { gamFit  :: GAMFit             -- ^ 'fitGAM' の結果 (基底係数 + fitted)。
  , gamXraw :: LA.Vector Double   -- ^ 散布図 x 軸の生 predictor @n@ (単変量の x)。
  }

-- | 単変量 @(x, y)@ から 'GAMModel' を組む。 内部で 1 特徴の 'fitGAM' を呼ぶ。
gamModel
  :: Int               -- ^ B-spline 次数 (3 = cubic 推奨)。
  -> Int               -- ^ 内部ノット数 (例 5)。
  -> Double            -- ^ ridge 罰則 λ (0 で無効)。
  -> LA.Vector Double  -- ^ 説明変数 x。
  -> LA.Vector Double  -- ^ 応答 y。
  -> GAMModel
gamModel degree nKnots lambda xs ys =
  let xsV = V.fromList (LA.toList xs)
      ysV = V.fromList (LA.toList ys)
      fit = fitGAM degree nKnots lambda [xsV] ysV
  in GAMModel { gamFit = fit, gamXraw = xs }

-- | df|-> 由来の (多予測子) GAM。 第1予測子を描画軸にする。
data GAMModelN = GAMModelN
  { gamNFit   :: GAMFit              -- ^ 'fitGAMAuto' の結果。
  , gamNXraws :: [LA.Vector Double]  -- ^ 予測子ごとの訓練 x (列名順)。
  , gamNNames :: [Text]             -- ^ 予測子名 (列名順)。
  }

-- | X (生 predictor) を束ねた描画可能な単回帰ロバストモデル。
data RobustModel = RobustModel
  { rmFit  :: RobustFit           -- ^ 'fitRobustLM' の結果 (β̂ / ŷ / 重み)。
  , rmXraw :: LA.Vector Double    -- ^ 散布図 x 軸の生 predictor @n@ (単回帰の x)。
  }

-- | 単回帰 @(x, y)@ と estimator から 'RobustModel' を組む。 設計行列は @[1, x]@、
-- fit は 'fitRobustLM' (max 50 iter / tol 1e-6)。
robustModel
  :: RobustEstimator   -- ^ Huber k or Tukey c。
  -> LA.Vector Double  -- ^ 説明変数 x。
  -> LA.Vector Double  -- ^ 応答 y。
  -> RobustModel
robustModel est xs ys =
  let dm  = designMatrix (V.fromList (LA.toList xs))
      fit = fitRobustLM est dm ys 50 1e-6
  in RobustModel { rmFit = fit, rmXraw = xs }

-- | X と複数 τ の fit を束ねた描画可能な分位点回帰モデル。
data QuantileModel = QuantileModel
  { qmFits :: [(Double, QRFit)]   -- ^ (τ, その fit) の並び (τ 昇順を推奨)。
  , qmXraw :: LA.Vector Double    -- ^ 散布図 x 軸の生 predictor @n@ (単回帰の x)。
  }

-- | 単回帰 @(x, y)@ と分位水準 τ のリストから 'QuantileModel' を組む。 設計行列は @[1, x]@、
-- 各 τ を 'fitQuantile' で fit。
quantileModel
  :: [Double]          -- ^ 分位水準 τ ∈ (0,1) のリスト (例 [0.1, 0.5, 0.9])。
  -> LA.Vector Double  -- ^ 説明変数 x。
  -> LA.Vector Double  -- ^ 応答 y。
  -> QuantileModel
quantileModel taus xs ys =
  let dm   = designMatrix (V.fromList (LA.toList xs))
      fits = [ (t, fitQuantile t dm ys) | t <- taus ]
  in QuantileModel { qmFits = fits, qmXraw = xs }

-- | 多変量 (重回帰) 分位点回帰の結果。 設計行列 @[1, x₁..xₚ]@ に各 τ で 'fitQuantile' を
--   当てた fit 群を保持する。 係数は @qfBeta@ ('mqmFits' の各 'QRFit') で取り出す
--   (分位点回帰は SE を持たないため 'coefSummary' は非対応・単変量 'quantile' と一貫)。
data MultiQuantileModel = MultiQuantileModel
  { mqmTaus  :: ![Double]            -- ^ 分位水準 τ の並び。
  , mqmNames :: ![Text]             -- ^ 予測子名 (intercept を除く・設計行列の 2 列目以降と対応)。
  , mqmFits  :: ![(Double, QRFit)]   -- ^ 各 τ の fit (係数 'qfBeta' = @[β₀, β₁, …, βₚ]@)。
  , mqmX     :: !(LA.Matrix Double)  -- ^ 設計行列 @[1, x₁, …, xₚ]@ (effect plot の評価元)。
  }

-- ===========================================================================
-- MCMC チェーン (描画可能)
-- ===========================================================================

-- | 1 パラメータを選んだ描画可能な MCMC チェーン。
data ChainModel = ChainModel
  { cmChain :: Chain   -- ^ サンプラ出力 (post-burn-in)。
  , cmParam :: Text    -- ^ 描画対象のパラメータ名。
  }

-- | パラメータ名と 'Chain' から 'ChainModel' を組む。
chainModel :: Text -> Chain -> ChainModel
chainModel name ch = ChainModel { cmChain = ch, cmParam = name }

-- ===========================================================================
-- HBM (ベイズ確率プログラム) の学習 — Phase 49 A1
-- ===========================================================================

-- | HBM 学習の設定。 NUTS の chain 数 / 本サンプル数 / warmup を保持する
-- (brms 既定 = 4 chains × 1000 draws + 1000 warmup に相当)。 'hbmSeed' は
-- 純粋化 (将来の ST 版 @hbmModelPure seed …@) の継ぎ目として今から署名に持つ。
data HBMConfig = HBMConfig
  { hbmChains    :: !Int            -- ^ chain 数 (既定 4)。
  , hbmSamples   :: !Int            -- ^ 本サンプル数 = post-warmup draws (既定 1000)。
  , hbmWarmup    :: !Int            -- ^ warmup / burn-in (既定 1000)。
  , hbmSeed      :: !(Maybe Word32) -- ^ 乱数シード (現状は IO 内で消費・将来 ST の継ぎ目)。
  , hbmAdaptMass :: !Bool           -- ^ 対角質量行列の適応 (既定 True・brms/PyMC 同様)。
                                     --   a/b と s のようにスケールが大きく異なる posterior で
                                     --   収束 (特に scale param) に必須。 OFF だと s が未収束に
                                     --   なりやすい (Phase 52.A12 で計測確認)。
  , hbmWarmupInitMaxDepth :: !(Maybe Int)
                                     -- ^ Phase 96 A5: 'nutsWarmupInitMaxDepth' の pass-through
                                     --   (opt-in・既定 'Nothing' = 無効)。 質量行列の初回更新前
                                     --   (M=I 期間) の tree depth 上限。 warmup 初期の ε 鋸歯で
                                     --   deep tree を掘る浪費 (05-mh 実測で warmup evals が
                                     --   nutpie 比 1.92×) を 'Just' 6 等で抑制する。 参照実装に
                                     --   無いヒューリスティックゆえ既定 OFF (NUTS.hs 側と同判断)。
  } deriving (Show, Eq)

-- | 既定の HBM 設定: 4 chains × 1000 draws + 1000 warmup (brms 既定相当)。 質量行列適応 ON。
defaultHBM :: HBMConfig
defaultHBM = HBMConfig
  { hbmChains    = 4
  , hbmSamples   = 1000
  , hbmWarmup    = 1000
  , hbmSeed      = Nothing
  , hbmAdaptMass = True
  , hbmWarmupInitMaxDepth = Nothing
  }

-- | 学習済 HBM モデル。 data placeholder を bind したモデル本体 ('hbmModelSpec')、
-- posterior draws ('hbmChainsR' = chain 群)、 bind 済みデータ ('hbmData') を保持する。
-- ★ 抽出子 ('epred' 等) はここから純粋に図を組む (= @df |>>@ と整合)。
data HBMModel = HBMModel
  { hbmModelSpec :: ModelP ()          -- ^ data を bind 済みのモデル (epred 評価でも使う)。
  , hbmChainsR   :: ![Chain]           -- ^ posterior draws (chain 群)。
  , hbmData      :: ![(Text, [Double])] -- ^ bind 済みデータ列 (列名 → 値)。
  , hbmFactorLevels :: ![(Text, [Text])]
    -- ^ Phase 60.3: 'dataNamedIx' slot に Text factor 列を bind した場合の
    --   sort 順 levels (slot 名 → levels)。 コード i = levels !! i で、
    --   indexed パラメータ (b0_2 等) がどの群かを引ける。 数値列 bind は空。
  }

-- | 列名→値の組をモデル中の data placeholder に順に 'withData' で bind する。
-- 明示再帰 (foldr ではなく) なのは、 'ModelP' が rank-2 多相 ('forall a.') ゆえ
-- ImpredicativeTypes 下の foldr では accumulator が単相化してしまうため。
bindCols :: [(Text, [Double])] -> ModelP r -> ModelP r
bindCols []             m = m
bindCols ((n, vs):rest) m = bindCols rest (withData n vs m)

-- | 'bindCols' の 'DataIx' 版 (Phase 60.3): 列名→index 列を 'withDataIx' で bind。
bindIxCols :: [(Text, [Int])] -> ModelP r -> ModelP r
bindIxCols []             m = m
bindIxCols ((n, is):rest) m = bindIxCols rest (withDataIx n is m)

-- | 確率プログラム 'ModelP' を NUTS で学習し 'HBMModel' にする (MCMC ゆえ IO)。
--
-- 列名で 'withData' を畳み込み、 モデル中の placeholder ('dataNamed' / observe の
-- 参照名) を df 由来の実データに差し替える (PyMC @set_data@ 同型)。 chain は既存
-- 'nutsChains' が並列実行 (実 OS スレッド並列には @-threaded +RTS -N@ が要る)。
--
-- 当面の入口は @[(Text,[Double])]@ (列名→値)。 'ColumnSource' 一般化
-- (Map/DataFrame/assoc 疎結合) は別 Phase (データ API)。
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

-- | NUTS の初期点 (制約空間)。 positive 制約 (σ 等) を 0 で初期化すると @log 0 = -∞@ で
-- 初手から全 proposal が divergence する (実測 2026-06-05)。 'getTransforms' で制約を検出し
-- PositiveT→1 / UnitIntervalT→0.5 / 他→0 で初期化する。
hbmInitPoint :: ModelP () -> Map.Map Text Double
hbmInitPoint bound = Map.map initFor (getTransforms bound)
  where
    initFor PositiveT      = 1.0
    initFor UnitIntervalT  = 0.5
    initFor UnconstrainedT = 0.0

-- | 'HBMConfig' → 'NUTSConfig'。 NUTS は @total = burnIn + iterations@ を回し iterations 本
-- だけ保持するので、 iterations に本サンプル数・burnIn に warmup を割り当てる。
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

-- | 純粋・決定的な HBM 学習 (Phase 50.4)。 'hbmModel' の ST/seed 版で IO を持たない。
-- 'nutsChainsPure' (chain 横断を spark 並列・seed で再現可能) を使う。 @hbmSeed@ が
-- 'Nothing' のときは固定既定 seed (42) を用いる (純粋・決定的を保証する設計判断)。
hbmModelPure :: HBMConfig -> ModelP () -> [(Text, [Double])] -> HBMModel
hbmModelPure cfg model dat = hbmModelPureWith cfg model dat [] []

-- | 'hbmModelPure' の拡張形 (Phase 60.3): 'DataIx' slot の index 列と
-- Text factor levels も bind する。 'df |-> hbm' ('Fit' instance) が
-- 'resolveIxSlots' で解決した結果を渡す主経路。
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

-- | 'hbmModelPure' の IO 版 (Phase 61.3): stderr に進捗を表示しながら学習する。
-- bind + seed 規約は 'hbmModelPureWith' と同一・chain ごとの seed は
-- 'chainSeeds' 共有 ('nutsChainsStream') ゆえ、 結果は同 cfg の
-- 'hbmModelPure' と**ビット一致**する (test-plot で固定)。
hbmModelIO :: HBMConfig -> ModelP () -> [(Text, [Double])] -> IO HBMModel
hbmModelIO cfg model dat = hbmModelIOWith cfg model dat [] []

-- | 'hbmModelPureWith' の IO + 進捗表示版 (Phase 61.3)。 '(|->!)' の
-- HBM 経路 ('fitIO') が 'resolveIxSlots' の解決結果を渡す主経路。
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

-- | 要約対象のパラメタ名 (latent 宣言順 → deterministic 宣言順の連結)。
-- deterministic 派生量を既定で含めるのは PyMC/arviz の @az.summary@ が
-- Deterministic を含むのと同型 (Phase 103 A1 確定)。
hbmSummaryNames :: HBMModel -> [Text]
hbmSummaryNames m = sampleNames spec ++ deterministicNames spec
  where spec :: ModelP ()
        spec = hbmModelSpec m

-- | deterministic 派生量を注入済みの chain 群。派生量が無いモデルでは
-- augment (全 draw の再評価) を省いて素の chain を返す。
hbmAugmentedChains :: HBMModel -> [Chain]
hbmAugmentedChains m
  | null (deterministicNames spec) = hbmChainsR m
  | otherwise = map (augmentChainWithDeterministic spec) (hbmChainsR m)
  where spec :: ModelP ()
        spec = hbmModelSpec m

-- | 学習済 HBM の事後要約表 (@az.summary@ 相当・純粋)。
-- mean / sd / HDI / ess_bulk (+ multi-chain 時 r_hat) を latent +
-- deterministic の全パラメタについて返す。
hbmSummary :: HBMModel -> [SummaryRow]
hbmSummary m = posteriorSummary (hbmSummaryNames m) (hbmAugmentedChains m)

-- | 'hbmSummary' をコンソール表として表示する。
printHBMSummary :: HBMModel -> IO ()
printHBMSummary m = printPosteriorSummary (hbmSummaryNames m) (hbmAugmentedChains m)

-- | 'hbmSummary' の DataFrame 化。列 = param / mean / sd / hdi_lo / hdi_hi /
-- ess_bulk (+ multi-chain 時のみ r_hat = 'printPosteriorSummary' の列規約と同じ)。
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

-- | 事後 draw の DataFrame 化 (1 パラメタ = 1 列・全 chain を chain 順に連結、
-- deterministic 派生量込み)。'Hanalyze.Data.Wrangle' の
-- @summarise@ / @groupBy@ 等で自由集計する入口。
hbmDrawsDf :: HBMModel -> DX.DataFrame
hbmDrawsDf m =
  let chains = hbmAugmentedChains m
  in DX.fromNamedColumns
       [ (n, DX.fromList (concatMap (chainVals n) chains))
       | n <- hbmSummaryNames m ]

-- ===========================================================================
-- 時系列予測 (描画可能)
-- ===========================================================================

-- | 履歴系列と AR fit・予測地平を束ねた描画可能な時系列予測モデル。
data ForecastModel = ForecastModel
  { fmFit     :: ARFit             -- ^ 'fitAR' の結果。
  , fmHistory :: LA.Vector Double  -- ^ 観測系列 (時系列順)。
  , fmHorizon :: Int               -- ^ 予測地平 h。
  }

-- | 系列・AR 次数・地平から 'ForecastModel' を組む ('fitAR' で fit)。
forecastModel
  :: Int               -- ^ AR 次数 p。
  -> Int               -- ^ 予測地平 h。
  -> LA.Vector Double  -- ^ 観測系列 (時系列順)。
  -> ForecastModel
forecastModel order horizon series =
  ForecastModel { fmFit = fitAR order series, fmHistory = series
                , fmHorizon = horizon }

-- ===========================================================================
-- データ源 → モデル を当てはめる統一型クラス。
-- ===========================================================================

-- | データ源 → モデル を当てはめる統一型クラス。
--
-- 'fitWith' / '(|->)' は **pure だが total ではない** (列欠落・parse 失敗は
-- 'error')。 検証パイプライン用に total な 'fitEither' を併設する
-- (既定の 'fitWith' は 'fitEither' を 'error' で潰したもの)。
class Fit spec where
  -- | この spec を当てはめた結果のモデル型。
  type Fitted spec
  -- | 当てはめ (pure・失敗は 'error')。 既定実装は 'fitEither' 経由。
  fitWith   :: ColumnSource d => spec -> d -> Fitted spec
  fitWith spec d = either error id (fitEither spec d)
  -- | 当てはめ (total・失敗は 'Left')。
  fitEither :: ColumnSource d => spec -> d -> Either String (Fitted spec)
  -- | 当てはめ (IO・進捗表示など副作用つき学習・Phase 61.4)。 既定 =
  -- @pure . fitWith@ で純粋 spec は挙動不変 (失敗の error 意味論も '(|->)'
  -- と同じ)。 学習が重い spec ('HBMSpec') だけ override して進捗を出す。
  fitIO     :: ColumnSource d => spec -> d -> IO (Fitted spec)
  fitIO spec d = pure (fitWith spec d)
  -- | 透過標準化ラッパ ('standardized' / 'standardizedY') が **標準化対象とする
  -- 予測子列名** (Phase 70.3 項目 C)。 既定は @[]@ = 「被せる意味の無い spec」
  -- であり、 'standardized' を付けても 'fitEither' が 'Left' で誤用を弾く。
  -- 距離ベース (kNN) や線形 (整形目的) の spec だけが実列名を返す。 内部標準化済
  -- ('GPSpec' \/ 'RegSpec' \/ 'PCASpec' \/ 'PLSSpec') と木系は二重標準化\/無意味回避で
  -- 既定 @[]@ のまま (= ラッパ拒否)。
  predictorCols :: spec -> [Text]
  predictorCols _ = []
  -- | 透過標準化ラッパが y も標準化する ('standardizedY') 際の**応答列名**
  -- (Phase 70.3 項目 C)。 既定 'Nothing'。 **連続応答の回帰 spec のみ** @Just@ を返す。
  -- 分類 (クラスラベル) や family\/link でスケールが拘束される GLM は標準化が不正ゆえ
  -- 'Nothing' のまま (= 'standardizedY' を付けると 'fitEither' が 'Left')。
  responseCol :: spec -> Maybe Text
  responseCol _ = Nothing

-- | 因果探索 (LiNGAM) の高レベル @df |->@ 結果ラッパ (Phase 77)。 各 LiNGAM fit 型は
--   変数名を持たないため、 学習した fit @a@ に**変数名** (@df |->@ が渡した列名) を添える。
--   'Plottable' (@Hanalyze.Plot.ML@) が @lfNames@ を DAG ノード名に使う
--   (無ければ @x0..@ フォールバック)。 侵襲的な per-fit-型 names フィールド追加を避ける汎用ラッパ。
data LiNGAMFitted a = LiNGAMFitted
  { lfFit   :: !a        -- ^ 各 variant の fit 結果 ('DirectLiNGAMFit' 等)
  , lfNames :: ![Text]   -- ^ 変数名 (行列の列順 = fit の変数 index 順)
  } deriving (Show)

-- | 列名で数値列を引き 'LA.Vector' 化 (無ければ 'Left')。 二変量近道の素経路。
reqColV :: ColumnSource d => Text -> d -> Either String (LA.Vector Double)
reqColV n d = case lookupCol n d of
  Just xs -> Right (LA.fromList xs)
  Nothing -> Left ("ColumnSource: 列が見つかりません: " <> T.unpack n)

-- | 複数の列名から @n × p@ 行列を組む (各列名 = 1 変数 = 行列の 1 列・行=標本)。
--   行列入力モデル (PCA \/ PLS \/ …) を列名 spec で高レベル化する際の素経路
--   (Phase 70.A)。 列が 1 つも無い / 長さ不揃いは 'Left'。
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

-- | WLS の結果。 内側 'LMModel' は **√w スケール設計行列** ('lmDesign'=X_w) と
--   その OLS 結果 ('lmResult')、 **元の x** ('lmXraw') を保持する (grid 経路で正しい
--   WLS CI を出すための容れ物)。 weighted R² 算出用に**重み** と**元 y** も保持する。
data WeightedLMModel = WeightedLMModel
  { wlmInner   :: !LMModel    -- ^ √w スケール設計・OLS 結果・元 x。
  , wlmWeights :: ![Double]   -- ^ 重み w (元の行順)。
  , wlmY       :: ![Double]   -- ^ 元の応答 y (weighted R² 算出用)。
  }

-- | 透過標準化の結果。 内側モデル (標準化空間で学習) と逆変換に要る (μ,σ) を保持する。
--   'SingleVarModel' / 'Plottable' instance (Phase 70.3 C2) がこれを使い元スケール軸で描く。
data StandardizedModel m = StandardizedModel
  { smInner :: !m                            -- ^ 標準化空間で fit した内側モデル。
  , smXStd  :: !Standardizer                 -- ^ 予測子列の (μ,σ)。'Stat.Standardize'。
  , smYStd  :: !(Maybe (Double, Double))     -- ^ 応答 y の (μ,σ)。'standardizedY' 時のみ。
  , smTrain :: !(Maybe ([Double], [Double])) -- ^ 元スケール訓練 (x,y)。単変量散布図用 (予測子 1 列時のみ)。
  }

-- | 群別フィットの結果。 各群ラベル → その群の 'Fitted spec' を保持する
--   **実結果型** ('HBMModel' 同族・'ModelSpec' ではない)。 'groupModels' で取り出す。
newtype GroupedFit spec = GroupedFit { gfGroups :: [(Text, Fitted spec)] }

-- ===========================================================================
-- カーネル回帰 (描画可能)
-- ===========================================================================

-- | カーネル回帰の 4 象限。 @seed@/@D@ は RFF 近似コンストラクタにだけ載る。
-- KRR 象限は 'Krr'/'KrrRff' (Kernel Ridge Regression・線形罰則回帰の 'Ridge' と区別)。
data GPMethod
  = Gp                       -- ^ 厳密 GP   (分布あり・事後分散→帯)。
  | Krr                      -- ^ 厳密 KRR  (点・KRR ≡ GP 事後平均)。
  | GpRff  !Int !Word32      -- ^ RFF 近似 GP  (@D@ 特徴次元, seed)。
  | KrrRff !Int !Word32      -- ^ RFF 近似 KRR (@D@ 特徴次元, seed)。
  deriving (Eq, Show)

-- | ハイパラの「決め方 + (固定時のみ) 値」を一箇所に集約 (役割重複ゆえ別フィールドは持たない)。
data HyperStrategy
  = FixedHyper GPParams   -- ^ 固定: この 'GPParams' を使う (最適化しない)。
  | AutoMarginalLik       -- ^ 周辺尤度で自動 ('GP.optimizeGP'・初期値はデータ駆動)。
  | AutoCV                -- ^ LOOCV (PRESS) で自動 ('GP.autoCVHyperGP'・初期値は同上)。

-- | 当てはめ済の統合カーネル回帰モデル。 象限 + 解決済ハイパラ + 予測子を保持する。
-- 予測子 'gprPredict' は @grid x → (μ̂, Maybe 事後分散)@: 分布あり象限 (Gp/GpRff) は
-- @Just 分散@、 点象限 (Ridge/RidgeRff) は @Nothing@ (= 帯なし)。 'SingleVarModel' /
-- 'Plottable' instance は E2。
data GPRegModel = GPRegModel
  { gprMethod  :: !GPMethod                                  -- ^ Periodic フォールバック後の象限。
  , gprKernel  :: !Kernel                                    -- ^ カーネル種 (Periodic は不変)。
  , gprParams  :: !GPParams                                  -- ^ 解決済ハイパラ。
  , gprXraw    :: !(LA.Vector Double)                        -- ^ 訓練 x (svRange / 散布図)。
  , gprY       :: !(LA.Vector Double)                        -- ^ 訓練 y。
  , gprPredict :: !([Double] -> ([Double], Maybe [Double]))  -- ^ grid x → (μ̂, Maybe 事後分散)。
  }

-- | 当てはめ済の多変量カーネル回帰モデル。 予測子 'gprnPredict' は評価行列 (@m × p@) を
-- 取り @(μ̂, Maybe 事後分散)@ を返す (分布あり象限のみ Just)。
data GPRegModelN = GPRegModelN
  { gprnMethod  :: !GPMethod
  , gprnKernel  :: !Kernel
  , gprnParams  :: !GPParams
  , gprnXraws   :: ![LA.Vector Double]                           -- ^ 予測子ごとの訓練 x (列名順)。
  , gprnNames   :: ![Text]                                       -- ^ 予測子名 (列名順)。
  , gprnYraw    :: !(LA.Vector Double)                           -- ^ 訓練応答 y (profiler 実測点の重ね用)。
  , gprnPredict :: !(LA.Matrix Double -> ([Double], Maybe [Double])) -- ^ testX (m×p) → (μ̂, Maybe 分散)。
  }

-- ===========================================================================
-- 罰則付き回帰 (描画可能)
-- ===========================================================================

-- | 罰則の種類 (実装済み全 7 種)。 追加パラメータも型に載せる。
data RegMethod
  = Ridge                      -- ^ L2。
  | Lasso                      -- ^ L1。
  | ElasticNet    !Double      -- ^ α = L1 比 (0..1)。
  | MCP           !Double      -- ^ γ concavity (推奨 ≥3)。
  | SCAD          !Double      -- ^ a (推奨 3.7)。
  | AdaptiveLasso !Double      -- ^ OLS pilot weight 指数 γ。
  | GroupLasso    ![Int]       -- ^ 各列の群 ID (列名順・長さ = 列数)。
  deriving (Eq, Show)

-- | 当てはめ済の罰則回帰モデル。 係数は **元スケール** (intercept + 特徴ごと)。
data RegModel = RegModel
  { rmgMethod    :: !RegMethod
  , rmgNames     :: ![Text]                       -- ^ 説明変数名 (列順)。
  , rmgLambda    :: !Double                        -- ^ 選択された λ。
  , rmgIntercept :: !Double                        -- ^ β₀ (元スケール)。
  , rmgCoefs     :: ![Double]                      -- ^ β (元スケール・特徴ごと・長さ = 列数)。
  , rmgFitStd    :: !RegFit                        -- ^ 標準化空間の fit (診断用)。
  , rmgCVPath    :: !(Maybe ([Double], [Double]))  -- ^ (λ grid, CV/LOOCV スコア)・自動選択時のみ。
  , rmgXraw      :: !(LA.Matrix Double)            -- ^ 生設計行列 (特徴のみ・intercept 列なし)。bootstrap refit 用。
  , rmgYraw      :: !(LA.Vector Double)            -- ^ 生応答 y。bootstrap refit 用。
  }

-- | 新規データ (各行が p 次元の特徴ベクトル) での予測 @ŷ = β₀ + Σ βⱼ xⱼ@。
regPredict :: RegModel -> [[Double]] -> [Double]
regPredict m rows = [ rmgIntercept m + sum (zipWith (*) (rmgCoefs m) r) | r <- rows ]
