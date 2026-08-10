{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Hanalyze.Fit
-- Description : plot 非依存の df |-> spec 統一 fit 演算子と各種モデル spec 型
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: データ源 → モデル学習層 (= @df |-> spec@ の plot 非依存部分)。
--
-- 'Hanalyze.Plot' から __図 (@VisualSpec@) に依存しない__「データ源から
-- モデルを当てはめる」部分 (各 @*Spec@ 型・'Fit' instance・@(|->)@ / @(|->!)@
-- 演算子・それらの smart ctor) をここへ切り出した非ゲートモジュール (常時 build)。
-- これにより @df |-> lm "x" "y"@ 等が別パッケージ @hanalyze-plot@
-- (@cabal build --project-file=cabal.project.plot@) を build せずとも使える。
--
-- ⚠ 本モジュールは 'Graphics.Hgg' を一切 import しない (= 図に依存する toPlot /
-- Plottable instance 等は 'Hanalyze.Plot' 側に残す)。 'Fit' クラス本体・
-- ラッパ型・reqColV/reqColsM 等は 'Hanalyze.Model.Wrappers' から取り込む。
--
-- [English]: The data source → model training layer (the plot-independent
-- part of @df |-> spec@).
--
-- An ungated module (always built) carving out from 'Hanalyze.Plot'
-- the __figure-independent (@VisualSpec@)__ part of "fitting a model from a
-- data source" — each @*Spec@ type, the 'Fit' instances, the
-- @(|->)@ \/ @(|->!)@ operators, and their smart constructors. This lets
-- @df |-> lm \"x\" \"y\"@ etc. be used without building the separate
-- @hanalyze-plot@ package (@cabal build
-- --project-file=cabal.project.plot@).
--
-- Warning: this module never imports 'Graphics.Hgg' at all (figure-dependent
-- things like toPlot \/ Plottable instances stay on the
-- 'Hanalyze.Plot' side). The 'Fit' class body, wrapper types,
-- reqColV\/reqColsM, etc. are pulled in from 'Hanalyze.Model.Wrappers'.
module Hanalyze.Fit
  ( -- * df |-> spec 統一 fit 演算子 (Phase 51)
    (|->)
  , (|->!)
    -- * 二変量近道 spec (列名 2 つ) — Phase 51.2
  , LMSpec (..)
  , lm
  , GLMSpec (..)
  , glm
  , SplineSpec (..)
  , spline
  , RobustSpec (..)
  , rlm
  , QuantileSpec (..)
  , rq
    -- * 重回帰 spec (列名リスト・formula 不要) — Phase 70.D
  , LMMultiSpec (..)
  , lmMulti
  , GLMMultiSpec (..)
  , glmMulti
  , RobustMultiSpec (..)
  , rlmMulti
  , QuantileMultiSpec (..)
  , rqMulti
    -- * formula 多変量 spec (R 流) — Phase 51.3
  , LMFormulaSpec (..)
  , lmF
  , GLMFormulaSpec (..)
  , glmF
  , GLMMFormulaSpec (..)
  , glmmF
    -- * DOE ワークフロー (Phase 78・設計オブジェクト + designModel)
  , Design (..)
  , DesignFactor (..)
  , FactorKind (..)
  , FactorScale (..)
  , DesignKind (..)
  , contFactor
  , contFactorLog
  , numFactor
  , catFactor
  , CustomSpec (..)
  , customSpec
  , customDesign
  , Structure (..)
  , splitPlot
  , stripPlot
  , blocked
  , Constraint (..)
  , ConstraintRel (..)
  , ConstraintGuard (..)
  , FactorValue (..)
  , NatConstraint (..)
  , natLeq
  , natGeq
  , natEq
  , natForbid
  , formulaToCustomModel
  , factorialDesign
  , centralCompositeDesign
  , boxBehnkenDesign
  , Resolution (..)
  , resNum
  , fractionalDesign
  , fractionalDesignGen
  , fractionalDesignInter
  , fractionalDesignGenInter
  , fractionalCatalog
  , fracResolution
  , aliasStructure
  , OATable (..)
  , taguchiDesign
  , taguchiDesignOA
  , OptCriterion (..)
  , optimalDesign
  , optimalDesignWith
  , optimalDesignLevels
  , mainEffects
  , twoWay
  , quadratic
  , designTable
  , designFrame
  , designFrameRound
  , designFactorNames
  , designFormula
  , RSMNature (..)
  , RSMReport (..)
  , rsmAnalysis
  , steepestAscentNatural
  , saveDesign
  , planFromFrame
  , DesignModelSpec (..)
  , designModel
  , DesignModelGPSpec (..)
  , designModelGP
  , ranIntercept
  , ranSlope
  , designHBMProgram
  , DesignHBMFit (..)
  , DesignModelHBMSpec (..)
  , designModelHBM
    -- * 多出力 (複数応答) fit コンビネータ — Phase 78.F
  , MultiOutputSpec (..)
  , multiOutput
  , modelFor
    -- * 重み付き最小二乗 (WLS) spec — Phase 52.A6
  , WeightedLMSpec (..)
  , weighted
  , weightedR2
    -- * 透過標準化ラッパ (自動逆変換) — Phase 70.3 項目 C
  , StandardizedSpec (..)
  , standardized
  , standardizedY
    -- * 群別フィット spec — Phase 52.A4
  , GroupedSpec (..)
  , grouped
  , groupModels
  , groupLabels
    -- * GAM 高レベル spec (基底一般化 + GCV) — Phase 70.6 F3
  , GAMConfig (..)
  , defaultGAMConfig
  , GAMSpec (..)
  , gam
  , gamMulti
    -- * カーネル法 (GP / KRR / RFF) spec — Phase 70.5 項目 E
  , GPConfig (..)
  , defaultGP
  , GPSpec
  , gp
  , GPMultiSpec
  , gpMulti
    -- * 罰則付き回帰 統合 spec — Phase 70.7 項目 G
  , LambdaStrat (..)
  , RegConfig (..)
  , defaultRidge
  , defaultLasso
  , RegSpec
  , regularized
  , regularizedMulti
  , ridge
  , ridgeMulti
  , lasso
  , lassoMulti
  , elasticNet
  , elasticNetMulti
    -- * 行列入力モデルの高レベル spec — Phase 70.A
  , PCASpec (..)
  , pca
    -- * MDS (多次元尺度構成法) — Phase 75.21
  , MDSSpec (..)
  , mds
  , MDSConfig (..)
  , MDSMethod (..)
  , defaultMDS
  , PLSSpec (..)
  , pls
  , LDASpec (..)
  , lda
  , CCASpec (..)
  , ccaOf
    -- * 教師あり ML 分類器/回帰器 spec — Phase 70.A
  , GBRSpec (..)
  , gbmReg
  , GBCSpec (..)
  , gbmCls
  , DTSpec (..)
  , decisionTree
  , KNNCSpec (..)
  , knnCls
  , KNNRSpec (..)
  , knnReg
  , NBSpec (..)
  , naiveBayes
    -- * seed 純粋化した RNG モデル spec (KMeans / RandomForest) — Phase 70.A
  , KMeansSpec (..)
  , kmeans
  , RFSpec (..)
  , randomForestReg
    -- * 因果探索 LiNGAM (高レベル df|-> ・Phase 77)
  , DirectLiNGAMSpec (..)
  , directLingam
  , ParceLiNGAMSpec (..)
  , parceLingam
  , MultiGroupLiNGAMSpec (..)
  , multiGroupLingam
  , VARLiNGAMSpec (..)
  , varLingam
  , PairwiseLiNGAMSpec (..)
  , pairwiseLingam
  , BootstrapLiNGAMSpec (..)
  , bootstrapLingam
  , ICALiNGAMSpec (..)
  , icaLingam
  , CorrelationSpec (..)
  , correlationOf
  , CorrelationGraph (..)
  , RFCSpec (..)
  , randomForestCls
    -- * SVM / 古典 MLP 高レベル spec (純粋・df |->) — Phase 75.9
  , MLPClsSpec (..)
  , mlpCls
  , MLPRegSpec (..)
  , mlpReg
  , SVMSpec (..)
  , svmCls
    -- * HBM spec — Phase 51.4
  , HBMSpec
  , hbm
  ) where

import           Data.List             (elemIndex, nub, sort, sortBy, transpose)
import qualified Data.Map.Strict       as Map
import           Data.Maybe            (fromMaybe)
import           Data.Ord              (comparing)
import           Data.Word             (Word32)
import qualified Data.Vector           as V
import qualified Data.Vector.Unboxed    as VU
import           System.Random.MWC     (initialize)
import           Control.Monad.ST      (runST)
import qualified Numeric.LinearAlgebra as LA

import           Data.Text             (Text)
import qualified Data.Text             as T

import           Hanalyze.Data.ColumnSource     (ColumnSource (..))
import           Hanalyze.DataIO.Convert        (getTextVec)
import           Hanalyze.DataIO.Preprocess     (dropMissingRows)
import           Hanalyze.Model.Formula         (Formula (..))
import           Hanalyze.Model.Formula.Mixed  (fitMixedLME, RandomSpec (..))
import           Hanalyze.Model.Formula.Frame   (ModelFrame (..), modelFrame)
import           Hanalyze.Model.Formula.Design  (designMatrixF, responseVec)
import           Hanalyze.Model.Formula.RFormula (parseModel)
import           Hanalyze.Model.GLMM           (GLMMResultRE (..), buildGroups)
import           Hanalyze.MCMC.Core             (chainVals)

import           Hanalyze.Model.Wrappers
import           Hanalyze.Diagnostics
import           Hanalyze.Model.Core   (coefficientsV)
import           Hanalyze.Model.GLM    ( Family (..), LinkFn (..) )
import           Hanalyze.Model.GP     (GPResult (..), Kernel (..), GPParams (..))
import qualified Hanalyze.Model.GP     as GP
import qualified Hanalyze.Model.RFF    as RFF
import qualified Debug.Trace                  as Trace
import           Hanalyze.Model.LM     ( designMatrix, fitLMVec )
import           Hanalyze.Model.Spline ( SplineKind (..) )
import           Hanalyze.Model.GAM    ( fitGAMAuto
                                              , GAMBasis (..), GAMLambda (..) )
import           Hanalyze.Model.Robust ( RobustEstimator )
import           Hanalyze.Stat.CorrelationNetwork (CorrelationGraph (..), correlationMatrix)
import           Hanalyze.Design.Workflow (Design (..), DesignFactor (..), FactorKind (..)
                                       , DesignKind (..), FactorScale (..), contFactor, contFactorLog, numFactor, catFactor
                                       , CustomSpec (..), customSpec, customDesign
                                       , Structure (..), splitPlot, stripPlot, blocked
                                       , Constraint (..), ConstraintRel (..), ConstraintGuard (..), FactorValue (..)
                                       , NatConstraint (..), natLeq, natGeq, natEq, natForbid
                                       , formulaToCustomModel
                                       , factorialDesign, centralCompositeDesign, boxBehnkenDesign
                                       , Resolution (..), resNum, fractionalDesign, fractionalDesignGen
                                       , fractionalDesignInter, fractionalDesignGenInter
                                       , fractionalCatalog, fracResolution, aliasStructure
                                       , OATable (..), taguchiDesign, taguchiDesignOA
                                       , OptCriterion (..), optimalDesign, optimalDesignWith, optimalDesignLevels
                                       , mainEffects, twoWay, quadratic
                                       , designTable, designFrame, designFrameRound
                                       , designFactorNames, designFormula
                                       , RSMNature (..), RSMReport (..), rsmAnalysis, steepestAscentNatural
                                       , saveDesign, planFromFrame)
-- 低レベル 'Hanalyze.Model.PCA.pca' は高レベル spec 'pca' (= PCASpec) と
-- 同名衝突するため qualified import で回避する (Phase 75.16)。
import           Hanalyze.Model.PCA     (PCAStandardize (..))
import qualified Hanalyze.Model.PCA     as PCALow
import           Hanalyze.Stat.Standardize
                   ( Standardizer (..), fitStandardizer, applyStandardizer
                   , applyStandardizerCol )
import           Hanalyze.Model.RandomForest (RandomForest (..), RFConfig (..), fitRFVPure)
import           Hanalyze.Model.LiNGAM.Direct (DirectLiNGAMConfig (..), DirectLiNGAMFit (..)
                                       , fitDirectLiNGAM)
import           Hanalyze.Model.LiNGAM.Parce (ParceConfig, ParceFit (..), fitParceLiNGAM)
import           Hanalyze.Model.LiNGAM.MultiGroup (MultiGroupConfig, MultiGroupFit (..)
                                       , fitMultiGroupLiNGAM)
import           Hanalyze.Model.LiNGAM.VAR (VARLiNGAMConfig, VARLiNGAMFit (..), fitVARLiNGAM)
import           Hanalyze.Model.LiNGAM.Pairwise (PairwiseResult (..), pairwiseLiNGAM)
import           Hanalyze.Model.LiNGAM.Bootstrap (BootstrapConfig, BootstrapResult (..)
                                       , fitBootstrapLiNGAMPure)
import           Hanalyze.Model.LiNGAM.ICA (ICALiNGAMConfig, ICALiNGAMFit (..), fitICALiNGAMPure)
import           Hanalyze.Model.RandomForestClassifier (RFClassifierFit (..), RFCConfig (..), fitRFClassifierPure)
import           Hanalyze.Model.GradientBoosting (GBRegressor (..), GBClassifier (..)
                                       , fitGBRegressor, fitGBClassifier, GBConfig (..))
import           Hanalyze.Model.DecisionTree (DTree (..), DTFit (..), fitDTV, DTConfig (..))
import           Hanalyze.Model.Discriminant (DiscriminantFit (..), fitLDA)
import           Hanalyze.Model.Multivariate (cca, CCAFit (..))
import           Hanalyze.Model.NaiveBayes (NBModel (..), GaussianNB (..), fitGNB)
import           Hanalyze.Model.KNN (KNNClassifier (..), KNNRegressor (..), fitKNNC, fitKNNR)
import           Hanalyze.Model.SVM (SVMConfig (..), SVMMulti (..)
                                       , fitSVMMulti
                                       , SVMHyper (..)
                                       , SVMTuneGrid (..), tuneSVM)
import           Hanalyze.Model.NeuralNetwork (MLPConfig (..), MLPFit (..)
                                       , fitMLPClassifierPure, fitMLPRegressorPure)
import           Hanalyze.Model.PLS (PLSFit (..), fitPLS, PLSConfig (..))
import           Hanalyze.Model.Cluster (KMeansResult (..)
                                       , KMeansConfig (..), kMeansPure)
import           Hanalyze.Model.Quantile (fitQuantile)
import           Hanalyze.Model.Regularized (RegFit (..),
                                           fitRidge, fitLasso, fitElasticNet)
import qualified Hanalyze.Model.RegularizedAdvanced as RA
import qualified Hanalyze.Stat.CV          as HCV
import           Hanalyze.Model.PCA     (PCAResult (..))
import           Hanalyze.Model.MDS     (MDSConfig (..), MDSMethod (..)
                                               , defaultMDS, MDSResult, runMDS)
import           Hanalyze.Model.HBM     (ModelP, dataSlots, dataIxSlots,
                                                 Distribution (Normal, HalfNormal),
                                                 sample, observe, reNormal, at, observeLMR,
                                                 lkjCorrCholesky, deterministic,
                                                 plateI, plateI_, plateForM_, (.#),
                                                 REff (..),
                                                 LMFamily (LMGaussian))

-- ---------------------------------------------------------------------------
-- GAM 高レベル (df|->) — Phase 70.6 F3
--
-- 'gamModel' は単変量・B-spline 固定・λ 手動だった。 'gam' / 'gamMulti' は **基底を選べ**
-- (B-spline / 自然3次 / 多項 / Fourier / RBF)、 **λ を GCV 自動選択**でき、 'lm'/'lmMulti'
-- と対称な命名で受ける (@df |-> gam cfg "x" "y"@ / @df |-> gamMulti cfg ["x1","x2"] "y"@)。 描画は第1予測子
-- 軸の平滑曲線 (多予測子では他を訓練平均に固定した偏依存曲線・band 非提供は GAM 共通)。
-- ---------------------------------------------------------------------------

-- | [日本語]: GAM の設定。 全項共通の基底 'gcBasis' と λ 方略 'gcLambda'。
--   [English]: GAM configuration. The basis 'gcBasis' shared by all terms
--   and the λ strategy 'gcLambda'.
data GAMConfig = GAMConfig
  { gcBasis  :: GAMBasis   -- ^ [日本語]: 各平滑項に使う基底 (全項共通)。 [English]: Basis used for each smooth term (shared across all terms).
  , gcLambda :: GAMLambda  -- ^ [日本語]: ridge λ の決め方 ('FixedL' / 'GCV')。 [English]: How the ridge λ is decided ('FixedL' \/ 'GCV').
  } deriving (Show)

-- | [日本語]: 既定設定: 3 次 B-spline・内部 6 ノット・λ は GCV 自動選択。
--   [English]: Default configuration: cubic B-spline, 6 interior knots, λ
--   auto-selected via GCV.
defaultGAMConfig :: GAMConfig
defaultGAMConfig = GAMConfig (BSplineB 3 6) GCV

-- | [日本語]: GAM spec (単/多予測子)。 @gam cfg "x" "y"@ または @gamMulti cfg ["x1","x2"] "y"@。
--   [English]: GAM spec (single or multiple predictors).
--   @gam cfg \"x\" \"y\"@ or @gamMulti cfg [\"x1\",\"x2\"] \"y\"@.
data GAMSpec = GAMSpec !GAMConfig ![Text] !Text

-- | [日本語]: @gam cfg xCol yCol@ — 単一予測子 GAM ('lm' と対)。
--   [English]: @gam cfg xCol yCol@ — a single-predictor GAM (paired with
--   'lm').
gam :: GAMConfig -> Text -> Text -> GAMSpec
gam cfg xn yn = GAMSpec cfg [xn] yn

-- | [日本語]: @gamMulti cfg xCols yCol@ — 多予測子 GAM ('lmMulti' と対)。 第1予測子を描画軸にする。
--   [English]: @gamMulti cfg xCols yCol@ — a multi-predictor GAM (paired
--   with 'lmMulti'). The first predictor is used as the plotting axis.
gamMulti :: GAMConfig -> [Text] -> Text -> GAMSpec
gamMulti = GAMSpec

instance Fit GAMSpec where
  type Fitted GAMSpec = GAMModelN
  fitEither (GAMSpec cfg xns yn) d = do
    xs <- mapM (`reqColV` d) xns
    y  <- reqColV yn d
    let xss   = map (V.fromList . LA.toList) xs
        yv    = V.fromList (LA.toList y)
        bases = [ gcBasis cfg | _ <- xns ]
        fit   = fitGAMAuto bases (gcLambda cfg) xss yv
    pure GAMModelN { gamNFit = fit, gamNXraws = xs, gamNNames = xns }

-- ===========================================================================
-- df |-> spec 統一 fit API (Phase 51)
-- ===========================================================================
--
-- 任意のデータ源 ('ColumnSource') から spec が指すモデルを学習する単一の動詞。
-- 新規の数値核は無く、 既存の fit 関数 (lmModel / glmModel / …) を列抽出で
-- 配線するだけの薄いエルゴノミック層。
--
-- * 二変量近道 (列名2つ): @lm "x" "y"@ / @glm fam link "x" "y"@ 等 (本節・51.2)。
-- * formula 多変量 (R 流): @lmF "y ~ x1 + x2"@ 等 (Phase 51.3)。
-- * HBM (手書き model): @hbm cfg model@ (Phase 51.4)。

-- | [日本語]: @df |-> spec = fitWith spec df@。 plot の @|>>@ と同列 (@infixl 1@)。
--   [English]: @df |-> spec = fitWith spec df@. On par with plot's @|>>@
--   (@infixl 1@).
infixl 1 |->
(|->) :: (ColumnSource d, Fit spec) => d -> spec -> Fitted spec
d |-> spec = fitWith spec d

-- | [日本語]: @df |->! spec = fitIO spec df@ (進捗つき IO 学習動詞)。
-- 純粋動詞 @(|->)@ との違いは__副作用 (進捗表示) の有無が動詞の選択に現れる__
-- こと: HBM のような重い学習は @|->!@ で stderr に進捗 1 行が出る。
-- 結果は同 cfg の @|->@ と__ビット一致__ (seed 規約共有)。
--   [English]: @df |->! spec = fitIO spec df@ (an IO training verb with
--   progress display). The difference from the pure verb @(|->)@ is that
--   __the choice of verb reflects whether there's a side effect__ (progress
--   display): heavy training like HBM emits one line of progress
--   to stderr via @|->!@. The result is __bit-identical__ to @|->@ with the
--   same cfg (they share the seed convention).
infixl 1 |->!
(|->!) :: (ColumnSource d, Fit spec) => d -> spec -> IO (Fitted spec)
d |->! spec = fitIO spec d

-- --- 二変量近道 spec (列名 x, y) — Phase 51.2 -------------------------------

-- | [日本語]: 単回帰 spec。 @lm "x" "y"@。
--   [English]: Simple regression spec. @lm \"x\" \"y\"@.
data LMSpec       = LMSpec       !Text !Text
-- | [日本語]: 二変量 GLM spec。 @glm fam link "x" "y"@。
--   [English]: Bivariate GLM spec. @glm fam link \"x\" \"y\"@.
data GLMSpec      = GLMSpec      !Family !LinkFn !Text !Text
-- | [日本語]: 二変量 spline spec。 @spline kind knots "x" "y"@。
--   [English]: Bivariate spline spec. @spline kind knots \"x\" \"y\"@.
data SplineSpec   = SplineSpec   !SplineKind ![Double] !Text !Text
-- | [日本語]: 二変量 robust spec。 @robust est "x" "y"@。
--   [English]: Bivariate robust regression spec. @robust est \"x\" \"y\"@.
data RobustSpec   = RobustSpec   !RobustEstimator !Text !Text
-- | [日本語]: 分位点回帰 spec。 @quantile taus "x" "y"@。
--   [English]: Quantile regression spec. @quantile taus \"x\" \"y\"@.
data QuantileSpec = QuantileSpec ![Double] !Text !Text

-- | [日本語]: @lm xCol yCol@ — 単回帰 ('LMModel') を当てはめる spec。
--   [English]: @lm xCol yCol@ — a spec that fits a simple regression
--   ('LMModel').
lm :: Text -> Text -> LMSpec
lm = LMSpec

-- | [日本語]: @glm fam link xCol yCol@ — 二変量 GLM ('GLMModel') の spec。
--   [English]: @glm fam link xCol yCol@ — a spec for a bivariate GLM
--   ('GLMModel').
glm :: Family -> LinkFn -> Text -> Text -> GLMSpec
glm = GLMSpec

-- | [日本語]: @spline kind knots xCol yCol@ — spline 回帰 ('SplineModel') の spec。
--   [English]: @spline kind knots xCol yCol@ — a spec for spline regression
--   ('SplineModel').
spline :: SplineKind -> [Double] -> Text -> Text -> SplineSpec
spline = SplineSpec

-- | [日本語]: @rlm est xCol yCol@ — ロバスト回帰 ('RobustModel') の spec (R @MASS::rlm@)。
--   [English]: @rlm est xCol yCol@ — a spec for robust regression
--   ('RobustModel'; R's @MASS::rlm@).
rlm :: RobustEstimator -> Text -> Text -> RobustSpec
rlm = RobustSpec

-- | [日本語]: @rq taus xCol yCol@ — 分位点回帰 ('QuantileModel') の spec (R @quantreg::rq@)。
--   [English]: @rq taus xCol yCol@ — a spec for quantile regression
--   ('QuantileModel'; R's @quantreg::rq@).
rq :: [Double] -> Text -> Text -> QuantileSpec
rq = QuantileSpec

instance Fit LMSpec where
  type Fitted LMSpec = LMModel
  fitEither (LMSpec xn yn) d = lmModel <$> reqColV xn d <*> reqColV yn d
  predictorCols (LMSpec xn _) = [xn]
  responseCol   (LMSpec _ yn) = Just yn


-- --- 重み付き最小二乗 (WLS) spec — Phase 52.A6 -----------------------------
--
-- WLS は各観測に重み @wᵢ@ を付けて @Σ wᵢ(yᵢ − ŷᵢ)²@ を最小化する (不等分散・
-- 観測の信頼度差に。 ggplot @geom_smooth(method=lm, aes(weight=w))@ 相当)。
-- 数値核 = √w スケール OLS: @X_w = diag(√w)·X@, @y_w = diag(√w)·y@ を OLS すると
-- β̂ = (XᵀWX)⁻¹XᵀWy・s² = Σ wᵢ(yᵢ−x̂ᵢ)²/(n−p) が得られる。
--
-- ★描画の整合: grid 評価 ('svGrid' → @confidenceBandAt@) は評価点を**非スケール**
-- @[1, gx]@ で渡すので、 設計行列を**スケール済** ('lmDesign'=X_w) にしておけば
-- @se = t·√(s²·x₀ᵀ(XᵀWX)⁻¹x₀)@ ＝ 正しい WLS pointwise CI が元 x スケールで出る。
-- 一方 'LMModel' の素の @toPlot@ (訓練点経路 @confidenceBand@) は評価点がスケール済
-- 行になり中心も se も √wᵢ 倍に壊れる。 ゆえに専用ラッパ 'WeightedLMModel' を設け
-- @toPlot@ を **grid 経路 ('statModel')** に固定する (= 元データ散布図と整合)。

-- | [日本語]: 重み付き最小二乗 spec。 @weighted "w" (lm "x" "y")@ (重みは __df の列名__・全て @≥ 0@)。
--   [English]: Weighted least squares spec. @weighted \"w\" (lm \"x\" \"y\")@
--   (the weight is __a df column name__; all values must be @≥ 0@).
data WeightedLMSpec = WeightedLMSpec !Text !LMSpec

-- | [日本語]: @weighted wCol (lm xCol yCol)@ — 重み__列__ @wCol@ で WLS を当てはめる spec ラッパ。
--   重みは x / y と同一 'ColumnSource' の列名で受ける (statsmodels @wls(…, weights=col)@ /
--   R @weights=@ と同型)。 現状 'LMSpec' 専用 (WLS は LM 固有。 GLM の重みは別軸ゆえ対象外)。
--   [English]: @weighted wCol (lm xCol yCol)@ — a spec wrapper that fits WLS
--   using the weight __column__ @wCol@. The weight is looked up by column
--   name in the same 'ColumnSource' as x \/ y (the same shape as
--   statsmodels's @wls(…, weights=col)@ \/ R's @weights=@). Currently
--   'LMSpec'-only (WLS is specific to LM; GLM weights are a different axis
--   and out of scope).
weighted :: Text -> LMSpec -> WeightedLMSpec
weighted = WeightedLMSpec

instance Fit WeightedLMSpec where
  type Fitted WeightedLMSpec = WeightedLMModel
  fitEither (WeightedLMSpec wn (LMSpec xn yn)) d = do
    xs <- reqColV xn d
    ys <- reqColV yn d
    wv <- reqColV wn d
    let n  = LA.size xs
        ws = LA.toList wv
    if length ws /= n
      then Left ("weighted: 重み列 " <> show wn <> " の長さ " <> show (length ws)
                  <> " が観測数 " <> show n <> " と一致しません")
      else if any (< 0) ws
        then Left "weighted: 重みに負値が含まれます (全て ≥ 0 が必要)"
        else
          let sw  = map sqrt ws                                       -- √w (長さ n)
              dm0 = designMatrix (V.fromList (LA.toList xs))          -- [1, x] 非スケール
              dmW = LA.fromRows (zipWith LA.scale sw (LA.toRows dm0)) -- diag(√w)·X
              yW  = LA.fromList (zipWith (*) sw (LA.toList ys))       -- √w·y
              res = fitLMVec dmW yW
          in Right WeightedLMModel
               { wlmInner   = LMModel { lmDesign = dmW, lmResult = res, lmXraw = xs }
               , wlmWeights = ws
               , wlmY       = LA.toList ys
               }

-- | [日本語]: 重み付き R²。 statsmodels WLS @rsquared@ と一致: @1 − Σwᵢ(yᵢ−ŷᵢ)² / Σwᵢ(yᵢ−ȳ_w)²@
--   (中心化は重み付き平均 @ȳ_w = Σwᵢyᵢ/Σwᵢ@)。 ★スケール空間の素朴な R² とは中心化が
--   異なる (= 内側 LM の @svCoefR2@ をそのまま使うと不一致になる罠)。
--   [English]: Weighted R². Matches statsmodels WLS's @rsquared@:
--   @1 − Σwᵢ(yᵢ−ŷᵢ)² / Σwᵢ(yᵢ−ȳ_w)²@ (centering uses the weighted mean
--   @ȳ_w = Σwᵢyᵢ/Σwᵢ@). Note: this centering differs from the naive R² in
--   scaled space — a trap where using the inner LM's @svCoefR2@ directly
--   gives a mismatched value.
weightedR2 :: [Double] -> [Double] -> [Double] -> Double
weightedR2 ws ys yhats =
  let sw   = sum ws
      ybar = sum (zipWith (*) ws ys) / sw
      ssr  = sum (zipWith3 (\w y yh -> w * (y - yh) ^ (2 :: Int)) ws ys yhats)
      tss  = sum (zipWith  (\w y    -> w * (y - ybar) ^ (2 :: Int)) ws ys)
  in 1 - ssr / tss

-- --- 透過標準化ラッパ (自動逆変換) — Phase 70.3 項目 C ----------------------
--
-- スケール敏感なモデル (とくに距離ベース kNN) は予測子の z-score 標準化が要る。
-- 'standardized' / @standardizedY@ は spec を**透過的に**ラップし、 学習は標準化空間で
-- 行いつつ図・予測は元スケールへ自動逆変換する (tidymodels @step_normalize@ /
-- sklearn @Pipeline@ 相当)。 標準化対象列は spec 自身が 'predictorCols' / 'responseCol'
-- で返すので**列名を二重に書かない**。 内部標準化済 (GP/Reg/PCA/PLS) や木系は
-- @predictorCols = []@ ゆえラッパが 'Left' で誤用を弾く (= 二重標準化バグ回避)。

-- | [日本語]: spec を透過標準化でラップする (@Bool@ = 応答 y も標準化するか)。
--   構成子は直接使わず 'standardized' / @standardizedY@ から作る。
--   [English]: Wraps a spec with transparent standardization (@Bool@ =
--   whether to also standardize the response y). Don't use the constructor
--   directly; build via 'standardized' \/ @standardizedY@.
data StandardizedSpec spec = StandardizedSpec !Bool !spec

-- | [日本語]: @standardized spec@ — __予測子列のみ__ z-score 標準化して内側 @spec@ を学習し、
--   描画・予測は__元スケール__で返す透過ラッパ。 距離ベース kNN
--   (@knnReg@ / @knnCls@) が本命。 標準化対象列は spec の 'predictorCols' から
--   取得する (列名を二重に書かない)。 被せる意味の無い spec
--   (内部標準化済 GP\/Reg\/PCA\/PLS・スケール不変な木系) は @predictorCols = []@
--   ゆえ 'fitEither' が 'Left' で誤用を弾く。
--
--   @df |-> standardized (knnReg 5 [\"x1\",\"x2\"] \"y\")@
--   [English]: @standardized spec@ — a transparent wrapper that
--   z-score-standardizes __only the predictor columns__ before training the
--   inner @spec@, returning plots \/ predictions on the __original scale__.
--   Aimed mainly at distance-based kNN (@knnReg@ \/ @knnCls@). The columns
--   to standardize are obtained from the spec's 'predictorCols' (no
--   duplicate column-name listing). Specs for which wrapping is meaningless
--   (already internally standardized GP\/Reg\/PCA\/PLS; scale-invariant
--   tree-based ones) have @predictorCols = []@, so 'fitEither' rejects the
--   misuse with 'Left'.
--
--   @df |-> standardized (knnReg 5 [\"x1\",\"x2\"] \"y\")@
standardized :: spec -> StandardizedSpec spec
standardized = StandardizedSpec False

-- | [日本語]: @standardizedY spec@ — 予測子 __+ 応答 y__ を標準化する版 (多目的最適化向け・
--   opt-in)。 y 列名は spec の 'responseCol' から取得し、 予測・図は元スケールへ
--   逆変換する。 応答がクラスラベル / family・link 拘束で標準化不可な spec
--   (@responseCol = Nothing@: 分類・GLM) に付けると 'fitEither' が 'Left' (誤用ガード)。
--   [English]: @standardizedY spec@ — a version that also standardizes the
--   response y in addition to the predictors (opt-in, aimed at
--   multi-objective optimization). The y column name is obtained from the
--   spec's 'responseCol'; predictions \/ plots are inverse-transformed back
--   to the original scale. Applying it to a spec whose response can't be
--   standardized (class labels, or a GLM constrained by family\/link —
--   @responseCol = Nothing@) makes 'fitEither' return 'Left' (a misuse
--   guard).
standardizedY :: spec -> StandardizedSpec spec
standardizedY = StandardizedSpec True

instance Fit spec => Fit (StandardizedSpec spec) where
  type Fitted (StandardizedSpec spec) = StandardizedModel (Fitted spec)
  fitEither (StandardizedSpec stdY inner) d =
    fitStd (predictorCols inner) (responseCol inner) stdY inner d
  -- 入れ子ラップは更なる標準化対象を持たない (二重ラップ無効化)。
  predictorCols _ = []
  responseCol   _ = Nothing

-- | [日本語]: 透過標準化の本体: 予測子列 (と opt-in で応答 y) を z-score 標準化した派生
--   'ColumnSource' を作り、 内側 spec をそれに当てはめる。 逆変換用の (μ,σ) を保持。
--   失敗 (空 predictorCols / 標準化不能応答への @standardizedY@ / 列欠落) は 'Left'。
--   [English]: The core of transparent standardization: builds a derived
--   'ColumnSource' with the predictor columns (and, opt-in, the response y)
--   z-score-standardized, and fits the inner spec to it. Keeps the (μ,σ)
--   needed for the inverse transform. Failure (empty predictorCols,
--   @standardizedY@ on a non-standardizable response, missing columns)
--   gives 'Left'.
fitStd :: (ColumnSource d, Fit spec)
       => [Text]       -- ^ [日本語]: 標準化対象の予測子列 (spec の 'predictorCols')。 [English]: Predictor columns to standardize (the spec's 'predictorCols').
       -> Maybe Text   -- ^ [日本語]: 応答列 (spec の 'responseCol')。 散布用 + y 標準化に使う。 [English]: Response column (the spec's 'responseCol'). Used for the scatter plot + y standardization.
       -> Bool         -- ^ [日本語]: y も標準化するか (@standardizedY@ なら True)。 [English]: Whether to also standardize y (True for @standardizedY@).
       -> spec
       -> d
       -> Either String (StandardizedModel (Fitted spec))
fitStd predCols mRespCol stdY inner d
  | null predCols =
      Left "standardized: この spec は標準化対象の予測子列を持ちません \
           \(predictorCols = [])。 内部標準化済 (GP/Reg/PCA/PLS) か木系 (スケール不変) \
           \ゆえ透過標準化は無意味です。"
  | stdY, Nothing <- mRespCol =
      Left "standardizedY: この spec は標準化可能な応答列を持ちません \
           \(responseCol = Nothing)。 分類 (クラスラベル) や GLM (family/link 拘束) の \
           \応答は標準化できません。 X のみ標準化する standardized を使ってください。"
  | otherwise = do
      xm <- reqColsM predCols d                       -- n × p (予測子行列)
      let xStdr = fitStandardizer xm
          xmZ   = applyStandardizer xStdr xm
          predZ = zip predCols (map LA.toList (LA.toColumns xmZ))
      (yStd, yOverride) <-
        if stdY
          then case mRespCol of
                 Nothing -> Left "fitStd: internal — stdY without responseCol"  -- ガード済で到達せず
                 Just yc -> do
                   yv <- reqColV yc d
                   let ys         = LA.toList yv
                       (muY, sdY) = meanSdSafe ys
                   Right (Just (muY, sdY), [(yc, [ (v - muY) / sdY | v <- ys ])])
          else Right (Nothing, [])
      -- 派生 ColumnSource: 元の全数値列を assoc 化し、 標準化列で同名上書き。
      let derived = overrideAssoc (numericCols d) (predZ ++ yOverride)
      innerM <- fitEither inner derived
      -- 単変量散布用に元スケール (x,y) を保持 (予測子 1 列 + 応答列ありの時のみ)。
      let mTrain = case (predCols, mRespCol) of
                     ([xc], Just yc)
                       | Just xs <- lookupCol xc d
                       , Just ys <- lookupCol yc d -> Just (xs, ys)
                     _ -> Nothing
      Right StandardizedModel { smInner = innerM, smXStd = xStdr
                              , smYStd = yStd,   smTrain = mTrain }

-- | [日本語]: assoc を別 assoc で同名上書き (上書きキーは元の位置・値だけ差替え、 元に
--   無いキーは末尾追加)。 透過標準化の派生 'ColumnSource' 生成に使う。
--   [English]: Overrides one assoc list with another, matching by key
--   (overridden keys keep their original position with only the value
--   replaced; keys absent from the original are appended at the end). Used
--   to build the derived 'ColumnSource' for transparent standardization.
overrideAssoc :: [(Text, [Double])] -> [(Text, [Double])] -> [(Text, [Double])]
overrideAssoc base ovr =
  let ovrMap   = Map.fromList ovr
      baseKeys = map fst base
  in [ (k, Map.findWithDefault v k ovrMap) | (k, v) <- base ]
     ++ [ kv | kv@(k, _) <- ovr, k `notElem` baseKeys ]

-- | [日本語]: 標本平均と (n-1) 標準偏差。 σ≈0 (定数列) は 1 に丸めて 0 割を回避
--   ('Stat.Standardize' の 'fitStandardizer' と同流儀)。
--   [English]: Sample mean and (n-1) standard deviation. σ≈0 (a constant
--   column) is rounded to 1 to avoid division by zero (the same convention
--   as 'Stat.Standardize''s 'fitStandardizer').
meanSdSafe :: [Double] -> (Double, Double)
meanSdSafe xs =
  let n = length xs
  in if n == 0 then (0, 1)
     else let mu = sum xs / fromIntegral n
          in if n == 1 then (mu, 1)
             else let var = sum [ (x - mu) ^ (2 :: Int) | x <- xs ] / fromIntegral (n - 1)
                      sd0 = sqrt var
                  in (mu, if sd0 < 1e-12 then 1 else sd0)

instance Fit GLMSpec where
  type Fitted GLMSpec = GLMModel
  fitEither (GLMSpec fam lnk xn yn) d =
    glmModel fam lnk <$> reqColV xn d <*> reqColV yn d
  predictorCols (GLMSpec _ _ xn _) = [xn]
  -- responseCol = Nothing (既定)。 GLM の応答は family/link でスケールが拘束される
  -- (count\/binary\/正値) ため標準化は不正。 X 標準化 ('standardized') のみ許す。

instance Fit SplineSpec where
  type Fitted SplineSpec = SplineModel
  fitEither (SplineSpec kind knots xn yn) d =
    splineModel kind knots <$> reqColV xn d <*> reqColV yn d
  predictorCols (SplineSpec _ _ xn _) = [xn]
  responseCol   (SplineSpec _ _ _ yn) = Just yn

instance Fit RobustSpec where
  type Fitted RobustSpec = RobustModel
  fitEither (RobustSpec est xn yn) d =
    robustModel est <$> reqColV xn d <*> reqColV yn d
  predictorCols (RobustSpec _ xn _) = [xn]
  responseCol   (RobustSpec _ _ yn) = Just yn

instance Fit QuantileSpec where
  type Fitted QuantileSpec = QuantileModel
  fitEither (QuantileSpec taus xn yn) d =
    quantileModel taus <$> reqColV xn d <*> reqColV yn d
  predictorCols (QuantileSpec _ xn _) = [xn]
  responseCol   (QuantileSpec _ _ yn) = Just yn

-- ===========================================================================
-- カーネル法ファミリ統合 — GP / KRR / RFF を 1 spec @gp@ に — Phase 70.5 項目 E
--
-- 4 つの実装は 2 軸で尽くされる: {厳密 (Gram) | 近似 (RFF)} × {分布あり (GP) |
-- 分布なし (Ridge 点)}。 本体は GP であり、 KRR の予測 ≡ GP 事後平均 (λ = σ_n²・
-- mean のみ)・RFF は GP の低ランク近似。 ゆえに内部カーネル評価を GP.hs に一本化し、
-- 4 象限を 'GPMethod' の明示コンストラクタで選ぶ。 'lm'/'lmMulti' 規約と対称な動詞
-- 'gp'/'gpMulti' (gpMulti は E3)。
-- ===========================================================================

-- | [日本語]: 'gp' / 'gpMulti' の設定。 カーネル種 × 象限 × ハイパラ方略。
--   [English]: Configuration for 'gp' \/ 'gpMulti'. Kernel kind × quadrant ×
--   hyperparameter strategy.
data GPConfig = GPConfig
  { gpcKernel :: !Kernel        -- ^ [日本語]: RBF / Matern52 / Periodic (GP.hs 由来・新型は作らない)。 [English]: RBF \/ Matern52 \/ Periodic (from GP.hs; no new kind is created here).
  , gpcMethod :: !GPMethod      -- ^ [日本語]: 象限 + (RFF 時) 近似次元・seed。 [English]: Quadrant + (for RFF) approximation dimension and seed.
  , gpcHyper  :: !HyperStrategy -- ^ [日本語]: ハイパラの決め方 + 固定時の値。 [English]: How the hyperparameters are decided + the value when fixed.
  }

-- | [日本語]: 既定: 厳密 GP・RBF・周辺尤度自動。 @df |-> gp defaultGP "x" "y"@。
--   [English]: Default: exact GP, RBF, automatic via marginal likelihood.
--   @df |-> gp defaultGP \"x\" \"y\"@.
defaultGP :: GPConfig
defaultGP = GPConfig RBF Gp AutoMarginalLik

-- | [日本語]: 二変量カーネル回帰 spec。 @gp cfg "x" "y"@ ('lm' と対称)。
--   [English]: Bivariate kernel regression spec. @gp cfg \"x\" \"y\"@
--   (symmetric with 'lm').
data GPSpec = GPSpec !GPConfig !Text !Text

-- | [日本語]: @gp cfg xCol yCol@ — 統合カーネル回帰 ('GPRegModel') を当てはめる spec。
--   [English]: @gp cfg xCol yCol@ — a spec that fits unified kernel
--   regression ('GPRegModel').
gp :: GPConfig -> Text -> Text -> GPSpec
gp = GPSpec

-- | [日本語]: RFF 非対応カーネルのフォールバック通知 (RFF は定常カーネル限定 = Bochner)。
-- Periodic は定常だがスペクトル密度未実装、 Linear/Poly は非定常ゆえ RFF 不可。
--   [English]: Fallback notice for kernels that don't support RFF (RFF is
--   limited to stationary kernels, via Bochner's theorem). Periodic is
--   stationary but its spectral density isn't implemented; Linear\/Poly are
--   non-stationary, so RFF isn't possible for them.
gpNonRffMsg :: Kernel -> String
gpNonRffMsg ker =
  "[gp] " <> show ker <> " カーネルは RFF 近似不可"
  <> " (Periodic はスペクトル密度未実装・Linear/Poly は非定常 = Bochner)。"
  <> " 厳密象限へフォールバックします。"

-- | [日本語]: RFF 近似象限 + RFF 非対応カーネル (Periodic/Linear/Poly) は厳密象限へ
-- フォールバック ('log' 通知)。 定常カーネル (RBF/Matern52) はそのまま。
--   [English]: An RFF-approximation quadrant combined with an RFF-unsupported
--   kernel (Periodic\/Linear\/Poly) falls back to the exact quadrant (with a
--   'log' notice). Stationary kernels (RBF\/Matern52) are left unchanged.
gpResolveMethod :: Kernel -> GPMethod -> GPMethod
gpResolveMethod ker (GpRff  _ _) | not (gpRffSupported ker) = Trace.trace (gpNonRffMsg ker) Gp
gpResolveMethod ker (KrrRff _ _) | not (gpRffSupported ker) = Trace.trace (gpNonRffMsg ker) Krr
gpResolveMethod _   m            = m

-- | [日本語]: RFF (Random Fourier Features) が近似可能なカーネルか (= 定常 + スペクトル密度実装済)。
--   [English]: Whether a kernel can be approximated by RFF (Random Fourier
--   Features) — i.e. stationary and with the spectral density implemented.
gpRffSupported :: Kernel -> Bool
gpRffSupported RBF      = True
gpRffSupported Matern52 = True
gpRffSupported _        = False   -- Periodic/Linear/Poly

-- | [日本語]: ハイパラ方略を解決して 'GPParams' を得る。
--   [English]: Resolves the hyperparameter strategy to obtain 'GPParams'.
gpResolveHyper :: HyperStrategy -> Kernel -> [Double] -> [Double] -> GPParams
gpResolveHyper (FixedHyper p)  _   _  _  = p
gpResolveHyper AutoMarginalLik ker xs ys = GP.optimizeGP ker xs ys (GP.initParamsFromData xs ys)
gpResolveHyper AutoCV          ker xs ys = GP.autoCVHyperGP ker xs ys

-- | [日本語]: 象限ごとの予測子を組む。 渡される @method@ は Periodic フォールバック後なので、
-- RFF 象限 (GpRff/RidgeRff) に来るカーネルは RBF / Matern52 に限られる。
--   [English]: Builds the predictor for each quadrant. Since the @method@
--   passed in is already past the Periodic fallback, kernels reaching the
--   RFF quadrants (GpRff\/RidgeRff) are limited to RBF \/ Matern52.
gpBuildPredict
  :: GPMethod -> Kernel -> GPParams -> [Double] -> [Double]
  -> ([Double] -> ([Double], Maybe [Double]))
gpBuildPredict method ker params xs ys = case method of
  Gp  -> \q -> let r = GP.fitGP (GP.GPModel ker params) xs ys q
               in (gpMean r, Just (gpVar r))
  Krr -> \q -> let r = GP.fitGP (GP.GPModel ker params) xs ys q
               in (gpMean r, Nothing)                 -- KRR ≡ GP 事後平均・帯は捨てる
  GpRff dDim seed ->
    let feats = gpSampleFeats ker dDim seed
        fit   = RFF.rffGP feats xs ys (sqrt (max 1e-12 (gpNoiseVar params)))
    in \q -> let ps = RFF.predictRFFGP fit q
             in (map fst ps, Just (map snd ps))
  KrrRff dDim seed ->
    let feats = gpSampleFeats ker dDim seed
        fit   = RFF.rffRidge feats xs ys (max 1e-12 (gpNoiseVar params))
    in \q -> (RFF.predictRFFRidge fit q, Nothing)
  where
    ell = gpLengthScale params
    sf  = sqrt (max 1e-12 (gpSignalVar params))
    gpSampleFeats Matern52 dDim seed = RFF.sampleRFFMatern52Pure dDim ell sf seed
    gpSampleFeats _        dDim seed = RFF.sampleRFFRBFPure      dDim ell sf seed

instance Fit GPSpec where
  type Fitted GPSpec = GPRegModel
  fitEither (GPSpec cfg xn yn) d = do
    xv <- reqColV xn d
    yv <- reqColV yn d
    let xs     = LA.toList xv
        ys     = LA.toList yv
        ker    = gpcKernel cfg
        method = gpResolveMethod ker (gpcMethod cfg)
        params = gpResolveHyper (gpcHyper cfg) ker xs ys
    Right GPRegModel
      { gprMethod  = method
      , gprKernel  = ker
      , gprParams  = params
      , gprXraw    = xv
      , gprY       = yv
      , gprPredict = gpBuildPredict method ker params xs ys
      }


-- ---------------------------------------------------------------------------
-- 多変量 gpMulti (E3)。 'gamMulti' / 'GAMModelN' と同型 = 第1予測子を描画軸に、
-- 他予測子を訓練平均に固定した偏依存曲線 (@SingleVarModel@ として扱う)。 4 象限の
-- MV 実装: Gp/Ridge=fitGPMV (Ridge は mean のみ)、 GpRff=rffGPMV、 RidgeRff=rffRidgeMV。
-- ---------------------------------------------------------------------------

-- | [日本語]: 多変量カーネル回帰 spec。 @gpMulti cfg ["x1","x2"] "y"@ ('lmMulti' と対称)。
--   [English]: Multivariate kernel regression spec.
--   @gpMulti cfg [\"x1\",\"x2\"] \"y\"@ (symmetric with 'lmMulti').
data GPMultiSpec = GPMultiSpec !GPConfig ![Text] !Text

-- | [日本語]: @gpMulti cfg xCols yCol@ — 多予測子カーネル回帰 ('lmMulti' と対)。 第1予測子を描画軸に。
--   [English]: @gpMulti cfg xCols yCol@ — multi-predictor kernel regression
--   (paired with 'lmMulti'). The first predictor is used as the plotting
--   axis.
gpMulti :: GPConfig -> [Text] -> Text -> GPMultiSpec
gpMulti = GPMultiSpec

-- | [日本語]: MV ハイパラ方略を解決 ('gpResolveHyper' の行列版)。
--   [English]: Resolves the MV hyperparameter strategy (the matrix version
--   of 'gpResolveHyper').
gpResolveHyperMV :: HyperStrategy -> Kernel -> LA.Matrix Double -> LA.Vector Double -> GPParams
gpResolveHyperMV (FixedHyper p)  _   _ _ = p
gpResolveHyperMV AutoMarginalLik ker x y = GP.optimizeGPMV ker x y (GP.initParamsFromDataMV x y)
gpResolveHyperMV AutoCV          ker x y = GP.autoCVHyperGPMV ker x y

-- | [日本語]: 象限ごとの MV 予測子。 渡される @method@ は Periodic フォールバック後。
--   [English]: The MV predictor for each quadrant. The @method@ passed in
--   is already past the Periodic fallback.
gpBuildPredictMV
  :: GPMethod -> Kernel -> GPParams -> LA.Matrix Double -> [Double]
  -> (LA.Matrix Double -> ([Double], Maybe [Double]))
gpBuildPredictMV method ker params trainX ys = case method of
  Gp  -> \tx -> let r = GP.fitGPMV (GP.GPModel ker params) trainX yV tx
                in (LA.toList (GP.gpmvMean r), Just (LA.toList (GP.gpmvVar r)))
  Krr -> \tx -> let r = GP.fitGPMV (GP.GPModel ker params) trainX yV tx
                in (LA.toList (GP.gpmvMean r), Nothing)
  GpRff dDim seed ->
    let feats = featsMV dDim seed
        fit   = RFF.rffGPMV feats trainX ys (sqrt (max 1e-12 (gpNoiseVar params)))
    in \tx -> let ps = RFF.predictRFFGPMV fit tx in (map fst ps, Just (map snd ps))
  KrrRff dDim seed ->
    let feats = featsMV dDim seed
        fit   = RFF.rffRidgeMV feats trainX ys (max 1e-12 (gpNoiseVar params))
    in \tx -> (RFF.predictRFFRidgeMV fit tx, Nothing)
  where
    yV  = LA.fromList ys
    pP  = LA.cols trainX
    ell = gpLengthScale params
    sf  = sqrt (max 1e-12 (gpSignalVar params))
    featsMV dDim seed = case ker of
      Matern52 -> RFF.sampleRFFMatern52MVPure pP dDim ell sf seed
      _        -> RFF.sampleRFFRBFMVPure      pP dDim ell sf seed

instance Fit GPMultiSpec where
  type Fitted GPMultiSpec = GPRegModelN
  fitEither (GPMultiSpec cfg xns yn) d = do
    xs <- mapM (`reqColV` d) xns
    yv <- reqColV yn d
    let trainX = LA.fromColumns xs        -- n × p
        ys     = LA.toList yv
        ker    = gpcKernel cfg
        method = gpResolveMethod ker (gpcMethod cfg)
        params = gpResolveHyperMV (gpcHyper cfg) ker trainX yv
    pure GPRegModelN
      { gprnMethod  = method
      , gprnKernel  = ker
      , gprnParams  = params
      , gprnXraws   = xs
      , gprnNames   = xns
      , gprnYraw    = yv
      , gprnPredict = gpBuildPredictMV method ker params trainX ys
      }

-- ===========================================================================
-- 罰則付き回帰の高レベル df|-> 化 — Phase 70.7 項目 G (G1)
--
-- Ridge / Lasso / Elastic Net / MCP / SCAD / Adaptive Lasso / Group Lasso を
-- 1 spec `regularized` (+近道 `ridge`/`lasso`/`elasticNet`) に統合。`lmMulti` と対称な
-- 多予測子のみ (罰則回帰は本質的に多特徴ゆえ単回帰版は作らない)。λ は `FixedLambda` /
-- `LambdaLOOCV` (Ridge 専用閉形式) / `LambdaCV` / `LambdaCV1SE` (k-fold・seed 純粋)。
-- X は内部で標準化し係数を元スケールに戻す (MCP/SCAD の非凸 CD が標準化を要件とするため
-- ・glmnet 慣例)。 [[selectLambdaCV]] の純粋化 (`selectLambdaCVPure`/`HCV.kFold` PrimMonad 化) を土台にする。
-- ===========================================================================

-- | [日本語]: λ の決め方 (GP の 'HyperStrategy' と同型)。
--   [English]: How λ is decided (the same shape as GP's 'HyperStrategy').
data LambdaStrat
  = FixedLambda !Double        -- ^ [日本語]: 固定 λ。 [English]: A fixed λ.
  | LambdaLOOCV                -- ^ [日本語]: 閉形式 LOOCV (★Ridge 等の線形平滑器専用・他は Left)。 [English]: Closed-form LOOCV (only for linear smoothers such as Ridge; 'Left' for others).
  | LambdaCV    !Int !Word32   -- ^ [日本語]: k-fold CV (k, seed)・best (CV-MSE 最小)。 [English]: k-fold CV (k, seed); picks the best (minimum CV-MSE).
  | LambdaCV1SE !Int !Word32   -- ^ [日本語]: k-fold CV + 1-SE rule (R glmnet 既定推奨)。 [English]: k-fold CV + the 1-SE rule (R glmnet's recommended default).
  deriving (Eq, Show)

-- | [日本語]: 罰則回帰の設定。
--   [English]: Configuration for penalized regression.
data RegConfig = RegConfig { rcMethod :: !RegMethod, rcLambda :: !LambdaStrat }

-- | [日本語]: 既定 (CV で λ 選択・5-fold・seed 42)。
--   [English]: Defaults (λ selected via CV; 5-fold; seed 42).
defaultRidge, defaultLasso :: RegConfig
defaultRidge = RegConfig Ridge (LambdaCV 5 42)
defaultLasso = RegConfig Lasso (LambdaCV 5 42)

-- | [日本語]: 罰則回帰 spec。 @regularized cfg ["x1","x2"] "y"@。
--   [English]: Penalized regression spec. @regularized cfg [\"x1\",\"x2\"] \"y\"@.
data RegSpec = RegSpec !RegConfig ![Text] !Text

-- | [日本語]: @regularized cfg xCols yCol@。 多予測子のみ。
--   [English]: @regularized cfg xCols yCol@. Multi-predictor only.
regularized, regularizedMulti :: RegConfig -> [Text] -> Text -> RegSpec
regularized      = RegSpec
regularizedMulti = RegSpec               -- 同一関数の別名 (Q4)

-- | [日本語]: 近道 (λ 方略は既定 CV)。 bare = 多変量、 @*Multi@ は同一関数の別名。
--   [English]: Shortcuts (the λ strategy defaults to CV). The bare name is
--   multivariate; @*Multi@ is an alias for the same function.
ridge, ridgeMulti, lasso, lassoMulti :: [Text] -> Text -> RegSpec
ridge      = RegSpec defaultRidge
ridgeMulti = ridge
lasso      = RegSpec defaultLasso
lassoMulti = lasso

-- | [日本語]: Elastic Net 近道 (α 指定・λ 方略は既定 CV)。
--   [English]: Elastic Net shortcut (α specified; the λ strategy defaults
--   to CV).
elasticNet, elasticNetMulti :: Double -> [Text] -> Text -> RegSpec
elasticNet a    = RegSpec (RegConfig (ElasticNet a) (LambdaCV 5 42))
elasticNetMulti = elasticNet

-- 列を平均0・分散1 (population sd・guard 1e-12) に標準化。 (Xstd, means, sds)。
regStandardize :: LA.Matrix Double -> (LA.Matrix Double, [Double], [Double])
regStandardize x =
  let n     = fromIntegral (LA.rows x)
      cols  = LA.toColumns x
      means = [ LA.sumElements c / n | c <- cols ]
      sds   = [ max 1e-12 (sqrt (sum [ (v - m) ^ (2 :: Int) | v <- LA.toList c ] / n))
              | (c, m) <- zip cols means ]
      stdCol c m s = LA.cmap (\v -> (v - m) / s) c
  in (LA.fromColumns (zipWith3 stdCol cols means sds), means, sds)

-- log 等間隔 λ grid。
regLogspace :: Double -> Double -> Int -> [Double]
regLogspace lo hi k
  | k <= 1    = [lo]
  | otherwise = [ exp (log lo + (log hi - log lo) * fromIntegral i / fromIntegral (k - 1))
                | i <- [0 .. k - 1] ]

-- データ駆動 λ grid (標準化 X・中心化 y 前提)。
regLambdaGrid :: RegMethod -> LA.Matrix Double -> LA.Vector Double -> [Double]
regLambdaGrid method x y =
  let n     = fromIntegral (LA.rows x)
      lmaxL = max 1e-8 (maximum [ abs (LA.dot c y) | c <- LA.toColumns x ] / n)
  in case method of
       Ridge -> regLogspace (n * 1e-3) (n * 1e2)   50   -- ridge は XᵀX スケール (1/n 無し)
       _     -> regLogspace (lmaxL * 1e-3) lmaxL    50

-- 群 ID (列順) → fitGroupLasso の [[列 index]] パーティション (群の初出順)。
regGroupsToPartition :: [Int] -> Int -> [[Int]]
regGroupsToPartition gids p =
  let distinct []       = []
      distinct (g : gs) = g : distinct (filter (/= g) gs)
  in [ [ j | (j, g) <- zip [0 .. p - 1] gids, g == gid ] | gid <- distinct gids ]

-- Adaptive Lasso = 列リスケール法。 penalty λΣwⱼ|βⱼ| ⇔ 列 Xⱼ を 1/wⱼ 倍 → Lasso → βⱼ = β'ⱼ/wⱼ。
regAdaptiveLasso :: Double -> Double -> LA.Matrix Double -> LA.Vector Double -> RegFit
regAdaptiveLasso gamma lam x y =
  let w    = RA.adaptiveWeightsFromOLS gamma x y
      xr   = LA.fromColumns (zipWith (\c wj -> LA.scale (1 / wj) c)
                               (LA.toColumns x) (LA.toList w))
      f    = fitLasso lam xr y 1000 1e-4
      bAdj = LA.fromList (zipWith (/) (LA.toList (rfBeta f)) (LA.toList w))
      yh   = x LA.#> bAdj
  in f { rfBeta = bAdj, rfYHat = yh, rfResid = y - yh }

-- 1 つの RegMethod を λ で当てはめる (標準化 X・中心化 y 前提)。
regFitAt :: RegMethod -> Double -> LA.Matrix Double -> LA.Vector Double -> RegFit
regFitAt method lam x y = case method of
  Ridge            -> fitRidge lam x y
  Lasso            -> fitLasso lam x y 1000 1e-4
  ElasticNet a     -> fitElasticNet (a * lam) ((1 - a) * lam) x y 1000 1e-4
  MCP g            -> RA.fitMCP lam g x y 1000 1e-4
  SCAD a           -> RA.fitSCAD lam a x y 1000 1e-4
  AdaptiveLasso g  -> regAdaptiveLasso g lam x y
  GroupLasso gids  -> RA.fitGroupLasso lam (regGroupsToPartition gids (LA.cols x)) x y 1000 1e-4

-- generic k-fold CV (全 RegMethod 対応・pure 'HCV.kFold' を seed で回す)。 (bestλ, 1seλ, λ grid, mean MSE)。
regKFoldCV :: RegMethod -> Int -> Word32 -> [Double] -> LA.Matrix Double -> LA.Vector Double
           -> (Double, Double, [Double], [Double])
regKFoldCV method k seed lams x y =
  let folds = runST (initialize (V.singleton seed) >>= HCV.kFold k (LA.rows x))
      sub idx = ( x LA.? idx
                , LA.fromList [ y `LA.atIndex` i | i <- idx ] )
      foldMSE lam (tr, te) =
        let (xTr, yTr) = sub tr
            (xTe, yTe) = sub te
            fit = regFitAt method lam xTr yTr
            r   = yTe - (xTe LA.#> rfBeta fit)
        in LA.sumElements (LA.cmap (^ (2 :: Int)) r) / fromIntegral (max 1 (length te))
      perLam lam =
        let scores = [ foldMSE lam f | f@(_, te) <- folds, not (null te) ]
            nF = fromIntegral (max 1 (length scores))
            m  = sum scores / nF
            v  = sum [ (s - m) ^ (2 :: Int) | s <- scores ] / max 1 (nF - 1)
        in (lam, m, sqrt (v / nF))
      stats = map perLam lams
      pick a@(_, ma, _) b@(_, mb, _) = if ma <= mb then a else b
      (bestL, bestM, bestSE) = foldr1 pick stats
      thr   = bestM + bestSE
      oneSe = maximum (bestL : [ lam | (lam, m, _) <- stats, m <= thr ])
  in (bestL, oneSe, lams, [ m | (_, m, _) <- stats ])

-- λ 方略を解決して (選択 λ, 標準化空間 fit, CV/LOOCV パス) を返す。
regResolve :: RegConfig -> LA.Matrix Double -> LA.Vector Double
           -> Either String (Double, RegFit, Maybe ([Double], [Double]))
regResolve (RegConfig method strat) x y = case strat of
  FixedLambda lam -> Right (lam, regFitAt method lam x y, Nothing)
  LambdaLOOCV -> case method of
    Ridge ->
      let lams   = regLambdaGrid Ridge x y
          scores = [ RFF.loocvFromPhi x y lam | lam <- lams ]
          best   = fst (foldr1 (\a b -> if snd a <= snd b then a else b) (zip lams scores))
      in Right (best, regFitAt Ridge best x y, Just (lams, scores))
    _ -> Left "LambdaLOOCV は線形平滑器 (Ridge) 専用です。 L1/非凸は LambdaCV を使ってください"
  LambdaCV k seed ->
    let lams = regLambdaGrid method x y
        (best, _, gl, ms) = regKFoldCV method k seed lams x y
    in Right (best, regFitAt method best x y, Just (gl, ms))
  LambdaCV1SE k seed ->
    let lams = regLambdaGrid method x y
        (_, one, gl, ms) = regKFoldCV method k seed lams x y
    in Right (one, regFitAt method one x y, Just (gl, ms))

instance Fit RegSpec where
  type Fitted RegSpec = RegModel
  fitEither (RegSpec cfg xns yn) d = do
    xRaw <- reqColsM xns d
    yv   <- reqColV yn d
    case rcMethod cfg of
      GroupLasso gids | length gids /= LA.cols xRaw ->
        Left ("GroupLasso: 群 ID の長さ " <> show (length gids)
              <> " が列数 " <> show (LA.cols xRaw) <> " と一致しません")
      _ -> Right ()
    let (xStd, means, sds) = regStandardize xRaw
        n    = fromIntegral (LA.rows xRaw)
        ybar = LA.sumElements yv / n
        yC   = LA.cmap (subtract ybar) yv
    (lam, fitStd, mbCV) <- regResolve cfg xStd yC
    let betaOrig  = zipWith (/) (LA.toList (rfBeta fitStd)) sds   -- βⱼ = β̃ⱼ / sⱼ
        intercept = ybar - sum (zipWith (*) betaOrig means)
    Right RegModel
      { rmgMethod    = rcMethod cfg
      , rmgNames     = xns
      , rmgLambda    = lam
      , rmgIntercept = intercept
      , rmgCoefs     = betaOrig
      , rmgFitStd    = fitStd
      , rmgCVPath    = mbCV
      , rmgXraw      = xRaw
      , rmgYraw      = yv
      }

-- | [日本語]: 罰則化回帰 ('RegModel') の case (行) bootstrap 係数サマリ。
--
-- 各 replicate で行を再標本化し、 __λ は full-fit で選択済の 'rmgLambda' に固定__
-- (CV を再実行しない) して 'regFitAt' で refit、 元スケール係数を集めて
-- 'bootCoefRows' で要約する。 標準化・中心化は replicate ごとに再計算する。
--
-- ⚠ これは __penalized の bootstrap percentile 区間であって有意性検定ではない__。
-- Lasso 等の変数選択を伴う罰則化では post-selection inference が別問題として
-- 存在し、 単純な percentile 区間はカバレッジを保証しない (selection の不確実性は
-- λ 固定 bootstrap だけでは捉えきれない)。 探索的な不確実性目安として使うこと。
--   [English]: Case (row) bootstrap coefficient summary for penalized
--   regression ('RegModel').
--
-- Resamples rows for each replicate, refits via 'regFitAt' with
-- __λ fixed at the 'rmgLambda' already selected by the full fit__ (CV is
-- not re-run), and summarizes the collected original-scale coefficients via
-- 'bootCoefRows'. Standardization\/centering is recomputed for each
-- replicate.
--
-- Warning: this is __a penalized bootstrap percentile interval, not a__
-- __significance test__. For penalization involving variable selection (e.g.
-- Lasso), post-selection inference is a separate problem, and a naive
-- percentile interval doesn't guarantee coverage (the uncertainty from
-- selection isn't fully captured by a λ-fixed bootstrap alone). Use this as
-- an exploratory measure of uncertainty.
instance HasCoefBoot RegModel where
  coefSummaryBoot seed b m =
    let xRaw    = rmgXraw m
        yv      = rmgYraw m
        n       = LA.rows xRaw
        idxSets = resampleRows seed b n
        repBeta idxs =
          let xr            = xRaw LA.? idxs
              yr            = LA.fromList [ yv `LA.atIndex` i | i <- idxs ]
              (xStd, _, sds) = regStandardize xr
              ybar          = LA.sumElements yr / fromIntegral (max 1 n)
              yC            = LA.cmap (subtract ybar) yr
              fitStd        = regFitAt (rmgMethod m) (rmgLambda m) xStd yC
          in zipWith (/) (LA.toList (rfBeta fitStd)) sds
        reps = map repBeta idxSets
    in bootCoefRows (rmgNames m) (rmgCoefs m) reps

-- | [日本語]: 罰則化回帰 ('RegModel') の統一要約玄関。
--
-- ⚠ これは bootstrap percentile (seed=42 / B=2000) であって__有意性検定ではない__
-- (post-selection inference は別問題。 'HasCoefBoot' RegModel の注記参照)。
-- 'HasCoefBoot' RegModel が Fit 層にあるため、 'HasReport' instance もここに置く。
--   [English]: Unified summary entry point for penalized regression
--   ('RegModel').
--
-- Warning: this is a bootstrap percentile (seed=42, B=2000), __not a__
-- __significance test__ (post-selection inference is a separate problem; see
-- the note on the 'HasCoefBoot' RegModel instance). Since 'HasCoefBoot'
-- RegModel lives in the Fit layer, the 'HasReport' instance is placed here
-- too.
instance HasReport RegModel where
  modelReport m = CoefReport (coefSummaryBoot 42 2000 m)

-- --- 行列入力モデルの高レベル化 spec (列名リスト) — Phase 70.A -------------
--
-- PCA / PLS など「行列入力」モデルも 'df |-> spec' で当てられるように、 列名リストを
-- 取る spec + 'Fit' instance を設ける ('reqColsM' で列名 → 行列)。 行列直叩き
-- ('pca' / 'fitPLS') は低レベル退避。 結果型 ('PCAResult' / 'PLSFit') は既存のまま。

-- | [日本語]: PCA spec。 @pca std mK ["x1","x2",…]@ (std = 標準化方針・mK = 保持成分数)。
--   [English]: PCA spec. @pca std mK [\"x1\",\"x2\",…]@ (std = standardization
--   policy, mK = number of components to keep).
data PCASpec = PCASpec !PCAStandardize !(Maybe Int) ![Text]

-- | [日本語]: @pca std mK cols@ — 列 @cols@ で PCA ('PCAResult') を当てる spec。
--   @df |-> pca CenterScale (Just 3) ["x1","x2","x3"]@。
--   [English]: @pca std mK cols@ — a spec that fits PCA ('PCAResult') on
--   columns @cols@. @df |-> pca CenterScale (Just 3) [\"x1\",\"x2\",\"x3\"]@.
pca :: PCAStandardize -> Maybe Int -> [Text] -> PCASpec
pca = PCASpec

instance Fit PCASpec where
  type Fitted PCASpec = PCAResult
  fitEither (PCASpec std mK cols) d = PCALow.pca std mK <$> reqColsM cols d

-- Phase 75.21: MDS (多次元尺度構成法) 高レベル spec。 教師なし変換ゆえ PCA と同様
-- @df |->@ に乗り、 **モデル型 'MDSResult'** (PCAResult 同格) を返す。 群色付けのため
-- 'toFrame' で **元データ (factor 温存)** を保持する (数値源なら数値列を再構築)。
data MDSSpec = MDSSpec !MDSConfig ![Text]

-- | [日本語]: @mds cfg cols@ — 列 @cols@ で MDS ('MDSResult') を当てる spec。
--   @m = df |-> mds defaultMDS ["x1","x2","x3"]@ → @noDf |>> toPlot m@ (単色散布)。
--   Sammon は @mds defaultMDS { mdsMethod = MDSSammon } cols@。
--   [English]: @mds cfg cols@ — a spec that fits MDS ('MDSResult') on
--   columns @cols@. @m = df |-> mds defaultMDS [\"x1\",\"x2\",\"x3\"]@ →
--   @noDf |>> toPlot m@ (a monochrome scatter). Sammon is
--   @mds defaultMDS { mdsMethod = MDSSammon } cols@.
mds :: MDSConfig -> [Text] -> MDSSpec
mds = MDSSpec

instance Fit MDSSpec where
  type Fitted MDSSpec = MDSResult
  fitEither (MDSSpec cfg cols) d = runMDS cfg (toFrame d) cols

-- | [日本語]: PLS spec。 @pls cfg ["x1","x2"] ["y1","y2"]@ (X 列・Y 列を分けて指定)。
--   [English]: PLS spec. @pls cfg [\"x1\",\"x2\"] [\"y1\",\"y2\"]@ (X columns and
--   Y columns specified separately).
data PLSSpec = PLSSpec !PLSConfig ![Text] ![Text]

-- | [日本語]: @pls cfg xcols ycols@ — X 列・Y 列で PLS ('PLSFit') を当てる spec。
--   @df |-> pls defaultPLS ["x1","x2"] ["y1","y2"]@。
--   [English]: @pls cfg xcols ycols@ — a spec that fits PLS ('PLSFit') using
--   X columns and Y columns.
--   @df |-> pls defaultPLS [\"x1\",\"x2\"] [\"y1\",\"y2\"]@.
pls :: PLSConfig -> [Text] -> [Text] -> PLSSpec
pls = PLSSpec

instance Fit PLSSpec where
  type Fitted PLSSpec = PLSFit
  fitEither (PLSSpec cfg xcols ycols) d = do
    x <- reqColsM xcols d
    y <- reqColsM ycols d
    either (Left . T.unpack) Right (fitPLS cfg x y)

-- | [日本語]: 判別分析 (LDA) spec。 @lda ["x1","x2"] "class"@ (特徴列 + クラス列・クラスは整数化)。
--   [English]: Discriminant analysis (LDA) spec. @lda [\"x1\",\"x2\"] \"class\"@
--   (feature columns + class column; the class is turned into integers).
data LDASpec = LDASpec ![Text] !Text

-- | [日本語]: @lda featureCols classCol@ — LDA ('DiscriminantFit') を当てる spec。
--   クラス列は数値を 'round' で整数ラベル化する。
--   [English]: @lda featureCols classCol@ — a spec that fits LDA
--   ('DiscriminantFit'). The class column's numeric values are turned into
--   integer labels via 'round'.
lda :: [Text] -> Text -> LDASpec
lda = LDASpec

instance Fit LDASpec where
  type Fitted LDASpec = DiscriminantFit
  fitEither (LDASpec feats clsCol) d = do
    x  <- reqColsM feats d
    yv <- reqColV clsCol d
    let yInt = V.fromList (map round (LA.toList yv)) :: V.Vector Int
    either (Left . T.unpack) Right (fitLDA x yInt)
  predictorCols (LDASpec feats _) = feats   -- 特徴標準化は可 (応答=クラスゆえ y は標準化せず)

-- | [日本語]: 正準相関分析 (CCA) spec。 @ccaOf ["x1","x2"] ["y1","y2"]@ (2 ブロックの列)。
--   [English]: Canonical correlation analysis (CCA) spec.
--   @ccaOf [\"x1\",\"x2\"] [\"y1\",\"y2\"]@ (columns for the two blocks).
data CCASpec = CCASpec ![Text] ![Text]

-- | [日本語]: @ccaOf xcols ycols@ — CCA ('CCAFit') を当てる spec (CCAFit は現状 toPlot 非対象)。
--   [English]: @ccaOf xcols ycols@ — a spec that fits CCA ('CCAFit'; CCAFit
--   isn't currently a toPlot target).
ccaOf :: [Text] -> [Text] -> CCASpec
ccaOf = CCASpec

instance Fit CCASpec where
  type Fitted CCASpec = CCAFit
  fitEither (CCASpec xcols ycols) d = cca <$> reqColsM xcols d <*> reqColsM ycols d

-- | [日本語]: ラベル列を整数 ('round') の 'VU.Vector Int' で引く (分類器の y)。
--   [English]: Looks up a label column as a 'VU.Vector Int' via 'round'
--   (the y of a classifier).
reqLabelI :: ColumnSource d => Text -> d -> Either String (VU.Vector Int)
reqLabelI n d = VU.fromList . map round . LA.toList <$> reqColV n d

-- | [日本語]: ラベル列を 'VU.Vector Double' で引く (回帰の y)。
--   [English]: Looks up a label column as a 'VU.Vector Double' (the y of a
--   regression).
reqLabelD :: ColumnSource d => Text -> d -> Either String (VU.Vector Double)
reqLabelD n d = VU.fromList . LA.toList <$> reqColV n d

-- --- 教師あり ML 分類器/回帰器の高レベル化 spec (特徴列 + ラベル列) — Phase 70.A ---
--
-- いずれも純粋 fit (RNG なし)。 特徴は 'reqColsM' で行列化、 ラベルは整数 / 実数で引く。

-- | [日本語]: 勾配ブースティング回帰 spec。 @gbmReg cfg ["x1","x2"] "y"@。
--   [English]: Gradient boosting regression spec.
--   @gbmReg cfg [\"x1\",\"x2\"] \"y\"@.
data GBRSpec = GBRSpec !GBConfig ![Text] !Text
-- | [日本語]: @gbmReg cfg featCols yCol@ — GBM 回帰 ('GBRegressor')。
--   [English]: @gbmReg cfg featCols yCol@ — GBM regression ('GBRegressor').
gbmReg :: GBConfig -> [Text] -> Text -> GBRSpec
gbmReg = GBRSpec
instance Fit GBRSpec where
  type Fitted GBRSpec = GBRegressor
  fitEither (GBRSpec cfg feats yc) d = fitGBRegressor cfg <$> reqColsM feats d <*> reqLabelD yc d

-- | [日本語]: 勾配ブースティング分類 spec。 @gbmCls cfg ["x1","x2"] "cls"@ (ラベルは {0,1})。
--   [English]: Gradient boosting classification spec.
--   @gbmCls cfg [\"x1\",\"x2\"] \"cls\"@ (labels are {0,1}).
data GBCSpec = GBCSpec !GBConfig ![Text] !Text
-- | [日本語]: @gbmCls cfg featCols clsCol@ — GBM 分類 ('GBClassifier')。
--   [English]: @gbmCls cfg featCols clsCol@ — GBM classification
--   ('GBClassifier').
gbmCls :: GBConfig -> [Text] -> Text -> GBCSpec
gbmCls = GBCSpec
instance Fit GBCSpec where
  type Fitted GBCSpec = GBClassifier
  fitEither (GBCSpec cfg feats cc) d = fitGBClassifier cfg <$> reqColsM feats d <*> reqLabelI cc d

-- | [日本語]: 決定木 spec。 @decisionTree cfg ["x1","x2"] "cls"@。
--   [English]: Decision tree spec. @decisionTree cfg [\"x1\",\"x2\"] \"cls\"@.
data DTSpec = DTSpec !DTConfig ![Text] !Text
-- | [日本語]: @decisionTree cfg featCols clsCol@ — 決定木 ('DTFit'・行列版 'fitDTV')。 fit 時に
--   実列名 (feats) とクラス列の levels を載せて返すので @treePlot@/@printRpart@ が
--   名前を手渡し不要 ('RFClassifierFit' と同型)。 クラス列は factor(text)/数値の両対応。
--   [English]: @decisionTree cfg featCols clsCol@ — a decision tree ('DTFit';
--   the matrix version 'fitDTV'). At fit time, the actual column names
--   (feats) and the class column's levels are attached to the result, so
--   @treePlot@\/@printRpart@ don't need names passed in manually (the same
--   shape as 'RFClassifierFit'). The class column supports both
--   factor(text) and numeric.
decisionTree :: DTConfig -> [Text] -> Text -> DTSpec
decisionTree = DTSpec
instance Fit DTSpec where
  type Fitted DTSpec = DTFit
  fitEither (DTSpec cfg feats cc) d = do
    x            <- reqColsM feats d
    (y, classes) <- reqLabelWithLevels cc d
    pure (DTFit (fitDTV cfg x y) feats classes)

-- | [日本語]: k-NN 分類 spec。 @knnCls 5 ["x1","x2"] "cls"@。
--   [English]: k-NN classification spec. @knnCls 5 [\"x1\",\"x2\"] \"cls\"@.
data KNNCSpec = KNNCSpec !Int ![Text] !Text
-- | [日本語]: @knnCls k featCols clsCol@ — k-NN 分類 ('KNNClassifier')。
--   [English]: @knnCls k featCols clsCol@ — k-NN classification
--   ('KNNClassifier').
knnCls :: Int -> [Text] -> Text -> KNNCSpec
knnCls = KNNCSpec
instance Fit KNNCSpec where
  type Fitted KNNCSpec = KNNClassifier
  -- fit 時にクラス列の levels を注入 (confusion/凡例でクラス名表示・factor/数値両対応)。
  fitEither (KNNCSpec k feats cc) d = do
    x            <- reqColsM feats d
    (y, classes) <- reqLabelWithLevels cc d
    pure ((fitKNNC k x y) { knnCClassNames = classes })
  predictorCols (KNNCSpec _ feats _) = feats   -- ★距離ベース・標準化の本命
  -- responseCol = Nothing (既定)。 分類の応答はクラスラベルゆえ標準化しない。

-- | [日本語]: k-NN 回帰 spec。 @knnReg 5 ["x1","x2"] "y"@。
--   [English]: k-NN regression spec. @knnReg 5 [\"x1\",\"x2\"] \"y\"@.
data KNNRSpec = KNNRSpec !Int ![Text] !Text
-- | [日本語]: @knnReg k featCols yCol@ — k-NN 回帰 ('KNNRegressor')。
--   [English]: @knnReg k featCols yCol@ — k-NN regression ('KNNRegressor').
knnReg :: Int -> [Text] -> Text -> KNNRSpec
knnReg = KNNRSpec
instance Fit KNNRSpec where
  type Fitted KNNRSpec = KNNRegressor
  fitEither (KNNRSpec k feats yc) d = fitKNNR k <$> reqColsM feats d <*> reqLabelD yc d
  predictorCols (KNNRSpec _ feats _) = feats   -- ★距離ベース・標準化の本命
  responseCol   (KNNRSpec _ _ yc)    = Just yc

-- | [日本語]: Naive Bayes (Gaussian) spec。 @naiveBayes ["x1","x2"] "cls"@ (連続特徴)。
--   [English]: Naive Bayes (Gaussian) spec.
--   @naiveBayes [\"x1\",\"x2\"] \"cls\"@ (continuous features).
data NBSpec = NBSpec ![Text] !Text
-- | [日本語]: @naiveBayes featCols clsCol@ — Gaussian NB ('NBModel' = 'NBGaussian')。
--   [English]: @naiveBayes featCols clsCol@ — Gaussian NB ('NBModel' =
--   'NBGaussian').
naiveBayes :: [Text] -> Text -> NBSpec
naiveBayes = NBSpec
instance Fit NBSpec where
  type Fitted NBSpec = NBModel
  fitEither (NBSpec feats cc) d = do
    x            <- reqColsM feats d
    (y, classes) <- reqLabelWithLevels cc d
    pure (NBGaussian ((fitGNB x y) { gnbClassNames = classes }))
  predictorCols (NBSpec feats _) = feats   -- 特徴標準化は可 (応答=クラスゆえ y は標準化せず)

-- --- seed 純粋化した RNG モデル spec (KMeans / RandomForest) — Phase 70.A ---
--
-- KMeans / RandomForest は RNG を使う ('MWC.GenIO -> IO')。 'df |->' (純粋 'fitEither')
-- に載せるため、 サンプラ本体を seed 純粋化 (Phase 50 と同方針) した変種
-- @kMeansPure@ / @fitRFVPure@ を呼ぶ。 spec は @seed :: Word32@ を取り、 同 seed →
-- ビット同一の決定的結果を返す。

-- | [日本語]: KMeans クラスタリング spec。 @kmeans cfg seed ["x1","x2"]@ (cfg = k 等・seed = 乱数種)。
--   [English]: KMeans clustering spec. @kmeans cfg seed [\"x1\",\"x2\"]@
--   (cfg = k etc.; seed = random seed).
data KMeansSpec = KMeansSpec !KMeansConfig !Word32 ![Text]
-- | [日本語]: @kmeans cfg seed featCols@ — KMeans ('KMeansResult'・toPlot = centroid 散布)。
--   @df |-> kmeans (defaultKMeans 3) 42 ["x1","x2"]@。
--   [English]: @kmeans cfg seed featCols@ — KMeans ('KMeansResult'; toPlot =
--   a centroid scatter). @df |-> kmeans (defaultKMeans 3) 42 [\"x1\",\"x2\"]@.
kmeans :: KMeansConfig -> Word32 -> [Text] -> KMeansSpec
kmeans = KMeansSpec
instance Fit KMeansSpec where
  type Fitted KMeansSpec = KMeansResult
  fitEither (KMeansSpec cfg seed cols) d = (\x -> kMeansPure cfg x seed) <$> reqColsM cols d

-- | [日本語]: ランダムフォレスト回帰 spec。 @randomForestReg cfg seed ["x1","x2"] "y"@ (cfg = 木数等・seed = 乱数種)。
--   [English]: Random forest regression spec.
--   @randomForestReg cfg seed [\"x1\",\"x2\"] \"y\"@ (cfg = number of trees
--   etc.; seed = random seed).
data RFSpec = RFSpec !RFConfig !Word32 ![Text] !Text
-- | [日本語]: @randomForestReg cfg seed featCols yCol@ — RF 回帰 ('RandomForest'・toPlot = 特徴重要度 bar)。
--   分類 'randomForestCls' と対の命名 (無印 randomForest は廃止)。
--   @df |-> randomForestReg defaultRandomForest 42 ["x1","x2"] "y"@。
--   [English]: @randomForestReg cfg seed featCols yCol@ — RF regression
--   ('RandomForest'; toPlot = a feature-importance bar chart). Named to pair
--   with the classification 'randomForestCls' (the bare @randomForest@ is
--   deprecated).
--   @df |-> randomForestReg defaultRandomForest 42 [\"x1\",\"x2\"] \"y\"@.
randomForestReg :: RFConfig -> Word32 -> [Text] -> Text -> RFSpec
randomForestReg = RFSpec
instance Fit RFSpec where
  type Fitted RFSpec = RandomForest
  -- fit 時に手元にある実列名 (feats) をモデルに載せる (df|-> 経路の責務)。
  fitEither (RFSpec cfg seed feats yc) d =
    (\x y -> (fitRFVPure cfg x y seed) { rfFeatureNames = feats })
      <$> reqColsM feats d <*> reqLabelD yc d

-- | [日本語]: DirectLiNGAM (因果探索) の高レベル spec。 @directLingam cfg cols@ =
--   列名で n×p 行列を組み因果構造を推定する (教師なし・y 無し)。 結果は変数名を添えた
--   'LiNGAMFitted' で、 @toPlot@ が__実変数名の DAG__ を描く (低レベルは @x0..@)。
--   @df |-> directLingam defaultDirectLiNGAMConfig ["smoking","tar","cancer"]@。
--   [English]: High-level spec for DirectLiNGAM (causal discovery).
--   @directLingam cfg cols@ builds an n×p matrix from column names and
--   estimates the causal structure (unsupervised, no y). The result is a
--   'LiNGAMFitted' carrying the variable names, so @toPlot@ draws a
--   __DAG with real variable names__ (the low-level version uses @x0..@).
--   @df |-> directLingam defaultDirectLiNGAMConfig [\"smoking\",\"tar\",\"cancer\"]@.
data DirectLiNGAMSpec = DirectLiNGAMSpec !DirectLiNGAMConfig ![Text]

directLingam :: DirectLiNGAMConfig -> [Text] -> DirectLiNGAMSpec
directLingam = DirectLiNGAMSpec

instance Fit DirectLiNGAMSpec where
  type Fitted DirectLiNGAMSpec = LiNGAMFitted DirectLiNGAMFit
  fitEither (DirectLiNGAMSpec cfg cols) d =
    (\x -> LiNGAMFitted (fitDirectLiNGAM cfg x) cols) <$> reqColsM cols d

-- | [日本語]: ParceLiNGAM (bottom-up sink 探索・潜在交絡に頑健) の高レベル spec。
--   @df |-> parceLingam defaultParceConfig cols@。 DAG は Direct と同型 (名前付き)。
--   [English]: High-level spec for ParceLiNGAM (bottom-up sink search;
--   robust to latent confounders). @df |-> parceLingam defaultParceConfig
--   cols@. The DAG has the same shape as Direct's (named).
data ParceLiNGAMSpec = ParceLiNGAMSpec !ParceConfig ![Text]

parceLingam :: ParceConfig -> [Text] -> ParceLiNGAMSpec
parceLingam = ParceLiNGAMSpec

instance Fit ParceLiNGAMSpec where
  type Fitted ParceLiNGAMSpec = LiNGAMFitted ParceFit
  fitEither (ParceLiNGAMSpec cfg cols) d =
    (\x -> LiNGAMFitted (fitParceLiNGAM cfg x) cols) <$> reqColsM cols d

-- | [日本語]: MultiGroupLiNGAM (複数群で共通 DAG 構造・Shimizu 2012) の高レベル spec。
--   @df |-> multiGroupLingam defaultMultiGroupConfig cols groupCol@。 @groupCol@ = __数値の群コード列__ で行を分割し各群を fit する。 DAG は多数決の共通構造 (名前付き)。
--   [English]: High-level spec for MultiGroupLiNGAM (a common DAG structure
--   across multiple groups; Shimizu 2012).
--   @df |-> multiGroupLingam defaultMultiGroupConfig cols groupCol@.
--   @groupCol@ splits rows by __a numeric group-code column__ and fits each
--   group. The DAG is the majority-vote common structure (named).
data MultiGroupLiNGAMSpec = MultiGroupLiNGAMSpec !MultiGroupConfig ![Text] !Text

multiGroupLingam :: MultiGroupConfig -> [Text] -> Text -> MultiGroupLiNGAMSpec
multiGroupLingam = MultiGroupLiNGAMSpec

instance Fit MultiGroupLiNGAMSpec where
  type Fitted MultiGroupLiNGAMSpec = LiNGAMFitted MultiGroupFit
  fitEither (MultiGroupLiNGAMSpec cfg cols gcol) d = do
    x <- reqColsM cols d
    g <- reqColV gcol d
    let codes    = LA.toList g
        distinct = nub codes                          -- 群ラベル (出現順)
        rowsFor gc = [ i | (i, c) <- zip [0 ..] codes, c == gc ]
        groups   = [ x LA.? rowsFor gc | gc <- distinct ]
    if null distinct
      then Left "multiGroupLingam: group 列が空です"
      else Right (LiNGAMFitted (fitMultiGroupLiNGAM cfg groups) cols)

-- | [日本語]: VARLiNGAM (時系列因果・同時刻 + 時間ラグ) の高レベル spec。
--   @df |-> varLingam defaultVARLiNGAMConfig cols@。 行 = 時刻順。 DAG は同時刻辺 + ラグ辺
--   (@x_j[t-l] → x_i[t]@) の__時間ラグ DAG__。
--   [English]: High-level spec for VARLiNGAM (time-series causality;
--   contemporaneous + time-lagged). @df |-> varLingam defaultVARLiNGAMConfig
--   cols@. Rows are in time order. The DAG is __a time-lagged DAG__ with
--   contemporaneous edges + lag edges (@x_j[t-l] → x_i[t]@).
data VARLiNGAMSpec = VARLiNGAMSpec !VARLiNGAMConfig ![Text]

varLingam :: VARLiNGAMConfig -> [Text] -> VARLiNGAMSpec
varLingam = VARLiNGAMSpec

instance Fit VARLiNGAMSpec where
  type Fitted VARLiNGAMSpec = LiNGAMFitted VARLiNGAMFit
  fitEither (VARLiNGAMSpec cfg cols) d =
    (\x -> LiNGAMFitted (fitVARLiNGAM cfg x) cols) <$> reqColsM cols d

-- | [日本語]: PairwiseLiNGAM (2 変数の因果向き判定) の高レベル spec。
--   @df |-> pairwiseLingam 0.0 "x" "y"@ (thr = |score| 閾値・0 で符号のみ)。 DAG でなく
--   2 ノード + 検出向きの矢印 (Inconclusive は無向)。
--   [English]: High-level spec for PairwiseLiNGAM (determining the causal
--   direction between two variables). @df |-> pairwiseLingam 0.0 \"x\" \"y\"@
--   (thr = |score| threshold; 0 uses the sign only). Not a DAG — 2 nodes plus
--   the detected direction's arrow (undirected if Inconclusive).
data PairwiseLiNGAMSpec = PairwiseLiNGAMSpec !Double !Text !Text

pairwiseLingam :: Double -> Text -> Text -> PairwiseLiNGAMSpec
pairwiseLingam = PairwiseLiNGAMSpec

instance Fit PairwiseLiNGAMSpec where
  type Fitted PairwiseLiNGAMSpec = LiNGAMFitted PairwiseResult
  fitEither (PairwiseLiNGAMSpec thr xc yc) d =
    (\x y -> LiNGAMFitted (pairwiseLiNGAM thr x y) [xc, yc])
      <$> reqColV xc d <*> reqColV yc d

-- | [日本語]: BootstrapLiNGAM (エッジ確信度) の高レベル spec。 @df |-> bootstrapLingam cfg cols@。
--   B 回リサンプルして各エッジの出現確率を出す (seed 純粋・決定的)。 toPlot = 確信度 DAG。
--   [English]: High-level spec for BootstrapLiNGAM (edge confidence).
--   @df |-> bootstrapLingam cfg cols@. Resamples B times to produce each
--   edge's occurrence probability (pure via seed, deterministic).
--   toPlot = a confidence DAG.
data BootstrapLiNGAMSpec = BootstrapLiNGAMSpec !BootstrapConfig ![Text]

bootstrapLingam :: BootstrapConfig -> [Text] -> BootstrapLiNGAMSpec
bootstrapLingam = BootstrapLiNGAMSpec

instance Fit BootstrapLiNGAMSpec where
  type Fitted BootstrapLiNGAMSpec = LiNGAMFitted BootstrapResult
  fitEither (BootstrapLiNGAMSpec cfg cols) d =
    (\x -> LiNGAMFitted (fitBootstrapLiNGAMPure cfg x) cols) <$> reqColsM cols d

-- | [日本語]: ICA-LiNGAM (Shimizu 2006・FastICA + Hungarian) の高レベル spec。
--   @df |-> icaLingam cfg cols@ (seed 純粋・決定的)。 DAG は名前付き (ilAdjacency)。
--   [English]: High-level spec for ICA-LiNGAM (Shimizu 2006; FastICA +
--   Hungarian algorithm). @df |-> icaLingam cfg cols@ (pure via seed,
--   deterministic). The DAG is named (ilAdjacency).
data ICALiNGAMSpec = ICALiNGAMSpec !ICALiNGAMConfig ![Text]

icaLingam :: ICALiNGAMConfig -> [Text] -> ICALiNGAMSpec
icaLingam = ICALiNGAMSpec

instance Fit ICALiNGAMSpec where
  type Fitted ICALiNGAMSpec = LiNGAMFitted ICALiNGAMFit
  fitEither (ICALiNGAMSpec cfg cols) d =
    (\x -> LiNGAMFitted (fitICALiNGAMPure cfg x) cols) <$> reqColsM cols d

-- | [日本語]: Pearson 相関ネットワーク。 @df |-> correlationOf thr cols@ で相関行列を求め、
--   @toPlot@ で @|r| > thr@ の対を辺にしたグラフを描く (因果でなく__周辺相関__)。 LiNGAM DAG
--   と対比すると、 相関は間接・交絡も辺にして過剰に密になり、 LiNGAM が直接因果に削減するのが分かる。
--   [English]: A Pearson correlation network. @df |-> correlationOf thr
--   cols@ computes the correlation matrix, and @toPlot@ draws a graph with
--   an edge for every pair where @|r| > thr@ (__marginal correlation__, not
--   causation). Contrasted with a LiNGAM DAG, correlation also edges
--   indirect \/ confounded relationships and becomes overly dense — which
--   shows how LiNGAM reduces this down to direct causation.
data CorrelationSpec = CorrelationSpec !Double ![Text]

correlationOf :: Double -> [Text] -> CorrelationSpec
correlationOf = CorrelationSpec

instance Fit CorrelationSpec where
  type Fitted CorrelationSpec = CorrelationGraph
  fitEither (CorrelationSpec thr cols) d =
    (\x -> CorrelationGraph (correlationMatrix x) cols thr) <$> reqColsM cols d

-- | [日本語]: ランダムフォレスト分類 spec。 @randomForestCls cfg seed ["x1","x2"] "cls"@ (cfg = 木数等・seed = 乱数種)。
--   [English]: Random forest classification spec.
--   @randomForestCls cfg seed [\"x1\",\"x2\"] \"cls\"@ (cfg = number of trees
--   etc.; seed = random seed).
data RFCSpec = RFCSpec !RFCConfig !Word32 ![Text] !Text
-- | [日本語]: @randomForestCls cfg seed featCols clsCol@ — RF 分類 ('RFClassifierFit'・toPlot = 重要度 2 パネル
--   permutation/gini)。 回帰 'randomForestReg' と対の命名 (旧名 rfClassifier)。
--   @df |-> randomForestCls defaultRFCConfig 42 ["x1","x2"] "cls"@。
--   [English]: @randomForestCls cfg seed featCols clsCol@ — RF
--   classification ('RFClassifierFit'; toPlot = a two-panel
--   permutation\/gini importance chart). Named to pair with the regression
--   'randomForestReg' (formerly named rfClassifier).
--   @df |-> randomForestCls defaultRFCConfig 42 [\"x1\",\"x2\"] \"cls\"@.
randomForestCls :: RFCConfig -> Word32 -> [Text] -> Text -> RFCSpec
randomForestCls = RFCSpec
instance Fit RFCSpec where
  type Fitted RFCSpec = RFClassifierFit
  -- 回帰 RFSpec と同方針: fit 時の実列名 (feats) をモデルへ。 seed 純粋版で df|-> に載せる。
  fitEither (RFCSpec cfg seed feats cc) d =
    (\x y -> (fitRFClassifierPure cfg x y seed) { rfcFeatureNames = feats })
      <$> reqColsM feats d <*> reqLabelI cc d
  -- 木ベースはスケール不変ゆえ predictorCols は設けない (標準化不要)。

-- Phase 75.12: カーネル SVM 分類 (多クラス one-vs-rest・SMO は乱数不使用ゆえ純粋) 高レベル spec。
data SVMSpec = SVMSpec !SVMConfig ![Text] !Text
-- | [日本語]: @svmCls cfg featCols clsCol@ — カーネル SVM 分類 ('SVMMulti'・RBF/poly で
--   非線形境界・真の SV)。 決定境界/SV 可視化は @decisionBoundaryOf@/@svmSupportVectorsOf@。
--   ハイパラ調整は @svmHyper@ ('SVMConfig') に畳む (GP と同型): 'SVMFixed' で固定 fit、
--   'SVMTuneCV' grid で k-fold CV 探索 → 最良ハイパラで再学習 (別動詞は作らない)。
--   [English]: @svmCls cfg featCols clsCol@ — kernel SVM classification
--   ('SVMMulti'; nonlinear boundaries with RBF\/poly, true SVs). Decision
--   boundary\/SV visualization via @decisionBoundaryOf@\/@svmSupportVectorsOf@.
--   Hyperparameter tuning is folded into @svmHyper@ ('SVMConfig'), the same
--   shape as GP: 'SVMFixed' for a fixed fit, 'SVMTuneCV' grid for a k-fold
--   CV search → refit with the best hyperparameters (no separate verb is
--   created).
svmCls :: SVMConfig -> [Text] -> Text -> SVMSpec
svmCls = SVMSpec
instance Fit SVMSpec where
  type Fitted SVMSpec = SVMMulti
  fitEither (SVMSpec cfg feats clsCol) d = do
    x <- reqColsM feats d
    (y, classes) <- reqLabelWithLevels clsCol d
    let cfg' = case svmHyper cfg of
                 SVMFixed       -> cfg
                 SVMTuneCV grid -> fst (tuneSVM cfg grid x y)
    pure ((fitSVMMulti cfg' x y) { svmmClassNames = classes })

-- Phase 75.9: 古典 MLP 分類 (seed 純粋 'fitMLPClassifierPure') 高レベル spec。
data MLPClsSpec = MLPClsSpec !MLPConfig !Word32 ![Text] !Text
-- | [日本語]: @mlpCls cfg seed featCols clsCol@ — 古典 MLP 分類 ('MLPFit')。 決定境界/混同行列対応。
--   [English]: @mlpCls cfg seed featCols clsCol@ — classic MLP
--   classification ('MLPFit'). Supports decision boundary \/ confusion
--   matrix.
mlpCls :: MLPConfig -> Word32 -> [Text] -> Text -> MLPClsSpec
mlpCls = MLPClsSpec
instance Fit MLPClsSpec where
  type Fitted MLPClsSpec = MLPFit
  fitEither (MLPClsSpec cfg seed feats clsCol) d = do
    x            <- reqColsM feats d
    (y, classes) <- reqLabelWithLevels clsCol d
    pure ((fitMLPClassifierPure cfg x y seed) { mlpClassNames = classes })

-- Phase 75.9: 古典 MLP 回帰 (seed 純粋 'fitMLPRegressorPure') 高レベル spec。
data MLPRegSpec = MLPRegSpec !MLPConfig !Word32 ![Text] !Text
-- | [日本語]: @mlpReg cfg seed featCols yCol@ — 古典 MLP 回帰 ('MLPFit'・応答は実数列)。
--   [English]: @mlpReg cfg seed featCols yCol@ — classic MLP regression
--   ('MLPFit'; the response is a real-valued column).
mlpReg :: MLPConfig -> Word32 -> [Text] -> Text -> MLPRegSpec
mlpReg = MLPRegSpec
instance Fit MLPRegSpec where
  type Fitted MLPRegSpec = MLPFit
  fitEither (MLPRegSpec cfg seed feats yc) d =
    (\x y -> fitMLPRegressorPure cfg x y seed) <$> reqColsM feats d <*> reqColV yc d


-- --- 群別フィット spec (群別フィット) — Phase 52.A4 ------------------------
--
-- 群カテゴリ列で行を分割し、 各群に同じ spec を当てはめて N 個のモデルを得る。
-- ★HBM 整合 = fit 結果 ('GroupedFit') が各群の 'Fitted spec' を保持し、
-- 'groupModels' で取り出せる (各群 'LMModel' 等に既存診断 = 'Hanalyze.Model.LM.Diagnostics'
-- がそのまま適用でき、 群間で傾きが本当に違うかを診断できる)。 描画は @Plottable@
-- で N 曲線を群色 + 凡例 (@ColorByCol@ + 'scaleColorManual') で重畳する (A3 機構の一般化)。
-- 群列は factor (文字) でも数値でも可 (factor は 'toFrame'+'getTextVec'、 数値は
-- 'lookupCol' を show でラベル化)。 主経路 = 二変量近道、 formula spec も副で許す。

-- | [日本語]: spec を「群別フィット」 でラップする。 @grouped "g" (lm "x" "y")@。
--   既存 spec ('lm'\/'glm'\/'spline'\/@robust@\/@quantile@、 formula も可) は不変。
--   [English]: Wraps a spec as a "per-group fit". @grouped \"g\" (lm \"x\" \"y\")@.
--   Existing specs ('lm'\/'glm'\/'spline'\/@robust@\/@quantile@; formula specs
--   too) are unchanged.
data GroupedSpec spec = GroupedSpec !Text spec

-- | [日本語]: @grouped "g" spec@ — 群列 @g@ で行を分け別々に @spec@ を当てはめる spec ラッパ。
--   [English]: @grouped \"g\" spec@ — a spec wrapper that splits rows by
--   group column @g@ and fits @spec@ to each separately.
grouped :: Text -> spec -> GroupedSpec spec
grouped = GroupedSpec

-- | [日本語]: 各群の (ラベル, fit 済みモデル) を取り出す (hbmDraws\/forestOf 流アクセサ)。
--   各群モデルに既存の診断 (例 LM なら 'Hanalyze.Model.LM.Diagnostics') がそのまま使える。
--   [English]: Extracts each group's (label, fitted model) pair (an
--   accessor in the style of hbmDraws\/forestOf). Existing diagnostics
--   (e.g. 'Hanalyze.Model.LM.Diagnostics' for LM) apply directly to
--   each group's model.
groupModels :: GroupedFit spec -> [(Text, Fitted spec)]
groupModels = gfGroups

-- | [日本語]: 群ラベルの一覧 (出現順)。
--   [English]: The list of group labels (in order of appearance).
groupLabels :: GroupedFit spec -> [Text]
groupLabels = map fst . gfGroups

instance Fit spec => Fit (GroupedSpec spec) where
  type Fitted (GroupedSpec spec) = GroupedFit spec
  fitEither (GroupedSpec gcol sp) d = do
    keys <- groupKeysCS gcol d
    let cols     = numericCols d                       -- 数値列 (x/y 含む) 全部
        ordered  = nub keys                            -- 群ラベル (出現順)
        rowsOf k = [ i | (i, k') <- zip [0 :: Int ..] keys, k' == k ]
        subOf ix = [ (n, [ vs !! i | i <- ix ]) | (n, vs) <- cols ]  -- 行抽出した sub 源
    fits <- mapM (\k -> (,) k <$> fitEither sp (subOf (rowsOf k))) ordered
    pure (GroupedFit fits)

-- | [日本語]: 群列を行ごとのラベル ('Text') 列に正規化する。 factor (文字) 列は
--   'toFrame'+'getTextVec'、 数値列は 'lookupCol' を show でラベル化。
--   [English]: Normalizes the group column into a per-row label ('Text')
--   column. Factor (text) columns go through 'toFrame'+'getTextVec';
--   numeric columns are labeled by 'show'-ing the result of 'lookupCol'.
groupKeysCS :: ColumnSource d => Text -> d -> Either String [Text]
groupKeysCS gcol d =
  case getTextVec gcol (toFrame d) of
    Just ts -> Right (V.toList ts)
    Nothing -> case lookupCol gcol d of
      Just vs -> Right (map showGroupLabel vs)
      Nothing -> Left ("grouped: 群列が見つかりません: " <> T.unpack gcol)

-- | [日本語]: クラス列を (整数ラベル列 0..K-1, levels 名) に正規化する。 factor(text) 列は
--   'toFrame'+'getTextVec' で __辞書順 unique__ を levels とし、 数値列は round して
--   __昇順 unique__ を levels とする (それぞれ show)。 いずれも各行を levels 中の
--   index (0..K-1) に符号化するので、 @levels !! label@ でクラス名が引ける。
--   ('reqLabelI' の levels 付き版・@treePlot@ 等でクラス名を出すのに使う。)
--   [English]: Normalizes a class column into (integer label column
--   0..K-1, level names). For factor(text) columns, levels are the
--   __lexicographically unique values__ via 'toFrame'+'getTextVec'; for
--   numeric columns, values are rounded and levels are the
--   __ascending unique values__ (each shown as text). Either way, each row
--   is encoded as its index (0..K-1) within levels, so @levels !! label@
--   retrieves the class name. (A levels-carrying version of 'reqLabelI',
--   used to display class names in @treePlot@ etc.)
reqLabelWithLevels :: ColumnSource d => Text -> d -> Either String (VU.Vector Int, [Text])
reqLabelWithLevels n d =
  case getTextVec n (toFrame d) of
    Just ts ->
      let labels = V.toList ts
          levels = sort (nub labels)                     -- factor は辞書順
          ix x   = fromMaybe 0 (elemIndex x levels)
      in Right (VU.fromList (map ix labels), levels)
    Nothing -> case lookupCol n d of
      Just vs ->
        let ints   = map round vs :: [Int]
            levels = sort (nub ints)                     -- 数値は昇順
            ix x   = fromMaybe 0 (elemIndex x levels)
        in Right (VU.fromList (map ix ints), map (T.pack . show) levels)
      Nothing -> Left ("label 列が見つかりません: " <> T.unpack n)

-- | [日本語]: 数値群ラベルの表示 (整数値は小数点を出さない)。
--   [English]: Displays a numeric group label (no decimal point for
--   integer values).
showGroupLabel :: Double -> Text
showGroupLabel v
  | fromIntegral (round v :: Integer) == v = T.pack (show (round v :: Integer))
  | otherwise                              = T.pack (show v)

-- --- formula 多変量 spec (R 流) — Phase 51.3 -------------------------------
--
-- 二変量近道と違い、 formula 文字列で多変量を表す (R の @lm(y ~ x1 + x2, df)@)。
-- 既存 'multiLMModel' / 'multiGLMModel' / 'fitMixedLME' を配線するだけ。
-- データ源は 'toFrame' で @DX.DataFrame@ に変換し **Phase 47 経路**
-- (MissingPolicy / contrast / 応答列判定) を通す (再実装しない)。 @DX.DataFrame@
-- 源は 'toFrame'=id ゆえ factor/NA を温存。

-- | [日本語]: formula LM spec。 @lmF "y ~ x1 + x2"@。
--   [English]: Formula LM spec. @lmF \"y ~ x1 + x2\"@.
data LMFormulaSpec   = LMFormulaSpec   !Text
-- | [日本語]: formula GLM spec。 @glmF fam link "y ~ x1 + x2"@。
--   [English]: Formula GLM spec. @glmF fam link \"y ~ x1 + x2\"@.
data GLMFormulaSpec  = GLMFormulaSpec  !Family !LinkFn !Text
-- | [日本語]: formula 混合モデル spec (random effects)。 @glmmF "y ~ x + (1|g)"@。
--   [English]: Formula mixed-model spec (random effects).
--   @glmmF \"y ~ x + (1|g)\"@.
data GLMMFormulaSpec = GLMMFormulaSpec !Text

-- | [日本語]: @lmF "y ~ x1 + x2"@ — formula 多変量 LM ('MultiLMModel') の spec。
--   [English]: @lmF \"y ~ x1 + x2\"@ — a spec for a formula-based
--   multivariate LM ('MultiLMModel').
lmF :: Text -> LMFormulaSpec
lmF = LMFormulaSpec

-- | [日本語]: @glmF fam link "y ~ x1 + x2"@ — formula 多変量 GLM ('MultiGLMModel') の spec。
--   [English]: @glmF fam link \"y ~ x1 + x2\"@ — a spec for a formula-based
--   multivariate GLM ('MultiGLMModel').
glmF :: Family -> LinkFn -> Text -> GLMFormulaSpec
glmF = GLMFormulaSpec

-- | [日本語]: @glmmF "y ~ x + (1|g)"@ — 線形混合モデル ('fitMixedLME') の spec。
--   返り型は @('GLMMResultRE', [Text])@ (固定効果係数名つき)。
--   [English]: @glmmF \"y ~ x + (1|g)\"@ — a spec for a linear mixed model
--   ('fitMixedLME'). The return type is @('GLMMResultRE', [Text])@ (with
--   fixed-effect coefficient names).
glmmF :: Text -> GLMMFormulaSpec
glmmF = GLMMFormulaSpec

-- | [日本語]: DOE 設計 (`Design`) の解析 spec。 設計が含意する formula
--   (要因計画=全交互作用 / RSM=2 次) で __LM を当てる__ (`MultiLMModel`)。 同じ @plan@ を
--   sim データ・実物データに使い回せる (formula は plan 由来・データは任意)。
--   @filledDf |-> designModel plan "y"@。
--   [English]: An analysis spec for a DOE design (`Design`). __Fits an LM__
--   (`MultiLMModel`) using the formula implied by the design (full
--   interactions for factorial designs, second order for RSM). The same
--   @plan@ can be reused across simulated and real data (the formula comes
--   from the plan; the data is arbitrary).
--   @filledDf |-> designModel plan \"y\"@.
data DesignModelSpec = DesignModelSpec !Design !Text

designModel :: Design -> Text -> DesignModelSpec
designModel = DesignModelSpec

instance Fit DesignModelSpec where
  type Fitted DesignModelSpec = MultiLMModel
  fitEither (DesignModelSpec plan y) d = multiLMModel (designFormula plan y) (toFrame d)

-- | [日本語]: DOE 設計 (`Design`) の __GP/RFF 解析__ spec。@designModel@ (LM) の
--   __非 LM 版__で、 plan の因子名で `gpMulti` を当てる (formula 不要 = kernel が非線形を
--   吸収するので 2 次項展開が要らない)。 同じ @plan@ を sim/実物データに使い回せる
--   (@designModel@ と対称)。 結果 (`GPRegModelN`) は @MultiVarModel@ ゆえ profiler/contour に
--   __GP 事後予測帯__を出せる。 ★__連続因子専用__ — 非連続因子 (`Num`/`Cat`) 混在は error
--   (GP kernel はカテゴリ距離を扱えない・`centralCompositeDesign` と同方針)。
--   @filledDf |-> designModelGP defaultGP plan "y"@。
--   [English]: A __GP\/RFF analysis__ spec for a DOE design (`Design`). The
--   __non-LM version__ of @designModel@, fitting `gpMulti` on the plan's
--   factor names (no formula needed — the kernel absorbs nonlinearity, so
--   no second-order term expansion is required). The same @plan@ can be
--   reused across sim\/real data (symmetric with @designModel@). Since the
--   result (`GPRegModelN`) is a @MultiVarModel@, the profiler\/contour can
--   show a __GP posterior predictive band__. Note: __continuous factors only__
--   — mixing in non-continuous factors (`Num`\/`Cat`) is an error
--   (the GP kernel can't handle categorical distances; the same policy as
--   `centralCompositeDesign`).
--   @filledDf |-> designModelGP defaultGP plan \"y\"@.
data DesignModelGPSpec = DesignModelGPSpec !GPConfig !Design !Text

designModelGP :: GPConfig -> Design -> Text -> DesignModelGPSpec
designModelGP = DesignModelGPSpec

instance Fit DesignModelGPSpec where
  type Fitted DesignModelGPSpec = GPRegModelN
  fitEither (DesignModelGPSpec cfg plan y) d =
    case [ dfName f | f <- dsFactors plan, notCont (dfKind f) ] of
      []  -> fitEither (GPMultiSpec cfg (map dfName (dsFactors plan)) y) d
      bad -> Left ("designModelGP: 連続因子専用 (GP kernel は非連続因子を扱えない)。"
                   <> " 非連続因子: " <> show bad)
    where notCont (Cont _ _ _) = False
          notCont _            = True

-- | [日本語]: 型付きランダム効果項。lme4 の @(1|g)@ / @(1+s|g)@ を型で表す。
--   文字列 formula を経由せず 'designModelHBM' に渡す。
--   [English]: Typed random-effect terms. Represents lme4's @(1|g)@ \/
--   @(1+s|g)@ in the type system. Passed to 'designModelHBM' without going
--   through a string formula.
ranIntercept :: Text -> RandomSpec
ranIntercept g = RandomSpec True [] g

ranSlope :: [Text] -> Text -> RandomSpec
ranSlope slopes g = RandomSpec True slopes g

-- | [日本語]: 前処理済みランダム効果。
--   @(群 idx (post-drop 行ごと), 群数, 傾き共変量列)@。 傾き列が空 = 切片のみ
--   (@(1|g)@)、 非空 = 相関ランダム傾き (@(1+s|g)@) で各列が観測ごとの共変量値
--   (長さ n・designX/ys と同じ post-drop 行整合)。
--   [English]: A preprocessed random effect.
--   @(group idx (per post-drop row), group count, slope covariate
--   columns)@. Empty slope columns = intercept only (@(1|g)@); non-empty =
--   correlated random slopes (@(1+s|g)@), where each column is the
--   per-observation covariate value (length n, aligned with the post-drop
--   rows of designX\/ys).
type PreparedRE = ([Int], Int, [[Double]])

-- | [日本語]: DOE 階層モデルの手書き 'ModelP' (核心・相関傾きを高速化)。
--   固定効果 = designX·β (β に弱情報 prior)、 観測ノイズ = σ。
--   ★HBM に formula 文字列を食わせる経路は無い (Fit.hs の方針) ため手書きで組む。
--
--   ランダム効果は 2 経路:
--   * __切片のみ__ (全 RE の傾き列が空): 'reNormal' + 'observeLMR' の解析勾配 REff 経路
--     (観測は affine = compiled 高速経路)。
--   * __相関ランダム傾き__ (いずれかの RE に傾き列): lme4 @(1+s|g)@。 __非中心化__ 化
--     済み。 群成分 raw latent @z_g^c ~ N(0,1)@、 スケール @τ_c ~ HalfNormal@、
--     相関 @Lcorr = LKJ Cholesky@ から @b_g^c = τ_c·Σ_{j≤c} Lcorr[c][j]·z_g^j@ を組み、
--     観測 μ = β·X + Σ_c b_g^c·x_c を per-obs scalar 'observe' に載せる。 μ は τ·z / L·z の
--     latent×latent 積を含む非 affine ゆえ (b) 閉形式には載らないが、 @synthVecIR@ が
--     (a) source-to-source AD (vecIR) に載せる (per-eval 5×・funnel 消滅を実測)。
--     centered の @potential@ 相関 prior + funnel (160×) は撤去済み。
--   [English]: A hand-written 'ModelP' for the DOE hierarchical model (the
--   core; correlated slopes are sped up). Fixed effects = designX·β (with a
--   weakly informative prior on β), observation noise = σ.
--   Note: there's no path for feeding a formula string into HBM (a Fit.hs
--   policy decision), so this is hand-assembled.
--
--   Random effects take 2 paths:
--   * __Intercept only__ (slope columns empty for all REs): the analytic
--     gradient REff path via 'reNormal' + 'observeLMR' (observations are
--     affine — the compiled fast path).
--   * __Correlated random slopes__ (any RE has slope columns): lme4's
--     @(1+s|g)@. __Non-centered__. Builds
--     @b_g^c = τ_c·Σ_{j≤c} Lcorr[c][j]·z_g^j@ from the group component's raw
--     latent @z_g^c ~ N(0,1)@, scale @τ_c ~ HalfNormal@, and correlation
--     @Lcorr = LKJ Cholesky@, feeding the observation
--     μ = β·X + Σ_c b_g^c·x_c into a per-obs scalar 'observe'. Since μ
--     includes a latent×latent product of τ·z \/ L·z, it's non-affine and
--     doesn't fit the (b) closed-form path, but @synthVecIR@ puts it on the
--     (a) source-to-source AD (vecIR) path (measured 5× per-eval with the
--     funnel eliminated). The centered @potential@ correlation prior +
--     funnel (160×) has been removed.
designHBMProgram :: [[Double]] -> [Text] -> [PreparedRE] -> [Double] -> ModelP ()
designHBMProgram designX betaNames res ys
  | all (\(_, _, sc) -> null sc) res = do
      -- === 切片のみ: 解析勾配 REff 高速経路 ===
      mapM_ (\nm -> sample nm (Normal 0 10)) betaNames
      _sigma <- sample "sigma" (HalfNormal 5)
      reffs  <- mapM mkInterceptRE (zip [0 ..] res)
      observeLMR "y" betaNames designX reffs (LMGaussian "sigma") ys
  | otherwise = do
      -- === 相関ランダム傾き (非中心化): μ に τ·L·z (latent×latent) → (a) vecIR ===
      -- Phase 80.2b: centered (potential 相関 prior + observeLMR + funnel 160×) を撤去し、
      -- 非中心化 b_g^c = τ_c·Σ_{j≤c} Lcorr[c][j]·z_g^j を per-obs scalar 'observe' に載せる。
      -- μ が latent×latent 非 affine ゆえ (b) 閉形式には載らないが、 @synthVecIR@ が
      -- (a) source-to-source AD に載せる (Phase 80.2a spike で per-eval 5×・funnel 消滅を実測)。
      betas <- mapM (\nm -> sample nm (Normal 0 10)) betaNames
      sigma <- sample "sigma" (HalfNormal 5)
      -- 各 RE 群の per-obs 寄与関数 (i → Σ_c b_{g(i)}^c · w_c(i))。
      contribs <- mapM (uncurry correlatedRE) (zip [0 ..] res)
      let nObs   = length ys
          muAt i = sum (zipWith (\b x -> b * realToFrac x) betas (designX !! i))
                   + sum [ c i | c <- contribs ]
      mapM_ (\i -> observe ("y_" <> T.pack (show i)) (Normal (muAt i) sigma) [ys !! i])
            [0 .. nObs - 1]
  where
    -- 切片のみ RE (解析勾配): reNormal + at。
    mkInterceptRE (gi :: Int, (idxRow, nG, _)) = do
      let base = "u_g" <> T.pack (show gi)
      tau <- sample ("tau_" <> base) (HalfNormal 5)
      u   <- reNormal base nG ("tau_" <> base) tau
      pure (u `at` idxRow)

    -- 相関 RE (非中心化): 群成分 raw latent z_g^c ~ N(0,1) (成分ごと 1D plate = family)、
    -- スケール τ_c ~ HalfNormal、 相関 Cholesky Lcorr = LKJ。 群効果は
    -- b_g^c = τ_c·Σ_{j≤c} Lcorr[c][j]·z_g^j = (diag(τ)·Lcorr·z)_c を deterministic で記録。
    -- 返り値 = per-obs 寄与関数 @i → Σ_c b_{g(i)}^c · w_c(i)@ (w_0=1 / w_c=slope列)。
    correlatedRE (gi :: Int) (idxRow, nG, slopeCols) = do
      let k       = 1 + length slopeCols
          tag     = "g" <> T.pack (show gi)
          znm c g = "z_" <> tag <> "_" <> T.pack (show c) <> "_" <> T.pack (show g)
          bnm c g = "b_" <> tag <> "_" <> T.pack (show c) <> "_" <> T.pack (show g)
      -- 成分 c ごとに群 raw latent の 1D plate: zByComp !! c !! g = z_g^c (family 単位)。
      zByComp <- mapM (\c -> plateI ("z_" <> tag <> "_" <> T.pack (show c)) nG
                               (\g -> sample (znm c g) (Normal 0 1)))
                      [0 .. k - 1]
      taus  <- mapM (\c -> sample ("tau_" <> tag <> "_" <> T.pack (show c)) (HalfNormal 5))
                    [0 .. k - 1]
      lcorr <- lkjCorrCholesky ("Lcorr_" <> tag) k 2.0
      -- b_g^c = τ_c·Σ_{j≤c} Lcorr[c][j]·z_g^j を deterministic で記録 (chain 出力用) + 使用。
      bByComp <- mapM (\c ->
                    mapM (\g ->
                      deterministic (bnm c g)
                        (taus !! c * sum [ (lcorr !! c !! j) * ((zByComp !! j) !! g)
                                         | j <- [0 .. c] ]))
                      [0 .. nG - 1])
                  [0 .. k - 1]
      -- per-obs 寄与: 重み c=0→1 / c>=1→slope 列 (post-drop 行整合)。
      let wOf c i | c == 0    = 1
                  | otherwise = realToFrac ((slopeCols !! (c - 1)) !! i)
      pure $ \i -> let g = idxRow !! i
                   in sum [ ((bByComp !! c) !! g) * wOf c i | c <- [0 .. k - 1] ]

-- | [日本語]: DOE 設計の __階層ベイズ (mixed-effects)__ fit spec。 固定効果 =
--   'designFormula' (factorial=交互作用 / RSM=2次) を LM 経路 (`modelFrame`/`designMatrixF`)
--   で設計行列化し、 ランダム効果 = 'RandomSpec' の群 (v1 = random intercept のみ) を
--   'designHBMProgram' で手書き 'ModelP' に組み、 'hbm' (NUTS) で学習する。 事後 draw を
--   'DesignHBMFit' に格納する。 同じ @plan@ を sim/実物データに使い回せる
--   (@designModel@ / 'designModelGP' と対称)。
--   @filledDf |-> designModelHBM defaultHBM plan [ranIntercept "lot"] "y"@。
--   [English]: A __hierarchical Bayesian (mixed-effects)__ fit spec for a
--   DOE design. Fixed effects: builds the design matrix via
--   'designFormula' (factorial=interactions \/ RSM=second order) through
--   the LM path (`modelFrame`\/`designMatrixF`). Random effects: assembles
--   the groups from 'RandomSpec' (v1 = random intercept only) into a
--   hand-written 'ModelP' via 'designHBMProgram', trained with 'hbm'
--   (NUTS). Stores the posterior draws in 'DesignHBMFit'. The same @plan@
--   can be reused across sim\/real data (symmetric with @designModel@ \/
--   'designModelGP').
--   @filledDf |-> designModelHBM defaultHBM plan [ranIntercept \"lot\"] \"y\"@.
data DesignHBMFit = DesignHBMFit
  { dhfFormula    :: !Formula      -- ^ [日本語]: 固定効果 formula ('designFormula' plan y の parse 結果)。 [English]: Fixed-effect formula (the parse result of 'designFormula' plan y).
  , dhfBetaNames  :: ![Text]       -- ^ [日本語]: 固定効果係数名 (設計列順)。 [English]: Fixed-effect coefficient names (in design-column order).
  , dhfBetaDraws  :: ![[Double]]   -- ^ [日本語]: draws × p ('dhfBetaNames' 列順)。 [English]: draws × p (in 'dhfBetaNames' column order).
  , dhfSigmaDraws :: ![Double]     -- ^ [日本語]: 観測ノイズ σ の事後 draw。 [English]: Posterior draws of the observation noise σ.
  , dhfFrame      :: !ModelFrame   -- ^ [日本語]: 訓練 frame (mvFrame 用)。 [English]: Training frame (for mvFrame).
  , dhfModel      :: !HBMModel     -- ^ [日本語]: 学習済 HBM 本体。 診断抽出子 (@dagOf@ / @tracesOf@ /
                                   --   @ppcOf@ / @energyOf@ 等) に @dhfModel fit@ で渡せる。
                                   --   [English]: The trained HBM body. Can be passed to diagnostic
                                   --   extractors (@dagOf@ \/ @tracesOf@ \/ @ppcOf@ \/ @energyOf@ etc.)
                                   --   via @dhfModel fit@.
  }

data DesignModelHBMSpec = DesignModelHBMSpec !HBMConfig !Design ![RandomSpec] !Text

designModelHBM :: HBMConfig -> Design -> [RandomSpec] -> Text -> DesignModelHBMSpec
designModelHBM = DesignModelHBMSpec

instance Fit DesignModelHBMSpec where
  type Fitted DesignModelHBMSpec = DesignHBMFit
  fitEither (DesignModelHBMSpec cfg plan res y) d = do
    fml               <- parseModel (designFormula plan y)
    let df       = toFrame d
        -- 'modelFrame' (DropRows) は formula 関与列 (resp:dvars) に NA を含む行を listwise
        -- 削除する。 群 idx (prepRE) は raw df からでなく、 designX/ys と同じ post-drop 行
        -- 集合 (dfClean) から作る必要がある (でないと NA 行がある場合に群 idx が行ずれし、
        -- observeLMR へ渡す群インデックスが誤対応する)。 dropMissingRows は modelFrame 内部
        -- (modelFrameWith) と同一関数・同一 involved 列なので、 落とす行集合・順序が一致する。
        involved = formResponse fml : formDataVars fml
        dfClean  = dropMissingRows involved df
    mf                <- modelFrame fml df
    (xMat, betaNames) <- designMatrixF fml mf
    yv                <- responseVec mf
    let designX = map LA.toList (LA.toRows xMat)
        ys      = V.toList yv
    resPrepared <- mapM (prepRE dfClean) res
    let prog :: ModelP ()
        prog      = designHBMProgram designX betaNames resPrepared ys
        model     = d |-> hbm cfg prog :: HBMModel
        betaDraws = [ concatMap (chainVals nm) (hbmChainsR model) | nm <- betaNames ]
    pure DesignHBMFit
      { dhfFormula    = fml
      , dhfBetaNames  = betaNames
      , dhfBetaDraws  = transpose betaDraws
      , dhfSigmaDraws = concatMap (chainVals "sigma") (hbmChainsR model)
      , dhfFrame      = mf
      , dhfModel      = model
      }
    where
      prepRE frame (RandomSpec _ slopes g) = case getTextVec g frame of
        Nothing -> Left ("designModelHBM: grouping 列 '" <> T.unpack g <> "' が見つかりません")
        Just gv -> do
          let (labels, idx, _) = buildGroups gv
          -- 傾き共変量列 (post-drop 行整合 = 群列と同じ dfClean 由来)。 空なら切片のみ。
          slopeCols <- mapM (getSlopeCol frame) slopes
          Right (V.toList idx, V.length labels, slopeCols)
      getSlopeCol frame s = case lookupCol s frame of
        Just col -> Right col
        Nothing  -> Left ("designModelHBM: 傾き共変量列 '" <> T.unpack s
                          <> "' が見つかりません (数値列である必要があります)")

-- | [日本語]: 多出力 (複数応答) fit コンビネータ。 応答名のリストと __応答名から spec を作る関数__
--   を受け、 各応答を同じデータ源で当てはめて @[(応答名, Fitted spec)]@ を返す。
--   @designModel plan@ が既にカレー化 (@Text -> DesignModelSpec@) なので接着剤なしで slot に嵌る:
--
--   > let model = filledDf |-> multiOutput ["strength","yield"] (designModel plan)
--   >     -- model :: [(Text, MultiLMModel)]
--
--   designModel 専用でなく汎用 (@multiOutput ys (lmF . mkFormula)@ 等も可)。 結果は profiler
--   ('Hanalyze.Plot.ML.profiler') が「行=応答 × 列=因子」 のグリッドに描ける。
--   [English]: A multi-output (multiple-response) fit combinator. Takes a
--   list of response names and __a response-name-to-spec function__, fits
--   each response on the same data source, and
--   returns @[(response name, Fitted spec)]@. Since @designModel plan@ is
--   already curried (@Text -> DesignModelSpec@), it slots in with no glue
--   code:
--
--   > let model = filledDf |-> multiOutput ["strength","yield"] (designModel plan)
--   >     -- model :: [(Text, MultiLMModel)]
--
--   Not specific to designModel — generic (e.g.
--   @multiOutput ys (lmF . mkFormula)@ also works). The result lets the
--   profiler ('Hanalyze.Plot.ML.profiler') draw a "rows = responses
--   × columns = factors" grid.
data MultiOutputSpec spec = MultiOutputSpec ![Text] (Text -> spec)

-- | [日本語]: @multiOutput responseNames mkSpec@ — 各応答名に @mkSpec@ を適用して当てはめる。
--   [English]: @multiOutput responseNames mkSpec@ — fits by applying
--   @mkSpec@ to each response name.
multiOutput :: [Text] -> (Text -> spec) -> MultiOutputSpec spec
multiOutput = MultiOutputSpec

-- | [日本語]: @multiOutput@ の結果 @[(応答名, model)]@ から__応答名でモデルを 1 つ取り出す__。 単一応答の
--   可視化 (@contourOf@ / @surfaceOf@) に multiOutput の結果を渡すとき @snd (head model)@ の代わりに使う。
--
--   > let models = filledDf |-> multiOutput ["strength","yield"] (designModel plan)
--   > noDf |>> contourOf (modelFor "strength" models) "temp" "time"
--
--   応答名が無ければ利用可能な名前を添えて error (対話で気付ける)。
--   [English]: __Extracts one model by response name__ from a
--   @multiOutput@ result @[(response name, model)]@. Used in place of
--   @snd (head model)@ when passing a multiOutput result to a single-response
--   visualization (@contourOf@ \/ @surfaceOf@).
--
--   > let models = filledDf |-> multiOutput ["strength","yield"] (designModel plan)
--   > noDf |>> contourOf (modelFor "strength" models) "temp" "time"
--
--   Raises an error listing the available names if the response name isn't
--   found (so it's noticed interactively).
modelFor :: Text -> [(Text, m)] -> m
modelFor r models = case lookup r models of
  Just m  -> m
  Nothing -> error
    ("modelFor: 応答 '" <> T.unpack r <> "' が見つかりません。 利用可能: "
      <> show (map (T.unpack . fst) models))

instance Fit spec => Fit (MultiOutputSpec spec) where
  type Fitted (MultiOutputSpec spec) = [(Text, Fitted spec)]
  fitEither (MultiOutputSpec ys mk) d =
    traverse (\y -> (,) y <$> fitEither (mk y) d) ys

instance Fit LMFormulaSpec where
  type Fitted LMFormulaSpec = MultiLMModel
  fitEither (LMFormulaSpec fml) d = multiLMModel fml (toFrame d)

instance Fit GLMFormulaSpec where
  type Fitted GLMFormulaSpec = MultiGLMModel
  fitEither (GLMFormulaSpec fam lnk fml) d = multiGLMModel fam lnk fml (toFrame d)

instance Fit GLMMFormulaSpec where
  type Fitted GLMMFormulaSpec = (GLMMResultRE, [Text])
  fitEither (GLMMFormulaSpec fml) d = fitMixedLME fml (toFrame d)

-- --- 重回帰 spec (列名リスト・formula 不要) — Phase 70.D --------------------
--
-- 重回帰 = 説明変数の**列名リスト**で多変量回帰を当てる (formula 文字列を書かない)。
-- 内部は 'additiveFormula' で設計行列 @[1, x1,…,xp]@ を直接合成し、 既存の
-- 'multiLMModelF' / 'multiGLMModelF' / 'multiRobustModelF' を配線する。 返り値は
-- effect plot (@statModelMulti@ / along / holdAt / byVar) と係数サマリ
-- (@coefSummary@) の両方が即使える。 'lmF' / 'glmF' は formula 糖衣として併存。

-- | [日本語]: 重回帰 (多変量 LM) spec。 @lmMulti [\"x1\",\"x2\",\"x3\"] \"y\"@。
--   [English]: Multiple-regression (multivariate LM) spec.
--   @lmMulti [\"x1\",\"x2\",\"x3\"] \"y\"@.
data LMMultiSpec     = LMMultiSpec     ![Text] !Text
-- | [日本語]: 重回帰 (多変量 GLM) spec。 @glmMulti fam link [\"x1\",\"x2\"] \"y\"@。
--   [English]: Multiple-regression (multivariate GLM) spec.
--   @glmMulti fam link [\"x1\",\"x2\"] \"y\"@.
data GLMMultiSpec    = GLMMultiSpec    !Family !LinkFn ![Text] !Text
-- | [日本語]: 重回帰 (多変量ロバスト) spec。 @robustMulti est [\"x1\",\"x2\"] \"y\"@。
--   [English]: Multiple-regression (multivariate robust) spec.
--   @robustMulti est [\"x1\",\"x2\"] \"y\"@.
data RobustMultiSpec = RobustMultiSpec !RobustEstimator ![Text] !Text

-- | [日本語]: @lmMulti predCols yCol@ — 列名リストで多変量線形回帰 ('MultiLMModel')。
--   @df |-> lmMulti [\"age\",\"bmi\",\"bp\"] \"y\"@。
--   [English]: @lmMulti predCols yCol@ — multivariate linear regression
--   ('MultiLMModel') from a column-name list.
--   @df |-> lmMulti [\"age\",\"bmi\",\"bp\"] \"y\"@.
lmMulti :: [Text] -> Text -> LMMultiSpec
lmMulti = LMMultiSpec

-- | [日本語]: @glmMulti fam link predCols yCol@ — 列名リストで多変量 GLM ('MultiGLMModel')。
--   [English]: @glmMulti fam link predCols yCol@ — multivariate GLM
--   ('MultiGLMModel') from a column-name list.
glmMulti :: Family -> LinkFn -> [Text] -> Text -> GLMMultiSpec
glmMulti = GLMMultiSpec

-- | [日本語]: @rlmMulti est predCols yCol@ — 列名リストで多変量ロバスト回帰 ('MultiRobustModel')。
--   [English]: @rlmMulti est predCols yCol@ — multivariate robust
--   regression ('MultiRobustModel') from a column-name list.
rlmMulti :: RobustEstimator -> [Text] -> Text -> RobustMultiSpec
rlmMulti = RobustMultiSpec

instance Fit LMMultiSpec where
  type Fitted LMMultiSpec = MultiLMModel
  fitEither (LMMultiSpec xs y) d = multiLMModelF (additiveFormula y xs) (toFrame d)
  predictorCols (LMMultiSpec xs _) = xs
  responseCol   (LMMultiSpec _ y)  = Just y

instance Fit GLMMultiSpec where
  type Fitted GLMMultiSpec = MultiGLMModel
  fitEither (GLMMultiSpec fam lnk xs y) d =
    multiGLMModelF fam lnk (additiveFormula y xs) (toFrame d)
  predictorCols (GLMMultiSpec _ _ xs _) = xs
  -- responseCol = Nothing (既定)。 GLM 応答は family/link 拘束ゆえ標準化不可。

instance Fit RobustMultiSpec where
  type Fitted RobustMultiSpec = MultiRobustModel
  fitEither (RobustMultiSpec est xs y) d =
    multiRobustModelF est (additiveFormula y xs) (toFrame d)
  predictorCols (RobustMultiSpec _ xs _) = xs
  responseCol   (RobustMultiSpec _ _ y)  = Just y

-- | [日本語]: 多変量 (重回帰) 分位点回帰 spec。 @quantileMulti [0.1,0.5,0.9] ["x1","x2"] "y"@。
--   単変量 @quantile@ の多予測子版 (各 τ を設計行列 @[1,x₁..xₚ]@ に当てる・statsmodels
--   @QuantReg@ 多予測子と同型)。 予測子は数値列を 'reqColsM' で直接行列化する。
--   [English]: Multivariate (multiple-regression) quantile regression spec.
--   @quantileMulti [0.1,0.5,0.9] [\"x1\",\"x2\"] \"y\"@. The multi-predictor
--   version of univariate @quantile@ (fits each τ to the design matrix
--   @[1,x₁..xₚ]@ — the same shape as statsmodels's multi-predictor
--   @QuantReg@). Predictors are turned directly into a matrix via numeric
--   columns and 'reqColsM'.
data QuantileMultiSpec = QuantileMultiSpec ![Double] ![Text] !Text

-- | [日本語]: @rqMulti taus predCols yCol@ — 多変量分位点回帰 ('MultiQuantileModel')。
--   [English]: @rqMulti taus predCols yCol@ — multivariate quantile
--   regression ('MultiQuantileModel').
rqMulti :: [Double] -> [Text] -> Text -> QuantileMultiSpec
rqMulti = QuantileMultiSpec

instance Fit QuantileMultiSpec where
  type Fitted QuantileMultiSpec = MultiQuantileModel
  fitEither (QuantileMultiSpec taus xs yn) d = do
    xm <- reqColsM xs d                                   -- n × p (予測子)
    yv <- reqColV yn d
    let nR   = LA.rows xm
        dm   = LA.fromColumns (LA.konst 1 nR : LA.toColumns xm)  -- [1, x₁..xₚ]
        fits = [ (t, fitQuantile t dm yv) | t <- taus ]
    Right MultiQuantileModel { mqmTaus = taus, mqmNames = xs, mqmFits = fits, mqmX = dm }
  predictorCols (QuantileMultiSpec _ xs _) = xs
  responseCol   (QuantileMultiSpec _ _ yn) = Just yn

-- --- HBM spec + データ散布図 — Phase 51.4 ----------------------------------
--
-- HBM は formula を取らず手書き 'ModelP' を学習する (brms 風 formula→HBM は別 Phase)。
-- spec は既存 'hbmModelPure' (純粋・seed 決定的) を配線するだけ。 データ源の
-- 数値列をすべて取り出し列名 assoc にして渡す (HBM 側 @dataNamed@ 名と突合)。

-- | [日本語]: HBM spec。 @hbm cfg model@ (設定 + 手書き確率プログラム)。
--   [English]: HBM spec. @hbm cfg model@ (configuration + hand-written
--   probabilistic program).
data HBMSpec = HBMSpec HBMConfig (ModelP ())

-- | [日本語]: @hbm cfg model@ — HBM ('HBMModel') を学習する spec。 cfg の seed で決定的。
--   [English]: @hbm cfg model@ — a spec that trains HBM ('HBMModel').
--   Deterministic via cfg's seed.
hbm :: HBMConfig -> ModelP () -> HBMSpec
hbm = HBMSpec

-- | [日本語]: データ源の数値列をすべて列名 assoc に取り出す (HBM の入力形)。
--   [English]: Extracts all numeric columns of a data source into a
--   column-name assoc list (HBM's input shape).
numericCols :: ColumnSource d => d -> [(Text, [Double])]
numericCols d = [ (n, vs) | n <- columnNames d, Just vs <- [lookupCol n d] ]

instance Fit HBMSpec where
  type Fitted HBMSpec = HBMModel
  -- Phase 60.3: 空 placeholder slot の突合を loud error 化 (旧: 黙って空 [] の
  -- まま学習が走り gids=[] 等の不可解な失敗になっていた)。 DataIx slot は
  -- Int/Integer 列直結 + Text factor 列の sort 順自動コード化。
  fitEither (HBMSpec cfg model) d = do
    checkDataSlots model d
    (ixCols, levels) <- resolveIxSlots model d
    Right (hbmModelPureWith cfg model (numericCols d) ixCols levels)
  -- Phase 61.4: @(|->!)@ 経路 = 同じ列解決 + 進捗表示つき IO 学習。
  -- seed 規約は pure 経路と共有ゆえ結果はビット一致 (test 固定)。
  fitIO (HBMSpec cfg model) d =
    case (do checkDataSlots model d; resolveIxSlots model d) of
      Left err              -> ioError (userError err)
      Right (ixCols, levels) ->
        hbmModelIOWith cfg model (numericCols d) ixCols levels

-- | [日本語]: @dataNamed@ slot の突合検査: __空 placeholder__ (@dataNamed n []@)
-- なのに対応する数値列が無ければ loud error。 実値入り placeholder の列欠落は
-- default 続行 (データ直書きの正当パターンを壊さない)。
--   [English]: Cross-checks @dataNamed@ slots: a loud error if there's no
--   matching numeric column for __an empty placeholder__
--   (@dataNamed n []@). A missing column for a placeholder that already
--   has real values falls through to the default (so it doesn't break the
--   legitimate pattern of writing data directly inline).
checkDataSlots :: ColumnSource d => ModelP () -> d -> Either String ()
checkDataSlots model d =
  case [ n | (n, True) <- dataSlots model
           , Nothing <- [lookupCol n d] ] of
    [] -> Right ()
    ns -> Left ("HBM: 空 placeholder の dataNamed slot に対応する数値列が"
                <> "ありません: " <> T.unpack (T.intercalate ", " ns)
                <> " (利用可能列: "
                <> T.unpack (T.intercalate ", " (columnNames d))
                <> ")。 Integer/Text 数値文字列は許容、 factor 列は dataNamedIx で")

-- | [日本語]: @dataNamedIx@ slot を列から解決する。
--   * Text factor 列 → __sort 順__ (辞書順) levels に 0.. コード化
--     (R @factor()@ / pandas parity・行順 shuffle に不変)
--   * 数値列 (Int / Integer / 整数値の Double) → @round@ で [Int]
--   * 空 placeholder で列なし / 非整数値 → loud error ('Left')
--   * 実値入り placeholder で列なし → default 続行 (bind しない)
--   [English]: Resolves @dataNamedIx@ slots from columns.
--   * Text factor columns → coded 0.. against __sort-order__
--     (lexicographic) levels (R's @factor()@ \/ pandas parity; invariant to
--     row-order shuffling).
--   * Numeric columns (Int \/ Integer \/ integer-valued Double) → [Int] via
--     @round@.
--   * No column for an empty placeholder \/ non-integer values → a loud
--     error ('Left').
--   * No column for a placeholder that already has real values → falls
--     through to the default (not bound).
resolveIxSlots :: ColumnSource d => ModelP () -> d
               -> Either String ([(Text, [Int])], [(Text, [Text])])
resolveIxSlots model d = do
  rs <- mapM resolve (dataIxSlots model)
  pure ( [ (n, is) | (n, Just is, _) <- rs ]
       , [ (n, ls) | (n, _, Just ls) <- rs ] )
  where
    fr = toFrame d
    resolve (n, isEmpty) = case getTextVec n fr of
      Just tv ->
        let vals   = V.toList tv
            levels = sort (nub vals)
            code t = fromMaybe 0 (elemIndex t levels)   -- levels 由来ゆえ必ず命中
        in Right (n, Just (map code vals), Just levels)
      Nothing -> case lookupCol n d of
        Just vs
          | all (\v -> fromIntegral (round v :: Int) == v) vs ->
              Right (n, Just (map round vs), Nothing)
          | otherwise ->
              Left ("HBM: dataNamedIx slot " <> T.unpack n
                    <> " の列に非整数値があります (離散 index 専用)")
        Nothing
          | isEmpty ->
              Left ("HBM: 空 placeholder の dataNamedIx slot に対応する列が"
                    <> "ありません: " <> T.unpack n
                    <> " (利用可能列: "
                    <> T.unpack (T.intercalate ", " (columnNames d)) <> ")")
          | otherwise -> Right (n, Nothing, Nothing)

