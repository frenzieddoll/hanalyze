{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Hanalyze.Plot
-- Description : 解析モデルを hgg の VisualSpec へ変換する連携層 (flag plot-integration)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- hgg 連携層 (= 解析モデル → 図 'VisualSpec')。
--
-- ⚠ 本モジュールは cabal flag @plot-integration@ (既定 off) を on にしたときのみ
-- build される。 @hgg-core@ に依存するため **upstream hanalyze には
-- cherry-pick しない** (= 依存方向 analyze→plot-core を flag で隔離。 plot Phase 15
-- / analyze Phase 46 の設計)。 中立 protocol ('Hanalyze.Model.Core' の
-- 'ResidualModel' / 'PredictiveModel') は portable、 こちらは非 portable。
--
-- 系統 A (モデル・アウト型): フィット済みモデルを 'toPlot' で 'VisualSpec' 化し、
-- hgg の layer 文法に @df |>> (layer scatter <> toPlot fit)@ で重畳する
-- ('VisualSpec' は Monoid なので新コンビネータ不要)。
module Hanalyze.Plot
  ( Plottable (..)
    -- * ルート1 grid 評価 (滑らかな回帰曲線・CI 帯) — Phase 16 §3 C1
  , ModelSpec
  , SingleVarModel (..)
  , GridOpts (..)
  , statModel
  , grid
  , gridRange
  , BandMode (..)
  , bandMode
    -- 予測区間の算出法セレクタ (closed-form / bootstrap — Phase 70.H)
  , PIMethod (..)
  , piMethod
  , statColor
  , statFill
  , statLinetype
  , LineType (..)
  , statLinewidth
  , statAlpha
  , statLabel
  , statEquation
  , statR2
  , statLevel
  , predAt
    -- * 多変量 effect plot — Phase 16 §3 C3
  , MultiVarModel (..)
  , AlongSpec
  , along
  , statModelMulti
  , HoldAgg (..)
  , holdAt
  , byVar
  , MultiLMModel (..)
  , multiLMModel
  , multiLMModelF
  , MultiGLMModel (..)
  , multiGLMModel
  , multiGLMModelF
    -- 多変量ロバスト回帰 (formula 不要・列名リスト — Phase 70.D)
  , MultiRobustModel (..)
  , multiRobustModelF
  , additiveFormula
    -- PLS effect plot (frame 保持ラッパ + 出力セレクタ — Phase 70.B2/B3)
  , PLSModel (..)
  , plsModel
  , selectOutput
    -- * 応答曲面 3D 直結 — plot Phase 24 A3
  , SurfaceOpts (..)
  , defaultSurfaceOpts
  , surfaceGrid
  , surfaceOf
  , surfaceOfWith
  , dataScatter3DOf
  , epredSurfaceOf
  , epredSurfaceOfWith
    -- * モデル API 層 (描画と独立: predict / describe / coefficients)
  , ModelAPI (..)
  , Coef (..)
    -- * 統一係数サマリ (t/z・p 値・95% CI — Phase 70.D)
  , CoefRow (..)
  , HasCoefSummary (..)
  , HasCoefBoot (..)
  , coefSummaryBoot
    -- * 平滑項単位の近似有意性 (mgcv 流 edf + 近似 F — Phase 72.2)
  , TermRow (..)
  , HasTermSummary (..)
  , termSummary
    -- * 統一玄関 (.summary() 風 — Phase 72.3)
  , ModelReport (..)
  , HasReport (..)
  , modelReport
  , showReport
    -- * 回帰診断の可視化 (係数 forest / 実測vs予測 — Phase 72.4/72.5)
  , HasObsPred (..)
  , obsVsPred
  , obsPredSpec
  , coefForest
    -- * 線形モデル (描画可能 = X 同梱)
  , LMModel (..)
  , lmModel
    -- * 一般化線形モデル (描画可能 = X + family/link 同梱)
  , GLMModel (..)
  , glmModel
    -- * ガウス過程 (描画可能 = 予測 grid 同梱の 'GPResult' をそのまま)
  , GPResult (..)
    -- * カーネル法ファミリ統合 (GP / KRR / RFF・df |-> gp) — Phase 70.5 項目 E
  , Kernel (..)
  , GPParams (..)
  , defaultGPParams
  , GPMethod (..)
  , HyperStrategy (..)
  , GPConfig (..)
  , defaultGP
  , GPSpec
  , gp
  , GPRegModel (..)
  , GPMultiSpec
  , gpMulti
  , GPRegModelN (..)
    -- * 罰則付き回帰 統合 (Ridge/Lasso/EN/MCP/SCAD/Adaptive/Group・df |-> regularized) — Phase 70.7 項目 G
  , RegMethod (..)
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
  , RegModel (..)
  , regPredict
    -- * スプライン回帰 (描画可能 = X 同梱、 平滑曲線 + CI band)
  , SplineModel (..)
  , splineModel
    -- * 一般化加法モデル (描画可能 = X 同梱、 平滑曲線のみ・band 非提供)
  , GAMModel (..)
  , gamModel
    -- ** GAM 基底一般化 + GCV (Phase 70.6 F3・df|-> 高レベル)
  , GAMBasis (..)
  , GAMLambda (..)
  , GAMConfig (..)
  , defaultGAMConfig
  , GAMSpec (..)
  , gam
  , gamMulti
  , GAMModelN (..)
  , fitGAMWith
    -- * ロバスト回帰 (描画可能 = X 同梱、 ロバスト直線・重み diagnostic)
  , RobustModel (..)
  , robustModel
    -- * 多出力線形回帰 (描画可能 = 自己完結の 'MultiFit'、 残差相関 heatmap)
  , MultiFit (..)
    -- * 分位点回帰 (描画可能 = X 同梱、 複数分位線を色分け重畳)
  , QuantileModel (..)
  , quantileModel
    -- * MCMC チェーン (描画可能 = trace + 周辺事後密度、 ベイズ出入口)
  , ChainModel (..)
  , chainModel
    -- * 生存解析 (描画可能 = 自己完結、 KM 生存曲線 / 競合リスク CIF)
  , KMResult (..)
  , CRFit (..)
    -- * 時系列予測 (描画可能 = 履歴 + AR 予測 + 予測区間 band)
  , ForecastModel (..)
  , forecastModel
    -- * 多変量・木 (描画可能 = 自己完結、 PCA scree / RF 重要度)
  , PCAResult (..)
  , RandomForest (..)
    -- * 木/アンサンブル — Phase 68 A2 (重要度 bar / 決定木 樹形図)
    --   GradientBoosting / RandomForestClassifier = 特徴重要度 bar、
    --   DecisionTree = MDAG 再利用の樹形図 (新規 mark 不要)
  , GBRegressor (..)
  , GBClassifier (..)
  , RFClassifierFit (..)
  , DTree (..)
  , DTFit (..)
  , treeImportances
  , treePlot
  , treePlotRaw
    -- * 分類 — Phase 68 A3 (決定境界 + confusion + 代表散布)
    --   Discriminant / NaiveBayes / KNN。 決定境界・confusion はヘルパ (要範囲/データ)、
    --   toPlot は KNN=訓練点散布 / Discriminant・NB=クラス平均散布
  , ClassPredict (..)
  , decisionBoundaryOf
  , confusionOf
  , MDSView
  , mdsView
  , mdsGroupBy
  , nnLossOf
  , ResidualMode (..)
  , ProfilerSpec (..)
  , profiler
  , profilerResidual
  , contourOf
    -- DOE ワークフロー (Phase 78・Hanalyze.Fit 由来)
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
  , DesignHBMFit (..)
  , designModelHBM
  , MultiOutputSpec (..)
  , multiOutput
  , modelFor
  , svmSupportVectorsOf
  , ScorePredict (..)
  , decisionLineOf
    -- 部分従属図 (PDP / ICE) — Phase 75.27
  , RegPredict (..)
  , PDPView
  , pdp
  , pdpIce
  , pdpOf
  , pdpIceOf
  , pdpPlot
  , pdpIcePlot
  , partialDependencePlot
  , partialDependenceIcePlot
  , DiscriminantFit (..)
  , NBModel (..)
  , GaussianNB (..)
  , KNNClassifier (..)
    -- * 次元圧縮 — Phase 68 A4 (PLS score/loading/VIP, MultiGP 多出力 curve)
  , PLSFit (..)
    -- ** PLS 診断ビュー (中間 Plottable Spec・HBM 式統一 — Phase 70.B)
  , PLSView (..)
  , PLSViewKind (..)
  , scoreView
  , loadingView
  , vipView
  , MultiGPResult (..)
  , multiGpCurves
    -- * 時系列・生存・FDA — Phase 68 A5
    --   GARCH=volatility 帯付き線 / AFT=生存曲線 / FDA=平均+固有関数 / β(t)
  , GARCHFit (..)
  , garchVolatility
  , AFTFit (..)
  , aftSurvivalAt
  , FunctionalPCA (..)
  , FLMResult (..)
    -- * 罰則回帰・因果探索 — Phase 68 A6
    --   Regularized=係数 bar/係数パス / LiNGAM=因果 DAG (MDAG 再利用)
  , RegFit (..)
  , regPathPlot
  , DirectLiNGAMFit (..)
  , lingamDag
    -- * 記述統計・検定 — Phase 68 A7 (describe 分布図 / 検定 effect-CI forest)
  , TestResult (..)
  , testForest
  , testForestLabeled
  , describeBox
    -- * クラスタリング (Phase 68 A1) — KMeans の図
    --   'Plottable' 'KMeansResult' (toPlot = centroid 散布) + データ点ヘルパ
  , clusterScatterOf
  , centroidsOf
  , clusterHullOf
  , clusterEllipseOf
  , DendroOpts (..)
  , defaultDendroOpts
  , dendrogramOf
  , dendrogramOf'
    -- * HBM (ベイズ確率プログラム) の学習 — Phase 49 A1
  , HBMConfig (..)
  , defaultHBM
  , HBMModel (..)
  , hbmModel
  , hbmModelPure
  , hbmModelIO
    -- * HBM の出力抽出子 — Phase 49 A2 / Phase 74 (trace / forest)
  , hbmParamNames
  , TraceOpts (..)
  , defaultTraceOpts
  , tracesOf
  , tracesOfWith
  , marginalsOf
  , marginalsByChainOf
    -- * HBM のサンプリング診断 — Phase 59 (divergence 可視化)
  , divergencesOf
  , pairOf
  , energyOf
  , autocorrOf
  , autocorrOfLag
  , defaultAutocorrMaxLag
  , rankOf
  , rankOfBins
  , defaultRankBins
  , ForestSpec (..)
  , forestOf
  , forestOfLevel
    -- * HBM の出力抽出子 — Phase 49 A3 (epred = 事後予測平均 + HDI band)
  , epred
  , epredAt
    -- * HBM の出力抽出子 — Phase 49 A4 (ppc = 事後予測チェック)
  , PPCConfig (..)
  , defaultPPC
  , PPCSpec (..)
  , ppcOf
  , ppcOfWith
  , ppcOfIO
  , ppcOfWithIO
    -- * HBM の出力抽出子 — Phase 49 A5 (dag = モデル構造の DAG)
  , DagSpec (..)
  , dagOf
  , dagOfRaw
  , dagOfModel
  , dagOfModelWith
    -- * HBM 診断ダッシュボード — Phase 74.8 (抽出子束ね)
  , dashboardOf
  , dashboardFullOf
  , traceDensityOf
    -- * df |-> spec 統一 fit API — Phase 51 (ColumnSource から学習)
  , Fit (..)
  , (|->)
  , (|->!)
    -- ** 二変量近道 spec (列名2つ) — Phase 51.2
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
    -- ** 行列入力モデルの高レベル spec (列名リスト) — Phase 70.A
  , PCASpec (..)
  , pca
    -- MDS (Phase 75.21)
  , MDSSpec (..)
  , mds
  , MDSConfig (..)
  , MDSMethod (..)
  , defaultMDS
  , MDSResult (..)
  , PCAStandardize (..)
  , PLSSpec (..)
  , pls
  , PLSConfig (..)
  , defaultPLS
  , LDASpec (..)
  , lda
  , CCASpec (..)
  , ccaOf
  , CCAFit (..)
    -- ** 教師あり ML 分類器/回帰器 spec (特徴列 + ラベル列) — Phase 70.A
  , GBRSpec (..)
  , gbmReg
  , GBCSpec (..)
  , gbmCls
  , GBConfig (..)
  , defaultGBM
  , DTSpec (..)
  , decisionTree
  , DTConfig (..)
  , defaultDecisionTree
  , KNNCSpec (..)
  , knnCls
  , KNNRSpec (..)
  , knnReg
  , NBSpec (..)
  , naiveBayes
    -- ** seed 純粋化した RNG モデル spec (KMeans / RandomForest) — Phase 70.A
  , KMeansSpec (..)
  , kmeans
  , KMeansConfig (..)
  , defaultKMeans
  , RFSpec (..)
  , randomForestReg
    -- 因果探索 LiNGAM (高レベル df|-> ・Phase 77)
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
  , LiNGAMFitted (..)
  , lingamDagNamed
  , varLagDagNamed
  , bootstrapEdgeProbOf
  , RFCSpec (..)
  , randomForestCls
  , RFCConfig (..)
  , defaultRFCConfig
  , RFConfig (..)
  , defaultRandomForest
    -- ** SVM / 古典 MLP 高レベル spec (純粋・df |->) — Phase 75.9
  , MLPClsSpec (..)
  , mlpCls
  , MLPRegSpec (..)
  , mlpReg
  , SVMSpec (..)
  , svmCls
  , SVMHyper (..)
  , SVMTuneGrid (..)
  , defaultSVMTuneGrid
  , SVMConfig (..)
  , defaultSVM
  , SVM (..)
  , SVMMulti (..)
  , numSupportVectors
    -- ** 重み付き最小二乗 (WLS) spec — Phase 52.A6
  , WeightedLMSpec (..)
  , weighted
  , WeightedLMModel (..)
    -- ** 透過標準化ラッパ (自動逆変換) — Phase 70.3 項目 C
  , StandardizedSpec (..)
  , standardized
  , standardizedY
  , StandardizedModel (..)
    -- ** 群別フィット spec — Phase 52.A4
  , GroupedSpec (..)
  , grouped
  , GroupedFit (..)
  , groupModels
  , groupLabels
  , groupedFullrange
    -- ** 係数診断の薄アクセサ — Phase 52.A9
  , CoefStats (..)
  , lmDiag
  , groupedLmDiag
    -- ** formula 多変量 spec (R 流) — Phase 51.3
  , LMFormulaSpec (..)
  , lmF
  , GLMFormulaSpec (..)
  , glmF
  , GLMMFormulaSpec (..)
  , glmmF
    -- ** 重回帰 spec (列名リスト・formula 不要) — Phase 70.D
  , LMMultiSpec (..)
  , lmMulti
  , GLMMultiSpec (..)
  , glmMulti
  , RobustMultiSpec (..)
  , rlmMulti
  , QuantileMultiSpec (..)
  , rqMulti
  , MultiQuantileModel (..)
    -- ** HBM spec + データ散布図 — Phase 51.4
  , HBMSpec
  , hbm
  , dataScatterOf
  ) where

import qualified Data.Map.Strict       as Map
import           Data.Maybe            (fromMaybe)
import qualified Data.Vector           as V
import qualified Data.Vector.Unboxed    as VU
import qualified Numeric.LinearAlgebra as LA

import           Data.Text             (Text)
import qualified Data.Text             as T
-- (DataFrame の直接 import は未使用のため削除 = upstream decomp PR#2 移植の副産物調査で判明)

import           Hanalyze.Data.ColumnSource     (ColumnSource (..))

import           Graphics.Hgg.Spec     ( VisualSpec, layer, inline, inlineCat
                                       , ColData (..)
                                       , scatter, line
                                       , heatmap, colorBy
                                       , scaleColorManual, legend
                                       , bar, title
                                       , LineType (..) )
import qualified Graphics.Hgg.ThreeD.Spec  as P3

import           Hanalyze.Model.Wrappers
import           Hanalyze.Plot.Core
-- 族別 instance module (Phase 71.5)。 orphan instance を scope に取り込み、
-- 移した族固有 helper (multiGpCurves) を re-export する。
import           Hanalyze.Plot.Linear ()
import           Hanalyze.Plot.Smooth (multiGpCurves)
import           Hanalyze.Plot.Robust ()
-- ベイズ / HBM 連携族 (Phase 71.6)。 orphan instance を scope に取り込み (())、
-- 移した抽出子・型を re-export する。 epredPredRange は本 module の
-- epredSurfaceOfWith でも使うため明示 import する。
import           Hanalyze.Plot.Bayes ()
import           Hanalyze.Plot.Bayes
                   ( hbmParamNames, TraceOpts (..), defaultTraceOpts
                   , tracesOf, tracesOfWith, marginalsOf
                   , marginalsByChainOf, divergencesOf
                   , pairOf, energyOf, autocorrOf, autocorrOfLag, defaultAutocorrMaxLag
                   , rankOf, rankOfBins, defaultRankBins
                   , ForestSpec (..), forestOf, forestOfLevel
                   , epred, epredAt, epredPredRange
                   , PPCConfig (..), defaultPPC, PPCSpec (..)
                   , ppcOf, ppcOfWith, ppcOfIO, ppcOfWithIO
                   , DagSpec (..), dagOf, dagOfRaw, dagOfModel, dagOfModelWith
                   , dashboardOf, dashboardFullOf, traceDensityOf
                   , epredSurfaceOf, epredSurfaceOfWith, dataScatterOf )
-- 汎用ラッパ族 (Phase 71.7)。 orphan instance を scope に取り込み (())、
-- 移したヘルパ (lmDiag / groupedLmDiag / groupedFullrange) を re-export する。
import           Hanalyze.Plot.Wrappers ()
import           Hanalyze.Plot.Wrappers
                   ( lmDiag, groupedLmDiag, groupedFullrange )
-- ML / 統計モデル連携族 (Phase 71.6)。 orphan instance を scope に取り込み (())、
-- 移した抽出子・ヘルパ・型を re-export する。
import           Hanalyze.Plot.ML ()
import           Hanalyze.Plot.ML
                   ( clusterScatterOf, centroidsOf, clusterHullOf, clusterEllipseOf
                   , DendroOpts (..), defaultDendroOpts, dendrogramOf, dendrogramOf'
                   , treeImportances, treePlot, treePlotRaw
                   , decisionBoundaryOf, confusionOf, MDSView, mdsView, mdsGroupBy, nnLossOf, svmSupportVectorsOf, ScorePredict (..), decisionLineOf
                   , RegPredict (..), PDPView, pdp, pdpIce
                   , pdpOf, pdpIceOf, pdpPlot, pdpIcePlot, partialDependencePlot, partialDependenceIcePlot
                   , PLSView (..), PLSViewKind (..), scoreView, loadingView, vipView
                   , garchVolatility, aftSurvivalAt
                   , regPathPlot, lingamDag, lingamDagNamed, varLagDagNamed, bootstrapEdgeProbOf
                   , ResidualMode (..), ProfilerSpec (..), profiler, profilerResidual, contourOf
                   , testForest, testForestLabeled, describeBox )
import           Hanalyze.Diagnostics
import           Hanalyze.Fit
import           Hanalyze.Model.SVM (SVMConfig (..)
                                       , defaultSVM, SVM (..)
                                       , SVMMulti (..), numSupportVectors
                                       , SVMHyper (..)
                                       , SVMTuneGrid (..), defaultSVMTuneGrid)
import           Hanalyze.Model.MDS (MDSResult (..))
import           Hanalyze.Model.LM.Diagnostics (CoefStats (..), lmCoefStats)
import           Hanalyze.Model.GP     (GPResult (..), Kernel (..), GPParams (..), defaultGPParams)
import           Hanalyze.Model.LM     (linspace)
import           Hanalyze.Model.GAM    (GAMBasis (..), GAMLambda (..)
                                              , fitGAMWith)
import           Hanalyze.Model.MultiLM (MultiFit (..))
import           Hanalyze.Model.Cluster (KMeansConfig (..), defaultKMeans)
import           Hanalyze.MCMC.Core     (Chain (..))
import           Hanalyze.Model.HBM     (ModelP, withData
                                       , runDeterministics)
import           Hanalyze.Model.Survival (KMResult (..))
import           Hanalyze.Model.CompetingRisks (CRFit (..))
import           Hanalyze.Model.PCA     (PCAResult (..), PCAStandardize (..))
import           Hanalyze.Stat.Standardize
                   ( Standardizer (..)
                   , applyStandardizerCol )
import           Hanalyze.Model.RandomForest (RandomForest (..)
                                       , RFConfig (..), defaultRandomForest)
import           Hanalyze.Model.GradientBoosting (GBRegressor (..), GBClassifier (..)
                                       , GBConfig (..), defaultGBM)
import           Hanalyze.Model.RandomForestClassifier (RFClassifierFit (..)
                                       , RFCConfig (..), defaultRFCConfig)
import           Hanalyze.Model.DecisionTree (DTree (..), DTFit (..), DTConfig (..), defaultDecisionTree)
import           Hanalyze.Model.Discriminant (DiscriminantFit (..))
import           Hanalyze.Model.Multivariate (CCAFit (..))
import           Hanalyze.Model.NaiveBayes (NBModel (..), GaussianNB (..))
import           Hanalyze.Model.KNN (KNNClassifier (..)
                                       , KNNRegressor (..), predictKNNR)
import           Hanalyze.Model.PLS (PLSFit (..), PLSConfig (..), defaultPLS)
import           Hanalyze.Model.MultiGP (MultiGPResult (..))
import           Hanalyze.Model.GARCH (GARCHFit (..))
import           Hanalyze.Model.AFT (AFTFit (..))
import           Hanalyze.Model.FDA (FunctionalPCA (..), FLMResult (..))
import           Hanalyze.Model.Regularized (RegFit (..))
import           Hanalyze.Model.LiNGAM.Direct (DirectLiNGAMFit (..))
import           Hanalyze.Stat.Test (TestResult (..))

-- ===========================================================================
-- 共通基盤 (class / ModelSpec / grid 評価核) は 'Hanalyze.Plot.Core' へ
-- 切り出した (Phase 71.4)。 本モジュールは Core を import して従来 export を
-- re-export しつつ、 各モデル族固有の instance を残置する。
-- ===========================================================================

-- ===========================================================================
-- ルート1 grid 評価 (ModelSpec) — Phase 16 §3 C1 [→ Plot.Core へ移動]
--
-- fit 済モデルの回帰曲線・CI 帯を **訓練点ではなく等間隔 grid** で評価して描く。
-- 疎・不均一データで曲線がガタつくのを解消する (散布図の点は従来通り訓練データ)。
-- 'statModel' で 'ModelSpec' を作り、 @<>@ でオプションを足す:
--
-- > df |>> (layer (scatter "x" "y") <> toPlot (statModel m <> grid 200))
--
-- 'ModelSpec' は Monoid。 学習済モデル @m@ はクロージャに閉じ込め、 予測は
-- 'toPlot' (描画時) に grid 評価する (ユーザ直感「m は学習・layer で予測」)。
-- ===========================================================================


-- ===========================================================================
-- 多変量 effect plot (Phase 16 §3 C3)
--
-- 単変数 grid 評価 (C1) を多変量モデルへ一般化する。 along 変数を grid で動かし、
-- 他の説明変数を 'HoldAgg' で固定した「評価点 ModelFrame」 を合成して、 訓練 formula の
-- 'designMatrixF' で評価点設計行列を組み CI を評価する。
--
-- ★評価点 ModelFrame の合成は **DataFrame を経由せず VarRole を直接差し替える**
-- ('designMatrixF' は 'mfRoles' のみ参照し応答列は使わない = Design.hs:331)。 列構造・
-- 順序が訓練と完全一致するので 'confidenceBandAt' / 'predictGlmMuWithCI' がそのまま使える。
-- 型で単/多変量を分離し ('SingleVarModel' / 'MultiVarModel')、 along 忘れをコンパイル時に弾く。
-- ===========================================================================



-- ===========================================================================
-- 多変量モデル型 (effect plot 用、 新規 fit)
--
-- 既存の単変数 'LMModel' / 'GLMModel' (設計行列が @[1, x]@ 固定) とは別型。
-- formula 文字列 + 'DataFrame' で多変量 fit し、 formula を保持して評価点設計行列を
-- 組む (HoldAgg 固定 + along grid)。 ★GLM は formula 経路が未整備なので
-- 'designMatrixF' で設計行列を作り 'fitGLMFull' を直接呼ぶ。
-- ===========================================================================

-- (instance MultiVarModel MultiLMModel は Hanalyze.Plot.Linear へ移動 — Phase 71.5)

-- ===========================================================================
-- 列名リスト → 加法線形 Formula AST (パース無し直接合成) — Phase 70.D
--
-- 重回帰 (multiple regression) は formula DSL とは別概念: 説明変数の列名リストから
-- 設計行列 @[1, x1, …, xp]@ を作るだけ。 これを文字列を介さず 'Formula' AST に直接
-- 組み立て、 既存の 'multiLMModelF' / 'designMatrixF' / effect plot 機構をそのまま使う
-- (= @parseModel "y ~ x1 + … + xp"@ と同一 AST。 パラメータ名 @_p0.._pp@ も同じ規約)。
-- ===========================================================================

-- ===========================================================================
-- 多変量ロバスト回帰 (effect plot + 係数サマリ) — Phase 70.D
--
-- ロバスト回帰は formula 経路を持たない (単回帰 'RobustModel' のみだった) ので、
-- 'MultiLMModel' と同型の frame-carrying ラッパを新設する。 設計行列は
-- 'additiveFormula' 由来 ('designMatrixF' で @[1, x1,…,xp]@)、 fit は 'fitRobustLM'、
-- CI 帯は M 推定量サンドイッチ共分散 ('robustCovBeta'・statsmodels RLM 一致)。
-- ===========================================================================

-- (instance MultiVarModel MultiRobustModel は Hanalyze.Plot.Robust へ移動 — Phase 71.5)

-- (instance MultiVarModel MultiGLMModel は Hanalyze.Plot.Linear へ移動 — Phase 71.5)

-- (instance MultiVarModel PLSModel は Hanalyze.Plot.ML へ移動 — Phase 71.6)

-- ===========================================================================
-- 線形モデル (描画可能)
--
-- 'FitResult' (数値核) は設計行列 X を保持しないが、 回帰線・CI band を描くには
-- X が要る ('confidenceBand' は X 引数)。 そこで X と生 predictor を束ねた
-- 「描画可能なモデル」 を別型にする (= plot Phase 15 §2.1 の開放論点を (i) で確定)。
-- ===========================================================================


-- (instance Plottable LMModel / SingleVarModel LMModel は
--  Hanalyze.Plot.Linear へ移動 — Phase 71.5)

-- ===========================================================================
-- 一般化線形モデル (描画可能)
--
-- GLM の不確実性帯は **μ (応答) スケールで非対称** (線形予測子 η の対称 Wald CI を
-- 逆リンク gInv で μ に写すため、 Logit/Log 等では下側・上側の半幅が異なる)。 ゆえに
-- LMModel/GPResult の対称 band (ŷ±se) では忠実に描けない。 そこで
-- 下境界 lo / 上境界 hi を別々に持てる 'band' layer (= MBand area fill) を使い、 μ 曲線は
-- 'line' で重ねる。 帯は **訓練点での Wald CI** を 'predictGlmMuWithCI' で評価する
-- (= grid 補間でなく fit と整合)。 'fitGLMFull' が返す逆 Fisher 情報 Σ=(XᵀWX)⁻¹ が要る。
-- ===========================================================================

-- (instance Plottable GLMModel / SingleVarModel GLMModel は
--  Hanalyze.Plot.Linear へ移動 — Phase 71.5)

-- ===========================================================================
-- ガウス過程 (描画可能)
--
-- 'GPResult' (Hanalyze.Model.GP) は予測 grid (gpTestX) + 事後平均 (gpMean) +
-- credible band (gpLower/gpUpper) を **自己完結** で保持する。 ゆえに LMModel の
-- ように X を別途束ねる必要がなく、 結果型をそのまま 'Plottable' にできる
-- (= 'FitResult' 系と異なる形でも protocol が成り立つことの実証 = plot Phase 15
-- / analyze Phase 46 A6)。
-- ===========================================================================

-- (instance Plottable GPResult は Hanalyze.Plot.Smooth へ移動 — Phase 71.5)

-- ===========================================================================
-- スプライン回帰 (描画可能)
--
-- 'SplineFit' (Hanalyze.Model.Spline) は基底係数 'sfBeta' と、 基底行列で fit した
-- 線形モデル核 'sfResult' (= 'FitResult') を保持する。 ゆえに **基底行列を設計行列と
-- みなせば** LMModel と同じ 'confidenceBand' (= X (XᵀX)⁻¹ Xᵀ の対角) がそのまま使える。
-- 違いは「曲線」 である点だけ: 単回帰の直線でなく、 訓練点を x 昇順に結ぶと基底展開に
-- よる平滑曲線になる ('renderRegression' は encX/encY を線形再フィットせず折れ線で
-- 結ぶため、 ソート済みの点列を渡せば曲線がそのまま描ける = GP と同じ性質)。 帯は
-- LM と同じ **線形モデルの対称 Wald CI** (基底空間での予測分散) なので意味付けも明快。
-- ===========================================================================

-- (splineBasisAt / instance Plottable SplineModel / SingleVarModel SplineModel /
--  Plottable GAMModel / gamGridCI / SingleVarModel GAMModel / SingleVarModel GAMModelN /
--  Plottable GAMModelN は Hanalyze.Plot.Smooth へ移動 — Phase 71.5)

-- ===========================================================================
-- ロバスト回帰 (描画可能)
--
-- 'RobustFit' (Hanalyze.Model.Robust) は M-estimator IRLS の係数 'rfCoef' / fitted
-- 'rfFitted' / 最終重み 'rfWeights' (≤ 1、 外れ値ほど小) を持つが、 **CI / 予測帯を
-- 返す helper を持たない** (sandwich 分散等を別途計算すれば帯は出せるが本 Phase 対象外)。
-- ゆえに代表図 ('toPlot') は **ロバスト直線のみ** (band 無し)。 ロバスト回帰の価値=
-- 「どの点がダウンウェイトされたか」 は 'diagnosticPlots' 側で **点サイズ = IRLS 重み**
-- の散布図に encode して見せる (主図に点を描くと合成 @df |>> layer scatter <> toPlot@
-- で点が二重になるため、 主図は直線だけにして重み表示は診断束へ回す = user 決定 2026-06-04)。
-- ===========================================================================

-- (instance Plottable RobustModel / robustBand / SingleVarModel RobustModel は
--  Hanalyze.Plot.Robust へ移動 — Phase 71.5)

-- ===========================================================================
-- 多出力線形回帰 (描画可能)
--
-- 'MultiFit' (Hanalyze.Model.MultiLM) は q 個の応答を共通の予測子で同時回帰し、
-- 固有の成果物として **出力間の残差相関 'mfResidCor' (q×q)** を保持する。 q 本の回帰
-- 関係を単一図に素直に載せる方法は一意でない (出力ごとスケールが異なり得る) ため、
-- 代表図 ('toPlot') は **残差相関 heatmap** とする (= 多出力回帰固有の図。 user 決定
-- 2026-06-04)。 'MultiFit' は heatmap に必要な相関行列を自己完結で持つので、 'GPResult'
-- 同様 X を別途束ねず結果型をそのまま 'Plottable' にできる。 個別の出力 j の回帰線は
-- 'predictMultiLM' で別途描ける (本 instance の対象外)。
--
-- ⚠ 'heatmap' (geom_tile) は **categorical 軸専用** (renderHeatmap が x/y をラベルとして
-- カテゴリ軸の index に引く。 実測: Render/Statistical.hs)。 ゆえに格子座標は数値でなく
-- **出力名ラベル** ("y1", "y2", …) を 'inlineCat' で渡す (数値だとカテゴリ軸が立たず
-- 全セルが drop されてタイルが描かれない = 計測で確認)。
-- ===========================================================================

-- (instance Plottable MultiFit は Hanalyze.Plot.Wrappers へ移動 — Phase 71.7)

-- ===========================================================================
-- 分位点回帰 (描画可能)
--
-- 'QRFit' (Hanalyze.Model.Quantile) は 1 つの分位 τ に対する係数 + fitted 'qfYHat' を
-- 持つ。 OLS が条件付き平均を引くのに対し分位回帰は条件付き τ-分位を引くので、 複数の
-- τ (例 0.1/0.5/0.9) の fit を重ねると **予測区間そのものを線群で** 表現できる
-- (= heteroscedastic データで帯より直接的)。 ゆえに 'QuantileModel' は複数の τ-fit を
-- 束ね、 'toPlot' で **分位ごとに 1 本の line layer を色分けして重畳** する (band は使わ
-- ない。 分位線自体が区間の縁を成すため)。 各線は 'color' ('fromHex') で固定色を割り当てる。
-- ===========================================================================

-- (instance Plottable QuantileModel / Plottable MultiQuantileModel は
--  Hanalyze.Plot.Robust へ移動 — Phase 71.5)


-- ===========================================================================
-- クラスタリング (KMeans) の図 — Phase 68 A1
--
-- KMeans の分野定番の図は「クラスタ別散布 (色=ラベル)」。 ただし
-- 'KMeansResult' は centroids + labels + inertia のみ保持し **生データ座標を
-- 持たない**。 そこで 'surfaceOf' <> 'dataScatter3DOf' と同じ **model 層 / data
-- 層の二層イディオム**に分ける:
--
--   * 'Plottable' 'KMeansResult' の 'toPlot' = centroid 散布のみ (データ不要・
--     クラス契約 @m -> VisualSpec@ を満たす)。 既定は centroid 行列の第 0/1 次元。
--   * 'clusterScatterOf' = データ点をラベル色で散布 (要データ源・列名指定)。
--   * 'centroidsOf' = centroid を任意 2 次元で重畳 (✚ マーカー・次元 index 明示)。
--
-- 定番図 = @df |>> (clusterScatterOf df res \"x\" \"y\" <> centroidsOf res 0 1)@。
-- ⚠ centroid 行列は **学習時の特徴量列順**のみで列名を持たない。 重畳時は
-- データ列 (@xn@, @yn@) と centroid 次元 (@i@, @j@) の対応をユーザが揃える。
-- ===========================================================================

-- (instance Plottable KMeansResult / clusterScatterOf / centroidsOf は
--  Hanalyze.Plot.ML へ移動 — Phase 71.6)

-- ===========================================================================
-- HBM (ベイズ確率プログラム) の学習 — Phase 49 A1
--
-- 'Hanalyze.Model.HBM' の free-monad DSL で書いた確率プログラム ('ModelP') を
-- NUTS で学習し、 「学習済 HBM モデル」 ('HBMModel') という第一級の値にする。
-- 命名は頻度論側の @lmModel → LMModel@ / @glmModel → GLMModel@ と対称的
-- ('hbmModel → HBMModel')。 違いは学習が MCMC ゆえ IO・重い・async 並列
-- (既存 'nutsChains' が 'mapConcurrently' で multi-chain 並列) という点のみ。
--
-- データは df 由来の列 (名前付き) を 'withData' でモデル中の placeholder
-- ('dataNamed' / observe の参照名) に **自動 bind** する。 これは PyMC の
-- @pm.Data@ + @set_data@ と同型 (= 同じモデルを別データで再評価できる設計)。
--
-- ★ 'HBMModel' は **直接 'Plottable' にしない** (確率プログラムは「単一の図」 に
-- 一意に落ちない)。 描画は抽出子 ('epred' / 'tracesOf' / 'ppcOf' / 'forestOf' /
-- 'dagOf'、 後続 sub で追加) を明示する設計 (Phase 49 計画 Q1)。
-- ===========================================================================


-- ===========================================================================
-- 生存解析 (描画可能)
--
-- 生存関数 Ŝ(t) (Kaplan-Meier) と累積発生関数 CIF (競合リスク) はいずれも **階段関数**
-- (イベント時刻で不連続にジャンプ、 その間は平坦)。 折れ線 ('line') は点間を線形に結ぶので、
-- そのまま渡すとジャンプが斜めになる。 ゆえに **階段頂点を明示展開** する helper
-- 'stepVerts' で (0, s0) から各イベント時刻の「水平→垂直」 2 頂点を作り、 line で結ぶ
-- (= 正しい階段形)。 KM は s0=1 で下降、 CIF は s0=0 で上昇。 KMResult / CRFit は時刻と
-- 値を自己完結で持つので 'GPResult' 同様そのまま 'Plottable' にできる。
-- ===========================================================================


-- (instance Plottable KMResult / Plottable CRFit は
--  Hanalyze.Plot.ML へ移動 — Phase 71.6)

-- ===========================================================================
-- 時系列予測 (描画可能)
--
-- AR(p) の点予測 'forecastAR' は将来値の中心のみを返す。 予測の不確実性帯は **h-step
-- 予測分散** から得る: AR の MA(∞) 表現の ψ-weights (ψ₀=1, ψⱼ=Σφᵢψⱼ₋ᵢ) を用いて
-- @Var(ŷ_{n+k}) = σ² Σ_{j=0}^{k-1} ψⱼ²@ (σ² = 革新分散 'arResidVar')。 これは Gaussian
-- 革新の下での正統な予測区間 (地平 k とともに単調に広がる)。 対称ゆえ band は
-- @中心 ± z·se@。 'toPlot' は履歴折れ線 + 予測折れ線 + 予測区間 band を 1 枚に重ねる。
-- ===========================================================================

-- (arPsiWeights / arForecastSE / instance Plottable ForecastModel は
--  Hanalyze.Plot.ML へ移動 — Phase 71.6)

-- ===========================================================================
-- 多変量・木 (描画可能)
--
-- PCA の代表図は **scree plot** (各主成分の寄与率 'pcaExplainedRatio' を棒で)、 木 (RF) の
-- 代表図は **特徴重要度バー** ('featureImportance')。 いずれも自己完結ゆえそのまま
-- 'Plottable'。 棒の x 軸はラベル ("PC1".. / "f1"..) なので 'inlineCat' (categorical) で渡す
-- (heatmap A9 と同じく 'bar' も categorical 軸が必要)。 優先低 (§3.5 A14) ゆえ scree/重要度
-- の 1 枚ずつに絞る (biplot や木構造図は将来拡張)。
-- ===========================================================================

-- (instance Plottable PCAResult / Plottable RandomForest は
--  Hanalyze.Plot.ML へ移動 — Phase 71.6)

-- ===========================================================================
-- 木/アンサンブル — Phase 68 A2
--
-- 各モデルの分野定番図を **既存 mark のみ**で描く (新規 plot mark 不要):
--
--   * GradientBoosting (回帰/分類)・RandomForestClassifier = **特徴重要度 bar**。
--     GBM は重要度フィールドを持たないので弱学習器 ('Tree') の split 使用回数から
--     純粋計算する ('treeImportances'・RF.'featureImportance' と同方式・正規化)。
--   * DecisionTree = **樹形図**。 決定木は DAG の特殊形 (二分木) ゆえ、 HBM の
--     ModelGraph と同じ MDAG (Sugiyama 階層 layout) を **再利用**して node-link で描く
--     (split ノード = "f{j} ≤ {thr}"、 葉 = "y={class}")。
--
-- ⚠ DecisionTree の edge True/False ラベル・gini・サンプル数表示 (sklearn plot_tree
-- 相当) は DAGNode/DAGEdge が持たないため v1 では描かない。 必要なら専用 mark を
-- plot 側 Phase として起こす (= dendrogram Phase 48 と同型の判断)。
-- ===========================================================================

-- (treeImportances / instance Plottable GBRegressor / GBClassifier /
--  RFClassifierFit / DTree / dtreeToDag は Hanalyze.Plot.ML へ移動 — Phase 71.6)

-- ===========================================================================
-- 分類 (Discriminant / NaiveBayes / KNN) — Phase 68 A3
--
-- 代表図は **決定境界** と **confusion 行列**。 いずれも「学習済モデルを評価点で
-- 走らせる」 図ゆえ、 KMeans (A1) と同じく **データ/範囲を取るヘルパ**で提供する
-- (新規 plot mark 不要):
--
--   * 'decisionBoundaryOf' = 2D grid を予測しクラス色で塗る (= 連続軸の散布を
--     四角マーカー・低 alpha で「領域」表現。 ★renderHeatmap はカテゴリ軸なので
--     連続 grid には不適 → 'MScatter' + 'colorBy' (離散色) を採用)。 2 特徴前提。
--   * 'confusionOf' = テストデータの真値×予測の件数を 'MHeatmap' で (カテゴリ軸が適合)。
--
-- 'Plottable' の 'toPlot' (データ非保持で描ける代表 1 枚):
--   * KNN は訓練データ ('knnCX'/'knnCY') を保持 → **ラベル色の訓練点散布**。
--   * Discriminant / NaiveBayes(Gaussian) は **クラス平均散布** (✚)、
--     NaiveBayes(Multinomial) は **クラス事前確率 bar**。
-- ===========================================================================


-- (instance ClassPredict DiscriminantFit / NBModel / KNNClassifier /
--  decisionBoundaryOf / confusionOf / instance Plottable KNNClassifier /
--  DiscriminantFit / NBModel は Hanalyze.Plot.ML へ移動 — Phase 71.6)

-- ===========================================================================
-- 次元圧縮 (PLS / MultiGP) — Phase 68 A4
--
-- どちらも結果が自己完結 ('PCAResult' 同様) なので外部データ不要で 'Plottable':
--
--   * 'PLSFit' = 潜在空間の **score plot** (標本 T) を代表図に、 'loading plot' (変数 P)
--     と **VIP bar** を診断図束に。 いずれも既存 'MScatter'/'bar'。
--   * 'MultiGPResult' = **多出力の予測曲線 + 95% band** (出力ごとに色分け・x=index)。
--     'MLine' + 'MBand' を出力数ぶん重畳。
--
-- ※ 'Hanalyze.Model.MultiOutput' は変換+メトリクスの **ユーティリティ**で
-- fit 結果型を持たないため 'Plottable' 対象外 (多出力の「相関」図は既存
-- 'MultiFit' = 残差相関 heatmap が担当)。 新規 plot mark は不要。
-- ===========================================================================


-- (PLSViewKind / PLSView / scoreView / loadingView / vipView /
--  instance Plottable PLSView / PLSFit は Hanalyze.Plot.ML へ移動 — Phase 71.6)

-- (multiGpCurves / instance Plottable MultiGPResult は
--  Hanalyze.Plot.Smooth へ移動 — Phase 71.5)

-- ===========================================================================
-- 時系列・生存・FDA (GARCH / AFT / FDA) — Phase 68 A5
--
-- 新規 plot mark は不要 (既存 line/band の重畳):
--
--   * 'GARCHFit'      = 系列 (μ + ε_t) + 条件付き volatility 帯 (μ ± 2σ_t) の帯付き線。
--   * 'AFTFit'        = パラメトリック生存曲線 S(t|x)。 fit は観測時刻を持たないので
--                       代表図 ('toPlot') は **基準共変量** (intercept のみ) の曲線、
--                       任意共変量は 'aftSurvivalAt' ヘルパ。 t 範囲は予測平均寿命から導出。
--   * 'FunctionalPCA' = 平均関数 + 上位固有関数を grid 上に重畳 (x = grid index)。
--   * 'FLMResult'     = 関数回帰係数 β(t) の曲線。
-- ===========================================================================

-- (garchVolatility / instance Plottable GARCHFit / aftSurvivalAt /
--  instance Plottable AFTFit / FunctionalPCA / FLMResult は
--  Hanalyze.Plot.ML へ移動 — Phase 71.6)

-- ===========================================================================
-- 罰則回帰・因果探索 (Regularized / LiNGAM) — Phase 68 A6
--
-- 新規 plot mark は不要:
--
--   * 'RegFit'          = 単一 λ の係数 ('rfBeta') を bar (代表図)。
--   * 'regPathPlot'     = 正則化パス @[(λ, [β_j])]@ ('regularizationPath' 出力) を、
--                         係数ごとに 1 本の line で λ-横軸に重畳 (= LASSO 係数パス図)。
--   * 'DirectLiNGAMFit' = 推定した因果構造を **MDAG** で描く (B 行列 → node/edge、
--                         決定木と同じ MDAG 再利用)。 edge j→i は @|adjacency[i,j]|>0@。
-- ===========================================================================

-- (instance Plottable RegFit / regPathPlot / lingamDag /
--  instance Plottable DirectLiNGAMFit は Hanalyze.Plot.ML へ移動 — Phase 71.6)

-- ===========================================================================
-- 記述統計・検定 (Stat.*) — Phase 68 A7
--
-- 新規 plot mark は不要:
--
--   * 'TestResult'  = 効果量 + 95% CI の **forest** (検定パラメータの区間 + 0 基準線)。
--                     代表図 ('toPlot') は 1 行 forest、 複数検定は 'testForest'。
--   * 'describeBox' = 生データ列の **box plot** (= describe の分布図・5 数要約を可視化)。
-- ===========================================================================

-- (testForest / testForestLabeled / instance Plottable TestResult /
--  describeBox は Hanalyze.Plot.ML へ移動 — Phase 71.6)


-- (instance SingleVarModel WeightedLMModel / Plottable WeightedLMModel は
--  Hanalyze.Plot.Linear へ移動 — Phase 71.5)



-- C2: 元スケール逆変換 instance (Phase 70.3 項目 C) -------------------------
--
-- 内側モデルは標準化空間で学習されている。 ここで予測子 x を入力時に標準化し、
-- ('standardizedY' なら) 応答 y を出力時に逆変換することで、 図・予測を**元スケール**で
-- 返す。 単変量 (1 特徴) 描画が対象 (smXStd の 0 次元を使う)。

-- (instance SingleVarModel KNNRegressor / stMu1 / stSd1 / unstdY /
--  SingleVarModel (StandardizedModel m) / Plottable (StandardizedModel m) は
--  Hanalyze.Plot.Wrappers へ移動 — Phase 71.7)

-- ===========================================================================
-- 混合効果モデル (random effects) — Phase 52 D3
--
-- 'GLMMResultRE' (Phase 48 の vector random effects: random intercept + slope)
-- を caterpillar plot で描く。 各 group の BLUP @b̂_j@ を **値で昇順ソート**し、
-- forest mark (水平棒) で並べる。 0 (= 固定効果からの偏差ゼロ) に参照線を引く。
-- group 間の random effect のばらつき・外れ群を一目で読めるのが GLMM 固有の定番図。
--
-- ★ CI 帯は現状なし (点のみ): 'GLMMResultRE' は per-group の conditional variance
-- も観測数 @n_j@ も格納しておらず (scalar 専用の 'glmmBLUPSE' は 'GLMMResult' 用で
-- 流用不可)、 BLUP の標準誤差を単体から計算できない。 将来 conditional variance を
-- 持たせれば forest の誤差半幅を埋めて帯化できる (forest mark は対称 CI 対応済)。
--
-- 'toPlot'          = random-effect 第 1 列 (通常 intercept) の caterpillar 1 枚。
-- 'diagnosticPlots' = 全 r 列 (intercept + 各 slope) の caterpillar list。
-- ===========================================================================



-- (instance SingleVarModel GPRegModel / Plottable GPRegModel /
--  SingleVarModel GPRegModelN / Plottable GPRegModelN は
--  Hanalyze.Plot.Smooth へ移動 — Phase 71.5)


-- (instance Plottable RegModel / regMethodName / roundTo は
--  Hanalyze.Plot.Wrappers へ移動 — Phase 71.7)

-- (familyObsDist は Hanalyze.Plot.Linear へ移動 — Phase 71.5)

-- (lmDiag / groupedLmDiag / instance Plottable (GroupedFit spec) /
--  renderGrouped / groupedFullrange / renderGroupedWith /
--  instance ColumnSource [(Text, ColData)] は
--  Hanalyze.Plot.Wrappers へ移動 — Phase 71.7)

-- (dataScatterOf は Hanalyze.Plot.Bayes へ移動 — Phase 71.7)

