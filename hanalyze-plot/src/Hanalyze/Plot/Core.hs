-- |
-- Module      : Hanalyze.Plot.Core
-- Description : hgg 連携層の共通基盤 (モデル族非依存のクラス・型・評価核)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: hgg 連携層の __共通基盤__ (= モデル族非依存のクラス・型・評価核)。
--
-- ⚠ 本モジュールは親 'Hanalyze.Plot' と同じく別パッケージ @hanalyze-plot@
-- に属し、 @cabal build --project-file=cabal.project.plot@ で build される。
-- @hgg-core@ に依存するため
-- __upstream hanalyze には cherry-pick しない__。
--
-- ここに集約するもの:
--
--   * 図化能力の最終クラス 'Plottable'、 grid 評価クラス 'SingleVarModel' /
--     @MultiVarModel@、 分類器抽象 'ClassPredict'。
--   * grid 評価の仕様 'ModelSpec' (Semigroup\/Monoid)・確定オプション 'GridOpts'・
--     ブートストラップ素材 'BootKit'、 および @statModel@\/@grid@\/@bandMode@ 等の
--     合成子 (smart ctor)。
--   * grid 評価核 ('renderGrid' \/ 'renderGridMulti' \/ 'bootstrapBands' \/ 'evalFrame'
--     系)・応答曲面核 ('surfaceGrid' \/ 'surfaceOf' 系) と、 複数のモデル族が共有する
--     描画 helper。
--
-- 各モデル族固有の @instance Plottable XxxModel@ 等は親 'Hanalyze.Plot' 側に
-- 残置する (orphan instance を許容: クラスは Core・instance は Plot・型は Wrappers)。
--
-- [English]: The __common foundation__ of the hgg integration layer
-- (= model-family-agnostic classes, types, and the evaluation core).
--
-- ⚠ This module lives in the same separate package @hanalyze-plot@ as
-- its parent 'Hanalyze.Plot', built via
-- @cabal build --project-file=cabal.project.plot@. Because it
-- depends on @hgg-core@, it is __never cherry-picked__ into
-- upstream hanalyze.
--
-- Gathered here:
--
--   * The final plotting-capability class 'Plottable', the grid-evaluation
--     classes 'SingleVarModel' \/ @MultiVarModel@, and the classifier
--     abstraction 'ClassPredict'.
--   * The grid-evaluation spec 'ModelSpec' (Semigroup\/Monoid), its resolved
--     options 'GridOpts', bootstrap material 'BootKit', and the smart
--     constructors @statModel@\/@grid@\/@bandMode@ etc. that build it up.
--   * The grid-evaluation core ('renderGrid' \/ 'renderGridMulti' \/
--     'bootstrapBands' \/ 'evalFrame' family), the response-surface core
--     ('surfaceGrid' \/ 'surfaceOf' family), and the drawing helpers shared
--     across multiple model families.
--
-- Family-specific @instance Plottable XxxModel@ declarations etc. remain in
-- the parent 'Hanalyze.Plot' module (orphan instances are accepted by
-- design: classes live in Core, instances in Plot, types in Wrappers).
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Hanalyze.Plot.Core
  ( -- * Plottable protocol
    Plottable (..)
    -- * ルート1 grid 評価 (ModelSpec)
  , ModelSpec (..)
  , GridOpts (..)
  , BootKit (..)
  , SingleVarModel (..)
  , MultiVarModel (..)
    -- ** 合成子 (smart ctor)
  , statModel
  , grid
  , gridRange
  , bandMode
  , piMethod
  , statColor
  , statFill
  , statLinetype
  , statLinewidth
  , statAlpha
  , statLabel
  , statEquation
  , statR2
  , statLevel
  , holdAt
  , byVar
  , predAt
  , statModelMulti
    -- ** 描画 deco / 凡例 helper
  , goLineDeco
  , goBandDeco
  , labelLegend
  , fitLabelText
    -- ** grid 評価核
  , bootstrapBands
  , renderGrid
  , renderGridMulti
  , marginalizeCurve
  , alongRange
  , evalFrame
  , setAlong
  , isResponseRole
  , holdRole
  , fixedRole
  , clampIdx
  , effectPalette
    -- * 応答曲面 3D 核
  , evalFrame2
  , surfaceGrid
  , chunkRows
  , surfaceOf
  , surfaceOfWith
  , dataScatter3DOf
    -- * 集約 helper (連続列の代表値)
  , meanV
  , medianV
  , modeV
  , modeIdx
  , mostCommon
    -- * 共有描画 helper (複数族が利用)
  , defaultCILevel
  , quantilePalette
  , stepVerts
  , gridCurves
  , importanceBar
  , matCols2
  , classMeansScatter
  , classMeansScatterNamed
  , chainColor
    -- * 分類器抽象
  , ClassPredict (..)
    -- * 回帰診断の可視化 (係数 forest / 実測vs予測)
  , HasObsPred (..)
  , obsVsPred
  , obsPredSpec
  , coefForest
  ) where

import           Control.Applicative   ((<|>))
import           Data.List             (group, maximumBy, sort, transpose)
import           Data.Maybe            (fromMaybe)
import           Data.Ord              (comparing)
import           Data.Word             (Word32)
import qualified Data.Vector           as V
import           System.Random.MWC     (initialize, uniformR)
import           Control.Monad.ST      (runST)
import           Control.Monad         (replicateM)
import           Hanalyze.Model.HBM.Interp (percentileOf)
import           Hanalyze.Model.HBM.Sampling (sampleDist)
import qualified Hanalyze.Model.HBM.Distribution as BD
import qualified Numeric.LinearAlgebra as LA

import           Data.Text             (Text)
import qualified Data.Text             as T

import           Graphics.Hgg.Spec     ( VisualSpec, Layer, layer, inline, inlineCat
                                       , Color (..), fromHex
                                       , scatter, line, band
                                       , shape, MarkShape (..)
                                       , color, colorBy, lineRange, bar
                                       , scaleColorManual, legend
                                       , forest, forestNull
                                       , xLabel, yLabel
                                       , LineType (..)
                                       , linetype, alpha, stroke )
import           Hanalyze.Diagnostics ( CoefRow (..), HasCoefSummary (..) )
import           Graphics.Hgg.Unit     (pt', (*~))
import qualified Graphics.Hgg.ThreeD.Spec  as P3
import           Graphics.Hgg.ThreeD.Types (Point3 (..))
import           Graphics.Hgg.Color        (toCss)

import           Hanalyze.Model.Wrappers
import           Hanalyze.Model.Formula.Frame   (ModelFrame (..), VarRole (..))
import           Hanalyze.Model.LM     (linspace)
import           Numeric               (showFFloat)

-- ===========================================================================
-- Plottable protocol
-- ===========================================================================

-- | [日本語]: 解析オブジェクトを図 (@VisualSpec@) に変換できる能力。
--
--   能力差は中立 protocol ('Hanalyze.Model.Core' の @ResidualModel@ /
--   @PredictiveModel@) 側に持たせ、 ここは「図にできる」 という最終能力のみを表す。
--   [English]: The capability to convert an analysis object into a figure
--   (@VisualSpec@).
--
--   Capability differences live on the neutral protocol side
--   ('Hanalyze.Model.Core''s @ResidualModel@ \/ @PredictiveModel@);
--   this class expresses only the final "can be plotted" capability.
class Plottable m where
  -- | [日本語]: 代表 1 枚の図 (= layer 重畳の主役、 @<>@ で他 layer と合成可)。
  --   [English]: The single representative figure (= the main layer to
  --   overlay onto; composable with other layers via @<>@).
  toPlot          :: m -> VisualSpec

  -- | [日本語]: 診断図の束 (= レポート用)。 既定は代表 1 枚のみ。
  --   [English]: A bundle of diagnostic figures (= for reports). Defaults
  --   to just the single representative figure.
  diagnosticPlots :: m -> [VisualSpec]
  diagnosticPlots m = [toPlot m]

-- ===========================================================================
-- ルート1 grid 評価 (ModelSpec) — Phase 16 §3 C1
--
-- fit 済モデルの回帰曲線・CI 帯を **訓練点ではなく等間隔 grid** で評価して描く。
-- 疎・不均一データで曲線がガタつくのを解消する (散布図の点は従来通り訓練データ)。
-- 'statModel' で 'ModelSpec' を作り、 @<>@ でオプションを足す:
--
-- > df |>> (layer (scatter "x" "y") <> toPlot (statModel m <> grid 200))
--
-- 'ModelSpec' は Monoid。 学習済モデル @m@ はクロージャに閉じ込め、 予測は
-- @toPlot@ (描画時) に grid 評価する (ユーザ直感「m は学習・layer で予測」)。
-- ===========================================================================

-- | [日本語]: grid 評価の確定オプション ('ModelSpec' の Maybe field を既定で埋めたもの)。
--   [English]: The resolved grid-evaluation options ('ModelSpec''s Maybe
--   fields, filled in with defaults).
data GridOpts = GridOpts
  { goN       :: Int                    -- ^ [日本語]: 評価点数 (既定 100)。 [English]: Number of evaluation points (default 100).
  , goRange   :: Maybe (Double, Double) -- ^ [日本語]: 評価範囲 (既定 = 説明変数 min/max)。 [English]: Evaluation range (default = predictor min/max).
  , goLevel   :: Double                 -- ^ [日本語]: CI 水準 (既定 0.95)。 [English]: CI confidence level (default 0.95).
  , goBandMode :: BandMode              -- ^ [日本語]: 帯モード (既定 'BandCI')。 [English]: Band mode (default 'BandCI').
  , goPIMethod :: PIMethod              -- ^ [日本語]: 帯の算出法 (既定 'PIClosedForm')。 [English]: Band computation method (default 'PIClosedForm').
  , goPredAt  :: [Double]               -- ^ [日本語]: 予測点 x のリスト。 [English]: List of prediction points x.
  , goHoldAt  :: HoldAgg                 -- ^ [日本語]: 多変量 effect の他変数固定方式 (既定 Mean)。 [English]: How other predictors are held fixed for a multivariate effect plot (default Mean).
  , goByVar   :: Maybe (Text, [Double])  -- ^ [日本語]: 層別 = 第2変数を複数値で固定。 [English]: Stratification = fixing a second variable at multiple values.
  , goColor   :: Maybe Color             -- ^ [日本語]: 線の固定色 (statColor)。 [English]: Fixed line color (statColor).
  , goFill    :: Maybe Color             -- ^ [日本語]: 帯の塗り色 (statFill)。 [English]: Band fill color (statFill).
  , goLinetype  :: Maybe LineType        -- ^ [日本語]: 線種 (statLinetype)。 [English]: Line type (statLinetype).
  , goLinewidth :: Maybe Double          -- ^ [日本語]: 線幅 = stroke (statLinewidth)。 [English]: Line width = stroke (statLinewidth).
  , goAlpha   :: Maybe Double            -- ^ [日本語]: 帯/線の透明度 (statAlpha)。 [English]: Band/line transparency (statAlpha).
  , goLabel   :: Maybe Text              -- ^ [日本語]: 単線の凡例ラベル (statLabel)。 [English]: Legend label for a single line (statLabel).
  , goShowEq  :: Bool                    -- ^ [日本語]: 回帰式を凡例ラベルに出す (statEquation)。 [English]: Show the regression equation in the legend label (statEquation).
  , goShowR2  :: Bool                    -- ^ [日本語]: R² を凡例ラベルに出す (statR2)。 [English]: Show R² in the legend label (statR2).
  }

-- | [日本語]: grid 上で曲線評価できる単変数モデル。 'statModel' が要求する能力。
--   [English]: A single-variable model that can be evaluated as a curve on a
--   grid. The capability required by 'statModel'.
class SingleVarModel m where
  -- | [日本語]: 生 predictor の範囲 (grid 既定範囲の算出元)。
  --   [English]: The range of the raw predictor (source of the grid's
  --   default range).
  svRange :: m -> (Double, Double)
  -- | [日本語]: 信頼水準と grid x 列から (中心 μ̂, 帯 @(lo, hi)@) を評価する。
  --   band を持たないモデル (GAM/Robust) は 'Nothing'。
  --   [English]: Evaluates (center μ̂, band @(lo, hi)@) from the confidence
  --   level and grid x column. Models without a band (GAM/Robust) return
  --   'Nothing'.
  svGrid  :: m -> Double -> [Double] -> ([Double], Maybe ([Double], [Double]))
  -- | [日本語]: __予測区間__ (PI) 帯 @(lo, hi)@ を評価する。 観測分散 σ̂² を持つモデル
  --   (LM・Gaussian/Identity GLM) のみ実装し、 それ以外は既定の 'Nothing' (= PI 非提供)。
  --   中心 μ̂ は 'svGrid' と共通ゆえここでは帯のみ返す。
  --   [English]: Evaluates the __prediction interval__ (PI) band @(lo,
  --   hi)@. Only implemented for models with an observation variance σ̂²
  --   (LM, Gaussian/Identity GLM); everything else falls back on the
  --   default 'Nothing' (= PI not provided). The center μ̂ is shared with
  --   'svGrid', so only the band is returned here.
  svGridPI :: m -> Double -> [Double] -> Maybe ([Double], [Double])
  svGridPI _ _ _ = Nothing
  -- | [日本語]: 当てはめ係数 @[β₀, β₁]@ と R² (式/R² 凡例注釈用)。 線形で「式」 の意味が
  --   明快なモデル (LM) のみ実装し、 それ以外は既定の 'Nothing' (= 式注釈を出さない)。
  --   GLM は係数が η (リンク) スケールゆえ @y = β₀ + β₁x@ の素朴な式が成り立たず Nothing。
  --   [English]: The fitted coefficients @[β₀, β₁]@ and R² (for the
  --   equation/R² legend annotation). Only implemented for models where
  --   "the equation" has an unambiguous linear meaning (LM); everything
  --   else defaults to 'Nothing' (= no equation annotation shown). GLM
  --   coefficients live on the η (link) scale, so the naive
  --   @y = β₀ + β₁x@ equation does not hold and it returns Nothing.
  svCoefR2 :: m -> Maybe ([Double], Double)
  svCoefR2 _ = Nothing
  -- | [日本語]: ブートストラップ ('piMethod (PIBootstrap …)') 用の素材。 訓練 (x, y)・
  --   再標本化データで refit する関数・新規観測の分布 (GLM family。 'Nothing' = 加法残差)
  --   を束ねて返す。 既定 'Nothing' (= ブートストラップ非対応 → 閉形式へフォールバック)。
  --   closed-form を持たないモデル (非 Gaussian GLM / ロバスト) でも、 これを実装すれば
  --   PI を出せる。
  --   [English]: Material for bootstrapping ('piMethod (PIBootstrap …)'). Bundles
  --   the training (x, y), a function that refits on resampled data, and the
  --   distribution of a new observation (GLM family; 'Nothing' = additive
  --   residual). Defaults to 'Nothing' (= bootstrap unsupported → falls back
  --   to closed form). Even models without a closed form (non-Gaussian GLM /
  --   robust) can offer a PI by implementing this.
  svBootKit :: m -> Maybe (BootKit m)
  svBootKit _ = Nothing

-- | [日本語]: ブートストラップに必要な素材 ('svBootKit' が返す。 内部利用)。
--   [English]: The material required for bootstrapping (returned by
--   'svBootKit'; internal use).
data BootKit m = BootKit
  { bkX       :: [Double]                              -- ^ [日本語]: 訓練 x。 [English]: Training x.
  , bkY       :: [Double]                              -- ^ [日本語]: 訓練 y。 [English]: Training y.
  , bkRefit   :: [Double] -> [Double] -> m             -- ^ [日本語]: 再標本化 (x, y) で refit。 [English]: Refit on a resampled (x, y).
  , bkObsDist :: Maybe (Double -> BD.Distribution Double) -- ^ [日本語]: 新規観測の分布 (GLM)。 Nothing=加法残差。 [English]: The distribution of a new observation (GLM). Nothing = additive residual.
  }

-- | [日本語]: grid 評価の仕様。 'statModel' で生成し @<>@ でオプション合成 (Monoid)。
--   @msRender@ にモデルの grid 評価関数をクロージャで保持する。
--   [English]: The grid-evaluation spec. Created via 'statModel' and
--   composed with options via @<>@ (Monoid). @msRender@ holds the model's
--   grid-evaluation function as a closure.
data ModelSpec = ModelSpec
  { msRender  :: Maybe (GridOpts -> VisualSpec)  -- ^ [日本語]: statModel が設定 (先勝ち)。 [English]: Set by statModel (first wins).
  , msN       :: Maybe Int                       -- ^ [日本語]: grid 点数 (後勝ち)。 [English]: Number of grid points (last wins).
  , msRange   :: Maybe (Double, Double)          -- ^ [日本語]: grid 範囲 (後勝ち)。 [English]: Grid range (last wins).
  , msLevel   :: Maybe Double                    -- ^ [日本語]: CI 水準 (後勝ち)。 [English]: CI confidence level (last wins).
  , msBandMode :: Maybe BandMode                 -- ^ [日本語]: 帯モード (後勝ち、 既定 'BandCI'。
                                                 --   帯 ON/OFF と CI/PI を 'bandMode' 1 本に統合)。
                                                 --   [English]: Band mode (last wins, default 'BandCI'.
                                                 --   'bandMode' unifies band on/off and CI/PI into one
                                                 --   knob).
  , msPIMethod :: Maybe PIMethod                 -- ^ [日本語]: 帯の算出法 (後勝ち、 既定 'PIClosedForm')。 [English]: Band computation method (last wins, default 'PIClosedForm').
  , msPredAt  :: [Double]                        -- ^ [日本語]: 予測点 x (リスト累積 ++)。 [English]: Prediction points x (accumulated via list ++).
  , msHoldAt  :: Maybe HoldAgg                   -- ^ [日本語]: 多変量 effect の固定方式 (後勝ち、 既定 Mean)。 [English]: How other predictors are held fixed for a multivariate effect plot (last wins, default Mean).
  , msByVar   :: Maybe (Text, [Double])          -- ^ [日本語]: 層別変数 (後勝ち)。 [English]: Stratification variable (last wins).
  , msColor   :: Maybe Color                      -- ^ [日本語]: 線の固定色 (後勝ち)。 [English]: Fixed line color (last wins).
  , msFill    :: Maybe Color                      -- ^ [日本語]: 帯の塗り色 (後勝ち)。 [English]: Band fill color (last wins).
  , msLinetype  :: Maybe LineType                 -- ^ [日本語]: 線種 (後勝ち)。 [English]: Line type (last wins).
  , msLinewidth :: Maybe Double                   -- ^ [日本語]: 線幅 = stroke (後勝ち)。 [English]: Line width = stroke (last wins).
  , msAlpha   :: Maybe Double                      -- ^ [日本語]: 帯/線の透明度 (後勝ち)。 [English]: Band/line transparency (last wins).
  , msLabel   :: Maybe Text                         -- ^ [日本語]: 単線の凡例ラベル (後勝ち)。 [English]: Legend label for a single line (last wins).
  , msShowEq  :: Bool                                 -- ^ [日本語]: 回帰式を凡例に出す (Any)。 [English]: Show the regression equation in the legend (Any monoid).
  , msShowR2  :: Bool                                 -- ^ [日本語]: R² を凡例に出す (Any)。 [English]: Show R² in the legend (Any monoid).
  }

instance Semigroup ModelSpec where
  a <> b = ModelSpec
    { msRender  = msRender a <|> msRender b      -- モデルは先勝ち (通常 1 個)
    , msN       = msN b      <|> msN a           -- オプションは後勝ち
    , msRange   = msRange b  <|> msRange a
    , msLevel   = msLevel b  <|> msLevel a
    , msBandMode = msBandMode b <|> msBandMode a  -- 帯モードは後勝ち
    , msPIMethod = msPIMethod b <|> msPIMethod a  -- 算出法も後勝ち
    , msPredAt  = msPredAt a ++ msPredAt b       -- 予測点はリスト累積
    , msHoldAt  = msHoldAt b  <|> msHoldAt a
    , msByVar   = msByVar b   <|> msByVar a
    , msColor   = msColor b     <|> msColor a     -- aes は後勝ち
    , msFill    = msFill b      <|> msFill a
    , msLinetype  = msLinetype b  <|> msLinetype a
    , msLinewidth = msLinewidth b <|> msLinewidth a
    , msAlpha   = msAlpha b     <|> msAlpha a
    , msLabel   = msLabel b     <|> msLabel a
    , msShowEq   = msShowEq a || msShowEq b           -- 注釈はオプトイン (Any)
    , msShowR2   = msShowR2 a || msShowR2 b
    }

instance Monoid ModelSpec where
  mempty = ModelSpec
    { msRender = Nothing, msN = Nothing, msRange = Nothing, msLevel = Nothing
    , msBandMode = Nothing, msPIMethod = Nothing, msPredAt = [], msHoldAt = Nothing, msByVar = Nothing
    , msColor = Nothing, msFill = Nothing, msLinetype = Nothing
    , msLinewidth = Nothing, msAlpha = Nothing, msLabel = Nothing
    , msShowEq = False, msShowR2 = False }

-- | [日本語]: 学習済の単変数モデルから grid 評価 'ModelSpec' を作る (along 不要)。
--   [English]: Builds a grid-evaluation 'ModelSpec' from a fitted
--   single-variable model (no along needed).
statModel :: SingleVarModel m => m -> ModelSpec
statModel m = mempty { msRender = Just (renderGrid m) }

-- | [日本語]: grid 評価点数を指定 (既定 100)。
--   [English]: Specifies the number of grid-evaluation points (default 100).
grid :: Int -> ModelSpec
grid n = mempty { msN = Just n }

-- | [日本語]: grid 評価範囲を指定 (既定 = 説明変数 min/max)。
--   [English]: Specifies the grid-evaluation range (default = predictor
--   min/max).
gridRange :: Double -> Double -> ModelSpec
gridRange lo hi = mempty { msRange = Just (lo, hi) }

-- | [日本語]: 出す帯を 1 つの値で選ぶ (帯 ON/OFF と CI/PI を統合)。 'BandMode' は
--   @BandOff@ (なし) \/ @BandCI@ (既定・信頼区間) \/ @BandPI@ (予測区間) \/ @BandCIPI@
--   (入れ子)。 既定 (未指定) は @BandCI@。 PI 非提供モデルでは PI 系は CI へフォールバック。
--
--   @statModel m \<\> bandMode BandPI@ \/ @… \<\> bandMode BandCIPI@ \/ @… \<\> bandMode BandOff@。
--   [English]: Picks which band to show via a single value (unifies band
--   on/off with CI/PI). 'BandMode' is @BandOff@ (none) \/ @BandCI@ (default;
--   confidence interval) \/ @BandPI@ (prediction interval) \/ @BandCIPI@
--   (nested). The default (unspecified) is @BandCI@. For models that don't
--   provide a PI, PI-based modes fall back to CI.
--
--   @statModel m \<\> bandMode BandPI@ \/ @… \<\> bandMode BandCIPI@ \/ @… \<\> bandMode BandOff@.
bandMode :: BandMode -> ModelSpec
bandMode m = mempty { msBandMode = Just m }

-- | [日本語]: 帯 (CI/PI) の__算出法__を選ぶ。 @bandMode@ が「どの帯を出すか」を選ぶのに対し、
--   @piMethod@ は「どう計算するか」を選ぶ直交軸:
--
--     * @PIClosedForm@   = 閉形式 (Wald / 基底空間 OLS。 __既定__)。
--     * @PIBootstrap seed draws@ = case-resampling ブートストラップ (seed で決定的)。
--       閉形式 CI/PI を持たないモデル (非 Gaussian GLM / ロバスト) でも PI を出せる。
--
--   @statModel m \<\> bandMode BandPI \<\> piMethod (PIBootstrap 42 2000)@。
--   [English]: Chooses the __computation method__ for a band (CI/PI). Where
--   @bandMode@ picks "which band to show," @piMethod@ is the orthogonal axis
--   that picks "how to compute it":
--
--     * @PIClosedForm@ = closed form (Wald \/ basis-space OLS. __default__).
--     * @PIBootstrap seed draws@ = case-resampling bootstrap (deterministic
--       given the seed). Lets even models without a closed-form CI/PI
--       (non-Gaussian GLM \/ robust) produce a PI.
--
--   @statModel m \<\> bandMode BandPI \<\> piMethod (PIBootstrap 42 2000)@.
piMethod :: PIMethod -> ModelSpec
piMethod p = mempty { msPIMethod = Just p }

-- | [日本語]: 回帰線の固定色 (ggplot @geom_smooth(color=)@)。 凡例は付かない (単線命名は
--   'statLabel')。 型安全な 'Color' を受ける (plot-core の 'color' と同じ方針)。
--   @statColor (fromHex "#ff0000")@ \/ @statColor N.red@ \/ @statColor (rgb 255 0 0)@。
--   Text→Color は 'fromHex' に委ねる。
--   [English]: Fixed color for the regression line (ggplot
--   @geom_smooth(color=)@). No legend is added (naming a single line is
--   'statLabel''s job). Takes a type-safe 'Color' (same policy as
--   plot-core's 'color'). @statColor (fromHex "#ff0000")@ \/ @statColor
--   N.red@ \/ @statColor (rgb 255 0 0)@. Text→Color conversion is delegated
--   to 'fromHex'.
statColor :: Color -> ModelSpec
statColor c = mempty { msColor = Just c }

-- | [日本語]: CI 帯の塗り色 (ggplot @geom_smooth(fill=)@)。 型安全な 'Color' を受ける。
--   [English]: Fill color for the CI band (ggplot @geom_smooth(fill=)@).
--   Takes a type-safe 'Color'.
statFill :: Color -> ModelSpec
statFill c = mempty { msFill = Just c }

-- | [日本語]: 回帰線の線種 (ggplot @geom_smooth(linetype=)@)。 'LineType' = 'LtSolid' /
--   'LtDashed' 等。
--   [English]: Line type for the regression line (ggplot
--   @geom_smooth(linetype=)@). 'LineType' = 'LtSolid' \/ 'LtDashed' etc.
statLinetype :: LineType -> ModelSpec
statLinetype lt = mempty { msLinetype = Just lt }

-- | [日本語]: 回帰線の太さ (= stroke 幅。 ggplot @geom_smooth(linewidth=)@)。
--   [English]: Thickness of the regression line (= stroke width; ggplot
--   @geom_smooth(linewidth=)@).
statLinewidth :: Double -> ModelSpec
statLinewidth w = mempty { msLinewidth = Just w }

-- | [日本語]: 帯/線の透明度 (ggplot @geom_smooth(alpha=)@)。 帯に適用 (薄い塗り潰しの ggplot 流)。
--   [English]: Transparency for the band/line (ggplot @geom_smooth(alpha=)@).
--   Applied to the band (following ggplot's convention of a light fill).
statAlpha :: Double -> ModelSpec
statAlpha a = mempty { msAlpha = Just a }

-- | [日本語]: 単線に凡例ラベルを付ける。 1 群カテゴリ (@ColorByCol@) + 'scaleColorManual' で
--   色を固定し凡例エントリを 1 つ出す (固定色 'color' は @hasColorEncoding=False@ で
--   凡例が出ない罠を回避)。 色は 'statColor' があればそれ、 なければ既定パレット先頭。
--   ★モデル比較で各線に名前を付ける用途 (= 群数 1 の @byGroup@ 特殊形)。
--   [English]: Adds a legend label to a single line. Fixes the color via a
--   single-group category (@ColorByCol@) + 'scaleColorManual' and emits one
--   legend entry (avoids the trap where a fixed 'color' has
--   @hasColorEncoding=False@ and no legend appears). Uses 'statColor' if
--   given, otherwise the first color in the default palette. ★Used to name
--   each line when comparing models (= a special case of @byGroup@ with a
--   single group).
statLabel :: Text -> ModelSpec
statLabel lbl = mempty { msLabel = Just lbl }

-- | [日本語]: 回帰式を凡例ラベルに出す (ggplot @ggpubr::stat_regline_equation@ 相当)。
--   @svCoefR2@ を持つモデル (LM) で @y = β₀ + β₁x@ を自動生成し 凡例機構に載せる。
--   明示 'statLabel' があればそちらを優先。 式の出せないモデル (GLM 等) では注釈なし。
--   'statR2' と併用すると @y = … + …x, R² = …@ のように 1 ラベルに連結する。
--   [English]: Shows the regression equation in the legend label (equivalent
--   to ggplot's @ggpubr::stat_regline_equation@). For models with
--   @svCoefR2@ (LM), auto-generates @y = β₀ + β₁x@ and feeds it into the
--   legend mechanism. An explicit 'statLabel' takes precedence. Models that
--   can't produce an equation (GLM etc.) get no annotation. Combined with
--   'statR2', the two are joined into one label as @y = … + …x, R² = …@.
statEquation :: ModelSpec
statEquation = mempty { msShowEq = True }

-- | [日本語]: R² を凡例ラベルに出す (ggplot @ggpubr::stat_cor(aes(label=..rr.label..))@ 相当)。
--   @svCoefR2@ を持つモデル (LM) の R² を @R² = 0.987@ の形で凡例に載せる。
--   [English]: Shows R² in the legend label (equivalent to ggplot's
--   @ggpubr::stat_cor(aes(label=..rr.label..))@). Puts the R² of a model
--   with @svCoefR2@ (LM) into the legend as @R² = 0.987@.
statR2 :: ModelSpec
statR2 = mempty { msShowR2 = True }

-- | [日本語]: CI 水準を指定 (既定 0.95)。
--   [English]: Specifies the CI confidence level (default 0.95).
statLevel :: Double -> ModelSpec
statLevel l = mempty { msLevel = Just l }

-- | [日本語]: 多変量 effect で along 以外の説明変数の固定方式を指定 (既定 'Mean')。
--   [English]: Specifies how predictors other than along are held fixed in
--   a multivariate effect plot (default 'Mean').
holdAt :: HoldAgg -> ModelSpec
holdAt h = mempty { msHoldAt = Just h }

-- | [日本語]: 層別 = 第2変数 @v@ を複数値 @vals@ で固定し、 値ごとに 1 曲線を色分け重畳する
--   (R @ggpredict@ terms 第2項相当)。 多変量モデル ('statModelMulti') 専用。
--   [English]: Stratification: fixes a second variable @v@ at multiple
--   values @vals@ and overlays one color-coded curve per value (equivalent
--   to the second term of R's @ggpredict@ terms). For multivariate models
--   ('statModelMulti') only.
byVar :: Text -> [Double] -> ModelSpec
byVar v vals = mempty { msByVar = Just (v, vals) }

-- | [日本語]: 予測点を 1 つ足す。 @<>@ でリスト累積 → @… <> predAt 1 <> predAt 3@ で複数点。
--   各点は μ̂ (scatter) + CI 区間 [lo, hi] (lineRange) で描かれる (band を持たない GAM/
--   Robust は μ̂ 点のみ)。 単変数モデル前提 (多変量 effect は statModelMulti で対応)。
--   [English]: Adds one prediction point. Accumulated via @<>@ → @… <>
--   predAt 1 <> predAt 3@ for multiple points. Each point is drawn as μ̂
--   (scatter) + a CI interval [lo, hi] (lineRange); models without a band
--   (GAM/Robust) get just the μ̂ point. Assumes a single-variable model
--   (multivariate effects are handled by statModelMulti).
predAt :: Double -> ModelSpec
predAt x = mempty { msPredAt = [x] }

-- | [日本語]: 線レイヤへ aes (色・線種・太さ) を適用。 色の決定順は
--   (1) 群色 @mCol@ (byVar) → 'color'、 (2) 'statLabel' (@goLabel@) → 1 群 'colorBy'
--   (凡例を出すため・@n@ 点ぶんのカテゴリ列)、 (3) 'statColor' → 'color'。
--   線種・太さは色と独立に適用。 @n@ = grid 点数 (label カテゴリ列の長さ)。
--   [English]: Applies aes (color, line type, width) to a line layer. Color
--   is resolved in order: (1) the group color @mCol@ (byVar) → 'color', (2)
--   'statLabel' (@goLabel@) → a single-group 'colorBy' (to show a legend; a
--   category column of length @n@), (3) 'statColor' → 'color'. Line type and
--   width are applied independently of color. @n@ = number of grid points
--   (the length of the label category column).
goLineDeco :: GridOpts -> Maybe Color -> Int -> Layer -> Layer
goLineDeco o mCol n l =
  let colorL = case (mCol, goLabel o) of
        (Just c, _)         -> color c                                   -- 群色優先
        (Nothing, Just lbl) -> colorBy (inlineCat (replicate n lbl))      -- statLabel: ColorByCol で凡例
        (Nothing, Nothing)  -> maybe mempty color (goColor o)            -- statColor or 無色
  in l <> colorL
       <> maybe mempty linetype (goLinetype o)
       <> maybe mempty (\lw -> stroke (lw *~ pt')) (goLinewidth o)

-- | [日本語]: 帯レイヤへ fill 色・透明度を適用。 群色 @mCol@ があれば fill は群色を優先
--   ('statFill' で上書き不可)。
--   [English]: Applies fill color and transparency to a band layer. If a
--   group color @mCol@ is present, fill prefers the group color (cannot be
--   overridden by 'statFill').
goBandDeco :: GridOpts -> Maybe Color -> Layer -> Layer
goBandDeco o mCol b =
  b <> maybe mempty color (mCol <|> goFill o)
    <> maybe mempty alpha       (goAlpha o)

-- | [日本語]: 'statLabel' があれば @scaleColorManual@ で色を固定し @legend@ を出す
--   @VisualSpec@。 色は 'statColor' (@goColor@) 優先・なければ既定パレット先頭。
--   ラベル無しは空。
--   [English]: A @VisualSpec@ that, when 'statLabel' is present, fixes the
--   color via @scaleColorManual@ and shows a @legend@. Color prefers
--   'statColor' (@goColor@), falling back to the first color in the default
--   palette. Empty when there is no label.
labelLegend :: GridOpts -> VisualSpec
labelLegend o = case goLabel o of
  Just lbl -> scaleColorManual [(lbl, maybe (head effectPalette) toCss (goColor o))] <> legend
  Nothing  -> mempty

-- | [日本語]: 式/R² 凡例ラベル文字列を組む。 @showEq@ で @y = β₀ + β₁x@、 @showR2@ で
--   @R² = 0.987@ を入れ、 両方なら @", "@ で連結する。 係数は単回帰 @[β₀, β₁]@ を想定
--   (β₁ の符号で @+@/@-@ を切替)。 どちらの flag も立っていなければ 'Nothing'。
--   [English]: Builds the equation/R² legend label string. Inserts @y = β₀ +
--   β₁x@ when @showEq@ is set and @R² = 0.987@ when @showR2@ is set, joining
--   the two with @", "@ if both are present. Coefficients are assumed to be
--   a simple regression @[β₀, β₁]@ (the sign of β₁ switches @+@\/@-@).
--   Returns 'Nothing' if neither flag is set.
fitLabelText :: Bool -> Bool -> [Double] -> Double -> Maybe Text
fitLabelText showEq showR2 coefs r2 =
  let f3 x = T.pack (showFFloat (Just 3) x "")          -- 小数 3 桁固定
      eqPart = case coefs of
        (b0 : b1 : _) ->
          let sgn = if b1 < 0 then " − " else " + "
          in "y = " <> f3 b0 <> sgn <> f3 (abs b1) <> "x"
        [b0]          -> "y = " <> f3 b0
        _             -> "y = ?"
      r2Part = "R² = " <> f3 r2
      parts  = [ eqPart | showEq ] ++ [ r2Part | showR2 ]
  in if null parts then Nothing else Just (T.intercalate ", " parts)

-- | [日本語]: case-resampling ブートストラップで grid 上の CI / PI 帯を計算する。
--   訓練 (x, y) を seed 付きで再標本化 → 'bkRefit' で refit → 'svGrid' で grid μ を予測、
--   を @draws@ 回。 CI = μ_b の分位点 (係数の不確実性)。 PI = 新規観測 y* の分位点
--   (加法残差 'bkObsDist'=Nothing、 または Family(μ) からの parametric ドロー)。 seed 純粋
--   (runST + mwc・同 seed でビット同一)。 戻り = (CI (lo,hi), PI (lo,hi))。
--   [English]: Computes CI / PI bands on the grid via case-resampling
--   bootstrap. Resamples the training (x, y) with a seed → refits via
--   'bkRefit' → predicts grid μ via 'svGrid', repeated @draws@ times. CI =
--   quantiles of μ_b (coefficient uncertainty). PI = quantiles of a new
--   observation y* (additive residual when 'bkObsDist'=Nothing, or a
--   parametric draw from Family(μ) otherwise). Pure given the seed (runST +
--   mwc; bit-identical for the same seed). Returns (CI (lo,hi), PI (lo,hi)).
bootstrapBands :: SingleVarModel m
               => m -> BootKit m -> Word32 -> Int -> Double -> [Double]
               -> (([Double], [Double]), ([Double], [Double]))
bootstrapBands m kit seed draws level gxs =
  let xs    = V.fromList (bkX kit)
      ys    = V.fromList (bkY kit)
      n     = V.length xs
      ng    = length gxs
      a2    = (1 - level) / 2
      resid = V.fromList (zipWith (-) (bkY kit) (fst (svGrid m level (bkX kit))))
      paths = runST $ do
        gen <- initialize (V.singleton seed)
        replicateM draws $ do
          idx <- replicateM n (uniformR (0, n - 1) gen)
          let xs' = [ xs V.! i | i <- idx ]
              ys' = [ ys V.! i | i <- idx ]
              muB = fst (svGrid (bkRefit kit xs' ys') level gxs)
          pis <- case bkObsDist kit of
            Just toDist -> mapM (\mu -> sampleDist (toDist mu) gen) muB
            Nothing     -> mapM (\mu -> do j <- uniformR (0, n - 1) gen
                                           pure (mu + resid V.! j)) muB
          pure (muB, pis)
      muT = transpose (map fst paths)   -- ng × draws
      piT = transpose (map snd paths)
      q lo xss = map (percentileOf lo) xss
  in if n < 2 || ng == 0
       then (([], []), ([], []))
       else ( (q a2 muT, q (1 - a2) muT), (q a2 piT, q (1 - a2) piT) )

-- | [日本語]: grid 評価して曲線 (+ 帯) + 予測点の @VisualSpec@ を組む。 'statModel' がクロージャ化。
--   帯がある場合は @band@ を先に置き @line@ (μ̂ 曲線) を上に重ねる。 予測点 (goPredAt) は
--   CI 区間を @lineRange@ (縦線 [lo,hi]) + μ̂ を @scatter@ で重ね、 μ̂ が区間内のどこにあるか
--   (非対称な GLM 帯でも) 忠実に示す。
--   [English]: Grid-evaluates the model and builds the @VisualSpec@ for the
--   curve (+ band) + prediction points. Closed over by 'statModel'. When a
--   band is present, @band@ is drawn first and @line@ (the μ̂ curve) is
--   layered on top. Prediction points (goPredAt) overlay the CI interval as
--   @lineRange@ (a vertical segment [lo,hi]) with μ̂ as a @scatter@ point,
--   faithfully showing where μ̂ sits within the interval (even for
--   asymmetric GLM bands).
renderGrid :: SingleVarModel m => m -> GridOpts -> VisualSpec
renderGrid m opts0 =
  -- A8: statEquation/statR2 が立っていれば svCoefR2 から式/R² 文字列を作り、
  -- A3 と同じ凡例経路 (goLabel) に流す。 明示 statLabel が優先 (上書きしない)。
  let autoLabel = case (goShowEq opts0 || goShowR2 opts0, svCoefR2 m) of
        (True, Just (coefs, r2)) -> fitLabelText (goShowEq opts0) (goShowR2 opts0) coefs r2
        _                        -> Nothing
      opts = case goLabel opts0 of
        Just _  -> opts0                              -- 明示ラベル優先
        Nothing -> opts0 { goLabel = autoLabel }
      (lo0, hi0) = svRange m
      (lo, hi)   = fromMaybe (lo0, hi0) (goRange opts)
      n          = max 2 (goN opts)
      gxs        = linspace lo hi n
      (mu, mbCIcf) = svGrid m (goLevel opts) gxs
      -- 帯の算出法 (Phase 70.H): 既定 closed-form、 PIBootstrap で case-resampling。
      -- bootstrap は CI/PI を両方その場で計算 ('svBootKit' を持つモデルのみ。 無ければ
      -- closed-form へフォールバック)。 中心曲線 mu は元の当てはめのまま。
      (mbCI, mbPI) = case goPIMethod opts of
        PIBootstrap seed draws
          | Just kit <- svBootKit m ->
              let (ci, pii) = bootstrapBands m kit seed draws (goLevel opts) gxs
              in (Just ci, Just pii)
        _ -> (mbCIcf, svGridPI m (goLevel opts) gxs)
      -- 帯モードで CI/PI/両方/なしを描く (Phase 70.F)。 PI 非提供は CI へフォールバック。
      lineL    = layer (goLineDeco opts Nothing n (line (inline gxs) (inline mu)))
      bandL deco mb = case mb of
        Just (los, his) -> layer (deco (band (inline gxs) (inline los) (inline his)))
        Nothing         -> mempty
      ciDeco = goBandDeco opts Nothing
      -- 入れ子時の PI 帯は薄め (CI が内側で見えるように)。
      piA    = maybe 0.10 (* 0.5) (goAlpha opts)
      piDeco = goBandDeco (opts { goAlpha = Just piA }) Nothing
      curve = case goBandMode opts of
        BandOff  -> lineL
        BandCI   -> bandL ciDeco mbCI <> lineL
        BandPI   -> case mbPI of
                      Just _  -> bandL ciDeco mbPI <> lineL   -- PI 単独 (通常の濃さ)
                      Nothing -> bandL ciDeco mbCI <> lineL   -- PI 非提供 → CI
        BandCIPI -> case mbPI of
                      Just _  -> bandL piDeco mbPI            -- 外: PI 薄 (下)
                              <> bandL ciDeco mbCI            -- 内: CI 濃 (上)
                              <> lineL
                      Nothing -> bandL ciDeco mbCI <> lineL   -- PI 非提供 → CI のみ
      pts = goPredAt opts
      predLayers
        | null pts  = mempty
        | otherwise =
            let (pmu, pmb) = svGrid m (goLevel opts) pts
            in case pmb of
                 Just (plos, phis) ->
                   let mids  = zipWith (\l h -> (l + h) / 2) plos phis
                       halfs = zipWith (\l h -> (h - l) / 2) plos phis
                   in layer (lineRange (inline pts) (inline mids) (inline halfs))
                        <> layer (scatter (inline pts) (inline pmu))
                 Nothing -> layer (scatter (inline pts) (inline pmu))
  in curve <> predLayers <> labelLegend opts

-- ★案B: 既存 'Plottable' の @toPlot@ を 'ModelSpec' にも overload (同綴り)。
instance Plottable ModelSpec where
  toPlot ms = case msRender ms of
    Nothing -> mempty   -- モデル未設定 (オプションのみ) は空図。
    Just f  -> f GridOpts
      { goN       = fromMaybe 100 (msN ms)
      , goRange   = msRange ms
      , goLevel   = fromMaybe 0.95 (msLevel ms)
      , goBandMode = fromMaybe BandCI (msBandMode ms)
      , goPIMethod = fromMaybe PIClosedForm (msPIMethod ms)
      , goPredAt  = msPredAt ms
      , goHoldAt  = fromMaybe Mean (msHoldAt ms)
      , goByVar   = msByVar ms
      , goColor     = msColor ms
      , goFill      = msFill ms
      , goLinetype  = msLinetype ms
      , goLinewidth = msLinewidth ms
      , goAlpha     = msAlpha ms
      , goLabel     = msLabel ms
      , goShowEq     = msShowEq ms
      , goShowR2     = msShowR2 ms
      }

-- ===========================================================================
-- 多変量 effect plot (Phase 16 §3 C3)
--
-- 単変数 grid 評価 (C1) を多変量モデルへ一般化する。 along 変数を grid で動かし、
-- 他の説明変数を 'HoldAgg' で固定した「評価点 ModelFrame」 を合成して、 訓練 formula の
-- @designMatrixF@ で評価点設計行列を組み CI を評価する。
--
-- ★評価点 ModelFrame の合成は **DataFrame を経由せず VarRole を直接差し替える**
-- (@designMatrixF@ は 'mfRoles' のみ参照し応答列は使わない = Design.hs:331)。 列構造・
-- 順序が訓練と完全一致するので @confidenceBandAt@ / 'predictGlmMuWithCI' がそのまま使える。
-- 型で単/多変量を分離し ('SingleVarModel' / @MultiVarModel@)、 along 忘れをコンパイル時に弾く。
-- ===========================================================================

-- | [日本語]: along を必須引数に持つ多変量モデル。 'statModelMulti' が要求する能力。
--   [English]: A multivariate model that requires along as a mandatory
--   argument. The capability required by 'statModelMulti'.
class MultiVarModel m where
  -- | [日本語]: 訓練 'ModelFrame' (along の range と他変数の集約元)。
  --   [English]: The training 'ModelFrame' (the source of along's range and
  --   of other variables' aggregate values).
  mvFrame     :: m -> ModelFrame
  -- | [日本語]: 評価点 'ModelFrame' から (中心 μ̂, CI 帯 @(lo, hi)@) を評価する。
  --   設計行列が組めない場合は空 + 'Nothing'。
  --   [English]: Evaluates (center μ̂, CI band @(lo, hi)@) from an
  --   evaluation-point 'ModelFrame'. Returns empty + 'Nothing' if the design
  --   matrix cannot be built.
  mvEvalFrame :: m -> Double -> ModelFrame -> ([Double], Maybe ([Double], [Double]))
  -- | [日本語]: 評価点での予測区間 (PI)。 既定 'Nothing' (PI 非提供)。 closed-form PI を持つ
  --   モデル ('MultiLMModel' = 多変量 OLS) のみ override する (@svGridPI@ と同じ方針)。
  --   [English]: The prediction interval (PI) at the evaluation points.
  --   Defaults to 'Nothing' (PI not provided). Only overridden by models
  --   with a closed-form PI ('MultiLMModel' = multivariate OLS), following
  --   the same policy as @svGridPI@.
  mvEvalFramePI :: m -> Double -> ModelFrame -> Maybe ([Double], [Double])
  mvEvalFramePI _ _ _ = Nothing

-- | [日本語]: 学習済の多変量モデルと along 変数から effect plot の 'ModelSpec' を作る。
--   along は __必須引数__ (型で単/多変量を分離し誤用を弾く)。
--   @df |>> (layer (scatter \"x1\" \"y\") <> toPlot (statModelMulti m (along \"x1\") <> holdAt Median))@。
--   [English]: Builds an effect-plot 'ModelSpec' from a fitted multivariate
--   model and an along variable. along is a __mandatory argument__ (the type
--   separates single/multivariate to catch misuse at compile time).
--   @df |>> (layer (scatter \"x1\" \"y\") <> toPlot (statModelMulti m (along \"x1\") <> holdAt Median))@.
statModelMulti :: MultiVarModel m => m -> AlongSpec -> ModelSpec
statModelMulti m (AlongSpec v) = mempty { msRender = Just (renderGridMulti m v) }

-- | [日本語]: effect plot の @VisualSpec@ を組む。 along を grid で動かし他変数を 'HoldAgg' で固定。
--   byVar があれば第2変数の各値で曲線を色分け重畳する。 'statModelMulti' がクロージャ化。
--   [English]: Builds the effect-plot @VisualSpec@. Sweeps along over the
--   grid while holding other variables fixed via 'HoldAgg'. If byVar is
--   present, overlays one color-coded curve per value of the second
--   variable. Closed over by 'statModelMulti'.
renderGridMulti :: MultiVarModel m => m -> Text -> GridOpts -> VisualSpec
renderGridMulti m alongV opts =
  let mf         = mvFrame m
      (lo0, hi0) = alongRange mf alongV
      (lo, hi)   = fromMaybe (lo0, hi0) (goRange opts)
      n          = max 2 (goN opts)
      gxs        = linspace lo hi n
      level      = goLevel opts
      hold       = goHoldAt opts
      -- 1 曲線分 (override = byVar 固定, mCol = 線色)。
      oneCurve override mCol =
        case hold of
          Marginalize -> marginalizeCurve opts m alongV level gxs override mCol
          _ ->
            let ef         = evalFrame mf alongV hold override gxs
                (mu, mbCI) = mvEvalFrame m level ef
                mbPI       = mvEvalFramePI m level ef
                lineL      = layer (goLineDeco opts mCol n (line (inline gxs) (inline mu)))
                bL deco mb = case mb of
                  Just (los, his) -> layer (deco (band (inline gxs) (inline los) (inline his)))
                  Nothing         -> mempty
                ciDeco = goBandDeco opts mCol
                piA    = maybe 0.10 (* 0.5) (goAlpha opts)
                piDeco = goBandDeco (opts { goAlpha = Just piA }) mCol
                bands  = case goBandMode opts of
                  BandOff  -> mempty
                  BandCI   -> bL ciDeco mbCI
                  BandPI   -> case mbPI of
                                Just _  -> bL ciDeco mbPI
                                Nothing -> bL ciDeco mbCI       -- PI 非提供 → CI
                  BandCIPI -> case mbPI of
                                Just _  -> bL piDeco mbPI <> bL ciDeco mbCI
                                Nothing -> bL ciDeco mbCI       -- PI 非提供 → CI のみ
            in bands <> lineL
  in case goByVar opts of
       Nothing          -> oneCurve [] Nothing <> labelLegend opts
       Just (v2, vals)  ->
         foldMap
           (\(i, val) ->
              let col = fromHex (effectPalette !! (i `mod` length effectPalette))
              in oneCurve [(v2, val)] (Just col))
           (zip [0 :: Int ..] vals)

-- | [日本語]: Marginalize (PDP/AME): 各 grid 点で along=gx に固定し他変数は __観測分布のまま__、
--   μ̂ を全観測行で平均する (band なし・曲線のみ。 全観測行 × grid で重い)。
--   [English]: Marginalize (PDP/AME): at each grid point, fixes along=gx
--   while leaving other variables at __their observed distribution__, and
--   averages μ̂ over all observation rows (no band, curve only; heavy since
--   it's all observation rows × grid).
marginalizeCurve :: MultiVarModel m
                 => GridOpts -> m -> Text -> Double -> [Double] -> [(Text, Double)] -> Maybe Color -> VisualSpec
marginalizeCurve opts m alongV level gxs override mCol =
  let mf   = mvFrame m
      nObs = mfNRows mf
      base = mf { mfRoles = [ (nm, baseRole nm r) | (nm, r) <- mfRoles mf ] }
      baseRole nm r
        | isResponseRole r              = RoleResponse (V.replicate nObs 0)
        | Just fv <- lookup nm override = fixedRole r nObs fv
        | otherwise                     = r                       -- 観測分布のまま
      muAt gx =
        let (mu, _) = mvEvalFrame m level (setAlong base alongV gx)
        in sum mu / fromIntegral (max 1 (length mu))
      mus  = map muAt gxs
  in layer (goLineDeco opts mCol (length gxs) (line (inline gxs) (inline mus)))

-- | [日本語]: along 変数の観測範囲 (effect grid の既定範囲)。 along が連続でなければ退避 @(0,1)@。
--   [English]: The observed range of the along variable (the effect grid's
--   default range). Falls back to @(0,1)@ if along is not continuous.
alongRange :: ModelFrame -> Text -> (Double, Double)
alongRange mf v = case lookup v (mfRoles mf) of
  Just (RoleContinuous xs) | not (V.null xs) -> (V.minimum xs, V.maximum xs)
  _                                          -> (0, 1)

-- | [日本語]: 各説明変数を 'HoldAgg' で固定値の定数列に差し替えた評価点 'ModelFrame' を合成する。
--   along 変数は grid (gxs)、 応答列はダミー (@designMatrixF@ は応答を使わない)。
--   override は byVar 等の明示固定で 'HoldAgg' より優先する。
--   [English]: Composes an evaluation-point 'ModelFrame' by replacing each
--   predictor with a constant column fixed via 'HoldAgg'. The along variable
--   becomes the grid (gxs); the response column is a dummy (@designMatrixF@
--   doesn't use the response). override (explicit fixes like byVar) takes
--   precedence over 'HoldAgg'.
evalFrame :: ModelFrame -> Text -> HoldAgg -> [(Text, Double)] -> [Double] -> ModelFrame
evalFrame mf alongV hold override gxs =
  let n = length gxs
      adjust (nm, role)
        | isResponseRole role           = (nm, RoleResponse (V.replicate n 0))
        | nm == alongV                  = (nm, RoleContinuous (V.fromList gxs))
        | Just fv <- lookup nm override = (nm, fixedRole role n fv)
        | otherwise                     = (nm, holdRole hold n nm role)
  in mf { mfRoles = map adjust (mfRoles mf), mfNRows = n }

-- | [日本語]: frame の along 列だけを定数 gx に差し替える (行数据え置き、 Marginalize 用)。
--   [English]: Replaces only the frame's along column with the constant gx
--   (row count unchanged; for Marginalize).
setAlong :: ModelFrame -> Text -> Double -> ModelFrame
setAlong mf alongV gx =
  let n = mfNRows mf
      adj (nm, role)
        | nm == alongV = (nm, RoleContinuous (V.replicate n gx))
        | otherwise    = (nm, role)
  in mf { mfRoles = map adj (mfRoles mf) }

isResponseRole :: VarRole -> Bool
isResponseRole (RoleResponse _) = True
isResponseRole _                = False

-- | [日本語]: 1 変数を 'HoldAgg' で固定した定数列にする (連続は集約値、 factor は固定水準 index)。
--   factor は Mean\/Median\/Mode\/Fixed すべて最頻水準に振替 (Reference のみ参照=index 0)。
--   [English]: Turns one variable into a constant column fixed via
--   'HoldAgg' (an aggregate value for continuous, a fixed-level index for
--   factor). For factor, Mean\/Median\/Mode\/Fixed all redirect to the most
--   common level (only Reference uses the reference level = index 0).
holdRole :: HoldAgg -> Int -> Text -> VarRole -> VarRole
holdRole hold n nm role = case role of
  RoleContinuous xs ->
    let v = case hold of
              Mean        -> meanV xs
              Median      -> medianV xs
              Mode        -> modeV xs
              Reference   -> meanV xs              -- 連続に参照水準は無し → 平均で代替
              Marginalize -> meanV xs              -- (Marginalize は別経路。 安全側に平均)
              Fixed fm    -> fromMaybe (meanV xs) (lookup nm fm)
    in RoleContinuous (V.replicate n v)
  RoleFactor levels idx ->
    let fixIdx = case hold of
                   Reference -> 0
                   Fixed fm  -> maybe (modeIdx idx) (clampIdx levels . round) (lookup nm fm)
                   _         -> modeIdx idx
    in RoleFactor levels (V.replicate n fixIdx)
  RoleResponse _ -> RoleResponse (V.replicate n 0)

-- | [日本語]: 明示値 (byVar / Fixed override) で 1 変数を定数列にする。
--   [English]: Turns one variable into a constant column using an explicit
--   value (byVar / Fixed override).
fixedRole :: VarRole -> Int -> Double -> VarRole
fixedRole role n fv = case role of
  RoleContinuous _    -> RoleContinuous (V.replicate n fv)
  RoleFactor levels _ -> RoleFactor levels (V.replicate n (clampIdx levels (round fv)))
  RoleResponse _      -> RoleResponse (V.replicate n fv)

clampIdx :: [Text] -> Int -> Int
clampIdx levels i = max 0 (min (length levels - 1) i)

-- | [日本語]: byVar 曲線の固定色パレット (層別の値ごとに 1 色)。
--   [English]: Fixed color palette for byVar curves (one color per
--   stratification value).
effectPalette :: [Text]
effectPalette =
  [ "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2" ]

-- ===========================================================================
-- 応答曲面 3D 直結 — plot Phase 24 A3 (fit 済み多変量モデル → surface)
--
-- JMP Surface Profiler 同型: 2 因子 (v1, v2) を grid で動かし他変数を 'HoldAgg'
-- で固定、 μ̂ を 3D surface (z colormap 既定 ON) で描く。 effect plot
-- ('statModelMulti') の 2 因子版で、 評価核は同じ 'mvEvalFrame'。
-- ===========================================================================

-- | [日本語]: 2 因子 grid + 'HoldAgg' の評価点 frame ('evalFrame' の 2 変数版)。
--   行 = v2 (外側)、 列 = v1 (内側) — 'P3.surface3D' の grid 規約
--   (row = y 方向) に一致させる。
--   [English]: The evaluation-point frame for a two-factor grid + 'HoldAgg'
--   (the two-variable version of 'evalFrame'). Rows = v2 (outer), columns =
--   v1 (inner) — matching 'P3.surface3D''s grid convention (row = y
--   direction).
evalFrame2 :: ModelFrame -> Text -> Text -> HoldAgg -> [Double] -> [Double] -> ModelFrame
evalFrame2 mf v1 v2 hold gxs gys =
  let n   = length gxs * length gys
      x1s = [ gx | _  <- gys, gx <- gxs ]
      x2s = [ gy | gy <- gys, _  <- gxs ]
      adjust (nm, role)
        | isResponseRole role = (nm, RoleResponse (V.replicate n 0))
        | nm == v1            = (nm, RoleContinuous (V.fromList x1s))
        | nm == v2            = (nm, RoleContinuous (V.fromList x2s))
        | otherwise           = (nm, holdRole hold n nm role)
  in mf { mfRoles = map adjust (mfRoles mf), mfNRows = n }

-- | [日本語]: 応答曲面の数値核: @(gxs, gys, grid)@。 @grid !! j !! i = μ̂(gxs!!i, gys!!j)@。
--   [English]: The numerical core of the response surface: @(gxs, gys,
--   grid)@. @grid !! j !! i = μ̂(gxs!!i, gys!!j)@.
surfaceGrid :: MultiVarModel m
            => m -> Text -> Text -> SurfaceOpts -> ([Double], [Double], [[Double]])
surfaceGrid m v1 v2 opts =
  let mf         = mvFrame m
      (xlo, xhi) = fromMaybe (alongRange mf v1) (soXRange opts)
      (ylo, yhi) = fromMaybe (alongRange mf v2) (soYRange opts)
      n          = max 2 (soN opts)
      gxs        = linspace xlo xhi n
      gys        = linspace ylo yhi n
      ef         = evalFrame2 mf v1 v2 (soHoldAt opts) gxs gys
      (mu, _)    = mvEvalFrame m 0.95 ef
  in (gxs, gys, chunkRows n mu)

chunkRows :: Int -> [a] -> [[a]]
chunkRows k = go
  where go [] = []
        go xs = let (h, t) = splitAt k xs in h : go t

-- | [日本語]: fit 済み多変量モデル → 3D 応答曲面 (z colormap 既定 ON・colorbar 自動)。
--   @saveSVG3D path (surfaceOf m "x1" "x2" <> dataScatter3DOf m "x1" "x2")@。
--   [English]: Fitted multivariate model → 3D response surface (z colormap
--   on by default, colorbar automatic).
--   @saveSVG3D path (surfaceOf m "x1" "x2" <> dataScatter3DOf m "x1" "x2")@.
surfaceOf :: MultiVarModel m => m -> Text -> Text -> P3.VisualSpec3D
surfaceOf m v1 v2 = surfaceOfWith m v1 v2 defaultSurfaceOpts

-- | [日本語]: オプション付き ('SurfaceOpts': grid 点数・hold・範囲)。
--   [English]: The variant with options ('SurfaceOpts': grid point count,
--   hold, range).
surfaceOfWith :: MultiVarModel m => m -> Text -> Text -> SurfaceOpts -> P3.VisualSpec3D
surfaceOfWith m v1 v2 opts =
  let (gxs, gys, grid') = surfaceGrid m v1 v2 opts
  in P3.layer3D ( P3.surface3DGrid grid'
               <> P3.xRange3D (head gxs, last gxs)
               <> P3.yRange3D (head gys, last gys)
               <> P3.colormap3D )

-- | [日本語]: 実測点の 3D overlay: 訓練データの @(v1, v2, y)@ を scatter3D で重畳。
--   [English]: A 3D overlay of the observed points: overlays the training
--   data's @(v1, v2, y)@ via scatter3D.
dataScatter3DOf :: MultiVarModel m => m -> Text -> Text -> P3.VisualSpec3D
dataScatter3DOf m v1 v2 =
  let mf = mvFrame m
      contOf nm = case lookup nm (mfRoles mf) of
        Just (RoleContinuous xs) -> V.toList xs
        _                        -> []
      ys = case [ v | (_, RoleResponse v) <- mfRoles mf ] of
        (v : _) -> V.toList v
        []      -> []
      pts = zipWith3 Point3 (contOf v1) (contOf v2) ys
  in P3.layer3D (P3.scatter3DPoints pts <> P3.color3D (fromHex "#d62728") <> P3.size3D 4)

meanV :: V.Vector Double -> Double
meanV xs | V.null xs = 0
         | otherwise = V.sum xs / fromIntegral (V.length xs)

medianV :: V.Vector Double -> Double
medianV xs
  | null ys   = 0
  | odd k     = ys !! (k `div` 2)
  | otherwise = (ys !! (k `div` 2 - 1) + ys !! (k `div` 2)) / 2
  where ys = sort (V.toList xs)
        k  = length ys

-- | [日本語]: 連続列の最頻 (観測値の完全一致でグループ化。 繰り返しのない真の連続では任意)。
--   [English]: The mode of a continuous column (grouped by exact value
--   match; arbitrary for genuinely continuous data with no repeats).
modeV :: V.Vector Double -> Double
modeV xs | V.null xs = 0
         | otherwise = mostCommon (V.toList xs)

-- | [日本語]: factor の最頻水準 index。
--   [English]: The index of the most common factor level.
modeIdx :: V.Vector Int -> Int
modeIdx idx | V.null idx = 0
            | otherwise  = mostCommon (V.toList idx)

mostCommon :: Ord a => [a] -> a
mostCommon = fst . maximumBy (comparing snd)
           . map (\g -> (head g, length g)) . group . sort

-- ===========================================================================
-- 共有描画 helper (複数のモデル族が利用)
-- ===========================================================================

-- | [日本語]: CI band の既定 level (95%)。
--   [English]: The default level for a CI band (95%).
defaultCILevel :: Double
defaultCILevel = 0.95

-- | [日本語]: 分位線の色パレット (τ 昇順に割当て。 必要数を循環)。
--   [English]: Color palette for quantile lines (assigned in ascending τ
--   order; cycles if more are needed).
quantilePalette :: [T.Text]
quantilePalette =
  [ "#4575b4", "#d73027", "#1a9850", "#984ea3", "#ff7f00", "#377eb8" ]

-- | [日本語]: 階段関数の頂点列を作る。 開始値 @s0@ (= t=0 での値) から、 各 @(tᵢ, sᵢ)@ について
--   直前の高さで @tᵢ@ まで水平に来てから @sᵢ@ に垂直に跳ぶ 2 頂点を出す。
--   [English]: Builds the vertex list of a step function. Starting from
--   @s0@ (= the value at t=0), for each @(tᵢ, sᵢ)@ emits two vertices: a
--   horizontal run to @tᵢ@ at the previous height, then a vertical jump to
--   @sᵢ@.
stepVerts :: Double -> [(Double, Double)] -> [(Double, Double)]
stepVerts s0 pts = (0, s0) : go s0 pts
  where
    go _    []            = []
    go prev ((t, s) : rest) = (t, prev) : (t, s) : go s rest

-- | [日本語]: grid index を x として複数曲線を色分け重畳する内部 helper。
--   [English]: An internal helper that overlays multiple color-coded curves
--   using the grid index as x.
gridCurves :: [(Text, [Double])] -> VisualSpec
gridCurves named =
  let mkLine (lbl, ys) =
        let xs = [ fromIntegral i | i <- [1 .. length ys] ] :: [Double]
        in layer ( line (inline xs) (inline ys)
                 <> colorBy (inlineCat (replicate (length ys) lbl)) )
  in mconcat (map mkLine named)

-- | [日本語]: 特徴重要度 → bar layer ("f1", "f2", … をカテゴリ軸に・値=重要度)。
--   [English]: Feature importances → bar layer ("f1", "f2", … as the
--   category axis; value = importance).
importanceBar :: [Double] -> VisualSpec
importanceBar imps =
  let labels = [ "f" <> T.pack (show k) | k <- [1 .. length imps] ]
  in layer (bar (inlineCat labels) (inline imps))

-- | [日本語]: 行列の第 @i@/@j@ 列を (xs, ys) として取り出す (列不足は 0 埋め)。
--   [English]: Extracts a matrix's @i@-th\/@j@-th columns as (xs, ys)
--   (missing columns are zero-filled).
matCols2 :: LA.Matrix Double -> Int -> Int -> ([Double], [Double])
matCols2 m i j =
  let cols = LA.toColumns m
      colAt k = if k < length cols then LA.toList (cols !! k) else replicate (LA.rows m) 0
  in (colAt i, colAt j)

-- | [日本語]: クラス代表点 (平均) をクラス色 ✚ で散布する (第 0/1 特徴)。 Discriminant /
--   NaiveBayes(Gaussian) の data-free 代表図。
--   [English]: Scatters each class's representative point (mean) as a
--   class-colored ✚ (features 0/1). A data-free representative figure for
--   Discriminant \/ NaiveBayes(Gaussian).
classMeansScatter :: [[Double]] -> [Int] -> VisualSpec
classMeansScatter rows cids = classMeansScatterNamed rows cids []

-- | [日本語]: 'classMeansScatter' の __クラス名つき__版。 @names@ があれば凡例をクラス名 (levels)
--   に、 無ければ整数へフォールバック (@names !! k@・範囲外は show)。 df|-> 経路が
--   levels を載せた分類モデルの代表図で使う。
--   [English]: The __class-named__ variant of 'classMeansScatter'. If
--   @names@ is present, the legend uses the class names (levels); otherwise
--   falls back to integers (@names !! k@; out-of-range uses show). Used by
--   the representative figure of classification models where the df|->
--   path attaches levels.
classMeansScatterNamed :: [[Double]] -> [Int] -> [Text] -> VisualSpec
classMeansScatterNamed rows cids names
  | null rows = mempty
  | otherwise =
      let xs   = [ if not (null r) then head r else 0 | r <- rows ]
          ys   = [ if length r >= 2 then r !! 1 else 0 | r <- rows ]
          nameOf k | k >= 0 && k < length names = names !! k
                   | otherwise                  = T.pack (show k)
          labs = map nameOf cids
      in layer ( scatter (inline xs) (inline ys)
               <> colorBy (inlineCat labs)
               <> shape MShCross )

-- | [日本語]: chain index → 色 (effectPalette を巡回)。
--   [English]: chain index → color (cycles through effectPalette).
chainColor :: Int -> Text
chainColor k = effectPalette !! (k `mod` length effectPalette)

-- ===========================================================================
-- 分類器抽象 (Discriminant / NaiveBayes / KNN 共通) — Phase 68 A3
-- ===========================================================================

-- | [日本語]: 学習済分類器を評価点行列で走らせ、 各行の予測クラスを返す共通インターフェース。
--   (@decisionBoundaryOf@ / @confusionOf@ が分類器種に依らず動くための薄い抽象)。
--   [English]: A common interface that runs a fitted classifier over an
--   evaluation-point matrix and returns each row's predicted class. (A thin
--   abstraction letting @decisionBoundaryOf@ \/ @confusionOf@ work
--   regardless of classifier type.)
class ClassPredict c where
  predictClasses :: c -> LA.Matrix Double -> [Int]
  -- | [日本語]: クラス番号 0..K-1 に対応する __クラス名 (levels)__。 高レベル @df |->@ 経路が
  --   fit 時に載せる (factor 列なら levels 名・数値列なら数値)。 既定は空 = 名前を
  --   持たないモデル (@confusionOf@ 等は空なら整数ラベルにフォールバック)。
  --   [English]: The __class names (levels)__ corresponding to class
  --   numbers 0..K-1. Attached by the high-level @df |->@ path at fit time
  --   (level names for a factor column, numbers for a numeric column).
  --   Defaults to empty = a model with no names (@confusionOf@ etc. fall
  --   back to integer labels when empty).
  classNamesOf :: c -> [Text]
  classNamesOf _ = []

-- ===========================================================================
-- 回帰診断の可視化 (係数 forest / 実測vs予測) — Phase 72.4/72.5
--
-- 係数表 (@coefSummary@・'Hanalyze.Diagnostics') と各モデルの実測/予測ペアを
-- 図に落とす薄い玄関。 数値層 (係数統計・予測) は別パッケージに依存しない
-- 'Diagnostics' / 各 fit が持ち、 ここ (別パッケージ hanalyze-plot 側) では
-- @VisualSpec@ 化だけを担う。
-- ===========================================================================

-- | [日本語]: fit 済モデルから (実測値, 予測値) の対を取り出せる能力。 実測値は
--   @fitted + residual@ で復元する (回帰一般で成り立つ)。 instance は各モデル族の
--   'Plottable' と同じ Plot.* 側に置く (orphan・クラス=Core / instance=族 module)。
--   [English]: The capability to extract (observed, predicted) pairs from a
--   fitted model. The observed value is reconstructed as @fitted +
--   residual@ (holds for regression in general). Instances live on the same
--   Plot.* side as each model family's 'Plottable' (orphan instances; class
--   in Core, instance in the family module).
class HasObsPred m where
  -- | [日本語]: @(observed, predicted)@。 長さは観測数 n で一致する。
  --   [English]: @(observed, predicted)@. Both have length equal to the
  --   number of observations n.
  obsPredPairs :: m -> ([Double], [Double])

-- | [日本語]: 実測 vs 予測プロット。 x=実測値・y=予測値の散布に @y = x@ の参照線 (灰の破線) を
--   重ねる。 点が参照線に近いほど当てはまりが良い (残差が小さい)。
--   [English]: Observed-vs-predicted plot. Overlays a @y = x@ reference line
--   (gray dashed) on a scatter of x=observed, y=predicted. The closer the
--   points are to the reference line, the better the fit (smaller
--   residuals).
obsVsPred :: HasObsPred m => m -> VisualSpec
obsVsPred m = let (obs, prd) = obsPredPairs m in obsPredSpec obs prd

-- | [日本語]: (実測, 予測) のリストから実測 vs 予測 spec を組む。 'obsVsPred' の純データ版
--   (テスト・任意のペアからの作図に再利用)。 空入力は空図。
--   [English]: Builds an observed-vs-predicted spec from lists of
--   (observed, predicted). The pure-data variant of 'obsVsPred' (reusable
--   for tests or plotting arbitrary pairs). Empty input yields an empty
--   figure.
obsPredSpec :: [Double] -> [Double] -> VisualSpec
obsPredSpec obs prd
  | null obs  = mempty
  | otherwise =
      let lo = minimum (obs ++ prd)
          hi = maximum (obs ++ prd)
      in  layer ( line (inline [lo, hi]) (inline [lo, hi])
                <> linetype LtDashed
                <> color (fromHex "#888888") )
       <> layer (scatter (inline obs) (inline prd))
       <> xLabel "observed"
       <> yLabel "predicted"

-- | [日本語]: 係数 forest plot。 各係数の点推定 ('crEstimate') を中心、 95% CI ('crCI95') の
--   半幅を誤差バーとして 1 行ずつ水平に並べ、 0 (= 効果なし) に参照線を引く。 解析
--   Wald CI (@coefSummary@) を持つ線形系で使う (CI は左右対称なので半幅で表せる)。
--   bootstrap 由来の非対称 CI を図にしたい場合は @coefSummaryBoot@ の行から個別に組む。
--   [English]: Coefficient forest plot. Lays out each coefficient's point
--   estimate ('crEstimate') as the center, one row per coefficient, with the
--   half-width of the 95% CI ('crCI95') as the error bar, and draws a
--   reference line at 0 (= no effect). Used for linear systems with an
--   analytic Wald CI (@coefSummary@) (since the CI is symmetric and can be
--   expressed as a half-width). To plot asymmetric bootstrap-derived CIs,
--   build the figure manually from 'coefSummaryBoot''s rows instead.
coefForest :: HasCoefSummary m => m -> VisualSpec
coefForest m =
  let rows  = coefSummary m
      names = [ crTerm r | r <- rows ]
      ests  = [ crEstimate r | r <- rows ]
      errs  = [ (hi - lo) / 2 | r <- rows, let (lo, hi) = crCI95 r ]
  in if null rows
       then mempty
       else layer (forest (inlineCat names) (inline ests) (inline errs) <> forestNull 0)
