-- |
-- Module      : Hanalyze.Plot.Core
-- Description : hgg 連携層の共通基盤 (モデル族非依存のクラス・型・評価核)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- hgg 連携層の **共通基盤** (= モデル族非依存のクラス・型・評価核)。
--
-- ⚠ 本モジュールは親 'Hanalyze.Plot' と同じ cabal flag @plot-integration@
-- (既定 off) を on にしたときのみ build される。 @hgg-core@ に依存するため
-- **upstream hanalyze には cherry-pick しない**。
--
-- ここに集約するもの (Phase 71.4 確定):
--
--   * 図化能力の最終クラス 'Plottable'、 grid 評価クラス 'SingleVarModel' /
--     'MultiVarModel'、 分類器抽象 'ClassPredict'。
--   * grid 評価の仕様 'ModelSpec' (Semigroup\/Monoid)・確定オプション 'GridOpts'・
--     ブートストラップ素材 'BootKit'、 および @statModel@\/@grid@\/@bandMode@ 等の
--     合成子 (smart ctor)。
--   * grid 評価核 ('renderGrid' \/ 'renderGridMulti' \/ 'bootstrapBands' \/ 'evalFrame'
--     系)・応答曲面核 ('surfaceGrid' \/ 'surfaceOf' 系) と、 複数のモデル族が共有する
--     描画 helper。
--
-- 各モデル族固有の @instance Plottable XxxModel@ 等は親 'Hanalyze.Plot' 側に
-- 残置する (orphan instance を許容: クラスは Core・instance は Plot・型は Wrappers)。
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
    -- * 回帰診断の可視化 (係数 forest / 実測vs予測) — Phase 72.4/72.5
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

-- | 解析オブジェクトを図 ('VisualSpec') に変換できる能力。
--
-- 能力差は中立 protocol ('Hanalyze.Model.Core' の 'ResidualModel' /
-- 'PredictiveModel') 側に持たせ、 ここは「図にできる」 という最終能力のみを表す。
class Plottable m where
  -- | 代表 1 枚の図 (= layer 重畳の主役、 @<>@ で他 layer と合成可)。
  toPlot          :: m -> VisualSpec

  -- | 診断図の束 (= レポート用)。 既定は代表 1 枚のみ。
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
-- 'toPlot' (描画時) に grid 評価する (ユーザ直感「m は学習・layer で予測」)。
-- ===========================================================================

-- | grid 評価の確定オプション ('ModelSpec' の Maybe field を既定で埋めたもの)。
data GridOpts = GridOpts
  { goN       :: Int                    -- ^ 評価点数 (既定 100)。
  , goRange   :: Maybe (Double, Double) -- ^ 評価範囲 (既定 = 説明変数 min/max)。
  , goLevel   :: Double                 -- ^ CI 水準 (既定 0.95)。
  , goBandMode :: BandMode              -- ^ 帯モード (既定 'BandCI'。 Phase 70.F)。
  , goPIMethod :: PIMethod              -- ^ 帯の算出法 (既定 'PIClosedForm'。 Phase 70.H)。
  , goPredAt  :: [Double]               -- ^ 予測点 x のリスト (C2)。
  , goHoldAt  :: HoldAgg                 -- ^ 多変量 effect の他変数固定方式 (既定 Mean, C3)。
  , goByVar   :: Maybe (Text, [Double])  -- ^ 層別 = 第2変数を複数値で固定 (C3)。
  , goColor   :: Maybe Color             -- ^ 線の固定色 (statColor, A2)。
  , goFill    :: Maybe Color             -- ^ 帯の塗り色 (statFill, A2)。
  , goLinetype  :: Maybe LineType        -- ^ 線種 (statLinetype, A2)。
  , goLinewidth :: Maybe Double          -- ^ 線幅 = stroke (statLinewidth, A2)。
  , goAlpha   :: Maybe Double            -- ^ 帯/線の透明度 (statAlpha, A2)。
  , goLabel   :: Maybe Text              -- ^ 単線の凡例ラベル (statLabel, A3)。
  , goShowEq  :: Bool                    -- ^ 回帰式を凡例ラベルに出す (statEquation, A8)。
  , goShowR2  :: Bool                    -- ^ R² を凡例ラベルに出す (statR2, A8)。
  }

-- | grid 上で曲線評価できる単変数モデル。 'statModel' が要求する能力。
class SingleVarModel m where
  -- | 生 predictor の範囲 (grid 既定範囲の算出元)。
  svRange :: m -> (Double, Double)
  -- | 信頼水準と grid x 列から (中心 μ̂, 帯 @(lo, hi)@) を評価する。
  -- band を持たないモデル (GAM/Robust) は 'Nothing'。
  svGrid  :: m -> Double -> [Double] -> ([Double], Maybe ([Double], [Double]))
  -- | **予測区間** (PI) 帯 @(lo, hi)@ を評価する (A5)。 観測分散 σ̂² を持つモデル
  -- (LM・Gaussian/Identity GLM) のみ実装し、 それ以外は既定の 'Nothing' (= PI 非提供)。
  -- 中心 μ̂ は 'svGrid' と共通ゆえここでは帯のみ返す。
  svGridPI :: m -> Double -> [Double] -> Maybe ([Double], [Double])
  svGridPI _ _ _ = Nothing
  -- | 当てはめ係数 @[β₀, β₁]@ と R² (A8 の式/R² 凡例注釈用)。 線形で「式」 の意味が
  -- 明快なモデル (LM) のみ実装し、 それ以外は既定の 'Nothing' (= 式注釈を出さない)。
  -- GLM は係数が η (リンク) スケールゆえ @y = β₀ + β₁x@ の素朴な式が成り立たず Nothing。
  svCoefR2 :: m -> Maybe ([Double], Double)
  svCoefR2 _ = Nothing
  -- | ブートストラップ ('piMethod (PIBootstrap …)') 用の素材。 訓練 (x, y)・再標本化データで
  -- refit する関数・新規観測の分布 (GLM family。 'Nothing' = 加法残差) を束ねて返す。 既定
  -- 'Nothing' (= ブートストラップ非対応 → 閉形式へフォールバック)。 closed-form を持たない
  -- モデル (非 Gaussian GLM / ロバスト) でも、 これを実装すれば PI を出せる。
  svBootKit :: m -> Maybe (BootKit m)
  svBootKit _ = Nothing

-- | ブートストラップに必要な素材 ('svBootKit' が返す。 内部利用)。
data BootKit m = BootKit
  { bkX       :: [Double]                              -- ^ 訓練 x。
  , bkY       :: [Double]                              -- ^ 訓練 y。
  , bkRefit   :: [Double] -> [Double] -> m             -- ^ 再標本化 (x, y) で refit。
  , bkObsDist :: Maybe (Double -> BD.Distribution Double) -- ^ 新規観測の分布 (GLM)。 Nothing=加法残差。
  }

-- | grid 評価の仕様。 'statModel' で生成し @<>@ でオプション合成 (Monoid)。
-- @msRender@ にモデルの grid 評価関数をクロージャで保持する (= 案A)。
data ModelSpec = ModelSpec
  { msRender  :: Maybe (GridOpts -> VisualSpec)  -- ^ statModel が設定 (先勝ち)。
  , msN       :: Maybe Int                       -- ^ grid 点数 (後勝ち)。
  , msRange   :: Maybe (Double, Double)          -- ^ grid 範囲 (後勝ち)。
  , msLevel   :: Maybe Double                    -- ^ CI 水準 (後勝ち)。
  , msBandMode :: Maybe BandMode                 -- ^ 帯モード (後勝ち、 既定 'BandCI'。 Phase 70.F
                                                 --   で帯 ON/OFF と CI/PI を 'bandMode' 1 本に統合)。
  , msPIMethod :: Maybe PIMethod                 -- ^ 帯の算出法 (後勝ち、 既定 'PIClosedForm'。 Phase 70.H)。
  , msPredAt  :: [Double]                        -- ^ 予測点 x (リスト累積 ++)。
  , msHoldAt  :: Maybe HoldAgg                   -- ^ 多変量 effect の固定方式 (後勝ち、 既定 Mean)。
  , msByVar   :: Maybe (Text, [Double])          -- ^ 層別変数 (後勝ち)。
  , msColor   :: Maybe Color                      -- ^ 線の固定色 (後勝ち, A2)。
  , msFill    :: Maybe Color                      -- ^ 帯の塗り色 (後勝ち, A2)。
  , msLinetype  :: Maybe LineType                 -- ^ 線種 (後勝ち, A2)。
  , msLinewidth :: Maybe Double                   -- ^ 線幅 = stroke (後勝ち, A2)。
  , msAlpha   :: Maybe Double                      -- ^ 帯/線の透明度 (後勝ち, A2)。
  , msLabel   :: Maybe Text                         -- ^ 単線の凡例ラベル (後勝ち, A3)。
  , msShowEq  :: Bool                                 -- ^ 回帰式を凡例に出す (Any, A8)。
  , msShowR2  :: Bool                                 -- ^ R² を凡例に出す (Any, A8)。
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

-- | 学習済の単変数モデルから grid 評価 'ModelSpec' を作る (along 不要)。
statModel :: SingleVarModel m => m -> ModelSpec
statModel m = mempty { msRender = Just (renderGrid m) }

-- | grid 評価点数を指定 (既定 100)。
grid :: Int -> ModelSpec
grid n = mempty { msN = Just n }

-- | grid 評価範囲を指定 (既定 = 説明変数 min/max)。
gridRange :: Double -> Double -> ModelSpec
gridRange lo hi = mempty { msRange = Just (lo, hi) }

-- | 出す帯を 1 つの値で選ぶ (Phase 70.F で帯 ON/OFF と CI/PI を統合)。 'BandMode' は
--   @BandOff@ (なし) \/ @BandCI@ (既定・信頼区間) \/ @BandPI@ (予測区間) \/ @BandCIPI@
--   (入れ子)。 既定 (未指定) は @BandCI@。 PI 非提供モデルでは PI 系は CI へフォールバック。
--
--   @statModel m \<\> bandMode BandPI@ \/ @… \<\> bandMode BandCIPI@ \/ @… \<\> bandMode BandOff@。
bandMode :: BandMode -> ModelSpec
bandMode m = mempty { msBandMode = Just m }

-- | 帯 (CI/PI) の**算出法**を選ぶ (Phase 70.H)。 @bandMode@ が「どの帯を出すか」を選ぶのに対し、
--   @piMethod@ は「どう計算するか」を選ぶ直交軸:
--
--     * @PIClosedForm@   = 閉形式 (Wald / 基底空間 OLS。 **既定**)。
--     * @PIBootstrap seed draws@ = case-resampling ブートストラップ (seed で決定的)。
--       閉形式 CI/PI を持たないモデル (非 Gaussian GLM / ロバスト) でも PI を出せる。
--
--   @statModel m \<\> bandMode BandPI \<\> piMethod (PIBootstrap 42 2000)@。
piMethod :: PIMethod -> ModelSpec
piMethod p = mempty { msPIMethod = Just p }

-- | 回帰線の固定色 (ggplot @geom_smooth(color=)@, A2)。 凡例は付かない (単線命名は 'statLabel')。
--   型安全な 'Color' を受ける (plot-core の 'color' と同じ方針)。 @statColor (fromHex "#ff0000")@
--   / @statColor N.red@ / @statColor (rgb 255 0 0)@。 Text→Color は 'fromHex' に委ねる。
statColor :: Color -> ModelSpec
statColor c = mempty { msColor = Just c }

-- | CI 帯の塗り色 (ggplot @geom_smooth(fill=)@, A2)。 型安全な 'Color' を受ける。
statFill :: Color -> ModelSpec
statFill c = mempty { msFill = Just c }

-- | 回帰線の線種 (ggplot @geom_smooth(linetype=)@, A2)。 'LineType' = 'LtSolid' / 'LtDashed' 等。
statLinetype :: LineType -> ModelSpec
statLinetype lt = mempty { msLinetype = Just lt }

-- | 回帰線の太さ (= stroke 幅。 ggplot @geom_smooth(linewidth=)@, A2)。
statLinewidth :: Double -> ModelSpec
statLinewidth w = mempty { msLinewidth = Just w }

-- | 帯/線の透明度 (ggplot @geom_smooth(alpha=)@, A2)。 帯に適用 (薄い塗り潰しの ggplot 流)。
statAlpha :: Double -> ModelSpec
statAlpha a = mempty { msAlpha = Just a }

-- | 単線に凡例ラベルを付ける (A3)。 1 群カテゴリ ('ColorByCol') + 'scaleColorManual' で
-- 色を固定し凡例エントリを 1 つ出す (固定色 'color' は @hasColorEncoding=False@ で
-- 凡例が出ない罠を回避)。 色は 'statColor' があればそれ、 なければ既定パレット先頭。
-- ★モデル比較で各線に名前を付ける用途 (= 群数 1 の 'byGroup' 特殊形)。
statLabel :: Text -> ModelSpec
statLabel lbl = mempty { msLabel = Just lbl }

-- | 回帰式を凡例ラベルに出す (A8。 ggplot @ggpubr::stat_regline_equation@ 相当)。
-- 'svCoefR2' を持つモデル (LM) で @y = β₀ + β₁x@ を自動生成し A3 機構 (凡例) に載せる。
-- 明示 'statLabel' があればそちらを優先。 式の出せないモデル (GLM 等) では注釈なし。
-- 'statR2' と併用すると @y = … + …x, R² = …@ のように 1 ラベルに連結する。
statEquation :: ModelSpec
statEquation = mempty { msShowEq = True }

-- | R² を凡例ラベルに出す (A8。 ggplot @ggpubr::stat_cor(aes(label=..rr.label..))@ 相当)。
-- 'svCoefR2' を持つモデル (LM) の R² を @R² = 0.987@ の形で凡例に載せる。
statR2 :: ModelSpec
statR2 = mempty { msShowR2 = True }

-- | CI 水準を指定 (既定 0.95)。
statLevel :: Double -> ModelSpec
statLevel l = mempty { msLevel = Just l }

-- | 多変量 effect で along 以外の説明変数の固定方式を指定 (既定 'Mean', C3)。
holdAt :: HoldAgg -> ModelSpec
holdAt h = mempty { msHoldAt = Just h }

-- | 層別 = 第2変数 @v@ を複数値 @vals@ で固定し、 値ごとに 1 曲線を色分け重畳する
-- (R @ggpredict@ terms 第2項相当, C3)。 多変量モデル ('statModelMulti') 専用。
byVar :: Text -> [Double] -> ModelSpec
byVar v vals = mempty { msByVar = Just (v, vals) }

-- | 予測点を 1 つ足す (C2)。 @<>@ でリスト累積 → @… <> predAt 1 <> predAt 3@ で複数点。
-- 各点は μ̂ (scatter) + CI 区間 [lo, hi] (lineRange) で描かれる (band を持たない GAM/
-- Robust は μ̂ 点のみ)。 単変数モデル前提 (多変量 effect は C3 の statModelMulti で対応)。
predAt :: Double -> ModelSpec
predAt x = mempty { msPredAt = [x] }

-- | A2/A3: 線レイヤへ aes (色・線種・太さ) を適用。 色の決定順は
-- (1) 群色 @mCol@ (byVar) → 'color'、 (2) 'statLabel' (@goLabel@) → 1 群 'colorBy'
-- (凡例を出すため・@n@ 点ぶんのカテゴリ列)、 (3) 'statColor' → 'color'。
-- 線種・太さは色と独立に適用。 @n@ = grid 点数 (label カテゴリ列の長さ)。
goLineDeco :: GridOpts -> Maybe Color -> Int -> Layer -> Layer
goLineDeco o mCol n l =
  let colorL = case (mCol, goLabel o) of
        (Just c, _)         -> color c                                   -- 群色優先
        (Nothing, Just lbl) -> colorBy (inlineCat (replicate n lbl))      -- statLabel: ColorByCol で凡例
        (Nothing, Nothing)  -> maybe mempty color (goColor o)            -- statColor or 無色
  in l <> colorL
       <> maybe mempty linetype (goLinetype o)
       <> maybe mempty (\lw -> stroke (lw *~ pt')) (goLinewidth o)

-- | A2: 帯レイヤへ fill 色・透明度を適用。 群色 @mCol@ があれば fill は群色を優先 ('statFill' で上書き不可)。
goBandDeco :: GridOpts -> Maybe Color -> Layer -> Layer
goBandDeco o mCol b =
  b <> maybe mempty color (mCol <|> goFill o)
    <> maybe mempty alpha       (goAlpha o)

-- | A3: 'statLabel' があれば @scaleColorManual@ で色を固定し @legend@ を出す 'VisualSpec'。
-- 色は 'statColor' (@goColor@) 優先・なければ既定パレット先頭。 ラベル無しは空。
labelLegend :: GridOpts -> VisualSpec
labelLegend o = case goLabel o of
  Just lbl -> scaleColorManual [(lbl, maybe (head effectPalette) toCss (goColor o))] <> legend
  Nothing  -> mempty

-- | A8: 式/R² 凡例ラベル文字列を組む。 @showEq@ で @y = β₀ + β₁x@、 @showR2@ で
-- @R² = 0.987@ を入れ、 両方なら @", "@ で連結する。 係数は単回帰 @[β₀, β₁]@ を想定
-- (β₁ の符号で @+@/@-@ を切替)。 どちらの flag も立っていなければ 'Nothing'。
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

-- | case-resampling ブートストラップで grid 上の CI / PI 帯を計算する (Phase 70.H)。
--   訓練 (x, y) を seed 付きで再標本化 → 'bkRefit' で refit → 'svGrid' で grid μ を予測、
--   を @draws@ 回。 CI = μ_b の分位点 (係数の不確実性)。 PI = 新規観測 y* の分位点
--   (加法残差 'bkObsDist'=Nothing、 または Family(μ) からの parametric ドロー)。 seed 純粋
--   (runST + mwc・同 seed でビット同一)。 戻り = (CI (lo,hi), PI (lo,hi))。
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

-- | grid 評価して曲線 (+ 帯) + 予測点の 'VisualSpec' を組む。 'statModel' がクロージャ化。
-- 帯がある場合は @band@ を先に置き @line@ (μ̂ 曲線) を上に重ねる。 予測点 (goPredAt) は
-- CI 区間を @lineRange@ (縦線 [lo,hi]) + μ̂ を @scatter@ で重ね、 μ̂ が区間内のどこにあるか
-- (非対称な GLM 帯でも) 忠実に示す。
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

-- ★案B: 既存 'Plottable' の 'toPlot' を 'ModelSpec' にも overload (同綴り)。
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
-- 'designMatrixF' で評価点設計行列を組み CI を評価する。
--
-- ★評価点 ModelFrame の合成は **DataFrame を経由せず VarRole を直接差し替える**
-- ('designMatrixF' は 'mfRoles' のみ参照し応答列は使わない = Design.hs:331)。 列構造・
-- 順序が訓練と完全一致するので 'confidenceBandAt' / 'predictGlmMuWithCI' がそのまま使える。
-- 型で単/多変量を分離し ('SingleVarModel' / 'MultiVarModel')、 along 忘れをコンパイル時に弾く。
-- ===========================================================================

-- | along を必須引数に持つ多変量モデル。 'statModelMulti' が要求する能力。
class MultiVarModel m where
  -- | 訓練 'ModelFrame' (along の range と他変数の集約元)。
  mvFrame     :: m -> ModelFrame
  -- | 評価点 'ModelFrame' から (中心 μ̂, CI 帯 @(lo, hi)@) を評価する。
  --   設計行列が組めない場合は空 + 'Nothing'。
  mvEvalFrame :: m -> Double -> ModelFrame -> ([Double], Maybe ([Double], [Double]))
  -- | 評価点での予測区間 (PI)。 既定 'Nothing' (PI 非提供)。 closed-form PI を持つ
  --   モデル ('MultiLMModel' = 多変量 OLS) のみ override する ('svGridPI' と同じ方針)。
  mvEvalFramePI :: m -> Double -> ModelFrame -> Maybe ([Double], [Double])
  mvEvalFramePI _ _ _ = Nothing

-- | 学習済の多変量モデルと along 変数から effect plot の 'ModelSpec' を作る。
--   along は **必須引数** (型で単/多変量を分離し誤用を弾く)。
--   @df |>> (layer (scatter \"x1\" \"y\") <> toPlot (statModelMulti m (along \"x1\") <> holdAt Median))@。
statModelMulti :: MultiVarModel m => m -> AlongSpec -> ModelSpec
statModelMulti m (AlongSpec v) = mempty { msRender = Just (renderGridMulti m v) }

-- | effect plot の 'VisualSpec' を組む。 along を grid で動かし他変数を 'HoldAgg' で固定。
--   byVar があれば第2変数の各値で曲線を色分け重畳する。 'statModelMulti' がクロージャ化。
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

-- | Marginalize (PDP/AME): 各 grid 点で along=gx に固定し他変数は **観測分布のまま**、
--   μ̂ を全観測行で平均する (band なし・曲線のみ。 全観測行 × grid で重い)。
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

-- | along 変数の観測範囲 (effect grid の既定範囲)。 along が連続でなければ退避 @(0,1)@。
alongRange :: ModelFrame -> Text -> (Double, Double)
alongRange mf v = case lookup v (mfRoles mf) of
  Just (RoleContinuous xs) | not (V.null xs) -> (V.minimum xs, V.maximum xs)
  _                                          -> (0, 1)

-- | 各説明変数を 'HoldAgg' で固定値の定数列に差し替えた評価点 'ModelFrame' を合成する。
--   along 変数は grid (gxs)、 応答列はダミー ('designMatrixF' は応答を使わない)。
--   override は byVar 等の明示固定で 'HoldAgg' より優先する。
evalFrame :: ModelFrame -> Text -> HoldAgg -> [(Text, Double)] -> [Double] -> ModelFrame
evalFrame mf alongV hold override gxs =
  let n = length gxs
      adjust (nm, role)
        | isResponseRole role           = (nm, RoleResponse (V.replicate n 0))
        | nm == alongV                  = (nm, RoleContinuous (V.fromList gxs))
        | Just fv <- lookup nm override = (nm, fixedRole role n fv)
        | otherwise                     = (nm, holdRole hold n nm role)
  in mf { mfRoles = map adjust (mfRoles mf), mfNRows = n }

-- | frame の along 列だけを定数 gx に差し替える (行数据え置き、 Marginalize 用)。
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

-- | 1 変数を 'HoldAgg' で固定した定数列にする (連続は集約値、 factor は固定水準 index)。
--   factor は Mean\/Median\/Mode\/Fixed すべて最頻水準に振替 (Reference のみ参照=index 0)。
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

-- | 明示値 (byVar / Fixed override) で 1 変数を定数列にする。
fixedRole :: VarRole -> Int -> Double -> VarRole
fixedRole role n fv = case role of
  RoleContinuous _    -> RoleContinuous (V.replicate n fv)
  RoleFactor levels _ -> RoleFactor levels (V.replicate n (clampIdx levels (round fv)))
  RoleResponse _      -> RoleResponse (V.replicate n fv)

clampIdx :: [Text] -> Int -> Int
clampIdx levels i = max 0 (min (length levels - 1) i)

-- | byVar 曲線の固定色パレット (層別の値ごとに 1 色)。
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

-- | 2 因子 grid + 'HoldAgg' の評価点 frame ('evalFrame' の 2 変数版)。
--   行 = v2 (外側)、 列 = v1 (内側) — 'P3.surface3D' の grid 規約
--   (row = y 方向) に一致させる。
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

-- | 応答曲面の数値核: @(gxs, gys, grid)@。 @grid !! j !! i = μ̂(gxs!!i, gys!!j)@。
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

-- | fit 済み多変量モデル → 3D 応答曲面 (z colormap 既定 ON・colorbar 自動)。
--   @saveSVG3D path (surfaceOf m "x1" "x2" <> dataScatter3DOf m "x1" "x2")@。
surfaceOf :: MultiVarModel m => m -> Text -> Text -> P3.VisualSpec3D
surfaceOf m v1 v2 = surfaceOfWith m v1 v2 defaultSurfaceOpts

-- | オプション付き ('SurfaceOpts': grid 点数・hold・範囲)。
surfaceOfWith :: MultiVarModel m => m -> Text -> Text -> SurfaceOpts -> P3.VisualSpec3D
surfaceOfWith m v1 v2 opts =
  let (gxs, gys, grid') = surfaceGrid m v1 v2 opts
  in P3.layer3D ( P3.surface3DGrid grid'
               <> P3.xRange3D (head gxs, last gxs)
               <> P3.yRange3D (head gys, last gys)
               <> P3.colormap3D )

-- | 実測点の 3D overlay: 訓練データの @(v1, v2, y)@ を scatter3D で重畳。
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

-- | 連続列の最頻 (観測値の完全一致でグループ化。 繰り返しのない真の連続では任意)。
modeV :: V.Vector Double -> Double
modeV xs | V.null xs = 0
         | otherwise = mostCommon (V.toList xs)

-- | factor の最頻水準 index。
modeIdx :: V.Vector Int -> Int
modeIdx idx | V.null idx = 0
            | otherwise  = mostCommon (V.toList idx)

mostCommon :: Ord a => [a] -> a
mostCommon = fst . maximumBy (comparing snd)
           . map (\g -> (head g, length g)) . group . sort

-- ===========================================================================
-- 共有描画 helper (複数のモデル族が利用)
-- ===========================================================================

-- | CI band の既定 level (95%)。
defaultCILevel :: Double
defaultCILevel = 0.95

-- | 分位線の色パレット (τ 昇順に割当て。 必要数を循環)。
quantilePalette :: [T.Text]
quantilePalette =
  [ "#4575b4", "#d73027", "#1a9850", "#984ea3", "#ff7f00", "#377eb8" ]

-- | 階段関数の頂点列を作る。 開始値 @s0@ (= t=0 での値) から、 各 @(tᵢ, sᵢ)@ について
-- 直前の高さで @tᵢ@ まで水平に来てから @sᵢ@ に垂直に跳ぶ 2 頂点を出す。
stepVerts :: Double -> [(Double, Double)] -> [(Double, Double)]
stepVerts s0 pts = (0, s0) : go s0 pts
  where
    go _    []            = []
    go prev ((t, s) : rest) = (t, prev) : (t, s) : go s rest

-- | grid index を x として複数曲線を色分け重畳する内部 helper。
gridCurves :: [(Text, [Double])] -> VisualSpec
gridCurves named =
  let mkLine (lbl, ys) =
        let xs = [ fromIntegral i | i <- [1 .. length ys] ] :: [Double]
        in layer ( line (inline xs) (inline ys)
                 <> colorBy (inlineCat (replicate (length ys) lbl)) )
  in mconcat (map mkLine named)

-- | 特徴重要度 → bar layer ("f1", "f2", … をカテゴリ軸に・値=重要度)。
importanceBar :: [Double] -> VisualSpec
importanceBar imps =
  let labels = [ "f" <> T.pack (show k) | k <- [1 .. length imps] ]
  in layer (bar (inlineCat labels) (inline imps))

-- | 行列の第 @i@/@j@ 列を (xs, ys) として取り出す (列不足は 0 埋め)。
matCols2 :: LA.Matrix Double -> Int -> Int -> ([Double], [Double])
matCols2 m i j =
  let cols = LA.toColumns m
      colAt k = if k < length cols then LA.toList (cols !! k) else replicate (LA.rows m) 0
  in (colAt i, colAt j)

-- | クラス代表点 (平均) をクラス色 ✚ で散布する (第 0/1 特徴)。 Discriminant /
--   NaiveBayes(Gaussian) の data-free 代表図。
classMeansScatter :: [[Double]] -> [Int] -> VisualSpec
classMeansScatter rows cids = classMeansScatterNamed rows cids []

-- | 'classMeansScatter' の **クラス名つき**版。 @names@ があれば凡例をクラス名 (levels)
--   に、 無ければ整数へフォールバック (@names !! k@・範囲外は show)。 df|-> 経路が
--   levels を載せた分類モデルの代表図で使う。
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

-- | chain index → 色 (effectPalette を巡回)。
chainColor :: Int -> Text
chainColor k = effectPalette !! (k `mod` length effectPalette)

-- ===========================================================================
-- 分類器抽象 (Discriminant / NaiveBayes / KNN 共通) — Phase 68 A3
-- ===========================================================================

-- | 学習済分類器を評価点行列で走らせ、 各行の予測クラスを返す共通インターフェース。
--   ('decisionBoundaryOf' / 'confusionOf' が分類器種に依らず動くための薄い抽象)。
class ClassPredict c where
  predictClasses :: c -> LA.Matrix Double -> [Int]
  -- | クラス番号 0..K-1 に対応する **クラス名 (levels)**。 高レベル @df |->@ 経路が
  --   fit 時に載せる (factor 列なら levels 名・数値列なら数値)。 既定は空 = 名前を
  --   持たないモデル ('confusionOf' 等は空なら整数ラベルにフォールバック)。
  classNamesOf :: c -> [Text]
  classNamesOf _ = []

-- ===========================================================================
-- 回帰診断の可視化 (係数 forest / 実測vs予測) — Phase 72.4/72.5
--
-- 係数表 ('coefSummary'・'Hanalyze.Diagnostics') と各モデルの実測/予測ペアを
-- 図に落とす薄い玄関。 数値層 (係数統計・予測) は非ゲートの 'Diagnostics' / 各 fit が
-- 持ち、 ここはゲート (plot-integration) 配下で 'VisualSpec' 化だけを担う。
-- ===========================================================================

-- | fit 済モデルから (実測値, 予測値) の対を取り出せる能力。 実測値は
--   @fitted + residual@ で復元する (回帰一般で成り立つ)。 instance は各モデル族の
--   'Plottable' と同じ Plot.* 側に置く (orphan・クラス=Core / instance=族 module)。
class HasObsPred m where
  -- | @(observed, predicted)@。 長さは観測数 n で一致する。
  obsPredPairs :: m -> ([Double], [Double])

-- | 実測 vs 予測プロット。 x=実測値・y=予測値の散布に @y = x@ の参照線 (灰の破線) を
--   重ねる。 点が参照線に近いほど当てはまりが良い (残差が小さい)。
obsVsPred :: HasObsPred m => m -> VisualSpec
obsVsPred m = let (obs, prd) = obsPredPairs m in obsPredSpec obs prd

-- | (実測, 予測) のリストから実測 vs 予測 spec を組む。 'obsVsPred' の純データ版
--   (テスト・任意のペアからの作図に再利用)。 空入力は空図。
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

-- | 係数 forest plot。 各係数の点推定 ('crEstimate') を中心、 95% CI ('crCI95') の
--   半幅を誤差バーとして 1 行ずつ水平に並べ、 0 (= 効果なし) に参照線を引く。 解析
--   Wald CI ('coefSummary') を持つ線形系で使う (CI は左右対称なので半幅で表せる)。
--   bootstrap 由来の非対称 CI を図にしたい場合は 'coefSummaryBoot' の行から個別に組む。
coefForest :: HasCoefSummary m => m -> VisualSpec
coefForest m =
  let rows  = coefSummary m
      names = [ crTerm r | r <- rows ]
      ests  = [ crEstimate r | r <- rows ]
      errs  = [ (hi - lo) / 2 | r <- rows, let (lo, hi) = crCI95 r ]
  in if null rows
       then mempty
       else layer (forest (inlineCat names) (inline ests) (inline errs) <> forestNull 0)
