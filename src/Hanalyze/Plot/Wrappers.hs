-- |
-- Module      : Hanalyze.Plot.Wrappers
-- Description : hgg 連携層 — 汎用ラッパの Plottable / SingleVarModel 連携 instance
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- hgg 連携層 — **汎用ラッパ (どの族にも属さない) の Plottable / SingleVarModel**
-- 連携 instance + 専用 helper (Phase 71.7)。
--
-- ⚠ 親 'Hanalyze.Plot' と同じ cabal flag @plot-integration@ (既定 off) を
-- on にしたときのみ build される。 共通基盤 (class / ModelSpec / grid 評価核) は
-- 'Hanalyze.Plot.Core' を import して取り込む (orphan instance を許容:
-- クラス=Core・instance=ここ・型=Wrappers/各 Model module)。
--
-- 担当する型・ヘルパ (= 特定の ML / ベイズ族に属さない汎用ラッパ):
--   多出力線形回帰 'MultiFit' の残差相関 heatmap・k-NN 回帰の単変量描画
--   ('KNNRegressor')・透過標準化ラッパ 'StandardizedModel'・罰則回帰結果
--   'RegModel' の係数 bar・群別フィット 'GroupedFit' の N 曲線重畳・plot ColData
--   源の 'ColumnSource'・LM 係数診断アクセサ ('lmDiag' / 'groupedLmDiag')。
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Hanalyze.Plot.Wrappers
  ( -- ** 係数診断の薄アクセサ — Phase 52.A9
    lmDiag
  , groupedLmDiag
    -- ** 群別フィットの fullrange レンダラ — Phase 52.A4 / A7
  , groupedFullrange
  ) where

import qualified Data.Map.Strict       as Map
import qualified Data.Vector           as V
import qualified Data.Vector.Unboxed    as VU
import qualified Numeric.LinearAlgebra as LA

import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified DataFrame.Internal.Column    as DX
import qualified DataFrame.Internal.DataFrame  as DX

import           Hanalyze.Data.ColumnSource     (ColumnSource (..))

import           Hgg.Plot.Spec     ( VisualSpec, layer, inline, inlineCat
                                       , ColData (..)
                                       , scatter, line
                                       , heatmap, colorBy
                                       , scaleColorManual, legend
                                       , bar, title )

import           Hanalyze.Model.Wrappers
import           Hanalyze.Plot.Core
import           Hanalyze.Fit
import           Hanalyze.Model.LM.Diagnostics (CoefStats (..), lmCoefStats)
import           Hanalyze.Model.LM     (linspace)
import           Hanalyze.Model.MultiLM (MultiFit (..))
import           Hanalyze.Stat.Standardize
                   ( Standardizer (..)
                   , applyStandardizerCol )
import           Hanalyze.Model.KNN (KNNRegressor (..), predictKNNR)

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

instance Plottable MultiFit where
  -- 残差相関 q×q を heatmap に。 行 i・列 j のセル (x=yⱼ, y=yᵢ) に相関値 mfResidCor[i,j]
  -- を割り当てて 'heatmap' (= geom_tile) layer を 1 枚返す。 軸は出力名ラベル (categorical)。
  toPlot mf =
    let cor   = LA.toLists (mfResidCor mf)
        q     = length cor
        lbl k = "y" <> T.pack (show (k + 1 :: Int))   -- 出力名ラベル
        cells = [ (lbl j, lbl i, (cor !! i) !! j)
                | i <- [0 .. q - 1], j <- [0 .. q - 1] ]
        xs = [ x | (x, _, _) <- cells ]
        ys = [ y | (_, y, _) <- cells ]
        vs = [ v | (_, _, v) <- cells ]
    in layer (heatmap (inlineCat xs) (inlineCat ys) (inline vs))

-- C2: 元スケール逆変換 instance (Phase 70.3 項目 C) -------------------------
--
-- 内側モデルは標準化空間で学習されている。 ここで予測子 x を入力時に標準化し、
-- ('standardizedY' なら) 応答 y を出力時に逆変換することで、 図・予測を**元スケール**で
-- 返す。 単変量 (1 特徴) 描画が対象 (smXStd の 0 次元を使う)。

-- | k-NN 回帰の単変量描画 (透過標準化の内側として要る)。 1 特徴 ('knnRX' が 1 列) を
--   仮定し grid 点を予測曲線にする。 局所平均ゆえ band は持たない (Nothing)。
instance SingleVarModel KNNRegressor where
  svRange m =
    let c0 = LA.toList (head (LA.toColumns (knnRX m)))
    in (minimum c0, maximum c0)
  svGrid m _ gxs =
    let xEval = LA.fromColumns [LA.fromList gxs]    -- n × 1 (単一特徴)
    in (VU.toList (predictKNNR m xEval), Nothing)   -- 帯なし

-- | 0 次元 (単変量描画の予測子) の (μ, σ)。
stMu1, stSd1 :: Standardizer -> Double
stMu1 = head . stMu
stSd1 = head . stSd

-- | 応答 y の逆変換 (@smYStd = Just@ のみ実施。 @Nothing@ は元 y スケールのまま)。
unstdY :: Maybe (Double, Double) -> Double -> Double
unstdY (Just (muY, sdY)) v = v * sdY + muY
unstdY Nothing           v = v

-- | 透過標準化ラッパの単変量描画 (元スケール)。 入力 x を標準化 → 内側を評価 →
--   ('standardizedY' なら) 出力 y を逆変換する。 内側 'svRange' (標準化空間) は
--   smXStd の 0 次元で元スケールへ戻す。 band/PI も同様に y を逆変換。
instance SingleVarModel m => SingleVarModel (StandardizedModel m) where
  svRange (StandardizedModel inner sx _ _) =
    let (zlo, zhi) = svRange inner
        unX z = z * stSd1 sx + stMu1 sx
    in (unX zlo, unX zhi)
  svGrid (StandardizedModel inner sx mY _) level xs =
    let zs            = map (applyStandardizerCol sx 0) xs
        (muZ, mbBand) = svGrid inner level zs
    in ( map (unstdY mY) muZ
       , fmap (\(lo, hi) -> (map (unstdY mY) lo, map (unstdY mY) hi)) mbBand )
  svGridPI (StandardizedModel inner sx mY _) level xs =
    let zs = map (applyStandardizerCol sx 0) xs
    in fmap (\(lo, hi) -> (map (unstdY mY) lo, map (unstdY mY) hi))
            (svGridPI inner level zs)
  -- 線形内側のみ式注釈 (係数を元スケールへ逆変換・R² はスケール不変で透過)。
  --   X のみ標準化: y = β₀ + β₁·(x−μₓ)/σₓ = (β₀ − β₁μₓ/σₓ) + (β₁/σₓ)·x。
  --   X+y 標準化:   y = σ_y·(β₀ + β₁·(x−μₓ)/σₓ) + μ_y。
  svCoefR2 (StandardizedModel inner sx mY _) =
    case svCoefR2 inner of
      Just ([b0, b1], r2) ->
        let mux = stMu1 sx; sdx = stSd1 sx
            (a0, a1) = case mY of
              Nothing         -> (b0 - b1 * mux / sdx, b1 / sdx)
              Just (muY, sdY) -> (b0 * sdY + muY - b1 * sdY * mux / sdx, b1 * sdY / sdx)
        in Just ([a0, a1], r2)
      _ -> Nothing   -- 非線形 (kNN 等) は式注釈なし

-- | 透過標準化ラッパの代表図 = 元スケールの予測曲線 (+ 単変量散布 'smTrain')。
--   内側 'toPlot' (標準化軸) には依存せず、 ラッパ自身の 'SingleVarModel' を
--   'statModel' grid 機構へ流す。
instance SingleVarModel m => Plottable (StandardizedModel m) where
  toPlot sm = case smTrain sm of
    Just (xs, ys) -> layer (scatter (inline xs) (inline ys)) <> toPlot (statModel sm)
    Nothing       -> toPlot (statModel sm)

-- | 係数 bar (特徴名ラベル・元スケール) を代表図に。 CV パスがあれば診断束に λ-MSE 図。
instance Plottable RegModel where
  toPlot m =
    layer (bar (inlineCat (rmgNames m)) (inline (rmgCoefs m)))
      <> title (regMethodName (rmgMethod m) <> " coefficients (\955="
                <> T.pack (show (roundTo 4 (rmgLambda m))) <> ")")
  diagnosticPlots m = toPlot m : case rmgCVPath m of
    Just (lams, scores) ->
      [ layer (line (inline lams) (inline scores)) <> title "CV/LOOCV score path" ]
    Nothing -> []

-- | RegMethod の表示名 (図タイトル用)。
regMethodName :: RegMethod -> Text
regMethodName Ridge            = "Ridge"
regMethodName Lasso            = "Lasso"
regMethodName (ElasticNet _)   = "Elastic Net"
regMethodName (MCP _)          = "MCP"
regMethodName (SCAD _)         = "SCAD"
regMethodName (AdaptiveLasso _) = "Adaptive Lasso"
regMethodName (GroupLasso _)   = "Group Lasso"

-- | 小数 n 桁丸め (タイトル表示用)。
roundTo :: Int -> Double -> Double
roundTo n v = let f = 10 ^^ n in fromIntegral (round (v * f) :: Integer) / f

-- | A9: 'LMModel' の係数診断 (SE / t値 / p値) を一発取得する薄アクセサ。
--   数値核は 'Hanalyze.Model.LM.Diagnostics.lmCoefStats'。 描画用に X を束ねた
--   'LMModel' から設計行列 ('lmDesign') と fit 結果 ('lmResult') を渡すだけ。
--   返りは係数順 (@[(Intercept), x]@) の 'CoefStats' リスト。
lmDiag :: LMModel -> [CoefStats]
lmDiag m = lmCoefStats (lmDesign m) (lmResult m)

-- | A9: 群別 LM フィット ('grouped "g" (lm …)' の結果) の各群係数診断を取り出す。
--   @[(群ラベル, [係数の CoefStats])]@。 群間で傾き SE/有意性を比較する用途。
--   ★@Fitted spec ~ LMModel@ に特殊化 (LM 群フィット専用)。
groupedLmDiag :: (Fitted spec ~ LMModel) => GroupedFit spec -> [(Text, [CoefStats])]
groupedLmDiag = map (fmap lmDiag) . groupModels

-- | 群別フィットを N 曲線で重畳する ('toPlot' = 各群 'svGrid' の μ̂ 曲線・群色 + 凡例)。
--   ★A3 の凡例機構を N 群へ一般化: 各曲線を 'ColorByCol' (群ラベル) に載せ
--   'scaleColorManual' で群色を固定し 'legend' を出す (固定色だと凡例が出ない罠を回避)。
--   grid 点数 100・帯なし (A1 既定 OFF) 固定。 群色は 'effectPalette' の循環。
instance SingleVarModel (Fitted spec) => Plottable (GroupedFit spec) where
  toPlot = renderGrouped

-- | 群別フィットを **各群の x 範囲のみ**で描く (既定。 'toPlot' = これ)。
renderGrouped :: SingleVarModel (Fitted spec) => GroupedFit spec -> VisualSpec
renderGrouped = renderGroupedWith False

-- | 群別フィットを **データ全幅** (全群 x の union 範囲) へ延ばして描く (A7 fullrange)。
--   ggplot @geom_smooth(fullrange = TRUE)@ 相当: 各群の回帰線を、 その群の x 範囲だけでなく
--   **全群を合わせた x の min/max** まで延長して評価する (群間の傾き差を全域で比較しやすい)。
--   ★単一モデルでは「データ全幅 = 訓練 x」 ゆえ意味を持たない (range 拡張は grouped 固有)。
--   'toPlot' とは別経路 (結果型 'GroupedFit' に描画 flag を持たせない・別レンダラとして提供)。
groupedFullrange :: SingleVarModel (Fitted spec) => GroupedFit spec -> VisualSpec
groupedFullrange = renderGroupedWith True

-- | 群別フィットの共通レンダラ。 @full@ で評価 x 範囲を切替える
--   (@False@ = 各群自範囲、 @True@ = 全群 union 範囲 = A7 fullrange)。
renderGroupedWith :: SingleVarModel (Fitted spec) => Bool -> GroupedFit spec -> VisualSpec
renderGroupedWith full gf =
  let pairs   = zip [0 :: Int ..] (gfGroups gf)
      n       = 100
      colOf i = effectPalette !! (i `mod` length effectPalette)
      -- fullrange = 全群 svRange の union (lo = 最小, hi = 最大)。 群が無ければ使われない。
      ranges  = [ svRange m | (_, (_, m)) <- pairs ]
      unionLo = minimum (map fst ranges)
      unionHi = maximum (map snd ranges)
      curveOf (_, (lbl, m)) =
        let (lo, hi) = if full then (unionLo, unionHi) else svRange m
            gxs      = linspace lo hi n
            (mu, _)  = svGrid m defaultCILevel gxs
        in layer (line (inline gxs) (inline mu)
                    <> colorBy (inlineCat (replicate n lbl)))
      legendSpec
        | null pairs = mempty
        | otherwise  = scaleColorManual [ (lbl, colOf i) | (i, (lbl, _)) <- pairs ]
                         <> legend
  in foldMap curveOf pairs <> legendSpec

-- --- plot ColData 源の ColumnSource instance (flag 配下・非 portable) -------
--
-- hgg の @[(Text, ColData)]@ (= df 中立表現)。 'NumData' は数値列、
-- 'TxtData' は factor 列。 'lookupCol' は数値列のみ返し、 'toFrame' は
-- 数値・文字列の両方を 'DX.DataFrame' に詰めて formula 経路で factor を温存する。
instance ColumnSource [(Text, ColData)] where
  lookupCol n cs = case lookup n cs of
    Just (NumData v) -> Just (V.toList v)
    _                -> Nothing
  columnNames = map fst
  toFrame cs  = DX.fromNamedColumns (concatMap toCol cs)
    where
      toCol (n, NumData v) = [(n, DX.fromList (V.toList v))]
      toCol (n, TxtData v) = [(n, DX.fromList (V.toList v))]

-- ===========================================================================
-- 実測 vs 予測 (HasObsPred) — Phase 72.4
--
-- 罰則回帰 ('RegModel') は元スケール係数 (rmgIntercept + rmgCoefs) と生設計
-- 'rmgXraw' から予測を再構成し、 実測は生応答 'rmgYraw' を使う。
-- ===========================================================================

instance HasObsPred RegModel where
  obsPredPairs m =
    let beta = LA.fromList (rmgCoefs m)
        prd  = map (+ rmgIntercept m) (LA.toList (rmgXraw m LA.#> beta))
    in (LA.toList (rmgYraw m), prd)
