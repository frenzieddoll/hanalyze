-- |
-- Module      : Hanalyze.Plot.Linear
-- Description : hgg 連携層 — 線形モデル族の図化 instance
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- hgg 連携層 — **線形モデル族** の図化 instance (Phase 71.5)。
--
-- ⚠ 親 'Hanalyze.Plot' と同じ cabal flag @plot-integration@ (既定 off) を
-- on にしたときのみ build される。 共通基盤 (class / ModelSpec / grid 評価核) は
-- 'Hanalyze.Plot.Core' を import して取り込む (orphan instance を許容:
-- クラス=Core・instance=ここ・型=Wrappers)。
--
-- 担当する型 (= LM 系・GLM 系・WLS):
--   LMModel / MultiLMModel / WeightedLMModel / GLMModel / MultiGLMModel。
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Hanalyze.Plot.Linear
  ( familyObsDist
  ) where

import           Data.List             (sortBy, zip4)
import           Data.Ord              (comparing)
import qualified Hanalyze.Model.HBM.Distribution as BD
import qualified Numeric.LinearAlgebra as LA

import           Hgg.Plot.Spec     ( layer, inline
                                       , scatter, line, band )

import           Hanalyze.Model.Wrappers
import           Hanalyze.Plot.Core
import           Hanalyze.Fit            (weightedR2)
import           Hanalyze.Model.Core     (FitResult, coefficientsV, fittedV, residualsV, rSquared1)
import           Hanalyze.Model.GLM      ( Family (..), LinkFn (..), GlmPredictCI (..)
                                                , predictGlmMuWithCI )
import           Hanalyze.Model.LM       ( CIBand (..), confidenceBand, confidenceBandAt
                                                , predictionBandAt )
import           Hanalyze.Model.Formula.Design  (designMatrixF)

-- ===========================================================================
-- 多変量モデル型 (effect plot 用、 新規 fit)
--
-- 既存の単変数 'LMModel' / 'GLMModel' (設計行列が @[1, x]@ 固定) とは別型。
-- formula 文字列 + 'DataFrame' で多変量 fit し、 formula を保持して評価点設計行列を
-- 組む (HoldAgg 固定 + along grid)。 ★GLM は formula 経路が未整備なので
-- 'designMatrixF' で設計行列を作り 'fitGLMFull' を直接呼ぶ。
-- ===========================================================================

instance MultiVarModel MultiLMModel where
  mvFrame = mlmFrame
  mvEvalFrame m level ef =
    case designMatrixF (mlmFormula m) ef of
      Left _        -> ([], Nothing)
      Right (xe, _) ->
        let cib = confidenceBandAt (mlmDesign m) (mlmResult m) level xe
            los = lowerBound cib
            his = upperBound cib
            mu  = zipWith (\l h -> (l + h) / 2) los his
        in (mu, Just (los, his))
  -- 多変量 OLS の closed-form PI (評価点設計行列 → predictionBandAt)。
  mvEvalFramePI m level ef =
    case designMatrixF (mlmFormula m) ef of
      Left _        -> Nothing
      Right (xe, _) ->
        let pib = predictionBandAt (mlmDesign m) (mlmResult m) level xe
        in Just (lowerBound pib, upperBound pib)

instance MultiVarModel MultiGLMModel where
  mvFrame = mglmFrame
  mvEvalFrame m level ef =
    case designMatrixF (mglmFormula m) ef of
      Left _        -> ([], Nothing)
      Right (xe, _) ->
        let beta = coefficientsV (mglmResult m)
            cis  = [ predictGlmMuWithCI (mglmLink m) level beta (mglmSigma m) r
                   | r <- LA.toRows xe ]
        in (map gpMu cis, Just (map gpLo cis, map gpHi cis))

-- ===========================================================================
-- 線形モデル (描画可能)
--
-- 'FitResult' (数値核) は設計行列 X を保持しないが、 回帰線・CI band を描くには
-- X が要る ('confidenceBand' は X 引数)。 そこで X と生 predictor を束ねた
-- 「描画可能なモデル」 を別型にする (= plot Phase 15 §2.1 の開放論点を (i) で確定)。
-- ===========================================================================


instance Plottable LMModel where
  -- 散布図に重ねる回帰線 + CI band。 'confidenceBand' は **訓練点**で評価し
  -- @yHats ± se@ を返す (= grid を渡すと fitted と不整合)。 ゆえに合成 grid を
  -- 使わず、 訓練 x を昇順ソートして直線を結ぶ (= 単回帰なら直線で grid と同形、
  -- かつ 'confidenceBand' を無改修で再利用できる)。 ± 半幅 errorY = se。
  toPlot m =
    let res    = lmResult m
        xs     = LA.toList (lmXraw m)
        yhat   = LA.toList (fittedV res)
        cib    = confidenceBand (lmDesign m) res defaultCILevel
        se     = zipWith (-) (upperBound cib) yhat   -- upper - ŷ = 片側半幅
        sorted = sortBy (comparing (\(x, _, _) -> x)) (zip3 xs yhat se)
        xsS    = [ x | (x, _, _) <- sorted ]
        yhatS  = [ y | (_, y, _) <- sorted ]
        seS    = [ e | (_, _, e) <- sorted ]
    in layer (band (inline xsS) (inline (zipWith (-) yhatS seS)) (inline (zipWith (+) yhatS seS)))
         <> layer (line (inline xsS) (inline yhatS))

  -- 残差診断 (代表回帰線 + 残差 vs fitted)。
  diagnosticPlots m =
    let res  = lmResult m
        yhat = LA.toList (fittedV res)
        resd = LA.toList (residualsV res)
    in [ toPlot m
       , layer (scatter (inline yhat) (inline resd))
       ]

-- | grid 評価 (Phase 16 C1)。 grid x で設計行列 @[1, x]@ を再構築し、
-- 訓練の分散核を流用する 'confidenceBandAt' で滑らかな曲線 + 対称 CI 帯を出す。
instance SingleVarModel LMModel where
  svRange m = let xs = LA.toList (lmXraw m) in (minimum xs, maximum xs)
  svGrid m level gxs =
    let xEval = LA.fromColumns [ LA.konst 1 (length gxs), LA.fromList gxs ]
        cib   = confidenceBandAt (lmDesign m) (lmResult m) level xEval
        los   = lowerBound cib
        his   = upperBound cib
        mu    = zipWith (\l h -> (l + h) / 2) los his
    in (mu, Just (los, his))
  -- PI = closed form σ̂²(1 + xᵀ(XᵀX)⁻¹x) (statsmodels obs_ci と一致)。
  svGridPI m level gxs =
    let xEval = LA.fromColumns [ LA.konst 1 (length gxs), LA.fromList gxs ]
        pib   = predictionBandAt (lmDesign m) (lmResult m) level xEval
    in Just (lowerBound pib, upperBound pib)
  -- A8: 係数 [β₀, β₁] と R² (式/R² 凡例注釈用)。
  svCoefR2 m = Just (LA.toList (coefficientsV (lmResult m)), rSquared1 (lmResult m))
  -- ブートストラップ: 加法誤差ゆえ obsDist=Nothing (μ + 再標本化残差)。
  svBootKit m = Just BootKit
    { bkX = LA.toList (lmXraw m)
    , bkY = zipWith (+) (LA.toList (fittedV (lmResult m))) (LA.toList (residualsV (lmResult m)))
    , bkRefit = \xs ys -> lmModel (LA.fromList xs) (LA.fromList ys)
    , bkObsDist = Nothing }

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

instance Plottable GLMModel where
  -- μ 曲線 + 非対称 Wald CI 帯。 各訓練点 (設計行列の行) で 'predictGlmMuWithCI' を
  -- 評価し、 x 昇順にソートして band (lo→hi の area) と μ 折れ線を重ねる。 帯を先に
  -- 置いて μ 線を上に描く。
  toPlot m =
    let beta  = coefficientsV (glmResult m)
        rows  = LA.toRows (glmDesign m)
        cis   = [ predictGlmMuWithCI (glmLink m) defaultCILevel beta (glmSigma m) r
                | r <- rows ]
        quads = sortBy (comparing (\(x, _, _, _) -> x))
                  (zip4 (LA.toList (glmXraw m))
                        (map gpMu cis) (map gpLo cis) (map gpHi cis))
        xsS = [ x | (x, _, _, _) <- quads ]
        muS = [ u | (_, u, _, _) <- quads ]
        loS = [ l | (_, _, l, _) <- quads ]
        hiS = [ h | (_, _, _, h) <- quads ]
    in layer (band (inline xsS) (inline loS) (inline hiS))
         <> layer (line (inline xsS) (inline muS))

  -- 残差診断 (μ 曲線 + 帯、 残差 vs fitted μ̂)。
  diagnosticPlots m =
    let res  = glmResult m
        yhat = LA.toList (fittedV res)
        resd = LA.toList (residualsV res)
    in [ toPlot m
       , layer (scatter (inline yhat) (inline resd))
       ]

-- | grid 評価 (Phase 16 C1)。 grid x の行 @[1, x]@ を 'predictGlmMuWithCI' に渡し、
-- μ スケールの非対称 Wald CI 帯を滑らかに評価する (band lo/hi は別々に保持)。
instance SingleVarModel GLMModel where
  svRange m = let xs = LA.toList (glmXraw m) in (minimum xs, maximum xs)
  svGrid m level gxs =
    let beta = coefficientsV (glmResult m)
        cis  = [ predictGlmMuWithCI (glmLink m) level beta (glmSigma m)
                   (LA.fromList [1, gx])
               | gx <- gxs ]
    in (map gpMu cis, Just (map gpLo cis, map gpHi cis))
  -- PI は **Gaussian + Identity のみ** = LM の closed form に帰着 (μ̂ = Xβ・W=I)。
  -- 非 Gaussian (Poisson/Binomial) は予測区間が応答分布の離散/非対称分位を要し
  -- closed form で出ないため 'Nothing' (over-claim しない・CI 帯と同じ部分集合方針)。
  svGridPI m level gxs = case (glmFamily m, glmLink m) of
    (Gaussian, Identity) ->
      let xEval = LA.fromColumns [ LA.konst 1 (length gxs), LA.fromList gxs ]
          pib   = predictionBandAt (glmDesign m) (glmResult m) level xEval
      in Just (lowerBound pib, upperBound pib)
    _ -> Nothing
  -- ブートストラップ: 新規観測は Family(μ) から parametric にドロー (Poisson/Bernoulli)。
  -- これにより closed form PI を持たない非 Gaussian GLM でも PI を出せる。
  svBootKit m = Just BootKit
    { bkX = LA.toList (glmXraw m)
    , bkY = zipWith (+) (LA.toList (fittedV (glmResult m))) (LA.toList (residualsV (glmResult m)))
    , bkRefit = \xs ys -> glmModel (glmFamily m) (glmLink m) (LA.fromList xs) (LA.fromList ys)
    , bkObsDist = familyObsDist (glmFamily m) }

-- ===========================================================================
-- 重み付き最小二乗 (WLS)
-- ===========================================================================

-- | grid 経路に委譲 (内側 LM の svGrid/PI は非スケール xEval × スケール設計で正しい
--   WLS CI を出す)。 'svRange' は元 x ('lmXraw') から。 'svCoefR2' のみ override し、
--   R² は statsmodels WLS と一致する weighted R² を返す (β̂ は内側のスケール OLS が WLS)。
instance SingleVarModel WeightedLMModel where
  svRange  (WeightedLMModel m _ _)  = svRange m
  svGrid   (WeightedLMModel m _ _)  = svGrid m
  svGridPI (WeightedLMModel m _ _)  = svGridPI m
  svCoefR2 (WeightedLMModel m ws ys) =
    let coefs = LA.toList (coefficientsV (lmResult m))
        yhats = case coefs of                                  -- ŷ = β₀ + β₁x (元スケール)
          (b0 : b1 : _) -> [ b0 + b1 * x | x <- LA.toList (lmXraw m) ]
          [b0]          -> [ b0 | _ <- LA.toList (lmXraw m) ]
          _             -> ys
    in Just (coefs, weightedR2 ws ys yhats)

-- | ★訓練点経路 ('LMModel' の素の 'toPlot') を**使わず** grid 経路 ('statModel') に
--   固定する。 これで WLS 線+CI が元 x スケールで出て、 元データ散布図と整合する。
instance Plottable WeightedLMModel where
  toPlot = toPlot . statModel

-- ===========================================================================
-- GLM family → 観測分布 (ブートストラップ PI 用)
-- ===========================================================================

-- | GLM family → 新規観測の分布関数 (μ ↦ 分布。 ブートストラップ PI の parametric ドロー用)。
--   Gaussian は加法残差で扱うため 'Nothing' (σ̂ を別途要さない)。 'svBootKit' が使う。
familyObsDist :: Family -> Maybe (Double -> BD.Distribution Double)
familyObsDist Poisson  = Just (\mu -> BD.Poisson  (max 1e-9 mu))
familyObsDist Binomial = Just (\mu -> BD.Bernoulli (min (1 - 1e-12) (max 1e-12 mu)))
familyObsDist Gaussian = Nothing

-- ===========================================================================
-- 実測 vs 予測 (HasObsPred) — Phase 72.4
--
-- 実測値 = fitted + residual で復元する (回帰一般)。 WLS は内側 fit が √w スケール
-- なので予測を 1/√w で元スケールへ戻し、 実測は保持した元 y ('wlmY') を使う。
-- ===========================================================================

-- | FitResult から (実測, 予測) を復元する共通ヘルパ。
obsPredFromFit :: FitResult -> ([Double], [Double])
obsPredFromFit r =
  let f = LA.toList (fittedV r)
      e = LA.toList (residualsV r)
  in (zipWith (+) f e, f)

instance HasObsPred LMModel where
  obsPredPairs = obsPredFromFit . lmResult

instance HasObsPred MultiLMModel where
  obsPredPairs = obsPredFromFit . mlmResult

instance HasObsPred GLMModel where
  obsPredPairs = obsPredFromFit . glmResult

instance HasObsPred MultiGLMModel where
  obsPredPairs = obsPredFromFit . mglmResult

instance HasObsPred WeightedLMModel where
  obsPredPairs m =
    let fScaled = LA.toList (fittedV (lmResult (wlmInner m)))
        prd     = zipWith (\f w -> if w > 0 then f / sqrt w else f) fScaled (wlmWeights m)
    in (wlmY m, prd)
