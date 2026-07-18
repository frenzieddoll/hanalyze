-- |
-- Module      : Hanalyze.Plot.Robust
-- Description : hgg 連携層 — ロバスト・分位点回帰族の図化 instance
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- hgg 連携層 — **ロバスト・分位点回帰族** の図化 instance (Phase 71.5)。
--
-- ⚠ 親 'Hanalyze.Plot' と同じ cabal flag @plot-integration@ (既定 off) を
-- on にしたときのみ build される。 共通基盤 (class / ModelSpec / grid 評価核) は
-- 'Hanalyze.Plot.Core' を import して取り込む (orphan instance を許容)。
--
-- 担当する型 (= M 推定ロバスト回帰・分位点回帰):
--   RobustModel / MultiRobustModel / QuantileModel / MultiQuantileModel。
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Hanalyze.Plot.Robust
  ( robustBand
  ) where

import           Data.List             (sortBy, minimumBy)
import           Data.Ord              (comparing)
import qualified Data.Vector           as V
import qualified Numeric.LinearAlgebra as LA

import           Graphics.Hgg.Spec     ( layer, inline, fromHex
                                       , scatter, line, band
                                       , sizeBy, color )

import           Hanalyze.Model.Wrappers
import           Hanalyze.Plot.Core
import           Hanalyze.Model.LM       (designMatrix)
import           Hanalyze.Model.Robust   (RobustFit (..), robustCovBeta)
import           Hanalyze.Model.Weibull  (quantileNormal)
import           Hanalyze.Model.Quantile (QRFit (..))
import           Hanalyze.Model.Formula.Design  (designMatrixF)

-- ===========================================================================
-- 多変量ロバスト回帰 (effect plot + 係数サマリ) — Phase 70.D
--
-- ロバスト回帰は formula 経路を持たない (単回帰 'RobustModel' のみだった) ので、
-- 'MultiLMModel' と同型の frame-carrying ラッパを新設する。 設計行列は
-- 'additiveFormula' 由来 ('designMatrixF' で @[1, x1,…,xp]@)、 fit は 'fitRobustLM'、
-- CI 帯は M 推定量サンドイッチ共分散 ('robustCovBeta'・statsmodels RLM 一致)。
-- ===========================================================================

instance MultiVarModel MultiRobustModel where
  mvFrame = mrmFrame
  mvEvalFrame m level ef =
    case designMatrixF (mrmFormula m) ef of
      Left _        -> ([], Nothing)
      Right (xe, _) ->
        let fit  = mrmFit m
            beta = rfCoef fit
            cov  = robustCovBeta (rfEstimator fit) (rfScale fit)
                                 (rfResiduals fit) (mrmDesign m)
            z    = quantileNormal ((1 + level) / 2)
            rows = LA.toRows xe
            mu   = [ r `LA.dot` beta | r <- rows ]
            se   = [ sqrt (max 0 (r `LA.dot` (cov LA.#> r))) | r <- rows ]
        in ( mu, Just ( zipWith (\mu' s -> mu' - z * s) mu se
                      , zipWith (\mu' s -> mu' + z * s) mu se ) )

-- ===========================================================================
-- ロバスト回帰 (描画可能)
--
-- 'RobustFit' (Hanalyze.Model.Robust) は M-estimator IRLS の係数 'rfCoef' / fitted
-- 'rfFitted' / 最終重み 'rfWeights' (≤ 1、 外れ値ほど小) を持つ。 代表図 ('toPlot') は
-- ロバスト直線 + サンドイッチ CI 帯。 「どの点がダウンウェイトされたか」 は
-- 'diagnosticPlots' 側で **点サイズ = IRLS 重み** の散布図に encode して見せる。
-- ===========================================================================

instance Plottable RobustModel where
  -- ロバスト直線 + CI 帯 ('robustBand' = M 推定量サンドイッチ共分散)。 LM と揃え、
  -- 訓練点の ŷ='rfFitted' を x 昇順に結ぶ (= 単回帰なので直線) + 帯を重ねる。
  toPlot m =
    let fit        = rmFit m
        xs         = LA.toList (rmXraw m)
        yhat       = LA.toList (rfFitted fit)
        sorted     = sortBy (comparing fst) (zip xs yhat)
        xsS        = map fst sorted
        yhatS      = map snd sorted
        (los, his) = robustBand m defaultCILevel xsS
    in layer (band (inline xsS) (inline los) (inline his))
         <> layer (line (inline xsS) (inline yhatS))

  -- 診断束: ロバスト直線 + 残差 vs fitted + **重み encode 散布図** (点サイズ = IRLS
  -- 重み、 小さい点 = ダウンウェイトされた外れ値)。 y は ŷ + 残差で復元。
  diagnosticPlots m =
    let fit  = rmFit m
        xs   = LA.toList (rmXraw m)
        yhat = LA.toList (rfFitted fit)
        resd = LA.toList (rfResiduals fit)
        ys   = zipWith (+) yhat resd
        ws   = LA.toList (rfWeights fit)
    in [ toPlot m
       , layer (scatter (inline yhat) (inline resd))
       , layer (scatter (inline xs) (inline ys) <> sizeBy (inline ws))
       ]

-- | ロバスト回帰の CI 帯。 M 推定量 β̂ の漸近共分散 ('robustCovBeta'・サンドイッチ・
-- statsmodels RLM 一致) から、 評価点 x での @se(ŷ) = √([1,x]·Cov·[1,x]ᵀ)@、
-- 帯 = @μ̂ ∓ z·se@ (z = 正規分位点・RLM は正規で Wald CI)。
robustBand :: RobustModel -> Double -> [Double] -> ([Double], [Double])
robustBand m level gxs =
  let fit  = rmFit m
      xd   = designMatrix (V.fromList (LA.toList (rmXraw m)))   -- [1, x]
      cov  = robustCovBeta (rfEstimator fit) (rfScale fit) (rfResiduals fit) xd
      z    = quantileNormal ((1 + level) / 2)
      beta = rfCoef fit
      b0   = LA.atIndex beta 0
      b1   = if LA.size beta > 1 then LA.atIndex beta 1 else 0
      muAt gx = b0 + b1 * gx
      seAt gx = let v = LA.fromList [1, gx]
                in sqrt (max 0 (v `LA.dot` (cov LA.#> v)))
  in ( [ muAt gx - z * seAt gx | gx <- gxs ]
     , [ muAt gx + z * seAt gx | gx <- gxs ] )

-- | grid 評価 (Phase 16 C1)。 grid x で β̂·[1, x] を評価しロバスト直線を滑らかに描く。
-- band は 'robustBand' (サンドイッチ CI) を返す (LM と揃えた)。
instance SingleVarModel RobustModel where
  svRange m = let xs = LA.toList (rmXraw m) in (minimum xs, maximum xs)
  svGrid m level gxs =
    let beta = rfCoef (rmFit m)
        mu   = [ LA.atIndex beta 0
                 + (if LA.size beta > 1 then LA.atIndex beta 1 * gx else 0)
               | gx <- gxs ]
        (los, his) = robustBand m level gxs
    in (mu, Just (los, his))
  -- ブートストラップ: 加法誤差 (残差再標本化)。 refit は同じ estimator で再 fit。
  svBootKit m =
    let fit = rmFit m
    in Just BootKit
       { bkX = LA.toList (rmXraw m)
       , bkY = zipWith (+) (LA.toList (rfFitted fit)) (LA.toList (rfResiduals fit))
       , bkRefit = \xs ys -> robustModel (rfEstimator fit) (LA.fromList xs) (LA.fromList ys)
       , bkObsDist = Nothing }

-- ===========================================================================
-- 分位点回帰 (描画可能)
--
-- 'QRFit' (Hanalyze.Model.Quantile) は 1 つの分位 τ に対する係数 + fitted 'qfYHat' を
-- 持つ。 複数の τ (例 0.1/0.5/0.9) の fit を重ねると **予測区間そのものを線群で** 表現
-- できる (= heteroscedastic データで帯より直接的)。 各線は 'color' ('fromHex') で固定色。
-- ===========================================================================

instance Plottable QuantileModel where
  -- 各 τ-fit を x 昇順に結んだ折れ線を、 固定色で重畳 (分位ごとに 1 layer)。
  toPlot m =
    let xs = LA.toList (qmXraw m)
        mkLine (i, (_tau, fit)) =
          let yhat   = LA.toList (qfYHat fit)
              sorted = sortBy (comparing fst) (zip xs yhat)
              col    = quantilePalette !! (i `mod` length quantilePalette)
          in layer (line (inline (map fst sorted)) (inline (map snd sorted))
                      <> color (fromHex col))
    in foldMap mkLine (zip [0 ..] (qmFits m))

-- | 多変量分位点回帰の代表図 = **第 1 予測子に沿った effect plot** (他予測子は訓練平均に
--   固定)。 各 τ を 1 本の線で色分け重畳する (単変量 'QuantileModel' の τ 別線群の一般化)。
--   分位点回帰は閉形式 CI を持たないため帯はなし。
instance Plottable MultiQuantileModel where
  toPlot m =
    case LA.toColumns (mqmX m) of                    -- [1, x₁, …, xₚ]
      (_ : x1 : rest) ->
        let xs1   = LA.toList x1
            means = [ LA.sumElements c / fromIntegral (max 1 (LA.size c)) | c <- rest ]  -- x₂..xₚ の平均
            (lo, hi) = (minimum xs1, maximum xs1)
            gn    = 100 :: Int
            grid' = [ lo + (hi - lo) * fromIntegral i / fromIntegral (gn - 1) | i <- [0 .. gn - 1] ]
            evalX = LA.fromRows [ LA.fromList (1 : gx : means) | gx <- grid' ]
            mkLine (i, (_t, fit)) =
              let yhat = LA.toList (evalX LA.#> qfBeta fit)
                  col  = quantilePalette !! (i `mod` length quantilePalette)
              in layer (line (inline grid') (inline yhat) <> color (fromHex col))
        in foldMap mkLine (zip [0 :: Int ..] (mqmFits m))
      _ -> mempty   -- 予測子が無い (設計行列が intercept のみ) = 描画不能

-- ===========================================================================
-- 実測 vs 予測 (HasObsPred) — Phase 72.4
--
-- ロバスト/分位点 fit は ŷ と残差を直接持つので 実測 = ŷ + residual。 分位点回帰は
-- 0.5 (中央値) に最も近い τ の fit を代表予測に使う (中央値回帰 = 条件付き中央値)。
-- ===========================================================================

instance HasObsPred RobustModel where
  obsPredPairs m =
    let f = LA.toList (rfFitted (rmFit m))
        e = LA.toList (rfResiduals (rmFit m))
    in (zipWith (+) f e, f)

instance HasObsPred MultiRobustModel where
  obsPredPairs m =
    let f = LA.toList (rfFitted (mrmFit m))
        e = LA.toList (rfResiduals (mrmFit m))
    in (zipWith (+) f e, f)

instance HasObsPred QuantileModel where
  obsPredPairs m =
    case qmFits m of
      [] -> ([], [])
      fs ->
        let (_, fit) = minimumBy (comparing (\(t, _) -> abs (t - 0.5))) fs
            f = LA.toList (qfYHat fit)
            e = LA.toList (qfResid fit)
        in (zipWith (+) f e, f)
