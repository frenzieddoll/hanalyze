-- |
-- Module      : Hanalyze.Plot.Smooth
-- Description : hgg 連携層 — 平滑化・カーネル法族の図化 instance
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- hgg 連携層 — **平滑化・カーネル法族** の図化 instance (Phase 71.5)。
--
-- ⚠ 親 'Hanalyze.Plot' と同じ cabal flag @plot-integration@ (既定 off) を
-- on にしたときのみ build される。 共通基盤 (class / ModelSpec / grid 評価核) は
-- 'Hanalyze.Plot.Core' を import して取り込む (orphan instance を許容)。
--
-- 担当する型 (= spline / GAM / GP / kernel 法):
--   SplineModel / GAMModel / GAMModelN / GPResult / GPRegModel / GPRegModelN /
--   MultiGPResult。
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Hanalyze.Plot.Smooth
  ( splineBasisAt
  , gamGridCI
  , multiGpCurves
  ) where

import           Data.List             (sortBy)
import           Data.Ord              (comparing)
import qualified Data.Vector           as V
import qualified Numeric.LinearAlgebra as LA

import           Data.Text             (Text)
import qualified Data.Text             as T

import           Graphics.Hgg.Spec     ( VisualSpec, layer, inline, inlineCat
                                       , scatter, line, band
                                       , colorBy, alpha )

import           Hanalyze.Model.Wrappers
import           Hanalyze.Plot.Core
import           Hanalyze.Model.Core     (fittedV, residualsV)
import           Hanalyze.Model.GP       (GPResult (..), gpNoiseVar)
import           Hanalyze.Model.LM       ( CIBand (..), confidenceBand, confidenceBandAt
                                                , predictionBandAt )
import           Hanalyze.Model.Spline   ( SplineKind (..), SplineFit (..)
                                                , bsplineBasis, naturalSplineBasis )
import           Hanalyze.Model.GAM      (GAMFit (..), predictGAMSE)
import           Hanalyze.Model.Weibull  (quantileNormal)
import           Hanalyze.Model.MultiGP  (MultiGPResult (..))
import qualified Statistics.Distribution         as SD
import           Statistics.Distribution.StudentT (studentT)

-- ===========================================================================
-- ガウス過程 (描画可能)
--
-- 'GPResult' (Hanalyze.Model.GP) は予測 grid (gpTestX) + 事後平均 (gpMean) +
-- credible band (gpLower/gpUpper) を **自己完結** で保持する。 ゆえに LMModel の
-- ように X を別途束ねる必要がなく、 結果型をそのまま 'Plottable' にできる。
-- ===========================================================================

instance Plottable GPResult where
  -- 事後平均 (曲線) + credible band。 予測 grid をソートして 'line' に渡せば GP の曲線が
  -- そのまま描ける。 band 半幅は対称 (mean ± 2σ) ゆえ es = gpUpper − gpMean とし、
  -- 'band' に [mean−es, mean+es] を、 'line' に mean を載せる。
  toPlot res =
    let triples = sortBy (comparing (\(x, _, _) -> x))
                    (zip3 (gpTestX res) (gpMean res)
                          (zipWith (-) (gpUpper res) (gpMean res)))
        xs = [ x | (x, _, _) <- triples ]
        ys = [ y | (_, y, _) <- triples ]
        es = [ e | (_, _, e) <- triples ]
    in layer (band (inline xs) (inline (zipWith (-) ys es)) (inline (zipWith (+) ys es)))
         <> layer (line (inline xs) (inline ys))

-- ===========================================================================
-- スプライン回帰 (描画可能)
--
-- 'SplineFit' (Hanalyze.Model.Spline) は基底係数 'sfBeta' と、 基底行列で fit した
-- 線形モデル核 'sfResult' (= 'FitResult') を保持する。 ゆえに **基底行列を設計行列と
-- みなせば** LMModel と同じ 'confidenceBand' (= X (XᵀX)⁻¹ Xᵀ の対角) がそのまま使える。
-- 違いは「曲線」 である点だけ: 単回帰の直線でなく、 訓練点を x 昇順に結ぶと基底展開に
-- よる平滑曲線になる。 帯は LM と同じ **線形モデルの対称 Wald CI** (基底空間での予測分散)。
-- ===========================================================================

-- | 'SplineFit' を訓練 x で評価したときの基底行列 (= confidenceBand の設計行列)。
splineBasisAt :: SplineFit -> LA.Vector Double -> LA.Matrix Double
splineBasisAt fit xs =
  let xsV = V.fromList (LA.toList xs)
  in case sfKind fit of
       BSpline k    -> bsplineBasis k (sfKnots fit) xsV
       NaturalCubic -> naturalSplineBasis (sfKnots fit) xsV

instance Plottable SplineModel where
  -- 平滑曲線 + CI band。 基底行列を設計行列とみなして 'confidenceBand' を訓練点で
  -- 評価し (LMModel と同じ Wald CI)、 x 昇順にソートして折れ線で結ぶ (= 平滑曲線)。
  -- ± 半幅 errorY = se (帯は基底空間の予測分散 = 対称)。
  toPlot m =
    let fit    = splFit m
        res    = sfResult fit
        xs     = LA.toList (splXraw m)
        yhat   = LA.toList (fittedV res)
        basis  = splineBasisAt fit (splXraw m)
        cib    = confidenceBand basis res defaultCILevel
        se     = zipWith (-) (upperBound cib) yhat   -- upper - ŷ = 片側半幅
        sorted = sortBy (comparing (\(x, _, _) -> x)) (zip3 xs yhat se)
        xsS    = [ x | (x, _, _) <- sorted ]
        yhatS  = [ y | (_, y, _) <- sorted ]
        seS    = [ e | (_, _, e) <- sorted ]
    in layer (band (inline xsS) (inline (zipWith (-) yhatS seS)) (inline (zipWith (+) yhatS seS)))
         <> layer (line (inline xsS) (inline yhatS))

  -- 残差診断 (平滑曲線 + 残差 vs fitted)。
  diagnosticPlots m =
    let res  = sfResult (splFit m)
        yhat = LA.toList (fittedV res)
        resd = LA.toList (residualsV res)
    in [ toPlot m
       , layer (scatter (inline yhat) (inline resd))
       ]

-- | grid 評価 (Phase 16 C1)。 grid x で基底行列を再構築し、 それを設計行列とみなして
-- 'confidenceBandAt' を評価する (基底空間の対称 Wald CI = 訓練 'confidenceBand' と同核)。
instance SingleVarModel SplineModel where
  svRange m = let xs = LA.toList (splXraw m) in (minimum xs, maximum xs)
  svGrid m level gxs =
    let fit        = splFit m
        basisTrain = splineBasisAt fit (splXraw m)
        basisGrid  = splineBasisAt fit (LA.fromList gxs)
        cib        = confidenceBandAt basisTrain (sfResult fit) level basisGrid
        los        = lowerBound cib
        his        = upperBound cib
        mu         = zipWith (\l h -> (l + h) / 2) los his
    in (mu, Just (los, his))
  -- PI = closed form σ̂²(1 + xᵀ(XᵀX)⁻¹x) (基底空間 OLS ゆえ LM と同型・statsmodels obs_ci 相当)。
  svGridPI m level gxs =
    let fit        = splFit m
        basisTrain = splineBasisAt fit (splXraw m)
        basisGrid  = splineBasisAt fit (LA.fromList gxs)
        pib        = predictionBandAt basisTrain (sfResult fit) level basisGrid
    in Just (lowerBound pib, upperBound pib)
  -- ブートストラップ: 加法誤差。 refit は同じ kind/knots で再 fit。
  svBootKit m =
    let fit = splFit m
        res = sfResult fit
    in Just BootKit
       { bkX = LA.toList (splXraw m)
       , bkY = zipWith (+) (LA.toList (fittedV res)) (LA.toList (residualsV res))
       , bkRefit = \xs ys -> splineModel (sfKind fit) (sfKnots fit) (LA.fromList xs) (LA.fromList ys)
       , bkObsDist = Nothing }

-- ===========================================================================
-- 一般化加法モデル (描画可能)
--
-- 'GAMFit' (Hanalyze.Model.GAM) は各特徴の基底係数 + fitted 'gamYHat' を保持する。
-- 本 Phase では mgcv 流 Bayesian CI を実装した平滑曲線 + CI 帯を描く。
-- ===========================================================================

instance Plottable GAMModel where
  -- 平滑曲線 + CI 帯 (Phase 70.6 G で mgcv 流 Bayesian CI を実装)。 grid 経路
  -- ('statModel') に固定し、 LM/spline と同様 band + line を出す。
  toPlot = toPlot . statModel

  -- 残差診断 (平滑曲線 + 残差 vs fitted)。
  diagnosticPlots m =
    let fit  = gamFit m
        yhat = LA.toList (gamYHat fit)
        resd = LA.toList (gamResid fit)
    in [ toPlot m
       , layer (scatter (inline yhat) (inline resd))
       ]

-- | GAM の grid 評価 (中心 μ̂ + **mgcv 流 Bayesian 信頼帯**)。 'predictGAMSE' の
--   pointwise se に t_{n−edf} 臨界値を掛けて帯にする (Vβ='gamCov')。
gamGridCI :: GAMFit -> Double -> [V.Vector Double] -> ([Double], Maybe ([Double], [Double]))
gamGridCI fit level cols =
  let (muV, seV) = predictGAMSE fit cols
      mu   = V.toList muV
      se   = V.toList seV
      df   = fromIntegral (LA.size (gamResid fit)) - gamEdf fit
      tVal = SD.quantile (studentT (max 1 df)) ((1 + level) / 2)
      lo   = zipWith (\u s -> u - tVal * s) mu se
      hi   = zipWith (\u s -> u + tVal * s) mu se
  in (mu, Just (lo, hi))

-- | grid 評価 (Phase 16 C1)。 grid x を 'predictGAMSE' に通し平滑曲線 + CI 帯を評価
-- (Phase 70.6 G: mgcv 流 Bayesian CI を実装)。
instance SingleVarModel GAMModel where
  svRange m = let xs = LA.toList (gamXraw m) in (minimum xs, maximum xs)
  svGrid m level gxs = gamGridCI (gamFit m) level [V.fromList gxs]


-- | 第1予測子を描画軸に、 他予測子は訓練平均に固定して偏依存曲線を評価する。
instance SingleVarModel GAMModelN where
  svRange m = case gamNXraws m of
    (x:_) -> let xs = LA.toList x in (minimum xs, maximum xs)
    []    -> (0, 1)
  svGrid m level gxs =
    let n          = length gxs
        others     = drop 1 (gamNXraws m)
        holdMean v = V.replicate n (LA.sumElements v / fromIntegral (LA.size v))
        cols       = V.fromList gxs : map holdMean others
    in gamGridCI (gamNFit m) level cols
  svCoefR2 m = Just ([gamIntercept (gamNFit m)], gamR2 (gamNFit m))

instance Plottable GAMModelN where
  -- 平滑曲線 + CI 帯 (Phase 70.6 G)。 grid 経路 ('statModel') に固定。 多予測子では
  -- 第1予測子を軸に他を訓練平均で固定した偏依存曲線 + その点の CI。
  toPlot = toPlot . statModel

-- ===========================================================================
-- カーネル回帰 (GP / KRR / RFF) の描画可能ラッパ
-- ===========================================================================

-- | grid 評価 (E2)。 予測子 'gprPredict' を grid x に当て、 分布あり象限 (Gp/GpRff) は
-- 事後分散→正規 credible 帯 (μ̂ ± z·σ)、 点象限 (Ridge/RidgeRff) は帯なし ('Nothing')。
-- 信頼水準 @level@ → @z = Φ⁻¹(1 − (1−level)/2)@ ('quantileNormal')。 'WeightedLMModel'
-- と同じく 'toPlot' を grid 経路 ('statModel') に固定する (元データ散布図と整合)。
instance SingleVarModel GPRegModel where
  svRange m = let xs = LA.toList (gprXraw m) in (minimum xs, maximum xs)
  svGrid m level gxs =
    let (mu, mbVar) = gprPredict m gxs
        z           = quantileNormal (1 - (1 - level) / 2)
    in case mbVar of
         Just vs -> let sds = map (sqrt . max 0) vs
                        los = zipWith (\u s -> u - z * s) mu sds
                        his = zipWith (\u s -> u + z * s) mu sds
                    in (mu, Just (los, his))
         Nothing -> (mu, Nothing)               -- Ridge 系 = 帯なし
  -- 予測区間 (PI) = 事後予測分散 (f の分散 + 観測ノイズ σ_n²) の正規帯。 分布あり象限のみ。
  svGridPI m level gxs =
    let (mu, mbVar) = gprPredict m gxs
        z           = quantileNormal (1 - (1 - level) / 2)
        sn2         = max 0 (gpNoiseVar (gprParams m))
    in case mbVar of
         Just vs -> let sds = map (\v -> sqrt (max 0 v + sn2)) vs
                    in Just ( zipWith (\u s -> u - z * s) mu sds
                            , zipWith (\u s -> u + z * s) mu sds )
         Nothing -> Nothing
  -- カーネル回帰は β₀+β₁x の線形「式」を持たないため式/R² 注釈は出さない。
  svCoefR2 _ = Nothing

-- | ★訓練点経路ではなく grid 経路 ('statModel') に固定 (元データ散布図と整合)。
-- 分布あり象限は曲線 + credible 帯、 点象限は曲線のみ。
instance Plottable GPRegModel where
  toPlot = toPlot . statModel


-- | 第1予測子を描画軸に、 他予測子を訓練平均に固定した偏依存曲線 (band は分布あり象限のみ)。
instance SingleVarModel GPRegModelN where
  svRange m = case gprnXraws m of
    (x:_) -> let xs = LA.toList x in (minimum xs, maximum xs)
    []    -> (0, 1)
  svGrid m level gxs =
    let n          = length gxs
        others     = drop 1 (gprnXraws m)
        holdMean v = LA.konst (LA.sumElements v / fromIntegral (LA.size v)) n
        testX      = LA.fromColumns (LA.fromList gxs : map holdMean others)
        (mu, mbVar) = gprnPredict m testX
        z          = quantileNormal (1 - (1 - level) / 2)
    in case mbVar of
         Just vs -> let sds = map (sqrt . max 0) vs
                    in (mu, Just ( zipWith (\u s -> u - z * s) mu sds
                                 , zipWith (\u s -> u + z * s) mu sds ))
         Nothing -> (mu, Nothing)
  svGridPI m level gxs =
    let n          = length gxs
        others     = drop 1 (gprnXraws m)
        holdMean v = LA.konst (LA.sumElements v / fromIntegral (LA.size v)) n
        testX      = LA.fromColumns (LA.fromList gxs : map holdMean others)
        (mu, mbVar) = gprnPredict m testX
        z          = quantileNormal (1 - (1 - level) / 2)
        sn2        = max 0 (gpNoiseVar (gprnParams m))
    in case mbVar of
         Just vs -> let sds = map (\v -> sqrt (max 0 v + sn2)) vs
                    in Just ( zipWith (\u s -> u - z * s) mu sds
                            , zipWith (\u s -> u + z * s) mu sds )
         Nothing -> Nothing
  svCoefR2 _ = Nothing

instance Plottable GPRegModelN where
  toPlot = toPlot . statModel

-- ===========================================================================
-- 多出力 GP (描画可能)
-- ===========================================================================

-- | 多出力 GP の予測曲線 + 95% band (出力ごとに色分け・x = 予測点 index)。
multiGpCurves :: MultiGPResult -> VisualSpec
multiGpCurves res =
  let outs = zip3 (mgpMean res) (mgpLower res) (mgpUpper res)
      mkOut k (m, lo, hi) =
        let xs  = [ fromIntegral i | i <- [1 .. length m] ] :: [Double]
            lbl = "y" <> T.pack (show (k :: Int))
            grp = inlineCat (replicate (length m) lbl)
        in layer (band (inline xs) (inline lo) (inline hi) <> colorBy grp <> alpha 0.2)
             <> layer (line (inline xs) (inline m) <> colorBy grp)
  in mconcat (zipWith mkOut [0 ..] outs)

instance Plottable MultiGPResult where
  toPlot = multiGpCurves

-- ===========================================================================
-- 実測 vs 予測 (HasObsPred) — Phase 72.4
--
-- spline は内側の線形 fit ('sfResult') から、 GAM は保持する ŷ/残差から復元する。
-- ===========================================================================

instance HasObsPred SplineModel where
  obsPredPairs m =
    let r = sfResult (splFit m)
        f = LA.toList (fittedV r)
        e = LA.toList (residualsV r)
    in (zipWith (+) f e, f)

instance HasObsPred GAMModel where
  obsPredPairs m =
    let f = LA.toList (gamYHat (gamFit m))
        e = LA.toList (gamResid (gamFit m))
    in (zipWith (+) f e, f)

instance HasObsPred GAMModelN where
  obsPredPairs m =
    let f = LA.toList (gamYHat (gamNFit m))
        e = LA.toList (gamResid (gamNFit m))
    in (zipWith (+) f e, f)
