{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | 'Viz.ReportBuilder.Reportable' のインスタンスを提供するモジュール。
--
-- ライブラリ利用者は、フィット結果を直接 'renderReport' に渡せる:
--
-- @
-- import Model.Regularized
-- import Viz.ReportBuilder
-- import Viz.ReportInstances ()
--
-- main = do
--   let fit = fitRegularized (L2 0.1) xMat yVec
--       cfg = defaultReportConfig "Ridge demo"
--   renderReport "out.html" cfg (toReport cfg df ["x"] "y" fit)
-- @
--
-- 提供されるインスタンス:
-- - 'RegFit'         (Model.Regularized) — 正則化線形回帰
-- - 'SplineFit'      (Model.Spline)      — B-spline / Natural cubic
-- - 'KernelRidgeFit' (Model.Kernel)      — Kernel Ridge regression
-- - 'RFFRidgeFit'    (Model.RFF)         — Random Fourier Features Ridge
-- - 'RobustGPFit'    (Model.GPRobust)    — ロバスト GP
--
-- LM/GLM/GLMM/GP/HBM は当面 'Viz.AnalysisReport' (非推奨) 経由。
-- ReportBuilder 化が次の課題。
module Viz.ReportInstances () where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Numeric.LinearAlgebra as LA
import Text.Printf (printf)

import DataFrame.Core (DataFrame, getNumeric)
import Model.Regularized (RegFit (..), Penalty (..), predictRegularized)
import Model.Spline     (SplineFit (..), SplineKind (..), predictSpline, sfBeta)
import Model.Kernel     (KernelRidgeFit (..), predictKernelRidge)
import Model.RFF        (RFFRidgeFit (..), predictRFFRidge, rffrFeatures,
                         rffSigmaF, rffLengthScale, rffOmegas)
import Model.GP         (GPParams (..))
import Model.GPRobust   (RobustGPFit (..), RobustLikelihood (..))
import Viz.ReportBuilder

-- ---------------------------------------------------------------------------
-- 内部ユーティリティ
-- ---------------------------------------------------------------------------

-- | x グリッド (データの min/max から 100 点)。
xGridFromVec :: V.Vector Double -> [Double]
xGridFromVec v
  | V.null v  = []
  | otherwise =
      let lo = V.minimum v
          hi = V.maximum v
      in [ lo + fromIntegral i * (hi - lo) / 99 | i <- [0 .. 99 :: Int] ]

-- | DataFrame から x 列 (1 つ) を numeric vector で取り出す。
firstNumericVec :: [Text] -> DataFrame -> Maybe (V.Vector Double)
firstNumericVec []     _  = Nothing
firstNumericVec (c:_)  df = getNumeric c df

penaltyName :: Penalty -> Text
penaltyName p = case p of
  NoPen          -> "OLS"
  L2 _           -> "Ridge (L2)"
  L1 _           -> "Lasso (L1)"
  ElasticNet _ _ -> "Elastic Net"

penaltyKVs :: Penalty -> [(Text, Text)]
penaltyKVs p = case p of
  NoPen           -> [("Penalty", "OLS")]
  L2 lam          -> [("Penalty", "L2 (Ridge)"), ("λ", T.pack (printf "%g" lam))]
  L1 lam          -> [("Penalty", "L1 (Lasso)"), ("λ", T.pack (printf "%g" lam))]
  ElasticNet l1 l2 ->
    [ ("Penalty", "ElasticNet")
    , ("λ₁ (L1)", T.pack (printf "%g" l1))
    , ("λ₂ (L2)", T.pack (printf "%g" l2))
    ]

splineKindName :: SplineKind -> Text
splineKindName (BSpline k) = "B-spline (degree " <> T.pack (show k) <> ")"
splineKindName NaturalCubic = "Natural cubic spline"

-- ---------------------------------------------------------------------------
-- RegFit (Regularized)
-- ---------------------------------------------------------------------------

instance Reportable RegFit where
  toReport _cfg df xCols yCol fit =
    let beta     = LA.toList (rfBeta fit)
        labels   = "intercept" : xCols
        coeffs   = zip labels beta
        nonZero  = rfNonZero fit
        n        = length beta
        modelLbl = penaltyName (rfPenalty fit)
        formula  = yCol <> " ~ "
                   <> T.intercalate " + " ("β₀" : xCols)
        residuals = LA.toList (rfResid fit)
        fitted    = LA.toList (rfYHat fit)

        -- 1 変数なら scatter + fit
        scatterSec = case (xCols, firstNumericVec xCols df,
                           getNumeric yCol df) of
          ([xc1], Just xVec, Just yVec) ->
            let grid = xGridFromVec xVec
                gridMat = LA.fromColumns
                            [ LA.konst 1 (length grid)
                            , LA.fromList grid ]
                gridY = LA.toList (predictRegularized fit gridMat)
                smooth = SmoothCurve grid gridY [] []
                _ = xc1
            in [secFitScatter xc1 yCol (V.toList xVec) (V.toList yVec)
                  (Just smooth)]
          _ -> []
    in [ secDataOverview df xCols yCol
       , secModelOverview modelLbl formula Nothing
       , secCoefficients coeffs (Just ("R²", rfR2 fit))
       , secKeyValue "Fit summary" $
           penaltyKVs (rfPenalty fit) ++
           [ ("|β| > 1e-8",
              T.pack (show nonZero) <> " / " <> T.pack (show n))
           ]
       ] ++ scatterSec ++
       [ secResiduals fitted residuals ]

-- ---------------------------------------------------------------------------
-- SplineFit
-- ---------------------------------------------------------------------------

instance Reportable SplineFit where
  toReport _cfg df xCols yCol fit =
    case (xCols, firstNumericVec xCols df, getNumeric yCol df) of
      ([xc], Just xVec, Just yVec) ->
        let kindLbl = splineKindName (sfKind fit)
            grid    = xGridFromVec xVec
            gridY   = V.toList (predictSpline fit (V.fromList grid))
            smooth  = SmoothCurve grid gridY [] []
            ys      = V.toList yVec
            yhat    = V.toList (predictSpline fit xVec)
            beta    = LA.toList (sfBeta fit)
            knots   = sfKnots fit
            formula = yCol <> " ~ s(" <> xc <> "; " <> T.pack (show (length knots))
                      <> " knots)"
        in [ secDataOverview df [xc] yCol
           , secModelOverview kindLbl formula Nothing
           , secKeyValue "Fit summary"
               [ ("Kind",  kindLbl)
               , ("Knots", T.pack (show (length knots)))
               , ("Coefficients", T.pack (show (length beta)))
               ]
           , secFitScatter xc yCol (V.toList xVec) ys (Just smooth)
           , secResiduals yhat (zipWith (-) ys yhat)
           ]
      _ -> [secDataOverview df xCols yCol
           , secModelOverview "Spline" "(needs single numeric x and y)" Nothing
           ]

-- ---------------------------------------------------------------------------
-- KernelRidgeFit
-- ---------------------------------------------------------------------------

instance Reportable KernelRidgeFit where
  toReport _cfg df xCols yCol fit =
    case (xCols, firstNumericVec xCols df, getNumeric yCol df) of
      ([xc], Just xVec, Just yVec) ->
        let grid    = xGridFromVec xVec
            gridV   = V.fromList grid
            gridY   = V.toList (predictKernelRidge fit gridV)
            smooth  = SmoothCurve grid gridY [] []
            ys      = V.toList yVec
            yhat    = V.toList (predictKernelRidge fit xVec)
            formula = yCol <> " ~ K_h(" <> xc <> ", ·)ᵀ α"
        in [ secDataOverview df [xc] yCol
           , secModelOverview "Kernel Ridge regression" formula Nothing
           , secKeyValue "Fit summary"
               [ ("Kernel",    T.pack (show (krKernel fit)))
               , ("Bandwidth", T.pack (printf "%.4f" (krH fit)))
               , ("Lambda",    T.pack (printf "%g" (krLambda fit)))
               , ("Train size",T.pack (show (V.length (krXs fit))))
               ]
           , secFitScatter xc yCol (V.toList xVec) ys (Just smooth)
           , secResiduals yhat (zipWith (-) ys yhat)
           ]
      _ -> [secDataOverview df xCols yCol
           , secModelOverview "Kernel Ridge" "(needs single numeric x and y)"
                              Nothing
           ]

-- ---------------------------------------------------------------------------
-- RFFRidgeFit
-- ---------------------------------------------------------------------------

instance Reportable RFFRidgeFit where
  toReport _cfg df xCols yCol fit =
    case (xCols, firstNumericVec xCols df, getNumeric yCol df) of
      ([xc], Just xVec, Just yVec) ->
        let feats   = rffrFeatures fit
            grid    = xGridFromVec xVec
            gridY   = predictRFFRidge fit grid
            smooth  = SmoothCurve grid gridY [] []
            ys      = V.toList yVec
            yhat    = predictRFFRidge fit (V.toList xVec)
            d       = V.length (rffOmegas feats)
            formula = yCol <> " ~ φ(" <> xc <> ")ᵀ w   (D=" <> T.pack (show d) <> ")"
            ellLbl  = T.pack (printf "%.4f" (rffLengthScale feats))
            sfLbl   = T.pack (printf "%.4f" (rffSigmaF feats))
        in [ secDataOverview df [xc] yCol
           , secModelOverview "RFF Ridge regression" formula Nothing
           , secKeyValue "Fit summary"
               [ ("Features (D)", T.pack (show d))
               , ("Length scale ℓ", ellLbl)
               , ("Signal σ_f",     sfLbl)
               , ("Lambda",         T.pack (printf "%g" (rffrLambda fit)))
               ]
           , secFitScatter xc yCol (V.toList xVec) ys (Just smooth)
           , secResiduals yhat (zipWith (-) ys yhat)
           ]
      _ -> [secDataOverview df xCols yCol
           , secModelOverview "RFF Ridge" "(needs single numeric x and y)"
                              Nothing
           ]

-- ---------------------------------------------------------------------------
-- RobustGPFit
-- ---------------------------------------------------------------------------

instance Reportable RobustGPFit where
  toReport _cfg df xCols yCol fit =
    let likLbl = case rgpLik fit of
          RGaussian s -> "Gaussian (σ_n=" <> T.pack (printf "%.3f" s) <> ")"
          RStudentT nu s -> "StudentT (ν=" <> T.pack (printf "%g" nu)
                            <> ", σ=" <> T.pack (printf "%.3f" s) <> ")"
          RCauchy g      -> "Cauchy (γ=" <> T.pack (printf "%.3f" g) <> ")"
        params = rgpParams fit
        formula = yCol <> " | f ~ " <> likLbl
                  <> ",   f ~ GP(0, K(" <> T.intercalate "," xCols <> "))"
    in [ secDataOverview df xCols yCol
       , secModelOverview "Robust Gaussian Process" formula Nothing
       , secKeyValue "Fit summary"
           [ ("Kernel", T.pack (show (rgpKernel fit)))
           , ("Likelihood", likLbl)
           , ("Length scale", T.pack (printf "%.4f" (gpLengthScale params)))
           , ("Signal σ_f²", T.pack (printf "%.4f" (gpSignalVar params)))
           , ("IRLS iterations", T.pack (show (rgpIters fit)))
           , ("Train size", T.pack (show (length (rgpTrainX fit))))
           ]
       ]
