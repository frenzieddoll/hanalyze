{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | 'Viz.ReportBuilder.Reportable' instances for the various fit types.
--
-- Importing this module (purely for its instances) lets a user pass any
-- supported fit result directly to 'renderReport':
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
module Viz.ReportInstances
  ( LMReport (..)
  , GLMReport (..)
  , RFReport (..)
  , GLMMReport (..)
  , GPReport (..)
  , HBMLinearReport (..)
  , HBMReport (..)
  , HBMRibbon (..)
  , RFFMVReport (..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (sortBy)
import qualified Data.Vector as V
import qualified Numeric.LinearAlgebra as LA
import Text.Printf (printf)

import qualified DataFrame.Internal.DataFrame as DXD
import DataIO.Convert (getDoubleVec, getMaybeTextVec)
import qualified Numeric.LinearAlgebra as LA2
import qualified Stat.Standardize as Std
import qualified Stat.NumberFormat as NF
import Viz.Scatter   (scatterWithGroups)
import Viz.Core      (defaultConfig, PlotConfig (..))
import Model.Core      (FitResult, coeffList, fittedList, residualsV, rSquared1)
import Model.LM        (SmoothFit (..))
import Model.GLM       (Family (..), LinkFn (..))
import Model.Regularized (RegFit (..), Penalty (..), predictRegularized)
import Model.Spline     (SplineFit (..), SplineKind (..), predictSpline, sfBeta)
import Model.Kernel     (KernelRidgeFit (..), predictKernelRidge)
import Model.RFF        (RFFRidgeFit (..), predictRFFRidge, rffrFeatures,
                         rffSigmaF, rffLengthScale, rffOmegas,
                         RFFRidgeFitMV (..), RFFFeaturesMV (..),
                         predictRFFRidgeMV)
import Model.GP         (GPParams (..))
import Model.GPRobust   (RobustGPFit (..), RobustLikelihood (..))
import Model.Quantile   (QRFit (..))
import Model.GAM        (GAMFit (..), predictGAMComponent)
import Model.RandomForest (RandomForest (..), featureImportance)
import qualified Model.GLMM as GLMM
import qualified Model.GP   as GP
import qualified MCMC.Core  as MC
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
firstNumericVec :: [Text] -> DXD.DataFrame -> Maybe (V.Vector Double)
firstNumericVec []     _  = Nothing
firstNumericVec (c:_)  df = getDoubleVec c df

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
                           getDoubleVec yCol df) of
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
    case (xCols, firstNumericVec xCols df, getDoubleVec yCol df) of
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
    case (xCols, firstNumericVec xCols df, getDoubleVec yCol df) of
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
    case (xCols, firstNumericVec xCols df, getDoubleVec yCol df) of
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

-- ---------------------------------------------------------------------------
-- LM / GLM (axis-1 C, Phase 1)
-- ---------------------------------------------------------------------------

-- | LM レポート用ラッパ。
--
-- 単変数 LM の `Reportable` instance に必要な情報を集約。
-- `lmrSmooth = Just sf` を渡すと信頼帯付き滑らか曲線を散布図に重ね描く。
--
-- 多変数 (xCols が 2 つ以上) の場合は scatter+smooth は省略され、
-- `secInteractiveMulti` が主軸 dropdown + 副軸 slider で予測点を表示する。
data LMReport = LMReport
  { lmrFit    :: FitResult
  , lmrSmooth :: Maybe SmoothFit
  } deriving Show

-- | GLM レポート用ラッパ。
data GLMReport = GLMReport
  { glmrFit    :: FitResult
  , glmrFamily :: Family
  , glmrLink   :: LinkFn
  , glmrSmooth :: Maybe SmoothFit
  } deriving Show

linkLabel :: LinkFn -> Text
linkLabel Identity = "identity"
linkLabel Log      = "log"
linkLabel Logit    = "logit"
linkLabel Sqrt     = "sqrt"

familyLabel :: Family -> Text
familyLabel Gaussian = "Gaussian"
familyLabel Binomial = "Binomial"
familyLabel Poisson  = "Poisson"

-- | 残差から σ_hat / RMSE / max|r| を作る。
residStats :: [Double] -> Int -> (Double, Double, Double)
residStats resid p =
  let n        = length resid
      sumSq    = sum [ r * r | r <- resid ]
      sigmaHat = sqrt (sumSq / fromIntegral (max 1 (n - p)))
      rmse     = sqrt (sumSq / fromIntegral (max 1 n))
      maxAbs   = maximum (0 : map abs resid)
  in (sigmaHat, rmse, maxAbs)

-- | smoothFit → SmoothCurve への変換 (空 Smooth は空カーブ)。
smoothFitToCurve :: Maybe SmoothFit -> SmoothCurve
smoothFitToCurve Nothing   = SmoothCurve [] [] [] []
smoothFitToCurve (Just sf) = SmoothCurve (sfX sf) (sfFit sf) (sfLower sf) (sfUpper sf)

-- | xCols + xVecs から InteractiveModel を構築 (LM/GLM 共通)。
mkInteractive :: [Text] -> Text -> [V.Vector Double] -> [Double]
              -> Double -> [Double] -> Text -> Maybe Double
              -> InteractiveModel
mkInteractive xCols yCol xVecs ys b0 betas link mSigma =
  let n        = length ys
      xRows    = [ [ xv V.! i | xv <- xVecs ] | i <- [0 .. n - 1] ]
      mkSlider xv =
        let lo = if V.null xv then 0 else V.minimum xv
            hi = if V.null xv then 1 else V.maximum xv
            ext = (hi - lo) * 0.5
        in (lo - ext, (lo + hi) / 2, hi + ext)
  in InteractiveModel
       { imXCols     = xCols
       , imYCol      = yCol
       , imXValues   = xRows
       , imYValues   = ys
       , imIntercept = b0
       , imBetas     = betas
       , imLink      = link
       , imSlider    = map mkSlider xVecs
       , imCISigma   = mSigma
       }

-- | 数式: y = β₀ + β₁ x_1 + ... + β_p x_p
linearFormula :: Text -> [Text] -> Text
linearFormula yCol xCols =
  yCol <> " ~ "
       <> T.intercalate " + "
            ("β₀" : [ "β" <> T.pack (show (i :: Int)) <> " · " <> x
                   | (i, x) <- zip [1 ..] xCols ])

instance Reportable LMReport where
  toReport _cfg df xCols yCol (LMReport fit mSmooth) =
    let beta    = coeffList fit
        coefLabels = "β₀ (intercept)"
                   : [ "β" <> T.pack (show (i :: Int)) <> " (" <> x <> ")"
                     | (i, x) <- zip [1 ..] xCols ]
        coeffs   = zip coefLabels beta
        fitted   = fittedList fit
        resid    = LA.toList (residualsV fit)
        p        = length beta
        (sigmaH, rmse, maxAbs) = residStats resid p

        xVecs    = [ v | c <- xCols, Just v <- [getDoubleVec c df] ]
        yVecMb   = getDoubleVec yCol df

        smoothC  = smoothFitToCurve mSmooth

        scatterCard = case (xCols, xVecs, yVecMb) of
          ([xc], [xv], Just yv)
            | length xVecs == length xCols ->
                [ secCard "散布図 + 回帰線"
                    [ secFitScatter xc yCol (V.toList xv) (V.toList yv)
                        (Just smoothC) ] ]
          _ -> []

        interactiveSec
          | length xVecs == length xCols, not (null xVecs)
          , Just yv <- yVecMb =
              let im = mkInteractive xCols yCol xVecs (V.toList yv)
                                     (head beta) (drop 1 beta)
                                     "identity" (Just sigmaH)
              in [secInteractiveMulti "対話的予測" im]
          | otherwise = []

        formula =
          "$" <> linearFormula yCol xCols <> "$<br>"
          <> "$\\varepsilon_i \\sim \\text{Normal}(0, \\sigma^2)$"

        statRow =
          secStatRow
            [ ("R²",         T.pack (printf "%.4f" (rSquared1 fit)))
            , ("方法",       "OLS (QR)")
            , ("σ_hat",      T.pack (printf "%.4f" sigmaH))
            , ("RMSE",       T.pack (printf "%.4f" rmse))
            , ("最大絶対残差", T.pack (printf "%.4f" maxAbs))
            ]

        resultSec =
          secCollapsible "<span class=\"sec-icon\">&#128200;</span> 回帰結果" True
            ([ statRow
             , secCard "係数" [secCoefficients coeffs (Just ("R²", rSquared1 fit))]
             ]
             ++ scatterCard
             ++ [ secCard "残差プロット" [secResiduals fitted resid] ])

    in [ secDataOverview df xCols yCol
       , secModelOverview "LM" formula Nothing
       , resultSec
       ] ++ interactiveSec

instance Reportable GLMReport where
  toReport _cfg df xCols yCol (GLMReport fit fam lk mSmooth) =
    let beta    = coeffList fit
        coefLabels = "β₀ (intercept)"
                   : [ "β" <> T.pack (show (i :: Int)) <> " (" <> x <> ")"
                     | (i, x) <- zip [1 ..] xCols ]
        coeffs   = zip coefLabels beta
        fitted   = fittedList fit
        resid    = LA.toList (residualsV fit)
        p        = length beta
        (sigmaH, rmse, maxAbs) = residStats resid p

        xVecs    = [ v | c <- xCols, Just v <- [getDoubleVec c df] ]
        yVecMb   = getDoubleVec yCol df

        smoothC  = smoothFitToCurve mSmooth

        modelType = "GLM(" <> familyLabel fam <> ")"
        linkTxt   = linkLabel lk

        formula = case fam of
          Poisson  -> "$" <> yCol <> "_i \\sim \\text{Poisson}(\\lambda_i)$<br>"
                      <> "$\\log \\lambda_i = "
                      <> T.intercalate " + "
                           ("\\beta_0" : [ "\\beta_" <> T.pack (show (i :: Int))
                                            <> " " <> x <> "_i"
                                          | (i, x) <- zip [1 ..] xCols ])
                      <> "$"
          Binomial -> "$" <> yCol <> "_i \\sim \\text{Binomial}(n_i, p_i)$<br>"
                      <> "$\\text{logit}(p_i) = \\beta_0 + \\sum \\beta_j x_{ij}$"
          Gaussian -> "$" <> linearFormula yCol xCols <> "$<br>"
                      <> "$\\varepsilon_i \\sim \\text{Normal}(0, \\sigma^2)$"

        scatterCard = case (xCols, xVecs, yVecMb) of
          ([xc], [xv], Just yv)
            | length xVecs == length xCols ->
                [ secCard "散布図 + 回帰線"
                    [ secFitScatter xc yCol (V.toList xv) (V.toList yv)
                        (Just smoothC) ] ]
          _ -> []

        interactiveSec
          | length xVecs == length xCols, not (null xVecs)
          , Just yv <- yVecMb =
              let im = mkInteractive xCols yCol xVecs (V.toList yv)
                                     (head beta) (drop 1 beta)
                                     linkTxt Nothing
              in [secInteractiveMulti "対話的予測" im]
          | otherwise = []

        r2Label = case fam of
          Gaussian -> "R²"
          _        -> "McFadden R²"

        statRow =
          secStatRow
            [ (r2Label,     T.pack (printf "%.4f" (rSquared1 fit)))
            , ("方法",       "IRLS")
            , ("σ_hat",      T.pack (printf "%.4f" sigmaH))
            , ("RMSE",       T.pack (printf "%.4f" rmse))
            , ("最大絶対残差", T.pack (printf "%.4f" maxAbs))
            ]

        resultSec =
          secCollapsible "<span class=\"sec-icon\">&#128200;</span> 回帰結果" True
            ([ statRow
             , secCard "係数" [secCoefficients coeffs (Just (r2Label, rSquared1 fit))]
             ]
             ++ scatterCard
             ++ [ secCard "残差プロット" [secResiduals fitted resid] ])

    in [ secDataOverview df xCols yCol
       , secModelOverviewLink modelType formula linkTxt Nothing
       , resultSec
       ] ++ interactiveSec

-- ---------------------------------------------------------------------------
-- Quantile Regression (axis-1 B)
-- ---------------------------------------------------------------------------

instance Reportable QRFit where
  toReport _cfg df xCols yCol fit =
    let beta    = LA.toList (qfBeta fit)
        coefLabels = "intercept"
                   : [ "β" <> T.pack (show (i :: Int)) <> " (" <> x <> ")"
                     | (i, x) <- zip [1 ..] xCols ]
        coeffs   = zip coefLabels beta
        fitted   = LA.toList (qfYHat fit)
        resid    = LA.toList (qfResid fit)
        p        = length beta
        (_sigmaH, rmse, maxAbs) = residStats resid p
        tau      = qfTau fit
        formula  = "$Q_{\\tau=" <> T.pack (printf "%.2f" tau)
                   <> "}(" <> yCol <> " | x) = "
                   <> T.intercalate " + "
                        ("\\beta_0" : [ "\\beta_" <> T.pack (show (i :: Int))
                                          <> " " <> x
                                       | (i, x) <- zip [1 ..] xCols ])
                   <> "$"
        statRow =
          secStatRow
            [ ("τ",            T.pack (printf "%.2f" tau))
            , ("Pseudo R¹",    T.pack (printf "%.4f" (qfR1 fit)))
            , ("Pinball loss", T.pack (printf "%.4f" (qfPinball fit)))
            , ("反復",         T.pack (show (qfIters fit)))
            , ("RMSE",         T.pack (printf "%.4f" rmse))
            , ("最大絶対残差", T.pack (printf "%.4f" maxAbs))
            ]
        scatterCard = case (xCols, firstNumericVec xCols df, getDoubleVec yCol df) of
          ([xc], Just xv, Just yv) ->
            -- 単変数: yHat を x ソート順で線として描く
            let pairs = zip (V.toList xv) fitted
                sorted = sortByFst pairs
                smooth = SmoothCurve (map fst sorted) (map snd sorted) [] []
            in [ secCard "散布図 + 推定 τ-分位点線"
                   [ secFitScatter xc yCol (V.toList xv) (V.toList yv)
                       (Just smooth) ] ]
          _ -> []
        resultSec =
          secCollapsible "<span class=\"sec-icon\">&#128200;</span> 回帰結果" True
            ([ statRow
             , secCard "係数" [secCoefficients coeffs (Just ("Pseudo R¹", qfR1 fit))]
             ]
             ++ scatterCard
             ++ [ secCard "残差プロット" [secResiduals fitted resid] ])
    in [ secDataOverview df xCols yCol
       , secModelOverview "Quantile Regression" formula Nothing
       , resultSec
       ]

sortByFst :: Ord a => [(a, b)] -> [(a, b)]
sortByFst = sortBy (\(a, _) (b, _) -> compare a b)

-- ---------------------------------------------------------------------------
-- GAM (axis-1 B)
-- ---------------------------------------------------------------------------

instance Reportable GAMFit where
  toReport _cfg df xCols yCol fit =
    let fitted = LA.toList (gamYHat fit)
        resid  = LA.toList (gamResid fit)
        n      = length fitted
        p      = sum [ LA.size b | b <- gamBetas fit ]
        (_sigmaH, rmse, maxAbs) = residStats resid p
        statRow =
          secStatRow
            [ ("R²",        T.pack (printf "%.4f" (gamR2 fit)))
            , ("Degree",    T.pack (show (gamDegree fit)))
            , ("Knots",     T.pack (show (length (head (gamKnots fit ++ [[]])))))
            , ("λ (Ridge)", T.pack (printf "%g" (gamLambda fit)))
            , ("RMSE",      T.pack (printf "%.4f" rmse))
            , ("最大絶対残差", T.pack (printf "%.4f" maxAbs))
            ]
        formula = "$" <> yCol <> "_i = \\beta_0 + \\sum_j s_j("
                  <> T.intercalate ", " xCols <> ")_i + \\varepsilon_i$"

        -- 各特徴の partial effect: s_j(x_j) を smooth として可視化
        partialCards =
          [ let mxVec = getDoubleVec x df
            in case mxVec of
                 Just xv ->
                   let xsRaw = V.toList xv
                       sorted = sortByFst (zip xsRaw [0 :: Int ..])
                       xsS    = map fst sorted
                       grid   = V.fromList xsS
                       sjV    = predictGAMComponent fit (j - 1) grid
                       sjList = V.toList sjV
                       partRes = [ resid !! i + (sjList !! k)
                                 | (k, (_, i)) <- zip [0 ..] sorted ]
                       smooth = SmoothCurve xsS sjList [] []
                   in secCard ("Partial effect: s(" <> x <> ")")
                        [ secFitScatter x ("s(" <> x <> ")")
                            xsS partRes (Just smooth) ]
                 Nothing -> secMarkdown ("Partial effect: " <> x)
                              ("(列 " <> x <> " が DataFrame に見つかりません)")
          | (j, x) <- zip [1 :: Int ..] xCols, n > 0 ]

        resultSec =
          secCollapsible "<span class=\"sec-icon\">&#128200;</span> 回帰結果" True
            ([ statRow ]
             ++ partialCards
             ++ [ secCard "残差プロット" [secResiduals fitted resid] ])
    in [ secDataOverview df xCols yCol
       , secModelOverview "GAM" formula Nothing
       , resultSec
       ]

-- ---------------------------------------------------------------------------
-- Random Forest (axis-1 B)
-- ---------------------------------------------------------------------------

-- | Random Forest レポート用ラッパ。
--
-- `RandomForest` 自体は yHat を保持しないため、ユーザーが学習データに対する
-- 予測値と R² を別途計算して渡す。
data RFReport = RFReport
  { rfrModel :: RandomForest
  , rfrYHat  :: V.Vector Double   -- ^ 学習データに対する予測値
  , rfrYObs  :: V.Vector Double   -- ^ 学習データの観測値 (R² 計算用)
  } deriving Show

instance Reportable RFReport where
  toReport _cfg df xCols yCol (RFReport rf yHatV yObsV) =
    let yHat   = V.toList yHatV
        yObs   = V.toList yObsV
        resid  = zipWith (-) yObs yHat
        n      = length yObs
        meanY  = if n == 0 then 0 else sum yObs / fromIntegral n
        ssTot  = sum [ (y - meanY) ^ (2 :: Int) | y <- yObs ]
        ssRes  = sum [ r * r | r <- resid ]
        r2     = if ssTot > 0 then 1 - ssRes / ssTot else 0
        (_sigmaH, rmse, maxAbs) = residStats resid 1

        importVec = featureImportance rf
        importPairs =
          [ (lbl, importVec V.! (i - 1))
          | (i, lbl) <- zip [1 ..] xCols
          , i - 1 < V.length importVec ]

        formula = "$\\hat{y}(x) = \\frac{1}{T} \\sum_{t=1}^{T} \\text{Tree}_t(x)$ "
                  <> "(T = bagged regression trees)"

        statRow =
          secStatRow
            [ ("R² (train)",  T.pack (printf "%.4f" r2))
            , ("Trees",       T.pack (show (length (rfTreesV rf))))
            , ("Features",    T.pack (show (rfNFeatures rf)))
            , ("RMSE",        T.pack (printf "%.4f" rmse))
            , ("最大絶対残差", T.pack (printf "%.4f" maxAbs))
            ]

        importanceCard =
          secCard "Feature importance" [ secFeatureImportance "" importPairs ]

        resultSec =
          secCollapsible "<span class=\"sec-icon\">&#128200;</span> 回帰結果" True
            [ statRow
            , importanceCard
            , secCard "残差プロット" [secResiduals yHat resid]
            ]
    in [ secDataOverview df xCols yCol
       , secModelOverview "Random Forest (regression)" formula Nothing
       , resultSec
       ]

-- ---------------------------------------------------------------------------
-- GLMM (axis-1 C, Phase A残)
-- ---------------------------------------------------------------------------

-- | GLMM (LME / non-Gaussian GLMM) レポート用ラッパ。
data GLMMReport = GLMMReport
  { glmmrResult   :: GLMM.GLMMResult
  , glmmrFamily   :: Family
  , glmmrLink     :: LinkFn
  , glmmrGroupCol :: Text
  } deriving Show

instance Reportable GLMMReport where
  toReport _cfg df xCols yCol (GLMMReport gr fam lk grpCol) =
    let fixed   = GLMM.glmmFixed gr
        beta    = coeffList fixed
        coefLabels = "β₀ (intercept)"
                   : [ "β" <> T.pack (show (i :: Int)) <> " (" <> x <> ")"
                     | (i, x) <- zip [1 ..] xCols ]
        coeffs   = zip coefLabels beta
        fitted   = fittedList fixed
        resid    = LA.toList (residualsV fixed)
        p        = length beta
        (_sigmaH, rmse, maxAbs) = residStats resid p

        groups   = V.toList (GLMM.glmmGroups gr)
        blups    = V.toList (GLMM.glmmBLUPs  gr)
        blupRows = [ [g, T.pack (printf "%+.4f" u)] | (g, u) <- zip groups blups ]

        xVecs    = [ v | c <- xCols, Just v <- [getDoubleVec c df] ]
        yVecMb   = getDoubleVec yCol df

        modelType = case fam of
          Gaussian -> "LME (linear mixed effects)"
          _        -> "GLMM(" <> familyLabel fam <> ")"
        linkTxt  = linkLabel lk

        formula =
          "$" <> yCol <> "_{ij} = \\beta_0 + \\sum \\beta_j x_{ij} + u_j "
          <> "+ \\varepsilon_{ij}$<br>"
          <> "$u_j \\sim \\text{Normal}(0, \\sigma^2_u),\\quad "
          <> "\\varepsilon_{ij} \\sim \\text{Normal}(0, \\sigma^2)$"

        interactiveSec
          | length xVecs == length xCols, not (null xVecs)
          , Just yv <- yVecMb =
              let im = mkInteractive xCols yCol xVecs (V.toList yv)
                                     (head beta) (drop 1 beta)
                                     linkTxt (Just (sqrt (GLMM.glmmResidVar gr)))
              in [secInteractiveMulti
                    "対話的予測 (固定効果のみ、ランダム効果 = 0)" im]
          | otherwise = []

        statRow =
          secStatRow
            [ ("周辺 R²",  T.pack (printf "%.4f" (rSquared1 fixed)))
            , ("σ²_u",     T.pack (printf "%.4f" (GLMM.glmmRandVar gr)))
            , ("σ²",       T.pack (printf "%.4f" (GLMM.glmmResidVar gr)))
            , ("ICC",      T.pack (printf "%.4f" (GLMM.glmmICC gr)))
            , ("RMSE",     T.pack (printf "%.4f" rmse))
            , ("最大絶対残差", T.pack (printf "%.4f" maxAbs))
            ]

        resultSec =
          secCollapsible "<span class=\"sec-icon\">&#128200;</span> 回帰結果" True
            [ statRow
            , secCard "固定効果"
                [secCoefficients coeffs (Just ("周辺 R²", rSquared1 fixed))]
            , secCard ("BLUP (" <> grpCol <> " 別ランダム切片)")
                [secTable "" ["グループ", "u_j"] blupRows]
            , secCard "残差プロット" [secResiduals fitted resid]
            ]
    in [ secDataOverview df xCols yCol
       , secModelOverviewLink modelType formula linkTxt Nothing
       , resultSec
       ] ++ interactiveSec

-- ---------------------------------------------------------------------------
-- GP (axis-1 C, Phase A残)
-- ---------------------------------------------------------------------------

-- | GP レポート用ラッパ。
--
-- `gprResult` は予測グリッド (`gprGridX`) 上の事後平均と 95% 信用帯を保持。
-- ライブラリ利用者は `Model.GP.fitGP` で外挿域も含めた grid を渡すと
-- 対話的予測の信頼帯がそのまま使える。
data GPReport = GPReport
  { gprKernel  :: GP.Kernel
  , gprParams  :: GP.GPParams
  , gprResult  :: GP.GPResult
  , gprGridX   :: [Double]
  , gprTrainX  :: [Double]
  , gprTrainY  :: [Double]
  , gprLML     :: Double
  } deriving Show

instance Reportable GPReport where
  toReport _cfg df xCols yCol rep =
    let xs   = gprTrainX rep
        ys   = gprTrainY rep
        params = gprParams rep
        kern   = gprKernel rep
        gridX  = gprGridX rep
        res    = gprResult rep
        lml    = gprLML rep

        smooth = SmoothCurve gridX (GP.gpMean res) (GP.gpLower res) (GP.gpUpper res)

        -- 学習点での残差: 観測 vs 各 x の事後平均
        yHat   = GP.gpMean (GP.fitGP (GP.GPModel kern params) xs ys xs)
        resid  = zipWith (-) ys yHat
        (_sigmaH, rmse, maxAbs) = residStats resid 1

        kernLbl = T.pack (show kern)
        formula =
          "$f \\sim \\text{GP}(0, k(x, x'))$<br>"
          <> "$y_i = f_i + \\varepsilon_i,\\quad "
          <> "\\varepsilon_i \\sim \\text{Normal}(0, \\sigma_n^2)$"

        statRow =
          secStatRow
            [ ("ℓ",    T.pack (printf "%.4f" (GP.gpLengthScale params)))
            , ("σ_f²", T.pack (printf "%.4f" (GP.gpSignalVar params)))
            , ("σ_n²", T.pack (printf "%.4f" (GP.gpNoiseVar params)))
            , ("LML",  T.pack (printf "%.2f"  lml))
            , ("RMSE", T.pack (printf "%.4f" rmse))
            , ("最大絶対残差", T.pack (printf "%.4f" maxAbs))
            ]

        resultSec =
          secCollapsible "<span class=\"sec-icon\">&#128200;</span> 回帰結果" True
            [ statRow
            , secCard "ハイパーパラメータ (周辺尤度最大化で推定)"
                [ secCoefficients
                    [ ("ℓ (length scale)",      GP.gpLengthScale params)
                    , ("σ_f² (signal variance)", GP.gpSignalVar params)
                    , ("σ_n² (noise variance)",  GP.gpNoiseVar params)
                    ]
                    (Just ("log p(y|X,θ)", lml))
                ]
            , secCard "残差プロット" [secResiduals yHat resid]
            ]

        sliderRange = case xs of
          [] -> (0, 1)
          _  ->
            let lo = minimum xs
                hi = maximum xs
                ext = (hi - lo) * 0.5
            in (lo - ext, hi + ext)

        xc = case xCols of { (c:_) -> c; _ -> "x" }

    in [ secDataOverview df xCols yCol
       , secModelOverviewExtras "GP" formula
           [("カーネル", kernLbl)] Nothing
       , resultSec
       , secInteractiveLM "対話的予測" xc yCol xs ys smooth sliderRange
       ]

-- ---------------------------------------------------------------------------
-- HBM (Bayesian Linear Regression) (axis-1 C, Phase A残)
-- ---------------------------------------------------------------------------

-- | ベイズ単回帰 (`y ~ Normal(α + β x, σ)`) の HBM レポート用ラッパ。
--
-- 一般的な HBM (任意の構造) は section を直接構築するか、用途別ラッパを別途定義する。
-- ここでは「α + β·x」という最も典型的なパターンに特化。
data HBMLinearReport = HBMLinearReport
  { hbmrChain     :: MC.Chain
  , hbmrXs        :: [Double]
  , hbmrYs        :: [Double]
  , hbmrAlphaName :: Text     -- ^ 例: "alpha"
  , hbmrBetaName  :: Text     -- ^ 例: "beta"
  , hbmrSigmaName :: Text     -- ^ 例: "sigma"
  , hbmrGraph     :: Maybe Text  -- ^ Mermaid DAG (`Viz.ModelGraph` で構築)
  }

-- | x の各点での α + β·x の事後分位点 (中央値, 2.5%, 97.5%)。
hbmRibbonAt :: [Double] -> [Double] -> [Double] -> ([Double], [Double], [Double])
hbmRibbonAt grid alphas betas =
  let qsAt p s =
        let n = length s
        in if n == 0 then 0 else s !! min (n - 1) (max 0 (floor (p * fromIntegral n)))
      atX x =
        let s  = sortByList (zipWith (\a b -> a + b * x) alphas betas)
        in (qsAt 0.5 s, qsAt 0.025 s, qsAt 0.975 s)
      preds = map atX grid
      (m, lo, hi) = unzip3 preds
  in (m, lo, hi)

sortByList :: Ord a => [a] -> [a]
sortByList = sortBy compare

instance Reportable HBMLinearReport where
  toReport _cfg df xCols yCol rep =
    let chain   = hbmrChain rep
        xs      = hbmrXs rep
        ys      = hbmrYs rep
        aName   = hbmrAlphaName rep
        bName   = hbmrBetaName  rep
        sName   = hbmrSigmaName rep
        params  = [aName, bName, sName]

        alphas  = MC.chainVals aName chain
        betas   = MC.chainVals bName chain
        sigmas  = MC.chainVals sName chain
        aMean   = mean0 alphas
        bMean   = mean0 betas
        sMean   = mean0 sigmas

        fitted  = [ aMean + bMean * x | x <- xs ]
        resid   = zipWith (-) ys fitted
        n       = length ys
        meanY   = if n == 0 then 0 else sum ys / fromIntegral n
        ssTot   = sum [ (y - meanY) ^ (2 :: Int) | y <- ys ]
        ssRes   = sum [ r * r | r <- resid ]
        r2      = if ssTot > 1e-12 then 1 - ssRes / ssTot else 0
        (_sH, rmse, maxAbs) = residStats resid 2

        xMin    = if null xs then 0 else minimum xs
        xMax    = if null xs then 1 else maximum xs
        ext     = (xMax - xMin) * 0.5
        gMin    = xMin - ext
        gMax    = xMax + ext
        grid    = if null xs then []
                  else [ gMin + i * (gMax - gMin) / 99 | i <- [0 .. 99] ]
        (mid, lo, hi) = hbmRibbonAt grid alphas betas
        smooth  = SmoothCurve grid mid lo hi

        formula =
          "$" <> yCol <> "_i \\sim \\text{Normal}(\\alpha + \\beta x_i, \\sigma)$<br>"
          <> "$\\alpha \\sim \\text{Normal}(0, 10),\\ "
          <> "\\beta \\sim \\text{Normal}(0, 10),\\ "
          <> "\\sigma \\sim \\text{Exponential}(1)$"

        accept = MC.chainAccepted chain
        total  = max 1 (MC.chainTotal chain)
        accRate :: Double
        accRate = fromIntegral accept / fromIntegral total

        statRow =
          secStatRow
            [ ("R²",         T.pack (printf "%.4f" r2))
            , ("サンプル数", T.pack (show total))
            , ("受容率",     T.pack (printf "%.1f%%" (accRate * 100)))
            , ("RMSE",       T.pack (printf "%.4f" rmse))
            , ("最大絶対残差", T.pack (printf "%.4f" maxAbs))
            ]

        coeffsCard = secCard "事後平均係数"
          [ secCoefficients
              [ ("α (intercept)", aMean)
              , ("β (slope)",      bMean)
              , ("σ",              sMean)
              ]
              (Just ("R²", r2))
          ]

        diagCard = secCard "MCMC 診断"
          [ secMCMCDiagnostics "Posterior + trace" params chain
          , secMCMCAutocorr   "自己相関 (max lag 40)" 40 params chain
          , secMCMCPair        "ペア散布 (α, β)" aName bName chain
          ]

        residCard = secCard "残差プロット" [ secResiduals fitted resid ]

        resultSec =
          secCollapsible "<span class=\"sec-icon\">&#128200;</span> 回帰結果" True
            [ statRow
            , coeffsCard
            , diagCard
            , residCard
            ]

        xc = case xCols of { (c:_) -> c; _ -> "x" }

    in [ secDataOverview df xCols yCol
       , secModelOverviewExtras "HBM(NUTS)" formula
           [("サンプラー", "NUTS")] (hbmrGraph rep)
       , resultSec
       , secInteractiveLM "対話的予測 (信用区間付)" xc yCol xs ys smooth (gMin, gMax)
       ]

mean0 :: [Double] -> Double
mean0 [] = 0
mean0 xs = sum xs / fromIntegral (length xs)

-- ---------------------------------------------------------------------------
-- HBM (一般) - multi-x / 非線形対応の汎用ラッパ (Cycle 7)
-- ---------------------------------------------------------------------------

-- | 単変数 x 上の予測リボン (中央値 + 信用区間)。
--
-- 任意の HBM (非線形を含む) に対しユーザー側で事後ドローから計算したものを渡す。
-- 'HBMReport' に含めると散布図 + リボン + 対話的予測 (信用帯付き) が描かれる。
data HBMRibbon = HBMRibbon
  { hribXCol :: Text          -- ^ x 軸ラベル (列名)
  , hribXObs :: [Double]      -- ^ 学習データ x
  , hribYObs :: [Double]      -- ^ 学習データ y
  , hribGrid :: [Double]      -- ^ 予測グリッド X (推奨: ±50% 外挿)
  , hribMid  :: [Double]      -- ^ 各グリッド点での事後中央値
  , hribLow  :: [Double]      -- ^ 各グリッド点での 2.5% 分位
  , hribHigh :: [Double]      -- ^ 各グリッド点での 97.5% 分位
  } deriving Show

-- | HBM (一般) レポート用ラッパ。multi-x / 非線形 / 任意の構造に対応。
--
-- 'HBMLinearReport' は @α + β·x@ という線形 HBM に特化したショートカット。
-- 一般のモデルでは 'HBMReport' に以下の情報をユーザー側で集約して渡す:
--
-- * `hbmrChainG` — MCMC チェーン (診断プロット用)
-- * `hbmrPostSummaryG` — 事後要約 (mean/SD/quantile/ESS/R-hat) を直接指定
-- * `hbmrYHatG` — 学習データへの予測値 (例: 事後中央値による予測)
-- * `hbmrRibbonG` — 単変数 x 上の予測リボン (省略可)
-- * `hbmrPairsG` — 興味のあるパラメータペア散布
--
-- @
-- let postRows =
--       [ ("alpha", aMean, aSD, aQ025, aQ975, aESS, Just aRhat)
--       , ...
--       ]
--     rep = HBMReport { hbmrChainG = chain, hbmrParamsG = ["alpha","beta","sigma"]
--                     , hbmrFormulaG = "$y_i \\sim ...$"
--                     , hbmrSamplerG = "NUTS"
--                     , hbmrModelTypeG = "HBM(NUTS)"
--                     , hbmrGraphG = Just dag
--                     , hbmrPostSummaryG = postRows
--                     , hbmrYObsG = ys, hbmrYHatG = yHat
--                     , hbmrRibbonG = Just ribbon
--                     , hbmrPairsG = [("alpha","beta")]
--                     }
-- renderReport "out.html" cfg (toReport cfg df xCols yCol rep)
-- @
data HBMReport = HBMReport
  { hbmrChainG       :: MC.Chain
  , hbmrParamsG      :: [Text]
  , hbmrFormulaG     :: Text
  , hbmrSamplerG     :: Text
  , hbmrModelTypeG   :: Text
  , hbmrGraphG       :: Maybe Text
  , hbmrPostSummaryG ::
      [(Text, Double, Double, Double, Double, Double, Maybe Double)]
  , hbmrYObsG        :: [Double]
  , hbmrYHatG        :: [Double]
  , hbmrRibbonG      :: Maybe HBMRibbon
  , hbmrPairsG       :: [(Text, Text)]
  }

instance Reportable HBMReport where
  toReport _cfg df xCols yCol rep =
    let chain    = hbmrChainG rep
        params   = hbmrParamsG rep
        ys       = hbmrYObsG rep
        yHat     = hbmrYHatG rep
        resid    = zipWith (-) ys yHat
        n        = length ys
        meanY    = if n == 0 then 0 else sum ys / fromIntegral n
        ssTot    = sum [ (y - meanY) ^ (2 :: Int) | y <- ys ]
        ssRes    = sum [ r * r | r <- resid ]
        r2       = if ssTot > 1e-12 then 1 - ssRes / ssTot else 0
        nP       = length params
        (_sH, rmse, maxAbs) = residStats resid (max 1 nP)

        accept   = MC.chainAccepted chain
        total    = max 1 (MC.chainTotal chain)
        accRate :: Double
        accRate  = fromIntegral accept / fromIntegral total

        statRow =
          secStatRow
            [ ("R²",         T.pack (printf "%.4f" r2))
            , ("サンプル数", T.pack (show total))
            , ("受容率",     T.pack (printf "%.1f%%" (accRate * 100)))
            , ("RMSE",       T.pack (printf "%.4f" rmse))
            , ("最大絶対残差", T.pack (printf "%.4f" maxAbs))
            ]

        postCard = secCard "事後要約"
          [ secPosteriorSummary "" (hbmrPostSummaryG rep) ]

        diagSecs =
          [ secMCMCDiagnostics "Posterior + trace" params chain
          , secMCMCAutocorr "自己相関 (max lag 40)" 40 params chain
          ]
          ++ [ secMCMCPair ("ペア散布 (" <> a <> ", " <> b <> ")") a b chain
             | (a, b) <- hbmrPairsG rep ]
        diagCard = secCard "MCMC 診断" diagSecs

        residCard = secCard "残差プロット" [ secResiduals yHat resid ]

        resultSec =
          secCollapsible "<span class=\"sec-icon\">&#128200;</span> 回帰結果" True
            [ statRow, postCard, diagCard, residCard ]

        -- 単変数の予測リボンセクション (オプション)
        ribbonSecs = case hbmrRibbonG rep of
          Nothing -> []
          Just rb ->
            let smooth = SmoothCurve (hribGrid rb) (hribMid rb)
                                     (hribLow rb)  (hribHigh rb)
                gMin = if null (hribGrid rb) then 0 else minimum (hribGrid rb)
                gMax = if null (hribGrid rb) then 1 else maximum (hribGrid rb)
            in [ secInteractiveLM "対話的予測 (信用区間付)"
                   (hribXCol rb) yCol
                   (hribXObs rb) (hribYObs rb)
                   smooth (gMin, gMax) ]

    in [ secDataOverview df xCols yCol
       , secModelOverviewExtras (hbmrModelTypeG rep) (hbmrFormulaG rep)
           [("サンプラー", hbmrSamplerG rep)] (hbmrGraphG rep)
       , resultSec
       ] ++ ribbonSecs

-- ---------------------------------------------------------------------------
-- RFFMVReport — 多変量 RFF Ridge (Phase B-RFF)
-- ---------------------------------------------------------------------------

-- | 多変量 RFF Ridge のレポート。`rfmvGroup` 列で色分けし、`rfmvXAxis` 列
-- (xCols のいずれか) を横軸にして観測点 + 予測曲線を描く。
data RFFMVReport = RFFMVReport
  { rfmvFit          :: RFFRidgeFitMV
  , rfmvGroup        :: Text
  , rfmvXAxis        :: Text
  , rfmvInteractive  :: Bool
    -- ^ True なら 'secInteractiveRFFMV' (スライダ + リアルタイム JS 予測) を含める
  , rfmvStandardizer :: Maybe Std.Standardizer
    -- ^ fit 時に X を標準化したときの μ/σ。Nothing なら未標準化。
    --   plot や JS 予測時はこれで raw → 標準化変換を行う。
  } deriving (Show)

instance Reportable RFFMVReport where
  toReport _cfg df xCols yCol r =
    case (mapM (`getDoubleVec` df) xCols, getDoubleVec yCol df,
          getMaybeTextVec (rfmvGroup r) df) of
      (Just xVecs, Just yVec, Just gv) ->
        let cols    = map V.toList xVecs
            ys      = V.toList yVec
            groups  = [ maybe "" id g | g <- V.toList gv ]
            xMatRaw = LA2.fromColumns (map LA2.fromList cols)
            -- fit は標準化空間で行われたので、観測点も標準化空間に投げる
            stdr    = case rfmvStandardizer r of
                        Just s  -> s
                        Nothing -> Std.identityStandardizer (length xCols)
            xMatObs = Std.applyStandardizer stdr xMatRaw
            yhat    = predictRFFRidgeMV (rfmvFit r) xMatObs
            sse     = sum (zipWith (\a b -> (a-b)*(a-b)) ys yhat)
            sst     = let m = sum ys / fromIntegral (max 1 (length ys))
                      in sum [(y - m)*(y - m) | y <- ys]
            r2      = if sst < 1e-12 then 0 else 1 - sse / sst
            n       = length ys
            rmse    = sqrt (sse / fromIntegral (max 1 n))
            feats   = rffrmvFeatures (rfmvFit r)
            d       = LA2.cols (rffmvOmegas feats)
            ellLbl  = NF.fmtNumT (rffmvLengthScale feats)
            sfLbl   = NF.fmtNumT (rffmvSigmaF feats)
            lamLbl  = NF.fmtNumT (rffrmvLambda (rfmvFit r))
            xColIdx = case [ i | (i, c) <- zip [0..] xCols, c == rfmvXAxis r ] of
                        (i:_) -> i
                        []    -> 0
            xValuesAll = cols !! xColIdx
            xMin = minimum xValuesAll
            xMax = maximum xValuesAll
            ngrid = 100
            xGrid = [ xMin + fromIntegral i * (xMax - xMin) / fromIntegral (ngrid - 1)
                    | i <- [0 .. ngrid - 1] ]
            ptData = zip3 groups xValuesAll ys
            uniqGroups = uniq2 groups
            rowsForGroup g = [ i | (i, gg) <- zip [0..] groups, gg == g ]
            repValues g = [ (cols !! j) !! head (rowsForGroup g)
                          | j <- [0 .. length xCols - 1] ]
            mkLineData g =
              let rep = repValues g
                  makeRow t =
                    [ if j == xColIdx then t else rep !! j
                    | j <- [0 .. length xCols - 1] ]
                  xMatRawGrid = LA2.fromLists [ makeRow t | t <- xGrid ]
                  xMatStdGrid = Std.applyStandardizer stdr xMatRawGrid
                  ys'  = predictRFFRidgeMV (rfmvFit r) xMatStdGrid
              in [ (g, t, y') | (t, y') <- zip xGrid ys' ]
            lnData = concatMap mkLineData uniqGroups
            plotCfg = (defaultConfig
                        (yCol <> " by " <> rfmvGroup r
                          <> " — RFF Ridge (multivariate)"))
                       { plotWidth = 720, plotHeight = 480 }
            vega = scatterWithGroups plotCfg (rfmvXAxis r) yCol ptData lnData
            xJoined = T.intercalate ", " xCols
            -- 完全な数式 (MathJax)。φ の中身、Ridge 形、ω/b の事前を明示。
            formula = T.unlines
              [ "$$"
              , "\\hat{y}(x) = \\sum_{j=1}^{D} w_j\\, \\varphi_j(x), \\qquad"
              , "\\varphi_j(x) = \\sigma_f \\sqrt{\\tfrac{2}{D}}"
              , "\\, \\cos\\!\\bigl(\\boldsymbol{\\omega}_j^{\\top} x + b_j\\bigr)"
              , "$$"
              , "$$"
              , "x = (\\mathrm{" <> T.replace ", " "},\\,\\mathrm{" xJoined
                <> "})^{\\top} \\in \\mathbb{R}^{p}, \\quad p="
                <> T.pack (show (length xCols))
                <> ", \\quad D=" <> T.pack (show d) <> "."
              , "$$"
              , "$$"
              , "\\boldsymbol{\\omega}_j \\sim \\mathcal{N}\\!\\left(\\mathbf{0},\\, \\ell^{-2} I_p\\right),"
              , "\\quad b_j \\sim \\mathrm{Uniform}(0, 2\\pi),"
              , "\\quad \\ell = " <> ellLbl
                <> ",\\ \\sigma_f = " <> sfLbl <> "."
              , "$$"
              , "$$"
              , "\\boldsymbol{w} = \\arg\\min_{w}\\,\\bigl\\| y - \\Phi w \\bigr\\|^2 + \\lambda\\,\\|w\\|^2"
              , " \\;=\\; (\\Phi^{\\top}\\Phi + \\lambda I_D)^{-1} \\Phi^{\\top} y,"
              , "\\quad \\lambda = " <> lamLbl <> " \\;(=\\sigma_n^2)."
              , "$$"
              , "ここで $\\Phi \\in \\mathbb{R}^{n \\times D}$ は $i$ 行目が $\\varphi(x_i)^{\\top}$。"
              , "標準化 ON のときは $x$ を $(x-\\mu)/\\sigma$ してから $\\varphi$ に投入する。"
              ]
            -- インタラクティブセクション (スライダで副軸を変えると JS が予測を再計算)
            sliderRows = mkSliders xCols xColIdx cols
            omegasRowMaj =
              concat [ LA2.toList (LA2.flatten (rffmvOmegas feats)) ]
              -- LA.flatten は row-major なので OK
            iSection
              | rfmvInteractive r =
                  [ secInteractiveRFFMV "対話的予測 (副軸スライダ)"
                      InteractiveRFFMV
                        { irfXCols       = xCols
                        , irfYCol        = yCol
                        , irfXObs        = cols
                        , irfYObs        = ys
                        , irfGroups      = groups
                        , irfMainAxis    = rfmvXAxis r
                        , irfMainGrid    = xGrid
                        , irfSliders     = sliderRows
                        , irfOmegasRowMaj = omegasRowMaj
                        , irfBs          = V.toList (rffmvBs feats)
                        , irfSigmaF      = rffmvSigmaF feats
                        , irfDim         = d
                        , irfP           = length xCols
                        , irfWeights     = LA2.toList (rffrmvWeights (rfmvFit r))
                        , irfStdMu       = fmap Std.stMu (rfmvStandardizer r)
                        , irfStdSd       = fmap Std.stSd (rfmvStandardizer r)
                        }
                  ]
              | otherwise = []
        in [ secDataOverview df xCols yCol
           , secModelOverview "Multivariate RFF Ridge" formula Nothing
           , secKeyValue "Fit summary"
               [ ("Features (D)",       T.pack (show d))
               , ("Length scale ℓ",     ellLbl)
               , ("Signal σ_f",         sfLbl)
               , ("Ridge λ (=σ_n²)",    lamLbl)
               , ("Standardize",
                   maybe "OFF" (const "ON") (rfmvStandardizer r))
               , ("R²",                 NF.fmtNumT r2)
               , ("RMSE",               NF.fmtNumT rmse)
               , ("n",                  T.pack (show n))
               ]
           , secVega ("予測曲線 + 観測点 (" <> rfmvGroup r <> " で色分け)") vega
           ] ++ iSection ++
           [ secResiduals yhat (zipWith (-) ys yhat) ]
      _ -> [ secDataOverview df xCols yCol
           , secModelOverview "Multivariate RFF Ridge"
               "(必要な列が取得できません: x_i, y, group の数値/Text 列を確認してください)"
               Nothing
           ]

uniq2 :: Ord a => [a] -> [a]
uniq2 []     = []
uniq2 (x:xs) = x : uniq2 (filter (/= x) xs)

-- | 副軸 (= 横軸以外) について (列名, min, mid, max) のスライダ情報を作る。
mkSliders :: [Text] -> Int -> [[Double]] -> [(Text, Double, Double, Double)]
mkSliders xCols mainIdx cols =
  [ (xCols !! j, minimum c, mid c, maximum c)
  | (j, c) <- zip [0..] cols
  , j /= mainIdx
  ]
  where
    mid xs = let s = sortBy compare xs
                 n = length s
             in if n == 0 then 0 else s !! (n `div` 2)
