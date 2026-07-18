-- |
-- Module      : Hanalyze.Model.LM
-- Description : 最小二乗法による線形回帰の fit・予測・信頼/予測区間
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Ordinary linear regression by least squares.
--
-- Solves @β = (XᵀX)⁻¹ Xᵀ y@ via hmatrix's @\\\\@ (LAPACK). Provides
-- confidence and prediction bands using
-- @t × √(s² xᵢᵀ(XᵀX)⁻¹xᵢ)@ and convenient adapters from a
-- @DataFrame@ for use from the CLI and report builder.
module Hanalyze.Model.LM
  ( LinearModel (..)
  , CIBand (..)
  , SmoothFit (..)
    -- * Matrix-canonical fit
  , fitLM
  , predictLM
    -- * Vector wrapper (1-output convenience)
  , fitLMVec
  , predictLMVec
    -- * Design matrices
  , designMatrix
  , polyDesignMatrix
  , multiPolyDesignMatrix
  , linspace
    -- * DataFrame helpers
  , fitDataFrameLM
  , confidenceBand
  , confidenceBandAt
  , predictionBandAt
  , fitWithCI
  , fitPolyWithSmooth
  ) where

import qualified DataFrame.Internal.DataFrame as DXD
import Hanalyze.DataIO.Convert (getDoubleVec)
import Hanalyze.Model.Core (FitResult (..), Model (..), Band (..),
                   coefficientsV, residualsV)

import Data.Text (Text)
import qualified Data.Vector as V
import qualified Numeric.LinearAlgebra as LA
import Statistics.Distribution (quantile)
import Statistics.Distribution.StudentT (studentT)

data LinearModel = LinearModel
  deriving (Show)

instance Model LinearModel where
  fit     _ = fitLM
  predict _ = predictLM

-- | Ordinary Least Squares (Matrix canonical, 多出力対応):
-- B = (XᵀX)⁻¹ Xᵀ Y、各列を独立に解く。
fitLM :: LA.Matrix Double -> LA.Matrix Double -> FitResult
fitLM x y =
  let beta  = x LA.<\> y                   -- p × q
      yHat  = x LA.<> beta                 -- n × q
      resid = y - yHat
      r2    = computeR2Multi y yHat
  in FitResult beta yHat resid r2

predictLM :: LA.Matrix Double -> LA.Matrix Double -> LA.Matrix Double
predictLM beta xNew = xNew LA.<> beta

-- | 単一出力 (Vector y) の便利ラッパ。@asColumn@ で 1 列行列に変換。
fitLMVec :: LA.Matrix Double -> LA.Vector Double -> FitResult
fitLMVec x y = fitLM x (LA.asColumn y)

-- | 1 出力での予測 (β は Vector)。
predictLMVec :: LA.Vector Double -> LA.Matrix Double -> LA.Vector Double
predictLMVec beta xNew = xNew LA.#> beta

-- | Build intercept + single predictor design matrix  [1, x].
designMatrix :: V.Vector Double -> LA.Matrix Double
designMatrix xs = LA.fromColumns
  [ LA.konst 1.0 n
  , LA.fromList (V.toList xs)
  ]
  where n = V.length xs

-- | Convenience: fit a simple LM directly from a DataFrame.
fitDataFrameLM :: DXD.DataFrame -> Text -> Text -> Maybe FitResult
fitDataFrameLM df xCol yCol = do
  xVec <- getDoubleVec xCol df
  yVec <- getDoubleVec yCol df
  let dm = designMatrix xVec
      y  = LA.fromList (V.toList yVec)
  return (fitLMVec dm y)

data CIBand = CIBand
  { lowerBound :: [Double]
  , upperBound :: [Double]
  , ciLevel    :: Double
  } deriving (Show)

-- | Pointwise confidence band for the mean response (1 出力前提)。
-- Formula: ŷᵢ ± t_{α/2, n−p} × sqrt(s² × xᵢᵀ (XᵀX)⁻¹ xᵢ)
--
-- 訓練設計行列上で評価する版 (= 各点の中心は fitted)。 grid 評価が要るときは
-- 'confidenceBandAt' を使う。
confidenceBand :: LA.Matrix Double -> FitResult -> Double -> CIBand
confidenceBand x res level = confidenceBandAt x res level x

-- | 訓練設計行列 @xTrain@ で推定した分散核 (s², (XᵀX)⁻¹, t 値) を、 別の
-- 評価点設計行列 @xEval@ の各行で band 化する。 中心は @xEval·β@、 半幅は
-- @t × √(s² × x₀ᵀ (XᵀX)⁻¹ x₀)@。 自由度・s² は訓練データで決まる。
--
-- ★grid 評価の核: 訓練点ではなく等間隔 grid の設計行列を @xEval@ に渡すと、
-- 回帰曲線・CI 帯が滑らかになる (= 疎・不均一データのガタつき解消)。 訓練点を
-- そのまま渡せば 'confidenceBand' と一致する (LM では @xTrain·β = fitted@)。
confidenceBandAt
  :: LA.Matrix Double  -- ^ 訓練設計行列 X (分散核の推定元)
  -> FitResult         -- ^ fit 結果 (β / 残差)
  -> Double            -- ^ 信頼水準 (例 0.95)
  -> LA.Matrix Double  -- ^ 評価点設計行列 X₀ (band を評価する行)
  -> CIBand
confidenceBandAt xTrain res level xEval =
  let df    = fromIntegral (LA.rows xTrain - LA.cols xTrain)
      beta  = coefficientsV res
      rs    = LA.toRows xEval
      yHats = [ xi `LA.dot` beta | xi <- rs ]
  -- df<=0 (飽和・過剰指定) は s²=0/0・studentT が例外 → CI 定義不能。
  -- 幅ゼロ帯 (lo=hi=ŷ) を返し、 帯は線に潰す (呼び元は線のみ描く)。
  in if df <= 0
       then CIBand yHats yHats level
       else
         let resV  = residualsV res
             s2    = (resV `LA.dot` resV) / df
             xtxi  = LA.inv (LA.tr xTrain LA.<> xTrain)
             tVal  = quantile (studentT df) ((1.0 + level) / 2.0)
             se xi = tVal * sqrt (s2 * (xi `LA.dot` (xtxi LA.#> xi)))
             los   = zipWith (\yh xi -> yh - se xi) yHats rs
             his   = zipWith (\yh xi -> yh + se xi) yHats rs
         in CIBand los his level

-- | 予測区間 (prediction interval) 版の 'confidenceBandAt'。 半幅に**観測分散**
-- @σ̂²@ を 1 つ加える: @t × √(s² × (1 + x₀ᵀ (XᵀX)⁻¹ x₀))@ (CI には @1 +@ が無い)。
-- = 新規観測 1 点が入る区間 (平均の信頼区間より広い)。 statsmodels の
-- @get_prediction().summary_frame()['obs_ci_lower/upper']@ と一致する。
-- (hanalyze-portable)
predictionBandAt
  :: LA.Matrix Double  -- ^ 訓練設計行列 X (分散核の推定元)
  -> FitResult         -- ^ fit 結果 (β / 残差)
  -> Double            -- ^ 信頼水準 (例 0.95)
  -> LA.Matrix Double  -- ^ 評価点設計行列 X₀ (band を評価する行)
  -> CIBand
predictionBandAt xTrain res level xEval =
  let df    = fromIntegral (LA.rows xTrain - LA.cols xTrain)
      beta  = coefficientsV res
      rs    = LA.toRows xEval
      yHats = [ xi `LA.dot` beta | xi <- rs ]
  -- df<=0 は CI/PI 定義不能 → 幅ゼロ帯 (線のみ)。 'confidenceBandAt' と同方針。
  in if df <= 0
       then CIBand yHats yHats level
       else
         let resV  = residualsV res
             s2    = (resV `LA.dot` resV) / df
             xtxi  = LA.inv (LA.tr xTrain LA.<> xTrain)
             tVal  = quantile (studentT df) ((1.0 + level) / 2.0)
             se xi = tVal * sqrt (s2 * (1 + xi `LA.dot` (xtxi LA.#> xi)))   -- ★CI との差は (1 +)
             los   = zipWith (\yh xi -> yh - se xi) yHats rs
             his   = zipWith (\yh xi -> yh + se xi) yHats rs
         in CIBand los his level

-- | Fit LM and compute confidence band in one step.
fitWithCI :: Double -> DXD.DataFrame -> Text -> Text -> Maybe (FitResult, CIBand)
fitWithCI level df xCol yCol = do
  xVec <- getDoubleVec xCol df
  yVec <- getDoubleVec yCol df
  let dm  = designMatrix xVec
      y   = LA.fromList (V.toList yVec)
      res = fitLMVec dm y
  return (res, confidenceBand dm res level)

-- | Polynomial design matrix [1, x, x², …, xᵈ].
polyDesignMatrix :: Int -> V.Vector Double -> LA.Matrix Double
polyDesignMatrix degree xs = LA.fromColumns
  [ LA.fromList [ x ^ k | x <- V.toList xs ]
  | k <- [0 .. degree]
  ]

-- | Multi-column polynomial design matrix.
-- Builds [1, x1, x1², …, x1^d1, x2, …, x2^d2, …] from a list of (column, degree) pairs.
multiPolyDesignMatrix :: [(V.Vector Double, Int)] -> LA.Matrix Double
multiPolyDesignMatrix [] = error "multiPolyDesignMatrix: empty predictor list"
multiPolyDesignMatrix colDegs@((firstXs, _) : _) =
  LA.fromColumns (intercept : concatMap polyExpand colDegs)
  where
    n           = V.length firstXs
    intercept   = LA.konst 1.0 n
    polyExpand (xs, deg) =
      [ LA.fromList [ x ^ k | x <- V.toList xs ] | k <- [1 .. deg] ]

-- | Grid of evenly spaced values from lo to hi.
linspace :: Double -> Double -> Int -> [Double]
linspace lo hi n
  | n <= 1    = [lo]
  | otherwise = [ lo + fromIntegral i * (hi - lo) / fromIntegral (n - 1)
                | i <- [0 .. n - 1] ]

-- | Pre-computed smooth curve data for plotting (evaluated on a fine grid).
data SmoothFit = SmoothFit
  { sfX       :: [Double]
  , sfFit     :: [Double]
  , sfLower   :: [Double]
  , sfUpper   :: [Double]
  , sfHasBand :: Bool
  } deriving (Show)

-- | Fit polynomial LM of given degree and compute a smooth curve with optional band
-- on a fine grid of nGrid points for clean visualisation.
fitPolyWithSmooth
  :: Band
  -> Int
  -> DXD.DataFrame
  -> Text
  -> Text
  -> Maybe (FitResult, SmoothFit)
fitPolyWithSmooth band nGrid df xCol yCol = do
  xVec <- getDoubleVec xCol df
  yVec <- getDoubleVec yCol df
  let degree = 1
      dm     = polyDesignMatrix degree xVec
      y      = LA.fromList (V.toList yVec)
      res    = fitLMVec dm y
      beta   = coefficientsV res

      xLa    = LA.fromList (V.toList xVec)
      xGrid  = V.fromList (linspace (LA.minElement xLa) (LA.maxElement xLa) nGrid)
      dmG    = polyDesignMatrix degree xGrid
      yGrid  = LA.toList (dmG LA.#> beta)

      dfStat = fromIntegral (LA.rows dm - LA.cols dm) :: Double
      resV   = residualsV res
      s2     = (resV `LA.dot` resV) / dfStat
      xtxi   = LA.inv (LA.tr dm LA.<> dm)
      gRows  = LA.toRows dmG

      computeBand level isPI
        -- df<=0 (飽和) は s²=0/0・studentT が例外 → 帯を線に潰す (lo=hi=yGrid)。
        | dfStat <= 0 = (yGrid, yGrid)
        | otherwise   =
            let tVal   = quantile (studentT dfStat) ((1.0 + level) / 2.0)
                extra  = if isPI then 1.0 else 0.0
                halfW xi = tVal * sqrt (s2 * (extra + xi `LA.dot` (xtxi LA.#> xi)))
                los    = zipWith (\yh xi -> yh - halfW xi) yGrid gRows
                his    = zipWith (\yh xi -> yh + halfW xi) yGrid gRows
            in (los, his)

  case band of
    NoBand ->
      return (res, SmoothFit (V.toList xGrid) yGrid yGrid yGrid False)
    CI level ->
      let (los, his) = computeBand level False
      in return (res, SmoothFit (V.toList xGrid) yGrid los his True)
    PI level ->
      let (los, his) = computeBand level True
      in return (res, SmoothFit (V.toList xGrid) yGrid los his True)

-- | 各列ごとに R² を計算 (多出力対応)。
computeR2Multi :: LA.Matrix Double -> LA.Matrix Double -> LA.Vector Double
computeR2Multi y yHat =
  let q = LA.cols y
  in LA.fromList
       [ let yj    = LA.flatten (y    LA.¿ [j])
             yhj   = LA.flatten (yHat LA.¿ [j])
             resid = yj - yhj
             yMean = LA.sumElements yj / fromIntegral (LA.size yj)
             dev   = LA.cmap (subtract yMean) yj
             ssRes = resid `LA.dot` resid
             ssTot = dev   `LA.dot` dev
         in if ssTot == 0 then 0
              else 1.0 - ssRes / ssTot
       | j <- [0 .. q - 1] ]
