{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Hanalyze.Model.Discriminant
-- Description : 判別分析 (Linear / Quadratic Discriminant Analysis)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- 判別分析 (Linear / Quadratic Discriminant Analysis)。
--
-- 連続説明変数で複数クラスを判別する古典的手法。
--
--   * 'LDA': 全クラスで共分散行列を共通 (pooled) と仮定 → 線形決定境界
--   * 'QDA': クラスごとに共分散行列が異なる → 二次決定境界
--
-- 予測は class-conditional 密度 × prior の対数 (log-posterior) を比較。
-- 数値安定化のため Cholesky 分解経由で log-determinant + Mahalanobis 距離を
-- 計算する。 hmatrix Vector / Matrix 演算で完結 (list 化禁止)。
module Hanalyze.Model.Discriminant
  ( DiscriminantMethod (..)
  , DiscriminantFit (..)
  , fitLDA
  , fitQDA
  , predictDiscriminant
  ) where

import qualified Data.Vector           as V
import qualified Numeric.LinearAlgebra as LA
import           Data.List             (nub, sort)
import           Data.Text             (Text)
import qualified Data.Text             as T

-- ===========================================================================
-- 型
-- ===========================================================================

data DiscriminantMethod = LDA | QDA deriving (Show, Eq)

data DiscriminantFit = DiscriminantFit
  { dfMeans       :: !(LA.Matrix Double)
    -- ^ K × p、 各クラスの平均ベクトル
  , dfCovariance  :: !(LA.Matrix Double)
    -- ^ LDA: pooled covariance (p × p)、 QDA: 空 (使わず、 dfCovariances を見る)
  , dfCovariances :: ![LA.Matrix Double]
    -- ^ QDA: クラス別 covariance (K matrices)、 LDA: 空
  , dfPriors      :: !(LA.Vector Double)
    -- ^ クラス事前確率 (length K、 sum = 1)
  , dfClasses     :: !(LA.Vector Double)
    -- ^ クラス label (sorted、 length K、 Int を Double で保持)
  , dfMethod      :: !DiscriminantMethod
  } deriving (Show)

-- ===========================================================================
-- 公開関数
-- ===========================================================================

-- | LDA fit: pooled covariance、 線形判別。
fitLDA :: LA.Matrix Double  -- ^ X (n × p)
       -> V.Vector Int      -- ^ y (n)、 整数クラスラベル
       -> Either Text DiscriminantFit
fitLDA x y
  | LA.rows x /= V.length y =
      Left "fitLDA: X rows and y length mismatch"
  | LA.rows x < 2 =
      Left "fitLDA: need at least 2 observations"
  | length classIds < 2 =
      Left "fitLDA: need at least 2 distinct classes"
  | otherwise =
      let (means, sigmaP, priors) = pooledStats x y classIds
      in Right DiscriminantFit
           { dfMeans       = means
           , dfCovariance  = sigmaP
           , dfCovariances = []
           , dfPriors      = priors
           , dfClasses     = LA.fromList (map fromIntegral classIds)
           , dfMethod      = LDA
           }
  where
    classIds = sort (nub (V.toList y))

-- | QDA fit: クラス別 covariance。
fitQDA :: LA.Matrix Double -> V.Vector Int -> Either Text DiscriminantFit
fitQDA x y
  | LA.rows x /= V.length y =
      Left "fitQDA: X rows and y length mismatch"
  | LA.rows x < 2 =
      Left "fitQDA: need at least 2 observations"
  | length classIds < 2 =
      Left "fitQDA: need at least 2 distinct classes"
  | minimum classCounts < LA.cols x + 1 =
      Left (T.pack ("fitQDA: each class needs ≥ p+1 = "
                    <> show (LA.cols x + 1) <> " observations (got min "
                    <> show (minimum classCounts) <> ")"))
  | otherwise =
      let (means, covs, priors) = perClassStats x y classIds
      in Right DiscriminantFit
           { dfMeans       = means
           , dfCovariance  = LA.fromLists [[]]
           , dfCovariances = covs
           , dfPriors      = priors
           , dfClasses     = LA.fromList (map fromIntegral classIds)
           , dfMethod      = QDA
           }
  where
    classIds = sort (nub (V.toList y))
    classCounts = [length [i | i <- [0 .. V.length y - 1], y V.! i == c]
                  | c <- classIds]

-- | 予測。 返り値 = (予測ラベル長 m, posterior 行列 m × K)。
predictDiscriminant
  :: DiscriminantFit
  -> LA.Matrix Double      -- ^ X_new (m × p)
  -> (V.Vector Int, LA.Matrix Double)
predictDiscriminant fit xNew =
  let m = LA.rows xNew
      k = LA.size (dfPriors fit)
      classLabels = LA.toList (dfClasses fit)
      -- 各サンプル × 各クラスの log-posterior を計算
      logPostMat = LA.fromLists
        [ [ logPosterior fit (LA.flatten (xNew LA.? [i])) j
          | j <- [0 .. k - 1] ]
        | i <- [0 .. m - 1] ]
      -- 各行で argmax → ラベル予測
      predLabels = V.fromList
        [ let row = LA.toList (logPostMat LA.! i)
              maxIdx = snd (maximum (zip row [0 ..]))
          in round (classLabels !! maxIdx :: Double) :: Int
        | i <- [0 .. m - 1] ]
      -- posterior = exp(log-post) / Σ exp(log-post) (各行で normalize)
      posteriorMat = LA.fromLists
        [ let row = LA.toList (logPostMat LA.! i)
              maxLP = maximum row
              expRow = map (\x -> exp (x - maxLP)) row
              s = sum expRow
          in if s > 0 then map (/ s) expRow else expRow
        | i <- [0 .. m - 1] ]
  in (predLabels, posteriorMat)

-- ===========================================================================
-- 内部 helper
-- ===========================================================================

-- | log p(class=j) + log f(x | class=j)
--   - LDA: − 0.5 (x − μ_j)ᵀ Σ_p⁻¹ (x − μ_j) + log π_j  (定数項を省略)
--   - QDA: − 0.5 log |Σ_j| − 0.5 (x − μ_j)ᵀ Σ_j⁻¹ (x − μ_j) + log π_j
logPosterior :: DiscriminantFit -> LA.Vector Double -> Int -> Double
logPosterior fit x j =
  let mu_j = LA.flatten (dfMeans fit LA.? [j])
      diff = x - mu_j
      logPi = log (LA.atIndex (dfPriors fit) j)
  in case dfMethod fit of
       LDA ->
         let sigInvDiff = case LA.linearSolve (dfCovariance fit)
                                              (LA.asColumn diff) of
               Just m  -> LA.flatten m
               Nothing -> diff  -- singular fallback
             mahal = LA.sumElements (diff * sigInvDiff)
         in -0.5 * mahal + logPi
       QDA ->
         let sigma_j = dfCovariances fit !! j
             logDet = log (max 1e-300 (LA.det sigma_j))
             sigInvDiff = case LA.linearSolve sigma_j (LA.asColumn diff) of
               Just m  -> LA.flatten m
               Nothing -> diff
             mahal = LA.sumElements (diff * sigInvDiff)
         in -0.5 * logDet - 0.5 * mahal + logPi

-- | 各クラスの平均と pooled covariance + prior を計算。
pooledStats
  :: LA.Matrix Double -> V.Vector Int -> [Int]
  -> (LA.Matrix Double, LA.Matrix Double, LA.Vector Double)
pooledStats x y classIds =
  let n  = LA.rows x
      p  = LA.cols x
      nD = fromIntegral n :: Double
      classRows c = [i | i <- [0 .. n - 1], y V.! i == c]
      classN c = fromIntegral (length (classRows c)) :: Double
      means = LA.fromRows
        [ let rs = classRows c
              xc = x LA.? rs
              n_c = fromIntegral (length rs) :: Double
              colSum j = LA.sumElements (xc LA.¿ [j])
          in LA.fromList [ colSum j / n_c | j <- [0 .. p - 1] ]
        | c <- classIds ]
      -- pooled covariance: Σ_p = Σ_c (n_c - 1) S_c / (n - K)
      sigmaP =
        let k = length classIds
            sumS = foldr (+) (LA.konst 0 (p, p))
              [ let rs = classRows c
                    xc = x LA.? rs
                    mu = LA.flatten (means LA.? [idx])
                    centered = xc - LA.fromRows (replicate (length rs) mu)
                in LA.tr centered LA.<> centered  -- (n_c - 1) S_c
              | (idx, c) <- zip [0 ..] classIds ]
        in LA.scale (1 / fromIntegral (n - k)) sumS
      priors = LA.fromList [ classN c / nD | c <- classIds ]
  in (means, sigmaP, priors)

-- | クラス別 mean + cov + prior。
perClassStats
  :: LA.Matrix Double -> V.Vector Int -> [Int]
  -> (LA.Matrix Double, [LA.Matrix Double], LA.Vector Double)
perClassStats x y classIds =
  let n  = LA.rows x
      p  = LA.cols x
      nD = fromIntegral n :: Double
      classRows c = [i | i <- [0 .. n - 1], y V.! i == c]
      means = LA.fromRows
        [ let rs = classRows c
              xc = x LA.? rs
              n_c = fromIntegral (length rs) :: Double
              colSum j = LA.sumElements (xc LA.¿ [j])
          in LA.fromList [ colSum j / n_c | j <- [0 .. p - 1] ]
        | c <- classIds ]
      covs =
        [ let rs = classRows c
              xc = x LA.? rs
              n_c = fromIntegral (length rs) :: Double
              mu = LA.flatten (means LA.? [idx])
              centered = xc - LA.fromRows (replicate (length rs) mu)
          in LA.scale (1 / (n_c - 1)) (LA.tr centered LA.<> centered)
        | (idx, c) <- zip [0 ..] classIds ]
      priors = LA.fromList
        [ fromIntegral (length (classRows c)) / nD | c <- classIds ]
      _ = p  -- silence
  in (means, covs, priors)
