{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Hanalyze.Model.NaiveBayes
-- Description : Naive Bayes 分類 (Gaussian + Multinomial)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: Naive Bayes 分類 (Gaussian + Multinomial).
--
-- @
-- import qualified Hanalyze.Model.NaiveBayes as NB
-- let nb = NB.fitGNB x y                    -- 連続特徴: Gaussian
--     yhat = NB.predictNB nb x
--
-- let mnb = NB.fitMNB 1.0 xCounts yCount    -- カウント特徴: Multinomial (Laplace α)
-- @
--
-- [English]: Naive Bayes classification (Gaussian + Multinomial).
--
-- @
-- import qualified Hanalyze.Model.NaiveBayes as NB
-- let nb = NB.fitGNB x y                    -- continuous features: Gaussian
--     yhat = NB.predictNB nb x
--
-- let mnb = NB.fitMNB 1.0 xCounts yCount    -- count features: Multinomial (Laplace α)
-- @
module Hanalyze.Model.NaiveBayes
  ( -- * Gaussian NB
    GaussianNB (..)
  , fitGNB
    -- * Multinomial NB
  , MultinomialNB (..)
  , fitMNB
    -- * Predict (両対応)
  , NBModel (..)
  , predictNB
  , predictNBLogProbs
  ) where

import qualified Data.Vector.Unboxed   as VU
import qualified Numeric.LinearAlgebra as LA
import           Data.Text             (Text)
import           Data.List             (nub, sort, foldl')

-- ---------------------------------------------------------------------------
-- Gaussian NB
-- ---------------------------------------------------------------------------

-- | [日本語]: クラスごとに各特徴を独立 Gaussian と仮定。
--   [English]: Assumes each feature is an independent Gaussian within each
--   class.
data GaussianNB = GaussianNB
  { gnbClasses    :: ![Int]
  , gnbLogPrior   :: ![Double]           -- ^ [日本語]: log π_c (classes 順) [English]: log π_c (in class order).
  , gnbMeans      :: ![LA.Vector Double] -- ^ [日本語]: 各クラスの μ (length d) [English]: The μ of each class (length d).
  , gnbVars       :: ![LA.Vector Double] -- ^ [日本語]: 各クラスの σ² (length d)、 var smoothing 済 [English]: The σ² of each class (length d), var-smoothed.
  , gnbClassNames :: ![Text]             -- ^ [日本語]: クラス名 (df|-> が levels 注入・空=数値表示)。 [English]: Class names (df|-> injects the levels; empty means numeric display).
  } deriving (Show)

-- | [日本語]: sklearn 互換の var smoothing (最大 var の 1e-9 倍を全 var に加算)。
--   [English]: sklearn-compatible var smoothing (adds 1e-9 times the maximum
--   variance to every variance).
varSmoothing :: Double
varSmoothing = 1e-9

fitGNB :: LA.Matrix Double -> VU.Vector Int -> GaussianNB
fitGNB x y =
  let !n        = VU.length y
      !d        = LA.cols x
      classes   = sort (nub (VU.toList y))
      rows c    = [ i | i <- [0 .. n - 1], y VU.! i == c ]
      meanV ids =
        let m = LA.fromRows [ LA.flatten (x LA.? [i]) | i <- ids ]
            nc = fromIntegral (length ids) :: Double
        in LA.scale (1 / nc) (LA.fromList (map LA.sumElements (LA.toColumns m)))
      varV ids mu =
        let nc = fromIntegral (length ids) :: Double
            sq i = let r = LA.flatten (x LA.? [i]) - mu
                   in r * r
            sumSq = sum (map sq ids)
        in LA.scale (1 / nc) sumSq
      mus  = [ meanV (rows c) | c <- classes ]
      vrs0 = zipWith (\c mu -> varV (rows c) mu) classes mus
      maxVar = maximum (map (LA.maxElement . LA.cmap abs) vrs0)
      eps    = varSmoothing * maxVar + 1e-300
      vrs    = map (LA.cmap (+ eps)) vrs0
      priors = [ log (fromIntegral (length (rows c)) / fromIntegral n)
               | c <- classes ]
      _ = d  -- d は使わない (内部で LA.size に頼る)
  in GaussianNB classes priors mus vrs []

-- | log p(x | c) = -1/2 Σ_j [ log(2π σ²_j) + (x_j - μ_j)² / σ²_j ]
gnbLogLik :: GaussianNB -> LA.Vector Double -> [Double]
gnbLogLik nb xv =
  [ let r   = xv - mu
        rsq = r * r
        logT = LA.sumElements (LA.cmap log (LA.scale (2 * pi) vr))
        chiT = LA.sumElements (rsq / vr)
    in -0.5 * (logT + chiT)
  | (mu, vr) <- zip (gnbMeans nb) (gnbVars nb) ]

-- ---------------------------------------------------------------------------
-- Multinomial NB
-- ---------------------------------------------------------------------------

-- | [日本語]: テキスト分類等のカウント特徴用。 ラプラス平滑化 α (典型 1.0)。
--   [English]: For count features such as in text classification. Laplace
--   smoothing α (typically 1.0).
data MultinomialNB = MultinomialNB
  { mnbClasses    :: ![Int]
  , mnbLogPrior   :: ![Double]
  , mnbLogFeat    :: ![LA.Vector Double]   -- ^ log p(feature_j | c)
  , mnbClassNames :: ![Text]               -- ^ [日本語]: クラス名 (df|-> が levels 注入・空=数値表示)。 [English]: Class names (df|-> injects the levels; empty means numeric display).
  } deriving (Show)

fitMNB :: Double             -- ^ Laplace α
       -> LA.Matrix Double  -- ^ [日本語]: 非負カウント (n × d) [English]: Non-negative counts (n × d).
       -> VU.Vector Int     -- ^ y
       -> MultinomialNB
fitMNB alpha x y =
  let !n       = VU.length y
      !d       = LA.cols x
      classes  = sort (nub (VU.toList y))
      rows c   = [ i | i <- [0 .. n - 1], y VU.! i == c ]
      sumRows ids =
        foldl' (+) (LA.konst 0 d)
          [ LA.flatten (x LA.? [i]) | i <- ids ]
      featLog c =
        let s     = sumRows (rows c)
            !sNum = LA.cmap (+ alpha) s
            !tot  = LA.sumElements sNum
        in LA.cmap log (LA.scale (1 / tot) sNum)
      priors = [ log (fromIntegral (length (rows c)) / fromIntegral n)
               | c <- classes ]
  in MultinomialNB classes priors [ featLog c | c <- classes ] []

mnbLogLik :: MultinomialNB -> LA.Vector Double -> [Double]
mnbLogLik nb xv =
  [ LA.dot xv lf | lf <- mnbLogFeat nb ]

-- ---------------------------------------------------------------------------
-- 共通インターフェース
-- ---------------------------------------------------------------------------

data NBModel = NBGaussian GaussianNB | NBMultinomial MultinomialNB
  deriving (Show)

nbClasses :: NBModel -> [Int]
nbClasses (NBGaussian m)    = gnbClasses m
nbClasses (NBMultinomial m) = mnbClasses m

nbLogPriorAndLik :: NBModel -> LA.Vector Double -> ([Double], [Double])
nbLogPriorAndLik (NBGaussian m) xv    = (gnbLogPrior m, gnbLogLik m xv)
nbLogPriorAndLik (NBMultinomial m) xv = (mnbLogPrior m, mnbLogLik m xv)

predictNBLogProbs :: NBModel -> LA.Matrix Double -> [[Double]]
predictNBLogProbs nb x =
  let !n = LA.rows x
      row i = LA.flatten (x LA.? [i])
      logits xv =
        let (lp, ll) = nbLogPriorAndLik nb xv
        in zipWith (+) lp ll
      -- log-sum-exp 正規化
      lse zs =
        let !mx = maximum zs
        in mx + log (sum [ exp (z - mx) | z <- zs ])
      one i =
        let zs = logits (row i)
            z  = lse zs
        in [ k - z | k <- zs ]
  in [ one i | i <- [0 .. n - 1] ]

predictNB :: NBModel -> LA.Matrix Double -> VU.Vector Int
predictNB nb x =
  let probs = predictNBLogProbs nb x
      classes = nbClasses nb
      pick zs =
        let (cMax, _) = foldr1
                          (\(c, v) (c', v') -> if v >= v' then (c, v) else (c', v'))
                          (zip classes zs)
        in cMax
  in VU.fromList (map pick probs)
