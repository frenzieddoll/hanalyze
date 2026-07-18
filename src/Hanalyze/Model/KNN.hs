{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Hanalyze.Model.KNN
-- Description : k近傍法 (k-Nearest Neighbours、 回帰 + 分類、 brute force ユークリッド距離)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- k-Nearest Neighbours (回帰 + 分類、 brute force ユークリッド距離).
--
-- @
-- import qualified Hanalyze.Model.KNN as KNN
-- let knnR = KNN.fitKNNR 5 xTrain yTrain
--     yR   = KNN.predictKNNR knnR xTest
-- @
--
-- /Complexity/: O(n_test · n_train · d)。 KD-tree は scope 外。
module Hanalyze.Model.KNN
  ( KNNRegressor (..)
  , KNNClassifier (..)
  , fitKNNR
  , fitKNNC
  , predictKNNR
  , predictKNNC
  , predictKNNCProbs
  ) where

import qualified Data.Vector.Unboxed   as VU
import qualified Numeric.LinearAlgebra as LA
import qualified Data.Map.Strict       as Map
import           Data.List             (foldl', sortBy, nub, sort)
import           Data.Ord              (comparing)
import           Data.Text             (Text)

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

data KNNRegressor = KNNRegressor
  { knnRK :: !Int
  , knnRX :: !(LA.Matrix Double)
  , knnRY :: !(VU.Vector Double)
  } deriving (Show)

data KNNClassifier = KNNClassifier
  { knnCK          :: !Int
  , knnCX          :: !(LA.Matrix Double)
  , knnCY          :: !(VU.Vector Int)
  , knnCClasses    :: ![Int]
  , knnCClassNames :: ![Text]   -- ^ クラス名 (df|-> が levels 注入・空=数値表示)。
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- Fit
-- ---------------------------------------------------------------------------

fitKNNR :: Int -> LA.Matrix Double -> VU.Vector Double -> KNNRegressor
fitKNNR k x y = KNNRegressor k x y

fitKNNC :: Int -> LA.Matrix Double -> VU.Vector Int -> KNNClassifier
fitKNNC k x y = KNNClassifier
  { knnCK          = k
  , knnCX          = x
  , knnCY          = y
  , knnCClasses    = sort (nub (VU.toList y))
  , knnCClassNames = []          -- df|-> 経路が reqLabelWithLevels で後から注入。
  }

-- ---------------------------------------------------------------------------
-- Predict helpers
-- ---------------------------------------------------------------------------

rowVec :: LA.Matrix Double -> Int -> LA.Vector Double
rowVec x i = LA.flatten (x LA.? [i])

-- | クエリ点に対し、 訓練データ各行までの距離 (二乗) と元 index のペア
-- を返す。
distancesSq :: LA.Matrix Double -> LA.Vector Double -> [(Int, Double)]
distancesSq xTrain q =
  let !n = LA.rows xTrain
  in [ (i, let v = rowVec xTrain i - q in LA.dot v v)
     | i <- [0 .. n - 1] ]

kNearest :: Int -> LA.Matrix Double -> LA.Vector Double -> [Int]
kNearest k xTrain q =
  let ds = sortBy (comparing snd) (distancesSq xTrain q)
  in map fst (take k ds)

-- ---------------------------------------------------------------------------
-- Predict (regression)
-- ---------------------------------------------------------------------------

predictKNNR :: KNNRegressor -> LA.Matrix Double -> VU.Vector Double
predictKNNR knn xTest =
  let !nT = LA.rows xTest
      !k  = knnRK knn
      !xT = knnRX knn
      !yT = knnRY knn
      pred1 i =
        let q   = rowVec xTest i
            ids = kNearest k xT q
            ys  = [ yT VU.! j | j <- ids ]
        in sum ys / fromIntegral (length ys)
  in VU.generate nT pred1

-- ---------------------------------------------------------------------------
-- Predict (classification)
-- ---------------------------------------------------------------------------

predictKNNCProbs :: KNNClassifier
                 -> LA.Matrix Double
                 -> [Map.Map Int Double]
predictKNNCProbs knn xTest =
  let !nT = LA.rows xTest
      !k  = knnCK knn
      !xT = knnCX knn
      !yT = knnCY knn
      counts1 i =
        let q   = rowVec xTest i
            ids = kNearest k xT q
            cs  = [ yT VU.! j | j <- ids ]
            !nk = fromIntegral (length cs) :: Double
            mp  = foldl' (\m c -> Map.insertWith (+) c 1 m)
                          Map.empty cs
        in Map.map (/ nk) mp
  in [ counts1 i | i <- [0 .. nT - 1] ]

predictKNNC :: KNNClassifier -> LA.Matrix Double -> VU.Vector Int
predictKNNC knn xTest =
  let probs = predictKNNCProbs knn xTest
      majority m =
        case sortBy (flip (comparing snd)) (Map.toList m) of
          ((c, _) : _) -> c
          []           -> 0
  in VU.fromList (map majority probs)
