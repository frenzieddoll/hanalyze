{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
-- | Clustering algorithms.
--
-- Implements:
--
--   * 'kMeans' (Lloyd / Forgy / k-means++ initialisation, multi-restart)
--   * 'silhouette' (cluster quality metric)
--   * 'inertia' (within-cluster sum of squared distances)
--
-- Hierarchical and DBSCAN are deferred to a follow-up phase.
module Model.Cluster
  ( -- * K-means
    KMeansConfig (..)
  , KMeansInit (..)
  , KMeansResult (..)
  , defaultKMeansConfig
  , kMeans
    -- * Quality metrics
  , silhouette
  , inertia
    -- * Helpers (exposed for advanced use)
  , assignLabels
  , updateCentroids
  ) where

import qualified Numeric.LinearAlgebra as LA
import qualified Stat.KernelDist       as KD
import qualified System.Random.MWC     as MWC
import           Control.Monad         (forM_)
import qualified Data.Vector           as V
import qualified Data.Vector.Mutable   as VM
import           Data.IORef
import           Data.List             (foldl', minimumBy)
import           Data.Ord              (comparing)

-- ---------------------------------------------------------------------------
-- K-means
-- ---------------------------------------------------------------------------

-- | Initialisation strategy.
data KMeansInit
  = Forgy        -- ^ Pick k random data points.
  | KMeansPlus   -- ^ k-means++ (Arthur & Vassilvitskii 2007).
  deriving (Show, Eq)

-- | K-means configuration.
data KMeansConfig = KMeansConfig
  { kmK        :: !Int
  , kmInit     :: !KMeansInit
  , kmMaxIter  :: !Int
  , kmTol      :: !Double
  , kmRestarts :: !Int
  } deriving (Show, Eq)

-- | Default: k-means++, 300 iters, tol 1e-4, 10 restarts.
defaultKMeansConfig :: Int -> KMeansConfig
defaultKMeansConfig k = KMeansConfig
  { kmK        = k
  , kmInit     = KMeansPlus
  , kmMaxIter  = 300
  , kmTol      = 1e-4
  , kmRestarts = 10
  }

-- | K-means result.
data KMeansResult = KMeansResult
  { kmrCentroids :: !(LA.Matrix Double)
  , kmrLabels    :: ![Int]
  , kmrInertia   :: !Double
  , kmrIters     :: !Int
  , kmrConverged :: !Bool
  } deriving (Show)

-- | Fit K-means; runs 'kmRestarts' independent restarts and keeps
-- the lowest-inertia solution.
kMeans :: KMeansConfig -> LA.Matrix Double -> MWC.GenIO -> IO KMeansResult
kMeans cfg x gen = do
  results <- mapM (\_ -> kMeansSingleRun cfg x gen) [1 .. kmRestarts cfg]
  pure (minimumBy (comparing kmrInertia) results)

kMeansSingleRun :: KMeansConfig -> LA.Matrix Double -> MWC.GenIO
                -> IO KMeansResult
kMeansSingleRun cfg x gen = do
  initC <- case kmInit cfg of
    Forgy      -> forgyInit (kmK cfg) x gen
    KMeansPlus -> kmppInit (kmK cfg) x gen
  let loop !iter !centroids
        | iter >= kmMaxIter cfg = pure (centroids, iter, False)
        | otherwise = do
            let labels  = assignLabels x centroids
                newC    = updateCentroids x labels (kmK cfg)
                shift   = LA.norm_2 (LA.flatten (newC - centroids))
            if shift < kmTol cfg
              then pure (newC, iter + 1, True)
              else loop (iter + 1) newC
  (finalC, iters, conv) <- loop 0 initC
  let labels = assignLabels x finalC
  pure KMeansResult
    { kmrCentroids = finalC
    , kmrLabels    = labels
    , kmrInertia   = inertia x finalC labels
    , kmrIters     = iters
    , kmrConverged = conv
    }

-- | Forgy initialisation: pick k random rows.
forgyInit :: Int -> LA.Matrix Double -> MWC.GenIO -> IO (LA.Matrix Double)
forgyInit k x gen = do
  let n = LA.rows x
  idxs <- pickKDistinct k n gen
  pure (LA.fromRows [LA.toRows x !! i | i <- idxs])

-- | k-means++ initialisation: 1st centroid uniform random, subsequent
-- centroids weighted by squared distance to nearest existing centroid.
kmppInit :: Int -> LA.Matrix Double -> MWC.GenIO -> IO (LA.Matrix Double)
kmppInit k x gen = do
  let n    = LA.rows x
      rows = LA.toRows x
  i0 <- MWC.uniformR (0, n - 1) gen
  centroidsRef <- newIORef [rows !! i0]
  forM_ [2 .. k] $ \_ -> do
    cs <- readIORef centroidsRef
    let dists = [ minimum [LA.norm_2 (r - c) ^ (2 :: Int) | c <- cs]
                | r <- rows ]
        total = sum dists
    if total <= 0
      then pure ()
      else do
        u <- MWC.uniformR (0, total) gen
        let pickIdx = findCum u (zip [0..] dists)
        modifyIORef' centroidsRef (++ [rows !! pickIdx])
  cs <- readIORef centroidsRef
  pure (LA.fromRows cs)

-- | Cumulative pick: smallest index where prefix-sum ≥ u.
findCum :: Double -> [(Int, Double)] -> Int
findCum _ []          = 0
findCum u ((i, d):rest)
  | u <= d    = i
  | otherwise = findCum (u - d) rest

-- | Pick k distinct indices in [0, n) via Fisher-Yates partial.
pickKDistinct :: Int -> Int -> MWC.GenIO -> IO [Int]
pickKDistinct k n gen = do
  v <- V.thaw (V.fromList [0 .. n - 1])
  forM_ [0 .. min k n - 1] $ \i -> do
    j <- MWC.uniformR (i, n - 1) gen
    a <- VM.read v i
    b <- VM.read v j
    VM.write v i b
    VM.write v j a
  V.toList . V.take k <$> V.freeze v

-- | Assign each row to its nearest centroid (Euclidean).
assignLabels :: LA.Matrix Double -> LA.Matrix Double -> [Int]
assignLabels x cs =
  let d2 = KD.pairwiseSqDistXY x cs   -- n × k
      go r = let xs = LA.toList r
                 (i, _) = minimumBy (comparing snd) (zip [0..] xs)
             in i
  in [go r | r <- LA.toRows d2]

-- | Recompute centroids as mean of points per cluster. Empty clusters
-- get the zero vector (a more robust handling would pick a random
-- point; we leave this for a future improvement).
updateCentroids :: LA.Matrix Double -> [Int] -> Int -> LA.Matrix Double
updateCentroids x labels k =
  let p     = LA.cols x
      rows  = LA.toRows x
      grouped = [ [r | (r, l) <- zip rows labels, l == c] | c <- [0..k-1] ]
      mean1 [] = LA.konst 0 p
      mean1 vs = LA.scale (1 / fromIntegral (length vs))
                          (foldl' (+) (LA.konst 0 p) vs)
  in LA.fromRows (map mean1 grouped)

-- | Sum of squared Euclidean distances of each point to its cluster
-- centroid (within-cluster SS).
inertia :: LA.Matrix Double -> LA.Matrix Double -> [Int] -> Double
inertia x cs labels =
  let rows  = LA.toRows x
      cRows = LA.toRows cs
  in sum [ LA.norm_2 (r - cRows !! l) ^ (2 :: Int)
         | (r, l) <- zip rows labels ]

-- ---------------------------------------------------------------------------
-- Quality
-- ---------------------------------------------------------------------------

-- | Silhouette coefficient. Mean over samples of
-- @(b − a) / max(a, b)@ where @a@ is the mean distance to other points
-- in the same cluster and @b@ is the mean distance to the closest
-- other cluster. Range @[-1, 1]@; higher is better.
silhouette :: LA.Matrix Double -> [Int] -> Double
silhouette x labels =
  let n     = LA.rows x
      d2    = KD.pairwiseSqDist x
      d     = LA.cmap sqrt d2
      lvec  = V.fromList labels
      uniqL = V.toList (V.fromList (foldr (\l acc ->
        if l `elem` acc then acc else l:acc) [] labels))
      meanD i js
        | null js   = 0
        | otherwise = sum [LA.atIndex d (i, j) | j <- js]
                      / fromIntegral (length js)
      sIof i =
        let li = lvec V.! i
            ai = meanD i [j | j <- [0..n-1], j /= i, lvec V.! j == li]
            otherClusters = filter (/= li) uniqL
            bi = if null otherClusters then 0
                   else minimum [meanD i [j | j <- [0..n-1], lvec V.! j == c]
                                | c <- otherClusters]
        in if max ai bi == 0 then 0 else (bi - ai) / max ai bi
  in if n == 0 then 0 else sum [sIof i | i <- [0..n-1]] / fromIntegral n
