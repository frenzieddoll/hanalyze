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

import qualified Numeric.LinearAlgebra        as LA
import qualified Stat.KernelDist              as KD
import qualified System.Random.MWC            as MWC
import           Control.Monad                (forM_)
import           Control.Monad.ST             (ST, runST)
import qualified Data.Vector                  as V
import qualified Data.Vector.Mutable          as VM
import qualified Data.Vector.Unboxed          as VU
import qualified Data.Vector.Unboxed.Mutable  as MVU
import qualified Data.Vector.Storable         as VS
import qualified Data.Vector.Storable.Mutable as VSM
import           Data.IORef
import           Data.List                    (minimumBy)
import           Data.Ord                     (comparing)

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
  -- Hot loop: keep labels as 'VU.Vector Int' to avoid the per-iteration
  -- list↔Vector roundtrip the previous version paid via 'assignLabels'
  -- + 'updateCentroids' on @[Int]@.
  let loop !iter !centroids
        | iter >= kmMaxIter cfg = pure (centroids, iter, False)
        | otherwise = do
            let labelsV = assignLabelsV x centroids
                newC    = updateCentroidsV x labelsV (kmK cfg)
                shift   = LA.norm_2 (LA.flatten (newC - centroids))
            if shift < kmTol cfg
              then pure (newC, iter + 1, True)
              else loop (iter + 1) newC
  (finalC, iters, conv) <- loop 0 initC
  let labelsV = assignLabelsV x finalC
  pure KMeansResult
    { kmrCentroids = finalC
    , kmrLabels    = VU.toList labelsV
    , kmrInertia   = inertiaV x finalC labelsV
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
--
-- Vector-incremental implementation. Maintain a Storable vector
-- @bestDist@ of length @n@ holding @min_c ‖x_i − c‖²@ across the
-- centroids picked so far. Adding a new centroid is one pass over
-- @n@ rows updating @bestDist@ and one weighted draw — no list of
-- centroids and no all-vs-all recomputation per iteration.
kmppInit :: Int -> LA.Matrix Double -> MWC.GenIO -> IO (LA.Matrix Double)
kmppInit k x gen = do
  let n     = LA.rows x
      xRows = LA.toRows x
      asArr = V.fromList xRows  -- O(1) row access
  -- Pick the first centroid.
  i0 <- MWC.uniformR (0, n - 1) gen
  let firstC = asArr V.! i0
      -- bestDist[i] = ‖x_i − firstC‖² for every row.
      initBest = VU.generate n
                   (\i -> let d = (asArr V.! i) - firstC
                          in LA.dot d d)
  bestRef <- newIORef initBest
  centroidsRef <- newIORef [firstC]
  let pickWeighted total bdv =
        if total <= 0
          then pure 0
          else do
            u <- MWC.uniformR (0, total) gen
            -- Linear scan of the cumulative weights via VU.unsafeIndex.
            let go !acc !i
                  | i >= n - 1 = pure i
                  | otherwise  = do
                      let nxt = acc + bdv VU.! i
                      if u <= nxt
                        then pure i
                        else go nxt (i + 1)
            go 0 0
  forM_ [2 .. k] $ \_ -> do
    bd <- readIORef bestRef
    let total = VU.sum bd
    pickIdx <- pickWeighted total bd
    let newC = asArr V.! pickIdx
    -- Update bestDist by taking min with squared distance to newC.
    let updated = VU.generate n
                    (\i -> let d  = (asArr V.! i) - newC
                               d2 = LA.dot d d
                           in min (bd VU.! i) d2)
    writeIORef bestRef updated
    modifyIORef' centroidsRef (newC :)
  cs <- readIORef centroidsRef
  pure (LA.fromRows (reverse cs))

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

-- | Assign each row to its nearest centroid (Euclidean) — public API.
assignLabels :: LA.Matrix Double -> LA.Matrix Double -> [Int]
assignLabels x cs = VU.toList (assignLabelsV x cs)

-- | Vector version of 'assignLabels'. Internal hot path; the public
-- @assignLabels@ wraps with @VU.toList@ at the boundary.
--
-- Implementation: BLAS-based @n × k@ distance matrix, then a single
-- 'runST' pass that takes the row-wise @argmin@ via @VS.unsafeIndex@
-- updates. Replaces the previous @[LA.toList r → minimumBy on a list@
-- per row], which paid one @k@-list allocation per row.
assignLabelsV :: LA.Matrix Double -> LA.Matrix Double -> VU.Vector Int
assignLabelsV x cs =
  let d2    = KD.pairwiseSqDistXY x cs   -- n × k
      n     = LA.rows d2
      k     = LA.cols d2
      flat  = LA.flatten d2
  in runST $ do
       lab <- MVU.new n
       let scanRow !i
             | i >= n    = pure ()
             | otherwise = do
                 let !base = i * k
                     -- inner argmin over the k-row of d2
                     pickArg !j !bestJ !bestVal
                       | j >= k    = bestJ
                       | otherwise =
                           let !v = flat `VS.unsafeIndex` (base + j)
                           in if v < bestVal
                                then pickArg (j + 1) j v
                                else pickArg (j + 1) bestJ bestVal
                     !bestJ0 = pickArg 1 0 (flat `VS.unsafeIndex` base)
                 MVU.unsafeWrite lab i bestJ0
                 scanRow (i + 1)
       scanRow 0
       VU.unsafeFreeze lab

-- | Recompute centroids — public API. Wraps 'updateCentroidsV'.
updateCentroids :: LA.Matrix Double -> [Int] -> Int -> LA.Matrix Double
updateCentroids x labels k = updateCentroidsV x (VU.fromList labels) k

-- | Vector version of 'updateCentroids'. Internal hot path.
--
-- Single-pass scatter-add: traverse the @n × p@ data matrix once,
-- accumulating each row into its assigned cluster's running sum and
-- bumping that cluster's count. Centroids are then @sum / count@.
-- Replaces the previous @[ [r | (r,l) ← zip rows labels, l == c]
-- | c ← [0..k-1] ]@ which scanned the whole label list once /per/
-- cluster — @O(n k)@ per call vs the new @O(n p)@.
updateCentroidsV
  :: LA.Matrix Double -> VU.Vector Int -> Int -> LA.Matrix Double
updateCentroidsV x labels k =
  let n    = LA.rows x
      p    = LA.cols x
      flat = LA.flatten x          -- length n*p, row-major
      out  = runST $ do
        sumBuf <- VSM.new (k * p)   -- per-cluster running sum
        cntBuf <- MVU.new k :: ST s (MVU.STVector s Int)
        -- Initialise with zeros (Storable does not auto-zero).
        forM_ [0 .. k * p - 1] $ \i ->
          VSM.unsafeWrite sumBuf i 0
        forM_ [0 .. k - 1] $ \c ->
          MVU.unsafeWrite cntBuf c 0
        -- Single pass over all rows.
        forM_ [0 .. n - 1] $ \i -> do
          let l    = labels VU.! i
              !off = i * p
              !sof = l * p
          forM_ [0 .. p - 1] $ \j -> do
            old <- VSM.unsafeRead sumBuf (sof + j)
            VSM.unsafeWrite sumBuf (sof + j)
              (old + flat `VS.unsafeIndex` (off + j))
          c0 <- MVU.unsafeRead cntBuf l
          MVU.unsafeWrite cntBuf l (c0 + 1)
        -- Divide each cluster's sum by its count.
        forM_ [0 .. k - 1] $ \c -> do
          cnt <- MVU.unsafeRead cntBuf c
          let !invN = if cnt == 0 then 0 else 1 / fromIntegral cnt
              !sof = c * p
          forM_ [0 .. p - 1] $ \j -> do
            v <- VSM.unsafeRead sumBuf (sof + j)
            VSM.unsafeWrite sumBuf (sof + j) (v * invN)
        VS.unsafeFreeze sumBuf
  in LA.reshape p out

-- | Sum of squared Euclidean distances — public API.
inertia :: LA.Matrix Double -> LA.Matrix Double -> [Int] -> Double
inertia x cs labels = inertiaV x cs (VU.fromList labels)

-- | Vector version. Single pass over the @n × p@ data matrix and the
-- @k × p@ centroid matrix, accumulating @‖x_i − c_{l_i}‖²@ via flat
-- indexing — no @LA.toRows@ list, no @cRows !! l@ list-index per row.
inertiaV
  :: LA.Matrix Double -> LA.Matrix Double -> VU.Vector Int -> Double
inertiaV x cs labels =
  let n     = LA.rows x
      p     = LA.cols x
      flatX = LA.flatten x
      flatC = LA.flatten cs
      go !i !acc
        | i >= n    = acc
        | otherwise =
            let l    = labels VU.! i
                !off = i * p
                !cof = l * p
                rowSq !j !s
                  | j >= p    = s
                  | otherwise =
                      let !d = (flatX `VS.unsafeIndex` (off + j))
                             - (flatC `VS.unsafeIndex` (cof + j))
                      in rowSq (j + 1) (s + d * d)
            in go (i + 1) (acc + rowSq 0 0)
  in go 0 0

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
