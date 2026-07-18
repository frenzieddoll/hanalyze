{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Hanalyze.Model.Cluster
-- Description : クラスタリングアルゴリズム (k-means / silhouette / inertia)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Clustering algorithms.
--
-- Implements:
--
--   * 'kMeans' (Lloyd / Forgy / k-means++ initialisation, multi-restart)
--   * 'silhouette' (cluster quality metric)
--   * 'inertia' (within-cluster sum of squared distances)
--
-- Hierarchical and DBSCAN are deferred to a follow-up phase.
module Hanalyze.Model.Cluster
  ( -- * K-means
    KMeansConfig (..)
  , KMeansInit (..)
  , KMeansResult (..)
  , defaultKMeans
  , kMeans
  , kMeansPure
    -- * Quality metrics
  , silhouette
  , inertia
    -- * Helpers (exposed for advanced use)
  , assignLabels
  , updateCentroids
  ) where

import qualified Numeric.LinearAlgebra        as LA
import qualified Hanalyze.Stat.KernelDist              as KD
import qualified System.Random.MWC            as MWC
import           Control.Monad                (forM_, foldM)
import           Control.Monad.Primitive      (PrimMonad, PrimState)
import           Control.Monad.ST             (ST, runST)
import qualified Data.Vector                  as V
import qualified Data.Vector.Mutable          as VM
import qualified Data.Vector.Unboxed          as VU
import qualified Data.Vector.Unboxed.Mutable  as MVU
import qualified Data.Vector.Storable         as VS
import qualified Data.Vector.Storable.Mutable as VSM
import           Data.List                    (minimumBy)
import           Data.Ord                     (comparing)
import           Data.Word                    (Word32)

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
defaultKMeans :: Int -> KMeansConfig
defaultKMeans k = KMeansConfig
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
--
-- IO ラッパ。 ロジックは 'PrimMonad' 汎用の 'kMeansM' (mwc は 'PrimMonad'
-- 汎用ゆえ ST/IO で同コードを共有) をそのまま IO に特殊化したもの。
kMeans :: KMeansConfig -> LA.Matrix Double -> MWC.GenIO -> IO KMeansResult
kMeans = kMeansM

-- | 純粋・決定的な K-means。 同じ @seed@ なら必ず同じ 'KMeansResult' を返す
-- (同 seed → ビット同一・IO 不要)。 'kMeansM' を 'ST' で走らせ 'runST' で
-- 閉じる ([[phase-50-mcmc-purification-status]] の 'nutsPure' と同方針)。
kMeansPure :: KMeansConfig -> LA.Matrix Double -> Word32 -> KMeansResult
kMeansPure cfg x seed =
  runST (MWC.initialize (V.singleton seed) >>= kMeansM cfg x)

-- | 'PrimMonad' 汎用の K-means 本体。 'kMeans' (IO) / 'kMeansPure' (ST) が共有。
kMeansM :: PrimMonad m
        => KMeansConfig -> LA.Matrix Double -> MWC.Gen (PrimState m)
        -> m KMeansResult
kMeansM cfg x gen = do
  results <- mapM (\_ -> kMeansSingleRunM cfg x gen) [1 .. kmRestarts cfg]
  pure (minimumBy (comparing kmrInertia) results)

kMeansSingleRunM :: PrimMonad m
                 => KMeansConfig -> LA.Matrix Double -> MWC.Gen (PrimState m)
                 -> m KMeansResult
kMeansSingleRunM cfg x gen = do
  initC <- case kmInit cfg of
    Forgy      -> forgyInitM (kmK cfg) x gen
    KMeansPlus -> kmppInitM (kmK cfg) x gen
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
forgyInitM :: PrimMonad m
           => Int -> LA.Matrix Double -> MWC.Gen (PrimState m)
           -> m (LA.Matrix Double)
forgyInitM k x gen = do
  let n     = LA.rows x
      xRowsV = V.fromList (LA.toRows x)   -- O(1) row access
  idxs <- pickKDistinctM k n gen
  pure (LA.fromRows [xRowsV V.! i | i <- idxs])

-- | k-means++ initialisation: 1st centroid uniform random, subsequent
-- centroids weighted by squared distance to nearest existing centroid.
--
-- /Implementation/. Maintain @bestDist[i] = min_c ‖x_i − c‖²@ across
-- the centroids picked so far. Adding a new centroid is __one BLAS
-- GEMV__ + element-wise min, not a per-row Vector subtract / dot.
--
-- The previous version paid @n@ separate @LA.Vector@ allocations and
-- @n@ BLAS @ddot@ dispatches per centroid update (e.g. for
-- @n = 2000, k = 5@ that was ~10 000 length-@p@ allocations and
-- ~10 000 BLAS calls per kMeans run, ×10 restarts ≈ 100 000 allocs).
-- The fused-BLAS form below uses pre-computed row sq-norms and a
-- single matrix-vector multiply per centroid — O(np) work for the
-- whole sweep instead of O(n) per row.
kmppInitM :: PrimMonad m
          => Int -> LA.Matrix Double -> MWC.Gen (PrimState m)
          -> m (LA.Matrix Double)
kmppInitM k x gen = do
  let n        = LA.rows x
      -- Pre-compute row squared norms once: ‖x_i‖² for all rows
      -- (length-n vector via @(X ⊙ X) · 1@).
      normsX   = KD.rowSqNorms x

  -- Pick the first centroid.
  i0 <- MWC.uniformR (0, n - 1) gen
  -- bestDist[i] = ‖x_i − x_{i0}‖²  in BLAS form:
  --   = ‖x_i‖² + ‖x_{i0}‖² − 2 x_iᵀ x_{i0}
  -- via @cross = X · x_{i0}@ (one GEMV), reusing 'normsX'.
  let initBest = sqDistsToRow x normsX i0

      pickWeighted total bdv =
        if total <= 0
          then pure 0
          else do
            u <- MWC.uniformR (0, total) gen
            -- Linear scan of the cumulative weights via VS.unsafeIndex.
            let go !acc !i
                  | i >= n - 1 = pure i
                  | otherwise  = do
                      let !nxt = acc + bdv `VS.unsafeIndex` i
                      if u <= nxt
                        then pure i
                        else go nxt (i + 1)
            go 0 0

      -- IORef を foldM で純粋に畳む (純粋化のため・乱数列順は不変ゆえ
      -- 旧 IORef 版とビット同一)。 state = (bestDist, 逆順 centroid idx)。
      step (bd, acc) _ = do
        let !total = VS.sum bd
        pickIdx <- pickWeighted total bd
        -- One GEMV → length-n @sq dist to new centroid@; element-wise
        -- min with @bestDist@ in a single Storable Vector pass.
        let !newDist = sqDistsToRow x normsX pickIdx
            !updated = VS.zipWith min bd newDist
        pure (updated, pickIdx : acc)

  (_, idxsRev) <- foldM step (initBest, [i0]) [2 .. k]
  -- Build the @k × p@ centroid matrix from row indices in one shot.
  let xRowsV = V.fromList (LA.toRows x)
  pure (LA.fromRows [xRowsV V.! i | i <- reverse idxsRev])

-- | Squared distance from every row of @X@ (n × p) to @X[i, :]@,
-- via the BLAS identity
-- @‖x_a − x_i‖² = ‖x_a‖² + ‖x_i‖² − 2 x_aᵀ x_i@.
--
-- Cost: 1 GEMV (@O(np)@) plus one length-@n@ element-wise pass.
-- Used by 'kmppInit' to avoid per-row Vector subtract/dot.
sqDistsToRow
  :: LA.Matrix Double      -- ^ Data matrix @X@ (@n × p@).
  -> LA.Vector Double      -- ^ Pre-computed row squared norms.
  -> Int                   -- ^ Reference row index @i@.
  -> LA.Vector Double      -- ^ Length-@n@ squared distances.
sqDistsToRow xMat normsX i =
  let xi    = LA.flatten (xMat LA.?? (LA.Pos (LA.idxs [i]), LA.All))
      ni    = normsX `LA.atIndex` i
      cross = xMat LA.#> xi                          -- length n, GEMV
      d     = normsX + LA.scalar ni - LA.scale 2 cross
  in LA.cmap (max 0) d   -- numerical-noise floor at 0

-- | Pick k distinct indices in [0, n) via Fisher-Yates partial.
pickKDistinctM :: PrimMonad m
               => Int -> Int -> MWC.Gen (PrimState m) -> m [Int]
pickKDistinctM k n gen = do
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
-- /Implementation/. The full @n × k@ squared-distance matrix is
-- /not/ materialised. Instead we use the BLAS identity
--
-- @‖x_i − c_j‖² = ‖x_i‖² + ‖c_j‖² − 2 x_iᵀ c_j@
--
-- of which only the cross term @cross = X · Cᵀ@ depends on @j@
-- per-row, so the row-wise argmin is equivalent to
--
-- @argmin_j (‖c_j‖² − 2 cross[i, j])@
--
-- (the @‖x_i‖²@ term is constant across @j@). Replaces the previous
-- @KD.pairwiseSqDistXY x cs@ + scan pipeline, which built a full
-- @n × k@ Storable matrix only to read every cell once. Now: one
-- BLAS GEMM (@O(npk)@) plus a length-@nk@ argmin scan with a small
-- per-row constant — half the writes, lower cache pressure.
assignLabelsV :: LA.Matrix Double -> LA.Matrix Double -> VU.Vector Int
assignLabelsV x cs =
  let n        = LA.rows x
      k        = LA.rows cs
      normsC   = KD.rowSqNorms cs               -- length k
      cross    = x LA.<> LA.tr cs               -- n × k, single GEMM
      flatXC   = LA.flatten cross
  in runST $ do
       lab <- MVU.new n
       let scanRow !i
             | i >= n    = pure ()
             | otherwise = do
                 let !base = i * k
                     -- argmin_j of (‖c_j‖² − 2 X·Cᵀ[i, j]).
                     pickArg !j !bestJ !bestVal
                       | j >= k    = bestJ
                       | otherwise =
                           let !v = (normsC `VS.unsafeIndex` j)
                                  - 2 * (flatXC `VS.unsafeIndex` (base + j))
                           in if v < bestVal
                                then pickArg (j + 1) j v
                                else pickArg (j + 1) bestJ bestVal
                     !v0      = (normsC `VS.unsafeIndex` 0)
                              - 2 * (flatXC `VS.unsafeIndex` base)
                     !bestJ0  = pickArg 1 0 v0
                 MVU.unsafeWrite lab i bestJ0
                 scanRow (i + 1)
       scanRow 0
       VU.unsafeFreeze lab

-- | Recompute centroids — public API. Wraps @updateCentroidsV@.
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
        -- VSM.replicate avoids the explicit init forM_ loops.
        sumBuf <- VSM.replicate (k * p) (0 :: Double)
        cntBuf <- MVU.replicate k     (0 :: Int)
            :: ST s (MVU.STVector s Int)
        -- Single pass over all rows. Tail-recursive Int loops keep the
        -- whole pass list-free; the previous @forM_ [0..n-1]@ +
        -- @forM_ [0..p-1]@ relied on GHC's list-fusion rewrite, which
        -- adds Haskell-level monadic-bind overhead for very small
        -- inner @p@.
        let goRow !i
              | i >= n    = pure ()
              | otherwise = do
                  let !l   = labels `VU.unsafeIndex` i
                      !off = i * p
                      !sof = l * p
                      goCol !j
                        | j >= p    = pure ()
                        | otherwise = do
                            old <- VSM.unsafeRead sumBuf (sof + j)
                            VSM.unsafeWrite sumBuf (sof + j)
                              (old + flat `VS.unsafeIndex` (off + j))
                            goCol (j + 1)
                  goCol 0
                  c0 <- MVU.unsafeRead cntBuf l
                  MVU.unsafeWrite cntBuf l (c0 + 1)
                  goRow (i + 1)
        goRow 0
        -- Divide each cluster's sum by its count.
        let goNorm !c
              | c >= k    = pure ()
              | otherwise = do
                  cnt <- MVU.unsafeRead cntBuf c
                  let !invN = if cnt == 0 then 0
                                          else 1 / fromIntegral cnt
                      !sof  = c * p
                      goScale !j
                        | j >= p    = pure ()
                        | otherwise = do
                            v <- VSM.unsafeRead sumBuf (sof + j)
                            VSM.unsafeWrite sumBuf (sof + j) (v * invN)
                            goScale (j + 1)
                  goScale 0
                  goNorm (c + 1)
        goNorm 0
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
