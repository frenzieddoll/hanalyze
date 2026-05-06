{-# LANGUAGE StrictData #-}
-- | BLAS-backed pairwise distance helpers.
--
-- Computes the @n × n@ (or @m × n@) matrix of squared Euclidean
-- distances between rows of input matrices via the identity
--
-- @
-- ‖x_i − y_j‖² = ‖x_i‖² + ‖y_j‖² − 2 x_iᵀ y_j
-- @
--
-- The cross term @X Yᵀ@ is delegated to BLAS (GEMM via @hmatrix@), so
-- the only non-vectorized work is the per-row squared norm. List
-- traversals over @n²@ pairs are avoided.
module Stat.KernelDist
  ( pairwiseSqDist
  , pairwiseSqDistXY
  , rowSqNorms
  , diagAB
  , rowDotsAB
  , mapMatrix
  , mapVector
  ) where

import qualified Numeric.LinearAlgebra        as LA
import qualified Data.Vector.Storable         as VS
import qualified Data.Vector.Storable.Mutable as VSM
import           Control.Monad.ST             (runST)

-- | Diagonal of the matrix product @A · B@ where @A@ is @m × n@ and
-- @B@ is @n × m@, computed without forming the full @m × m@ product.
--
-- @diag(A·B)[i] = Σ_j A[i, j] · B[j, i] = Σ_j (A ⊙ Bᵀ)[i, j]@,
-- i.e. one element-wise multiply (@m × n@) plus one row-sum (GEMV
-- against a length-@n@ ones vector). Replaces the naive
-- @[A[i,:] `dot` B[:,i] | i]@ which paid an m-times BLAS-dispatch
-- overhead. Used for GP posterior variance computation
-- (@σ² = sf − diag(K_* · K_y⁻¹ K_*ᵀ)@).
diagAB :: LA.Matrix Double -> LA.Matrix Double -> LA.Vector Double
diagAB a b =
  let n    = LA.cols a
      ones = LA.konst 1 n :: LA.Vector Double
  in (a * LA.tr b) LA.#> ones
{-# INLINE diagAB #-}

-- | Per-row dot products of two same-shape matrices.
--
-- @rowDotsAB A B[i] = Σ_j A[i, j] · B[i, j] = (A ⊙ B)[i, :] · 1@.
-- Replaces @[A[i,:] `dot` B[i,:] | i]@ which paid an m-times BLAS
-- dispatch overhead.
rowDotsAB :: LA.Matrix Double -> LA.Matrix Double -> LA.Vector Double
rowDotsAB a b =
  let n    = LA.cols a
      ones = LA.konst 1 n :: LA.Vector Double
  in (a * b) LA.#> ones
{-# INLINE rowDotsAB #-}

-- | Squared Euclidean norm of every row of @X@. Length-@n@ vector.
--
-- Vectorised: @(X ⊙ X) · 1_p@ — one element-wise square (BLAS-friendly
-- per-element multiply) plus one GEMV. Replaces the naive
-- @[row `dot` row | row <- toRows x]@ which paid an n-times BLAS
-- dispatch overhead on small rows.
rowSqNorms :: LA.Matrix Double -> LA.Vector Double
rowSqNorms x =
  let p    = LA.cols x
      ones = LA.konst 1 p :: LA.Vector Double
  in (x * x) LA.#> ones
{-# INLINE rowSqNorms #-}

-- | Pairwise squared distance among rows of one matrix.
--
-- @D[i, j] = ‖X[i,:] − X[j,:]‖²@ for @X@ of shape @n × p@; result is
-- @n × n@ with zeros on the diagonal (exactly).
--
-- Phase 11a (2026-05-06): rewritten with @runST@ + @MVector@. Profile
-- showed the previous massiv-fused version spent 75% of its time in
-- @trivialScheduler_@ overhead. A pure @LA.outer@-based replacement
-- was 6× /slower/ because the two @n × n@ broadcast intermediates
-- dominated allocation. The current version computes the cross term
-- with BLAS GEMM (one alloc) and fills the result @n²@ matrix with
-- a tight @runST + MVector@ loop using flat indices — single alloc,
-- no scheduler dispatch, no per-element function call. Mutable use
-- is justified: immutable was bottleneck (profile evidence) and
-- in-place fill with flat indexing is the algorithmically correct
-- representation.
pairwiseSqDist :: LA.Matrix Double -> LA.Matrix Double
pairwiseSqDist x =
  let n     = LA.rows x
      sq    = rowSqNorms x                              -- length n
      cross = x LA.<> LA.tr x                           -- n × n, BLAS GEMM
      crossF = LA.flatten cross                          -- length n²
      out = runST $ do
        v <- VSM.new (n * n)
        let go i j
              | i == n = pure ()
              | j == n = go (i + 1) 0
              | otherwise = do
                  let sqi = sq    `VS.unsafeIndex` i
                      sqj = sq    `VS.unsafeIndex` j
                      cij = crossF `VS.unsafeIndex` (i * n + j)
                      d   = if i == j
                              then 0
                              else let !s = sqi + sqj - 2 * cij
                                   in if s < 0 then 0 else s
                  VSM.unsafeWrite v (i * n + j) d
                  go i (j + 1)
        go 0 0
        VS.unsafeFreeze v
  in LA.reshape n out

-- | Pairwise squared distance between rows of two matrices.
--
-- @D[i, j] = ‖X[i,:] − Y[j,:]‖²@ for @X@ of shape @m × p@ and @Y@ of
-- shape @n × p@; result is @m × n@.
--
-- Phase 11a: same @runST + MVector@ rewrite as 'pairwiseSqDist'. No
-- diagonal special-case (matrices are different sources).
pairwiseSqDistXY :: LA.Matrix Double -> LA.Matrix Double -> LA.Matrix Double
pairwiseSqDistXY x y =
  let m      = LA.rows x
      n      = LA.rows y
      sx     = rowSqNorms x
      sy     = rowSqNorms y
      cross  = x LA.<> LA.tr y                          -- m × n, BLAS GEMM
      crossF = LA.flatten cross                          -- length m·n
      out = runST $ do
        v <- VSM.new (m * n)
        let go i j
              | i == m = pure ()
              | j == n = go (i + 1) 0
              | otherwise = do
                  let sxi = sx     `VS.unsafeIndex` i
                      syj = sy     `VS.unsafeIndex` j
                      cij = crossF `VS.unsafeIndex` (i * n + j)
                      !s  = sxi + syj - 2 * cij
                      d   = if s < 0 then 0 else s
                  VSM.unsafeWrite v (i * n + j) d
                  go i (j + 1)
        go 0 0
        VS.unsafeFreeze v
  in LA.reshape n out

-- ---------------------------------------------------------------------------
-- Element-wise helpers
-- ---------------------------------------------------------------------------

-- | Element-wise map over a hmatrix Matrix.
--
-- Implementation: flatten + 'VS.map' + reshape. The earlier massiv
-- ('A.map' with @Comp = Seq@) version was ~1.7× faster than 'LA.cmap'
-- on a single 2000×2000 call, but iterative paths (GP HP loop, GLM
-- IRLS) call this many times per fit and the per-call
-- 'trivialScheduler_' overhead dominated — profile attributed
-- 10–16% of GP fit time and 4% of GLM IRLS time to scheduler
-- bookkeeping. Direct 'VS.map' has zero scheduling overhead and is
-- the right default here.
{-# INLINE mapMatrix #-}
mapMatrix :: (Double -> Double) -> LA.Matrix Double -> LA.Matrix Double
mapMatrix f m =
  let cs = LA.cols m
  in LA.reshape cs (VS.map f (LA.flatten m))

-- | Element-wise map over a hmatrix Vector. Direct 'VS.map'; see
-- 'mapMatrix' for why we no longer route through massiv.
{-# INLINE mapVector #-}
mapVector :: (Double -> Double) -> LA.Vector Double -> LA.Vector Double
mapVector = VS.map
