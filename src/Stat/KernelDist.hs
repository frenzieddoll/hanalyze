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

import qualified Numeric.LinearAlgebra as LA
import qualified Data.Massiv.Array     as A
import           Data.Massiv.Array     (Array, Comp (..), Ix2 (..), Sz (..))

-- | Choose massiv 'Comp' mode based on workload size.
--
-- Default: 'Seq'. Tested 'Par' at thresholds 250 K, 4 M elements but
-- both regressed real-world benchmarks because:
--
-- 1. Iterative algorithms (GP HP loop, Lasso CD) call kernel/elementwise
--    helpers many times per fit. Per-call Par-scheduler setup overhead
--    accumulates over the iterations.
-- 2. Even with @-threaded@ off (single capability), 'Par' adds
--    bookkeeping cost that 'Seq' avoids.
-- 3. Standalone bench (bench-massiv) shows ~1.7× speedup on a single
--    large call, but in algorithms the calls are smaller and more
--    frequent.
--
-- For users who need parallelism on a single huge kernel matrix
-- (e.g. GP fit with n > 5000 in one shot), the underlying massiv
-- API can be invoked directly with @setComp Par@. Inside the
-- iterative paths used here, 'Seq' is the right default.
compFor :: Int -> Comp
compFor _ = Seq

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

-- | Pairwise squared distance among rows of one matrix.
--
-- @D[i, j] = ‖X[i,:] − X[j,:]‖²@ for @X@ of shape @n × p@; result is
-- @n × n@ with zeros on the diagonal (exactly).
--
-- F4: implemented via massiv's index-based 'A.makeArrayR' so the
-- cross term @X · Xᵀ@ comes from BLAS GEMM (hmatrix) and the
-- elementwise @sq[i] + sq[j] − 2·cross[i,j]@ (with diagonal zero and
-- max-0 clamp) is fused into a single sweep. Avoids the two
-- intermediate @n × n@ outer-product matrices used by the previous
-- hmatrix-only version. Measured 3.7× speedup at @n = 2000@.
pairwiseSqDist :: LA.Matrix Double -> LA.Matrix Double
pairwiseSqDist x =
  let n     = LA.rows x
      sq    = rowSqNorms x                              -- length n
      cross = x LA.<> LA.tr x                           -- n × n, BLAS GEMM
      comp  = compFor (n * n)
      sqA   = A.fromStorableVector comp sq              -- Array S Ix1 Double
      crA   = A.setComp comp (hmatrixToMassiv cross)
      raw   = A.computeAs A.S $
                A.makeArrayR A.D comp (Sz (n :. n)) $ \(i :. j) ->
                  if i == j
                    then 0
                    else max 0 ( A.index' sqA i
                               + A.index' sqA j
                               - 2 * A.index' crA (i :. j) )
  in massivToHmatrix raw

-- | Pairwise squared distance between rows of two matrices.
--
-- @D[i, j] = ‖X[i,:] − Y[j,:]‖²@ for @X@ of shape @m × p@ and @Y@ of
-- shape @n × p@; result is @m × n@.
--
-- F4: same fusion strategy as 'pairwiseSqDist'.
pairwiseSqDistXY :: LA.Matrix Double -> LA.Matrix Double -> LA.Matrix Double
pairwiseSqDistXY x y =
  let m     = LA.rows x
      n     = LA.rows y
      sx    = rowSqNorms x
      sy    = rowSqNorms y
      cross = x LA.<> LA.tr y                           -- m × n, BLAS GEMM
      comp  = compFor (m * n)
      sxA   = A.fromStorableVector comp sx
      syA   = A.fromStorableVector comp sy
      crA   = A.setComp comp (hmatrixToMassiv cross)
      raw   = A.computeAs A.S $
                A.makeArrayR A.D comp (Sz (m :. n)) $ \(i :. j) ->
                  max 0 ( A.index' sxA i
                        + A.index' syA j
                        - 2 * A.index' crA (i :. j) )
  in massivToHmatrix raw

-- ---------------------------------------------------------------------------
-- hmatrix ↔ massiv conversion (safe API, no unsafe / no raw pointers)
-- ---------------------------------------------------------------------------

-- | hmatrix 'LA.Matrix' → massiv @Array S Ix2 Double@. Round-trips
-- through the row-major flat 'LA.Vector' (Storable) and resizes via
-- massiv's 'A.resize''.
hmatrixToMassiv :: LA.Matrix Double -> Array A.S Ix2 Double
hmatrixToMassiv m =
  let rs      = LA.rows m
      cs      = LA.cols m
      arrFlat = A.fromStorableVector Seq (LA.flatten m)
  in A.resize' (Sz (rs :. cs)) arrFlat

-- | massiv @Array S Ix2 Double@ → hmatrix 'LA.Matrix'. Uses
-- 'A.toStorableVector' (no copy) and reshapes.
massivToHmatrix :: Array A.S Ix2 Double -> LA.Matrix Double
massivToHmatrix a =
  let Sz (_ :. cs) = A.size a
  in LA.reshape cs (A.toStorableVector (A.flatten a))

-- | Element-wise map over a hmatrix Matrix using massiv. ~1.7×
-- faster than 'LA.cmap' on 2000×2000 matrices (bench-massiv).
{-# INLINE mapMatrix #-}
mapMatrix :: (Double -> Double) -> LA.Matrix Double -> LA.Matrix Double
mapMatrix f m =
  let rs      = LA.rows m
      cs      = LA.cols m
      comp    = compFor (rs * cs)
      flat    = LA.flatten m
      arrFlat = A.fromStorableVector comp flat
      arr     = A.resize' (Sz (rs :. cs)) arrFlat
      out     = A.computeAs A.S (A.map f arr)
  in LA.reshape cs (A.toStorableVector (A.flatten out))

-- | Element-wise map over a hmatrix Vector using massiv. ~1.6× faster
-- than 'LA.cmap' on length-10000 vectors. Useful in IRLS / weighting
-- inner loops where 'cmap' runs many times per fit.
{-# INLINE mapVector #-}
mapVector :: (Double -> Double) -> LA.Vector Double -> LA.Vector Double
mapVector f v =
  let comp = compFor (LA.size v)
      arr  = A.fromStorableVector comp v
  in A.toStorableVector (A.computeAs A.S (A.map f arr))
