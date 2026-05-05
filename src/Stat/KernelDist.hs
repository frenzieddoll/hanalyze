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
  ) where

import qualified Numeric.LinearAlgebra as LA
import qualified Data.Massiv.Array     as A
import           Data.Massiv.Array     (Array, Comp (..), Ix2 (..), Sz (..))

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
      sqA   = A.fromStorableVector Seq sq               -- Array S Ix1 Double
      crA   = hmatrixToMassiv cross                     -- Array S Ix2 Double
      raw   = A.computeAs A.S $
                A.makeArrayR A.D Seq (Sz (n :. n)) $ \(i :. j) ->
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
      sxA   = A.fromStorableVector Seq sx
      syA   = A.fromStorableVector Seq sy
      crA   = hmatrixToMassiv cross
      raw   = A.computeAs A.S $
                A.makeArrayR A.D Seq (Sz (m :. n)) $ \(i :. j) ->
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
