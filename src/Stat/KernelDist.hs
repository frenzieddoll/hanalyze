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

-- | Squared Euclidean norm of every row of @X@. Length-@n@ vector.
rowSqNorms :: LA.Matrix Double -> LA.Vector Double
rowSqNorms x = LA.fromList [ row `LA.dot` row | row <- LA.toRows x ]

-- | Pairwise squared distance among rows of one matrix.
--
-- @D[i, j] = ‖X[i,:] − X[j,:]‖²@ for @X@ of shape @n × p@; result is
-- @n × n@ with zeros on the diagonal (exactly).
pairwiseSqDist :: LA.Matrix Double -> LA.Matrix Double
pairwiseSqDist x =
  let n     = LA.rows x
      sq    = rowSqNorms x
      ones  = LA.konst 1 n :: LA.Vector Double
      r2    = LA.outer sq ones      -- n × n with ‖x_i‖² along rows
      c2    = LA.outer ones sq      -- n × n with ‖x_j‖² along cols
      cross = x LA.<> LA.tr x       -- BLAS GEMM
      d     = r2 + c2 - LA.scale 2 cross
  in zeroDiagonal (LA.cmap (max 0) d)
  where
    -- 数値誤差で対角が −ε になることがあるため陽に 0 化する。
    zeroDiagonal m =
      let n = LA.rows m
      in m - LA.diag (LA.takeDiag m) + LA.diagl (replicate n 0)

-- | Pairwise squared distance between rows of two matrices.
--
-- @D[i, j] = ‖X[i,:] − Y[j,:]‖²@ for @X@ of shape @m × p@ and @Y@ of
-- shape @n × p@; result is @m × n@.
pairwiseSqDistXY :: LA.Matrix Double -> LA.Matrix Double -> LA.Matrix Double
pairwiseSqDistXY x y =
  let m     = LA.rows x
      n     = LA.rows y
      sx    = rowSqNorms x
      sy    = rowSqNorms y
      onesM = LA.konst 1 m :: LA.Vector Double
      onesN = LA.konst 1 n :: LA.Vector Double
      r2    = LA.outer sx onesN     -- m × n with ‖x_i‖²
      c2    = LA.outer onesM sy     -- m × n with ‖y_j‖²
      cross = x LA.<> LA.tr y       -- BLAS GEMM
  in LA.cmap (max 0) (r2 + c2 - LA.scale 2 cross)
