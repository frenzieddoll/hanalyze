{-# LANGUAGE StrictData #-}
-- | Cholesky-based linear solver for symmetric positive-definite (SPD)
-- systems.
--
-- Replaces the generic least-squares solve @LA.\<\\\>@ in code paths
-- where the matrix is known to be SPD (Gram matrices @K + λI@, posterior
-- precision matrices, etc.). hmatrix's @\<\\\>@ uses the LAPACK QR
-- (@dgels@) which is general but ~2-3× slower than the SPD-specific
-- Cholesky (@dpotrf@ + @dpotrs@).
--
-- The solver also handles near-singular matrices by progressively
-- adding a multiple of the identity (jittering) until the Cholesky
-- factorization succeeds.
module Stat.Cholesky
  ( cholSolve
  , cholSolveJitter
  , cholSolveJitterWith
  , cholFactor
  , cholSolveWithFactor
  ) where

import qualified Numeric.LinearAlgebra as LA
import           Control.Exception     (SomeException, try, evaluate)
import           System.IO.Unsafe      (unsafePerformIO)

-- | Default sequence of jitter ratios applied to the diagonal until the
-- Cholesky factorization succeeds. The first attempt adds nothing; the
-- subsequent attempts add @ratio × max(diag(A))@ (the largest diagonal
-- entry, used to scale to the matrix's natural magnitude).
defaultJitters :: [Double]
defaultJitters = [0, 1e-10, 1e-8, 1e-6, 1e-4]

-- | Solve @A X = B@ for SPD @A@. Equivalent to @A LA.\<\\\> B@ but ~2×
-- faster. Tries an exact Cholesky first, falling back to a jittered
-- version (see @defaultJitters@) when the matrix is numerically
-- non-positive-definite.
--
-- If every jitter fails, returns 'Nothing' (caller chooses a fallback;
-- typically 'LA.\<\\\>').
cholSolve :: LA.Matrix Double -> LA.Matrix Double -> Maybe (LA.Matrix Double)
cholSolve = cholSolveJitterWith defaultJitters
{-# INLINE cholSolve #-}

-- | Like 'cholSolve' but always returns a result by falling back to
-- @LA.\<\\\>@ (the general LSQ solver) if the Cholesky path fails for
-- every jitter level. Logs no information about which jitter level (if
-- any) was used; for diagnostics, call 'cholSolveJitterWith' directly.
cholSolveJitter :: LA.Matrix Double -> LA.Matrix Double -> LA.Matrix Double
cholSolveJitter a b = case cholSolve a b of
  Just x  -> x
  Nothing -> a LA.<\> b

-- | Try a custom sequence of jitter ratios. Returns 'Nothing' when none
-- succeeds.
cholSolveJitterWith
  :: [Double] -> LA.Matrix Double -> LA.Matrix Double
  -> Maybe (LA.Matrix Double)
cholSolveJitterWith jitters a b
  | LA.rows a /= LA.cols a = Nothing      -- not square
  | otherwise              = go jitters
  where
    n     = LA.rows a
    sigma = max 1.0 (LA.maxElement (LA.cmap abs (LA.takeDiag a)))
    go []         = Nothing
    go (eps : rest) =
      let aPlus = if eps <= 0 then a
                  else a + LA.scale (eps * sigma) (LA.ident n)
      in case tryChol aPlus of
           Nothing -> go rest
           Just r  ->
             -- A = Rᵀ R. Solve Rᵀ y = B then R X = y.
             let y = LA.triSolve LA.Lower (LA.tr r) b
                 x = LA.triSolve LA.Upper r y
             in Just x

-- | Wrapper around @LA.chol (LA.sym a)@ that catches the LAPACK error
-- (raised as a Haskell exception) when the matrix is not SPD.
cholFactor :: LA.Matrix Double -> Maybe (LA.Matrix Double)
cholFactor = tryChol
{-# INLINE cholFactor #-}

-- | Solve @A X = B@ given an /already-computed/ Cholesky factor @R@
-- (from 'cholFactor', upper-triangular with @A = Rᵀ R@). Cheaper when
-- the same factor is used for multiple right-hand sides or when the
-- factor was needed elsewhere (e.g. for the log-determinant during
-- marginal-likelihood evaluation).
cholSolveWithFactor :: LA.Matrix Double -> LA.Matrix Double -> LA.Matrix Double
cholSolveWithFactor r b =
  LA.triSolve LA.Upper r (LA.triSolve LA.Lower (LA.tr r) b)
{-# INLINE cholSolveWithFactor #-}

tryChol :: LA.Matrix Double -> Maybe (LA.Matrix Double)
tryChol a =
  let r = unsafePerformIO $
            try (evaluate (LA.chol (LA.sym a)))
              :: Either SomeException (LA.Matrix Double)
  in case r of
       Right x -> Just x
       Left  _ -> Nothing
