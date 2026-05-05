{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-full-laziness -fno-cse #-}
-- | Standalone bench to compare hmatrix vs massiv for pairwise squared
-- distance computation. Tests the F4 plan's central question: can
-- massiv outperform hmatrix on the kernel-distance hot path?
--
-- All paths are pure Haskell (no unsafe*, no raw pointers).
module Main where

import qualified Numeric.LinearAlgebra as LA
import qualified Stat.KernelDist       as KD
import qualified Data.Massiv.Array     as A
import           Data.Massiv.Array     ( Array, Comp (..), Ix2 (..), Sz (..) )
import           Data.Time.Clock       (getCurrentTime, diffUTCTime)
import           Control.DeepSeq       (NFData, deepseq)

-- ---------------------------------------------------------------------------
-- hmatrix ↔ massiv conversion (safe API only)
-- ---------------------------------------------------------------------------

-- | hmatrix 'LA.Matrix' → massiv @Array S Ix2 Double@. Round-trips
-- through the row-major flat 'LA.Vector' (Storable) which both sides
-- understand, then resizes via massiv's 'A.resize''.
hMatrixToMassiv :: LA.Matrix Double -> Array A.S Ix2 Double
hMatrixToMassiv m =
  let rs = LA.rows m
      cs = LA.cols m
      v  = LA.flatten m  -- LA.Vector Double (Storable, row-major)
      arrFlat = A.fromStorableVector Seq v  -- Array S Ix1 Double
  in A.resize' (Sz (rs :. cs)) arrFlat

-- | massiv @Array S Ix2 Double@ → hmatrix 'LA.Matrix'. Uses
-- 'A.toStorableVector' (no copy if storage matches) and reshapes.
massivToHMatrix :: Array A.S Ix2 Double -> LA.Matrix Double
massivToHMatrix a =
  let Sz (_ :. cs) = A.size a
      flat         = A.toStorableVector (A.flatten a)
  in LA.reshape cs flat

-- ---------------------------------------------------------------------------
-- pairwiseSqDist via massiv
-- ---------------------------------------------------------------------------

-- | Pairwise squared distance using massiv. Uses the identity
-- @D[i,j] = ‖x_i‖² + ‖x_j‖² − 2 X·Xᵀ@.
pairwiseSqDistMassiv :: LA.Matrix Double -> LA.Matrix Double
pairwiseSqDistMassiv x =
  let am   = hMatrixToMassiv x          -- n × p
      Sz (n :. _p) = A.size am
      -- (am * am) is element-wise square
      sq  = A.compute (am A.!*! A.compute (A.transpose am)) :: Array A.U Ix2 Double
      _   = sq
      -- Easier: do the linear-algebra side via hmatrix BLAS, only the
      -- elementwise piece in massiv.
      sqVec = LA.fromList [ row `LA.dot` row | row <- LA.toRows x ]  -- placeholder
      _ = sqVec
      _ = n
  in pairwiseSqDistMassiv2 x

-- | Cleaner version: keep matrix multiply in hmatrix BLAS (faster for
-- now), do only the elementwise +/- part in massiv.
pairwiseSqDistMassiv2 :: LA.Matrix Double -> LA.Matrix Double
pairwiseSqDistMassiv2 x =
  let n     = LA.rows x
      sq    = KD.rowSqNorms x                           -- length n
      ones  = LA.konst 1 n :: LA.Vector Double
      r2    = LA.outer sq ones                          -- n × n
      c2    = LA.outer ones sq                          -- n × n
      cross = x LA.<> LA.tr x                           -- BLAS GEMM
      -- elementwise: r2 + c2 - 2*cross, then max 0, then zero diagonal
      mr2   = hMatrixToMassiv r2
      mc2   = hMatrixToMassiv c2
      mcr   = hMatrixToMassiv cross
      mDiff = A.computeAs A.S
                (A.zipWith3
                   (\a b c -> max 0 (a + b - 2 * c))
                   mr2 mc2 mcr)
      raw   = massivToHMatrix mDiff
  in raw - LA.diag (LA.takeDiag raw) + LA.diagl (replicate n 0)

-- | Fusion-friendly version: skip the r2 / c2 outer-product
-- intermediates entirely. Build the result via massiv's index-based
-- 'A.makeArrayR': for each (i, j) read sq[i], sq[j] and cross[i, j]
-- in a single sweep — no per-position write to r2/c2.
pairwiseSqDistMassiv3 :: LA.Matrix Double -> LA.Matrix Double
pairwiseSqDistMassiv3 x =
  let n     = LA.rows x
      sq    = KD.rowSqNorms x                           -- length n (Storable)
      cross = x LA.<> LA.tr x                           -- n × n, BLAS GEMM
      sqA   = A.fromStorableVector Seq sq               -- Array S Ix1 Double
      crA   = hMatrixToMassiv cross                     -- Array S Ix2 Double
      raw   = A.computeAs A.S $
                A.makeArrayR A.D Seq (Sz (n :. n)) $ \(i :. j) ->
                  if i == j
                    then 0
                    else max 0 ( A.index' sqA i
                               + A.index' sqA j
                               - 2 * A.index' crA (i :. j) )
      result = massivToHMatrix raw
  in result

-- | Same as v3 but with parallel comp.
pairwiseSqDistMassiv3Par :: LA.Matrix Double -> LA.Matrix Double
pairwiseSqDistMassiv3Par x =
  let n     = LA.rows x
      sq    = KD.rowSqNorms x
      cross = x LA.<> LA.tr x
      sqA   = A.fromStorableVector Par sq
      crA0  = hMatrixToMassiv cross
      crA   = A.setComp Par crA0
      raw   = A.computeAs A.S $
                A.makeArrayR A.D Par (Sz (n :. n)) $ \(i :. j) ->
                  if i == j
                    then 0
                    else max 0 ( A.index' sqA i
                               + A.index' sqA j
                               - 2 * A.index' crA (i :. j) )
  in massivToHMatrix raw

-- ---------------------------------------------------------------------------
-- Benchmark loop
-- ---------------------------------------------------------------------------

timeIt :: NFData a => String -> IO a -> IO a
timeIt label act = do
  -- Warm-up
  !w <- act
  w `deepseq` pure ()
  t0 <- getCurrentTime
  let n = 5
  results <- mapM (\_ -> do { !r <- act; r `deepseq` pure r }) [1 .. n]
  t1 <- getCurrentTime
  let totalMs = realToFrac (diffUTCTime t1 t0) * 1000 :: Double
      avgMs   = totalMs / fromIntegral n
  putStrLn $ label ++ ": " ++ show avgMs ++ " ms (avg over " ++ show n ++ " runs)"
  pure (head results)

main :: IO ()
main = do
  let mkX seedBase n p =
        LA.fromLists
          [ [ sin (fromIntegral (seedBase * i + j))
            | j <- [1 .. p] ]
          | i <- [1 .. n] ]
      x500   = mkX 1  500   20
      x1000  = mkX 1  1000  20
      x2000  = mkX 1  2000  20

  putStrLn "=== Conversion overhead (hmatrix ↔ massiv ↔ hmatrix) ==="
  _ <- timeIt "  conv only n=2000" $ pure $! massivToHMatrix (hMatrixToMassiv x2000)

  putStrLn ""
  putStrLn "=== pairwiseSqDist n=500 ==="
  d1 <- timeIt "  hmatrix" $ pure $! KD.pairwiseSqDist x500
  d2 <- timeIt "  massiv" $ pure $! pairwiseSqDistMassiv2 x500
  let !diff1 = LA.norm_2 (LA.flatten (d1 - d2))
  putStrLn $ "  numeric diff (should be 0): " ++ show diff1

  putStrLn ""
  putStrLn "=== pairwiseSqDist n=1000 ==="
  d3 <- timeIt "  hmatrix" $ pure $! KD.pairwiseSqDist x1000
  d4 <- timeIt "  massiv" $ pure $! pairwiseSqDistMassiv2 x1000
  let !diff2 = LA.norm_2 (LA.flatten (d3 - d4))
  putStrLn $ "  numeric diff (should be 0): " ++ show diff2

  putStrLn ""
  putStrLn "=== pairwiseSqDist n=2000 ==="
  d5 <- timeIt "  hmatrix" $ pure $! KD.pairwiseSqDist x2000
  d6 <- timeIt "  massiv v2 (zipWith3)" $ pure $! pairwiseSqDistMassiv2 x2000
  d7 <- timeIt "  massiv v3 (makeArray)" $ pure $! pairwiseSqDistMassiv3 x2000
  let !diff3 = LA.norm_2 (LA.flatten (d5 - d6))
      !diff4 = LA.norm_2 (LA.flatten (d5 - d7))
  putStrLn $ "  v2 numeric diff: " ++ show diff3
  putStrLn $ "  v3 numeric diff: " ++ show diff4

  putStrLn ""
  putStrLn "=== Parallel comp (Par instead of Seq) for v3 ==="
  d8 <- timeIt "  massiv v3 Par" $ pure $! pairwiseSqDistMassiv3Par x2000
  let !diff5 = LA.norm_2 (LA.flatten (d5 - d8))
  putStrLn $ "  numeric diff: " ++ show diff5
