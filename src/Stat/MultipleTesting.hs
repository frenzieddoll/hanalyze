{-# LANGUAGE OverloadedStrings #-}
-- | Multiple-testing correction.
--
-- Adjusts a list of p-values to control either:
--
--   * Family-wise error rate (FWER):
--     'bonferroni', 'holm'
--   * False discovery rate (FDR):
--     'benjaminiHochberg' (BH), 'benjaminiYekutieli' (BY)
--
-- All functions take and return @[Double]@; the order of input
-- p-values is preserved in the output.
module Stat.MultipleTesting
  ( CorrectionMethod (..)
  , pAdjust
    -- * Individual methods
  , bonferroni
  , holm
  , benjaminiHochberg
  , benjaminiYekutieli
    -- * Storable-vector variants (avoid boxed list ↔ unboxed Vector
    -- conversions; the same numerical algorithms as the @[Double]@
    -- versions above, but accepting and returning @VU.Vector Double@).
  , benjaminiHochbergV
  , holmV
  ) where

import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as MVU
import qualified Data.Vector.Algorithms.Intro as VAI
import           Control.Monad.ST             (runST, ST)

-- | Correction method.
data CorrectionMethod
  = Bonferroni
  | Holm
  | BenjaminiHochberg   -- ^ FDR (BH 1995)
  | BenjaminiYekutieli  -- ^ FDR under arbitrary dependence (BY 2001)
  deriving (Show, Eq)

-- | Apply a correction by name.
pAdjust :: CorrectionMethod -> [Double] -> [Double]
pAdjust Bonferroni         = bonferroni
pAdjust Holm               = holm
pAdjust BenjaminiHochberg  = benjaminiHochberg
pAdjust BenjaminiYekutieli = benjaminiYekutieli

-- | Bonferroni: @p_adj = min(1, p · m)@ where @m@ is the number of tests.
-- Most conservative; controls FWER.
bonferroni :: [Double] -> [Double]
bonferroni ps =
  let m = fromIntegral (length ps) :: Double
  in map (\p -> min 1 (p * m)) ps

-- | Holm-Bonferroni step-down: less conservative than 'bonferroni',
-- still controls FWER.
holm :: [Double] -> [Double]
holm = VU.toList . holmV . VU.fromList

-- | Holm step-down on an unboxed vector — see 'benjaminiHochbergV'
-- for the rationale on bypassing the @[Double]@ API.
holmV :: VU.Vector Double -> VU.Vector Double
holmV ps = runST $ do
  let !m  = VU.length ps
      !mD = fromIntegral m :: Double
  if m <= 1
    then return ps
    else do
      idx <- VU.thaw (VU.generate m id) :: ST s (MVU.STVector s Int)
      VAI.sortBy (\i j -> compare (VU.unsafeIndex ps i) (VU.unsafeIndex ps j)) idx
      idxV <- VU.unsafeFreeze idx
      raw  <- MVU.new m
      let goRaw !k
            | k >= m    = pure ()
            | otherwise = do
                let !p = VU.unsafeIndex ps (VU.unsafeIndex idxV k)
                    !q = min 1 (p * (mD - fromIntegral k))
                MVU.unsafeWrite raw k q
                goRaw (k + 1)
      goRaw 0
      let goMax !k
            | k >= m    = pure ()
            | otherwise = do
                a <- MVU.unsafeRead raw (k - 1)
                b <- MVU.unsafeRead raw k
                MVU.unsafeWrite raw k (max a b)
                goMax (k + 1)
      goMax 1
      out <- MVU.new m
      let goSc !k
            | k >= m    = pure ()
            | otherwise = do
                v <- MVU.unsafeRead raw k
                MVU.unsafeWrite out (VU.unsafeIndex idxV k) v
                goSc (k + 1)
      goSc 0
      VU.unsafeFreeze out

-- | Benjamini-Hochberg (BH) FDR control.
benjaminiHochberg :: [Double] -> [Double]
benjaminiHochberg = VU.toList . benjaminiHochbergV . VU.fromList

-- | BH on an unboxed 'VU.Vector Double'. Equivalent to
-- 'benjaminiHochberg' but skips the @[Double]@↔@VU.Vector Double@
-- conversion, which on the n=1000 bench dominates the @[Double]@
-- API by a 2× factor (boxed-Double allocation + GC pressure).
--
-- Numerical algorithm:
--
--   1. argsort p ascending.
--   2. raw_k = min(1, p_(k) · m / (k+1)).
--   3. Right-to-left prefix-min on @raw@ (step-up monotonisation).
--   4. Scatter back to original positions.
--
-- All steps are written as hand-rolled ST loops (not @forM_ [0..m-1]@)
-- so we avoid the per-iter list-cell allocation that GHC otherwise
-- has to fuse away.
benjaminiHochbergV :: VU.Vector Double -> VU.Vector Double
benjaminiHochbergV ps = runST $ do
  let !m  = VU.length ps
      !mD = fromIntegral m :: Double
  if m <= 1
    then return ps
    else do
      idx <- VU.thaw (VU.generate m id) :: ST s (MVU.STVector s Int)
      VAI.sortBy (\i j -> compare (VU.unsafeIndex ps i) (VU.unsafeIndex ps j)) idx
      idxV <- VU.unsafeFreeze idx
      raw  <- MVU.new m
      -- raw_k = min(1, p_(k) · m / (k+1))
      let goRaw !k
            | k >= m    = pure ()
            | otherwise = do
                let !p = VU.unsafeIndex ps (VU.unsafeIndex idxV k)
                    !q = min 1 (p * mD / fromIntegral (k + 1))
                MVU.unsafeWrite raw k q
                goRaw (k + 1)
      goRaw 0
      -- Right-to-left prefix-min monotonisation.
      let goMin !k
            | k < 0     = pure ()
            | otherwise = do
                a <- MVU.unsafeRead raw k
                b <- MVU.unsafeRead raw (k + 1)
                MVU.unsafeWrite raw k (min a b)
                goMin (k - 1)
      goMin (m - 2)
      -- Scatter back to original positions.
      out <- MVU.new m
      let goSc !k
            | k >= m    = pure ()
            | otherwise = do
                v <- MVU.unsafeRead raw k
                MVU.unsafeWrite out (VU.unsafeIndex idxV k) v
                goSc (k + 1)
      goSc 0
      VU.unsafeFreeze out

-- | Benjamini-Yekutieli (BY) FDR control under arbitrary dependence.
-- Multiplies each BH q-value by the harmonic-number factor
-- @c(m) = Σ_{i=1..m} 1/i@.
benjaminiYekutieli :: [Double] -> [Double]
benjaminiYekutieli ps =
  let m  = length ps
      cM = sum [ 1 / fromIntegral i | i <- [1..m] ] :: Double
      bh = benjaminiHochberg ps
  in map (\p -> min 1 (p * cM)) bh

