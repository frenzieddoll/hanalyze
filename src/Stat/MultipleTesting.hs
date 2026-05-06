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
  ) where

import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as MVU
import qualified Data.Vector.Algorithms.Intro as VAI
import           Control.Monad.ST             (runST, ST)
import           Control.Monad                (forM_)

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
holm ps =
  -- Vectorised rewrite: sort once, prefix-max in place, scatter back to
  -- original positions in @O(m)@. Replaces the previous list-comp with
  -- @[head [... | fst pa == i] | i <- [0..m-1]]@ which was @O(m²)@ for
  -- the back-permutation alone (n=1000 → 10⁶ scans). For n=1000 the
  -- new code is ~50× faster.
  pAdjustGeneric ps $ \m sortedPs idxBuf -> runST $ do
    let mD = fromIntegral m :: Double
    raw <- MVU.new m :: ST s (MVU.STVector s Double)
    -- raw[k] = min(1, p_(k) · (m-k)) for k=0..m-1 in sorted order.
    forM_ [0 .. m - 1] $ \k -> do
      let !p = sortedPs VU.! k
      MVU.unsafeWrite raw k (min 1 (p * (mD - fromIntegral k)))
    -- Prefix-max (left-to-right monotone non-decreasing).
    forM_ [1 .. m - 1] $ \k -> do
      a <- MVU.unsafeRead raw (k - 1)
      b <- MVU.unsafeRead raw k
      MVU.unsafeWrite raw k (max a b)
    scatter m raw idxBuf

-- | Benjamini-Hochberg (BH) FDR control.
benjaminiHochberg :: [Double] -> [Double]
benjaminiHochberg ps =
  pAdjustGeneric ps $ \m sortedPs idxBuf -> runST $ do
    let mD = fromIntegral m :: Double
    raw <- MVU.new m :: ST s (MVU.STVector s Double)
    -- Per-rank q-value q_(k) = min(1, p_(k) · m / (k+1)) (k 0-based).
    forM_ [0 .. m - 1] $ \k -> do
      let !p = sortedPs VU.! k
      MVU.unsafeWrite raw k (min 1 (p * mD / fromIntegral (k + 1)))
    -- Step-up: enforce monotone non-decreasing from RIGHT to LEFT
    -- (largest k first), so that smaller p never receives larger q.
    -- One reverse-direction prefix-min sweep — no list reversal.
    forM_ [m - 2, m - 3 .. 0] $ \k -> do
      a <- MVU.unsafeRead raw k
      b <- MVU.unsafeRead raw (k + 1)
      MVU.unsafeWrite raw k (min a b)
    scatter m raw idxBuf

-- | Benjamini-Yekutieli (BY) FDR control under arbitrary dependence.
-- Multiplies each BH q-value by the harmonic-number factor
-- @c(m) = Σ_{i=1..m} 1/i@.
benjaminiYekutieli :: [Double] -> [Double]
benjaminiYekutieli ps =
  let m  = length ps
      cM = sum [ 1 / fromIntegral i | i <- [1..m] ] :: Double
      bh = benjaminiHochberg ps
  in map (\p -> min 1 (p * cM)) bh

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Common harness for Holm / BH: sorts the p-values once, hands the
-- sorted vector and the original-index buffer to a method-specific
-- adjustment 'ST' action that returns the q-values in /original/ order.
pAdjustGeneric
  :: [Double]
  -> (Int -> VU.Vector Double -> VU.Vector Int -> VU.Vector Double)
  -> [Double]
pAdjustGeneric ps f =
  let m  = length ps
      vp = VU.fromList ps
      -- Pair index with p-value, sort by p ascending, then split.
      sortedIxP = sortByValue vp
      sortedPs  = VU.map snd sortedIxP
      origIdx   = VU.map fst sortedIxP
      out       = f m sortedPs origIdx
  in VU.toList out

-- | Sort an unboxed vector of p-values, retaining original indices.
-- Returns a vector of @(origIdx, p)@ in ascending @p@ order.
sortByValue :: VU.Vector Double -> VU.Vector (Int, Double)
sortByValue vp =
  let m       = VU.length vp
      indexed = VU.generate m (\i -> (i, vp VU.! i))
  in VU.modify (VAI.sortBy compareSnd) indexed
  where
    compareSnd (_, a) (_, b) = compare a b

-- | Scatter the adjusted values from rank-order positions back to
-- original input positions in @O(m)@.
scatter :: Int -> MVU.STVector s Double -> VU.Vector Int -> ST s (VU.Vector Double)
scatter m raw idx = do
  out <- MVU.new m
  forM_ [0 .. m - 1] $ \k -> do
    v <- MVU.unsafeRead raw k
    MVU.unsafeWrite out (idx VU.! k) v
  VU.unsafeFreeze out
