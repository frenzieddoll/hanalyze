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

import           Data.List   (sortBy)
import           Data.Ord    (comparing)

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
  let m         = length ps
      indexed   = zip [0 :: Int ..] ps
      sorted    = sortBy (comparing snd) indexed
      adjusted  = scanlMonotone
                    [ min 1 (p * fromIntegral (m - i))
                    | (i, (_, p)) <- zip [0..] sorted ]
      reordered = zip (map fst sorted) adjusted
  in [ snd (head [pa | pa <- reordered, fst pa == i])
     | i <- [0 .. m - 1] ]

-- | Benjamini-Hochberg (BH) FDR control.
benjaminiHochberg :: [Double] -> [Double]
benjaminiHochberg ps =
  let m         = length ps
      indexed   = zip [0 :: Int ..] ps
      sorted    = sortBy (comparing snd) indexed
      adjustedR = [ min 1 (p * fromIntegral m / fromIntegral (i + 1))
                  | (i, (_, p)) <- zip [0..] sorted ]
      -- Step-up: enforce monotone non-increasing from largest to smallest
      -- so that a smaller p never gets a larger q-value.
      adjusted  = reverse (scanlMonotone (reverse adjustedR))
      reordered = zip (map fst sorted) adjusted
  in [ snd (head [pa | pa <- reordered, fst pa == i])
     | i <- [0 .. m - 1] ]

-- | Benjamini-Yekutieli (BY) FDR control under arbitrary dependence.
-- Multiplies each BH q-value by the harmonic-number factor
-- @c(m) = Σ_{i=1..m} 1/i@.
benjaminiYekutieli :: [Double] -> [Double]
benjaminiYekutieli ps =
  let m   = length ps
      cM  = sum [ 1 / fromIntegral i | i <- [1..m] ] :: Double
      bh  = benjaminiHochberg ps
  in map (\p -> min 1 (p * cM)) bh

-- | Cumulative max from left to right (used for monotone enforcement).
scanlMonotone :: [Double] -> [Double]
scanlMonotone []     = []
scanlMonotone (x:xs) = scanl1 max (x:xs)
