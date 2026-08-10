{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : Hanalyze.Model.ReliabilityBlockDiagram
-- Description : 信頼性ブロック図 (RBD) の直列/並列/k-out-of-n 再帰合成による系全体信頼度計算
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Reliability Block Diagram (RBD).
--
-- Computes system reliability from a structural composition of components
-- with known individual reliabilities. The three primitive combinators
-- are the textbook ones (e.g. O'Connor & Kleyner, /Practical Reliability
-- Engineering/):
--
--   * Series (every block must work):
--       @R = ∏ Rᵢ@
--   * Parallel (any block working suffices):
--       @R = 1 − ∏ (1 − Rᵢ)@
--   * k-out-of-n (at least @k@ of @n@ blocks must work):
--       @R = Σ_{i = k}^{n} P(exactly i succeed)@
--       computed by Poisson-binomial DP — works with heterogeneous block
--       reliabilities (the binomial closed form is the homogeneous
--       special case).
--
-- Blocks can be arbitrarily nested. Failure independence between blocks
-- is assumed (the standard RBD assumption).
--
-- @
-- import Hanalyze.Model.ReliabilityBlockDiagram
--
-- -- Two-out-of-three redundancy of three series strings:
-- let sys = KofN 2 [ Series [Leaf 0.95, Leaf 0.99]
--                  , Series [Leaf 0.95, Leaf 0.99]
--                  , Series [Leaf 0.95, Leaf 0.99] ]
--     r   = reliabilityOf sys
-- @
--
-- == Implemented
--
--   * 'RBDBlock' — composable tree of components.
--   * 'reliabilityOf' — recursive evaluation.
module Hanalyze.Model.ReliabilityBlockDiagram
  ( RBDBlock (..)
  , reliabilityOf
  ) where

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | A block in a reliability diagram. @Leaf p@ is a single component with
-- reliability @p ∈ [0, 1]@; the other constructors compose sub-blocks.
data RBDBlock
  = Leaf     !Double
  | Series   ![RBDBlock]
  | Parallel ![RBDBlock]
  | KofN     !Int ![RBDBlock]
  deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Evaluation
-- ---------------------------------------------------------------------------

-- | System reliability of a block, in @[0, 1]@. Component reliabilities
-- are assumed independent (the standard RBD assumption).
reliabilityOf :: RBDBlock -> Double
reliabilityOf (Leaf p)         = p
reliabilityOf (Series bs)      = product (map reliabilityOf bs)
reliabilityOf (Parallel bs)    = 1 - product [ 1 - reliabilityOf b | b <- bs ]
reliabilityOf (KofN k bs)
  | k <= 0           = 1                   -- always satisfied
  | k > length bs    = 0                   -- impossible
  | otherwise        =
      let ps     = map reliabilityOf bs
          n      = length ps
          -- Poisson-binomial DP: pmf!!i = P(exactly i blocks work).
          pmf    = foldr step [1.0] ps
            where
              step pi acc =
                -- acc = pmf of current partial product (length = current j + 1).
                let len = length acc
                in [ let aPrev = if i - 1 >= 0    then acc !! (i - 1) else 0
                         aHere = if i     <  len then acc !! i         else 0
                     in pi * aPrev + (1 - pi) * aHere
                   | i <- [0 .. len] ]
      in sum (drop k pmf)
