{-# LANGUAGE OverloadedStrings #-}
-- | Desirability functions (Derringer & Suich 1980).
--
-- A classical scalarization for multi-objective optimization. Each response
-- @y_j@ is mapped to a per-response desirability @d_j ∈ [0, 1]@, and the
-- overall desirability is the geometric mean:
--
-- @
-- D = (Π d_j)^(1/q)
-- @
--
-- The @x@ that maximizes @D@ is a point that satisfies all responses
-- reasonably well.
module Hanalyze.Optim.Desirability
  ( DesirabilityType (..)
  , individualDesirability
  , overallDesirability
  ) where

-- | The three desirability shapes.
data DesirabilityType
  = Maximize  Double Double          -- ^ Maximize: thresholds @low@ (→ 0) and @high@ (→ 1).
  | Minimize  Double Double          -- ^ Minimize: thresholds @high@ (→ 0) and @low@ (→ 1).
  | Target    Double Double Double   -- ^ Target value @t@ with allowed range @[low, high]@.
  deriving (Show, Eq)

-- | Compute the individual desirability @d_j(y)@.
individualDesirability :: DesirabilityType -> Double -> Double
individualDesirability dt y = case dt of
  Maximize lo hi
    | y <= lo   -> 0
    | y >= hi   -> 1
    | otherwise -> (y - lo) / (hi - lo)
  Minimize hi lo
    | y >= hi   -> 0
    | y <= lo   -> 1
    | otherwise -> (hi - y) / (hi - lo)
  Target t lo hi
    | y == t                  -> 1
    | y < lo || y > hi        -> 0
    | y < t                   -> (y - lo) / (t - lo)
    | otherwise               -> (hi - y) / (hi - t)

-- | Overall desirability @D = (Π d_j)^(1/q)@.
--
-- Any single zero collapses @D@ to zero — out-of-range responses are
-- strongly penalized.
overallDesirability :: [DesirabilityType] -> [Double] -> Double
overallDesirability dts ys
  | length dts /= length ys = 0
  | null ys                 = 0
  | otherwise =
      let ds = zipWith individualDesirability dts ys
          q  = fromIntegral (length ds) :: Double
      in if any (<= 0) ds then 0
           else (product ds) ** (1 / q)
