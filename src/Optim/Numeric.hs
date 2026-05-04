{-# LANGUAGE OverloadedStrings #-}
-- | Numeric gradients (finite differences).
--
-- For situations where automatic differentiation is impractical (e.g. GP
-- log-marginal likelihood whose @det@ is computed inside hmatrix and would
-- be cumbersome to AD-ify).
--
--   * 'numGradCentral' — central differences (error @O(h²)@; recommended).
--   * 'numGradForward' — forward differences (error @O(h)@; half the cost).
--   * 'numHessianCentral' — Hessian approximation via central differences.
module Optim.Numeric
  ( numGradCentral
  , numGradForward
  , numHessianCentral
  ) where

-- | Central-difference gradient.
--
-- @∂f/∂x_i ≈ (f(x + h e_i) − f(x − h e_i)) / (2h)@.
numGradCentral :: Double                       -- ^ Step size @h@.
               -> ([Double] -> Double)         -- ^ Objective @f@.
               -> [Double] -> [Double]
numGradCentral h f x =
  [ (f (set i (x !! i + h)) - f (set i (x !! i - h))) / (2 * h)
  | i <- [0 .. length x - 1] ]
  where
    set i v = take i x ++ [v] ++ drop (i + 1) x

-- | One-sided forward-difference gradient (half the cost of
-- 'numGradCentral'):
--
-- @∂f/∂x_i ≈ (f(x + h e_i) − f(x)) / h@.
numGradForward :: Double -> ([Double] -> Double) -> [Double] -> [Double]
numGradForward h f x =
  let fx = f x
  in [ (f (set i (x !! i + h)) - fx) / h
     | i <- [0 .. length x - 1] ]
  where
    set i v = take i x ++ [v] ++ drop (i + 1) x

-- | Hessian approximation by mixed forward differences.
--
-- @∂²f/∂x_i∂x_j ≈ [f(x+h eᵢ+h eⱼ) − f(x+h eᵢ) − f(x+h eⱼ) + f(x)] / h²@.
--
-- Forward-only, so accuracy is @O(h)@. The fully central variant would
-- be more accurate at four times the cost.
numHessianCentral :: Double -> ([Double] -> Double) -> [Double] -> [[Double]]
numHessianCentral h f x =
  [ [ second i j | j <- [0 .. n - 1] ]
  | i <- [0 .. n - 1] ]
  where
    n = length x
    set k v = take k x ++ [v] ++ drop (k + 1) x
    setBoth i j vi vj =
      let x1 = set i vi
      in take j x1 ++ [vj] ++ drop (j + 1) x1
    fx = f x
    second i j =
      let f_ij = f (setBoth i j (x !! i + h) (x !! j + h))
          f_i  = f (set i (x !! i + h))
          f_j  = f (set j (x !! j + h))
      in (f_ij - f_i - f_j + fx) / (h * h)
