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

-- | 中央差分による勾配:
--   ∂f/∂x_i ≈ (f(x + h e_i) − f(x − h e_i)) / (2h)
numGradCentral :: Double                       -- ^ 刻み幅 h
               -> ([Double] -> Double)         -- ^ 目的関数 f
               -> [Double] -> [Double]
numGradCentral h f x =
  [ (f (set i (x !! i + h)) - f (set i (x !! i - h))) / (2 * h)
  | i <- [0 .. length x - 1] ]
  where
    set i v = take i x ++ [v] ++ drop (i + 1) x

-- | 前進差分による勾配 (片側、コスト半分):
--   ∂f/∂x_i ≈ (f(x + h e_i) − f(x)) / h
numGradForward :: Double -> ([Double] -> Double) -> [Double] -> [Double]
numGradForward h f x =
  let fx = f x
  in [ (f (set i (x !! i + h)) - fx) / h
     | i <- [0 .. length x - 1] ]
  where
    set i v = take i x ++ [v] ++ drop (i + 1) x

-- | 中央差分による Hessian 近似:
--   ∂²f/∂x_i∂x_j ≈ [f(x+h e_i+h e_j) − f(x+h e_i) − f(x+h e_j) + f(x)] / h²
--   (前進差分版なので O(h)、より高精度な中央差分の二重適用は計算コスト 4 倍)
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
