-- | Constrained optimization via the **Augmented Lagrangian** method.
--
-- Internalizes equality constraints @g_i(x) = 0@ and inequality constraints
-- @h_j(x) ≤ 0@ via Lagrange multipliers + a quadratic penalty, exposing an
-- outer loop that calls an existing unconstrained solver (typically
-- @Optim.LBFGS@) on each subproblem.
--
-- Augmented Lagrangian:
--
-- @
-- L_A(x, λ, μ, ρ) = f(x)
--                 + Σ_i λ_i g_i(x) + (ρ/2) Σ_i g_i(x)²
--                 + Σ_j (1/(2ρ)) [max(0, μ_j + ρ h_j(x))² - μ_j²]
-- @
--
-- Each outer iteration:
--
--   1. Minimize @L_A@ in @x@ with the inner solver (L-BFGS or Nelder-Mead).
--   2. Update multipliers: @λ ← λ + ρ g(x*)@, @μ ← max(0, μ + ρ h(x*))@.
--   3. Grow the penalty @ρ@ if the constraint violation did not improve.
--
-- Reference: Nocedal & Wright, /Numerical Optimization/, Ch. 17.
module Optim.Constrained
  ( ConstrainedConfig (..)
  , ConstraintSet (..)
  , defaultConstrainedConfig
  , runAugmentedLagrangian
  , penaltyMethod
  , boxToIneq
  ) where

import qualified Optim.LBFGS  as LBFGS
import qualified Optim.Common as OC

-- | A set of constraints.
--
-- Equality constraints:   @g_i(x) = 0@.
-- Inequality constraints: @h_j(x) ≤ 0@.
data ConstraintSet = ConstraintSet
  { csEq   :: ![[Double] -> Double]   -- ^ Equality constraints @g_i@
                                      --   (the satisfying value is 0).
  , csIneq :: ![[Double] -> Double]   -- ^ Inequality constraints @h_j ≤ 0@.
  }

-- | Augmented Lagrangian configuration.
data ConstrainedConfig = ConstrainedConfig
  { ccOuterIter :: !Int                -- ^ Outer iterations (10–30 typical).
  , ccRho0      :: !Double             -- ^ Initial penalty coefficient @ρ₀@.
  , ccRhoGrowth :: !Double             -- ^ Growth rate for @ρ@ (2.0–10.0 typical).
  , ccTolViol   :: !Double             -- ^ Constraint-violation tolerance.
  , ccInnerStop :: !OC.StopCriteria    -- ^ Stop criteria for the inner L-BFGS solver.
  } deriving (Show, Eq)

-- | Default configuration: 20 outer iterations, @ρ₀ = 1.0@, growth 5.0,
-- violation tolerance 1e-6, inner solver capped at 200 iterations.
defaultConstrainedConfig :: ConstrainedConfig
defaultConstrainedConfig = ConstrainedConfig
  { ccOuterIter = 20
  , ccRho0      = 1.0
  , ccRhoGrowth = 5.0
  , ccTolViol   = 1e-6
  , ccInnerStop = OC.defaultStopCriteria { OC.stMaxIter = 200 }
  }

-- | Solve a constrained problem via the Augmented Lagrangian method.
--
-- Returns @(inner solver result, constraint-violation norm)@.
runAugmentedLagrangian
  :: ConstrainedConfig
  -> ([Double] -> Double)        -- ^ Objective (minimized).
  -> ConstraintSet
  -> [Double]                     -- ^ Initial point.
  -> IO (OC.OptimResult, Double)  -- ^ Inner L-BFGS result and violation norm.
runAugmentedLagrangian cfg f cs x0 = do
  let neq    = length (csEq cs)
      nineq  = length (csIneq cs)
      lam0   = replicate neq   0
      mu0    = replicate nineq 0
      rho0   = ccRho0 cfg
  go 0 x0 lam0 mu0 rho0
  where
    go iter x lam mu rho
      | iter >= ccOuterIter cfg = do
          r <- innerSolve x lam mu rho
          return (r, viol (OC.orBest r))
      | otherwise = do
          r <- innerSolve x lam mu rho
          let xNew = OC.orBest r
              vNorm = viol xNew
          if vNorm < ccTolViol cfg
            then return (r, vNorm)
            else do
              -- 乗数更新
              let lamN = zipWith (\l g_i -> l + rho * g_i) lam
                                 [g xNew | g <- csEq cs]
                  muN  = zipWith (\m h_j -> max 0 (m + rho * h_j)) mu
                                 [h xNew | h <- csIneq cs]
                  rhoN = rho * ccRhoGrowth cfg
              go (iter + 1) xNew lamN muN rhoN

    -- 拡張 Lagrangian を内側で最小化
    innerSolve x lam mu rho = do
      let lagrangian xs =
            let fx     = f xs
                eqVals = [g xs | g <- csEq cs]
                inVals = [h xs | h <- csIneq cs]
                eqTerm = sum (zipWith (*) lam eqVals)
                       + (rho / 2) * sum [v * v | v <- eqVals]
                inTerm = sum [ let z = max 0 (m + rho * v)
                               in (z * z - m * m) / (2 * rho)
                             | (m, v) <- zip mu inVals ]
            in fx + eqTerm + inTerm
          lcfg = LBFGS.defaultLBFGSConfig { LBFGS.lbStop = ccInnerStop cfg }
      LBFGS.runLBFGSNumeric lcfg lagrangian x

    -- 制約違反ノルム ||g||² + Σ max(0, h)²
    viol xs =
      let eqV = sum [(g xs)^(2::Int) | g <- csEq cs]
          ineqV = sum [(max 0 (h xs))^(2::Int) | h <- csIneq cs]
      in sqrt (eqV + ineqV)

-- | Expand box constraints (@lo_i ≤ x_i ≤ hi_i@) into two inequality
-- constraints (@≤ 0@) per dimension.
--
-- For each dimension @i@ this emits @lo_i - x_i ≤ 0@ (lower bound) and
-- @x_i - hi_i ≤ 0@ (upper bound). The returned list has length
-- @2 × length bs@.
--
-- @
-- let cs = ConstraintSet { csEq = []
--                        , csIneq = boxToIneq bs ++ otherIneq }
-- (r, viol) <- runAugmentedLagrangian defaultConstrainedConfig f cs x0
-- @
boxToIneq :: OC.Bounds -> [[Double] -> Double]
boxToIneq bs = concat
  [ [ \xs -> lo - (xs !! i)
    , \xs -> (xs !! i) - hi ]
  | (i, (lo, hi)) <- zip [0 ..] bs ]

-- | The simpler **penalty method** — a stripped-down Augmented Lagrangian
-- that omits the multiplier updates and only grows the penalty. Easy to
-- implement and lightweight, but prone to ill-conditioning.
penaltyMethod
  :: ConstrainedConfig
  -> ([Double] -> Double)
  -> ConstraintSet
  -> [Double]
  -> IO (OC.OptimResult, Double)
penaltyMethod cfg f cs x0 = do
  go 0 x0 (ccRho0 cfg)
  where
    go iter x rho
      | iter >= ccOuterIter cfg = do
          r <- innerSolve x rho
          return (r, viol (OC.orBest r))
      | otherwise = do
          r <- innerSolve x rho
          let xNew = OC.orBest r
              vNorm = viol xNew
          if vNorm < ccTolViol cfg
            then return (r, vNorm)
            else go (iter + 1) xNew (rho * ccRhoGrowth cfg)

    innerSolve x rho = do
      let penalty xs =
            let fx     = f xs
                eqV    = sum [(g xs)^(2::Int) | g <- csEq cs]
                ineqV  = sum [(max 0 (h xs))^(2::Int) | h <- csIneq cs]
            in fx + (rho / 2) * (eqV + ineqV)
          lcfg = LBFGS.defaultLBFGSConfig { LBFGS.lbStop = ccInnerStop cfg }
      LBFGS.runLBFGSNumeric lcfg penalty x

    viol xs =
      let eqV = sum [(g xs)^(2::Int) | g <- csEq cs]
          ineqV = sum [(max 0 (h xs))^(2::Int) | h <- csIneq cs]
      in sqrt (eqV + ineqV)
