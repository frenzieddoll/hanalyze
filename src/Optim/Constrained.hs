-- | 制約付き最適化 — **Augmented Lagrangian** 法。
--
-- 等式制約 g_i(x) = 0 と不等式制約 h_j(x) ≤ 0 を Lagrange 乗数 + 二次罰則で
-- 内部化し、無制約問題として既存の `Optim.LBFGS` 等で解く外側ループを提供。
--
-- 拡張 Lagrangian:
--
--   L_A(x, λ, μ, ρ) = f(x)
--                   + Σ_i λ_i g_i(x) + (ρ/2) Σ_i g_i(x)²
--                   + Σ_j (1/(2ρ)) [max(0, μ_j + ρ h_j(x))² - μ_j²]
--
-- 各外側反復で:
--   1. L_A を x について最小化 (内側 = L-BFGS or Nelder-Mead)
--   2. 乗数を更新: λ ← λ + ρ g(x*)、μ ← max(0, μ + ρ h(x*))
--   3. 罰則 ρ を増加 (制約違反が改善しなければ)
--
-- 参考: Nocedal & Wright "Numerical Optimization" Ch. 17。
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

-- | 制約セット。
--
-- 等式制約: g_i(x) = 0
-- 不等式制約: h_j(x) ≤ 0
data ConstraintSet = ConstraintSet
  { csEq   :: ![[Double] -> Double]   -- ^ 等式制約 g_i (= 0 が満たすべき値)
  , csIneq :: ![[Double] -> Double]   -- ^ 不等式制約 h_j (≤ 0)
  }

-- | Augmented Lagrangian の設定。
data ConstrainedConfig = ConstrainedConfig
  { ccOuterIter :: !Int           -- ^ 外側反復数 (典型 10-30)
  , ccRho0      :: !Double         -- ^ 初期罰則係数 ρ_0
  , ccRhoGrowth :: !Double         -- ^ ρ の成長率 (典型 2.0-10.0)
  , ccTolViol   :: !Double         -- ^ 制約違反 tolerance
  , ccInnerStop :: !OC.StopCriteria   -- ^ 内側 LBFGS の停止基準
  } deriving (Show, Eq)

defaultConstrainedConfig :: ConstrainedConfig
defaultConstrainedConfig = ConstrainedConfig
  { ccOuterIter = 20
  , ccRho0      = 1.0
  , ccRhoGrowth = 5.0
  , ccTolViol   = 1e-6
  , ccInnerStop = OC.defaultStopCriteria { OC.stMaxIter = 200 }
  }

-- | Augmented Lagrangian で制約付き最適化を解く。
--
-- 戻り値: (最良 x, f(x), 制約違反ノルム)。
runAugmentedLagrangian
  :: ConstrainedConfig
  -> ([Double] -> Double)        -- ^ 目的関数 (最小化)
  -> ConstraintSet
  -> [Double]                     -- ^ 初期点
  -> IO (OC.OptimResult, Double)  -- (内部 LBFGS の結果, 制約違反ノルム)
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

-- | box 制約 (各次元 lo_i ≤ x_i ≤ hi_i) を 2 本ずつの不等式制約 (≤ 0) に展開。
--
-- 各次元 i から `lo_i - x_i ≤ 0` (下限) と `x_i - hi_i ≤ 0` (上限) を生成。
-- 戻り値は @2 * length bs@ 本の `[Double] -> Double` リスト。
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

-- | シンプルな **罰則法** (penalty method)。
-- 拡張 Lagrangian の簡易版で、乗数更新を省略し罰則だけを増加させる。
-- 実装が単純で軽量だが、ill-conditioning が起きやすい。
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
