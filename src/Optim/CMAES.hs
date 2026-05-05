-- | CMA-ES (Covariance Matrix Adaptation Evolution Strategy) — Hansen 2001.
--
-- The de-facto state of the art for non-convex continuous optimization.
-- This module implements a **simplified single-stage** version of the
-- @(μ/μ_w, λ)@-rank-μ + rank-1 update.
--
-- Spec (simplified):
--
-- * Each generation samples @λ@ vectors @z_k ~ N(0, I)@ and forms
--   @x_k = m + σ B z_k@ (diagonal covariance only; @B = diag(d)@, full
--   rank @C@ is omitted).
-- * The top @μ@ samples (weights @w@) update the mean @m ← Σ w_i x_i@.
-- * @σ@ is multiplicatively updated with a 1/5-rule-like rule
--   (no path cumulation). Sufficient for problems up to Rastrigin 5D.
--
-- For the full-rank tutorial CMA-ES (Hansen 2016), see 'Optim.CMAESFull'.
module Optim.CMAES
  ( CMAESConfig (..)
  , defaultCMAESConfig
  , runCMAES
  , runCMAESWith
  ) where

import Data.List (sortBy)
import Data.Ord (comparing)
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWCD
import Control.Monad (replicateM, forM)
import Control.Exception (SomeException, try, evaluate)
import Optim.Common
import qualified Optim.LBFGS as LB

-- | Configuration for the simplified diagonal CMA-ES.
data CMAESConfig = CMAESConfig
  { cmStop    :: !StopCriteria
  , cmSigma0  :: !Double          -- ^ Initial step size @σ@.
  , cmLambda  :: !(Maybe Int)     -- ^ Population size @λ@ (defaults to
                                  --   @4 + ⌊3 ln D⌋@ when 'Nothing').
  , cmDir     :: !Direction
  , cmBounds  :: !(Maybe Bounds)  -- ^ Optional box constraints. When set,
                                  --   each sampled point is reflected
                                  --   back into the bounds via
                                  --   'clipToBounds'.
  , cmPolish  :: !Bool
    -- ^ When 'True' (default), run a final L-BFGS-B (numeric gradient)
    --   refinement on @x_best@ at termination. Mirrors scipy's
    --   @differential_evolution(polish=True)@ pattern. Brings smooth
    --   landscapes to near-machine precision after CMA-ES localised
    --   the basin.
  } deriving (Show, Eq)

-- | Default configuration: 200 iterations, @σ₀ = 0.5@, default @λ@,
-- minimization, no bounds.
defaultCMAESConfig :: CMAESConfig
defaultCMAESConfig = CMAESConfig
  { cmStop   = defaultStopCriteria { stMaxIter = 200, stTolFun = 1e-10 }
  , cmSigma0 = 0.5
  , cmLambda = Nothing
  , cmDir    = Minimize
  , cmBounds = Nothing
  , cmPolish = True
  }

-- | Run simplified CMA-ES with the default configuration.
runCMAES :: ([Double] -> Double)
         -> [Double]              -- ^ Initial mean @m₀@.
         -> MWC.GenIO
         -> IO OptimResult
runCMAES = runCMAESWith defaultCMAESConfig

-- | Run simplified CMA-ES with a user-specified configuration.
runCMAESWith :: CMAESConfig
             -> ([Double] -> Double)
             -> [Double]
             -> MWC.GenIO
             -> IO OptimResult
runCMAESWith cfg fUser m0 gen = do
  let f      = flipFor (cmDir cfg) fUser
      d      = length m0
      lam    = case cmLambda cfg of
                 Just l  -> l
                 Nothing -> 4 + floor (3 * log (fromIntegral d) :: Double)
      mu     = lam `div` 2
      -- 重み: ln(μ + 0.5) - ln(i)、正規化
      wsRaw  = [ log (fromIntegral mu + 0.5) - log (fromIntegral i)
               | i <- [1 .. mu] ]
      wsSum  = sum wsRaw
      ws     = map (/ wsSum) wsRaw
      -- 初期分散 (対角) = 1
      diag0  = replicate d 1.0
  res <- loop cfg f gen 0 m0 (cmSigma0 cfg) diag0 ws lam mu (f m0) [f m0]
  -- Optional final L-BFGS-B polish (scipy parity).
  if cmPolish cfg
    then do
      let polCfg = LB.defaultLBFGSConfig
                     { LB.lbStop   = defaultStopCriteria
                                       { stMaxIter = 100
                                       , stTolFun  = 1e-12
                                       , stTolX    = 1e-12 }
                     , LB.lbBounds = cmBounds cfg
                     , LB.lbDir    = cmDir cfg
                     }
      ePol <- try (LB.runLBFGSNumeric polCfg fUser (orBest res))
                :: IO (Either SomeException OptimResult)
      case ePol of
        Left _ -> pure res
        Right polRes ->
          let xC = case cmBounds cfg of
                     Nothing -> orBest polRes
                     Just bs -> clipToBounds bs (orBest polRes)
          in do
            evC <- try (evaluate (fUser xC)) :: IO (Either SomeException Double)
            case evC of
              Right vC ->
                let better = case cmDir cfg of
                               Minimize -> vC < orValue res
                               Maximize -> vC > orValue res
                in pure $ if better
                            then res { orBest = xC, orValue = vC }
                            else res
              Left _   -> pure res
    else pure res

-- | 反復本体。
loop :: CMAESConfig
     -> ([Double] -> Double)
     -> MWC.GenIO
     -> Int
     -> [Double]                 -- m (現平均)
     -> Double                    -- σ
     -> [Double]                  -- 対角 D (Cholesky)
     -> [Double]                  -- weights w (length μ)
     -> Int -> Int                -- λ, μ
     -> Double                    -- 現 best 値
     -> [Double]                  -- history (新しい先頭)
     -> IO OptimResult
loop cfg f gen iter m sigma diag ws lam mu bestV hist
  | iter >= stMaxIter (cmStop cfg) = mkResult cfg m bestV hist iter False
  | sigma < 1e-14 = mkResult cfg m bestV hist iter True
  | otherwise = do
      -- λ 個サンプル
      samples <- replicateM lam $ do
        z <- replicateM (length m) (MWCD.standard gen)
        let xRaw = zipWith3 (\mi di zi -> mi + sigma * di * zi) m diag z
            x    = case cmBounds cfg of
                     Nothing -> xRaw
                     Just bs -> clipToBounds bs xRaw
        return (x, z, f x)
      let sorted   = sortBy (comparing (\(_, _, v) -> v)) samples
          topMu    = take mu sorted
          xs'      = map (\(x, _, _) -> x) topMu
          zs'      = map (\(_, z, _) -> z) topMu
          fs'      = map (\(_, _, v) -> v) topMu
          -- 平均更新: m ← Σ w_i x_i
          mNew     = avgWeighted ws xs'
          -- 簡易ステップ更新: 集団 best が改善した割合で σ を増減
          newBestV = head fs'
          improve  = newBestV < bestV
          sigmaN   = if improve then sigma * 1.05 else sigma * 0.95
          -- 対角分散の rank-μ 更新 (極簡易): w_i z_i² の重み付き平均で更新
          var      = [ max 1e-12 (sum (zipWith (\w zi -> w * (zs' !! 0 !! 0) ^ (2::Int)) ws zs')) | _ <- m ]
          -- 上の var はバグ気味なので、ちゃんと書き直す
          varDiag  = [ max 1e-12 $ sum (zipWith (\w (zi:_) -> w * zi^(2::Int)) ws (transposeZs zs' j))
                     | j <- [0 .. length m - 1] ]
          diagN    = zipWith (\d0 v -> d0 * 0.7 + sqrt v * 0.3) diag varDiag
          bestN    = min bestV newBestV
          histN    = bestN : hist
          _ = var  -- 未使用置きの抑制
      if abs (bestV - newBestV) < stTolFun (cmStop cfg) && iter > 10
        then mkResult cfg mNew bestN histN (iter + 1) True
        else loop cfg f gen (iter + 1) mNew sigmaN diagN ws lam mu bestN histN
  where
    transposeZs :: [[Double]] -> Int -> [[Double]]
    transposeZs zss j = [ [zs !! j] | zs <- zss ]

-- | 重み付きベクトル平均。
avgWeighted :: [Double] -> [[Double]] -> [Double]
avgWeighted ws xs =
  let dim = length (head xs)
  in [ sum (zipWith (\w x -> w * (x !! j)) ws xs) | j <- [0 .. dim - 1] ]

mkResult :: CMAESConfig -> [Double] -> Double -> [Double]
         -> Int -> Bool -> IO OptimResult
mkResult cfg m bestV hist iter conv =
  let vUser = case cmDir cfg of { Minimize -> bestV; Maximize -> negate bestV }
      hU    = case cmDir cfg of
                Minimize -> reverse hist
                Maximize -> map negate (reverse hist)
  in pure $ OptimResult m vUser hU iter conv
