-- | Common foundation for the single-objective optimization algorithms.
--
-- Provides the shared types and defaults used by every single-objective
-- optimizer (@Optim.NelderMead@, @Optim.LBFGS@, @Optim.LineSearch@,
-- @Optim.DifferentialEvolution@, @Optim.CMAES@, @Optim.CMAESFull@,
-- @Optim.SimulatedAnnealing@, @Optim.ParticleSwarm@), plus the unified
-- 'Bounds' type for box constraints.
--
-- Each optimizer's runner has the same shape:
--
-- @
-- runX :: XConfig -> ([Double] -> Double) -> [Double] -> IO OptimResult
-- @
--
-- (Deterministic algorithms also return @IO@ for uniformity. A pure-only
-- variant can be exported separately when needed.)
module Optim.Common
  ( OptimResult (..)
  , StopCriteria (..)
  , defaultStopCriteria
  , Direction (..)
  , flipFor
    -- * Box constraints (探索範囲)
  , Bounds
  , clipToBounds
  , projectToBounds
  , sampleUniformIn
  , boundsPenalty
  , inBounds
  ) where

import Control.Monad (forM)
import qualified System.Random.MWC as MWC

-- | 最適化方向。
data Direction = Minimize | Maximize deriving (Show, Eq)

-- | 停止基準。
--
-- - @stMaxIter@   : 最大反復数
-- - @stTolFun@    : |Δf| < tol で収束
-- - @stTolX@      : ||Δx||∞ < tol で収束 (Nelder-Mead 等の単体サイズで使う場合あり)
data StopCriteria = StopCriteria
  { stMaxIter :: !Int
  , stTolFun  :: !Double
  , stTolX    :: !Double
  } deriving (Show, Eq)

-- | 標準的な汎用設定。汎用ベンチでは十分。
defaultStopCriteria :: StopCriteria
defaultStopCriteria = StopCriteria
  { stMaxIter = 1000
  , stTolFun  = 1e-8
  , stTolX    = 1e-10
  }

-- | 最適化結果。
data OptimResult = OptimResult
  { orBest      :: ![Double]   -- ^ 最良点 x*
  , orValue     :: !Double     -- ^ 最良値 f(x*)  (内部は常に最小化値で持つ)
  , orHistory   :: ![Double]   -- ^ 反復ごとの best value 履歴 (最大 stMaxIter+1 個)
  , orIters     :: !Int        -- ^ 実反復回数
  , orConverged :: !Bool       -- ^ tol 判定で打ち切られたか
  } deriving (Show, Eq)

-- | 内部 (常に最小化) ↔ ユーザー指定の `Direction` を変換するヘルパ。
-- 各オプティマイザが先頭で適用し、結果を呼び出し時に再変換する想定。
--
-- > flipFor Maximize f x = -(f x)
-- > flipFor Minimize f x =   f x
flipFor :: Direction -> ([Double] -> Double) -> ([Double] -> Double)
flipFor Minimize f = f
flipFor Maximize f = negate . f

-- ---------------------------------------------------------------------------
-- Box constraints (各次元の上下限)
-- ---------------------------------------------------------------------------

-- | 各次元の (下限, 上限) のリスト。
type Bounds = [(Double, Double)]

-- | 反射 (reflection) で範囲外を内側に折り返す。
-- 過大な逸脱は範囲幅でクランプ。
clipToBounds :: Bounds -> [Double] -> [Double]
clipToBounds bs xs = zipWith reflect bs xs
  where
    reflect (lo, hi) x
      | x < lo    = let d = lo - x in lo + min d (hi - lo)
      | x > hi    = let d = x - hi in hi - min d (hi - lo)
      | otherwise = x

-- | 単純切り捨て (clip)。範囲外を境界値に貼り付ける。
projectToBounds :: Bounds -> [Double] -> [Double]
projectToBounds bs xs =
  zipWith (\(lo, hi) x -> max lo (min hi x)) bs xs

-- | 一様乱数で 1 個体生成 (DE/PSO/SA/NSGA 共通の初期化)。
sampleUniformIn :: Bounds -> MWC.GenIO -> IO [Double]
sampleUniformIn bs gen = forM bs $ \(lo, hi) -> MWC.uniformR (lo, hi) gen

-- | 範囲外への soft penalty。L-BFGS / Nelder-Mead で目的関数に加算する想定。
-- 範囲内なら 0、範囲外なら $\sum_i (\text{距離}_i)^2 \cdot k$ (k は十分大きい定数)。
--
-- @
-- objWithPenalty xs = f xs + boundsPenalty (Just bs) xs
-- @
boundsPenalty :: Maybe Bounds -> [Double] -> Double
boundsPenalty Nothing   _  = 0
boundsPenalty (Just bs) xs =
  let k = 1e6 :: Double
      dists = zipWith dist bs xs
  in k * sum [d * d | d <- dists]
  where
    dist (lo, hi) x
      | x < lo    = lo - x
      | x > hi    = x - hi
      | otherwise = 0

-- | 全次元が bounds 内なら True。
inBounds :: Bounds -> [Double] -> Bool
inBounds bs xs = all (\((lo, hi), x) -> x >= lo && x <= hi) (zip bs xs)
