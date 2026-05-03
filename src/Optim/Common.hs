-- | 単目的最適化アルゴリズム共通基盤。
--
-- すべての単目的オプティマイザ (`Optim.NelderMead`, `Optim.LBFGS`,
-- `Optim.LineSearch`, `Optim.DifferentialEvolution`, `Optim.CMAES`) が
-- 共有する型・既定値を提供する。
--
-- 各オプティマイザの実行関数は次の形に揃える:
--
-- @
-- runX :: XConfig -> ([Double] -> Double) -> [Double] -> IO OptimResult
-- @
--
-- (決定的なものも `IO` を返して統一。`pure`-only 版が必要なら別途エクスポート)
module Optim.Common
  ( OptimResult (..)
  , StopCriteria (..)
  , defaultStopCriteria
  , Direction (..)
  , flipFor
  ) where

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
