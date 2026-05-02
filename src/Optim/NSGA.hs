{-# LANGUAGE OverloadedStrings #-}
-- | NSGA-II (Non-dominated Sorting Genetic Algorithm II) — Deb et al. 2002。
--
-- **Phase R0.2 — スケルトン段階**: 型と関数シグネチャのみ定義し、
-- 本実装は Phase S で行う。これにより:
--
-- - 型レベルで API 設計を確定
-- - Phase S 内で `undefined` を順次実装に置換
-- - 後続 Phase (T, U, V) は型を信頼してコードを書ける
--
-- アルゴリズム (Phase S で実装):
--
-- @
-- 1. 初期母集団 P_0 を生成 (LHS or random)
-- 2. for t = 0..T:
--    a) 子母集団 Q_t を生成 (selection + SBX crossover + polynomial mutation)
--    b) R_t = P_t ∪ Q_t
--    c) Non-dominated sorting で R_t を front F_1, F_2, ... に分割
--    d) crowding distance で各 front 内をソート
--    e) P_{t+1} を上位 N 個から取る
-- 3. 最終 front を Pareto 近似として返す
-- @
module Optim.NSGA
  ( -- * 型
    Bounds
  , Solution (..)
  , NSGAConfig (..)
  , defaultNSGAConfig
    -- * 高レベル API (Phase S で実装)
  , nsga2
    -- * 構成要素 (Phase S1〜S3 で実装)
  , dominates
  , nonDominatedSort
  , crowdingDistance
  ) where

import System.Random.MWC (GenIO)

-- ---------------------------------------------------------------------------
-- 型
-- ---------------------------------------------------------------------------

-- | 各次元の探索範囲 (lo, hi)。
type Bounds = [(Double, Double)]

-- | 個体: 決定変数 + 評価結果 (目的関数値ベクトル) + 制約違反量。
data Solution = Solution
  { solDecision   :: [Double]   -- ^ 決定変数 (長さ d)
  , solObjectives :: [Double]   -- ^ 目的関数値 (長さ m)、すべて最小化問題として扱う
  , solViolation  :: Double     -- ^ 制約違反量 (0 = 実行可能、>0 = 違反)
  } deriving (Show, Eq)

-- | NSGA-II の設定。
data NSGAConfig = NSGAConfig
  { nsgaPopSize     :: Int       -- ^ 母集団サイズ N (偶数推奨)
  , nsgaGenerations :: Int       -- ^ 世代数 T
  , nsgaCrossoverP  :: Double    -- ^ 交叉確率 p_c (default 0.9)
  , nsgaMutationP   :: Maybe Double  -- ^ 突然変異確率 (Nothing = 1/d)
  , nsgaEtaCross    :: Double    -- ^ SBX の分布指数 η_c (default 15)
  , nsgaEtaMut      :: Double    -- ^ Polynomial mutation の η_m (default 20)
  } deriving (Show)

defaultNSGAConfig :: NSGAConfig
defaultNSGAConfig = NSGAConfig
  { nsgaPopSize     = 100
  , nsgaGenerations = 200
  , nsgaCrossoverP  = 0.9
  , nsgaMutationP   = Nothing
  , nsgaEtaCross    = 15.0
  , nsgaEtaMut      = 20.0
  }

-- ---------------------------------------------------------------------------
-- API (実装は Phase S で行う)
-- ---------------------------------------------------------------------------

-- | NSGA-II 本体。`objFun` は決定変数を受け取り目的関数値ベクトルを返す。
-- 戻り値は最終世代の Pareto 近似 front (= rank 0 の個体集合)。
--
-- TODO Phase S3 で実装。
nsga2 :: NSGAConfig
      -> ([Double] -> [Double])  -- ^ 目的関数 (m 次元出力)
      -> Bounds                  -- ^ 探索範囲 (d 次元)
      -> GenIO
      -> IO [Solution]
nsga2 _cfg _f _bounds _gen = error "Optim.NSGA.nsga2: not yet implemented (Phase S3)"

-- | 個体 a が個体 b を **支配** するか (Pareto dominance)。
--
-- a dominates b ⇔ ∀ i: a_i ≤ b_i かつ ∃ j: a_j < b_j
-- 制約付きの場合: 実行可能性が優先される。
--
-- TODO Phase S1 で実装。
dominates :: Solution -> Solution -> Bool
dominates _a _b = error "Optim.NSGA.dominates: not yet implemented (Phase S1)"

-- | 非優越ソート (Deb 2002 fast nondominated sort)。
-- 母集団を front F_1, F_2, ... に分割し、front の rank 列を返す。
--
-- TODO Phase S1 で実装。
nonDominatedSort :: [Solution] -> [[Solution]]
nonDominatedSort _pop = error "Optim.NSGA.nonDominatedSort: not yet implemented (Phase S1)"

-- | 各 front 内で crowding distance を計算し、降順にソートする。
--
-- TODO Phase S1 で実装。
crowdingDistance :: [Solution] -> [Solution]
crowdingDistance _front = error "Optim.NSGA.crowdingDistance: not yet implemented (Phase S1)"
