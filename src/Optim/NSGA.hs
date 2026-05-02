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
    -- * 構成要素
  , dominates
  , paretoDominates
  , nonDominatedSort
  , crowdingDistance
  ) where

import Data.List (sortBy)
import Data.Ord  (comparing)
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

-- | 個体 a が個体 b を **支配** するか (制約付き Pareto dominance)。
--
-- 制約 (Deb 2000 "constrained-domination"):
--   1. a が実行可能 (violation = 0) かつ b が不実行可能 → a が支配
--   2. 両方不実行可能 → violation の小さい方が支配
--   3. 両方実行可能 → 通常の Pareto dominance
--      (∀ i: a_i ≤ b_i) かつ (∃ j: a_j < b_j)
dominates :: Solution -> Solution -> Bool
dominates a b
  | va == 0 && vb >  0 = True
  | va >  0 && vb == 0 = False
  | va >  0 && vb >  0 = va < vb
  | otherwise          = paretoDominates (solObjectives a) (solObjectives b)
  where
    va = solViolation a
    vb = solViolation b

-- | 通常の (制約無視) Pareto dominance: a dominates b ⇔
-- ∀ i: aᵢ ≤ bᵢ かつ ∃ j: aⱼ < bⱼ。
paretoDominates :: [Double] -> [Double] -> Bool
paretoDominates as bs =
  all (\(x, y) -> x <= y) zipped
    && any (\(x, y) -> x <  y) zipped
  where
    zipped = zip as bs

-- | 非優越ソート (Deb 2002 fast nondominated sort)。
-- 母集団を Pareto front に分割: F_1 (最も非優越), F_2, ...
--
-- アルゴリズム (O(MN²)):
--
--   for each p in P:
--     n_p = |{q : q dominates p}|        -- p を支配する数
--     S_p = {q : p dominates q}          -- p が支配する集合
--     if n_p = 0: p ∈ F_1
--   for i = 1, 2, ...:
--     for each p in F_i, each q in S_p:
--       n_q -= 1
--       if n_q = 0: q ∈ F_{i+1}
nonDominatedSort :: [Solution] -> [[Solution]]
nonDominatedSort [] = []
nonDominatedSort pop =
  let n = length pop
      idxs = [0 .. n - 1]
      ps   = pop                 -- インデックスでアクセス
      -- (S_p, n_p) を計算
      domInfo i =
        let pi = ps !! i
            (sp, np) = foldr step ([], 0 :: Int) idxs
            step j (s, c)
              | i == j               = (s, c)
              | dominates pi (ps !! j) = (j : s, c)
              | dominates (ps !! j) pi = (s, c + 1)
              | otherwise              = (s, c)
        in (sp, np)
      info = [domInfo i | i <- idxs]   -- [(S_i, n_i)]
      -- F_1 の構築
      front1 = [i | (i, (_, np)) <- zip idxs info, np == 0]
      -- 反復で次の front を作る
      go currentFront acc remaining =
        if null currentFront then reverse acc
        else
          let -- currentFront の S_p 集合から各 q について n_q を 1 減らし、
              -- 0 になったものを次の front に
              decrements = concat [ fst (info !! i) | i <- currentFront ]
              counts'    = foldr (\j m -> updateAt j (subtract 1) m) remaining decrements
              nextF      = [j | j <- [0 .. length counts' - 1]
                              , counts' !! j == 0
                              , j `elem` candidatePool]
              candidatePool = [j | j <- [0 .. n - 1]
                                 , j `notElem` concat (currentFront : acc)]
          in go nextF (currentFront : acc) counts'
      -- mutable-style update on list
      updateAt :: Int -> (a -> a) -> [a] -> [a]
      updateAt _ _ [] = []
      updateAt 0 f (x:xs) = f x : xs
      updateAt k f (x:xs) = x : updateAt (k - 1) f xs
      initialCounts = map snd info
      idxFronts = go front1 [] initialCounts
  in map (map (ps !!)) idxFronts

-- | 各 front 内で crowding distance (Deb 2002) を計算し、距離降順にソート。
--
-- アルゴリズム (O(MN log N)):
--
--   for each m in objectives:
--     sort I by f_m
--     I[0].dist = I[l-1].dist = ∞
--     for i = 1..l-2:
--       I[i].dist += (f_m(i+1) - f_m(i-1)) / (f_max_m - f_min_m)
--
-- 戻り値: 距離の降順 (= 多様性が高い個体が先頭)。NSGA-II の選別で使う。
crowdingDistance :: [Solution] -> [Solution]
crowdingDistance front
  | length front <= 2 = front           -- 全員 ∞ 扱いなので順序不問
  | otherwise =
      let l    = length front
          m    = length (solObjectives (head front))
          ps   = zip [0 ..] front       -- index 付き
          -- 各目的について寄与を計算
          contributions :: Int -> [(Int, Double)]
          contributions objIdx =
            let sorted = sortBy
                          (comparing (\(_, s) -> solObjectives s !! objIdx)) ps
                vals   = map (\(_, s) -> solObjectives s !! objIdx) sorted
                fMin   = minimum vals
                fMax   = maximum vals
                rng    = fMax - fMin
            in if rng == 0
                 then [(idx, 0) | (idx, _) <- sorted]
                 else
                   let n = length sorted
                       go k acc
                         | k <  0      = acc
                         | k == 0      = go (k + 1) ((fst (sorted !! k), inf) : acc)
                         | k == n - 1  = (fst (sorted !! k), inf) : acc
                         | otherwise   =
                             let prev = vals !! (k - 1)
                                 next = vals !! (k + 1)
                                 d    = (next - prev) / rng
                             in go (k + 1) ((fst (sorted !! k), d) : acc)
                   in go 0 []
          -- 全目的の寄与を合算
          totalDist :: Int -> Double
          totalDist origIdx =
            sum [ d | objIdx <- [0 .. m - 1]
                    , (i, d) <- contributions objIdx
                    , i == origIdx ]
          withDist = [(totalDist i, s) | (i, s) <- ps]
          -- 距離降順にソート
          sortedDesc = sortBy (\(d1, _) (d2, _) -> compare d2 d1) withDist
      in map snd sortedDesc
  where
    inf = 1 / 0  -- ∞
