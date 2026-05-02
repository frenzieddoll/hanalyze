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
    -- * 遺伝的演算子 (Phase S3)
  , sbxCrossover
  , polynomialMutation
  , randomInBounds
  , binaryTournament
  , crowdedCompare
  ) where

import Control.Monad (zipWithM)
import Data.List (sortBy)
import Data.Ord  (comparing)
import System.Random.MWC (GenIO, uniform, uniformR)

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

-- ---------------------------------------------------------------------------
-- 遺伝的演算子 (Phase S3)
-- ---------------------------------------------------------------------------

-- | SBX (Simulated Binary Crossover, Deb 1995)。
--
-- 2 親 (p1, p2) から 2 子 (c1, c2) を生成。各次元独立に:
--
--   1. 確率 0.5 で交叉実施 (それ以外は親をそのままコピー)
--   2. \|p1 - p2\| < eps なら交叉せず親を返す (退化対策)
--   3. β ~ SBX 分布 (η_c で形状制御):
--        u ∈ [0, 0.5)  →  β = (2u)^(1/(η+1))
--        u ∈ [0.5, 1)  →  β = (1/(2(1-u)))^(1/(η+1))
--   4. c1 = 0.5 * ((1+β) p1 + (1-β) p2)
--      c2 = 0.5 * ((1-β) p1 + (1+β) p2)
--   5. 範囲外なら境界に clip
--
-- 大きい η_c は親付近に集中、小さい η_c はより広く探索。
sbxCrossover :: Double      -- η_c (分布指数、典型 15-20)
             -> Bounds      -- 各次元の範囲
             -> [Double]    -- 親 1
             -> [Double]    -- 親 2
             -> GenIO
             -> IO ([Double], [Double])
sbxCrossover etaC bounds p1 p2 gen = do
  pairs <- zipWithM (sbxOneVar etaC gen) bounds (zip p1 p2)
  let (c1, c2) = unzip pairs
  return (c1, c2)

sbxOneVar :: Double -> GenIO -> (Double, Double) -> (Double, Double)
          -> IO (Double, Double)
sbxOneVar etaC gen (lo, hi) (a, b) = do
  flip_ <- uniform gen :: IO Double  -- 各次元 50% で交叉
  if flip_ >= 0.5 || abs (a - b) < 1e-12
    then return (a, b)
    else do
      u <- uniform gen :: IO Double
      let beta
            | u < 0.5    = (2 * u) ** (1 / (etaC + 1))
            | otherwise  = (1 / (2 * (1 - u))) ** (1 / (etaC + 1))
          c1 = 0.5 * ((1 + beta) * a + (1 - beta) * b)
          c2 = 0.5 * ((1 - beta) * a + (1 + beta) * b)
          clip x = min hi (max lo x)
      return (clip c1, clip c2)

-- | Polynomial mutation (Deb & Goyal 1996)。
--
-- 各次元独立に確率 @pMut@ で:
--
--   δq = (2u)^(1/(η+1)) − 1               (u < 0.5)
--      = 1 − (2(1-u))^(1/(η+1))           (u ≥ 0.5)
--   y' = y + δq * (yU − yL)
--
-- 大きい η_m は元値付近、小さい η_m は大きい変異。
polynomialMutation :: Double    -- η_m (分布指数、典型 20)
                   -> Double    -- 突然変異確率 (典型 1/d)
                   -> Bounds
                   -> [Double]
                   -> GenIO
                   -> IO [Double]
polynomialMutation etaM pMut bounds xs gen =
  zipWithM (mutateOneVar etaM pMut gen) bounds xs

mutateOneVar :: Double -> Double -> GenIO -> (Double, Double) -> Double
             -> IO Double
mutateOneVar etaM pMut gen (lo, hi) x = do
  r <- uniform gen :: IO Double
  if r >= pMut
    then return x
    else do
      u <- uniform gen :: IO Double
      let dq
            | u < 0.5    = (2 * u) ** (1 / (etaM + 1)) - 1
            | otherwise  = 1 - (2 * (1 - u)) ** (1 / (etaM + 1))
          y = x + dq * (hi - lo)
      return (min hi (max lo y))

-- | 各次元の範囲から uniform にランダムドロー (初期母集団生成用)。
randomInBounds :: Bounds -> GenIO -> IO [Double]
randomInBounds bounds gen =
  mapM (\(lo, hi) -> do
           u <- uniform gen :: IO Double
           return (lo + u * (hi - lo)))
       bounds

-- | NSGA-II の crowded comparison operator:
--   1. rank が低い (front 番号小) 方が良い
--   2. rank 同じなら crowding distance 大が良い
--
-- LT = 第 1 引数が良い、GT = 第 2 引数が良い、EQ = 同等。
crowdedCompare :: (Int, Double) -> (Int, Double) -> Ordering
crowdedCompare (r1, d1) (r2, d2)
  | r1 < r2          = LT
  | r1 > r2          = GT
  | d1 > d2          = LT   -- 距離大が良い
  | d1 < d2          = GT
  | otherwise        = EQ

-- | 二項トーナメント選択。
-- pop からランダムに 2 個体取り、cmp に従って勝者を返す。
-- cmp x y == LT のとき x が勝者。
binaryTournament :: [a] -> (a -> a -> Ordering) -> GenIO -> IO a
binaryTournament pop cmp gen = do
  let n = length pop
  i <- uniformR (0, n - 1) gen
  j <- uniformR (0, n - 1) gen
  let xi = pop !! i
      xj = pop !! j
  return $ case cmp xi xj of
    GT -> xj
    _  -> xi
