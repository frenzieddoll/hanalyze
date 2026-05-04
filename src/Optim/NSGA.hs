{-# LANGUAGE OverloadedStrings #-}
-- | NSGA-II (Non-dominated Sorting Genetic Algorithm II) — Deb et al. 2002.
--
-- A widely-used multi-objective evolutionary algorithm based on fast
-- non-dominated sorting + crowding-distance comparison.
--
-- Algorithm:
--
-- @
-- 1. Generate the initial population P_0 (LHS or random).
-- 2. For t = 0..T:
--    a) Generate offspring Q_t (selection + SBX crossover + polynomial mutation).
--    b) R_t = P_t ∪ Q_t.
--    c) Fast non-dominated sort partitions R_t into fronts F_1, F_2, ...
--    d) Sort each front by crowding distance.
--    e) Take the top N to form P_{t+1}.
-- 3. Return the final front as a Pareto approximation.
-- @
module Optim.NSGA
  ( -- * 型
    Bounds
  , Solution (..)
  , NSGAConfig (..)
  , defaultNSGAConfig
    -- * 高レベル API
  , nsga2
  , nsga2WithConstraints
  , evaluateSolution
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
import qualified Optim.Common as OC

-- ---------------------------------------------------------------------------
-- 型
-- ---------------------------------------------------------------------------

-- | 各次元の探索範囲 (lo, hi)。`Optim.Common.Bounds` の再エクスポート。
type Bounds = OC.Bounds

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
-- 制約なし版。制約付きには 'nsga2WithConstraints' を使う。
nsga2 :: NSGAConfig
      -> ([Double] -> [Double])  -- ^ 目的関数 (m 次元出力)
      -> Bounds                  -- ^ 探索範囲 (d 次元)
      -> GenIO
      -> IO [Solution]
nsga2 cfg f bounds gen =
  nsga2WithConstraints cfg f (const 0) bounds gen

-- | 制約付き NSGA-II。`constrFun` は決定変数を受け取り、制約違反量を返す
-- (0 = 実行可能、>0 = 違反量)。複数の制約 g_i(x) ≤ 0 がある場合は
-- @sum [max 0 (g_i x)]@ などで集約して渡す。
nsga2WithConstraints
  :: NSGAConfig
  -> ([Double] -> [Double])    -- 目的関数 (m 次元)
  -> ([Double] -> Double)      -- 制約違反量 (≥ 0、0 = feasible)
  -> Bounds                    -- 探索範囲 (d 次元)
  -> GenIO
  -> IO [Solution]
nsga2WithConstraints cfg f cFn bounds gen = do
  let n  = nsgaPopSize cfg
      d  = length bounds
      pM = case nsgaMutationP cfg of
             Just p  -> p
             Nothing -> 1.0 / fromIntegral d
      etaC = nsgaEtaCross cfg
      etaM = nsgaEtaMut cfg
      pC   = nsgaCrossoverP cfg

  -- 初期母集団
  initPop <- mapM (const (do
                            x <- randomInBounds bounds gen
                            return (evaluateSolution f cFn x)))
                  [1 .. n]

  -- 世代ループ
  finalPop <- generationLoop (nsgaGenerations cfg) initPop pC etaC etaM pM bounds f cFn gen

  -- 最終世代の最初の front (Pareto 近似) を返す
  case nonDominatedSort finalPop of
    (front : _) -> return front
    []          -> return []

-- | 決定変数 x から Solution を作る (目的関数 + 制約評価)。
evaluateSolution :: ([Double] -> [Double])
                 -> ([Double] -> Double)
                 -> [Double]
                 -> Solution
evaluateSolution f cFn x =
  Solution { solDecision   = x
           , solObjectives = f x
           , solViolation  = cFn x
           }

-- | 1 世代の進化ステップを T 回反復。
generationLoop
  :: Int -> [Solution]
  -> Double -> Double -> Double -> Double  -- pC, etaC, etaM, pM
  -> Bounds
  -> ([Double] -> [Double])
  -> ([Double] -> Double)
  -> GenIO
  -> IO [Solution]
generationLoop 0 pop _ _ _ _ _ _ _ _ = return pop
generationLoop t pop pC etaC etaM pM bounds f cFn gen = do
  let n = length pop

  -- ── ranked + crowding 情報を計算 ──
  let fronts = nonDominatedSort pop
      sortedFronts = map crowdingDistance fronts
      -- (個体, rank, distance) のリスト
      ranked = concat
        [ zip3 (repeat r) (frontDistances fr) fr
        | (r, fr) <- zip [0 :: Int ..] sortedFronts ]
      -- ranked = [(rank, distance, sol)]

  -- ── 子母集団 Q を生成 ──
  -- N/2 ペアを生成 (各ペアで 2 子)
  let nPairs = n `div` 2
  childPairs <- mapM (const (makeChildPair pC etaC etaM pM bounds f cFn ranked gen))
                     [1 .. nPairs]
  let children = concatMap (\(c1, c2) -> [c1, c2]) childPairs
      -- N が奇数なら 1 個追加
      childrenAdj = if length children >= n
                       then take n children
                       else children   -- ほぼ起きない
      _ = childrenAdj

  -- ── R = P ∪ Q から上位 N を選別 ──
  let combined = pop ++ children
      combinedFronts = nonDominatedSort combined
      newPop = selectTopN n combinedFronts

  generationLoop (t - 1) newPop pC etaC etaM pM bounds f cFn gen

-- | 1 ペアの子 (c1, c2) を生成。tournament 選択 → SBX → mutation。
makeChildPair
  :: Double -> Double -> Double -> Double  -- pC, etaC, etaM, pM
  -> Bounds
  -> ([Double] -> [Double])
  -> ([Double] -> Double)
  -> [(Int, Double, Solution)]   -- ranked pop
  -> GenIO
  -> IO (Solution, Solution)
makeChildPair pC etaC etaM pM bounds f cFn ranked gen = do
  -- 親選び (tournament)
  let cmp (r1, d1, _) (r2, d2, _) = crowdedCompare (r1, d1) (r2, d2)
  (_, _, parent1) <- binaryTournament ranked cmp gen
  (_, _, parent2) <- binaryTournament ranked cmp gen

  -- SBX (確率 pC) または親をそのまま
  u <- uniform gen :: IO Double
  (c1Vec, c2Vec) <-
    if u < pC
      then sbxCrossover etaC bounds (solDecision parent1) (solDecision parent2) gen
      else return (solDecision parent1, solDecision parent2)

  -- Polynomial mutation
  c1Mut <- polynomialMutation etaM pM bounds c1Vec gen
  c2Mut <- polynomialMutation etaM pM bounds c2Vec gen

  return ( evaluateSolution f cFn c1Mut
         , evaluateSolution f cFn c2Mut )

-- | front の各個体の crowding distance を取り出す。
-- crowdingDistance はソート済リストを返すので、正確な距離は内部に隠れる。
-- 簡易対処: 距離を再計算してインデックスで突き合わせ。
frontDistances :: [Solution] -> [Double]
frontDistances front
  | length front <= 2 = replicate (length front) (1 / 0)
  | otherwise =
      let l    = length front
          m    = length (solObjectives (head front))
          ps   = zip [0 :: Int ..] front
          contrib objIdx =
            let sorted = sortBy
                          (comparing (\(_, s) -> solObjectives s !! objIdx)) ps
                vals   = map (\(_, s) -> solObjectives s !! objIdx) sorted
                fMin   = minimum vals
                fMax   = maximum vals
                rng    = fMax - fMin
            in if rng == 0
                 then [(idx, 0) | (idx, _) <- sorted]
                 else
                   let nL = length sorted
                       go k acc
                         | k <  0      = acc
                         | k == 0      = go (k + 1) ((fst (sorted !! k), 1/0) : acc)
                         | k == nL - 1 = (fst (sorted !! k), 1/0) : acc
                         | otherwise   =
                             let prev = vals !! (k - 1)
                                 next = vals !! (k + 1)
                                 d    = (next - prev) / rng
                             in go (k + 1) ((fst (sorted !! k), d) : acc)
                   in go 0 []
          totalDist origIdx =
            sum [ d | objIdx <- [0 .. m - 1]
                    , (i, d) <- contrib objIdx
                    , i == origIdx ]
      in [totalDist i | (i, _) <- ps]

-- | ソート済 fronts (上から良い順) から n 個を選別。
-- - 入る front は丸ごと採用
-- - 最後の front は crowding distance 順で半分採用
selectTopN :: Int -> [[Solution]] -> [Solution]
selectTopN _ [] = []
selectTopN n (fr : rest)
  | length fr >= n = take n (crowdingDistance fr)
  | otherwise =
      let fr' = fr  -- 全採用
          remaining = n - length fr
      in fr' ++ selectTopN remaining rest

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
-- `Optim.Common.sampleUniformIn` の thin wrapper (後方互換のため残置)。
randomInBounds :: Bounds -> GenIO -> IO [Double]
randomInBounds = OC.sampleUniformIn

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
