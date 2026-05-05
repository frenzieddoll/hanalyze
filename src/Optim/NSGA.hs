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
    -- * High-level API
  , nsga2
  , nsga2WithConstraints
  , evaluateSolution
    -- * Building blocks
  , dominates
  , paretoDominates
  , nonDominatedSort
  , crowdingDistance
    -- * Matrix-based internal API (N3)
  , PopMatrix (..)
  , fromSolutions
  , toSolutions
  , dominationMatrix
    -- * Genetic operators
  , sbxCrossover
  , polynomialMutation
  , randomInBounds
  , binaryTournament
  , crowdedCompare
  ) where

import Control.Monad (zipWithM)
import Data.List (sortBy)
import Data.Ord  (comparing)
import qualified Data.IntSet as IS
import qualified Data.Vector as V
import System.Random.MWC (GenIO, uniform, uniformR)
import qualified Numeric.LinearAlgebra as LA
import qualified Optim.Common    as OC
import qualified Stat.QuasiRandom as QR

-- ---------------------------------------------------------------------------
-- 型
-- ---------------------------------------------------------------------------

-- | Per-dimension @(lo, hi)@ bounds. Re-exported from 'Optim.Common.Bounds'.
type Bounds = OC.Bounds

-- | An individual: decision variables, objective-value vector, and
-- constraint violation.
data Solution = Solution
  { solDecision   :: [Double]   -- ^ Decision vector (length @d@).
  , solObjectives :: [Double]   -- ^ Objective values (length @m@); all
                                --   objectives are treated as minimized.
  , solViolation  :: Double     -- ^ Constraint violation (0 = feasible,
                                --   @> 0@ = violated).
  } deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- PopMatrix — Matrix-based internal population representation
-- ---------------------------------------------------------------------------

-- | Internal population representation backed by hmatrix matrices.
--
-- The user-facing 'Solution' type stores per-individual lists, which
-- forces the inner non-dominated sort and crowding-distance loops to
-- pay @O(MN)@ list traversals on every pair compare. 'PopMatrix' keeps
-- the same data laid out as one dense matrix per attribute, so that
-- the same loops become a small number of @O(N²)@ BLAS / 'LA.cmap'
-- calls — the same vectorisation that lets pymoo do a generation in
-- ~5 ms on numpy.
--
-- /Layout/:
--
--   * @pmX@ — decision matrix of shape @n × d@ (one row per individual)
--   * @pmF@ — objective matrix of shape @n × m@ (minimisation; smaller
--     is better)
--   * @pmCV@ — constraint-violation vector of length @n@ (zero =
--     feasible, positive = violated)
--
-- The 'Solution' API is preserved as a boundary representation; we
-- convert via 'fromSolutions' / 'toSolutions' once per generation.
data PopMatrix = PopMatrix
  { pmX  :: !(LA.Matrix Double)  -- ^ Decision matrix (@n × d@).
  , pmF  :: !(LA.Matrix Double)  -- ^ Objective matrix (@n × m@).
  , pmCV :: !(LA.Vector Double)  -- ^ Constraint violations (length @n@).
  } deriving (Show)

-- | Number of individuals in a 'PopMatrix'.
pmSize :: PopMatrix -> Int
pmSize = LA.rows . pmF

-- | Number of objectives in a 'PopMatrix'.
pmObjs :: PopMatrix -> Int
pmObjs = LA.cols . pmF

-- | Convert a list of 'Solution' to a 'PopMatrix'. All solutions must
-- share the same dimensions; the empty list yields an empty matrix.
fromSolutions :: [Solution] -> PopMatrix
fromSolutions []   = PopMatrix
  { pmX  = (0 LA.>< 0) []
  , pmF  = (0 LA.>< 0) []
  , pmCV = LA.fromList []
  }
fromSolutions sols = PopMatrix
  { pmX  = LA.fromLists (map solDecision   sols)
  , pmF  = LA.fromLists (map solObjectives sols)
  , pmCV = LA.fromList  (map solViolation  sols)
  }

-- | Inverse of 'fromSolutions'.
toSolutions :: PopMatrix -> [Solution]
toSolutions pm =
  let xs  = LA.toLists (pmX  pm)
      fs  = LA.toLists (pmF  pm)
      cvs = LA.toList  (pmCV pm)
  in zipWith3 (\d o v -> Solution d o v) xs fs cvs

-- | Pairwise constrained-Pareto domination matrix.
--
-- Returns an @n × n@ matrix @M@ in which:
--
--   * @M[i, j] = +1@ iff individual @i@ dominates @j@
--   * @M[i, j] = -1@ iff individual @j@ dominates @i@
--   * @M[i, j] =  0@ otherwise (mutually non-dominated, identical, or
--     diagonal entries)
--
-- Equivalent to calling 'dominates' on every pair, but evaluated as a
-- handful of @n × n@ array operations:
--
--   1. For each objective @k@, build the @n × n@ pairwise-difference
--      matrix @D_k[i, j] = F[i, k] - F[j, k]@ via two outer products.
--   2. @smallerK[i, j] = (D_k[i, j] < 0)@; @largerK[i, j] = (D_k[i, j] > 0)@.
--   3. Aggregate over @k@: @anySm = OR_k smallerK@, @anyLg = OR_k largerK@.
--   4. @iDomJ = anySm AND NOT anyLg@; @jDomI = anyLg AND NOT anySm@.
--   5. Constraint layer: a feasible individual dominates an infeasible
--      one; among two infeasible ones the smaller violation wins.
dominationMatrix :: PopMatrix -> LA.Matrix Double
dominationMatrix pm =
  let f      = pmF pm
      cv     = pmCV pm
      n      = LA.rows f
      m      = LA.cols f
      ones   = LA.konst 1 n :: LA.Vector Double
      onesNN = LA.konst 1 (n, n) :: LA.Matrix Double
      indicator x | x > 0     = 1
                  | otherwise = 0

      -- Per-objective contributions to "any smaller" and "any larger".
      -- We accumulate by addition, then collapse with @indicator@; this
      -- avoids constructing a 3-D tensor.
      perObj k =
        let fk = LA.flatten (f LA.¿ [k])
            d  = LA.outer fk ones - LA.outer ones fk     -- D_k[i,j] = f_k[i] - f_k[j]
            sm = LA.cmap (\v -> if v < 0 then 1 else 0) d
            lg = LA.cmap (\v -> if v > 0 then 1 else 0) d
        in (sm, lg)

      zeroNN = LA.konst 0 (n, n) :: LA.Matrix Double
      objContribs :: [(LA.Matrix Double, LA.Matrix Double)]
      objContribs =
        if m == 0
          then [(zeroNN, zeroNN)]
          else map perObj [0 .. m - 1]
      anySm = LA.cmap indicator (sum (map fst objContribs))
      anyLg = LA.cmap indicator (sum (map snd objContribs))

      -- Pareto-only domination ignoring constraints.
      iDomJpar = LA.cmap indicator (anySm * (onesNN - anyLg))
      jDomIpar = LA.cmap indicator (anyLg * (onesNN - anySm))
      paretoM  = iDomJpar - jDomIpar

      -- Constraint layer.
      cvFeas   = LA.cmap (\v -> if v == 0 then 1 else 0) cv
      cvInfes  = LA.cmap (\v -> if v >  0 then 1 else 0) cv
      -- a_feas[i,j] = 1 iff i feasible
      aFeas    = LA.outer cvFeas ones
      aInfes   = LA.outer cvInfes ones
      bFeas    = LA.outer ones cvFeas
      bInfes   = LA.outer ones cvInfes
      -- Both feasible: keep paretoM
      bothFeas = aFeas * bFeas
      -- a feasible, b infeasible: a dominates → +1
      aBeatsB  = aFeas * bInfes
      -- a infeasible, b feasible: b dominates → -1
      bBeatsA  = aInfes * bFeas
      -- Both infeasible: smaller cv wins
      cvDiff   = LA.outer cv ones - LA.outer ones cv
      aSmCV    = LA.cmap (\v -> if v < 0 then 1 else 0) cvDiff
      bSmCV    = LA.cmap (\v -> if v > 0 then 1 else 0) cvDiff
      bothInf  = aInfes * bInfes
      cvLayer  = bothInf * (aSmCV - bSmCV)

      m0 = bothFeas * paretoM + aBeatsB - bBeatsA + cvLayer
      -- Zero-out diagonal (i == j has no domination).
      identityMask = onesNN - LA.diag (LA.konst 1 n)
  in m0 * identityMask

-- | NSGA-II configuration.
data NSGAConfig = NSGAConfig
  { nsgaPopSize     :: Int            -- ^ Population size @N@ (prefer even).
  , nsgaGenerations :: Int            -- ^ Number of generations @T@.
  , nsgaCrossoverP  :: Double         -- ^ Crossover probability @p_c@ (default 0.9).
  , nsgaMutationP   :: Maybe Double   -- ^ Mutation probability ('Nothing' uses @1/d@).
  , nsgaEtaCross    :: Double         -- ^ SBX distribution index @η_c@ (default 15).
  , nsgaEtaMut      :: Double         -- ^ Polynomial-mutation @η_m@ (default 20).
  } deriving (Show)

-- | Default configuration: population 100, 200 generations, @p_c = 0.9@,
-- mutation @1/d@, @η_c = 15@, @η_m = 20@.
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

-- | NSGA-II main entry point. The user-supplied function maps a decision
-- vector to an objective vector. Returns the final generation's Pareto
-- approximation (= rank-0 individuals).
--
-- This is the unconstrained variant; for constraints use
-- 'nsga2WithConstraints'.
nsga2 :: NSGAConfig
      -> ([Double] -> [Double])  -- ^ Objective function (@m@-dimensional output).
      -> Bounds                  -- ^ Search bounds (@d@ dimensions).
      -> GenIO
      -> IO [Solution]
nsga2 cfg f bounds gen =
  nsga2WithConstraints cfg f (const 0) bounds gen

-- | Constrained NSGA-II. The constraint function maps a decision vector
-- to a /violation amount/ (@0@ = feasible, @> 0@ = violated). When there
-- are multiple constraints @g_i(x) ≤ 0@, aggregate them via e.g.
-- @sum [max 0 (g_i x)]@.
nsga2WithConstraints
  :: NSGAConfig
  -> ([Double] -> [Double])    -- ^ Objective function (@m@ dimensions).
  -> ([Double] -> Double)      -- ^ Constraint violation (@≥ 0@; @0@ = feasible).
  -> Bounds                    -- ^ Search bounds (@d@ dimensions).
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

  -- 初期母集団: Latin-Hypercube Sampling で各次元のセルを 1 度ずつ
  -- 埋める (iid uniform より初期世代の被覆良 → 第 1 世代で既に
  -- 全域の情報が手に入るため、世代あたりの収束が上がる)。
  initXs <- QR.lhsSamplesIn n bounds gen
  let initPop = [ evaluateSolution f cFn x | x <- initXs ]

  -- 世代ループ
  finalPop <- generationLoop (nsgaGenerations cfg) initPop pC etaC etaM pM bounds f cFn gen

  -- 最終世代の最初の front (Pareto 近似) を返す
  case nonDominatedSort finalPop of
    (front : _) -> return front
    []          -> return []

-- | Build a 'Solution' from a decision vector by evaluating both the
-- objective and the constraint function.
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
      ranked = concat
        [ zip3 (repeat r) (frontDistances fr) fr
        | (r, fr) <- zip [0 :: Int ..] sortedFronts ]

  -- ── 子母集団 Q を生成 (重複除去 retry 付き、pymoo 互換) ──
  --
  -- 'fillOffspring' は新規 child を 1 ペアずつ生成、現プール (pop +
  -- 既存 offspring) と L∞ 距離が dupEpsilon 以下のものを破棄、必要数
  -- (n) に達するまで最大 dupMaxRetries 回まで retry する。
  -- pymoo の DefaultDuplicateElimination + Mating の retry ループと
  -- 同等の挙動。重複が抑えられる分、有効 popSize が縮まずに済み、
  -- 100 gen 単一 RNG でも安定して pymoo を上回る。
  children <- fillOffspring n pop pC etaC etaM pM bounds f cFn ranked gen

  -- ── R = P ∪ Q から上位 N を選別 ──
  let combined = pop ++ children
      combinedFronts = nonDominatedSort combined
      newPop = selectTopN n combinedFronts

  generationLoop (t - 1) newPop pC etaC etaM pM bounds f cFn gen

-- | Duplicate-detection threshold (L∞).
dupEpsilon :: Double
dupEpsilon = 1e-12

-- | Maximum mating retries before giving up.
dupMaxRetries :: Int
dupMaxRetries = 10

-- | 'pop' との重複を除去しつつ @needed@ 個の child を集めるまで SBX
-- ペア生成を繰り返す。pymoo の InfillCriterion.do と同等の役割。
--
-- 親選びは **random-permutation tournament** (NF3): 各反復で 2 回の
-- pop 順列を取り、各個体が tournament に正確に 2 回出るようにペアを
-- 組む。これで selection pressure の variance が下がり、ZDT のような
-- iid-uniform tournament で convergence がブレる問題を抑える。
fillOffspring
  :: Int                         -- ^ 必要な child 数 @n@
  -> [Solution]                  -- ^ 現世代 pop (重複比較用)
  -> Double -> Double -> Double -> Double  -- ^ pC, etaC, etaM, pM
  -> Bounds
  -> ([Double] -> [Double])
  -> ([Double] -> Double)
  -> [(Int, Double, Solution)]
  -> GenIO
  -> IO [Solution]
fillOffspring needed pop pC etaC etaM pM bounds f cFn ranked gen =
  let go acc retries
        | length acc >= needed = return (take needed (reverse acc))
        | retries <= 0         = return (take needed (reverse acc))
        | otherwise = do
            -- 必要な親数 = 2 child / pair × n pairs。
            -- ここでは一度に @needed@ pair (= 2*needed 親) を作る。
            let want   = needed - length acc
                nPairs = (want + 1) `div` 2
                nPar   = 2 * 2 * nPairs            -- = 親 4 個 / pair (1 pair = 2 children = 4 tournament 出走)
            parentsW <- pickParentsByPermutation nPar ranked gen
            -- parentsW = [w_0, w_1, w_2, w_3, ...] (tournament 勝者列)
            -- 1 child pair に 2 親、 1 pair に 2 child → 4 親 / pair
            -- pairwise SBX/mutation を 1 batch
            batch <- mapM
              (\(p1, p2) ->
                 makeChildPairFromParents pC etaC etaM pM
                   bounds f cFn p1 p2 gen)
              (chunkPairs parentsW)
            let raw   = concatMap (\(c1, c2) -> [c1, c2]) batch
                refs  = map solDecision (pop ++ acc)
                isDup x = any (\r -> linfDist x r < dupEpsilon) refs
                kept  = [ s | s <- raw, not (isDup (solDecision s)) ]
                deduped = dedupBy
                            (\sa sb ->
                               linfDist (solDecision sa) (solDecision sb)
                                 < dupEpsilon)
                            kept
                acc'  = foldr (:) acc deduped
            go acc' (retries - 1)
  in go [] dupMaxRetries
  where
    -- 4 つずつ取って [(p1, p2), (p1', p2'), ...] のペアにする。
    -- 1 child pair につき 4 winners 必要 (子 1 = 親 2 のペア、子 2 も親 2)。
    -- ここでは簡単化して 1 child pair = 親 2 で十分とし、2 つずつ取る。
    chunkPairs (a : b : rest) = (a, b) : chunkPairs rest
    chunkPairs _              = []

-- | Random-permutation tournament: pop 全体の順列を 2 回作って先頭から
-- ペア取り、binaryTournament で勝者を出す。各個体が正確に 2 回出走。
pickParentsByPermutation
  :: Int                          -- ^ 必要な親の数 (≤ 2 × pop size、
                                  --   超える場合は permutation を repeat)
  -> [(Int, Double, Solution)]    -- ^ ranked pop
  -> GenIO
  -> IO [Solution]
pickParentsByPermutation nNeeded ranked gen = do
  let popSize = length ranked
      cmp (r1, d1, _) (r2, d2, _) = crowdedCompare (r1, d1) (r2, d2)
      -- 1 完全周 (= 2 順列でペア) からは popSize 親が取れる。
      nRounds = (nNeeded + popSize - 1) `div` popSize
  rounds <- mapM (\_ -> do
                    p1 <- shuffle ranked gen
                    p2 <- shuffle ranked gen
                    -- 1 round = popSize 親 (各 pair で 1 勝者)
                    let pairs = zip p1 p2
                    mapM (\(a, b) -> case cmp a b of
                            LT -> return (third a)
                            GT -> return (third b)
                            EQ -> do
                              r <- uniform gen :: IO Double
                              return (third (if r < 0.5 then a else b)))
                         pairs
                  ) [1 .. nRounds]
  return (take nNeeded (concat rounds))
  where
    third (_, _, s) = s

-- | Fisher-Yates shuffle. Pure traversal (we don't write to the list
-- in-place), but the shuffle index sequence is collected up front in O(n)
-- random calls.
shuffle :: [a] -> GenIO -> IO [a]
shuffle xs gen = do
  let n = length xs
  -- Generate a random key for each element, then sort by key.
  keys <- mapM (\_ -> uniform gen :: IO Double) [1 .. n]
  let pairs = zip keys xs
  return (map snd (sortBy (comparing fst) pairs))

-- | 1 ペアの子 (c1, c2) を、すでに選ばれた 2 親から作る。
-- 'makeChildPair' (random-tournament 内蔵版) との重複コードを避ける
-- ため SBX/mutation の本体だけ抽出。
makeChildPairFromParents
  :: Double -> Double -> Double -> Double
  -> Bounds
  -> ([Double] -> [Double])
  -> ([Double] -> Double)
  -> Solution -> Solution
  -> GenIO
  -> IO (Solution, Solution)
makeChildPairFromParents pC etaC etaM pM bounds f cFn parent1 parent2 gen = do
  u <- uniform gen :: IO Double
  (c1Vec, c2Vec) <-
    if u < pC
      then sbxCrossover etaC bounds (solDecision parent1) (solDecision parent2) gen
      else return (solDecision parent1, solDecision parent2)
  c1Mut <- polynomialMutation etaM pM bounds c1Vec gen
  c2Mut <- polynomialMutation etaM pM bounds c2Vec gen
  return ( evaluateSolution f cFn c1Mut
         , evaluateSolution f cFn c2Mut )

linfDist :: [Double] -> [Double] -> Double
linfDist xs ys = maximum (0 : zipWith (\a b -> abs (a - b)) xs ys)

dedupBy :: (a -> a -> Bool) -> [a] -> [a]
dedupBy _   []     = []
dedupBy eq (x:xs)  = x : dedupBy eq (filter (not . eq x) xs)

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

-- | Does individual @a@ /dominate/ @b@ under constrained Pareto
-- dominance?
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

-- | Standard (constraint-free) Pareto dominance: @a@ dominates @b@ iff
-- @∀ i: aᵢ ≤ bᵢ@ and @∃ j: aⱼ < bⱼ@.
paretoDominates :: [Double] -> [Double] -> Bool
paretoDominates as bs =
  all (\(x, y) -> x <= y) zipped
    && any (\(x, y) -> x <  y) zipped
  where
    zipped = zip as bs

-- | Fast non-dominated sort (Deb 2002): partitions the population into
-- ranked Pareto fronts.
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
  -- Pop is moved into a 'Data.Vector' so per-individual access is O(1)
  -- (the original list-based @ps !! j@ was @O(j)@ which made the whole
  -- sort @O(n³)@ rather than @O(n²m)@). Front/dominance bookkeeping
  -- still uses BLAS Vector for fused @LA.accum@ updates and an IntSet
  -- to track placed individuals across iterations.
  let n      = length pop
      ps     = V.fromList pop
      idxs   = [0 .. n - 1]
      domInfo i =
        let pi = ps V.! i
            (sp, np) = foldr step ([], 0 :: Int) idxs
            step j (s, c)
              | i == j                   = (s, c)
              | dominates pi (ps V.! j)  = (j : s, c)
              | dominates (ps V.! j) pi  = (s, c + 1)
              | otherwise                = (s, c)
        in (sp, np)
      info   = V.fromList [domInfo i | i <- idxs]
      sList  = V.map fst info
      front0 = [ i | (i, (_, c)) <- zip idxs (V.toList info), c == 0 ]
      nVec0  = LA.fromList (map (fromIntegral . snd) (V.toList info))
                 :: LA.Vector Double
      go counts current placedSet acc
        | null current = reverse acc
        | otherwise =
            let decrements = [ (j, -1)
                             | i <- current
                             , j <- sList V.! i ]
                counts'    = LA.accum counts (+) decrements
                placedSet' = foldr IS.insert placedSet current
                nextF =
                  [ j
                  | j <- [0 .. n - 1]
                  , not (IS.member j placedSet')
                  , let v = LA.atIndex counts' j
                  , v <= 0.5 && v > -0.5
                  ]
            in go counts' nextF placedSet' (current : acc)
      idxFronts = go nVec0 front0 IS.empty []
  in map (map (ps V.!)) idxFronts

-- | Matrix-driven non-dominated sort. Given a 'PopMatrix', returns a
-- list of fronts as @[[Int]]@ index lists.
--
-- Implementation: build the @n × n@ 'dominationMatrix' once; from it
-- derive @S_p@ (set of individuals dominated by @p@) and @n_p@ (count
-- of individuals dominating @p@) by row sums on the @+1@ / @-1@
-- patterns. The remainder is the standard Deb 2002 BFS-style level
-- assignment, but on integer arrays rather than per-element list
-- traversals.
nonDominatedSortIdx :: PopMatrix -> [[Int]]
nonDominatedSortIdx pm
  | pmSize pm == 0 = []
  | otherwise      =
      let n     = pmSize pm
          mDom  = dominationMatrix pm
          rows  = LA.toRows mDom
          -- Single pass per row: extract S_i (j with +1) and count
          -- dominators (entries with -1).
          dInfo = [ rowToSN (LA.toList r) | r <- rows ]
          sList = map fst dInfo
          nVec0 = LA.fromList (map (fromIntegral . snd) dInfo)
                    :: LA.Vector Double
          front0 = [ i | (i, (_, c)) <- zip [0 ..] dInfo, c == 0 ]
          go counts current placedSet acc
            | null current = reverse acc
            | otherwise =
                let decrements = [ (j, -1)
                                 | i <- current
                                 , j <- sList !! i ]
                    counts'    = LA.accum counts (+) decrements
                    placedSet' = foldr IS.insert placedSet current
                    nextF =
                      [ j
                      | j <- [0 .. n - 1]
                      , not (IS.member j placedSet')
                      , let v = LA.atIndex counts' j
                      , v <= 0.5 && v > -0.5
                      ]
                in go counts' nextF placedSet' (current : acc)
      in go nVec0 front0 IS.empty []
  where
    -- Walk one row, producing (S_i, n_i) in a single pass.
    rowToSN :: [Double] -> ([Int], Int)
    rowToSN vs = go' 0 [] 0 vs
      where
        go' _ s c []     = (reverse s, c)
        go' j s c (x:xs)
          | x >  0.5 = go' (j + 1) (j : s) c xs
          | x < -0.5 = go' (j + 1) s       (c + 1) xs
          | otherwise = go' (j + 1) s       c       xs

-- | Compute the crowding distance (Deb 2002) inside a front and sort it
-- by descending distance.
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

-- | Simulated Binary Crossover (SBX, Deb 1995). A real-coded analogue of
-- single-point crossover for binary GAs.
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
  -- 注: pymoo は prob_bin による per-dim c1↔c2 swap を持つが、ZDT2 の
  -- 凹 Pareto front では親由来 lineage の保持が convergence に重要で
  -- swap が逆効果になることが計測で確認できたため採用しない (NF5 試行
  -- → revert)。

-- | One-dimensional SBX update — **boundary-aware** form (Deb 1995
-- Algorithm 1, matching pymoo / DEAP / jMetal).
--
-- The key difference vs the simplified variant we used previously is
-- that the spread parameter @β@ depends on **how close the parent is
-- to its bound**: a parent right at the lower bound @xl@ is paired with
-- @β ≈ 1@ (= no spread), so the produced child stays near @xl@. The
-- old @β = (2u)^{1/(η+1)}@ was completely bound-agnostic, which means
-- a parent at @x = 0@ paired with one at @x = 0.5@ would produce a
-- child near @0.25@ — the optimum-tracking behaviour ZDT problems
-- demand was lost.
--
-- Algorithm:
--
-- @
-- y1 = min(a, b);  y2 = max(a, b);  Δ = y2 - y1
--
-- For child c1 (anchored to the lower side):
--   β   = 1 + 2(y1 - xl) / Δ
--   α   = 2 - β^{-(η+1)}
--   β_q = (u·α)^{1/(η+1)}                    if u ≤ 1/α
--       = (1 / (2 - u·α))^{1/(η+1)}          otherwise
--   c1  = 0.5 [(y1 + y2) - β_q · Δ]
--
-- For child c2 (anchored to the upper side):
--   β   = 1 + 2(xu - y2) / Δ
--   α, β_q as above
--   c2  = 0.5 [(y1 + y2) + β_q · Δ]
-- @
sbxOneVar :: Double -> GenIO -> (Double, Double) -> (Double, Double)
          -> IO (Double, Double)
sbxOneVar etaC gen (lo, hi) (a, b) = do
  flip_ <- uniform gen :: IO Double          -- per-dim 50% gating
  if flip_ >= 0.5 || abs (a - b) < 1e-14 || hi <= lo
    then return (a, b)
    else do
      u <- uniform gen :: IO Double
      let (y1, y2) = if a < b then (a, b) else (b, a)
          delta   = y2 - y1
          mPow    = 1 / (etaC + 1)

          -- Boundary-aware β_q for one side. 'beta' is the
          -- distance-to-bound term; 'alpha = 2 - β^{-(η+1)}' is the
          -- adapted threshold that pymoo's @calc_betaq@ uses.
          calcBetaQ beta =
            let alpha = 2 - beta ** (- (etaC + 1))
                inv   = 1 / alpha
            in if u <= inv
                 then (u * alpha) ** mPow
                 else (1 / (2 - u * alpha)) ** mPow

          beta1 = 1 + 2 * (y1 - lo) / delta
          beta2 = 1 + 2 * (hi - y2) / delta
          bq1   = calcBetaQ beta1
          bq2   = calcBetaQ beta2
          c1    = 0.5 * ((y1 + y2) - bq1 * delta)
          c2    = 0.5 * ((y1 + y2) + bq2 * delta)
          clip x = min hi (max lo x)
      return (clip c1, clip c2)

-- | Polynomial mutation (Deb & Goyal 1996).
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
  if r >= pMut || hi <= lo
    then return x
    else do
      u <- uniform gen :: IO Double
      -- Deb & Goyal 1996 polynomial mutation with **boundary correction**.
      -- The simplified variant @(2u)^(1/(η+1)) - 1@ ignores the distance
      -- to the bounds and produces over-aggressive jumps when @u@ is
      -- near 0 or 1 (= effectively snaps to the boundary). The corrected
      -- form below scales the perturbation by how close @x@ already is
      -- to each bound, which is what pymoo / DEAP / jMetal use.
      let delta1 = (x - lo) / (hi - lo)        -- normalized distance to lo
          delta2 = (hi - x) / (hi - lo)        -- normalized distance to hi
          mp     = 1 / (etaM + 1)
          dq
            | u <= 0.5  =
                let val = 2 * u + (1 - 2 * u) * (1 - delta1) ** (etaM + 1)
                in val ** mp - 1
            | otherwise =
                let val = 2 * (1 - u) + (2 * u - 1) * (1 - delta2) ** (etaM + 1)
                in 1 - val ** mp
          y = x + dq * (hi - lo)
      return (min hi (max lo y))

-- | Sample one decision vector uniformly from the bounds (used for the
-- initial population). Thin wrapper around 'Optim.Common.sampleUniformIn',
-- kept for backwards compatibility.
randomInBounds :: Bounds -> GenIO -> IO [Double]
randomInBounds = OC.sampleUniformIn

-- | NSGA-II's crowded-comparison operator:
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
-- EQ (両者同等) の場合は **ランダムに勝敗を決める** (pymoo / DEAP と同方式)。
-- 以前は常に xi を返していたため early-population indices が選択圧で
-- 有利になり ZDT 系で per-generation 収束が遅れていた。
binaryTournament :: [a] -> (a -> a -> Ordering) -> GenIO -> IO a
binaryTournament pop cmp gen = do
  let n = length pop
  i <- uniformR (0, n - 1) gen
  j <- uniformR (0, n - 1) gen
  let xi = pop !! i
      xj = pop !! j
  case cmp xi xj of
    LT -> return xi
    GT -> return xj
    EQ -> do
      r <- uniform gen :: IO Double
      return (if r < 0.5 then xi else xj)
