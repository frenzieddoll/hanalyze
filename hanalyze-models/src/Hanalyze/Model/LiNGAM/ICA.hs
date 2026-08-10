{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns      #-}
-- |
-- Module      : Hanalyze.Model.LiNGAM.ICA
-- Description : ICA-LiNGAM (Shimizu 2006、原典版) by FastICA + Hungarian 順列
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: ICA-LiNGAM (Shimizu et al. 2006、 LiNGAM の原典版) by FastICA。
--
-- ## アルゴリズム
--
-- 1. 観測 X (n × p) に対し FastICA で __分離行列 W__ (= ICA unmixing) を求める
--    (元座標、 'Hanalyze.Math.ICA.icaUnmixing')
-- 2. __A = pinv(W)__ を計算 (X = S · Aᵀ + mean)
-- 3. __行/列順列で下三角化__:
--    a. A の絶対値の __逆数__ をコスト行列とし、 行・列順列で対角要素を
--       絶対値最大に揃える Hungarian-like (本実装は近似貪欲)
--    b. 順列適用後の A を対角要素で正規化、 B = I - A_perm⁻¹
--    c. B の下三角化のための __行順列__ を別途決定 (= causal order)
-- 4. B 行列を pruning して隣接行列を返す
--
-- ## DirectLiNGAM との違い
--
-- DirectLiNGAM は ICA 不要で残差独立性 + 1 変数ずつ確定。 ICA-LiNGAM は ICA
-- (FastICA) で全成分を同時推定 → 順列で因果順序を後付けで決める。 ICA の
-- 収束性に依存するが、 因子数が多いときは並列度で有利な場合がある。
--
-- 行/列順列は __Hungarian (Kuhn-Munkres, O(p³))__ で大域最適化する
-- ('Hanalyze.Math.Hungarian')。 cdt15/lingam の Python 実装は
-- @scipy.optimize.linear_sum_assignment(1 / |W|)@ で同等のことをしており、
-- コスト関数も @1 / (|W| + ε)@ で揃えている。 旧来の貪欲版 ('greedyAssignRows')
-- は @ilcUseHungarian = False@ で復元可能 (回帰確認・ベンチ比較用)。
--
-- ## リファレンス
--
-- Shimizu et al. (2006) "A Linear Non-Gaussian Acyclic Model for Causal
-- Discovery", JMLR 7. Python 実装は cdt15/lingam の `lingam/ica_lingam.py`。
--
-- [English]: ICA-LiNGAM (Shimizu et al. 2006, the original LiNGAM
-- formulation) via FastICA.
--
-- ## Algorithm
--
-- 1. For observations X (n × p), obtain the __separating matrix W__
--    (= ICA unmixing) by FastICA, in the original coordinates
--    ('Hanalyze.Math.ICA.icaUnmixing').
-- 2. Compute __A = pinv(W)__ (X = S · Aᵀ + mean).
-- 3. __Lower-triangularize by row\/column permutation__:
--    a. Use the __reciprocal__ of |A| as the cost matrix and align the
--       diagonal entries to the largest absolute values by row\/column
--       permutation, Hungarian-like (this implementation is an approximate
--       greedy one).
--    b. Normalize the permuted A by its diagonal entries, B = I - A_perm⁻¹.
--    c. Separately determine the __row permutation__ that lower-triangularizes
--       B (= the causal order).
-- 4. Prune the B matrix and return the adjacency matrix.
--
-- ## Difference from DirectLiNGAM
--
-- DirectLiNGAM needs no ICA and fixes one variable at a time via residual
-- independence. ICA-LiNGAM estimates all components simultaneously by ICA
-- (FastICA) and then decides the causal order afterwards by permutation. It
-- depends on the convergence of ICA, but can be advantageous in parallelism
-- when the number of factors is large.
--
-- The row\/column permutation is globally optimized by
-- __Hungarian (Kuhn-Munkres, O(p³))__ ('Hanalyze.Math.Hungarian').
-- The Python implementation in cdt15\/lingam does the equivalent with
-- @scipy.optimize.linear_sum_assignment(1 / |W|)@, and the cost function is
-- matched here as @1 / (|W| + ε)@. The legacy greedy version
-- ('greedyAssignRows') can be restored with @ilcUseHungarian = False@ (for
-- regression checks and benchmark comparison).
--
-- ## References
--
-- Shimizu et al. (2006) "A Linear Non-Gaussian Acyclic Model for Causal
-- Discovery", JMLR 7. The Python implementation is `lingam/ica_lingam.py`
-- in cdt15\/lingam.
module Hanalyze.Model.LiNGAM.ICA
  ( ICALiNGAMConfig (..)
  , ICALiNGAMFit (..)
  , fitICALiNGAMPure
  , defaultICALiNGAMConfig
  , fitICALiNGAM
  , ilDAG
  ) where

import qualified Numeric.LinearAlgebra as LA
import qualified Data.Vector.Unboxed   as VU
import           Data.List             (sortBy)
import           Data.Ord              (comparing, Down (..))

import qualified Hanalyze.Math.ICA          as ICA
import qualified Hanalyze.Math.Hungarian    as Hung
import qualified Hanalyze.Model.DAG         as DAG

-- ===========================================================================
-- 設定 / 結果
-- ===========================================================================

data ICALiNGAMConfig = ICALiNGAMConfig
  { ilcPruneThr      :: !Double
  , ilcICACfg        :: !ICA.ICAConfig
  , ilcUseHungarian  :: !Bool
    -- ^ [日本語]: True: 行順列を Hungarian (O(p³)) で大域最適化 (default、 推奨)。
    --   False: 旧来の貪欲版を使う (回帰比較・ベンチ用)。
    --   [English]: True: globally optimize the row permutation with Hungarian
    --   (O(p³)) (default, recommended). False: use the legacy greedy version
    --   (for regression comparison and benchmarking).
  } deriving (Show)

defaultICALiNGAMConfig :: ICALiNGAMConfig
defaultICALiNGAMConfig = ICALiNGAMConfig
  { ilcPruneThr     = 0.05
  , ilcICACfg       = ICA.defaultICAConfig
  , ilcUseHungarian = True
  }

data ICALiNGAMFit = ICALiNGAMFit
  { ilOrder      :: ![Int]
  , ilB          :: !(LA.Matrix Double)
  , ilAdjacency  :: !(LA.Matrix Double)
  , ilICAResult  :: !ICA.ICAResult
  } deriving (Show)

-- ===========================================================================
-- 主実装
-- ===========================================================================

fitICALiNGAM :: ICALiNGAMConfig -> LA.Matrix Double -> IO ICALiNGAMFit
fitICALiNGAM cfg x = do
  ica <- ICA.fitICA (ilcICACfg cfg) x
  pure (assembleICALiNGAM cfg ica)

-- | [日本語]: 'fitICALiNGAM' の __seed 純粋版__ (@df |->@ 用)。 @fitICAPure@ (seed) で
--   FastICA を回す。 同 seed で IO 版とビット一致。
--   [English]: The __seed-based pure version__ of 'fitICALiNGAM' (for
--   @df |->@). Runs FastICA via @fitICAPure@ (seed). Bit-identical to the IO
--   version for the same seed.
fitICALiNGAMPure :: ICALiNGAMConfig -> LA.Matrix Double -> ICALiNGAMFit
fitICALiNGAMPure cfg x = assembleICALiNGAM cfg (ICA.fitICAPure (ilcICACfg cfg) x)

-- | [日本語]: ICA 結果 → 'ICALiNGAMFit' の純粋組み立て (行順列 → 正規化 → 下三角化 → adjacency)。
--   [English]: Pure assembly of an 'ICALiNGAMFit' from an ICA result (row
--   permutation → normalization → lower-triangularization → adjacency).
assembleICALiNGAM :: ICALiNGAMConfig -> ICA.ICAResult -> ICALiNGAMFit
assembleICALiNGAM cfg ica =
  let !w = ICA.icaUnmixing ica      -- (p × p)
      !p = LA.rows w
      -- step 3a: 対角絶対値最大化の行順列を決定。 Hungarian は大域最適、
      -- 貪欲は p > 10 でしばしば劣化する (cdt15/lingam も Hungarian 採用)。
      !rowPerm    = if ilcUseHungarian cfg
                      then hungarianAssignRows w
                      else greedyAssignRows w
      !wPerm1     = permuteRows w rowPerm
      -- step 3b: 各行を対角で正規化
      !wNorm      = normalizeDiag wPerm1
      -- B' = I - W_norm
      !bPrime     = LA.ident p - wNorm
      -- step 3c: bPrime の行順列を causal order に並べる
      -- 下三角化: 順列の絶対値和が下三角寄りになるよう貪欲に並べ替え
      !causal     = causalOrderFromTriangle bPrime
      -- causal order で再順列した B を返す
      !bReorder   = permuteRowsCols bPrime causal causal
      -- 元 variable index に戻す
      -- bPrime[i, j] は permuted index 上の値、 rowPerm を逆引きする必要あり
      !bFinal     = restoreOriginalIndex p bPrime rowPerm causal
      !adj        = adjMatrix (ilcPruneThr cfg) bFinal
      _ = bReorder  -- 内部debug 用、 未使用
  in ICALiNGAMFit
    { ilOrder      = mapPerm causal rowPerm
    , ilB          = bFinal
    , ilAdjacency  = adj
    , ilICAResult  = ica
    }

-- | [日本語]: DAG への変換
--   [English]: Conversion to a DAG.
ilDAG :: ICALiNGAMConfig -> ICALiNGAMFit -> DAG.DAG
ilDAG cfg fit = DAG.fromBMatrix (ilcPruneThr cfg) (ilB fit)

-- ===========================================================================
-- 内部: 順列ヘルパ
-- ===========================================================================

-- | [日本語]: Hungarian による行順列決定。 コスト C[i, j] = 1 / (|W[i, j]| + ε) で
--   'Hung.hungarianMin' を呼び、 row i → col j の割当を得てから
--   perm[j] = i に反転する (col j に row i を置く)。
--   cdt15/lingam の Python 実装 (scipy linear_sum_assignment(1/|W|)) と同型。
--   [English]: Determines the row permutation by the Hungarian algorithm.
--   Calls 'Hung.hungarianMin' with cost C[i, j] = 1 / (|W[i, j]| + ε), obtains
--   the assignment row i → col j, and then inverts it into perm[j] = i (place
--   row i at col j). Isomorphic to the Python implementation in cdt15\/lingam
--   (scipy linear_sum_assignment(1/|W|)).
hungarianAssignRows :: LA.Matrix Double -> [Int]
hungarianAssignRows w =
  let p        = LA.rows w
      eps      = 1.0e-12
      cost     = LA.build (p, p)
                   (\i j -> 1.0 / (abs (LA.atIndex w (round i, round j)) + eps)
                            :: Double)
      assign   = Hung.hungarianMin cost  -- assign[i] = j (row i → col j)
      pairs    = sortBy (comparing fst)
                   [ (assign VU.! i, i) | i <- [0 .. p - 1] ]
                                          -- (col j, row i)
  in map snd pairs                        -- perm[j] = i

-- | [日本語]: 行順列の貪欲決定: 各列の絶対値最大要素を見て、 行と列を 1-1 対応させる
--   greedy assignment (Hungarian の近似版)。 戻り値 perm の意味:
--   「permuted index j に元 row index perm[j] を持ってくる」 (= rows ordering)。
--   [English]: Greedy determination of the row permutation: a greedy
--   assignment that matches rows and columns one-to-one by looking at the
--   largest-magnitude entry of each column (an approximation of Hungarian).
--   The returned perm means "bring the original row index perm[j] to permuted
--   index j" (= rows ordering).
greedyAssignRows :: LA.Matrix Double -> [Int]
greedyAssignRows w =
  let p = LA.rows w
      -- 候補を (元 row i, 元 col j, abs value) として絶対値降順に並べる
      candidates :: [((Int, Int), Double)]
      candidates = sortBy (comparing (Down . snd))
        [ ((i, j), abs (LA.atIndex w (i, j)))
        | i <- [0 .. p - 1], j <- [0 .. p - 1] ]
      -- 貪欲: row と col を使用済にしながら (col j に row i を割当て)
      assign :: [Int] -> [Int] -> [((Int, Int), Double)] -> [(Int, Int)]
      assign _        _        []                = []
      assign usedRows usedCols (((i, j), _):rest)
        | i `elem` usedRows || j `elem` usedCols = assign usedRows usedCols rest
        | otherwise = (j, i) : assign (i:usedRows) (j:usedCols) rest
      pairs    = assign [] [] candidates           -- (col j, row i) のペア
      sortedPairs = sortBy (comparing fst) pairs   -- col 昇順
      perm        = map snd sortedPairs            -- perm[j] = i
  in if length perm == p
       then perm
       else [0 .. p - 1]   -- fallback

-- | [日本語]: 行を perm で並べ替える (perm[i] = 元 index)。
--   [English]: Reorders the rows by perm (perm[i] = the original index).
permuteRows :: LA.Matrix Double -> [Int] -> LA.Matrix Double
permuteRows m perm = m LA.? perm

-- | [日本語]: 各行を対角要素で正規化する (W → W / diag(W))。
--   [English]: Normalizes each row by its diagonal entry (W → W / diag(W)).
normalizeDiag :: LA.Matrix Double -> LA.Matrix Double
normalizeDiag w =
  let p = LA.rows w
      diags = [ LA.atIndex w (i, i) | i <- [0 .. p - 1] ]
      f i j =
        let d = diags !! i
            v = LA.atIndex w (i, j)
        in if abs d > 1e-12 then v / d else v
  in LA.build (p, p) (\i j -> f (round i) (round j) :: Double)

-- | [日本語]: B から下三角化のための行順列を貪欲に決める。
--   各行の非零要素数が少ない行 (根) を先に置く戦略。
--   [English]: Greedily determines the row permutation that
--   lower-triangularizes B. The strategy places rows with fewer nonzero
--   entries (the roots) first.
causalOrderFromTriangle :: LA.Matrix Double -> [Int]
causalOrderFromTriangle b =
  let p = LA.rows b
      scoreRow i =
        sum [ abs (LA.atIndex b (i, j))
            | j <- [0 .. p - 1], j /= i ]
      sorted = sortBy (comparing snd)
                 [ (i, scoreRow i) | i <- [0 .. p - 1] ]
  in map fst sorted

-- | [日本語]: 行と列を同じ perm で並び替え (DAG 構造を保つ)。
--   [English]: Reorders rows and columns by the same perm (preserving the DAG
--   structure).
permuteRowsCols :: LA.Matrix Double -> [Int] -> [Int] -> LA.Matrix Double
permuteRowsCols m rp cp =
  let mR = m LA.? rp
      mTr = LA.tr mR LA.? cp
  in LA.tr mTr

-- | [日本語]: 元の variable index に戻す。
--   permuted index 上での B → original index 上での B。
--   [English]: Restores the original variable indices. B on permuted indices
--   → B on original indices.
restoreOriginalIndex
  :: Int
  -> LA.Matrix Double    -- B_prime (permuted index 上)
  -> [Int]               -- rowPerm: permuted_i ← original_rowPerm[i]
  -> [Int]               -- causal: permuted index 上での causal order
  -> LA.Matrix Double
restoreOriginalIndex p bPrime rowPerm _causal =
  -- bPrime は rowPerm で permuted されている。 inverse perm で元に戻す。
  let invPerm = invertPerm rowPerm
      f i j   = LA.atIndex bPrime (invPerm !! i, invPerm !! j)
  in LA.build (p, p) (\i j -> f (round i) (round j) :: Double)

invertPerm :: [Int] -> [Int]
invertPerm perm =
  let p = length perm
      pairs = zip perm [0 ..]
      sorted = sortBy (comparing fst) pairs
  in map snd sorted ++ replicate (p - length sorted) 0

-- | [日本語]: original index 上での causal order (= permuted causal を rowPerm で戻す)
--   [English]: The causal order on original indices (= mapping the permuted
--   causal order back through rowPerm).
mapPerm :: [Int] -> [Int] -> [Int]
mapPerm causal rowPerm = map (rowPerm !!) causal

-- | [日本語]: adjacency 行列 (|B| > thr のマスク)
--   [English]: The adjacency matrix (the mask of |B| > thr).
adjMatrix :: Double -> LA.Matrix Double -> LA.Matrix Double
adjMatrix thr b =
  let p = LA.rows b
      f i j
        | i == j                          = 0
        | abs (LA.atIndex b (i, j)) > thr = 1
        | otherwise                       = 0
  in LA.build (p, p) (\i j -> f (round i) (round j) :: Double)
