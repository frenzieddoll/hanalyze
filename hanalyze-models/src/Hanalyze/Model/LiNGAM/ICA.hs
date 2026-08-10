{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns      #-}
-- |
-- Module      : Hanalyze.Model.LiNGAM.ICA
-- Description : ICA-LiNGAM (Shimizu 2006、原典版) by FastICA + Hungarian 順列
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- ICA-LiNGAM (Shimizu et al. 2006、 LiNGAM の原典版) by FastICA。
--
-- ## アルゴリズム
--
-- 1. 観測 X (n × p) に対し FastICA で **分離行列 W** (= ICA unmixing) を求める
--    (元座標、 'Hanalyze.Math.ICA.icaUnmixing')
-- 2. **A = pinv(W)** を計算 (X = S · Aᵀ + mean)
-- 3. **行/列順列で下三角化**:
--    a. A の絶対値の **逆数** をコスト行列とし、 行・列順列で対角要素を
--       絶対値最大に揃える Hungarian-like (本実装は近似貪欲)
--    b. 順列適用後の A を対角要素で正規化、 B = I - A_perm⁻¹
--    c. B の下三角化のための **行順列** を別途決定 (= causal order)
-- 4. B 行列を pruning して隣接行列を返す
--
-- ## DirectLiNGAM との違い
--
-- DirectLiNGAM は ICA 不要で残差独立性 + 1 変数ずつ確定。 ICA-LiNGAM は ICA
-- (FastICA) で全成分を同時推定 → 順列で因果順序を後付けで決める。 ICA の
-- 収束性に依存するが、 因子数が多いときは並列度で有利な場合がある。
--
-- 行/列順列は **Hungarian (Kuhn-Munkres, O(p³))** で大域最適化する
-- ('Hanalyze.Math.Hungarian')。 cdt15/lingam の Python 実装は
-- @scipy.optimize.linear_sum_assignment(1 / |W|)@ で同等のことをしており、
-- コスト関数も @1 / (|W| + ε)@ で揃えている。 旧来の貪欲版 ('greedyAssignRows')
-- は @ilcUseHungarian = False@ で復元可能 (回帰確認・ベンチ比較用)。
--
-- ## リファレンス
--
-- Shimizu et al. (2006) "A Linear Non-Gaussian Acyclic Model for Causal
-- Discovery", JMLR 7. Python 実装は cdt15/lingam の `lingam/ica_lingam.py`。
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
    -- ^ True: 行順列を Hungarian (O(p³)) で大域最適化 (default、 推奨)。
    --   False: 旧来の貪欲版を使う (回帰比較・ベンチ用)。
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

-- | 'fitICALiNGAM' の **seed 純粋版** (Phase 77.C・@df |->@ 用)。 'fitICAPure' (seed) で
--   FastICA を回す。 同 seed で IO 版とビット一致。
fitICALiNGAMPure :: ICALiNGAMConfig -> LA.Matrix Double -> ICALiNGAMFit
fitICALiNGAMPure cfg x = assembleICALiNGAM cfg (ICA.fitICAPure (ilcICACfg cfg) x)

-- | ICA 結果 → 'ICALiNGAMFit' の純粋組み立て (行順列 → 正規化 → 下三角化 → adjacency)。
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

-- | DAG への変換
ilDAG :: ICALiNGAMConfig -> ICALiNGAMFit -> DAG.DAG
ilDAG cfg fit = DAG.fromBMatrix (ilcPruneThr cfg) (ilB fit)

-- ===========================================================================
-- 内部: 順列ヘルパ
-- ===========================================================================

-- | Hungarian による行順列決定。 コスト C[i, j] = 1 / (|W[i, j]| + ε) で
--   'Hung.hungarianMin' を呼び、 row i → col j の割当を得てから
--   perm[j] = i に反転する (col j に row i を置く)。
--   cdt15/lingam の Python 実装 (scipy linear_sum_assignment(1/|W|)) と同型。
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

-- | 行順列の貪欲決定: 各列の絶対値最大要素を見て、 行と列を 1-1 対応させる
--   greedy assignment (Hungarian の近似版)。 戻り値 perm の意味:
--   「permuted index j に元 row index perm[j] を持ってくる」 (= rows ordering)。
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

-- | 行を perm で並べ替える (perm[i] = 元 index)。
permuteRows :: LA.Matrix Double -> [Int] -> LA.Matrix Double
permuteRows m perm = m LA.? perm

-- | 各行を対角要素で正規化する (W → W / diag(W))。
normalizeDiag :: LA.Matrix Double -> LA.Matrix Double
normalizeDiag w =
  let p = LA.rows w
      diags = [ LA.atIndex w (i, i) | i <- [0 .. p - 1] ]
      f i j =
        let d = diags !! i
            v = LA.atIndex w (i, j)
        in if abs d > 1e-12 then v / d else v
  in LA.build (p, p) (\i j -> f (round i) (round j) :: Double)

-- | B から下三角化のための行順列を貪欲に決める。
--   各行の非零要素数が少ない行 (根) を先に置く戦略。
causalOrderFromTriangle :: LA.Matrix Double -> [Int]
causalOrderFromTriangle b =
  let p = LA.rows b
      scoreRow i =
        sum [ abs (LA.atIndex b (i, j))
            | j <- [0 .. p - 1], j /= i ]
      sorted = sortBy (comparing snd)
                 [ (i, scoreRow i) | i <- [0 .. p - 1] ]
  in map fst sorted

-- | 行と列を同じ perm で並び替え (DAG 構造を保つ)。
permuteRowsCols :: LA.Matrix Double -> [Int] -> [Int] -> LA.Matrix Double
permuteRowsCols m rp cp =
  let mR = m LA.? rp
      mTr = LA.tr mR LA.? cp
  in LA.tr mTr

-- | 元の variable index に戻す。
--   permuted index 上での B → original index 上での B。
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

-- | original index 上での causal order (= permuted causal を rowPerm で戻す)
mapPerm :: [Int] -> [Int] -> [Int]
mapPerm causal rowPerm = map (rowPerm !!) causal

-- | adjacency 行列 (|B| > thr のマスク)
adjMatrix :: Double -> LA.Matrix Double -> LA.Matrix Double
adjMatrix thr b =
  let p = LA.rows b
      f i j
        | i == j                          = 0
        | abs (LA.atIndex b (i, j)) > thr = 1
        | otherwise                       = 0
  in LA.build (p, p) (\i j -> f (round i) (round j) :: Double)
