{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hanalyze.Design.Optimal
-- Description : 最適計画 (D/A/I/E/G-optimal) — Fedorov 交換法による候補集合からの選択・拡張
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Optimal designs: D-optimal and A-optimal.
--
-- Selects a subset of @n@ runs from a candidate set, maximizing /
-- minimizing a criterion based on the information matrix @XᵀX@.
--
--   * **D-optimal** — @max det(XᵀX)@ → joint estimation precision of
--     all parameters.
--   * **A-optimal** — @min trace((XᵀX)⁻¹)@ → minimum average estimation
--     variance.
--
-- Algorithm: the Fedorov exchange method (sequential exchanges). Starts
-- from a random selection of candidates and
-- 改善する交換が見つからなくなるまで繰り返す。
module Hanalyze.Design.Optimal
  ( OptCriterion (..)
  , dOptimal
  , aOptimal
  , iOptimal
  , eOptimal
  , gOptimal
  , optimalDesign
  , candidateGrid
  , quadraticCandidates
  , pseudoShuffle
    -- * Augment Design (Phase 5、 request/160)
  , AugmentResult (..)
  , augmentDesign
  ) where

import Data.List (foldl')
import qualified Numeric.LinearAlgebra as LA

-- | Optimality criterion.
data OptCriterion
  = DOpt   -- ^ D-optimal: maximize @det(XᵀX)@.
  | AOpt   -- ^ A-optimal: minimize @trace((XᵀX)⁻¹)@.
  | IOpt   -- ^ I-optimal: minimize average prediction variance, approximated
           --   by @trace((XᵀX)⁻¹ · M_moment)@. ここでは M_moment を全候補
           --   から推定した moment matrix @candᵀ cand / n_cand@ とする。
  | EOpt   -- ^ E-optimal: minimize the maximum eigenvalue of @(XᵀX)⁻¹@、
           --   = maximize the minimum eigenvalue of @XᵀX@。
  | GOpt   -- ^ G-optimal (self approximation): minimize the maximum leverage
           --   @max_i (H_ii)@ where @H = X (XᵀX)⁻¹ Xᵀ@。
           --   候補集合に依存しない self-G 定義 (= 設計自身の hat 対角の最大)。
           --   厳密な G-optimal (候補空間全体の max prediction variance) は
           --   Custom Design spec 側で扱う。 spec: doe-spec v0.2 §2.9。
  | Compound ![(Double, OptCriterion)]
           -- ^ Compound (alphabetic) criterion: 各 inner criterion を
           --   /minimize/ 方向に揃えた 'critValue' の重み付き和。
           --   重みは正数を仮定 (合計 1 への正規化はユーザ側責任)。
           --   ネストした @Compound@ も許容 (展開して評価)。
           --   注意: inner criterion 同士のスケールはユーザが責任を持って
           --   揃える (例: D 0.7 + I 0.3 は両方を efficiency 形に正規化
           --   してから渡す)。 v0.2 では正規化ヘルパは未提供、 v0.3+ 候補。
           --   spec: doe-spec v0.2 §2.9。
  | BayesianD ![[Double]]
           -- ^ Bayesian D-optimality (DuMouchel-Jones 1994):
           --   maximize @det(XᵀX + K)@、 K = prior precision matrix (p × p)。
           --   K = 0 行列で classic D に縮退。 spec: doe-custom-design-spec v0.1.1 §2.7。
           --   K は @[[Double]]@ (Show / Eq 要件のため)、 expand 後の列数と一致必須。
  | IOptRegion ![[Double]]
           -- ^ I-optimal (region 積分版、 Phase 28-4):
           --   minimize @trace((XᵀX)⁻¹ · M_R)@、 M_R = region moment matrix
           --   @∫_R f(z)f(z)' dz / vol(R)@ (p × p)。 旧 'IOpt' は self-moment
           --   近似で @= p/n@ に縮退するため設計に依らず無意味、 region 版で差し替えた。
           --   M_R は @[[Double]]@ (Show / Eq 要件のため)、 expand 後の列数と一致必須。
           --   Custom Design 内では 'Hanalyze.Design.Custom.Compare.regionMomentMatrixAnalytic'
           --   が連続 U[-1,1] + Categorical 等確率規約で M_R を構築する。
  deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- 基準値の計算
-- ---------------------------------------------------------------------------

-- | D-criterion value for a design matrix @X@: @det(XᵀX)@.
dValue :: [[Double]] -> Double
dValue rows
  | null rows = 0
  | otherwise = LA.det xtx
  where
    m   = LA.fromLists rows
    xtx = LA.tr m LA.<> m

-- | A-criterion value for a design matrix @X@: @trace((XᵀX)⁻¹)@.
-- Returns @∞@ when the inverse does not exist.
aValue :: [[Double]] -> Double
aValue rows
  | null rows = 1 / 0
  | otherwise =
      let m   = LA.fromLists rows
          xtx = LA.tr m LA.<> m
          d   = LA.det xtx
      in if abs d < 1e-12 then 1 / 0
           else
             let inv = LA.inv xtx
                 p   = LA.cols m
             in sum [ inv `LA.atIndex` (i, i) | i <- [0 .. p - 1] ]

-- | Criterion value used for optimization. Both criteria are returned
-- as quantities to /minimize/; D-optimality is encoded as
-- @-det(XᵀX)@.
critValue :: OptCriterion -> [[Double]] -> Double
critValue DOpt rows = -dValue rows  -- 最小化問題に統一
critValue AOpt rows =  aValue rows
critValue IOpt rows = iValueWithSelf rows
critValue EOpt rows = eValue rows
critValue GOpt rows = gValue rows
critValue (Compound ws) rows =
  sum [ w * critValue c rows | (w, c) <- ws ]
critValue (BayesianD k) rows = -bayesianDValue k rows
critValue (IOptRegion mr) rows = iValueRegion mr rows

-- | I-criterion (region 積分版): @trace((XᵀX)⁻¹ · M_R)@ を返す (minimize 方向)。
-- @M_R@ の次元が X の列数と不一致 / X が rank-deficient なら @∞@ を返す。
iValueRegion :: [[Double]] -> [[Double]] -> Double
iValueRegion mr rows
  | null rows = 1 / 0
  | otherwise =
      let m   = LA.fromLists rows
          p   = LA.cols m
          mrM = LA.fromLists mr
          xtx = LA.tr m LA.<> m
          d   = LA.det xtx
      in if LA.rows mrM /= p || LA.cols mrM /= p || abs d < 1e-12
           then 1 / 0
           else LA.sumElements (LA.takeDiag (LA.inv xtx LA.<> mrM))

-- | Bayesian D-criterion value: @det(XᵀX + K)@。
-- K の次元が X の列数と不一致なら 0 を返す (= 採用されない)。
bayesianDValue :: [[Double]] -> [[Double]] -> Double
bayesianDValue k rows
  | null rows = 0
  | otherwise =
      let m  = LA.fromLists rows
          p  = LA.cols m
          km = LA.fromLists k
      in if LA.rows km /= p || LA.cols km /= p
           then 0
           else LA.det (LA.tr m LA.<> m + km)

-- | I-criterion with self moment: trace((XᵀX)⁻¹ · (XᵀX) / n) = p / n。
-- 簡略実装として trace((XᵀX)⁻¹) を返す (A-criterion と同等の方向性)。
-- 真の I-optimal は外部 moment matrix が必要だが、 ここでは候補集合と
-- 同分布を仮定して self-moment で代用する近似版。
iValueWithSelf :: [[Double]] -> Double
iValueWithSelf rows
  | null rows = 1 / 0
  | otherwise =
      let m   = LA.fromLists rows
          xtx = LA.tr m LA.<> m
          d   = LA.det xtx
      in if abs d < 1e-12 then 1 / 0
           else
             let inv     = LA.inv xtx
                 moment  = LA.scale (1 / fromIntegral (length rows)) xtx
             in LA.sumElements (LA.takeDiag (inv LA.<> moment))

-- | G-criterion value (self approximation): max leverage of @H = X (XᵀX)⁻¹ Xᵀ@
-- の対角の最大値。 既に「小さい方が良い」 方向 (= max leverage が小さい設計が
-- 望ましい) なので符号反転なし。
gValue :: [[Double]] -> Double
gValue rows
  | null rows = 1 / 0
  | otherwise =
      let m   = LA.fromLists rows
          xtx = LA.tr m LA.<> m
          d   = LA.det xtx
      in if abs d < 1e-12 then 1 / 0
           else
             let inv = LA.inv xtx
                 h   = m LA.<> inv LA.<> LA.tr m
                 dia = LA.toList (LA.takeDiag h)
             in if null dia then 1 / 0 else maximum dia

-- | E-criterion value: − (minimum eigenvalue of XᵀX)。
-- 最小化方向に統一するため負号。
eValue :: [[Double]] -> Double
eValue rows
  | null rows = 1 / 0
  | otherwise =
      let m   = LA.fromLists rows
          xtx = LA.tr m LA.<> m
          eigs = LA.toList (LA.eigenvaluesSH (LA.trustSym xtx))
      in if null eigs then 1 / 0 else - minimum eigs

-- ---------------------------------------------------------------------------
-- Fedorov 交換アルゴリズム
-- ---------------------------------------------------------------------------

-- | Generic optimal design: pick @n@ rows from a candidate set.
optimalDesign :: OptCriterion        -- ^ Optimization criterion.
              -> [[Double]]          -- ^ Candidate set (each row is a
                                     --   potential design row).
              -> Int                 -- ^ Number of runs to select.
              -> Int                 -- ^ Seed for the initial selection.
              -> ([Int], [[Double]]) -- ^ Selected candidate indices and
                                     --   the resulting design matrix.
optimalDesign crit cands n seed
  | n <= 0 || nC == 0 = ([], [])
  | otherwise =
  let -- ★点の反復を許す exact design。 候補を循環させて必ず n 点の初期選択を作る
      --   (@n > nC@ でも頭打ちにならない)。 @n <= nC@ なら @take n shuffled@ に一致し従来と同じ。
      initIdx = take n (cycle (pseudoShuffle seed [0 .. nC - 1]))
      design  = map (cands !!) initIdx
      -- 改善する交換が無くなるまで反復。 追加候補 @j@ は @current@ に既にあってもよい
      --   (= 同一候補点の反復を許す)。 反復が criterion を悪化させる (@n <= nC@ で distinct が
      --   最適な) 場合は @newC < bestC@ が成り立たず不採用ゆえ、 従来の distinct 結果は不変。
      improve current currentCrit =
        let pairs =
              [ (i, j)
              | i <- [0 .. n - 1]   -- 取り除く index (current の中で)
              , j <- [0 .. nC - 1]  -- 追加候補 (cands の中で・反復可)
              ]
            tryEach (bestIdx, bestC) (i, j) =
              let swapped = take i bestIdx ++ [j] ++ drop (i + 1) bestIdx
                  newDes  = map (cands !!) swapped
                  newC    = critValue crit newDes
              in if newC < bestC then (swapped, newC) else (bestIdx, bestC)
            (improved, improvedC) =
              foldl' tryEach (current, currentCrit) pairs
        in if improvedC < currentCrit
             then improve improved improvedC
             else (improved, currentCrit)
      initC = critValue crit design
      (finalIdx, _) = improve initIdx initC
  in (finalIdx, map (cands !!) finalIdx)
  where
    nC = length cands

-- | Build a D-optimal design (specialization of 'optimalDesign').
dOptimal :: [[Double]] -> Int -> Int -> ([Int], [[Double]])
dOptimal = optimalDesign DOpt

-- | Build an A-optimal design.
aOptimal :: [[Double]] -> Int -> Int -> ([Int], [[Double]])
aOptimal = optimalDesign AOpt

-- | Build an I-optimal design (specialization of 'optimalDesign').
iOptimal :: [[Double]] -> Int -> Int -> ([Int], [[Double]])
iOptimal = optimalDesign IOpt

-- | Build an E-optimal design (specialization of 'optimalDesign').
eOptimal :: [[Double]] -> Int -> Int -> ([Int], [[Double]])
eOptimal = optimalDesign EOpt

-- | Build a G-optimal design (self approximation、 specialization of
-- 'optimalDesign')。 spec: doe-spec v0.2 §2.9 / §3.6。
gOptimal :: [[Double]] -> Int -> Int -> ([Int], [[Double]])
gOptimal = optimalDesign GOpt

-- ---------------------------------------------------------------------------
-- 候補集合の生成
-- ---------------------------------------------------------------------------

-- | Equally-spaced grid of candidates: @k@ factors, @numLevels@ values
-- per factor on @[-1, 1]@.
candidateGrid :: Int -> Int -> [[Double]]
candidateGrid k numLevels =
  let levels = if numLevels == 1 then [0]
                else [-1 + 2 * fromIntegral i / fromIntegral (numLevels - 1)
                     | i <- [0 .. numLevels - 1] :: [Int]]
      go 0 = [[]]
      go d = [v : row | v <- levels, row <- go (d - 1)]
  in go k

-- | Expand a candidate grid into the @quadraticDesign@-style row
-- representation.
--
-- @quadraticCandidates k numLevels@ — each candidate is the row
-- @[1, x_1, …, x_k, x_1², …, x_k²,
-- pairwise interactions]@.
quadraticCandidates :: Int -> Int -> [[Double]]
quadraticCandidates k numLevels =
  let baseGrid = candidateGrid k numLevels
      expand row =
        let sqE   = [x * x | x <- row]
            interE = [(row !! i) * (row !! j)
                     | i <- [0 .. k - 1], j <- [i + 1 .. k - 1]]
        in 1 : row ++ sqE ++ interE
  in map expand baseGrid

-- ---------------------------------------------------------------------------
-- ヘルパ
-- ---------------------------------------------------------------------------

-- | LCG ベースの簡易シャッフル (再現性のため seed 指定)。
pseudoShuffle :: Int -> [a] -> [a]
pseudoShuffle seed xs =
  let lcg s = (s * 1103515245 + 12345) `mod` (2 ^ (31 :: Int))
      seeds = take (length xs) (drop 1 (iterate lcg seed))
      paired = zip seeds xs
      sorted = sortByKey paired
  in map snd sorted
  where
    sortByKey [] = []
    sortByKey (p:ps) =
      sortByKey [q | q <- ps, fst q <= fst p]
      ++ [p]
      ++ sortByKey [q | q <- ps, fst q > fst p]


-- ===========================================================================
-- Augment Design (Phase 5、 request/160)
-- ===========================================================================

-- | 'augmentDesign' の結果。
data AugmentResult = AugmentResult
  { arNewIndices  :: ![Int]
    -- ^ 候補集合から選ばれた追加点の index リスト (長さ = 要求した N)
  , arNewRows     :: ![[Double]]
    -- ^ 追加点の実値 (= map (cands !!) arNewIndices)
  , arFullDesign  :: ![[Double]]
    -- ^ 完成 design 行列 (existing ++ new、 元の existing 順序を保つ)
  , arInitialCrit :: !Double
    -- ^ existing 単独の criterion 値 (D-opt なら |XᵀX|; n < p 等で singular なら 0)
  , arFinalCrit   :: !Double
    -- ^ 完成 design の criterion 値
  } deriving (Show)

-- | 既存 design に N 行追加するための D-opt / A-opt 最適化。
--
-- 既存行は固定 (swap されない)。 候補集合から N 個を選び、
-- 完成 design (= existing ++ new) の criterion を最大化する Fedorov 交換を行う。
--
-- アルゴリズム:
--
-- 1. seed-based pseudoShuffle で候補集合から N 個を初期選択
-- 2. 「現在の追加行 i ↔ 未選択候補 j」 の全ペアを試行
-- 3. swap した完成 design の criterion が改善するなら採用
-- 4. 1 sweep で改善が無くなるまで反復
--
-- 失敗: N ≤ 0 や候補数 < N の場合は AugmentResult { arNewIndices = [], ... }
-- (= 空の追加) を返す。
augmentDesign
  :: OptCriterion
  -> [[Double]]            -- existing rows (固定)
  -> Int                   -- N (追加する行数)
  -> [[Double]]            -- candidate set
  -> Int                   -- seed
  -> AugmentResult
augmentDesign crit existing n cands seed
  | n <= 0 || nC < n =
      AugmentResult
        { arNewIndices  = []
        , arNewRows     = []
        , arFullDesign  = existing
        , arInitialCrit = safeCrit crit existing
        , arFinalCrit   = safeCrit crit existing
        }
  | otherwise =
      let initIdx = take n (pseudoShuffle seed [0 .. nC - 1])
          initial = combine initIdx
          initC   = critValue crit initial
          improve current currentC =
            let pairs =
                  [ (i, j)
                  | i <- [0 .. n - 1]
                  , j <- [0 .. nC - 1]
                  , j `notElem` current
                  ]
                tryEach (bestIdx, bestC) (i, j) =
                  let swapped = take i bestIdx ++ [j] ++ drop (i + 1) bestIdx
                      newC    = critValue crit (combine swapped)
                  in if newC < bestC then (swapped, newC) else (bestIdx, bestC)
                (improved, improvedC) =
                  foldl' tryEach (current, currentC) pairs
            in if improvedC < currentC
                 then improve improved improvedC
                 else (improved, currentC)
          (finalIdx, _) = improve initIdx initC
          newRows       = map (cands !!) finalIdx
      in AugmentResult
           { arNewIndices  = finalIdx
           , arNewRows     = newRows
           , arFullDesign  = existing ++ newRows
           , arInitialCrit = safeCrit crit existing
           , arFinalCrit   = safeCrit crit (existing ++ newRows)
           }
  where
    nC = length cands
    combine idx = existing ++ map (cands !!) idx

-- | criterion を「比較用 sign」 でなく、 実際の表示値 (D-opt は |XᵀX|、
--   A-opt は trace((XᵀX)⁻¹)) で返すヘルパ。 D-opt は singular で 0、 A-opt は ∞
--   になりうるので、 numeric guard を入れる。
safeCrit :: OptCriterion -> [[Double]] -> Double
safeCrit _    []   = 0
safeCrit DOpt rows = dValue rows
safeCrit AOpt rows = aValue rows
safeCrit IOpt rows = iValueWithSelf rows
safeCrit EOpt rows = eValue rows
safeCrit GOpt rows = gValue rows
safeCrit (Compound ws) rows =
  sum [ w * safeCrit c rows | (w, c) <- ws ]
safeCrit (BayesianD k) rows = bayesianDValue k rows
safeCrit (IOptRegion mr) rows = iValueRegion mr rows
