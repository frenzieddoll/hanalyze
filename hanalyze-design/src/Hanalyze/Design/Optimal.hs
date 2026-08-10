{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hanalyze.Design.Optimal
-- Description : 最適計画 (D/A/I/E/G-optimal) — Fedorov 交換法による候補集合からの選択・拡張
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: 最適計画: D-optimal と A-optimal。
--
-- 候補集合から @n@ run の部分集合を選び、 情報行列 @XᵀX@ に基づく規準を
-- 最大化 / 最小化する。
--
--   - __D-optimal__ — @max det(XᵀX)@ → 全パラメータの同時推定精度。
--   - __A-optimal__ — @min trace((XᵀX)⁻¹)@ → 平均推定分散の最小化。
--
-- アルゴリズム: Fedorov 交換法 (逐次交換)。 候補のランダム選択から始めて、
-- 改善する交換が見つからなくなるまで繰り返す。
--
-- [English]: Optimal designs: D-optimal and A-optimal.
--
-- Selects a subset of @n@ runs from a candidate set, maximizing /
-- minimizing a criterion based on the information matrix @XᵀX@.
--
--   - __D-optimal__ — @max det(XᵀX)@ → joint estimation precision of
--     all parameters.
--   - __A-optimal__ — @min trace((XᵀX)⁻¹)@ → minimum average estimation
--     variance.
--
-- Algorithm: the Fedorov exchange method (sequential exchanges). Starts
-- from a random selection of candidates and repeats until no improving
-- exchange can be found.
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

-- | [日本語]: 最適性規準。 [English]: Optimality criterion.
data OptCriterion
  = DOpt   -- ^ [日本語]: D-optimal: @det(XᵀX)@ を最大化。 [English]: D-optimal: maximize @det(XᵀX)@.
  | AOpt   -- ^ [日本語]: A-optimal: @trace((XᵀX)⁻¹)@ を最小化。 [English]: A-optimal: minimize @trace((XᵀX)⁻¹)@.
  | IOpt   -- ^ [日本語]: I-optimal: @trace((XᵀX)⁻¹ · M_moment)@ で近似した平均予測分散
           --   を最小化。 ここでは M_moment を全候補から推定した moment matrix
           --   @candᵀ cand / n_cand@ とする。
           --   [English]: I-optimal: minimize average prediction variance,
           --   approximated by @trace((XᵀX)⁻¹ · M_moment)@. Here M_moment is
           --   the moment matrix @candᵀ cand / n_cand@ estimated from all
           --   candidates.
  | EOpt   -- ^ [日本語]: E-optimal: @(XᵀX)⁻¹@ の最大固有値を最小化 (= @XᵀX@ の最小
           --   固有値を最大化するのと同義)。
           --   [English]: E-optimal: minimize the maximum eigenvalue of
           --   @(XᵀX)⁻¹@, = maximize the minimum eigenvalue of @XᵀX@.
  | GOpt   -- ^ [日本語]: G-optimal (self 近似): @H = X (XᵀX)⁻¹ Xᵀ@ とした最大
           --   leverage @max_i (H_ii)@ を最小化。 候補集合に依存しない self-G
           --   定義 (= 設計自身の hat 対角の最大)。 厳密な G-optimal (候補空間
           --   全体の max prediction variance) は Custom Design spec 側で
           --   扱う。 spec: doe-spec v0.2 §2.9。
           --   [English]: G-optimal (self approximation): minimize the
           --   maximum leverage @max_i (H_ii)@ where @H = X (XᵀX)⁻¹ Xᵀ@. A
           --   self-G definition independent of the candidate set (i.e. the
           --   maximum of the design's own hat diagonal). The exact
           --   G-optimal (max prediction variance over the whole candidate
           --   space) is handled on the Custom Design spec side. spec:
           --   doe-spec v0.2 §2.9.
  | Compound ![(Double, OptCriterion)]
           -- ^ [日本語]: Compound (alphabetic) 規準: 各 inner criterion を
           --   /minimize/ 方向に揃えた 'critValue' の重み付き和。 重みは正数を
           --   仮定 (合計 1 への正規化はユーザ側責任)。 ネストした @Compound@ も
           --   許容 (展開して評価)。 注意: inner criterion 同士のスケールは
           --   ユーザが責任を持って揃える (例: D 0.7 + I 0.3 は両方を
           --   efficiency 形に正規化してから渡す)。 v0.2 では正規化ヘルパは
           --   未提供、 v0.3 以降で対応予定。 spec: doe-spec v0.2 §2.9。
           --   [English]: Compound (alphabetic) criterion: a weighted sum
           --   of 'critValue' with each inner criterion aligned to the
           --   /minimize/ direction. Weights are assumed positive
           --   (normalizing to a sum of 1 is the user's responsibility).
           --   Nested @Compound@ is also allowed (expanded and evaluated).
           --   Note: the user is responsible for aligning the scale
           --   between inner criteria (e.g. for D 0.7 + I 0.3, normalize
           --   both to an efficiency form before passing them in). A
           --   normalization helper is not provided in v0.2; planned for a
           --   later version. spec: doe-spec v0.2 §2.9.
  | BayesianD ![[Double]]
           -- ^ [日本語]: Bayesian D-optimality (DuMouchel-Jones 1994):
           --   @det(XᵀX + K)@ を最大化、 K = 事前精度行列 (p × p)。 K = 0
           --   行列で classic D に縮退。 spec: doe-custom-design-spec
           --   v0.1.1 §2.7。 K は @[[Double]]@ (Show / Eq 要件のため)、
           --   expand 後の列数と一致必須。
           --   [English]: Bayesian D-optimality (DuMouchel-Jones 1994):
           --   maximize @det(XᵀX + K)@, K = prior precision matrix (p × p).
           --   Degenerates to classic D with K = the zero matrix. spec:
           --   doe-custom-design-spec v0.1.1 §2.7. K is @[[Double]]@ (for
           --   the Show \/ Eq requirement) and must match the expanded
           --   column count.
  | IOptRegion ![[Double]]
           -- ^ [日本語]: I-optimal (region 積分版): @trace((XᵀX)⁻¹ · M_R)@ を
           --   最小化、 M_R = region moment matrix
           --   @∫_R f(z)f(z)' dz / vol(R)@ (p × p)。 旧 'IOpt' は self-moment
           --   近似で @= p/n@ に縮退するため設計に依らず無意味、 region 版で
           --   差し替えた。 M_R は @[[Double]]@ (Show / Eq 要件のため)、
           --   expand 後の列数と一致必須。 Custom Design 内では
           --   'Hanalyze.Design.Custom.Compare.regionMomentMatrixAnalytic'
           --   が連続 U[-1,1] + Categorical 等確率規約で M_R を構築する。
           --   [English]: I-optimal (region-integral version): minimize
           --   @trace((XᵀX)⁻¹ · M_R)@, M_R = the region moment matrix
           --   @∫_R f(z)f(z)' dz / vol(R)@ (p × p). The old 'IOpt' degenerates
           --   to @= p/n@ under the self-moment approximation, making it
           --   meaningless regardless of the design, so it was replaced by
           --   the region version. M_R is @[[Double]]@ (for the Show \/ Eq
           --   requirement) and must match the expanded column count.
           --   Within Custom Design,
           --   'Hanalyze.Design.Custom.Compare.regionMomentMatrixAnalytic'
           --   builds M_R under the continuous U[-1,1] + Categorical
           --   equal-probability convention.
  deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- 基準値の計算
-- ---------------------------------------------------------------------------

-- | [日本語]: 設計行列 @X@ の D-criterion 値: @det(XᵀX)@。
--   [English]: D-criterion value for a design matrix @X@: @det(XᵀX)@.
dValue :: [[Double]] -> Double
dValue rows
  | null rows = 0
  | otherwise = LA.det xtx
  where
    m   = LA.fromLists rows
    xtx = LA.tr m LA.<> m

-- | [日本語]: 設計行列 @X@ の A-criterion 値: @trace((XᵀX)⁻¹)@。
--   逆行列が存在しないときは @∞@ を返す。
--   [English]: A-criterion value for a design matrix @X@:
--   @trace((XᵀX)⁻¹)@. Returns @∞@ when the inverse does not exist.
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

-- | [日本語]: 最適化に使う criterion 値。 いずれの規準も /最小化/ すべき量として
--   返す。 D-optimality は @-det(XᵀX)@ として符号化する。
--   [English]: Criterion value used for optimization. Both criteria are
--   returned as quantities to /minimize/; D-optimality is encoded as
--   @-det(XᵀX)@.
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

-- | [日本語]: I-criterion (region 積分版): @trace((XᵀX)⁻¹ · M_R)@ を返す (minimize 方向)。
--   @M_R@ の次元が X の列数と不一致 / X が rank-deficient なら @∞@ を返す。
--   [English]: I-criterion (region-integral version): returns
--   @trace((XᵀX)⁻¹ · M_R)@ (in the minimize direction). Returns @∞@ if
--   @M_R@'s dimensions don't match X's column count, or if X is
--   rank-deficient.
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

-- | [日本語]: Bayesian D-criterion 値: @det(XᵀX + K)@。
--   K の次元が X の列数と不一致なら 0 を返す (= 採用されない)。
--   [English]: Bayesian D-criterion value: @det(XᵀX + K)@. Returns 0 if
--   K's dimensions don't match X's column count (i.e. not adopted).
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

-- | [日本語]: self moment 版 I-criterion: trace((XᵀX)⁻¹ · (XᵀX) / n) = p / n。
--   簡略実装として trace((XᵀX)⁻¹) を返す (A-criterion と同等の方向性)。
--   真の I-optimal は外部 moment matrix が必要だが、 ここでは候補集合と
--   同分布を仮定して self-moment で代用する近似版。
--   [English]: I-criterion with self moment: trace((XᵀX)⁻¹ · (XᵀX) / n) =
--   p / n. As a simplified implementation, this returns trace((XᵀX)⁻¹)
--   (the same direction as the A-criterion). A true I-optimal requires an
--   external moment matrix, but this approximate version substitutes the
--   self-moment under the assumption that the candidate set follows the
--   same distribution.
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

-- | [日本語]: G-criterion 値 (self 近似): @H = X (XᵀX)⁻¹ Xᵀ@ の対角の最大値
--   (= max leverage)。 既に「小さい方が良い」 方向 (= max leverage が小さい設計が
--   望ましい) なので符号反転なし。
--   [English]: G-criterion value (self approximation): the maximum of the
--   diagonal of @H = X (XᵀX)⁻¹ Xᵀ@ (= max leverage). Already in the
--   "smaller is better" direction (a design with smaller max leverage is
--   preferred), so no sign flip is needed.
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

-- | [日本語]: E-criterion 値: − (XᵀX の最小固有値)。 最小化方向に統一するため負号。
--   [English]: E-criterion value: − (minimum eigenvalue of XᵀX). Negated
--   to unify with the minimize direction.
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

-- | [日本語]: 汎用最適計画: 候補集合から @n@ 行を選ぶ。
--   [English]: Generic optimal design: pick @n@ rows from a candidate set.
optimalDesign :: OptCriterion        -- ^ [日本語]: 最適化規準。 [English]: Optimization criterion.
              -> [[Double]]          -- ^ [日本語]: 候補集合 (各行が設計行の候補)。 [English]: Candidate set (each row is a potential design row).
              -> Int                 -- ^ [日本語]: 選択する run 数。 [English]: Number of runs to select.
              -> Int                 -- ^ [日本語]: 初期選択用の seed。 [English]: Seed for the initial selection.
              -> ([Int], [[Double]]) -- ^ [日本語]: 選択された候補 index と結果の設計行列。 [English]: Selected candidate indices and the resulting design matrix.
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

-- | [日本語]: D-optimal 計画を構築 ('optimalDesign' の特殊化)。
--   [English]: Build a D-optimal design (specialization of 'optimalDesign').
dOptimal :: [[Double]] -> Int -> Int -> ([Int], [[Double]])
dOptimal = optimalDesign DOpt

-- | [日本語]: A-optimal 計画を構築。
--   [English]: Build an A-optimal design.
aOptimal :: [[Double]] -> Int -> Int -> ([Int], [[Double]])
aOptimal = optimalDesign AOpt

-- | [日本語]: I-optimal 計画を構築 ('optimalDesign' の特殊化)。
--   [English]: Build an I-optimal design (specialization of 'optimalDesign').
iOptimal :: [[Double]] -> Int -> Int -> ([Int], [[Double]])
iOptimal = optimalDesign IOpt

-- | [日本語]: E-optimal 計画を構築 ('optimalDesign' の特殊化)。
--   [English]: Build an E-optimal design (specialization of 'optimalDesign').
eOptimal :: [[Double]] -> Int -> Int -> ([Int], [[Double]])
eOptimal = optimalDesign EOpt

-- | [日本語]: G-optimal 計画を構築 (self 近似、 'optimalDesign' の特殊化)。
--   spec: doe-spec v0.2 §2.9 / §3.6。
--   [English]: Build a G-optimal design (self approximation,
--   specialization of 'optimalDesign'). spec: doe-spec v0.2 §2.9 / §3.6.
gOptimal :: [[Double]] -> Int -> Int -> ([Int], [[Double]])
gOptimal = optimalDesign GOpt

-- ---------------------------------------------------------------------------
-- 候補集合の生成
-- ---------------------------------------------------------------------------

-- | [日本語]: 等間隔な候補グリッド: @k@ 因子、 各因子 @[-1, 1]@ 上に @numLevels@ 個の値。
--   [English]: Equally-spaced grid of candidates: @k@ factors, @numLevels@
--   values per factor on @[-1, 1]@.
candidateGrid :: Int -> Int -> [[Double]]
candidateGrid k numLevels =
  let levels = if numLevels == 1 then [0]
                else [-1 + 2 * fromIntegral i / fromIntegral (numLevels - 1)
                     | i <- [0 .. numLevels - 1] :: [Int]]
      go 0 = [[]]
      go d = [v : row | v <- levels, row <- go (d - 1)]
  in go k

-- | [日本語]: 候補グリッドを @quadraticDesign@ 流の行表現に展開する。
--
--   @quadraticCandidates k numLevels@ — 各候補は行
--   @[1, x_1, …, x_k, x_1², …, x_k², pairwise interactions]@。
--
--   [English]: Expand a candidate grid into the @quadraticDesign@-style
--   row representation.
--
--   @quadraticCandidates k numLevels@ — each candidate is the row
--   @[1, x_1, …, x_k, x_1², …, x_k²,
--   pairwise interactions]@.
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

-- | [日本語]: LCG ベースの簡易シャッフル (再現性のため seed 指定)。
--   [English]: A simple LCG-based shuffle (takes a seed for reproducibility).
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

-- | [日本語]: 'augmentDesign' の結果。 [English]: The result of 'augmentDesign'.
data AugmentResult = AugmentResult
  { arNewIndices  :: ![Int]
    -- ^ [日本語]: 候補集合から選ばれた追加点の index リスト (長さ = 要求した N)
    --   [English]: List of indices of the added points chosen from the
    --   candidate set (length = the requested N)
  , arNewRows     :: ![[Double]]
    -- ^ [日本語]: 追加点の実値 (= map (cands !!) arNewIndices)
    --   [English]: The actual values of the added points
    --   (= map (cands !!) arNewIndices)
  , arFullDesign  :: ![[Double]]
    -- ^ [日本語]: 完成 design 行列 (existing ++ new、 元の existing 順序を保つ)
    --   [English]: The completed design matrix (existing ++ new,
    --   preserving the original existing order)
  , arInitialCrit :: !Double
    -- ^ [日本語]: existing 単独の criterion 値 (D-opt なら |XᵀX|; n < p 等で
    --   singular なら 0)
    --   [English]: The criterion value for existing alone (|XᵀX| for
    --   D-opt; 0 if singular, e.g. when n < p)
  , arFinalCrit   :: !Double
    -- ^ [日本語]: 完成 design の criterion 値
    --   [English]: The criterion value of the completed design
  } deriving (Show)

-- | [日本語]: 既存 design に N 行追加するための D-opt / A-opt 最適化。
--
--   既存行は固定 (swap されない)。 候補集合から N 個を選び、
--   完成 design (= existing ++ new) の criterion を最大化する Fedorov 交換を行う。
--
--   アルゴリズム:
--
--   1. seed-based pseudoShuffle で候補集合から N 個を初期選択
--   2. 「現在の追加行 i ↔ 未選択候補 j」 の全ペアを試行
--   3. swap した完成 design の criterion が改善するなら採用
--   4. 1 sweep で改善が無くなるまで反復
--
--   失敗: N ≤ 0 や候補数 < N の場合は AugmentResult { arNewIndices = [], ... }
--   (= 空の追加) を返す。
--
--   [English]: D-opt \/ A-opt optimization for adding N rows to an
--   existing design.
--
--   The existing rows are fixed (not swapped). N rows are chosen from the
--   candidate set, performing a Fedorov exchange that maximizes the
--   criterion of the completed design (= existing ++ new).
--
--   Algorithm:
--
--   1. Initial selection of N rows from the candidate set via
--      seed-based pseudoShuffle
--   2. Try every pair of "current added row i ↔ unselected candidate j"
--   3. Adopt the swap if it improves the criterion of the completed design
--   4. Repeat until one sweep produces no improvement
--
--   Failure: if N ≤ 0 or the candidate count < N, returns
--   AugmentResult { arNewIndices = [], ... } (i.e. an empty addition).
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

-- | [日本語]: criterion を「比較用 sign」 でなく、 実際の表示値 (D-opt は |XᵀX|、
--   A-opt は trace((XᵀX)⁻¹)) で返すヘルパ。 D-opt は singular で 0、 A-opt は ∞
--   になりうるので、 numeric guard を入れる。
--   [English]: A helper that returns the criterion as its actual display
--   value (|XᵀX| for D-opt, trace((XᵀX)⁻¹) for A-opt) rather than its
--   "comparison sign". D-opt can be 0 when singular and A-opt can be ∞, so
--   a numeric guard is included.
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
