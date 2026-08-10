{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns      #-}
-- |
-- Module      : Hanalyze.Model.LiNGAM.Parce
-- Description : ParceLiNGAM (Tashiro 2014、潜在交絡に頑健な bottom-up + HSIC LiNGAM 拡張)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: ParceLiNGAM (Tashiro et al. 2014):
--   __潜在交絡 (unobserved confounders) に頑健な__ LiNGAM 拡張。
--
-- ## モデル
--
-- 通常の LiNGAM は @X = B X + e@ で e の各成分独立を要求する。 潜在交絡が
-- ある場合、 観測 X だけ見ると e が独立に見えず DirectLiNGAM は誤った因果
-- 順序を出すことがある。 ParceLiNGAM は:
--
-- > X = B X + Λ · f + e
--
-- ここで f が潜在交絡変数。
--
-- ## アルゴリズム (v0.2、 bottom-up + HSIC、 cdt15/lingam 準拠)
--
-- cdt15/lingam の `lingam/bottom_up_parce_lingam.py` を参照実装とする
-- bottom-up 探索:
--
-- 1. 候補集合 U = {0, .., p-1} を初期化
-- 2. 各候補 j ∈ U について、 残り @U \\ {j}@ の変数で x_j を OLS 回帰した
--    残差 R を作る。 「x_j が最も下流 (sink)」 ならば
--    @{x_i : i ∈ U \\ {j}}@ と R は独立になるはず
-- 3. 独立度を @hsicAggregate (x_{U \\ {j}}, R)@ で測る (HSIC 総和)。
--    最小のものを最も下流の候補 j* として選ぶ
-- 4. その HSIC 集約値が threshold @pcAcceptThr@ を下回れば j* を順序末尾に
--    追加して U から削除。 そうでなければ探索停止
-- 5. 未確定の変数群は __unresolved group__ ('pcUnresolvedGroup') として
--    まとめて返す (潜在交絡で順序が同定不能)
--
-- v0.1 (per-pair OLS + Pairwise LiNGAM) は __削除__ した。 v0.2 は
-- リファレンス実装と同じ「集合 vs 単変量残差」 の依存判定に切替。
--
-- ## 独立性判定の妥協点
--
-- cdt15/lingam では HSIC を gamma 近似で p 値化し Fisher 法で合成する。
-- v0.2 では HSIC __統計量の総和__ を直接スコアとして使い、 閾値で判定する
-- (実装軽量化、 p 値の校正は将来課題)。 相対比較 (どの候補が最も独立か)
-- は機能する。 absolute threshold はサンプル数 / 分散依存なので、 ユーザは
-- @pcAcceptThr@ をデータに合わせて調整する想定。
--
-- ## リファレンス
--
-- Tashiro et al. (2014) "ParceLiNGAM: A causal ordering method robust against
-- latent confounders", Neural Computation 26(1).
-- cdt15/lingam の `lingam/bottom_up_parce_lingam.py`。
--
-- [English]: ParceLiNGAM (Tashiro et al. 2014): a LiNGAM extension
-- __robust to latent confounders (unobserved confounders)__.
--
-- ## Model
--
-- Ordinary LiNGAM, @X = B X + e@, requires each component of e to be
-- independent. When latent confounders are present, e may not appear
-- independent when looking only at the observed X, and DirectLiNGAM can
-- produce an incorrect causal order. ParceLiNGAM instead assumes:
--
-- > X = B X + Λ · f + e
--
-- where f is the latent confounding variable.
--
-- ## Algorithm (v0.2, bottom-up + HSIC, follows cdt15/lingam)
--
-- A bottom-up search whose reference implementation is cdt15/lingam's
-- `lingam/bottom_up_parce_lingam.py`:
--
-- 1. Initialize the candidate set U = {0, .., p-1}
-- 2. For each candidate j ∈ U, build the residual R from OLS-regressing
--    x_j on the remaining variables @U \\ {j}@. If "x_j is the most
--    downstream (sink)", then @{x_i : i ∈ U \\ {j}}@ and R should be
--    independent
-- 3. Measure independence via @hsicAggregate (x_{U \\ {j}}, R)@ (the
--    HSIC sum). Pick the candidate j* with the smallest value as the
--    most downstream
-- 4. If that HSIC aggregate falls below the threshold @pcAcceptThr@,
--    append j* to the end of the order and remove it from U. Otherwise
--    stop the search
-- 5. Any undetermined variables are returned together as an
--    __unresolved group__ ('pcUnresolvedGroup') (order could not be
--    identified due to latent confounders)
--
-- v0.1 (per-pair OLS + Pairwise LiNGAM) was __removed__. v0.2 switched
-- to the same "set vs univariate residual" dependence test as the
-- reference implementation.
--
-- ## Compromise on the independence test
--
-- cdt15/lingam converts HSIC into p-values via a gamma approximation
-- and combines them with Fisher's method. v0.2 uses the
-- __sum of the HSIC statistic__ directly as the score and thresholds it (a
-- lighter-weight implementation; calibrating p-values is future work).
-- Relative comparison (which candidate is most independent) works
-- fine. Since the absolute threshold depends on sample size \/
-- variance, the user is expected to tune @pcAcceptThr@ to their data.
--
-- ## References
--
-- Tashiro et al. (2014) "ParceLiNGAM: A causal ordering method robust
-- against latent confounders", Neural Computation 26(1).
-- cdt15/lingam's `lingam/bottom_up_parce_lingam.py`.
module Hanalyze.Model.LiNGAM.Parce
  ( ParceConfig (..)
  , ParceFit (..)
  , defaultParceConfig
  , fitParceLiNGAM
  , parceDAG
  ) where

import qualified Numeric.LinearAlgebra as LA
import           Data.List             (foldl', sortBy)
import           Data.Ord              (comparing)

import qualified Hanalyze.Math.HSIC    as HSIC
import qualified Hanalyze.Model.DAG    as DAG

-- ===========================================================================
-- 設定 / 結果
-- ===========================================================================

data ParceConfig = ParceConfig
  { pcRelRatio :: !Double
    -- ^ [日本語]: 受理判定の相対比閾値。 best 候補の HSIC 集約値が 2 番目候補の値の
    --   pcRelRatio 倍未満なら sink として受理。 default 0.5
    --   (best が 2nd の半分未満で「明瞭に独立」 と判断)。
    --
    --   絶対 HSIC の値はサンプル数 / 分散 / median bandwidth に強く依存する
    --   ため、 v0.2 では絶対閾値を捨て __相対比のみ__ で判定する。 集合サイズ |U|
    --   = 2 のときは 2 候補のうち小さい方/大きい方が pcRelRatio 未満
    --   なら受理 (= 自然な「明瞭差」 検出)。
    --
    --   [English]: The relative-ratio threshold for the acceptance
    --   test. Accepted as a sink if the best candidate's HSIC aggregate
    --   is under pcRelRatio times the second candidate's value. Default
    --   0.5 (judged "clearly independent" if the best is under half the
    --   2nd).
    --
    --   Because the absolute HSIC value depends strongly on sample
    --   size \/ variance \/ median bandwidth, v0.2 discards the
    --   absolute threshold and judges by __relative ratio only__. When
    --   |U| = 2, it is accepted if the smaller\/larger of the two
    --   candidates is under pcRelRatio (= a natural detector of a
    --   "clear gap").
  , pcPruneThr :: !Double
    -- ^ [日本語]: B 行列 pruning 閾値、 default 0.05。 [English]: The B-matrix pruning threshold, default 0.05.
  } deriving (Show)

defaultParceConfig :: ParceConfig
defaultParceConfig = ParceConfig
  { pcRelRatio = 0.5
  , pcPruneThr = 0.05
  }

data ParceFit = ParceFit
  { pcOrder            :: ![Int]
    -- ^ [日本語]: 確定できた causal order (sink → source の順で逆に並んだものを
    --   さらに反転 → source → sink の順)。 unresolved group があるときは
    --   その後ろに連結 (Spec 互換のため任意順で末尾追加)。
    --   [English]: The determined causal order (found in sink → source
    --   order, then reversed → source → sink order). If there is an
    --   unresolved group, it is concatenated after (appended at the end
    --   in arbitrary order for spec compatibility).
  , pcB                :: !(LA.Matrix Double)
    -- ^ [日本語]: 構造方程式係数行列。 unresolved 群内の係数は OLS で仮置きされる
    --   (確定的順序が無いので解釈は控えめに)。
    --   [English]: The structural-equation coefficient matrix. The
    --   coefficients within the unresolved group are provisionally set
    --   via OLS (interpret with caution since there is no definite
    --   order).
  , pcAdjacency        :: !(LA.Matrix Double)
  , pcUnresolvedGroup  :: ![Int]
    -- ^ [日本語]: 潜在交絡で順序が同定不能と判定された変数群 (空ならば全変数確定)。
    --   [English]: The group of variables judged unidentifiable in
    --   order due to latent confounders (empty if all variables are
    --   determined).
  } deriving (Show)

-- ===========================================================================
-- 主実装
-- ===========================================================================

fitParceLiNGAM :: ParceConfig -> LA.Matrix Double -> ParceFit
fitParceLiNGAM cfg x =
  let !p          = LA.cols x
      (sinkList, leftover) = bottomUpSearch cfg x [0 .. p - 1]
      -- sinkList は新しく見つけた順に **prepend** しているので、
      -- 自然と「upstream → downstream」 (source → sink) の順に並ぶ。
      -- leftover (確定できなかった残り) を先頭に置く: 長さ 1 なら単なる
      -- source、 長さ ≥ 2 なら **潜在交絡で順序不能** のグループ。
      !fullOrder         = leftover ++ sinkList
      !unresolved        = if length leftover > 1 then leftover else []
      !bMat       = buildBFromOrder p x fullOrder
      !adjMat     = adjFromB (pcPruneThr cfg) bMat
  in ParceFit
       { pcOrder           = fullOrder
       , pcB               = bMat
       , pcAdjacency       = adjMat
       , pcUnresolvedGroup = unresolved
       }

-- | [日本語]: DAG 表現を返す。
--   [English]: Returns the DAG representation.
parceDAG :: ParceConfig -> ParceFit -> DAG.DAG
parceDAG cfg fit = DAG.fromBMatrix (pcPruneThr cfg) (pcB fit)

-- ===========================================================================
-- bottom-up 探索
-- ===========================================================================

-- | [日本語]: 候補集合 U から sink を 1 つずつ削り出す。
--   戻り値: (確定した sink を upstream→downstream の順で並べたリスト、
--   残り未確定 U)。 ※ prepend で蓄積するため、 最後に見つけたもの
--   (=最も upstream に近い) が先頭、 最初に見つけたもの (=最も downstream)
--   が末尾、 つまり自然な source → sink 順。
--   [English]: Peels off sinks one at a time from the candidate set U.
--   Returns: (the list of determined sinks in upstream→downstream
--   order, the remaining undetermined U). Note: since it accumulates
--   via prepend, the last one found (= closest to upstream) is at the
--   head, and the first one found (= most downstream) is at the tail —
--   i.e. the natural source → sink order.
bottomUpSearch
  :: ParceConfig
  -> LA.Matrix Double
  -> [Int]                   -- 初期 U (全変数 index)
  -> ([Int], [Int])
bottomUpSearch cfg x = go []
  where
    go !sinks u
      | length u <= 1 = (sinks, u)         -- 1 個以下なら確定済とみなす
      | otherwise =
          let scored      = sortBy (comparing snd)
                              [ (j, scoreSink x u j) | j <- u ]
              (jStar, sB) = head scored
              sNext       = snd (scored !! 1)
              accept      = sB < pcRelRatio cfg * sNext
          in if accept
               then go (jStar : sinks) (filter (/= jStar) u)
               else (sinks, u)              -- 明瞭な sink が無い → halt

-- | [日本語]: 候補 j を sink と仮定したときの「他変数 U\\{j} ⊥ R_j」 の HSIC 集約値。
--   R_j = x_j を x_{U\\{j}} で OLS 回帰した残差。
--   [English]: The HSIC aggregate of "other variables U\\{j} ⊥ R_j"
--   under the assumption that candidate j is the sink. R_j is the
--   residual of x_j OLS-regressed on x_{U\\{j}}.
scoreSink :: LA.Matrix Double -> [Int] -> Int -> Double
scoreSink x u j =
  let others = filter (/= j) u
      xj     = LA.flatten (x LA.¿ [j])
      xRest  = LA.fromColumns [ LA.flatten (x LA.¿ [k]) | k <- others ]
      r      = partialResidual xj xRest
  in HSIC.hsicAggregate xRest r

-- ===========================================================================
-- 内部ヘルパ
-- ===========================================================================

-- | [日本語]: y を Z (n × q 行列) に OLS 回帰した残差。
--   [English]: The residual of OLS-regressing y on Z (an n × q matrix).
partialResidual :: LA.Vector Double -> LA.Matrix Double -> LA.Vector Double
partialResidual y z =
  let xtx  = LA.tr z LA.<> z
      xty  = LA.tr z LA.#> y
      beta = LA.flatten (LA.linearSolveLS xtx (LA.asColumn xty))
  in y - z LA.#> beta

-- | [日本語]: causal order に従い OLS で B 行列を構築 (DirectLiNGAM と同手順)。
--   [English]: Builds the B matrix via OLS following the causal order
--   (same procedure as DirectLiNGAM).
buildBFromOrder :: Int -> LA.Matrix Double -> [Int] -> LA.Matrix Double
buildBFromOrder p x order =
  let mkRow j =
        let kj      = order !! j
            parents = take j order
        in if null parents
             then LA.fromList (replicate p 0)
             else
               let pm = LA.fromColumns
                     [ LA.flatten (x LA.¿ [pi_]) | pi_ <- parents ]
                   y  = LA.flatten (x LA.¿ [kj])
                   xtx = LA.tr pm LA.<> pm
                   xty = LA.tr pm LA.#> y
                   beta = LA.flatten
                            (LA.linearSolveLS xtx (LA.asColumn xty))
                   updates = zip parents (LA.toList beta)
                   coefV   = replicate p 0
                   filled  = foldl' (\acc (i, v) -> set acc i v) coefV updates
               in LA.fromList filled
      bRows = [ mkRow j | j <- [0 .. p - 1] ]
      pos i = case lookup i (zip order [0 ..]) of
                Just k  -> k
                Nothing -> 0
  in LA.fromRows [ bRows !! pos i | i <- [0 .. p - 1] ]
  where
    set xs i v = take i xs ++ [v] ++ drop (i + 1) xs

adjFromB :: Double -> LA.Matrix Double -> LA.Matrix Double
adjFromB thr b =
  let p = LA.rows b
      f i j
        | i == j                          = 0
        | abs (LA.atIndex b (i, j)) > thr = 1
        | otherwise                       = 0
  in LA.build (p, p) (\i j -> f (round i) (round j) :: Double)
