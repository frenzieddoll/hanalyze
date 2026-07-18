{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns      #-}
-- |
-- Module      : Hanalyze.Model.LiNGAM.Direct
-- Description : DirectLiNGAM (Shimizu 2011) による線形非ガウシアン因果探索
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- DirectLiNGAM (Shimizu et al. 2011) による線形非ガウシアン因果探索。
--
-- ## 前提モデル
--
-- 観測 X ∈ ℝ^(n×p) が **線形 + acyclic + 非ガウシアン独立 noise** な構造方程式
-- モデル X = B X + e に従う (B は適切な行/列順列で下三角化可能、 e の各成分は
-- 互いに独立かつ非ガウシアン)。 このとき DirectLiNGAM は ICA を経由せず、
-- 残差独立性 (差分相互情報量) の最大化で因果順序を 1 変数ずつ確定する。
--
-- ## アルゴリズム概要
--
-- 1. 候補集合 U = {0..p-1}、 因果順序 K = []
-- 2. p 回 loop:
--    a. searchCausalOrder で M(m) = -Σ_{j∈U,j≠m} min(0, ΔMI(x_m,x_j,r_{mj},r_{jm}))²
--       を最大化する m を選ぶ
--    b. U の各 i ≠ m について x_i ← residual(x_i, x_m) (m で残差化)
--    c. K に m を追加、 U から m を除く
-- 3. K から B 行列を OLS で組み上げる (causal order に従い順に回帰)
--
-- ## ΔMI (差分相互情報量)
--
-- 標準化後の x_i, x_j と残差 r_{ij}, r_{ji} (互いに片方を片方で回帰した残差)
-- に対し:
--
-- > ΔMI(x_i, x_j, r_{ij}, r_{ji}) = [H(x_j) + H(r_{ij}/σ_{r_{ij}})]
-- >                                - [H(x_i) + H(r_{ji}/σ_{r_{ji}})]
--
-- H は Hyvärinen (1998) の maximum entropy 近似:
--
-- > H(u) = (1 + log 2π)/2 - k1·(E[log cosh u] - γ)² - k2·(E[u·exp(-u²/2)])²
-- > k1 = 79.047, k2 = 7.4129, γ = 0.37457
--
-- ## リファレンス
--
-- Shimizu et al. (2011) "DirectLiNGAM: A direct method for learning a linear
-- non-Gaussian structural equation model", JMLR 12. Python 実装は
-- cdt15/lingam の `lingam/direct_lingam.py` で動作対応を確認した。
--
-- ## 落とし穴メモ
--
-- * 観測変数が **完全ガウシアン** だと ΔMI ≈ 0 となり順序が一意決まらない。
--   ガウシアン応答には Phase 30 の causal inference (介入効果) や PC algorithm
--   等を使う
-- * **n < 100** だと entropy の sample 推定が不安定。 n ≥ 200 推奨
-- * 行列 B は **causal order の根本変数を 0 行目** に置く慣習。 出力の
--   dlB[K[j], K[i]] = β_i (i < j) で表される (= 影響先 ← 影響元 規約)
module Hanalyze.Model.LiNGAM.Direct
  ( DirectLiNGAMConfig (..)
  , DirectLiNGAMFit (..)
  , defaultDirectLiNGAMConfig
  , fitDirectLiNGAM
  , dlDAG
  -- helpers (re-export 不要時は internal だが、 単体テスト用に公開)
  , entropyApprox
  , diffMutualInfo
  , olsResidual
  , standardize
  ) where

import qualified Numeric.LinearAlgebra as LA
import           Data.List             (foldl')

import qualified Hanalyze.Model.DAG    as DAG

-- ===========================================================================
-- 公開型
-- ===========================================================================

-- | DirectLiNGAM の設定。
data DirectLiNGAMConfig = DirectLiNGAMConfig
  { dlcPruneThr :: !Double
    -- ^ |B_ij| < 'dlcPruneThr' は隣接行列で 0 と扱う。 default 0.05。
  } deriving (Show)

defaultDirectLiNGAMConfig :: DirectLiNGAMConfig
defaultDirectLiNGAMConfig = DirectLiNGAMConfig
  { dlcPruneThr = 0.05
  }

-- | DirectLiNGAM の推定結果。
data DirectLiNGAMFit = DirectLiNGAMFit
  { dlOrder     :: ![Int]
    -- ^ 推定 causal order (topological)。 K[0] が最も外生的、 K[p-1] が
    --   最も末端 (どの変数からも影響を受ける可能性のある変数)
  , dlB         :: !(LA.Matrix Double)
    -- ^ 構造方程式係数行列 (p × p)。 X_i = Σ_j dlB[i, j] · X_j + e_i。
    --   causal order に従い適切な行/列順列で下三角化可能
  , dlAdjacency :: !(LA.Matrix Double)
    -- ^ |dlB| > dlcPruneThr の 0/1 マスク
  , dlResiduals :: !(LA.Matrix Double)
    -- ^ 各サンプルの推定残差 e_i (n × p)。 独立性検定の事後評価に使う
  } deriving (Show)

-- ===========================================================================
-- 主アルゴリズム
-- ===========================================================================

-- | DirectLiNGAM を fit する。 X は n × p 行列 (各列 = 1 変数)。
--
-- 計算量: 因果順序探索 O(p² · n) per iteration × p iterations = O(p³ · n)
-- (entropy 評価 + 残差化が dominant)。
-- | 'DirectLiNGAMFit' を 'Hanalyze.Model.DAG.DAG' 表現に変換 (threshold は
--   元の 'dlcPruneThr' を再利用)。
dlDAG :: DirectLiNGAMConfig -> DirectLiNGAMFit -> DAG.DAG
dlDAG cfg fit = DAG.fromBMatrix (dlcPruneThr cfg) (dlB fit)

fitDirectLiNGAM :: DirectLiNGAMConfig -> LA.Matrix Double -> DirectLiNGAMFit
fitDirectLiNGAM cfg xs =
  let !p = LA.cols xs
      !n = LA.rows xs
      -- 各列を Vector に分解した可変リスト (residualize 用)
      cols0 :: [LA.Vector Double]
      cols0 = [ LA.flatten (xs LA.¿ [j]) | j <- [0 .. p - 1] ]
      -- 主 loop: cols / activeU / order を順次更新
      (order, _finalCols) = causalOrderLoop cols0 [0 .. p - 1] []
      -- 元の X から causal order に従い B 行列を OLS で組み立て
      bMat    = estimateB xs order
      adjMat  = buildAdjacency (dlcPruneThr cfg) bMat
      -- 残差: e = X - X·B^T (行ベクトル view、 単純な線形変換)
      resid   = xs - xs LA.<> LA.tr bMat
      _ = n  -- shadow warn 防止
  in DirectLiNGAMFit
       { dlOrder     = order
       , dlB         = bMat
       , dlAdjacency = adjMat
       , dlResiduals = resid
       }

-- | causal order を 1 つずつ確定する主ループ。
--   引数:
--     cols    : 現在の (残差化された) 列ベクトルのリスト (length p、 元 index で並ぶ)
--     activeU : まだ確定していない元 index のリスト
--     orderRev: これまでに確定した順序 (逆順、 後で reverse)
causalOrderLoop
  :: [LA.Vector Double]   -- 現状の列ベクトル
  -> [Int]                -- active 集合
  -> [Int]                -- 確定済 (逆順)
  -> ([Int], [LA.Vector Double])
causalOrderLoop cols activeU orderRev
  | null activeU = (reverse orderRev, cols)
  | length activeU == 1 =
      (reverse (head activeU : orderRev), cols)
  | otherwise =
      let !m = searchCausalOrder cols activeU
          xm = cols !! m
          -- m 以外の active で残差化
          colsNew = [ if j `elem` activeU && j /= m
                        then olsResidual (cols !! j) xm
                        else cols !! j
                    | j <- [0 .. length cols - 1] ]
          activeNew = [ j | j <- activeU, j /= m ]
      in causalOrderLoop colsNew activeNew (m : orderRev)

-- | 候補集合 activeU から、 「最も外生的 (= 他から残差化された後の独立性が
--   崩れにくい)」 index を 1 つ返す。
--   M(m) = -Σ_{j∈U, j≠m} min(0, ΔMI(x_m,x_j,r_{mj},r_{jm}))² を最大化。
searchCausalOrder :: [LA.Vector Double] -> [Int] -> Int
searchCausalOrder cols activeU =
  let !scores = [ (m, score m) | m <- activeU ]
      score m =
        let xm = cols !! m
            xmStd = standardize xm
            contribs =
              [ let xj = cols !! j
                    xjStd = standardize xj
                    rmj = olsResidual xmStd xjStd   -- xm を xj で残差化
                    rjm = olsResidual xjStd xmStd   -- xj を xm で残差化
                    dmi = diffMutualInfo xmStd xjStd rmj rjm
                in min 0 dmi ** 2
              | j <- activeU, j /= m ]
        in negate (sum contribs)
  in fst (foldl' pickMax (head scores) (tail scores))
  where
    pickMax acc@(_, s0) cur@(_, s1)
      | s1 > s0   = cur
      | otherwise = acc

-- | 差分相互情報量 ΔMI = [H(xj) + H(rij/σ)] - [H(xi) + H(rji/σ)]。
--   入力 xi/xj は標準化済、 rij/rji は **標準化前**の残差。
diffMutualInfo
  :: LA.Vector Double  -- xi (標準化済)
  -> LA.Vector Double  -- xj (標準化済)
  -> LA.Vector Double  -- rij = xi - β xj 残差
  -> LA.Vector Double  -- rji = xj - β xi 残差
  -> Double
diffMutualInfo xi xj rij rji =
  let !hxi  = entropyApprox xi
      !hxj  = entropyApprox xj
      !srij = stdSafe rij
      !srji = stdSafe rji
      !hrij = entropyApprox (LA.scale (1 / srij) rij)
      !hrji = entropyApprox (LA.scale (1 / srji) rji)
  in (hxj + hrij) - (hxi + hrji)
  where
    stdSafe v =
      let s = LA.norm_2 (v - LA.scalar (LA.sumElements v / fromIntegral (LA.size v)))
                / sqrt (fromIntegral (LA.size v))
      in if s > 1e-12 then s else 1.0

-- | Hyvärinen (1998) maximum entropy 近似:
--   H(u) = (1 + log 2π)/2 - k1·(E[log cosh u] - γ)² - k2·(E[u·exp(-u²/2)])²
--   u は事前に標準化されていることが前提。
entropyApprox :: LA.Vector Double -> Double
entropyApprox u =
  let !k1    = 79.047
      !k2    = 7.4129
      !gamma = 0.37457
      !n     = fromIntegral (LA.size u) :: Double
      !logCosh = LA.sumElements (LA.cmap (\v -> log (cosh v)) u) / n
      !uExp    = LA.sumElements (u * LA.cmap (\v -> exp (-v * v / 2)) u) / n
  in (1 + log (2 * pi)) / 2
     - k1 * (logCosh - gamma) ** 2
     - k2 * uExp ** 2

-- | OLS による残差: r = xi - (Cov(xi,xj) / Var(xj)) · xj
olsResidual :: LA.Vector Double -> LA.Vector Double -> LA.Vector Double
olsResidual xi xj =
  let !n   = fromIntegral (LA.size xi) :: Double
      !mxi = LA.sumElements xi / n
      !mxj = LA.sumElements xj / n
      !ci  = xi - LA.scalar mxi
      !cj  = xj - LA.scalar mxj
      !cov = ci `LA.dot` cj / n
      !var = cj `LA.dot` cj / n
      !beta = if var > 1e-12 then cov / var else 0
  in xi - LA.scale beta xj

-- | 中心化 + 標準偏差で割る (zero-mean, unit-variance)。
standardize :: LA.Vector Double -> LA.Vector Double
standardize v =
  let !n  = fromIntegral (LA.size v) :: Double
      !mu = LA.sumElements v / n
      !c  = v - LA.scalar mu
      !s  = sqrt (c `LA.dot` c / n)
      !sd = if s > 1e-12 then s else 1.0
  in LA.scale (1 / sd) c

-- ===========================================================================
-- B 行列 + 隣接行列
-- ===========================================================================

-- | causal order に従い B 行列を OLS で組み立てる。
--   B[K[j], K[i]] = OLS 回帰 X[:,K[j]] ~ X[:,K[0..j-1]] の i 番目係数。
estimateB :: LA.Matrix Double -> [Int] -> LA.Matrix Double
estimateB xs order =
  let !p    = LA.cols xs
      bRows = [ buildRow j | j <- [0 .. p - 1] ]
      buildRow j =
        let kj   = order !! j
            -- 影響元候補: order の j より前
            parents = take j order
        in if null parents
             then LA.fromList (replicate p 0)
             else
               let parentMat = LA.fromColumns
                     [ LA.flatten (xs LA.¿ [pIdx]) | pIdx <- parents ]
                   target = LA.flatten (xs LA.¿ [kj])
                   beta = olsBeta parentMat target
                   coefVec = replicate p 0
                   -- beta を parent 位置に散布
                   updates = zip parents (LA.toList beta)
                   filled = foldl' (\acc (idx, v) -> setAt acc idx v) coefVec updates
               in LA.fromList filled
      -- 行は K の順序、 列は元 variable index。
      -- bRows[j] は variable K[j] の行ベクトル → reorder で元 variable index 順に
      origOrderMat = LA.fromRows
        [ bRows !! posInOrder i | i <- [0 .. p - 1] ]
      posInOrder i = case lookup i (zip order [0 ..]) of
        Just k  -> k
        Nothing -> 0   -- unreachable
  in origOrderMat

-- | OLS 係数: β = (XᵀX)⁻¹ Xᵀy
olsBeta :: LA.Matrix Double -> LA.Vector Double -> LA.Vector Double
olsBeta x y =
  let xtx = LA.tr x LA.<> x
      xty = LA.tr x LA.#> y
  in LA.flatten (LA.linearSolveLS xtx (LA.asColumn xty))

setAt :: [a] -> Int -> a -> [a]
setAt xs i v = take i xs ++ [v] ++ drop (i + 1) xs

-- | |B_ij| > threshold で 1、 以外 0 の隣接行列。 対角は 0 に固定。
buildAdjacency :: Double -> LA.Matrix Double -> LA.Matrix Double
buildAdjacency thr b =
  let !p = LA.rows b
      f i j
        | i == j    = 0
        | abs (LA.atIndex b (i, j)) > thr = 1
        | otherwise = 0
  in LA.build (p, p) (\i j -> f (round i) (round j) :: Double)
