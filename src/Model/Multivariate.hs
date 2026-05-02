{-# LANGUAGE OverloadedStrings #-}
-- | 多変量回帰の特殊形: Reduced Rank Regression / PLS / CCA。
--
-- これらは複数応答 (Y is n × q) と複数説明変数 (X is n × p) の関係を
-- 低ランクで表現する。
--
-- - 'reducedRankRegression': B = U_r V_rᵀ (rank r、低ランク制約)
-- - 'pls':                   X と Y の共分散最大の方向を逐次抽出
-- - 'cca':                   X と Y の相関最大の対 (canonical pairs)
module Model.Multivariate
  ( -- * Reduced Rank Regression
    RRRFit (..)
  , reducedRankRegression
  , predictRRR
    -- * Partial Least Squares
  , PLSFit (..)
  , pls
  , predictPLS
    -- * Canonical Correlation Analysis
  , CCAFit (..)
  , cca
  ) where

import qualified Numeric.LinearAlgebra as LA

-- ---------------------------------------------------------------------------
-- Reduced Rank Regression
-- ---------------------------------------------------------------------------

-- | RRR の結果。係数行列 B が rank r に制約される。
data RRRFit = RRRFit
  { rrrBeta   :: LA.Matrix Double  -- p × q (rank ≤ r)
  , rrrU      :: LA.Matrix Double  -- p × r (左因子)
  , rrrV      :: LA.Matrix Double  -- q × r (右因子)
  , rrrRank   :: Int
  } deriving (Show)

-- | Reduced Rank Regression: B = U V^T with rank r.
--
-- アルゴリズム: OLS の B̂ を SVD 分解し、上位 r 特異値で truncate。
-- B̂_RRR = U_r Σ_r V_rᵀ
reducedRankRegression :: Int                  -- rank r
                     -> LA.Matrix Double      -- X (n × p)
                     -> LA.Matrix Double      -- Y (n × q)
                     -> RRRFit
reducedRankRegression r x y =
  let bOLS = x LA.<\> y                -- OLS: p × q
      (u, sv, vt) = LA.svd bOLS
      r' = min r (LA.size sv)
      uR = u LA.?? (LA.All, LA.Take r')
      sR = LA.subVector 0 r' sv
      vR = (LA.tr vt) LA.?? (LA.All, LA.Take r')
      bRRR = uR LA.<> LA.diag sR LA.<> LA.tr vR
  in RRRFit bRRR uR vR r'

predictRRR :: RRRFit -> LA.Matrix Double -> LA.Matrix Double
predictRRR fit xNew = xNew LA.<> rrrBeta fit

-- ---------------------------------------------------------------------------
-- Partial Least Squares (NIPALS algorithm)
-- ---------------------------------------------------------------------------

-- | PLS の結果。t_k スコア、p_k 重み、回帰係数 B。
data PLSFit = PLSFit
  { plsBeta    :: LA.Matrix Double   -- 回帰係数 (p × q)
  , plsW       :: LA.Matrix Double   -- 重み (p × k)
  , plsT       :: LA.Matrix Double   -- スコア (n × k)
  , plsP       :: LA.Matrix Double   -- ローディング (p × k)
  , plsQ       :: LA.Matrix Double   -- Y ローディング (q × k)
  , plsK       :: Int                -- 成分数
  } deriving (Show)

-- | NIPALS-PLS (Wold 1975)。k 成分を逐次抽出。
--
-- 各成分:
--   1. w = X^T Y u / ||X^T Y u|| で X 重み (u は Y の方向)
--   2. t = X w
--   3. p = X^T t / (t^T t)
--   4. q = Y^T t / (t^T t)
--   5. X ← X − t pᵀ、Y ← Y − t qᵀ で deflate
pls :: Int                       -- 成分数 k
    -> LA.Matrix Double          -- X (n × p)
    -> LA.Matrix Double          -- Y (n × q)
    -> PLSFit
pls k x0 y0 =
  let p = LA.cols x0
      q = LA.cols y0
      n = LA.rows x0
      _ = n
      go' iter xCur yCur ws ts ps qs
        | iter >= k = (reverse ws, reverse ts, reverse ps, reverse qs)
        | otherwise =
            let u    = LA.flatten (yCur LA.¿ [0])
                xtyu = LA.tr xCur LA.#> u
                w    = if LA.norm_2 xtyu > 1e-12
                         then LA.scale (1 / LA.norm_2 xtyu) xtyu
                         else LA.fromList (replicate p 0)
                t    = xCur LA.#> w
                tt   = max 1e-12 (LA.dot t t)
                pVec = LA.scale (1/tt) (LA.tr xCur LA.#> t)
                qVec = LA.scale (1/tt) (LA.tr yCur LA.#> t)
                xNew = xCur - LA.outer t pVec
                yNew = yCur - LA.outer t qVec
            in go' (iter + 1) xNew yNew (w:ws) (t:ts) (pVec:ps) (qVec:qs)
      (wsL, tsL, psL, qsL) = go' 0 x0 y0 [] [] [] []
      wM = LA.fromColumns wsL  -- p × k
      tM = LA.fromColumns tsL  -- n × k
      pM = LA.fromColumns psL  -- p × k
      qM = LA.fromColumns qsL  -- q × k
      -- 回帰係数: B = W (PᵀW)⁻¹ Qᵀ (Wold formula)
      ptw = LA.tr pM LA.<> wM   -- k × k
      bMat = wM LA.<> LA.inv ptw LA.<> LA.tr qM   -- p × q
      _ = q
  in PLSFit bMat wM tM pM qM k

predictPLS :: PLSFit -> LA.Matrix Double -> LA.Matrix Double
predictPLS fit xNew = xNew LA.<> plsBeta fit

-- ---------------------------------------------------------------------------
-- Canonical Correlation Analysis
-- ---------------------------------------------------------------------------

-- | CCA の結果。
data CCAFit = CCAFit
  { ccaA           :: LA.Matrix Double  -- X 側基底 (p × r)
  , ccaB           :: LA.Matrix Double  -- Y 側基底 (q × r)
  , ccaCorr        :: LA.Vector Double  -- canonical correlations (r 次元)
  , ccaScoresX     :: LA.Matrix Double  -- X scores (n × r)
  , ccaScoresY     :: LA.Matrix Double  -- Y scores (n × r)
  } deriving (Show)

-- | CCA: X と Y の相関を最大化する基底ペア (a_k, b_k) を見つける。
--
-- アルゴリズム:
--   1. C_xx = XᵀX / (n-1)、C_yy、C_xy を計算
--   2. M = C_xx^{-1/2} C_xy C_yy^{-1/2} の SVD: M = U Σ Vᵀ
--   3. a = C_xx^{-1/2} U、b = C_yy^{-1/2} V、相関 = Σ
cca :: LA.Matrix Double -> LA.Matrix Double -> CCAFit
cca x y =
  let n  = fromIntegral (LA.rows x) :: Double
      _p = LA.cols x
      _q = LA.cols y
      -- 中心化
      meanCol m = LA.fromList [LA.sumElements (LA.flatten (m LA.¿ [j])) / n
                              | j <- [0 .. LA.cols m - 1]]
      mxs = meanCol x
      mys = meanCol y
      cx0 i = LA.flatten (x LA.¿ [i]) - LA.scalar (mxs LA.! i)
      cy0 i = LA.flatten (y LA.¿ [i]) - LA.scalar (mys LA.! i)
      xC  = LA.fromColumns [cx0 i | i <- [0 .. LA.cols x - 1]]
      yC  = LA.fromColumns [cy0 i | i <- [0 .. LA.cols y - 1]]
      -- 共分散
      cxx = LA.scale (1 / (n - 1)) (LA.tr xC LA.<> xC)
      cyy = LA.scale (1 / (n - 1)) (LA.tr yC LA.<> yC)
      cxy = LA.scale (1 / (n - 1)) (LA.tr xC LA.<> yC)
      -- 平方根逆行列 (固有値分解で計算)
      invSqrt sym =
        let (eigs, evec) = LA.eigSH (LA.sym sym)
            invSqrtVals = LA.fromList
              [ if v > 1e-12 then 1 / sqrt v else 0
              | v <- LA.toList eigs ]
        in evec LA.<> LA.diag invSqrtVals LA.<> LA.tr evec
      cxxIS = invSqrt cxx
      cyyIS = invSqrt cyy
      mMat  = cxxIS LA.<> cxy LA.<> cyyIS
      (uM, sM, vtM) = LA.svd mMat
      aMat = cxxIS LA.<> uM
      bMat = cyyIS LA.<> LA.tr vtM
      scoresX = xC LA.<> aMat
      scoresY = yC LA.<> bMat
  in CCAFit aMat bMat sM scoresX scoresY
