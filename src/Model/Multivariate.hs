{-# LANGUAGE OverloadedStrings #-}
-- | Specialized multivariate regression: Reduced-Rank Regression, PLS,
-- and CCA.
--
-- These all express the relationship between a multi-response @Y@
-- (@n × q@) and multi-predictor @X@ (@n × p@) via a low-rank structure.
--
--   * 'reducedRankRegression' — @B = U_r V_rᵀ@ (rank-@r@ constraint).
--   * 'pls'                   — extracts directions of maximum
--     @X@-@Y@ covariance one at a time.
--   * 'cca'                   — canonical pairs maximizing @X@-@Y@
--     correlation.
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

-- | Reduced-Rank Regression result. The coefficient matrix @B@ is
-- constrained to rank @r@.
data RRRFit = RRRFit
  { rrrBeta :: LA.Matrix Double  -- ^ @B@ of shape @p × q@ (rank @≤ r@).
  , rrrU    :: LA.Matrix Double  -- ^ Left factor (@p × r@).
  , rrrV    :: LA.Matrix Double  -- ^ Right factor (@q × r@).
  , rrrRank :: Int               -- ^ Effective rank.
  } deriving (Show)

-- | Reduced-Rank Regression: @B = U Vᵀ@ with rank @r@.
--
-- The OLS estimate @B̂@ is SVD-truncated to its top @r@ singular values:
-- @B̂_RRR = U_r Σ_r V_rᵀ@.
reducedRankRegression :: Int                -- ^ Target rank @r@.
                     -> LA.Matrix Double    -- ^ Design matrix @X@ (@n × p@).
                     -> LA.Matrix Double    -- ^ Response @Y@ (@n × q@).
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

-- | Predict @Ŷ@ for new inputs from a 'RRRFit'.
predictRRR :: RRRFit -> LA.Matrix Double -> LA.Matrix Double
predictRRR fit xNew = xNew LA.<> rrrBeta fit

-- ---------------------------------------------------------------------------
-- Partial Least Squares (NIPALS algorithm)
-- ---------------------------------------------------------------------------

-- | PLS fit result.
data PLSFit = PLSFit
  { plsBeta :: LA.Matrix Double  -- ^ Regression coefficients (@p × q@).
  , plsW    :: LA.Matrix Double  -- ^ Weights (@p × k@).
  , plsT    :: LA.Matrix Double  -- ^ Scores (@n × k@).
  , plsP    :: LA.Matrix Double  -- ^ Loadings (@p × k@).
  , plsQ    :: LA.Matrix Double  -- ^ Y-loadings (@q × k@).
  , plsK    :: Int               -- ^ Number of components extracted.
  } deriving (Show)

-- | NIPALS-PLS (Wold 1975). Extracts @k@ components sequentially.
--
-- For each component:
--
--   1. @w = Xᵀ Y u / ‖Xᵀ Y u‖@ — the X-side weight (@u@ is the Y direction).
--   2. @t = X w@.
--   3. @p = Xᵀ t / (tᵀ t)@.
--   4. @q = Yᵀ t / (tᵀ t)@.
--   5. Deflate: @X ← X − t pᵀ@, @Y ← Y − t qᵀ@.
pls :: Int                      -- ^ Number of components @k@.
    -> LA.Matrix Double         -- ^ Design matrix @X@ (@n × p@).
    -> LA.Matrix Double         -- ^ Response @Y@ (@n × q@).
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

-- | Predict @Ŷ@ for new inputs from a 'PLSFit'.
predictPLS :: PLSFit -> LA.Matrix Double -> LA.Matrix Double
predictPLS fit xNew = xNew LA.<> plsBeta fit

-- ---------------------------------------------------------------------------
-- Canonical Correlation Analysis
-- ---------------------------------------------------------------------------

-- | CCA fit result.
data CCAFit = CCAFit
  { ccaA       :: LA.Matrix Double  -- ^ X-side basis (@p × r@).
  , ccaB       :: LA.Matrix Double  -- ^ Y-side basis (@q × r@).
  , ccaCorr    :: LA.Vector Double  -- ^ Canonical correlations (length @r@).
  , ccaScoresX :: LA.Matrix Double  -- ^ X scores (@n × r@).
  , ccaScoresY :: LA.Matrix Double  -- ^ Y scores (@n × r@).
  } deriving (Show)

-- | Canonical Correlation Analysis: find basis pairs @(a_k, b_k)@ that
-- maximize the correlation between @X@ and @Y@.
--
-- Algorithm:
--
--   1. Compute @C_xx = XᵀX/(n-1)@, @C_yy@, @C_xy@.
--   2. SVD of @M = C_xx^{−1/2} C_xy C_yy^{−1/2}@: @M = U Σ Vᵀ@.
--   3. @a = C_xx^{−1/2} U@, @b = C_yy^{−1/2} V@, correlations = @Σ@.
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
