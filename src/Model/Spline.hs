{-# LANGUAGE OverloadedStrings #-}
-- | B-spline and natural cubic-spline regression.
--
-- Builds a design matrix @B@ from spline basis functions and solves
-- ordinary least squares for the coefficients @β@:
--
-- @
-- y_i = Σ_j β_j B_j(x_i) + ε_i
-- @
--
--   * 'bsplineBasis'       — degree-@k@ B-spline basis via the Cox-de Boor
--     recursion.
--   * 'naturalSplineBasis' — natural cubic spline (linear outside the
--     boundary).
--   * 'fitSpline'          — fit using the basis matrix + LM.
--   * 'predictSpline'      — predict at new @x@ values.
module Model.Spline
  ( SplineKind (..)
  , SplineFit (..)
  , SplineFitMulti (..)
  , bsplineBasis
  , naturalSplineBasis
  , fitSpline
  , fitSplineMulti
  , predictSpline
  , predictSplineMulti
  , equalSpacedKnots
  , quantileKnots
  ) where

import qualified Data.Vector as V
import qualified Numeric.LinearAlgebra as LA
import Data.List (sort)
import Model.Core (FitResult (..))
import Model.LM (fitLM)

-- | Spline kind.
data SplineKind
  = BSpline Int    -- ^ B-spline of degree @k@ (3 = cubic is typical).
  | NaturalCubic   -- ^ Natural cubic spline.
  deriving (Show, Eq)

-- | Spline fit result, with everything needed to reproduce predictions.
data SplineFit = SplineFit
  { sfKind   :: SplineKind
  , sfKnots  :: [Double]         -- ^ Interior knots (boundaries included).
  , sfBeta   :: LA.Vector Double -- ^ Basis-coefficient vector.
  , sfResult :: FitResult        -- ^ Underlying linear-model fit.
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- B-spline basis (Cox-de Boor recursion)
-- ---------------------------------------------------------------------------

-- | Evaluate every B-spline basis function at a single point.
--
-- Inputs: degree @k@, extended knot sequence @t@ (length
-- @n_basis + k + 1@), and the evaluation point @x@. Returns
-- @[B_0(x), B_1(x), ..., B_{n_basis-1}(x)]@.
bsplineEval :: Int -> [Double] -> Double -> [Double]
bsplineEval k tKnots x =
  let nBasis = length tKnots - k - 1
      -- Order 0 (= k=0): 1 if x in [t_i, t_{i+1}), else 0
      -- 端点処理: 最後のノットでは右閉
      order0 i =
        let ti  = tKnots !! i
            ti1 = tKnots !! (i + 1)
            isLast = i == length tKnots - 2
        in if (x >= ti && x < ti1) || (isLast && x <= ti1 && x >= ti)
             then 1.0 else 0.0
      -- 高次: Cox-de Boor
      go p prev =
        let n_p = length prev - 1   -- prev の長さは n + p
        in [ let ti   = tKnots !! i
                 tipk = tKnots !! (i + p)
                 ti1  = tKnots !! (i + 1)
                 ti1pk = tKnots !! (i + p + 1)
                 d1   = tipk - ti
                 d2   = ti1pk - ti1
                 a    = if d1 == 0 then 0
                          else (x - ti) / d1 * (prev !! i)
                 b    = if d2 == 0 then 0
                          else (ti1pk - x) / d2 * (prev !! (i + 1))
             in a + b
           | i <- [0 .. n_p - 1] ]
      step p prev | p > k     = prev
                  | otherwise = step (p + 1) (go p prev)
      ord0 = [order0 i | i <- [0 .. length tKnots - 2]]
  in take nBasis (step 1 ord0)

-- | B-spline basis matrix.
--
-- Inputs:
--
--   * @k@        — degree (3 typical).
--   * @intKnots@ — interior knots (boundaries included; assumed sorted).
--   * @xs@       — evaluation points.
--
-- The output matrix has shape @n × n_basis@ where
-- @n_basis = length intKnots + k - 1@. The extended knot sequence is
-- built by replicating each boundary @k+1@ times (clamped B-spline).
bsplineBasis :: Int -> [Double] -> V.Vector Double -> LA.Matrix Double
bsplineBasis k intKnots xs =
  let knots = sort intKnots
      lo    = head knots
      hi    = last knots
      tExt  = replicate (k + 1) lo
              ++ tail (init knots)        -- 内部ノット
              ++ replicate (k + 1) hi
      -- 上で tExt の長さは (k+1) + (length knots - 2) + (k+1) = length knots + 2k
      -- n_basis = length knots + 2k - k - 1 = length knots + k - 1
      rows  = [ bsplineEval k tExt x | x <- V.toList xs ]
  in LA.fromLists rows

-- ---------------------------------------------------------------------------
-- Natural cubic spline basis
-- ---------------------------------------------------------------------------

-- | Natural cubic-spline basis (zero second derivative at the
-- boundaries; linear outside the boundary).
--
-- ノット K1 < K2 < ... < KN に対して、N 個の基底関数:
--   N_1(x) = 1
--   N_2(x) = x
--   N_{k+2}(x) = d_k(x) - d_{N-1}(x)  for k = 1..N-2
-- where
--   d_k(x) = [(x - K_k)_+^3 - (x - K_N)_+^3] / (K_N - K_k)
--
-- 出力: 行列 (n × N)。
naturalSplineBasis :: [Double] -> V.Vector Double -> LA.Matrix Double
naturalSplineBasis knots xs =
  let ks = sort knots
      n  = length ks
      kN = last ks
      kNm1 = ks !! (n - 2)
      pos3 v = if v <= 0 then 0 else v ^ (3 :: Int)
      d k x =
        let kk = ks !! k
        in (pos3 (x - kk) - pos3 (x - kN)) / (kN - kk)
      basis x =
        [1.0, x] ++
        [ d k x - d (n - 2) x | k <- [0 .. n - 3] ]
  in LA.fromLists [basis xv | xv <- V.toList xs]

-- ---------------------------------------------------------------------------
-- Fit / predict
-- ---------------------------------------------------------------------------

-- | Single-output spline regression. Delegates to 'fitSplineMulti' by
-- promoting @y@ to a one-column matrix.
fitSpline :: SplineKind -> [Double] -> V.Vector Double -> V.Vector Double -> SplineFit
fitSpline kind knots xs ys =
  let yMat = LA.asColumn (LA.fromList (V.toList ys))
      mf   = fitSplineMulti kind knots xs yMat
      beta = LA.flatten (smfBeta mf LA.¿ [0])
  in SplineFit kind knots beta (smfResult mf)

-- | Predict at new @x@ values from a 'SplineFit'.
predictSpline :: SplineFit -> V.Vector Double -> V.Vector Double
predictSpline fit xsNew =
  let dm = case sfKind fit of
        BSpline k     -> bsplineBasis k (sfKnots fit) xsNew
        NaturalCubic  -> naturalSplineBasis (sfKnots fit) xsNew
      yPred = dm LA.#> sfBeta fit
  in V.fromList (LA.toList yPred)

-- | Multi-output spline regression: fit @q@ outputs jointly on the same
-- @x@ grid. Internally a basis matrix plus a multi-output LM.
data SplineFitMulti = SplineFitMulti
  { smfKind   :: SplineKind
  , smfKnots  :: [Double]
  , smfBeta   :: LA.Matrix Double  -- ^ Basis coefficients (@basis_dim × q@).
  , smfResult :: FitResult
  } deriving (Show)

-- | Fit a multi-output spline. @Y@ has shape @n × q@; columns share the
-- basis but are otherwise fit independently.
fitSplineMulti :: SplineKind
               -> [Double]            -- ^ Knots.
               -> V.Vector Double     -- ^ Inputs @xs@ (length @n@).
               -> LA.Matrix Double    -- ^ Response @Y@ (@n × q@).
               -> SplineFitMulti
fitSplineMulti kind knots xs ys =
  let dm = case kind of
        BSpline k     -> bsplineBasis k knots xs
        NaturalCubic  -> naturalSplineBasis knots xs
      r  = fitLM dm ys
  in SplineFitMulti kind knots (coefficients r) r

-- | Predict @Ŷ@ at new inputs from a 'SplineFitMulti'.
predictSplineMulti :: SplineFitMulti -> V.Vector Double -> LA.Matrix Double
predictSplineMulti fit xsNew =
  let dm = case smfKind fit of
        BSpline k     -> bsplineBasis k (smfKnots fit) xsNew
        NaturalCubic  -> naturalSplineBasis (smfKnots fit) xsNew
  in dm LA.<> smfBeta fit

-- ---------------------------------------------------------------------------
-- Knot helpers
-- ---------------------------------------------------------------------------

-- | Equal-spaced knots (both endpoints included, @n@ points total).
equalSpacedKnots :: Int -> Double -> Double -> [Double]
equalSpacedKnots n lo hi
  | n < 2     = [lo, hi]
  | otherwise = [lo + fromIntegral i * (hi - lo) / fromIntegral (n - 1)
                | i <- [0 .. n - 1]]

-- | Quantile-based knots (boundaries at min/max, interior knots at
-- evenly-spaced sample quantiles).
quantileKnots :: Int -> V.Vector Double -> [Double]
quantileKnots n xs
  | n < 2     = [V.minimum xs, V.maximum xs]
  | otherwise =
      let sorted = sort (V.toList xs)
          m      = length sorted
          qAt p  = sorted !! min (m - 1) (max 0 (floor (p * fromIntegral m) :: Int))
          ps     = [fromIntegral i / fromIntegral (n - 1) | i <- [0 .. n - 1] :: [Int]]
      in map qAt ps
