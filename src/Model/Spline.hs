{-# LANGUAGE OverloadedStrings #-}
-- | B-spline / Natural cubic spline 回帰。
--
-- スプライン基底関数で計画行列 B を構築し、通常の OLS を解いて係数 β を得る:
--
--   y_i = Σ_j β_j B_j(x_i) + ε_i
--
-- - 'bsplineBasis': Cox-de Boor 再帰で次数 k の B-spline 基底
-- - 'naturalSplineBasis': 自然立方スプライン (境界外で線形)
-- - 'fitSpline': 基底行列 + LM で fit
-- - 'predictSpline': 新しい x に対して予測
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

-- | スプラインの種類。
data SplineKind
  = BSpline Int    -- ^ 次数 k (通常 k=3 = cubic B-spline)
  | NaturalCubic   -- ^ 自然立方スプライン
  deriving (Show, Eq)

-- | フィット結果と再現に必要な情報。
data SplineFit = SplineFit
  { sfKind    :: SplineKind
  , sfKnots   :: [Double]   -- ^ 内部ノット (境界含む)
  , sfBeta    :: LA.Vector Double
  , sfResult  :: FitResult
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- B-spline basis (Cox-de Boor recursion)
-- ---------------------------------------------------------------------------

-- | 1 つの x に対する B-spline 基底値を返す。
--
-- 入力: 次数 k、拡張ノット列 t (長さ n_basis + k + 1)、評価点 x
-- 出力: [B_0(x), B_1(x), ..., B_{n_basis-1}(x)]  (長さ n_basis)
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

-- | B-spline 基底行列。
--
-- 入力:
--   * @k@ — 次数 (通常 3)
--   * @intKnots@ — 内部ノット (境界含む、ソート済を想定)
--   * @xs@ — 評価点
--
-- 出力: 行列 (n × n_basis) where n_basis = length intKnots + k - 1
--
-- 拡張ノット列は両端を `replicate (k+1)` で複製して構築 (clamped B-spline)。
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

-- | 自然立方スプライン基底 (境界の二階微分が 0 = 境界外で線形)。
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

-- | 単出力スプライン回帰。多出力 'fitSplineMulti' に Y を 1 列行列化して委譲。
fitSpline :: SplineKind -> [Double] -> V.Vector Double -> V.Vector Double -> SplineFit
fitSpline kind knots xs ys =
  let yMat = LA.asColumn (LA.fromList (V.toList ys))
      mf   = fitSplineMulti kind knots xs yMat
      beta = LA.flatten (smfBeta mf LA.¿ [0])
  in SplineFit kind knots beta (smfResult mf)

predictSpline :: SplineFit -> V.Vector Double -> V.Vector Double
predictSpline fit xsNew =
  let dm = case sfKind fit of
        BSpline k     -> bsplineBasis k (sfKnots fit) xsNew
        NaturalCubic  -> naturalSplineBasis (sfKnots fit) xsNew
      yPred = dm LA.#> sfBeta fit
  in V.fromList (LA.toList yPred)

-- | 多出力スプライン回帰: 同じ x グリッドで q 出力を同時 fit。
-- 内部は基底行列 + Multi LM。
data SplineFitMulti = SplineFitMulti
  { smfKind    :: SplineKind
  , smfKnots   :: [Double]
  , smfBeta    :: LA.Matrix Double  -- (basis_dim × q)
  , smfResult  :: FitResult
  } deriving (Show)

-- | Y は n × q (Matrix)。各列を独立に fit するが基底は共有。
fitSplineMulti :: SplineKind
               -> [Double]            -- ノット
               -> V.Vector Double     -- xs (n)
               -> LA.Matrix Double    -- Y (n × q)
               -> SplineFitMulti
fitSplineMulti kind knots xs ys =
  let dm = case kind of
        BSpline k     -> bsplineBasis k knots xs
        NaturalCubic  -> naturalSplineBasis knots xs
      r  = fitLM dm ys
  in SplineFitMulti kind knots (coefficients r) r

predictSplineMulti :: SplineFitMulti -> V.Vector Double -> LA.Matrix Double
predictSplineMulti fit xsNew =
  let dm = case smfKind fit of
        BSpline k     -> bsplineBasis k (smfKnots fit) xsNew
        NaturalCubic  -> naturalSplineBasis (smfKnots fit) xsNew
  in dm LA.<> smfBeta fit

-- ---------------------------------------------------------------------------
-- Knot helpers
-- ---------------------------------------------------------------------------

-- | 等間隔ノット (両端含む、合計 n 個)。
equalSpacedKnots :: Int -> Double -> Double -> [Double]
equalSpacedKnots n lo hi
  | n < 2     = [lo, hi]
  | otherwise = [lo + fromIntegral i * (hi - lo) / fromIntegral (n - 1)
                | i <- [0 .. n - 1]]

-- | 標本分位点ベースのノット (両端は min/max、内部は等分位点)。
quantileKnots :: Int -> V.Vector Double -> [Double]
quantileKnots n xs
  | n < 2     = [V.minimum xs, V.maximum xs]
  | otherwise =
      let sorted = sort (V.toList xs)
          m      = length sorted
          qAt p  = sorted !! min (m - 1) (max 0 (floor (p * fromIntegral m) :: Int))
          ps     = [fromIntegral i / fromIntegral (n - 1) | i <- [0 .. n - 1] :: [Int]]
      in map qAt ps
