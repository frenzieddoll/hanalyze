{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hanalyze.Model.GAM
-- Description : 一般化加法モデル (Generalized Additive Model, GAM)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: 一般化加法モデル (Generalized Additive Model, GAM)。
--
-- @y = β₀ + Σ_j s_j(x_j) + ε@ で、 各平滑項 @s_j(x_j) = B_j(x_j) γ_j@ は
-- 任意の基底 @B_j@ (B-spline / 自然3次 / 多項式 / Fourier / RBF) について
-- __係数について線形__ である。 そのため基底は 'GAMBasis' として抽象化され、
-- fit・predict・成分ごとの経路は、 いずれも訓練 @x@ から学習された実体化済み
-- 基底 ('BasisRealized') に基づいて分岐する。 これにより新しい点での予測は
-- __同一の基底行列__ を再構築する。
--
-- 設計:
--
--   - 各予測子 @x_j@ について、 'GAMBasis' に従って基底行列 @B_j@ (@n × m_j@)
--     を構築する。
--   - 単一の設計行列 @X = [1 | B_1 | B_2 | ... | B_p]@ (@1 + Σ m_j@ 列) に
--     結合する。
--   - Ridge 正則化 OLS:
--     @β = (XᵀX + λ P)⁻¹ Xᵀ y@、 @P = diag(0,1,…,1)@ (切片は免除)。
--     同一の @λ@ が全基底の平滑度を安定化する。
--   - @λ@ は固定 ('FixedL') か、 GCV ('GCV') により基底の実体化から選ぶかを
--     選択できる。
--   - 予測: 各特徴の寄与 @s_j(x_j)@ は個別に抽出でき、 各因子の効果の可視化に
--     使える。
--
-- 注: 識別性のため、各基底は中央化 (列平均を引く) する。
-- これで β₀ は y の平均、s_j は変動成分のみを表す。
--
-- [English]: Generalized Additive Model (GAM).
--
-- @y = β₀ + Σ_j s_j(x_j) + ε@ where each smooth term @s_j(x_j) = B_j(x_j) γ_j@
-- is __linear in its coefficients__ for *any* basis @B_j@ (B-spline / natural
-- cubic / polynomial / Fourier / RBF). The basis is therefore abstracted as
-- 'GAMBasis'; the fit, predict, and per-component paths all dispatch on the
-- realized basis ('BasisRealized') learned from the training @x@, so
-- prediction at new points rebuilds the __same__ basis matrix.
--
-- Design:
--
--   - For each predictor @x_j@, build a basis matrix @B_j@ (@n × m_j@) per
--     'GAMBasis'.
--   - Stack into a single design matrix
--     @X = [1 | B_1 | B_2 | ... | B_p]@ (@1 + Σ m_j@ columns).
--   - Ridge-regularized OLS:
--     @β = (XᵀX + λ P)⁻¹ Xᵀ y@ with @P = diag(0,1,…,1)@ (intercept exempt).
--     The same @λ@ stabilizes every basis (smoothness regularization).
--   - @λ@ may be fixed ('FixedL') or chosen by GCV ('GCV') from the realized
--     basis.
--   - Prediction: the per-feature contribution @s_j(x_j)@ can be extracted
--     individually for visualization of each factor's effect.
--
-- Note: for identifiability, each basis is centered (column means
-- subtracted). This makes β₀ the mean of y, and each s_j purely the
-- variation component.
module Hanalyze.Model.GAM
  ( -- * 基底の抽象化
    GAMBasis (..)
  , BasisRealized (..)
  , GAMLambda (..)
    -- * フィット結果
  , GAMFit (..)
    -- * フィット
  , fitGAM
  , fitGAMWith
  , fitGAMAuto
    -- * 予測
  , predictGAM
  , predictGAMSE
  , predictGAMComponent
  ) where

import qualified Data.Vector as V
import qualified Numeric.LinearAlgebra as LA
import Hanalyze.Model.Spline (bsplineBasis, naturalSplineBasis, equalSpacedKnots)

-- ---------------------------------------------------------------------------
-- 基底の抽象化
-- ---------------------------------------------------------------------------

-- | [日本語]: 平滑項 @s_j(x_j)@ の基底の種類 (係数について線形なものを列挙)。
--   各々 @x → 基底行列 (n × m)@ を与える。
--   [English]: The kind of basis for the smooth term @s_j(x_j)@ (enumerates
--   those linear in their coefficients). Each gives @x → basis matrix (n × m)@.
data GAMBasis
  = BSplineB Int Int   -- ^ [日本語]: @BSplineB degree nKnots@: degree 次 B-spline (内部ノット @nKnots@)。 [English]: @BSplineB degree nKnots@: a degree-th order B-spline (with @nKnots@ interior knots).
  | NaturalCubicB Int  -- ^ [日本語]: @NaturalCubicB nKnots@: 自然3次回帰スプライン (内部ノット @nKnots@)。 [English]: @NaturalCubicB nKnots@: a natural cubic regression spline (with @nKnots@ interior knots).
  | PolyB Int          -- ^ [日本語]: @PolyB degree@: 直交化なしの多項式 (@[t,t²,…,t^degree]@・@t∈[-1,1]@ にスケール)。 [English]: @PolyB degree@: an unorthogonalized polynomial (@[t,t²,…,t^degree]@; scaled to @t∈[-1,1]@).
  | FourierB Int       -- ^ [日本語]: @FourierB nHarmonics@: Fourier 基底 (@sin/cos@ を @nHarmonics@ 次まで)。 [English]: @FourierB nHarmonics@: a Fourier basis (@sin/cos@ up to order @nHarmonics@).
  | RBFB Int Double    -- ^ [日本語]: @RBFB nCenters bandwidthRel@: ガウス RBF (等間隔中心・帯域 = 中心間隔×bandwidthRel)。 [English]: @RBFB nCenters bandwidthRel@: a Gaussian RBF (equally spaced centers; bandwidth = center spacing × bandwidthRel).
  deriving (Show, Eq)

-- | [日本語]: 学習済み基底。 訓練 @x@ から決まる具体パラメタ (ノット/中心/レンジ) を保持し、
--   任意の新 @x@ に対し同一の基底行列を再構築できる ('evalBasis')。
--   [English]: A fitted basis. Holds the concrete parameters (knots/centers/
--   range) determined from the training @x@, so the same basis matrix can be
--   rebuilt for any new @x@ ('evalBasis').
data BasisRealized
  = RBSpline Int [Double]      -- ^ [日本語]: degree, 内部ノット列。 [English]: degree, list of interior knots.
  | RNaturalCubic [Double]     -- ^ [日本語]: ノット列。 [English]: list of knots.
  | RPoly Int Double Double    -- ^ [日本語]: degree, xmin, xmax (@t = 2(x−lo)/(hi−lo)−1@ にスケール)。 [English]: degree, xmin, xmax (scaled as @t = 2(x−lo)/(hi−lo)−1@).
  | RFourier Int Double Double -- ^ [日本語]: nHarmonics, xmin, period (@t = (x−lo)/period@)。 [English]: nHarmonics, xmin, period (@t = (x−lo)/period@).
  | RRBF [Double] Double       -- ^ [日本語]: 中心列, 帯域 (絶対値)。 [English]: list of centers, bandwidth (absolute value).
  deriving (Show)

-- | [日本語]: @λ@ の決め方。 'FixedL' は固定値、 'GCV' は一般化交差検証で 1 次元探索する。
--   [English]: How @λ@ is chosen. 'FixedL' is a fixed value; 'GCV' performs a
--   1-D search via generalized cross-validation.
data GAMLambda
  = FixedL Double  -- ^ [日本語]: 固定 @λ@ (@0@ で罰則なし)。 [English]: A fixed @λ@ (@0@ disables the penalty).
  | GCV            -- ^ [日本語]: GCV @λ* = argmin_λ n·RSS(λ)/(n−edf(λ))²@ を log グリッド探索。 [English]: Searches a log grid for GCV @λ* = argmin_λ n·RSS(λ)/(n−edf(λ))²@.
  deriving (Show, Eq)

-- | [日本語]: 'GAMBasis' を訓練 @x@ で実体化する。
--   [English]: Realizes a 'GAMBasis' at the training @x@.
realizeBasis :: GAMBasis -> V.Vector Double -> BasisRealized
realizeBasis b xs =
  let lo = if V.null xs then 0 else V.minimum xs
      hi = if V.null xs then 1 else V.maximum xs
  in case b of
       BSplineB deg nK     -> RBSpline deg (equalSpacedKnots (nK + 2) lo hi)
       -- 自然3次は基底に ≥3 ノット必要 (端2 + 内部)。 等間隔で nK+2 点 (両端含む)。
       NaturalCubicB nK    -> RNaturalCubic (equalSpacedKnots (max 3 (nK + 2)) lo hi)
       PolyB deg           -> RPoly (max 1 deg) lo hi
       FourierB h          -> RFourier (max 1 h) lo (let p = hi - lo in if p <= 0 then 1 else p)
       RBFB c bwRel        ->
         let nc      = max 2 c
             centers = equalSpacedKnots nc lo hi
             spacing = if nc < 2 then 1 else (hi - lo) / fromIntegral (nc - 1)
             bw      = (if spacing <= 0 then 1 else spacing) * (if bwRel <= 0 then 1 else bwRel)
         in RRBF centers bw

-- | [日本語]: 学習済み基底で新 @x@ の基底行列 (@n × m@・__未中央化__) を作る。
--   [English]: Builds the basis matrix (@n × m@; __not centered__) for a new
--   @x@ from the fitted basis.
evalBasis :: BasisRealized -> V.Vector Double -> LA.Matrix Double
evalBasis br xs = case br of
  RBSpline deg knots -> bsplineBasis deg knots xs
  -- naturalSplineBasis は先頭に定数列を含む → GAM は別途切片を持つので落とす。
  RNaturalCubic knots ->
    let m = naturalSplineBasis knots xs
    in if LA.cols m <= 1 then m else m LA.?? (LA.All, LA.Drop 1)
  RPoly deg lo hi ->
    let denom = hi - lo
        t x   = if denom <= 0 then 0 else 2 * (x - lo) / denom - 1
        row x = [ t x ^^ k | k <- [1 .. deg] ]
    in LA.fromLists [ row x | x <- V.toList xs ]
  RFourier h lo period ->
    let t x   = (x - lo) / period
        row x = concat [ [ sin (2 * pi * fromIntegral k * t x)
                         , cos (2 * pi * fromIntegral k * t x) ]
                       | k <- [1 .. h] ]
    in LA.fromLists [ row x | x <- V.toList xs ]
  RRBF centers bw ->
    let row x = [ exp (negate 0.5 * ((x - c) / bw) ^ (2 :: Int)) | c <- centers ]
    in LA.fromLists [ row x | x <- V.toList xs ]

-- ---------------------------------------------------------------------------
-- 型
-- ---------------------------------------------------------------------------

-- | GAM fit result.
data GAMFit = GAMFit
  { gamDegree    :: Int                  -- ^ [日本語]: (後方互換) 先頭 B-spline 項の degree。非 B-spline は 0。 [English]: (Backward compatibility) The degree of the leading B-spline term. 0 for non-B-spline bases.
  , gamKnots     :: [[Double]]           -- ^ [日本語]: (後方互換) 項ごとのノット列。ノットを持たない基底は @[]@。 [English]: (Backward compatibility) The knot list per term. @[]@ for bases without knots.
  , gamBases     :: [BasisRealized]      -- ^ [日本語]: __評価の正典__: 項ごとの学習済み基底。 [English]: The __canonical source for evaluation__: the fitted basis per term.
  , gamBetas     :: [LA.Vector Double]   -- ^ Per-feature spline coefficients @γ_j@.
  , gamColMeans  :: [LA.Vector Double]   -- ^ Per-feature column means of @B_j@ (for centering).
  , gamIntercept :: Double               -- ^ Intercept @β₀@.
  , gamYHat      :: LA.Vector Double     -- ^ Fitted values.
  , gamResid     :: LA.Vector Double     -- ^ Residuals.
  , gamR2        :: Double               -- ^ R².
  , gamLambda    :: Double               -- ^ [日本語]: Ridge penalty @λ@ used (GCV のときは選ばれた値)。 [English]: The ridge penalty @λ@ used (the value chosen by GCV, when applicable).
  , gamEdf       :: Double               -- ^ [日本語]: 有効自由度 @tr(S_λ)@ (GCV 用)。 [English]: The effective degrees of freedom @tr(S_λ)@ (used by GCV).
  , gamCov       :: LA.Matrix Double     -- ^ [日本語]: 係数共分散 @Vβ = (XᵀX+λP)⁻¹·φ̂@
                                         --   (mgcv 流 Bayesian CI 用・@φ̂ = RSS/(n−edf)@)。
                                         --   [English]: Coefficient covariance @Vβ = (XᵀX+λP)⁻¹·φ̂@
                                         --   (for mgcv-style Bayesian CIs; @φ̂ = RSS/(n−edf)@).
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- フィット
-- ---------------------------------------------------------------------------

-- | [日本語]: Fit a GAM (B-spline 基底固定の薄ラッパ・後方互換)。
--   [English]: Fits a GAM (a thin wrapper fixed to a B-spline basis, kept for
--   backward compatibility).
fitGAM :: Int                    -- ^ B-spline degree (3 = cubic recommended).
       -> Int                    -- ^ Number of interior knots (e.g. 5).
       -> Double                 -- ^ Ridge penalty @λ@ (0 disables regularization).
       -> [V.Vector Double]      -- ^ Predictors @[x₁, x₂, …]@.
       -> V.Vector Double        -- ^ Response @y@.
       -> GAMFit
fitGAM degree nKnots lambda xss =
  fitGAMWith [ BSplineB degree nKnots | _ <- xss ] lambda xss

-- | [日本語]: Fit a GAM with per-term基底を明示 + 固定 @λ@。
--   [English]: Fits a GAM with an explicit per-term basis and a fixed @λ@.
fitGAMWith :: [GAMBasis]          -- ^ [日本語]: 項ごとの基底 (長さ = 予測子数)。 [English]: The basis per term (length = number of predictors).
           -> Double              -- ^ Ridge penalty @λ@.
           -> [V.Vector Double]   -- ^ Predictors.
           -> V.Vector Double     -- ^ Response @y@.
           -> GAMFit
fitGAMWith bases lambda xss y =
  let realized = zipWith realizeBasis bases xss
  in fitCore realized lambda xss y

-- | [日本語]: Fit a GAM choosing @λ@ via 'GAMLambda' (FixedL / GCV)。
--   [English]: Fits a GAM, choosing @λ@ via 'GAMLambda' (FixedL / GCV).
fitGAMAuto :: [GAMBasis] -> GAMLambda -> [V.Vector Double] -> V.Vector Double -> GAMFit
fitGAMAuto bases lam xss y =
  let realized = zipWith realizeBasis bases xss
  in case lam of
       FixedL l -> fitCore realized l xss y
       GCV      ->
         let grid = [ 10 ** e | e <- [(-4.0), (-3.5) .. 4.0 :: Double] ]
             score l = gamGCV (fitCore realized l xss y)
             best = snd (minimum [ (score l, l) | l <- grid ])
         in fitCore realized best xss y

-- | [日本語]: GCV 値 @n·RSS/(n−edf)²@ (小さいほど良い)。
--   [English]: The GCV value @n·RSS/(n−edf)²@ (smaller is better).
gamGCV :: GAMFit -> Double
gamGCV fit =
  let n   = fromIntegral (LA.size (gamResid fit)) :: Double
      rss = LA.sumElements (LA.cmap (^ (2 :: Int)) (gamResid fit))
      den = n - gamEdf fit
  in if den <= 1e-9 then 1/0 else n * rss / (den * den)

-- | [日本語]: 学習済み基底列 + 固定 @λ@ で最小二乗を解く中核。
--   [English]: The core solver that fits least squares from a list of fitted
--   bases and a fixed @λ@.
fitCore :: [BasisRealized] -> Double -> [V.Vector Double] -> V.Vector Double -> GAMFit
fitCore realized lambda xss y =
  let n         = V.length y
      -- 各 B_j (n × m_j) を構築 + 列平均で中央化
      basisRaw  = zipWith evalBasis realized xss
      colMeans  = [ LA.fromList
                      [ LA.sumElements (LA.flatten (b LA.¿ [j])) / fromIntegral n
                      | j <- [0 .. LA.cols b - 1] ]
                  | b <- basisRaw ]
      basisCent = zipWith centerCols basisRaw colMeans

      -- 統合計画行列 X = [1 | B_1 | B_2 | ...]
      ones = LA.asColumn (LA.konst 1 n)
      x    = foldl1 (LA.|||) (ones : basisCent)
      yLA  = LA.fromList (V.toList y)
      p    = LA.cols x

      -- Ridge: β = (XᵀX + λ I')⁻¹ Xᵀ y  (intercept 列はペナルティ免除)
      pen  = LA.diag (LA.fromList (0 : replicate (p - 1) lambda))
      xtx  = LA.tr x LA.<> x
      lhs  = xtx + pen
      lhsInv = LA.inv lhs                -- (XᵀX+λP)⁻¹ (edf と Vβ で共用)
      xty  = LA.tr x LA.#> yLA
      beta = lhsInv LA.#> xty

      -- 有効自由度 edf = tr(S_λ) = tr((XᵀX+λP)⁻¹ XᵀX)
      edf  = sumDiag (lhsInv LA.<> xtx)

      -- intercept = β[0]、各特徴の γ_j を切り出す
      mSizes = [ LA.cols b | b <- basisRaw ]
      starts = scanl (+) 1 mSizes        -- intercept は 0
      betas  = [ LA.subVector (starts !! j) (mSizes !! j) beta
               | j <- [0 .. length xss - 1] ]
      intercept = beta LA.! 0

      yhat  = x LA.#> beta
      resid = yLA - yhat
      yMean = LA.sumElements yLA / fromIntegral n
      tss   = LA.sumElements (LA.cmap (\v -> (v - yMean) ^ (2 :: Int)) yLA)
      rss   = LA.sumElements (LA.cmap (^ (2 :: Int)) resid)
      r2    = if tss < 1e-12 then 0 else 1 - rss / tss
      -- CI 用係数共分散 Vβ = (XᵀX+λP)⁻¹·φ̂ (mgcv 流 Bayesian・φ̂ = RSS/(n−edf))。
      dfRes = fromIntegral n - edf
      phi   = if dfRes > 1e-9 then rss / dfRes else rss
      cov   = LA.scale phi lhsInv
  in GAMFit
       { gamDegree    = case realized of { (RBSpline d _ : _) -> d; _ -> 0 }
       , gamKnots     = map knotsOf realized
       , gamBases     = realized
       , gamBetas     = betas
       , gamColMeans  = colMeans
       , gamIntercept = intercept
       , gamYHat      = yhat
       , gamResid     = resid
       , gamR2        = r2
       , gamLambda    = lambda
       , gamEdf       = edf
       , gamCov       = cov
       }
  where
    -- 列平均を引いて中央化
    centerCols :: LA.Matrix Double -> LA.Vector Double -> LA.Matrix Double
    centerCols m mu =
      let cols = LA.toColumns m
          centered = zipWith (\c muVal -> LA.cmap (\v -> v - muVal) c)
                       cols (LA.toList mu)
      in LA.fromColumns centered
    sumDiag :: LA.Matrix Double -> Double
    sumDiag = LA.sumElements . LA.takeDiag
    knotsOf :: BasisRealized -> [Double]
    knotsOf (RBSpline _ k)     = k
    knotsOf (RNaturalCubic k)  = k
    knotsOf _                  = []

-- ---------------------------------------------------------------------------
-- 予測
-- ---------------------------------------------------------------------------

-- | Predict at new predictors.
predictGAM :: GAMFit -> [V.Vector Double] -> V.Vector Double
predictGAM fit xss =
  let n = if null xss then 0 else V.length (head xss)
      contributions = zipWith4 componentVec
                        (gamBases fit) (gamBetas fit) (gamColMeans fit) xss
      total = foldl' (V.zipWith (+)) (V.replicate n (gamIntercept fit))
                contributions
  in total
  where
    foldl' f z [] = z
    foldl' f z (a:as) = let !z' = f z a in foldl' f z' as
    componentVec :: BasisRealized -> LA.Vector Double -> LA.Vector Double
                 -> V.Vector Double -> V.Vector Double
    componentVec br gamma mu xs =
      let b      = evalBasis br xs
          n'     = LA.rows b
          ys     = b LA.#> gamma
          shiftV = LA.dot mu gamma
      in V.fromList [ ys LA.! i - shiftV | i <- [0 .. n' - 1] ]

-- | [日本語]: Predict + 各評価点の __pointwise standard error__ を返す (CI 帯用)。
--
--   評価点設計行列 @Xeval = [1 | (B_j − colMean_j) | …]@ を fit と同じ中央化で組み、
--   @se_i = √(b_i Vβ b_iᵀ)@ ('gamCov' = @Vβ@)。 中心 @μ̂@ は 'predictGAM' と一致する。
--   信頼水準 → 臨界値 (t) の掛け算は呼び出し側 (描画層) が行う。
--
--   [English]: Predicts and returns the __pointwise standard error__ at each
--   evaluation point (for CI bands).
--
--   Builds the evaluation design matrix @Xeval = [1 | (B_j − colMean_j) | …]@
--   with the same centering as the fit, giving @se_i = √(b_i Vβ b_iᵀ)@
--   ('gamCov' = @Vβ@). The center @μ̂@ matches 'predictGAM'. Multiplying by the
--   critical value (t) for a confidence level is left to the caller (the
--   plotting layer).
predictGAMSE :: GAMFit -> [V.Vector Double] -> (V.Vector Double, V.Vector Double)
predictGAMSE fit xss =
  let nEval     = if null xss then 0 else V.length (head xss)
      mu        = predictGAM fit xss
      basisRaw  = zipWith evalBasis (gamBases fit) xss
      basisCent = zipWith subtractColMeans basisRaw (gamColMeans fit)
      ones      = LA.asColumn (LA.konst 1 nEval)
      xEval     = foldl1 (LA.|||) (ones : basisCent)      -- nEval × p
      m1        = xEval LA.<> gamCov fit                  -- nEval × p
      varVec    = [ LA.dot rM rX | (rM, rX) <- zip (LA.toRows m1) (LA.toRows xEval) ]
      se        = map (sqrt . max 0) varVec
  in (mu, V.fromList se)

-- | [日本語]: 各列から学習時の列平均を引く (評価点を fit と同じ中央化にする)。
--   [English]: Subtracts the training-time column mean from each column
--   (applies the same centering to the evaluation points as the fit).
subtractColMeans :: LA.Matrix Double -> LA.Vector Double -> LA.Matrix Double
subtractColMeans m mu =
  LA.fromColumns (zipWith (\c muVal -> LA.cmap (subtract muVal) c)
                          (LA.toColumns m) (LA.toList mu))

-- | The contribution @s_j(x)@ from feature @j@ only (without the intercept).
predictGAMComponent :: GAMFit -> Int -> V.Vector Double -> V.Vector Double
predictGAMComponent fit j xs
  | j < 0 || j >= length (gamBetas fit) = V.empty
  | otherwise =
      let b      = evalBasis (gamBases fit !! j) xs
          gamma  = gamBetas fit !! j
          mu     = gamColMeans fit !! j
          ys     = b LA.#> gamma
          shiftV = LA.dot mu gamma
          n      = LA.rows b
      in V.fromList [ ys LA.! i - shiftV | i <- [0 .. n - 1] ]

-- 4-引数 zipWith (base に無いので局所定義)。
zipWith4 :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
zipWith4 f (a:as) (b:bs) (c:cs) (d:ds) = f a b c d : zipWith4 f as bs cs ds
zipWith4 _ _ _ _ _ = []
