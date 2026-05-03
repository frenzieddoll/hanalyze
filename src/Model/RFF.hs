{-# LANGUAGE OverloadedStrings #-}
-- | Random Fourier Features (RFF) によるカーネル近似 (1D)。
--
-- Bochner の定理により、定常カーネル k(x, x') = ∫ p(ω) e^{iω(x-x')} dω は
-- 確率密度 p(ω) からサンプリングした周波数 ω_j と一様分布の位相 b_j で:
--
--   φ(x) = σ_f √(2/D) [cos(ω_j x + b_j)]_{j=1..D}
--
-- なる明示的な特徴写像で k(x, x') ≈ φ(x)·φ(x') と近似できる (Rahimi-Recht 2007)。
--
-- これを使うと:
-- - O(n³) のカーネル計算 → O(n D + D³) で線形スケール
-- - Ridge 回帰 / GP 事後をすべて D 次元線形空間で計算
--
-- このモジュールは 1 次元入力のみ対応 (拡張は容易):
-- - 'sampleRFFRBF':      RBF カーネル (ω ~ N(0, 1/ℓ²))
-- - 'sampleRFFMatern52': Matérn 5/2 (ω ~ scaled t with df = 5)
-- - 'rffFeatures':  特徴行列 Φ を構築 (n × D)
-- - 'rffRidge':     RFF + Ridge 回帰 (=O(n³) Kernel Ridge の近似)
-- - 'rffGP':        RFF + ベイズ線形回帰 = GP 事後の近似 (mean + variance)
module Model.RFF
  ( RFFKernel (..)
  , RFFFeatures (..)
  , rffDim
    -- * 特徴生成
  , sampleRFFRBF
  , sampleRFFMatern52
  , rffFeatures
  , rffApproxKernel
    -- * RFF Ridge 回帰
  , RFFRidgeFit (..)
  , rffRidge
  , predictRFFRidge
    -- * RFF GP (事後 mean + variance)
  , RFFGPFit (..)
  , rffGP
  , predictRFFGP
    -- * 多変量入力 (p 次元) 対応 (Phase B-RFF)
  , RFFFeaturesMV (..)
  , sampleRFFRBFMV
  , sampleRFFMatern52MV
  , rffFeaturesMV
  , RFFRidgeFitMV (..)
  , rffRidgeMV
  , predictRFFRidgeMV
    -- * 周辺尤度最大化 (Phase 2: ℓ, σ_f, σ_n の自動チューニング)
  , logMarginalLikRBFMV
  , maximizeMarginalLikRBFMV
  , MLikResult (..)
    -- * LOOCV 解析解 (Phase 3: HP 自動チューニング高速版)
  , loocvRFFRidgeMV
  , gridSearchLOOCVRBFMV
  , LOOCVResult (..)
  ) where

import Control.Exception (SomeException, try, evaluate)
import qualified Data.Vector as V
import qualified Numeric.LinearAlgebra as LA
import System.IO.Unsafe (unsafePerformIO)
import System.Random.MWC (GenIO, uniformR)
import qualified System.Random.MWC.Distributions as MWCD

-- ---------------------------------------------------------------------------
-- 型
-- ---------------------------------------------------------------------------

-- | 対応カーネル。
data RFFKernel = RFFRBF | RFFMatern52
  deriving (Show, Eq)

-- | RFF の特徴生成に必要な情報。
data RFFFeatures = RFFFeatures
  { rffKernel      :: RFFKernel
  , rffOmegas      :: V.Vector Double   -- ^ D 個のランダム周波数
  , rffBs          :: V.Vector Double   -- ^ D 個の位相 b_j ∈ [0, 2π)
  , rffSigmaF      :: Double            -- ^ 信号 sd σ_f
  , rffLengthScale :: Double            -- ^ 長さスケール ℓ
  } deriving (Show)

rffDim :: RFFFeatures -> Int
rffDim = V.length . rffOmegas

-- ---------------------------------------------------------------------------
-- 周波数サンプリング
-- ---------------------------------------------------------------------------

-- | RBF カーネル用の RFF。ω_j ~ N(0, 1/ℓ²)、b_j ~ U(0, 2π)。
sampleRFFRBF :: Int      -- ^ 特徴次元 D
             -> Double   -- ^ 長さスケール ℓ
             -> Double   -- ^ 信号 sd σ_f
             -> GenIO -> IO RFFFeatures
sampleRFFRBF d ell sf gen = do
  ws <- V.replicateM d (MWCD.normal 0 (1/ell) gen)
  bs <- V.replicateM d (uniformR (0, 2*pi) gen)
  return RFFFeatures
    { rffKernel      = RFFRBF
    , rffOmegas      = ws
    , rffBs          = bs
    , rffSigmaF      = sf
    , rffLengthScale = ell
    }

-- | Matérn 5/2 用の RFF。ω = z/√u where z ~ N(0, 1/ℓ²), u ~ Gamma(ν, ν), ν=5/2。
-- これは df=5 の StudentT 分布をスケーリングしたもの (spectral density に一致)。
sampleRFFMatern52 :: Int -> Double -> Double -> GenIO -> IO RFFFeatures
sampleRFFMatern52 d ell sf gen = do
  let nu = 2.5 :: Double
  ws <- V.replicateM d $ do
    z <- MWCD.normal 0 (1/ell) gen
    -- mwc-random-distributions の gamma は (shape, scale) 渡し → mean = shape * scale
    -- Gamma(ν, 1/ν) で mean = 1
    u <- MWCD.gamma nu (1/nu) gen
    return (z / sqrt u)
  bs <- V.replicateM d (uniformR (0, 2*pi) gen)
  return RFFFeatures
    { rffKernel      = RFFMatern52
    , rffOmegas      = ws
    , rffBs          = bs
    , rffSigmaF      = sf
    , rffLengthScale = ell
    }

-- ---------------------------------------------------------------------------
-- 特徴写像
-- ---------------------------------------------------------------------------

-- | 特徴行列 Φ ∈ ℝ^(n×D)。
-- φ(x) = σ_f √(2/D) [cos(ω_j x + b_j)]_{j=1..D}
rffFeatures :: RFFFeatures -> [Double] -> LA.Matrix Double
rffFeatures rff xs =
  let d  = rffDim rff
      sf = rffSigmaF rff
      coef = sf * sqrt (2 / fromIntegral d)
      ws = rffOmegas rff
      bs = rffBs rff
      cells =
        [ coef * cos (ws V.! j * x + bs V.! j)
        | x <- xs
        , j <- [0 .. d - 1] ]
  in LA.reshape d (LA.fromList cells)

-- | RFF が近似するカーネル行列 K[i,j] ≈ k(x_i, x_j) = φ(x_i)·φ(x_j)。
rffApproxKernel :: RFFFeatures -> [Double] -> LA.Matrix Double
rffApproxKernel rff xs =
  let phi = rffFeatures rff xs
  in phi LA.<> LA.tr phi

-- ---------------------------------------------------------------------------
-- RFF Ridge 回帰
-- ---------------------------------------------------------------------------

data RFFRidgeFit = RFFRidgeFit
  { rffrFeatures :: RFFFeatures
  , rffrWeights  :: LA.Vector Double   -- ^ D 次元重み
  , rffrLambda   :: Double
  } deriving (Show)

-- | RFF Ridge: w = (ΦᵀΦ + λI)⁻¹ Φᵀ y。
-- λ → 0 で通常 Kernel Ridge に近づき、Φ が正確に kernel を再現するなら一致。
rffRidge :: RFFFeatures -> [Double] -> [Double] -> Double -> RFFRidgeFit
rffRidge rff xs ys lam =
  let phi   = rffFeatures rff xs        -- n × D
      d     = rffDim rff
      yV    = LA.fromList ys
      gram  = LA.tr phi LA.<> phi       -- D × D
      regK  = gram + LA.scale lam (LA.ident d)
      rhs   = LA.tr phi LA.#> yV        -- D
      w     = regK LA.<\> rhs
  in RFFRidgeFit rff w lam

predictRFFRidge :: RFFRidgeFit -> [Double] -> [Double]
predictRFFRidge fit xNew =
  let phi  = rffFeatures (rffrFeatures fit) xNew
      yhat = phi LA.#> rffrWeights fit
  in LA.toList yhat

-- ---------------------------------------------------------------------------
-- RFF GP (ベイズ線形回帰 with prior w ~ N(0, I))
-- ---------------------------------------------------------------------------

-- | 事後 N(μ, Σ) を保持。
-- prior w ~ N(0, I) (RFF の振幅は σ_f に乗っているため Σ_p = I が一致する)
-- 尤度: y = φᵀ w + ε, ε ~ N(0, σ_n²)
-- 事後: Σ⁻¹ = ΦᵀΦ / σ_n² + I, μ = Σ Φᵀ y / σ_n²
data RFFGPFit = RFFGPFit
  { rffgpFeatures :: RFFFeatures
  , rffgpSigma    :: LA.Matrix Double   -- ^ 事後共分散 Σ (D × D)
  , rffgpMean     :: LA.Vector Double   -- ^ 事後平均 μ (D)
  , rffgpSigmaN   :: Double             -- ^ 観測ノイズ sd σ_n
  } deriving (Show)

rffGP :: RFFFeatures -> [Double] -> [Double] -> Double -> RFFGPFit
rffGP rff xs ys sigmaN =
  let phi    = rffFeatures rff xs
      d      = rffDim rff
      sigN2  = sigmaN ^ (2 :: Int)
      yV     = LA.fromList ys
      sigInv = LA.scale (1 / sigN2) (LA.tr phi LA.<> phi)
                 `LA.add` LA.ident d
      sigma  = LA.inv sigInv
      mu     = sigma LA.#> LA.scale (1 / sigN2) (LA.tr phi LA.#> yV)
  in RFFGPFit
       { rffgpFeatures = rff
       , rffgpSigma    = sigma
       , rffgpMean     = mu
       , rffgpSigmaN   = sigmaN
       }

-- | 予測点ごとに (mean, variance of f) を返す (観測ノイズ σ_n² は加えない)。
-- mean = φ(x*)ᵀ μ, var = φ(x*)ᵀ Σ φ(x*)
predictRFFGP :: RFFGPFit -> [Double] -> [(Double, Double)]
predictRFFGP fit xNew =
  let rff   = rffgpFeatures fit
      phi   = rffFeatures rff xNew                  -- n_new × D
      mu    = rffgpMean fit
      sigma = rffgpSigma fit
      means = LA.toList (phi LA.#> mu)
      vars  = [ max 0 (LA.dot phi_i (sigma LA.#> phi_i))
              | phi_i <- LA.toRows phi ]
  in zip means vars

-- ---------------------------------------------------------------------------
-- 多変量入力 (p 次元) 対応 (Phase B-RFF)
-- ---------------------------------------------------------------------------

-- | 多変量 RFF の特徴生成情報。'rffmvOmegas' は p×D 行列で、各列が
-- 1 個の周波数ベクトル ω_j ∈ ℝ^p を表す。
data RFFFeaturesMV = RFFFeaturesMV
  { rffmvKernel      :: RFFKernel
  , rffmvDim         :: Int                   -- ^ 入力次元 p
  , rffmvOmegas      :: LA.Matrix Double      -- ^ p × D
  , rffmvBs          :: V.Vector Double       -- ^ D
  , rffmvSigmaF      :: Double
  , rffmvLengthScale :: Double                -- ^ 共通 ℓ (ARD 未対応)
  } deriving (Show)

-- | RBF カーネル用 RFF (多変量)。各 ω_j[k] ~ N(0, 1/ℓ²) 独立。
sampleRFFRBFMV
  :: Int -> Int -> Double -> Double -> GenIO -> IO RFFFeaturesMV
sampleRFFRBFMV p d ell sf gen = do
  let total = p * d
  ws <- V.replicateM total (MWCD.normal 0 (1/ell) gen)
  bs <- V.replicateM d (uniformR (0, 2*pi) gen)
  let omegaMat = LA.reshape d (LA.fromList (V.toList ws))
  return RFFFeaturesMV
    { rffmvKernel      = RFFRBF
    , rffmvDim         = p
    , rffmvOmegas      = omegaMat
    , rffmvBs          = bs
    , rffmvSigmaF      = sf
    , rffmvLengthScale = ell
    }

-- | Matérn 5/2 用 RFF (多変量)。
sampleRFFMatern52MV
  :: Int -> Int -> Double -> Double -> GenIO -> IO RFFFeaturesMV
sampleRFFMatern52MV p d ell sf gen = do
  let nu = 2.5 :: Double
  ws <- V.replicateM (p * d) $ do
    z <- MWCD.normal 0 (1/ell) gen
    u <- MWCD.gamma nu (1/nu) gen
    return (z / sqrt u)
  bs <- V.replicateM d (uniformR (0, 2*pi) gen)
  return RFFFeaturesMV
    { rffmvKernel      = RFFMatern52
    , rffmvDim         = p
    , rffmvOmegas      = LA.reshape d (LA.fromList (V.toList ws))
    , rffmvBs          = bs
    , rffmvSigmaF      = sf
    , rffmvLengthScale = ell
    }

-- | 入力 X (n×p) → Φ (n×D)。
-- φ_j(x) = σ_f √(2/D) cos(ω_j^T x + b_j)
rffFeaturesMV :: RFFFeaturesMV -> LA.Matrix Double -> LA.Matrix Double
rffFeaturesMV rff x =
  let d   = LA.cols (rffmvOmegas rff)
      sf  = rffmvSigmaF rff
      coef = sf * sqrt (2 / fromIntegral d)
      -- X @ Ω → n × D
      xo  = x LA.<> rffmvOmegas rff
      bs  = LA.fromList (V.toList (rffmvBs rff))
      -- broadcast b を各行に加える: 各行に同じ vector を加える
      rows = LA.toRows xo
      withB = LA.fromRows [ r + bs | r <- rows ]
  in LA.scale coef (LA.cmap cos withB)

-- | 多変量 RFF Ridge fit。
data RFFRidgeFitMV = RFFRidgeFitMV
  { rffrmvFeatures :: RFFFeaturesMV
  , rffrmvWeights  :: LA.Vector Double   -- ^ D
  , rffrmvLambda   :: Double
  } deriving (Show)

-- | 多変量 RFF Ridge: w = (ΦᵀΦ + λI)⁻¹ Φᵀ y。X は n×p。
rffRidgeMV :: RFFFeaturesMV -> LA.Matrix Double -> [Double] -> Double
           -> RFFRidgeFitMV
rffRidgeMV rff x ys lam =
  let phi  = rffFeaturesMV rff x         -- n × D
      d    = LA.cols (rffmvOmegas rff)
      yV   = LA.fromList ys
      gram = LA.tr phi LA.<> phi
      regK = gram + LA.scale lam (LA.ident d)
      rhs  = LA.tr phi LA.#> yV
      w    = regK LA.<\> rhs
  in RFFRidgeFitMV rff w lam

predictRFFRidgeMV :: RFFRidgeFitMV -> LA.Matrix Double -> [Double]
predictRFFRidgeMV fit xNew =
  let phi = rffFeaturesMV (rffrmvFeatures fit) xNew
  in LA.toList (phi LA.#> rffrmvWeights fit)

-- ---------------------------------------------------------------------------
-- 周辺尤度最大化 (RFF GP 流の HP チューニング、Phase 2)
-- ---------------------------------------------------------------------------

-- | 多変量入力 X (n×p), 観測 y に対する RBF カーネルの log-marginal-likelihood。
--
--   K_ij = σ_f² · exp(-‖x_i - x_j‖² / (2 ℓ²))
--   y | θ ~ N(0, K + σ_n² I)
--
--   log p(y|θ) = -½ yᵀ (K+σ_n² I)⁻¹ y - ½ log|K+σ_n² I| - n/2 log(2π)
--
-- Cholesky 分解で安定計算。ℓ が極小で K が特異化したら -∞ 近似値を返す。
logMarginalLikRBFMV
  :: LA.Matrix Double      -- ^ X (n × p)
  -> LA.Vector Double      -- ^ y (n)
  -> Double                -- ^ ℓ
  -> Double                -- ^ σ_f
  -> Double                -- ^ σ_n
  -> Double
logMarginalLikRBFMV x y ell sf sn =
  let n     = LA.rows x
      kMat  = rbfKernelMat x ell sf
      cMat  = kMat + LA.scale (sn * sn) (LA.ident n)
      -- Cholesky: cMat = Rᵀ R (R 上三角)。失敗時は jitter を加えて再試行。
      tryChol c =
        let result = unsafePerformIO $ try (evaluate (LA.chol (LA.sym c))) :: Either SomeException (LA.Matrix Double)
        in case result of
             Right r -> Just r
             Left _  -> Nothing
      mR = case tryChol cMat of
             Just r  -> Just r
             Nothing -> tryChol (cMat + LA.scale 1e-6 (LA.ident n))
  in case mR of
       Nothing -> -1e30  -- 特異 → ペナルティ
       Just r  ->
         let logDet  = 2 * sum (map log (LA.toList (LA.takeDiag r)))
             alpha   = cMat LA.<\> y
             dataFit = LA.dot y alpha
         in -0.5 * dataFit - 0.5 * logDet
            - fromIntegral n / 2 * log (2 * pi)

-- | RBF カーネル行列 (X が n×p)。K[i,j] = σ_f² · exp(-‖x_i - x_j‖² / (2ℓ²))
rbfKernelMat :: LA.Matrix Double -> Double -> Double -> LA.Matrix Double
rbfKernelMat x ell sf =
  let n     = LA.rows x
      sf2   = sf * sf
      twol2 = 2 * ell * ell
      rows  = LA.toRows x
  in LA.fromLists
       [ [ sf2 * exp (negate (LA.norm_2 (rows !! i - rows !! j) ^ (2::Int)) / twol2)
         | j <- [0 .. n-1] ]
       | i <- [0 .. n-1] ]

-- | 周辺尤度最大化結果。
data MLikResult = MLikResult
  { mlEll      :: !Double
  , mlSigmaF   :: !Double
  , mlSigmaN   :: !Double
  , mlLogMlik  :: !Double
  , mlGridPts  :: !Int      -- ^ 評価したグリッド点数 (debug 用)
  } deriving (Show)

-- | (ℓ, σ_f, σ_n) のグリッド探索で marg-lik を最大化。
--
-- 戦略:
--
-- 1. ℓ は median pairwise distance を中心に log 等間隔で n_ℓ 点
-- 2. σ_f は std(y) を中心に log で n_σf 点
-- 3. σ_n は std(y)·{0.001..0.5} の log 等間隔で n_σn 点
-- 4. 全 n_ℓ × n_σf × n_σn 点で log-mlik を評価し最良を取る
-- 5. 最良点周辺で 1/3 の幅で同点数のグリッドを再探索 (1 段の coarse-to-fine)
--
-- デフォルトは (20, 8, 8) = 1280 点。最終的に 2560 点 (再探索込)。
-- n=200 までは数秒。
maximizeMarginalLikRBFMV
  :: LA.Matrix Double
  -> LA.Vector Double
  -> Maybe (Int, Int, Int)         -- ^ (n_ℓ, n_σf, n_σn). Default (20,8,8)
  -> MLikResult
maximizeMarginalLikRBFMV x y mGrid =
  let (nL, nSF, nSN) = case mGrid of
        Just g  -> g
        Nothing -> (20, 8, 8)
      yStd  = sampleStd (LA.toList y)
      ellM  = max 1e-3 (medianPairwiseDist x)
      sfM   = max 1e-6 yStd
      -- Stage 1: 広めグリッド
      ellGrid1 = logSpace (ellM * 0.05) (ellM * 20)   nL
      sfGrid1  = logSpace (sfM  * 0.25) (sfM  * 4)    nSF
      snGrid1  = logSpace (yStd * 1e-3) (yStd * 0.5)  nSN
      stage1   = bestOver x y ellGrid1 sfGrid1 snGrid1
      -- Stage 2: 最良点周辺で 1/3 幅
      (ell1, sf1, sn1, _) = stage1
      ellGrid2 = logSpace (ell1 / 3) (ell1 * 3) nL
      sfGrid2  = logSpace (sf1  / 2) (sf1  * 2) nSF
      snGrid2  = logSpace (sn1  / 3) (sn1  * 3) nSN
      stage2   = bestOver x y ellGrid2 sfGrid2 snGrid2
      (ell2, sf2, sn2, ml2) = stage2
  in MLikResult ell2 sf2 sn2 ml2
       (nL * nSF * nSN * 2)

-- | 与えられた (ellGrid, sfGrid, snGrid) 全組合せで log-mlik 最良を返す。
bestOver
  :: LA.Matrix Double -> LA.Vector Double
  -> [Double] -> [Double] -> [Double]
  -> (Double, Double, Double, Double)
bestOver x y ells sfs sns =
  let evaluations =
        [ (ell, sf, sn, logMarginalLikRBFMV x y ell sf sn)
        | ell <- ells, sf <- sfs, sn <- sns ]
      best = foldr1 (\a@(_,_,_,la) b@(_,_,_,lb) ->
                       if la >= lb then a else b) evaluations
  in best

-- | log 等間隔な n 点。
logSpace :: Double -> Double -> Int -> [Double]
logSpace lo hi n
  | n <= 1    = [lo]
  | lo <= 0   = logSpace 1e-9 hi n  -- 安全フォールバック
  | otherwise =
      let lLo = log lo
          lHi = log hi
          step = (lHi - lLo) / fromIntegral (n - 1)
      in [ exp (lLo + fromIntegral i * step) | i <- [0 .. n - 1] ]

-- | 行ペアの median pairwise distance (median heuristic for RBF ℓ)。
medianPairwiseDist :: LA.Matrix Double -> Double
medianPairwiseDist x =
  let rows = LA.toRows x
      pairs = [ LA.norm_2 (rows !! i - rows !! j)
              | i <- [0 .. length rows - 1]
              , j <- [i+1 .. length rows - 1] ]
  in case pairs of
       [] -> 1.0
       _  ->
         let sorted = LA.toList (LA.fromList pairs)  -- to immutable
             sorted2 = qSort sorted
             k       = length sorted2 `div` 2
         in if null sorted2 then 1.0 else sorted2 !! k

qSort :: Ord a => [a] -> [a]
qSort []     = []
qSort (p:xs) = qSort [x | x <- xs, x <= p] ++ [p] ++ qSort [x | x <- xs, x > p]

sampleStd :: [Double] -> Double
sampleStd xs
  | length xs <= 1 = 1.0
  | otherwise =
      let n = fromIntegral (length xs)
          m = sum xs / n
          v = sum [ (x - m) * (x - m) | x <- xs ] / (n - 1)
      in if v <= 0 then 1.0 else sqrt v


-- ---------------------------------------------------------------------------
-- LOOCV 解析解 (Phase 3 — Ridge の closed-form leave-one-out cross-validation)
-- ---------------------------------------------------------------------------

-- | LOOCV 探索結果。
data LOOCVResult = LOOCVResult
  { lcEll      :: !Double
  , lcSigmaF   :: !Double   -- ^ 信号 sd (= std(y) を使う簡易版)
  , lcLambda   :: !Double   -- ^ Ridge 正則化
  , lcLOOCV    :: !Double   -- ^ LOOCV(λ) = mean square LOO residual
  , lcGridPts  :: !Int
  } deriving (Show)

-- | RFF Ridge の LOOCV を Cholesky と「ハット行列の対角」を使って解析的に計算する。
--
--   H = Φ (ΦᵀΦ + λI)⁻¹ Φᵀ
--   ŷ = H y
--   LOOCV(λ) = (1/n) Σᵢ ((y_i - ŷ_i) / (1 - H_ii))²
--
-- 本関数は与えられた特徴行列 'feats' (= 既に ω/b/σ_f が決まったもの) と
-- Ridge λ に対して LOOCV を返す。グリッドサーチ側ではこれを多数の λ で
-- 呼び出すが、Φ は 1 度だけ計算すれば良いので外側でキャッシュする。
loocvRFFRidgeMV
  :: RFFFeaturesMV
  -> LA.Matrix Double           -- ^ X (n × p)
  -> LA.Vector Double           -- ^ y (n)
  -> Double                     -- ^ λ
  -> Double
loocvRFFRidgeMV feats x y lam =
  let phi = rffFeaturesMV feats x      -- n × D
  in loocvFromPhi phi y lam

-- | Φ から LOOCV を計算する内部実装 (グリッドサーチでキャッシュ用)。
-- Cholesky ベース (Φ_ridge = Φᵀ Φ + λI、A = chol(Φ_ridge))。
--   H = Φ Φ_ridge⁻¹ Φᵀ
--   T = Φ Φ_ridge⁻¹  → diag(H) = row-sum(T ⊙ Φ)
loocvFromPhi :: LA.Matrix Double -> LA.Vector Double -> Double -> Double
loocvFromPhi phi y lam =
  let n     = LA.rows phi
      d     = LA.cols phi
      gram  = LA.tr phi LA.<> phi             -- D × D
      regK  = gram + LA.scale lam (LA.ident d)
      -- 解析解: w = regK⁻¹ Φᵀ y
      w     = regK LA.<\> (LA.tr phi LA.#> y)
      yhat  = phi LA.#> w
      -- diag(H) = diag(Φ M Φᵀ) where M = regK⁻¹
      -- T = Φ M  (n × D)。Φ M Φᵀ の対角 = row(T) · row(Φ)
      tMat  = LA.tr (regK LA.<\> LA.tr phi)   -- T = Φ M、n × D
      hDiag = LA.fromList
                [ LA.dot (LA.flatten (tMat LA.? [i]))
                         (LA.flatten (phi  LA.? [i]))
                | i <- [0 .. n - 1] ]
      -- 1 - H_ii の極小ガード
      oneMinusH = LA.cmap (\h -> max 1e-12 (1 - h)) hDiag
      resid     = y - yhat
      ratios    = LA.toList resid `divList` LA.toList oneMinusH
      sse       = sum [ r * r | r <- ratios ]
  in sse / fromIntegral (max 1 n)
  where
    divList xs ys = zipWith (/) xs ys

-- | (ℓ, λ) を log-spaced グリッドで探索し、LOOCV 最小を見つける。
--
-- ℓ ごとに ω を新規サンプリングするため IO。グリッドサイズ default (8, 20):
-- ℓ 8 点 × λ 20 点 = 160 fit。各 fit O(n D + D³) で n=545, D=200 程度なら
-- 全体で数秒程度。
--
-- σ_f は std(y) 固定 (Ridge ↔ GP 等価では σ_f は ω 分散と一緒に動くべきだが、
-- λ で吸収できるので簡易化)。
gridSearchLOOCVRBFMV
  :: Int                               -- ^ p (入力次元)
  -> Int                               -- ^ D (特徴次元)
  -> LA.Matrix Double                  -- ^ X
  -> LA.Vector Double                  -- ^ y
  -> Maybe (Int, Int)                  -- ^ (n_ℓ, n_λ) default (8, 20)
  -> GenIO
  -> IO LOOCVResult
gridSearchLOOCVRBFMV p d x y mGrid gen = do
  let (nL, nLam) = case mGrid of { Just g -> g; Nothing -> (8, 20) }
      yStd  = sampleStd (LA.toList y)
      sf    = max 1e-9 yStd
      ellM  = max 1e-3 (medianPairwiseDist x)
      ellGrid = logSpace (ellM * 0.05) (ellM * 20)  nL
      lamGrid = logSpace (yStd * 1e-6) (yStd * 10)  nLam
  -- 各 ℓ について 1 度サンプリングしてから λ ループ
  evals <- mapM (\ell -> do
                   feats <- sampleRFFRBFMV p d ell sf gen
                   let phi = rffFeaturesMV feats x
                   let scoresAtLam = [ (ell, sf, lam, loocvFromPhi phi y lam)
                                     | lam <- lamGrid ]
                   return scoresAtLam)
                ellGrid
  let evaluations = concat evals
      best = foldr1 (\a@(_,_,_,la) b@(_,_,_,lb) ->
                       if la <= lb then a else b) evaluations
      (bEll, bSf, bLam, bL) = best
  return LOOCVResult
    { lcEll = bEll
    , lcSigmaF = bSf
    , lcLambda = bLam
    , lcLOOCV  = bL
    , lcGridPts = nL * nLam
    }
