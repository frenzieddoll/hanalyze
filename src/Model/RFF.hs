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
  ) where

import qualified Data.Vector as V
import qualified Numeric.LinearAlgebra as LA
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
