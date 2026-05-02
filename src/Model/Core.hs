{-# LANGUAGE OverloadedStrings #-}
-- | 全回帰モデルが共有する結果型と Model クラス。
--
-- **Phase R0 案 C**: 多出力対応のため `FitResult` の主フィールドを
-- `Matrix Double` (n × q) または `Vector Double` (q 次元) に一般化した。
-- 単一出力 (q=1) の場合は便利アクセサ ('coefficientsV', 'fittedV',
-- 'residualsV', 'rSquared1') で従来通り `Vector` / `Double` として扱える。
--
-- 単一出力モデルから多出力モデルへの移行は、`fitLM` (Matrix×Matrix) を
-- 直接使い、結果を `MultiFitResult` 風に解釈するだけで済む。
module Model.Core
  ( FitResult (..)
  , Model (..)
  , Band (..)
    -- * Vec / Scalar 便利アクセサ (q = 1 用)
  , coefficientsV
  , fittedV
  , residualsV
  , rSquared1
    -- * リスト変換
  , fittedList
  , coeffList
    -- * 列単位アクセス
  , coefficientsCol
  , fittedCol
  , residualsCol
  ) where

import qualified Numeric.LinearAlgebra as LA

-- | 全フィット結果の共通型 (多出力対応)。
--
-- 形状:
--
-- * 'coefficients' :: p × q  (p 列の特徴量、q 列の応答)
-- * 'fitted'       :: n × q  (n 観測、q 応答)
-- * 'residuals'    :: n × q
-- * 'rSquared'     :: q 次元 (応答ごとの R²)
--
-- 単一出力の場合は q = 1 (1 列の Matrix) になる。
data FitResult = FitResult
  { coefficients :: LA.Matrix Double
  , fitted       :: LA.Matrix Double
  , residuals    :: LA.Matrix Double
  , rSquared     :: LA.Vector Double
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- Vec / Scalar アクセサ (q = 1 用)
-- ---------------------------------------------------------------------------

-- | 1 出力のフィット結果から係数を Vector として取り出す。
-- 多出力の場合は 1 列目のみ。多出力で全列が必要なら 'coefficients' を直接使う。
coefficientsV :: FitResult -> LA.Vector Double
coefficientsV = LA.flatten . coefficients

-- | 1 出力のフィット結果から ŷ を Vector として取り出す。
fittedV :: FitResult -> LA.Vector Double
fittedV = LA.flatten . fitted

-- | 1 出力のフィット結果から残差を Vector として取り出す。
residualsV :: FitResult -> LA.Vector Double
residualsV = LA.flatten . residuals

-- | 1 出力のフィット結果から R² (Double) を取り出す。
-- 多出力では 1 列目のみ; 全応答の R² が必要なら 'rSquared' を直接使う。
rSquared1 :: FitResult -> Double
rSquared1 r = case LA.toList (rSquared r) of
  (h : _) -> h
  []      -> 0

-- ---------------------------------------------------------------------------
-- 後方互換ヘルパ (旧 Vec API 利用者用)
-- ---------------------------------------------------------------------------

-- | ŷ を [Double] に変換 (1 出力前提)。
fittedList :: FitResult -> [Double]
fittedList = LA.toList . fittedV

-- | 係数を [Double] に変換 (1 出力前提)。
coeffList :: FitResult -> [Double]
coeffList = LA.toList . coefficientsV

-- ---------------------------------------------------------------------------
-- 列単位アクセス (多出力時)
-- ---------------------------------------------------------------------------

-- | j 列目 (応答 j) の係数を Vector で取り出す。
coefficientsCol :: Int -> FitResult -> LA.Vector Double
coefficientsCol j r = LA.flatten (coefficients r LA.¿ [j])

-- | j 列目 (応答 j) の予測 ŷ を Vector で取り出す。
fittedCol :: Int -> FitResult -> LA.Vector Double
fittedCol j r = LA.flatten (fitted r LA.¿ [j])

-- | j 列目 (応答 j) の残差を Vector で取り出す。
residualsCol :: Int -> FitResult -> LA.Vector Double
residualsCol j r = LA.flatten (residuals r LA.¿ [j])

-- ---------------------------------------------------------------------------
-- 不確実性帯
-- ---------------------------------------------------------------------------

-- | 平均応答に描く不確実性帯。
data Band
  = NoBand      -- ^ 無し
  | CI Double   -- ^ 信頼区間
  | PI Double   -- ^ 予測区間 (Gaussian のみ)
  deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Model クラス (多出力に対応)
-- ---------------------------------------------------------------------------

-- | 各回帰モデルが満たすインタフェース。
--
-- @
-- fit     m X Y        :: FitResult       -- X (n×p), Y (n×q)
-- predict m beta Xnew  :: Matrix          -- ŷ (m × q) where m = rows Xnew
-- @
class Model m where
  fit     :: m -> LA.Matrix Double -> LA.Matrix Double -> FitResult
  predict :: m -> LA.Matrix Double -> LA.Matrix Double -> LA.Matrix Double
  --             coefficients (p × q)  Xnew (m × p)      ŷ (m × q)
