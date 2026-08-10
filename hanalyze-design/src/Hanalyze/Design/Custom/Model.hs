{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hanalyze.Design.Custom.Model
-- Description : Custom Design の Model 定義と設計行列展開 (項 ADT → treatment coding)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Custom Design の Model 定義 + 設計行列展開 (Phase 24-2)。
--
-- spec: doe-custom-design-spec v0.1.1 §2.2 / §3.1。
--
-- ## raw matrix の Categorical 表現規約 (重要、 型安全ではない)
--
-- `expandDesignMatrix` の入力 `Matrix Double` における Categorical / Ordinal
-- 因子の列は **level index 0..K-1 を Double で保持** する。
-- expandDesignMatrix は reference (treatment) coding で K-1 列に展開、
-- 参照水準 = index 0。
--
-- `Matrix Double` は連続値も index も同じ型なので、 0.5 のような小数や
-- 範囲外 index を **型では防げない**。 検出は runtime check (`Left Text`)。
-- 王道再設計 (R `model.matrix` / patsy 流の型分離) は phase-plan の
-- Phase 27 候補に登録済。 詳細は specification/phases/phase-24-custom-design-core.md。
--
-- ## 未対応 (Phase 24 v0.2 候補)
--
--   * `mNorm` は ADT として持つが現状 'NCoded' は identity、 'NUnit' / 'NRaw' は
--     呼び出し側で適切な値を渡す前提
--   * `TNested` / `TCustom` (`Left` を返す)
--   * `TPower` を Categorical 因子に適用するのは無意味 (indicator^k = indicator)
--     なので `Left`
module Hanalyze.Design.Custom.Model
  ( ParamNormalize (..)
  , ModelTerm (..)
  , Model (..)
  , expandDesignMatrix
  , modelNumColumns
  ) where

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.List (elemIndex)
import qualified Numeric.LinearAlgebra as LA

import           Hanalyze.Design.Custom.Factor

-- | 因子値の正規化方針。
data ParamNormalize
  = NCoded   -- ^ coded units (連続因子は @[-1, 1]@ に既に変換済前提)
  | NUnit    -- ^ unit cube (@[0, 1]@) 想定
  | NRaw     -- ^ raw 単位 (= 何も変換しない)
  deriving (Eq, Show)

-- | モデル項。
data ModelTerm
  = TIntercept                     -- ^ 切片 (全 1 列)
  | TMain   !Text                  -- ^ 主効果 (因子名)
  | TInter  ![Text]                -- ^ 交互作用 (k 因子)
  | TPower  !Text !Int             -- ^ @x^k@ (k ≥ 2 を想定、 連続因子のみ)
  | TNested !Text !Text            -- ^ @A within B@ (未対応)
  deriving (Eq, Show)

-- | モデル = 項リスト + 正規化方針。
data Model = Model
  { mTerms :: ![ModelTerm]
  , mNorm  :: !ParamNormalize
  } deriving (Eq, Show)

-- | モデル全体が設計行列に占める列数 (Categorical 因子の K-1 展開を考慮)。
-- Categorical 因子参照中の TMain / TInter / TPower は factorDimension を使う。
modelNumColumns :: [Factor] -> Model -> Int
modelNumColumns factors m = sum (map termWidth (mTerms m))
  where
    findF n = lookup n [(fName f, f) | f <- factors]
    dim n   = maybe 1 factorDimension (findF n)
    termWidth t = case t of
      TIntercept    -> 1
      TMain n       -> dim n
      TInter ns     -> product (map dim ns)
      TPower _ _    -> 1
      TNested a b   -> levelsOf b * dim a   -- Phase 28-1: K_B × (K_A - 1) cols
    levelsOf n = case lookup n [(fName f, f) | f <- factors] of
      Just f -> case fKind f of
        Categorical xs -> length xs
        Ordinal     xs -> length xs
        _              -> 0
      Nothing -> 0

-- | 因子の raw 値行列 (n × p_factors) からモデル設計行列 (n × p_terms) を展開。
--
-- 入力 @raw@ の列順は @factors@ の順序と一致する前提。
-- Categorical / Ordinal 因子の列は **level index 0..K-1 を Double で保持**
-- する規約 (上記モジュール doc 参照)。
--
-- 失敗を返すケース:
--   * @TNested@ を含む
--   * 参照される因子名が見つからない
--   * Categorical の raw 値が非整数 / 範囲外
--   * @TPower@ を Categorical 因子に適用
--   * 因子行列の列数が @factors@ の長さと一致しない
expandDesignMatrix
  :: [Factor]
  -> Model
  -> LA.Matrix Double            -- ^ 因子 raw 値 (n × p_factors)
  -> Either Text (LA.Matrix Double)
expandDesignMatrix factors model raw
  | LA.cols raw /= length factors =
      Left (T.pack "expandDesignMatrix: raw column count ≠ #factors")
  | otherwise = do
      colss <- mapM (termColumns factors raw) (mTerms model)
      pure (LA.fromColumns (concat colss))

-- | 単一項を 0 個以上の列に変換 (Categorical の TMain は K-1 列、
-- Categorical × Categorical の TInter はクロス積で (K1-1)(K2-1) 列等)。
termColumns
  :: [Factor]
  -> LA.Matrix Double
  -> ModelTerm
  -> Either Text [LA.Vector Double]
termColumns _ raw TIntercept =
  Right [LA.fromList (replicate (LA.rows raw) 1.0)]
termColumns factors raw (TMain name) =
  factorColumns factors raw name
termColumns factors raw (TInter names)
  | null names = Left (T.pack "TInter with no factor names is invalid")
  | otherwise = do
      colGroups <- mapM (factorColumns factors raw) names
      -- 各因子の列群を cartesian product で elementwise 積。
      Right (foldr1 crossMultiply colGroups)
termColumns factors raw (TPower name k)
  | k < 2     = Left (T.pack ("TPower: k must be >= 2 (got " <> show k <> ")"))
  | otherwise = do
      f <- findFactor factors name
      if factorIsContinuous f
        then do
          v <- numericFactorVector factors raw name
          Right [LA.cmap (** fromIntegral k) v]
        else Left (T.pack
               ("TPower on categorical/ordinal factor " <> T.unpack name
                <> " is degenerate (indicator^k = indicator)"))
termColumns factors raw (TNested aName bName) = do
  (aIdx, fA) <- findFactorWithIndex factors aName
  (bIdx, fB) <- findFactorWithIndex factors bName
  let kindCat fk = case fk of
        Categorical xs -> Just xs
        Ordinal     xs -> Just xs
        _              -> Nothing
  case (kindCat (fKind fA), kindCat (fKind fB)) of
    (Just aXs, Just bXs) -> do
      let aCol = LA.flatten (LA.subMatrix (0, aIdx) (LA.rows raw, 1) raw)
          bCol = LA.flatten (LA.subMatrix (0, bIdx) (LA.rows raw, 1) raw)
      aIxs <- traverse (validateLevelIndex aName (length aXs)) (LA.toList aCol)
      bIxs <- traverse (validateLevelIndex bName (length bXs)) (LA.toList bCol)
      let kB = length bXs
          kA = length aXs
          n  = LA.rows raw
          mkCol bLvl aLvl = LA.fromList
            [ if (bIxs !! i) == bLvl && (aIxs !! i) == aLvl then 1.0 else 0.0
            | i <- [0 .. n - 1] ]
      -- 列順: outer = B level (0..K_B-1)、 inner = A level (1..K_A-1) (treatment coding)
      Right [ mkCol b a | b <- [0 .. kB - 1], a <- [1 .. kA - 1] ]
    _ ->
      Left (T.pack
        ("TNested " <> T.unpack aName <> " within " <> T.unpack bName
         <> ": both factors must be Categorical/Ordinal (Phase 28-1 制限)"))

-- | 2 つの列群を elementwise 積で cartesian-product 化。
-- 結果列数 = length xs * length ys。
crossMultiply :: [LA.Vector Double] -> [LA.Vector Double] -> [LA.Vector Double]
crossMultiply xs ys = [x * y | x <- xs, y <- ys]
  -- Vector の Num instance は elementwise

-- | 因子名 → 設計行列に挿入する列群。
-- 連続系: 単一列 (raw そのまま)。
-- Categorical / Ordinal: treatment coding で K-1 列 (reference = index 0)。
factorColumns
  :: [Factor]
  -> LA.Matrix Double
  -> Text
  -> Either Text [LA.Vector Double]
factorColumns factors raw name = do
  (i, f) <- findFactorWithIndex factors name
  let col = LA.flatten (LA.subMatrix (0, i) (LA.rows raw, 1) raw)
  case fKind f of
    Continuous  _ _ -> Right [col]
    DiscreteNum _   -> Right [col]
    Mixture     _ _ -> Right [col]
    Categorical xs  -> treatmentCoding name (length xs) col
    Ordinal     xs  -> treatmentCoding name (length xs) col

-- | reference (treatment) coding。 K 水準なら K-1 列、 reference = index 0。
-- 列 k (1-based: 1..K-1) の値 = 1 if raw == k else 0。
treatmentCoding
  :: Text                           -- ^ 因子名 (エラーメッセージ用)
  -> Int                            -- ^ 水準数 K
  -> LA.Vector Double               -- ^ raw 列 (level index を Double で)
  -> Either Text [LA.Vector Double]
treatmentCoding name k col
  | k <= 0 = Left (T.pack
               ("factor " <> T.unpack name <> ": categorical with 0 levels"))
  | k == 1 = Right []  -- 1 水準は constant、 列なし
  | otherwise = do
      idxs <- traverse (validateLevelIndex name k) (LA.toList col)
      let mkCol lvl = LA.fromList
            [ if i == lvl then 1.0 else 0.0 | i <- idxs ]
      Right [mkCol lvl | lvl <- [1 .. k - 1]]

-- | level index validation: 整数値かつ [0, K-1] 範囲内。
validateLevelIndex :: Text -> Int -> Double -> Either Text Int
validateLevelIndex name k x =
  let xi = round x :: Int
      delta = abs (x - fromIntegral xi)
  in if delta > 1e-9
       then Left (T.pack
              ("factor " <> T.unpack name
               <> ": categorical raw value " <> show x
               <> " is not an integer level index"))
       else if xi < 0 || xi >= k
              then Left (T.pack
                     ("factor " <> T.unpack name
                      <> ": level index " <> show xi
                      <> " out of range [0," <> show (k - 1) <> "]"))
              else Right xi

-- | 連続因子の生の列 (TPower 用に分離した helper)。
numericFactorVector
  :: [Factor]
  -> LA.Matrix Double
  -> Text
  -> Either Text (LA.Vector Double)
numericFactorVector factors raw name = do
  (i, _) <- findFactorWithIndex factors name
  Right (LA.flatten (LA.subMatrix (0, i) (LA.rows raw, 1) raw))

findFactor :: [Factor] -> Text -> Either Text Factor
findFactor factors name = snd <$> findFactorWithIndex factors name

findFactorWithIndex :: [Factor] -> Text -> Either Text (Int, Factor)
findFactorWithIndex factors name =
  case elemIndex name (map fName factors) of
    Nothing -> Left (T.pack ("factor not found: " <> T.unpack name))
    Just i  -> Right (i, factors !! i)
