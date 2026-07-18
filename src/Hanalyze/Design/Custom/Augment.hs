{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns      #-}
-- |
-- Module      : Hanalyze.Design.Custom.Augment
-- Description : Custom Design の Augment 5 メニュー (Replicate/AddCenter/AddAxial/AddRuns/Foldover)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Custom Design の Augment 5 メニュー (Phase 25-6/7/8)。
--
-- spec: doe-custom-design-spec v0.1.1 §2.6 / §3。
-- 参考: JMP "Augment Design" platform。
--
-- ## 5 メニュー
--
--   * 'Replicate n'   : 既存 design を n 回複製
--   * 'AddCenter  n'  : 中心点 (全連続因子 = 0、 categorical は ref level) を n 行追加
--   * 'AddAxial   α'  : 1 因子だけを ±α、 他を 0 にした axial 点を全連続因子で追加
--                       (= 2 * #continuous-factors 行)
--   * 'AddRuns    n'  : 既存 augmentDesign (古典 Fedorov 交換) で N 行追加
--   * 'Foldover   k'  : 既存 design の sign-flipped 行を全部追加 (Full)、
--                       または指定因子のみ flip (Partial)
--
-- ## 制限 (Phase 25-8 暫定)
--
--   * 'cdsInitial' が 'Nothing' の場合は 'Left' (既存 design 必須)
--   * AddCenter / AddAxial は連続因子のみ。 categorical 列は ref index 0 を使う
--   * Foldover は 2 水準連続因子のみ正しく動作。 categorical はそのまま (flip しない)
--   * AddAxial は coded space ([-1, 1]) 想定、 raw range を考慮しない
module Hanalyze.Design.Custom.Augment
  ( AugmentMenu (..)
  , FoldoverKind (..)
  , AugmentMenuResult (..)
  , augmentMenu
  ) where

import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Numeric.LinearAlgebra    as LA

import           Hanalyze.Design.Custom.Factor
import           Hanalyze.Design.Custom.Coordinate
                   (CustomDesignSpec (..))
import qualified Hanalyze.Design.Optimal  as Opt

-- ---------------------------------------------------------------------------
-- 型
-- ---------------------------------------------------------------------------

data AugmentMenu
  = Replicate !Int
  | AddCenter !Int
  | AddAxial  !Double !Bool
    -- ^ axial 点。 第 2 引数 @rawUnits@ が False のとき (= 既定) は coded
    -- @[-1, 1]@ 空間で center 0 + ±α (NCoded モデル想定)。 True のとき raw
    -- 単位で center (lo+hi)/2 ± α·(hi-lo)/2 (Phase 28-10 で追加)。 raw 形式の
    -- 既存設計に直接 ±α coded 相当の axial 点を入れたいケースに使う
  | AddRuns   !Int
  | Foldover  !FoldoverKind
  deriving (Show, Eq)

data FoldoverKind
  = FullFoldover
  | PartialFoldover ![Text]  -- ^ flip する因子名のリスト
  | CategoricalSwap ![(Text, [(Text, Text)])]
    -- ^ Phase 28-7: categorical 因子の level swap mapping。 各エントリ
    -- @(factor_name, [(old_level, new_level), ...])@ に対し、 既存設計の
    -- 該当列の level を mapping で置換した行を追加。 連続因子の符号 flip は
    -- 行わない (CategoricalSwap は categorical 専用)。 mapping に現れない
    -- level はそのまま (自分自身に map)
  deriving (Show, Eq)

data AugmentMenuResult = AugmentMenuResult
  { amrMatrix :: !(LA.Matrix Double)
    -- ^ 増補後の design (existing + added)
  , amrAdded  :: !Int
    -- ^ 追加された行数
  , amrMethod :: !Text
    -- ^ "Replicate" / "AddCenter" / etc.
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- 公開 API
-- ---------------------------------------------------------------------------

augmentMenu :: CustomDesignSpec -> AugmentMenu -> IO (Either Text AugmentMenuResult)
augmentMenu spec menu =
  case cdsInitial spec of
    Nothing -> pure (Left (T.pack "augmentMenu: cdsInitial is required"))
    Just existing ->
      case menu of
        Replicate k    -> pure (augmentReplicate existing k)
        AddCenter k    -> pure (augmentAddCenter (cdsFactors spec) existing k)
        AddAxial alpha rawUnits ->
          pure (augmentAddAxial (cdsFactors spec) existing alpha rawUnits)
        AddRuns k      -> pure (augmentAddRuns spec existing k)
        Foldover kind  -> pure (augmentFoldover (cdsFactors spec) existing kind)

-- ---------------------------------------------------------------------------
-- Replicate
-- ---------------------------------------------------------------------------

augmentReplicate :: LA.Matrix Double -> Int -> Either Text AugmentMenuResult
augmentReplicate existing k
  | k < 1 = Left (T.pack "Replicate: k must be >= 1")
  | otherwise =
      let !rows = LA.toRows existing
          !reps = concat (replicate k rows)
          !added = LA.fromRows reps
          !full = LA.fromRows (rows ++ reps)
      in Right AugmentMenuResult
           { amrMatrix = full
           , amrAdded  = LA.rows added
           , amrMethod = T.pack "Replicate"
           }

-- ---------------------------------------------------------------------------
-- AddCenter
-- ---------------------------------------------------------------------------

-- | 中心点: 連続因子は 0、 categorical は level index 0 (= reference)。
augmentAddCenter
  :: [Factor]
  -> LA.Matrix Double
  -> Int
  -> Either Text AugmentMenuResult
augmentAddCenter factors existing k
  | k < 1 = Left (T.pack "AddCenter: k must be >= 1")
  | LA.cols existing /= length factors =
      Left (T.pack "AddCenter: existing column count ≠ #factors")
  | otherwise =
      let !centerRow = LA.fromList (map factorCenter factors)
          !added = LA.fromRows (replicate k centerRow)
          !full  = existing LA.=== added
      in Right AugmentMenuResult
           { amrMatrix = full
           , amrAdded  = k
           , amrMethod = T.pack "AddCenter"
           }

factorCenter :: Factor -> Double
factorCenter f = case fKind f of
  Continuous  _ _ -> 0
  DiscreteNum xs  -> case xs of
                       []      -> 0
                       (h:_)   -> sum xs / fromIntegral (length xs)
                         where _ = h
  Mixture lo hi   -> (lo + hi) / 2
  Categorical _   -> 0
  Ordinal     _   -> 0

-- ---------------------------------------------------------------------------
-- AddAxial
-- ---------------------------------------------------------------------------

-- | axial / star 点: 各連続因子について、 その因子だけを +α / -α、
-- 他を 0 (中心) にした 2 点ずつを追加。
--
-- @rawUnits@ False: coded 空間 (center 0 + ±α) で生成。 NCoded モデルで
--   raw 行列が既に coded されている前提。
-- @rawUnits@ True: 因子の (lo, hi) を使って center (lo+hi)/2 + ±α·(hi-lo)/2
--   で生成 (raw 単位、 coded ±α 相当の位置)。 Continuous / DiscreteNum /
--   Mixture でそれぞれの range を解釈する。
augmentAddAxial
  :: [Factor]
  -> LA.Matrix Double
  -> Double
  -> Bool
  -> Either Text AugmentMenuResult
augmentAddAxial factors existing alpha rawUnits
  | alpha <= 0 = Left (T.pack "AddAxial: alpha must be > 0")
  | LA.cols existing /= length factors =
      Left (T.pack "AddAxial: existing column count ≠ #factors")
  | otherwise =
      let !contIxs =
            [ i | (i, f) <- zip [0 ..] factors, factorIsContinuous f ]
      in if null contIxs
           then Left (T.pack "AddAxial: no continuous factors to augment")
           else
             let !centers = if rawUnits
                              then map factorCenterRaw factors
                              else map factorCenter factors
                 axialOffset i = if rawUnits
                                   then alpha * factorHalfRange (factors !! i)
                                   else alpha
                 !rows =
                   [ LA.fromList
                       [ if j == i then (centers !! j) + sgn * axialOffset i
                                   else centers !! j
                       | j <- [0 .. length factors - 1]
                       ]
                   | i <- contIxs, sgn <- [1, -1]
                   ]
                 !added = LA.fromRows rows
                 !full  = existing LA.=== added
             in Right AugmentMenuResult
                  { amrMatrix = full
                  , amrAdded  = length rows
                  , amrMethod = T.pack "AddAxial"
                  }

-- | 因子の半幅 = (hi - lo) / 2 (Continuous / DiscreteNum / Mixture)。
-- raw 単位 axial の scale factor として使う。 Categorical / Ordinal は意味を
-- 持たないため 0 を返す (caller 側 contIxs で除外済の想定)。
factorHalfRange :: Factor -> Double
factorHalfRange f = case fKind f of
  Continuous  lo hi -> (hi - lo) / 2
  DiscreteNum xs    -> case xs of
                         [] -> 0
                         _  -> (maximum xs - minimum xs) / 2
  Mixture     lo hi -> (hi - lo) / 2
  _                 -> 0

-- | 因子の raw 単位での中心 = (lo + hi) / 2 (Phase 28-10 AddAxial rawUnits=True 用)。
-- 'factorCenter' は coded 空間想定 (Continuous → 0)、 これは raw 空間。
factorCenterRaw :: Factor -> Double
factorCenterRaw f = case fKind f of
  Continuous  lo hi -> (lo + hi) / 2
  DiscreteNum xs    -> case xs of
                         [] -> 0
                         _  -> (maximum xs + minimum xs) / 2
  Mixture     lo hi -> (lo + hi) / 2
  _                 -> 0

-- ---------------------------------------------------------------------------
-- AddRuns (既存 augmentDesign を wrap)
-- ---------------------------------------------------------------------------

-- | AddRuns: 既存 'Hanalyze.Design.Optimal.augmentDesign' を使い、
-- 候補集合は連続因子は ±1 grid、 categorical は全 level の cartesian product。
-- 候補集合サイズが大きくなりすぎる場合 (例 2^20 等) は呼び出し側で nRuns を
-- 抑制すること。
augmentAddRuns
  :: CustomDesignSpec
  -> LA.Matrix Double
  -> Int
  -> Either Text AugmentMenuResult
augmentAddRuns spec existing k
  | k < 1 = Left (T.pack "AddRuns: k must be >= 1")
  | otherwise =
      let factors  = cdsFactors spec
          cands    = candidateRows factors
          existRow = LA.toLists existing
          seed     = case cdsSeed spec of Just s -> s; Nothing -> 0
          arRes    = Opt.augmentDesign (cdsCriterion spec) existRow k cands seed
      in if length (Opt.arNewRows arRes) /= k
           then Left (T.pack
             ("AddRuns: failed to add " <> show k <> " rows (candidates may be too few)"))
           else
             let !added = LA.fromLists (Opt.arNewRows arRes)
                 !full  = existing LA.=== added
             in Right AugmentMenuResult
                  { amrMatrix = full
                  , amrAdded  = k
                  , amrMethod = T.pack "AddRuns"
                  }

-- | 候補集合: 連続因子は ±1、 categorical / ordinal は全 level、 DiscreteNum は xs、
-- Mixture は [lo, hi] の 2 点 とする (簡略化、 Phase 25-7 で grid 拡張可)。
candidateRows :: [Factor] -> [[Double]]
candidateRows = cart . map factorCandidates
  where
    factorCandidates f = case fKind f of
      Continuous _ _    -> [-1, 1]
      DiscreteNum xs    -> xs
      Mixture lo hi     -> [lo, hi]
      Categorical xs    -> [fromIntegral i | i <- [0 .. length xs - 1]]
      Ordinal     xs    -> [fromIntegral i | i <- [0 .. length xs - 1]]
    cart :: [[Double]] -> [[Double]]
    cart [] = [[]]
    cart (xs:xss) =
      [ x : ys | x <- xs, ys <- cart xss ]

-- ---------------------------------------------------------------------------
-- Foldover
-- ---------------------------------------------------------------------------

-- | Foldover: 既存 design の符号反転行を追加。
-- Full: 全因子の符号を flip。
-- Partial [names]: 指定因子のみ flip。
-- categorical 列は flip しない (符号の概念が無い)。
augmentFoldover
  :: [Factor]
  -> LA.Matrix Double
  -> FoldoverKind
  -> Either Text AugmentMenuResult
augmentFoldover factors existing kind
  | LA.cols existing /= length factors =
      Left (T.pack "Foldover: existing column count ≠ #factors")
  | otherwise = case kind of
      CategoricalSwap entries -> applyCatSwap factors existing entries
      _ ->
        let !names = map fName factors
            !flipIdxs = case kind of
              FullFoldover -> [ i | (i, f) <- zip [0 ..] factors, factorIsContinuous f ]
              PartialFoldover ns ->
                [ i | (i, f) <- zip [0 ..] factors
                , factorIsContinuous f, fName f `elem` ns
                ]
              CategoricalSwap _ -> []  -- 上で処理済 (到達不可)
        in if null flipIdxs && case kind of { FullFoldover -> False; _ -> True }
             then Left (T.pack "Foldover: no factors to flip (check factor names)")
             else
               let !nE = LA.rows existing
                   !p  = LA.cols existing
                   !rows = LA.toLists existing
                   !flipped =
                     [ [ if j `elem` flipIdxs then negate (r !! j) else r !! j
                       | j <- [0 .. p - 1] ]
                     | r <- rows ]
                   !added = LA.fromLists flipped
                   !full  = existing LA.=== added
                   _ = names  -- 未使用警告対策
               in Right AugmentMenuResult
                    { amrMatrix = full
                    , amrAdded  = nE
                    , amrMethod = case kind of
                        FullFoldover     -> T.pack "Foldover/Full"
                        PartialFoldover _ -> T.pack "Foldover/Partial"
                        CategoricalSwap _ -> T.pack "Foldover/CatSwap"  -- 到達不可
                    }

-- | Phase 28-7: categorical level swap foldover。 各エントリ
-- @(factor_name, [(old, new), ...])@ について、 該当列の level index 値を
-- old → new mapping で置換する (raw 値は level index Double として保持)。
applyCatSwap
  :: [Factor]
  -> LA.Matrix Double
  -> [(Text, [(Text, Text)])]
  -> Either Text AugmentMenuResult
applyCatSwap factors existing entries
  | null entries = Left (T.pack "Foldover/CatSwap: empty mapping list")
  | otherwise = do
      perCol <- traverse (resolveSwap factors) entries
      let nE   = LA.rows existing
          p    = LA.cols existing
          rows = LA.toLists existing
          swapAt j v = case lookup j perCol of
            Nothing -> v
            Just m  -> case lookup (round v :: Int) m of
              Just newIx -> fromIntegral newIx
              Nothing    -> v
          newRows = [ [ swapAt j (r !! j) | j <- [0 .. p - 1] ] | r <- rows ]
          added = LA.fromLists newRows
          full  = existing LA.=== added
      pure AugmentMenuResult
        { amrMatrix = full
        , amrAdded  = nE
        , amrMethod = T.pack "Foldover/CatSwap"
        }

-- | factor 名 + level 名 mapping を、 列 index + level index mapping に解決。
resolveSwap
  :: [Factor]
  -> (Text, [(Text, Text)])
  -> Either Text (Int, [(Int, Int)])
resolveSwap factors (fn, pairs) =
  case lookupWithIdx fn factors of
    Nothing -> Left (T.pack ("Foldover/CatSwap: factor not found: " <> T.unpack fn))
    Just (i, f) -> case fKind f of
      Categorical xs -> Right (i, mkPairs xs)
      Ordinal     xs -> Right (i, mkPairs xs)
      _ -> Left (T.pack
            ("Foldover/CatSwap: factor " <> T.unpack fn <> " is not categorical/ordinal"))
  where
    mkPairs xs = [ (idx old, idx new) | (old, new) <- pairs
                                      , idx old >= 0, idx new >= 0 ]
      where
        idx t = case lookup t (zip (map fst (zip xs [(0::Int)..])) [0..]) of
          Just k  -> k
          Nothing -> case elemIxOf t xs of Just k -> k; Nothing -> -1
    elemIxOf t = go 0
      where
        go _ [] = Nothing
        go k (x:xs') | t == x = Just k
                     | otherwise = go (k + 1) xs'
    lookupWithIdx :: Text -> [Factor] -> Maybe (Int, Factor)
    lookupWithIdx n fs = go 0 fs
      where
        go _ [] = Nothing
        go k (g:gs)
          | fName g == n = Just (k, g)
          | otherwise    = go (k + 1) gs
