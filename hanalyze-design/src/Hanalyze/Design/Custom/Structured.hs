{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns      #-}
-- |
-- Module      : Hanalyze.Design.Custom.Structured
-- Description : 役割 (fRole) 非依存の構造駆動座標交換エンジン (cells + M⁻¹ による GLS 最適化)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- 構造駆動の座標交換エンジン (Phase 79.2)。
--
--   Phase 78.M の split-plot 専用エンジン ('Custom.SplitPlot') を **役割 (fRole) 非依存**に
--   一般化したもの。 実験のランダム化/階層構造を、
--
--     * 各因子列がどの行集合で一定か = **cells** ('gpCells')
--     * 観測の共分散 M の逆行列 = **M⁻¹** ('gpMInv')
--
--   の 2 つに落とした 'GroupingPlan' で受け取り、
--
--     * 群単位ムーブ: 因子 j の cell (= 同値であるべき行集合) 内の全行を一度に書き換える
--     * GLS 基準: @critValueM crit (chol (Xᵀ M⁻¹ X))@ (検証済・Jones-Goos golden 一致)
--
--   で解く。 CRD (per-row cell・M=I) は 'Custom.Coordinate' の高速路にそのまま委譲するので、
--   本エンジンは SplitPlot / StripPlot / Blocked (= 非自明な群/共分散) 専用。
--
--   ★基準式の根拠: @xtmx = Xᵀ M⁻¹ X@、 @L = chol xtmx@ (L Lᵀ = xtmx) を 'critValueM' に渡すと
--   DOpt で @det(Lᵀ L) = det(xtmx) = det(Xᵀ M⁻¹ X)@ = 厳密な REML 情報量 (Goos-Vandebroek 2003)。
--   M⁻¹ の白色化 X̃ = L⁻¹X でも等価だが、 既存 SplitPlot エンジンで文献値一致を確認済の
--   この式を踏襲する。
module Hanalyze.Design.Custom.Structured
  ( -- * 入力
    GroupingPlan (..)
    -- * 共分散
  , buildMInvFromGroups
    -- * アルゴリズム
  , structuredExchangePure
  ) where

import           Control.Monad             (forM, forM_, when)
import           Control.Monad.Primitive   (PrimMonad, PrimState)
import           Control.Monad.ST          (runST)
import           Data.Maybe                (fromMaybe)
import           Data.Primitive.MutVar
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Numeric.LinearAlgebra     as LA
import qualified System.Random.MWC         as MWC
import qualified Data.Vector               as V
import qualified Data.Vector.Unboxed       as VU
import qualified Data.Vector.Storable      as VS

import           Hanalyze.Design.Custom.Factor  (Factor)
import           Hanalyze.Design.Custom.Model   (Model, expandDesignMatrix)
import           Hanalyze.Design.Custom.Constraint (Constraint)
import           Hanalyze.Design.Custom.Coordinate
                   ( CustomDesignSpec (..), DesignBudget (..)
                   , factorGrid, critValueM, rowFeasible
                   , mkGenSeed, defaultPureSeed )
import           Hanalyze.Design.Optimal        (OptCriterion (..))

-- ---------------------------------------------------------------------------
-- 入力型
-- ---------------------------------------------------------------------------

-- | 'Structure' をエンジン内部表現にコンパイルしたもの (Workflow が構築)。
data GroupingPlan = GroupingPlan
  { gpCells :: ![[[Int]]]
    -- ^ 列 j → その列の値を共有すべき行集合 (cells) の分割。 全 cell の和集合 = @[0..n-1]@。
    --   CRD 因子 = @[[0],[1],…,[n-1]]@ (per-row)、 whole-plot 因子 = 各 WP の行集合。
  , gpMInv  :: !(LA.Matrix Double)
    -- ^ n×n の GLS 重み M⁻¹ (@M = I + Σ η_g Z_g Z_gᵀ@)。 CRD なら I。
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- 共分散 M⁻¹
-- ---------------------------------------------------------------------------

-- | @M = I + Σ_g η_g Z_g Z_gᵀ@ の逆行列を dense に構築 (n ≤ ~100 前提で数値 inv)。
--   各群 @(η_g, ids_g)@ は「分散比 η_g と各行の群 ID」。 SplitPlot = 1 群、
--   StripPlot = 2 群 (WP と strip の交差)、 Blocked = 1 群。 block-diagonal に限らないので
--   一様に dense inv で扱う (解析 block 逆と数値的に一致)。 特異なら安全側で単位行列。
buildMInvFromGroups :: Int -> [(Double, VS.Vector Int)] -> LA.Matrix Double
buildMInvFromGroups n groups =
  let mEntry i j =
        (if i == j then 1 else 0)
          + sum [ if ids VS.! i == ids VS.! j then eta else 0 | (eta, ids) <- groups ]
      mMat = (n LA.>< n) [ mEntry i j | i <- [0 .. n - 1], j <- [0 .. n - 1] ]
  in if abs (LA.det mMat) < 1e-12 then LA.ident n else LA.inv mMat

-- ---------------------------------------------------------------------------
-- アルゴリズム (seed 決定的・pure)
-- ---------------------------------------------------------------------------

-- | 構造駆動の座標交換 (pure・seed 決定的)。 'GroupingPlan' の cells で群単位ムーブ、
--   M⁻¹ で GLS 基準を評価する。 戻り値 = (raw 設計行列, 最小化方向の基準値)。
--   'cdsSeed' が 'Nothing' なら 'defaultPureSeed'。 制約は各ムーブ候補で 'rowFeasible'
--   (影響行ごと) を課す。
structuredExchangePure
  :: CustomDesignSpec -> GroupingPlan -> Either Text (LA.Matrix Double, Double)
structuredExchangePure spec gplan
  | null (cdsFactors spec)         = Left "structuredExchange: empty factor list"
  | cdsNRuns spec < 1              = Left "structuredExchange: nRuns must be >= 1"
  | dbRestarts (cdsBudget spec) < 1 = Left "structuredExchange: dbRestarts must be >= 1"
  | length (gpCells gplan) /= length (cdsFactors spec) =
      Left "structuredExchange: gpCells length must equal factor count"
  | LA.rows (gpMInv gplan) /= cdsNRuns spec =
      Left "structuredExchange: gpMInv dimension must equal nRuns"
  | otherwise = runST $ do
      gen <- mkGenSeed (fromMaybe defaultPureSeed (cdsSeed spec))
      let !factors = cdsFactors spec
          !model   = cdsModel spec
          !crit    = cdsCriterion spec
          !cons    = cdsConstraints spec
          !budget  = cdsBudget spec
          !n       = cdsNRuns spec
          !mInv    = gpMInv gplan
          !cells   = gpCells gplan
          !grids   = map (factorGrid budget) factors
      bestRef <- newMutVar Nothing
      forM_ [1 .. dbRestarts budget] $ \_ -> do
        -- 制約なしは高速な単純抽選 (既存挙動)、 制約ありは実行可能な初期解を棄却サンプリング。
        mInit <- if null cons
                   then Just <$> randomInitG grids cells n gen
                   else randomInitGFeasible factors cons grids cells n gen
        case mInit of
          Nothing    -> pure ()   -- この restart は実行可能初期解を引けず (次 restart へ)
          Just init0 -> do
            (finalM, finalC) <-
              runExchangeG factors model crit cons budget mInv grids cells init0
            modifyMutVar' bestRef $ \mb -> case mb of
              Nothing -> Just (finalM, finalC)
              Just (_, c0) | finalC < c0 -> Just (finalM, finalC)
                           | otherwise   -> mb
      mb <- readMutVar bestRef
      pure $ case mb of
        Nothing     -> Left "structuredExchange: 実行可能な初期解が得られませんでした (制約が厳しすぎる可能性)"
        Just (m, c) -> Right (m, c)

-- | 初期 raw matrix。 各列は cell ごとに 1 つの grid 値を抽選し、 cell 内全行へ同値で置く。
randomInitG
  :: PrimMonad m
  => [VU.Vector Double] -> [[[Int]]] -> Int -> MWC.Gen (PrimState m)
  -> m (LA.Matrix Double)
randomInitG grids cells n gen = do
  let gridsV = V.fromList grids
  cols <- mapM
    (\(j, cellsOfCol) -> do
        let g  = gridsV V.! j
            gl = VU.length g
        -- cell ごとに 1 値、 cell 内全行に配る
        vals <- mapM (\rows -> do
                        k <- MWC.uniformR (0, gl - 1) gen
                        pure (rows, g VU.! k)) cellsOfCol
        let assign = [ (i, v) | (rows, v) <- vals, i <- rows ]
        pure (LA.fromList [ lookupRow i assign | i <- [0 .. n - 1] ]))
    (zip [0 ..] cells)
  pure (LA.fromColumns cols)
  where
    lookupRow i assign = case lookup i assign of
      Just v  -> v
      Nothing -> 0   -- cells が [0..n-1] を被覆する前提 (不達)

-- | 制約下で実行可能な初期 raw matrix を棄却サンプリングで構築 (Phase 79.5)。
--   群構造を保つため 2 段階で引く:
--
--     1. **群 (grouped) 列** (cell が n 未満 = whole-plot / strip 因子) を cell ごとに 1 値抽選。
--        群内の行はこの値を共有する (= 階層構造の保持)。
--     2. **各行**について、 per-row 列 (sub-plot 因子) を棄却サンプリングし、 群列の固定値と
--        合わせた行全体が全制約を満たすまで再抽選 (行あたり 200 回上限)。
--
--   ある行が群固定値の下でどうしても実行可能にできなければ、 群値ごと引き直す (外側 50 回上限)。
--   全て失敗すれば 'Nothing' (制約が厳しすぎる)。 群列固定 → per-row 探索の順で、
--   whole-plot 因子が群内一定かつ制約満足を両立させる。
randomInitGFeasible
  :: PrimMonad m
  => [Factor] -> [Constraint] -> [VU.Vector Double] -> [[[Int]]] -> Int
  -> MWC.Gen (PrimState m) -> m (Maybe (LA.Matrix Double))
randomInitGFeasible factors cons grids cells n gen = tryOuter maxOuter
  where
    maxOuter = 50 :: Int
    maxRow   = 200 :: Int
    gridsV   = V.fromList grids
    p        = length grids
    isGrouped j = length (cells !! j) < n   -- cell 数 < n → 群 (共有) 列

    tryOuter 0 = pure Nothing
    tryOuter t = do
      -- 1. 群列の値を cell ごとに抽選 → 各群列 j の「行 → 値」 (Just)、 per-row 列は Nothing
      grouped <- forM [0 .. p - 1] $ \j ->
        if isGrouped j
          then do
            let g  = gridsV V.! j
                gl = VU.length g
            cellVals <- forM (cells !! j) $ \rows -> do
              k <- MWC.uniformR (0, gl - 1) gen
              pure (rows, g VU.! k)
            let rowVal = VU.generate n
                  (\i -> head [ v | (rs, v) <- cellVals, i `elem` rs ])
            pure (Just rowVal)
          else pure Nothing
      -- 2. 各行を棄却サンプリング (群列は固定・per-row 列を引く)
      mRows <- forM [0 .. n - 1] $ \i -> drawRow grouped i maxRow
      case sequence mRows of
        Just rs -> pure (Just (LA.fromRows rs))
        Nothing -> tryOuter (t - 1)   -- どこかの行が詰んだ → 群値ごと引き直し

    drawRow _       _ 0  = pure Nothing
    drawRow grouped i tr = do
      vs <- forM [0 .. p - 1] $ \j ->
        case grouped !! j of
          Just rowVal -> pure (rowVal VU.! i)     -- 群列: 固定値
          Nothing     -> do                        -- per-row 列: 抽選
            let g  = gridsV V.! j
                gl = VU.length g
            k <- MWC.uniformR (0, gl - 1) gen
            pure (g VU.! k)
      let row = LA.fromList vs
      if rowFeasible factors cons row
        then pure (Just row)
        else drawRow grouped i (tr - 1)

-- | 1 restart 分の群単位 coordinate exchange。 列ごと・cell ごとに grid を走査し、
--   制約 (影響行) を満たす範囲で基準最小の値を cell 内全行へ書き込む。
runExchangeG
  :: PrimMonad m
  => [Factor] -> Model -> OptCriterion -> [Constraint] -> DesignBudget
  -> LA.Matrix Double            -- ^ M⁻¹
  -> [VU.Vector Double]          -- ^ 因子ごとの grid
  -> [[[Int]]]                   -- ^ 列ごとの cells
  -> LA.Matrix Double            -- ^ 初期 raw
  -> m (LA.Matrix Double, Double)
runExchangeG factors model crit cons budget mInv grids cells init0 = do
  matRef  <- newMutVar init0
  critRef <- newMutVar (evalCritG factors model crit mInv init0)
  let gridsV = V.fromList grids
      cellsV = V.fromList cells
      !p     = length grids
  let loopOuter !it
        | it > dbMaxIter budget = pure ()
        | otherwise = do
            beforeC <- readMutVar critRef
            forM_ [0 .. p - 1] $ \j ->
              forM_ (cellsV V.! j) $ \rows -> do
                curMat <- readMutVar matRef
                curC   <- readMutVar critRef
                let g    = gridsV V.! j
                    gl   = VU.length g
                    oldV = if null rows then 0
                             else curMat `LA.atIndex` (head rows, j)
                bestRef <- newMutVar (oldV, curC)
                forM_ [0 .. gl - 1] $ \k -> do
                  let !v = g VU.! k
                  when (cellFeasible factors cons curMat rows j v) $ do
                    let !cand = setColumnInRows curMat rows j v
                        !c    = evalCritG factors model crit mInv cand
                    modifyMutVar' bestRef $ \cur@(_, bc) ->
                      if c < bc then (v, c) else cur
                (bv, bc) <- readMutVar bestRef
                when (bc < curC) $ do
                  writeMutVar matRef  (setColumnInRows curMat rows j bv)
                  writeMutVar critRef bc
            afterC <- readMutVar critRef
            let rel = if abs beforeC < 1e-12
                        then beforeC - afterC
                        else (beforeC - afterC) / abs beforeC
            when (rel > dbTol budget) (loopOuter (it + 1))
  loopOuter 1
  finalM <- readMutVar matRef
  finalC <- readMutVar critRef
  pure (finalM, finalC)

-- | cell 内全行を列 j = v にしたとき、 影響する全行が制約を満たすか。
--   cell 内の各行は他列の値が異なり得るので行ごとに判定する。
cellFeasible :: [Factor] -> [Constraint] -> LA.Matrix Double -> [Int] -> Int -> Double -> Bool
cellFeasible _ [] _ _ _ _ = True
cellFeasible factors cons mat rows j v =
  all (\i -> rowFeasible factors cons (replaceVecAt (rowVec i) j v)) rows
  where rowVec i = LA.flatten (LA.subMatrix (i, 0) (1, LA.cols mat) mat)

-- | raw matrix → design matrix → GLS 基準値 (最小化方向)。 expand 失敗は +∞。
evalCritG :: [Factor] -> Model -> OptCriterion -> LA.Matrix Double -> LA.Matrix Double -> Double
evalCritG factors model crit mInv raw =
  case expandDesignMatrix factors model raw of
    Left _  -> 1 / 0
    Right x ->
      let !xtmx = LA.tr x LA.<> (mInv LA.<> x)   -- Xᵀ M⁻¹ X
      in critValueM crit (chol xtmx)

-- | @chol m@ = L (下三角、 L Lᵀ = sym m)。 非 PD 時は 0 行列 (基準が候補を棄却)。
chol :: LA.Matrix Double -> LA.Matrix Double
chol m = case LA.mbChol (LA.sym m) of
  Just u  -> LA.tr u
  Nothing -> LA.konst 0 (LA.rows m, LA.cols m)

-- ---------------------------------------------------------------------------
-- matrix / vector utility
-- ---------------------------------------------------------------------------

setColumnInRows :: LA.Matrix Double -> [Int] -> Int -> Double -> LA.Matrix Double
setColumnInRows m rows j v = LA.accum m const [ ((i, j), v) | i <- rows ]

replaceVecAt :: LA.Vector Double -> Int -> Double -> LA.Vector Double
replaceVecAt v j x =
  LA.fromList [ if k == j then x else v `LA.atIndex` k | k <- [0 .. LA.size v - 1] ]
