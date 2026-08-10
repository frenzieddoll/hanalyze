{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns      #-}
-- |
-- Module      : Hanalyze.Design.Custom.Structured
-- Description : 役割 (fRole) 非依存の構造駆動座標交換エンジン (cells + M⁻¹ による GLS 最適化)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: 構造駆動の座標交換エンジン。
--
--   split-plot 専用エンジン ('Custom.SplitPlot') を __役割 (fRole) 非依存__に
--   一般化したもの。 実験のランダム化/階層構造を、
--
--     - 各因子列がどの行集合で一定か = __cells__ ('gpCells')
--     - 観測の共分散 M の逆行列 = __M⁻¹__ ('gpMInv')
--
--   の 2 つに落とした @GroupingPlan@ で受け取り、
--
--     - 群単位ムーブ: 因子 j の cell (= 同値であるべき行集合) 内の全行を一度に書き換える
--     - GLS 基準: @critValueM crit (chol (Xᵀ M⁻¹ X))@ (検証済・Jones-Goos golden 一致)
--
--   で解く。 CRD (per-row cell・M=I) は 'Custom.Coordinate' の高速路にそのまま委譲するので、
--   本エンジンは SplitPlot / StripPlot / Blocked (= 非自明な群/共分散) 専用。
--
--   ★基準式の根拠: @xtmx = Xᵀ M⁻¹ X@、 @L = chol xtmx@ (L Lᵀ = xtmx) を 'critValueM' に渡すと
--   DOpt で @det(Lᵀ L) = det(xtmx) = det(Xᵀ M⁻¹ X)@ = 厳密な REML 情報量 (Goos-Vandebroek 2003)。
--   M⁻¹ の白色化 X̃ = L⁻¹X でも等価だが、 既存 SplitPlot エンジンで文献値一致を確認済の
--   この式を踏襲する。
--
-- [English]: A structure-driven coordinate-exchange engine.
--
--   Generalizes the split-plot-only engine ('Custom.SplitPlot') to be
--   __role (fRole)-independent__. Takes the experiment's
--   randomization/hierarchical structure via a @GroupingPlan@ that
--   distills it down to two things:
--
--     - which row set each factor column is constant over = __cells__
--       ('gpCells')
--     - the inverse of the observations' covariance M = __M⁻¹__
--       ('gpMInv')
--
--   and solves it by:
--
--     - group-wise moves: rewrites all rows within factor j's cell
--       (= the row set that should share a value) at once
--     - GLS criterion: @critValueM crit (chol (Xᵀ M⁻¹ X))@ (verified;
--       matches the Jones-Goos golden values)
--
--   CRD (per-row cells, M=I) is delegated straight to 'Custom.Coordinate''s
--   fast path, so this engine is only for SplitPlot \/ StripPlot \/ Blocked
--   (= non-trivial group/covariance).
--
--   ★Rationale for the criterion formula: passing @xtmx = Xᵀ M⁻¹ X@,
--   @L = chol xtmx@ (L Lᵀ = xtmx) to 'critValueM' gives, for DOpt,
--   @det(Lᵀ L) = det(xtmx) = det(Xᵀ M⁻¹ X)@ = the exact REML information
--   quantity (Goos-Vandebroek 2003). Whitening via M⁻¹ (X̃ = L⁻¹X) is
--   equivalent, but this formula follows the one already verified to match
--   the literature values in the existing SplitPlot engine.
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

-- | [日本語]: @Structure@ をエンジン内部表現にコンパイルしたもの (Workflow が構築)。
--   [English]: @Structure@ compiled into the engine's internal
--   representation (built by the Workflow).
data GroupingPlan = GroupingPlan
  { gpCells :: ![[[Int]]]
    -- ^ [日本語]: 列 j → その列の値を共有すべき行集合 (cells) の分割。 全 cell の和集合 = @[0..n-1]@。
    --   CRD 因子 = @[[0],[1],…,[n-1]]@ (per-row)、 whole-plot 因子 = 各 WP の行集合。
    --   [English]: Column j → the partition into row sets (cells) that
    --   should share that column's value. The union of all cells =
    --   @[0..n-1]@. A CRD factor = @[[0],[1],…,[n-1]]@ (per-row); a
    --   whole-plot factor = the row set of each WP.
  , gpMInv  :: !(LA.Matrix Double)
    -- ^ [日本語]: n×n の GLS 重み M⁻¹ (@M = I + Σ η_g Z_g Z_gᵀ@)。 CRD なら I。
    --   [English]: The n×n GLS weight M⁻¹ (@M = I + Σ η_g Z_g Z_gᵀ@). I
    --   for CRD.
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- 共分散 M⁻¹
-- ---------------------------------------------------------------------------

-- | [日本語]: @M = I + Σ_g η_g Z_g Z_gᵀ@ の逆行列を dense に構築 (n ≤ ~100 前提で数値 inv)。
--   各群 @(η_g, ids_g)@ は「分散比 η_g と各行の群 ID」。 SplitPlot = 1 群、
--   StripPlot = 2 群 (WP と strip の交差)、 Blocked = 1 群。 block-diagonal に限らないので
--   一様に dense inv で扱う (解析 block 逆と数値的に一致)。 特異なら安全側で単位行列。
--   [English]: Builds a dense inverse of @M = I + Σ_g η_g Z_g Z_gᵀ@
--   (assumes n ≤ ~100 for a numerical inverse). Each group
--   @(η_g, ids_g)@ is "the variance ratio η_g and each row's group ID".
--   SplitPlot = 1 group, StripPlot = 2 groups (the intersection of WP and
--   strip), Blocked = 1 group. Since it isn't restricted to
--   block-diagonal, a uniform dense inverse is used (numerically matches
--   the analytical block inverse). Falls back safely to the identity
--   matrix if singular.
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

-- | [日本語]: 構造駆動の座標交換 (pure・seed 決定的)。 @GroupingPlan@ の cells で群単位ムーブ、
--   M⁻¹ で GLS 基準を評価する。 戻り値 = (raw 設計行列, 最小化方向の基準値)。
--   'cdsSeed' が 'Nothing' なら 'defaultPureSeed'。 制約は各ムーブ候補で 'rowFeasible'
--   (影響行ごと) を課す。
--   [English]: The structure-driven coordinate exchange (pure,
--   seed-deterministic). Performs group-wise moves via 'GroupingPlan''s
--   cells, and evaluates the GLS criterion via M⁻¹. Return value =
--   (raw design matrix, the minimized-direction criterion value). Uses
--   'defaultPureSeed' if 'cdsSeed' is 'Nothing'. Constraints are imposed
--   via 'rowFeasible' (per affected row) for each move candidate.
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

-- | [日本語]: 初期 raw matrix。 各列は cell ごとに 1 つの grid 値を抽選し、 cell 内全行へ同値で置く。
--   [English]: The initial raw matrix. For each column, draws one grid
--   value per cell and places it identically across all rows in the cell.
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

-- | [日本語]: 制約下で実行可能な初期 raw matrix を棄却サンプリングで構築。
--   群構造を保つため 2 段階で引く:
--
--     1. __群 (grouped) 列__ (cell が n 未満 = whole-plot / strip 因子) を cell ごとに 1 値抽選。
--        群内の行はこの値を共有する (= 階層構造の保持)。
--     2. __各行__について、 per-row 列 (sub-plot 因子) を棄却サンプリングし、 群列の固定値と
--        合わせた行全体が全制約を満たすまで再抽選 (行あたり 200 回上限)。
--
--   ある行が群固定値の下でどうしても実行可能にできなければ、 群値ごと引き直す (外側 50 回上限)。
--   全て失敗すれば 'Nothing' (制約が厳しすぎる)。 群列固定 → per-row 探索の順で、
--   whole-plot 因子が群内一定かつ制約満足を両立させる。
--   [English]: Builds a feasible initial raw matrix under constraints via
--   rejection sampling. Draws in two stages to preserve the group
--   structure:
--
--     1. Draws one value per cell for the __grouped columns__ (cells with
--        fewer than n rows = whole-plot \/ strip factors). Rows within a
--        group share this value (= preserving the hierarchical
--        structure).
--     2. For __each row__, rejection-samples the per-row columns
--        (sub-plot factors), redrawing until the whole row (combined with
--        the group columns' fixed values) satisfies all constraints (up to
--        200 draws per row).
--
--   If a row can't be made feasible under the group's fixed values no
--   matter what, the group values are redrawn from scratch (up to 50
--   outer attempts). If all attempts fail, returns 'Nothing' (constraints
--   too strict). By fixing the group columns first and then searching
--   per-row, both keeping whole-plot factors constant within a group and
--   satisfying constraints are achieved simultaneously.
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

-- | [日本語]: 1 restart 分の群単位 coordinate exchange。 列ごと・cell ごとに grid を走査し、
--   制約 (影響行) を満たす範囲で基準最小の値を cell 内全行へ書き込む。
--   [English]: One restart's worth of group-wise coordinate exchange.
--   Scans the grid per column and per cell, writing the
--   criterion-minimizing value across all rows in the cell, within the
--   range that satisfies the constraint (affected rows).
runExchangeG
  :: PrimMonad m
  => [Factor] -> Model -> OptCriterion -> [Constraint] -> DesignBudget
  -> LA.Matrix Double            -- ^ M⁻¹
  -> [VU.Vector Double]          -- ^ [日本語]: 因子ごとの grid。 [English]: The grid for each factor.
  -> [[[Int]]]                   -- ^ [日本語]: 列ごとの cells。 [English]: The cells for each column.
  -> LA.Matrix Double            -- ^ [日本語]: 初期 raw。 [English]: The initial raw values.
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

-- | [日本語]: cell 内全行を列 j = v にしたとき、 影響する全行が制約を満たすか。
--   cell 内の各行は他列の値が異なり得るので行ごとに判定する。
--   [English]: Whether all affected rows satisfy the constraints when all
--   rows in the cell have column j = v. Since each row in the cell may
--   differ in other columns' values, it's judged row by row.
cellFeasible :: [Factor] -> [Constraint] -> LA.Matrix Double -> [Int] -> Int -> Double -> Bool
cellFeasible _ [] _ _ _ _ = True
cellFeasible factors cons mat rows j v =
  all (\i -> rowFeasible factors cons (replaceVecAt (rowVec i) j v)) rows
  where rowVec i = LA.flatten (LA.subMatrix (i, 0) (1, LA.cols mat) mat)

-- | [日本語]: raw matrix → design matrix → GLS 基準値 (最小化方向)。 expand 失敗は +∞。
--   [English]: raw matrix → design matrix → GLS criterion value
--   (minimization direction). +∞ on expand failure.
evalCritG :: [Factor] -> Model -> OptCriterion -> LA.Matrix Double -> LA.Matrix Double -> Double
evalCritG factors model crit mInv raw =
  case expandDesignMatrix factors model raw of
    Left _  -> 1 / 0
    Right x ->
      let !xtmx = LA.tr x LA.<> (mInv LA.<> x)   -- Xᵀ M⁻¹ X
      in critValueM crit (chol xtmx)

-- | [日本語]: @chol m@ = L (下三角、 L Lᵀ = sym m)。 非 PD 時は 0 行列 (基準が候補を棄却)。
--   [English]: @chol m@ = L (lower triangular, L Lᵀ = sym m). The zero
--   matrix when non-PD (the criterion rejects the candidate).
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
