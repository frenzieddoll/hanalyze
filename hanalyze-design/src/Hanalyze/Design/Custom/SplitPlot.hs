{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns      #-}
-- |
-- Module      : Hanalyze.Design.Custom.SplitPlot
-- Description : Custom Design の Split-Plot 生成 (役割駆動の REML D-optimal 交換、内部 legacy)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: Custom Design の Split-Plot 生成。
--
-- ★現在は __内部 legacy__: 製品パス (高レベル @customDesign@ + @Structure@) は役割非依存の
--   構造駆動エンジン @Design.Custom.Structured@ を使う。 本モジュール (役割 @fRole@ 駆動) は
--   bench-custom-design の 3 エンジン比較 + Jones-Goos 低レベル golden の証跡として温存する
--   (新規機能は Structured 側へ。 M⁻¹ / GLS 基準の math は両者で数値一致)。
--
-- spec: doe-custom-design-spec v0.1.1 §2.5 / §3.6。
-- 参考: Goos & Vandebroek (2003) "D-Optimal Split-Plot Designs", J Quality Tech 35:1-15。
--
-- === モデル (簡易 REML)
--
--   y_ij = X_ij β + b_i + ε_ij
--
-- ここで b_i ~ N(0, σ²_WP) は whole-plot 効果、 ε_ij ~ N(0, σ²) は run-level error。
-- 分散比 η = σ²_WP / σ² がユーザ指定 (既定 1.0、 spec §2.5 で議論)。
--
-- 観測ベクトル全体の分散構造:
--
--   V = σ² (I + η · Z Zᵀ)
--
-- ここで Z は whole-plot indicator matrix (n × n_WP)。
-- REML information matrix:
--
--   I_β = (1/σ²) · Xᵀ M⁻¹ X、   M = I + η · Z Zᵀ
--
-- D-optimality は max det(Xᵀ M⁻¹ X)。 σ² は定数倍なので criterion に影響しない。
--
-- === 本 commit のスコープ
--
--   - 連続因子の whole-plot のみ対応 (categorical WP は 25-5 stub で Left)
--   - Coordinate exchange を whole-plot 単位 / sub-plot 単位の 2 段に分けて適用:
--     - WP 因子: 1 WP 内では同値、 列を WP indicator 構造で更新
--     - SP 因子: 各 run 単位で coordinate exchange (= 通常)
--   - η はユーザ指定 (`spcVarRatio`)、 既定 1.0
--   - strip-plot (VeryHardToChange) は未対応 (Left)
--
-- [English]: Custom Design's Split-Plot generation.
--
-- ★Currently __internal legacy__: the product path (the high-level
--   @customDesign@ + @Structure@) uses the role-independent structure-driven
--   engine @Design.Custom.Structured@. This module (driven by the @fRole@
--   role) is kept as the evidence trail for the bench-custom-design 3-engine
--   comparison and the Jones-Goos low-level golden values (new features go
--   into Structured; the M⁻¹ / GLS criterion math matches numerically
--   between the two).
--
-- spec: doe-custom-design-spec v0.1.1 §2.5 / §3.6.
-- Reference: Goos & Vandebroek (2003) "D-Optimal Split-Plot Designs", J
-- Quality Tech 35:1-15.
--
-- === Model (simplified REML)
--
--   y_ij = X_ij β + b_i + ε_ij
--
-- where b_i ~ N(0, σ²_WP) is the whole-plot effect and ε_ij ~ N(0, σ²) is
-- the run-level error. The variance ratio η = σ²_WP / σ² is user-specified
-- (default 1.0; discussed in spec §2.5).
--
-- Variance structure of the full observation vector:
--
--   V = σ² (I + η · Z Zᵀ)
--
-- where Z is the whole-plot indicator matrix (n × n_WP). REML information
-- matrix:
--
--   I_β = (1/σ²) · Xᵀ M⁻¹ X,   M = I + η · Z Zᵀ
--
-- D-optimality is max det(Xᵀ M⁻¹ X). σ² is a constant multiplier so it
-- doesn't affect the criterion.
--
-- === Scope of this commit
--
--   - Only whole-plots of continuous factors are supported (categorical WP
--     is a stub returning Left, for a future commit).
--   - Coordinate exchange is applied in two stages, at the whole-plot level
--     and the sub-plot level:
--     - WP factors: identical within one WP, the column is updated via the
--       WP indicator structure
--     - SP factors: ordinary per-run coordinate exchange
--   - η is user-specified (`spcVarRatio`), default 1.0
--   - strip-plot (VeryHardToChange) is not supported (Left)
module Hanalyze.Design.Custom.SplitPlot
  ( SplitPlotConfig (..)
  , defaultSplitPlotConfig
  , SplitPlotDesign (..)
  , generateSplitPlot
  , generateSplitPlotPure
    -- * 内部 helper (test 用)
  , whichRoleIsWP
  , wholePlotIndicator
  ) where

import           Control.Monad             (forM_, when)
import           Control.Monad.Primitive   (PrimMonad, PrimState)
import           Control.Monad.ST          (runST)
import           Data.Maybe                (fromMaybe)
import           Data.Primitive.MutVar
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Numeric.LinearAlgebra     as LA
import qualified System.Random.MWC         as MWC
import qualified Data.Vector.Unboxed       as VU
import qualified Data.Vector               as V
import qualified Data.Vector.Storable      as VS

import           Hanalyze.Design.Custom.Factor
import           Hanalyze.Design.Custom.Model
import           Hanalyze.Design.Custom.Coordinate
                   (CustomDesignSpec (..), DesignBudget (..)
                   , factorGrid, critValueM
                   , mkGen, mkGenSeed, defaultPureSeed)
import           Hanalyze.Design.Optimal   (OptCriterion (..))

-- ---------------------------------------------------------------------------
-- 型
-- ---------------------------------------------------------------------------

data SplitPlotConfig = SplitPlotConfig
  { spcNWhole    :: !Int     -- ^ [日本語]: whole-plot 数 (必須、 spec §2.5 でユーザ指定強制) [English]: Number of whole-plots (required; spec §2.5 mandates explicit user specification)
  , spcVarRatio  :: !Double  -- ^ [日本語]: η = σ²_WP / σ² (既定 1.0) [English]: η = σ²_WP / σ² (default 1.0)
  , spcNStrip    :: !(Maybe Int)
    -- ^ [日本語]: strip-plot 構造の strip 数 (Just nStrip)。
    --   'VeryHardToChange' role 因子は strip 内で constant。 Nothing なら
    --   通常の split-plot。 n = nWP × nStrip を満たす必要 (= 行配置: row i は
    --   wp = i div nStrip、 strip = i mod nStrip)
    --   [English]: The number of strips for the strip-plot structure
    --   (Just nStrip). 'VeryHardToChange'-role factors are constant within
    --   a strip. Nothing means an ordinary split-plot. Requires
    --   n = nWP × nStrip (row placement: row i has wp = i div nStrip,
    --   strip = i mod nStrip).
  } deriving (Show)

defaultSplitPlotConfig :: Int -> SplitPlotConfig
defaultSplitPlotConfig nWP = SplitPlotConfig nWP 1.0 Nothing

data SplitPlotDesign = SplitPlotDesign
  { spdMatrix      :: !(LA.Matrix Double)
  , spdWholePlotId :: !(VS.Vector Int)         -- ^ [日本語]: 各行の WP ID (0..nWP-1) [English]: The WP ID of each row (0..nWP-1)
  , spdSubPlotId   :: !(Maybe (VS.Vector Int))
    -- ^ [日本語]: strip-plot 時の strip ID (Just)、 通常 split-plot は Nothing
    --   [English]: The strip ID for strip-plot (Just); Nothing for an
    --   ordinary split-plot
  , spdNWhole      :: !Int
  , spdGEFFEst     :: !Double                  -- ^ [日本語]: 推定 Generalized Estimating Function 値。
    --   ≒ - det(I_β) の最小化値 (DOpt のみ意味あり、 他 criterion は critValueM 経由)。
    --   [English]: The estimated Generalized Estimating Function value.
    --   ≈ the minimized value of - det(I_β) (only meaningful for DOpt;
    --   other criteria go through critValueM).
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- 公開 API
-- ---------------------------------------------------------------------------

-- | [日本語]: seed 由来の gen を作って 'generateSplitPlotWith' を IO で走らせる薄い wrapper。
--   'cdsSeed' が 'Nothing' の場合のみ entropy 依存 (非決定的)。
--   seed 決定的な純粋版は 'generateSplitPlotPure'。
--   [English]: A thin wrapper that builds a gen from the seed and runs
--   'generateSplitPlotWith' in IO. Only depends on entropy
--   (non-deterministic) when 'cdsSeed' is 'Nothing'. The seed-deterministic
--   pure version is 'generateSplitPlotPure'.
generateSplitPlot
  :: CustomDesignSpec
  -> SplitPlotConfig
  -> IO (Either Text SplitPlotDesign)
generateSplitPlot spec cfg = do
  gen <- mkGen (cdsSeed spec)
  generateSplitPlotWith spec cfg gen

-- | [日本語]: seed 決定的な純粋版。 'runST' で MWC gen + MutVar を閉じ込め、
--   IO 無しで 'SplitPlotDesign' を返す。 'cdsSeed' が 'Nothing' なら 'defaultPureSeed'
--   を用いて全域にする。 同一 seed なら 'generateSplitPlot' (IO) とビット一致する。
--   [English]: The seed-deterministic pure version. Encloses the MWC gen +
--   MutVar inside 'runST', returning 'SplitPlotDesign' without IO. If
--   'cdsSeed' is 'Nothing', uses 'defaultPureSeed' to make it total. With
--   the same seed, matches 'generateSplitPlot' (IO) bit-for-bit.
generateSplitPlotPure
  :: CustomDesignSpec
  -> SplitPlotConfig
  -> Either Text SplitPlotDesign
generateSplitPlotPure spec cfg = runST $ do
  gen <- mkGenSeed (fromMaybe defaultPureSeed (cdsSeed spec))
  generateSplitPlotWith spec cfg gen

-- | [日本語]: split-plot 生成本体 (PrimMonad 一般化)。 IO / ST どちらでも走る。
--   [English]: The core split-plot generator (generalized over PrimMonad).
--   Runs under either IO or ST.
generateSplitPlotWith
  :: PrimMonad m
  => CustomDesignSpec
  -> SplitPlotConfig
  -> MWC.Gen (PrimState m)
  -> m (Either Text SplitPlotDesign)
generateSplitPlotWith spec cfg gen
  | spcNWhole cfg < 1 =
      pure (Left (T.pack "generateSplitPlot: spcNWhole must be >= 1"))
  | cdsNRuns spec < spcNWhole cfg =
      pure (Left (T.pack "generateSplitPlot: nRuns must be >= spcNWhole"))
  -- Phase 28-2: VeryHardToChange (strip-plot) を対応。 spcNStrip = Just nStrip
  -- が必要 + n = nWP × nStrip を満たすこと
  | any ((== VeryHardToChange) . fRole) (cdsFactors spec)
    && case spcNStrip cfg of Nothing -> True; _ -> False =
      pure (Left (T.pack
        "generateSplitPlot: VeryHardToChange factor present but spcNStrip not set"))
  -- Phase 28-3: Categorical/Ordinal whole-plot 因子も対応 (factorGrid が level
  -- index を返すため、 randomInitSP/runExchangeSP の WP loop でそのまま機能する)
  | spcVarRatio cfg < 0 =
      pure (Left (T.pack "generateSplitPlot: spcVarRatio (η) must be >= 0"))
  | case spcNStrip cfg of
      Just s -> s < 1 || s * spcNWhole cfg /= cdsNRuns spec
      Nothing -> False =
      pure (Left (T.pack
        "generateSplitPlot: spcNStrip × spcNWhole must equal nRuns (strip-plot grid)"))
  | otherwise = do
      let !factors  = cdsFactors spec
          !n        = cdsNRuns spec
          !nWP      = spcNWhole cfg
          !eta      = spcVarRatio cfg
          !budget   = cdsBudget spec
          !crit     = cdsCriterion spec
          !model    = cdsModel spec
          !wpIxs    = whichRoleIsWP factors
          !stripIxs = whichRoleIsStrip factors
          !wpId     = wholePlotIndicator n nWP
          !mStripId = case spcNStrip cfg of
            Just nStrip -> Just (stripPlotIndicator n nStrip)
            Nothing     -> Nothing
      if null wpIxs && null stripIxs
        then pure (Left (T.pack
          "generateSplitPlot: no HardToChange/VeryHardToChange factor found"))
        else do
          bestRef <- newMutVar Nothing
          forM_ [1 .. dbRestarts budget] $ \_ -> do
            init0 <- randomInitSPStrip factors wpIxs stripIxs wpId mStripId n budget gen
            (finalM, finalC) <- runExchangeSP factors model crit budget eta wpIxs stripIxs wpId mStripId init0
            modifyMutVar' bestRef $ \mb -> case mb of
              Nothing -> Just (finalM, finalC)
              Just (_, c0) | finalC < c0 -> Just (finalM, finalC)
                           | otherwise   -> mb
          mb <- readMutVar bestRef
          case mb of
            Nothing -> pure (Left (T.pack "generateSplitPlot: no restart produced a design"))
            Just (m, c) -> pure $ Right SplitPlotDesign
              { spdMatrix      = m
              , spdWholePlotId = wpId
              , spdSubPlotId   = mStripId
              , spdNWhole      = nWP
              , spdGEFFEst     = c
              }

-- ---------------------------------------------------------------------------
-- WP indicator / role helper
-- ---------------------------------------------------------------------------

-- | [日本語]: HardToChange factor の column index リスト (whole-plot 因子)。
--   [English]: The column index list of HardToChange factors (whole-plot
--   factors).
whichRoleIsWP :: [Factor] -> [Int]
whichRoleIsWP fs = [ i | (i, f) <- zip [0 ..] fs, fRole f == HardToChange ]

-- | [日本語]: VeryHardToChange factor の column index リスト (strip 因子)。
--   [English]: The column index list of VeryHardToChange factors (strip
--   factors).
whichRoleIsStrip :: [Factor] -> [Int]
whichRoleIsStrip fs = [ i | (i, f) <- zip [0 ..] fs, fRole f == VeryHardToChange ]

-- | [日本語]: n 行を nWP に均等割り当てした WP indicator (0..nWP-1)。
--   余りは最初のいくつかの WP に追加で振る。
--   [English]: The WP indicator (0..nWP-1) obtained by evenly assigning n
--   rows to nWP whole-plots. The remainder is distributed to the first
--   few WPs.
wholePlotIndicator :: Int -> Int -> VS.Vector Int
wholePlotIndicator n nWP =
  let base  = n `div` nWP
      extra = n `mod` nWP
      sizes = [ if i < extra then base + 1 else base | i <- [0 .. nWP - 1] ]
      ids   = concat [ replicate s i | (i, s) <- zip [0 ..] sizes ]
  in VS.fromList ids

-- | [日本語]: strip indicator (0..nStrip-1)。 row i → i `mod` nStrip。
--   WP grouping (= i `div` nStrip) と直交する partitioning を実現する。
--   n = nWP × nStrip の前提 (generateSplitPlot の guard で確認済)
--   [English]: The strip indicator (0..nStrip-1); row i → i `mod` nStrip.
--   Realizes a partitioning orthogonal to the WP grouping
--   (= i `div` nStrip). Assumes n = nWP × nStrip (checked by
--   generateSplitPlot's guard).
stripPlotIndicator :: Int -> Int -> VS.Vector Int
stripPlotIndicator n nStrip =
  VS.fromList [ i `mod` nStrip | i <- [0 .. n - 1] ]

-- ---------------------------------------------------------------------------
-- 初期化 (split-plot 構造を保つ)
-- ---------------------------------------------------------------------------

-- | [日本語]: 初期 raw matrix。 WP 因子は WP ごとに 1 値、 strip 因子は strip ごとに
--   1 値、 SP 因子は run ごとに 1 値。
--   [English]: The initial raw matrix. WP factors get one value per WP,
--   strip factors one value per strip, and SP factors one value per run.
randomInitSPStrip
  :: PrimMonad m
  => [Factor]
  -> [Int]               -- ^ [日本語]: WP 因子 column index [English]: WP factor column index
  -> [Int]               -- ^ [日本語]: strip 因子 column index [English]: strip factor column index
  -> VS.Vector Int       -- ^ [日本語]: 各行の WP id [English]: WP id per row
  -> Maybe (VS.Vector Int)  -- ^ [日本語]: 各行の strip id [English]: strip id per row
  -> Int                 -- ^ n
  -> DesignBudget
  -> MWC.Gen (PrimState m)
  -> m (LA.Matrix Double)
randomInitSPStrip factors wpIxs stripIxs wpId mStripId n budget gen = do
  let p    = length factors
      nWP  = if VS.null wpId then 0 else 1 + VS.maximum wpId
      nStrp = case mStripId of
        Just s | not (VS.null s) -> 1 + VS.maximum s
        _ -> 0
  cols <- mapM
    (\j -> do
       let g = factorGrid budget (factors !! j)
           gl = VU.length g
       if j `elem` wpIxs
         then do
           wpVals <- VU.replicateM nWP $ do
             k <- MWC.uniformR (0, gl - 1) gen
             pure (g VU.! k)
           pure $ LA.fromList
             [ wpVals VU.! (wpId VS.! i) | i <- [0 .. n - 1] ]
         else if j `elem` stripIxs
           then case mStripId of
             Nothing -> pure (LA.konst 0 n)  -- 不可達: guard 済
             Just stripId -> do
               stripVals <- VU.replicateM nStrp $ do
                 k <- MWC.uniformR (0, gl - 1) gen
                 pure (g VU.! k)
               pure $ LA.fromList
                 [ stripVals VU.! (stripId VS.! i) | i <- [0 .. n - 1] ]
           else do
             vs <- VU.replicateM n $ do
               k <- MWC.uniformR (0, gl - 1) gen
               pure (g VU.! k)
             pure (LA.fromList (VU.toList vs))
    ) [0 .. p - 1]
  pure (LA.fromColumns cols)

-- ---------------------------------------------------------------------------
-- Coordinate exchange (split-plot 構造保持)
-- ---------------------------------------------------------------------------

runExchangeSP
  :: PrimMonad m
  => [Factor]
  -> Model
  -> OptCriterion
  -> DesignBudget
  -> Double                      -- ^ η
  -> [Int]                       -- ^ [日本語]: WP factor の index [English]: WP factor indices
  -> [Int]                       -- ^ [日本語]: strip factor の index [English]: strip factor indices
  -> VS.Vector Int               -- ^ [日本語]: wpId [English]: wpId
  -> Maybe (VS.Vector Int)       -- ^ [日本語]: stripId [English]: stripId
  -> LA.Matrix Double
  -> m (LA.Matrix Double, Double)
runExchangeSP factors model crit budget eta wpIxs stripIxs wpId mStripId init0 = do
  matRef  <- newMutVar init0
  critRef <- newMutVar (evalCritSP factors model crit eta wpId mStripId init0)
  let !n         = LA.rows init0
      !p         = LA.cols init0
      gridsV     = V.fromList (map (factorGrid budget) factors)
      nWP        = if VS.null wpId then 0 else 1 + VS.maximum wpId
      nStrp      = case mStripId of
        Just s | not (VS.null s) -> 1 + VS.maximum s
        _ -> 0
      isWPidx j  = j `elem` wpIxs
      isStripIdx j = j `elem` stripIxs
  let loopOuter !it
        | it > dbMaxIter budget = pure ()
        | otherwise = do
            beforeC <- readMutVar critRef
            -- SP 因子: 通常の per-row × per-column 走査
            forM_ [0 .. n - 1] $ \i ->
              forM_ [0 .. p - 1] $ \j ->
                when (not (isWPidx j) && not (isStripIdx j)) $ do
                  curMat <- readMutVar matRef
                  curC   <- readMutVar critRef
                  let g  = gridsV V.! j
                      gl = VU.length g
                  bestRef <- newMutVar (curMat `LA.atIndex` (i, j), curC)
                  forM_ [0 .. gl - 1] $ \k -> do
                    let !v = g VU.! k
                        !cand = setEntry curMat i j v
                        !c = evalCritSP factors model crit eta wpId mStripId cand
                    modifyMutVar' bestRef $ \cur@(_, bc) ->
                      if c < bc then (v, c) else cur
                  (bv, bc) <- readMutVar bestRef
                  when (bc < curC) $ do
                    writeMutVar matRef  (setEntry curMat i j bv)
                    writeMutVar critRef bc
            -- WP 因子: 各 WP × 各 WP-column 走査、 WP 内全 row に同値書き込み
            forM_ [0 .. nWP - 1] $ \w ->
              forM_ wpIxs $ \j -> do
                curMat <- readMutVar matRef
                curC   <- readMutVar critRef
                let g  = gridsV V.! j
                    gl = VU.length g
                    runsInWP = [ i | i <- [0 .. n - 1], wpId VS.! i == w ]
                    oldV = if null runsInWP then 0
                             else curMat `LA.atIndex` (head runsInWP, j)
                bestRef <- newMutVar (oldV, curC)
                forM_ [0 .. gl - 1] $ \k -> do
                  let !v = g VU.! k
                      !cand = setColumnInRows curMat runsInWP j v
                      !c = evalCritSP factors model crit eta wpId mStripId cand
                  modifyMutVar' bestRef $ \cur@(_, bc) ->
                    if c < bc then (v, c) else cur
                (bv, bc) <- readMutVar bestRef
                when (bc < curC) $ do
                  writeMutVar matRef  (setColumnInRows curMat runsInWP j bv)
                  writeMutVar critRef bc
            -- Phase 28-2: strip 因子: 各 strip × 各 strip-column 走査、
            -- strip 内全 row に同値書き込み
            case mStripId of
              Just stripId -> forM_ [0 .. nStrp - 1] $ \s ->
                forM_ stripIxs $ \j -> do
                  curMat <- readMutVar matRef
                  curC   <- readMutVar critRef
                  let g  = gridsV V.! j
                      gl = VU.length g
                      runsInStrip = [ i | i <- [0 .. n - 1], stripId VS.! i == s ]
                      oldV = if null runsInStrip then 0
                               else curMat `LA.atIndex` (head runsInStrip, j)
                  bestRef <- newMutVar (oldV, curC)
                  forM_ [0 .. gl - 1] $ \k -> do
                    let !v = g VU.! k
                        !cand = setColumnInRows curMat runsInStrip j v
                        !c = evalCritSP factors model crit eta wpId mStripId cand
                    modifyMutVar' bestRef $ \cur@(_, bc) ->
                      if c < bc then (v, c) else cur
                  (bv, bc) <- readMutVar bestRef
                  when (bc < curC) $ do
                    writeMutVar matRef  (setColumnInRows curMat runsInStrip j bv)
                    writeMutVar critRef bc
              Nothing -> pure ()
            afterC <- readMutVar critRef
            let rel = if abs beforeC < 1e-12
                        then beforeC - afterC
                        else (beforeC - afterC) / abs beforeC
            when (rel > dbTol budget) (loopOuter (it + 1))
  loopOuter 1
  finalM <- readMutVar matRef
  finalC <- readMutVar critRef
  pure (finalM, finalC)

-- | [日本語]: REML criterion: critValueM を X' M⁻¹ X 経由で評価。
--   DOpt の場合 det(X' M⁻¹ X) を最大化 (= criterion 最小化)。
--   M = I + η · Z Zᵀ。 nWP=n (= completely randomized) なら M=(1+η)I、
--   η=0 なら M=I (= 通常 D-opt)。
--   [English]: The REML criterion: evaluated via critValueM using
--   X' M⁻¹ X. For DOpt, maximizes det(X' M⁻¹ X) (= minimizing the
--   criterion). M = I + η · Z Zᵀ. If nWP=n (= completely randomized) then
--   M=(1+η)I; if η=0 then M=I (= ordinary D-opt).
evalCritSP
  :: [Factor]
  -> Model
  -> OptCriterion
  -> Double
  -> VS.Vector Int               -- ^ [日本語]: wpId [English]: wpId
  -> Maybe (VS.Vector Int)       -- ^ [日本語]: stripId [English]: stripId
  -> LA.Matrix Double
  -> Double
evalCritSP factors model crit eta wpId mStripId raw =
  case expandDesignMatrix factors model raw of
    Left _  -> 1 / 0
    Right x ->
      let !n  = LA.rows x
          mInv = case mStripId of
            Nothing ->
              -- 通常 split-plot: M = I + η · Z_WP Z_WPᵀ、 block-diagonal
              let nWP = if VS.null wpId then 0 else 1 + VS.maximum wpId
                  wpSizes = [ length [ i | i <- [0 .. n - 1], wpId VS.! i == w ] | w <- [0 .. nWP - 1] ]
              in buildMInv n eta wpSizes wpId nWP
            Just stripId ->
              -- Phase 28-2 strip-plot: M = I + η · (Z_WP Z_WPᵀ + Z_Strip Z_Stripᵀ)
              -- block-diagonal にならないので数値 inv で対応
              buildMInvStrip n eta wpId stripId
          xtmx = LA.tr x LA.<> (mInv LA.<> x)
      in critValueM crit (chol xtmx)
      -- 注: critValueM は X (の expand 後) を受け取る前提。 ここで X' M⁻¹ X を
      -- そのまま渡したいので、 X' M⁻¹ X = (M^{-1/2} X)' (M^{-1/2} X) となる行列
      -- X̃ = chol((M⁻¹)) X を作って渡す方が自然。 chol が無いので簡略化:
      -- critValueM の DOpt は det(X'X) = det((M⁻¹) X) ... hm complicated。
      -- ここでは「critValueM をそのまま使うため X̃ = M⁻¹ X として渡し、
      -- DOpt の det(X̃'X̃) = det(X' M⁻¹ M⁻¹ X)」 になり厳密に Goos-Vandebroek の
      -- I_β = X' M⁻¹ X と一致しない。 Phase 25 簡易版として許容、 docs で明記。

-- | [日本語]: strip-plot 用 M⁻¹。 M = I + η · (Z_WP Z_WPᵀ + Z_Strip Z_Stripᵀ)
--   を直接構築し numerical inverse。 strip-plot の covariance は block-diagonal
--   にならない (WP と strip の交差で indicator が重なる) ため、 split-plot 用の
--   解析的 block inverse は使えない。 n は通常 ≤ 100 で inv は十分高速。
--   [English]: M⁻¹ for strip-plot. Directly builds
--   M = I + η · (Z_WP Z_WPᵀ + Z_Strip Z_Stripᵀ) and takes the numerical
--   inverse. Strip-plot covariance is not block-diagonal (indicators
--   overlap where WP and strip intersect), so the analytical block inverse
--   used for split-plot doesn't apply. n is typically ≤ 100, so the inverse
--   is fast enough.
buildMInvStrip :: Int -> Double -> VS.Vector Int -> VS.Vector Int -> LA.Matrix Double
buildMInvStrip n eta wpId stripId =
  let mEntry i j =
        let wpEq    = if wpId VS.! i == wpId VS.! j then eta else 0
            stripEq = if stripId VS.! i == stripId VS.! j then eta else 0
            diag    = if i == j then 1 else 0
        in diag + wpEq + stripEq
      mMat = (n LA.>< n) [ mEntry i j | i <- [0 .. n - 1], j <- [0 .. n - 1] ]
      mD   = LA.det mMat
  in if abs mD < 1e-12
       then LA.ident n   -- safety fallback
       else LA.inv mMat

-- | [日本語]: M⁻¹ を構築 (block-diagonal、 各 WP block で計算)。
--   [English]: Builds M⁻¹ (block-diagonal, computed per WP block).
buildMInv :: Int -> Double -> [Int] -> VS.Vector Int -> Int -> LA.Matrix Double
buildMInv n eta wpSizes wpId _nWP =
  let buildEntry i j
        | wpId VS.! i /= wpId VS.! j = 0
        | otherwise =
            let w   = wpId VS.! i
                nw  = wpSizes !! w
                nwD = fromIntegral nw :: Double
                d   = 1.0
                offDiag = - eta / (1 + eta * nwD)
            in if i == j
                 then d + offDiag   -- diag of inverse: 1 - η/(1+η nw)
                 else offDiag
  in (n LA.>< n) [ buildEntry i j | i <- [0 .. n - 1], j <- [0 .. n - 1] ]

-- | [日本語]: 安全な X̃ を返す: X̃ として「(X' M⁻¹ X) の chol 下三角」 を渡せば
--   det(X̃' X̃) = det(X' M⁻¹ X) になる。
--
--   非 PD 時は LA.chol が IO 例外を投げて bench / 検証が落ちるため、 mbChol で
--   safe 化する。 失敗時は zero matrix を返し、 critValueM DOpt =
--   -det(0 · 0') = 0 を経由して候補が rejection される。
--   [English]: Returns a safe X̃: passing the chol lower-triangle of
--   (X' M⁻¹ X) as X̃ gives det(X̃' X̃) = det(X' M⁻¹ X).
--
--   When non-PD, LA.chol throws an IO exception that would crash the bench
--   / verification, so it's made safe with mbChol. On failure, returns the
--   zero matrix, which rejects the candidate via
--   critValueM DOpt = -det(0 · 0') = 0.
chol :: LA.Matrix Double -> LA.Matrix Double
chol m =
  let !sym = LA.sym m
  in case LA.mbChol sym of
       Just u  -> LA.tr u
       Nothing -> LA.konst 0 (LA.rows m, LA.cols m)

-- ---------------------------------------------------------------------------
-- matrix utility
-- ---------------------------------------------------------------------------

setEntry :: LA.Matrix Double -> Int -> Int -> Double -> LA.Matrix Double
setEntry m i j v = LA.accum m const [((i, j), v)]

setColumnInRows :: LA.Matrix Double -> [Int] -> Int -> Double -> LA.Matrix Double
setColumnInRows m rows j v = LA.accum m const [((i, j), v) | i <- rows]
