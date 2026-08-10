{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns      #-}
-- |
-- Module      : Hanalyze.Design.Custom.Coordinate
-- Description : Custom Design の Coordinate Exchange + Modified Fedorov hybrid アルゴリズム
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Custom Design の Coordinate Exchange + Modified Fedorov hybrid (Phase 24-4)。
--
-- spec: doe-custom-design-spec v0.1.1 §2.4 / §3.6。
-- 参考: Meyer & Nachtsheim (1995) "The Coordinate-Exchange Algorithm for
-- Constructing Exact Optimal Experimental Designs", Technometrics 37:60-69。
--
-- ## アーキテクチャ (24-4)
--
-- 「連続因子は coordinate exchange、 categorical 因子は Modified Fedorov
-- (候補集合 = 全 level)」 を **因子ごとに探索 grid を切り替える** ことで
-- 1 つの outer loop に統合した。 spec §2.4 で言う「hybrid」 は実質
-- per-column grid の選び分けに帰着する。
--
-- 因子ごとの grid (NCoded 想定):
--   * Continuous  : linspace [-1, 1] (長さ dbCxStepGrid、 既定 21)
--   * DiscreteNum : ユーザ指定の離散水準 (そのまま)
--   * Mixture     : linspace [lo, hi] (長さ dbCxStepGrid、 制約は 24-5 で別途)
--   * Categorical : [0, 1, ..., K-1] (level index、 expand 側で treatment coding)
--   * Ordinal     : 同上
--
-- ## 本 commit (24-5) のスコープ
--
--   * 制約 (`cdsConstraints` = LinearIneq / Forbidden / Conditional / RangeBound) を
--     **per-grid-point filter** として統合: 各 cell 候補値について、 変更後の row が
--     全制約を満たさなければ +∞ 評価 (= 採用されない)。
--   * 初期 randomInit は **rejection sampling** で row 単位に制約を満たすまで再抽選
--     (1 row あたり 200 回上限、 越えたら Left)。
--   * `cdsInitial` は **無視** (24-augment phase で対応)。
--   * 全 'OptCriterion' (DOpt/AOpt/IOpt/EOpt/GOpt/Compound) を Matrix-native で評価。
--     IOpt の moment matrix は self-moment (= A-criterion と同方向の近似、
--     既存 `Hanalyze.Design.Optimal.iValueWithSelf` と整合)。
--
-- ## 設計指針
--
--   * 内部 loop は hmatrix Matrix / Vector で完結 (list 化禁止、 Phase 17 教訓)。
--   * outer multi-start / iter loop は `IO` で IORef 更新。
--   * 各 grid 点での criterion 評価は `expandDesignMatrix` + `critValueM`。
--   * 初期解は grid 上で uniform random 抽出 (再現性は `cdsSeed`)。
module Hanalyze.Design.Custom.Coordinate
  ( -- * 入力型
    CustomDesignSpec (..)
  , DesignBudget (..)
  , defaultBudget
    -- * 結果型
  , CustomDesign (..)
  , CustomDesignReport (..)
    -- * アルゴリズム
  , coordinateExchange
  , coordinateExchangePure
    -- * seed / gen helper (SplitPlot 等が再利用)
  , mkGen
  , mkGenSeed
  , defaultPureSeed
    -- * 内部 helper (test 用 / Structured 再利用)
  , critValueM
  , gridForBudget
  , factorGrid
  , rowFeasible
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
import qualified Data.Vector               as V
import qualified Data.Vector.Unboxed       as VU
import qualified Data.Map.Strict           as M

import           Hanalyze.Design.Custom.Factor
import           Hanalyze.Design.Custom.Model
import           Hanalyze.Design.Custom.Constraint
                   (Constraint, FactorValue (..), checkRowAgainst)
import qualified Hanalyze.Design.Custom.RegionMoment as RM
import           Hanalyze.Design.Custom.RegionMoment (resolveIOptRegion)
import           Hanalyze.Design.Optimal        (OptCriterion (..))

-- ---------------------------------------------------------------------------
-- 入力型
-- ---------------------------------------------------------------------------

-- | Custom Design 生成の仕様。 spec §2.4。
data CustomDesignSpec = CustomDesignSpec
  { cdsFactors     :: ![Factor]
  , cdsModel       :: !Model
  , cdsConstraints :: ![Constraint]
    -- ^ Phase 24-3 では未使用 (24-5 で grid filter として統合)。
  , cdsNRuns       :: !Int
  , cdsCriterion   :: !OptCriterion
  , cdsBudget      :: !DesignBudget
  , cdsSeed        :: !(Maybe Int)
  , cdsInitial     :: !(Maybe (LA.Matrix Double))
    -- ^ Augment 用、 24-3 では未使用。
  , cdsDJConvention :: !Bool
    -- ^ Phase 28-12 自動: True かつ criterion が BayesianD を含むとき、
    -- 候補集合 (factor grid の cartesian product) から DuMouchel-Jones §2.2
    -- 規約 ('Custom.Bayesian.djFitTransform') を fit し、 内部 criterion 評価
    -- の expand 後に 'djApplyTransform' を適用してから 'critValueM' に渡す。
    -- 'cdMatrix' は raw 表現のまま保存、 'cdReport.crCriterionValue' は
    -- 変換後 X 上の det を示す。 paper §3.3 と同じ意味の最適化が走る。
  } deriving (Show)

-- | 探索バジェット。 spec §2.4。
data DesignBudget = DesignBudget
  { dbMaxIter    :: !Int     -- ^ outer iteration 上限 (改善なしで break)
  , dbRestarts   :: !Int     -- ^ multi-start 数
  , dbTol        :: !Double  -- ^ outer 収束判定の相対改善閾値
  , dbCxStepGrid :: !Int     -- ^ 連続因子 grid 点数 (既定 21)
  } deriving (Show)

-- | spec §2.4 既定値 + JMP デフォルト互換 (21 grid)。
defaultBudget :: DesignBudget
defaultBudget = DesignBudget
  { dbMaxIter    = 50
  , dbRestarts   = 5
  , dbTol        = 1e-6
  , dbCxStepGrid = 21
  }

-- ---------------------------------------------------------------------------
-- 結果型
-- ---------------------------------------------------------------------------

data CustomDesign = CustomDesign
  { cdMatrix  :: !(LA.Matrix Double)   -- ^ 因子 raw 値行列 (nRuns × #factors)
  , cdFactors :: ![Factor]
  , cdModel   :: !Model
  , cdReport  :: !CustomDesignReport
  } deriving (Show)

data CustomDesignReport = CustomDesignReport
  { crCriterion      :: !OptCriterion
  , crCriterionValue :: !Double     -- ^ 最小化方向の値 (DOpt なら −det)
  , crIterations     :: !Int        -- ^ best restart で要した outer iter 数
  , crRestarts       :: !Int        -- ^ 実行した restart 数
  , crConverged      :: !Bool       -- ^ best restart が maxIter 前に収束したか
  , crSeed           :: !(Maybe Int)
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- 公開 API
-- ---------------------------------------------------------------------------

-- | Coordinate Exchange + Modified Fedorov hybrid による Custom Design 生成。
--
-- 失敗ケース:
--   * 因子が空 / nRuns < 1
--   * Categorical / Ordinal 因子で水準数 0 (Phase 24-1 expandDesignMatrix と整合)
--   * モデルが categorical を参照しているが Phase 24-2 の制限に該当
--   * 'TNested' をモデルに含む (Phase 24-1 から未対応)
--   * dbRestarts < 1 / dbCxStepGrid < 2
-- | seed 由来の gen を作って 'coordinateExchangeWith' を IO で走らせる薄い wrapper。
-- 'cdsSeed' が 'Nothing' の場合のみ entropy 依存 (非決定的)。
-- Phase 78.M: seed 決定的な純粋版が要るなら 'coordinateExchangePure' を使う。
coordinateExchange :: CustomDesignSpec -> IO (Either Text CustomDesign)
coordinateExchange spec = do
  gen <- mkGen (cdsSeed spec)
  coordinateExchangeWith spec gen

-- | seed 決定的な純粋版 (Phase 78.M)。'runST' で MWC gen + MutVar を閉じ込め、
-- IO 無しで 'CustomDesign' を返す。'cdsSeed' が 'Nothing' なら
-- 'defaultPureSeed' を用いて全域関数にする (同 spec → 常に同結果)。
-- 同一 seed なら 'coordinateExchange' (IO) とビット一致する。
coordinateExchangePure :: CustomDesignSpec -> Either Text CustomDesign
coordinateExchangePure spec = runST $ do
  gen <- mkGenSeed (fromMaybe defaultPureSeed (cdsSeed spec))
  coordinateExchangeWith spec gen

-- | 座標交換本体 (PrimMonad 一般化)。IO / ST どちらでも走る。gen は呼び出し側が
-- seed から用意する ('coordinateExchange' = IO entropy 可 / 'coordinateExchangePure'
-- = ST seed 必須)。アルゴリズムは gen の生成源に依らず同 seed → 同結果。
coordinateExchangeWith
  :: PrimMonad m
  => CustomDesignSpec -> MWC.Gen (PrimState m) -> m (Either Text CustomDesign)
coordinateExchangeWith spec gen
  | null (cdsFactors spec) =
      pure (Left (T.pack "coordinateExchange: empty factor list"))
  | cdsNRuns spec < 1 =
      pure (Left (T.pack "coordinateExchange: nRuns must be >= 1"))
  | dbRestarts (cdsBudget spec) < 1 =
      pure (Left (T.pack "coordinateExchange: dbRestarts must be >= 1"))
  | dbCxStepGrid (cdsBudget spec) < 2 =
      pure (Left (T.pack "coordinateExchange: dbCxStepGrid must be >= 2"))
  | otherwise = do
      let !n        = cdsNRuns spec
          !budget   = cdsBudget spec
          !critIn   = cdsCriterion spec
          !model    = cdsModel spec
          !factors  = cdsFactors spec
          !cons     = cdsConstraints spec
          !grids    = map (factorGrid budget) factors
      let prep = do
            () <- maybe (Right ()) Left (validateGrids factors grids)
            crit <- resolveIOptRegion factors model cons critIn
            mDJ  <- fitDJTransformIfRequested spec factors model grids crit
            Right (crit, mDJ)
      case prep of
        Left e -> pure (Left e)
        Right (crit, mDJ) -> do
          let dummy = LA.fromColumns
                [ LA.konst (VU.head g) n | g <- grids ]
          case expandDesignMatrix factors model dummy of
            Left e  -> pure (Left (T.pack "coordinateExchange: model invalid — " <> e))
            Right _ -> do
              bestRef <- newMutVar Nothing
              initErrRef <- newMutVar Nothing
              forM_ [1 .. dbRestarts budget] $ \_ -> do
                mInit <- randomInit factors cons n grids gen
                case mInit of
                  Left e -> writeMutVar initErrRef (Just e)
                  Right init0 -> do
                    (finalM, finalC, iters, conv) <-
                      runExchange factors model crit mDJ cons budget grids init0
                    modifyMutVar' bestRef $ \mb -> case mb of
                      Nothing -> Just (finalM, finalC, iters, conv)
                      Just (_, c0, _, _)
                        | finalC < c0 -> Just (finalM, finalC, iters, conv)
                        | otherwise   -> mb
              mb <- readMutVar bestRef
              case mb of
                Just (m, c, iters, conv) -> pure $ Right CustomDesign
                  { cdMatrix  = m
                  , cdFactors = factors
                  , cdModel   = model
                  , cdReport  = CustomDesignReport
                      { crCriterion      = critIn
                      , crCriterionValue = c
                      , crIterations     = iters
                      , crRestarts       = dbRestarts budget
                      , crConverged      = conv
                      , crSeed           = cdsSeed spec
                      }
                  }
                Nothing -> do
                  initErr <- readMutVar initErrRef
                  pure (Left (case initErr of
                    Just e  -> e
                    Nothing -> T.pack "coordinateExchange: no restart produced a design"))

-- | 全因子の grid が非空である事を確認 (Categorical 0 level、 DiscreteNum 空 等を弾く)。
validateGrids :: [Factor] -> [VU.Vector Double] -> Maybe Text
validateGrids fs gs = go (zip fs gs)
  where
    go [] = Nothing
    go ((f, g):rest)
      | VU.length g < 1 = Just (T.pack
          ("coordinateExchange: factor " <> T.unpack (fName f)
           <> " has empty search grid (categorical with 0 levels?)"))
      | otherwise = go rest

-- ---------------------------------------------------------------------------
-- アルゴリズム内部
-- ---------------------------------------------------------------------------

-- | 1 restart 分の coordinate exchange / Modified Fedorov 混合 loop を走らせる。
-- 戻り値: (最終 raw matrix, 最終 criterion 値, 要した outer iter, 収束フラグ)。
runExchange
  :: PrimMonad m
  => [Factor]
  -> Model
  -> OptCriterion
  -> Maybe RM.DJTransform      -- ^ Phase 28-12: 自動 DJ 規約変換
  -> [Constraint]              -- ^ 制約 (per-grid-point filter)
  -> DesignBudget
  -> [VU.Vector Double]        -- ^ 因子ごとの探索 grid (列順)
  -> LA.Matrix Double          -- ^ 初期 raw matrix (n × p)
  -> m (LA.Matrix Double, Double, Int, Bool)
runExchange factors model crit mDJ cons budget grids init0 = do
  matRef    <- newMutVar init0
  critRef   <- newMutVar (evalCrit factors model crit mDJ init0)
  iterRef   <- newMutVar 0
  convRef   <- newMutVar False
  let !n        = LA.rows init0
      !p        = LA.cols init0
      gridsV    = V.fromList grids
      gridLensV = V.fromList (map VU.length grids)
  let loopOuter !it
        | it > dbMaxIter budget = pure ()
        | otherwise = do
            beforeC <- readMutVar critRef
            forM_ [0 .. n - 1] $ \i ->
              forM_ [0 .. p - 1] $ \j -> do
                curMat <- readMutVar matRef
                curC   <- readMutVar critRef
                let oldV    = curMat `LA.atIndex` (i, j)
                    !g      = gridsV V.! j
                    !gl     = gridLensV V.! j
                (bestV, bestC) <-
                  searchBestOnGrid factors model crit mDJ cons curMat i j g gl oldV curC
                when (bestC < curC) $ do
                  let !newMat = setEntry curMat i j bestV
                  writeMutVar matRef  newMat
                  writeMutVar critRef bestC
            afterC <- readMutVar critRef
            writeMutVar iterRef it
            let !rel = relImprovement beforeC afterC
            if rel <= dbTol budget
              then writeMutVar convRef True
              else loopOuter (it + 1)
  loopOuter 1
  finalM    <- readMutVar matRef
  finalC    <- readMutVar critRef
  finalIter <- readMutVar iterRef
  conv      <- readMutVar convRef
  -- p は randomInit が決定論的に正しい次元を返すので冗長検査は省く
  _ <- pure (n, p)
  pure (finalM, finalC, finalIter, conv)

-- | 1 セル (i, j) について grid 上を線形走査、 制約を満たす範囲で
-- criterion 最小の (v, c) を返す。 制約違反 grid 点は scoring 段階で +∞ 扱い
-- (= 採用されない)。
searchBestOnGrid
  :: PrimMonad m
  => [Factor]
  -> Model
  -> OptCriterion
  -> Maybe RM.DJTransform
  -> [Constraint]
  -> LA.Matrix Double
  -> Int -> Int
  -> VU.Vector Double
  -> Int
  -> Double               -- ^ 現状値 (oldV)
  -> Double               -- ^ 現状の criterion
  -> m (Double, Double)
searchBestOnGrid factors model crit mDJ cons mat i j grid gridLen oldV oldC = do
  bestRef <- newMutVar (oldV, oldC)
  let curRow = LA.flatten (LA.subMatrix (i, 0) (1, LA.cols mat) mat)
  forM_ [0 .. gridLen - 1] $ \k -> do
    let !v = grid VU.! k
        !proposedRow = replaceVecAt curRow j v
    when (rowFeasible factors cons proposedRow) $ do
      let !candMat = setEntry mat i j v
          !c = evalCrit factors model crit mDJ candMat
      modifyMutVar' bestRef $ \cur@(_, bc) -> if c < bc then (v, c) else cur
  readMutVar bestRef

-- | raw matrix → design matrix → (optional) DJ 変換 → criterion 値 (最小化方向)。
-- expandDesignMatrix が `Left` を返したら +∞ を返す (= 採用されない)。
evalCrit :: [Factor] -> Model -> OptCriterion -> Maybe RM.DJTransform
         -> LA.Matrix Double -> Double
evalCrit factors model crit mDJ raw =
  case expandDesignMatrix factors model raw of
    Left _  -> 1 / 0
    Right x ->
      let xT = case mDJ of
            Nothing -> x
            Just t  -> RM.djApplyTransform t x
      in critValueM crit xT

-- ---------------------------------------------------------------------------
-- criterion (Matrix-native、 list 化禁止)
-- ---------------------------------------------------------------------------

-- | OptCriterion の Matrix 版。 全 criterion を /minimize/ 方向で返す
-- (`Hanalyze.Design.Optimal.critValue` と整合)。 X は expand 済設計行列。
critValueM :: OptCriterion -> LA.Matrix Double -> Double
critValueM DOpt       x = - dValueM x
critValueM AOpt       x = aValueM x
critValueM IOpt       x = iValueSelfM x
critValueM EOpt       x = eValueM x
critValueM GOpt       x = gValueM x
critValueM (Compound ws) x =
  sum [ w * critValueM c x | (w, c) <- ws ]
critValueM (BayesianD k) x =
  let p  = LA.cols x
      km = LA.fromLists k
  in if LA.rows km /= p || LA.cols km /= p
       then 1 / 0
       else - LA.det (LA.tr x LA.<> x + km)
critValueM (IOptRegion mr) x =
  let p   = LA.cols x
      mrM = LA.fromLists mr
  in if LA.rows mrM /= p || LA.cols mrM /= p
       then 1 / 0
       else iValueRegionMatrix mrM x

-- | region moment matrix を直接 Matrix で受け取る I-criterion (内部用)。
-- 'Compare.iValueRegionM' と同義だが、 Coordinate からの import 循環回避の
-- ため重複定義。
iValueRegionMatrix :: LA.Matrix Double -> LA.Matrix Double -> Double
iValueRegionMatrix mrM x
  | LA.rows x == 0 = 1 / 0
  | otherwise =
      let xtx = LA.tr x LA.<> x
          d   = LA.det xtx
      in if abs d < 1e-12 then 1 / 0
           else LA.sumElements (LA.takeDiag (LA.inv xtx LA.<> mrM))

dValueM :: LA.Matrix Double -> Double
dValueM x
  | LA.rows x == 0 = 0
  | otherwise = LA.det (LA.tr x LA.<> x)

aValueM :: LA.Matrix Double -> Double
aValueM x
  | LA.rows x == 0 = 1 / 0
  | otherwise =
      let xtx = LA.tr x LA.<> x
          d   = LA.det xtx
      in if abs d < 1e-12 then 1 / 0
           else LA.sumElements (LA.takeDiag (LA.inv xtx))

-- | I-criterion の self-moment 版 (`Optimal.iValueWithSelf` と同義)。
iValueSelfM :: LA.Matrix Double -> Double
iValueSelfM x
  | LA.rows x == 0 = 1 / 0
  | otherwise =
      let xtx = LA.tr x LA.<> x
          d   = LA.det xtx
      in if abs d < 1e-12 then 1 / 0
           else
             let inv    = LA.inv xtx
                 moment = LA.scale (1 / fromIntegral (LA.rows x)) xtx
             in LA.sumElements (LA.takeDiag (inv LA.<> moment))

eValueM :: LA.Matrix Double -> Double
eValueM x
  | LA.rows x == 0 = 1 / 0
  | otherwise =
      let xtx = LA.tr x LA.<> x
          eigs = LA.toList (LA.eigenvaluesSH (LA.sym xtx))
      in if null eigs then 1 / 0 else - minimum eigs

gValueM :: LA.Matrix Double -> Double
gValueM x
  | LA.rows x == 0 = 1 / 0
  | otherwise =
      let xtx = LA.tr x LA.<> x
          d   = LA.det xtx
      in if abs d < 1e-12 then 1 / 0
           else
             let inv = LA.inv xtx
                 h   = x LA.<> inv LA.<> LA.tr x
                 dia = LA.toList (LA.takeDiag h)
             in if null dia then 1 / 0 else maximum dia

-- ---------------------------------------------------------------------------
-- 補助
-- ---------------------------------------------------------------------------

-- | [-1, 1] の等間隔 grid (NCoded 連続因子の既定)。
gridForBudget :: DesignBudget -> VU.Vector Double
gridForBudget b =
  let !k  = dbCxStepGrid b
      !km = fromIntegral (k - 1) :: Double
  in VU.generate k (\i -> -1 + 2 * fromIntegral i / km)

-- | 因子ごとの探索 grid (Phase 24-4)。 raw matrix の値表現規約
-- (`Hanalyze.Design.Custom.Model` のモジュール doc 参照) と整合する点を返す。
--
-- * Continuous (lo, hi)  : linspace [-1, 1] (NCoded、 dbCxStepGrid 点)
-- * DiscreteNum xs       : xs そのまま
-- * Mixture (lo, hi)     : linspace [lo, hi] (dbCxStepGrid 点、 制約は 24-5 で別途)
-- * Categorical / Ordinal: [0, 1, ..., K-1] (level index、 expand 側で treatment coding)
factorGrid :: DesignBudget -> Factor -> VU.Vector Double
factorGrid b f = case fKind f of
  Continuous _ _    -> gridForBudget b
  DiscreteNum xs    -> VU.fromList xs
  Mixture lo hi     -> linspaceVU lo hi (dbCxStepGrid b)
  Categorical xs    -> VU.fromList (map fromIntegral [0 .. length xs - 1])
  Ordinal     xs    -> VU.fromList (map fromIntegral [0 .. length xs - 1])

-- | 任意区間の等間隔 grid (k 点)。 k <= 1 は単一中央値を返す。
linspaceVU :: Double -> Double -> Int -> VU.Vector Double
linspaceVU lo hi k
  | k <= 1    = VU.singleton ((lo + hi) / 2)
  | otherwise = VU.generate k
      (\i -> lo + (hi - lo) * fromIntegral i / fromIntegral (k - 1))

-- | 初期 raw matrix を rejection sampling で構築 (n × p)。
-- 各 row を制約満足するまで再抽選 (1 row あたり 200 回まで)。
-- 200 回試して失敗した row があれば 'Left'。
randomInit
  :: PrimMonad m
  => [Factor]
  -> [Constraint]
  -> Int
  -> [VU.Vector Double]
  -> MWC.Gen (PrimState m)
  -> m (Either Text (LA.Matrix Double))
randomInit factors cons n grids gen = do
  let p = length grids
      gridsV = V.fromList grids
      maxTries = 200 :: Int
      drawRow = do
        vs <- mapM (\j -> do
                       let g = gridsV V.! j
                           gl = VU.length g
                       k <- MWC.uniformR (0, gl - 1) gen
                       pure (g VU.! k)) [0 .. p - 1]
        pure (LA.fromList vs)
      tryRow t
        | t > maxTries = pure Nothing
        | otherwise = do
            r <- drawRow
            if rowFeasible factors cons r
              then pure (Just r)
              else tryRow (t + 1)
  rowsR <- mapM (\_ -> tryRow 1) [1 .. n]
  case sequence rowsR of
    Just rs -> pure (Right (LA.fromRows rs))
    Nothing -> pure (Left (T.pack
      ("randomInit: failed to find feasible row within "
       <> show maxTries <> " rejection-sampling tries — "
       <> "constraints may be infeasible or too tight")))

-- | row vector (length p) の j 番目を v に置換した新 vector。
replaceVecAt :: LA.Vector Double -> Int -> Double -> LA.Vector Double
replaceVecAt v j x =
  LA.fromList [if k == j then x else v `LA.atIndex` k | k <- [0 .. LA.size v - 1]]

-- | row (raw Vector) が全制約を満たすかを評価。
-- Categorical / Ordinal 列は level index → 因子の level 名 ('FVText') に変換、
-- 連続系は 'FVDouble' に変換して 'checkRowAgainst' に渡す。
rowFeasible :: [Factor] -> [Constraint] -> LA.Vector Double -> Bool
rowFeasible _ [] _ = True
rowFeasible factors cons row =
  let m = buildRowFV factors row
  in all (checkRowAgainst m) cons

-- | raw 値 vector (列順 = factors 順) を 因子名 → FactorValue Map に変換。
-- Categorical / Ordinal は level index を level 名 ('FVText') に変換、
-- 非整数 / 範囲外 index は安全のため 'FVDouble' のまま (rowFeasible で
-- 不一致 → 制約違反 として扱われる、 expandDesignMatrix が別途 Left を返す)。
buildRowFV :: [Factor] -> LA.Vector Double -> M.Map Text FactorValue
buildRowFV factors row =
  M.fromList
    [ (fName f, toFV (fKind f) (row `LA.atIndex` i))
    | (i, f) <- zip [0 ..] factors
    ]
  where
    toFV (Categorical xs) x = catIndexToFV xs x
    toFV (Ordinal     xs) x = catIndexToFV xs x
    toFV _                x = FVDouble x

    catIndexToFV :: [Text] -> Double -> FactorValue
    catIndexToFV xs x =
      let xi = round x :: Int
          delta = abs (x - fromIntegral xi)
      in if delta < 1e-9 && xi >= 0 && xi < length xs
           then FVText (xs !! xi)
           else FVDouble x  -- 不正値 → 文字列 level に一致しない = 不一致

-- | accum で 1 セルだけ置換した新 matrix を返す。
-- 注意: hmatrix `LA.accum` の combining fn は @f new old@ の順 (= 第 1 引数が
-- リストの値、 第 2 引数が現行値)。 'const' で「リストの値で置換」 を意味する。
setEntry :: LA.Matrix Double -> Int -> Int -> Double -> LA.Matrix Double
setEntry m i j v = LA.accum m const [((i, j), v)]

-- | 相対改善 = (before − after) / |before| (前後とも最小化方向の criterion 値)。
-- 値が小さい (≤ dbTol) ほど「改善が止まった」 と解釈、 outer loop で break。
relImprovement :: Double -> Double -> Double
relImprovement before after
  | abs before < 1e-12 = before - after
  | otherwise          = (before - after) / abs before

-- | seed から MWC.Gen を作る (IO)。 Nothing なら entropy 由来 (非決定的)。
mkGen :: Maybe Int -> IO MWC.GenIO
mkGen Nothing  = MWC.createSystemRandom
mkGen (Just s) = mkGenSeed s

-- | seed から MWC.Gen を作る (PrimMonad 一般化・決定的)。IO / ST 両対応。
mkGenSeed :: PrimMonad m => Int -> m (MWC.Gen (PrimState m))
mkGenSeed s = MWC.initialize (VU.fromList [fromIntegral s])

-- | 純粋版 'coordinateExchangePure' で 'cdsSeed' が 'Nothing' のときに使う既定 seed。
-- 純粋 = 全域である必要があるため固定値を用いる (同 spec → 常に同結果)。
defaultPureSeed :: Int
defaultPureSeed = 0x5EED

-- ---------------------------------------------------------------------------
-- Phase 28-12 自動 DJ 規約変換
-- ---------------------------------------------------------------------------

-- | criterion 木に BayesianD が含まれているか。
critContainsBayesianD :: OptCriterion -> Bool
critContainsBayesianD (BayesianD _)  = True
critContainsBayesianD (Compound ws)  = any (critContainsBayesianD . snd) ws
critContainsBayesianD _              = False

-- | 因子 grid から候補集合 (cartesian product) の raw matrix を構築。
candidateFromGrids :: [VU.Vector Double] -> LA.Matrix Double
candidateFromGrids gs =
  let lists = map VU.toList gs
      rows  = sequence lists   -- cartesian product
  in if null rows then (0 LA.>< length gs) []
                  else LA.fromLists rows

-- | spec の `cdsDJConvention` が True かつ criterion に BayesianD を含むときのみ
-- 候補集合から 'DJTransform' を fit する。 それ以外は @Right Nothing@。
fitDJTransformIfRequested
  :: CustomDesignSpec
  -> [Factor]
  -> Model
  -> [VU.Vector Double]
  -> OptCriterion
  -> Either Text (Maybe RM.DJTransform)
fitDJTransformIfRequested spec fs model grids crit
  | not (cdsDJConvention spec)        = Right Nothing
  | not (critContainsBayesianD crit)  = Right Nothing
  | otherwise =
      let cand = candidateFromGrids grids
      in case RM.djFitTransform fs model cand of
           Left e  -> Left e
           Right t -> Right (Just t)
