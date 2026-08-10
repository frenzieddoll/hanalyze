{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hanalyze.Design.Custom.RegionMoment
-- Description : Custom Design の region moment matrix (I-criterion 用の region 積分)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Custom Design の region moment matrix (Phase 28-4)。
--
-- JMP 同等 I-criterion を実装するための region 積分 M_R を解析的に構築する。
--
-- @
--   I(X) = ∫_R f(z)' (X'X)⁻¹ f(z) dz / vol(R)
--        = trace( (X'X)⁻¹ · M_R )
--   M_R = ∫_R f(z) f(z)' dz / vol(R)
-- @
--
-- ## region 規約 (JMP 既定と整合)
--
--   * Continuous: coded @z ∈ U[-1, 1]@ 独立 (raw range は coded 後の前提で無視)
--   * DiscreteNum xs: xs から等確率に抽出 (有限サポート)
--   * Categorical / Ordinal (K 水準): 等確率
--   * Mixture: 非対応 (Phase 28-4a スコープ外、 簡plex 上の積分は 28-9/28-10
--     候補。 'regionMomentMatrixAnalytic' は Left を返す)
--
-- 「Compare / Coordinate のどちらからも import される」 ため、 'CustomDesign'
-- には依存しない (Factor + Model + Optimal のみ依存)。
module Hanalyze.Design.Custom.RegionMoment
  ( regionMomentMatrixAnalytic
  , regionMomentMatrixMC
  , iValueRegionM
  , resolveIOptRegion
    -- * DuMouchel-Jones §2.2 column transform (Phase 28-12)
  , DJTransform (..)
  , djFitTransform
  , djApplyTransform
  , djTransformColumns
  ) where

import           Data.List                (elemIndex)
import qualified Data.Map.Strict          as M
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Numeric.LinearAlgebra    as LA

import           Hanalyze.Design.Custom.Factor
import           Hanalyze.Design.Custom.Model
import           Hanalyze.Design.Custom.Constraint
                   (Constraint, FactorValue (..), checkRowAgainst)
import           Hanalyze.Design.Optimal       (OptCriterion (..))
import qualified Hanalyze.Stat.QuasiRandom as QR

-- ---------------------------------------------------------------------------
-- 列構造記述
-- ---------------------------------------------------------------------------

-- | 1 因子分の expand 後寄与。 連続因子なら指数 k (≥ 1)、 categorical/ordinal
-- なら treatment coding の level index l (1..K-1)。
data FactorContrib
  = ContPow  !Int   -- ^ @z_i^k@、 k ≥ 1
  | CatLevel !Int   -- ^ indicator at level l (1..K-1)
  deriving (Eq, Show)

-- | expand 後 1 列の構造記述: 因子 index → 寄与。 map に無い因子は「無寄与 = 1」。
type ColDesc = M.Map Int FactorContrib

-- | 因子と Model から expand 後の各列の構造記述を 'expandDesignMatrix' と同順で生成。
-- Mixture / TNested は Left。
columnDescriptors :: [Factor] -> Model -> Either Text [ColDesc]
columnDescriptors fs model =
  concat <$> traverse (termDescriptors fs) (mTerms model)

termDescriptors :: [Factor] -> ModelTerm -> Either Text [ColDesc]
termDescriptors _  TIntercept     = Right [M.empty]
termDescriptors fs (TMain n)      = mainDescs fs n
termDescriptors fs (TPower n k)
  | k < 2     = Left (T.pack ("regionMomentMatrixAnalytic: TPower k must be >= 2 (got " <> show k <> ")"))
  | otherwise = do
      (i, f) <- findFactorIdx fs n
      case fKind f of
        Continuous  _ _ -> Right [M.singleton i (ContPow k)]
        DiscreteNum _   -> Right [M.singleton i (ContPow k)]
        Mixture     _ _ -> Left (T.pack ("regionMomentMatrixAnalytic: Mixture factor " <> T.unpack n <> " not supported (Phase 28-4a)"))
        _               -> Left (T.pack ("regionMomentMatrixAnalytic: TPower on categorical/ordinal factor " <> T.unpack n))
termDescriptors fs (TInter ns)
  | null ns   = Left (T.pack "regionMomentMatrixAnalytic: TInter with no factor names")
  | otherwise = foldr1 crossDesc <$> traverse (mainDescs fs) ns
termDescriptors _ (TNested _ _) =
  Left (T.pack "regionMomentMatrixAnalytic: TNested not supported (Phase 28-1 候補)")

-- | 主効果 (= TMain) 相当の寄与記述。 連続 → 1 個 (ContPow 1)、
-- Categorical K 水準 → K-1 個 (CatLevel 1..K-1)、 Mixture → Left。
mainDescs :: [Factor] -> Text -> Either Text [ColDesc]
mainDescs fs n = do
  (i, f) <- findFactorIdx fs n
  case fKind f of
    Continuous  _ _ -> Right [M.singleton i (ContPow 1)]
    DiscreteNum _   -> Right [M.singleton i (ContPow 1)]
    Mixture     _ _ -> Left (T.pack ("regionMomentMatrixAnalytic: Mixture factor " <> T.unpack n <> " not supported (Phase 28-4a)"))
    Categorical xs  -> Right [M.singleton i (CatLevel l) | l <- [1 .. length xs - 1]]
    Ordinal     xs  -> Right [M.singleton i (CatLevel l) | l <- [1 .. length xs - 1]]

crossDesc :: [ColDesc] -> [ColDesc] -> [ColDesc]
crossDesc xs ys = [M.unionWith mergeContrib x y | x <- xs, y <- ys]
  where
    mergeContrib (ContPow a) (ContPow b) = ContPow (a + b)
    mergeContrib a           _           = a

findFactorIdx :: [Factor] -> Text -> Either Text (Int, Factor)
findFactorIdx fs n = case elemIndex n (map fName fs) of
  Nothing -> Left (T.pack ("regionMomentMatrixAnalytic: factor not found: " <> T.unpack n))
  Just i  -> Right (i, fs !! i)

-- ---------------------------------------------------------------------------
-- 解析積分 + I-criterion
-- ---------------------------------------------------------------------------

-- | region moment matrix を解析的に構築。 列順は 'expandDesignMatrix' と一致。
-- Mixture / TNested を含むモデルは Left。 categorical 1 水準等で列数 0 のときは 0×0。
regionMomentMatrixAnalytic
  :: [Factor] -> Model -> Either Text (LA.Matrix Double)
regionMomentMatrixAnalytic fs model = do
  cols <- columnDescriptors fs model
  let p = length cols
  if p == 0
    then Right ((0 LA.>< 0) [])
    else
      let fsArr = zip [0 :: Int ..] fs
          ent i j =
            let ca = cols !! i; cb = cols !! j
            in product
                 [ expectFactorProduct (fKind f) (M.lookup k ca) (M.lookup k cb)
                 | (k, f) <- fsArr ]
      in Right (LA.fromLists
                  [ [ ent i j | j <- [0 .. p - 1] ] | i <- [0 .. p - 1] ])

-- | 単一因子分の期待値 @E[part_a(z) · part_b(z)]@。
expectFactorProduct
  :: FactorKind -> Maybe FactorContrib -> Maybe FactorContrib -> Double
expectFactorProduct kind ma mb = case kind of
  Continuous _ _ -> contMomentPM1 (contPow ma + contPow mb)
  DiscreteNum xs ->
    let s = contPow ma + contPow mb
        n = length xs
    in if n == 0 then 0
                 else sum (map (^^ s) xs) / fromIntegral n
  Categorical xs -> catExp (length xs) (catLvl ma) (catLvl mb)
  Ordinal     xs -> catExp (length xs) (catLvl ma) (catLvl mb)
  Mixture _ _    -> 0 / 0
  where
    contPow Nothing             = 0
    contPow (Just (ContPow k))  = k
    contPow (Just (CatLevel _)) = 0
    catLvl Nothing              = Nothing
    catLvl (Just (CatLevel l))  = Just l
    catLvl (Just (ContPow _))   = Nothing

contMomentPM1 :: Int -> Double
contMomentPM1 p
  | odd p     = 0
  | otherwise = 1 / fromIntegral (p + 1)

catExp :: Int -> Maybe Int -> Maybe Int -> Double
catExp _ Nothing  Nothing            = 1
catExp k (Just _) Nothing            = 1 / fromIntegral k
catExp k Nothing  (Just _)           = 1 / fromIntegral k
catExp k (Just la) (Just lb)
  | la == lb                         = 1 / fromIntegral k
  | otherwise                        = 0

-- ---------------------------------------------------------------------------
-- MC 版 (Phase 28-4c): 制約有り / Mixture / 非 polynomial model 用
-- ---------------------------------------------------------------------------
--
-- Halton quasi-random sequence で region から N 点抽出 (deterministic、 seed 不要)、
-- 'Custom.Constraint.checkRowAgainst' で制約 region 内のみ採用。 採用率が低い
-- 場合 maxAttempts (= 10×N) で打ち切り、 採用数 < N/10 のとき Left。 採用された
-- raw rows を expand → @M_R = X^T X / N_accepted@ で構築。
--
-- 規約 (analytic と共通):
--   * Continuous (lo, hi): coded @z ∈ U[-1, 1]@ 独立 (raw range 無視)
--   * DiscreteNum xs: xs から等確率に抽出
--   * Mixture (lo, hi): @[lo, hi]@ uniform (Halton 1 次元 → 線形写像)
--   * Categorical / Ordinal (K 水準): 等確率

regionMomentMatrixMC
  :: Int              -- ^ 希望サンプル数 N (採用後、 採用率次第で短くなる場合あり)
  -> [Factor]
  -> Model
  -> [Constraint]     -- ^ rejection sampling filter
  -> Either Text (LA.Matrix Double)
regionMomentMatrixMC nWant fs model cons
  | nWant < 1 = Left (T.pack "regionMomentMatrixMC: N must be >= 1")
  | null fs   = Left (T.pack "regionMomentMatrixMC: empty factor list")
  | otherwise =
      let nF          = length fs
          maxAttempts = nWant * 10   -- 採用率 10% 想定の安全係数
          halton      = QR.haltonMatrix maxAttempts nF
          rawAll      =
            [ [ mapU01ToFactorLocal (fs !! j) (halton `LA.atIndex` (i, j))
              | j <- [0 .. nF - 1] ]
            | i <- [0 .. maxAttempts - 1] ]
          accepted = take nWant
                     [ row | row <- rawAll, rowFeasibleLocal fs cons row ]
          nAcc = length accepted
      in if nAcc < max 1 (nWant `div` 10)
           then Left (T.pack ("regionMomentMatrixMC: too few accepted samples ("
                              <> show nAcc <> "/" <> show nWant <> "); 制約 region が極端に狭い可能性"))
           else case expandDesignMatrix fs model (LA.fromLists accepted) of
                  Left e  -> Left e
                  Right x ->
                    let nAccD = fromIntegral nAcc :: Double
                    in Right (LA.scale (1 / nAccD) (LA.tr x LA.<> x))

-- | Halton 1 次元値 u ∈ [0, 1] を 1 因子の raw 値に写像
-- ('Custom.Compare.mapU01ToFactor' と同等、 module cycle 回避で再実装)。
mapU01ToFactorLocal :: Factor -> Double -> Double
mapU01ToFactorLocal f u = case fKind f of
  Continuous _ _ -> -1 + 2 * u
  DiscreteNum xs ->
    let k = length xs
    in if k <= 0 then 0
                 else xs !! min (k - 1) (floor (u * fromIntegral k))
  Mixture lo hi  -> lo + (hi - lo) * u
  Categorical xs ->
    let k = length xs
    in if k <= 0 then 0
                 else fromIntegral (min (k - 1) (floor (u * fromIntegral k)))
  Ordinal xs     ->
    let k = length xs
    in if k <= 0 then 0
                 else fromIntegral (min (k - 1) (floor (u * fromIntegral k)))

-- | 1 row が全制約を満たすか
-- ('Custom.Coordinate.rowFeasible' と同等、 module cycle 回避で再実装)。
rowFeasibleLocal :: [Factor] -> [Constraint] -> [Double] -> Bool
rowFeasibleLocal fs cons row =
  let mkFV f x = case fKind f of
        Categorical xs -> catIdx xs x
        Ordinal     xs -> catIdx xs x
        _              -> FVDouble x
      catIdx xs x =
        let xi = round x :: Int
            d  = abs (x - fromIntegral xi)
        in if d < 1e-9 && xi >= 0 && xi < length xs
             then FVText (xs !! xi)
             else FVDouble x
      rowMap = M.fromList
        [ (fName f, mkFV f v) | (f, v) <- zip fs row ]
  in all (checkRowAgainst rowMap) cons

-- | region moment matrix を用いた I-criterion: @trace((X'X)⁻¹ · M_R)@。
-- 設計行列が rank-deficient (det(X'X) ≈ 0) なら ∞ を返す (minimize 方向)。
iValueRegionM :: LA.Matrix Double -> LA.Matrix Double -> Double
iValueRegionM mR x
  | LA.rows x == 0 = 1 / 0
  | otherwise =
      let xtx = LA.tr x LA.<> x
          d   = LA.det xtx
      in if abs d < 1e-12 then 1 / 0
           else LA.sumElements (LA.takeDiag (LA.inv xtx LA.<> mR))

-- ---------------------------------------------------------------------------
-- IOpt → IOptRegion 解決 (Phase 28-4b)
-- ---------------------------------------------------------------------------

-- | OptCriterion 木を走査し、 'IOpt' を 'IOptRegion mR' に置換する。
-- 'IOptRegion' は once-only に「凍結された M_R」 を持つので、 'Compound' で
-- 入れ子になっていても 1 回 M_R を作って全 IOpt を共有して置換する。
--
-- Phase 28-4c: 制約有り (cons 非空) または Mixture 因子を含むとき、
-- 'regionMomentMatrixAnalytic' は Left を返すため自動で
-- 'regionMomentMatrixMC' (Halton quasi-random、 N=10000) に fallback する。
-- IOpt を含まない criterion は M_R 構築をスキップして Right で原型を返す。
resolveIOptRegion
  :: [Factor] -> Model -> [Constraint] -> OptCriterion
  -> Either Text OptCriterion
resolveIOptRegion fs model cons crit
  | not (containsIOpt crit) = Right crit
  | otherwise = do
      mR <- buildMR
      let mrRows = LA.toLists mR
      pure (rewriteCrit mrRows crit)
  where
    needsMC = not (null cons) || any isMixture fs || containsTNested model
    isMixture f = case fKind f of
      Mixture _ _ -> True
      _           -> False
    containsTNested m = any isTN (mTerms m)
    isTN (TNested _ _) = True
    isTN _             = False

    buildMR
      | needsMC = case regionMomentMatrixMC 10000 fs model cons of
          Right m -> Right m
          Left  e ->
            -- MC が失敗したら analytic を最後の砦として試す (制約無視で粗い近似)
            case regionMomentMatrixAnalytic fs model of
              Right m -> Right m
              Left _  -> Left e
      | otherwise = regionMomentMatrixAnalytic fs model

    containsIOpt IOpt              = True
    containsIOpt (Compound ws)     = any (containsIOpt . snd) ws
    containsIOpt _                 = False

    rewriteCrit mrRows IOpt          = IOptRegion mrRows
    rewriteCrit mrRows (Compound ws) =
      Compound [ (w, rewriteCrit mrRows c) | (w, c) <- ws ]
    rewriteCrit _      c             = c

-- ---------------------------------------------------------------------------
-- DuMouchel-Jones §2.2 column transform (Phase 28-12)
-- ---------------------------------------------------------------------------
--
-- 詳細は doc: src/Hanalyze/Design/Custom/Bayesian.hs (Phase 28-12 section)。
-- Power.termColumnIndices に依存しないよう、 列 index 列挙を本 module 内に
-- 再実装している (Coordinate ↔ Bayesian の module cycle 回避)。

data DJTransform = DJTransform
  { djtPrimaryIdx   :: ![Int]
  , djtPotentialIdx :: ![Int]
  , djtMeanQ        :: !(LA.Vector Double)
  , djtBetaPQ       :: !(LA.Matrix Double)
  , djtScaleQ       :: !(LA.Vector Double)
  } deriving (Show)

isPotentialTerm :: ModelTerm -> Bool
isPotentialTerm TIntercept     = False
isPotentialTerm (TMain _)      = False
isPotentialTerm (TInter ns)    = length ns >= 2
isPotentialTerm (TPower _ k)   = k >= 2
isPotentialTerm (TNested _ _)  = True

-- | 各 term の expand 後 column index 範囲 (Power.termColumnIndices の再実装)。
termColumnIndicesLocal :: [Factor] -> Model -> [(ModelTerm, [Int])]
termColumnIndicesLocal fs model = go (mTerms model) 0
  where
    go [] _ = []
    go (t:ts) off =
      let w = termWidth t
          cols = [off .. off + w - 1]
      in (t, cols) : go ts (off + w)
    termWidth t = case t of
      TIntercept    -> 1
      TMain n       -> dim n
      TInter ns     -> product (map dim ns)
      TPower _ _    -> 1
      TNested a b   -> levelsOf b * dim a   -- Phase 28-1
    dim n = case lookup n [(fName f, f) | f <- fs] of
      Just f  -> factorDimension f
      Nothing -> 1
    levelsOf n = case lookup n [(fName f, f) | f <- fs] of
      Just f -> case fKind f of
        Categorical xs -> length xs
        Ordinal     xs -> length xs
        _              -> 0
      Nothing -> 0

djFitTransform
  :: [Factor] -> Model -> LA.Matrix Double -> Either Text DJTransform
djFitTransform fs model cand = do
  xCand <- expandDesignMatrix fs model cand
  let pairs = termColumnIndicesLocal fs model
      primaryIdx   = concat [ cols | (t, cols) <- pairs, not (isPotentialTerm t) ]
      potentialIdx = concat [ cols | (t, cols) <- pairs, isPotentialTerm t ]
      nC = LA.rows xCand
      nCD = fromIntegral nC :: Double
      q = length potentialIdx
  if q == 0
    then pure DJTransform
           { djtPrimaryIdx   = primaryIdx
           , djtPotentialIdx = []
           , djtMeanQ        = LA.fromList []
           , djtBetaPQ       = (0 LA.>< 0) []
           , djtScaleQ       = LA.fromList []
           }
    else do
      let xP = if null primaryIdx then (nC LA.>< 0) [] else xCand LA.¿ primaryIdx
          xQ = xCand LA.¿ potentialIdx
          meanRow = LA.fromList
            [ LA.sumElements (LA.flatten (xQ LA.¿ [j])) / nCD
            | j <- [0 .. q - 1] ]
          ones    = LA.konst 1 nC :: LA.Vector Double
          xQc = xQ - LA.outer ones meanRow
          betas = if null primaryIdx
                    then (0 LA.>< q) []
                    else
                      let xtxP = LA.tr xP LA.<> xP
                          d = LA.det xtxP
                      in if abs d < 1e-12
                           then LA.konst 0 (LA.cols xP, q)
                           else LA.inv xtxP LA.<> LA.tr xP LA.<> xQc
          xQo = if null primaryIdx then xQc else xQc - xP LA.<> betas
          rangesL =
            [ let v = LA.flatten (xQo LA.¿ [j])
              in LA.maxElement v - LA.minElement v
            | j <- [0 .. q - 1] ]
      pure DJTransform
        { djtPrimaryIdx   = primaryIdx
        , djtPotentialIdx = potentialIdx
        , djtMeanQ        = meanRow
        , djtBetaPQ       = betas
        , djtScaleQ       = LA.fromList rangesL
        }

djApplyTransform :: DJTransform -> LA.Matrix Double -> LA.Matrix Double
djApplyTransform t x
  | null (djtPotentialIdx t) = x
  | otherwise =
      let n   = LA.rows x
          pIdx = djtPrimaryIdx t
          qIdx = djtPotentialIdx t
          xP   = if null pIdx then (n LA.>< 0) [] else x LA.¿ pIdx
          xQ   = x LA.¿ qIdx
          ones = LA.konst 1 n :: LA.Vector Double
          xQc  = xQ - LA.outer ones (djtMeanQ t)
          xQo  = if null pIdx then xQc else xQc - xP LA.<> djtBetaPQ t
          invR = LA.cmap (\r -> if abs r < 1e-12 then 1 else 1 / r) (djtScaleQ t)
          xQf  = xQo LA.<> LA.diag invR
          q    = length qIdx
          col k = LA.flatten (xQf LA.¿ [k])
          potMap  = zip qIdx [0 .. q - 1]
          pickCol i = case lookup i potMap of
            Just k  -> col k
            Nothing -> LA.flatten (x LA.¿ [i])
      in LA.fromColumns [ pickCol i | i <- [0 .. LA.cols x - 1] ]

djTransformColumns
  :: [Factor] -> Model -> LA.Matrix Double -> LA.Matrix Double
  -> Either Text (LA.Matrix Double)
djTransformColumns fs model cand x = do
  t <- djFitTransform fs model cand
  pure (djApplyTransform t x)
