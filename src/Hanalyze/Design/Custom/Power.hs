{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns      #-}
-- |
-- Module      : Hanalyze.Design.Custom.Power
-- Description : Custom Design の設計行列から model term ごとの検出力を直接算出
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Custom Design の設計行列ベース Power Analysis (Phase 24-8)。
--
-- spec: doe-custom-design-spec v0.1.1 §2.8 / §3.5。
--
-- 既存の 'Hanalyze.Design.Power' は ANOVA effect size (Cohen's f) ベースだが、
-- 本モジュールは **生成済の Custom Design の設計行列 X から各 model term の
-- noncentrality λ を直接算出** し、 noncentral F 分布の正規近似で power を返す。
--
-- ## アルゴリズム
--
-- 1. 設計行列 X を `expandDesignMatrix` で取得 (n × p)。 \(M = X^T X\) の
--    逆行列を 1 回計算。
-- 2. 各 model term について、 expand 出力のどの列を占めるかを `termColumns`
--    で特定 (Categorical TMain は K-1 列に展開されるので複数列になる)。
-- 3. effect size β (ユーザ入力) と σ (事前推定) から noncentrality:
--
--    \( \lambda = \frac{1}{\sigma^2} \sum_{j \in \mathrm{cols}} \frac{\beta^2}{(X^T X)^{-1}_{jj}} \)
--
--    これは「全 term 列に同じ true coefficient β が乗る」 + 「直交近似
--    (block-diagonal Σ_J)」 という単純化の下で正確。 非直交ケースでは過大評価
--    気味の近似値となる。 改善 (block-inverse 版) は将来 commit。
-- 4. F 検定 (df1 = #term cols, df2 = n - p) の critical value を 1 - α で取得、
--    Patnaik / 正規近似で `power = 1 - Φ((fCrit*df1 - (df1 + λ)) / sqrt(2(df1 + 2λ)))`。
--    既存 'Hanalyze.Design.Power.powerOneWayAnova' と同手法。
--
-- ## term 名 (ユーザが指定する @[(Text, Double)]@ のキー)
--
--   * @TIntercept@         → @"(Intercept)"@
--   * @TMain "x1"@         → @"x1"@
--   * @TInter ["x1","x2"]@ → @"x1:x2"@ (因子順は元 ADT 通り、 sort しない)
--   * @TPower "x1" k@      → @"x1^k"@
--   * @TNested a b@        → @"a(b)"@
--
-- 該当 term が見つからない場合は 'dpPower = 0' で返す (warning なし、
-- スコア用途のため Left にはしない)。
module Hanalyze.Design.Custom.Power
  ( DesignPower (..)
  , designPower
  , termName
  , termColumnIndices
  ) where

import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Numeric.LinearAlgebra    as LA
import qualified Statistics.Distribution                as SD
import qualified Statistics.Distribution.FDistribution  as FD
import qualified Statistics.Distribution.Normal         as NormalD

import           Hanalyze.Design.Custom.Factor
import           Hanalyze.Design.Custom.Model
import           Hanalyze.Design.Custom.Coordinate (CustomDesign (..))

-- ---------------------------------------------------------------------------
-- 型
-- ---------------------------------------------------------------------------

data DesignPower = DesignPower
  { dpTerm   :: !Text
  , dpEffect :: !Double
  , dpAlpha  :: !Double
  , dpPower  :: !Double
  } deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- 公開 API
-- ---------------------------------------------------------------------------

-- | 各 term の power を算出。 expand 失敗時は全 term で @dpPower = 0@。
designPower
  :: CustomDesign
  -> Double                  -- ^ σ の事前推定
  -> [(Text, Double)]        -- ^ 各 term の effect size β
  -> Double                  -- ^ alpha
  -> [DesignPower]
designPower cd sigma effects alpha =
  case expandDesignMatrix (cdFactors cd) (cdModel cd) (cdMatrix cd) of
    Left _ ->
      [ DesignPower nm eff alpha 0 | (nm, eff) <- effects ]
    Right x ->
      let !n   = LA.rows x
          !p   = LA.cols x
          xtx  = LA.tr x LA.<> x
          d    = LA.det xtx
      in if abs d < 1e-12 || n - p < 1 || sigma <= 0
           then [ DesignPower nm eff alpha 0 | (nm, eff) <- effects ]
           else
             let inv     = LA.inv xtx
                 !diag   = [ inv `LA.atIndex` (j, j) | j <- [0 .. p - 1] ]
                 termMap = termColumnIndices (cdFactors cd) (cdModel cd)
             in [ powerFor termMap diag n p sigma alpha nm eff
                | (nm, eff) <- effects ]

-- | 1 term の power を算出。 該当 term が無ければ power = 0。
powerFor
  :: [(Text, [Int])]
  -> [Double]      -- ^ (X'X)⁻¹ の対角 (column j)
  -> Int           -- ^ n
  -> Int           -- ^ p (model total columns)
  -> Double        -- ^ sigma
  -> Double        -- ^ alpha
  -> Text          -- ^ term name
  -> Double        -- ^ effect size β
  -> DesignPower
powerFor termMap diag n p sigma alpha nm eff =
  case lookup nm termMap of
    Nothing -> DesignPower nm eff alpha 0
    Just []  -> DesignPower nm eff alpha 0
    Just cols ->
      let !df1 = length cols
          !df2 = n - p
          -- noncentrality λ = (β/σ)² · sum_j 1 / (X'X)⁻¹_{jj}
          --   = sum_j β² / (σ² · (X'X)⁻¹_{jj})
          !ncp = sum
            [ (eff * eff) / (sigma * sigma * diag !! j) | j <- cols ]
          fCrit = SD.quantile (FD.fDistribution df1 df2) (1 - alpha)
          -- noncentral F の chi² 正規近似 (powerOneWayAnova と同手法)
          mean1 = fromIntegral df1 + ncp
          var1  = 2 * (fromIntegral df1 + 2 * ncp)
          z     = (fCrit * fromIntegral df1 - mean1) / sqrt var1
          !pw   = 1 - SD.cumulative (NormalD.normalDistr 0 1) z
      in DesignPower nm eff alpha pw

-- ---------------------------------------------------------------------------
-- term 名 + column index の対応
-- ---------------------------------------------------------------------------

-- | term ADT を canonical 名 (Text) に変換。
termName :: ModelTerm -> Text
termName TIntercept     = T.pack "(Intercept)"
termName (TMain nm)     = nm
termName (TInter ns)    = T.intercalate (T.pack ":") ns
termName (TPower nm k)  = nm <> T.pack "^" <> T.pack (show k)
termName (TNested a b)  = a <> T.pack "(" <> b <> T.pack ")"

-- | 各 model term の expand 後 column indices (term 名でルックアップ可)。
-- expandDesignMatrix の列順 (= mTerms 順) と整合。
-- Categorical TMain は K-1 列、 TInter は cartesian product 数の列を占める。
termColumnIndices :: [Factor] -> Model -> [(Text, [Int])]
termColumnIndices factors model = snd (go (mTerms model) 0 [])
  where
    go :: [ModelTerm] -> Int -> [(Text, [Int])] -> (Int, [(Text, [Int])])
    go [] off acc = (off, reverse acc)
    go (t:ts) off acc =
      let w = termWidthOf factors t
          cols = [off .. off + w - 1]
      in go ts (off + w) ((termName t, cols) : acc)

-- | 単一 term の column width (modelNumColumns の per-term 版)。
termWidthOf :: [Factor] -> ModelTerm -> Int
termWidthOf factors t = case t of
  TIntercept -> 1
  TMain n    -> dim n
  TInter ns  -> product (map dim ns)
  TPower _ _ -> 1
  TNested a b -> levelsOf b * dim a  -- Phase 28-1: K_B × (K_A - 1)
  where
    dim n = case lookup n [(fName f, f) | f <- factors] of
      Just f  -> factorDimension f
      Nothing -> 1
    levelsOf n = case lookup n [(fName f, f) | f <- factors] of
      Just f -> case fKind f of
        Categorical xs -> length xs
        Ordinal     xs -> length xs
        _              -> 0
      Nothing -> 0
