{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns      #-}
-- |
-- Module      : Hanalyze.Design.Custom.Compare
-- Description : Custom Design 群の post-hoc 比較 (D/A/G/I efficiency・FDS・alias norm)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: Custom Design の Design Comparison / FDS。
--
-- spec: doe-custom-design-spec v0.1.1 §2.8 / §3.5。
--
-- JMP "Design Evaluation" 相当の post-hoc 比較関数。 生成済の 'CustomDesign'
-- (複数) を受け取り、 D/A/G/I efficiency、 FDS (Fraction of Design Space)
-- 分布、 alias matrix の Frobenius norm を計算する。
--
-- === 設計判断
--
--   - __pure 関数__ (`IO` 不要): FDS の点サンプリングは Halton 列で deterministic、
--     reproducibility は seed 不要。
--   - __efficiency 算出__: D/A/G eff は 'Hanalyze.Design.Diagnostics.diagnostics'
--     を再利用。 I-eff は 'regionMomentMatrixAnalytic' 経由の
--     region 積分版 (連続 U[-1,1] + Categorical 等確率 で M_R を解析構築、
--     I-eff = @1 / (n · trace((X'X)⁻¹ · M_R))@)。 Mixture を含む等で M_R 構築
--     失敗時は旧 self-moment 近似 (= @1/p@) に fallback。
--   - __FDS 規定__: N=500 点を Halton で抽出、 各因子型ごとに [0, 1] → 因子値に
--     マップ (Continuous は [-1, 1]、 DiscreteNum / Mixture / Categorical /
--     Ordinal はそれぞれ自然なマッピング)、 expand 後の予測分散 v = x'(X'X)⁻¹x
--     を昇順 sort して返す (JMP plot で x = 累積分率、 y = v)。
--   - __alias norm の範囲__: 現状は
--     __連続因子 × 連続因子の 2fi で model に含まれていない組合せ__ のみを
--     Z に含める (categorical absent interaction や TPower 2 などは将来
--     commit で拡張)。
--
-- === 既知の制限 (将来 commit で拡張候補)
--
--   - alias matrix の Z 構築範囲 (現状 連続 × 連続 2fi のみ)
--   - FDS の region 定義 (現状全因子 を独立 uniform、 制約付きの場合は
--     rejection sampling 必要)
--   - I-eff の Mixture / 制約付き region (現状 fallback)
--
-- [English]: Custom Design's Design Comparison / FDS.
--
-- spec: doe-custom-design-spec v0.1.1 §2.8 / §3.5.
--
-- Post-hoc comparison functions equivalent to JMP's "Design Evaluation".
-- Takes (multiple) already-generated 'CustomDesign's and computes D/A/G/I
-- efficiency, the FDS (Fraction of Design Space) distribution, and the
-- Frobenius norm of the alias matrix.
--
-- === Design decisions
--
--   - __pure functions__ (no `IO` needed): FDS point sampling is
--     deterministic via a Halton sequence, so reproducibility doesn't
--     need a seed.
--   - __efficiency computation__: D/A/G eff reuse
--     'Hanalyze.Design.Diagnostics.diagnostics'. I-eff is the
--     region-integral version via 'regionMomentMatrixAnalytic'
--     (analytically builds M_R with continuous U[-1,1] + Categorical
--     equal-probability; I-eff = @1 / (n · trace((X'X)⁻¹ · M_R))@). Falls
--     back to the old self-moment approximation (= @1/p@) if building M_R
--     fails (e.g. when Mixture is involved).
--   - __FDS specification__: samples N=500 points via Halton, maps
--     [0, 1] → factor values per factor type (Continuous uses [-1, 1];
--     DiscreteNum \/ Mixture \/ Categorical \/ Ordinal each use their
--     natural mapping), and returns the post-expansion predicted variance
--     v = x'(X'X)⁻¹x sorted ascending (for JMP plots, x = cumulative
--     fraction, y = v).
--   - __scope of the alias norm__: currently only includes in Z
--     __the continuous × continuous 2fi not already in the model__
--     (categorical absent interactions, TPower 2, etc. are extended in a
--     future commit).
--
-- === Known limitations (candidates for extension in a future commit)
--
--   - the scope of the alias matrix's Z construction (currently only
--     continuous × continuous 2fi)
--   - the FDS region definition (currently all factors are independent
--     uniform; the constrained case needs rejection sampling)
--   - I-eff's Mixture \/ constrained region (currently falls back)
module Hanalyze.Design.Custom.Compare
  ( DesignComparison (..)
  , compareDesigns
    -- * 内部 helper (test 用)
  , fdsVector
  , aliasNormOf
    -- * Compound criterion (Phase 26-6)
  , normalizeCompoundWeights
    -- * I-efficiency region 積分 (Phase 28-4a、 RegionMoment 再export)
  , regionMomentMatrixAnalytic
  , iValueRegionM
    -- * Compound 幾何平均 + 各 criterion の efficiency (Phase 28-9)
  , compoundGeometric
  , dEfficiency
  , aEfficiency
    -- * 多変量 Cp との統合 (Phase 28-8)
  , DesignComparisonExt (..)
  , compareDesignsWithResponses
  ) where

import           Data.List                (sort)
import           Data.Text                (Text)
import qualified Numeric.LinearAlgebra    as LA

import           Hanalyze.Design.Custom.Factor
import           Hanalyze.Design.Custom.Model
import           Hanalyze.Design.Custom.Coordinate
                   (CustomDesign (..), CustomDesignReport (..))
import           Hanalyze.Design.Custom.RegionMoment
                   (regionMomentMatrixAnalytic, iValueRegionM)
import           Hanalyze.Design.Diagnostics
                   (DesignDiagnostics (..), diagnostics)
import           Hanalyze.Design.Optimal           (OptCriterion (..))
import qualified Hanalyze.Design.Quality           as Q
import qualified Hanalyze.Stat.QuasiRandom         as QR

-- ---------------------------------------------------------------------------
-- 型
-- ---------------------------------------------------------------------------

-- | [日本語]: 複数 Custom Design の比較結果。
--   [English]: The comparison result of multiple Custom Designs.
data DesignComparison = DesignComparison
  { dcDesigns   :: ![(Text, CustomDesign)]
  , dcEffTable  :: !(LA.Matrix Double)
    -- ^ [日本語]: 行: 設計 (`dcDesigns` 順)、 列: D / A / G / I efficiency
    --   [English]: Rows: designs (in `dcDesigns` order); columns: D / A /
    --   G / I efficiency
  , dcFDS       :: ![(Text, LA.Vector Double)]
    -- ^ [日本語]: 設計ごとの FDS sorted vector (長さ = N_FDS、 既定 500)
    --   [English]: The FDS sorted vector per design (length = N_FDS,
    --   default 500)
  , dcAliasNorm :: ![(Text, Double)]
    -- ^ [日本語]: 設計ごとの alias matrix Frobenius norm
    --   [English]: The Frobenius norm of the alias matrix per design
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- 公開 API
-- ---------------------------------------------------------------------------

nFDS :: Int
nFDS = 500

-- | [日本語]: Custom Design 群を比較。 全 4 列の efficiency + FDS + alias norm を集約。
--   [English]: Compares a group of Custom Designs. Aggregates all 4
--   columns of efficiency + FDS + alias norm.
compareDesigns :: [(Text, CustomDesign)] -> DesignComparison
compareDesigns named =
  let effRows = map (designEffs . snd) named
      effTable
        | null effRows = (0 LA.>< 4) []
        | otherwise    = LA.fromLists effRows
      fds     = [ (nm, fdsVector cd) | (nm, cd) <- named ]
      aliases = [ (nm, aliasNormOf cd) | (nm, cd) <- named ]
  in DesignComparison
       { dcDesigns   = named
       , dcEffTable  = effTable
       , dcFDS       = fds
       , dcAliasNorm = aliases
       }

-- ---------------------------------------------------------------------------
-- efficiency
-- ---------------------------------------------------------------------------

-- | [日本語]: 1 設計に対する [D-eff, A-eff, G-eff, I-eff]。 expand 失敗時は 4 個の 0。
--
--   D-eff は 'CustomDesignReport.crCriterion' に応じて分岐:
--     - 'BayesianD k': @D-eff = (det(X'X + K) / n^p)^(1/p)@ (Bayesian D-criterion)
--     - その他: 古典 D-criterion @det(X'X)@ ベース
--
--   I-eff は region moment matrix 版:
--   @I-eff = 1 / (n · trace((X'X)⁻¹ · M_R))@、 @M_R@ は
--   'regionMomentMatrixAnalytic' で構築。 Mixture を含む等で M_R 構築に
--   失敗した場合は旧 self-moment 近似 (= @1/p@、 設計に依らず定数) に fallback。
--   [English]: [D-eff, A-eff, G-eff, I-eff] for one design. 4 zeros on
--   expand failure.
--
--   D-eff branches according to 'CustomDesignReport.crCriterion':
--     - 'BayesianD k': @D-eff = (det(X'X + K) / n^p)^(1/p)@ (Bayesian D-criterion)
--     - otherwise: based on the classic D-criterion @det(X'X)@
--
--   I-eff is the region moment matrix version:
--   @I-eff = 1 / (n · trace((X'X)⁻¹ · M_R))@, where @M_R@ is built via
--   'regionMomentMatrixAnalytic'. Falls back to the old self-moment
--   approximation (= @1/p@, constant regardless of design) if building
--   M_R fails (e.g. when Mixture is involved).
designEffs :: CustomDesign -> [Double]
designEffs cd =
  case expandDesignMatrix (cdFactors cd) (cdModel cd) (cdMatrix cd) of
    Left _  -> [0, 0, 0, 0]
    Right x ->
      let d        = diagnostics x
          n        = LA.rows x
          p        = LA.cols x
          nD       = fromIntegral n :: Double
          pD       = fromIntegral p :: Double
          dEffBayes k =
            let km = LA.fromLists k
            in if LA.rows km /= p || LA.cols km /= p
                 then ddDEff d
                 else
                   let det = LA.det (LA.tr x LA.<> x + km)
                   in if det <= 0 || nD == 0 then 0
                        else (det / (nD ** pD)) ** (1 / pD)
          dEff = case crCriterion (cdReport cd) of
            BayesianD k -> dEffBayes k
            _           -> ddDEff d
          iEffReg  = case regionMomentMatrixAnalytic (cdFactors cd) (cdModel cd) of
            Right mR
              | LA.rows mR == LA.cols x ->
                  let t  = iValueRegionM mR x
                  in if isInfinite t || t <= 0 then 0 else 1 / (nD * t)
            _ -> ddIEff d
      in [dEff, ddAEff d, ddGEff d, iEffReg]

-- ---------------------------------------------------------------------------
-- FDS (Fraction of Design Space)
-- ---------------------------------------------------------------------------

-- | [日本語]: FDS vector: Halton で region から N_FDS 点を抽出、 各点の予測分散
--   v = x'(X'X)⁻¹x を昇順 sort して返す。 expand 失敗時は空 Vector。
--
--   region の取り方 (暫定):
--     - Continuous (lo, hi)  : [-1, 1] (NCoded、 lo/hi 情報は無視)
--     - DiscreteNum xs       : xs から uniform 抽出
--     - Mixture lo hi        : [lo, hi]
--     - Categorical / Ordinal: 0..K-1 から uniform 抽出
--   [English]: The FDS vector: extracts N_FDS points from the region via
--   Halton, and returns each point's predicted variance
--   v = x'(X'X)⁻¹x sorted ascending. An empty Vector on expand failure.
--
--   How the region is taken (provisional):
--     - Continuous (lo, hi)  : [-1, 1] (NCoded; lo/hi info is ignored)
--     - DiscreteNum xs       : uniform draw from xs
--     - Mixture lo hi        : [lo, hi]
--     - Categorical / Ordinal: uniform draw from 0..K-1
fdsVector :: CustomDesign -> LA.Vector Double
fdsVector cd =
  case expandDesignMatrix (cdFactors cd) (cdModel cd) (cdMatrix cd) of
    Left _  -> LA.fromList []
    Right x ->
      let p   = LA.cols x
          xtx = LA.tr x LA.<> x
          d   = LA.det xtx
      in if abs d < 1e-12
           then LA.fromList []
           else
             let inv     = LA.inv xtx
                 factors = cdFactors cd
                 model   = cdModel cd
                 nF      = length factors
                 halton  = QR.haltonMatrix nFDS nF  -- N × nF in [0, 1]
                 rawRows = LA.fromRows
                   [ LA.fromList
                       [ mapU01ToFactor (factors !! j)
                                        (halton `LA.atIndex` (i, j))
                       | j <- [0 .. nF - 1] ]
                   | i <- [0 .. nFDS - 1] ]
             in case expandDesignMatrix factors model rawRows of
                  Left _  -> LA.fromList []
                  Right xSamp ->
                    let !vs = [ let xi = LA.flatten (xSamp LA.? [i])
                                in xi `LA.dot` (inv LA.#> xi)
                              | i <- [0 .. LA.rows xSamp - 1] ]
                        _ = p  -- 未使用警告対策、 dimension は後の commit で使う
                    in LA.fromList (sort vs)

-- | [日本語]: Halton 1 次元値 u ∈ [0, 1] を 1 因子の raw 値に写像。
--   Categorical / Ordinal は floor(u * K) で level index に量子化。
--   [English]: Maps a 1-dimensional Halton value u ∈ [0, 1] to one
--   factor's raw value. Categorical \/ Ordinal are quantized to a level
--   index via floor(u * K).
mapU01ToFactor :: Factor -> Double -> Double
mapU01ToFactor f u = case fKind f of
  Continuous _ _ -> -1 + 2 * u                       -- [-1, 1]
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

-- ---------------------------------------------------------------------------
-- alias norm
-- ---------------------------------------------------------------------------

-- | [日本語]: alias matrix の Frobenius norm。 Z 範囲を拡張済み:
--
--   - 連続 × 連続 2fi (元実装)
--   - __Categorical × 連続 2fi__ (追加)
--   - __Categorical × Categorical 2fi__ (追加)
--   - __連続因子の TPower k=2 (二乗項)__ (追加)
--
--   すべて model に __含まれていない__ ものだけを Z に追加。 Z が空なら 0。
--   [English]: The Frobenius norm of the alias matrix. The Z scope has
--   been extended:
--
--   - continuous × continuous 2fi (original implementation)
--   - __Categorical × continuous 2fi__ (added)
--   - __Categorical × Categorical 2fi__ (added)
--   - __TPower k=2 (squared term) of continuous factors__ (added)
--
--   Only terms __not already included__ in the model are added to Z. 0
--   if Z is empty.
aliasNormOf :: CustomDesign -> Double
aliasNormOf cd =
  let factors      = cdFactors cd
      model        = cdModel cd
      raw          = cdMatrix cd
      allNames     = map fName factors
      contNames    = [ fName f | f <- factors, factorIsContinuous f ]
      existing2fi  =
        [ canonInter ns | TInter ns <- mTerms model, length ns == 2 ]
      existingPow  =
        [ (n, k) | TPower n k <- mTerms model ]
      -- 2fi candidates: 全因子の組合せ (連続 × 連続、 cat × 連続、 cat × cat)
      pair2fiCands =
        [ TInter (canonInter [a, b])
        | a <- allNames, b <- allNames, a < b
        , canonInter [a, b] `notElem` existing2fi
        ]
      -- 連続因子の二乗項 (k=2)
      powCands =
        [ TPower n 2
        | n <- contNames, (n, 2) `notElem` existingPow
        ]
      zTerms = pair2fiCands ++ powCands
  in if null zTerms
       then 0
       else
         case (expandDesignMatrix factors model raw,
               expandDesignMatrix factors (Model zTerms (mNorm model)) raw) of
           (Right x, Right z) ->
             let xtx = LA.tr x LA.<> x
                 d   = LA.det xtx
             in if abs d < 1e-12 then 0 / 0
                  else
                    let a = LA.inv xtx LA.<> LA.tr x LA.<> z
                    in sqrt (LA.sumElements (LA.cmap (** 2) a))
           _ -> 0 / 0

-- | [日本語]: 2fi のペアを正規化 (順序非依存にするため sort)。
--   [English]: Normalizes a 2fi pair (sorted to make it order-independent).
canonInter :: [Text] -> [Text]
canonInter = sort

-- ---------------------------------------------------------------------------
-- Compound 重み正規化 (Phase 26-6)
-- ---------------------------------------------------------------------------

-- | [日本語]: Compound criterion の重みを合計 1 に正規化。 負の重みは 0 に丸める
--   (criterion の min 方向と矛盾するため)。 入力は @[(weight, OptCriterion)]@、
--   出力は同形で重み正規化済。 重み合計 ≤ 0 の場合は元の入力をそのまま返す
--   (= no-op、 ユーザ責任)。
--
--   使い方:
--
--   @
--   import qualified Hanalyze.Design.Optimal as Opt
--   let ws  = [(0.7, Opt.DOpt), (0.5, Opt.AOpt), (-0.1, Opt.IOpt)]
--       ws' = normalizeCompoundWeights ws
--   -- ws' = [(0.583, DOpt), (0.417, AOpt), (0, IOpt)]
--   @
--   [English]: Normalizes a Compound criterion's weights to sum to 1.
--   Negative weights are clamped to 0 (since they'd contradict the
--   criterion's min direction). The input is @[(weight, OptCriterion)]@;
--   the output has the same shape with weights normalized. If the total
--   weight is ≤ 0, returns the original input unchanged (= no-op, the
--   user's responsibility).
--
--   Usage:
--
--   @
--   import qualified Hanalyze.Design.Optimal as Opt
--   let ws  = [(0.7, Opt.DOpt), (0.5, Opt.AOpt), (-0.1, Opt.IOpt)]
--       ws' = normalizeCompoundWeights ws
--   -- ws' = [(0.583, DOpt), (0.417, AOpt), (0, IOpt)]
--   @
normalizeCompoundWeights
  :: [(Double, a)] -> [(Double, a)]
normalizeCompoundWeights pairs =
  let clamped = [ (max 0 w, c) | (w, c) <- pairs ]
      total   = sum (map fst clamped)
  in if total <= 0
       then pairs
       else [ (w / total, c) | (w, c) <- clamped ]

-- ---------------------------------------------------------------------------
-- Compound 幾何平均 + efficiency 正規化 (Phase 28-9)
-- ---------------------------------------------------------------------------

-- | [日本語]: 重み付き幾何平均 = @exp(Σ w_i · log eff_i / Σ w_i)@。 各 efficiency が
--   [0, 1] 範囲の正規化済値であることを前提に「合成効率」 を返す (alphabetic
--   criterion の geometric variant、 JMP の Compound criterion で利用される)。
--
--     - 重みは正数を仮定 (負は 0 にクランプ)、 重み合計 0 のときは 0 を返す
--     - 任意の eff が ≤ 0 のときは 0 を返す (log が ∞)、 線形 Compound (= no-op
--       合算) と挙動を揃える
--
--   使い方:
--
--   @
--   let effD = dEfficiency x
--       effA = aEfficiency x
--       comp = compoundGeometric [(0.7, effD), (0.3, effA)]
--   @
--   [English]: The weighted geometric mean =
--   @exp(Σ w_i · log eff_i / Σ w_i)@. Returns the "combined efficiency",
--   assuming each efficiency is a normalized value in [0, 1] (the
--   geometric variant of the alphabetic criterion, used in JMP's Compound
--   criterion).
--
--     - Weights are assumed positive (negatives are clamped to 0); returns
--       0 when the total weight is 0
--     - Returns 0 if any eff is ≤ 0 (log would be ∞), matching the
--       behavior of the linear Compound (= no-op summation)
--
--   Usage:
--
--   @
--   let effD = dEfficiency x
--       effA = aEfficiency x
--       comp = compoundGeometric [(0.7, effD), (0.3, effA)]
--   @
compoundGeometric :: [(Double, Double)] -> Double
compoundGeometric pairs
  | any ((<= 0) . snd) clamped = 0
  | totalW <= 0                = 0
  | otherwise                  =
      exp (sum [ w * log e | (w, e) <- clamped ] / totalW)
  where
    clamped = [ (max 0 w, e) | (w, e) <- pairs ]
    totalW  = sum (map fst clamped)

-- | [日本語]: D-efficiency @= (det(X'X) / n^p)^(1/p)@ ([0, ∞) 値、 reference D-opt で 1)。
--   singular なら 0。
--   [English]: D-efficiency @= (det(X'X) / n^p)^(1/p)@ (a [0, ∞) value; 1
--   for a reference D-opt design). 0 if singular.
dEfficiency :: LA.Matrix Double -> Double
dEfficiency x
  | LA.rows x == 0 || LA.cols x == 0 = 0
  | otherwise =
      let n  = fromIntegral (LA.rows x) :: Double
          p  = fromIntegral (LA.cols x) :: Double
          d  = LA.det (LA.tr x LA.<> x)
      in if d <= 0 then 0
                   else (d / (n ** p)) ** (1 / p)

-- | [日本語]: A-efficiency @= p / (n · trace((X'X)⁻¹))@ ([0, ∞)、 reference A-opt で 1)。
--   singular なら 0。
--   [English]: A-efficiency @= p / (n · trace((X'X)⁻¹))@ ([0, ∞); 1 for a
--   reference A-opt design). 0 if singular.
aEfficiency :: LA.Matrix Double -> Double
aEfficiency x
  | LA.rows x == 0 || LA.cols x == 0 = 0
  | otherwise =
      let n   = fromIntegral (LA.rows x) :: Double
          p   = fromIntegral (LA.cols x) :: Double
          xtx = LA.tr x LA.<> x
          dd  = LA.det xtx
      in if abs dd < 1e-12 then 0
           else
             let tr = LA.sumElements (LA.takeDiag (LA.inv xtx))
             in if tr <= 0 then 0 else p / (n * tr)


-- ---------------------------------------------------------------------------
-- 多変量 Cp の Compare 統合 (Phase 28-8)
-- ---------------------------------------------------------------------------

-- | [日本語]: 'DesignComparison' を拡張、 design ごとに観測 response (実験結果 y) と
--   spec bounds から計算した多変量 process capability を追加。
--   [English]: Extends 'DesignComparison', adding the multivariate process
--   capability computed per design from the observed responses
--   (experimental results y) and the spec bounds.
data DesignComparisonExt = DesignComparisonExt
  { dceBase     :: !DesignComparison
  , dceMCp      :: ![(Text, Either Text Double)]
    -- ^ [日本語]: design ごとの MCp (Wang-Hubele-Lawrence 体積比)
    --   [English]: MCp per design (Wang-Hubele-Lawrence volume ratio)
  , dceMCpk     :: ![(Text, Either Text Double)]
    -- ^ [日本語]: design ごとの MCpk (中心オフセット penalty 含む)
    --   [English]: MCpk per design (includes the center-offset penalty)
  , dceInSpec   :: ![(Text, Either Text Double)]
    -- ^ [日本語]: spec box 内包率 (実測)
    --   [English]: The observed spec-box containment rate
  } deriving (Show)

-- | [日本語]: Compare に多変量 response 評価を追加。 各エントリ
--   @(name, design, responses, specs)@:
--     - @responses@ は n × p 観測行列 (n = 設計の行数、 p = 応答数)
--     - @specs@ は各応答の @(LSL, USL)@ を列順に
--
--   @processCapabilityMultivariate@ が Left を返した場合 (singular cov など)
--   は @dceMCp@ / @dceMCpk@ / @dceInSpec@ の該当エントリに Left を保持する。
--   [English]: Adds a multivariate response evaluation to Compare. Each
--   entry @(name, design, responses, specs)@:
--     - @responses@ is an n × p observation matrix (n = the design's row
--       count, p = the number of responses)
--     - @specs@ is each response's @(LSL, USL)@ in column order
--
--   If @processCapabilityMultivariate@ returns Left (e.g. singular
--   covariance), the corresponding entry in @dceMCp@ \/ @dceMCpk@ \/
--   @dceInSpec@ retains the Left.
compareDesignsWithResponses
  :: [(Text, CustomDesign, LA.Matrix Double, [(Double, Double)])]
  -> DesignComparisonExt
compareDesignsWithResponses tuples =
  let base = compareDesigns [ (nm, cd) | (nm, cd, _, _) <- tuples ]
      results =
        [ (nm, Q.processCapabilityMultivariate y specs)
        | (nm, _, y, specs) <- tuples ]
      mcp     = [ (nm, fmap Q.mcMCp r)        | (nm, r) <- results ]
      mcpk    = [ (nm, fmap Q.mcMCpk r)       | (nm, r) <- results ]
      inSpec  = [ (nm, fmap Q.mcInSpecRate r) | (nm, r) <- results ]
  in DesignComparisonExt
       { dceBase   = base
       , dceMCp    = mcp
       , dceMCpk   = mcpk
       , dceInSpec = inSpec
       }
