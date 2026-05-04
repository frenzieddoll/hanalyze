{-# LANGUAGE OverloadedStrings #-}
-- | The Taguchi method — an analytical layer that extends orthogonal
-- arrays ('Design.Orthogonal') for robust design.
--
-- Main building blocks:
--
-- 1. **Signal-to-Noise ratio (SN)** — quantifies variability:
--
--    * @SmallerBetter@   — smaller-the-better (e.g. defect rate),
--      @η = -10 log₁₀(Σ y²/n)@.
--    * @LargerBetter@    — larger-the-better (e.g. strength),
--      @η = -10 log₁₀(Σ (1/y²)/n)@.
--    * @NominalBest@     — nominal-the-best (mean/variance),
--      @η = 10 log₁₀(μ²/σ²)@.
--    - NominalBestTarget m: 目標値 m への二乗平均偏差    η = -10 log₁₀(Σ (y-m)²/n)
--
-- 2. **内側/外側配置 (Inner/Outer Arrays)** — 制御因子 (内側) と
--    雑音因子 (外側) のクロス設計。各内側試行で外側全条件を観測 → 行ごとに
--    SN 比を計算 → 雑音に頑健な制御因子の組合せを発見。
--
-- 3. **要因効果 (FactorEffect)** — 各因子の各水準での平均 SN 比。
--    最良水準 = 平均 SN 比が最大の水準。
module Design.Taguchi
  ( -- * SN 比
    SNType (..)
  , snTypeName
  , snRatio
  , snRatioRows
    -- * Factor effects and optimal levels
  , FactorEffect (..)
  , analyzeSN
  , optimalLevels
  , predictSN
    -- * Inner/outer arrays
  , InnerOuterDesign (..)
  , makeInnerOuter
  , renderInnerOuterCSV
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf (printf)

import Design.Orthogonal
  ( OA (..)
  , AssignedDesign (..)
  , FactorSpec (..)
  , LevelValue (..)
  )

-- ---------------------------------------------------------------------------
-- SN 比
-- ---------------------------------------------------------------------------

-- | Signal-to-noise ratio rule. Taguchi's four canonical cases.
data SNType
  = SmallerBetter
    -- ^ Smaller-the-better: @y → 0@ is desired (defect rates, errors,
    --   noise). @η = -10 log₁₀(Σ y²/n)@.
  | LargerBetter
    -- ^ Larger-the-better: @y → ∞@ is desired (strength, lifetime,
    --   efficiency). @η = -10 log₁₀(Σ (1/y²)/n)@.
  | NominalBest
    -- ^ Nominal-the-best: hold the mean and minimize variance.
    --   @η = 10 log₁₀(μ²/σ²)@.
  | NominalBestTarget Double
    -- ^ Nominal-the-best with explicit target @m@:
    --   @η = -10 log₁₀(Σ(y - m)²/n)@.
  deriving (Show, Eq)

-- | Display name of an 'SNType'.
snTypeName :: SNType -> Text
snTypeName SmallerBetter         = "smaller-the-better"
snTypeName LargerBetter          = "larger-the-better"
snTypeName NominalBest           = "nominal-the-best"
snTypeName (NominalBestTarget m) =
  "nominal-the-best (target=" <> T.pack (printf "%g" m) <> ")"

-- | Compute the SN ratio @η@ (in dB) from one run's repeated
-- observations.
snRatio :: SNType -> [Double] -> Double
snRatio _    [] = 0
snRatio sn   ys = case sn of
  SmallerBetter ->
    let msd = sum [ y * y | y <- ys ] / fromIntegral n
    in -10 * logBase 10 (max msd epsLog)
  LargerBetter ->
    let msd = sum [ 1 / max (y * y) epsLog | y <- ys ] / fromIntegral n
    in -10 * logBase 10 (max msd epsLog)
  NominalBest ->
    let mu  = sum ys / fromIntegral n
        var = sum [ (y - mu) ^ (2 :: Int) | y <- ys ]
                / fromIntegral (max 1 (n - 1))
    in if var <= 0
         then 0
         else 10 * logBase 10 ((mu * mu) / max var epsLog)
  NominalBestTarget target ->
    let msd = sum [ (y - target) ^ (2 :: Int) | y <- ys ]
                / fromIntegral n
    in -10 * logBase 10 (max msd epsLog)
  where
    n      = length ys
    epsLog = 1e-30   -- 0 で log を取るのを防ぐ

-- | For an @inner-run × outer-run@ observation matrix, return the SN
-- ratio of each inner run.
snRatioRows :: SNType -> [[Double]] -> [Double]
snRatioRows sn = map (snRatio sn)

-- ---------------------------------------------------------------------------
-- 要因効果と最適水準
-- ---------------------------------------------------------------------------

-- | Per-level mean SN ratio for a single factor.
data FactorEffect = FactorEffect
  { feFactor    :: Text          -- ^ Factor name.
  , feLevels    :: [LevelValue]  -- ^ Level values in order.
  , feSNByLevel :: [Double]      -- ^ Mean SN ratio at each level.
  } deriving (Show, Eq)

-- | From the per-inner-run SN ratios, compute the mean SN ratio for
-- every (factor, level) pair.
--
-- For each inner run @i@, gather the @SN_i@ values where factor @j@
-- has level @k@ and average them.
analyzeSN :: AssignedDesign -> [Double] -> [FactorEffect]
analyzeSN ad sns =
  let factors = adFactors ad
      table   = oaTable (adArray ad)
      runs    = zip table sns                    -- (oaRow, sn_i)
  in [ FactorEffect
         { feFactor    = fsName f
         , feLevels    = fsLevels f
         , feSNByLevel = meanByLevel j (length (fsLevels f)) runs
         }
     | (j, f) <- zip [0..] factors ]
  where
    meanByLevel j nLvl runs =
      [ let xs = [ sn | (oaRow, sn) <- runs
                       , length oaRow > j
                       , (oaRow !! j) == k ]
        in if null xs then 0
                      else sum xs / fromIntegral (length xs)
      | k <- [1 .. nLvl] ]

-- | For each factor, the best level (the one with the largest mean SN)
-- together with that SN ratio.
optimalLevels :: [FactorEffect] -> [(Text, LevelValue, Double)]
optimalLevels effects =
  [ let (ix, snBest) = argmax (feSNByLevel fe)
        lvl = if ix < length (feLevels fe)
                then feLevels fe !! ix
                else LText "?"
    in (feFactor fe, lvl, snBest)
  | fe <- effects ]
  where
    argmax xs = foldl1 better (zip [0::Int ..] xs)
    better a@(_, va) b@(_, vb) = if vb > va then b else a

-- | Predicted SN ratio at the best-level combination (main-effects-only
-- additive model):
--
-- @η_pred = mean(η_all) + Σ_j (η_best_j − mean(η_all))@.
predictSN :: [FactorEffect] -> [Double] -> Double
predictSN effects allSN =
  let muAll = if null allSN then 0
              else sum allSN / fromIntegral (length allSN)
      maxPerFactor = [ maximum (feSNByLevel fe) | fe <- effects ]
  in muAll + sum [ best - muAll | best <- maxPerFactor ]

-- ---------------------------------------------------------------------------
-- 内側/外側配置
-- ---------------------------------------------------------------------------

-- | Inner × outer cross design: inner is the control-factor array,
-- outer the noise-factor array.
data InnerOuterDesign = InnerOuterDesign
  { ioInner :: AssignedDesign
  , ioOuter :: AssignedDesign
  } deriving (Show, Eq)

-- | Construct an 'InnerOuterDesign'.
makeInnerOuter :: AssignedDesign -> AssignedDesign -> InnerOuterDesign
makeInnerOuter = InnerOuterDesign

-- | Render the cross design as CSV. Each row corresponds to one inner
-- run; columns hold the inner-factor values followed by empty cells
-- @y_outer1..y_outerM@ for the user to fill in measurements. The outer
-- run table is appended afterwards.
renderInnerOuterCSV :: InnerOuterDesign -> Text
renderInnerOuterCSV io =
  let inner   = ioInner io
      outer   = ioOuter io
      innerN  = length (adRows inner)
      outerN  = length (adRows outer)
      innerHs = map fsName (adFactors inner)
      outerHs = map fsName (adFactors outer)
      yLabels = [ "y_outer" <> T.pack (show (k :: Int)) | k <- [1 .. outerN] ]
      header  = T.intercalate ","
                  ("InnerRun" : innerHs ++ yLabels)
      rows    = [ T.intercalate ","
                    (T.pack (show i)
                     : map fmtLV (adRows inner !! (i - 1))
                     ++ replicate outerN "")
                | i <- [1 .. innerN] ]
      -- 外側表 (参考情報) を末尾に追記
      footer  = "\n# Outer array (noise factors): "
                <> T.intercalate ", " outerHs <> "\n"
                <> T.intercalate "\n"
                     [ "# OuterRun " <> T.pack (show k) <> ": "
                       <> T.intercalate ", "
                            (zipWith (\h v -> h <> "=" <> fmtLV v)
                              outerHs (adRows outer !! (k - 1)))
                     | k <- [1 .. outerN] ]
                <> "\n"
  in header <> "\n" <> T.intercalate "\n" rows <> "\n" <> footer

-- | LevelValue を CSV 用に文字列化。整数値は 150、小数は 0.1 形式。
fmtLV :: LevelValue -> Text
fmtLV (LText t) = t
fmtLV (LNumeric d)
  | d == fromIntegral (round d :: Integer) = T.pack (show (round d :: Integer))
  | otherwise                              = T.pack (printf "%g" d)
