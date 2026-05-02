{-# LANGUAGE OverloadedStrings #-}
-- | タグチメソッド (Taguchi method) — 直交表 ('Design.Orthogonal') を
-- ロバスト設計に拡張する解析層。
--
-- 主要要素:
--
-- 1. **SN 比 (Signal-to-Noise ratio)** — 観測のばらつきを定量化:
--    - SmallerBetter:    望小特性 (e.g. 不良率)         η = -10 log₁₀(Σ y²/n)
--    - LargerBetter:     望大特性 (e.g. 強度)            η = -10 log₁₀(Σ (1/y²)/n)
--    - NominalBest:      望目特性 (mean/variance)        η = 10 log₁₀(μ²/σ²)
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
    -- * 要因効果と最適水準
  , FactorEffect (..)
  , analyzeSN
  , optimalLevels
  , predictSN
    -- * 内側/外側配置
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

-- | SN 比の計算規則。タグチが想定する 4 つのケース。
data SNType
  = SmallerBetter
    -- ^ 望小: y → 0 が良い (不良率、誤差、騒音)。η = -10 log₁₀(Σ y²/n)
  | LargerBetter
    -- ^ 望大: y → ∞ が良い (強度、寿命、効率)。η = -10 log₁₀(Σ (1/y²)/n)
  | NominalBest
    -- ^ 望目: 平均が一定 + 分散最小。η = 10 log₁₀(μ²/σ²)
  | NominalBestTarget Double
    -- ^ 望目 (目標値 m 指定): η = -10 log₁₀(Σ(y-m)²/n)
  deriving (Show, Eq)

-- | 表示用の名前。
snTypeName :: SNType -> Text
snTypeName SmallerBetter         = "smaller-the-better"
snTypeName LargerBetter          = "larger-the-better"
snTypeName NominalBest           = "nominal-the-best"
snTypeName (NominalBestTarget m) =
  "nominal-the-best (target=" <> T.pack (printf "%g" m) <> ")"

-- | 1 試行の繰返し観測値 ys から SN 比 η (dB) を計算。
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

-- | (内側試行 × 外側試行) の観測行列に対し、各行 (内側試行) の SN 比を返す。
snRatioRows :: SNType -> [[Double]] -> [Double]
snRatioRows sn = map (snRatio sn)

-- ---------------------------------------------------------------------------
-- 要因効果と最適水準
-- ---------------------------------------------------------------------------

-- | 1 因子の各水準における平均 SN 比。
data FactorEffect = FactorEffect
  { feFactor    :: Text          -- ^ 因子名
  , feLevels    :: [LevelValue]  -- ^ 水準値 (順番に対応)
  , feSNByLevel :: [Double]      -- ^ 各水準での平均 SN 比
  } deriving (Show, Eq)

-- | 内側試行ごとの SN 比から、各因子・各水準の平均 SN 比を計算。
--
-- 各内側試行 i で、因子 j が水準 k のときの SN_i を集めて平均する。
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

-- | 各因子の最良水準 (平均 SN 比が最大の水準) と、そのときの SN 比を返す。
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

-- | 最良水準での予測 SN 比 (主効果のみ加法モデル):
-- η_pred = mean(η_all) + Σ_j (η_best_j − mean(η_all))
predictSN :: [FactorEffect] -> [Double] -> Double
predictSN effects allSN =
  let muAll = if null allSN then 0
              else sum allSN / fromIntegral (length allSN)
      maxPerFactor = [ maximum (feSNByLevel fe) | fe <- effects ]
  in muAll + sum [ best - muAll | best <- maxPerFactor ]

-- ---------------------------------------------------------------------------
-- 内側/外側配置
-- ---------------------------------------------------------------------------

-- | 内側 (制御因子) × 外側 (雑音因子) のクロス設計。
data InnerOuterDesign = InnerOuterDesign
  { ioInner :: AssignedDesign
  , ioOuter :: AssignedDesign
  } deriving (Show, Eq)

makeInnerOuter :: AssignedDesign -> AssignedDesign -> InnerOuterDesign
makeInnerOuter = InnerOuterDesign

-- | クロス設計を CSV 化。各行 = 内側 1 試行、各列 = 内側因子値 + 外側条件分の
-- 観測列 (空)。ユーザーが y_outer1..y_outerM 列を埋めて測定結果を記録する。
-- 別途 outer の試行表を後置する。
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
