{-# LANGUAGE OverloadedStrings #-}
-- | 直交表 (Orthogonal Arrays / Taguchi 流の Lₙ 表)。
--
-- - 'OA': 直交表の表現 (名前 / 試行数 / 因子数 / 各列水準数 / 表本体)
-- - 'standardArrays': L4 / L8 / L9 / L12 / L16 / L18 の標準表
-- - 'lookupOA': 名前 (\"L9\" 等) で標準表を取得
-- - 'assignFactors': ユーザー因子と水準値を割り当てる
-- - 'renderCSV' / 'renderTSV' / 'renderPretty': 試行表の出力
--
-- 2 水準系 (L8, L16, ...) は @mkL2k@ で生成し、L4 / L9 / L12 / L18
-- は手動定義 (Plackett-Burman / 混合水準は単純な部分集合積では生成不能)。
module Design.Orthogonal
  ( -- * 型
    OA (..)
  , LevelValue (..)
  , FactorSpec (..)
  , AssignedDesign (..)
    -- * 標準表
  , l4
  , l8
  , l9
  , l12
  , l16
  , l18
  , standardArrays
  , lookupOA
  , listArrays
    -- * 2 水準系の生成
  , mkL2k
    -- * 因子割当
  , assignFactors
    -- * 出力
  , renderRawCSV
  , renderRawTSV
  , renderRawPretty
  , renderCSV
  , renderTSV
  , renderPretty
  ) where

import Data.Bits (testBit, popCount, (.&.), bit)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf (printf)

-- ---------------------------------------------------------------------------
-- 型
-- ---------------------------------------------------------------------------

-- | 直交表。試行数 × 列数 の 1-based 水準コード表で表現。
data OA = OA
  { oaName    :: Text       -- ^ \"L9(3^4)\" 等
  , oaRuns    :: Int        -- ^ 試行数
  , oaFactors :: Int        -- ^ 最大因子数 (= 列数)
  , oaLevels  :: [Int]      -- ^ 各列の水準数 (length = oaFactors)
  , oaTable   :: [[Int]]    -- ^ runs × cols, 1-based 水準コード
  } deriving (Show, Eq)

-- | 因子の水準値 (テキストまたは数値)。
data LevelValue = LText Text | LNumeric Double
  deriving (Show, Eq)

-- | ユーザーが指定する因子 (名前と水準値リスト)。
data FactorSpec = FactorSpec
  { fsName   :: Text
  , fsLevels :: [LevelValue]
  } deriving (Show, Eq)

-- | 因子割当後の試行表。
data AssignedDesign = AssignedDesign
  { adArray   :: OA
  , adFactors :: [FactorSpec]
  , adRows    :: [[LevelValue]]
  } deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- 標準表 (手動定義)
-- ---------------------------------------------------------------------------

-- | L4(2³) — 4 試行、最大 3 因子 × 2 水準。
l4 :: OA
l4 = OA "L4(2^3)" 4 3 (replicate 3 2)
  [ [1,1,1]
  , [1,2,2]
  , [2,1,2]
  , [2,2,1]
  ]

-- | L9(3⁴) — 9 試行、最大 4 因子 × 3 水準。
l9 :: OA
l9 = OA "L9(3^4)" 9 4 (replicate 4 3)
  [ [1,1,1,1]
  , [1,2,2,2]
  , [1,3,3,3]
  , [2,1,2,3]
  , [2,2,3,1]
  , [2,3,1,2]
  , [3,1,3,2]
  , [3,2,1,3]
  , [3,3,2,1]
  ]

-- | L12(2¹¹) — 12 試行、最大 11 因子 × 2 水準 (Plackett-Burman)。
-- 主効果のみを想定 (交互作用は全列に分散される)。
l12 :: OA
l12 = OA "L12(2^11)" 12 11 (replicate 11 2)
  [ [1,1,1,1,1,1,1,1,1,1,1]
  , [1,1,1,1,1,2,2,2,2,2,2]
  , [1,1,2,2,2,1,1,1,2,2,2]
  , [1,2,1,2,2,1,2,2,1,1,2]
  , [1,2,2,1,2,2,1,2,1,2,1]
  , [1,2,2,2,1,2,2,1,2,1,1]
  , [2,1,2,2,1,1,2,2,1,2,1]
  , [2,1,2,1,2,2,2,1,1,1,2]
  , [2,1,1,2,2,2,1,2,2,1,1]
  , [2,2,2,1,1,1,1,2,2,1,2]
  , [2,2,1,2,1,2,1,1,1,2,2]
  , [2,2,1,1,2,1,2,1,2,2,1]
  ]

-- | L18(2¹×3⁷) — 18 試行、最大 8 因子 (1 因子は 2 水準、残り 7 因子は 3 水準)。
-- タグチ流で最も推奨される表の一つ。主効果と (列 1)x(列 2) の交互作用を測定可能。
l18 :: OA
l18 = OA "L18(2^1*3^7)" 18 8 (2 : replicate 7 3)
  [ [1,1,1,1,1,1,1,1]
  , [1,1,2,2,2,2,2,2]
  , [1,1,3,3,3,3,3,3]
  , [1,2,1,1,2,2,3,3]
  , [1,2,2,2,3,3,1,1]
  , [1,2,3,3,1,1,2,2]
  , [1,3,1,2,1,3,2,3]
  , [1,3,2,3,2,1,3,1]
  , [1,3,3,1,3,2,1,2]
  , [2,1,1,3,3,2,2,1]
  , [2,1,2,1,1,3,3,2]
  , [2,1,3,2,2,1,1,3]
  , [2,2,1,2,3,1,3,2]
  , [2,2,2,3,1,2,1,3]
  , [2,2,3,1,2,3,2,1]
  , [2,3,1,3,2,3,1,2]
  , [2,3,2,1,3,1,2,3]
  , [2,3,3,2,1,2,3,1]
  ]

-- ---------------------------------------------------------------------------
-- 2 水準系の生成
-- ---------------------------------------------------------------------------

-- | L₍₂^k₎(2^(2^k − 1)) を生成。Taguchi 標準の列順 (col j の値は
-- popCount(j ∧ revBits k r) のパリティ)。
mkL2k :: Int -> OA
mkL2k k =
  OA
    { oaName    = T.pack ("L" ++ show n ++ "(2^" ++ show m ++ ")")
    , oaRuns    = n
    , oaFactors = m
    , oaLevels  = replicate m 2
    , oaTable   = [ [ levelAt r j | j <- [1 .. m] ] | r <- [0 .. n - 1] ]
    }
  where
    n = 2 ^ k
    m = n - 1
    -- Taguchi の標準的な列ラベル順 (col 1 は最上位ビット相当) に合わせるため
    -- 行インデックスをビット反転する。
    revBits :: Int -> Int
    revBits r = sum [ if testBit r i then bit (k - 1 - i) else 0
                    | i <- [0 .. k - 1] ]
    levelAt r j = 1 + (popCount (j .&. revBits r) `mod` 2)

-- | L8(2⁷) — 8 試行、最大 7 因子 × 2 水準 (生成式)。
l8 :: OA
l8 = mkL2k 3

-- | L16(2¹⁵) — 16 試行、最大 15 因子 × 2 水準 (生成式)。
l16 :: OA
l16 = mkL2k 4

-- ---------------------------------------------------------------------------
-- ルックアップ
-- ---------------------------------------------------------------------------

standardArrays :: [OA]
standardArrays = [l4, l8, l9, l12, l16, l18]

lookupOA :: Text -> Maybe OA
lookupOA name0 = case T.toUpper name0 of
  "L4"  -> Just l4
  "L8"  -> Just l8
  "L9"  -> Just l9
  "L12" -> Just l12
  "L16" -> Just l16
  "L18" -> Just l18
  _     -> Nothing

-- | 利用可能な直交表の一覧 (CLI \"doe list\" 用)。
listArrays :: [(Text, Text)]
listArrays = [ (oaName a, descr a) | a <- standardArrays ]
  where
    descr a =
      T.pack (show (oaRuns a)) <> " runs, max "
      <> T.pack (show (oaFactors a)) <> " factors"

-- ---------------------------------------------------------------------------
-- 因子割当
-- ---------------------------------------------------------------------------

-- | 因子と水準値を直交表に割り当て、ユーザー指定の値で展開した試行表を返す。
--
-- - 因子数が表の列数を超えるとエラー
-- - 各因子の水準数が割当先列の水準数と一致しないとエラー
assignFactors :: OA -> [FactorSpec] -> Either Text AssignedDesign
assignFactors oa specs
  | nSpecs > oaFactors oa =
      Left $ "Too many factors: " <> oaName oa
             <> " has only " <> T.pack (show (oaFactors oa)) <> " columns; got "
             <> T.pack (show nSpecs)
  | not (null mismatches) =
      Left $ "Factor level mismatch: " <> T.intercalate "; " mismatches
  | otherwise =
      Right AssignedDesign
        { adArray   = oa
        , adFactors = specs
        , adRows    = [ [ fsLevels (specs !! (j - 1)) !! (lvl - 1)
                        | (j, lvl) <- zip [1 .. nSpecs] (take nSpecs row) ]
                      | row <- oaTable oa ]
        }
  where
    nSpecs    = length specs
    expected  = take nSpecs (oaLevels oa)
    actuals   = map (length . fsLevels) specs
    mismatches =
      [ fsName (specs !! i) <> " expected " <> T.pack (show e)
        <> " levels, got " <> T.pack (show a)
      | (i, (e, a)) <- zip [0..] (zip expected actuals)
      , e /= a ]

-- ---------------------------------------------------------------------------
-- 出力
-- ---------------------------------------------------------------------------

-- | 直交表をそのまま CSV 化 (列名は F1, F2, ...)。
renderRawCSV :: OA -> Text
renderRawCSV oa = renderRawWith "," oa

renderRawTSV :: OA -> Text
renderRawTSV oa = renderRawWith "\t" oa

renderRawWith :: Text -> OA -> Text
renderRawWith sep oa =
  let header = T.intercalate sep
                 [ "F" <> T.pack (show j) | j <- [1 .. oaFactors oa] ]
      body   = T.intercalate "\n"
                 [ T.intercalate sep [ T.pack (show v) | v <- row ]
                 | row <- oaTable oa ]
  in header <> "\n" <> body <> "\n"

-- | 名前付き表を pretty-print (列幅揃え)。
renderRawPretty :: OA -> Text
renderRawPretty oa =
  let names    = "Run" : [ "F" <> T.pack (show j) | j <- [1 .. oaFactors oa] ]
      colWidth = maximum (map T.length names) `max` 3
      pad t    = let n = colWidth - T.length t
                 in T.replicate n " " <> t
      header   = T.intercalate "  " (map pad names)
      body     = T.intercalate "\n"
                   [ T.intercalate "  "
                       (pad (T.pack (show r))
                       : [ pad (T.pack (show v)) | v <- row ])
                   | (r, row) <- zip [1::Int ..] (oaTable oa) ]
  in T.pack (T.unpack (oaName oa)) <> "\n" <> header <> "\n" <> body

-- | 因子割当済み試行表を CSV 化。
renderCSV :: AssignedDesign -> Text
renderCSV = renderWith ","

-- | 因子割当済み試行表を TSV 化。
renderTSV :: AssignedDesign -> Text
renderTSV = renderWith "\t"

renderWith :: Text -> AssignedDesign -> Text
renderWith sep ad =
  let header = T.intercalate sep ("Run" : map fsName (adFactors ad))
      body   = T.intercalate "\n"
                 [ T.intercalate sep (T.pack (show r) : map fmtLevel row)
                 | (r, row) <- zip [1::Int ..] (adRows ad) ]
  in header <> "\n" <> body <> "\n"

fmtLevel :: LevelValue -> Text
fmtLevel (LText t)    = t
fmtLevel (LNumeric d)
  | d == fromIntegral (round d :: Integer) = T.pack (show (round d :: Integer))
  | otherwise                              = T.pack (printf "%g" d)

-- | 因子割当済み試行表を pretty-print。
renderPretty :: AssignedDesign -> Text
renderPretty ad =
  let names      = "Run" : map fsName (adFactors ad)
      cells      =
        [ T.pack (show r) : map fmtLevel row
        | (r, row) <- zip [1::Int ..] (adRows ad) ]
      colWidths  = map (\i -> maximum (map (T.length . safeIx i)
                                       (names : cells)))
                       [0 .. length names - 1]
      safeIx i xs = if i < length xs then xs !! i else ""
      pad i t    = let n = colWidths !! i - T.length t
                   in T.replicate n " " <> t
      fmtRow row = T.intercalate "  "
                     [ pad i (safeIx i row) | i <- [0 .. length names - 1] ]
  in oaName (adArray ad) <> "  (" <> T.pack (show (oaRuns (adArray ad)))
     <> " runs, " <> T.pack (show (length (adFactors ad)))
     <> " of " <> T.pack (show (oaFactors (adArray ad))) <> " columns assigned)\n"
     <> fmtRow names <> "\n"
     <> T.intercalate "\n" (map fmtRow cells)
