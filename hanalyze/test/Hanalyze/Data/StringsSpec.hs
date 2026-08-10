{-# LANGUAGE OverloadedStrings #-}

-- | Hanalyze.Data.Strings の HSpec (Phase 28 Ch14)。
--   R4DS Ch14 の例を一次根拠に str_* の挙動を固定する。
module Hanalyze.Data.StringsSpec (spec) where

import           Test.Hspec
import           Control.Exception  (evaluate)
import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Vector        as V
import qualified DataFrame.Internal.Column    as DF
import qualified DataFrame.Internal.DataFrame  as DF
import qualified DataFrame.Operations.Core     as DF
import           Hanalyze.Data.Strings
import           Hanalyze.DataIO.Convert (getMaybeTextVec)

-- | 純粋値を spine + 要素まで強制して error を IO に持ち上げる (deepseq 非依存)。
forceStrs :: [a] -> (a -> Int) -> IO Int
forceStrs xs f = evaluate (sum (map f xs))

-- | DataFrame の列を @[Maybe Text]@ で読み戻す (テスト検証用)。
colMT :: Text -> DF.DataFrame -> [Maybe Text]
colMT c d = maybe (error ("no col " ++ T.unpack c)) V.toList (getMaybeTextVec c d)

spec :: Spec
spec = do
  describe "strLength / strSub (str_length / str_sub)" $ do
    it "strLength = 文字数" $ do
      strLength "Apple"  `shouldBe` 5
      strLength ""       `shouldBe` 0
    it "strSub: 1 始まり両端含む" $
      strSub 1 3 "Apple" `shouldBe` "App"
    it "strSub: 負 index は末尾から" $ do
      strSub (-3) (-1) "Apple" `shouldBe` "ple"
      strSub (-1) (-1) "Apple" `shouldBe` "e"     -- last char (R4DS 14.5.2)
      strSub 1 1     "Apple"   `shouldBe` "A"      -- first char
    it "strSub: 範囲外はクリップ・逆転は空" $ do
      strSub 1 100 "Pear" `shouldBe` "Pear"
      strSub 3 2   "Pear" `shouldBe` ""

  describe "strC / strCMaybe (str_c・recycling + NA)" $ do
    it "リテラル + 列の recycling" $
      strC [["Hello "], ["Flora","David","Terra"], ["!"]]
        `shouldBe` ["Hello Flora!","Hello David!","Hello Terra!"]
    it "NA 伝播 (R4DS 14.3.1: NA を含む name)" $
      strCMaybe [[Just "Hello "], [Just "Flora", Nothing, Just "Terra"], [Just "!"]]
        `shouldBe` [Just "Hello Flora!", Nothing, Just "Hello Terra!"]
    it "長さ不整合は error" $
      forceStrs (strC [["a","b"], ["x","y","z"]]) strLength `shouldThrow` anyErrorCall

  describe "strFlatten (str_flatten)" $
    it "collapse で結合" $
      strFlatten ", " ["a","b","c"] `shouldBe` "a, b, c"

  describe "strGlue (str_glue)" $ do
    it "{key} を列で置換 (R4DS 14.3.2)" $
      strGlue "Hello {name}!" [("name", ["Flora","David"])]
        `shouldBe` ["Hello Flora!","Hello David!"]
    it "複数 placeholder + リテラル" $
      strGlue "{a}-{b}" [("a",["1","2"]),("b",["x","y"])]
        `shouldBe` ["1-x","2-y"]
    it "{{ }} はリテラル波括弧" $
      strGlue "{{x}}" [] `shouldBe` ["{x}"]
    it "未知 key は error" $
      forceStrs (strGlue "{z}" [("a",["1"])]) strLength `shouldThrow` anyErrorCall

  describe "strToUpper / strSort" $ do
    it "大文字化" $
      strToUpper "abc" `shouldBe` "ABC"
    it "既定 (コードポイント) 昇順" $
      strSort ["banana","apple","cherry"] `shouldBe` ["apple","banana","cherry"]

  describe "separateLongerDelim / separateLongerPosition (§14.4.1)" $ do
    let df = DF.fromNamedColumns
               [ ("x",  DF.fromList ["a,b,c","d,e","f" :: Text])
               , ("id", DF.fromList ["r1","r2","r3" :: Text]) ]
    it "delim: 1 行→複数行に展開し他列を複製 (R4DS df1)" $ do
      let out = separateLongerDelim "x" "," df
      DF.dimensions out `shouldBe` (6, 2)
      colMT "x"  out `shouldBe` map Just ["a","b","c","d","e","f"]
      colMT "id" out `shouldBe` map Just ["r1","r1","r1","r2","r2","r3"]
    it "delim: NA (Nothing) は分割せず 1 行保持" $ do
      let dfn = DF.fromNamedColumns
                  [("x", DF.fromList [Just "a,b", Nothing, Just "c" :: Maybe Text])]
          out = separateLongerDelim "x" "," dfn
      DF.dimensions out `shouldBe` (4, 1)
      colMT "x" out `shouldBe` [Just "a", Just "b", Nothing, Just "c"]
    it "position: width=1 で各文字を 1 行に (R4DS df3)" $ do
      let df3 = DF.fromNamedColumns [("x", DF.fromList ["1211","131","21" :: Text])]
          out = separateLongerPosition "x" 1 df3
      DF.dimensions out `shouldBe` (9, 1)
      colMT "x" out `shouldBe` map (Just . T.singleton) "121113121"
    it "position: width=2 で 2 文字塊・端数は短い塊" $ do
      let df4 = DF.fromNamedColumns [("x", DF.fromList ["abcde","fg" :: Text])]
          out = separateLongerPosition "x" 2 df4
      colMT "x" out `shouldBe` map Just ["ab","cd","e","fg"]

  describe "strEqual / charToRaw (§14.6 Non-English Text)" $ do
    it "strEqual: 合成 ü と 基底+結合 ü は NFC で等価 (R4DS 14.6.2)" $ do
      -- u = c("ü", "ü")
      let u1 = "\x00fc"; u2 = "u\x0308"
      (u1 == u2)        `shouldBe` False   -- 素の比較は不一致
      strEqual u1 u2    `shouldBe` True     -- str_equal は一致
      strLength u1      `shouldBe` 1        -- 14.6.2: 長さは 1
      strLength u2      `shouldBe` 2        --         と 2 で違う
    it "strEqual: 異なる文字は不一致" $
      strEqual "a" "b" `shouldBe` False
    it "charToRaw: UTF-8 バイト列 (R4DS 14.6.1: Hadley)" $
      charToRaw "Hadley" `shouldBe` [0x48,0x61,0x64,0x6c,0x65,0x79]

  describe "separateWiderDelim / separateWiderPosition (§14.4.2-3)" $ do
    it "delim + names: 1 セル→複数列 (R4DS df3)" $ do
      let df3 = DF.fromNamedColumns
                  [("x", DF.fromList ["a10.1.2022","b10.2.2011","e15.1.2015" :: Text])]
          out = separateWiderDelim "x" "." [Just "code", Just "edition", Just "year"] df3
      DF.dimensions out `shouldBe` (3, 3)
      colMT "code"    out `shouldBe` map Just ["a10","b10","e15"]
      colMT "edition" out `shouldBe` map Just ["1","2","1"]
      colMT "year"    out `shouldBe` map Just ["2022","2011","2015"]
    it "delim + names に NA: その piece を捨てる (R4DS)" $ do
      let df3 = DF.fromNamedColumns
                  [("x", DF.fromList ["a10.1.2022","b10.2.2011","e15.1.2015" :: Text])]
          out = separateWiderDelim "x" "." [Just "code", Nothing, Just "year"] df3
      DF.columnNames out `shouldMatchList` ["code","year"]
      colMT "code" out `shouldBe` map Just ["a10","b10","e15"]
      colMT "year" out `shouldBe` map Just ["2022","2011","2015"]
    it "position widths: 固定幅で列分割 (R4DS df4)" $ do
      let df4 = DF.fromNamedColumns
                  [("x", DF.fromList ["202215TX","202122LA","202325CA" :: Text])]
          out = separateWiderPosition "x" [("year",4),("age",2),("state",2)] df4
      colMT "year"  out `shouldBe` map Just ["2022","2021","2023"]
      colMT "age"   out `shouldBe` map Just ["15","22","25"]
      colMT "state" out `shouldBe` map Just ["TX","LA","CA"]

    -- §14.4.3 診断 df: a = c("1-1-1","1-1-2","1-3","1-3-2","1")
    let dfA = DF.fromNamedColumns
                [("a", DF.fromList ["1-1-1","1-1-2","1-3","1-3-2","1" :: Text])]
        nm  = [Just "x", Just "y", Just "z"]
    it "too_few = align_start: 不足は右を NA (R4DS)" $ do
      let out = separateWiderDelimWith "a" "-" nm AlignStart TooManyError dfA
      colMT "x" out `shouldBe` map Just ["1","1","1","1","1"]
      colMT "y" out `shouldBe` [Just "1", Just "1", Just "3", Just "3", Nothing]
      colMT "z" out `shouldBe` [Just "1", Just "2", Nothing, Just "2", Nothing]
    it "too_few = align_end: 不足は左を NA" $ do
      let out = separateWiderDelimWith "a" "-" nm AlignEnd TooManyError dfA
      colMT "x" out `shouldBe` [Just "1", Just "1", Nothing, Just "1", Nothing]
      colMT "y" out `shouldBe` [Just "1", Just "1", Just "1", Just "3", Nothing]
      colMT "z" out `shouldBe` [Just "1", Just "2", Just "3", Just "2", Just "1"]
    it "too_few = error: 不足があれば error" $
      forceStrs [DF.dimensions (separateWiderDelimWith "a" "-" nm TooFewError TooManyError dfA)] fst
        `shouldThrow` anyErrorCall

    -- §14.4.3 too_many df: a = c("1-1-1","1-1-2","1-3-5-6","1-3-2","1-3-5-7-9")
    let dfM = DF.fromNamedColumns
                [("a", DF.fromList ["1-1-1","1-1-2","1-3-5-6","1-3-2","1-3-5-7-9" :: Text])]
    it "too_many = drop: 余剰は捨てる (R4DS)" $ do
      let out = separateWiderDelimWith "a" "-" nm AlignStart DropExtra dfM
      colMT "x" out `shouldBe` map Just ["1","1","1","1","1"]
      colMT "y" out `shouldBe` map Just ["1","1","3","3","3"]
      colMT "z" out `shouldBe` map Just ["1","2","5","2","5"]
    it "too_many = merge: 余剰を最終列に再結合 (R4DS)" $ do
      let out = separateWiderDelimWith "a" "-" nm AlignStart MergeExtra dfM
      colMT "x" out `shouldBe` map Just ["1","1","1","1","1"]
      colMT "y" out `shouldBe` map Just ["1","1","3","3","3"]
      colMT "z" out `shouldBe` map Just ["1","2","5-6","2","5-7-9"]
    it "debug: 診断列 _ok / _pieces / _remainder を付与" $ do
      let out = separateWiderDelimWith "a" "-" nm TooFewError TooManyDebug dfM
      DF.columnNames out `shouldMatchList`
        ["x","y","z","a_ok","a_pieces","a_remainder"]
      colMT "a_remainder" out `shouldBe`
        [Just "", Just "", Just "6", Just "", Just "7-9"]

  describe "正規表現 (§15 Regular expressions・regex-tdfa)" $ do
    it "strDetect: 基本マッチ + \\d 変換 + \\b 単語境界" $ do
      strDetect "an" "banana"            `shouldBe` True
      strDetect "z"  "banana"            `shouldBe` False
      strDetect "\\d" "a1b"              `shouldBe` True   -- \d → [[:digit:]]
      strDetect "\\d" "abc"              `shouldBe` False
      strDetect "[[:digit:]]" "a1b"      `shouldBe` True
      strDetect "\\bcat\\b" "the cat sat" `shouldBe` True
      strDetect "\\bcat\\b" "category"    `shouldBe` False
    it "strDetectWith: ignore_case (§15.5)" $ do
      strDetect "ABC" "xabcy"            `shouldBe` False
      strDetectWith True "ABC" "xabcy"   `shouldBe` True
    it "strCount: マッチ回数" $
      strCount "a" "banana"              `shouldBe` 3
    it "strSubset / strWhich" $ do
      strSubset "^a" ["apple","banana","avocado"] `shouldBe` ["apple","avocado"]
      strWhich  "a"  ["xyz","cat","dog"]          `shouldBe` [2]
    it "strExtract / strExtractAll (\\d+)" $ do
      strExtract    "\\d+" "abc123def45" `shouldBe` Just "123"
      strExtractAll "\\d+" "abc123def45" `shouldBe` ["123","45"]
      strExtract    "\\d+" "no digits"   `shouldBe` Nothing
    it "strMatch: capture groups (whole + groups)" $
      strMatch "([a-z])-([0-9])" "x-5" `shouldBe`
        [Just "x-5", Just "x", Just "5"]
    it "strReplace / strReplaceAll + backref \\1\\2 (§15.3.3 swap)" $ do
      strReplace    "a" "_" "banana"           `shouldBe` "b_nana"
      strReplaceAll "a" "_" "banana"           `shouldBe` "b_n_n_"
      strReplaceAll "([a-z])([a-z])" "\\2\\1" "abcd" `shouldBe` "badc"
    it "strRemove / strRemoveAll" $ do
      strRemove    "a" "banana" `shouldBe` "bnana"
      strRemoveAll "a" "banana" `shouldBe` "bnn"
    it "strSplit (delim + \\s+)" $ do
      strSplit ","     "a,b,c"   `shouldBe` ["a","b","c"]
      strSplit "\\s+"  "a  b c"  `shouldBe` ["a","b","c"]
    it "strLocate: 1 始まり両端含む" $ do
      strLocate "an" "banana" `shouldBe` Just (2,3)
      strLocate "z"  "banana" `shouldBe` Nothing
    it "strEscape: メタ文字をエスケープ" $
      strEscape "a.b+c" `shouldBe` "a\\.b\\+c"
    it "separateWiderRegex: 名前付きグループで列分割 (§15.3.4)" $ do
      let dfR = DF.fromNamedColumns [("x", DF.fromList ["1-2","3-4" :: Text])]
          out = separateWiderRegex "x"
                  [(Just "a", "\\d+"), (Nothing, "-"), (Just "b", "\\d+")] dfR
      DF.columnNames out `shouldMatchList` ["a","b"]
      colMT "a" out `shouldBe` [Just "1", Just "3"]
      colMT "b" out `shouldBe` [Just "2", Just "4"]
