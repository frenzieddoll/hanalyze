{-# LANGUAGE OverloadedStrings #-}

-- | Hanalyze.Data.Factor の HSpec (Phase 28 Ch16 "Factors")。
--   R4DS Ch16 / forcats の挙動を一次根拠に Factor 型の生成・参照を固定する。
module Hanalyze.Data.FactorSpec (spec) where

import           Test.Hspec
import qualified Data.List
import qualified Data.Vector.Unboxed as VU
import           Hanalyze.Data.Factor

spec :: Spec
spec = describe "Data.Factor (A2: 生成 / 参照)" $ do

  describe "factor (R factor() 既定 = ソート済 unique)" $ do
    it "水準は値のソート済 unique" $
      levels (factor ["Dec", "Apr", "Jan", "Mar"]) `shouldBe` ["Apr", "Dec", "Jan", "Mar"]
    it "重複は 1 水準に畳む" $
      levels (factor ["a", "b", "a", "c", "b"]) `shouldBe` ["a", "b", "c"]
    it "コードは 0 始まりで水準 index を指す" $
      facCodes (factor ["b", "a", "b"]) `shouldBe` VU.fromList [1, 0, 1]

  describe "factorWith (水準明示)" $ do
    it "指定 levels の順序を保つ (sort しない)" $ do
      let f = factorWith ["Jan", "Feb", "Mar"] ["Mar", "Jan", "Mar"]
      levels f `shouldBe` ["Jan", "Feb", "Mar"]
      facCodes f `shouldBe` VU.fromList [2, 0, 2]
    it "levels に無い値は NA (naCode)" $
      facCodes (factorWith ["a", "b"] ["a", "z", "b"]) `shouldBe` VU.fromList [0, naCode, 1]

  describe "fct (forcats fct() = 出現順 unique)" $
    it "水準は出現順 (factor() の sort と異なる)" $
      levels (fct ["Dec", "Apr", "Jan", "Mar"]) `shouldBe` ["Dec", "Apr", "Jan", "Mar"]

  describe "ordered (R ordered())" $ do
    it "順序付きフラグが立つ" $
      isOrdered (ordered ["lo", "mid", "hi"] ["mid", "hi"]) `shouldBe` True
    it "factor は既定で非順序" $
      isOrdered (factor ["a", "b"]) `shouldBe` False

  describe "asTexts / asTextsMaybe (as.character())" $ do
    it "コードをラベルに復元" $
      asTexts (factorWith ["a", "b", "c"] ["c", "a", "b"]) `shouldBe` ["c", "a", "b"]
    it "NA は asTexts で \"\"・asTextsMaybe で Nothing" $ do
      let f = factorWith ["a", "b"] ["a", "z"]
      asTexts f      `shouldBe` ["a", ""]
      asTextsMaybe f `shouldBe` [Just "a", Nothing]

  describe "fctCount (dplyr count())" $ do
    it "水準順に頻度・0 件水準も含む" $
      fctCount (factorWith ["a", "b", "c"] ["a", "a", "c"])
        `shouldBe` [("a", 2), ("b", 0), ("c", 1)]
    it "NA は集計から除外" $
      fctCount (factorWith ["a", "b"] ["a", "z", "b", "a"])
        `shouldBe` [("a", 2), ("b", 1)]

  -- A3: 順序操作 (16.4) ----------------------------------------------------
  describe "fctReorder (fct_reorder = x の集約値で昇順)" $ do
    it "各水準の x 集約値の昇順に並べ替える (観測は不変)" $ do
      -- a→[3], b→[1], c→[2] : 集約 (median 相当=identity) 昇順は b,c,a
      let f  = factorWith ["a", "b", "c"] ["a", "b", "c"]
          f' = fctReorder medianD f [3, 1, 2]
      levels f' `shouldBe` ["b", "c", "a"]
      -- どの観測がどの水準かは保存される
      asTexts f' `shouldBe` ["a", "b", "c"]
    it "1 水準に複数値があれば集約関数を適用" $
      levels (fctReorder medianD (factorWith ["a", "b"] ["a", "a", "b"]) [10, 20, 5])
        `shouldBe` ["b", "a"]  -- a の median 15 > b の 5

  describe "fctRelevel (fct_relevel = 指定水準を先頭)" $ do
    it "指定水準を先頭へ・残りは相対順序保持" $
      levels (fctRelevel ["c"] (factorWith ["a", "b", "c", "d"] []))
        `shouldBe` ["c", "a", "b", "d"]
    it "複数指定は指定順で先頭に並ぶ" $
      levels (fctRelevel ["d", "b"] (factorWith ["a", "b", "c", "d"] []))
        `shouldBe` ["d", "b", "a", "c"]
    it "存在しない水準名は無視" $
      levels (fctRelevel ["z", "b"] (factorWith ["a", "b", "c"] []))
        `shouldBe` ["b", "a", "c"]

  describe "fctReorder2 (fct_reorder2 = 最大 x での y で降順)" $
    it "各水準の最大 x に対応する y の降順" $ do
      -- a: x=[1,3] y=[9,1] → max x=3 の y=1 ; b: x=[2] y=[5] → 5
      let f = factorWith ["a", "b"] ["a", "a", "b"]
      levels (fctReorder2 f [1, 3, 2] [9, 1, 5]) `shouldBe` ["b", "a"]

  describe "fctInfreq (fct_infreq = 頻度降順)" $ do
    it "出現頻度の降順" $
      levels (fctInfreq (factorWith ["a", "b", "c"] ["a", "b", "b", "c", "c", "c"]))
        `shouldBe` ["c", "b", "a"]
    it "同頻度は元の水準順を保つ (安定)" $
      levels (fctInfreq (factorWith ["a", "b", "c"] ["a", "b", "c"]))
        `shouldBe` ["a", "b", "c"]

  describe "fctRev (fct_rev = 水準逆順)" $
    it "水準を逆順にする (観測は不変)" $ do
      let f' = fctRev (factorWith ["a", "b", "c"] ["a", "c"])
      levels f'  `shouldBe` ["c", "b", "a"]
      asTexts f' `shouldBe` ["a", "c"]

  -- A4: 水準操作 (16.5) ----------------------------------------------------
  describe "fctRecode (fct_recode = ラベル改名)" $ do
    it "ラベルを改名・未言及は不変・順序保持" $ do
      let f' = fctRecode [("A", "a"), ("C", "c")] (factorWith ["a", "b", "c"] ["a", "b", "c"])
      levels f'  `shouldBe` ["A", "b", "C"]
      asTexts f' `shouldBe` ["A", "b", "C"]
    it "複数の旧を同じ新に向けると併合される" $ do
      let f' = fctRecode [("X", "a"), ("X", "b")] (factorWith ["a", "b", "c"] ["a", "b", "c", "a"])
      levels f'  `shouldBe` ["X", "c"]
      asTexts f' `shouldBe` ["X", "X", "c", "X"]

  describe "fctCollapse (fct_collapse = 複数水準を併合)" $
    it "(新,[旧]) で併合・未言及は残す" $ do
      let f' = fctCollapse [("other", ["b", "c"])] (factorWith ["a", "b", "c", "d"] ["a", "b", "c", "d"])
      levels f'  `shouldBe` ["a", "other", "d"]
      asTexts f' `shouldBe` ["a", "other", "other", "d"]

  describe "fctLumpN (fct_lump_n = 上位 n 残し他を Other)" $ do
    it "上位 n 水準を残し他を Other (末尾)" $ do
      -- 頻度: a=3,b=2,c=1,d=1 → 上位2 = a,b
      let f  = factorWith ["a", "b", "c", "d"] ["a", "a", "a", "b", "b", "c", "d"]
          f' = fctLumpN 2 f
      levels f' `shouldBe` ["a", "b", "Other"]
      fctCount f' `shouldBe` [("a", 3), ("b", 2), ("Other", 2)]
    it "n が水準数以上なら Other を作らない" $
      levels (fctLumpN 9 (factorWith ["a", "b"] ["a", "b"])) `shouldBe` ["a", "b"]

  describe "fctLumpMin (fct_lump_min = 回数<min を Other)" $
    it "出現回数 < min を Other へ" $ do
      let f  = factorWith ["a", "b", "c"] ["a", "a", "a", "b", "b", "c"]
          f' = fctLumpMin 2 f   -- c(1) のみ < 2
      levels f'   `shouldBe` ["a", "b", "Other"]
      fctCount f' `shouldBe` [("a", 3), ("b", 2), ("Other", 1)]

  describe "fctLumpProp (fct_lump_prop = 割合<prop を Other)" $
    it "出現割合 < prop を Other へ" $ do
      -- total=10: a=6(0.6),b=3(0.3),c=1(0.1)。prop=0.2 → c のみ lump
      let f  = factorWith ["a", "b", "c"] (replicate 6 "a" ++ replicate 3 "b" ++ ["c"])
          f' = fctLumpProp 0.2 f
      levels f'   `shouldBe` ["a", "b", "Other"]
      fctCount f' `shouldBe` [("a", 6), ("b", 3), ("Other", 1)]

  describe "fctLumpLowfreq (fct_lump_lowfreq = Other が最小のまま低頻度を併合)" $
    it "降順で残り合計を上回る水準以降を Other に" $ do
      -- 頻度 a=100,b=5,c=3,d=2 → a > 10 で cut=1、b,c,d を Other(=10<100)
      let f  = factorWith ["a", "b", "c", "d"]
                 (replicate 100 "a" ++ replicate 5 "b" ++ replicate 3 "c" ++ replicate 2 "d")
          f' = fctLumpLowfreq f
      levels f'   `shouldBe` ["a", "Other"]
      fctCount f' `shouldBe` [("a", 100), ("Other", 10)]

-- | テスト用 median (偶数個は中央 2 値平均)。
medianD :: [Double] -> Double
medianD xs =
  let s = Data.List.sort xs
      n = length s
  in if odd n then s !! (n `div` 2)
     else (s !! (n `div` 2 - 1) + s !! (n `div` 2)) / 2
