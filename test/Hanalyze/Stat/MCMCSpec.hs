-- | Hanalyze.Stat.MCMC の spec (Phase 92 B4: essBulk の arviz 互換性)。
--
-- golden 値は arviz (pymc312 venv・2026-07-17) の @az.ess(method="bulk")@ で
-- 採取した。テストデータは整数 LCG + AR(1) フィルタで生成する —
-- 整数演算 + IEEE の積和のみなので Python 側と **bit 一致** し、rank が
-- 言語間で入れ替わらないことを保証できる (sin 等の libm 依存を避ける)。
-- 再採取手順は本 spec 末尾のコメント参照。
module Hanalyze.Stat.MCMCSpec (spec) where

import Test.Hspec

import Hanalyze.Stat.MCMC (ess, essBulk)

-- | glibc 系数の LCG (mod 2^31)・[-0.5, 0.5) 一様。Integer 演算なので厳密。
lcg :: Int -> Int -> [Double]
lcg seed n = take n (map toU (drop 1 (iterate step (fromIntegral seed))))
  where
    step x = (1103515245 * x + 12345) `mod` (2 ^ (31 :: Int)) :: Integer
    toU x  = fromIntegral x / 2 ^ (31 :: Int) - 0.5

-- | AR(1): y_i = phi*y_{i-1} + u_i (y_0 起点 0)。積和 2 op のみで言語間 bit 一致。
ar1 :: Int -> Int -> Double -> [Double]
ar1 seed n phi = drop 1 (scanl (\prev u -> phi * prev + u) 0 (lcg seed n))

relClose :: Double -> Double -> Double -> Bool
relClose tol expected actual = abs (actual - expected) <= tol * abs expected

spec :: Spec
spec = do
  describe "essBulk (arviz az.ess(method=\"bulk\") 互換)" $ do
    it "4 chain x 300 (AR(1) phi=0.9) で arviz golden に一致" $ do
      let chains = [ ar1 (c + 1) 300 0.9 | c <- [0 .. 3] ]
      essBulk chains `shouldSatisfy` relClose 1e-6 84.42798772749184

    it "2 chain x 100 (AR(1) phi=0.3) で arviz golden に一致" $ do
      let chains = [ ar1 (c + 10) 100 0.3 | c <- [0 .. 1] ]
      essBulk chains `shouldSatisfy` relClose 1e-6 94.75370782074775

    it "奇数長 3 chain x 101 (split の中央落ち) で arviz golden に一致" $ do
      let chains = [ ar1 (c + 5) 101 0.5 | c <- [0 .. 2] ]
      essBulk chains `shouldSatisfy` relClose 1e-6 126.38779986541799

    it "定数 chain (分散 0) は総 draw 数へフォールバック" $
      essBulk [replicate 50 1.0, replicate 50 1.0] `shouldBe` 100

    it "短すぎる chain (split 後 4 draw 未満) は元の総 draw 数を返す" $
      essBulk [[1, 2, 3]] `shouldBe` 3

    it "独立標本 (iid 一様 4 chain x 500) で arviz golden に一致" $ do
      -- 独立標本では総 draw 数 2000 を多少超える (arviz と同挙動)
      let chains = [ lcg (c + 100) 500 | c <- [0 .. 3] ]
      essBulk chains `shouldSatisfy` relClose 1e-6 2190.946800658338

  describe "ess (既存・単 chain Geyer IMSE)" $
    it "essBulk 導入後も従来挙動 (tau 下限 1 で n 頭打ち) を維持" $ do
      let xs = lcg 7 200
      ess xs `shouldSatisfy` (<= 200.000001)

-- golden 再採取 (pymc312 venv):
--   def lcg(seed,n):
--     x=seed; out=[]
--     for _ in range(n): x=(1103515245*x+12345)%(2**31); out.append(x/2**31-0.5)
--     return out
--   def ar1(seed,n,phi):
--     u=lcg(seed,n); p=0.0; y=[]
--     for ui in u: p=phi*p+ui; y.append(p)
--     return y
--   az.ess(az.convert_to_dataset({"x": np.array([ar1(c+1,300,0.9) for c in range(4)])}), method="bulk")
