{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
-- | DOE demo: RSM の予測精度 vs 実験数 (= 必要実験数の見極め)。
--
-- 温度 (3 水準・Num)・エネルギー (3 水準・Num)・角度 (連続・Cont) の
-- 二次応答曲面を対象に:
--   1. @customDesign@ (二次モデル) で n-run の D-最適設計を作る
--   2. 既知の真の曲面 + Gaussian noise で応答を sim
--   3. 二次 OLS を fit
--   4. 独立テスト集合で 真の曲面 vs 予測 の RMSE を測る (設計シード × noise 反復で平均)
--   5. n を振り、 hgg で「RMSE vs n」の折れ線 (noise sd 参照線つき) を SVG 出力
--
-- 真の曲面には二次モデルで表せない @0.8·angle³@ を混ぜてあり、 n を増やしても
-- 消えない bias 床を作る (= 実務の「これ以上は実験でなくモデルを増やせ」を再現)。
module Main where

import qualified Data.Vector as V
import qualified Numeric.LinearAlgebra as LA
import qualified Data.Text as T
import           Data.Text (Text)
import           Text.Printf (printf)
import           System.Directory (createDirectoryIfMissing)

import           Hanalyze.Plot
                   ( customDesign, customSpec, numFactor, contFactor, quadratic, designFrame
                   , designTable, designModel, profiler, toPlot, (|->) )
import           Hanalyze.DataIO.Convert (getDoubleVec)
import           Hgg.Plot.Spec
                   ( ColData (..), layer, linePoints, scatterPoints, Point2 (..)
                   , color, markWidth
                   , refHorizontal, title, subtitle, xLabel, yLabel, width, height
                   , theme, ThemeName (..) )
import           Hgg.Plot.Frame ((|>>))
import           Hgg.Plot.Color (fromHex)
import           Hgg.Plot.Backend.SVG (saveSVG, saveSVGBound)

-- === 真の応答曲面とモデル ==================================================

-- 正規化座標 (各 ~[-1,1])。
ut, ue, ua :: Double -> Double
ut t = (t - 165) / 15
ue e = (e - 20) / 10
ua a = (a - 45) / 45

-- 真の曲面 = 二次 + 0.8·angle³ (二次モデルでは捉えられない bias 源)。
gTrue :: Double -> Double -> Double -> Double
gTrue t e a =
  let x = ut t; y = ue e; z = ua a
  in 50 + 8*x + 5*y + 6*z - 4*x*x - 3*y*y - 5*z*z + 2*x*y + 1.5*x*z - 1*y*z
       + 0.8*z*z*z

-- 二次モデルの特徴ベクトル (10 項: 切片 + 主 3 + 二乗 3 + 交互 3)。
feat :: Double -> Double -> Double -> [Double]
feat t e a = let x = ut t; y = ue e; z = ua a
             in [1, x, y, z, x*x, y*y, z*z, x*y, x*z, y*z]

noiseSd :: Double
noiseSd = 1.5

-- === 決定的 PRNG → Gaussian (Box-Muller) ==================================

lcg :: Int -> Int
lcg s = (1103515245 * s + 12345) `mod` 2147483648

u01 :: Int -> Double
u01 s = fromIntegral s / 2147483648

gaussians :: Int -> [Double]
gaussians seed = go (lcg seed)
  where
    go s = let s1 = lcg s; s2 = lcg s1
               u1 = max 1e-12 (u01 s1); u2 = u01 s2
           in sqrt (-2 * log u1) * cos (2 * pi * u2) : go s2

-- === 設計・fit・評価 ======================================================

-- 温度/エネルギー = 3 水準 Num、 角度 = 連続。 二次モデルで D-最適設計。
design n dseed = customDesign (customSpec
  [ numFactor "temp"   [150, 165, 180]
  , numFactor "energy" [10, 20, 30]
  , contFactor "angle" (0, 90) ]
  (quadratic ["temp", "energy", "angle"]) n dseed)

-- 設計 (designFrame) から (temp, energy, angle) 実値の行を取り出す。
rowsOf :: Int -> Int -> [(Double, Double, Double)]
rowsOf n dseed =
  let df = designFrame (design n dseed)
      col :: Text -> [Double]
      col c = maybe (error (T.unpack c)) V.toList (getDoubleVec c df)
  in zip3 (col "temp") (col "energy") (col "angle")

-- 固定・独立なテスト集合 (2000 点): 離散 temp/energy + 連続 angle。
testSet :: [(Double, Double, Double)]
testSet = take 2000
  [ ([150,165,180] !! i, [10,20,30] !! j, 90 * u01 (lcg (lcg (7919*k + 13))))
  | (i, j, k) <- zip3 (cycle [0,1,2,1,0,2,2,0,1]) (cycle [1,2,0,2,1,0,1,2,0]) [1 ..] ]

-- OLS: beta = pinv(X) y。
ols :: [[Double]] -> [Double] -> [Double]
ols xs ys = LA.toList (LA.flatten (LA.pinv (LA.fromLists xs) LA.<> LA.asColumn (LA.fromList ys)))

-- 1 fit (設計シード dseed・noise 反復 rep) の テスト RMSE。
rmseFor :: Int -> Int -> Int -> Double
rmseFor n dseed rep =
  let rws  = rowsOf n dseed
      xs   = [ feat t e a | (t, e, a) <- rws ]
      ys   = [ gTrue t e a + noiseSd * z
             | ((t, e, a), z) <- zip rws (gaussians (n*1000 + dseed*37 + rep)) ]
      beta = ols xs ys
      errs = [ sum (zipWith (*) (feat t e a) beta) - gTrue t e a | (t, e, a) <- testSet ]
  in sqrt (sum (map (^ (2 :: Int)) errs) / fromIntegral (length errs))

-- 各 n を 6 設計シード × 6 noise 反復 = 36 fit で平均。
meanRmse :: Int -> Double
meanRmse n = sum [ rmseFor n ds rp | ds <- [1..6], rp <- [1..6] ] / 36

-- === 効果プロット (CI 帯) : n による信頼区間の変化 =======================

-- 応答 y を design frame に載せて二次モデルを当てはめ、 profiler 用モデルを返す。
--   @(("y", ys) : designTable plan) |-> designModel plan "y"@ が慣用形。
--   各 n は別々の設計・データ → 別々のモデル (データはモデル内に同梱される)。
modelN :: Int -> Int -> _
modelN n dseed =
  let plan   = design n dseed
      rs     = designTable plan
      colD :: Text -> [Double]
      colD c = maybe (error (T.unpack c)) id (lookup c rs)
      ts = colD "temp"; es = colD "energy"; as = colD "angle"
      ys = [ gTrue t e a + noiseSd * z
           | (t, e, a, z) <- zip4 ts es as (gaussians (n*1000 + 1)) ]
  in (("y", ys) : rs) |-> designModel plan "y"
  where
    zip4 (a:as') (b:bs) (c:cs) (d:ds) = (a, b, c, d) : zip4 as' bs cs ds
    zip4 _ _ _ _                      = []

-- 効果プロット図: 横 = 因子・縦 = 応答、 1 行 3 列 (因子) を n ごとに縦積み。
-- **profiler** が予測線 + 95% CI 帯 + 実測散布を (応答×因子) で自動描画する (JMP 流)。
-- 行 = models リスト (= n)、 列 = 因子。 各 n は別モデルで、 データはモデル内に同梱
-- されるため 束ねは空 df でよい。 n=10 は係数10個=飽和 (df=0) だが、 CI 帯計算の
-- df<=0 ガード (Phase 82・LM.hs) により例外を出さず帯が線に潰れる (= CI 不能を素直に表現)。
effectGridFigure :: IO ()
effectGridFigure = do
  let dseed = 20260708
      spec  = toPlot (profiler [ ("n=10", modelN 10 dseed)
                               , ("n=20", modelN 20 dseed)
                               , ("n=40", modelN 40 dseed) ]
                               ["temp", "energy", "angle"])
                <> title "RSM 効果プロット: n で信頼区間がどう変わるか (行=n・列=因子)"
                <> width 780 <> height 720       -- 3×3 grid (意図的な非既定サイズ)
                <> theme ThemeGrey
      noDf = [] :: [(Text, ColData)]             -- profiler のデータはモデル内・束ねは空
  createDirectoryIfMissing True "demo-output/doe"
  saveSVGBound "demo-output/doe/rsm-effects-by-n.svg" (noDf |>> spec)
  putStrLn "  → wrote demo-output/doe/rsm-effects-by-n.svg"

-- === main ================================================================

main :: IO ()
main = do
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  RSM 予測精度 vs 実験数 (温度3水準 × エネルギー3水準 × 角度連続)"
  putStrLn "═══════════════════════════════════════════════════════════════"
  printf "  真の曲面 = 二次 + 0.8·angle³ (bias 源), noise sd = %.2f\n" noiseSd
  printf "  各 n を 6 設計シード × 6 noise 反復 = 36 fit で平均、 テスト %d 点\n\n"
         (length testSet)
  let ns    = [10, 12, 14, 16, 20, 24, 30, 40, 60, 80]
      rmses = [ (n, meanRmse n) | n <- ns ]
  printf "  %4s | %8s | %8s | %s\n" ("n" :: String) ("RMSE" :: String)
         ("noise比" :: String) ("前点比 改善%" :: String)
  putStrLn "  -----|----------|----------|-----------"
  mapM_ (\((n, r), prev) ->
           let imp = case prev of
                       Nothing         -> ""
                       Just (_, pr)    -> printf "%.1f%%" (100 * (pr - r) / pr) :: String
           in printf "  %4d | %8.4f | %8.2f | %s\n" n r (r / noiseSd) imp)
        (zip rmses (Nothing : map Just rmses))
  putStrLn ""

  -- === hgg で RMSE vs n を描画 ===
  createDirectoryIfMissing True "demo-output/doe"
  let pts   = [ Point2 (fromIntegral n) r | (n, r) <- rmses ]
      curve = fromHex "#2c7fb8"
      spec  = layer (linePoints pts <> color curve <> markWidth 2.5)
           <> layer (scatterPoints pts <> color curve)
           <> refHorizontal noiseSd                     -- noise sd の水平参照線
           <> title    "RSM 予測精度 vs 実験数"
           <> subtitle "参照線 = noise sd (1 測定の誤差)。 下回れば曲面が生データより正確"
           <> xLabel   "実験数 n"
           <> yLabel   "予測 RMSE (真の曲面 vs 予測)"
           -- サイズは指定せず既定 (468×288pt = 624×384px・README 図と同寸) に合わせる
           <> theme ThemeGrey
  saveSVG "demo-output/doe/rsm-samplesize.svg" spec
  putStrLn "  → wrote demo-output/doe/rsm-samplesize.svg"

  -- === 効果プロット (n=10/20/40 で CI 帯がどう変わるか) ===
  putStrLn ""
  putStrLn "  効果プロット (行=n · 列=因子・CI 帯) を生成..."
  effectGridFigure
