{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
-- | 逐次 DOE デモ: 半導体インプラ条件の最適化 (30 因子スクリーニング → 試作 RSM)。
--
-- ストーリー (実務の逐次 DOE を再現):
--   * 動かせる因子 = 10 インプラ工程 × { dose (連続) / energy (連続) / tilt (2 水準) }
--     = **30 因子**。 前タイプ recipe (= ref・中心条件) では新タイプの **spec 未達**。
--   * 真の応答はスパース: 10 工程中 **3 工程だけ活性**、 各活性工程で dose×energy /
--     dose×tilt の **within-implant 交互作用** (= 物理的交絡) が強い。 残り 7 工程は inert。
--   * **Phase 1 (sim スクリーニング)**: 30 因子は D-最適座標交換だと 1 設計 ~160 秒で非現実的
--     (実測)。 → **DSD (Definitive Screening Design・61 run)** を使う。 61×31 の主効果
--     モデル行列は rank 31・条件数 2.86 = ほぼ直交で主効果を疎に同定できる (実測)。
--     → 効果 Pareto で活性工程を絞り込む。
--   * **Phase 2 (試作 RSM)**: 絞り込んだ支配 2 工程の dose/energy = 4 因子で 2 次応答曲面。
--     4 lot = 4 block・各 lot に **センター 2 枚必須** (runsheet に付加)。 D-最適 (座標交換・
--     小因子なら数秒) で設計 → RSM fit → 停留点で最適条件 → **spec 達成**を数値で示す。
--
-- 出力図 (demo-output/doe/・git ignore):
--   * implant-screening-pareto.svg  — Phase 1 効果 Pareto (どの工程が効くか)
--   * implant-rsm-contour.svg        — Phase 2 応答曲面 contour + ref/最適点
--   * implant-rsm-profiler.svg       — Phase 2 予測プロファイラ (絞り込み因子・CI 帯)
module Main where

import qualified Numeric.LinearAlgebra as LA
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.List (sortBy, nub)
import           Data.Ord (comparing, Down (..))
import           Text.Printf (printf)
import           System.Directory (createDirectoryIfMissing)

import           Hanalyze.Plot
                   ( customDesign, customSpec, contFactor, quadratic, blocked
                   , designTable, designModel, profiler, contourOf
                   , rsmAnalysis, RSMReport (..), RSMNature (..)
                   , Design (..), toPlot, (|->) )
import           Hanalyze.Design.Workflow (CustomSpec (..))
import           Hanalyze.Design.DSD (dsdDesign, DSDResult (..))
import           Hgg.Plot.Spec
                   ( ColData (..), layer, bar, scatterPoints, Point2 (..)
                   , inline, inlineCat, colorBy, color
                   , title, subtitle, xLabel, yLabel, width, height
                   , xAxis, axisRotate
                   , theme, ThemeName (..) )
import           Hgg.Plot.Frame ((|>>))
import           Hgg.Plot.Color (fromHex)
import           Hgg.Plot.Backend.SVG (saveSVGBound)

-- ===========================================================================
-- Phase 0: 因子・真の応答 (ground truth)・ref・spec
-- ===========================================================================

nImplant :: Int
nImplant = 10

-- 因子 index j (0..29): implant = j `div` 3、 role = j `mod` 3 (0=dose,1=energy,2=tilt)。
implantOf, roleOf :: Int -> Int
implantOf j = j `div` 3
roleOf    j = j `mod` 3

-- 因子の自然単位レンジ。 dose = ions/cm²、 energy = keV、 tilt = 2 水準 {0,7} deg。
factorRange :: Int -> (Double, Double)
factorRange j = case roleOf j of
  0 -> (1.0e14, 5.0e14)   -- dose
  1 -> (10, 100)          -- energy (keV)
  _ -> (0, 7)             -- tilt (deg・2 水準)

-- coded c∈[-1,1] → 自然単位 (中心 0 が範囲中点)。
toNat :: Int -> Double -> Double
toNat j c = let (lo, hi) = factorRange j in lo + (c + 1) / 2 * (hi - lo)

-- 因子名 (compact ラベル): "I3 dose" 等 (implant は 1-based 表示)。
roleName :: Int -> Text
roleName j = case roleOf j of { 0 -> "dose"; 1 -> "energy"; _ -> "tilt" }

factorLabel :: Int -> Text
factorLabel j = "I" <> T.pack (show (implantOf j + 1)) <> " " <> roleName j

-- formula / 因子名で使う安全な変数名 (空白なし): "I3_dose"。
factorVar :: Int -> Text
factorVar j = "I" <> T.pack (show (implantOf j + 1)) <> "_" <> roleName j

-- --- スパースな真の応答 (性能指数 P、 高いほど良い) --------------------------
-- 活性工程 = implant index {2, 5, 8} (= 1-based の 3・6・9)。 各活性工程で
-- dose/energy 主効果 + within-implant 2FI (dose×energy, dose×tilt) + 負の 2 次
-- (⇒ 内点に最大)。 tilt は小さな主効果。 他 7 工程は inert (係数 0)。

-- (implant index, bDose, bEnergy, bTilt, bDoseEnergy, bDoseTilt, qDose, qEnergy)
activeImplants :: [(Int, Double, Double, Double, Double, Double, Double, Double)]
activeImplants =
  [ (2, 8.0, 6.0, 1.5, 3.0, 2.0, 8.0, 6.0)   -- 支配工程 (I3)
  , (5, 5.0, 4.0, 1.0, 2.0, 1.0, 5.0, 4.0)   -- 中位工程 (I6)
  , (8, 3.0, 2.5, 0.5, 1.0, 0.0, 2.5, 2.0)   -- 弱い工程 (I9)
  ]

baseP :: Double
baseP = 50.0

-- 真の応答 g(coded 30 次元)。 coded 値を直接使う (screening 係数と同単位)。
gCoded :: [Double] -> Double
gCoded x = baseP + sum (map contrib activeImplants)
  where
    contrib (i, bd, be, bt, bde, bdt, qd, qe) =
      let xd = x !! (3*i);  xe = x !! (3*i + 1);  xt = x !! (3*i + 2)
      in bd*xd + be*xe + bt*xt + bde*xd*xe + bdt*xd*xt - qd*xd*xd - qe*xe*xe

-- ref 条件 = 前タイプ recipe = 全因子 coded 0 (中心条件)。
refCoded :: [Double]
refCoded = replicate (3 * nImplant) 0

-- spec: 性能指数 P >= specP。 ref (= baseP=50) は未達。
specP :: Double
specP = 60.0

-- ===========================================================================
-- 決定的 PRNG → Gaussian (Box-Muller) — RSMSampleSizeDemo と同型
-- ===========================================================================

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

noiseSd :: Double
noiseSd = 0.8

-- OLS: beta = pinv(X) y。
ols :: [[Double]] -> [Double] -> [Double]
ols xs ys = LA.toList (LA.flatten (LA.pinv (LA.fromLists xs) LA.<> LA.asColumn (LA.fromList ys)))

-- ===========================================================================
-- Phase 1: DSD スクリーニング (61 run)
-- ===========================================================================

-- 効果 (因子 index, |係数|, 係数) を降順で返す + DSD 情報。
screening :: (Int, Int, Bool, [(Int, Double, Double)])
screening =
  let dsd       = either (error . T.unpack) id (dsdDesign (3 * nImplant))
      rows      = LA.toLists (dsdMatrix dsd)                 -- 61 × 30 (coded {-1,0,1})
      ys        = [ gCoded r + noiseSd * z
                  | (r, z) <- zip rows (gaussians 83001) ]
      xs        = [ 1 : r | r <- rows ]                      -- 主効果モデル [1 | 30]
      beta      = ols xs ys
      effects   = [ (j, abs (beta !! (j+1)), beta !! (j+1)) | j <- [0 .. 3*nImplant - 1] ]
      ranked    = sortBy (comparing (Down . (\(_,a,_) -> a))) effects
  in (dsdNRuns dsd, LA.rank (LA.fromLists xs), dsdHasOptimal dsd, ranked)

-- 活性因子 index の集合 (ground truth 由来・図の色分け用)。
activeFactorIdxs :: [Int]
activeFactorIdxs =
  concat [ [3*i, 3*i+1, 3*i+2] | (i,_,_,_,_,_,_,_) <- activeImplants ]

-- Phase 1 図: 効果 Pareto (top 15・活性/inert 色分け)。
screeningFigure :: [(Int, Double, Double)] -> IO ()
screeningFigure ranked = do
  let topN   = 15
      top    = take topN ranked
      -- bar の categorical 軸はラベルをアルファベット順に並べる (hgg に
      -- xCatOrder 未実装)。 Pareto は降順必須ゆえ、 順位をゼロ埋め前置して
      -- アルファベット順 = |効果| 降順に一致させる ("01 I3 dose" 等)。
      labels = [ T.pack (printf "%02d " k) <> factorLabel j
               | (k, (j,_,_)) <- zip [1 :: Int ..] top ]
      vals   = [ a | (_,a,_) <- top ]
      cats   = [ if j `elem` activeFactorIdxs then "活性 (真に効く)" else "inert (ノイズ)"
               | (j,_,_) <- top ] :: [Text]
      spec'  = layer ( bar (inlineCat labels) (inline vals)
                     <> colorBy (inlineCat cats) )
                <> title    "Phase 1: 効果 Pareto (DSD 61 run で 30 因子スクリーニング)"
                <> subtitle "|主効果係数| 降順 top 15。 活性 3 工程の dose/energy が上位に立つ"
                <> xLabel   "因子 (I<工程> <パラメタ>)"
                <> yLabel   "|効果| (coded 単位)"
                <> xAxis (axisRotate 90)     -- 横軸ラベルを y 軸タイトルと同じ向き (CCW 90°・下→上読み)
                <> width 720 <> height 460
                <> theme ThemeGrey
      noDf   = [] :: [(Text, ColData)]
  createDirectoryIfMissing True "demo-output/doe"
  saveSVGBound "demo-output/doe/implant-screening-pareto.svg" (noDf |>> spec')
  putStrLn "  → wrote demo-output/doe/implant-screening-pareto.svg"

-- ===========================================================================
-- Phase 2: 試作 RSM (blocked 4 lot・center 2/lot・D-最適)
-- ===========================================================================

-- coded → 自然単位の逆 (自然 → coded)。 stationary(自然) を coded に戻し true P を評価。
toCoded :: Int -> Double -> Double
toCoded j nat = let (lo, hi) = factorRange j in 2 * (nat - lo) / (hi - lo) - 1

nLot :: Int
nLot = 4

centerPerLot :: Int
centerPerLot = 2

-- screening から RSM に carry する因子を導出:
--   * dose/energy で |効果| > 2.0 の工程 = 「支配工程」 → その dose/energy を carry。
-- 非 carry で |効果| > 0.35 の因子 (= 活性 tilt) は screening の符号方向に固定 (背景条件)。
selectedImplants :: [(Int, Double, Double)] -> [Int]
selectedImplants ranked =
  nub [ implantOf j | (j, a, _) <- ranked, roleOf j /= 2, a > 2.0 ]

carriedIdxs :: [(Int, Double, Double)] -> [Int]
carriedIdxs ranked =
  concat [ [3*i, 3*i + 1] | i <- selectedImplants ranked ]  -- 各支配工程の dose, energy

-- 背景 coded (非 carry 因子): 活性 tilt 等は screening 符号方向に固定、 他は ref(0)。
backgroundCoded :: [(Int, Double, Double)] -> [Int] -> [Double]
backgroundCoded ranked carried =
  [ bgAt j | j <- [0 .. 3*nImplant - 1] ]
  where
    effOf j = head ([ c | (k, _, c) <- ranked, k == j ] ++ [0])
    bgAt j | j `elem` carried        = 0                       -- carry は design が上書き
           | abs (effOf j) > 0.35    = signum (effOf j)        -- 有意非 carry (tilt) を固定
           | otherwise               = 0                       -- inert は ref

-- Phase 2 本体。 screening ranked を受け、 設計 → sim → RSM fit → 最適条件を返す。
data RSMOut = RSMOut
  { roCarried   :: ![Int]              -- carry した因子 index
  , roNFree     :: !Int                -- 自由 run 数 (block D-最適)
  , roNCenter   :: !Int                -- center 数 (2/lot × 4)
  , roReport    :: !RSMReport          -- rsmAnalysis 結果
  , roTruePOpt  :: !Double             -- 見つけた最適条件での「真の」P
  , roNames     :: ![Text]             -- carry 因子の safe 変数名
  }

runPhase2 :: [(Int, Double, Double)] -> (RSMOut, IO ())
runPhase2 ranked =
  let carried  = carriedIdxs ranked
      names    = map factorVar carried
      facs     = [ contFactor (factorVar j) (factorRange j) | j <- carried ]
      nFree    = 44
      plan     = customDesign ((customSpec facs (quadratic names) nFree 83002)
                                 { csStructure = blocked nLot })
      -- 設計の coded 行 (自由 run) + center (2/lot = 8 行・全 0)。
      codedFree   = dsCoded plan
      nCenter     = nLot * centerPerLot
      codedCenter = replicate nCenter (replicate (length carried) 0)
      codedAug    = codedFree ++ codedCenter
      -- 自然単位 runsheet (図の data frame 用)。
      tbl      = designTable plan
      colOf c  = maybe (error (T.unpack c)) id (lookup c tbl)
      mids     = [ toNat j 0 | j <- carried ]                     -- center の自然値 = 中点
      rowsAug  = [ (nm, colOf nm ++ replicate nCenter mid)
                 | (nm, mid) <- zip names mids ]
      -- sim 応答 (真の応答 + noise)。 coded 行を full-30 に埋め込み。
      bg       = backgroundCoded ranked carried
      idxPos   = zip carried [0 :: Int ..]
      embed r6 = [ maybe (bg !! j) (r6 !!) (lookup j idxPos) | j <- [0 .. 3*nImplant - 1] ]
      ysAug    = [ gCoded (embed r6) + noiseSd * z
                 | (r6, z) <- zip codedAug (gaussians 83003) ]
      -- RSM 解析 (coded augmented を持つ Design で停留点を自然単位へ)。
      report   = rsmAnalysis (plan { dsCoded = codedAug }) ysAug
      -- 見つけた最適条件 (自然) を coded に戻し、 背景に埋め込んで「真の」P を評価。
      optCoded = [ maybe 0 (\pos -> toCoded (carried !! pos)
                                       (maybe 0 id (lookup (factorVar (carried !! pos))
                                                           (rsmStationary report))))
                          (lookup j idxPos)
                 | j <- [0 .. 3*nImplant - 1] ]
      -- 背景 + carry 最適 を合成した full coded で真の P。
      optFull  = [ if j `elem` carried then optCoded !! j else bg !! j
                 | j <- [0 .. 3*nImplant - 1] ]
      truePOpt = gCoded optFull
      -- 図 (contour + profiler)。 designModel は data frame で fit (plan は formula のみ)。
      model    = (("P", ysAug) : rowsAug) |-> designModel plan "P"
      figs     = do
        createDirectoryIfMissing True "demo-output/doe"
        let noDf = [] :: [(Text, ColData)]
            -- 支配 2 因子 (I3 dose × I3 energy) で contour。
            v1 = names !! 0; v2 = names !! 1
            refD = toNat (carried !! 0) 0; refE = toNat (carried !! 1) 0
            optD = maybe refD id (lookup v1 (rsmStationary report))
            optE = maybe refE id (lookup v2 (rsmStationary report))
            contourSpec =
              (noDf |>> ( contourOf model v1 v2
                          <> layer (scatterPoints [Point2 refD refE] <> color (fromHex "#333333"))
                          <> layer (scatterPoints [Point2 optD optE] <> color (fromHex "#d73027"))
                          <> title    "Phase 2: 応答曲面 contour (支配工程 I3 dose × energy)"
                          <> subtitle "灰=ref(中心・spec未達) / 赤=RSM 最適点。 他因子は中央値固定"
                          <> xLabel (factorLabel (carried !! 0))
                          <> yLabel (factorLabel (carried !! 1))
                          <> theme ThemeGrey ))
            profSpec =
              (noDf |>> ( toPlot (profiler [("P", model)] names)
                          <> title "Phase 2: 予測プロファイラ (絞り込み 6 因子・95% CI 帯)"
                          <> width 900 <> height 360
                          <> theme ThemeGrey ))
        saveSVGBound "demo-output/doe/implant-rsm-contour.svg" contourSpec
        putStrLn "  → wrote demo-output/doe/implant-rsm-contour.svg"
        saveSVGBound "demo-output/doe/implant-rsm-profiler.svg" profSpec
        putStrLn "  → wrote demo-output/doe/implant-rsm-profiler.svg"
  in ( RSMOut carried nFree nCenter report truePOpt names
     , figs )

-- ===========================================================================
-- main (Phase 0 + Phase 1 + Phase 2)
-- ===========================================================================

main :: IO ()
main = do
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  逐次 DOE デモ: 半導体インプラ条件最適化 (30 因子 → 試作 RSM)"
  putStrLn "═══════════════════════════════════════════════════════════════"
  printf "  因子: 10 工程 × {dose, energy, tilt} = %d 因子\n" (3 * nImplant)
  printf "  真の活性工程 = I3 / I6 / I9 (残り 7 工程は inert)\n"
  printf "  ref (中心条件) の性能指数 P = %.1f、 spec = P >= %.1f → ref は未達\n\n"
         (gCoded refCoded) specP

  putStrLn "── Phase 1: DSD スクリーニング ──────────────────────────────"
  let (nRun, rnk, hasOpt, ranked) = screening
  printf "  DSD: %d run・主効果モデル行列 rank = %d (= 31 なら全主効果推定可)・%s\n"
         nRun rnk (if hasOpt then "verified" else "structural 近似" :: String)
  putStrLn "  効果 Pareto (top 10):"
  printf "  %-10s | %8s | %s\n" ("因子" :: String) ("|効果|" :: String) ("活性?" :: String)
  putStrLn "  -----------|----------|------"
  mapM_ (\(j, a, _) ->
           printf "  %-10s | %8.3f | %s\n" (T.unpack (factorLabel j)) a
                  (if j `elem` activeFactorIdxs then "●" else "" :: String))
        (take 10 ranked)
  putStrLn ""
  screeningFigure ranked

  putStrLn ""
  putStrLn "── Phase 2: 試作 RSM (絞り込み → blocked D-最適 → 最適条件) ──"
  let (out, figs) = runPhase2 ranked
      sel  = selectedImplants ranked
      rep  = roReport out
  printf "  絞り込み: 支配工程 = %s → carry 因子 (dose/energy) = %d 個\n"
         (unwords [ "I" ++ show (i+1) | i <- sel ]) (length (roCarried out))
  printf "  試作設計: %d lot × (自由 %d + center %d) = %d 枚 (D-最適・block=lot)\n"
         nLot (roNFree out `div` nLot) centerPerLot
         (roNFree out + roNCenter out)
  printf "  RSM fit: R² = %.3f・停留点の性質 = %s・領域内 = %s\n"
         (rsmR2 rep)
         (case rsmNature rep of RMaximum -> "極大"; RMinimum -> "極小"; RSaddle -> "鞍点" :: String)
         (if rsmInRegion rep then "yes" else "no (外挿)" :: String)
  putStrLn "  最適条件 (自然単位):"
  mapM_ (\(nm, v) -> printf "    %-10s = %s\n" (T.unpack nm) (fmtNat nm v))
        (rsmStationary rep)
  putStrLn ""
  printf "  ── ストーリー closure ──────────────────────\n"
  printf "  ref (前タイプ・中心条件)   P = %6.2f  → spec %.0f %s\n"
         (gCoded refCoded) specP (verdict (gCoded refCoded))
  printf "  RSM 予測 (最適条件)         P = %6.2f\n" (rsmPredicted rep)
  printf "  真の応答 (最適条件で検証)   P = %6.2f  → spec %.0f %s\n"
         (roTruePOpt out) specP (verdict (roTruePOpt out))
  putStrLn ""
  figs
  putStrLn ""
  putStrLn "  完了: 3 図を demo-output/doe/ に出力。"
  where
    verdict p = if p >= specP then "達成 ✓" else "未達 ✗" :: String
    -- 自然単位の見やすい整形 (dose は指数、 他は小数)。
    fmtNat :: Text -> Double -> String
    fmtNat nm v
      | "_dose" `T.isSuffixOf` nm = printf "%.2e ions/cm²" v
      | "_energy" `T.isSuffixOf` nm = printf "%.1f keV" v
      | otherwise = printf "%.2f" v
