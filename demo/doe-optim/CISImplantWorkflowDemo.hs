{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns      #-}
-- | CMOS Image Sensor (CIS) PD implant 工程の workflow デモ。
--
-- マニュアル `docs/manual/semiconductor-design-workflow.md` の付録 B 相当を、
-- ユーザ実用ケース (3 因子 + tilt 離散 + CIS 応答) で動作可能形にしたもの。
--
-- ## モチーフ
--
-- CMOS Image Sensor のフォトダイオード (PD) implant 工程を題材とし、 注入条件
-- (dose、 energy、 tilt) が画素特性に与える影響を Custom Design で評価する。
--
-- 3 因子:
--
--   * dose   (1e13 .. 5e13 cm^-2、 5 水準)
--   * energy (5 .. 50 keV、 5 水準)
--   * tilt   (0 / 7 / 15 / 30 deg、 装置制約で 4 水準離散)
--
-- 3 応答:
--
--   * defect  (画素欠陥カウント、 数万オーダの自然数、 Poisson GLM)
--   * fwc     (Full Well Capacity [e-]、 連続、 二次 RSM、 maximize)
--   * dark    (Dark Current [pA/cm^2]、 連続 log-scale、 LM、 minimize)
--
-- ## フロー
--
-- 1. Custom Design I-optimal で 23 runs を生成
-- 2. AddCenter 2 で強制中心行を追加 → 計 25 runs (1 ロット枠)
-- 3. 合成 Sim (本来は実機 / TCAD) で 3 応答を測定
-- 4. defect → Poisson GLM (Log link)、 fwc → RSM 二次、 dark → log-LM
-- 5. Desirability で多目的統合スコアを評価、 最適条件を特定
--
-- ## 数値合成
--
-- 各応答は **dose / energy / tilt の物理直感に沿った合成関数** + 小さい
-- 確定的揺らぎ (run 番号由来) で生成する。 実機データ取得を模した骨組み。
module Main where

import qualified Data.Text                          as T
import qualified Numeric.LinearAlgebra              as LA
import           Text.Printf                        (printf)

import qualified Hanalyze.Design.Custom.Factor      as DF
import qualified Hanalyze.Design.Custom.Model       as DM
import qualified Hanalyze.Design.Custom.Coordinate  as DC
import qualified Hanalyze.Design.Custom.Augment     as DA
import qualified Hanalyze.Design.Optimal            as DO
import qualified Hanalyze.Design.RSM                as RSM
import qualified Hanalyze.Model.Core                as Core
import qualified Hanalyze.Model.LM                  as LM
import qualified Hanalyze.Model.GLM                 as GLM
import qualified Hanalyze.Optim.Desirability        as Des
import qualified Hanalyze.Model.LiNGAM.Direct       as LNG
import qualified Hanalyze.Model.DAG                 as DAG
import qualified Data.Text.IO                       as TIO
import qualified Data.Vector                        as V
import           System.Directory                   (createDirectoryIfMissing)

-- ===========================================================================
-- 因子定義
-- ===========================================================================

doseLo, doseHi :: Double
doseLo = 1e13
doseHi = 5e13

energyLo, energyHi :: Double
energyLo = 5
energyHi = 50

tiltLevels :: [Double]
tiltLevels = [0, 7, 15, 30]

factors :: [DF.Factor]
factors =
  [ DF.Factor "dose"   (DF.Continuous   doseLo   doseHi)   DF.Controllable
  , DF.Factor "energy" (DF.Continuous   energyLo energyHi) DF.Controllable
  , DF.Factor "tilt"   (DF.DiscreteNum  tiltLevels)        DF.Controllable
  ]

-- | 二次モデル: main + 2-way interactions + pure quadratic
--   (10 項、 23 runs で十分推定可能)
quadModel :: DM.Model
quadModel = DM.Model
  { DM.mTerms =
      [ DM.TIntercept
      , DM.TMain "dose"
      , DM.TMain "energy"
      , DM.TMain "tilt"
      , DM.TInter ["dose", "energy"]
      , DM.TInter ["dose", "tilt"]
      , DM.TInter ["energy", "tilt"]
      , DM.TPower "dose"   2
      , DM.TPower "energy" 2
      , DM.TPower "tilt"   2
      ]
  , DM.mNorm = DM.NCoded
  }

-- ===========================================================================
-- 合成応答 (synthetic ground truth)
-- ===========================================================================
--
-- 物理直感ベースの合成関数:
--   * defect: 高 dose で増、 高 energy で増、 tilt 中央で最小 (チャネリング)
--             → Poisson λ ≈ exp(10 .. 11) 程度 → 数万カウント
--   * fwc:    energy で増、 dose 中央で極大、 tilt 弱影響
--   * dark:   高 dose で増、 高 energy で増 (損傷)、 tilt 中央で最小
--             → log-scale で扱う

codedDose :: Double -> Double
codedDose x = 2 * (x - (doseLo + doseHi) / 2) / (doseHi - doseLo)

codedEnergy :: Double -> Double
codedEnergy x = 2 * (x - (energyLo + energyHi) / 2) / (energyHi - energyLo)

codedTilt :: Double -> Double
codedTilt x = (x - 13) / 17    -- tilt 範囲 0..30 を ~[-0.76, 1] にざっくり

-- | 引数は **既に coded された値** (dose / energy ∈ [-1,1])、 ただし
--   tilt は raw 値 (DiscreteNum 因子はライブラリの内部表現も raw)。
--   ここで tilt のみ coded に変換する。
syntheticResp :: (Double, Double, Double) -> (Int, Double, Double)
syntheticResp (dC, eC, tiltRaw) =
  let !tC = codedTilt tiltRaw
      -- defect: Poisson λ。 中心 ~exp(10.5) ≈ 36300
      !logLam = 10.5 + 0.8*dC + 0.4*eC + 0.30*tC*tC - 0.20*dC*eC
      !lam    = exp logLam
      !defect = round lam :: Int
      -- fwc (e-): 中心 ~12000、 energy で増、 dose 中央極大 (dose^2 で減少)
      !fwc = 12000 + 1000*eC - 800*dC*dC - 200*tC + 50*dC*eC
      -- dark (pA/cm^2): log-scale
      !logDark = -1.0 + 0.5*dC + 0.3*eC + 0.4*tC*tC
      !dark    = exp logDark
  in (defect, fwc, dark)

-- ===========================================================================
-- ヘルパ
-- ===========================================================================

-- | 設計行列各行から (dose, energy, tilt) を取り出す
rowToFactors :: LA.Matrix Double -> Int -> (Double, Double, Double)
rowToFactors m i =
  ( LA.atIndex m (i, 0)
  , LA.atIndex m (i, 1)
  , LA.atIndex m (i, 2)
  )

-- | coded 値の行列に変換 (analysis 用)。
--   ライブラリの cdMatrix: Continuous は既に coded ±1、 DiscreteNum (tilt) は raw。
--   ここで tilt のみ codedTilt に通す。
toCodedMatrix :: LA.Matrix Double -> LA.Matrix Double
toCodedMatrix m =
  let n   = LA.rows m
      dC  = LA.fromList [ LA.atIndex m (i,0)               | i <- [0..n-1] ]
      eC  = LA.fromList [ LA.atIndex m (i,1)               | i <- [0..n-1] ]
      tC  = LA.fromList [ codedTilt (LA.atIndex m (i,2))   | i <- [0..n-1] ]
  in LA.fromColumns [dC, eC, tC]

-- | coded dose/energy を raw 単位に戻す (表示用)
rawDose :: Double -> Double
rawDose c = (doseLo + doseHi) / 2 + c * (doseHi - doseLo) / 2

rawEnergy :: Double -> Double
rawEnergy c = (energyLo + energyHi) / 2 + c * (energyHi - energyLo) / 2

-- | 二次モデル設計行列を coded 行列から構築 (intercept + 3 main + 3 inter + 3 quad)
buildQuadDesign :: LA.Matrix Double -> LA.Matrix Double
buildQuadDesign xCoded =
  let n = LA.rows xCoded
      d = LA.flatten (xCoded LA.¿ [0])
      e = LA.flatten (xCoded LA.¿ [1])
      t = LA.flatten (xCoded LA.¿ [2])
      ones = LA.fromList (replicate n 1)
  in LA.fromColumns
       [ ones
       , d, e, t
       , d * e, d * t, e * t
       , d * d, e * e, t * t
       ]

quadTermLabels :: [String]
quadTermLabels =
  [ "intercept"
  , "dose", "energy", "tilt"
  , "dose*energy", "dose*tilt", "energy*tilt"
  , "dose^2", "energy^2", "tilt^2"
  ]

-- ===========================================================================
-- main
-- ===========================================================================

main :: IO ()
main = do
  let bar = replicate 75 '='
  putStrLn bar
  putStrLn "  CMOS Image Sensor PD implant workflow demo"
  putStrLn "  3 因子 (dose / energy / tilt 離散) × 25 runs (23 + 2 center)"
  putStrLn bar
  putStrLn ""

  -- ── 1. Custom Design I-optimal 23 runs ──
  putStrLn "[1] Custom Design I-optimal で 23 runs を生成中 ..."
  let spec = DC.CustomDesignSpec
        { DC.cdsFactors      = factors
        , DC.cdsModel        = quadModel
        , DC.cdsConstraints  = []
        , DC.cdsNRuns        = 23
        , DC.cdsCriterion    = DO.IOpt
        , DC.cdsBudget       = DC.defaultBudget
        , DC.cdsSeed         = Just 20260530
        , DC.cdsInitial      = Nothing
        , DC.cdsDJConvention = False
        }
  eDesign <- DC.coordinateExchange spec
  case eDesign of
    Left err -> putStrLn ("  FAIL: " ++ T.unpack err)
    Right cd -> do
      let base    = DC.cdMatrix cd
          report  = DC.cdReport cd
      printf "  ✓ runs=%d, restarts=%d, conv=%s, crit=%.6g\n"
        (LA.rows base) (DC.crRestarts report)
        (show (DC.crConverged report)) (DC.crCriterionValue report)
      putStrLn ""

      -- ── 2. AddCenter 2 で 25 runs に ──
      putStrLn "[2] AddCenter 2 で強制中心 2 行を追加 → 計 25 runs"
      let specWithBase = spec { DC.cdsInitial = Just base }
      eAug <- DA.augmentMenu specWithBase (DA.AddCenter 2)
      case eAug of
        Left err  -> putStrLn ("  FAIL: " ++ T.unpack err)
        Right amr -> do
          let full = DA.amrMatrix amr
          printf "  ✓ 最終 runs=%d (= %d + center %d)\n"
            (LA.rows full) (LA.rows base) (DA.amrAdded amr)
          putStrLn ""

          -- ── 3. 合成 Sim で応答取得 ──
          putStrLn "[3] 合成 Sim による応答取得 (defect / fwc / dark)"
          let n = LA.rows full
              triples = [ syntheticResp (rowToFactors full i)
                        | i <- [0..n-1] ]
              defects = [ d | (d, _, _) <- triples ]
              fwcs    = [ f | (_, f, _) <- triples ]
              darks   = [ k | (_, _, k) <- triples ]
          printf "  defect: min=%d  max=%d  mean=%.0f\n"
            (minimum defects) (maximum defects)
            (fromIntegral (sum defects) / fromIntegral n :: Double)
          printf "  fwc:    min=%.0f  max=%.0f  mean=%.1f\n"
            (minimum fwcs) (maximum fwcs) (sum fwcs / fromIntegral n)
          printf "  dark:   min=%.3g  max=%.3g  mean=%.3g\n"
            (minimum darks) (maximum darks) (sum darks / fromIntegral n)
          putStrLn ""

          -- ── 4. 解析 ──
          let xCoded    = toCodedMatrix full
              xQuad     = buildQuadDesign xCoded
              yDefect   = LA.fromList (map fromIntegral defects)
              yFwc      = LA.fromList fwcs
              yLogDark  = LA.fromList (map log darks)

          -- 4a. defect: Poisson GLM (LogLink)
          putStrLn "[4a] defect → Poisson GLM (LogLink)"
          let (glmRes, _glmCov) = GLM.fitGLMFull GLM.Poisson GLM.Log xQuad yDefect
          printFitCoefs quadTermLabels glmRes
          putStrLn ""

          -- 4b. fwc: 二次 RSM
          putStrLn "[4b] fwc → 二次 RSM (canonical analysis)"
          let qFit = RSM.fitQuadratic (LA.toLists xCoded) (LA.toList yFwc)
              (xStar, yStar, eigs) = RSM.optimumPoint qFit
          printf "  推定極値座標 (coded): %s\n" (show xStar)
          printf "  そこでの fwc: %.3g e-\n" yStar
          printf "  eigenvalues: %s\n" (show eigs)
          let nearZero = any (\v -> abs v < 1e-6) eigs
          if nearZero
            then putStrLn "  (注: 1 つの eigenvalue が ~0 → quadratic に効かない\n\
                          \   軸あり。 fwc 合成式が dose のみ quadratic、 energy/tilt\n\
                          \   は線形であることを canonical analysis が正しく示している)"
            else pure ()
          let curvature :: String
              curvature
                | all (> 0) eigs = "局所極小 (応答最小)"
                | all (< 0) eigs = "局所極大 (応答最大)"
                | otherwise      = "鞍点 (mixed sign)"
          printf "  → %s\n" curvature
          putStrLn ""

          -- 4c. dark: log-LM (log-scale 応答に対する線形モデル)
          putStrLn "[4c] dark (log-scale) → LM"
          let lmFit = LM.fitLMVec xQuad yLogDark
          printFitCoefs quadTermLabels lmFit
          putStrLn ""

          -- ── 5. Desirability で多目的統合スコア ──
          putStrLn "[5] Desirability で多目的統合スコア"
          --   defect: minimize、 上限 50000、 目標 10000
          --   fwc:    maximize、 下限 10000、 目標 14000
          --   dark:   minimize、 上限 5.0、 目標 0.5
          -- 閾値は実データ範囲を踏まえ動的に設定 (デモ用)
          let defMin = fromIntegral (minimum defects) :: Double
              defMax = fromIntegral (maximum defects) :: Double
              fwcMin = minimum fwcs
              fwcMax = maximum fwcs
              darkMin = minimum darks
              darkMax = maximum darks
              dTypes =
                [ Des.Minimize defMax  defMin
                , Des.Maximize fwcMin  fwcMax
                , Des.Minimize darkMax darkMin
                ]
              scorePerRun =
                [ Des.overallDesirability dTypes
                    [ fromIntegral (defects !! i)
                    , fwcs !! i
                    , darks !! i
                    ]
                | i <- [0..n-1]
                ]
              bestIdx = argmax scorePerRun
              bestRow = rowToFactors full bestIdx
          printf "  best run idx = %d (score = %.4f)\n"
            bestIdx (scorePerRun !! bestIdx)
          let (bdC, beC, btR) = bestRow
          printf "  best 条件 (raw):  dose=%.2e  energy=%.2f keV  tilt=%.1f deg\n"
            (rawDose bdC) (rawEnergy beC) btR
          printf "  best 条件 (coded): dose=%+.3f  energy=%+.3f  tilt(raw)=%.1f\n"
            bdC beC btR
          let (bd, bf, bk) = syntheticResp bestRow
          printf "  best 応答: defect=%d  fwc=%.0f  dark=%.3g\n" bd bf bk
          putStrLn ""

          -- ── 6. LiNGAM 因果探索 (3 応答間の因果構造を観測データから推定) ──
          putStrLn "[6] LiNGAM 因果探索 (defect / fwc / dark 間の因果構造)"
          -- 3 応答を縦に並べた n × 3 行列を組む。 dark は log-scale。
          let respMat = LA.fromColumns
                [ LA.fromList (map fromIntegral defects)
                , yFwc
                , yLogDark
                ]
              lingamFit = LNG.fitDirectLiNGAM LNG.defaultDirectLiNGAMConfig respMat
              respLabels = V.fromList
                [ T.pack "defect", T.pack "fwc", T.pack "log_dark" ]
              dag = DAG.withNames respLabels
                      (LNG.dlDAG LNG.defaultDirectLiNGAMConfig lingamFit)
          printf "  causal order: %s\n" (show (LNG.dlOrder lingamFit))
          putStrLn "  推定 B 行列 (係数):"
          let b = LNG.dlB lingamFit
              rows = [ (i, j, LA.atIndex b (i, j))
                     | i <- [0..2], j <- [0..2], i /= j
                     , abs (LA.atIndex b (i, j)) > 0.05 ]
          mapM_ (\(i, j, w) ->
                  printf "    %s ← %s (%+.3f)\n"
                    (T.unpack (DAG.dagNodeName dag i))
                    (T.unpack (DAG.dagNodeName dag j))
                    w) rows
          putStrLn ""
          printf "  DAG acyclic? %s\n" (show (DAG.isAcyclic dag))
          printf "  topological sort: %s\n"
            (case DAG.topoSort dag of
               Just ord -> show ord ++ " ("
                          ++ unwords [T.unpack (DAG.dagNodeName dag i) | i <- ord]
                          ++ ")"
               Nothing  -> "(循環あり)")
          putStrLn ""

          -- ── 7. DOT エクスポート (Graphviz で可視化) ──
          putStrLn "[7] DOT エクスポート"
          createDirectoryIfMissing True "demo-output"
          let dotPath = "demo-output/cis-implant-dag.dot"
              dotText = DAG.toDOT dag
          TIO.writeFile dotPath dotText
          printf "  → %s に出力 (graphviz: dot -Tpng %s -o dag.png)\n"
            dotPath dotPath
          putStrLn ""

          putStrLn bar
          putStrLn "  CIS implant workflow demo 完了"
          putStrLn bar

-- | 係数ベクトルを項ラベル付きで表示。 単一応答 FitResult (q=1) を仮定し
--   coefficients の 1 列目を取り出す。
printFitCoefs :: [String] -> Core.FitResult -> IO ()
printFitCoefs labels res = do
  let !beta = Core.coefficients res
      cs    = if LA.cols beta > 0
                then LA.toList (LA.flatten (beta LA.¿ [0]))
                else []
  mapM_ (\(lbl, c) -> printf "  %-14s %+12.4g\n" lbl c)
        (zip labels cs)

argmax :: Ord a => [a] -> Int
argmax xs = snd $ foldr1 (\a b -> if fst a >= fst b then a else b)
                         (zip xs [0..])
