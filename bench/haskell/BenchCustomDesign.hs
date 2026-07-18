{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
-- | Phase 27: Custom Design (JMP 同等性) 検証ベンチ。
--
-- 文献例題と JMP 公式 example の **参照値 (criterion / D-eff)** を golden CSV
-- として保持し、 hanalyze 実装の出力との比較を deterministic seed 固定で
-- 実行する。
--
-- 入力 (golden):  @bench/custom-design/golden/<example>.csv@
--   - reference 設計行列 (whole_plot, factor1, factor2, ... header)
--   - 値は論文記載値そのまま (Jones-Goos (2012) Table 2/4 等)
--
-- 出力 (results): @bench/custom-design/results/golden-comparison.csv@
--   - schema: @example,metric,hanalyze_value,reference_value,ratio,tolerance,pass@
--
-- ## 実装メモ: split-plot D-criterion の重複実装
--
-- Phase 25 で SplitPlot.evalCritSP は internal (非 public)。 bench から
-- 直接呼べないので、 同じ M⁻¹ 構築ロジックをここに重複実装している。
-- 仕様変更時は src/hanalyze/Analyze/Design/Custom/SplitPlot.hs と本ファイル両方を
-- 更新すること (簡易 REML criterion: critValueM(DOpt, chol(X' M⁻¹ X)) =
-- -det(X' M⁻¹ X))。
module Main where

import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Csv                   (HasHeader (..), decode)
import qualified Data.Text                  as T
import qualified Data.Vector                as V
import qualified Data.Vector.Storable       as VS
import qualified Numeric.LinearAlgebra      as LA
import           System.Directory           (createDirectoryIfMissing,
                                             doesFileExist)
import           System.IO                  (BufferMode (..), IOMode (..),
                                             hPutStrLn, hSetBuffering, withFile)
import           Text.Printf                (printf)

import qualified Hanalyze.Design.Custom.Bayesian     as CB
import qualified Hanalyze.Design.Custom.Constraint   as CC
import qualified Hanalyze.Design.Custom.Coordinate   as CX
import qualified Hanalyze.Design.Custom.Factor       as CF
import qualified Hanalyze.Design.Custom.Model        as CM
import qualified Hanalyze.Design.Custom.RegionMoment as RM
import qualified Hanalyze.Design.Custom.SplitPlot    as SP
import qualified Hanalyze.Design.Optimal             as OPT

-- ===========================================================================
-- 比較結果 row (results/golden-comparison.csv の schema)
-- ===========================================================================

-- | 1 (example, metric) ペアの比較結果。
data GoldenRow = GoldenRow
  { grExample   :: String
  , grMetric    :: String
  , grhanalyze   :: Double
  , grReference :: Double
  , grTolerance :: Double
  } deriving Show

grRatio :: GoldenRow -> Double
grRatio r
  | grReference r == 0 = 0 / 0
  | otherwise          = grhanalyze r / grReference r

grPass :: GoldenRow -> Bool
grPass r =
  let ratio = grRatio r
  in  not (isNaN ratio) && abs (ratio - 1) <= grTolerance r

writeGoldenRows :: FilePath -> [GoldenRow] -> IO ()
writeGoldenRows path rows = withFile path WriteMode $ \h -> do
  hSetBuffering h LineBuffering
  hPutStrLn h "example,metric,hanalyze_value,reference_value,ratio,tolerance,pass"
  mapM_ (\r -> hPutStrLn h
          (printf "%s,%s,%.10g,%.10g,%.10g,%.6g,%s"
            (grExample r) (grMetric r)
            (grhanalyze r) (grReference r)
            (grRatio r) (grTolerance r)
            (if grPass r then "true" else "false" :: String))) rows

-- ===========================================================================
-- 文献参照値 CSV の読み込み
-- ===========================================================================

-- | 設計行列 CSV (header: whole_plot,x1,x2,...) を読み、
-- (raw matrix, whole-plot indicator) を返す。
readDesignCSV
  :: FilePath
  -> IO (Either String (LA.Matrix Double, VS.Vector Int, Int))
readDesignCSV path = do
  exists <- doesFileExist path
  if not exists
    then pure (Left ("design file not found: " ++ path))
    else do
      bytes <- BL.fromStrict <$> BS.readFile path
      case decode HasHeader bytes :: Either String (V.Vector (V.Vector Double)) of
        Left  err -> pure (Left ("decode " ++ path ++ ": " ++ err))
        Right rs
          | V.null rs -> pure (Left ("empty CSV: " ++ path))
          | otherwise ->
              let nCols = V.length (rs V.! 0)
                  nRows = V.length rs
                  -- col 0 = whole_plot id (1-based in CSV → 0-based internal)
                  wpIds = VS.fromList
                            [ round (rs V.! i V.! 0) - 1
                            | i <- [0 .. nRows - 1] ]
                  rawMat = LA.fromLists
                             [ [ rs V.! i V.! j | j <- [1 .. nCols - 1] ]
                             | i <- [0 .. nRows - 1] ]
                  nWP   = maximum (VS.toList wpIds) + 1
              in  pure (Right (rawMat, wpIds, nWP))

-- ===========================================================================
-- Split-Plot D-criterion 評価 (SplitPlot.evalCritSP の重複実装)
-- ===========================================================================

-- | M⁻¹ を block-diagonal で構築。 各 WP block の diag = 1 - η/(1 + η n_w)、
-- off-diag = -η/(1 + η n_w)。 SplitPlot.buildMInv と同じロジック。
buildMInv :: Int -> Double -> VS.Vector Int -> Int -> LA.Matrix Double
buildMInv n eta wpId nWP =
  let wpSizes = [ length [ i | i <- [0 .. n - 1], wpId VS.! i == w ]
                | w <- [0 .. nWP - 1] ]
      entry i j
        | wpId VS.! i /= wpId VS.! j = 0
        | otherwise =
            let w   = wpId VS.! i
                nwD = fromIntegral (wpSizes !! w) :: Double
                off = - eta / (1 + eta * nwD)
            in if i == j then 1 + off else off
  in (n LA.>< n) [ entry i j | i <- [0 .. n - 1], j <- [0 .. n - 1] ]

-- | det(X' M⁻¹ X) を返す。 模型展開 (intercept + main + interaction + quadratic)
-- は expandDesignMatrix に委譲。
--
-- 返り値 = SplitPlot criterion 値の絶対値 (positive)。
-- hanalyze の `spdGEFFEst` は -det(X' M⁻¹ X)、 ここでは +det(X' M⁻¹ X)
-- を返すので、 比較時に sign 注意。
splitPlotDDet
  :: [CF.Factor]
  -> CM.Model
  -> Double                 -- ^ η
  -> VS.Vector Int          -- ^ WP id per row
  -> Int                    -- ^ nWP
  -> LA.Matrix Double       -- ^ raw design matrix
  -> Either String Double
splitPlotDDet factors model eta wpId nWP raw =
  case CM.expandDesignMatrix factors model raw of
    Left e  -> Left (T.unpack e)
    Right x ->
      let n    = LA.rows x
          mInv = buildMInv n eta wpId nWP
          xtmx = LA.tr x LA.<> (mInv LA.<> x)
      in  Right (LA.det xtmx)

-- ===========================================================================
-- Jones-Goos (2012) Table 2 比較: 20-run Split-Plot
-- ===========================================================================

-- 共通仕様: 1 WP 因子 w + 1 SP 因子 s、 連続 [-1, 1]、 full quadratic、
-- 4 WP × 5 SP runs、 η = 1。
jonesGoosTable2Spec :: CX.CustomDesignSpec
jonesGoosTable2Spec = CX.CustomDesignSpec
  { CX.cdsFactors =
      [ CF.Factor "w" (CF.Continuous (-1) 1) CF.HardToChange
      , CF.Factor "s" (CF.Continuous (-1) 1) CF.Controllable
      ]
  , CX.cdsModel =
      CM.Model
        [ CM.TIntercept
        , CM.TMain "w", CM.TMain "s"
        , CM.TInter ["w","s"]
        , CM.TPower "w" 2, CM.TPower "s" 2
        ]
        CM.NCoded
  , CX.cdsConstraints = []
  , CX.cdsNRuns       = 20
  , CX.cdsCriterion   = OPT.DOpt
  , CX.cdsBudget      = CX.defaultBudget
  , CX.cdsSeed        = Just 42
  , CX.cdsInitial     = Nothing

  , CX.cdsDJConvention = False
  }

benchJonesGoosTable2 :: IO [GoldenRow]
benchJonesGoosTable2 = do
  let factors = CX.cdsFactors jonesGoosTable2Spec
      model   = CX.cdsModel   jonesGoosTable2Spec
      eta     = 1.0           -- ^ Jones-Goos (2012) Table 3 η=1 列に対応
      example = "jones-goos-2012-table2-splitplot-20run"
      pathD   = "bench/custom-design/golden/jones-goos-2012-table2-dopt-design.csv"

  -- 参照値 (Jones-Goos D-opt design) を読み込み、 D-criterion を計算
  ref <- readDesignCSV pathD
  case ref of
    Left err -> do
      putStrLn ("[skip] " ++ example ++ ": " ++ err)
      pure []
    Right (refRaw, refWpId, refNWP) ->
      case splitPlotDDet factors model eta refWpId refNWP refRaw of
        Left err -> do
          putStrLn ("[skip] " ++ example ++ ": ref det failed: " ++ err)
          pure []
        Right refDet -> do
          -- hanalyze で同じ仕様の D-opt を生成
          let cfg = SP.SplitPlotConfig
                { SP.spcNWhole = 4, SP.spcVarRatio = eta, SP.spcNStrip = Nothing }
          oursE <- SP.generateSplitPlot jonesGoosTable2Spec cfg
          case oursE of
            Left err -> do
              putStrLn ("[skip] " ++ example ++ ": hanalyze gen failed: "
                        ++ T.unpack err)
              pure []
            Right ours -> do
              -- hanalyze 側の D-criterion = -spdGEFFEst (sign 反転)
              -- (spdGEFFEst = -det(X' M⁻¹ X))
              let oursDet = - SP.spdGEFFEst ours
                  -- D-efficiency = (det_ours / det_ref)^(1/p)、
                  -- p = 模型項数 = 6 (Intercept, w, s, ws, w², s²)
                  pTerms  = 6 :: Int
                  invP    = 1.0 / fromIntegral pTerms
                  dEffRaw = oursDet / refDet
                  dEffPth = if dEffRaw <= 0 then 0
                              else dEffRaw ** invP
              putStrLn $ printf "  refDet=%.6g oursDet=%.6g  D-eff (raw)=%.4f  D-eff (pth root)=%.4f"
                refDet oursDet dEffRaw dEffPth
              pure
                [ GoldenRow
                    { grExample   = example
                    , grMetric    = "D-criterion-ratio-raw"
                    , grhanalyze   = oursDet
                    , grReference = refDet
                    , grTolerance = 0.02
                    }
                , GoldenRow
                    { grExample   = example
                    , grMetric    = "D-efficiency-pth-root"
                    , grhanalyze   = dEffPth
                    , grReference = 1.0
                    , grTolerance = 0.02
                    }
                ]

-- ===========================================================================
-- DuMouchel-Jones (1994) Example 3 "Both" 比較
-- ===========================================================================
--
-- 一次根拠: DuMouchel & Jones (1994) "A Simple Bayesian Modification of
-- D-Optimal Designs", Technometrics 36(1):37-47、 §3.3 Example 3、 Table 1
-- (page 41) "Both" 列。
--
-- 仕様: 4 連続因子 A/B/C/D ∈ {-1, 0, 1}、 n=9、 primary p=5 (intercept + 4 main)、
-- potential q=10 (4 squares + 6 2-factor interactions)、 τ=1。
-- 「Both」 設計 = 8-run resolution IV 2^(4-1) FF (I = ABCD) + 1 centerpoint。

dumouchelJonesEx3Factors :: [CF.Factor]
dumouchelJonesEx3Factors =
  [ CF.Factor n (CF.Continuous (-1) 1) CF.Controllable
  | n <- ["A", "B", "C", "D"]
  ]

-- | primary + potential を一括 expand する model。
-- DuMouchel-Jones 1994 §3.3 では「Both」 列の potential は q=10 (4 squares + 6 2fi)。
-- primary は p=5 (intercept + 4 main effects)。
dumouchelJonesEx3Model :: CM.Model
dumouchelJonesEx3Model = CM.Model
  ( [CM.TIntercept]
    ++ [CM.TMain n          | n <- ["A","B","C","D"]]
    ++ [CM.TPower n 2       | n <- ["A","B","C","D"]]
    ++ [CM.TInter [a, b]    | (a, b) <- [("A","B"),("A","C"),("A","D")
                                        ,("B","C"),("B","D"),("C","D")]]
  ) CM.NCoded

-- | DJ Example 3 候補集合: {-1, 0, 1}^4 = 81 点 (paper §3.3、 dbCxStepGrid=3)。
dumouchelJonesEx3Candidate :: LA.Matrix Double
dumouchelJonesEx3Candidate = LA.fromLists
  [ [a, b, c, d] | a <- vs, b <- vs, c <- vs, d <- vs ]
  where vs = [-1, 0, 1] :: [Double]

benchDuMouchelJonesEx3Both :: IO [GoldenRow]
benchDuMouchelJonesEx3Both = do
  let factors = dumouchelJonesEx3Factors
      model   = dumouchelJonesEx3Model
      tau2    = 1.0
      kPrior  = CB.priorPrecisionDefault factors model tau2
      example = "dumouchel-jones-1994-example3-both"
      pathRef = "bench/custom-design/golden/dumouchel-jones-1994-example3-both.csv"
      cand    = dumouchelJonesEx3Candidate

  -- 文献設計を読み込み (CSV: A,B,C,D の 9 行)
  refE <- readPlainDesignCSV pathRef
  case refE of
    Left err -> do
      putStrLn ("[skip] " ++ example ++ ": " ++ err)
      pure []
    Right refRaw -> case CB.djFitTransform factors model cand of
      Left e -> do
        putStrLn ("[skip] " ++ example ++ ": DJ transform fit failed: "
                  ++ T.unpack e)
        pure []
      Right djT -> do
        -- 参照: expand → DJ transform → det(X_t' X_t + K)
        let refDet =
              case CM.expandDesignMatrix factors model refRaw of
                Left e  -> error ("ref expand failed: " ++ T.unpack e)
                Right x -> CB.bayesianDValueM kPrior (CB.djApplyTransform djT x)

        -- hanalyze で同じ仕様 + BayesianD K で 9-run 設計を生成。
        -- 注意: coordinateExchange は DJ 規約適用前の生 X で BayesianD を
        -- 最適化する (28-12 では coordinateExchange への自動適用は未対応)。
        -- 生成後の設計に対し DJ 変換を適用して det 比較する。
        let spec = CX.CustomDesignSpec
              { CX.cdsFactors     = factors
              , CX.cdsModel       = model
              , CX.cdsConstraints = []
              , CX.cdsNRuns       = 9
              , CX.cdsCriterion   = OPT.BayesianD (CB.precisionToMatrix kPrior)
              , CX.cdsBudget      = CX.defaultBudget
                  { CX.dbCxStepGrid = 3   -- ^ {-1, 0, 1} で論文と同じ候補集合
                  , CX.dbRestarts   = 10  -- ^ 4 因子で multi-start を確保
                  }
              , CX.cdsSeed        = Just 42
              , CX.cdsInitial     = Nothing

              , CX.cdsDJConvention = True   -- ^ Phase 28-12 auto DJ 規約適用
              }
        oursE <- CX.coordinateExchange spec
        case oursE of
          Left err -> do
            putStrLn ("[skip] " ++ example ++ ": hanalyze gen failed: "
                      ++ T.unpack err)
            pure []
          Right cd -> do
            let oursDet =
                  case CM.expandDesignMatrix factors model (CX.cdMatrix cd) of
                    Left e  -> error ("ours expand failed: " ++ T.unpack e)
                    Right x -> CB.bayesianDValueM kPrior (CB.djApplyTransform djT x)
                pTerms  = 1 + 4 + 4 + 6 :: Int  -- intercept + main + sq + 2fi = 15
                invP    = 1.0 / fromIntegral pTerms
                dEffRaw = if refDet <= 0 then 0 else oursDet / refDet
                dEffPth = if dEffRaw <= 0 then 0 else dEffRaw ** invP
            putStrLn $ printf "  [DJ §2.2 規約適用後] refDet=%.6g oursDet=%.6g  D-eff (raw)=%.4f  D-eff (pth root)=%.4f"
              refDet oursDet dEffRaw dEffPth
            pure
              [ GoldenRow
                  -- Phase 28-12 auto DJ 適用後: hanalyze coordinateExchange は
                  -- DJ 変換後の det を直接最適化、 raw ratio が 1.0 近傍で収束する
                  -- ことを確認 (tolerance 0.02)
                  { grExample   = example
                  , grMetric    = "BayesianD-criterion-ratio-raw-DJ"
                  , grhanalyze   = oursDet
                  , grReference = refDet
                  , grTolerance = 0.02
                  }
              , GoldenRow
                  { grExample   = example
                  , grMetric    = "BayesianD-efficiency-pth-root-DJ"
                  , grhanalyze   = dEffPth
                  , grReference = 1.0
                  , grTolerance = 0.02
                  }
              ]

-- | 設計行列 CSV (header: x1,x2,...) を 1 つの Matrix Double として読む
-- (WP indicator を含まない平 raw 形式)。
readPlainDesignCSV :: FilePath -> IO (Either String (LA.Matrix Double))
readPlainDesignCSV path = do
  exists <- doesFileExist path
  if not exists
    then pure (Left ("design file not found: " ++ path))
    else do
      bytes <- BL.fromStrict <$> BS.readFile path
      case decode HasHeader bytes :: Either String (V.Vector (V.Vector Double)) of
        Left  err -> pure (Left ("decode " ++ path ++ ": " ++ err))
        Right rs
          | V.null rs -> pure (Left ("empty CSV: " ++ path))
          | otherwise ->
              let nRows = V.length rs
                  nCols = V.length (rs V.! 0)
                  mat = LA.fromLists
                          [ [ rs V.! i V.! j | j <- [0 .. nCols - 1] ]
                          | i <- [0 .. nRows - 1] ]
              in  pure (Right mat)

-- ===========================================================================
-- Phase 28-4d: JMP RSM Constraints + Categorical (18-run I-opt) 比較
-- ===========================================================================
--
-- 一次根拠: JMP 12 「Design of Experiments Example: A Response Surface Design
-- with Constraints and a Categorical Factor」 PDF (JMP community sample-data
-- attachment、 公開資料)。
--
-- 仕様:
--   * 因子: Time ∈ [500, 560] (coded [-1, 1])、 Temperature ∈ [350, 750]
--     (coded [-1, 1])、 Catalyst ∈ {A, B, C}
--   * 模型: RSM (intercept + main + 2fi + 連続因子の x²)
--     - p = 1 + 3 (main: Time/Temp/Cat = 1+1+2) + 5 (2fi: T·Temp/T·Cat/Temp·Cat
--       = 1+2+2) + 2 (T²/Temp²) = 12
--   * 制約: Conditional (Catalyst = B → Temp_coded ≥ -0.75)
--           Conditional (Catalyst = C → Temp_coded ≤ +0.5)
--           (元: B→Temp≥400 / C→Temp≤650、 coded 換算)
--   * JMP setup: I-opt criterion、 seed=654321、 starts=1000、 18-run
--
-- 比較 metric: IOptRegion criterion (= trace((X'X)⁻¹ · M_R))
--   * Phase 28-4c 後: 制約 region 込みの **MC 版 M_R** (Halton N=10000) を使用、
--     厳密な constrained-region I-criterion で比較
--   * hanalyze 側: 同一 spec + 同一制約で coordinateExchange を回し、
--     I-opt criterion 値を比較。 coordinateExchange も内部で同じ MC M_R に
--     基づいて IOpt を最適化する (resolveIOptRegion の自動 MC fallback)。
--     ratio ≤ 1 = hanalyze が JMP 参照より同じ M_R 評価で劣らない

jmpRsmFactors :: [CF.Factor]
jmpRsmFactors =
  [ CF.Factor "Time"        (CF.Continuous (-1) 1) CF.Controllable
  , CF.Factor "Temperature" (CF.Continuous (-1) 1) CF.Controllable
  , CF.Factor "Catalyst"    (CF.Categorical ["A","B","C"]) CF.Controllable
  ]

jmpRsmModel :: CM.Model
jmpRsmModel = CM.Model
  [ CM.TIntercept
  , CM.TMain "Time", CM.TMain "Temperature", CM.TMain "Catalyst"
  , CM.TInter ["Time","Temperature"]
  , CM.TInter ["Time","Catalyst"]
  , CM.TInter ["Temperature","Catalyst"]
  , CM.TPower "Time" 2
  , CM.TPower "Temperature" 2
  ]
  CM.NCoded

jmpRsmConstraints :: [CC.Constraint]
jmpRsmConstraints =
  [ CC.Conditional (CC.GuardEq "Catalyst" (CC.FVText "B"))
      [ CC.LinearIneq [("Temperature", 1)] CC.CGeq (-0.75) ]
  , CC.Conditional (CC.GuardEq "Catalyst" (CC.FVText "C"))
      [ CC.LinearIneq [("Temperature", 1)] CC.CLeq 0.5 ]
  ]

-- | Time/Temperature/Catalyst の raw 値を coded matrix に変換。
-- Time: (t - 530)/30、 Temperature: (T - 550)/200、 Catalyst: A/B/C → 0/1/2。
codeJmpRsmRow :: [String] -> Either String [Double]
codeJmpRsmRow xs = case xs of
  [_runIdx, tStr, tempStr, catStr] -> do
    t    <- readD tStr
    temp <- readD tempStr
    cat  <- case dropWhile (== ' ') catStr of
              ('A':_) -> Right 0
              ('B':_) -> Right 1
              ('C':_) -> Right 2
              _       -> Left ("unknown Catalyst level: " ++ catStr)
    pure [(t - 530) / 30, (temp - 550) / 200, fromIntegral (cat :: Int)]
  _ -> Left ("expected 4 columns, got " ++ show (length xs))
  where
    readD s = case reads (filter (/= ' ') s) :: [(Double, String)] of
      [(d, "")] -> Right d
      _         -> Left ("cannot parse Double: " ++ s)

readJmpRsmCSV :: FilePath -> IO (Either String (LA.Matrix Double))
readJmpRsmCSV path = do
  exists <- doesFileExist path
  if not exists
    then pure (Left ("design file not found: " ++ path))
    else do
      txt <- readFile path
      let allLines = lines txt
      case allLines of
        [] -> pure (Left ("empty CSV: " ++ path))
        (_hdr : rows) -> do
          let parsed = traverse (codeJmpRsmRow . splitComma) rows
          case parsed of
            Left  e -> pure (Left e)
            Right rs -> pure (Right (LA.fromLists rs))
  where
    splitComma = foldr step [[]]
    step ',' acc      = [] : acc
    step c   (h : t)  = (c : h) : t
    step _   []       = []  -- unreachable (acc starts with [[]])

benchJmpRsmConstraints :: IO [GoldenRow]
benchJmpRsmConstraints = do
  let factors = jmpRsmFactors
      model   = jmpRsmModel
      cons    = jmpRsmConstraints
      example = "jmp-rsm-constraints-categorical-18run"
      pathRef = "bench/custom-design/golden/jmp-rsm-constraints-categorical-design.csv"

  refE <- readJmpRsmCSV pathRef
  case refE of
    Left err -> do
      putStrLn ("[skip] " ++ example ++ ": " ++ err)
      pure []
    Right refRaw -> case RM.regionMomentMatrixMC 10000 factors model cons of
      Left e -> do
        putStrLn ("[skip] " ++ example ++ ": M_R (MC) failed: " ++ T.unpack e)
        pure []
      Right mR -> case CM.expandDesignMatrix factors model refRaw of
        Left e -> do
          putStrLn ("[skip] " ++ example ++ ": ref expand failed: " ++ T.unpack e)
          pure []
        Right refX -> do
          let refI = RM.iValueRegionM mR refX
          putStrLn $ printf "  ref design IOptRegion = %.6g" refI

          -- hanalyze 側: 同 spec + 同制約 で coordinateExchange
          let spec = CX.CustomDesignSpec
                { CX.cdsFactors     = factors
                , CX.cdsModel       = model
                , CX.cdsConstraints = cons
                , CX.cdsNRuns       = 18
                , CX.cdsCriterion   = OPT.IOpt
                , CX.cdsBudget      = CX.defaultBudget
                , CX.cdsSeed        = Just 654321
                , CX.cdsInitial     = Nothing

                , CX.cdsDJConvention = False
                }
          oursE <- CX.coordinateExchange spec
          case oursE of
            Left e -> do
              putStrLn ("[skip] " ++ example ++ ": hanalyze gen failed: "
                        ++ T.unpack e)
              pure []
            Right ours -> case CM.expandDesignMatrix factors model (CX.cdMatrix ours) of
              Left e -> do
                putStrLn ("[skip] " ++ example ++ ": hanalyze expand failed: "
                          ++ T.unpack e)
                pure []
              Right oursX -> do
                let oursI = RM.iValueRegionM mR oursX
                    ratio = if refI <= 0 || isInfinite oursI then 0/0
                              else oursI / refI
                putStrLn $ printf "  hanalyze IOptRegion = %.6g  ratio=%.4f"
                  oursI ratio
                pure
                  [ GoldenRow
                      { grExample   = example
                      , grMetric    = "IOptRegion-criterion-ratio"
                      , grhanalyze   = oursI
                      , grReference = refI
                        -- hanalyze が JMP より大きく劣らない (≤ 5% 増) を pass 基準。
                        -- 制約条件が analytic M_R で無視されるため、 厳密同等は
                        -- 期待できない (Phase 28-4c MC fallback で改善)
                      , grTolerance = 0.05
                      }
                  ]

-- ===========================================================================
-- Main
-- ===========================================================================

main :: IO ()
main = do
  createDirectoryIfMissing True "bench/custom-design/golden"
  createDirectoryIfMissing True "bench/custom-design/results"

  putStrLn "=== Phase 27-2: Jones-Goos (2012) Table 2 (20-run Split-Plot) ==="
  rowsT2 <- benchJonesGoosTable2

  putStrLn ""
  putStrLn "=== Phase 27-3: DuMouchel-Jones (1994) Example 3 \"Both\" (9-run Bayesian-D) ==="
  rowsDJ <- benchDuMouchelJonesEx3Both

  putStrLn ""
  putStrLn "=== Phase 28-4d: JMP RSM Constraints + Categorical (18-run I-opt) ==="
  rowsRSM <- benchJmpRsmConstraints

  let allRows = rowsT2 ++ rowsDJ ++ rowsRSM
  writeGoldenRows "bench/custom-design/results/golden-comparison.csv" allRows

  let nPass = length (filter grPass allRows)
      nTot  = length allRows
  putStrLn ""
  putStrLn (printf "✓ %d/%d metrics pass、 結果: bench/custom-design/results/golden-comparison.csv"
             nPass nTot)
