{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-full-laziness -fno-cse -Wno-incomplete-uni-patterns #-}
-- | Phase 1-7 機能 (Spotfire/JMP gap) の Python/R 比較ベンチ (Haskell 側)。
--
-- 比較先:
--   * Weibull MLE          → scipy.stats.weibull_min.fit
--   * MANOVA (Wilks Λ)     → 自前 numpy 実装 (Haskell と同 algorithm)
--   * Hotelling T²         → 自前 numpy 実装
--   * Lasso/Ridge λ CV     → sklearn.linear_model.{LassoCV,RidgeCV}
--   * SpaceFilling LHS     → scipy.stats.qmc.LatinHypercube
--   * SpaceFilling Halton  → scipy.stats.qmc.Halton
--   * Augment Design       → Python 直接等価なし (Haskell-only 計測)
--   * DSD (Jones-Nachtsheim) → Python 直接等価なし (Haskell-only 計測)
--   * SPC X̄-R              → Python 直接等価なし (Haskell-only 計測)
--
-- 共通入力 CSV は本ファイルで生成 (Halton 列で再現性確保) し
-- @bench/data/@ に書き出す。 Python 側は同じ CSV を読む。
--
-- 出力: @bench/results/haskell/phase17.csv@ (unified BenchRow schema)。
module Main where

import qualified Data.Text               as T
import qualified Data.Vector             as V
import qualified Numeric.LinearAlgebra   as LA
import qualified System.Random.MWC       as MWC
import           System.Directory        (createDirectoryIfMissing)
import           System.IO               (withFile, IOMode (..), hPutStrLn)
import           Text.Printf             (printf)

import qualified Hanalyze.Model.Weibull         as Wei
import qualified Hanalyze.Model.Regularized     as Reg
import qualified Hanalyze.Design.Optimal        as Opt
import qualified Hanalyze.Design.SpaceFilling   as SF
import qualified Hanalyze.Design.DSD            as DSD
import qualified Hanalyze.Stat.SPC              as SPC
import qualified Hanalyze.Stat.QuasiRandom      as QR
import qualified Hanalyze.Stat.Test             as ST

import           BenchUtil

-- ===========================================================================
-- 共通 helper
-- ===========================================================================

-- | Halton 1D 列で (0, 1) 上の n 個の deterministic uniform 値。
haltonU1 :: Int -> Int -> [Double]
haltonU1 prime n = [ QR.radicalInverse prime i | i <- [1 .. n] ]

-- ===========================================================================
-- Weibull MLE
-- ===========================================================================

weibullSamples :: Int -> Double -> Double -> [Double]
weibullSamples n k lam =
  [ lam * (negate (log (1 - u))) ** (1 / k)
  | u <- haltonU1 2 n
  ]

writeWeibullCSV :: FilePath -> [Double] -> IO ()
writeWeibullCSV path xs = withFile path WriteMode $ \h -> do
  hPutStrLn h "time"
  mapM_ (hPutStrLn h . printf "%.10g") xs

{-# NOINLINE fitWeibullPhantom #-}
fitWeibullPhantom :: Int -> V.Vector Double -> Either T.Text Wei.WeibullFit
fitWeibullPhantom _ = Wei.fitWeibullMLE

benchWeibullMLE :: Int -> Double -> Double -> IO BenchRow
benchWeibullMLE n trueK trueLam = do
  let xs   = weibullSamples n trueK trueLam
      vec  = V.fromList xs
      name = "WeibullMLE_n" ++ show n
  (medMs, result) <- timeitIO 7 forceFit (\i -> pure (fitWeibullPhantom i vec))
  let (kErr, lamErr) = case result of
        Right f -> ( abs (Wei.wfShape f - trueK) / trueK
                   , abs (Wei.wfScale f - trueLam) / trueLam )
        Left _  -> (1/0, 1/0)
  pure BenchRow
    { brSystem = "haskell", brSuite = "phase17", brName = name
    , brTimeMs = medMs, brAccMain = kErr, brAccAux = lamErr
    , brExtra = printf "trueK=%g trueLam=%g" trueK trueLam
    }
  where
    forceFit (Right f) = Wei.wfShape f + Wei.wfScale f
    forceFit (Left _)  = 0

-- ===========================================================================
-- MANOVA
-- ===========================================================================

generateManovaGroups :: Int -> Double -> [LA.Matrix Double]
generateManovaGroups nPerGroup groupShift =
  let centers = [(0, 0), (groupShift, 0), (0, groupShift)]
      mkGroup (cx, cy) i0 =
        LA.fromLists
          [ [ cx + 2 * (QR.radicalInverse 2 (i0 + i) - 0.5)
            , cy + 2 * (QR.radicalInverse 3 (i0 + i) - 0.5)
            ]
          | i <- [1 .. nPerGroup]
          ]
  in [ mkGroup c (g * 1000) | (g, c) <- zip [0..] centers ]

writeManovaCSV :: FilePath -> [LA.Matrix Double] -> IO ()
writeManovaCSV path groups = withFile path WriteMode $ \h -> do
  hPutStrLn h "group,x1,x2"
  let rows =
        [ (g, x1, x2)
        | (g, mat) <- zip [0 :: Int ..] groups
        , row <- LA.toLists mat
        , let [x1, x2] = row
        ]
  mapM_ (\(g, x1, x2) -> hPutStrLn h (printf "%d,%.10g,%.10g" g x1 x2)) rows

{-# NOINLINE manovaPhantom #-}
manovaPhantom :: Int -> [LA.Matrix Double] -> ST.TestResult
manovaPhantom _ = ST.manova

benchManova :: Int -> Double -> IO BenchRow
benchManova nPerGroup groupShift = do
  let groups = generateManovaGroups nPerGroup groupShift
      name   = "MANOVA_3grp_n" ++ show nPerGroup
  (medMs, result) <- timeitIO 7 forceTR (\i -> pure (manovaPhantom i groups))
  let wilks = case ST.trEffect result of
        Just (_, w) -> w
        Nothing     -> 1/0
  pure BenchRow
    { brSystem = "haskell", brSuite = "phase17", brName = name
    , brTimeMs = medMs, brAccMain = wilks, brAccAux = ST.trPValue result
    , brExtra = printf "groupShift=%g 2vars" groupShift
    }
  where
    forceTR tr = ST.trStatistic tr + ST.trPValue tr

-- ===========================================================================
-- Hotelling T² (1-sample)
-- ===========================================================================

-- | n × 2 の deterministic データ、 中心が (shift, shift)。
generateHotellingData :: Int -> Double -> LA.Matrix Double
generateHotellingData n shift =
  LA.fromLists
    [ [ shift + 2 * (QR.radicalInverse 2 i - 0.5)
      , shift + 2 * (QR.radicalInverse 3 i - 0.5)
      ]
    | i <- [1 .. n]
    ]

writeHotellingCSV :: FilePath -> LA.Matrix Double -> IO ()
writeHotellingCSV path mat = withFile path WriteMode $ \h -> do
  hPutStrLn h "x1,x2"
  mapM_ (\[x1, x2] -> hPutStrLn h (printf "%.10g,%.10g" x1 x2)) (LA.toLists mat)

{-# NOINLINE hotellingPhantom #-}
hotellingPhantom :: Int -> LA.Matrix Double -> LA.Vector Double -> ST.TestResult
hotellingPhantom _ = ST.hotellingsT2

benchHotelling :: Int -> Double -> IO BenchRow
benchHotelling n shift = do
  let mat = generateHotellingData n shift
      mu0 = LA.fromList [0.0, 0.0]
      name = "HotellingT2_n" ++ show n
  (medMs, result) <- timeitIO 7 forceTR (\i -> pure (hotellingPhantom i mat mu0))
  let t2 = case ST.trEffect result of
        Just (_, w) -> w
        Nothing     -> 1/0
  pure BenchRow
    { brSystem = "haskell", brSuite = "phase17", brName = name
    , brTimeMs = medMs, brAccMain = t2, brAccAux = ST.trPValue result
    , brExtra = printf "shift=%g 2vars mu0=(0,0)" shift
    }
  where
    forceTR tr = ST.trStatistic tr + ST.trPValue tr

-- ===========================================================================
-- Lasso / Ridge λ CV
-- ===========================================================================

-- | LM-like data: y = X β_true + ε、 X は Halton 多次元、 ε は Halton-Box-Muller。
generateLMData :: Int -> Int -> ([Double], LA.Matrix Double, LA.Vector Double)
generateLMData n p =
  let baseTrue = [2.0, 1.0, 0.5] ++ replicate (max 0 (p - 3)) 0.0
      betaTrue = take p baseTrue
      -- design matrix via Halton p-D
      xMat = LA.fromLists
        [ [ 2 * QR.radicalInverse (primes !! (j - 1)) i - 1
          | j <- [1 .. p]
          ]
        | i <- [1 .. n]
        ]
      -- noise via Box-Muller from Halton (bases 11, 13)
      noises =
        [ let u1 = QR.radicalInverse 11 i
              u2 = QR.radicalInverse 13 i
          in 0.1 * sqrt (-2 * log (max 1e-10 u1)) * cos (2 * pi * u2)
        | i <- [1 .. n]
        ]
      yVec = (xMat LA.#> LA.fromList betaTrue) + LA.fromList noises
  in (betaTrue, xMat, yVec)
  where
    primes :: [Int]
    primes = [ 2,  3,  5,  7, 11, 13, 17, 19, 23, 29
             , 31, 37, 41, 43, 47, 53, 59, 61, 67, 71
             , 73, 79, 83, 89, 97,101,103,107,109,113
             ]

writeLMCSV :: FilePath -> Int -> LA.Matrix Double -> LA.Vector Double -> IO ()
writeLMCSV path p xMat yVec = withFile path WriteMode $ \h -> do
  hPutStrLn h (intercalate "," ([ "x" ++ show j | j <- [1 .. p] ] ++ ["y"]))
  let n = LA.rows xMat
  mapM_ (\i ->
            let xRow = LA.toLists (xMat LA.? [i]) !! 0
                y    = yVec LA.! i
                cells = [ printf "%.10g" x | x <- xRow ] ++ [ printf "%.10g" y ]
            in hPutStrLn h (intercalate "," cells))
        [0 .. n - 1]
  where
    intercalate sep = foldr1 (\a b -> a ++ sep ++ b)

lambdaGrid :: [Double]
lambdaGrid = [0.001, 0.01, 0.05, 0.1, 0.5, 1.0, 5.0]

benchRegCV :: Reg.PenaltyKind -> String -> Int -> Int -> IO BenchRow
benchRegCV kind tag n p = do
  let (_betaTrue, xMat, yVec) = generateLMData n p
      name = tag ++ "CV_n" ++ show n ++ "_p" ++ show p
  -- selectLambdaCV is IO で MWC.GenIO 要なので、 fresh gen を使う
  gen <- MWC.create
  (medMs, sel) <- timeitIO 5 (\s -> Reg.lsBestLambda s)
                              (\_ -> Reg.selectLambdaCV 5 kind lambdaGrid xMat yVec gen)
  pure BenchRow
    { brSystem = "haskell", brSuite = "phase17", brName = name
    , brTimeMs = medMs, brAccMain = Reg.lsBestLambda sel
    , brAccAux = Reg.lsOneSeLambda sel
    , brExtra = printf "grid=%d folds=5" (length lambdaGrid)
    }

-- ===========================================================================
-- SpaceFilling LHS / Halton
-- ===========================================================================

benchLHS :: Int -> Int -> IO BenchRow
benchLHS n d = do
  let name = "LHS_n" ++ show n ++ "_d" ++ show d
  (medMs, sfd) <- timeitIO 5 (\s -> SF.sfdMinDist s)
                              (\_ -> do
                                  gen <- MWC.create
                                  SF.latinHypercube n d gen)
  pure BenchRow
    { brSystem = "haskell", brSuite = "phase17", brName = name
    , brTimeMs = medMs, brAccMain = SF.sfdMinDist sfd
    , brAccAux = 0
    , brExtra = "method=LHS"
    }

benchHalton :: Int -> Int -> IO BenchRow
benchHalton n d = do
  let name = "Halton_n" ++ show n ++ "_d" ++ show d
      sfd  = SF.haltonDesign n d
  (medMs, sfd2) <- timeitIO 5 (\s -> SF.sfdMinDist s)
                               (\_ -> pure sfd)
  let _ = sfd2
  pure BenchRow
    { brSystem = "haskell", brSuite = "phase17", brName = name
    , brTimeMs = medMs, brAccMain = SF.sfdMinDist sfd
    , brAccAux = 0
    , brExtra = "method=Halton"
    }

-- ===========================================================================
-- Augment Design (Haskell-only、 Python 等価なし)
-- ===========================================================================

benchAugment :: Int -> IO BenchRow
benchAugment nNew = do
  let cands = Opt.quadraticCandidates 2 3  -- 9 候補
      existing =
        [ [1, -1, -1, 1, 1,  1]
        , [1,  1, -1, 1, 1, -1]
        , [1, -1,  1, 1, 1, -1]
        , [1,  0,  0, 0, 0,  0]
        ]
      name = "Augment_existing4_add" ++ show nNew
  (medMs, res) <- timeitIO 7 (\r -> Opt.arFinalCrit r)
                              (\_ -> pure (Opt.augmentDesign Opt.DOpt existing nNew cands 42))
  pure BenchRow
    { brSystem = "haskell", brSuite = "phase17", brName = name
    , brTimeMs = medMs, brAccMain = Opt.arFinalCrit res
    , brAccAux = Opt.arInitialCrit res
    , brExtra = "no_python_equivalent"
    }

-- ===========================================================================
-- DSD (Haskell-only)
-- ===========================================================================

benchDSD :: Int -> IO BenchRow
benchDSD k = do
  let name = "DSD_k" ++ show k
  (medMs, res) <- timeitIO 7
        (\eth -> case eth of
            Right r -> fromIntegral (DSD.dsdNRuns r)
            Left  _ -> 0)
        (\_ -> pure (DSD.dsdDesign k))
  let nRuns = case res of
        Right r -> DSD.dsdNRuns r
        Left  _ -> 0
      hasOpt = case res of
        Right r -> if DSD.dsdHasOptimal r then 1 else 0
        Left  _ -> 0 :: Int
  pure BenchRow
    { brSystem = "haskell", brSuite = "phase17", brName = name
    , brTimeMs = medMs, brAccMain = fromIntegral nRuns
    , brAccAux = fromIntegral hasOpt
    , brExtra = "no_python_equivalent"
    }

-- ===========================================================================
-- SPC X̄-R (Haskell-only)
-- ===========================================================================

benchSPC :: Int -> IO BenchRow
benchSPC nSubgroups = do
  let subs = V.fromList
        [ V.fromList
            [ 10 + 0.5 * (QR.radicalInverse 2 (g * 5 + j) - 0.5)
            | j <- [1 .. 5]
            ]
        | g <- [1 .. nSubgroups]
        ]
      name = "SPC_XR_subgroups" ++ show nSubgroups
  (medMs, result) <- timeitIO 7 forceR (\_ -> pure (SPC.fitSPC SPC.XR (SPC.VarSubgroups subs)))
  let center = case result of
        Right (ch:_) -> SPC.spcCenter ch
        _            -> 0
  pure BenchRow
    { brSystem = "haskell", brSuite = "phase17", brName = name
    , brTimeMs = medMs, brAccMain = center
    , brAccAux = 0
    , brExtra = "no_python_equivalent subgroupSize=5"
    }
  where
    forceR (Right (ch:_)) = SPC.spcCenter ch
    forceR _              = 0

-- ===========================================================================
-- Main
-- ===========================================================================

main :: IO ()
main = do
  createDirectoryIfMissing True "bench/data"
  createDirectoryIfMissing True "bench/results/haskell"

  -- Weibull MLE
  let trueK   = 2.0
      trueLam = 10.0
  mapM_ (\n ->
            writeWeibullCSV (printf "bench/data/weibull_n%d.csv" n)
                            (weibullSamples n trueK trueLam))
        [100, 1000, 10000 :: Int]
  weibullRows <- mapM (\n -> benchWeibullMLE n trueK trueLam)
                      [100, 1000, 10000 :: Int]

  -- MANOVA
  let groupShift = 1.5
  mapM_ (\nPer -> do
            let groups = generateManovaGroups nPer groupShift
            writeManovaCSV (printf "bench/data/manova_3grp_n%d.csv" nPer) groups)
        [30, 100, 500 :: Int]
  manovaRows <- mapM (\nPer -> benchManova nPer groupShift)
                     [30, 100, 500 :: Int]

  -- Hotelling T²
  mapM_ (\n -> do
            let mat = generateHotellingData n 0.5
            writeHotellingCSV (printf "bench/data/hotelling_n%d.csv" n) mat)
        [50, 200, 1000 :: Int]
  hotellingRows <- mapM (\n -> benchHotelling n 0.5) [50, 200, 1000 :: Int]

  -- Lasso / Ridge CV
  mapM_ (\(n, p) -> do
            let (_, xMat, yVec) = generateLMData n p
            writeLMCSV (printf "bench/data/lm_n%d_p%d.csv" n p) p xMat yVec)
        [(200, 10), (500, 20)]
  lassoRows <- mapM (\(n, p) -> benchRegCV Reg.KindLasso "Lasso" n p)
                    [(200, 10), (500, 20)]
  ridgeRows <- mapM (\(n, p) -> benchRegCV Reg.KindRidge "Ridge" n p)
                    [(200, 10), (500, 20)]

  -- SpaceFilling
  lhsRows    <- mapM (\(n, d) -> benchLHS n d) [(50, 2), (200, 3)]
  haltonRows <- mapM (\(n, d) -> benchHalton n d) [(50, 2), (200, 3)]

  -- Augment Design
  augmentRows <- mapM benchAugment [2, 3, 4 :: Int]

  -- DSD
  dsdRows <- mapM benchDSD [4, 6, 8, 10 :: Int]

  -- SPC
  spcRows <- mapM benchSPC [10, 30, 100 :: Int]

  writeRows "bench/results/haskell/phase17.csv"
            (weibullRows ++ manovaRows ++ hotellingRows
             ++ lassoRows ++ ridgeRows
             ++ lhsRows ++ haltonRows
             ++ augmentRows ++ dsdRows ++ spcRows)
  putStrLn "✓ bench/results/haskell/phase17.csv written"
