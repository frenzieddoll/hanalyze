{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-full-laziness -fno-cse -Wno-incomplete-uni-patterns #-}
-- | Phase 9-16 (Tier 1 + Tier 2) 機能の Python 比較ベンチ (Haskell 側)。
--
-- Python 比較対象あり:
--   * PLS            → sklearn.cross_decomposition.PLSRegression
--   * LDA / QDA      → sklearn.discriminant_analysis.{Linear,Quadratic}DA
--   * HCluster Ward  → scipy.cluster.hierarchy.linkage(method='ward')
--   * Friedman       → scipy.stats.friedmanchisquare
--   * RFClassifier   → sklearn.ensemble.RandomForestClassifier
--   * MLPRegressor   → sklearn.neural_network.MLPRegressor
--
-- Haskell-only (Python 直接等価なし or 重い依存):
--   * TOST、 AFT (lifelines は重)、 EWMA / CUSUM、 GaugeRR、
--     ProcessCapability 非正規、 DoE 診断、 I/E-optimal、 Kalman (statsmodels
--     入れない方針)、 Fit Y by X (wrapper)
--
-- 共通入力 CSV は本ファイルで Halton 列から生成し bench/data/tier12_*.csv に
-- 書き出す。 Python 側は同じ CSV を読む。
module Main where

import qualified Data.Text               as T
import qualified Data.Vector             as V
import qualified Data.Vector.Unboxed     as VU
import qualified Numeric.LinearAlgebra   as LA
import qualified System.Random.MWC       as MWC
import           System.Directory        (createDirectoryIfMissing)
import           System.IO               (withFile, IOMode (..), hPutStrLn)
import           Text.Printf             (printf)

import qualified Hanalyze.Model.PLS                    as PLS
import qualified Hanalyze.Model.Discriminant           as Disc
import qualified Hanalyze.Model.HierarchicalCluster    as HC
import qualified Hanalyze.Model.AFT                    as AFT
import qualified Hanalyze.Model.RandomForestClassifier as RFC
import qualified Hanalyze.Model.NeuralNetwork          as NN
import qualified Hanalyze.Model.StateSpace             as SS
import qualified Hanalyze.Design.GaugeRR               as GRR
import qualified Hanalyze.Design.Quality               as Quality
import qualified Hanalyze.Design.Diagnostics           as DDiag
import qualified Hanalyze.Design.Optimal               as Opt
import qualified Hanalyze.Stat.SPC                     as SPC
import qualified Hanalyze.Stat.Test                    as ST
import qualified Hanalyze.Stat.QuasiRandom             as QR
import qualified Hanalyze.Model.Weibull                as Wei

import           BenchUtil

-- ===========================================================================
-- 共通 helper
-- ===========================================================================

-- 決定的 N(0,1)風列 (Box-Muller via Halton)
gaussianHalton :: Int -> Int -> Int -> [Double]
gaussianHalton primeU primeV n =
  let us = [ QR.radicalInverse primeU i | i <- [1 .. n] ]
      vs = [ QR.radicalInverse primeV i | i <- [1 .. n] ]
  in zipWith (\u v -> sqrt (-2 * log (u + 1e-12)) * cos (2 * pi * v)) us vs

writeMatrixCSV :: FilePath -> [String] -> [[Double]] -> IO ()
writeMatrixCSV path header rows = withFile path WriteMode $ \h -> do
  hPutStrLn h (commaJoin header)
  mapM_ (\r -> hPutStrLn h (commaJoin (map (printf "%.10g") r))) rows
  where
    commaJoin :: [String] -> String
    commaJoin = foldr1 (\a b -> a ++ "," ++ b)

-- ===========================================================================
-- PLS
-- ===========================================================================

-- y = x1 + 0.5*x2 (+ noise),  p = 10 (8 ノイズ列)
plsData :: Int -> Int -> (LA.Matrix Double, LA.Vector Double)
plsData n p =
  let cols = [ LA.fromList (gaussianHalton (primeAt j) (primeAt (j + 1)) n)
             | j <- [0 .. p - 1] ]
      x    = LA.fromColumns cols
      x0   = LA.flatten (x LA.¿ [0])
      x1   = LA.flatten (x LA.¿ [1])
      eps  = LA.fromList (gaussianHalton 19 23 n)
      y    = x0 + LA.scale 0.5 x1 + LA.scale 0.1 eps
  in (x, y)
  where
    primes :: [Int]
    primes = [2,3,5,7,11,13,17,19,23,29,31,37,41,43]
    primeAt :: Int -> Int
    primeAt k = primes !! (k `mod` length primes)

writePLSCSV :: FilePath -> LA.Matrix Double -> LA.Vector Double -> IO ()
writePLSCSV path x y =
  let p = LA.cols x
      header = [ "x" ++ show j | j <- [0 .. p - 1] ] ++ ["y"]
      rows = [ [ LA.atIndex x (i, j) | j <- [0 .. p - 1] ]
                 ++ [LA.atIndex y i]
             | i <- [0 .. LA.rows x - 1] ]
  in writeMatrixCSV path header rows

{-# NOINLINE plsPhantom #-}
plsPhantom :: Int -> LA.Matrix Double -> LA.Vector Double -> Either String PLS.PLSFit
plsPhantom _ x y = case PLS.fitPLS1 (PLS.defaultPLSConfig { PLS.plsN_Components = 3 }) x y of
  Left e  -> Left (show e)
  Right f -> Right f

benchPLS :: Int -> Int -> IO BenchRow
benchPLS n p = do
  let (x, y) = plsData n p
  (medMs, res) <- timeitIO 5 forceR (\i -> pure (plsPhantom i x y))
  let nrmse = case res of
        Right f ->
          let yhat = PLS.predictPLS1 f x
              d = yhat - y
          in sqrt (LA.sumElements (d * d) / fromIntegral n)
                / (LA.maxElement y - LA.minElement y + 1e-12)
        Left _  -> 1/0
  pure BenchRow
    { brSystem = "haskell", brSuite = "tier12"
    , brName = printf "PLS_n%d_p%d" n p
    , brTimeMs = medMs, brAccMain = nrmse, brAccAux = 0
    , brExtra = "k=3"
    }
  where
    forceR (Right f) = LA.atIndex (PLS.plsCoef f) (0, 0)
    forceR _         = 0

-- ===========================================================================
-- LDA / QDA
-- ===========================================================================

ldaData :: Int -> Int -> Int -> (LA.Matrix Double, V.Vector Int)
ldaData nPerClass p k =
  let totN = nPerClass * k
      base = [ LA.fromList (gaussianHalton (primeAt j) (primeAt (j + 1)) totN)
             | j <- [0 .. p - 1] ]
      x0   = LA.fromColumns base
      labels = V.fromList (concat [ replicate nPerClass c | c <- [0 .. k - 1] ])
      -- shift by class on first 2 dims
      shift i =
        let c = labels V.! i
        in LA.fromList ([fromIntegral c * 3, fromIntegral c * 2]
                        ++ replicate (p - 2) 0)
      shifted = LA.fromRows
        [ LA.flatten (x0 LA.? [i]) + shift i | i <- [0 .. totN - 1] ]
  in (shifted, labels)
  where
    primes :: [Int]
    primes = [2,3,5,7,11,13,17,19,23,29]
    primeAt :: Int -> Int
    primeAt j = primes !! (j `mod` length primes)

writeLDACSV :: FilePath -> LA.Matrix Double -> V.Vector Int -> IO ()
writeLDACSV path x y =
  let p = LA.cols x
      n = LA.rows x
      header = [ "x" ++ show j | j <- [0 .. p - 1] ] ++ ["class"]
      rows = [ [ LA.atIndex x (i, j) | j <- [0 .. p - 1] ]
                 ++ [fromIntegral (y V.! i)]
             | i <- [0 .. n - 1] ]
  in writeMatrixCSV path header rows

{-# NOINLINE ldaPhantom #-}
ldaPhantom :: Int -> LA.Matrix Double -> V.Vector Int -> Disc.DiscriminantFit
ldaPhantom _ x y = case Disc.fitLDA x y of
  Right f -> f
  Left _  -> error "LDA fit failed"

{-# NOINLINE qdaPhantom #-}
qdaPhantom :: Int -> LA.Matrix Double -> V.Vector Int -> Disc.DiscriminantFit
qdaPhantom _ x y = case Disc.fitQDA x y of
  Right f -> f
  Left _  -> error "QDA fit failed"

benchLDA :: Int -> Int -> Int -> IO BenchRow
benchLDA nPerClass p k = do
  let (x, y) = ldaData nPerClass p k
  (medMs, fit) <- timeitIO 5 (\f -> LA.atIndex (Disc.dfPriors f) 0)
                              (\i -> pure (ldaPhantom i x y))
  let (preds, _) = Disc.predictDiscriminant fit x
      correct = length [ () | i <- [0 .. V.length y - 1], preds V.! i == y V.! i ]
      acc = fromIntegral correct / fromIntegral (V.length y)
  pure BenchRow
    { brSystem = "haskell", brSuite = "tier12"
    , brName = printf "LDA_n%d_p%d_k%d" (nPerClass * k) p k
    , brTimeMs = medMs, brAccMain = acc, brAccAux = 0
    , brExtra = ""
    }

benchQDA :: Int -> Int -> Int -> IO BenchRow
benchQDA nPerClass p k = do
  let (x, y) = ldaData nPerClass p k
  (medMs, fit) <- timeitIO 5 (\f -> LA.atIndex (Disc.dfPriors f) 0)
                              (\i -> pure (qdaPhantom i x y))
  let (preds, _) = Disc.predictDiscriminant fit x
      correct = length [ () | i <- [0 .. V.length y - 1], preds V.! i == y V.! i ]
      acc = fromIntegral correct / fromIntegral (V.length y)
  pure BenchRow
    { brSystem = "haskell", brSuite = "tier12"
    , brName = printf "QDA_n%d_p%d_k%d" (nPerClass * k) p k
    , brTimeMs = medMs, brAccMain = acc, brAccAux = 0
    , brExtra = ""
    }

-- ===========================================================================
-- Hierarchical Cluster Ward
-- ===========================================================================

hcData :: Int -> LA.Matrix Double
hcData n = fst (plsData n 5)

writeHCData :: FilePath -> LA.Matrix Double -> IO ()
writeHCData path x =
  let p = LA.cols x
      n = LA.rows x
      header = [ "x" ++ show j | j <- [0 .. p - 1] ]
      rows = [ [ LA.atIndex x (i, j) | j <- [0 .. p - 1] ]
             | i <- [0 .. n - 1] ]
  in writeMatrixCSV path header rows

{-# NOINLINE hcPhantom #-}
hcPhantom :: Int -> LA.Matrix Double -> HC.HClusterFit
hcPhantom _ = HC.fitHierarchical HC.Ward

benchHC :: Int -> IO BenchRow
benchHC n = do
  let x = hcData n
  (medMs, fit) <- timeitIO 3 (\f -> head (HC.hcHeights f))
                              (\i -> pure (hcPhantom i x))
  pure BenchRow
    { brSystem = "haskell", brSuite = "tier12"
    , brName = printf "HClusterWard_n%d" n
    , brTimeMs = medMs
    , brAccMain = last (HC.hcHeights fit)
    , brAccAux = 0
    , brExtra = ""
    }

-- ===========================================================================
-- Friedman
-- ===========================================================================

friedmanData :: Int -> LA.Matrix Double
friedmanData nBlocks =
  let xs = [ [ fromIntegral b + fromIntegral t * 0.5
                 + (gaussianHalton (2 + t) (3 + t) nBlocks !! b) * 0.1
             | t <- [0 .. 2] ]
           | b <- [0 .. nBlocks - 1] ]
  in LA.fromLists xs

writeFriedmanCSV :: FilePath -> LA.Matrix Double -> IO ()
writeFriedmanCSV path m =
  let header = [ "t0", "t1", "t2" ]
      rows = [ [ LA.atIndex m (i, j) | j <- [0 .. 2] ]
             | i <- [0 .. LA.rows m - 1] ]
  in writeMatrixCSV path header rows

{-# NOINLINE friedmanPhantom #-}
friedmanPhantom :: Int -> LA.Matrix Double -> ST.TestResult
friedmanPhantom _ = ST.friedmanTest

benchFriedman :: Int -> IO BenchRow
benchFriedman n = do
  let m = friedmanData n
  (medMs, tr) <- timeitIO 7 ST.trStatistic (\i -> pure (friedmanPhantom i m))
  pure BenchRow
    { brSystem = "haskell", brSuite = "tier12"
    , brName = printf "Friedman_n%d" n
    , brTimeMs = medMs
    , brAccMain = ST.trStatistic tr
    , brAccAux = ST.trPValue tr
    , brExtra = ""
    }

-- ===========================================================================
-- RF Classifier
-- ===========================================================================

benchRFC :: Int -> Int -> Int -> IO BenchRow
benchRFC n p k = do
  gen <- MWC.create
  let (x, y) = ldaData (n `div` k) p k
      yU = VU.fromList (V.toList y)
  (medMs, fit) <- timeitIO 3 RFC.rfcOOBError
                              (\_ -> RFC.fitRFClassifier
                                       (RFC.defaultRFCConfig { RFC.rfcNTrees = 50 })
                                       x yU gen)
  pure BenchRow
    { brSystem = "haskell", brSuite = "tier12"
    , brName = printf "RFC_n%d_p%d_k%d" n p k
    , brTimeMs = medMs
    , brAccMain = 1 - RFC.rfcOOBError fit
    , brAccAux = 0
    , brExtra = "trees=50"
    }

-- ===========================================================================
-- MLP Regressor
-- ===========================================================================

benchMLP :: Int -> Int -> IO BenchRow
benchMLP n p = do
  gen <- MWC.create
  let (x, y) = plsData n p
      cfg = NN.defaultMLP
              { NN.mlpHidden = [16]
              , NN.mlpEpochs = 100
              , NN.mlpBatch  = 16
              , NN.mlpLR     = 0.01
              }
  (medMs, fit) <- timeitIO 3 (\f -> last (NN.mlpLossHist f))
                              (\_ -> NN.fitMLPRegressor cfg x y gen)
  let preds = LA.flatten (NN.predictMLP fit x)
      d = preds - y
      mse = LA.sumElements (d * d) / fromIntegral n
  pure BenchRow
    { brSystem = "haskell", brSuite = "tier12"
    , brName = printf "MLPRegressor_n%d_p%d" n p
    , brTimeMs = medMs
    , brAccMain = mse
    , brAccAux = 0
    , brExtra = "hidden=16 epochs=100"
    }

-- ===========================================================================
-- Haskell-only benches
-- ===========================================================================

benchTOST :: Int -> IO BenchRow
benchTOST n = do
  let xs = LA.fromList (take n (gaussianHalton 2 3 (n * 2)))
      ys = LA.fromList (take n (drop n (gaussianHalton 2 3 (n * 2))))
      delta = 0.5
  (medMs, tr) <- timeitIO 7 ST.trPValue
                              (\_ -> pure (ST.tostWelch xs ys delta))
  pure BenchRow
    { brSystem = "haskell", brSuite = "tier12"
    , brName = printf "TOSTWelch_n%d" n
    , brTimeMs = medMs
    , brAccMain = ST.trPValue tr
    , brAccAux = ST.trStatistic tr
    , brExtra = "no_python_equivalent"
    }

benchAFT :: Int -> IO BenchRow
benchAFT n = do
  let ts = LA.fromList [ exp (1 + (gaussianHalton 5 7 n !! i) * 0.3)
                       | i <- [0 .. n - 1] ]
      delta_ = V.fromList (replicate n True)
      x1 = LA.fromColumns [LA.fromList (replicate n 1.0)]
  (medMs, r) <- timeitIO 3 (\_ -> 1.0)
                            (\_ -> AFT.fitAFT AFT.AFTLogNormal x1 ts delta_)
  let sc = case r of
        Right f -> AFT.aftScale f
        Left _  -> 1/0
  pure BenchRow
    { brSystem = "haskell", brSuite = "tier12"
    , brName = printf "AFTLogNormal_n%d" n
    , brTimeMs = medMs
    , brAccMain = sc
    , brAccAux = 0
    , brExtra = "no_python_equivalent"
    }

{-# NOINLINE ewmaPhantom #-}
ewmaPhantom :: Int -> V.Vector Double -> Either T.Text [SPC.SPCChartResult]
ewmaPhantom _ xs = SPC.fitSPC SPC.EWMAChart (SPC.EWMAInput xs 0.2 3.0 0 1.0)

benchEWMA :: Int -> IO BenchRow
benchEWMA n = do
  let xs = LA.fromList (gaussianHalton 11 13 n)
      xsV = V.fromList (LA.toList xs)
  (medMs, res) <- timeitIO 7 forceEWMA (\i -> pure (ewmaPhantom i xsV))
  let lastZ = case res of
        Right (ch:_) -> V.last (SPC.spcPoints ch)
        _            -> 0
  pure BenchRow
    { brSystem = "haskell", brSuite = "tier12"
    , brName = printf "EWMA_n%d" n
    , brTimeMs = medMs, brAccMain = lastZ, brAccAux = 0
    , brExtra = "no_python_equivalent"
    }
  where
    forceEWMA (Right (ch:_)) = V.sum (SPC.spcPoints ch)
    forceEWMA _              = 0

{-# NOINLINE cusumPhantom #-}
cusumPhantom :: Int -> V.Vector Double -> Either T.Text [SPC.SPCChartResult]
cusumPhantom _ xs = SPC.fitSPC SPC.CUSUMChart (SPC.CUSUMInput xs 0 1.0 0.5 4.0)

benchCUSUM :: Int -> IO BenchRow
benchCUSUM n = do
  let xs = LA.fromList (gaussianHalton 11 13 n)
      xsV = V.fromList (LA.toList xs)
  (medMs, res) <- timeitIO 7 forceCUSUM (\i -> pure (cusumPhantom i xsV))
  let lastC = case res of
        Right (cp:_) -> V.last (SPC.spcPoints cp)
        _            -> 0
  pure BenchRow
    { brSystem = "haskell", brSuite = "tier12"
    , brName = printf "CUSUM_n%d" n
    , brTimeMs = medMs, brAccMain = lastC, brAccAux = 0
    , brExtra = "no_python_equivalent"
    }
  where
    forceCUSUM (Right (cp:_)) = V.sum (SPC.spcPoints cp)
    forceCUSUM _              = 0

{-# NOINLINE gaugeRRPhantom #-}
gaugeRRPhantom :: Int -> V.Vector Int -> V.Vector Int -> V.Vector Double
               -> Either T.Text GRR.GaugeRRResult
gaugeRRPhantom _ ops parts ys = GRR.gaugeRRCrossed ops parts ys

benchGaugeRR :: IO BenchRow
benchGaugeRR = do
  let parts = V.fromList (concat (replicate 9 [0, 1, 2]))
      ops   = V.fromList (concatMap (\o -> replicate 9 o) [0, 1, 2])
      ys    = V.fromList
        [ fromIntegral (parts V.! i) * 2
          + fromIntegral (ops V.! i) * 0.1
          + (gaussianHalton 17 19 27 !! i) * 0.05
        | i <- [0 .. 26] ]
  (medMs, res) <- timeitIO 7 forceGRR
                              (\i -> pure (gaugeRRPhantom i ops parts ys))
  let pctGRR = case res of
        Right r -> GRR.grrPctGRR r
        _       -> 0
  pure BenchRow
    { brSystem = "haskell", brSuite = "tier12"
    , brName = "GaugeRRCrossed_3p3o3r"
    , brTimeMs = medMs, brAccMain = pctGRR, brAccAux = 0
    , brExtra = "no_python_equivalent"
    }
  where
    forceGRR (Right r) = GRR.grrTotalVar r + GRR.grrPartVar r
    forceGRR _         = 0

benchProcessCapWeibull :: IO BenchRow
benchProcessCapWeibull = do
  let wf = Wei.WeibullFit 2.0 100.0 0 0 0 (0, 0, 0)
  (medMs, cap) <- timeitIO 100 Quality.capCp
    (\_ -> pure (Quality.processCapabilityWeibull wf 10 300))
  pure BenchRow
    { brSystem = "haskell", brSuite = "tier12"
    , brName = "ProcCapWeibull"
    , brTimeMs = medMs, brAccMain = Quality.capCp cap, brAccAux = Quality.capCpk cap
    , brExtra = "no_python_equivalent"
    }

benchDoEDiag :: IO BenchRow
benchDoEDiag = do
  let x = LA.fromLists
            [ [1, x1, x2, x1 * x2, x1 * x1, x2 * x2]
            | x1 <- [-1, 0, 1], x2 <- [-1, 0, 1] ]
  (medMs, dd) <- timeitIO 30 DDiag.ddDEff
                              (\_ -> pure (DDiag.diagnostics x))
  pure BenchRow
    { brSystem = "haskell", brSuite = "tier12"
    , brName = "DoEDiagnostics_n9p6"
    , brTimeMs = medMs
    , brAccMain = DDiag.ddDEff dd
    , brAccAux = DDiag.ddAEff dd
    , brExtra = "no_python_equivalent"
    }

{-# NOINLINE optimalPhantom #-}
optimalPhantom :: Int -> Opt.OptCriterion -> [[Double]] -> Int -> ([Int], [[Double]])
optimalPhantom _ crit cands seed = Opt.optimalDesign crit cands 6 seed

benchIEOptimal :: Opt.OptCriterion -> String -> IO BenchRow
benchIEOptimal crit label = do
  let cands = [ [1, x1, x2, x1 * x2] | x1 <- [-1, 0, 1], x2 <- [-1, 0, 1] ]
  (medMs, (idxs, _)) <- timeitIO 5 forceOpt
    (\i -> pure (optimalPhantom i crit cands (42 + i)))
  pure BenchRow
    { brSystem = "haskell", brSuite = "tier12"
    , brName = label ++ "_n9_6"
    , brTimeMs = medMs
    , brAccMain = fromIntegral (sum idxs)
    , brAccAux = 0
    , brExtra = "no_python_equivalent"
    }
  where
    forceOpt (idxs, _) = fromIntegral (sum idxs)

benchKalman :: Int -> IO BenchRow
benchKalman tT = do
  let obs = LA.fromLists
              [ [ fromIntegral i * 0.1 + (gaussianHalton 5 7 tT !! i) * 0.1
                | i <- [0 .. tT - 1] ] ]
      ssm = SS.StateSpaceModel
        { SS.ssF  = LA.fromLists [[1]]
        , SS.ssH  = LA.fromLists [[1]]
        , SS.ssQ  = LA.fromLists [[0.01]]
        , SS.ssR  = LA.fromLists [[0.1]]
        , SS.ssX0 = LA.fromList  [0]
        , SS.ssP0 = LA.fromLists [[1.0]]
        }
  (medMs, kr) <- timeitIO 5 SS.krLogLik
                              (\_ -> pure (SS.kalmanFilter ssm obs))
  pure BenchRow
    { brSystem = "haskell", brSuite = "tier12"
    , brName = printf "KalmanFilter_T%d" tT
    , brTimeMs = medMs
    , brAccMain = SS.krLogLik kr
    , brAccAux = 0
    , brExtra = "no_python_equivalent"
    }

-- ===========================================================================
-- Main
-- ===========================================================================

main :: IO ()
main = do
  createDirectoryIfMissing True "bench/data"
  createDirectoryIfMissing True "bench/results/haskell"

  -- 共通 CSV を書き出し (Python 側が読む)
  let plsParams = [(100, 10), (500, 10)]
  mapM_ (\(n, p) -> do
            let (x, y) = plsData n p
            writePLSCSV (printf "bench/data/tier12_pls_n%d_p%d.csv" n p) x y)
        plsParams

  let ldaParams = [(30, 5, 3), (100, 5, 3)]
  mapM_ (\(nc, p, k) -> do
            let (x, y) = ldaData nc p k
            writeLDACSV (printf "bench/data/tier12_lda_n%d_p%d_k%d.csv" (nc * k) p k)
                        x y)
        ldaParams

  mapM_ (\n -> writeFriedmanCSV (printf "bench/data/tier12_friedman_n%d.csv" n)
                                 (friedmanData n))
        [10, 30, 100 :: Int]

  mapM_ (\n -> writeHCData (printf "bench/data/tier12_hc_n%d.csv" n) (hcData n))
        [20, 50 :: Int]

  plsRows  <- mapM (\(n, p) -> benchPLS n p) plsParams
  ldaRows  <- mapM (\(nc, p, k) -> benchLDA nc p k) ldaParams
  qdaRows  <- mapM (\(nc, p, k) -> benchQDA nc p k) ldaParams
  hcRows   <- mapM benchHC [20, 50 :: Int]
  friRows  <- mapM benchFriedman [10, 30, 100 :: Int]
  -- RFC は LDA と同じ data CSV を使うので n_total = nc*k を渡す
  rfcRows  <- mapM (\(nc, p, k) -> benchRFC (nc * k) p k) ldaParams
  mlpRows  <- mapM (\(n, p) -> benchMLP n p) plsParams
  -- Haskell-only
  tostRows <- mapM benchTOST [50, 200 :: Int]
  aftRows  <- mapM benchAFT [50, 200 :: Int]
  ewmaRows <- mapM benchEWMA [100, 500 :: Int]
  cusRows  <- mapM benchCUSUM [100, 500 :: Int]
  grrRow   <- benchGaugeRR
  pcRow    <- benchProcessCapWeibull
  diagRow  <- benchDoEDiag
  iOptRow  <- benchIEOptimal Opt.IOpt "IOptimal"
  eOptRow  <- benchIEOptimal Opt.EOpt "EOptimal"
  kfRows   <- mapM benchKalman [50, 200 :: Int]

  writeRows "bench/results/haskell/tier12.csv"
    (concat [ plsRows, ldaRows, qdaRows, hcRows, friRows
            , rfcRows, mlpRows
            , tostRows, aftRows, ewmaRows, cusRows
            , [grrRow, pcRow, diagRow, iOptRow, eOptRow]
            , kfRows ])
  putStrLn "✓ bench/results/haskell/tier12.csv written"
