{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-full-laziness -fno-cse #-}

-- | Phase 54.4a per-call 勾配ベンチ (推測するな計測せよ)。
--
-- 54.4a で gradADU をハイブリッド化した (Gaussian-恒等リンク ObserveLM ブロックの
-- 観測尤度勾配を自作 vector-op tape で計算・他は ad)。 その「実速度」 を、 同一の
-- 階層 Gaussian モデル (M2: random intercept) を 2 通りにエンコードして比較する:
--
--   scalar = glmmRandomIntercept (per-obs scalar observe) → gradADU は全体 ad
--   vecLM  = 同型を observeLM で表現 (群効果を設計行列の指示列に畳む)
--            → gradADU はハイブリッド (ObserveLM 部を vec-tape)
--
-- NUTS は 1 draw あたり leapfrog ごとに gradADU を多数回呼ぶので、 per-call の
-- gradADU 単価がそのまま per-draw コストの支配項。 ここでは per-call を直接測る
-- (NUTS の分散・固定費を排した最もクリーンな比較)。 各サイズで中心差分一致も確認。
module Main where

import           Control.Monad   (forM, forM_)
import qualified Data.Map.Strict as Map
import qualified Data.Text       as T
import qualified Data.Vector     as V
import qualified System.Random.MWC               as MWC
import           System.Random.MWC.Distributions (standard)
import           Text.Printf     (printf)

import           Hanalyze.Model.HBM
                   ( Distribution (..), ModelP, LMFamily (..), REff (..)
                   , sample, observe, observeLMR
                   , sampleNames, getTransforms, gradADU, compileGradU
                   , logJointUnconstrained )
import           Hanalyze.Stat.Distribution (Transform)
import           Hanalyze.MCMC.NUTS         (NUTSConfig (..), defaultNUTSConfig, nuts)
import           Hanalyze.MCMC.Core         (Chain, chainTotal)

import           BenchUtil (timeitIO)

-- ---------------------------------------------------------------------------
-- 決定的データ (BenchHBMScaling.genM2 と同型)
-- ---------------------------------------------------------------------------

normals :: Int -> Int -> IO [Double]
normals seed k = do
  g <- MWC.initialize (V.singleton (fromIntegral seed))
  mapM (const (standard g)) [1 .. k]

-- | nG 群 × perG。 xRows=[[1,x]]、 gids、 ys を返す。
genM2 :: Int -> Int -> IO ([[Double]], [Int], [Double])
genM2 nG perG = do
  let n = nG * perG
      (b0, b1, tauU, s) = (1.0, 0.8, 1.5, 1.0) :: (Double, Double, Double, Double)
  xz <- normals 21 n
  ez <- normals 22 n
  uz <- normals 23 nG
  let us   = map (* tauU) uz
      gids = [ i `div` perG | i <- [0 .. n - 1] ]
      xs   = map (* 2.0) xz
      ys   = [ b0 + b1 * x + (us !! g) + s * e | (x, g, e) <- zip3 xs gids ez ]
      xRows = [ [1.0, x] | x <- xs ]
  pure (xRows, gids, ys)

-- ---------------------------------------------------------------------------
-- 2 通りのエンコード
-- ---------------------------------------------------------------------------

-- | prior 部のみ (observe 無し)。 gradADU は ObserveLM 無しゆえ全体 `ad`、
--   = vec 経路の priorGrad 部 (prior+jacobian) の単体コスト計測用 (54.4c 内訳)。
m2PriorOnly :: [[Double]] -> [Int] -> [Double] -> ModelP ()
m2PriorOnly xRows gids _ys = do
  let p  = if null xRows then 0 else length (head xRows)
      nG = if null gids then 0 else maximum gids + 1
  _   <- mapM (\k -> sample (T.pack ("beta_" ++ show k)) (Normal 0 5)) [0 .. p - 1]
  tau <- sample "tau_u" (HalfNormal 5)
  _   <- mapM (\j -> sample (T.pack ("u_" ++ show j)) (Normal 0 tau)) [0 .. nG - 1]
  _   <- sample "sigma" (Exponential 1)
  pure ()

-- | scalar 経路 (per-obs observe を手書き)。 全体が `ad` で微分される基準。
--   latent 宣言・順序は m2VecLM と完全一致 (beta_0,beta_1,tau_u,u_*,sigma)。
m2Scalar :: [[Double]] -> [Int] -> [Double] -> ModelP ()
m2Scalar xRows gids ys = do
  let p  = if null xRows then 0 else length (head xRows)
      nG = if null gids then 0 else maximum gids + 1
  betas <- mapM (\k -> sample (T.pack ("beta_" ++ show k)) (Normal 0 5)) [0 .. p - 1]
  tau   <- sample "tau_u" (HalfNormal 5)
  us    <- mapM (\j -> sample (T.pack ("u_" ++ show j)) (Normal 0 tau)) [0 .. nG - 1]
  s     <- sample "sigma" (Exponential 1)
  forM_ (zip3 [0 :: Int ..] (zip3 xRows gids ys) (repeat ())) $ \(i, (xr, g, y), _) ->
    let eta = sum (zipWith (\b x -> b * realToFrac x) betas xr) + us !! g
    in observe (T.pack ("y_" ++ show i)) (Normal eta s) [y]

-- | vec 経路 (observeLMR)。 固定効果 β は密設計行列、 群効果 u_j は gather
--   (REff) で疎に表現する。 prior 宣言は scalar 版と完全に同一 (同じ分布・同じ
--   順序) ゆえ logJoint/gradADU は一致する。
m2VecLM :: [[Double]] -> [Int] -> [Double] -> ModelP ()
m2VecLM xRows gids ys = do
  let p  = if null xRows then 0 else length (head xRows)
      nG = if null gids then 0 else maximum gids + 1
      betaNames = [ T.pack ("beta_" ++ show k) | k <- [0 .. p - 1] ]
      uNames    = [ T.pack ("u_" ++ show j)    | j <- [0 .. nG - 1] ]
  _   <- forM [0 .. p - 1] $ \k -> sample (betaNames !! k) (Normal 0 5)
  tau <- sample "tau_u" (HalfNormal 5)
  _   <- forM [0 .. nG - 1] $ \j -> sample (uNames !! j) (Normal 0 tau)
  _   <- sample "sigma" (Exponential 1)
  observeLMR "y" betaNames xRows [REff uNames gids Nothing] (LMGaussian "sigma") ys

-- | 54.4c 経路: m2VecLM と latent 宣言・観測は完全同一で、 REff に prior スケール
--   名 @Just "tau_u"@ を載せた版。 これにより compileGradU が u-prior 勾配を解析的
--   (O(nG) の素な Double) に計算し、 u_j Sample を ad walk から除外する。
--   m2VecLM (prior を ad) と数値は一致 (test で担保)・per-call で prior の O(nG) ad
--   が消えるぶん速くなるはず (計測で確認)。
m2VecLMAna :: [[Double]] -> [Int] -> [Double] -> ModelP ()
m2VecLMAna xRows gids ys = do
  let p  = if null xRows then 0 else length (head xRows)
      nG = if null gids then 0 else maximum gids + 1
      betaNames = [ T.pack ("beta_" ++ show k) | k <- [0 .. p - 1] ]
      uNames    = [ T.pack ("u_" ++ show j)    | j <- [0 .. nG - 1] ]
  _   <- forM [0 .. p - 1] $ \k -> sample (betaNames !! k) (Normal 0 5)
  tau <- sample "tau_u" (HalfNormal 5)
  _   <- forM [0 .. nG - 1] $ \j -> sample (uNames !! j) (Normal 0 tau)
  _   <- sample "sigma" (Exponential 1)
  observeLMR "y" betaNames xRows [REff uNames gids (Just "tau_u")] (LMGaussian "sigma") ys

-- ---------------------------------------------------------------------------
-- 計測補助
-- ---------------------------------------------------------------------------

-- | 真値近傍の unconstrained 初期点 (β/u は identity、 tau_u/sigma は log)。
initU :: [T.Text] -> [Double]
initU names =
  [ case n of
      "tau_u" -> log 1.5
      "sigma" -> log 1.0
      _       -> 0.1
  | n <- names ]

centralDiff :: ([Double] -> Double) -> [Double] -> [Double]
centralDiff f ps =
  [ let h = 1e-6 * (abs (ps !! j) + 1e-3)
    in (f (bump j h) - f (bump j (-h))) / (2 * h)
  | j <- [0 .. length ps - 1] ]
  where bump j d = [ if k == j then p + d else p | (k, p) <- zip [0 ..] ps ]

relErr :: [Double] -> [Double] -> Double
relErr a b = maximum [ abs (x - y) / (abs y + 1e-6) | (x, y) <- zip a b ]

-- | gradADU の per-call median 時間 (ms)。 静的部分を毎回再構築 (54.4a 経路)。
--   index で入力を微小摂動し CSE を防ぐ。
timeGrad :: ModelP () -> [T.Text] -> [Transform] -> [Double] -> IO Double
timeGrad m names trans us = do
  (ms, _) <- timeitIO 50 (sum . map abs)
               (\i -> let us' = [ u + fromIntegral i * 1e-12 | u <- us ]
                      in pure (gradADU m names trans us'))
  pure ms

-- | compileGradU で静的部分を **1 度だけ**前処理しクロージャを 50 回再利用した
--   per-call median 時間 (ms) (54.4b 経路・NUTS と同じ使い方)。
timeGradCompiled :: ModelP () -> [T.Text] -> [Transform] -> [Double] -> IO Double
timeGradCompiled m names trans us = do
  let cl = compileGradU m names trans     -- 静的前処理は 1 度だけ
  (ms, _) <- timeitIO 50 (sum . map abs)
               (\i -> let us' = [ u + fromIntegral i * 1e-12 | u <- us ]
                      in pure (cl us'))
  pure ms

-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "=== Phase 54.4a per-call 勾配ベンチ (scalar=全ad vs vecLM=ハイブリッド) ===\n"
  putStrLn "対象: 階層 Gaussian (M2 random intercept)。 obs/群=12。"
  putStrLn "gradADU 1 回の median 時間 (ms・50 reps)。 sc=scalar(全ad)・vl=vecLM(vec-tape)。\n"
  putStrLn "vlc=vecLM compiled(54.4b・prior ad)・vla=同 compiled(54.4c・prior 解析)・vlc/vla=54.4c 短縮率。"
  printf "%4s %4s %5s | %9s %9s | %8s\n"
    ("nG"::String) ("p"::String) ("n"::String)
    ("vlc(ms)"::String) ("vla(ms)"::String) ("vlc/vla"::String)
  forM_ [2, 4, 8, 16, 32] $ \nG -> do
    (xRows, gids, ys) <- genM2 nG 12
    -- ModelP は rank-N 多相エイリアスゆえ let 束縛せず各 rank-N 消費箇所へ直接渡す。
    let names = sampleNames (m2VecLM xRows gids ys)
        tmap  = getTransforms (m2VecLM xRows gids ys)
        trans = [ tmap Map.! n | n <- names ]
        us    = initU names
        p     = length (head xRows)
        n     = length ys
        -- 正しさ: 解析 prior 経路 (54.4c) が ad 経路・中心差分と一致 (relErr)。
        gSc = gradADU (m2Scalar    xRows gids ys) names trans us
        gVa = gradADU (m2VecLMAna  xRows gids ys) names trans us
        cd  = centralDiff (\vs -> logJointUnconstrained (m2VecLM xRows gids ys) names trans
                                    (Map.fromList (zip names vs))) us
        e   = max (relErr gVa cd) (relErr gVa gSc)
    printf "  (relErr 54.4c vs ad/中心差分 nG=%d: %.2e)\n" nG e
    tVlc <- timeGradCompiled (m2VecLM    xRows gids ys) names trans us
    tVla <- timeGradCompiled (m2VecLMAna xRows gids ys) names trans us
    printf "%4d %4d %5d | %9.4f %9.4f | %8s\n"
      nG p n tVlc tVla
      (printf "x%.2f" (tVlc / tVla) :: String)

  -- per-draw NUTS wall-time (per-call とは別。 NUTS 統合後の実速度)。
  putStrLn "\n=== per-draw NUTS wall-time (warmup 300 + 300 draws・3 reps median) ==="
  putStrLn "sc=scalar(全ad)・vl=vecLM(54.4b prior ad)・vla=vecLM(54.4c prior 解析)。"
  printf "%4s %5s | %11s %11s %11s | %8s %8s\n"
    ("nG"::String) ("n"::String)
    ("sc(ms/dr)"::String) ("vl(ms/dr)"::String) ("vla(ms/dr)"::String)
    ("sc/vla"::String) ("vl/vla"::String)
  forM_ [8, 32] $ \nG -> do
    (xRows, gids, ys) <- genM2 nG 12
    let n     = length ys
        nGn   = nG
        initP = Map.fromList $
          [ ("beta_0", 1.0), ("beta_1", 0.8), ("tau_u", 1.5), ("sigma", 1.0) ]
          ++ [ (T.pack ("u_" ++ show j), 0.0) | j <- [0 .. nGn - 1] ]
        cfg = defaultNUTSConfig
          { nutsIterations = 300, nutsBurnIn = 300, nutsStepSize = 0.1
          , nutsMaxDepth = 10, nutsAdaptStepSize = True
          , nutsTargetAccept = 0.8, nutsAdaptMass = True }
        runWith :: ModelP () -> Int -> IO Chain
        runWith mdl i = do
          g <- MWC.initialize (V.singleton (fromIntegral (42 + i)))
          nuts mdl cfg initP g
        probe ch = fromIntegral (chainTotal ch)
    (msSc, _)  <- timeitIO 3 probe (runWith (m2Scalar    xRows gids ys))
    (msVl, _)  <- timeitIO 3 probe (runWith (m2VecLM     xRows gids ys))
    (msVla, _) <- timeitIO 3 probe (runWith (m2VecLMAna  xRows gids ys))
    -- 総 wall-time を draw 数 (300) で割って per-draw に正規化。
    let perDraw t = t / 300.0
    printf "%4d %5d | %11.4f %11.4f %11.4f | %8s %8s\n"
      nG n (perDraw msSc) (perDraw msVl) (perDraw msVla)
      (printf "x%.1f" (msSc / msVla) :: String)
      (printf "x%.2f" (msVl / msVla) :: String)
