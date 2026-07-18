{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Hanalyze.Model.PLS
-- Description : PLS (Partial Least Squares) — 応答 Y との共分散を最大化する低ランク回帰 (NIPALS)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Partial Least Squares (PLS) — chemometrics 標準の応答連動低ランク回帰。
--
-- PCA (`Hanalyze.Model.PCA`) は応答無視の分散最大化、 PLS は **応答 Y と
-- X の共分散を最大化**する低ランク射影。 多変量分光分析 / 材料設計の予測 +
-- 変数選択を 1 モデルで実現する。
--
-- アルゴリズム:
--
--   * 'NIPALS' (default): 反復的 power iteration、 sklearn `PLSRegression` と
--     数値一致しやすい
--   * 'SIMPLS' (Phase 9.5 で追加予定): de Jong 1993、 SVD ベース、 multi-Y で
--     直接的
--
-- 内部実装は hmatrix Matrix / Vector 演算で完結 (list 化しない)。
module Hanalyze.Model.PLS
  ( -- * Config
    PLSAlgorithm (..)
  , PLSConfig (..)
  , defaultPLS
    -- * Fit / predict
  , PLSFit (..)
  , fitPLS
  , fitPLS1
  , predictPLS
  , predictPLS1
    -- * CV による component 数選択
  , PLSLambdaSelection (..)
  , selectPLSComponentsCV
  ) where

import qualified Numeric.LinearAlgebra as LA
import qualified System.Random.MWC     as MWC
import           Data.List             (sortBy)
import           Data.Ord              (comparing)
import           Data.Text             (Text)
import qualified Data.Text             as T

import qualified Hanalyze.Stat.CV      as HCV

-- ===========================================================================
-- Config
-- ===========================================================================

data PLSAlgorithm
  = NIPALS   -- ^ 反復的 power iteration (default)
  | SIMPLS   -- ^ de Jong 1993 (Phase 9.5 で追加)
  deriving (Show, Eq)

data PLSConfig = PLSConfig
  { plsN_Components :: !Int
  , plsAlgorithm    :: !PLSAlgorithm
  , plsScale        :: !Bool           -- ^ True で X, Y を column-wise に標準化
  , plsTol          :: !Double         -- ^ NIPALS 収束許容誤差
  , plsMaxIter      :: !Int            -- ^ NIPALS 最大反復
  } deriving (Show)

defaultPLS :: PLSConfig
defaultPLS = PLSConfig
  { plsN_Components = 2
  , plsAlgorithm    = NIPALS
  , plsScale        = True
  , plsTol          = 1e-8
  , plsMaxIter      = 500
  }

-- ===========================================================================
-- 結果型
-- ===========================================================================

data PLSFit = PLSFit
  { plsScoresT    :: !(LA.Matrix Double)  -- ^ T (n × K) X scores
  , plsLoadingsP  :: !(LA.Matrix Double)  -- ^ P (p × K) X loadings
  , plsLoadingsQ  :: !(LA.Matrix Double)  -- ^ Q (q × K) Y loadings
  , plsWeightsW   :: !(LA.Matrix Double)  -- ^ W (p × K) X weights
  , plsCoef       :: !(LA.Matrix Double)
    -- ^ β (p × q) 回帰係数 (元スケール)。 @Ŷ = (X - X̄) · β + Ȳ@
  , plsXMean      :: !(LA.Vector Double)  -- ^ X 列平均
  , plsXStd       :: !(LA.Vector Double)  -- ^ X 列標準偏差 (plsScale=True なら、 そうでなければ 1)
  , plsYMean      :: !(LA.Vector Double)
  , plsYStd       :: !(LA.Vector Double)
  , plsR2X        :: !(LA.Vector Double)  -- ^ 各 component の X 説明分散率
  , plsR2Y        :: !(LA.Vector Double)  -- ^ 各 component の Y 説明分散率
  , plsVIP        :: !(LA.Vector Double)  -- ^ 変数重要度 (Variable Importance in Projection)
  , plsConfig     :: !PLSConfig
  } deriving (Show)

-- ===========================================================================
-- 公開関数
-- ===========================================================================

-- | PLS fit (multi-output Y、 q ≥ 1)。
fitPLS :: PLSConfig
       -> LA.Matrix Double      -- ^ X (n × p)
       -> LA.Matrix Double      -- ^ Y (n × q)
       -> Either Text PLSFit
fitPLS cfg x y
  | LA.rows x /= LA.rows y =
      Left "fitPLS: X and Y must have the same number of rows"
  | LA.rows x < 2 =
      Left "fitPLS: need at least 2 observations"
  | plsN_Components cfg < 1 =
      Left "fitPLS: n_components must be ≥ 1"
  | plsN_Components cfg > min (LA.rows x - 1) (LA.cols x) =
      Left (T.pack ("fitPLS: n_components (" <> show (plsN_Components cfg) <>
                    ") exceeds min(n-1, p)"))
  | otherwise =
      case plsAlgorithm cfg of
        NIPALS -> Right (nipalsFit cfg x y)
        SIMPLS -> Left "fitPLS: SIMPLS not yet implemented (Phase 9.5)"

-- | 単出力 Y ショートカット (q = 1)。
fitPLS1 :: PLSConfig -> LA.Matrix Double -> LA.Vector Double -> Either Text PLSFit
fitPLS1 cfg x y = fitPLS cfg x (LA.asColumn y)

-- | 予測 (multi-output)。 `plsCoef` は元スケールの回帰係数なので、
--   X を中央化するだけで予測可能 (= scaling は不要、 coef が吸収済)。
predictPLS :: PLSFit -> LA.Matrix Double -> LA.Matrix Double
predictPLS fit xNew =
  let nRow = LA.rows xNew
      xCentered = xNew - LA.fromRows (replicate nRow (plsXMean fit))
      yCentered = xCentered LA.<> plsCoef fit
  in yCentered + LA.fromRows (replicate nRow (plsYMean fit))

predictPLS1 :: PLSFit -> LA.Matrix Double -> LA.Vector Double
predictPLS1 fit xNew = LA.flatten (predictPLS fit xNew)

-- ===========================================================================
-- NIPALS 実装
-- ===========================================================================

-- | NIPALS 内部実装。 中央化 + (option で) 標準化 → component loop → 後処理。
nipalsFit :: PLSConfig -> LA.Matrix Double -> LA.Matrix Double -> PLSFit
nipalsFit cfg xRaw yRaw =
  let !n = LA.rows xRaw
      !p = LA.cols xRaw
      !q = LA.cols yRaw
      k  = plsN_Components cfg

      -- 列平均
      xMean = LA.scale (1 / fromIntegral n) (LA.fromList
                [ LA.sumElements (xRaw LA.¿ [j]) | j <- [0 .. p - 1] ])
      yMean = LA.scale (1 / fromIntegral n) (LA.fromList
                [ LA.sumElements (yRaw LA.¿ [j]) | j <- [0 .. q - 1] ])

      xCentered = xRaw - LA.fromRows (replicate n xMean)
      yCentered = yRaw - LA.fromRows (replicate n yMean)

      -- 列標準偏差 (n-1 分母、 plsScale=False なら 1 ベクトル)
      -- Bug fix (Phase 17.1): 旧実装 LA.sumElements (c LA.<> LA.tr c) は
      -- n×n 行列 c_i c_j を生成 → sumElements で (Σ c)² になっていた。
      -- 正しくは Σ c_i² = c `LA.dot` c。
      colSD m mean_
        | LA.rows m < 2 = LA.fromList (replicate (LA.cols m) 1)
        | otherwise =
            let nm = fromIntegral (LA.rows m - 1) :: Double
                centered = m - LA.fromRows (replicate (LA.rows m) mean_)
                sqSum = LA.fromList
                  [ let c = LA.flatten (centered LA.¿ [j])
                    in c `LA.dot` c
                  | j <- [0 .. LA.cols m - 1] ]
            in LA.cmap (\v -> let s = sqrt (v / nm) in if s > 1e-12 then s else 1) sqSum

      xStd = if plsScale cfg then colSD xRaw xMean else LA.fromList (replicate p 1)
      yStd = if plsScale cfg then colSD yRaw yMean else LA.fromList (replicate q 1)

      xScaled = if plsScale cfg
                  then xCentered / LA.fromRows (replicate n xStd)
                  else xCentered
      yScaled = if plsScale cfg
                  then yCentered / LA.fromRows (replicate n yStd)
                  else yCentered

      -- component loop: 各 component で deflate しながら w, t, p, q を取り出す
      (wMat, tMat, pMat, qMat) = nipalsLoop cfg k xScaled yScaled

      -- 回帰係数 β = W (Pᵀ W)⁻¹ Qᵀ  (centered/scaled 空間)
      ptw = LA.tr pMat LA.<> wMat       -- K × K
      ptwInv = case LA.linearSolve ptw (LA.ident k) of
        Just inv -> inv
        Nothing  -> LA.scale 0 (LA.ident k)  -- singular なら 0
      betaScaled = wMat LA.<> ptwInv LA.<> LA.tr qMat  -- p × q

      -- R²X, R²Y を component 別に計算
      ssTotalX = LA.sumElements (xScaled * xScaled)
      ssTotalY = LA.sumElements (yScaled * yScaled)
      r2X = LA.fromList
        [ let tk = tMat LA.¿ [j]
              pk = pMat LA.¿ [j]
              recon = tk LA.<> LA.tr pk
              ss = LA.sumElements (recon * recon)
          in if ssTotalX > 0 then ss / ssTotalX else 0
        | j <- [0 .. k - 1] ]
      r2Y = LA.fromList
        [ let tk = tMat LA.¿ [j]
              qk = qMat LA.¿ [j]
              recon = tk LA.<> LA.tr qk
              ss = LA.sumElements (recon * recon)
          in if ssTotalY > 0 then ss / ssTotalY else 0
        | j <- [0 .. k - 1] ]

      -- VIP: VIP_j = sqrt( p · Σ_k (W²_jk · SS_Y_k) / Σ_k SS_Y_k )
      ssYPerComp = LA.fromList
        [ let tk = tMat LA.¿ [j]
              qk = qMat LA.¿ [j]
              recon = tk LA.<> LA.tr qk
          in LA.sumElements (recon * recon)
        | j <- [0 .. k - 1] ]
      ssYTotal = LA.sumElements ssYPerComp
      vip = if ssYTotal > 0
              then LA.fromList
                [ let wj = LA.flatten (LA.tr wMat LA.¿ [j])  -- length K
                      contribs = (wj * wj) * ssYPerComp
                      total = LA.sumElements contribs
                  in sqrt (fromIntegral p * total / ssYTotal)
                | j <- [0 .. p - 1] ]
              else LA.fromList (replicate p 0)

      -- 元スケールの coef
      coefOrig =
        if plsScale cfg
          then let xStdInv = LA.cmap (1 /) xStd
                   yStdDiag = LA.diag yStd
                   xStdDiagInv = LA.diag xStdInv
               in xStdDiagInv LA.<> betaScaled LA.<> yStdDiag
          else betaScaled

  in PLSFit
       { plsScoresT    = tMat
       , plsLoadingsP  = pMat
       , plsLoadingsQ  = qMat
       , plsWeightsW   = wMat
       , plsCoef       = coefOrig
       , plsXMean      = xMean
       , plsXStd       = xStd
       , plsYMean      = yMean
       , plsYStd       = yStd
       , plsR2X        = r2X
       , plsR2Y        = r2Y
       , plsVIP        = vip
       , plsConfig     = cfg
       }

-- | NIPALS 反復ループ: scaled X, Y から K components を抽出。
nipalsLoop
  :: PLSConfig
  -> Int                        -- K
  -> LA.Matrix Double           -- X_scaled (n × p)
  -> LA.Matrix Double           -- Y_scaled (n × q)
  -> ( LA.Matrix Double  -- W (p × K)
     , LA.Matrix Double  -- T (n × K)
     , LA.Matrix Double  -- P (p × K)
     , LA.Matrix Double  -- Q (q × K)
     )
nipalsLoop cfg k x0 y0 = go 0 x0 y0 [] [] [] []
  where
    go !i !x !y wAcc tAcc pAcc qAcc
      | i >= k =
          ( LA.fromColumns (reverse wAcc)
          , LA.fromColumns (reverse tAcc)
          , LA.fromColumns (reverse pAcc)
          , LA.fromColumns (reverse qAcc)
          )
      | otherwise =
          let (w, t, ploading, qloading) = nipalsOneComponent cfg x y
              -- Deflate: E = E - t pᵀ, F = F - t qᵀ
              x' = x - LA.asColumn t LA.<> LA.asRow ploading
              y' = y - LA.asColumn t LA.<> LA.asRow qloading
          in go (i + 1) x' y' (w : wAcc) (t : tAcc) (ploading : pAcc) (qloading : qAcc)

-- | NIPALS の 1 component 抽出。 power iteration で w, t, p, q を得る。
nipalsOneComponent
  :: PLSConfig
  -> LA.Matrix Double
  -> LA.Matrix Double
  -> ( LA.Vector Double   -- w (p)
     , LA.Vector Double   -- t (n)
     , LA.Vector Double   -- p loading (p)
     , LA.Vector Double   -- q loading (q)
     )
nipalsOneComponent cfg x y =
  let -- 初期 u: Y の最初の列
      u0 = LA.flatten (y LA.¿ [0])
      (uFinal, _iter) = iterate' cfg x y u0 0
      -- 最終 w 計算 (deflate 前の x, y で)
      xtu = LA.tr x LA.#> uFinal
      normXtu = sqrt (LA.sumElements (xtu * xtu))
      w = if normXtu > 1e-12 then LA.scale (1 / normXtu) xtu
                              else xtu
      t = x LA.#> w
      tt = LA.sumElements (t * t)
      qy = if tt > 1e-12 then LA.scale (1 / tt) (LA.tr y LA.#> t)
                         else LA.tr y LA.#> t
      pload = if tt > 1e-12 then LA.scale (1 / tt) (LA.tr x LA.#> t)
                            else LA.tr x LA.#> t
  in (w, t, pload, qy)

-- | NIPALS の収束反復。 u を更新し続け、 |u_new - u| < tol で終了。
iterate'
  :: PLSConfig
  -> LA.Matrix Double
  -> LA.Matrix Double
  -> LA.Vector Double   -- u
  -> Int                -- iter count
  -> (LA.Vector Double, Int)
iterate' cfg x y u !i
  | i >= plsMaxIter cfg = (u, i)
  | otherwise =
      let xtu = LA.tr x LA.#> u
          normXtu = sqrt (LA.sumElements (xtu * xtu))
          w = if normXtu > 1e-12 then LA.scale (1 / normXtu) xtu else xtu
          t = x LA.#> w
          tt = LA.sumElements (t * t)
          ytt = LA.tr y LA.#> t
          q = if tt > 1e-12 then LA.scale (1 / tt) ytt else ytt
          fq = y LA.#> q
          normFq = sqrt (LA.sumElements (fq * fq))
          uNew = if normFq > 1e-12 then LA.scale (1 / normFq) fq else fq
          diff = uNew - u
          err = sqrt (LA.sumElements (diff * diff))
      in if err < plsTol cfg
           then (uNew, i + 1)
           else iterate' cfg x y uNew (i + 1)

-- ===========================================================================
-- CV による component 数選択
-- ===========================================================================

data PLSLambdaSelection = PLSLambdaSelection
  { plsBestK   :: !Int
  , plsCVMSEs  :: ![Double]
  , plsCVSDs   :: ![Double]
  , plsOneSeK  :: !Int
  } deriving (Show)

-- | k-fold CV で component 数を 1..maxK の中から選ぶ。
selectPLSComponentsCV
  :: Int                       -- ^ k-fold の k
  -> Int                       -- ^ maxK (component 数上限)
  -> LA.Matrix Double          -- ^ X
  -> LA.Matrix Double          -- ^ Y
  -> MWC.GenIO
  -> IO PLSLambdaSelection
selectPLSComponentsCV kFold maxK xMat yMat gen = do
  let n = LA.rows xMat
  folds <- HCV.kFold kFold n gen
  let perK kk =
        let cfg = defaultPLS { plsN_Components = kk }
            scores =
              [ mseForFold cfg xMat yMat trainIdx testIdx
              | (trainIdx, testIdx) <- folds, not (null testIdx)
              ]
            !nFolds = fromIntegral (length scores) :: Double
            meanMSE = sum scores / nFolds
            varN    = sum [(s - meanMSE) ** 2 | s <- scores] / max 1 (nFolds - 1)
            !se     = sqrt (varN / nFolds)
        in (meanMSE, se)
      ks = [1 .. maxK]
      stats = map perK ks
      mses  = map fst stats
      ses   = map snd stats
      indexedMSEs = zip3 ks mses ses
      sortedAsc   = sortBy (comparing (\(_, m, _) -> m)) indexedMSEs
      (bestK_, bestMSE, bestSE) =
        case sortedAsc of
          (h:_) -> h
          []    -> (1, 0, 0)
      threshold = bestMSE + bestSE
      -- 1-SE rule: 最も sparse な K (= 最小 K) で best MSE + 1·SE 以内
      oneSe = case [k | (k, m, _) <- indexedMSEs, m <= threshold] of
                [] -> bestK_
                xs -> minimum xs
  pure PLSLambdaSelection
    { plsBestK   = bestK_
    , plsCVMSEs  = mses
    , plsCVSDs   = ses
    , plsOneSeK  = oneSe
    }

mseForFold
  :: PLSConfig
  -> LA.Matrix Double
  -> LA.Matrix Double
  -> [Int]
  -> [Int]
  -> Double
mseForFold cfg xMat yMat trainIdx testIdx =
  let xTr = xMat LA.? trainIdx
      yTr = yMat LA.? trainIdx
      xTe = xMat LA.? testIdx
      yTe = yMat LA.? testIdx
  in case fitPLS cfg xTr yTr of
       Left _    -> 1/0
       Right fit ->
         let yHat = predictPLS fit xTe
             resid = yTe - yHat
             nTe = fromIntegral (length testIdx) :: Double
         in LA.sumElements (resid * resid) / nTe
