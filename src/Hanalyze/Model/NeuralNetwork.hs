{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Hanalyze.Model.NeuralNetwork
-- Description : Multi-Layer Perceptron (MLP) — feedforward neural network (mini-batch SGD + Adam)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Multi-Layer Perceptron (MLP) — feedforward neural network。
--
-- Mini-batch SGD + 自前 Adam で学習。 hmatrix Matrix/Vector で全演算。
--
-- 対応:
--
--   * 'fitMLPRegressor': 出力 1 次元の回帰 (MSE loss)
--   * 'fitMLPClassifier': 多クラス分類 (cross-entropy + softmax 出力)
--   * 'predictMLP': forward 推論
--
-- 隠れ層の活性化は ReLU 既定、 出力層は task に応じて自動 (回帰=Identity、
-- 分類=Softmax)。
module Hanalyze.Model.NeuralNetwork
  ( Activation (..)
  , MLPConfig (..)
  , defaultMLP
  , Layer (..)
  , MLPFit (..)
  , MLPEpochEvent (..)
  , fitMLPRegressor
  , fitMLPRegressorWithCallback
  , fitMLPRegressorPure
  , fitMLPClassifier
  , fitMLPClassifierWithCallback
  , fitMLPClassifierPure
  , predictMLP
  , predictMLPClass
  ) where

import qualified Data.Vector             as V
import qualified Data.Vector.Unboxed     as VU
import           Data.Text               (Text)
import qualified Numeric.LinearAlgebra   as LA
import           Control.Monad           (forM_)
import           Control.Monad.Primitive (PrimMonad, PrimState)
import           Control.Monad.ST        (runST)
import           Data.Primitive.MutVar   (newMutVar, readMutVar, writeMutVar,
                                          modifyMutVar')
import           Data.Word               (Word32)
import qualified System.Random.MWC       as MWC
import           System.Random.MWC       (initialize)
import           System.Random.MWC.Distributions (standard)

-- ===========================================================================
-- 型
-- ===========================================================================

data Activation = ReLU | Sigmoid | Tanh | Identity | Softmax
  deriving (Show, Eq)

data Layer = Layer
  { lyrW :: !(LA.Matrix Double)   -- (in × out)
  , lyrB :: !(LA.Vector Double)   -- (out)
  , lyrAct :: !Activation
  } deriving (Show)

data MLPConfig = MLPConfig
  { mlpHidden    :: ![Int]
  , mlpActHidden :: !Activation
  , mlpLR        :: !Double
  , mlpEpochs    :: !Int
  , mlpBatch     :: !Int
  , mlpL2        :: !Double
  , mlpStandardize :: !Bool
    -- ^ True で X を z-score 標準化してから学習 (predict 時は同じ
    --   mean/std で逆変換)。 Phase 17.3 で追加、 default True。
  } deriving (Show)

defaultMLP :: MLPConfig
defaultMLP = MLPConfig
  { mlpHidden    = [16]
  , mlpActHidden = ReLU
  , mlpLR        = 0.01
  , mlpEpochs    = 200
  , mlpBatch     = 16
  , mlpL2        = 0
  , mlpStandardize = True
  }

data MLPFit = MLPFit
  { mlpLayers   :: ![Layer]
  , mlpLossHist :: ![Double]
  , mlpClasses  :: ![Int]
    -- ^ 分類器の場合の class label 順 (sorted)。 回帰時は空。
  , mlpClassNames :: ![Text]
    -- ^ クラス名 (df|-> が levels 注入・空=数値表示/回帰時は空)。
  , mlpXMean    :: !(LA.Vector Double)
    -- ^ X 標準化に使った列平均 (Phase 17.3、 標準化 off なら length 0)
  , mlpXStd     :: !(LA.Vector Double)
  , mlpYMean    :: !Double
    -- ^ regressor の場合の y 平均 (標準化 off なら 0)
  , mlpYStd     :: !Double
  } deriving (Show)

-- ===========================================================================
-- 活性化
-- ===========================================================================

applyAct :: Activation -> LA.Matrix Double -> LA.Matrix Double
applyAct ReLU     = LA.cmap (\v -> max 0 v)
applyAct Sigmoid  = LA.cmap (\v -> 1 / (1 + exp (-v)))
applyAct Tanh     = LA.cmap tanh
applyAct Identity = id
applyAct Softmax  = softmaxRows

actGrad :: Activation -> LA.Matrix Double -> LA.Matrix Double -> LA.Matrix Double
actGrad ReLU     pre _   = LA.cmap (\v -> if v > 0 then 1 else 0) pre
actGrad Sigmoid  _   out = out * (1 - out)
actGrad Tanh     _   out = 1 - out * out
actGrad Identity _   _   = LA.fromLists [[1 :: Double]]
actGrad Softmax  _   _   = LA.fromLists [[1 :: Double]]

softmaxRows :: LA.Matrix Double -> LA.Matrix Double
softmaxRows m = LA.fromRows
  [ let r = LA.flatten (m LA.? [i])
        mx = LA.maxElement r
        ex = LA.cmap (\v -> exp (v - mx)) r
        s  = LA.sumElements ex
    in LA.scale (1 / s) ex
  | i <- [0 .. LA.rows m - 1] ]

-- ===========================================================================
-- 初期化
-- ===========================================================================

initLayers :: PrimMonad m
           => MWC.Gen (PrimState m) -> Int -> Int -> [Int] -> Activation -> Activation -> m [Layer]
initLayers gen inDim outDim hidden hidAct outAct = do
  let sizes = inDim : hidden ++ [outDim]
      pairs = zip sizes (tail sizes)
      acts  = replicate (length hidden) hidAct ++ [outAct]
  mapM (\((nin, nout), act) -> do
          let scale = sqrt (2 / fromIntegral nin)
          ws <- mapM (\_ -> standard gen) [1 .. nin * nout]
          let w = LA.scale scale
                    (LA.fromLists (chunksOf nout ws))
              b = LA.fromList (replicate nout 0)
          pure (Layer w b act))
       (zip pairs acts)
  where
    chunksOf _ [] = []
    chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- ===========================================================================
-- Forward pass
-- ===========================================================================

forward :: [Layer] -> LA.Matrix Double -> [(LA.Matrix Double, LA.Matrix Double)]
forward layers x = go x layers []
  where
    go _    []     acc = reverse acc
    go inp (l:ls) acc =
      let pre = addBias (inp LA.<> lyrW l) (lyrB l)
          out = applyAct (lyrAct l) pre
      in go out ls ((pre, out) : acc)

-- | Add bias vector (length = out) to every row of the (n × out) matrix.
addBias :: LA.Matrix Double -> LA.Vector Double -> LA.Matrix Double
addBias m b = m + LA.fromRows (replicate (LA.rows m) b)

-- ===========================================================================
-- Backprop (回帰 MSE)
-- ===========================================================================

-- | Backprop with MSE for regression OR cross-entropy with softmax for
--   classification. Output gradient at last layer differs by task:
--     reg:   dL/dz_out = (yhat - y) / n   (with Identity output)
--     class: dL/dz_out = (yhat - yOH) / n (softmax + CE simplification)
backprop
  :: [Layer]
  -> LA.Matrix Double                       -- x (n × in)
  -> LA.Matrix Double                       -- y (n × out) target
  -> Bool                                   -- True = classification (softmax+CE)
  -> Double                                 -- L2 weight
  -> [(LA.Matrix Double, LA.Vector Double)] -- gradients (dW, dB) per layer
backprop layers x y isClass l2 =
  let cache = forward layers x   -- list of (pre, out) per layer
      n     = fromIntegral (LA.rows x) :: Double
      out_  = snd (last cache)
      dPre_last
        | isClass   = LA.scale (1/n) (out_ - y)
        | otherwise = LA.scale (1/n) (out_ - y)   -- Identity output, same shape
      -- walk backward
      walk !dPre [] _ acc = acc
      walk !dPre (l:ls) (c:cs) acc =
        let -- input to layer l = (previous out) or x if first
            inpToL = case cs of
                       []      -> x
                       (cPrev:_) -> snd cPrev
            (preL, _) = c
            dW = LA.tr inpToL LA.<> dPre + LA.scale l2 (lyrW l)
            dB = LA.fromList [ LA.sumElements (dPre LA.¿ [j])
                             | j <- [0 .. LA.cols dPre - 1] ]
            -- propagate to previous layer
            dOutPrev = dPre LA.<> LA.tr (lyrW l)
            dPrePrev =
              case ls of
                []      -> dOutPrev  -- unused
                (lPrev:_) ->
                  let (prePrev, outPrev) = head cs
                      g = actGrad (lyrAct lPrev) prePrev outPrev
                  in dOutPrev * g
        in walk dPrePrev ls cs ((dW, dB) : acc)
      grads = walk dPre_last (reverse layers) (reverse cache) []
  in grads

-- ===========================================================================
-- 学習ループ (Adam)
-- ===========================================================================

-- | Per-epoch event emitted by 'fitMLPRegressorWithCallback' /
-- 'fitMLPClassifierWithCallback'。 Phase 21 で追加。
data MLPEpochEvent = MLPEpochEvent
  { meEpoch     :: !Int
    -- ^ 0-based epoch index (0..epochs-1)
  , meTrainLoss :: !Double
    -- ^ epoch 終端での full-batch training loss
  , meValLoss   :: !(Maybe Double)
    -- ^ validation split loss。 v1 では常に 'Nothing' (= reserved for future)
  , meCurrentLR :: !Double
    -- ^ そのときの学習率 (現在は constant scheduler のみ、 将来 LR scheduler
    --   実装で意味が出る)
  } deriving (Show)

trainMLP
  :: PrimMonad m
  => MWC.Gen (PrimState m) -> MLPConfig
  -> LA.Matrix Double -> LA.Matrix Double
  -> Bool         -- isClass
  -> (MLPEpochEvent -> m ())   -- per-epoch callback (no-op で旧挙動)
  -> m ([Layer], [Double])
trainMLP gen cfg x y isClass onEpoch = do
  let inDim   = LA.cols x
      outDim  = LA.cols y
      outAct  = if isClass then Softmax else Identity
  layers0 <- initLayers gen inDim outDim (mlpHidden cfg) (mlpActHidden cfg) outAct
  -- Adam state per layer (mW, vW, mB, vB)
  let zeroLike w = LA.scale 0 w
      zeroLikeV v = LA.scale 0 v
  state <- mapM (\l -> do
                    mw <- newMutVar (zeroLike (lyrW l))
                    vw <- newMutVar (zeroLike (lyrW l))
                    mb <- newMutVar (zeroLikeV (lyrB l))
                    vb <- newMutVar (zeroLikeV (lyrB l))
                    pure (mw, vw, mb, vb)) layers0
  layersRef <- newMutVar layers0
  lossRef   <- newMutVar ([] :: [Double])
  let n  = LA.rows x
      lr = mlpLR cfg
      b1 = 0.9
      b2 = 0.999
      eps = 1e-8
  tRef <- newMutVar (0 :: Int)
  forM_ [0 .. mlpEpochs cfg - 1] $ \epochIdx -> do
    -- shuffle indices
    idx <- fisherYates gen [0 .. n - 1]
    let batches = chunksOf (mlpBatch cfg) idx
    forM_ batches $ \batch -> do
      let xb = x LA.? batch
          yb = y LA.? batch
      ls0 <- readMutVar layersRef
      let grads = backprop ls0 xb yb isClass (mlpL2 cfg)
      modifyMutVar' tRef (+1)
      t <- readMutVar tRef
      let tD = fromIntegral t :: Double
          c1 = 1 - b1 ** tD
          c2 = 1 - b2 ** tD
      newLayers <-
        mapM (\(l, (dW, dB), (mwR, vwR, mbR, vbR)) -> do
                mw <- readMutVar mwR
                vw <- readMutVar vwR
                mb <- readMutVar mbR
                vb <- readMutVar vbR
                let mw' = LA.scale b1 mw + LA.scale (1 - b1) dW
                    vw' = LA.scale b2 vw + LA.scale (1 - b2) (dW * dW)
                    mb' = LA.scale b1 mb + LA.scale (1 - b1) dB
                    vb' = LA.scale b2 vb + LA.scale (1 - b2) (dB * dB)
                    mwHat = LA.scale (1 / c1) mw'
                    vwHat = LA.scale (1 / c2) vw'
                    mbHat = LA.scale (1 / c1) mb'
                    vbHat = LA.scale (1 / c2) vb'
                    wNew = lyrW l - LA.scale lr
                             (mwHat / LA.cmap (\v -> sqrt v + eps) vwHat)
                    bNew = lyrB l - LA.scale lr
                             (mbHat / LA.cmap (\v -> sqrt v + eps) vbHat)
                writeMutVar mwR mw'
                writeMutVar vwR vw'
                writeMutVar mbR mb'
                writeMutVar vbR vb'
                pure l { lyrW = wNew, lyrB = bNew })
             (zip3 ls0 grads state)
      writeMutVar layersRef newLayers
    -- record epoch loss + per-epoch callback (Phase 21)
    lsFinal <- readMutVar layersRef
    let cache = forward lsFinal x
        out_ = snd (last cache)
        loss = if isClass
                 then crossEntropyLoss out_ y
                 else mseLoss out_ y
    modifyMutVar' lossRef (loss :)
    onEpoch MLPEpochEvent
      { meEpoch     = epochIdx
      , meTrainLoss = loss
      , meValLoss   = Nothing
      , meCurrentLR = lr
      }
  finalLayers <- readMutVar layersRef
  losses <- readMutVar lossRef
  pure (finalLayers, reverse losses)

mseLoss :: LA.Matrix Double -> LA.Matrix Double -> Double
mseLoss yhat y =
  let d = yhat - y
  in LA.sumElements (d * d) / fromIntegral (LA.rows y * LA.cols y)

crossEntropyLoss :: LA.Matrix Double -> LA.Matrix Double -> Double
crossEntropyLoss yhat y =
  let safe = LA.cmap (\v -> log (max 1e-15 v)) yhat
  in - LA.sumElements (y * safe) / fromIntegral (LA.rows y)

-- ===========================================================================
-- 公開 API
-- ===========================================================================

-- | X の列ごと平均と標準偏差 (n-1)。
standardizeStats :: LA.Matrix Double -> (LA.Vector Double, LA.Vector Double)
standardizeStats x =
  let n   = LA.rows x
      nD  = fromIntegral n :: Double
      mean_ = LA.fromList
        [ LA.sumElements (x LA.¿ [j]) / nD | j <- [0 .. LA.cols x - 1] ]
      std_ = if n < 2
               then LA.fromList (replicate (LA.cols x) 1)
               else LA.fromList
                      [ let c = LA.flatten (x LA.¿ [j]) - LA.scalar (mean_ `LA.atIndex` j)
                            v = (c `LA.dot` c) / (nD - 1)
                            s = sqrt v
                        in if s > 1e-12 then s else 1
                      | j <- [0 .. LA.cols x - 1] ]
  in (mean_, std_)

applyStandardize :: LA.Vector Double -> LA.Vector Double -> LA.Matrix Double
                 -> LA.Matrix Double
applyStandardize mean_ std_ x =
  let n   = LA.rows x
      mRow = LA.fromRows (replicate n mean_)
      sRow = LA.fromRows (replicate n std_)
  in (x - mRow) / sRow

fitMLPRegressor
  :: MLPConfig -> LA.Matrix Double -> LA.Vector Double
  -> MWC.GenIO -> IO MLPFit
fitMLPRegressor cfg x y gen =
  fitMLPRegressorWithCallback cfg x y gen (\_ -> pure ())

-- | Phase 21 で追加。 epoch 終端ごとに 'MLPEpochEvent' を渡す callback 付き
-- regressor 学習。 既存 'fitMLPRegressor' は no-op callback で本関数を呼ぶ
-- 薄い wrapper として保持される。
fitMLPRegressorWithCallback
  :: PrimMonad m
  => MLPConfig -> LA.Matrix Double -> LA.Vector Double
  -> MWC.Gen (PrimState m)
  -> (MLPEpochEvent -> m ())
  -> m MLPFit
fitMLPRegressorWithCallback cfg x y gen onEpoch = do
  let (xMean, xStd) = if mlpStandardize cfg
                        then standardizeStats x
                        else (LA.fromList [], LA.fromList [])
      xUse = if mlpStandardize cfg then applyStandardize xMean xStd x else x
      yMat = LA.asColumn y
  (layers, losses) <- trainMLP gen cfg xUse yMat False onEpoch
  pure MLPFit
    { mlpLayers   = layers
    , mlpLossHist = losses
    , mlpClasses  = []
    , mlpClassNames = []
    , mlpXMean    = xMean
    , mlpXStd     = xStd
    , mlpYMean    = 0
    , mlpYStd     = 1
    }

fitMLPClassifier
  :: MLPConfig -> LA.Matrix Double -> VU.Vector Int
  -> MWC.GenIO -> IO MLPFit
fitMLPClassifier cfg x y gen =
  fitMLPClassifierWithCallback cfg x y gen (\_ -> pure ())

-- | Phase 21 で追加。 'fitMLPRegressorWithCallback' の classifier 版。
fitMLPClassifierWithCallback
  :: PrimMonad m
  => MLPConfig -> LA.Matrix Double -> VU.Vector Int
  -> MWC.Gen (PrimState m)
  -> (MLPEpochEvent -> m ())
  -> m MLPFit
fitMLPClassifierWithCallback cfg x y gen onEpoch = do
  let classes = uniqueSort (VU.toList y)
      k       = length classes
      n       = VU.length y
      classIdx c = case lookup c (zip classes [0 ..]) of
        Just i  -> i
        Nothing -> 0
      yOH = LA.fromLists
              [ [ if j == classIdx (y VU.! i) then 1 else 0
                | j <- [0 .. k - 1] ]
              | i <- [0 .. n - 1] ]
      (xMean, xStd) = if mlpStandardize cfg
                        then standardizeStats x
                        else (LA.fromList [], LA.fromList [])
      xUse = if mlpStandardize cfg then applyStandardize xMean xStd x else x
  (layers, losses) <- trainMLP gen cfg xUse yOH True onEpoch
  pure MLPFit
    { mlpLayers   = layers
    , mlpLossHist = losses
    , mlpClasses  = classes
    , mlpClassNames = []
    , mlpXMean    = xMean
    , mlpXStd     = xStd
    , mlpYMean    = 0
    , mlpYStd     = 1
    }

-- | 'fitMLPRegressor' の純粋版 (Phase 75.8)。 Word32 seed から @runST@ + MWC で重み初期化・
-- shuffle を決定的に閉じる ('fitRFVPure'/'nutsPure' と同方針・同 seed → ビット同一)。
-- IO 版は進捗 callback 用に残る。
fitMLPRegressorPure :: MLPConfig -> LA.Matrix Double -> LA.Vector Double -> Word32 -> MLPFit
fitMLPRegressorPure cfg x y seed =
  runST (initialize (V.singleton seed)
           >>= \gen -> fitMLPRegressorWithCallback cfg x y gen (\_ -> pure ()))

-- | 'fitMLPClassifier' の純粋版 (Phase 75.8)。 seed から @runST@ で決定的に学習。
fitMLPClassifierPure :: MLPConfig -> LA.Matrix Double -> VU.Vector Int -> Word32 -> MLPFit
fitMLPClassifierPure cfg x y seed =
  runST (initialize (V.singleton seed)
           >>= \gen -> fitMLPClassifierWithCallback cfg x y gen (\_ -> pure ()))

predictMLP :: MLPFit -> LA.Matrix Double -> LA.Matrix Double
predictMLP fit xNew =
  let xUse = if LA.size (mlpXMean fit) > 0
               then applyStandardize (mlpXMean fit) (mlpXStd fit) xNew
               else xNew
      cache = forward (mlpLayers fit) xUse
      raw   = snd (last cache)
      -- regressor の場合、 y も標準化して学習しているので戻す
  in if null (mlpClasses fit) && mlpYStd fit /= 1
       then LA.cmap (\v -> v * mlpYStd fit + mlpYMean fit) raw
       else raw

predictMLPClass :: MLPFit -> LA.Matrix Double -> V.Vector Int
predictMLPClass fit xNew =
  let probs = predictMLP fit xNew
      classes = mlpClasses fit
  in V.generate (LA.rows probs) $ \i ->
       let row = LA.toList (LA.flatten (probs LA.? [i]))
           (best, _) = foldr1 (\(j, p) (jb, pb) ->
                                  if p > pb then (j, p) else (jb, pb))
                       (zip [0 ..] row)
       in classes !! best

-- ===========================================================================
-- helpers
-- ===========================================================================

uniqueSort :: Ord a => [a] -> [a]
uniqueSort = uniqAdj . sortL
  where
    sortL xs = foldr insertSorted [] xs
    insertSorted x [] = [x]
    insertSorted x ys@(y:rest)
      | x <  y = x : ys
      | x == y = ys
      | otherwise = y : insertSorted x rest
    uniqAdj []  = []
    uniqAdj [a] = [a]
    uniqAdj (a:b:rest)
      | a == b = uniqAdj (b : rest)
      | otherwise = a : uniqAdj (b : rest)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

fisherYates :: PrimMonad m => MWC.Gen (PrimState m) -> [a] -> m [a]
fisherYates gen xs =
  let v0 = V.fromList xs
  in go v0 (V.length v0 - 1)
  where
    go v 0 = pure (V.toList v)
    go v i = do
      j <- MWC.uniformR (0, i) gen
      let vi = v V.! i
          vj = v V.! j
          v' = v V.// [(i, vj), (j, vi)]
      go v' (i - 1)
