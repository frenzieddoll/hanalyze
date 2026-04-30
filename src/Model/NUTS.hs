{-# LANGUAGE OverloadedStrings #-}
-- | No-U-Turn Sampler (NUTS)。
--
-- Hoffman & Gelman (2014) Algorithm 3 を実装。
-- リープフロッグと勾配は "Model.HMC" から再利用。
-- 自動的に最適な軌道長を決定するため、HMC のステップ数チューニングが不要。
--
-- 使い方:
--
-- @
-- cfg   = defaultNUTSConfig { nutsStepSize = 0.05 }
-- chain <- nuts myModel cfg initParams gen
-- @
module Model.NUTS
  ( -- * Configuration
    NUTSConfig (..)
  , defaultNUTSConfig
    -- * Sampler
  , nuts
  ) where

import Control.Monad (foldM, forM, when)
import Data.IORef
import Data.Text (Text)
import System.Random.MWC (GenIO, uniform)
import System.Random.MWC.Distributions (standard)

import Model.HBM (Model, Params, logJoint, sampleNames)
import Model.MCMC (Chain (..))
import Model.HMC (kinetic, leapfrog, paramsToVec)

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

data NUTSConfig = NUTSConfig
  { nutsIterations :: Int
    -- ^ バーンイン後に保存するサンプル数
  , nutsBurnIn     :: Int
    -- ^ 破棄するバーンインステップ数
  , nutsStepSize   :: Double
    -- ^ リープフロッグのステップサイズ ε
  , nutsMaxDepth   :: Int
    -- ^ 木の最大深さ (2^maxDepth 回のリープフロッグが上限)。デフォルト 10。
  } deriving (Show)

defaultNUTSConfig :: NUTSConfig
defaultNUTSConfig = NUTSConfig
  { nutsIterations = 2000
  , nutsBurnIn     = 500
  , nutsStepSize   = 0.1
  , nutsMaxDepth   = 10
  }

-- ---------------------------------------------------------------------------
-- 内部: バイナリツリー
-- ---------------------------------------------------------------------------

data NUTSTree = NUTSTree
  { ntThMinus :: Params    -- 木の左端 (負方向) の位置
  , ntRMinus  :: [Double]  -- 木の左端の運動量
  , ntThPlus  :: Params    -- 木の右端 (正方向) の位置
  , ntRPlus   :: [Double]  -- 木の右端の運動量
  , ntThPrime :: Params    -- 採用候補の位置
  , ntN       :: Int       -- スライス内の有効状態数
  , ntS       :: Bool      -- False = U-Turn 検出または発散 → 木の成長を停止
  }

-- | エネルギー増加の上限。これを超えた場合は木の成長を止める。
deltaMax :: Double
deltaMax = 1000.0

-- | U-Turn 判定: (θ+ - θ-) · r- < 0 または (θ+ - θ-) · r+ < 0 なら True を返す。
uTurn :: [Text] -> Params -> [Double] -> Params -> [Double] -> Bool
uTurn names thMinus rMinus thPlus rPlus =
  let delta     = zipWith (-) (paramsToVec names thPlus) (paramsToVec names thMinus)
      dot xs ys = sum (zipWith (*) xs ys)
  in dot delta rMinus < 0 || dot delta rPlus < 0

-- ---------------------------------------------------------------------------
-- 再帰的ツリービルダー (Algorithm 3, Hoffman & Gelman 2014)
-- ---------------------------------------------------------------------------

-- | depth = 0: 1 回のリープフロッグステップ (葉ノード)。
--   depth > 0: 2 つのサブツリーを再帰的に結合。
--
-- dir =  1 → 正方向 (θ+, r+ 側を伸ばす)
-- dir = -1 → 負方向 (θ-, r- 側を伸ばす)
buildTree
  :: Model a
  -> [Text]
  -> Double    -- ^ ε (ステップサイズ)
  -> Params    -- ^ 現在の位置 θ
  -> [Double]  -- ^ 現在の運動量 r
  -> Double    -- ^ スライス変数 (log u)
  -> Int       -- ^ 方向 (+1 / -1)
  -> Int       -- ^ 木の深さ
  -> GenIO
  -> IO NUTSTree
buildTree model names eps theta r logU dir depth gen
  | depth == 0 = do
      -- 葉: 1 ステップのリープフロッグ (dir < 0 なら逆方向)
      let (theta', r') = leapfrog model names (fromIntegral dir * eps) 1 theta r
          h'  = -(logJoint model theta') + kinetic r'
          n'  = if logU <= -h' then 1 else 0
          s'  = logU < deltaMax - h'
      return NUTSTree
        { ntThMinus = theta', ntRMinus = r'
        , ntThPlus  = theta', ntRPlus  = r'
        , ntThPrime = theta', ntN = n', ntS = s'
        }
  | otherwise = do
      -- 内部ノード: サブツリー 1 を構築
      t1 <- buildTree model names eps theta r logU dir (depth - 1) gen
      if not (ntS t1) then return t1
      else do
        -- サブツリー 2 を構築 (木の先端から伸ばす)
        let (th0, r0) = if dir == -1
              then (ntThMinus t1, ntRMinus t1)
              else (ntThPlus  t1, ntRPlus  t1)
        t2 <- buildTree model names eps th0 r0 logU dir (depth - 1) gen
        -- 候補点を確率 min(1, n2/n1) で更新 (Algorithm 6)
        let n1 = ntN t1; n2 = ntN t2
        thPrime' <-
          if n1 == 0 then return (ntThPrime t2)
          else if n2 == 0 then return (ntThPrime t1)
          else do
            u <- uniform gen :: IO Double
            return $ if u < min 1.0 (fromIntegral n2 / fromIntegral n1)
                     then ntThPrime t2
                     else ntThPrime t1
        -- 木の端点を更新
        let (minus', rMinus', plus', rPlus') = if dir == -1
              then (ntThMinus t2, ntRMinus t2, ntThPlus t1, ntRPlus t1)
              else (ntThMinus t1, ntRMinus t1, ntThPlus t2, ntRPlus t2)
            s' = ntS t2
                 && not (uTurn names minus' rMinus' plus' rPlus')
        return NUTSTree
          { ntThMinus = minus', ntRMinus = rMinus'
          , ntThPlus  = plus',  ntRPlus  = rPlus'
          , ntThPrime = thPrime', ntN = n1 + n2, ntS = s'
          }

-- ---------------------------------------------------------------------------
-- NUTS サンプラー
-- ---------------------------------------------------------------------------

-- | NUTS を実行する。
--
-- 1 ステップの手順:
--   1. 運動量 r ~ N(0, I) をサンプリング
--   2. スライス変数 log u ~ Uniform(-∞, -H(θ, r)) をサンプリング
--   3. U-Turn が発生するか最大深さに達するまで木を倍々に成長させる
--   4. 木の中からスライス条件を満たす状態を確率的に選択
nuts :: Model a -> NUTSConfig -> Params -> GenIO -> IO Chain
nuts model cfg init_ gen = do
  let names = sampleNames model
      total = nutsBurnIn cfg + nutsIterations cfg

  samplesRef  <- newIORef []
  acceptedRef <- newIORef (0 :: Int)

  let step current = do
        -- 1. 運動量をサンプリング
        r0 <- forM names (\_ -> standard gen)
        -- 2. スライス変数 (対数域): log u = log(U01) - H(θ, r)
        u0 <- uniform gen :: IO Double
        let h0   = -(logJoint model current) + kinetic r0
            logU = log u0 - h0
        -- 3. 初期ツリー (深さ 0; 現在点のみ)
        let tree0 = NUTSTree
              { ntThMinus = current, ntRMinus = r0
              , ntThPlus  = current, ntRPlus  = r0
              , ntThPrime = current, ntN = 1, ntS = True
              }
        -- 4. 木を倍々に成長させる
        let doubleTree tree j =
              if not (ntS tree) then return tree
              else do
                u <- uniform gen :: IO Double
                let dir = if u < 0.5 then -1 else 1 :: Int
                    (th0, r0') = if dir == -1
                      then (ntThMinus tree, ntRMinus tree)
                      else (ntThPlus  tree, ntRPlus  tree)
                subtree <- buildTree model names (nutsStepSize cfg) th0 r0' logU dir j gen
                -- s'=True のときのみ候補を更新: 確率 min(1, n2/n1) (Algorithm 3)
                let n1 = ntN tree; n2 = ntN subtree
                thPrime' <-
                  if not (ntS subtree) || n2 == 0
                  then return (ntThPrime tree)
                  else do
                    u2 <- uniform gen :: IO Double
                    return $ if u2 < min 1.0 (fromIntegral n2 / fromIntegral n1)
                             then ntThPrime subtree
                             else ntThPrime tree
                -- 端点と停止フラグを更新
                let (minus', rMinus', plus', rPlus') = if dir == -1
                      then (ntThMinus subtree, ntRMinus subtree,
                            ntThPlus  tree,    ntRPlus  tree)
                      else (ntThMinus tree,    ntRMinus tree,
                            ntThPlus  subtree, ntRPlus  subtree)
                    s' = ntS subtree
                         && not (uTurn names minus' rMinus' plus' rPlus')
                return NUTSTree
                  { ntThMinus = minus', ntRMinus = rMinus'
                  , ntThPlus  = plus',  ntRPlus  = rPlus'
                  , ntThPrime = thPrime', ntN = n1 + n2, ntS = s'
                  }
        finalTree <- foldM doubleTree tree0 [0 .. nutsMaxDepth cfg - 1]
        let proposed = ntThPrime finalTree
        when (proposed /= current) $ modifyIORef' acceptedRef (+1)
        return proposed

  let loop 0 current = return current
      loop i current = do
        next <- step current
        if i <= nutsIterations cfg
          then modifyIORef' samplesRef (next :)
          else return ()
        loop (i - 1) next

  _ <- loop total init_
  samples  <- fmap reverse (readIORef samplesRef)
  accepted <- readIORef acceptedRef
  return Chain
    { chainSamples  = samples
    , chainAccepted = accepted
    , chainTotal    = total
    }
