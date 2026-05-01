{-# LANGUAGE OverloadedStrings #-}
-- | Random Walk Metropolis サンプラー。
--
-- 'metropolis' で事後分布からサンプルを得て、'posteriorMean' 等で要約します。
-- ステップサイズ ('mcmcStepSizes') を調整して受容率が 20〜50% になるようにしてください。
-- 'Viz.Report.renderReport' と組み合わせると診断プロットを一括生成できます。
module Model.MCMC
  ( -- * 設定
    MCMCConfig (..)
  , defaultMCMCConfig
    -- * サンプラー
  , Chain (..)
  , metropolis
  , metropolisChains
    -- * マルチチェーンユーティリティ
  , chainVals
    -- * 事後統計量
    -- | 変数名が存在しない場合は 'Nothing' を返します。
  , acceptanceRate
  , posteriorMean
  , posteriorSD
  , posteriorQuantile
  ) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (forM, replicateM)
import Data.IORef
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Word (Word32)
import qualified Data.Vector as V
import System.Random.MWC (GenIO, uniform, initialize)
import System.Random.MWC.Distributions (normal)

import Model.HBM (Model, Params, logJoint, sampleNames)

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

data MCMCConfig = MCMCConfig
  { mcmcIterations :: Int
    -- ^ バーンイン後に保存するサンプル数
  , mcmcBurnIn     :: Int
    -- ^ 破棄するバーンインステップ数
  , mcmcStepSizes  :: Map.Map Text Double
    -- ^ パラメータごとの提案分布 SD。受容率が 0.2〜0.5 になるよう調整する。
  } deriving (Show)

-- | Sensible defaults: 2000 post-burn-in samples, 500 burn-in,
-- step size 1.0 for every named parameter.
defaultMCMCConfig :: [Text] -> MCMCConfig
defaultMCMCConfig names = MCMCConfig
  { mcmcIterations = 2000
  , mcmcBurnIn     = 500
  , mcmcStepSizes  = Map.fromList [(n, 1.0) | n <- names]
  }

-- ---------------------------------------------------------------------------
-- Chain
-- ---------------------------------------------------------------------------

data Chain = Chain
  { chainSamples  :: [Params]  -- ^ Post-burn-in samples in draw order
  , chainAccepted :: Int       -- ^ Accepted proposals (incl. burn-in)
  , chainTotal    :: Int       -- ^ Total proposals (incl. burn-in)
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- Random Walk Metropolis
-- ---------------------------------------------------------------------------

-- | Run the Random Walk Metropolis algorithm.
-- Proposals are joint: every latent variable is perturbed simultaneously
-- by an independent Normal(0, stepSize) offset.
-- Returns the post-burn-in chain; the initial point is NOT included.
metropolis :: Model a -> MCMCConfig -> Params -> GenIO -> IO Chain
metropolis model cfg init_ gen = do
  let names = sampleNames model
      total = mcmcBurnIn cfg + mcmcIterations cfg
      steps = mcmcStepSizes cfg

  samplesRef  <- newIORef []
  acceptedRef <- newIORef (0 :: Int)

  -- One Metropolis step: propose → accept/reject → return new state.
  let step current = do
        proposed <- fmap Map.fromList $ forM names $ \n -> do
          let s   = Map.findWithDefault 1.0 n steps
              cur = Map.findWithDefault 0.0 n current
          eps <- normal 0 s gen
          return (n, cur + eps)
        let logA = logJoint model proposed - logJoint model current
        u <- uniform gen
        if log (u :: Double) < logA
          then do modifyIORef' acceptedRef (+1)
                  return proposed
          else return current

  -- Count down from total to 1; collect when i <= mcmcIterations.
  let loop 0 current = return current
      loop i current = do
        next <- step current
        if i <= mcmcIterations cfg
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

-- ---------------------------------------------------------------------------
-- マルチチェーン
-- ---------------------------------------------------------------------------

-- | 基底 GenIO から独立した子 GenIO を生成する。
spawnGen :: GenIO -> IO GenIO
spawnGen base = do
  seed <- uniform base :: IO Word32
  initialize (V.singleton seed)

-- | Random Walk Metropolis を numChains 本並列実行する。
-- 各チェーンは独立した乱数列を使い、OS スレッドで並列実行される
-- (+RTS -N で CPU 並列になる)。
-- 初期値は全チェーン共通。
metropolisChains
  :: Model a -> MCMCConfig -> Int -> Params -> GenIO -> IO [Chain]
metropolisChains model cfg numChains initP baseGen = do
  gens <- replicateM numChains (spawnGen baseGen)
  mapConcurrently (\g -> metropolis model cfg initP g) gens

-- ---------------------------------------------------------------------------
-- Summary statistics
-- ---------------------------------------------------------------------------

-- | チェーンから指定パラメータのサンプル列を取り出す。
-- rhat 等に渡す用途に使う。
chainVals :: Text -> Chain -> [Double]
chainVals name ch = [v | Just v <- map (Map.lookup name) (chainSamples ch)]

-- | Fraction of proposals that were accepted (including burn-in).
acceptanceRate :: Chain -> Double
acceptanceRate ch =
  fromIntegral (chainAccepted ch) / fromIntegral (chainTotal ch)

-- | Posterior mean for one parameter. Nothing if the name is absent.
posteriorMean :: Text -> Chain -> Maybe Double
posteriorMean name ch =
  let vals = extractVals name ch
  in if null vals then Nothing
     else Just (sum vals / fromIntegral (length vals))

-- | Posterior standard deviation for one parameter.
posteriorSD :: Text -> Chain -> Maybe Double
posteriorSD name ch =
  case posteriorMean name ch of
    Nothing -> Nothing
    Just mu ->
      let vals = extractVals name ch
      in if null vals then Nothing
         else Just (sqrt (sum (map (\x -> (x - mu) ^ (2 :: Int)) vals)
                         / fromIntegral (length vals)))

-- | Empirical quantile (0 ≤ p ≤ 1) of the marginal posterior.
posteriorQuantile :: Double -> Text -> Chain -> Maybe Double
posteriorQuantile p name ch =
  let vals = sort (extractVals name ch)
      n    = length vals
  in if null vals then Nothing
     else
       let idx = min (n - 1) (floor (p * fromIntegral n) :: Int)
       in Just (vals !! idx)

-- ---------------------------------------------------------------------------
-- Internal
-- ---------------------------------------------------------------------------

extractVals :: Text -> Chain -> [Double]
extractVals name ch = [v | Just v <- map (Map.lookup name) (chainSamples ch)]
