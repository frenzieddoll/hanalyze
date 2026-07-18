-- |
-- Module      : Hanalyze.MCMC.Progress
-- Description : MCMC サンプリングの進捗表示 (全 chain 集計を stderr に描画)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- MCMC サンプリングの進捗表示 (Phase 61.2)。
--
-- 'Hanalyze.MCMC.NUTS.nutsChainsStream' の chain index 付き callback に
-- 接続して、 全 chain 集計の進捗 1 行を stderr に描画する:
--
-- > chains 2/4 done | draw 3400/8000 (warmup) | div 12 | 380.0 it/s
--
-- 設計 (phase-61 計画の柱):
--
-- * 表示は「現在の chain」 でなく**全 chain 集計** (chain は mapConcurrently
--   並列で同時進行するため「現在」 が無い)。
-- * callback はサンプラループ内で**同期実行**される ('nutsStream' doc 明記)
--   ので、 描画はカウンタ先行の間引き (全体の ~0.5% 刻み) を通過した時だけ
--   時刻取得 + 描画する。 ホットパスに乗るのはカウンタ更新のみ。
-- * TTY (対話端末) では @\\r@ 上書きの 1 行、 非 TTY (CI ログ等) では
--   10% 刻みの行出力。
-- * 並列 chain からの stderr 競合は 'MVar' の単一描画権で回避
--   (取れなければ描画 skip = 次の間引き通過で追いつく)。
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
module Hanalyze.MCMC.Progress
  ( ProgressSnapshot (..)
  , formatProgress
  , newProgressRenderer
  ) where

import Control.Concurrent.MVar (newMVar, tryTakeMVar, putMVar)
import Control.Monad (when)
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import Data.Text (Text)
import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Clock (getMonotonicTime)
import Numeric (showFFloat)
import System.IO (stderr, hIsTerminalDevice, hFlush)

import Hanalyze.MCMC.NUTS (SampleEvent (..))

-- ===========================================================================
-- スナップショット + 純粋フォーマッタ
-- ===========================================================================

-- | 全 chain 集計の進捗スナップショット (描画と独立な純粋データ)。
data ProgressSnapshot = ProgressSnapshot
  { psChains      :: Int     -- ^ 総 chain 数。
  , psChainsDone  :: Int     -- ^ 完了 chain 数。
  , psDraw        :: Int     -- ^ 全 chain 合算の消化 iteration 数 (burn-in 込み)。
  , psTotal       :: Int     -- ^ 全 chain 合算の総 iteration 数。
  , psWarmup      :: Bool    -- ^ いずれかの chain が warmup (burn-in) 中か。
  , psDivergent   :: Int     -- ^ divergence 累計 (全 chain)。
  , psItersPerSec :: Double  -- ^ 開始からの平均スループット (iteration/s)。
  } deriving (Show, Eq)

-- | 進捗 1 行の純粋フォーマッタ。 例:
--
-- @
-- formatProgress (ProgressSnapshot 4 2 3400 8000 True 12 380.0)
--   == "chains 2\/4 done | draw 3400\/8000 (warmup) | div 12 | 380.0 it\/s"
-- @
formatProgress :: ProgressSnapshot -> Text
formatProgress ps = T.intercalate " | "
  [ "chains " <> tshow (psChainsDone ps) <> "/" <> tshow (psChains ps) <> " done"
  , "draw " <> tshow (psDraw ps) <> "/" <> tshow (psTotal ps)
      <> (if psWarmup ps then " (warmup)" else "")
  , "div " <> tshow (psDivergent ps)
  , T.pack (showFFloat (Just 1) (psItersPerSec ps) "") <> " it/s"
  ]
  where tshow = T.pack . show

-- ===========================================================================
-- stderr レンダラ
-- ===========================================================================

-- | レンダラ内部の可変状態 (chain ごとの消化数 / warmup フラグ / div 累計)。
data RState = RState
  { rsDraws :: !(IM.IntMap Int)   -- ^ chain index → 消化 iteration 数。
  , rsWarm  :: !(IM.IntMap Bool)  -- ^ chain index → 直近 event が burn-in か。
  , rsDiv   :: !Int               -- ^ divergence 累計。
  }

-- | stderr 進捗レンダラを作る。 返り値 = (chain index 付き callback, 終了処理)。
--
-- 終了処理は最終スナップショットを描画して行を閉じる (TTY では改行を補う)。
-- 'Hanalyze.MCMC.NUTS.nutsChainsStream' に渡す想定:
--
-- @
-- (onSample, finish) <- newProgressRenderer chains (burnIn + iters)
-- chains <- nutsChainsStream m cfg chains initC seed onSample
-- finish
-- @
newProgressRenderer :: Int   -- ^ 総 chain 数
                    -> Int   -- ^ chain あたりの総 iteration 数 (burn-in 込み)
                    -> IO (Int -> SampleEvent -> IO (), IO ())
newProgressRenderer nChains perChain = do
  isTTY    <- hIsTerminalDevice stderr
  t0       <- getMonotonicTime
  stRef    <- newIORef (RState IM.empty IM.empty 0)
  lastPct  <- newIORef (-1 :: Int)   -- 非 TTY の 10% 刻み判定
  drawLock <- newMVar ()             -- 単一描画権
  let totalAll = nChains * perChain
      stride   = max 1 (totalAll `div` 200)   -- ~0.5% 刻みで描画候補

      snapshot :: RState -> Double -> ProgressSnapshot
      snapshot st now =
        let drawn = sum (IM.elems (rsDraws st))
            done  = IM.size (IM.filter (>= perChain) (rsDraws st))
            warm  = or (IM.elems (rsWarm st))
            dt    = max 1e-9 (now - t0)
        in ProgressSnapshot
             { psChains = nChains, psChainsDone = done
             , psDraw = drawn, psTotal = totalAll
             , psWarmup = warm, psDivergent = rsDiv st
             , psItersPerSec = fromIntegral drawn / dt
             }

      -- 描画権が取れた時だけ描画 (競合時は skip・次の間引きで追いつく)。
      render :: Bool -> IO ()
      render final = do
        got <- tryTakeMVar drawLock
        case got of
          Nothing -> pure ()
          Just () -> do
            st  <- readIORef stRef
            now <- getMonotonicTime
            let snap = snapshot st now
                line = formatProgress snap
            if isTTY
              then do
                TIO.hPutStr stderr ("\r" <> line)
                when final (TIO.hPutStr stderr "\n")
                hFlush stderr
              else do
                -- 非 TTY: 10% 境界を跨いだ時 (or 終了時) だけ 1 行出す。
                let pct10 = (10 * psDraw snap) `div` max 1 totalAll
                prev <- readIORef lastPct
                when (pct10 > prev || final) $ do
                  atomicModifyIORef' lastPct (\p -> (max p pct10, ()))
                  TIO.hPutStrLn stderr line
                  hFlush stderr
            putMVar drawLock ()

      onSample :: Int -> SampleEvent -> IO ()
      onSample i ev = do
        n <- atomicModifyIORef' stRef $ \st ->
          let st' = RState
                { rsDraws = IM.insertWith (+) i 1 (rsDraws st)
                , rsWarm  = IM.insert i (seIsBurnIn ev) (rsWarm st)
                , rsDiv   = rsDiv st + (if seDivergent ev then 1 else 0)
                }
          in (st', sum (IM.elems (rsDraws st')))
        when (n `mod` stride == 0) (render False)

  pure (onSample, render True)
