-- |
-- Module      : Hanalyze.Plot.Bayes
-- Description : hgg 連携層 — ベイズ / HBM 連携族の図化 instance + 抽出子
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- hgg 連携層 — **ベイズ / HBM 連携族** の図化 instance + 抽出子 (Phase 71.6)。
--
-- ⚠ 親 'Hanalyze.Plot' と同じ cabal flag @plot-integration@ (既定 off) を
-- on にしたときのみ build される。 共通基盤 (class / ModelSpec / grid 評価核) は
-- 'Hanalyze.Plot.Core' を import して取り込む (orphan instance を許容)。
--
-- 担当する型・抽出子 (= MCMC chain / HBM 出力):
--   ChainModel の trace / 周辺事後密度・HBM の trace/forest/epred/ppc/dag 抽出子 (Phase 74 統一)・
--   GLMMResultRE の caterpillar plot。 HBM の *学習* (hbmModel 等) は
--   'Hanalyze.Fit' / 'Hanalyze.Model.Wrappers' 側 (こちらは描画連携のみ)。
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Hanalyze.Plot.Bayes
  ( -- * HBM の出力抽出子 — Phase 49 A2 / Phase 74 (trace / forest)
    hbmParamNames
  , TraceOpts (..)
  , defaultTraceOpts
  , tracesOf
  , tracesOfWith
  , marginalsOf
  , marginalsByChainOf
    -- * HBM のサンプリング診断 — Phase 59 (divergence 可視化)
  , divergencesOf
  , pairOf
  , energyOf
  , autocorrOf
  , autocorrOfLag
  , defaultAutocorrMaxLag
  , rankOf
  , rankOfBins
  , defaultRankBins
  , ForestSpec (..)
  , forestOf
  , forestOfLevel
    -- * HBM の出力抽出子 — Phase 49 A3 (epred = 事後予測平均 + HDI band)
  , epred
  , epredAt
  , epredPredRange
    -- * 応答曲面 3D / 散布 (HBM 固有) — Phase 71.7
  , epredSurfaceOf
  , epredSurfaceOfWith
  , dataScatterOf
    -- * HBM の出力抽出子 — Phase 49 A4 (ppc = 事後予測チェック)
  , PPCConfig (..)
  , defaultPPC
  , PPCSpec (..)
  , ppcOf
  , ppcOfWith
  , ppcOfIO
  , ppcOfWithIO
    -- * HBM の出力抽出子 — Phase 49 A5 (dag = モデル構造の DAG)
  , DagSpec (..)
  , dagOf
  , dagOfRaw
  , dagOfModel
  , dagOfModelWith
    -- * HBM 診断ダッシュボード — Phase 74.8 (抽出子束ね)
  , dashboardOf
  , dashboardFullOf
  , traceDensityOf
  ) where

import           Data.List             (sortBy, transpose)
import qualified Data.Map.Strict       as Map
import           Data.Maybe            (fromMaybe)
import           Data.Ord              (comparing)
import           Data.Word             (Word32)
import qualified Data.Vector           as V
import           System.Random.MWC     (createSystemRandom, initialize, Gen)
import           Control.Monad.Primitive (PrimMonad, PrimState)
import           Control.Monad.ST      (runST)
import qualified Numeric.LinearAlgebra as LA

import           Data.Text             (Text)
import qualified Data.Text             as T

import           Hgg.Plot.Spec     ( VisualSpec, layer, inline, inlineCat
                                       , Color (..), fromHex
                                       , scatter, line, band, bar
                                       , position, Position (..)
                                       , color, colorBy, lineRange
                                       , scaleColorManual, legendOff
                                       , legendPos, LegendPosition (..)
                                       , trace, density, forest, forestNull
                                       , subplots, subplotCols, width, height
                                       , xLabel, yLabel, title
                                       , ecdf, alpha
                                       , dagFromListsWithPlates
                                       , DAGNode (..), DAGEdge (..), DAGPlate (..)
                                       , DAGNodeKind (..), DAGLayoutAlgorithm (..) )
import           Hgg.Plot.DAG      (layoutHierarchicalFullWithPlates)
import           Hgg.Plot.Render.Special (bakeDAGRoutesInSpec)
import qualified Hgg.Plot.ThreeD.Spec  as P3

import           Hanalyze.Model.Wrappers
import           Hanalyze.Plot.Core
import           Hanalyze.MCMC.Core     (Chain (..), chainVals)
import           Hanalyze.MCMC.BayesianTest (highestDensityInterval)
import           Hanalyze.Stat.MCMC    (kde, autocorr, rankHist)
import           Hanalyze.Model.HBM.Sampling (sampleObsRep)
import           Hanalyze.Model.HBM     (ModelP, withData
                                       , runDeterministics, runObserveDists
                                       , buildModelGraph, ModelGraph (..)
                                       , collapseIndexedPlateNodes
                                       , sampleNames
                                       , Node (..), NodeKind (..))
import           Hanalyze.Model.LM     (linspace)
import           Hanalyze.Model.GLMM   (GLMMResultRE (..))

-- ===========================================================================
-- MCMC チェーン (描画可能)
--
-- 'Chain' (Hanalyze.MCMC.Core) は post-burn-in の draw 列 'chainSamples' を保持する
-- (各 draw は Map パラメータ名→値)。 ベイズの「出入口」 = サンプラの収束診断と周辺事後の
-- 可視化。 1 つのパラメータを選び、 代表図 ('toPlot') は **trace plot** (draw index 対値、
-- = 混合・定常性の目視)、 診断束 ('diagnosticPlots') に **周辺事後密度** (MDensity) を加える。
-- trace と density は座標系が異なる (index-値 vs 値-密度) ため 1 枚に混ぜず別図にする。
-- ===========================================================================

instance Plottable ChainModel where
  -- trace plot: draw index 対 パラメータ値 (折れ線 = MTrace)。
  toPlot m =
    let vals  = chainVals (cmParam m) (cmChain m)
        iters = [ fromIntegral i | i <- [1 .. length vals] ] :: [Double]
    in layer (trace (inline iters) (inline vals))

  -- 診断束: trace + 周辺事後密度 (MDensity)。
  diagnosticPlots m =
    let vals  = chainVals (cmParam m) (cmChain m)
        iters = [ fromIntegral i | i <- [1 .. length vals] ] :: [Double]
    in [ layer (trace (inline iters) (inline vals))
       , layer (density (inline vals))
       ]

-- ===========================================================================
-- HBM の出力抽出子 — Phase 49 A2 / Phase 74 (trace / forest)
--
-- 'HBMModel' は直接 'Plottable' にしない (確率プログラムは単一の図に一意に落ちない)。
-- 代わりに抽出子を明示する。 trace は 'tracesOf' / 'tracesOfWith' に統一:
--   * 'tracesOf'     = 各 latent パラメータの trace plot を **param ごと独立パネル**
--                      ('[VisualSpec]') で返す。 divergence rug は既定 ON (ArviZ 流)。
--   * 'tracesOfWith' = 'TraceOpts' で divergence on/off と chain 別重畳を切り替える。
--   * 'forestOf'     = 各 latent の事後区間 (事後平均 + 94% HDI) を 'MForest' mark で。
--
-- ★ Phase 74 で旧 'traceOf' ([ChainModel]) / 'tracesByChainOf' /
-- 'tracesWithDivergencesOf' の 3 本を統合した。 戻り型を兄弟抽出子 (marginalsOf 等)
-- と同じ '[VisualSpec]' に揃え、 @vconcat (tracesOf m)@ で param ごと縦並びに描ける
-- (旧 docs の @foldMap toPlot (traceOf m)@ = 全 param を 1 軸に重畳する誤りを排除)。
-- ===========================================================================

-- | 学習済モデルの latent パラメータ名 (= 事後を持つ未知数の一覧)。
hbmParamNames :: HBMModel -> [Text]
hbmParamNames = sampleNames . hbmModelSpec

-- | 1 パラメータの post-burn-in draw を全 chain 連結で取り出す。
hbmDraws :: Text -> HBMModel -> [Double]
hbmDraws name = concatMap (chainVals name) . hbmChainsR

-- | 全 chain の draw を 1 本に連結した 'Chain' (trace 表示用)。 index は
-- chain を端から端へ並べた通し番号になる (A2 の trace は混合の目視が目的)。
-- Phase 59.4: divergence index も同じ連結順の通し番号に変換する
-- ('pooledDivergences' が正本。 chain 内 index のまま連結すると merged frame で
-- 別の draw を指してしまう)。
mergeChains :: [Chain] -> Chain
mergeChains []  = Chain [] 0 0 [] [] []
mergeChains chs = Chain
  { chainSamples     = concatMap chainSamples chs
  , chainAccepted    = sum (map chainAccepted chs)
  , chainTotal       = sum (map chainTotal chs)
  , chainEnergy      = concatMap chainEnergy chs
  , chainDivergences = pooledDivergences chs
  , chainTreeDepths  = concatMap chainTreeDepths chs
  }

-- | trace 診断の設定 ('ppcOf' / 'PPCConfig' と同じ「関数 + config」 慣用)。
data TraceOpts = TraceOpts
  { toShowDivergences :: !Bool  -- ^ 発散 draw の rug を重ねる (既定 True・ArviZ 流)。
  , toByChain         :: !Bool  -- ^ True で chain 別重畳、 False で全 chain merged (既定)。
  } deriving (Show, Eq)

-- | 既定の trace 設定 = divergence rug ON・全 chain merged。
defaultTraceOpts :: TraceOpts
defaultTraceOpts = TraceOpts { toShowDivergences = True, toByChain = False }

-- | 各 latent パラメータの trace plot を **param ごと独立パネル** ('[VisualSpec]')
-- で返す (divergence rug 既定 ON)。 @noDf |>> vconcat (tracesOf m)@ で param ごとに
-- 縦並びの trace になる (= ArviZ @plot_trace@ 右列)。 設定は 'tracesOfWith'。
tracesOf :: HBMModel -> [VisualSpec]
tracesOf = tracesOfWith defaultTraceOpts

-- | 'TraceOpts' を明示する 'tracesOf'。 旧 'traceOf' (merged 単線) /
-- 'tracesByChainOf' (chain 別重畳) / 'tracesWithDivergencesOf' (chain 別 + rug) の
-- 3 本を 1 つに統合したもの:
--
--   * @tracesOfWith (TraceOpts False False)@ = 旧 'traceOf' 相当 (merged 単線・rug 無し)
--   * @tracesOfWith (TraceOpts False True )@ = 旧 'tracesByChainOf' (chain 別重畳・rug 無し)
--   * @tracesOfWith (TraceOpts True  True )@ = 旧 'tracesWithDivergencesOf' (chain 別 + rug)
--   * 既定 @tracesOf@ = @TraceOpts True False@ (merged + rug)
--
-- divergence rug は各図下端 (y = 当該 param の全 chain 最小値) に発散 draw の x 位置を
-- 縦棒 ('lineRange') で打つ。 merged では通し index ('divergencesOf')、 chain 別では
-- chain 内 1-based iteration を x にする (それぞれの trace の x 軸と整合)。
-- divergence が無ければ rug レイヤは付かない。
tracesOfWith :: TraceOpts -> HBMModel -> [VisualSpec]
tracesOfWith opts hbm =
  [ traceLayers nm <> rugLayer nm <> title nm | nm <- hbmParamNames hbm ]
  where
    chs = hbmChainsR hbm
    traceLayers nm
      | toByChain opts =
          foldMap (\(k, ch) ->
              let vals  = chainVals nm ch
                  iters = [ fromIntegral i | i <- [1 .. length vals] ] :: [Double]
              in layer (trace (inline iters) (inline vals) <> color (fromHex (chainColor k))))
            (zip [0 ..] chs)
      | otherwise =
          let vals  = chainVals nm (mergeChains chs)
              iters = [ fromIntegral i | i <- [1 .. length vals] ] :: [Double]
          in layer (trace (inline iters) (inline vals))
    rugLayer nm
      | not (toShowDivergences opts) = mempty
      | otherwise =
          let allVals = concatMap (chainVals nm) chs
              -- merged: 連結通し index (divergencesOf)。chain 別: chain 内 1-based iteration。
              xs | toByChain opts = [ fromIntegral (i + 1) | ch <- chs, i <- chainDivergences ch ] :: [Double]
                 | otherwise      = [ fromIntegral (i + 1) | i <- divergencesOf hbm ] :: [Double]
          in if null xs || null allVals
               then mempty
               -- ArviZ tick 同型 = 下端から値域 2% の短い縦棒。 定数 trace では 1e-9 最小高。
               -- ★lineRange の意味論は (x, 中心 y, ±err) = 下端 yMin〜yMin+tick の棒。
               else let yMin = minimum allVals
                        yMax = maximum allVals
                        tick = max ((yMax - yMin) * 0.02) 1e-9
                        nDiv = length xs
                    in layer (lineRange (inline xs)
                                        (inline (replicate nDiv (yMin + tick / 2)))
                                        (inline (replicate nDiv (tick / 2)))
                              <> color (fromHex divergenceColor))

-- | 各 latent パラメータの **周辺事後密度** を per-param で list 返しする。
-- 'tracesOf' (per-param trace) の密度版で、 'ChainModel' の @diagnosticPlots@ が出す
-- 周辺事後密度 (@density@、 root: 'diagnosticPlots' ChainModel 経路) を 1 パラメータ
-- 1 図に切り出したもの。 全 chain の post-burn-in draw をプール ('hbmDraws') した
-- 周辺分布を描き、 図タイトルにパラメータ名を付す。
--
-- @subplots (map toPlot (marginalsOf fit)) <> subplotCols 1@ で周辺事後の grid を組め、
-- B1 の入れ子 subplots と合わせて HBM ダッシュボードの 1 列になる。
marginalsOf :: HBMModel -> [VisualSpec]
marginalsOf hbm =
  [ layer (density (inline (hbmDraws nm hbm))) <> title nm
  | nm <- hbmParamNames hbm ]

-- ===========================================================================
-- HBM のサンプリング診断 — Phase 59.4 / 74 (divergence の通し index + pair/energy)
--
-- 'Chain' は NUTS の発散 draw index ('chainDivergences' = chain 内 0-based・
-- post-burn-in、 root: request/255 §4) と Hamiltonian energy を記録済み。 ここでは
-- それを plot-core の語彙 (scatter + color) で図示する。 rug 用の新 MarkKind は
-- 追加しない (計画 md の設計判断: 既存 mark の組合せで足りることを確認してから諮る)。
-- trace の divergence rug 自体は 'tracesOfWith' (Phase 74 統合) に移譲した。
-- ===========================================================================

-- | [Chain] の発散 draw を連結順の通し index に変換する内部正本
-- (chain c の offset = それ以前の chain の draw 数合計)。 'mergeChains' /
-- 'divergencesOf' の双方がこれを使う (重複実装しない)。
pooledDivergences :: [Chain] -> [Int]
pooledDivergences chs =
  concat [ map (+ off) (chainDivergences ch)
         | (off, ch) <- zip offsets chs ]
  where offsets = scanl (+) 0 (map (length . chainSamples) chs)

-- | 全 chain を pool した発散 draw の通し index ('mergeChains' の連結順と整合)。
-- 'tracesOf' (merged trace) の rug 位置や、 発散 draw の抽出
-- (@map (chainSamples merged !!) (divergencesOf fit)@) に使う。
divergencesOf :: HBMModel -> [Int]
divergencesOf = pooledDivergences . hbmChainsR

-- | divergence rug / 強調点の色 ('Hanalyze.Viz.MCMC' の pairScatterDiv と同じ赤)。
divergenceColor :: Text
divergenceColor = "#dd2222"   -- 小文字 (toCss 出力と byte 一致・視覚は #DD2222 と同一)

-- | ArviZ @plot_pair(divergences=True)@ 流: 指定パラメータ対の joint 散布
-- (全 chain pool・薄表示) + 発散 draw を強調色で重畳。 funnel 診断の本命
-- (例: @pairOf fit [("tau_b1", "b1_2")]@ で漏斗の首に発散が集中するのが見える)。
-- 発散 draw の抽出は 'divergencesOf' の通し index を pool 後の draw 列に引く
-- (chain 連結順は 'hbmDraws' = 'mergeChains' と同一)。
-- divergence が無ければ強調レイヤは付かない。
pairOf :: HBMModel -> [(Text, Text)] -> [VisualSpec]
pairOf hbm prs =
  [ let xs   = hbmDraws xn hbm
        ys   = hbmDraws yn hbm
        n    = min (length xs) (length ys)
        dIdx = [ i | i <- divergencesOf hbm, i < n ]
        dxs  = map (xs !!) dIdx
        dys  = map (ys !!) dIdx
    in layer (scatter (inline xs) (inline ys) <> alpha 0.25)
       <> (if null dIdx
             then mempty
             else layer (scatter (inline dxs) (inline dys)
                         <> color (fromHex divergenceColor)))
       <> xLabel xn <> yLabel yn <> title (xn <> " × " <> yn)
  | (xn, yn) <- prs ]

-- | ArviZ @plot_energy@ 流: marginal energy (E − Ē、 chain 別中心化) と
-- transition energy (ΔE = E_{i+1} − E_i、 chain 内差分・境界を跨がない) の密度重畳。
-- ΔE 分布が marginal より極端に狭ければ、 サンプラが posterior の energy 分布を
-- 探索しきれていないサイン (低 BFMI 相当。 数値は 'Hanalyze.Viz.MCMC' の bfmi)。
-- energy ('chainEnergy' = draw ごとの Hamiltonian) は HMC / NUTS のみ記録される
-- ため、 MH / Gibbs 等の fit では空図になる。 系列名は Viz 側 energyPlot と同一。
--
-- ★mark は 'density' でなく KDE ('Hanalyze.Stat.MCMC' の kde 200 = Viz energyPlot と
-- 同一) + 'line'。 理由: 固定色 'color' と categorical 'colorBy' は同一 field (lyColor) の
-- Last で相互排他、 かつ renderDensity は categorical 色を見ない (staticColorOr のみ) ため、
-- density mark では「2 色の曲線 + 凡例」 が両立できない。 line は群色対応済なので
-- 多モデル重畳 (line + color inlineCat + scaleColorManual + legend) の確立パターンに
-- 乗せる。
energyOf :: HBMModel -> VisualSpec
energyOf hbm =
  curve lblMar eMar <> curve lblTr eTrans <> legendSpec
    <> xLabel "Energy" <> yLabel "Density" <> title "energy"
  where
    lblMar = "marginal E (centered)"
    lblTr  = "transition ΔE"
    ess    = filter (not . null) (map chainEnergy (hbmChainsR hbm))
    center es = let mu = sum es / fromIntegral (length es)
                in map (subtract mu) es
    eMar   = concatMap center ess
    eTrans = concatMap (\es -> zipWith (-) (drop 1 es) es) ess
    curve lbl vals
      | length vals < 2 = mempty
      | otherwise =
          let (gx, gy) = unzip (kde 200 vals)
          in layer (line (inline gx) (inline gy)
                    <> colorBy (inlineCat (replicate (length gx) lbl)))
    legendSpec
      | null eMar = mempty
      | otherwise = scaleColorManual [ (lblMar, "#4C72B0"), (lblTr, "#DD8452") ]
                      -- 凡例は図内 (右上)。 密度は中央が高く右裾は 0 ゆえ右上が空く。
                      -- 外・右だと右に余白が出て subplot/dashboard が不格好になる。
                      <> legendPos LegendInsideTopRight

-- | chain 別の **周辺事後密度** を 1 図に重畳した per-param list (= ArviZ @plot_trace@ 左側 /
-- @plot_posterior@ の chain 重ね)。 'marginalsOf' が全 chain プールの 1 本を描くのに対し、
-- こちらは chain ごとに別レイヤを 'color' ('fromHex') で重ねる。
marginalsByChainOf :: HBMModel -> [VisualSpec]
marginalsByChainOf hbm =
  [ foldMap (\(k, ch) -> layer (density (inline (chainVals nm ch)) <> color (fromHex (chainColor k))))
            (zip [0 ..] (hbmChainsR hbm))
    <> title nm
  | nm <- hbmParamNames hbm ]

-- | 自己相関 plot の既定最大ラグ (= ArviZ @plot_autocorr@ の見やすさに合わせた 30。
-- ArviZ 既定の 100 は SVG では横に潰れるので短めにする)。
defaultAutocorrMaxLag :: Int
defaultAutocorrMaxLag = 30

-- | 各 latent パラメータの **自己相関** を per-param list で返す (= ArviZ @plot_autocorr@)。
-- lag 0..'defaultAutocorrMaxLag' の ACF を縦棒 ('bar') で描く。 chain 連結の境界アーティ
-- ファクトを避けるため **chain ごとに 'autocorr' を計算し lag ごとに平均**する
-- ('energyOf' が chain 別に算出して連結するのと同方針)。 ACF が速く 0 に減衰するほど
-- mixing が良い (高い自己相関 = ESS 低下のサイン)。
autocorrOf :: HBMModel -> [VisualSpec]
autocorrOf = autocorrOfLag defaultAutocorrMaxLag

-- | 最大ラグを明示する 'autocorrOf'。
autocorrOfLag :: Int -> HBMModel -> [VisualSpec]
autocorrOfLag maxLag hbm =
  [ acSpec nm | nm <- hbmParamNames hbm ]
  where
    chains = hbmChainsR hbm
    acSpec nm =
      let perChain = [ autocorr maxLag vs
                     | c <- chains, let vs = chainVals nm c, not (null vs) ]
      in case perChain of
           []      -> mempty
           (ac0:_) ->
             let lags    = map (fromIntegral . fst) ac0 :: [Double]
                 acfByCh = map (map snd) perChain                  -- [chain][lag]
                 meanACF = map (\col -> sum col / fromIntegral (length col))
                               (transpose acfByCh)                 -- lag ごとの chain 平均
             -- y 軸ラベルは省く (図が潰れるため。 title でパラメータ名は分かる)。
             in layer (bar (inline lags) (inline meanACF))
                  <> title nm <> xLabel "lag"

-- | rank plot の既定ビン数 (= PyMC @plot_rank@ 既定 20)。
defaultRankBins :: Int
defaultRankBins = 20

-- | 各 latent パラメータの **rank plot** を per-param list で返す (= ArviZ @plot_rank@・
-- Vehtari et al. 2021)。 全 chain をプールした値の rank を chain ごとにヒストグラム化し、
-- chain 別の棒を色分けして重畳する。 **収束時は各 chain がほぼ一様** (= どのビンも同程度)。
-- chain が偏る (= 山ができる) と R̂ 悪化のサイン。 rank 計算は 'rankHist' (Stat.MCMC) に
-- 一元化し Viz 経路と共有する。 **要 chain ≥ 2** (1 本だと rank が自明に一様ゆえ空図)。
rankOf :: HBMModel -> [VisualSpec]
rankOf = rankOfBins defaultRankBins

-- | ビン数を明示する 'rankOf'。
rankOfBins :: Int -> HBMModel -> [VisualSpec]
rankOfBins nBins hbm =
  [ rankSpec nm | nm <- hbmParamNames hbm ]
  where
    chains = hbmChainsR hbm
    nCh    = length chains
    rankSpec nm =
      let perChain = map (chainVals nm) chains
      in if nCh < 2 || all null perChain
           then mempty
           else
             -- chain を横並び (dodge) にした 1 層の bar (= ArviZ plot_rank の単一パネル版)。
             -- long-form: (bin, count, chain) を chain×bin 行で展開し colorBy + PosDodge。
             let hists    = rankHist nBins perChain                 -- [chain][bin]
                 -- ビンは categorical だが軸はアルファベット順ゆえ、 数値順を保つよう
                 -- 0 埋めラベル ("00".."19") にする (= 文字列ソート = 数値順)。
                 w        = length (show (nBins - 1))
                 pad i    = let s = show (i :: Int)
                            in T.pack (replicate (w - length s) '0' ++ s)
                 binCat   = concat [ [ pad b | b <- [0 .. nBins - 1] ] | _ <- [1 .. nCh] ]
                 cntCol   = concatMap (map fromIntegral) hists :: [Double]
                 chainCat = concat [ replicate nBins (T.pack ("chain " <> show k))
                                   | k <- [0 .. nCh - 1] ]
             -- y 軸ラベル・凡例は省く (図が潰れるため。 chain は色 dodge で判別可)。
             -- colorBy は既定で凡例を出すので legendOff で明示的に抑制する。
             in layer ( bar (inlineCat binCat) (inline cntCol)
                        <> colorBy (inlineCat chainCat)
                        <> position PosDodge )
                  <> title nm <> xLabel "rank bin" <> legendOff


-- | 係数 forest plot の描画仕様。 'HBMModel' を直接 'Plottable' にしないため、
-- 抽出後の図を包む薄い newtype (後続 sub の ppc/epred/dag も同型に揃える)。
newtype ForestSpec = ForestSpec { unForestSpec :: VisualSpec }

instance Plottable ForestSpec where
  toPlot = unForestSpec

-- | 各 latent パラメータの事後区間を 1 枚の forest plot にする (94% HDI 既定)。
forestOf :: HBMModel -> ForestSpec
forestOf = forestOfLevel 0.94

-- | 信頼水準を明示する 'forestOf'。 point = 事後平均、 bar 半幅 = HDI 半幅。
--
-- ★ 'forest' mark は対称 CI (± 半幅) のみ対応するため、 非対称な HDI は
-- 「事後平均 ± (hi−lo)/2」 の対称バーで近似表示する (mark 側の TODO = 非対称 forest)。
forestOfLevel :: Double -> HBMModel -> ForestSpec
forestOfLevel level hbm = ForestSpec $
  layer (forest (inlineCat names) (inline ests) (inline errs) <> forestNull 0)
  where
    names = hbmParamNames hbm
    rows  = [ (mean, (hi - lo) / 2)
            | nm <- names
            , let d        = hbmDraws nm hbm
                  mean     = if null d then 0 else sum d / fromIntegral (length d)
                  (lo, hi) = highestDensityInterval level d
            ]
    ests = map fst rows
    errs = map snd rows

-- ===========================================================================
-- HBM の事後予測平均 — Phase 49 A3 (epred = E[y|x] の grid 評価 + HDI band)
--
-- ベイズ回帰の代表図。 予測子 (@predName@、 学習時 'dataNamed' の参照名) を grid 上で
-- 1 点ずつ動かし、 各 posterior draw でモデル中の deterministic ノード (@muName@、
-- 通常は線形予測子の平均 μ) を 'runDeterministics' で評価する。 これで grid 点ごとに
-- N draws 分の μ サンプルが得られ、 その **事後平均** (線) と **94% HDI** (帯、 ArviZ 既定)
-- を描く。 これは PyMC の @pm.sample_posterior_predictive@ で得る epred (expected value of
-- the posterior predictive) に相当する (観測ノイズを含まない平均の不確実性)。
--
-- ★ O1 規約: epred 用モデルは予測子を @dataNamed predName@ で受け、 その平均を
-- @deterministic muName@ で 1 点スカラとして公開する (学習 likelihood とは併存)。
-- grid 評価では @withData predName [xi]@ で 1 点に差し替えるため、 deterministic 内で
-- @head x@ を取れば @xi@ が読める。
--
-- ★ Phase 74: 多予測子の hold。 非軸の予測子 slot は、 既定では 'HoldAgg' に従って
-- bind データの集約値 ('Mean' 既定) で固定する (旧実装は bind データ先頭値 @head@ に
-- 固定で選択不能だった)。 頻度論 effect plot ('statModelMulti') と **同じ語彙**を共有:
--   * @epred fit "x1" "mu" \<\> holdAt Median@         … 非軸を中央値で固定
--   * @epred fit "x1" "mu" \<\> holdAt (Fixed [("x2", 5)])@  … x2 のみ 5・他は Mean
--   * @epred fit "x1" "mu" \<\> byVar "x2" [0, 1]@      … x2 の水準別に曲線色分け重畳
-- 'holdAt' / 'byVar' は 'Hanalyze.Plot.Core' の既存コンビネータ (= ModelSpec の
-- @msHoldAt@ / @msByVar@ を設定) をそのまま使う (epred 専用版は作らない)。
--
-- ★ 設計: 専用 newtype を作らず **'ModelSpec' を再利用**する (record は描画クロージャの
-- 容れ物で 'SingleVarModel' 束縛ではない)。 これにより @epred hbm "x" "mu" \<\> grid 200
-- \<\> statLevel 0.9@ が Phase 16 C1 のコンビネータと同綴りで合成できる (既定 level 0.94 と
-- 帯 ON = ArviZ 流の HDI 帯を焼き込む。 epred の帯はオプトアウト不可)。
-- ===========================================================================

-- | 1 つの予測子値 @x@ における事後予測平均と HDI (非軸予測子は bind データのまま)。
-- @predName@ を @[x]@ に差し替え、 全 chain の各 draw で deterministic @muName@ を
-- 評価し、 (事後平均, (lo, hi)) を返す。 非軸予測子を固定する版は 'epredAtHeld'。
epredAt
  :: HBMModel
  -> Text     -- ^ 予測子の data 参照名 (@dataNamed@ / @withData@ の名前)。
  -> Text     -- ^ 平均の deterministic ノード名。
  -> Double   -- ^ HDI 水準 (例 0.94)。
  -> Double   -- ^ 予測子値 x。
  -> (Double, (Double, Double))
epredAt hbm = epredAtHeld hbm []

-- | (slot 名, 固定値) のリストを @withData@ で 1 点ずつ bind してネストする。
-- 'ModelP' は impredicative (@forall a. Model a r@) ゆえ foldr では多相が逃げる。
-- トップレベル再帰なら各 'withData' が @ModelP r -> ModelP r@ を保つので通る。
bindHolds :: [(Text, Double)] -> ModelP r -> ModelP r
bindHolds []              m = m
bindHolds ((nm, v) : rest) m = withData nm [v] (bindHolds rest m)

-- | 'epredAt' の多予測子版。 @holds@ = 非軸予測子の (slot 名, 固定値) を 1 点ずつ
-- @withData@ で bind し ('head' でその値が読める)、 軸 @predName@ を @[gx]@ に差し替える。
epredAtHeld
  :: HBMModel
  -> [(Text, Double)]   -- ^ 非軸予測子の固定 (slot 名, 値)。
  -> Text -> Text -> Double -> Double
  -> (Double, (Double, Double))
epredAtHeld hbm holds predName muName level gx =
  let bound :: ModelP ()
      bound = withData predName [gx] (bindHolds holds (hbmModelSpec hbm))
      draws = concatMap chainSamples (hbmChainsR hbm)
      mus   = [ v | ps <- draws
                  , Just v <- [Map.lookup muName (runDeterministics bound ps)] ]
      mean  = if null mus then 0 else sum mus / fromIntegral (length mus)
  in (mean, highestDensityInterval level mus)

-- | grid 点 @gx@ における事後予測区間 (PI = 観測ノイズ込みの新規 1 点の HDI)。
-- 'epredAtHeld' が deterministic μ の HDI (= CI 相当) を返すのに対し、 こちらは
-- **観測ノードの予測分布から y をサンプルしてプール**し HDI を取る。 観測ノード名は
-- 引数に取らず 'runObserveDists' でモデルから自動検出する (頻度論 'svGridPI' が obs 名を
-- 要らないのと対称)。 単一 likelihood の通常ケースが対象で、 observe が複数なら全プール。
-- 任意の観測分布 (Normal/Poisson/NegBinom…) に効く ('ppc' の 'sampleDist' を再利用)。
-- @runST@ + 固定 seed (既定 'epredPISeed' = 42・'ppcOfWith' と同方式) で純粋・決定的。
epredPIAtHeld
  :: HBMModel
  -> [(Text, Double)]   -- ^ 非軸予測子の固定 (slot 名, 値)。
  -> Text               -- ^ 軸予測子の data 参照名。
  -> Word32             -- ^ サンプリング seed。
  -> Double             -- ^ HDI 水準。
  -> Double             -- ^ 予測子値 x。
  -> (Double, Double)
epredPIAtHeld hbm holds predName seed level gx =
  let bound :: ModelP ()
      bound = withData predName [gx] (bindHolds holds (hbmModelSpec hbm))
      draws = concatMap chainSamples (hbmChainsR hbm)
      samples = runST $ do
        gen <- initialize (V.singleton seed)
        concat <$> mapM
          (\ps ->
             let nodes = [ (d, ys) | (_, d, ys) <- runObserveDists bound ps ]
             in concat <$> mapM (\(d, ys) -> sampleObsRep gen d ys) nodes)
          draws
  in if null samples then (0, 0) else highestDensityInterval level samples

-- | 'epredPIAtHeld' の既定サンプリング seed (純粋・決定的に閉じる。 'ppcOfWith' と同値)。
epredPISeed :: Word32
epredPISeed = 42

-- | 非軸予測子 1 slot の固定値を 'HoldAgg' (+ byVar override) から決める。
-- @override@ (byVar の明示固定) が 'HoldAgg' より優先。 HBM データは数値列ゆえ
-- factor / Reference は無く、 Reference\/Marginalize は安全側に Mean とする
-- (Marginalize の真の周辺化は epred では未対応)。
epredHoldValue :: HoldAgg -> Text -> [Double] -> [(Text, Double)] -> Double
epredHoldValue hold nm vs override =
  case lookup nm override of
    Just v  -> v
    Nothing -> case hold of
      Mean        -> meanL vs
      Median      -> medianL vs
      Mode        -> medianL vs                       -- 数値連続に最頻は無意味 → 中央値で代替
      Reference   -> meanL vs
      Marginalize -> meanL vs
      Fixed fm    -> fromMaybe (meanL vs) (lookup nm fm)
  where
    meanL xs   = if null xs then 0 else sum xs / fromIntegral (length xs)
    medianL xs = case xs of
      [] -> 0
      _  -> let s = sortBy compare xs
                k = length s
            in if even k
                 then (s !! (k `div` 2 - 1) + s !! (k `div` 2)) / 2
                 else s !! (k `div` 2)

-- | grid 上の事後予測平均線 + HDI 帯を組む 'GridOpts' クロージャ ('epred' が設定)。
-- 'renderGridMulti' (頻度論 effect plot) と同型: 非軸予測子を 'goHoldAt' で固定し、
-- 'goByVar' があれば第2予測子の水準ごとに曲線を色分け重畳する。 各曲線は帯 (先) +
-- 線 (後)。 'goPredAt' 指定点は lineRange (区間) + scatter (事後平均) で重畳する。
-- 帯は非対称な HDI を lo/hi で忠実に描く。
renderEpred :: HBMModel -> Text -> Text -> GridOpts -> VisualSpec
renderEpred hbm predName muName opts =
  let (lo0, hi0) = epredPredRange hbm predName
      (lo, hi)   = fromMaybe (lo0, hi0) (goRange opts)
      n          = max 2 (goN opts)
      gxs        = linspace lo hi n
      level      = goLevel opts
      hold       = goHoldAt opts
      -- 非軸予測子 (= hbmData の predName 以外の slot) を HoldAgg + byVar override で固定。
      -- 応答列も含むが deterministic μ は応答に依存しないため無害。
      holdBinds override =
        [ (nm, epredHoldValue hold nm vs override)
        | (nm, vs) <- hbmData hbm, nm /= predName ]
      -- 1 曲線分 (override = byVar 固定の (名,値)、 mCol = 線/帯色)。
      -- BandMode で CI (μ HDI) / PI (観測ノイズ込み) / CIPI (入れ子) / なし を切替
      -- (頻度論 'renderGrid' と同型)。 PI 系は遅延ゆえ CI/Off では評価されない。
      oneCurve override mCol =
        let holds = holdBinds override
            rows  = map (epredAtHeld hbm holds predName muName level) gxs
            mu    = map fst rows
            ciLos = map (fst . snd) rows
            ciHis = map (snd . snd) rows
            piPairs = map (epredPIAtHeld hbm holds predName epredPISeed level) gxs
            piLos = map fst piPairs
            piHis = map snd piPairs
            lineL = layer (goLineDeco opts mCol n (line (inline gxs) (inline mu)))
            ciDeco = goBandDeco opts mCol
            -- 入れ子時の PI 帯は薄め (CI が内側で見えるように・頻度論と同じ既定)。
            piA    = maybe 0.10 (* 0.5) (goAlpha opts)
            piDeco = goBandDeco (opts { goAlpha = Just piA }) mCol
            mkBand deco los his = layer (deco (band (inline gxs) (inline los) (inline his)))
        in case goBandMode opts of
             BandOff  -> lineL
             BandCI   -> mkBand ciDeco ciLos ciHis <> lineL
             BandPI   -> mkBand ciDeco piLos piHis <> lineL
             BandCIPI -> mkBand piDeco piLos piHis        -- 外: PI 薄 (下)
                      <> mkBand ciDeco ciLos ciHis        -- 内: CI 濃 (上)
                      <> lineL
      curves = case goByVar opts of
        Nothing         -> oneCurve [] Nothing
        Just (v2, vals) ->
          foldMap
            (\(i, val) ->
               let col = fromHex (effectPalette !! (i `mod` length effectPalette))
               in oneCurve [(v2, val)] (Just col))
            (zip [0 :: Int ..] vals)
      pts = goPredAt opts
      predLayers
        | null pts  = mempty
        | otherwise =
            let prows = map (epredAtHeld hbm (holdBinds []) predName muName level) pts
                pmu   = map fst prows
                mids  = map (\(_, (l, h)) -> (l + h) / 2) prows
                halfs = map (\(_, (l, h)) -> (h - l) / 2) prows
            in layer (lineRange (inline pts) (inline mids) (inline halfs))
                 <> layer (scatter (inline pts) (inline pmu))
  in curves <> predLayers <> labelLegend opts

-- | 予測子列の観測範囲 (grid 既定範囲)。 bind 済みデータ ('hbmData') から引く。
epredPredRange :: HBMModel -> Text -> (Double, Double)
epredPredRange hbm predName =
  case lookup predName (hbmData hbm) of
    Just vs | not (null vs) -> (minimum vs, maximum vs)
    _                       -> (0, 1)

-- | HBM の事後予測平均 (E[y|x]) を grid 評価する 'ModelSpec' を作る。 既定は 94% HDI 帯
-- (ArviZ 流・帯 ON 焼き込み)、 grid 100 点、 範囲 = 予測子の観測 min/max。 @\<\>@ で
-- 'grid' / 'gridRange' / 'statLevel' / 'predAt' を合成できる (Phase 16 C1 と同綴り)。
--
-- @
-- noDf |>> toPlot (epred fit \"x\" \"mu\" \<\> grid 200 \<\> statLevel 0.9)
-- @
epred
  :: HBMModel
  -> Text   -- ^ 予測子の data 参照名。
  -> Text   -- ^ 平均の deterministic ノード名。
  -> ModelSpec
epred hbm predName muName = mempty
  { msRender = Just (renderEpred hbm predName muName)
  , msLevel  = Just 0.94          -- ArviZ 既定の 94% HDI (statLevel で上書き可)
  , msBandMode = Just BandCI      -- HDI 帯が epred の本体ゆえ既定で出す
  }

-- ===========================================================================
-- HBM の事後予測チェック — Phase 49 A4 (ppc = posterior predictive check)
--
-- 観測 y の分布に対して、 学習済モデルが再現する複製データ y_rep の分布を重ねる
-- (ArviZ @az.plot_ppc@ 相当)。 各 posterior draw について 'runObserveDists' で
-- observe ノードの分布 (= 観測ノイズ込みの予測分布) を取り出し、 'sampleDist' で
-- 1 セット y_rep をサンプリングする。 これを N draw 分重ねると「観測がモデルの予測
-- 分布の典型から外れていないか」 を目視できる。
--
-- 描画 (ArviZ 流):
--   * 観測 density (濃色・実線)            … 実データ。
--   * y_rep density を N 本 (薄色・低 alpha) … 各 draw の複製データ。
--   * プール y_rep density (破線)          … 事後予測分布全体 (= ppc の中心)。
--
-- ★ サンプリングに RNG が要るため 'IO' (頻度論 toPlot や epred/forest と違い純粋に
-- できない)。 'hbmModel' 自体 IO なので非対称ではない。 cumulative 版は density を
-- 'ecdf' に差し替える ('ppcCumulative')。
-- ===========================================================================

-- | ppc の設定: 重ねる複製データ本数 ('ppcReps')、 乱数シード、 累積版 (ecdf) 切替。
data PPCConfig = PPCConfig
  { ppcReps       :: !Int            -- ^ 重ねる y_rep 本数 (既定 40・draw から等間隔抽出)。
  , ppcSeed       :: !(Maybe Word32) -- ^ サンプリングのシード (Nothing = system)。
  , ppcCumulative :: !Bool           -- ^ True で density を ecdf (累積分布) に差し替える。
  } deriving (Show, Eq)

-- | 既定 ppc 設定: y_rep 40 本・system 乱数・density 表示。
defaultPPC :: PPCConfig
defaultPPC = PPCConfig { ppcReps = 40, ppcSeed = Nothing, ppcCumulative = False }

-- | 事後予測チェック plot の描画仕様 ('forestOf' 等と同型の薄い newtype)。
newtype PPCSpec = PPCSpec { unPPCSpec :: VisualSpec }

instance Plottable PPCSpec where
  toPlot = unPPCSpec

-- | observe ノード名が prefix に一致するか。 単一 @observe \"obs\"@ (n == prefix) と
-- 'observeColumns' 由来の @\"obs_0\"@.. (prefix <> \"_\" が接頭辞) の両方を拾う。
ppcMatches :: Text -> Text -> Bool
ppcMatches prefix n = n == prefix || (prefix <> "_") `T.isPrefixOf` n

-- | 1 draw 分の複製データ y_rep をサンプリングする。 prefix 一致の各 observe ノードの
-- 分布から、 観測値と同数だけ引いてプールする。 Phase 50: 'PrimMonad' に一般化
-- (IO でも ST でも引ける → 純粋な 'ppcOf' が runST で決定的にサンプリングできる)。
sampleYRep :: PrimMonad m
           => Gen (PrimState m) -> ModelP () -> Text -> Map.Map Text Double -> m [Double]
sampleYRep gen spec prefix ps =
  let nodes = [ (d, ys) | (n, d, ys) <- runObserveDists spec ps, ppcMatches prefix n ]
  in concat <$> mapM (\(d, ys) -> sampleObsRep gen d ys) nodes

-- | 観測値 (prefix 一致 observe ノードの ys をプール)。 params に依らないので任意 draw から。
ppcObserved :: HBMModel -> Text -> [Double]
ppcObserved hbm prefix =
  case concatMap chainSamples (hbmChainsR hbm) of
    (p0:_) -> concat [ ys | (n, _, ys) <- runObserveDists (hbmModelSpec hbm) p0
                          , ppcMatches prefix n ]
    []     -> []

-- | ppc の対象 draw 群 ('ppcReps' 本に間引き)。
ppcDrawsFor :: PPCConfig -> HBMModel -> [Map.Map Text Double]
ppcDrawsFor cfg hbm = selectEvenly (ppcReps cfg) (concatMap chainSamples (hbmChainsR hbm))

-- | 観測値・y_rep 群から ppc plot を組む (純粋)。 薄い y_rep 群 (背景・各 draw) を先に、
-- 観測 (濃) を上に重ねる。 純粋 'ppcOfWith' と IO 'ppcOfWithIO' で共有。
--
-- ★ 旧実装はプール y_rep (全 draw 連結) の密度を赤破線で重ねていたが、 KDE の Silverman
-- バンド幅が **n 依存** (@h ∝ n^(-0.2)@) ゆえ、 n=Σ(draw×n_obs) のプールは観測 (n=n_obs) より
-- バンド幅が小さく過小平滑になり、 観測と異なる形 (外側へ膨らむ) に見えて誤解を招いた。
-- 比較は観測 (黒) vs 各 draw の y_rep (青・同じ n) で行うべきなので、 プール線は削除した
-- (ArviZ @plot_ppc@ もプール KDE は描かない)。
buildPPCSpec :: PPCConfig -> [Double] -> [[Double]] -> PPCSpec
buildPPCSpec cfg observed yreps =
  let densLayer = if ppcCumulative cfg then ecdf else density
      repLayers = foldMap
        (\yr -> layer (densLayer (inline yr) <> color (fromHex "#1f77b4") <> alpha 0.15))
        yreps
      obsLayer    = layer (densLayer (inline observed) <> color (fromHex "#000000"))
  in PPCSpec (repLayers <> obsLayer)

-- | draw 列から 'ppcReps' 本を等間隔で抽出する (本数以下ならそのまま)。
selectEvenly :: Int -> [a] -> [a]
selectEvenly k xs
  | k <= 0 || n <= k = xs
  | otherwise        = [ xs !! (i * n `div` k) | i <- [0 .. k - 1] ]
  where n = length xs

-- | 既定設定の事後予測チェック (純粋・決定的が**正本**。 'ppcOfWith' 'defaultPPC')。
-- y_rep サンプリングを @runST@ で閉じ、 @ppcSeed@ 既定 (42) で常に再現可能。 IO 版は 'ppcOfIO'。
ppcOf :: HBMModel -> Text -> PPCSpec
ppcOf = ppcOfWith defaultPPC

-- | 事後予測チェックを組む (純粋・正本)。 @prefix@ は observe ノード名 (@observeColumns@ なら接頭辞)。
-- y_rep サンプリングを @runST@ で閉じる。 @ppcSeed@ が 'Nothing' のときは固定既定 seed (42) で再現可能。
ppcOfWith :: PPCConfig -> HBMModel -> Text -> PPCSpec
ppcOfWith cfg hbm prefix =
  let spec :: ModelP ()
      spec  = hbmModelSpec hbm
      draws = ppcDrawsFor cfg hbm
      seed  = fromMaybe 42 (ppcSeed cfg)
      yreps = runST $ do
        gen <- initialize (V.singleton seed)
        mapM (sampleYRep gen spec prefix) draws
  in buildPPCSpec cfg (ppcObserved hbm prefix) yreps

-- | 既定設定の事後予測チェック (IO 版・'ppcOfWithIO' 'defaultPPC')。 通常は純粋な 'ppcOf' を使う
-- (将来 deprecate 予定)。 @ppcSeed@ 'Nothing' でシステム乱数を引きたいときだけ IO 版が要る。
ppcOfIO :: HBMModel -> Text -> IO PPCSpec
ppcOfIO = ppcOfWithIO defaultPPC

-- | 事後予測チェックを組む (IO 版)。 @ppcSeed@ 'Nothing' で 'createSystemRandom' を引く。
ppcOfWithIO :: PPCConfig -> HBMModel -> Text -> IO PPCSpec
ppcOfWithIO cfg hbm prefix = do
  let spec :: ModelP ()
      spec  = hbmModelSpec hbm
      draws = ppcDrawsFor cfg hbm
  gen <- case ppcSeed cfg of
           Nothing -> createSystemRandom
           Just w  -> initialize (V.singleton w)
  yreps <- mapM (sampleYRep gen spec prefix) draws
  pure $ buildPPCSpec cfg (ppcObserved hbm prefix) yreps

-- ===========================================================================
-- HBM 診断ダッシュボード — 複数の抽出子を 1 枚に束ねる便宜関数 (Phase 74.8)
--
-- 個別の抽出子 (dagOf / forestOf / ppcOf / energyOf / tracesOf / marginalsOf) を
-- 'subplots' で並べ「構造・推定・当てはまり・収束を一目で点検する」 パネル束にする。 2 種:
--   * 'dashboardOf'     … コンパクト 2×2 (構造 / 推定値 / 当てはまり / サンプラ健全性)。
--                          各 1 パネルゆえ param 数に依らず一定で見やすい。
--   * 'dashboardFullOf' … 上段に同じ 2×2、 その下に param ごと [事後分布 | trace] を 2 列で
--                          連結 (ArviZ @plot_trace@ 流)。 係数が増えると下へ行が増えるだけ。
-- どちらも observe ノード名を引数に取る (ppc 用)。 @noDf |>> dashboardOf m "obs"@。
-- autocorr/rank はダッシュボードに入れない (mixing は trace・BFMI は energy で見えるため。
-- ESS 定量は個別 'autocorrOf'、 chain 一様性は 'rankOf' で見る)。
-- ===========================================================================

-- | コンパクト健全性 2×2 のパネル群 (左上から 構造 / 推定値 / 当てはまり / サンプラ健全性)。
-- 'dashboardOf' (単体) と 'dashboardFullOf' (上段) で共有する内部ヘルパ。
dashboardHealthPanels :: HBMModel -> Text -> [VisualSpec]
dashboardHealthPanels hbm obsName =
  [ toPlot (dagOf hbm)         <> title "構造 (DAG)"
  , toPlot (forestOf hbm)      <> title "推定値 (forest 94% HDI)"
  , toPlot (ppcOf hbm obsName) <> title "当てはまり (PPC: 観測 vs 事後予測)"
  , energyOf hbm               <> title "サンプラ健全性 (energy / BFMI)" ]

-- | コンパクトな HBM 診断ダッシュボード (2×2)。 **構造** ('dagOf'・左上)・**推定値**
-- ('forestOf'・94% HDI)・**当てはまり** ('ppcOf'・観測 vs 事後予測の密度重ね)・**サンプラ
-- 健全性** ('energyOf'・BFMI) を 1 パネルずつ。 各 1 パネルゆえ param 数に依らず見やすい
-- (係数が増えても forest が縦に密になるだけ。 収束 R̂/trace は 'dashboardFullOf' で見る)。
dashboardOf :: HBMModel -> Text -> VisualSpec
dashboardOf hbm obsName =
  subplots (dashboardHealthPanels hbm obsName)
    <> subplotCols 2 <> width 1100 <> height 760

-- | param ごと **[事後分布 (左) | trace (右)]** のパネル群 (ArviZ @plot_trace@ の中身)。
-- 'traceDensityOf' (単体) と 'dashboardFullOf' (下段) で共有する内部ヘルパ。 事後分布・
-- trace とも chain 別を色違いで重畳する ('marginalsByChainOf' / 'tracesOfWith' byChain)。
tracePostPanels :: HBMModel -> [VisualSpec]
tracePostPanels hbm =
  concat (zipWith (\p t -> [p, t])
            (marginalsByChainOf hbm)
            (tracesOfWith defaultTraceOpts { toByChain = True } hbm))

-- | trace と事後分布だけのダッシュボード (= ArviZ @plot_trace@ 相当)。 param ごとに
-- **[事後分布 (左) | trace (右)]** を 2 列で並べる (chain は色違いで重畳)。 収束 (定常・
-- chain 一致) と事後の形を同時に確認する定番。 係数が増えると下に行が増える。
traceDensityOf :: HBMModel -> VisualSpec
traceDensityOf hbm =
  let np = max 1 (length (hbmParamNames hbm))
  in subplots (tracePostPanels hbm)
       <> subplotCols 2 <> width 900 <> height (180 * fromIntegral np)

-- | フルの HBM 診断ダッシュボード。 上段に 'dashboardOf' と同じ健全性 2×2、 その下に
-- param ごと **[事後分布 (左) | trace (右)]** を 2 列で連結する (ArviZ @plot_trace@ 流・
-- chain は色違いで重畳)。 全体が 1 つの 2 列グリッドなので、 **係数が増えると下に行が
-- 増えるだけ** (高さを行数 = 2 + param 数 に比例させ各パネルを潰さない)。 epred (予測曲線)
-- はモデル固有の予測子/平均ノード名と df が要るためここには含めない (個別に描く)。
dashboardFullOf :: HBMModel -> Text -> VisualSpec
dashboardFullOf hbm obsName =
  let np   = max 1 (length (hbmParamNames hbm))
      rows = 2 + np                                  -- 健全性 2 行 + param 行
  in subplots (dashboardHealthPanels hbm obsName ++ tracePostPanels hbm)
       <> subplotCols 2 <> width 1100 <> height (220 * fromIntegral rows)

-- ===========================================================================
-- HBM のモデル構造 DAG — Phase 49 A5 (dag = 確率プログラムの依存グラフ)
--
-- 確率プログラム ('ModelP') の依存構造を 'buildModelGraph' (= 'extractDeps' +
-- 同名ノード統合) で 'ModelGraph' (nodes / edges / plates) にし、 plot-core の
-- DAG 描画 ('dagFromListsWithPlates'、 Sugiyama 階層 layout) に橋渡しする。 PyMC の
-- @pm.model_to_graphviz@ に相当する「モデルの絵」。
--
-- ノード種 (latent / observed) と分布名は 'Node' のメタデータをそのまま 'DAGNode' に
-- 写す。 plate ('plate' で囲んだ繰り返し) は 'mgPlates' を 'DAGPlate' に変換する
-- (plate メンバは 'nodePlates' から逆引き)。 plate を使わないモデルでは
-- 'observeColumns' 由来の @obs_0..@ が個別ノードとして出る (collapse したい場合は
-- モデル側を 'plate' で囲む)。
-- ===========================================================================

-- | モデル構造 DAG の描画仕様 ('forestOf' 等と同型の薄い newtype)。
newtype DagSpec = DagSpec { unDagSpec :: VisualSpec }

instance Plottable DagSpec where
  toPlot = unDagSpec

-- | 学習済モデルの構造を DAG にする ('buildModelGraph' → plate-collapse →
-- plot-core DAG)。 layout は階層 ('LayoutHierarchical')。 学習結果には依存しない
-- (構造のみ)。 Phase 59.3: plate 内の indexed RV (@b0_0..b0_2@ 等) を
-- 'collapseIndexedPlateNodes' で 1 ノードに畳むのが既定 (PyMC
-- @model_to_graphviz@ と同じ見た目)。 indexed 個別ノードのまま見たい場合は
-- 'dagOfRaw'。
dagOf :: HBMModel -> DagSpec
dagOf = dagFromModelGraph . collapseIndexedPlateNodes . buildModelGraph . hbmModelSpec

-- | 'dagOf' の plate-collapse 無し版 (Phase 49-59.2 の旧既定。 plate 内 indexed RV を
-- 個別ノードで列挙する。 展開後の全ノード/エッジを確認するデバッグ用)。
dagOfRaw :: HBMModel -> DagSpec
dagOfRaw = dagFromModelGraph . buildModelGraph . hbmModelSpec

-- | **学習前**にモデル構造だけを DAG にする (PyMC @pm.model_to_graphviz@ 相当。 Phase 74.9)。
-- 'dagOf' が学習済 'HBMModel' を取るのに対し、 こちらは生の 'ModelP' を直接取り
-- **サンプリングを一切しない** (構造は事後に依らないため)。 @noDf |>> toPlot (dagOfModel m)@。
--
-- ★ 注意: データ駆動 plate (@plateForM_@ / @observeColumns@ で plate サイズを **データ長**から
-- 決めるモデル) は、 データ未束縛 (slot が @[]@) だとループ本体が回らず plate 内ノード
-- (mu / obs 等) が出ない。 その場合は 'dagOfModelWith' でダミーでないデータを束ねてから描く
-- (サンプリングは走らない)。 明示 plate (@plate name N@ / @plateI@ で N を直書き) のモデルは
-- データ無しでも構造が完全に出る。
dagOfModel :: ModelP () -> DagSpec
dagOfModel = dagFromModelGraph . collapseIndexedPlateNodes . buildModelGraph

-- | 'dagOfModel' のデータ束ね版 (PyMC で観測を渡してから @model_to_graphviz@ する形)。
-- @dat@ を 'bindCols' でモデルへ束ねてから DAG を組む = **データ駆動 plate のサイズが
-- 正しく出る**。 'hbmModel' と同じ束ね方だが **NUTS は走らない** (学習前のプレビュー)。
-- @noDf |>> toPlot (dagOfModelWith [("x", xs), ("y", ys)] m)@。
dagOfModelWith :: [(Text, [Double])] -> ModelP () -> DagSpec
dagOfModelWith dat = dagOfModel . bindCols dat

-- | 'ModelGraph' → plot-core DAG 描画仕様 ('dagOf' / 'dagOfRaw' の共通部)。
dagFromModelGraph :: ModelGraph -> DagSpec
dagFromModelGraph mg =
  -- ★ renderDAG は dnX/dnY をそのまま使い layout を実行しない。 ゆえに描画前に
  -- Sugiyama 階層 layout ('layoutHierarchicalFullWithPlates') で座標を確定させる
  -- (これを省くと全ノードが原点 (0,0) に重なる)。
  let (positioned, routed) = layoutHierarchicalFullWithPlates dnodes dedges dplates
  -- ★ HS=PS parity: routing を spec へ焼き込む (= 'deRoute' 充填)。 これが無いと PS canvas
  --   は 'deRoute = Nothing' で直線フォールバックになり、 HS の live routing (曲線) と乖離する。
  --   baking は area 非依存 (dagToScreen が 0..1 domain を正規化 pt 空間へ map・描画時に
  --   fitPrimsToArea で affine fit) なので layout 直後のここで焼ける。
  in DagSpec $ bakeDAGRoutesInSpec $
       layer (dagFromListsWithPlates positioned routed LayoutHierarchical dplates)
  where
    ns = mgNodes mg
    dnodes = map toDNode ns
    dedges = [ DAGEdge { deFrom = p, deTo = c, dePath = Nothing, deRoute = Nothing }
             | (p, c) <- mgEdges mg ]
    dplates = [ DAGPlate
                  { dpLabel   = nm <> " (" <> T.pack (show sz) <> ")"
                  , dpNodeIds = [ nodeName n | n <- ns, nm `elem` nodePlates n ] }
              | (nm, sz) <- Map.toList (mgPlates mg) ]
    toDNode n = DAGNode
      { dnId    = nodeName n
      , dnLabel = nodeName n
      , dnKind  = case nodeKind n of
                    LatentN        -> NodeLatent
                    ObservedN _    -> NodeObserved
                    DeterministicN -> NodeDeterministic
                    -- Phase 60.4: NodeData は plot-core に既実装 (Phase 26 §E-6)
                    DataN _        -> NodeData
      , dnDist  = Just (nodeDist n)
      , dnX     = 0
      , dnY     = 0
      }

-- | random-effect 第 @k@ 列の caterpillar plot。 BLUP を group ごとに取り、
-- **値で昇順ソート**して forest mark (errs=0 の点) で並べ、 0 に 'forestNull' 参照線。
caterpillarColumn :: GLMMResultRE -> Int -> VisualSpec
caterpillarColumn res k =
  let cols   = LA.toColumns (reBLUPs res)
      blups  = if k >= 0 && k < length cols then LA.toList (cols !! k) else []
      groups = V.toList (reGroups res)
      sorted = sortBy (comparing snd) (zip groups blups)
      gs     = map fst sorted
      es     = map snd sorted
      zeros  = map (const (0 :: Double)) es
  in layer (forest (inlineCat gs) (inline es) (inline zeros) <> forestNull 0)
       <> title ("Random effects (col " <> T.pack (show k) <> ")")

instance Plottable GLMMResultRE where
  -- 代表 1 枚 = 第 1 列 (通常 random intercept) の caterpillar。
  toPlot res = caterpillarColumn res 0
  -- 診断束 = 全 r 列 (intercept + slope) の caterpillar。
  diagnosticPlots res =
    [ caterpillarColumn res k | k <- [0 .. LA.cols (reBLUPs res) - 1] ]

-- | HBM の事後予測平均 (epred) 応答曲面。 2 つの予測子 slot (@p1@, @p2@) を
--   grid で動かし、 各点で deterministic @muName@ の事後平均を取る
--   ('epredAt' の 2 変数版・O1 規約は 'renderEpred' の節を参照)。
--   ★コスト = grid 点数² × 全 draw のモデル評価。 既定 n=30 (900 点)。
epredSurfaceOf :: HBMModel -> Text -> Text -> Text -> P3.VisualSpec3D
epredSurfaceOf hbm p1 p2 muName =
  epredSurfaceOfWith hbm p1 p2 muName defaultSurfaceOpts { soN = 30 }

epredSurfaceOfWith :: HBMModel -> Text -> Text -> Text -> SurfaceOpts -> P3.VisualSpec3D
epredSurfaceOfWith hbm p1 p2 muName opts =
  let (xlo, xhi) = fromMaybe (epredPredRange hbm p1) (soXRange opts)
      (ylo, yhi) = fromMaybe (epredPredRange hbm p2) (soYRange opts)
      n     = max 2 (soN opts)
      gxs   = linspace xlo xhi n
      gys   = linspace ylo yhi n
      draws = concatMap chainSamples (hbmChainsR hbm)
      muAt gx gy =
        let bound :: ModelP ()
            bound = withData p1 [gx] (withData p2 [gy] (hbmModelSpec hbm))
            mus   = [ v | ps <- draws
                        , Just v <- [Map.lookup muName (runDeterministics bound ps)] ]
        in if null mus then 0 else sum mus / fromIntegral (length mus)
      grid = [ [ muAt gx gy | gx <- gxs ] | gy <- gys ]
  in P3.layer3D ( P3.surface3DGrid grid
               <> P3.xRange3D (xlo, xhi)
               <> P3.yRange3D (ylo, yhi)
               <> P3.colormap3D )

-- | 学習済 HBM が保持するデータ列 ('hbmData') から散布図層を作る (B10)。
--
-- @df |-> hbm cfg model@ で学習した後、 @dataScatterOf m \"x\" \"y\"@ で
-- 観測散布図を出せるので、 epred\/forest 等の抽出子と重畳するとき
-- **df を学習時 1 回だけ**書けばよい:
--
-- > let m = df |-> hbm defaultHBM model
-- > noDf |>> (dataScatterOf m "x" "y" <> toPlot (epred m "x" "mu"))
dataScatterOf :: HBMModel -> Text -> Text -> VisualSpec
dataScatterOf m xn yn =
  case (lookup xn (hbmData m), lookup yn (hbmData m)) of
    (Just xs, Just ys) -> layer (scatter (inline xs) (inline ys))
    _                  -> mempty
