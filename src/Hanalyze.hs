-- |
-- Module      : Hanalyze
-- Description : モデル fit・統計・可視化・CSV I/O をまとめた quickstart 用 umbrella module
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Hanalyze の quickstart 出入口 (umbrella module)。
--
-- 最初に触れる中核 (モデル fit・基本統計・可視化・CSV I/O) を 1 つの
-- @import Hanalyze@ で揃える窓口。 個別機能は各サブモジュール
-- (@Hanalyze.Model.*@ / @Hanalyze.Stat.*@ / @Hanalyze.Viz.*@) を直接 import する。
--
-- 方針 (Phase 46 / plot Phase 15 §A5):
--   * ここは **plot 非依存・portable** (re-export のみ、 flag 不要)。
--     plot 連携 (@toPlot@ / @Plottable@) は @flag plot-integration@ 配下の
--     @Hanalyze.Plot@ に分離してあり、 本 umbrella には含めない。
--   * Formula DSL は本 Phase 対象外 (ロードマップ B 段)。
module Hanalyze
  ( -- * モデル fit の共有核 / 能力別 protocol
    module Hanalyze.Model.Core
    -- * 線形 / 一般化線形モデル
  , module Hanalyze.Model.LM
  , module Hanalyze.Model.GLM
    -- * 記述統計・検定・効果量・分布
  , module Hanalyze.Stat.Summary
  , module Hanalyze.Stat.Test
  , module Hanalyze.Stat.Effect
  , module Hanalyze.Stat.Distribution
    -- * 可視化 (散布図 / 棒 / ヒストグラム)
  , module Hanalyze.Viz.Core
  , module Hanalyze.Viz.Scatter
  , module Hanalyze.Viz.Bar
  , module Hanalyze.Viz.Histogram
    -- * データ入力
  , module Hanalyze.DataIO.CSV
    -- * HBM 事後要約 (Phase 103: fit 結果 → 要約表 / DataFrame の一発 API)
  , hbmSummaryNames
  , hbmSummary
  , printHBMSummary
  , hbmSummaryDf
  , hbmDrawsDf
  ) where

-- ===

import Hanalyze.Model.Core
import Hanalyze.Model.LM
import Hanalyze.Model.GLM
import Hanalyze.Stat.Summary
import Hanalyze.Stat.Test
import Hanalyze.Stat.Effect
-- Binomial / Poisson は GLM の 'Family' と名前衝突するため、 umbrella では
-- GLM 側を優先 (quickstart は @fitGLM Poisson ...@ が典型)。 'Distribution' の
-- 同名コンストラクタが要る場合は @Hanalyze.Stat.Distribution@ を直接 import する。
import Hanalyze.Stat.Distribution hiding (Binomial, Poisson)
import Hanalyze.Viz.Core
import Hanalyze.Viz.Scatter
import Hanalyze.Viz.Bar
import Hanalyze.Viz.Histogram
import Hanalyze.DataIO.CSV
import Hanalyze.Model.Wrappers (hbmSummaryNames, hbmSummary, printHBMSummary,
                                       hbmSummaryDf, hbmDrawsDf)
