-- | hgg 統合 build root (= hanalyze-plot package を含む)。
-- |
-- | デフォルトの cabal.project は standalone (plot 非依存・upstream portable) のまま
-- | 据え置き、 plot 連携 (Hanalyze.Plot = toPlot/Plottable) を build/test するときだけ
-- | こちらを使う:
-- |
-- |   cabal build --project-file=cabal.project.plot hanalyze
-- |
-- | 依存は一方向 analyze → plot-core/-svg (= plot コアは analyze 非依存)。 plot 側
-- | (plot.cabal.project) は analyze を flag off で build するため循環しない。
-- | 設計: plot Phase 15 / analyze Phase 46。

packages:
  hanalyze
  -- Phase 106: multi-package 化 (flat 7 package + CLI)。 hanalyze-plot は
  -- Fit/Wrappers を import するため umbrella の上に載る最上位 package (この root のみ)。
  hanalyze-core
  hanalyze-frame
  hanalyze-bayes
  hanalyze-models
  hanalyze-design
  hanalyze-viz
  hanalyze-plot
  hanalyze-cli

-- hanalyze-plot は sibling の hgg packages (hgg-core/-frame/-svg/-pdf/
-- -rasterific/-3d/-custom) に依存する。 hgg が Hackage に公開されるまでは
-- checkout を cabal.project.plot.local (untracked) で指す:
--
--   packages:
--     ../hgg/hgg-core
--     ../hgg/hgg-frame
--     ../hgg/hgg-svg
--     ../hgg/hgg-pdf
--     ../hgg/hgg-rasterific
--     ../hgg/hgg-3d
--     ../hgg/hgg-custom

-- Phase 106: plot 連携 lib は package hanalyze-plot として無条件 build になった
-- (flag plot-integration は 106.4 で全 user 消滅につき撤去済)。
-- plot 依存の demo/bench exe 群は hanalyze-demos へ移設
-- (opt-in build = cabal.project.demos 経由。 この root の build all には含めない)。

tests: True
