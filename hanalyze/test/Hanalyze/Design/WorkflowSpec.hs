{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Phase 78.A/B: DOE ワークフロー (設計オブジェクト + runsheet + designModel)。
--
-- 大半は standalone (flag plot-integration off・upstream portable) で build/run できるが、
-- 一部の診断テスト (tracesOf / MultiVarModel の事後予測帯) は plot 連携層
-- (Hanalyze.Plot.*) に依存するため @PLOT_INTEGRATION@ CPP で囲む
-- (= flag plot-integration on のときだけ compile)。
module Hanalyze.Design.WorkflowSpec (spec) where

import qualified Data.Text as T
import           Data.List (findIndex, isInfixOf, nub)
import           Data.Maybe (isJust)
import           Control.Exception (evaluate, ErrorCall (..))
import           System.CPUTime (getCPUTime)
import           System.IO (hPutStrLn, stderr)
import           System.IO.Temp (withSystemTempDirectory)
import qualified Numeric.LinearAlgebra as LA
import qualified Data.Vector as V
import qualified DataFrame.Internal.Column    as DX
import qualified DataFrame.Internal.DataFrame  as DX
import           Test.Hspec
import           Hanalyze.Design.Workflow
import           Hanalyze.Fit             (designModel, designModelGP, defaultGP, gpMulti, GPConfig (..), (|->), ranIntercept, ranSlope, designHBMProgram, designModelHBM, DesignHBMFit (..), multiOutput, modelFor)
#ifdef PLOT_INTEGRATION
import           Hanalyze.Plot.Bayes      (tracesOf)
#endif
import           Hanalyze.Model.HBM       (sampleNames, ModelP)
import           Hanalyze.Model.Wrappers  (MultiLMModel (..), GPRegModelN (..), GPMethod (..), HyperStrategy (..), defaultHBM, HBMConfig (..))
#ifdef PLOT_INTEGRATION
-- ModelFrame/VarRole は MultiVarModel 事後予測帯テスト (guarded) 専用ゆえ一緒に囲む。
import           Hanalyze.Model.Formula.Frame (ModelFrame (..), VarRole (..))
import           Hanalyze.Plot.Core       (MultiVarModel (..))
import           Hanalyze.Plot.ML         ()  -- instance MultiVarModel DesignHBMFit
#endif
import           Hanalyze.Model.GP        (GPParams (..), Kernel (..))
import           Hanalyze.Model.Core      (rSquared1)
import           Hanalyze.Model.Formula.RFormula (parseRFormula)
import           Hanalyze.Model.Formula.Mixed (RandomSpec (..))
import qualified Hanalyze.Design.Custom.Model as CMd
import qualified Hanalyze.Design.Custom.Factor as CF
import qualified Hanalyze.Design.Custom.Structured as ST
import qualified Data.Vector.Storable as VS
import           Hanalyze.DataIO.Convert (getDoubleVec)

spec :: Spec
spec = do
  describe "Design.Workflow (Phase 78.A/B)" $ do
    let planF = factorialDesign [contFactor "temp" (150, 180), contFactor "time" (10, 20)]

    it "factorialDesign: formula = 全交互作用" $
      designFormula planF "y" `shouldBe` "y ~ temp * time"

    it "factorialDesign: runsheet は 2^k row・uncoded 実値・run 番号列" $ do
      let rs = designTable planF
      lookup "temp" rs `shouldBe` Just [150, 150, 180, 180]
      lookup "time" rs `shouldBe` Just [10, 20, 10, 20]
      lookup "run"  rs `shouldBe` Just [1, 2, 3, 4]
      designFactorNames planF `shouldBe` ["temp", "time"]

    it "centralCompositeDesign: formula = 2 次モデル (交互作用 + 平方項)" $
      designFormula (centralCompositeDesign [contFactor "x1" (150, 180), contFactor "x2" (10, 20)]) "y"
        `shouldBe` "y ~ x1 + x2 + x1:x2 + I(x1^2) + I(x2^2)"

    it "centralCompositeDesign: CCD (factorial + 軸点 + 中心点) の run 数 > 2^k" $ do
      let rs = designTable (centralCompositeDesign [contFactor "x1" (0, 1), contFactor "x2" (0, 1)])
      length (maybe [] id (lookup "x1" rs)) `shouldSatisfy` (> 4)   -- 2^2=4 より多い

    it "designModel: 真モデル由来データを飽和当てはめ (R^2 ~ 1)" $ do
      let rs    = designTable planF
          temps = maybe [] id (lookup "temp" rs)
          times = maybe [] id (lookup "time" rs)
          ys    = zipWith (\t d -> 5 + 2 * t + 3 * d + 0.01 * t * d) temps times
          m     = (("y", ys) : rs) |-> designModel planF "y"
      -- 2^2 factorial × 交互作用モデル = 飽和 (4 param / 4 点) → 完全再現
      rSquared1 (mlmResult m) `shouldSatisfy` (> 0.999)

    -- Phase 78.G-e: FRR/HBM 化 (v1 = GP/RFF)。designModel の非 LM 版。
    it "designModelGP: 連続因子で GP を当てはめ・予測子名を plan から保持" $ do
      let rs    = designTable planF
          temps = maybe [] id (lookup "temp" rs)
          times = maybe [] id (lookup "time" rs)
          ys    = zipWith (\t d -> 5 + 2 * t + 3 * d) temps times
          m     = (("y", ys) : rs) |-> designModelGP defaultGP planF "y"
      gprnNames m `shouldBe` ["temp", "time"]

    it "designModelGP: カテゴリ因子混在は error (連続専用)" $ do
      let planMix = factorialDesign [contFactor "temp" (150, 180), catFactor "cat" ["a", "b"]]
          df      = [("y", [1, 2, 3, 4]), ("temp", [150, 150, 180, 180])] :: [(T.Text, [Double])]
      evaluate (gprnNames (df |-> designModelGP defaultGP planMix "y")) `shouldThrow` anyErrorCall

    -- GP/GpRff 象限は帯 (事後予測分散) を出す。 Krr/KrrRff (mean-only) は帯なし。
    it "designModelGP: GpRff 象限は帯 (予測分散 Just) を出す = RFF 開通" $ do
      let rs     = designTable planF
          temps  = maybe [] id (lookup "temp" rs)
          times  = maybe [] id (lookup "time" rs)
          ys     = zipWith (\t d -> 5 + 2 * t + 3 * d) temps times
          cfgRff = GPConfig RBF (GpRff 128 7) AutoMarginalLik
          mRff   = (("y", ys) : rs) |-> designModelGP cfgRff planF "y"
      snd (gprnPredict mRff (LA.fromLists [[165, 15]])) `shouldSatisfy` isJust

    it "designModelGP: Krr 象限は帯なし (mean-only・var=Nothing)" $ do
      let rs     = designTable planF
          temps  = maybe [] id (lookup "temp" rs)
          times  = maybe [] id (lookup "time" rs)
          ys     = zipWith (\t d -> 5 + 2 * t + 3 * d) temps times
          cfgKrr = GPConfig RBF Krr AutoMarginalLik
          mKrr   = (("y", ys) : rs) |-> designModelGP cfgKrr planF "y"
      snd (gprnPredict mKrr (LA.fromLists [[165, 15]])) `shouldBe` Nothing

    -- Phase 78.G-e 回帰: GP のハイパラ自動調整 (AutoMarginalLik) が退化しないこと。
    -- かつて LBFGS の初回ステップが未スケール最急降下方向に α=1 を打ち、勾配の
    -- 大きい GP 周辺尤度で巨大オーバーシュートして ℓ が真の峰 (≈105) を越え 1e12 に
    -- 飛び、予測が水平化していた (spread=0)。LBFGS.hs の初回 α₀=min(1,1/‖g‖₁) 修正で
    -- sklearn 一致の大域最適 (ℓ≈105・LML=-35.77) に到達する。ここでは既知の 2 次応答で
    -- 予測レンジ (spread) が真値に一致し ℓ が有限であることを担保する。
    it "GP (AutoMarginalLik) が退化せず既知応答を当てる (LBFGS 初回ステップ回帰)" $ do
      let respf t d = 50 + 0.6 * t - 0.02 * (t - 165) * (t - 165) + 1.5 * d
          noise  = cycle [0.6, -0.5, 0.4, -0.3, 0.2, -0.1]
          planG  = centralCompositeDesign [contFactor "temp" (150, 180), contFactor "time" (10, 20)]
          rs     = designTable planG
          temps  = maybe [] id (lookup "temp" rs)
          times  = maybe [] id (lookup "time" rs)
          ys     = zipWith3 (\t d e -> respf t d + e) temps times noise
          dat    = ("y", ys) : rs
          predAt m t d = head (fst (gprnPredict m (LA.fromLists [[t, d]])))
          trueSpread   = respf 180 15 - respf 150 15   -- = 18
          spreadOf m   = predAt m 180 15 - predAt m 150 15
          -- (a) 高レベル DOE 経路 (designModelGP)  (b) 汎用 gpMulti (uncoded 素通し)
          mDoe = dat |-> designModelGP defaultGP planG "y"
          mGp  = dat |-> gpMulti defaultGP ["temp", "time"] "y"
      -- 退化なら spread≈0。真値 18 に十分近い (±2) こと・ℓ が有限で妥当 (<1e6)。
      spreadOf mDoe `shouldSatisfy` (\s -> abs (s - trueSpread) < 2)
      spreadOf mGp  `shouldSatisfy` (\s -> abs (s - trueSpread) < 2)
      gpLengthScale (gprnParams mGp) `shouldSatisfy` (< 1e6)

    it "designModel: 同じ plan を別データ (sim/実物想定) に使い回せる" $ do
      let rs    = designTable planF
          temps = maybe [] id (lookup "temp" rs)
          times = maybe [] id (lookup "time" rs)
          ysA   = zipWith (\t d -> 1 + t + d) temps times          -- 「sim」
          ysB   = zipWith (\t d -> 9 + 2 * t - d) temps times      -- 「実物」
          mA    = (("y", ysA) : rs) |-> designModel planF "y"
          mB    = (("y", ysB) : rs) |-> designModel planF "y"
      rSquared1 (mlmResult mA) `shouldSatisfy` (> 0.99)
      rSquared1 (mlmResult mB) `shouldSatisfy` (> 0.99)

    -- Phase 78.G-f: 型付きランダム効果項 (designModelHBM 用の smart constructor)。
    it "ranIntercept は (1|g) 相当の RandomSpec" $
      ranIntercept "lot" `shouldBe` RandomSpec True [] "lot"
    it "ranSlope は intercept 込みの (1+s|g)" $
      ranSlope ["temp"] "lot" `shouldBe` RandomSpec True ["temp"] "lot"

    it "designHBMProgram は betaNames と群効果を含む ModelP を組む" $ do
      let x   = [[1,0],[1,1],[1,0],[1,1]]      -- intercept + temp
          bn  = ["b0","temp"]
          res = [([0,0,1,1], 2, [])]           -- lot: 行0,1=群0 / 行2,3=群1 (切片のみ)
          ys  = [1.0, 2.0, 1.1, 2.2]
          prog :: ModelP ()
          prog = designHBMProgram x bn res ys
          nms  = sampleNames prog
      nms `shouldContain` ["b0"]
      nms `shouldContain` ["temp"]
      nms `shouldContain` ["u_g0_0"]

    -- Phase 78.G-f: designModelHBM 本体 (df → designHBMProgram → hbm → 事後 draw)。
    -- temp∈{-1,1}・lot∈{A,B} の 2 群、 lot 差 (切片ずれ) を仕込んだ合成データで
    -- 固定効果係数 temp の事後平均が真値 3 に近く収束することを確認する。
    -- ('|->' は失敗を error に落とすので、 ここでは happy-path のみ検証する。)
    it "designModelHBM は lot 群込みで固定効果を回復する" $ do
      let mean0 xs = sum xs / fromIntegral (length xs)
          -- ★designMatrixF (R formula 経路) は列ラベルを合成パラメータ込みの項全体
          --   (例 "_p1 * temp") で付ける (Wrappers.hs の additiveFormula と同じ命名規約)。
          --   ここでは変数名の suffix 一致で該当列を探す。
          drawsFor nm m = case findIndex (T.isSuffixOf nm) (dhfBetaNames m) of
            Just i  -> map (!! i) (dhfBetaDraws m)
            Nothing -> []
          -- y = 2 + 3*temp + lotShift + small noise (固定値・シード不要)。
          temps  = [-1, 1, -1, 1, -1, 1, -1, 1, -1, 1, -1, 1] :: [Double]
          lots   = ["A","A","A","A","A","A","B","B","B","B","B","B"] :: [T.Text]
          noise  = [0.05, -0.03, 0.02, -0.04, 0.01, -0.02, 0.03, -0.01, 0.04, -0.05, 0.02, -0.03]
          lotShift l = if l == "A" then (-1.0) else 1.0
          ys     = [ 2 + 3 * t + lotShift l + e
                   | (t, l, e) <- zip3 temps lots noise ]
          mkFrameHBM = DX.insertColumn "lot"  (DX.fromList lots)
                     $ DX.insertColumn "temp" (DX.fromList temps)
                     $ DX.insertColumn "y"    (DX.fromList ys)
                     $ DX.empty
          plan = factorialDesign [contFactor "temp" (-1, 1)]
          m    = mkFrameHBM |-> designModelHBM defaultHBM plan [ranIntercept "lot"] "y"
          bTemp = mean0 (drawsFor "temp" m)
      bTemp `shouldSatisfy` (\b -> abs (b - 3) < 1.0)

#ifdef PLOT_INTEGRATION
    -- Phase 78.J: designModelHBM の学習済 HBM を dhfModel で露出し、 診断抽出子
    -- (tracesOf / dagOf 等) に渡せる (DesignHBMFit から診断が出せる)。
    -- ※ tracesOf は plot 連携層 (Hanalyze.Plot.Bayes)・plot-integration 限定。
    it "designModelHBM: dhfModel で診断 (tracesOf) が出せる" $ do
      let temps  = [-1, 1, -1, 1, -1, 1, -1, 1] :: [Double]
          lots   = ["A","A","A","A","B","B","B","B"] :: [T.Text]
          ys     = [ 2 + 3 * t + (if l == "A" then -1 else 1) | (t, l) <- zip temps lots ]
          df     = DX.insertColumn "lot"  (DX.fromList lots)
                 $ DX.insertColumn "temp" (DX.fromList temps)
                 $ DX.insertColumn "y"    (DX.fromList ys)
                 $ DX.empty
          plan   = factorialDesign [contFactor "temp" (-1, 1)]
          m      = df |-> designModelHBM defaultHBM plan [ranIntercept "lot"] "y"
      length (tracesOf (dhfModel m)) `shouldSatisfy` (> 0)   -- param ごとの trace が出る
#endif

    -- Phase 78.G-f 回帰: NA 行 drop と群 idx の行ずれ (code review 指摘)。
    -- 'modelFrame' (DropRows) は formula 関与列 (temp/y) に NA を含む行を落として
    -- designX/ys を作るが、 群 idx (prepRE) が raw df (NA 除去前・行数不変) から
    -- 作られていると、 designX/ys (post-drop・行数 -k) と 1:1 対応しなくなる。
    -- lot を A,A,B,B の 2 行ブロック周期にし、 4 行 (temp) を NA にすると
    -- 「raw 先頭 n 行を機械的に使う」 バグ実装では postDrop の並びに対して行ずれが
    -- 生じ、 複数箇所で A/B が入れ替わって群対応が壊れる (単純な A↔B 一括入替では
    -- 吸収できない不整合・実測: バグ版 bTemp ≈ 1.96 = |b-3| ≈ 1.04 > 1.0 で検出可)。
    -- lot 差 (±50、 通常の ±1 より大きめ) を仕込むことで群対応の誤りが固定効果
    -- temp の事後平均に十分な偏りとして現れるようにしてある。
    -- fix 後は "同じ post-drop 行集合" から群 idx を作るので、 NA 行があっても
    -- 固定効果 temp は真値 3 近辺へ回復する。
    it "designModelHBM: NA 行があっても群 idx が post-drop 行と正しく対応する (行ずれ回帰)" $ do
      let mean0 xs = sum xs / fromIntegral (length xs)
          drawsFor nm m = case findIndex (T.isSuffixOf nm) (dhfBetaNames m) of
            Just i  -> map (!! i) (dhfBetaDraws m)
            Nothing -> []
          -- lot は 2 行ブロック周期 (A,A,B,B の繰り返し) — 複数 NA drop で生じる
          -- 「先頭 n 行 raw vs post-drop」 のオフセットが単純な A/B 一括入替に
          -- 縮退しない配置 (境界が複数あるので relabeling で吸収できない)。
          n        = 16 :: Int
          lots     = take n (cycle ["A", "A", "B", "B"]) :: [T.Text]
          tempsRaw = take n (cycle [-1, 1]) :: [Double]
          noise    = take n (cycle [0.05, -0.03, 0.02, -0.04, 0.01, -0.02, 0.03, -0.01])
          lotShift l = if l == "A" then (-50.0) else 50.0
          ysRaw    = [ 2 + 3 * t + lotShift l + e
                     | (t, l, e) <- zip3 tempsRaw lots noise ]
          -- 行 0,2,4,6 (temp) を NA にする — dropMissingRows で 4 行落ちる。
          naIdxs   = [0, 2, 4, 6] :: [Int]
          tempsNA  = [ if i `elem` naIdxs then Nothing else Just t
                     | (i, t) <- zip [0 :: Int ..] tempsRaw ] :: [Maybe Double]
          mkFrameHBM = DX.insertColumn "lot"  (DX.fromList lots)
                     $ DX.insertColumn "temp" (DX.fromList tempsNA)
                     $ DX.insertColumn "y"    (DX.fromList ysRaw)
                     $ DX.empty
          plan  = factorialDesign [contFactor "temp" (-1, 1)]
          m     = mkFrameHBM |-> designModelHBM defaultHBM plan [ranIntercept "lot"] "y"
          bTemp = mean0 (drawsFor "temp" m)
      bTemp `shouldSatisfy` (\b -> abs (b - 3) < 1.0)

    -- Phase 78.G-f2: 相関ランダム傾き (1+temp|lot)。 lot ごとに temp の傾きが
    -- 異なるデータ (A の傾き 5・B の傾き 1・平均 3) を作る。 変量傾きを捉えられれば
    -- 残差 σ は仕込んだ noise 水準 (~0.05) まで縮む。 切片のみ (ranIntercept) では
    -- 傾き差 (±2·temp) を残差に押し込むため σ が ~2 に膨らむ = 判別可能。
    -- 固定効果 temp は両群平均の 3 近辺へ回復する。
    -- Phase 80.2b で相関 RE を非中心化 (a) vecIR 化 (funnel 撤去) したため再有効化。
    -- 非中心化パラメタ化で funnel が消え defaultHBM でも収束する。
    it "designModelHBM: 相関ランダム傾き (ranSlope) は群別の傾き差を捉え σ を縮める" $ do
      let mean0 xs = sum xs / fromIntegral (length xs)
          drawsFor nm mm = case findIndex (T.isSuffixOf nm) (dhfBetaNames mm) of
            Just i  -> map (!! i) (dhfBetaDraws mm)
            Nothing -> []
          temps  = [-1, 1, -1, 1, -1, 1, -1, 1, -1, 1, -1, 1] :: [Double]
          lots   = ["A","A","A","A","A","A","B","B","B","B","B","B"] :: [T.Text]
          noise  = [0.05, -0.03, 0.02, -0.04, 0.01, -0.02, 0.03, -0.01, 0.04, -0.05, 0.02, -0.03]
          -- lot A: y = 2 + 5·temp、 lot B: y = 2 + 1·temp (傾き差を仕込む)。
          slopeOf l = if l == "A" then 5.0 else 1.0
          ys     = [ 2 + slopeOf l * t + e
                   | (t, l, e) <- zip3 temps lots noise ]
          mkFrameHBM = DX.insertColumn "lot"  (DX.fromList lots)
                     $ DX.insertColumn "temp" (DX.fromList temps)
                     $ DX.insertColumn "y"    (DX.fromList ys)
                     $ DX.empty
          plan   = factorialDesign [contFactor "temp" (-1, 1)]
          m      = mkFrameHBM |-> designModelHBM defaultHBM plan [ranSlope ["temp"] "lot"] "y"
          bTemp  = mean0 (drawsFor "temp" m)
          sigBar = mean0 (dhfSigmaDraws m)
      bTemp  `shouldSatisfy` (\b -> abs (b - 3) < 1.0)   -- 平均傾き 3 を回復
      sigBar `shouldSatisfy` (< 1.0)                     -- 変量傾きで σ が縮む (切片のみなら ~2)

    -- ===== PROFILE: ボトルネック切り分け (壁時計 + iter スケーリング) =====
    -- 同一データ・同一軽量 config で「相関 (per-obs observe = full ad 疑い) vs
    -- 切片のみ (compiled observeLMR)」の壁時計を測り、 さらに相関を 2 倍 iter で
    -- 測って線形性 (per-eval 支配 vs tree-depth 増大) を見る。
    let mkFrameP slopeOf =
          let temps  = [-1, 1, -1, 1, -1, 1, -1, 1, -1, 1, -1, 1] :: [Double]
              lots   = ["A","A","A","A","A","A","B","B","B","B","B","B"] :: [T.Text]
              noise  = [0.05,-0.03,0.02,-0.04,0.01,-0.02,0.03,-0.01,0.04,-0.05,0.02,-0.03]
              ys     = [ 2 + slopeOf l * t + e | (t,l,e) <- zip3 temps lots noise ]
          in DX.insertColumn "lot"  (DX.fromList lots)
           $ DX.insertColumn "temp" (DX.fromList temps)
           $ DX.insertColumn "y"    (DX.fromList ys) DX.empty
        planP    = factorialDesign [contFactor "temp" (-1, 1)]
        tinyCfg n = defaultHBM { hbmChains = 1, hbmWarmup = n, hbmSamples = n }
        timeFit label fit = do
          t0 <- getCPUTime
          s  <- evaluate (sum (dhfSigmaDraws fit))
          t1 <- getCPUTime
          hPutStrLn stderr ("[PROFILE] " <> label <> ": "
            <> show (fromIntegral (t1 - t0) / 1e12 :: Double) <> " s  (Σσ=" <> show s <> ")")
    -- Phase 79 用の perf 診断 (pending)。 手動計測時のみ xit→it に戻して回す。
    xit "PROFILE 切片のみ 50iter" $ do
      timeFit "intercept  1x50"
        (mkFrameP (\l -> if l=="A" then 3 else 3)
           |-> designModelHBM (tinyCfg 50) planP [ranIntercept "lot"] "y")
      True `shouldBe` True
    xit "PROFILE 相関 50iter" $ do
      timeFit "correlated 1x50"
        (mkFrameP (\l -> if l=="A" then 5 else 1)
           |-> designModelHBM (tinyCfg 50) planP [ranSlope ["temp"] "lot"] "y")
      True `shouldBe` True
    xit "PROFILE 相関 100iter" $ do
      timeFit "correlated 1x100"
        (mkFrameP (\l -> if l=="A" then 5 else 1)
           |-> designModelHBM (tinyCfg 100) planP [ranSlope ["temp"] "lot"] "y")
      True `shouldBe` True

#ifdef PLOT_INTEGRATION
    -- Phase 78.G-f Task 4: profiler/contour が designModelHBM に載る事後予測帯。
    -- designMatrixF (dhfFormula m) ef を直接叩き、 fit 時と同じ列順の設計行列を作る
    -- ( evalFrameAt 相当の helper は無いので eval ModelFrame を手組みする)。
    -- ★mfParams (合成パラメータ名 "_p0"/"_p1"…) は formula 内部表現なので手で推測せず、
    --   訓練済 'dhfFrame' (= mvFrame m) を土台に mfRoles/mfNRows だけ差し替える
    --   (本番の Core.hs evalFrame と同じ据え置き方)。
    -- ※ MultiVarModel (mvFrame/mvEvalFrame) は plot 連携層・plot-integration 限定。
    it "MultiVarModel DesignHBMFit は事後予測帯を返す" $ do
      let temps  = [-1, 1, -1, 1, -1, 1, -1, 1, -1, 1, -1, 1] :: [Double]
          lots   = ["A","A","A","A","A","A","B","B","B","B","B","B"] :: [T.Text]
          noise  = [0.05, -0.03, 0.02, -0.04, 0.01, -0.02, 0.03, -0.01, 0.04, -0.05, 0.02, -0.03]
          lotShift l = if l == "A" then (-1.0) else 1.0
          ys     = [ 2 + 3 * t + lotShift l + e
                   | (t, l, e) <- zip3 temps lots noise ]
          mkFrameHBM = DX.insertColumn "lot"  (DX.fromList lots)
                     $ DX.insertColumn "temp" (DX.fromList temps)
                     $ DX.insertColumn "y"    (DX.fromList ys)
                     $ DX.empty
          plan = factorialDesign [contFactor "temp" (-1, 1)]
          m    = mkFrameHBM |-> designModelHBM defaultHBM plan [ranIntercept "lot"] "y"
          ef   = (mvFrame m)
                   { mfRoles  = [ ("y",    RoleResponse (V.fromList [0, 0, 0]))
                                , ("temp", RoleContinuous (V.fromList [-1, 0, 1]))
                                ]
                   , mfNRows  = 3
                   }
          (mu, band) = mvEvalFrame m 0.95 ef
      length mu `shouldBe` 3
      band `shouldSatisfy` isJust
      -- 中心 μ は temp とともに増加 (真の傾き ≈ 3)。
      (last mu - head mu) `shouldSatisfy` (> 3)
#endif

    -- Phase 78.G-f Task 5: multiOutput が designModelHBM とも対称に使えること
    -- (LM/GP と同じ「カレー化 spec (Text -> spec) を渡せば複数応答へ一括適用」 が
    -- 効くか) の確認。 designModelHBM :: HBMConfig -> Design -> [RandomSpec] -> Text -> spec
    -- は y が最終引数なので、 部分適用した @designModelHBM defaultHBM plan [ranIntercept "lot"]@
    -- は @Text -> DesignModelHBMSpec@ となり multiOutput にそのまま渡せる。
    it "multiOutput で designModelHBM を複数応答に適用できる" $ do
      let temps  = [-1, 1, -1, 1, -1, 1, -1, 1, -1, 1, -1, 1] :: [Double]
          lots   = ["A","A","A","A","A","A","B","B","B","B","B","B"] :: [T.Text]
          noise  = [0.05, -0.03, 0.02, -0.04, 0.01, -0.02, 0.03, -0.01, 0.04, -0.05, 0.02, -0.03]
          lotShift l = if l == "A" then (-1.0) else 1.0
          -- y1: 傾き 3、 y2: 傾き -2 (符号違いの別応答 → 2 fit が実際に異なることの傍証)。
          y1s    = [ 2 + 3    * t + lotShift l + e | (t, l, e) <- zip3 temps lots noise ]
          y2s    = [ 5 + (-2) * t + lotShift l + e | (t, l, e) <- zip3 temps lots noise ]
          mkFrameHBM2 = DX.insertColumn "lot"  (DX.fromList lots)
                      $ DX.insertColumn "temp" (DX.fromList temps)
                      $ DX.insertColumn "y1"   (DX.fromList y1s)
                      $ DX.insertColumn "y2"   (DX.fromList y2s)
                      $ DX.empty
          plan = factorialDesign [contFactor "temp" (-1, 1)]
          ms   = mkFrameHBM2 |-> multiOutput ["y1", "y2"] (designModelHBM defaultHBM plan [ranIntercept "lot"])
      map fst ms `shouldBe` ["y1", "y2"]

    -- Phase 78.J: modelFor で multiOutput 結果 [(Text,m)] から応答名でモデルを取り出せる
    -- (contourOf / surfaceOf に snd (head …) でなく応答名で渡すための selector)。
    it "modelFor: 応答名でモデルを取り出す (無い名前は error)" $ do
      modelFor "b" [("a", 1), ("b", 2)] `shouldBe` (2 :: Int)
      evaluate (modelFor "z" [("a", 1)] :: Int) `shouldThrow` anyErrorCall

  -- Phase 78.G-d: 応答曲面 解析 (自然単位で報告)。 coded 空間に既知の最適点を仕込んだ
  -- 応答から、 停留点が **自然単位** で復元されること・canonical で性質判定されること・
  -- steepest ascent 経路が自然単位で出ることを検証する。
  describe "Design.Workflow RSM 解析 (Phase 78.G-d)" $ do
    let planR = centralCompositeDesign [contFactor "x1" (150, 180), contFactor "x2" (10, 20)]
        -- runsheet (自然単位) → coded へ逆変換 (c = (x-center)/half)。
        codedOf = do
          let rs    = designTable planR
              temps = maybe [] id (lookup "x1" rs)
              times = maybe [] id (lookup "x2" rs)
              c1s   = map (\t -> (t - 165) / 15) temps
              c2s   = map (\d -> (d - 15)  / 5)  times
          (c1s, c2s)
        approx tol a b = abs (a - b) < tol
        stat name rep = maybe (1/0) id (lookup name (rsmStationary rep))

    it "rsmAnalysis: coded 原点が最大の曲面 → 停留点は自然単位の中心・RMaximum" $ do
      let (c1s, c2s) = codedOf
          ys  = zipWith (\a b -> 100 - (a*a + b*b)) c1s c2s   -- coded max at (0,0)
          rep = rsmAnalysis planR ys
      rsmNature rep `shouldBe` RMaximum
      rsmInRegion rep `shouldBe` True
      stat "x1" rep `shouldSatisfy` approx 1e-6 165   -- 中心 (自然単位)
      stat "x2" rep `shouldSatisfy` approx 1e-6 15
      rsmPredicted rep `shouldSatisfy` approx 1e-6 100
      rsmR2 rep `shouldSatisfy` (> 0.999)
      all ((< 0) . fst) (rsmCanonical rep) `shouldBe` True   -- 全固有値 < 0

    it "rsmAnalysis: coded (0.5,-0.5) が最大 → 停留点が対応する自然単位へ decode" $ do
      let (c1s, c2s) = codedOf
          ys  = zipWith (\a b -> 50 - ((a-0.5)^(2::Int) + (b+0.5)^(2::Int))) c1s c2s
          rep = rsmAnalysis planR ys
      rsmNature rep `shouldBe` RMaximum
      stat "x1" rep `shouldSatisfy` approx 1e-6 172.5   -- 165 + 0.5*15
      stat "x2" rep `shouldSatisfy` approx 1e-6 12.5    -- 15  + (-0.5)*5

    it "rsmAnalysis: c1² - c2² は鞍点 (固有値 混在) → RSaddle" $ do
      let (c1s, c2s) = codedOf
          ys  = zipWith (\a b -> a*a - b*b) c1s c2s
          rep = rsmAnalysis planR ys
      rsmNature rep `shouldBe` RSaddle
      any ((> 0) . fst) (rsmCanonical rep) `shouldBe` True
      any ((< 0) . fst) (rsmCanonical rep) `shouldBe` True

    it "steepestAscentNatural: 線形上昇 (coded x1 方向) の経路が自然単位で出る" $ do
      let (c1s, c2s) = codedOf
          ys   = zipWith (\a _ -> a) c1s c2s          -- coded 勾配 = (1, 0)
          path = steepestAscentNatural True planR ys 0.5 2
          x1s  = map (maybe (1/0) id . lookup "x1") path
          x2s  = map (maybe (1/0) id . lookup "x2") path
      length path `shouldBe` 3                          -- nSteps + 1
      and (zipWith (approx 1e-6) x1s [165, 172.5, 180]) `shouldBe` True
      all (approx 1e-6 15) x2s `shouldBe` True          -- x2 は動かない

    it "rsmAnalysis: カテゴリ因子を含む設計は error (連続専用)" $ do
      let planCat = factorialDesign [contFactor "x1" (0,1), catFactor "g" ["a","b"]]
      evaluate (rsmNature (rsmAnalysis planCat [1,2,3,4])) `shouldThrow` anyException

  -- Phase 78.G: Box-Behnken 応答曲面計画 (k = 3,4,5・±α なし・2 次モデル)。
  describe "Design.Workflow Box-Behnken (Phase 78.G)" $ do
    let planBB = boxBehnkenDesign [contFactor "t" (150,180), contFactor "p" (1,3), contFactor "c" (5,15)]

    it "boxBehnkenDesign: k=3 は 12 corner + 中心点 (= CCD より少なめ・±α なし)" $ do
      let rs = designTable planBB
          n  = length (maybe [] id (lookup "t" rs))
      n `shouldBe` (12 + 3)                                        -- corner 12 + nC=k=3

    it "boxBehnkenDesign: 各因子は -1/0/+1 の 3 水準に収まる (±α を持たない)" $ do
      let rs   = designTable planBB
          -- t: center 165・half 15 → 実値は 150/165/180 のみ (coded -1/0/+1)
          tvals = maybe [] id (lookup "t" rs)
      -- CCD の軸点 (143.8.. 等) のような立方体外の値が無いことを確認
      all (`elem` [150,165,180]) tvals `shouldBe` True

    it "boxBehnkenDesign: RSM ゆえ formula = 2 次モデル" $
      designFormula planBB "y" `shouldBe`
        "y ~ t + p + c + t:p + t:c + p:c + I(t^2) + I(p^2) + I(c^2)"

    it "boxBehnkenDesign: designModel で 2 次モデルを当てられる (真モデル再現)" $ do
      let rs    = designTable planBB
          tv    = maybe [] id (lookup "t" rs)
          pv    = maybe [] id (lookup "p" rs)
          cv    = maybe [] id (lookup "c" rs)
          ys    = zipWith3 (\t p c -> 10 + 0.1*t + 2*p - 0.5*c + 0.01*t*p) tv pv cv
          m     = (("y", ys) : rs) |-> designModel planBB "y"
      rSquared1 (mlmResult m) `shouldSatisfy` (> 0.99)

  -- Phase 78.G: 一部実施要因 (fractional factorial・run 削減)。
  describe "Design.Workflow fractional (Phase 78.G)" $ do
    let specs7 = [ contFactor (T.pack [c]) (0, 1) | c <- "abcdefg" ]   -- 7 因子

    -- ★カタログ自己検証: 各 generator の解像度が表のラベルと一致するか (誤り混入ガード)。
    it "fractionalCatalog: 全エントリの generator 解像度がラベルと一致 (fracResolution 照合)" $
      [ (k, n, resNum r, fracResolution k gens)
      | k <- [3..16], (n, r, gens) <- fractionalCatalog k
      , resNum r /= fracResolution k gens ]
        `shouldBe` []

    it "fractionalCatalog: run 数 = 2^(k - p) (p = generator 数)" $
      [ (k, n, 2 ^ (k - length gens))
      | k <- [3..16], (n, _, gens) <- fractionalCatalog k
      , n /= 2 ^ (k - length gens) ]
        `shouldBe` []

    -- Phase 78.G-c k>7 拡張: 16/32-run 追加設計 (NIST 標準表)。
    let mkN n = [ contFactor (T.pack [c]) (0,1) | c <- take n ['a' ..] ]

    it "fractionalCatalog: k=8 は 16-run Res IV を持つ (NIST 2^(8-4))" $
      [ (n, resNum r) | (n, r, _) <- fractionalCatalog 8, n == 16 ]
        `shouldBe` [(16, 4)]

    it "fractionalDesign: 8 因子 ResIV は 16 run" $
      length (dsCoded (fractionalDesign (mkN 8) ResIV)) `shouldBe` 16

    it "fractionalDesign: 9 因子 ResIV は 32 run (16-run では ResIII しか出ない)" $
      length (dsCoded (fractionalDesign (mkN 9) ResIV)) `shouldBe` 32

    it "fractionalDesign: 11 因子 ResIV は 32 run" $
      length (dsCoded (fractionalDesign (mkN 11) ResIV)) `shouldBe` 32

    it "fractionalDesign: 15 因子 (16-run 飽和 ResIII) は 16 run" $
      length (dsCoded (fractionalDesign (mkN 15) ResIII)) `shouldBe` 16

    it "fractionalDesign: 追加設計も主効果が直交 (k=9 ResIV 32-run・k=15 16-run)" $ do
      let orthogonal plan =
            let k    = length (dsFactors plan)
                cols = [ [ row !! j | row <- dsCoded plan ] | j <- [0 .. k - 1] ]
                dot a b = sum (zipWith (*) a b)
            in all (\c -> abs (sum c) < 1e-9) cols
               && and [ abs (dot (cols!!i) (cols!!j)) < 1e-9
                      | i <- [0..k-1], j <- [i+1..k-1] ]
      orthogonal (fractionalDesign (mkN 9) ResIV)  `shouldBe` True
      orthogonal (fractionalDesign (mkN 15) ResIII) `shouldBe` True

    it "fractionalDesign: 7 因子 ResIII は 8 run (完全要因 128 から大幅削減)" $ do
      let plan = fractionalDesign specs7 ResIII
          rs   = designTable plan
      length (maybe [] id (lookup "a" rs)) `shouldBe` 8

    it "fractionalDesign: 解像度指定は「以上」で最小 run を選ぶ (5 因子 ResV = 16 run)" $ do
      let plan = fractionalDesign [ contFactor (T.pack [c]) (0,1) | c <- "abcde" ] ResV
      length (dsCoded plan) `shouldBe` 16

    it "fractionalDesign: formula は主効果のみ (交互作用なし)" $
      designFormula (fractionalDesign specs7 ResIII) "y"
        `shouldBe` "y ~ a + b + c + d + e + f + g"

    it "fractionalDesign: 主効果は直交 (列同士の内積 = 0)・各列は平衡 (和 = 0)" $ do
      let plan = fractionalDesign specs7 ResIII
          cols = [ [ row !! j | row <- dsCoded plan ] | j <- [0 .. 6] ]
          dot a b = sum (zipWith (*) a b)
      all (\c -> abs (sum c) < 1e-9) cols `shouldBe` True                 -- 平衡
      and [ abs (dot (cols!!i) (cols!!j)) < 1e-9
          | i <- [0..6], j <- [i+1..6] ] `shouldBe` True                 -- 直交

    it "fractionalDesignGen: generator 明示 (D=ABC の 2^(4-1)) は 8 run・主効果直交" $ do
      let plan = fractionalDesignGen [ contFactor (T.pack [c]) (0,1) | c <- "abcd" ] [[1,2,3]]
      length (dsCoded plan) `shouldBe` 8

    it "fractionalDesign: 削減設計でも主効果 LM を当てられる (主効果 DGP を再現)" $ do
      let plan  = fractionalDesign specs7 ResIII
          rs    = designTable plan
          cols  = map (\c -> maybe [] id (lookup (T.pack [c]) rs)) "abcdefg"
          ys    = foldr1 (zipWith (+))
                    (zipWith (\w col -> map (* w) col) [2,3,-1,4,-2,1,0.5] cols)
          m     = (("y", ys) : rs) |-> designModel plan "y"
      rSquared1 (mlmResult m) `shouldSatisfy` (> 0.99)

    -- Phase 78.G-c: alias 構造 + clear 2FI formula (fractionalDesignInter/Gen)。
    let mkS cs = [ contFactor (T.pack [c]) (0,1) | c <- cs ]
        nInter = T.count ":"   -- 2FI 項数 = formula 中の ":" 数

    it "fractionalDesignGenInter: Res V (k=5, I=ABCDE) は全 2FI (C(5,2)=10) を足す" $
      nInter (designFormula (fractionalDesignGenInter (mkS "abcde") [[1,2,3,4]]) "y")
        `shouldBe` 10

    it "fractionalDesignGenInter: Res IV (k=4, D=ABC) は交絡群ごと代表 1 個 = 3 個" $
      nInter (designFormula (fractionalDesignGenInter (mkS "abcd") [[1,2,3]]) "y")
        `shouldBe` 3

    it "fractionalDesignGenInter: Res III (k=3, C=AB) は 2FI が全て主効果と交絡 → 0 個" $
      designFormula (fractionalDesignGenInter (mkS "abc") [[1,2]]) "y"
        `shouldBe` "y ~ a + b + c"

    it "aliasStructure: Res IV (k=4, D=ABC) で a:b は c:d と交絡" $ do
      let plan = fractionalDesignGenInter (mkS "abcd") [[1,2,3]]
      fmap (elem "c:d") (lookup "a:b" (aliasStructure plan)) `shouldBe` Just True

    it "fractionalDesignInter: 主効果 + 交互作用 DGP を飽和当てはめ (k=4 ResIV・p=8・R^2~1)" $ do
      let plan = fractionalDesignGenInter (mkS "abcd") [[1,2,3]]   -- 8 run
          rs   = designTable plan
          col c = maybe [] id (lookup (T.pack [c]) rs)
          [a,b,cc,d] = map col "abcd"
          -- 主効果 + 3 交互作用 (交絡群の代表) の DGP。
          ys   = [ 2 + 3*(a!!i) - (b!!i) + 2*(cc!!i) + 0.5*(d!!i)
                     + 1.5*(a!!i * b!!i) - 0.8*(a!!i * cc!!i) + 0.6*(a!!i * d!!i)
                 | i <- [0 .. length a - 1] ]
          m    = (("y", ys) : rs) |-> designModel plan "y"
      snd (LA.size (mlmDesign m)) `shouldBe` 8            -- 切片 + 4 主効果 + 3 2FI
      rSquared1 (mlmResult m) `shouldSatisfy` (> 0.999)

    it "fractionalDesignInter: 解像度自動 (k=5 ResV) は 16 run + 全 2FI" $ do
      let plan = fractionalDesignInter (mkS "abcde") ResV
      length (dsCoded plan) `shouldBe` 16
      nInter (designFormula plan "y") `shouldBe` 10

  -- Phase 78.G-a: Taguchi 直交表 (2 水準スクリーニング)。
  describe "Design.Workflow taguchi (Phase 78.G-a)" $ do
    let mkSpecs cs = [ contFactor (T.pack [c]) (0, 1) | c <- cs ]

    it "taguchiDesign: 最小 OA 自動選択 (3→L4/7→L8/11→L12/15→L16 run)" $
      [ length (dsCoded (taguchiDesign (mkSpecs cs)))
      | cs <- [take n ['a'..] | n <- [3, 7, 11, 15]] ]
        `shouldBe` [4, 8, 12, 16]

    it "taguchiDesign: 列数境界で次の表へ繰り上がる (4 因子→L8・8 因子→L12)" $
      [ length (dsCoded (taguchiDesign (mkSpecs (take n ['a'..]))))
      | n <- [4, 8] ]
        `shouldBe` [8, 12]

    it "taguchiDesign: coded は ±1 のみ" $
      all (`elem` [-1, 1]) (concat (dsCoded (taguchiDesign (mkSpecs "abcde"))))
        `shouldBe` True

    it "taguchiDesign: formula は主効果のみ (交互作用なし)" $
      designFormula (taguchiDesign (mkSpecs "abcde")) "y"
        `shouldBe` "y ~ a + b + c + d + e"

    it "taguchiDesign: 各列は平衡 (和 = 0)・列同士は直交 (内積 = 0)" $ do
      let plan   = taguchiDesign (mkSpecs "abcde")   -- L8 の先頭 5 列
          k      = 5
          cols   = [ [ row !! j | row <- dsCoded plan ] | j <- [0 .. k - 1] ]
          dot a b = sum (zipWith (*) a b)
      all (\c -> abs (sum c) < 1e-9) cols `shouldBe` True
      and [ abs (dot (cols !! i) (cols !! j)) < 1e-9
          | i <- [0 .. k - 1], j <- [i + 1 .. k - 1] ] `shouldBe` True

    it "taguchiDesignOA: L12 は 11 因子/12 run のスクリーニング (Plackett-Burman)" $ do
      let plan = taguchiDesignOA L12 (mkSpecs "abcdefghijk")
      length (dsCoded plan) `shouldBe` 12
      length (dsFactors plan) `shouldBe` 11

    it "taguchiDesignOA: 列挙型 OATable で表を明示できる (L8 = 8 run)" $
      length (dsCoded (taguchiDesignOA L8 (mkSpecs "abc")))
        `shouldBe` 8

    it "taguchiDesign: L12 で主効果 LM を当てられる (11 因子・主効果 DGP を再現)" $ do
      let plan = taguchiDesignOA L12 (mkSpecs "abcdefghijk")
          rs   = designTable plan
          cols = map (\c -> maybe [] id (lookup (T.pack [c]) rs)) "abcdefghijk"
          ws   = [2, 3, -1, 4, -2, 1, 0.5, -3, 2.5, -1.5, 0.8]
          ys   = foldr1 (zipWith (+))
                   (zipWith (\w col -> map (* w) col) ws cols)
          m    = (("y", ys) : rs) |-> designModel plan "y"
      rSquared1 (mlmResult m) `shouldSatisfy` (> 0.99)

  -- Phase 78.G-a2: 3 水準/混合直交表 (L9/L18/L27)・数値 (opoly) / カテゴリ (contrast)。
  describe "Design.Workflow taguchi 3水準/混合 (Phase 78.G-a2)" $ do
    let cats4 = [ catFactor (T.pack [c]) ["lo","mid","hi"] | c <- "abcd" ]
        nums4 = [ numFactor (T.pack [c]) [1, 2, 3] | c <- "abcd" ]

    it "taguchiDesign: 3 水準カテゴリ 4 因子 → L9 (9 run)" $
      length (dsCoded (taguchiDesign cats4)) `shouldBe` 9

    it "taguchiDesign: 3 水準数値 4 因子 → L9 (9 run)" $
      length (dsCoded (taguchiDesign nums4)) `shouldBe` 9

    it "taguchiDesign: 数値 3 水準の formula は opoly (直交多項式 2 自由度)" $
      designFormula (taguchiDesign [ numFactor "temp" [150,165,180]
                                   , numFactor "time" [10,20,30] ]) "y"
        `shouldBe` "y ~ opoly(temp,2) + opoly(time,2)"

    it "taguchiDesign: カテゴリ 3 水準の formula は主効果名 (contrast は engine 任せ)" $
      designFormula (taguchiDesign [ catFactor "cat" ["A","B","C"]
                                   , catFactor "mat" ["X","Y","Z"] ]) "y"
        `shouldBe` "y ~ cat + mat"

    it "designTable: 数値 3 水準因子は実水準値の Double 列 (index→実値)" $ do
      -- L9 の先頭列 (level code 1/2/3) が実値 150/165/180 に写る。 Num は numeric ゆえ
      -- designTable (numeric runsheet) に載る (Cat と違い error にならない)。
      let rs  = designTable (taguchiDesign [ numFactor "temp" [150,165,180] ])
          col = maybe [] id (lookup "temp" rs)
      (length col, all (`elem` [150,165,180]) col) `shouldBe` (9, True)

    it "designFrame: 数値 3 水準因子の列を持つ" $
      DX.columnNames (designFrame (taguchiDesign [ numFactor "temp" [150,165,180] ]))
        `shouldContain` ["temp"]

    it "taguchiDesign: 混合 (連続 1 + カテゴリ 3水準 2) → L18 (18 run)" $
      length (dsCoded (taguchiDesign [ contFactor "p" (0, 1)
                                     , catFactor "q" ["a","b","c"]
                                     , catFactor "r" ["x","y","z"] ]))
        `shouldBe` 18

    it "taguchiDesignOA: L9 を明示し 3 水準因子を割り当てられる" $
      length (dsCoded (taguchiDesignOA L9 nums4)) `shouldBe` 9

    it "taguchiDesign: 数値 3 水準で加法 2 次 DGP を designModel で再現 (R^2 ~ 1)" $ do
      let plan = taguchiDesign [ numFactor "a" [1,2,3], numFactor "b" [1,2,3] ]
          rs   = designTable plan
          as   = maybe [] id (lookup "a" rs)
          bs   = maybe [] id (lookup "b" rs)
          ys   = zipWith (\a b -> 2 + 3*a - 0.5*a*a + 1*b + 0.2*b*b) as bs
          m    = (("y", ys) : rs) |-> designModel plan "y"
      rSquared1 (mlmResult m) `shouldSatisfy` (> 0.999)

  -- Phase 78.G-b1: 最適計画 (D/A/I/E/G-最適・カスタム formula・連続因子)。
  describe "Design.Workflow optimal (Phase 78.G-b1)" $ do
    let mkSpecs cs = [ contFactor (T.pack [c]) (-1, 1) | c <- cs ]

    it "optimalDesign: run 数 = 指定 n" $
      length (dsCoded (optimalDesign (mkSpecs "abc") (mainEffects ["a","b","c"]) 6))
        `shouldBe` 6

    -- Phase 78.I: 候補点数 (2 因子 quadratic = 3×3 = 9) を超える n も点を反復して satisfy する。
    it "optimalDesign: n が候補点数を超えても run 数 = n (点を反復)" $
      length (dsCoded (optimalDesign (mkSpecs "ab") (quadratic ["a","b"]) 12))
        `shouldBe` 12

    it "optimalDesign: 主効果モデル既定は 2 水準候補 (coded ∈ {-1,+1})" $
      all (`elem` [-1, 1])
          (concat (dsCoded (optimalDesign (mkSpecs "ab") (mainEffects ["a","b"]) 4)))
        `shouldBe` True

    it "optimalDesign: 2 次項ありモデル既定は 3 水準候補 (coded ∈ {-1,0,+1})" $
      all (`elem` [-1, 0, 1])
          (concat (dsCoded (optimalDesign (mkSpecs "ab") (quadratic ["a","b"]) 8)))
        `shouldBe` True

    it "optimalDesignLevels: 水準数を明示できる (3 水準グリッド)" $
      all (`elem` [-1, 0, 1])
          (concat (dsCoded (optimalDesignLevels 3 (mkSpecs "ab") (mainEffects ["a","b"]) 5)))
        `shouldBe` True

    it "optimalDesign: 主効果 n=2^k は全格子 = 直交・平衡な full factorial を選ぶ" $ do
      let plan = optimalDesign (mkSpecs "ab") (mainEffects ["a","b"]) 4
          cols = [ [ row !! j | row <- dsCoded plan ] | j <- [0, 1] ]
          dot a b = sum (zipWith (*) a b)
      all (\c -> abs (sum c) < 1e-9) cols `shouldBe` True          -- 平衡
      abs (dot (cols !! 0) (cols !! 1)) < 1e-9 `shouldBe` True     -- 直交

    it "optimalDesign: KCustom formula を焼き込む (designFormula に応答名が入る)" $
      designFormula (optimalDesign (mkSpecs "ab") (twoWay ["a","b"]) 4) "yield"
        `shouldSatisfy` T.isPrefixOf "yield "

    it "optimalDesign: n < p (パラメータ数) は error" $
      -- quadratic 2 因子 = p=6・n=4 < 6
      evaluate (length (dsCoded (optimalDesign (mkSpecs "ab") (quadratic ["a","b"]) 4)))
        `shouldThrow` anyErrorCall

    it "optimalDesign: designModel で当てられ・列数 p が formula と一致 (quadratic 2 因子 = 6)" $ do
      let plan = optimalDesign [contFactor "t" (150,180), contFactor "p" (1,5)] (quadratic ["t","p"]) 9
          rs   = designTable plan
          tv   = maybe [] id (lookup "t" rs)
          pv   = maybe [] id (lookup "p" rs)
          ys   = zipWith (\t p -> 2 + 0.1*t - 0.5*p + 0.01*(t-165)^(2::Int) + 0.02*p*p) tv pv
          m    = (("y", ys) : rs) |-> designModel plan "y"
      snd (LA.size (mlmDesign m)) `shouldBe` 6                     -- 切片+t+p+t:p+t^2+p^2
      rSquared1 (mlmResult m) `shouldSatisfy` (> 0.99)            -- 真 2 次 DGP を再現

    it "optimalDesign: parseRFormula 文字列でモデル指定できる" $ do
      case (either (const Nothing) Just (parseRFormula "y ~ a + b + a:b")) of
        Nothing  -> expectationFailure "parseRFormula 失敗"
        Just fml ->
          length (dsCoded (optimalDesign (mkSpecs "ab") fml 4)) `shouldBe` 4

  -- Phase 78.G-b2: カテゴリ因子 (DesignFactor の FactorKind 和型化・型手術)。
  describe "Design.Workflow categorical (Phase 78.G-b2)" $ do
    -- 連続1 (temp) × カテゴリ3水準 (cat) の完全要因 = 2×3 = 6 run。
    let planMix = factorialDesign
                    [ contFactor "temp" (150, 180)
                    , catFactor  "cat"  ["A", "B", "C"] ]

    it "factorialDesign: 連続×カテゴリ混合は各水準総当り (2×3 = 6 run)" $
      length (dsCoded planMix) `shouldBe` 6

    it "factorialDesign: 単一カテゴリ3水準は 3 run (水準数ぶん)" $
      length (dsCoded (factorialDesign [catFactor "cat" ["A","B","C"]]))
        `shouldBe` 3

    it "designFrame: カテゴリ列は水準名 (Text)・連続列と run 列を持つ" $ do
      let df = designFrame planMix
      DX.columnNames df `shouldContain` ["cat"]
      DX.columnNames df `shouldContain` ["temp"]
      DX.columnNames df `shouldContain` ["run"]

    it "designTable: カテゴリ因子を含むと error (designFrame へ誘導)" $
      evaluate (length (designTable planMix)) `shouldThrow` anyErrorCall

    it "designModel: カテゴリを contrast 展開して当てられる (factor×factor 飽和 → p=6・R^2~1)" $ do
      -- 純カテゴリ 2 因子 (3水準×2水準 = 6 run) の完全要因 y ~ cat * grp。 treatment contrast で
      -- 列数 = Kcat*Kgrp = 6 (切片+cat 2+grp 1+cat:grp 2)・6 点 = 飽和 → R^2~1 (DesignSpec 検証点①と同型)。
      let planCC = factorialDesign [ catFactor "cat" ["A","B","C"], catFactor "grp" ["X","Y"] ]
          coded  = dsCoded planCC
          catEff i = [0, 5, -3] !! (round i :: Int) :: Double
          grpEff j = [0, 4]    !! (round j :: Int) :: Double
          ys     = [ 10 + catEff (row !! 0) + grpEff (row !! 1) | row <- coded ] :: [Double]
          df     = DX.insertColumn "y" (DX.fromList ys) (designFrame planCC)
          m      = df |-> designModel planCC "y"
      snd (LA.size (mlmDesign m)) `shouldBe` 6                 -- 切片+cat(2)+grp(1)+cat:grp(2)
      rSquared1 (mlmResult m) `shouldSatisfy` (> 0.999)

    it "centralCompositeDesign: カテゴリ因子は error (連続専用・±α 軸点ゆえ)" $
      evaluate (length (dsCoded (centralCompositeDesign [contFactor "x" (0,1), catFactor "c" ["A","B"]])))
        `shouldThrow` anyErrorCall

    it "taguchiDesign: 連続 + binary カテゴリ (共に 2 水準) → L4 (G-a2 で受理)" $
      -- G-a2 以降 taguchi はカテゴリを受ける。 連続(2)+binary(2) = 2 因子とも 2 水準 → L4。
      length (dsCoded (taguchiDesign [contFactor "x" (0,1), catFactor "c" ["A","B"]]))
        `shouldBe` 4

    it "fractionalDesign: 2 水準 (binary) カテゴリは許容 (coded ±1 に写す)" $ do
      -- 連続5 + binary カテゴリ2 = 7 因子・ResIII = 8 run。
      let specs = [ contFactor (T.pack [c]) (0,1) | c <- "abcde" ]
                  ++ [ catFactor "cat1" ["lo","hi"], catFactor "cat2" ["off","on"] ]
      length (dsCoded (fractionalDesign specs ResIII)) `shouldBe` 8

    it "fractionalDesign: 3 水準以上のカテゴリは error (binary のみ対応)" $
      evaluate (length (dsCoded
        (fractionalDesign
          ([ contFactor (T.pack [c]) (0,1) | c <- "abc" ]
            ++ [ catFactor "cat" ["A","B","C"] ]) ResIII)))
        `shouldThrow` anyErrorCall

    it "optimalDesign: カテゴリ因子を候補展開して選べる (run 数 = n)" $
      length (dsCoded
        (optimalDesign
          [ contFactor "x" (-1,1), catFactor "cat" ["A","B"] ]
          (mainEffects ["x","cat"]) 4))
        `shouldBe` 4

  -- Phase 78.K: 設計の保存 (CSV) と DataFrame からの復元 (planFromFrame)。
  describe "Design.Workflow save / planFromFrame (Phase 78.K)" $ do
    let approxRows a b =
          length a == length b
            && and (zipWith (\r s -> length r == length s
                                       && and (zipWith (\x y -> abs (x - y) < 1e-9) r s)) a b)

    it "planFromFrame: designFrame の逆で coded を復元 (連続・round-trip)" $ do
      let facs  = [ contFactor "temp" (150, 180), contFactor "time" (10, 20) ]
          plan  = centralCompositeDesign facs
          plan2 = planFromFrame facs (quadratic ["temp", "time"]) (designFrame plan)
      approxRows (dsCoded plan2) (dsCoded plan) `shouldBe` True

    it "planFromFrame: カテゴリ混在も復元し designModel で当たる" $ do
      let facs = [ contFactor "temp" (150, 180), catFactor "cat" ["A", "B", "C"] ]
          plan = factorialDesign facs
          df   = designFrame plan
          plan2 = planFromFrame facs (mainEffects ["temp", "cat"]) df
      length (dsCoded plan2) `shouldBe` length (dsCoded plan)      -- run 数一致
      -- 応答を足して fit できる (formula = 主効果・p = 切片+temp+cat(2) = 4)
      let ys = map fromIntegral [1 .. length (dsCoded plan)] :: [Double]
          m  = DX.insertColumn "y" (DX.fromList ys) df |-> designModel plan2 "y"
      snd (LA.size (mlmDesign m)) `shouldBe` 4

    it "planFromFrame: 因子列が df に無いと error" $ do
      let df = designFrame (factorialDesign [contFactor "temp" (150, 180)])
      evaluate (length (dsCoded
        (planFromFrame [contFactor "NOPE" (0, 1)] (mainEffects ["NOPE"]) df)))
        `shouldThrow` anyErrorCall

    it "saveDesign: runsheet を CSV に書ける (header + N run 行)" $
      withSystemTempDirectory "doe-save" $ \dir -> do
        let plan = centralCompositeDesign [contFactor "temp" (150, 180), contFactor "time" (10, 20)]
            path = dir ++ "/runsheet.csv"
        saveDesign path plan
        content <- readFile path
        let ls = filter (not . null) (lines content)
        length ls `shouldBe` (1 + length (dsCoded plan))     -- header + 10 run
        head ls `shouldSatisfy` (\h -> all (`isInfixOf` h) ["run", "temp", "time"])

  -- Phase 79: 因子は純粋に因子 (役割 dfRole は撤去・階層は Structure が名前で持つ)。
  describe "Phase 79: 因子の smart ctor (dfName/dfKind)" $ do
    it "smart ctor の dfName/dfKind (連続 / 数値順序 / カテゴリ)" $ do
      dfName (contFactor "temp" (150, 180)) `shouldBe` "temp"
      dfKind (contFactor "temp" (150, 180)) `shouldBe` Cont 150 180 SLinear
      dfKind (contFactorLog "conc" (0.01, 10)) `shouldBe` Cont 0.01 10 SLog
      dfKind (numFactor "t" [150, 165, 180]) `shouldBe` Num [150, 165, 180]
      dfKind (catFactor "cat" ["A", "B"])    `shouldBe` Cat ["A", "B"]

  -- Phase 78.M M3: Formula (効果DSL) → Custom.Model 変換。
  describe "Phase 78.M M3: formulaToCustomModel" $ do
    let names = ["a", "b"]
        terms f = fmap CMd.mTerms (formulaToCustomModel names f)
    it "mainEffects → 切片 + 各主効果 (TIntercept/TMain)" $
      terms (mainEffects names)
        `shouldBe` Right [CMd.TIntercept, CMd.TMain "a", CMd.TMain "b"]

    it "twoWay → 主効果 + 2 因子交互作用 (TInter)" $
      terms (twoWay names)
        `shouldBe` Right [CMd.TIntercept, CMd.TMain "a", CMd.TMain "b", CMd.TInter ["a", "b"]]

    it "quadratic → 主効果 + 交互作用 + 2 次項 (TPower)" $
      terms (quadratic names)
        `shouldBe` Right
          [ CMd.TIntercept, CMd.TMain "a", CMd.TMain "b"
          , CMd.TInter ["a", "b"], CMd.TPower "a" 2, CMd.TPower "b" 2 ]

    it "正規化は NCoded (optimalDesign と同じ coded 規約)" $
      fmap CMd.mNorm (formulaToCustomModel names (quadratic names))
        `shouldBe` Right CMd.NCoded

    it "基底関数など未対応項は Left" $ do
      let f = either (error "parse") id (parseRFormula "_y ~ opoly(a, 2)")
      case formulaToCustomModel ["a"] f of
        Left _  -> pure ()
        Right m -> expectationFailure ("expected Left, got " ++ show (CMd.mTerms m))

  -- Phase 79: 完全カスタムデザインエンジン (CustomSpec / Structure)。
  describe "Phase 79: customDesign (CustomSpec)" $ do
    it "CRD: n run の Design を返す (KStructured [])" $ do
      let plan = customDesign (customSpec
                   [contFactor "temp" (150, 180), contFactor "time" (10, 20)]
                   (quadratic ["temp", "time"]) 10 42)
      length (dsCoded plan) `shouldBe` 10
      case dsKind plan of
        KStructured [] _ -> pure ()               -- CRD = 群列なし
        k                -> expectationFailure ("expected KStructured [] , got " ++ show k)

    it "CRD: seed 決定的 (2 回同結果・pure)" $ do
      let mk = customDesign (customSpec [contFactor "a" (-1, 1), contFactor "b" (-1, 1)]
                            (twoWay ["a", "b"]) 8 7)
      dsCoded mk `shouldBe` dsCoded mk

    it "customSpec の既定は CRD・DOpt・制約なし" $ do
      let cs = customSpec [contFactor "a" (-1, 1)] (mainEffects ["a"]) 4 1
      csStructure cs  `shouldBe` CRD
      csCriterion cs  `shouldBe` DOpt
      csConstraints cs `shouldBe` []

    it "制約つき CRD: 線形制約 x1+x2<=0.5 (coded) を全 run が満たす" $ do
      let cons = [ LinearIneq [("x1", 1), ("x2", 1)] CLeq 0.5 ]
          plan = customDesign (customSpec
                   [contFactor "x1" (-1, 1), contFactor "x2" (-1, 1)]
                   (twoWay ["x1", "x2"]) 8 42) { csConstraints = cons }
          sums = map sum (dsCoded plan)          -- coded x1 + x2
      length (dsCoded plan) `shouldBe` 8
      all (<= 0.5 + 1e-9) sums `shouldBe` True    -- 制約違反 (例 [1,1]=2.0) が出ない

    it "csConstraints=[] は制約なし既定と同結果 (回帰)" $ do
      let fs   = [contFactor "x1" (-1, 1), contFactor "x2" (-1, 1)]
          base = customSpec fs (twoWay ["x1", "x2"]) 8 42
      dsCoded (customDesign base)
        `shouldBe` dsCoded (customDesign base { csConstraints = [] })

    it "CRD: Num 因子 (数値順序) も使える — dsCoded は水準 index" $ do
      let plan = customDesign (customSpec
                   [numFactor "temp" [150, 165, 180], contFactor "time" (10, 20)]
                   (quadratic ["temp", "time"]) 9 3)
      length (dsCoded plan) `shouldBe` 9
      -- temp (列 0) は水準 index {0,1,2} (numLevelAt が実値へ戻す規約)
      let tempIdx = map (round . head) (dsCoded plan) :: [Int]
      all (`elem` [0, 1, 2]) tempIdx `shouldBe` True
      -- designFrame は temp を実水準値 {150,165,180} に decode する (round-trip)
      let tempVals = case getDoubleVec "temp" (designFrame plan) of
            Just v  -> V.toList v
            Nothing -> error "temp 列が無い"
      all (`elem` [150, 165, 180]) tempVals `shouldBe` True

    -- Phase 79.2: SplitPlot 構造 (群単位ムーブ + GLS 基準)。
    it "SplitPlot: KStructured [(wholePlot, ids)]・群 ID が n 個・nWhole 群" $ do
      let plan = customDesign (customSpec
                   [contFactor "temp" (150, 180), contFactor "rate" (10, 20)]
                   (twoWay ["temp", "rate"]) 8 50) { csStructure = splitPlot ["temp"] 4 }
      length (dsCoded plan) `shouldBe` 8
      case dsKind plan of
        KStructured [("wholePlot", ids)] _ -> do
          length ids `shouldBe` 8
          length (nub ids) `shouldBe` 4          -- 4 whole-plot
        k -> expectationFailure ("expected KStructured [(wholePlot,_)], got " ++ show k)

    it "SplitPlot: whole-plot 因子 (temp) は群内で一定" $ do
      let plan = customDesign (customSpec
                   [contFactor "temp" (150, 180), contFactor "rate" (10, 20)]
                   (twoWay ["temp", "rate"]) 8 50) { csStructure = splitPlot ["temp"] 4 }
      case dsKind plan of
        KStructured [("wholePlot", ids)] _ -> do
          let tempCol   = [ row !! 0 | row <- dsCoded plan ]       -- temp = 列 0
              byGroup g = [ t | (t, i) <- zip tempCol ids, i == g ]
          all (\g -> let vs = byGroup g in all (== head vs) vs) (nub ids)
            `shouldBe` True
        _ -> expectationFailure "expected KStructured [(wholePlot,_)]"

    it "SplitPlot: seed 決定的 (2 回同結果・pure)" $ do
      let mk = customDesign (customSpec
                 [contFactor "temp" (150, 180), contFactor "rate" (10, 20)]
                 (twoWay ["temp", "rate"]) 8 50) { csStructure = splitPlot ["temp"] 4 }
      dsCoded mk `shouldBe` dsCoded mk

    -- Jones-Goos (2012) Table 2 golden を高レベル API へ移植: 20-run split-plot
    -- (4 WP × 5 SP・full quadratic・η=1) の D-criterion det(Xᵀ M⁻¹ X) が文献値 2684.44 に到達。
    -- 低レベル golden (Custom.SplitPlotSpec) と同じ math を新エンジンで再確認する。
    it "SplitPlot golden: det(Xᵀ M⁻¹ X) が Jones-Goos 2012 文献値 ≥ 2684" $ do
      let fs   = [contFactor "w" (-1, 1), contFactor "s" (-1, 1)]
          plan = customDesign (customSpec fs (quadratic ["w", "s"]) 20 42)
                   { csStructure = splitPlot ["w"] 4 }
      case dsKind plan of
        KStructured [("wholePlot", ids)] _ -> do
          let raw = LA.fromLists (dsCoded plan)
              cfW = [ CF.Factor "w" (CF.Continuous (-1) 1) CF.Controllable
                    , CF.Factor "s" (CF.Continuous (-1) 1) CF.Controllable ]
              model = either (error . T.unpack) id
                        (formulaToCustomModel ["w", "s"] (quadratic ["w", "s"]))
              mInv = ST.buildMInvFromGroups 20 [(1.0, VS.fromList ids)]
          case CMd.expandDesignMatrix cfW model raw of
            Left e  -> expectationFailure (T.unpack e)
            Right x -> do
              let dval = LA.det (LA.tr x LA.<> (mInv LA.<> x))
              dval `shouldSatisfy` (>= 2684.0)
        k -> expectationFailure ("expected KStructured [(wholePlot,_)], got " ++ show k)

    -- Phase 79.3: StripPlot (交差 2 階層・whole-plot × strip)。
    it "StripPlot: 2 群列 (wholePlot, strip)・n = nWhole × nStrip" $ do
      let plan = customDesign (customSpec
                   [ contFactor "wp" (-1, 1), contFactor "st" (-1, 1), contFactor "sp" (-1, 1) ]
                   (mainEffects ["wp", "st", "sp"]) 12 7)
                   { csStructure = stripPlot ["wp"] 4 ["st"] 3 }
      length (dsCoded plan) `shouldBe` 12
      case dsKind plan of
        KStructured [("wholePlot", wpIds), ("strip", stIds)] _ -> do
          length wpIds `shouldBe` 12
          length stIds `shouldBe` 12
          length (nub wpIds) `shouldBe` 4
          length (nub stIds) `shouldBe` 3
        k -> expectationFailure ("expected KStructured [(wholePlot,_),(strip,_)], got " ++ show k)

    it "StripPlot: whole-plot 因子は WP 群内で一定・strip 因子は strip 群内で一定" $ do
      let plan = customDesign (customSpec
                   [ contFactor "wp" (-1, 1), contFactor "st" (-1, 1), contFactor "sp" (-1, 1) ]
                   (mainEffects ["wp", "st", "sp"]) 12 7)
                   { csStructure = stripPlot ["wp"] 4 ["st"] 3 }
      case dsKind plan of
        KStructured [("wholePlot", wpIds), ("strip", stIds)] _ -> do
          let colOf j   = [ row !! j | row <- dsCoded plan ]
              constIn ids col = all (\g -> let vs = [ c | (c, i) <- zip col ids, i == g ]
                                           in all (== head vs) vs) (nub ids)
          constIn wpIds (colOf 0) `shouldBe` True    -- wp (列 0) は WP 群内で一定
          constIn stIds (colOf 1) `shouldBe` True    -- st (列 1) は strip 群内で一定
        _ -> expectationFailure "expected KStructured [(wholePlot,_),(strip,_)]"

    it "StripPlot: n ≠ nWhole × nStrip は error" $ do
      let bad = customDesign (customSpec
                  [ contFactor "wp" (-1, 1), contFactor "st" (-1, 1) ]
                  (mainEffects ["wp", "st"]) 10 7)
                  { csStructure = stripPlot ["wp"] 4 ["st"] 3 }   -- 4×3=12 ≠ 10
      evaluate (length (dsCoded bad)) `shouldThrow` anyErrorCall

    -- Phase 79.4: Blocked (ランダムブロック)。 全因子はブロック内で自由・block は run 割付のみ。
    it "Blocked: KStructured [(block, ids)]・n run・nBlocks 群・因子列は block を含まない" $ do
      let plan = customDesign (customSpec
                   [ contFactor "x1" (-1, 1), contFactor "x2" (-1, 1) ]
                   (twoWay ["x1", "x2"]) 12 5) { csStructure = blocked 3 }
      length (dsCoded plan) `shouldBe` 12
      map length (dsCoded plan) `shouldSatisfy` all (== 2)   -- 因子は 2 列 (block は列でない)
      case dsKind plan of
        KStructured [("block", ids)] _ -> do
          length ids `shouldBe` 12
          length (nub ids) `shouldBe` 3
        k -> expectationFailure ("expected KStructured [(block,_)], got " ++ show k)

    it "Blocked: det(Xᵀ M⁻¹ X) > 0 (非特異・full-rank 設計) + seed 決定的" $ do
      let fs   = [ contFactor "x1" (-1, 1), contFactor "x2" (-1, 1) ]
          plan = customDesign (customSpec fs (twoWay ["x1", "x2"]) 12 5)
                   { csStructure = blocked 3 }
      dsCoded plan `shouldBe` dsCoded plan   -- pure
      case dsKind plan of
        KStructured [("block", ids)] _ -> do
          let raw = LA.fromLists (dsCoded plan)
              cfs = [ CF.Factor "x1" (CF.Continuous (-1) 1) CF.Controllable
                    , CF.Factor "x2" (CF.Continuous (-1) 1) CF.Controllable ]
              model = either (error . T.unpack) id
                        (formulaToCustomModel ["x1", "x2"] (twoWay ["x1", "x2"]))
              mInv = ST.buildMInvFromGroups 12 [(1.0, VS.fromList ids)]
          case CMd.expandDesignMatrix cfs model raw of
            Left e  -> expectationFailure (T.unpack e)
            Right x -> LA.det (LA.tr x LA.<> (mInv LA.<> x)) `shouldSatisfy` (> 0)
        _ -> expectationFailure "expected KStructured [(block,_)]"

    -- Phase 79.5: 制約 × 構造。 78.M では customDesign (制約) と splitPlotDesign (階層) が
    -- 別関数で両立不可だった本命ギャップ。 whole-plot 群内一定 かつ 制約満足を同時に満たす。
    it "SplitPlot + 線形制約: whole-plot 群内一定 かつ rate<=0.5 (coded) を全 run が満たす" $ do
      let cons = [ LinearIneq [("rate", 1)] CLeq 0.5 ]
          plan = customDesign (customSpec
                   [contFactor "temp" (150, 180), contFactor "rate" (-1, 1)]
                   (twoWay ["temp", "rate"]) 8 50)
                   { csStructure = splitPlot ["temp"] 4, csConstraints = cons }
      length (dsCoded plan) `shouldBe` 8
      case dsKind plan of
        KStructured [("wholePlot", ids)] _ -> do
          let tempCol = [ row !! 0 | row <- dsCoded plan ]     -- temp = 列 0 (whole-plot)
              rateCol = [ row !! 1 | row <- dsCoded plan ]     -- rate = 列 1 (sub-plot)
          -- (a) 制約: 全 run の rate <= 0.5
          all (<= 0.5 + 1e-9) rateCol `shouldBe` True
          -- (b) 階層: temp が whole-plot 群内で一定
          let byGroup g = [ t | (t, i) <- zip tempCol ids, i == g ]
          all (\g -> let vs = byGroup g in all (== head vs) vs) (nub ids)
            `shouldBe` True
        k -> expectationFailure ("expected KStructured [(wholePlot,_)], got " ++ show k)

    it "SplitPlot + 制約: seed 決定的 (pure)" $ do
      let cons = [ LinearIneq [("rate", 1)] CLeq 0.5 ]
          mk   = customDesign (customSpec
                   [contFactor "temp" (150, 180), contFactor "rate" (-1, 1)]
                   (twoWay ["temp", "rate"]) 8 50)
                   { csStructure = splitPlot ["temp"] 4, csConstraints = cons }
      dsCoded mk `shouldBe` dsCoded mk

    -- Phase 79.6: 複数群列 → 複数 ranIntercept の HBM round-trip。 StripPlot は 2 群列
    -- (wholePlot, strip) を designFrame に Text ラベルで出すので、 そのまま階層モデルに乗る。
    it "round-trip: StripPlot → designFrame に 2 群列 (Text)・両方 ranIntercept が当たる" $ do
      let plan = customDesign (customSpec
                   [ contFactor "wp" (-1, 1), contFactor "st" (-1, 1), contFactor "sp" (-1, 1) ]
                   (mainEffects ["wp", "st", "sp"]) 12 7)
                   { csStructure = stripPlot ["wp"] 4 ["st"] 3 }
          n   = length (dsCoded plan)
          ys  = [ 100 + 2 * fromIntegral i | i <- [1 .. n] ] :: [Double]
          df  = DX.insertColumn "y" (DX.fromList ys) (designFrame plan)
          m   = df |-> designModelHBM defaultHBM plan
                        [ranIntercept "wholePlot", ranIntercept "strip"] "y"
      -- 固定効果 beta が取り出せれば 2 階層 round-trip は地続きに成立 (happy-path 構造検証)
      length (dhfBetaNames m) `shouldSatisfy` (> 0)

  -- Phase 82: 自然単位の制約 (natLeq / natGeq / natForbid)。 実単位で書いた制約が
  -- customDesign 入口で coded へ正規化され、 designFrame の実値がその実単位境界を守る。
  describe "Phase 82: 自然単位の線形制約 (natLeq/natForbid)" $ do
    it "natLeq temp<=160 (Cont 150..180): designFrame の temp が全 run <= 160" $ do
      let plan  = customDesign (customSpec
                    [contFactor "temp" (150, 180), contFactor "time" (10, 20)]
                    (mainEffects ["temp", "time"]) 8 42)
                    { csNatConstraints = [natLeq [("temp", 1)] 160] }
          temps = maybe (error "temp 列なし") V.toList (getDoubleVec "temp" (designFrame plan))
      length temps `shouldBe` 8
      all (<= 160 + 1e-9) temps `shouldBe` True

    it "natLeq temp+time<=175 (実単位和): 全 run で temp+time <= 175" $ do
      let plan = customDesign (customSpec
                   [contFactor "temp" (150, 180), contFactor "time" (10, 20)]
                   (mainEffects ["temp", "time"]) 8 42)
                   { csNatConstraints = [natLeq [("temp", 1), ("time", 1)] 175] }
          df   = designFrame plan
          ts   = maybe (error "temp") V.toList (getDoubleVec "temp" df)
          tm   = maybe (error "time") V.toList (getDoubleVec "time" df)
      all (<= 175 + 1e-9) (zipWith (+) ts tm) `shouldBe` True

    it "natForbid: Num 因子を実水準値 (180) で禁止 → designFrame に 180 が出ない" $ do
      let plan = customDesign (customSpec
                   [numFactor "temp" [150, 165, 180], contFactor "time" (10, 20)]
                   (mainEffects ["temp", "time"]) 8 42)
                   { csNatConstraints = [natForbid [("temp", FVDouble 180)]] }
          temps = maybe (error "temp") V.toList (getDoubleVec "temp" (designFrame plan))
      notElem 180 temps `shouldBe` True
      all (`elem` [150, 165]) temps `shouldBe` True

    it "natLeq がカテゴリ因子を参照するとエラー (順序なし)" $ do
      let plan = customDesign (customSpec
                   [catFactor "cat" ["A", "B"], contFactor "x" (0, 1)]
                   (mainEffects ["cat", "x"]) 4 1)
                   { csNatConstraints = [natLeq [("cat", 1)] 0.5] }
      evaluate (length (dsCoded plan)) `shouldThrow` anyErrorCall

  -- Phase 82.2: 離散数値 (Num) 因子の実値閾値。 単一項 natLeq/natGeq/natEq は
  -- 「閾値を満たさない水準を除外」に展開される (半空間でなく水準フィルタ)。
  describe "Phase 82.2: 離散数値因子の実値閾値フィルタ (natLeq/natGeq Num)" $ do
    it "natLeq temp<=160 (Num 150/165/180): 全 run が 160 以下 (= 150 のみ残る)" $ do
      let plan  = customDesign (customSpec
                    [numFactor "temp" [150, 165, 180], contFactor "time" (10, 20)]
                    (mainEffects ["temp", "time"]) 8 42)
                    { csNatConstraints = [natLeq [("temp", 1)] 160] }
          temps = maybe (error "temp") V.toList (getDoubleVec "temp" (designFrame plan))
      all (<= 160 + 1e-9) temps `shouldBe` True
      all (== 150) temps `shouldBe` True

    it "natGeq temp>=165 (Num): 165/180 のみ残り 150 は消える" $ do
      let plan  = customDesign (customSpec
                    [numFactor "temp" [150, 165, 180], contFactor "time" (10, 20)]
                    (mainEffects ["temp", "time"]) 8 42)
                    { csNatConstraints = [natGeq [("temp", 1)] 165] }
          temps = maybe (error "temp") V.toList (getDoubleVec "temp" (designFrame plan))
      notElem 150 temps `shouldBe` True
      all (`elem` [165, 180]) temps `shouldBe` True

    it "係数付き natLeq 2·temp<=330 (Num): 実値換算で 165 以下 (150/165 が残る)" $ do
      let plan  = customDesign (customSpec
                    [numFactor "temp" [150, 165, 180], contFactor "time" (10, 20)]
                    (mainEffects ["temp", "time"]) 8 42)
                    { csNatConstraints = [natLeq [("temp", 2)] 330] }
          temps = maybe (error "temp") V.toList (getDoubleVec "temp" (designFrame plan))
      notElem 180 temps `shouldBe` True
      all (`elem` [150, 165]) temps `shouldBe` True

    it "Num 因子を他因子と線形結合するとエラー (単一項のみ)" $ do
      let plan = customDesign (customSpec
                   [numFactor "temp" [150, 165, 180], contFactor "time" (10, 20)]
                   (mainEffects ["temp", "time"]) 8 42)
                   { csNatConstraints = [natLeq [("temp", 1), ("time", 1)] 175] }
      evaluate (length (dsCoded plan)) `shouldThrow` anyErrorCall

    it "どの水準も満たさない閾値はエラー (temp<=100・水準 150..180)" $ do
      let plan = customDesign (customSpec
                   [numFactor "temp" [150, 165, 180], contFactor "time" (10, 20)]
                   (mainEffects ["temp", "time"]) 8 42)
                   { csNatConstraints = [natLeq [("temp", 1)] 100] }
      evaluate (length (dsCoded plan)) `shouldThrow` anyErrorCall

  -- Phase 82.3: 対数スケール連続因子 (contFactorLog)。 coded [-1,1] は不変だが自然単位へは
  -- 幾何的 (10^…) に写す。 中心点が幾何平均・自然単位制約も log 空間で載る。
  describe "Phase 82.3: 対数スケール連続因子 (contFactorLog)" $ do
    it "幾何 decode: 中心点 conc が幾何平均 (10^-0.5≈0.316)・算術平均 (5.005) でない" $ do
      let plan  = customDesign (customSpec
                    [contFactorLog "conc" (0.01, 10), contFactor "t" (10, 20)]
                    (quadratic ["conc", "t"]) 12 7)
          concs = maybe (error "conc 列なし") V.toList (getDoubleVec "conc" (designFrame plan))
          geoM  = 10 ** (-0.5)          -- 幾何平均
      -- 全 conc が範囲内 [0.01, 10]
      all (\x -> x >= 0.01 - 1e-9 && x <= 10 + 1e-9) concs `shouldBe` True
      -- 中心点は幾何平均 (0.316…) が現れ、 算術平均 (5.005) は現れない
      any (\x -> abs (x - geoM) < 1e-6) concs `shouldBe` True
      all (\x -> abs (x - 5.005) > 1e-3) concs `shouldBe` True

    it "natLeq conc<=0.1 (log因子・単一境界): 全 conc <= 0.1" $ do
      let plan  = customDesign (customSpec
                    [contFactorLog "conc" (0.01, 10), contFactor "t" (10, 20)]
                    (mainEffects ["conc", "t"]) 8 7)
                    { csNatConstraints = [natLeq [("conc", 1)] 0.1] }
          concs = maybe (error "conc") V.toList (getDoubleVec "conc" (designFrame plan))
      all (<= 0.1 + 1e-9) concs `shouldBe` True

    it "自然単位の線形結合に log 因子が混ざるとエラー (非線形)" $ do
      let plan = customDesign (customSpec
                   [contFactorLog "conc" (0.01, 10), contFactor "t" (10, 20)]
                   (mainEffects ["conc", "t"]) 8 7)
                   { csNatConstraints = [natLeq [("conc", 1), ("t", 1)] 15] }
      evaluate (length (dsCoded plan)) `shouldThrow` anyErrorCall

    it "実行不能エラーは有効な制約と因子範囲を実単位で添える" $ do
      -- temp∈[150,180] に temp<=100 は両立不能。 エラーに実単位の制約と範囲が出る。
      let plan = customDesign (customSpec
                   [contFactor "temp" (150, 180), contFactor "t" (10, 20)]
                   (mainEffects ["temp", "t"]) 6 1)
                   { csNatConstraints = [natLeq [("temp", 1)] 100] }
      evaluate (length (dsCoded plan)) `shouldThrow`
        (\(ErrorCall m) -> "1.0·temp ≤ 100.0" `isInfixOf` m
                             && "temp ∈ [150.0, 180.0]" `isInfixOf` m)

  -- designFrameRound: runsheet の桁数調整 (CCD 軸点の長い小数を丸める)。
  describe "designFrameRound (runsheet 桁数調整)" $ do
    it "連続因子の実値を小数第 n 位に丸める (CCD ±α 軸点)" $ do
      let plan  = centralCompositeDesign
                    [contFactor "temp" (150, 180), contFactor "time" (10, 20)]
          df    = designFrameRound 2 plan
          temps = maybe (error "temp 列なし") V.toList (getDoubleVec "temp" df)
      -- 軸点 143.78679… / 186.21320… が第 2 位に丸まる
      (143.79 `elem` temps) `shouldBe` True
      (186.21 `elem` temps) `shouldBe` True
      -- 全値が小数第 2 位以内 (x*100 が整数)
      all (\x -> abs (x * 100 - fromIntegral (round (x * 100) :: Integer)) < 1e-9) temps
        `shouldBe` True

    it "run 数は designFrame と同じ (丸めのみ・行は増減しない)" $ do
      let plan  = centralCompositeDesign [contFactor "temp" (150, 180), contFactor "time" (10, 20)]
          nrun df = maybe 0 V.length (getDoubleVec "run" df)
      nrun (designFrameRound 3 plan) `shouldBe` nrun (designFrame plan)
