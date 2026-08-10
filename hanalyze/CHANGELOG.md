# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [PVP](https://pvp.haskell.org/) versioning.

## [Unreleased]

### Added (Phase 78: DOE 整備 — 設計オブジェクト + runsheet + designModel + profiler grid)
- **階層ベイズ DOE `designModelHBM`** (Phase 78.G-f): `designModel` の階層 (mixed-effects) 版。 固定効果
  (含意 formula) に群のランダム効果 (部分プーリング) を加え、 lot/day/operator/batch 等の群間差を
  変量効果として扱う。 群は型付きランダム効果 `ranIntercept "lot"` (= lme4 `(1|lot)`) で指定
  (`ranSlope` は現状 error)。 推定は NUTS (`defaultHBM`)、 手書き `ModelP` を `hbm` で学習し、
  `MultiVarModel DesignHBMFit` instance で profiler/contour に **HBM 事後予測帯** (固定効果 β の draw 分散
  + 観測 noise σ²・群平均で marginalize) を開通する。 `designModelGP` が連続因子専用 (カテゴリは error) なのと
  逆に、 **カテゴリ/群列をランダム効果に活用**できるのが差別化。 `multiOutput` / `profiler` はそのまま。
  `Hanalyze.Plot` から再エクスポート。 解析側 (既存/sim データの群込み fit) が scope。
- **応答曲面 解析の自然単位報告** (Phase 78.G-d): `rsmAnalysis` / `steepestAscentNatural` を
  `Design.Workflow` に追加。 coded/uncoded の要点は「fit を coded でやること」ではない (同一項 LM は
  再パラメータ化で予測・R²・profiler が同値)。 実質的に coding が効くのは**スケール依存な最適化幾何**
  (停留点方向・canonical 軸・steepest ascent 方向) だけなので、 そこだけ内部で coded の計量を使い
  **結果を自然単位で報告**する。 `rsmAnalysis` は停留点 (自然単位)・性質 (`RMaximum`/`RMinimum`/`RSaddle`)・
  canonical (固有値+coded 方向)・実験領域内外判定・R² を返す (R `rsm::canonical` 相当)。
  `steepestAscentNatural` は coded 勾配方向 (scale 不変) の経路を各点自然単位へ decode して返す。
  低レベル側は `Design.RSM` に `canonicalAnalysis` (B 行列の固有値+固有ベクトル) / `quadBMatrix` を新設。
  いずれも連続因子専用。
- **一部実施要因カタログの k>7 拡張** (Phase 78.G-c): `fractionalCatalog` を NIST e-Handbook (§5.3.3.4.7) の
  16/32-run 標準設計で拡張 — 16-run: k=8 (Res IV) / k=9,10,11 (Res III) / k=15 (飽和 Res III)、
  32-run: k=9,10,11 (Res IV・16-run では Res III しか出ない帯)。 各 generator は NIST の defining relation を
  一次根拠に起こし、 `fracResolution` 自己検証 (解像度一致) + 主効果直交性 test で担保。 NIST 非収録の帯
  (16-run k=12〜14・32-run k=12〜16) は本カタログも持たない (踏襲)。
- **一部実施要因の交互作用 formula + alias 構造** (Phase 78.G-c): `fractionalDesignInter` (解像度自動) /
  `fractionalDesignGenInter` (generator 明示) を opt-in で追加。 設計点は `fractionalDesign` と同一だが、
  generator を保持する新 `DesignKind KFracInter` で `designFormula` が主効果に加え**主効果と交絡しない
  2 因子交互作用の代表** (交絡群ごと 1 個) を生成する。 alias 剰余類 (coset) の計算 (`fracResolution` の
  defining 部分群計算を `definingSubgroup`/`aliasCoset` に抽出・拡張) で Res V=全 2FI・Res IV=群ごと 1 個・
  Res III=主効果非交絡の 2FI のみ、 を一様に導く (満ランク・主効果不バイアス)。 `aliasStructure :: Design ->
  [(Text,[Text])]` を query として公開 (`lookup "a:b"` で交絡相手を引ける)。 既存 `fractionalDesign`/`Gen`
  (主効果のみ) は無改変 (後方互換)。 WorkflowSpec に 6 test (全解像度の 2FI 数・alias・飽和当てはめ p=8・R²~1)。
- **3 水準/混合 Taguchi 直交表 + 数値順序因子** (Phase 78.G-a2): `taguchiDesign` / `taguchiDesignOA` を
  3 水準 (L9(3⁴)/L27(3¹³)) と混合水準 (L18(2¹×3⁷)) に拡張。 各因子の要求水準数 (連続=2・数値順序/カテゴリ=水準数)
  を表の列水準に**貪欲突合**して割り当てる (混合表は 2 水準因子を 2 水準列へ・3 水準因子を 3 水準列へ)。
  `FactorKind` に **`Num [Double]`** (数値順序因子) を追加し smart constructor **`numFactor "temp" [150,165,180]`**。
  数値順序因子の formula は **直交多項式** `opoly(name, 水準数−1)` になり、 実測間隔で linear+quadratic を
  **直交分解** (raw べきと違い linear ⊥ quadratic ゆえ効果検定が独立・不等間隔でも正しい)。 R-formula に
  `opoly(x,n)` 基底 (Vandermonde の QR 直交化・R `poly()` 既定 raw=FALSE と同 span) を追加。 低レベル
  `Design.Orthogonal` に **L27** (GF(3) 線形形式で生成・強度 2 直交を test 保証)。 `designTable` は数値順序因子
  (numeric) を実水準値で載せる (カテゴリのみ error 継続)。 formula 76 + 本体 test 無回帰・WorkflowSpec に 10 test。
- **カテゴリ因子** (`contFactor` / `catFactor`・Phase 78.G-b2): `DesignFactor` を型手術し
  `DesignFactor = { dfName, dfKind }` / `FactorKind = Cont Double Double | Cat [Text]` の**因子レベル和型**へ
  (要素でなく因子を和型化し混在不正状態を型排除・`dfName` は total)。 全コンストラクタを
  `[(Text,(Double,Double))]` → **`[DesignFactor]`** に統一 (smart constructor `contFactor "temp" (150,180)` /
  `catFactor "cat" ["A","B","C"]`)。 `factorialDesign` は連続 (2 水準) × カテゴリ (m 水準) を総当り、
  `optimalDesign` はカテゴリ水準を候補格子に展開し contrast で最適化、 `fractionalDesign` は **binary カテゴリ
  のみ** (coded ±1)。 `centralCompositeDesign`/`boxBehnkenDesign`/`taguchiDesign` は連続専用 (カテゴリは error)。
  runsheet はカテゴリ列を持つため **`designFrame`** (Cont=Double 列 / Cat=Text 列) が正・数値専用の
  `designTable` はカテゴリで error。
- **R-formula の character→factor 自動判定** (Phase 78.G-b2 派生・formula engine 改修): `y ~ x + group` の
  `group` が非数値 (Text) 列なら **`!` 添字なしでも factor** として treatment contrast 展開する (R 本来の
  意味論)。 従来は `!` 添字限定で、 裸の Text 列は「連続変数が数値列として見つかりません」で error だった。
  `Frame.buildFrame` (非数値列を factor 集合に追加) + `Design.classify` (裸 factor 項を主効果に) の 2 点改修。
  従来 error だった経路だけを成功に変える**単調変更**ゆえ formula 76 / 本体 1249 test 無回帰。
- **DOE ワークフロー** (`Hanalyze.Design.Workflow`): 散在する低レベル設計関数を **設計
  オブジェクト `Design`** に束ね R 流の対話的 DOE に。 `factorialDesign`/`centralCompositeDesign` (pure・
  モデル formula を含意) → `designTable` で **uncoded runsheet** (実値・run 番号・ColumnSource)。
- **`designModel plan "y"`** (Fit spec): 設計 formula (要因=全交互作用 / RSM=2 次 `I(x^2)`) で
  既存 LM (`MultiLMModel`) を当てる。 同じ `plan` を sim データ・実物データに**使い回せる**。
- **`fractionalDesign specs res` / `fractionalDesignGen specs generators`** (Phase 78.G): **一部実施要因**
  (run 削減)。 完全要因 2^k の交互作用の一部を主効果と交絡させ **2^(k-p)** に減らす (7 因子 ResIII で
  128→8 run)。 解像度 (`Resolution` = ResIII..ResVII) 指定で **最小交絡 (minimum aberration)** の標準
  generator (Montgomery Table 8-14 / NIST・k=3〜7) を自動選択、 または generator 明示。 formula は
  **v1 は主効果のみ** (交互作用は交絡)。 ★generator 表は `fracResolution` (defining word 最短長) で
  解像度ラベルを test 自己検証 (誤り混入ガード)・主効果列の直交/平衡も test。
- **`taguchiDesign specs` / `taguchiDesignOA "L12" specs`** (Phase 78.G-a): **Taguchi 直交表**による
  多因子スクリーニング (v1 は 2 水準表 L4/L8/L12/L16)。 因子数に対し列数が足りる**最小 run の直交表を
  自動選択** (自動版)、 または表を名指し (escape hatch・`fractionalDesignGen` に対応)。 ★目玉は一部実施に
  無い **L12 (Plackett-Burman・11 因子/12 run)**。 `Design.Orthogonal` の 1-based level code を先頭 k 列で
  coded ±1 に写し `KFractional` (主効果のみ formula) の `Design` に包む — `designModel`/`designFrame` は
  共通経路。 主効果列の直交/平衡・自動選択の run 数境界・L12 主効果 LM 再現を test。 3 水準/カテゴリ
  (L9/L18) は型手術が要るため後段 (G-a2)。
- **`optimalDesign specs formula n`** (+ `optimalDesignLevels` / `optimalDesignWith`・Phase 78.G-b1):
  **最適計画** (D/A/I/E/G-最適・カスタムモデル)。 標準格子計画と逆で、 **当てたいモデル (formula) と
  run 数 `n` を先に決め**、 候補格子から情報行列 XᵀX の基準を最適化する `n` 点を Fedorov 交換で選ぶ
  (既定 = D-最適)。 モデルは **`Formula` に一本化** — 効果 DSL `mainEffects`/`twoWay`/`quadratic`
  (`[Text] -> Formula`) か `parseRFormula` 文字列。 候補水準は 2 次項ありで 3・他は 2 (自動)、
  明示は `optimalDesignLevels`。 基準/seed のフル指定は `optimalDesignWith` (既定 seed 42)。 `n` 必須・
  `n < p` は error。 実装は `candidateGrid`→`designMatrixF`→`Design.Optimal.optimalDesign` の**接着**で
  新規数値アルゴなし、 選択点 + formula を新 `DesignKind KCustom Formula` に焼き込み `designModel`/
  `designFrame` 共通経路に載せる。 run 数/水準/直交 full factorial/`n<p` error/designModel 当てはめを test。
  ★v1 は**連続因子のみ** (カテゴリは型手術を伴い後段 G-b2)。
- **`boxBehnkenDesign specs`** (Phase 78.G): Box-Behnken 応答曲面計画 (k=3,4,5)。 CCD より run が
  少なく **極端な軸点 (±α) を持たない** (全点が立方体の辺中点・因子は −1/0/+1 の 3 水準) 2 次モデル
  RSM。 既存低レベル `boxBehnken` を `Design` ワークフローに包む (`KRSM` = 2 次 formula 含意)。
  3 因子で 12 corner + 中心点 = 15 run。 `designModel`/`profiler`/`designFrame` はそのまま使える。
- **`designFrame plan`** (Phase 78.H): runsheet を整形表 (`DataFrame`) にするヘルパー
  (`= toFrame . designTable`)。 `print (designFrame plan)` で **型付き ASCII テーブル**として設計の
  run を確認できる (完全要因=4隅・CCD=cube+軸点+中心が一目で分かる)。 docs 09-doe に型付き API 表 +
  runsheet 整形表の出力例を追加。 ★factorial=完全 2^k・RSM=完全 CCD・2 水準のみ (削減/多水準は今後)。
- **`multiOutput ys mkSpec`** (Phase 78.F): 複数応答を 1 動詞で当てはめる汎用 Fit コンビネータ。
  `df |-> multiOutput ["strength","yield"] (designModel plan)` → `[(応答名, MultiLMModel)]`。
  `designModel plan :: Text -> spec` がカレー化済ゆえ接着剤なし。 designModel 専用でなく任意 spec で可。
- **`profiler models factors` + `profilerResidual` + `ProfilerSpec`** (Phase 78.F): JMP Prediction
  Profiler 相当を **Plottable 中間型**に再設計 (HBM `epred` 流)。 `noDf |>> toPlot (profiler model
  ["temp","time"] <> profilerResidual Partial)`。 **行=応答 × 列=因子**のグリッド (複数応答対応)、
  `<>` で打点モード合成 (`Raw` 実測 / `Partial` 偏残差 @fⱼ(xⱼ)+全モデル残差@・R `crPlots` 相当)。
  打点はモデル観測値から算出 (`noDf` で束ねられる)。 ★旧 `profilerOf`/`profilerPartialOf` は撤去。
- **`contourOf model "x1" "x2"`** (Phase 78.E): 2 因子の **RSM 等高線 / 応答曲面**。 応答面 grid を
  評価し塗り等値帯 (`contourFilled`) + 等高線 (`contour`) で俯瞰 (R `rsm::contour` 相当・他因子は中央値
  固定)。 3D 応答曲面は既存 `surfaceOf`。
- docs: `09-doe.md` を workflow 語り (組む→sim/試作→精度確認→反復) に再構成・profiler 図 (生点/偏残差) を掲載。
- ★モデルは v1 で LM。 GP/RFF・HBM 化は将来 TODO。

### Changed (Phase 78.L: Taguchi 直交表の指定を列挙型に — ★破壊的)
- **`taguchiDesignOA` の表指定を文字列 → 列挙型 `OATable`** (`L4`/`L8`/`L9`/`L12`/`L16`/`L18`/`L27`) に。
  `taguchiDesignOA :: OATable -> [DesignFactor] -> Design` (旧 `Text -> …`)。 打ち間違い (`"L10"` 等) が
  **コンパイル時に弾かれ**、 未知表名の実行時 error が無くなる。 低レベル `Design.Orthogonal.lookupOA`
  (`Text -> Maybe OA`) は据置。
- docs: 09-doe.md の **低レベル API 節を `docs/internal/09-doe-lowlevel.md` へ移設** (api-guide は高レベル
  ワークフロー主役に集中・低レベルは内部リファレンスへ pointer リンク)。

### Added (Phase 78.K: 設計の保存 / DataFrame からの復元)
- **`saveDesign :: FilePath -> Design -> IO ()`** — runsheet (`designFrame`) を CSV に書き出す
  (実験者へ渡す runsheet の保存経路)。
- **`planFromFrame :: [DesignFactor] -> Formula -> DataFrame -> Design`** — DataFrame から `Design` を復元
  (因子 + モデル formula を明示・各因子列を coded 化して `KCustom` 設計に)。 CSV から読み戻した runsheet を
  `designModel` / `rsmAnalysis` に載せ直せる。 いずれも `Hanalyze.Plot` から利用可。
- **DOE ワークフロー API を `Hanalyze.Plot` から全面 re-export** — `numFactor` / `taguchiDesign` /
  `optimalDesign` 系 / `mainEffects`/`twoWay`/`quadratic` / `rsmAnalysis` / `steepestAscentNatural` /
  `aliasStructure` / `fractionalDesignInter` 系 等、 従来 `Design.Workflow` 直 import が必要だった関数を
  `Hanalyze.Plot` / `Fit` から利用可能に (単一 import で DOE 全体をカバー)。

### Added (Phase 78.J: DOE モデル/可視化の拡充 — HBM 診断露出・3D・modelFor)
- **`DesignHBMFit` に `dhfModel :: HBMModel` を追加** — 学習済 HBM を保持し、 `dagOf` / `tracesOf` /
  `ppcOf` / `energyOf` 等の診断抽出子を `designModelHBM` の結果からも使えるように (`dagOf (dhfModel m)`)。
- **`modelFor :: Text -> [(Text,m)] -> m`** — `multiOutput` の結果から応答名でモデルを 1 つ取り出す
  selector。 `contourOf` / `surfaceOf` に `snd (head model)` でなく `modelFor "strength" model` で渡せる。
- docs `09-doe.md` モデル章に LM 節、 可視化章に 3D 応答曲面 (`surfaceOf` + `saveSVG3D` / WebGL
  `saveHTML3D` / `showBrowser`) と HBM 診断節を追加。 `designModelHBM` の固定効果自動展開を明記。

### Changed (Phase 78.I: 最適計画の点反復対応 — exact D-最適)
- **`optimalDesign` / 低レベル `Design.Optimal.optimalDesign` が点の反復を許す exact 最適計画に**。 従来は
  候補格子から**相異なる点のみ**選び `n` が候補点数を超えると頭打ちだった。 Fedorov 交換で同一候補の反復を
  許し、 初期選択も候補を循環して `n` 点作るよう修正。 これにより (1) `n > 候補点数` でも頭打ちせず `n` run を
  返す、 (2) 反復が D を上げる場合は `n <= 候補点数` でも反復する (真の exact D-最適・JMP/AlgDesign と同挙動)。
  既存の run 数・直交/平衡の契約は不変。

### Changed (Phase 78.I: `rsmDesign` → `centralCompositeDesign` リネーム — ★破壊的)
- **応答曲面計画の CCD コンストラクタ `rsmDesign` を `centralCompositeDesign` に改名**。 他の設計種別名
  コンストラクタ (`factorialDesign`/`boxBehnkenDesign` 等) と命名を揃え、 RSM は CCD/BBD を含む方法論の
  呼称に格上げ。 動作・型・含意 formula は不変 (`[DesignFactor] -> Design`)。 低レベル
  `Design.RSM.centralComposite` とは別物 (高レベル workflow 版)。
- docs `09-doe.md` を返り値別 API 一覧 + ワークフロー/計画/モデル/可視化 章に再構成、 計画各サブ章に
  `designFrame` の実出力表を掲載 (Phase 78.I)。

### Added (Phase 77: 因果探索 LiNGAM 高レベル API + plot カバレッジ)
- **全 7 variant の高レベル `df |->` API** (Direct/Parce/ICA/VAR/MultiGroup/Pairwise/Bootstrap):
  `directLingam cfg cols` / `parceLingam` / `icaLingam` / `varLingam` / `multiGroupLingam cfg cols groupCol`
  / `pairwiseLingam thr xcol ycol` / `bootstrapLingam`。 従来は生 `LA.Matrix` 入力のみだった。
- **名前付き DAG** (77.A): 汎用ラッパ `LiNGAMFitted a` (fit + varNames) で各 fit 型を無改修のまま
  変数名を保持し、 `toPlot` が `x0/x1/x2` でなく**実変数名**で DAG を描く。 `lingamDagNamed` を全 variant 共有。
- **variant 別 plot**: VAR = **時間ラグ DAG** (`varLagDagNamed`・同時刻 + `x_j[t-l]→x_i[t]`)、
  Pairwise = 2 変数の向き、 Bootstrap = **確信度 DAG** (出現確率≥0.5) + `bootstrapEdgeProbOf`
  **エッジ確率ヒートマップ**、 MultiGroup = 群分割の共通 DAG。
- **IO variant の seed 純粋化** (77.C): `fitBootstrapLiNGAMPure` (runST) / `fitICAPure`+`fitICALiNGAMPure`
  (`fitICA` を `fitICAGen` で PrimMonad 一般化・IORef→MutVar)。 同 seed で IO 版と**ビット一致**。
- docs: `08-causal.md` を全 variant + 高レベル API へ拡充 (ペアプロットで相関≠因果の対比・計算量 O(p³·n) の実測目安)。

### Added (Phase 76: mark 拡充 — 決定領域塗り / クラスタ囲み / dendrogram)
- **決定領域のカテゴリ塗り分け** (76.A): `decisionBoundaryOf` を `res×res` グリッド予測 →
  各セルを予測クラス色の塗り矩形 (`annotRectP`) で敷き詰める実描画へ (旧「四角散布=縞模様」を
  解消・sklearn `DecisionBoundaryDisplay` 相当)。 クラス色は render の categorical 順
  (`sort.nub`→`ggplotHue`) を再現し `toPlot` 凡例と一致。 `coordCartesian` で軸をグリッド範囲へ
  固定しフレーム外はみ出しを防止。 k-NN/LDA/NB/SVM(非線形)/MLP(非線形) で描画。
- **クラスタを囲む** (76.B): `clusterHullOf` (Andrew monotone chain 凸包の輪郭・ggplot
  `geom_encircle` 相当) / `clusterEllipseOf` (群 μ/Σ を `LA.eigSH` 固有分解し χ²(0.95,2)=5.991 の
  95% 楕円折れ線・`stat_ellipse` 相当)。 群色は `clusterScatterOf` と一致。 輪郭線のみ
  (annotation 制約・塗りは将来 `MPolygon` 移譲)。
- **dendrogram** (76.C): `instance Plottable HClusterFit` + `dendrogramOf` / `dendrogramOf'`。
  `hcMerges`/`hcHeights` から U 字リンクを描画・葉は根からの DFS で交差なし配置・`DendroOpts`
  の色閾値で葉クラスタを色分け (scipy `color_threshold` 流・`cutTree`)。 スタイルは R base
  `hclust` 準拠 (grid/枠/x軸線/tickマークなし・y軸線のみ・葉ラベル縦書き)。 R/scipy と Ward
  高さ完全一致を突合済。
- docs: `stat/05-cluster` にクラスタ囲み (hull/ellipse) + 階層クラスタリング (dendrogram) 節を追加。
  api-guide 05-ml の決定境界「未実装」 記述を実装済み (領域塗り) へ更新。
- 実装形態: 新規 plot mark を足さず **analyze 側の annotation** で実装 (treePlot 前例)。
  回転規約 (CCW 正化) は plot future phase へ記録。

### Added (Phase 75: 機械学習 (05-ml) の可視化カバレッジ拡充)
- **RandomForest 重要度図**を api-guide 05-ml に掲載 + title 付与 (GBM と同流儀で demo 側付与・75.1)。
- **混同行列 `confusionOf` のセルにカウント数値**を追加 (背景 box 付き label・viridis のどのセル色
  でも可読・sklearn `ConfusionMatrixDisplay` 同型・75.2)。
- **SVM / 分類 NN を分類器種非依存の図機構へ**: `ClassPredict SVMBinary`/`SVMMulti`/`MLPFit`
  instance で `decisionBoundaryOf` / `confusionOf` が SVM・NN でも動く (75.3/75.5)。
- **`mdsScatterOf`** (`LA.Matrix Double -> Maybe [Int] -> VisualSpec`) = MDS 埋め込み行列の先頭
  2 次元散布・`Just labels` でクラス色分け (75.4)。
- **`nnLossOf`** (`MLPFit -> VisualSpec`) = NN 学習損失曲線 (`mlpLossHist`・keras `history` 同型・75.5)。
- docs: 05-ml「その他 (数値フィットのみ)」 を「SVM/MDS/NN (低レベル fit + 可視化)」 へ書き換え・
  図 (svm-decision-boundary / mds-scatter / nn-loss / rf-importance) を gen-doc-figures + 本文に追加。
- docs: NN が **古典 MLP (フィードフォワード + 誤差逆伝播 + mini-batch Adam + L2)** であり
  CNN/RNN/Transformer・GPU は対象外であることを明記 (過度な期待回避・JMP Neural と同等範囲)。
- **拡張 (75.7-75.13)**:
  - NB 決定境界図を docs 掲載 (Plottable/ClassPredict 既存)。
  - **乱数純粋化**: `fitSVMPure`/`fitSVMMultiPure` (SVM は乱数不使用ゆえ seed 不要・決定的・
    `runLBFGSWithPure` 切り出し) / `fitMLPClassifierPure`/`fitMLPRegressorPure` (Word32 seed・
    `runST`+MWC・PrimMonad 一般化 = `fitRFVPure`/`nutsPure` 同方針)。 既存 IO API 後方互換。
  - **高レベル `df |->`**: `svmClsOf` (多クラス one-vs-rest・`SVMMulti`)/`mlpClsOf`/`mlpRegOf`/
    `kernelSvmOf` + Fit instance。 MDS は `mdsPlotOf df cols (Maybe labelCol)` (df 直結 plot ヘルパ)。
  - **カーネル SVM** (新 `Hanalyze.Model.KernelSVM`): 双対 C-SVC を SMO (Platt・第2変数
    |Ei-Ej| 最大=決定的・乱数不使用) で解く。 RBF/poly/linear カーネル + Gram 行列 + α>0 の
    **スパースな真のサポートベクタ**。 `ClassPredict` で非線形決定境界・`svmSupportVectorsOf`
    で SV 強調。 既存線形 SVM と共存 (sklearn LinearSVC vs SVC 同様)。
  - **`decisionLineOf`** (`ScorePredict c => c -> range -> range -> Int -> VisualSpec`): 決定境界を
    **線** (決定スコア=0 の等高線・marching squares) で描く (`decisionBoundaryOf` の色塗りでなく
    曲線・sklearn `contour(levels=[0])` 相当・スコアベースで滑らか)。 SVMBinary/KernelSVM に対応。
- 描画層のみ (75.1-75.7) + 純粋化/高レベル/カーネル SVM は数値核追加・サンプラ非影響。 `(hanalyze-portable)`

### Changed (Phase 75 拡張2: SVM カーネル統合 + 高レベル ML API 命名統一 — ★破壊的)
- **共有カーネルに Linear/Poly 追加** (75.14): `Hanalyze.Model.GP.Kernel` に `Linear`
  (`σ_f² x·x'`) と `Poly d` (`(γ x·x' + 1)^d`, `γ=1/(2ℓ²)`) を追加。 全カーネル法 (GP/KRR/RFF/SVM)
  が 1 つの語彙を共有。 内積カーネルの multi-input gram は `buildKernelMatrixMV` が内積経路へ分岐・
  汎用評価 `kEvalMV` を追加 (距離関数 `applyKernel`/`kernelOfParams` は距離専用のまま)。
- **SVM をカーネル SVM 一本化** (75.15): `Hanalyze.Model.KernelSVM` を共有 `Kernel` + `GPParams`
  へ書換 (独自 `KRBF`/`KPoly`/`KLinear` 撤去・γ は `gpLengthScale` から `γ=1/(2ℓ²)`)。 既定カーネルは
  `Linear`。 **線形 L2-SVM `Hanalyze.Model.SVM` を完全削除** (`fitSVM`/`SVMBinary`/`SVMMulti`/
  `defaultSVMConfig` + 高レベル `svmClsOf` + 図 `svm-decision-boundary` + test/demo・後方互換なし)。
- **高レベル ML API を R 流命名へ統一・`*Of` 全廃** (75.16):
  `rfOf→randomForest` / `nbOf→naiveBayes` / `dtOf→decisionTree` / `gbmRegOf→gbmReg` /
  `gbmClsOf→gbmCls` / `knnClsOf→knnCls` / `knnRegOf→knnReg` / `ldaOf→lda` / `pcaOf→pca` /
  `plsOf→pls` / `kmeansOf→kmeans` / `mlpClsOf→mlpCls` / `mlpRegOf→mlpReg` /
  (`svmClsOf`+`kernelSvmOf`)→`svmCls` (kernel は config で選択)。 回帰 spec も R 整合:
  `quantile→rq` (+`quantileMulti→rqMulti`・`quantreg::rq`) / `robust→rlm` (+`robustMulti→rlmMulti`・
  `MASS::rlm`)。 `spline` は据置・`Cls`/`Reg` 接尾辞は維持。
- **config 既定値を短縮** (75.16): `defaultRFConfig→defaultRandomForest` /
  `defaultDTConfig→defaultDecisionTree` / `defaultGBConfig→defaultGBM` /
  `defaultKMeansConfig→defaultKMeans` / `defaultMLPConfig→defaultMLP` / `defaultPLSConfig→defaultPLS` /
  `defaultKernelSVMConfig→defaultSVM` (型名 `XxxConfig` は不変)。
- docs: `docs/api-guide/05-ml.md` を全面改訂 (R 流命名・SVM 一本化・kernel 選択 KRR 流・config 中身明記・
  引数の意味 [特徴列/クラス列]・xlo/xhi 具体値・MDS の Maybe 理由・SVM/MDS/NN を独立節へ)。
- ★**後方互換は取らない** (正しい実装優先)。 別 repo (canvas/stream) が旧名を使う場合は別途追従。
  数値核の変更は Kernel 追加と SVM 配線のみ・既存サンプラ/GP の数値は不変。 `(hanalyze-portable)`

### Changed (Phase 75 拡張3: Kernel 分離 / SVM 命名是正・自動最適化 / MDS API 再設計 / docs 是正 — ★破壊的)
- **共有カーネルを独立モジュールへ分離** (75.18): GP 族の共有カーネル (`Kernel` 型 +
  新 `KernelParams` 型 + `kernelFn`/`kEvalMV`/`applyKernel`/`kernelOfParams`/`ardScaleXY`/
  `buildKernelMatrix(MV)`) を `Hanalyze.Model.GP` から新 `Hanalyze.Model.Kernel` へ移動。
  既存の MDS でない方の `Model.Kernel` (NW/kernel-ridge 回帰) は `Model.KernelRegression` へ改名。
  `GPParams` は据置 (5 フィールド)・`gpKernelParams` で `KernelParams` へ射影。 GP は後方互換のため
  re-export。 **SVM は `GPParams` 依存を解消し `KernelParams` のみ持つ** (GP を import しない)。 数値不変。
- **SVM の脱「Kernel」命名** (75.19): 線形 L2-SVM 削除済で「Kernel」接頭辞は残骸ゆえ撤去。
  `Model.KernelSVM→Model.SVM` / `KernelSVMConfig→SVMConfig` / `KernelSVM→SVM` / `KernelSVMMulti→SVMMulti` /
  `fitKernelSVM*→fitSVM*` / `predictKernelSVM*→predictSVM*` / フィールド `ksvm*→svm*` /
  spec `KernelSVMSpec→SVMSpec`。 `defaultSVM`/`svmCls` は据置。
- **SVM の CV 自動最適化** (75.20・Added): `tuneSVM` (C×kernel×ℓ を k-fold CV accuracy 最大で選ぶ
  格子探索・固定 seed の `Stat.CV.kFold` を `runST`・決定的) + `SVMTuneGrid`/`defaultSVMTuneGrid` +
  高レベル `svmClsTuned`。 sklearn `GridSearchCV` / R `e1071::tune.svm` 相当。
- **MDS API 再設計** (75.21): MDS を他の fit と同型に — 教師なし変換ゆえ `|->` に乗り **モデル型
  `MDSResult`** (PCAResult 同格) を返す (df 型をやめる)。 `MDSConfig{mdsMethod,mdsSammon}` レコード +
  `defaultMDS`・`mds` spec・`Plottable MDSResult` (toPlot=単色散布)・群色は `toPlot (mdsView m <>
  mdsGroupBy "g")`。 旧 `mdsPlotOf`/`mdsScatterOf` (Maybe・特別扱い) を撤去。 低レベル `mdsClassical`/
  `mdsSammon`/`euclideanDist` は `Stat.MDS` に残置。
- docs 是正 (75.22): 05-ml の config フィールドを**実名 + 構築例**に (`rfNumTrees`等の憶測フィールド全廃)・
  02-regression を `rq`/`rlm` 系へ・`decisionBoundaryOf`/`decisionLineOf`/`confusionOf`/
  `svmSupportVectorsOf` を初出で説明・MDS の意義 (距離保存 2D・PCA との違い・図の読み方) を明記・
  **NN 決定境界図 `nn-decision-boundary.svg` を主役に**追加 (loss は副次)。
- ★**後方互換は取らない**。 SVM が GP を import しない・SVM 型/関数から「Kernel」接頭辞が消える。
  数値核は移動のみ (Kernel 分離) + SVM CV 調律の追加・既存サンプラ/GP/SVM 推論は不変。 `(hanalyze-portable)`

### Changed (Phase 74.10: ppcOf のプール赤線を削除)
- `ppcOf` (`buildPPCSpec`) からプール y_rep (全 draw 連結) の赤破線を削除。 KDE の Silverman
  バンド幅が **n 依存** (`h ∝ n^(-0.2)`) ゆえ、 n=Σ(draw×n_obs) のプールは観測 (n=n_obs) より
  バンド幅が小さく過小平滑になり、 観測と異なる形 (外側へ膨らむ) に見えて誤解を招いた。
  比較は観測 (黒) vs 各 draw の y_rep (青・同じ n) で行うべきもので、 ArviZ `plot_ppc` も
  プール KDE は描かない。 図は観測 + 各 draw y_rep のみになった (層数 = draw+1)。

### Changed (Phase 74: HBM 診断抽出子 API 統一 — ★破壊的)
- HBM trace 抽出子を `tracesOf` / `tracesOfWith` (+ `TraceOpts`) に統一し、 旧
  `traceOf` (`[ChainModel]`) / `tracesByChainOf` / `tracesWithDivergencesOf` の 3 本を
  撤去 (ゲート `plot-integration`・`Hanalyze.Plot.Bayes`)。 戻り型を兄弟抽出子と
  同じ `[VisualSpec]` に揃え、 `subplots`/`vconcat (tracesOf m)` で param ごと独立パネルに
  描ける (旧 docs の `foldMap toPlot` = 全 param を 1 軸に重畳する誤りを排除)。
  `TraceOpts { toShowDivergences, toByChain }` で発散 rug on/off と chain 別重畳を切替
  (`ppcOf`/`PPCConfig` と同じ「関数 + config」 慣用)。 発散 rug は既定 ON。
  - 移行: `traceOf m` (+ `foldMap toPlot`) → `subplots (tracesOf m) <> subplotCols 1`、
    `tracesByChainOf m` / `tracesWithDivergencesOf m` →
    `tracesOfWith defaultTraceOpts { toByChain = True } m`。
  - サンプラ非影響 (描画層のみ)。 `(hanalyze-portable)`
  - docs: 発散 rug の実例図 `hbm-trace-divergent.svg` を追加 (中心化 8-schools・funnel で
    NUTS が実測 157 発散・viz-diagnostics に掲載)。
  - docs: 診断抽出子 API↔図の 1:1 を満たすため未掲載だった `pairOf` / `energyOf` /
    `marginalsOf` の図 (`hbm-pair.svg` / `hbm-energy.svg` / `hbm-marginals.svg`) を追加
    (pairOf/energyOf は funnel fit から生成・api-guide 03 / viz-diagnostics に掲載)。

### Added (Phase 74.8: HBM 診断ダッシュボード関数 + api-guide 03 診断図の再構成)
- `dashboardOf` / `dashboardFullOf` (`HBMModel -> Text -> VisualSpec`・引数 = observe 名) を追加。
  `dashboardOf` = コンパクト 2×2 (構造 `dagOf` 左上 / 推定値 `forestOf` / 当てはまり `ppcOf` /
  サンプラ健全性 `energyOf`)・各 1 パネルで param 数に依らず見やすい。 `dashboardFullOf` =
  上段に同じ 2×2、 その下に param ごと [事後分布 | trace] を 2 列で連結 (ArviZ `plot_trace` 流)・
  **係数が増えると下に行が増えるだけ** (高さ ∝ 2 + param 数)。 旧 demo の手組み 5 列を関数化。
  autocorr/rank はダッシュボードに入れない (mixing は trace・BFMI は energy で見えるため)。
- `energyOf` の凡例を図内 (右上・`LegendInsideTopRight`) へ移動。 外・右だと右に余白が出て
  単体図・dashboard が不格好になるため (density は中央が高く右裾 0 ゆえ右上が空く)。
- `traceDensityOf` (`HBMModel -> VisualSpec`) を追加 = trace + 事後分布だけのダッシュボード
  (ArviZ `plot_trace` 相当・param ごと [事後分布 | trace] を 2 列・chain 色違い)。 `dashboardFullOf`
  の下段を単体図として切り出したもの (共通部 `tracePostPanels` を共有)。

### Added (Phase 74.9: 学習前 DAG = ModelP から直接・サンプリングなし)
- `dagOfModel` (`ModelP () -> DagSpec`) / `dagOfModelWith` (`[(Text,[Double])] -> ModelP () -> DagSpec`)
  を追加。 `dagOf` が学習済 `HBMModel` を取るのに対し、 **fit (NUTS) せず**に生の `ModelP` から
  構造 DAG を描く (PyMC `pm.model_to_graphviz(model)` 相当・「fit 前にモデルの形を見る」 用)。
  DAG は事後に依らないので `|->` 経由のサンプリングが不要になる。
  - データ駆動 plate (`plateForM_` / `observeColumns` で plate サイズをデータ長から決める) は
    データ未束縛だとループ本体 (mu/obs) が出ないため `dagOfModelWith` でデータを束ねてから描く
    (`bindCols` = `hbmModel` と同じ束ね方・ただし NUTS は走らない)。 明示 plate (`plate name N`/
    `plateI`) のモデルは `dagOfModel` だけで完全に出る。
- docs: api-guide 03 の診断図節を PyMC/ArviZ ワークフロー頻度順に再構成し、 図と読み方を
  1 単位で配置。 表に ArviZ 相当列を追加。 ダッシュボードを表の直後に置き、 epred を末尾で
  CI/PI/CIPI まで厚く記述。 唐突だった「ppcOf は純粋…」 の一文を撤去。
- docs/demo: `subplots ss <> subplotCols 1` を既存の `vconcat ss` に統一 (冗長な括弧も除去)。
- 図 `hbm-dashboard.svg` (compact) を更新・`hbm-dashboard-full.svg` を追加。
- サンプラ非影響 (描画層のみ)。 `(hanalyze-portable)`

### Added (Phase 74.5: epred の予測区間 (PI) 帯 — 頻度論と CI/PI 対称化)
- `epred` が頻度論モデルと同綴りの `bandMode` を honor するようになり、 `<> bandMode BandCI`
  (既定・μ の事後 HDI = CI 相当) / `BandPI` (観測ノイズ込みの予測区間) / `BandCIPI`
  (外 PI 薄 + 内 CI 濃 の入れ子ファンチャート) / `BandOff` (線のみ) を切り替えられる。
  `epred` の型・export・既定 (CI) は不変・後方互換。
  - PI は観測ノードの予測分布を `runObserveDists` でモデルから**自動検出**し (obs 名引数なし =
    頻度論 `svGridPI` と対称)、 各 draw の分布から `sampleDist` で y をプール → HDI。 任意の
    観測分布 (Normal/Poisson/NegBinom…) に効く。 サンプリングは `runST` + 固定 seed (42・
    `ppcOf` と同方式) で純粋・決定的。 非軸 hold / `byVar` は本体機能を再利用。
  - docs: api-guide 03 / viz-diagnostics に CI/PI/CIPI 例 + 図 `hbm-epred-pi.svg` を追加。
  - サンプラ非影響 (描画層のみ)。 `(hanalyze-portable)`

### Added (Phase 74: epred の多予測子 hold)
- `epred` が `holdAt` / `byVar` (既存 `Hanalyze.Plot.Core` コンビネータ) を honor
  するようになり、 多予測子モデルで軸にしない予測子を固定できる。 `holdAt Median` /
  `holdAt (Fixed [(slot, 値)])` で非軸を集約値/任意値に、 `byVar slot [水準…]` で第2予測子の
  水準別に曲線を色分け重畳 (頻度論 effect plot `statModelMulti` と同綴り)。 非軸の既定が
  bind データ先頭値 (`head`) → 平均 (`Mean`) に変化 (単予測子モデルは μ が非軸非依存ゆえ図不変)。
  `(hanalyze-portable)`

### Added (Phase 73: HBM 診断ギャップ + docs 再編)
- HBM 事後診断の抽出子を 2 種追加 (ゲート `plot-integration`・`Hanalyze.Plot.Bayes`):
  - `autocorrOf` — パラメータ別の自己相関 (mixing 診断)。
  - `rankOf` — rank plot (chain 一様性・要 ≥2 chain)。
  - 縦 1 列 subplots の総高を param 数比例にする demo 修正 (固定キャンバスで潰れていた・
    ArviZ の height ∝ rows と同方針)。 `colorBy` の自動凡例は `legendOff` で抑制。
- plate notation に `plateI_` を追加 (`Hanalyze.Model.HBM`)。 `plateI` の返り値破棄版で
  観測のみ index ループ向け (`plateForM`/`plateForM_` と同じ 結果あり/破棄 × index/行リストの
  2×2 対称を補完)。 `plate name n (forM_ [0..n-1] f)` の糖衣・サンプラ不変。 `(hanalyze-portable)`

### Changed (Phase 73: docs 再編 — api-guide へ API 集約)
- `docs/api-guide/` を「型 + 最小例の辞書」、 `docs/<topic>/usage-*.ja.md` を
  「理論 + 罠の学習ガイド」 に役割分離。 各 topic の usage にあった API を対応
  api-guide ページへ集約し、 usage は学習要素に純化:
  - causal→08 / regression→02 / ml→05 / stat→10 / timeseries→06 / survival→07 /
    doe→09 (Custom Design / Augment / Split-Plot / Bayesian D / 古典拡張・新規集約) /
    fda→04-multivariate (新規節)。
  - これまで api-guide 未掲載だった機能を集約: FitYByX/Friedman/Dunn/cohenDCI/LCA/
    Graphical Lasso (10)、 StateSpace/Kalman (06)、 CompetingRisks/RBD (07)、
    Custom Design 全群 (09)、 FDA 全群 (04)。
  - api-guide (ja 単一言語) → usage の一次根拠リンクを `.ja.md` に統一。 リンク切れ 0。
- EN 版 `usage-*.md` の純化は後回し (別途同期)。 docs のみ・サンプラ/数値は不変。

### Added (Phase 72: 回帰診断の拡充)
- 係数診断を重回帰以外へ拡張。 非ゲート `Hanalyze.Diagnostics`:
  - `coefSummaryBoot` (`HasCoefBoot`) — 分位点・罰則回帰の bootstrap 係数サマリ
    (seed 固定純粋・返り値は `coefSummary` と同型 `[CoefRow]`)。
  - `termSummary` (`HasTermSummary`) — GAM/spline の平滑項単位の近似有意性
    (mgcv 流 edf + 近似 F・`TermRow`)。 spline は R `splines::bs`+`anova` と byte-exact。
  - `modelReport` / `showReport` (`HasReport`・`ModelReport`) — `.summary()` 風の統一玄関。
- 回帰診断の可視化 (ゲート `plot-integration`・`Hanalyze.Plot.Core`):
  - `obsVsPred` (`HasObsPred`・`obsPredPairs`) — 実測 vs 予測散布 + y=x 参照線。
    instance = LM/重回帰/GLM/WLS/ロバスト/分位点(中央値)/spline/GAM/罰則。
  - `coefForest` — 係数 forest plot (点推定 + 95% CI バー + 0 基準線・既存 forest mark 再利用)。
- 検証: 本体 1189 / test-plot 310→317 pass。 statsmodels QuantReg case bootstrap と
  SE 相対差<0.5%。 全て `(hanalyze-portable)`。

### Changed (Phase 64: module namespace 全面 rename — ★破壊的)
- 全 module を `Hanalyze.*` から **`Hanalyze.*`** へ rename
  (plot `Graphics.Hgg.*` と対称・191 module)。 パッケージ名・関数名・数値は
  不変 (全 test 同数 green: 本体 1066 / test-plot 189 / stream 31 / plot bridge 42)。
  **移行 = import 行の機械置換**: `sed -i 's|Hanalyze|Hanalyze|g'`。
  upstream (hanalyze・Hackage 公開) へは方式 B (変換 mirror・
  `scripts/to-upstream.sh`・branching-and-release.md §6.5) で反映し、
  **公開 API は `Hanalyze.*` のまま**。 cabal の package/executable/test-suite 名
  (`hanalyze`/`hanalyze`/`hanalyze-test` 等) は据え置き。

### Fixed (Phase 63: HBM DAG 修正 2 件 — y slot 浮遊 + plate はみ出し)
- `extractDeps` (Track): `dataNamedObs` slot がエッジゼロで source rank に
  浮遊し x データと重なる問題を修正。 walk 終端で **obs 名ごとの連結 ys と
  slot 生値の値一致**により obs→slot エッジを張る (PyMC `make_compute_graph`
  の `obs -> y` 同型 = slot は obs の子に描画)。 per-point loop observe も
  連結で一致。 同名 (dataNamedObs "y" + observe "y") は mergeByName 統合が
  優先のため対象外 (自己ループ回避)。 ★値一致は表示専用ヒューリスティック
  (偶然同値の誤エッジ = `docs/bayesian/dag-extraction.ja.md` 罠 11)。
  Track = DAG 専用変更で sampler 非影響 (extractDeps の数値側利用
  `constPriorsOf` は LatentN deps のみ参照・本修正は DataN のみ変更)。
  (hanalyze-portable)
- plate 枠からノードがはみ出す問題は **plot Phase 23** で修正 (真因 =
  plot-core `renderPlate` の固定 pad が label 長依存の glyph 幅 `nodeExtent`
  を見ていない。 glyph bbox + margin に変更・HS=PS mirror)。

### Added (Phase 62: REff 経路 (atIx) の slot エッジ)
- `REff` に 5 番目 field `!(Maybe Text)` (gids の由来 slot 名) を追加。
  `atIx` が `[Ix]` 先頭の `ixSlot` を格納し、 `lmParents` が slot 名を親集合に
  加える → ObserveLM 構造化ブロックでも **dataNamedIx slot→観測ノードの DAG
  エッジ**が出る (PyMC `b0[gid]` 同型・Phase 60.7 の既知の制限を解消)。
  `at` ([Int]) / IR 合成経路は `Nothing` = 従来挙動。 数値不変 (bench acc 全
  ビット一致)・per-draw 回帰なし (codegen probe 帯域内)。 (hanalyze-portable)

### Added (Phase 61: サンプリング進捗表示 — IO 動詞 `(|->!)` / `fitIO`)
- `Hanalyze.MCMC.NUTS`: `chainSeeds` (親 seed → child seed 列・pure/IO 共有) +
  `nutsChainsStream` (chain index 付き `SampleEvent` callback の IO multi-chain。
  no-op callback で `nutsChainsPure` と**ビット一致**)。 (hanalyze-portable)
- 新 module `Hanalyze.MCMC.Progress`: 全 chain 集計 1 行の進捗レンダラ
  (`chains 2/4 done | draw 3400/8000 (warmup) | div 12 | 380.0 it/s`・
  TTY は `\r` 上書き / 非 TTY は 10% 刻み行・描画は間引き + 単一描画権)。 (hanalyze-portable)
- `Hanalyze.Plot` (flag plot-integration): `hbmModelIO` (進捗つき IO 学習・
  `hbmModelPure` とビット一致) + `Fit` class に `fitIO` (既定 = `pure . fitWith`) +
  IO 動詞 **`(|->!)`**。 進捗の有無は HBMConfig でなく動詞の選択 (`|->` vs `|->!`)。
- 進捗オーバーヘッド実測 ~1% (4 chains × 2000 iter・no-op 比 min-of-3)。

### Changed (Phase 60: HBM データ slot 刷新 — ★破壊的)
- **★破壊的: `dataNamed` の戻り値が `[Double]` → `[a]`** (モデル数値型)。 データ値が
  そのまま式に入り `realToFrac` が消える。 旧コードの `realToFrac xi` は**型エラーになる**
  (無言の挙動変化なし・機械的に削除すれば移行完了)。 `ModelF` の `Data` 継続は
  `([a], [Double])` の 2 view (lazy・未使用側コスト 0)。 (hanalyze-portable)
- 観測値 (= `observe` に渡す側・`[Double]` 固定) は新設の **`dataNamedObs`** で受ける:
  `ys <- dataNamedObs "y" []; observe "y" (Normal mu s) ys`。 同一 slot 名を両 view で
  読んでよい (`withData` は slot 名単位で全 view に効く)。
- bench 位置統制 20 run で acc 全行ビット一致 + per-draw min Δ 全モデル ±3% 内 (性能中立)。

### Changed (Phase 60.7: dataNamedIx の Ix 化 — ★破壊的)
- **★破壊的: `dataNamedIx` の戻り値が `[Int]` → `[Ix]`** (slot 名タグ付き index)。
  索引は新演算子 **`(!!!) :: TrackTag b => [b] -> Ix -> b`** で行い、 Track (DAG) 解釈
  でのみ slot 名が依存タグとして注入される = **DAG に slot→利用先エッジ**
  (PyMC の `b0[gid]` 同型)。 数値解釈は `!!` と同コスト (既定 `tagDep = id`・
  サンプリングはビット不変を bench acc 一致で確認)。 旧 `bs !! g` は型エラーに
  なる (機械的に `!!!` へ)。 `at` 用は `atIx` (REff 経路は slot エッジ未対応)。
- **`ModelP` の制約に `TrackTag a` を追加**: `forall a. (Floating a, Ord a, TrackTag a)`。
  alias 経由のユーザコードは無風。 数値リテラルの defaulting が止まる文脈では
  `:: Double` 注釈が要ることがある (test 4 箇所で発生)。 (hanalyze-portable)
- **DataN の plate 帰属を PyMC dims 同様に後決め** (60.6 追補): モデル冒頭 (plate 外)
  で宣言した data slot を「データ長 = plate サイズ」 の一意 match で plate cluster 内に
  描く (フローティング解消)。 (hanalyze-portable)

### Added (Phase 60: dataNamedIx / DAG データノード / 束縛強化)
- **`dataNamedIx :: Text -> [Int] -> Model a [Int]`** / `withDataIx`: 群 index 等の離散値を
  `[Int]` のまま運ぶ専用 slot (`bs !! g` に直結・round 罠の根治・AD 非持ち上げで
  ホットコストなし)。 (hanalyze-portable)
- **DAG データノード** (`NodeKind` に `DataN Int`): `dataNamed`/`dataNamedIx` が pm.Data 相当の
  角丸灰色 box で DAG に出る (既定 ON)。 `dataNamed` の値には slot 名の dep タグが載り
  x→mu エッジが自動。 同名の observe があるときは観測ノードが data 容器を吸収 (PyMC 同型)。
  `dagOf` (plot-core 経路) は `NodeData` へ写像。 制限: `dataNamedObs` (生 view) と
  `dataNamedIx` (`[Int]`) は dep タグを載せられず slot からのエッジは出ない。 (hanalyze-portable)
- **`df |-> hbm` の束縛強化** (Phase 60.3・flag plot-integration): 空 placeholder slot の
  列欠落/型非対応は `fitEither` が `Left` (黙殺ゼロ)。 `dataNamedIx` slot は Int/Integer 列
  直結 + **Text factor 列の sort 順自動コード化** (R `factor()` parity)、 levels は
  `hbmFactorLevels` で逆引き。 `Integer` 列が数値列として拾われるように
  (`readMaybeDoubleColumn` の判定列追加・portable)。
- divergence rug が点 (scatter) → **下端から値域 2% の縦棒** (`lineRange`・ArviZ tick 同型)。

### Fixed (Phase 60.4)
- mermaid 経路 (`Hanalyze.Viz.ModelGraph`) の `mkNodeLine` が `DeterministicN` で
  non-exhaustive crash (Phase 59.2 の DOT 経路と同類の既存バグ)。 (hanalyze-portable)

### Added (Phase 59: HBM divergence 可視化 — Plot 抽出子経路)
- **divergence 診断抽出子** (`Hanalyze.Plot`・flag plot-integration): `divergencesOf` (発散 draw の
  全 chain pool 通し index・`mergeChains` 同順の正本)、 `tracesWithDivergencesOf` (chain 別 trace +
  下端 divergence rug = ArviZ `plot_trace` 流)、 `pairOf` (joint 散布 + 発散強調 =
  `plot_pair(divergences=True)` 流・funnel 診断)、 `energyOf` (marginal vs transition energy 密度 =
  `plot_energy` 流・系列名/色は Viz `energyPlot` と整合)。 rug/強調は既存 scatter mark + 強調色
  (新 MarkKind なし)。 funnel 実測 (発散が τ 小領域へ集中) を目視確認済。

### Changed (Phase 59.3: dagOf の plate-collapse 既定化)
- `dagOf` が `collapseIndexedPlateNodes` 適用済 (PyMC `model_to_graphviz` 同等の見た目) に変更。
  旧挙動 (plate 内 indexed RV の個別ノード列挙) は `dagOfRaw` に退避。 ★collapse は plate 外の
  indexed ノード (`obs_0..` 等) も畳む。

### Fixed (Phase 59.2/59.4)
- `renderModelGraphDot` が `DeterministicN` ノードを含むモデルで crash していた
  (`mkNodeLine` の label/attrs 両 case が non-exhaustive)。 box 形状 (PyMC Deterministic 流) で
  描画するよう修正 + 回帰 test。 (hanalyze-portable)
- `mergeChains` が `chainDivergences` を chain 内 index のまま無 offset 連結していた
  (merged frame で別 draw を指す潜在バグ) — `pooledDivergences` 正本経由に修正。
- `NUTSConfig.nutsIterations` の doc コメント「Total iterations (burn-in included)」 が実体
  (post-burn-in draw 数・loop は burnIn+iterations) と不一致だったのを修正 (挙動不変)。
  (hanalyze-portable)

### Changed (Phase 53-54: HBM/NUTS 勾配の高速化 = AD モード + ベクトル化 + 解析 prior)
- **Reverse-mode AD 統一** (Phase 53): `gradAD`/`gradADU` を `Numeric.AD.Mode.Reverse.Double` に統一
  (旧 forward モードは勾配 1 本が O(p) 評価)。 階層 M2 (p=12) で per-draw 17.4→5.9ms。 PyMC 差 42→14×。
- **ハイブリッド勾配** (Phase 54.1/54.4a): `ModelF` に構造化線形予測子 observe `ObserveLM` を追加
  (`observeLM` / `observeLMR`)。 Gaussian-恒等リンクの観測尤度勾配を自作 vector-op tape
  (`Hanalyze.Model.HBM.VecAD`) で計算・他は `ad`・chain rule で加算。 群効果は密 one-hot でなく
  `REff` の gather (`gatherHR`) で O(n) 維持 (密展開は階層モデルで O(nG·n) 逆効果・計測で確定)。
- **静的部分の hoisting** (Phase 54.4b): `compileGradU` が設計列ベクトル化・群 id unbox 等を draw ループ
  外で 1 度だけ前処理しクロージャ再利用。 PyMC 差 14.1→6.0× (M2・per-draw 線形フィット突合)。
- **第一級ランダム効果値 + 解析 prior 勾配** (Phase 54.4c): `data REffect a` + `reNormal` / `at` /
  `observeNormalLM` で群効果を構造化値として宣言 (文字列添字・`!!` を排除)。 `REff` に prior スケール名
  (`Maybe Text`) を持たせ、 `compileGradU` が `u_j ~ Normal(0, τ)` の勾配を解析計算
  (∂/∂u_j=-u_j/τ²・∂/∂τ=-nG/τ+Σu²/τ³・O(nG) 素な Double)・対応 `u_j` Sample を `ad` walk から除外。
  `glmmRandomIntercept` は内部で `reNormal`/`at` を使用 (公開 API 不変)。 54.4b 比 per-draw ×1.04 (nG=8)
  〜×1.32 (nG=32)・per-call ×1.1-2.1 (同一セッション A/B)。 M2 (nG=8) PyMC 差 5.98→4.96× (fresh run・
  注意は `HBM_SCALING.md`)。 数値は中心差分・ad と一致 (relErr ≤ 5e-7)。
- **logp 値評価のコンパイル + 残 prior 解析勾配** (Phase 54.4d/e): cost-centre profile で残 gap の
  実測内訳 (値評価 46% + 残 prior ad 19%) を確定してから実装。 `compileLogPU` (LM ブロック値を素
  Double ベクトル演算 + 解析 u-prior 値・NUTS のエネルギー評価に配線) + `constPriorGradD` (定数
  パラメタ prior 13 分布の解析勾配・`extractDeps` deps ∅ で検出) + residual 空なら `ad` クロージャを
  丸ごと省略 (reflection tape 生成ゼロ)。 scalar Observe/Potential/非 Gauss LM 残存時は従来経路に
  fallback。 M2 (nG=8) per-draw 2.068→**0.574ms = PyMC 差 4.96×→1.38×** (Phase 54 開始時 14.1×)・
  固定費 347ms (PyMC 1745 の 1/5)。 ⚠per-obs scalar 手書きモデル (M1 形) は fallback のままで
  26.7× — 高速経路は `observeLM`/`observeLMR`/`glmmRandomIntercept` 等の構造化観測が条件。
  ⚠FP 和順序変更で chain は旧版とビット非同一 (数値等価は test・posterior 品質は bench で担保)。
- **解析閉形式カーネル + positional vector 化** (Phase 54.6): Gaussian-恒等リンク LM の勾配は
  閉形式 (∂β=Xᵀr/σ² 等) ゆえ汎用 vec-tape を撤去し fused 1 パス残差 + 直接計算。 name→index を
  compile 時に解決した vector-native `compileGradUV`/`compileLogPUV` を追加 (list API は wrapper で
  不変・NUTS は V 版直結)。 M2 (nG=8) per-draw 0.574→**0.283ms = PyMC 比 0.68× (追い越し)**・
  固定費 186ms (PyMC の ~1/9)。 Phase 54 累計で per-draw 約 20.8× (5.894→0.283)。
- **カーネル割当の実測検証 + fused ループ化** (Phase 54.7a): prof の alloc 84% を通常ビルド
  `+RTS -s` で検証 (2.56GB = 本物・per-grad ~48-82KB)。 row-major X 前計算 (`cliXMat`) +
  残差/sumR2 の 1 パス手動ループ (`lmResidualS`) + dot/scatter 明示ループで割当 ×5.2 減 (0.49GB)・
  wall ×2.0。 M2 scaling fit 0.283→**0.166ms = PyMC 比 0.40×**・固定費 103ms (~1/17)。
- **NUTS 本体の SCC 内訳 → SPECIALIZE** (Phase 54.7b): nutsStream/leapfrog に明示 SCC を置き
  prof で **RNG 系 (運動量 + uniform = 11.2%/alloc 25%) が NUTS 側の半分超**と確定。 真因 =
  Phase 50 の `PrimMonad m` 多相化に SPECIALIZE が無く mwc の `uniform`/`standard` が
  dictionary 渡しで boxed 化。 `nuts`/`nutsStream`/`buildTree` に IO・`ST s` の
  `{-# SPECIALIZE #-}` を追加 (コード変更なし・API 不変)。 wall ×1.67 (0.253→0.151s)・
  M2 scaling fit 0.166→**0.106ms = PyMC 比 0.25×**・固定費 73ms (~1/24)。 Phase 54 累計で
  per-draw 約 **55.7×** (5.894→0.106)。 ⚠多相 RNG ホットループは SPECIALIZE とセットが必須 (教訓)。
- **per-obs 手書きモデルの自動 ObserveLM 化** (Phase 54.8): affine 追跡 interpreter (`AffV` =
  AffC/AffL/NA・非線形演算で NA 化) で scalar `Observe (Normal μ σ)` 群 (μ=affine・σ=単一
  latent) を Gaussian LM ブロックに自動合成 (`synthGaussLMBlocks`)。 係数常 1 + prior
  `Normal(0,τ)` 共有 + 各行ちょうど 1 つの latent 族は one-hot→REff gather に昇格 (dense
  one-hot の O(nG·n) 逆効果を回避)。 安全網 2 段 = ①非定数比較 error poison→try/force 捕捉で
  fallback (値依存分岐の誤抽出防止) ②probe 2 点で walk 評価と突合。 公開 API/authoring 不変 —
  手書き per-obs モデルが書き換えなしで解析閉形式カーネルに乗る。 **M1 per-draw fit
  1.268→0.0169ms (×75)・PyMC 比 30.2×→0.40× (約 2.5 倍速)**・固定費 928→12ms・posterior 不変
  (b mean 1.4299→1.4303・ESS 1600/1600)。 test +3 (M1 形 / one-hot→REff / 値依存分岐 fallback)。
- **非 affine 系の prof 計測** (Phase 54.9): M3-M6 を bench harness に追加し PyMC と同一
  セッション再突合 (affine 系 M1/M2/M4 は勝ち・非 affine 系 M3 3.0×/M5 ≳27×/M6 4.5× 負けを
  実測確定)。 cost-centre prof で支配項を確定: M3 = Free walk 再構築 ~31% (alloc ~60%)・
  M5/M6 = per-obs スカラ AD 帰着 ~90% (`logDensityObs` ~52% 筆頭)・NUTS 本体 <2% (棄却)。
- **係数付き gather = random slope の REff 化** (Phase 54.10): `REff` に per-row 重みスロット
  追加 (`REff [Text] [Int] (Maybe Text) (Maybe [Double])`・`Nothing` = 全 1 = 後方互換)。
  η_i += w_i·v_{g_i} をカーネル/解析 prior に配線し、 54.8 の族検出を「prior `Normal(0,τ)`
  共有 + 各行ちょうど 1 つ (係数任意)」 に一般化 — random slope `v_g·x_i` も gather 昇格。
  **M3 per-draw 2.52→0.258ms (×9.8)・PyMC 比 3.0×→0.31× (追い越し)**。 test +2。
- **非線形 μ のベクトル式 IR** (Phase 54.11): スカラ式 IR (`SExp`・poison Eq/Ord・定数畳込み)
  を Sample 継続に給餌する追跡 interpreter で per-obs `Observe (Normal μ σ)` の μ 式
  (非線形可) を収集し、 行間で式形が同型なら定数列をベクトル leaf に束ねて「ベクトル式 IR」
  (`UExp`→`CompiledVecIR`) へ持ち上げ (μ⃗ = f(θ, x⃗))。 階層 prior 族 (構造同一 `Normal(m,τ)`)
  も gather + ベクトル化密度で同 IR に乗せる。 勾配 = VecAD vector-op tape (per-call 構築・
  `vmap1HR`/`map1S` 追加)・値 = 素 Double ベクトル評価。 統合 = `compileGradUV`/`compileLogPUV`
  の全体 ad fallback 手前 (affine 経路優先・公開 API/authoring 不変・安全網 = poison +
  probe 2 点)。 **M5 per-draw 3.59→0.296ms (×12.1)・M6 2.46→0.274ms (×9.0・PyMC 比 0.50׆)**
  († = PyMC per-draw は固定費支配 R² 低の下限目安)。 per-call は手組み spike 上限に到達
  (gradADU M5 0.0063ms)。 M1-M4 は chain ビット一致 (非回帰)。 test +4。
- 詳細: `specification/phases/phase-54-hbm-logdensity-compile.md` + `bench/results/HBM_SCALING.md`。

### Changed (Phase 55: HBM 高速経路のカバレッジ拡張 = 非 Gaussian GLM + σ 式 + 形混在)
- **GLM bench + prof ゲート** (Phase 55.1): M7_pois (Poisson 回帰 log link) / M8_logit
  (logistic) を bench harness に追加 (`bench-hbm-scaling glm`/`m7-long`/`m8-long` + PyMC 同一
  モデル)。 baseline (long grid R²>0.999): M7 5.27× / M8 4.77× (HS が遅い)。 prof で支配項 =
  per-obs スカラ AD ~90% (M5/M6 = 54.9 と同型) を確定してから 55.4 に着手。
- **式形混在のサブグループ化** (Phase 55.2): `synthVecIRWalk` のグループキーを (σ名) →
  (σ指紋, μ式形指紋 `sexpShape`) に。 同一 σ 下で式形が混在しても形ごとに独立吸収
  (従来は σ グループ丸ごと residual 落ち)。
- **σ 式 / heteroscedastic** (Phase 55.3): `collectSymRows` の σ を単一 latent → 任意式に拡張
  (σ 側キーは名前付き指紋 `sexpKeyNamed`・σ leaf の族 gather 化はしない保守設計)。 スカラ σ 式
  (例 `2*s`) は従来密度・行依存 σ⃗ (例 `exp(g0+g1·z_i)`) はベクトル密度 -Σlogσ_i - Σr_i²/(2σ_i²)
  を値/tape に追加。 σ = 単一 latent は tape ノード追加ゼロ = 54.11 と同一 tape (M5/M6 chain
  ビット一致で実証)。 per-call 勾配 ×9.4 (`bench-hbm-het` 新設・IR 0.0106 vs fallback 0.0998ms)。
- **非 Gaussian 観測密度の IR 化** (Phase 55.4): Poisson / Bernoulli の scalar `Observe` を
  ベクトル式 IR に吸収 (`SymDist`/`VecGroupSrc`/`VecObsIR`・分布別密度ノード)。 Poisson は
  Σlog y! を compile 時前計算 (y·logλ の y は raw = `logDensityObs` kA 一致)・Bernoulli は
  round 済 0/1 の定数係数化。 guard は値側のみ `logDensityObs` 一致・観測値 guard 行 (y<0 /
  y∉{0,1}) を含むグループは収集時に弾く (walk の -∞ 縮退を残す)。 族 prior (階層 GLM 形) も
  既存機構のまま乗る。 公開 API / authoring / ModelF / Distribution 不変。
- **再計測** (Phase 55.5): **M7 per-draw 0.842→0.0938ms (×9.0)・PyMC 比 5.27×→0.60× (追い越し)** /
  **M8 0.761→0.152ms (×5.0)・4.77×→1.04× (ほぼ同水準・僅差で PyMC 優位)** (同一セッション
  long grid 突合・posterior 一致)。 M1-M6 は全モデル chain ビット一致 (非回帰)。 test 1028→1035
  (+7: 形混在×2 / σ式×2 / M7形 / M8形 / 階層GLM形 / StudentT fallback — 既存「部分吸収」 test
  期待値更新含む)。
- 詳細: `specification/phases/phase-55-hbm-fastpath-coverage.md` + `bench/results/HBM_SCALING.md`。

### Changed (Phase 56: 観測分布 IR Part 2 = 記号微分化 + 計 16 family)
- **digamma 基盤** (Phase 56.1): `digamma :: Double -> Double` (lgammaApprox と同一 recurrence +
  漸近級数)。 lgamma 系の記号微分の足場。
- **IR 記号微分化** (Phase 56.2): 観測密度の per-call VecAD tape を撤去し、 compile 時に
  静的命令列 (SSA + 構造 CSE) を 1 回生成・per-call は unboxed arena の forward/backward のみ。
  分布追加が「densityIR の式 + 値 guard」 だけになり**勾配コード不要**に。 per-call het ×2.0 /
  M5 ×1.4 / M6 ×2.0・per-draw M7 ×1.24 / M8 ×1.36。 ★旧 tape の潜在バグ (invLogit FP 飽和行の
  unguarded NaN が全勾配汚染) を発見・修正 (guard 違反 call のみ per-call fallback)。
- **12 分布の IR 吸収** (Phase 56.3-56.5): StudentT (ν=SC 定数のみ)・Cauchy・Logistic・Gumbel /
  Exponential・Weibull (k latent 可)・LogNormal (Gaussian ノード再利用)・Gamma (α latent 可)・
  Beta (α=μφ 回帰形) / Binomial (n 定数)・Geometric・NegativeBinomial (α latent 可・
  lgammaΓ(k+α) は SLgammaO elementwise) → 観測高速経路は計 **16 family**。 ZIP は見送り。
  SLgammaO 導関数は digamma でなく **lgammaApprox の項別微分** (walk+ad との 1e-9 一致優先)。
  公開 API / authoring / ModelF / Distribution 不変。 test 1035→1051。
- **bench** (Phase 56.6): M9_negbin (NegBin 回帰・α latent) を bench harness + PyMC に追加
  (パラメタ化は密度 5 点突合 diff≤9e-15 で一致確認)。 **per-draw 確定 (long grid): HS 0.366ms vs
  PyMC 0.247ms = 1.48× (HS 遅い)・posterior parity・実用域 total は PyMC 固定費 2.5s で HS 有利**
  (iter1600 0.69s vs 2.80s)。 `bench-hbm-dist` 新設 (15 family の per-call 勾配 A/B =
  walk 比 ×7.6〜×68.1・per-draw 波及は M9 以外未計測)。 非回帰 = M1-M5/M7 chain ビット一致・
  M6/M8 は 56.2 FP 順序変更の posterior parity。 ⚠hanalyze `ess` は τ≥1 clip (ESS≤n) ゆえ
  ArviZ bulk ESS との ESS/sec 直接比較は不採用。
- 詳細: `specification/phases/phase-56-hbm-obs-dist-ir-part2.md` + `bench/results/HBM_SCALING.md`。

### Added (Phase 51: DataFrame ↔ analyze fit API = `df |-> spec` / ColumnSource)
- **統一 fit 動詞** (`Hanalyze.Plot`, flag `plot-integration` 配下): `(|->) :: (ColumnSource d,
  Fit spec) => d -> spec -> Fitted spec` で任意データ源から任意モデルを学習 (R の
  `lm(y~x, data=df)` 体験)。 `class Fit spec` (associated `Fitted spec`) + pure 主 `fitWith` +
  total 副 `fitEither :: … -> Either String (Fitted spec)`。 ★新規数値核ゼロ = 既存 fit 関数の配線。
- **データ源抽象** (`Hanalyze.Data.ColumnSource`, **portable**): `class ColumnSource d`
  (`lookupCol` / `columnNames` / `toFrame`)。 instance = `[(Text,[Double])]` / `Map Text [Double]` /
  `DataFrame` (core)・`[(Text, ColData)]` (flag 配下)。 `DataFrame` 源は `toFrame=id` ゆえ factor/欠損が
  **Phase 47** 経路 (`MissingPolicy`/contrast) をそのまま通る。
- **spec ビルダー**: 二変量近道 `lm`/`glm`/`spline`/`robust`/`quantile` (列名2つ→単変数モデル) と
  R 流 formula `lmF`/`glmF`/`glmmF "y ~ …"` (多変量モデル・`glmmF` は **Phase 48** 混合モデル接続) と
  `hbm cfg model` (HBM・手書き `ModelP`・`hbmModelPure` 配線・seed 決定的)。
- **`dataScatterOf m "x" "y"`** (B10): 学習済 HBM が保持する `hbmData` から散布図層。 epred/forest 等と
  重畳するとき **df を学習時 1 回だけ**書けばよい。
- test-plot 104→**122** (+18: 51.2 二変量 +7 / 51.3 formula +6 / 51.4 HBM +5)。
- 51.1-51.3 は `(hanalyze-portable)`、 51.4 のみ flag 配下 (非 portable)。 後続 = formula→HBM 自動生成
  (brms 風) / route2 stat 自動生成。 詳細: `docs/io/04-fit-api.md`。

### Added (Phase 50: MCMC/HBM 純粋化 = PrimMonad 一般化 + ST/seed)
- **純粋・決定的な NUTS** (`Hanalyze.MCMC.NUTS`): `nutsPure :: ModelP r -> NUTSConfig -> Params
  -> Word32 -> Chain` (seed → 確定 Chain・IO 不要) + `nutsChainsPure` (親 seed から子 seed を純粋導出し
  各 chain 別 `runST` → `parList rdeepseq` で chain 横断を並列評価)。 同 seed でビット同一・IO `nuts` とも
  ビット同一 (ST/IO 等価)。 並列は spark で純粋値に効くので決定性 (= seed 由来) と直交。
- **MCMC コアの PrimMonad 一般化** (`nuts`/`nutsStream`/`buildTree`/`sampleMomentum`、 `Hanalyze.Model.HBM`
  の `sampleDist`/`samplePoissonKnuth`/`sampleMvDist`、 `Hanalyze.MCMC.Core` の `spawnGen`): `GenIO -> IO`
  から `PrimMonad m => Gen (PrimState m) -> m` に一般化 (`IO` も `ST s` も走る)。 進捗コールバックは
  `SampleEvent -> m ()`。 内部 `IORef` を `Data.Primitive.MutVar` に置換。 既存 IO 版は `m=IO` で完全不変。
- **純粋・決定的な HBM 学習・事後予測** (`Hanalyze.Plot`, flag `plot-integration` 配下):
  `hbmModelPure :: HBMConfig -> ModelP () -> [(Text,[Double])] -> HBMModel` (IO 無し) +
  `ppcOfPure` / `ppcOfPureWith` (y_rep を `runST` でサンプリング)。 seed 未指定時は固定既定 seed 42。
- 使い分け: **純粋版 (`*Pure`) が本流** = 再現可能・テスト容易・`let`/ノートブック合成可。 IO 版
  (`nuts`/`nutsChains`/`hbmModel`/`ppcOf`、 async 並列・進捗コールバック) は移行期の後方互換で将来 deprecate 予定。
  並列性能は両者同等 (spark vs OS スレッド)。
- 50.1-50.3 は `(hanalyze-portable)`、 50.4 のみ flag 配下 (非 portable)。 NUTS の per-sample 計算は IO/ST で
  ビット同一 (回帰テストで実証) ゆえ純粋化による per-sample 性能変化は無い。
- **他サンプラの純粋化** (`Hanalyze.MCMC.{MH,Slice,HMC,Gibbs,SMC}`): `metropolisPure`/`metropolisChainsPure` /
  `slicePure`/`sliceChainsPure` / `hmcPure`/`hmcChainsPure` / `gibbsMHPure`/`gibbsMHChainsPure`/
  `gibbsPure`/`gibbsChainsPure` (汎用 update・rank-N 引数) / `gibbsBetaBinomialPure` / `smcPure` を追加。 各 IO 版 (`metropolis`/`slice`/`hmc`/`gibbs`/`gibbsMH`/`smc`) も
  PrimMonad 一般化 (m=IO で不変)。 `IORef`→`MutVar`。 ★`Gibbs.GibbsUpdate` は kind `* -> *` の monad
  パラメタ型 (`GibbsUpdate m`) に変更し関数側を多相化 (rank-N alias の impredicative 構築を回避)。 SMC は
  `SMCResult` 返却ゆえ単一純粋値。 全サンプラ同 seed でビット同一・IO 版と ST 等価を回帰テストで実証 (本体 test 1006)。

### Added (Phase 49: HBM plot integration = `hbmModel` + epred/trace/ppc/forest/dag)
- **学習済 HBM モデル型** (`Hanalyze.Plot`, flag `plot-integration` 配下): `HBMModel` +
  `hbmModel :: HBMConfig -> ModelP () -> [(Text,[Double])] -> IO HBMModel` (`lmModel`→`LMModel`
  と対称命名)。 df 列名で `dataNamed`/observe placeholder を `withData` 自動 bind (PyMC `set_data`
  同型)、 既存 `nutsChains` の async 並列で multi-chain 学習。 ★positive 制約 latent は制約空間で
  init しないと初手 divergence ⇒ `getTransforms` で 1/0.5/0 init。
- **HBM 出力抽出子** (`HBMModel` は単一図に一意化せず抽出子を明示):
  - `epred hbm "x" "mu"` = 予測子 grid を `withData` 差し替え → draw ごと `runDeterministics "mu"`
    評価で事後予測平均線 + **94% HDI band** (ArviZ 流)。 `ModelSpec` を再利用し `<> grid/gridRange/
    statLevel/bandOff/predAt` で Phase 16 C1 と同綴り合成可。 `epredAt` で 1 点 (平均,(lo,hi))。
  - `traceOf` = 各 latent の trace plot (`ChainModel` を per-param 流用)。
  - `forestOf` / `forestOfLevel` = 事後平均 + 94% HDI の forest (`MForest` mark)。
  - `ppcOf` / `ppcOfWith` (IO) = 事後予測チェック = 観測 density + y_rep N 本 density(薄) +
    プール density(破線)。 y_rep は draw ごと `runObserveDists` の observe 分布から `sampleDist`。
    `PPCConfig { ppcReps, ppcSeed, ppcCumulative }` (cumulative で `ecdf` に切替)。
  - `dagOf` = モデル構造 DAG (`buildModelGraph` → plot-core `dagFromListsWithPlates`,
    Sugiyama 階層 layout)。 latent/observed + 分布名 + plate を写す。
- demo (`plot-integration-demo`): HBM 5 抽出子の SVG を viewer.html に。 PyMC + ArviZ
  (`az.plot_lm`/`az.plot_ppc`/`az.plot_trace`/`az.plot_forest`/`pm.model_to_graphviz`) と同型。

### Added (Phase 48: random effect = GLMM random intercept + slope)
- **混合効果モデルの一般ランダム効果** (`Hanalyze.Model.GLMM`): `GLMMResultRE` (共分散 `G` r×r +
  q×r BLUP) + `fitLMEGeneral` (Gaussian EM, Laird-Ware) / `fitGLMMGeneral` (非 Gaussian 多変量
  Laplace)。 既存の random intercept 専用 `fitLME`/`fitGLMM`/`GLMMResult` は不変 (後方互換)。
  r=1・intercept のみで既存スカラー版に厳密一致。 hmatrix native。
- **Formula DSL の random effect 構文** (`Hanalyze.Model.Formula.Mixed`): lme4 流 `(1|g)` / `(x|g)` /
  `(1+x|g)` / `(0+x|g)` を `fitMixedLME` / `fitMixedGLMM` で fit。 `extractRandom` が `(…|g)` を字句
  プリパスで抽出 (`Term`/`Formula` 不変)、 固定効果は既存 `parseModel`/`designMatrixF` 経路に route。
  単一 grouping factor のみ。
- `statsmodels smf.mixedlm(reml=False)` (ML) と突合 (`bench/python/bench_formula.py`): random slope の
  β / 共分散 G / σ² が一致。

### Added (Phase 47: Formula DSL 拡充 = ロードマップ B 段の残り)
- **欠損 policy** (`Hanalyze.Model.Formula.Frame.MissingPolicy`): `DropRows`/`Pairwise`/`Impute`/
  `TreatAsCategory`/`ErrorOnMissing` + `modelFrameWith`。 NA 検出・除去・補完を ModelFrame の単一責務点に。
  `modelFrame = modelFrameWith DropRows` で後方互換 (NA 無しデータでは不変)。
- **contrast coding** (`Hanalyze.Model.Formula.Design.ContrastCoding`): `Treatment`/`Sum`/`Helmert`/
  `Polynomial`/`CustomContrast` + `contrastMatrix`/`parseContrast`。 `factorColumns` を treatment 固定から
  contrast 行列の Kronecker 積へ一般化。 構文 `bg ! C(g, Sum)` (正本) / `C(g, Sum)` (R)。 factor×連続
  (masked 列) は full coding で全水準保持。
- **weights / offset = WLS** (`fitWLSF` + `WLSConfig`): `smf.wls` に倣い列名で渡す。 WLS=√w スケールで
  OLS 帰着、 線形 offset は `y−offset`。
- **非線形フィット = NLS** (`Hanalyze.Model.Formula.Nonlinear`): `fitNLS` が parse 済 AST を評価関数化し
  `Hanalyze.Optim.NelderMead` で SSR 最小化。 初期値ユーザ必須。
- **外部オラクル突合 (再現可能化)**: `formula-ref-gen` executable が `formula_haskell_ref.json` を生成、
  `bench/python/bench_formula.py` が statsmodels (`smf.ols`/`smf.wls`) + scipy (`curve_fit`) と突合
  (6 OLS + WLS + NLS、 ALL PASS)。

### Added (Phase 46: hgg 統合 = `toPlot` / 統一出入口)
- **top-level umbrella `module Hanalyze`** 新設 = quickstart 出入口。 `Hanalyze.Model.{Core,LM,GLM}` +
  `Hanalyze.Stat.{Summary,Test,Effect,Distribution}` + `Hanalyze.Viz.{Core,Scatter,Bar,Histogram}` +
  `Hanalyze.DataIO.CSV` を 1 つの `import Hanalyze` で集約。 **plot 非依存・portable**。
  GLM `Family` と `Stat.Distribution` の `Binomial`/`Poisson` 衝突は GLM 優先で hiding
  (分布値が要る場合は `Hanalyze.Stat.Distribution` 直 import)。
- **能力別中立 protocol** (`Hanalyze.Model.Core`): `class PredictiveModel { predictAt }` /
  `class ResidualModel { residualsOf }` + LM/GLM/GLMM 共有 `FitResult` への instance。 **portable**。
- **`Hanalyze.Plot` (cabal `flag plot-integration`、 既定 off)**: `class Plottable { toPlot, diagnosticPlots }`
  + `LMModel` (回帰線 + CI band) / `GPResult` (事後平均 + credible band) の instance。 `hgg-core`/
  `-svg` 依存ゆえ flag 隔離 (off = standalone 維持・upstream portable)。 `df |>> (layer (scatter ..) <> toPlot fit)`
  で散布図に重畳。 ※flag on build: `cabal build --project-file=cabal.project.plot`。

### Added (Phase 46 §3.6: Formula DSL = ロードマップ B 段の線形核、 **portable**)
- **`Hanalyze.Model.Formula`** = 正本 `Formula` AST (`Term` = Lit/Ref/App/Index/Neg/Bin、 意味論分類前の
  構文木) + 独自・明示係数構文 parser `parseFormula` (megaparsec) + round-trip pretty `prettyFormula`。
  例 `"y x group = b0 + b1*x + bg ! group"`。 優先順位 (高→低) `!` > `^` > 単項`-` > `* /` > `+ -`。
- **`Hanalyze.Model.Formula.Frame`** = `modelFrame :: Formula -> DataFrame -> Either String ModelFrame`。
  `VarRole` (Response/Continuous/Factor) 割当 + パラメータ分離。 **factor は列型でなく「! の右に現れたか」**
  (使われ方) で判定。
- **`Hanalyze.Model.Formula.Design`** = `designMatrixF` / `fitLMF` / `linearityCheck`。 treatment contrast
  (参照水準 drop で満ランク化)、 交互作用 (連続×連続・factor×連続・factor×factor)、 基底展開
  `bp ! poly(x,n)` (x¹..xⁿ) / `bs ! bspline(x,n)` (= `fitSpline (BSpline 3) (quantileKnots n x)`)。
  **線形 OLS では係数名は fit に無関係**ゆえ、 パラメータがデータ式の内側に出たら非線形と検出 (Left)。
- **`Hanalyze.Model.Formula.RFormula`** = `parseRFormula` + `parseModel` (`~` 含めば R/patsy・無ければ独自に
  dispatch)。 R 意味論 (暗黙切片・`C(g)` factor・`a*b` crossing・`I(expr)`・`poly`/`bs`) を**同一 AST** へ。
- **新規依存**: `megaparsec` / `parser-combinators` (lib) + `QuickCheck` (test)。
- **検証 (昇格ゲート 4 点・Python 非依存オラクル中心)**: ① 飽和 factor×factor で ŷ=セル平均・満ランク /
  ② poly=厳密二次・bspline ŷ=fitSpline / ③ QuickCheck round-trip + golden / ④ R≡独自 両構文で同 ŷ
  (外部 statsmodels 突合は `bench/python/bench_formula.py` + `formula_haskell_ref.json` で ready・要 venv)。
  hanalyze-test 929 例 pass。

### Changed (Phase 41: HBM categorical 列対応 = DataMap sum 型化)
- `Hanalyze.Model.HBM.Interp.DataMap` を `Map Text [Double]` から
  `Map Text Column` (`data Column = Numeric [Double] | Factor {facLevels, facCodes}`)
  に変更。 categorical (文字列) 列を factor (level 辞書 + 出現順整数 code) として
  HBM に渡せる。 アクセサ `colDoubles` / `colLength` / `colLevels` / `lookupDoubles`
  を追加・export し、 群化・観測の内部ロジックは `colDoubles` 経由で透過。
  **BREAKING** (`DataMap` 型シノニムの構造変化): `data_map` を直接構築/分解して
  いた呼び出し側は `Numeric` ラップ / `colDoubles` が必要。
- `forEachGroup` の群 node 名 suffix を `groupSuffixFor` で factor level ラベル化
  (例 `alpha_setosa`、 不安全な識別子は数値 code suffix にフォールバック)。
- categorical observe: factor 列を観測すると code (0..K-1) が観測値として渡る
  (2 値応答 `Bernoulli` で完結)。 多値 `Categorical` / `OrderedLogistic` の DSL
  露出は未対応 (TODO)。
- streaming bridge worker `lookupDataMap`: data_map CBOR を後方互換 decode
  (`Numeric`=`[float]` / `Factor`=tagged map `{kind:"factor",levels,codes}`)。

### Added (Phase 21: MLP / BO の per-iter callback API + streaming UX 完成)
- `Hanalyze.Model.NeuralNetwork.fitMLPRegressorWithCallback` /
  `fitMLPClassifierWithCallback` 新規 — epoch 終端ごとに `MLPEpochEvent`
  (epoch idx / train_loss / current_lr) を渡す callback API。
  既存 `fitMLPRegressor` / `fitMLPClassifier` は no-op callback で呼ぶ
  薄い wrapper として保持 (= 既存 480 tests 改修不要)。
- `Hanalyze.Optim.BayesOpt.bayesOptWithCallback` 新規 — BO iteration ごとに
  `BOIterEvent` (iter idx / proposed (x,y) / current best) を渡す。
  既存 `bayesOpt` は wrapper。
- `hanalyze.Stream.Kinds.MLP` / `hanalyze.Stream.Kinds.BO` の handler を新
  callback API で書き換え、 真の per-epoch / per-iter progress を emit。
  以前は「2 点 emit (= 開始/完了のみ)」 で UX が固まっていた問題を解消。
- streaming bridge に per-iter emit 検証 hspec test 2 件追加 (= 計 22 件)。
- `(hanalyze-portable)` タグ: NeuralNetwork / BayesOpt の callback 拡張は
  upstream hanalyze に cherry-pick 可能。

### Added (Phase 20: cv.kfold kind — lm / ridge / lasso)
- `hanalyze.Stream.Kinds.CV` 新規 — cv.kfold kind handler。
  `estimator_kind` で `"lm"` / `"ridge"` / `"lasso"` を dispatch、 各 fold で
  test MSE を `emitProgress` で吐き、 完了時に `scores` / `mean_score` /
  `std_score` を `final` に格納。
- dispatcher 拡張 (`"cv.kfold"` → CV.runCVKFoldStream)
- KindsSpec に 2 件追加 (lm / ridge)、 streaming bridge-test 計 20 件
- spec §A.5 (cv.kfold) 完成

### Added (Phase 19: Streaming kind 拡充 — nsga2 / mlp.train / bo)
- `hanalyze.Stream.Kinds.NSGA2` — nsga2 kind handler。 ZDT1 (2 objective、
  30 variable) を内蔵 objective として `Hanalyze.Optim.NSGA.nsga2WithProgress`
  を per-generation callback で呼び、 各世代の `pareto_size` / `best_f1` /
  `best_f2` を progress に emit。
- `hanalyze.Stream.Kinds.MLP` — mlp.train kind handler。 synthetic linear data
  (y = 2x + 1 + 噪音) を内蔵、 `Hanalyze.Model.NeuralNetwork.fitMLPRegressor`
  を呼ぶ。 library 側に per-epoch callback が無いため進捗は開始/完了の 2 点
  emit (= 真の per-epoch progress は別 Phase で library 拡張要)。
- `hanalyze.Stream.Kinds.BO` — bo kind handler。 1D negative quadratic
  `(x - 0.3)²` を内蔵 objective として `Hanalyze.Optim.BayesOpt.bayesOpt` を
  呼ぶ。 同様に開始 / 完了の 2 点 emit。
- `hanalyze.Stream.QueryDispatcher.defaultDispatcher` を上 3 kind に対応拡張。
- hspec 3 件追加 (各 kind の end-to-end completion test)、 計 18 件。

cv.kfold は Phase 20 候補に繰越 (= generic CV module が無く、 estimator 別
wrapper が必要なため別 phase 規模)。

### Added (Phase 18: Streaming Protocol v1.0 PoC — `streaming bridge` package)
- 新規 cabal package `streaming bridge` (`cabal.project` で multi-package 化)
- `hanalyze.Stream.Protocol` — length-prefix CBOR frame I/O、 Event 型 +
  Serialise インスタンス、 NDJSON 並列出力 helper (`eventToNdjsonLine`)
- `hanalyze.Stream.Server` — stream 多重化 (Map UUID Async) + mutex stdout +
  cancel + duplicate ID 拒否
- `hanalyze.Stream.QueryDispatcher` — kind → worker dispatch
- `hanalyze.Stream.Kinds.MCMC` — mcmc.nuts kind handler (hanalyze の
  `Hanalyze.MCMC.NUTS.nutsStream` を library import で呼び、 per-iteration
  progress を emit)
- `hanalyze.Stream.JobsApi` — Job Registry POST client (status / milestone、
  exponential backoff retry 3 回、 失敗時は stderr log + stream 継続)
- `hanalyze.Stream.Transport.Sidecar` — stdin/stdout pipe transport
- `hanalyze.Stream.Transport.Server` — WS server skeleton (将来 web 版で実装)
- exe `streaming bridge` (`--sidecar` / `--debug-dump-ndjson PATH` /
  `--jobs-api-url URL` / `--no-jobs-api`)
- bench `bench-streaming` で protocol overhead 測定: 1000 samples NUTS で
  直呼び vs sidecar 経由が同等 (jitter range)、 完了条件 < 5% 達成
- Python `cbor2` smoke test (`streaming bridge/test/python/`)
- 15 新 hspec tests (Protocol round-trip / Server lifecycle / mcmc.nuts
  end-to-end / debug-dump-ndjson)、 既存 hanalyze 480 tests も
  保持

### Fixed (Phase 17: Tier 1+2 bench で判明した改善)
- **PLS の精度バグ修正** (`Hanalyze.Model.PLS` の `colSD`): 旧実装が
  `LA.sumElements (c LA.<> LA.tr c)` で `(Σc)²` を計算していた (centering 後 ≈ 0、
  std fallback で 1 になり実質スケールしていなかった)。 `c \`LA.dot\` c` に
  修正。 NRMSE が sklearn と完全一致 (PLS_n500_p10 で 0.138 → 6.09e-5)、
  時間も 4.44 ms → 0.36 ms に改善 (12× 高速化、 sklearn と同等)。
- **MLP 入力標準化追加** (`Hanalyze.Model.NeuralNetwork`): `MLPConfig` に
  `mlpStandardize` (default True) を追加。 fit 時に X を z-score 標準化、
  予測時に同じ mean/std で逆変換。 MLPFit に `mlpXMean` / `mlpXStd` フィールド
  追加。 MSE n=500 が 2.5e-3 → 1.52e-3 (1.6× 改善)。
- **HClusterWard 速度改善** (`Hanalyze.Model.HierarchicalCluster`): boolean
  active 配列を廃止し active な ID リスト + unsafe Unboxed MVector に変更。
  小幅改善のみ (1.94 → 1.69 ms、 scipy の C 実装 NN-chain には届かず)。
- **ベンチ信頼性回復** (`bench/haskell/BenchTier12.hs`): EWMA / CUSUM / GaugeRR /
  I-Opt / E-Opt の force probe を計算結果の非自明なスカラーに変更し GHC CSE
  による削除を阻止。 計測値が 0.00002 ms → μs〜ms オーダーの妥当値に。

### Added (Phase 16: MLP Neural Network)
- `Hanalyze.Model.NeuralNetwork` 新規 — feedforward MLP。
  ReLU / Sigmoid / Tanh / Identity / Softmax 活性化、 mini-batch + Adam、
  L2 正則化対応。 `fitMLPRegressor` (MSE) / `fitMLPClassifier`
  (cross-entropy + softmax) / `predictMLP` / `predictMLPClass`。

### Added (Phase 15: Kalman Filter / State Space)
- `Hanalyze.Model.StateSpace` 新規 — 線形ガウス状態空間モデル。
  `kalmanFilter` で前向きフィルタ + innovation 対数尤度、
  `kalmanSmoother` で RTS 後ろ向きスムージング。
  hmatrix Vector/Matrix で完結。

### Added (Phase 14: DoE 診断 + A/I/E-optimal)
- `Hanalyze.Design.Diagnostics` 新規 — 設計行列の VIF / D-efficiency /
  A-efficiency / G-efficiency / I-efficiency / Alias Matrix を一括算出。
- `Hanalyze.Design.Optimal.OptCriterion` に `IOpt` (I-optimal、 self-moment
  近似) と `EOpt` (E-optimal、 最小固有値最大化) を追加。
- 対応する specialization 関数 `iOptimal` / `eOptimal`。

### Added (Phase 13: Tier 2 軽量バンドル)
- `Hanalyze.Stat.Test.friedmanTest` — repeated-measures ノンパラ ANOVA。
- `Hanalyze.Stat.Test.dunnTest` — Kruskal-Wallis 後の Dunn 多重比較
  (Holm 補正済 p_adj)。
- `Hanalyze.Stat.Effect.cohenDCI` — Cohen's d の 1-α CI (Hedges-Olkin SE)。
- `Hanalyze.Stat.Effect.eta2CI` — η² の CI (noncentrality 二点近似)。
- `Hanalyze.Design.Quality.processCapabilityWeibull` /
  `processCapabilityLogNormal` — 非正規分布対応の Cp/Cpk (ISO 22514 推奨
  パーセンタイル法)。
- `Hanalyze.Model.RandomForestClassifier` — bootstrap aggregation の分類版 RF。
  OOB error + permutation importance を併出。
- `Hanalyze.Model.FitYByX` — JMP "Fit Y by X" platform 相当の wrapper。
  X/Y の型 (Continuous/Categorical) で LM / ANOVA / logistic GLM / chi-square
  に自動 dispatch。

### Added (250 + 260 + 270: TOST + Hierarchical Clustering + AFT — Phase 12)
- `Hanalyze.Stat.Test.tostWelch` — Two One-Sided Tests for equivalence
  using Welch's degrees of freedom. Returns `TestResult` with the
  conventional 90% CI (1 − 2α).
- `Hanalyze.Model.HierarchicalCluster` — agglomerative clustering with
  Single / Complete / Average / Ward linkage via Lance-Williams update
  (O(n²)). `cutTree` extracts K-cluster labels from the merge sequence.
- `Hanalyze.Model.AFT` — Accelerated Failure Time parametric survival
  model. Supports Weibull / LogNormal / LogLogistic / Exponential, MLE
  via Nelder-Mead, handles right-censored observations. `predictAFT`
  returns expected lifetime.

### Added (240: SPC EWMA / CUSUM — Phase 11)
- `Hanalyze.Stat.SPC` extended with two small-shift detection charts.
  - `EWMAChart` + `EWMAInput` — exponentially weighted moving average with
    time-varying control limits (`μ₀ ± L σ √(λ/(2−λ)·(1−(1−λ)^{2i}))`).
  - `CUSUMChart` + `CUSUMInput` — two-sided cumulative sum (`C⁺`, `C⁻`)
    with allowance `k` (σ units) and decision interval `h` (σ units).
  - Both reuse the existing `SPCChartResult` / rule-checking infrastructure.
- 7 hspec tests cover in-control, shifted, parameter validation, and the
  asymptotic EWMA limit `μ₀ ± L σ √(λ/(2−λ))`.
- `Hanalyze.Optim.Desirability` (Phase 11.3) was already implemented in
  an earlier phase — no change needed.

### Added (220 + 230: Gauge R&R + Discriminant Analysis)
- `Hanalyze.Design.GaugeRR` — ANOVA-based Measurement System Analysis.
  - `gaugeRRCrossed` (operator × part 直交設計、 期待値式から σ²_repeat /
    σ²_reproducibility / σ²_part を計算)
  - `gaugeRRNested` (簡略版、 詳細実装は将来 Phase)
  - 結果: `GaugeRRResult { grrPartVar, grrReproducVar, grrRepeatVar,
    grrTotalVar, grrPct*, grrNumDistinct }`
  - 出典: AIAG MSA Manual 4th ed.
- `Hanalyze.Model.Discriminant` — LDA / QDA 判別分析。
  - `fitLDA` (pooled covariance, 線形決定境界) /
    `fitQDA` (クラス別 covariance, 二次決定境界)
  - `predictDiscriminant` で予測ラベル + posterior 行列
  - 数値安定化: hmatrix linearSolve + log-det を使用
  - 比較先: scikit-learn `LinearDiscriminantAnalysis` /
    `QuadraticDiscriminantAnalysis`
- Both modules: hmatrix Matrix / Vector arithmetic only (no list drift).
- Closes requests `request/done/220-gauge-rr.md` and
  `request/done/230-discriminant.md`.

### Added (210: Partial Least Squares regression)
- `Hanalyze.Model.PLS` — chemometrics-standard PLS regression with NIPALS
  implementation. SIMPLS reserved for a future Phase 9.5. All math via
  hmatrix Matrix / Vector arithmetic (no list drift).
- `PLSConfig` / `PLSFit` / `PLSAlgorithm` / `defaultPLSConfig`
- `fitPLS`, `fitPLS1`, `predictPLS`, `predictPLS1`
- `selectPLSComponentsCV` (k-fold CV + 1-SE rule for component count)
- PLSFit carries T / P / Q / W, back-transformed coefficient `plsCoef`,
  per-component R²X / R²Y, and Wold-formula VIP vector
- Closes request `request/done/210-pls.md`

### Added (190 + 200: Mixture Design + Sequential RSM)
- `Hanalyze.Design.Mixture` — blending-experiment designs where component
  proportions sum to 1:
  - `data MixtureDesignType = SimplexLattice Int | SimplexCentroid`
  - `data MixtureResult { mdMatrix, mdNComponents, mdNRuns, mdType }`
  - `mixtureDesign :: MixtureDesignType -> Int -> Either Text MixtureResult`
  - SimplexLattice {m, d}: all non-negative integer m-tuples with Σ = d,
    scaled by 1/d. Point count C(m+d−1, d).
  - SimplexCentroid (m): every k-subset of {0..m−1} placed at 1/k.
    Point count 2^m − 1.
  - Bounded extreme-vertices design deferred to a future phase.
  - Closes request `request/done/190-mixture-design.md`.
- `Hanalyze.Design.Sequential` — driver helpers for the iterative
  "fit → steepest ascent → next CCD" RSM loop:
  - `data SteepestAscentResult { sarDirection, sarStepPoints, sarMaximize }`
  - `steepestAscent :: Bool -> [Double] -> [Double] -> Double -> Int
                    -> SteepestAscentResult` (raw first-order coefficients)
  - `steepestAscentFromQuad :: Bool -> [Double] -> QuadFit -> Double -> Int
                            -> SteepestAscentResult` (extracts β_main from a
    `Hanalyze.Design.RSM.QuadFit`)
  - `data SequentialCCDResult { sccdCenter, sccdSpan, sccdCoded, sccdReal }`
  - `sequentialCCD :: [Double] -> Double -> Int -> RSM.CCDType -> Int
                   -> SequentialCCDResult` (places a CCD around a new
    center and returns both coded and original-unit row sets).
  - Closes request `request/done/200-sequential-rsm.md`.

### Added (170 + 180: Space-filling designs + Definitive Screening Design)
- `Hanalyze.Design.SpaceFilling` — Latin Hypercube + Maximin LHS + Halton
  space-filling designs for computer experiments / surrogate modelling:
  - `latinHypercube :: Int -> Int -> GenIO -> IO SpaceFillingDesign`
  - `latinHypercubeMaximin :: Int -> Int -> Int -> GenIO -> IO SpaceFillingDesign`
    (swap-based local search to maximise minimum pairwise distance while
    preserving the LHS stratification)
  - `haltonDesign :: Int -> Int -> SpaceFillingDesign` (deterministic
    low-discrepancy sequence; same input → same output)
  - `SpaceFillingDesign { sfdMatrix, sfdNPoints, sfdNDims, sfdMinDist,
    sfdMethod }` carries the quality metric so callers can compare methods
  - Closes request `request/done/170-spacefilling.md`
- `Hanalyze.Design.DSD` — Definitive Screening Design (Jones-Nachtsheim 2011):
  - `dsdDesign :: Int -> Either Text DSDResult` produces a 2k+1 run / k factor
    DSD with values in {-1, 0, +1}
  - `DSDResult { dsdMatrix, dsdNFactors, dsdNRuns, dsdHasOptimal }`
  - k = 4: verified DSD from the Jones-Nachtsheim Table 1 conference matrix
    (`dsdHasOptimal = True`)
  - other k ≥ 2: structural DSD via Sylvester-Hadamard sign pattern
    (`dsdHasOptimal = False`; usable run structure but not conference-matrix
    orthogonal)
  - Future phases will add verified DSDs for additional k values
  - Closes request `request/done/180-dsd.md`

### Added (160: D-optimal Augment Design)
- `Hanalyze.Design.Optimal.augmentDesign` — Fedorov-exchange selection of
  additional design rows for a fixed existing design. The existing rows are
  immutable; only the new rows are swapped against the candidate set, so the
  combined design's criterion (`|XᵀX|` for D-opt, `tr((XᵀX)⁻¹)` for A-opt)
  is maximised given prior experimental commitments.
- `AugmentResult { arNewIndices, arNewRows, arFullDesign, arInitialCrit,
  arFinalCrit }` carries both the new rows and a before/after criterion
  comparison so canvas frontend can show the information gain to the user.
- Closes request `request/160-augment-design.md`.

### Added (130 + 140 + 150: GroupComparison + MANOVA + Regularized CV)
- `Hanalyze.Stat.GroupComparison` — new module implementing Spotfire-style
  "Good vs Bad" parallel multivariable comparison. `goodVsBad` computes
  Welch t-test p-value + Cohen's d effect for each named variable and
  returns a list sorted by |effect| descending. Multiple-testing
  correction is left to callers via `Hanalyze.Stat.MultipleTesting`.
- `Hanalyze.Stat.Test` adds three multivariate location tests, all
  returning the existing `TestResult` record:
  - `hotellingsT2 :: Matrix Double -> Vector Double -> TestResult`
    (one-sample H₀: μ = μ_0)
  - `hotellingsT2TwoSample :: Matrix Double -> Matrix Double -> TestResult`
    (equal-variance two-sample H₀: μ_1 = μ_2)
  - `manova :: [Matrix Double] -> TestResult`
    (one-way MANOVA via Wilks' Λ + Rao F approximation)
- `Hanalyze.Model.Regularized` adds k-fold CV λ selection:
  - `data PenaltyKind = KindRidge | KindLasso | KindElasticNet Double` (α)
  - `data LambdaSelection { lsBestLambda, lsLambdas, lsCVScores,
    lsCVScoreSE, lsOneSeLambda, lsKind }`
  - `selectLambdaCV :: Int -> PenaltyKind -> [Double] -> Matrix Double
                    -> Vector Double -> GenIO -> IO LambdaSelection`
  - 1-SE rule (most regularised λ within 1·SE of the best MSE) is
    surfaced in the result.
- Requests `request/130-group-comparison.md`, `request/140-manova.md`,
  and `request/150-regularized-cv.md` close with this change.

### Added (070 + 080: NSGA-II all-fronts + per-generation progress callback)
- `Hanalyze.Optim.NSGA.nsga2AllFronts` and `nsga2AllFrontsWithConstraints` —
  return `[[Solution]]`, one list per rank (0-origin). Useful when downstream
  UI wants to display rank ≥ 1 alternatives next to the Pareto approximation.
  Filtering to the first k+1 fronts is just `take (k+1)` on the result.
- `Hanalyze.Optim.NSGA.nsga2WithProgress` and `nsga2WithProgressAndConstraints` —
  per-generation callback for live progress streaming. Callback fires exactly
  `nsgaGenerations` times with `NSGAProgress { ngpGeneration, ngpTotal,
  ngpParetoSize, ngpBestObjs }`; canvas backend pushes the events over
  SSE / WebSocket for live "best objectives per generation" UIs.
- Existing `nsga2` / `nsga2WithConstraints` are unchanged; the new functions
  share an internal helper `runNSGAFinalPopulation[Cb]` so behaviour is bit-
  for-bit identical on the legacy path.
- Removed dead helper `generationLoop` (was superseded by `generationLoopCb`
  used internally by all current entry points; never exported, only referenced
  in a benchmark comment which still applies).
- Requests `request/070-nsga2-rank.md` and `request/080-nsga2-progress-streaming.md`
  close with this change.

### Added (120: Weibull MLE + accelerated-life models)
- `Hanalyze.Model.Weibull` — new module covering Weibull reliability:
  - `fitWeibullMLE :: Vector Double -> Either Text WeibullFit` — score
    equation solved by 1D bisection (monotonic in k), λ closed form.
  - `fitWeibullCensored :: Vector Double -> Vector Bool -> Either Text WeibullFit`
    — right-censoring via the same equation summing A(k), B(k) over all
    observations and the log-sum over failures only; True = failure
    observed, False = right-censored.
  - `bxLife :: Double -> WeibullFit -> Double` — F⁻¹(p) percentile life.
  - `bxLifeCI :: Double -> Double -> WeibullFit -> (Double, Double, Double)`
    — Wald CI for B_p via delta method (lower clipped at 0).
  - `weibullParameterSE`, `weibullParameterCovariance` — Fisher info inverse,
    marginal SEs and the (k, λ) covariance for caller-side delta-method work.
- `Hanalyze.Model.Reliability` — new module covering accelerated-life models:
  - `fitArrhenius` (`t = A·exp(Ea/k_B·T)`) — log-linearised OLS, returns
    A, Ea (eV), log-likelihood.
  - `accelerationFactor :: ArrheniusFit -> Double -> Double -> Double` —
    AF(T_use, T_test) = exp(Ea/k_B · (1/T_use − 1/T_test)).
  - `fitEyring` (`t·T = A·exp(Ea/k_B·T)·exp(B·S)`) — 3-parameter OLS via
    hmatrix linearSolve.
  - `fitInversePower` (`t = A·S^(-n)`) — log-log OLS, recovers n = -slope.
  - `kBoltzmann` exported (8.617333262145e-5 eV/K).
- Use case: canvas backend `POST /api/analysis/reliability` dispatches
  the four fits and returns parameter estimates, log-likelihoods, and
  (for Weibull) percentile-life Wald CIs for plotting. Request
  `request/120-weibull-reliability.md` closes with this change.

### Added (110: SPC control charts + Western Electric / Nelson rules)
- `Hanalyze.Stat.SPC` — new module covering Statistical Process Control:
  - 6 control charts: X̄-R, I-MR (variable), p, np, c, u (attribute).
  - Common API `fitSPC :: SPCChart -> SPCInput -> Either Text [SPCChartResult]`;
    X̄-R / I-MR return a pair of charts (location + variability).
  - Montgomery (9th ed. Appendix VI) constants A2 / D3 / D4 / d2 hard-coded
    for subgroup sizes n = 2..15; out-of-range n returns `Left`.
  - `SPCChartResult` invariants documented in haddock: UCL/LCL are
    `Vector Double` so p / u charts can vary per point; fixed-limit
    charts return constant-valued vectors.
- `westernElectricRules` — 8-rule WECO set (1956 handbook + common 8-rule
  extension).
- `nelsonRules` — 8-rule Nelson (1984) set; differs from WE only on
  Rule 2 (9 vs 8 consecutive on same side of CL) and rule numbering.
- `checkRules :: [SPCRule] -> SPCChartResult -> [SPCViolation]` —
  pure rule application combinator; fit and rule-checking are decoupled
  so existing chart fits can be re-evaluated against either set.
- Use case: canvas backend `POST /api/analysis/spc` can dispatch all
  six chart kinds and serialise per-point UCL/LCL + violation indices
  for Vega-Lite display. Request `request/110-spc-control-charts.md`
  closes with this change.

### Added (NUTS streaming callback for live MCMC progress)
- `Hanalyze.MCMC.NUTS.nutsStream` — new sampler entry point taking a
  per-iteration callback `(SampleEvent -> IO ())`. Each event reports
  iteration index, burn-in flag, current sample (constrained), Hamiltonian
  energy, divergence / accept flags, and current step size.
- `Hanalyze.MCMC.NUTS.SampleEvent` — exported record carrying the above.
- Existing `nuts` is now a thin wrapper over `nutsStream` with a no-op
  callback (API and behaviour unchanged).
- Use case: downstream apps (e.g. CanvasApp) can push live trace plots /
  R-hat / ESS over WebSocket / SSE without modifying NUTS internals.
## [0.1.0.1] - 2026-05-20

### Changed
- Tightened the `library` lower bound on `dataframe` from `>= 0.3` to `>= 1.3`.
  hanalyze's data layer relies on the `qualified DX` / `DXC` / `DXD` API surface
  introduced in `dataframe-1.3`; the previous range allowed cabal to pick an
  ancient release that no longer builds against the library.

## [0.1.0.0] - 2026-05-19

First public release on Hackage.

### Added (130: HPotfire Vega-Lite migration foundation)
- `Hanalyze.Viz.PlotConfig`: `PlotConfig` moved out of `Viz.Core` and gained
  optional fields `plotColorScheme` / `plotFacetColumn` / `plotLegendPos`.
  `Viz.Core` re-exports both `PlotConfig` and `defaultConfig`, so existing
  imports keep working unchanged.
- `Hanalyze.Viz.PlotData`: source-agnostic intermediate
  `PlotData { pdNumeric, pdText, pdLength }` plus a `ToPlotData` adapter type
  class so future backends (DB / Parquet stream) can feed `*Spec` functions
  without taking a hard `dataframe` dependency. Hackage `dataframe` adapter
  lives in `Hanalyze.Viz.PlotData.DataFrame`.
- `Hanalyze.Viz.Core.vlJson :: VegaLite -> Text` — canonical JSON serialisation
  helper for downstream consumers (HPotfire `/api/viz`).
- `Hanalyze.Viz.Scatter.scatterSpec` / `Histogram.histSpec` /
  `Bar.barSpec` — `PlotConfig -> ... -> PlotData -> VegaLite` entry points.
  Scatter honours `plotColorScheme` / `plotFacetColumn` / `plotLegendPos`.

### Changed (130: Pareto Viz API)
- **BREAKING**: `Hanalyze.Viz.Pareto` rewritten on the `PlotData` convention.
  All public functions (`paretoScatter` / `paretoPair` / `parallelCoordinates`
  / `hypervolumeHistory` / `paretoCompare`) now take `PlotData` instead of
  `[Solution]`. Use the new `solutionsToPlotData :: [Text] -> [Solution] ->
  PlotData` helper to bridge from NSGA-II results.
- Demos `MaterialsMOODemo.hs` and `NSGADemo.hs` updated accordingly.

### Added (090: GLM diagnostics + predict SE)
- `Hanalyze.Model.GLM` exports the previously-internal helpers `Link`,
  `linkFnOf`, `glmDeviance`, `glmLogLik`, `glmVariance` (request 090-CD)
  so HPotfire can drop its local re-implementations.
- `glmPearsonResiduals` / `glmDevianceResiduals` for diagnostics
  (Q-Q / Scale-Location plots).
- `predictGlmEtaWithSE` and `predictGlmMuWithCI` (with `GlmPredictCI`
  record) for proper Wald CI on η and μ scales — replaces the
  `η ± 2·rse` approximation HPotfire has been using.

### Added (100: GLMM SE)
- `Hanalyze.Model.GLMM.glmmFixedSE :: Matrix -> Vector Int -> GLMMResult ->
  Vector Double` — exact LME (Gaussian) fixed-effect SE via
  block-structured `(Xᵀ V⁻¹ X)⁻¹`; non-Gaussian families fall back to a
  `σ² = 1` Gaussian approximation.
- `glmmBLUPSE :: Vector Int -> GLMMResult -> Vector Double` — posterior
  SD of random-intercept BLUPs `(1/σ²_u + n_j/σ²)⁻¹^½`. Suitable for
  forest plot whiskers.

### Fixed (P1: RFF OOM)
- `Hanalyze.Model.RFF.medianPairwiseDist`: rewrote with BLAS gram matrix
  (`Hanalyze.Stat.KernelDist.pairwiseSqDist`) + `Data.Vector.Algorithms.Intro.sort`
  on a flat `Vector`. The previous implementation built an `O(n²)` list of pair
  distances using `rows !! i` (each `O(i)`, so `O(n³)` walks total) and ran a
  naive list quicksort, which exploded space to many GB of thunks and OOM-killed
  WSL2 around `n=768` (e.g. inside `maximizeMarginalLikRBFMV`).
- `Hanalyze.Model.RFF.rbfKernelMat`: rewrote as
  `LA.cmap (...) (KD.pairwiseSqDist x)`. The old nested list comprehension with
  `rows !! i / rows !! j` shared the same `O(n³)` shape and hit the same WSL2
  OOM via `logMarginalLikRBFMV`.
- Removed the file-local naive `qSort` from `RFF.hs`.
- New `bench-rff-oom` executable as a regression guard. Post-fix:
  `maximizeMarginalLikRBFMV` with a 3·2·2 grid runs at `n=768` in ~10 s and
  ~45 MiB peak residency (was OOM).

### Fixed (P4: Tier-2 O(n²) helpers in Preprocess)
- `Hanalyze.DataIO.Preprocess.dropMissingRows`: cache per-column Text
  `Vector` once instead of calling `tryColumnAsList` + @xs !! i@ inside
  the inner row loop. O(rows² × cols) → O(rows × cols).
- `Hanalyze.DataIO.Preprocess.sliceColumn` (`tryAs`): convert the
  column to a `Vector` once and use `unsafeIndex` instead of
  @xs !! i@ in a list comprehension. O(n²) → O(n).

### Fixed (P3: GC pressure / O(n²) helpers)
- `Hanalyze.Model.GP.buildKernelMatrix` (1D variant): rewrote with a
  flat `Storable.Vector` filled via `runST + MVector` instead of
  materialising the @|xs|·|xs'|@ lazy `[Double]` list that the old
  `(n><m) [..]` form created (~30 MB of cons cells at `n=768`, pure
  GC pressure). API is unchanged so the `Periodic` kernel keeps its
  signed-difference behaviour.
- `Hanalyze.Model.GLMM.buildGroups`: replaced `sort . nub` with
  `Set.toAscList . Set.fromList` (O(n log n) vs O(n²)). Important for
  grouping vectors with thousands of distinct group IDs.

### Fixed (P2: stray naive quicksorts)
- `Hanalyze.Model.Quantile.quantile`: replaced file-local naive list quicksort
  with `Data.List.sort` (mergesort, O(n log n) / O(n) space). Pivot-bias could
  push the old version to O(n²) space on adversarial inputs.
- `Hanalyze.Stat.Test.sortVec` and the file-local `qsort` used by
  `mannWhitneyManual`: same replacement (`Data.List.sort` /
  `sortBy (comparing fst)`). Both `qSort`/`qsort` definitions removed.

## [0.1.0.1] - 2026-05-14

Initial Hackage release. (Version 0.1.0.0 was uploaded only as a
candidate and never published; the multi-output GP API was rearranged
before publication — see below.)

### Multi-output GP — API のデフォルトを shared-HP に変更
- `Hanalyze.Model.MultiGP.fitMultiGP` / `fitMultiGPMV` の **挙動を sklearn 流
  shared-HP 版に置き換え**。1 回の HP 最適化で全 q 出力の合算周辺尤度を
  最大化し、`Ky = K + σ_n² I` の Cholesky を再利用する (RBF 専用、
  `q > 1` で旧版比 ~q× 速い)。
- 旧来の per-output 独立 HP 版 (任意カーネル対応) は
  `fitMultiGPIndep` / `fitMultiGPMVIndep` に **改名**。
- 旧 `fitMultiGPMVSharedHP` は新しい `fitMultiGPMV` に統合済 (削除)。
- 既存ユーザーは `fitMultiGP kern ...` を `fitMultiGPIndep kern ...` に
  置き換えれば従来の挙動を維持できる。

### LM diagnostics + Taguchi/Quality 拡張
- `Hanalyze.Model.LM.Diagnostics` (new module): inference and residual diagnostics
  for OLS — `ciTValue`, `lmStdErrors[Multi]`, `CoefStats` /
  `lmCoefStats[Multi]` (SE / t / two-sided p), `FStat` / `lmFStatistic`
  (whole-model F, follows R-style df1 = p − 1, df2 = n − p), `ICs` /
  `lmInformationCriteria` (R `lm()` convention with k = p + 1, σ counted),
  `hatDiagonal`, `standardizedResiduals`, `cooksDistance`,
  `predictorStdDevs`. Multi-output (Matrix p × q) is the canonical form;
  Vector wrappers cover q = 1.
- `Hanalyze.Design.Orthogonal.OAMetadata` + `listArraysWithSize`: structured
  metadata (name / runs / factors / levels / description) for the
  standard L4–L18 arrays.
- `Hanalyze.Design.Taguchi.SNDetails` + `snRatioWithDetails`: SN ratio bundled
  with sample mean / variance / N.
- `Hanalyze.Design.Taguchi.FactorEffectExt` + `factorEffectsTable`: factor-effect
  rows enriched with `feeRange` and `feeContribution`.
- `Hanalyze.Design.Quality.Capability` + `processCapability` /
  `processCapabilityUpper` / `processCapabilityLower`: Cp / Cpk for
  two-sided and one-sided spec limits.

### Performance (Phase 1-13)
- Build flags: added `-O2 -funbox-strict-fields` to all 75 stanzas (library +
  executables + tests) via the new `common opt` block.
- Strict data: enabled `{-# LANGUAGE StrictData #-}` on 22 hot-path modules
  (Optim.{NSGA,LBFGS,DE,CMAES,CMAESFull,SA,PSO,Common,BayesOpt,Acquisition,
  Pareto,NelderMead,LineSearch}, Model.{GLM,Regularized,RFF,GP,Kernel},
  Stat.{KernelDist,Cholesky}, MCMC.{HMC,NUTS}).
- INLINE pragmas on hot-path wrappers: `Hanalyze.Stat.Cholesky.{cholSolve,cholFactor,
  cholSolveWithFactor}`, `Hanalyze.Stat.KernelDist.{diagAB,rowDotsAB,rowSqNorms}`,
  `Hanalyze.Optim.Common.flipFor`, plus 9 polymorphic helpers in `Hanalyze.Stat.AD`.
- `Hanalyze.Stat.KernelDist.pairwiseSqDist` rewritten with `runST + Storable.Mutable`
  flat-index loop; massiv dependency removed from this hot path
  (16-26% speedup on KR/Gram benchmarks).
- `Hanalyze.Model.GLM.glmLogLik` switched from list-based `zipWith`+`sum` to
  `VS.zipWith`+`VS.sum` (~20% speedup on GLM_logit_n=10000).
- `Hanalyze.Model.GLM.irlsStep` weight/working-response computation switched from
  massiv `MA.map`/`MA.zipWith3` to `VS.map`/`VS.zipWith3`.
- `Hanalyze.Stat.ModelSelect.lmPosteriorLogLiks`/`glmPosteriorLogLiks` switched to
  the same `VS.zipWith`-based pattern (avoids per-sample `LA.toList`
  allocations).
- Benchmark infrastructure: added `bench-tasty` (focused tasty-bench
  micro suite) and `bench-profile` (profiling runner with
  `cabal.project.local: profiling-detail: late-toplevel`). Migrated
  `bench-regression` and `bench-kernel` to use the new
  `BenchUtil.timeitTasty` (adaptive iteration, 5% relative stdev) instead
  of fixed-N `timeit`. CSV output schema is preserved.
- Reverted experiments documented for future reference (all in
  `bench/results/perf_profile_findings.md`):
  parallel `Strategies` on `Hanalyze.Stat.Bootstrap` (Storable allocator
  contention), mutable axpy in Lasso CD (BLAS daxpy already optimal),
  `VS.map`-based `mapMatrix`/`mapVector` (massiv's fused map wins on
  large matrices).

### Documentation
- Added Haddock `>>>` examples to a curated set of pure helpers
  (`Hanalyze.Stat.Interpolate.interp1d`, `Hanalyze.Stat.AdaptiveGrid.uniformGrid`,
  `Hanalyze.Optim.Common.projectToBounds` / `inBounds`, `Hanalyze.Model.MultiOutput.asMultiY`,
  `Hanalyze.DataIO.Log.hasErrors`). The doctest runner test-suite is deferred until
  the cabal/doctest package-db wiring is settled; the examples remain
  valid as Haddock documentation.
- Updated `bench/results/SUMMARY.md` and `bench/results/OPEN_ISSUES.md`
  to reflect Phase 1-13 numbers; deleted stale `bench/results/REPORT.md`
  (Phase B0-B5) and the 160k-line auto-generated `bench/results/summary.md`.

### Release engineering
- `cabal sdist` and `cabal haddock --haddock-for-hackage` both succeed
  cleanly (`cabal check` reports no errors or warnings). Hackage candidate
  upload is left as a manual step:
  ```
  cabal upload dist-newstyle/sdist/hanalyze-0.1.0.0.tar.gz                 # candidate
  cabal upload --documentation dist-newstyle/hanalyze-0.1.0.0-docs.tar.gz   # candidate docs
  cabal upload --publish dist-newstyle/sdist/hanalyze-0.1.0.0.tar.gz       # final
  ```

### Models
- Linear models: `Hanalyze.Model.LM`, `Hanalyze.Model.GLM` (Gaussian / Binomial / Poisson + IRLS),
  `Hanalyze.Model.GLMM` (LME via exact EM, GLMM via Laplace).
- Smoothers: `Hanalyze.Model.Spline` (B-spline / natural cubic),
  `Hanalyze.Model.Kernel` (Nadaraya-Watson + kernel ridge).
- Gaussian process: `Hanalyze.Model.GP` (RBF / Matérn / periodic, single + multi output),
  `Hanalyze.Model.GPRobust` (Student-t / Cauchy via IRLS MAP),
  `Hanalyze.Model.RFF` (random Fourier features, multi-output).
- Regularization: `Hanalyze.Model.Regularized` (ridge / lasso / elastic net).
- Probabilistic DSL: `Hanalyze.Model.HBM` (free monad with structure / log-joint / AD /
  dependency interpretations).

### MCMC and inference
- `Hanalyze.MCMC.MH`, `Hanalyze.MCMC.HMC`, `Hanalyze.MCMC.NUTS`, `Hanalyze.MCMC.Gibbs`, `Hanalyze.MCMC.Slice`.
- `Hanalyze.Stat.VI` (mean-field ADVI), `Hanalyze.Stat.ModelSelect` (WAIC / PSIS-LOO / pseudo-BMA),
  `Hanalyze.Stat.MCMC` (split R-hat, ESS, autocorrelation, KDE).

### Distributions
- `Hanalyze.Stat.Distribution`: 27 distributions including Truncated, Censored, MvNormal,
  Dirichlet, LKJ, Multinomial, ZeroInflated, AR(1).

### Design of Experiments
- `Hanalyze.Design.Factorial`, `Hanalyze.Design.Block`, `Hanalyze.Design.RSM`, `Hanalyze.Design.Optimal`,
  `Hanalyze.Design.Anova`, `Hanalyze.Design.Power`, `Hanalyze.Design.Quality`, `Hanalyze.Design.MultiRSM`,
  `Hanalyze.Design.Orthogonal` (L4-L18), `Hanalyze.Design.Taguchi` (4 SN ratios, inner/outer).

### Optimization
- Single-objective: `Hanalyze.Optim.NelderMead`, `Hanalyze.Optim.LBFGS`, `Hanalyze.Optim.LineSearch`,
  `Hanalyze.Optim.DifferentialEvolution`, `Hanalyze.Optim.CMAES`, `Hanalyze.Optim.CMAESFull`,
  `Hanalyze.Optim.SimulatedAnnealing`, `Hanalyze.Optim.ParticleSwarm`.
- Multi-objective: `Hanalyze.Optim.NSGA`, `Hanalyze.Optim.Pareto`, `Hanalyze.Optim.Acquisition`,
  `Hanalyze.Optim.BayesOpt`, `Hanalyze.Optim.Desirability`.
- Constrained: `Hanalyze.Optim.Constrained` (augmented Lagrangian + penalty).
- Unified `Hanalyze.Optim.Common.Bounds` API for box constraints across all algorithms.

### Data I/O
- `Hanalyze.DataIO.CSV` with `loadAuto` / `loadAutoSafe` / `loadAutoSafeWith`,
  `Hanalyze.DataIO.External` (Parquet / JSON via @dataframe@),
  `Hanalyze.DataIO.Convert`, `Hanalyze.DataIO.Preprocess` (NA handling, group-by, melt, regrid).
- Dirty-data defense: `Hanalyze.DataIO.Log` (W001..W008), `Hanalyze.DataIO.Health`,
  `Hanalyze.DataIO.Sniff` (delimiter / header / comment auto-detection),
  `Hanalyze.DataIO.Clean` (column-cleaning DSL).
- Long-form regrid: `Hanalyze.Stat.Interpolate` (Linear / NaturalSpline / PCHIP),
  `Hanalyze.Stat.AdaptiveGrid` (peak |dy/dz|-based grid), `regridLong`.

### Visualization
- `Hanalyze.Viz.Core` (HTML / PNG / SVG via @vl-convert@), `Hanalyze.Viz.Bar`, `Hanalyze.Viz.Scatter`,
  `Hanalyze.Viz.Histogram`, `Hanalyze.Viz.MCMC` (PyMC-style diagnostics),
  `Hanalyze.Viz.ModelGraph` (Mermaid DAG via Track interpretation),
  `Hanalyze.Viz.ReportBuilder` (compositional report API, 11 `Reportable` instances,
  20+ section helpers including `secInterpolation`).

### Command-line interface
- `hanalyze` with subcommands: `regress`, `info`, `hist`, `doe`, `taguchi`,
  `ridge`, `kernel`, `spline`, `multireg`, `clean`, `melt`, `regrid`.

[Unreleased]: https://github.com/frenzieddoll/hanalyze/compare/v0.1.0.0...HEAD
[0.1.0.0]: https://github.com/frenzieddoll/hanalyze/releases/tag/v0.1.0.0
