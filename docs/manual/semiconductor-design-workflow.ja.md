# 半導体デバイス開発ワークフロー マニュアル

> **対象読者**: 半導体デバイス設計者 / プロセス TEG 担当 / 信頼性 + 歩留 + 特性
> マージン担保を兼任する技術者。 統計的設計手法に明るくなくても読める粒度で書く。
>
> **本マニュアルの目的**: 「Sim でセンター設計 → 実デバイスでマージン探索 → 経験則
> も入れて検証」 という現場ワークフローを、 **単独振りに頼らない多因子 + 多水準
> DoE** + **サロゲートで Sim 加速** という方法論で整理する。 ライブラリ
> `hanalyze` の機能と対応付けながら実務に落とせる手順書にする。
>
> **branch**: feature/phase28-jmp-equivalence-gaps (Phase 22-35 完了)
>
> **ステータス**: Phase 4 完了 (章 1〜10 + 付録 A + 付録 B、 §3.1.1 / §4.0.4
> / §5.1.5 / §7.3.1〜7.3.3 / §7.6 深掘り)。 付録 B 実装は
> `cabal run cis-implant-workflow-demo` で動作可能形に統合済 (LiNGAM 因果
> 探索 + DAG DOT エクスポート含む)

---

## 目次

1. [はじめに](#1-はじめに)
2. [設計の典型ワークフロー](#2-設計の典型ワークフロー)
3. [因子と応答の整理](#3-因子と応答の整理)
4. [広く多水準でかつ交互作用を捉える DoE](#4-広く多水準でかつ交互作用を捉える-doe)
   - 4.0 [現場の運用制約: **1 ロット = 25 水準、 うち 2 つはセンター固定**](#40-現場制約-1-ロット--25-水準-うち-2-つはセンター固定)
   - 4.1 [Definitive Screening Design (DSD)](#41-definitive-screening-design-dsd)
   - 4.2 [Custom Design (I-optimal / D-optimal)](#42-custom-design-i-optimal--d-optimal)
   - 4.3 [Space-filling (Halton / Sobol / LHS)](#43-space-filling-halton--sobol--lhs)
   - 4.4 [Full / Fractional Factorial / Orthogonal Array](#44-full--fractional-factorial--orthogonal-array)
5. [Sim 段階の効率化 (サロゲートモデル)](#5-sim-段階の効率化-サロゲートモデル) — Phase 2 で埋める
6. [実デバイス段階のマージン探索](#6-実デバイス段階のマージン探索) — Phase 2 で埋める
7. [非線形 + 境界値の応答解析](#7-非線形--境界値の応答解析) — Phase 2 で埋める
8. [多目的最適化](#8-多目的最適化) — Phase 2 で埋める
9. [進め方ベスプラ チェックリスト](#9-進め方ベスプラ-チェックリスト)
10. [落とし穴集](#10-落とし穴集)
- [付録 A: ユースケース → ライブラリ機能 早見表](#付録-a-ユースケース--ライブラリ機能-早見表)
- 付録 B: サンプルコード (一気通し) — Phase 2 で追加

---

## 1. はじめに

### 1.1 何を解決するマニュアルか

半導体デバイス開発の典型的なフローは:

1. **Sim** (デバイス + プロセス) でセンター条件を設計
2. 全要件 (歩留、 駆動電流、 リーク、 信頼性、 …) を Sim で同時に満たす条件は
   存在しないことが多い → **実デバイス** で検討
3. 経験則 (前世代の knob、 装置担当の感覚) で気になる軸を振る
4. しかしほとんどが **単独振り (One-Factor-At-A-Time, OFAT)** で、 因子間
   交互作用が見えない
5. 解析時に「線形近似」 でフィッティングしがちだが、 実応答は カウント
   データ (歩留 = N\_pass / N\_die、 リーク = カウント) や 範囲制約 (規格幅、
   駆動下限) を持ち、 **二次関数の極小** や **立ち上がりからのマージン** を
   見つけたい

このマニュアルは、 上記の現場フローを **多因子 DoE + サロゲートモデル + 適切な
応答モデル** で整理し直し、 同じ実験回数で **より多くの情報** を得るための
ベストプラクティスを記述する。

### 1.2 本マニュアルが前提とする現場制約

ユーザ環境から確定している制約 (2026-05-30):

- **1 ロット = 25 水準 (= 25 runs)**
- そのうち **2 水準はセンター条件で固定** (= 強制中心点 2 つ)
- 実質、 因子振りに使えるのは **23 runs**
- 1 ロット試作には数週間〜数ヶ月 + 高コスト → 1 回の実験で取れる情報を最大化したい

この制約下での DoE 設計選択肢は §4.0 で詳述。

### 1.3 ライブラリ `hanalyze` の位置付け

本ライブラリは Haskell 実装の統計 + DoE + サロゲート + 最適化 toolkit。
JMP の機能群 (`Fit Y by X` / `Fit Model` / `DoE > Custom Design` / `Profiler` 等)
に概ね対応し、 各 Phase で機能を拡張中 (現在 Phase 35 まで完了、 Phase 36 候補
構想中)。

本マニュアル中で **コード例** を示す箇所では、 `Hanalyze.*` モジュールの
公開 API を用いる。 対応モジュール / 関数は各節末に **対応コード** として
明記する (file:line 形式)。

---

## 2. 設計の典型ワークフロー

### 2.1 全景図

```
   ┌──────────────────────────────────────────────────────────────┐
   │  Phase A: Sim 駆動の設計                                   │
   │                                                              │
   │   1. 要件定義 (規格、 目標マージン)                          │
   │   2. 因子洗い出し (制御 + 雑音)                              │
   │   3. Space-filling Sim で初期探索 (Halton / Sobol)           │
   │   4. サロゲートモデル構築 (RFF Ridge / GP / RandomForest)    │
   │   5. サロゲート上で最適化 → Sim センター条件 (proposal)     │
   │                                                              │
   └─────────────────────────┬────────────────────────────────────┘
                             │ センター案 + 残課題リスト
                             ▼
   ┌──────────────────────────────────────────────────────────────┐
   │  Phase B: 実デバイスでのマージン探索 (1 ロット 25 runs)      │
   │                                                              │
   │   6. 因子絞り込み (Sim サロゲートの感度上位 + 経験則 knob)   │
   │   7. **DoE 選択** (DSD / Custom / OA、 §4 参照)              │
   │      - 強制中心 2 runs を込みで 25 runs に納める             │
   │   8. ロット実行 → 応答測定                                   │
   │   9. 解析: main / interaction / quadratic 効果分解           │
   │      - 線形近似で済むか、 非線形 (GLM / RSM) が必要かを判断  │
   │  10. プロファイラで応答曲面 + マージン可視化                 │
   │                                                              │
   └─────────────────────────┬────────────────────────────────────┘
                             │ 改善方向 + 残懸念
                             ▼
   ┌──────────────────────────────────────────────────────────────┐
   │  Phase C: 追加検証 (augment、 robust check)                  │
   │                                                              │
   │  11. augment (axial / center 追加) で二次効果 / lack-of-fit  │
   │  12. 雑音因子 (温度、 ロット間、 ウェハ位置) を直交振り      │
   │  13. 多目的最適化 + 検証実験 (final lot)                     │
   │                                                              │
   └──────────────────────────────────────────────────────────────┘
```

### 2.2 各 Phase の責務

| Phase | 主目的 | 1 サイクル工数 | アウトプット |
|---|---|---|---|
| A | 候補因子の洗い出し + サロゲート構築 | 数日〜数週 (Sim 量による) | サロゲートモデル + センター提案 |
| B | 実物応答の取得 + 実マージン特定 | 1 ロット (週〜月) | 効果分解 + 改善方向 |
| C | 補強と確認 | 追加ロット | 最終提案 + 検証データ |

各 Phase の間で「同じ因子定義」 「同じ応答メトリック」 を貫くことが、 サロゲートと
実物データを比較する上で重要 (§3 で定義方法を統一)。

---

## 3. 因子と応答の整理

### 3.1 因子 (Factor) のタイプ分類

DoE / 解析で取り扱える因子のタイプを最初に確定させる。 ライブラリの
`Hanalyze.Design.Custom.Factor` で扱う `Factor` 型に揃える:

| ライブラリ表現 | 半導体現場の例 |
|---|---|
| `Continuous lo hi` | imp 量 (1e13〜5e13)、 アニール温度 (900〜1100℃)、 ゲート長 (28〜36 nm) |
| `DiscreteNum [xs]` | 厚さリスト [3, 5, 7, 10 nm]、 step 回数 [1, 2, 3] |
| `Mixture lo hi` | 比率 (合計 1) — CMP slurry 比率、 reactant 比率 |
| `Categorical [tags]` | マスク種、 装置号機 (Cat A / B / C)、 ガス種 |
| `Ordinal [tags]` | レシピ世代 (gen1 < gen2 < gen3) |

**ルール**:
- **連続でも実機が離散選択** な場合 (装置の設定刻み) は `DiscreteNum` で明示
- カテゴリは効果順に並べたいなら `Ordinal`
- Sim では `Continuous` でも、 実機段階で `DiscreteNum` に切替える運用 OK

**対応コード**: `src/hanalyze/Analyze/Design/Custom/Factor.hs:1`

#### 3.1.1 discrete / categorical 因子の DoE 上の扱い (深掘り)

**「discrete だから単独振りで OK」 は誤り**。 DoE 内では離散因子も他因子と
同列に同時動かす。 ここでは離散因子をどう coding するかと、 design 構築上の
落とし穴を整理する。

##### coding 方式

| coding | 例 (3 水準 A/B/C) | 用途 | quadratic |
|---|---|---|---|
| **Reference (Treatment) contrast** | A → (0,0), B → (1,0), C → (0,1) | LM / GLM、 1 水準を基準に効果差を見たい | ✗ |
| **Sum / Effect contrast** | A → (1,0), B → (0,1), C → (-1,-1) | ANOVA、 全水準平均を 0 にしたい | ✗ |
| **Ordinal numeric coding** | gen1 → 1, gen2 → 2, gen3 → 3 | Ordinal 因子で順序が等間隔と仮定 | △ (擬似) |
| **Polynomial contrast** | -1, 0, +1 (3 水準) | Ordinal で線形 + 二次効果を分離 | ◯ |

**Categorical の場合**: 順序のない離散 (装置号機、 マスク種)。 Reference か Sum
contrast を選ぶ。 quadratic は **意味なし** (水準間に距離がないため)。
ライブラリの `Categorical [tags]` を Custom Design に渡すと内部で Reference
contrast 展開される。

**Ordinal の場合**: 順序のある離散 (世代、 グレード)。 等間隔仮定が成立すれば
**polynomial contrast** で linear + quadratic を直接推定できる。 ライブラリの
`Ordinal [tags]` で対応。

**DiscreteNum の場合**: 数値列だが装置制約で値が選択不可 (例: tilt 角
= {0°, 7°, 15°, 30°})。 数値そのものを使うので **2 水準なら linear、 3 水準
以上なら quadratic も推定可** (ただし水準間隔が不等間隔なら推定の精度に注意)。

##### 落とし穴: 「水準数 = quadratic 推定可能性」

quadratic 効果を推定するには **少なくとも 3 水準** が必要 (1 つの曲線を一意
特定するため)。

| 水準数 | 推定可能な効果 |
|---|---|
| 2 | linear のみ |
| 3 | linear + quadratic |
| 4-5 | linear + quadratic + cubic |
| 5 以上 | より高次 (現場で実用するのは 5 水準まで) |

「広く多水準で振りたい」 場合は **5 水準が実用上の天井**。 これ以上は runs を
食う割に推定精度が伸びない (overfitting / 純誤差増)。

##### 落とし穴: discrete 水準を Custom Design の I-optimal/D-optimal に与える

ライブラリの `Hanalyze.Design.Optimal` は連続因子の coded space [-1, 1] を
前提に Fedorov 交換 + 局所探索で最適化する。 `DiscreteNum [xs]` の場合、
**探索集合を xs に限定** することで離散制約を尊重する (内部実装の `factorGrid`
で対応)。

具体的に:

```haskell
-- tilt 角 (装置制約) を 4 水準離散で指定
let tiltFactor = F.discreteNumFactor "tilt_deg" [0, 7, 15, 30]
-- → I-optimal 探索は {0, 7, 15, 30} の組合せ内のみを候補とする
```

これを **Continuous で書いて手で離散化する** とライブラリの最適化は連続
最適化を返してしまい、 装置で再現不能な値 (例: tilt = 4.3°) が出る危険あり。
**必ず `DiscreteNum` で宣言**する。

**対応コード**: `src/hanalyze/Analyze/Design/Custom/Factor.hs` の `DiscreteNum`
コンストラクタと、 `src/hanalyze/Analyze/Design/Optimal.hs` の探索集合構築部分

### 3.2 応答 (Response) のタイプ分類

応答は **解析モデルを選ぶ前提条件** で、 これを誤ると線形回帰で全部やって
精度を落とすことになる:

| 応答タイプ | 例 | 推奨解析モデル | ライブラリ |
|---|---|---|---|
| 連続 (~正規) | 駆動電流、 Vth、 オン抵抗 | OLS LM / RFF Ridge | Phase 13, 17 |
| カウント (歩留分子) | 良品ダイ数 / ロット | Poisson GLM / Binomial GLM | Phase 13 GLM |
| 比率 (0,1 範囲) | 歩留 (=良品率)、 規格内率 | Logistic / Beta regression | Phase 13 |
| 二値 (pass/fail) | 信頼性試験合否 | Logistic GLM | Phase 13 |
| 打切付き時間 | TDDB / EM 故障時間 | Weibull AFT / Kaplan-Meier | Phase 12 AFT |
| カウント時系列 | 工程内 in-line 計測の経時推移 | State-space + Poisson | Phase 15 |

**「線形 LM で全部やってしまう」** ことが現場でよくある罠 (§10.2 参照)。 応答
タイプに応じた **適切な link function + 分布** で GLM を組むだけで RMSE が
3-5 倍改善することは珍しくない (推測なので **着手時に必ず比較**)。

**対応コード**: `src/hanalyze/Analyze/Model/GLM.hs:1`, `src/hanalyze/Analyze/Model/AFT.hs:1`

### 3.3 要件 (Spec) とマージン目標の表現

各応答に対し:

- **規格 (LSL, USL)**: 製品としての合否ライン
- **目標 (Target)**: 設計が狙う値 (規格中央でない場合もある)
- **目標マージン**: 6σ 換算で何 σ ほしいか (歩留要求から逆算)

これを明文化しないと、 §4 の DoE 設計時に 「どの応答にどれだけ重みを置くか」 が
ブレる。 Multi-objective 最適化 (§8) でも同じ情報が必要。

---

## 4. 広く多水準でかつ交互作用を捉える DoE

### 4.0 現場制約: 1 ロット = 25 水準、 うち 2 つはセンター固定

ユーザ環境の制約 (2026-05-30 確定):

- **N = 25 runs / 1 ロット**
- うち **2 runs は事前にセンター条件 (全因子 = 中央値) で固定** (装置調整 +
  経時 drift モニタ目的と推測)
- 残り **23 runs で因子振りを行なう**

この制約に対する **本ライブラリでの実用解** を 3 つ提示する。 どれが最適かは
因子数 k と、 「主効果のみで OK」 か 「二次効果 + 交互作用も推定したい」 かで
決まる。

#### 選択肢 A — Custom Design (I-optimal) N=23 + AddCenter 2 (推奨第一)

**手順**:

1. `cdsRuns = 23` の Custom Design Spec を組む (model = main + 2-way 交互作用
   + quadratic)
2. I-optimal で 23 行を最適化
3. `augmentMenu (AddCenter 2)` で中央 2 行を追加 → 計 25 行
4. ロット投入時、 装置側で 「2 つの強制中心 run」 を **時系列の先頭 + 末尾**
   等に配置 (装置 drift を出させる目的なら time-randomization と組み合わせる)

**長所**:
- 因子数 / 水準数 / 推定モデルを **自由設定可** (k=4〜10 程度が現実的)
- 強制中心の位置 (先頭固定 / 末尾固定 / 中間挿入) は augment 後の **行並べ替え** で自由
- 二次効果 + 交互作用を同時推定可能
- **本ライブラリの最新 Phase 22-26 機能** に直接乗る

**短所**:
- 直交性は完全には保証されない (I-optimal は予測分散を最小化、 D-optimal は
  情報行列を最大化、 いずれも厳密直交ではない)
- k が増えると 23 runs では quadratic + interaction を全部推定するのは
  情報量不足 → モデルを線形 + 主要 2-way に絞る判断が必要

**対応コード**:
- `Hanalyze.Design.Custom.Coordinate.CustomDesignSpec` (`src/hanalyze/Analyze/Design/Custom/Coordinate.hs:80` 付近)
- `Hanalyze.Design.Optimal.iOptimalDesign` / `dOptimalDesign` (`src/hanalyze/Analyze/Design/Optimal.hs:1`)
- `Hanalyze.Design.Custom.Augment.augmentAddCenter` (`src/hanalyze/Analyze/Design/Custom/Augment.hs:116`)

**コード例 (擬似)**:

```haskell
-- 4 連続因子の場合: imp量、 アニール温度、 ゲート長、 thickness
let factors =
      [ continuousFactor "imp"    1e13  5e13
      , continuousFactor "anneal" 900   1100
      , continuousFactor "Lg"     28    36
      , continuousFactor "tox"    1.5   3.0
      ]
    spec0 = defaultCustomDesignSpec
              { cdsFactors = factors
              , cdsModel   = mainEffects <> twoWayInteractions <> pureQuadratic
              , cdsRuns    = 23
              , cdsCriterion = IOptimal
              }
base   <- runCustomDesignBuild spec0       -- 23 行
final  <- augmentAddCenter factors base 2  -- + 2 center → 25 行
```

#### 選択肢 B — Definitive Screening Design (DSD) k=11、 N=25 (3 centers)

DSD は **2k+1 runs で k 因子の主効果 + 全二次 + 主要二要因交互作用** を同時推定
できる Jones-Nachtsheim 2011 の設計。 中心点が自然に組み込まれる構造。

**N=25 の組合せ**:

| k | DSD runs | 中心点 | 合計 |
|---|---|---|---|
| 12 | 24 (= 2k) | 1 | 25 |
| **11** | **22** | **3** | **25** |
| 10 | 20 | 5 | 25 |

**おすすめ**: **k=11 + 3 centers** で、 ユーザの 2 強制中心は 「3 centers のうち
2 を ロット先頭・末尾に固定配置」 と解釈する (残り 1 中心は中盤に挿入)。

**長所**:
- **直交性が保証** (DSD は main 効果が直交、 quadratic と main は直交)
- **k=11 まで** 因子を入れられる (ユーザの 「広く多水準」 ニーズに合致)
- 主要交互作用も alias なしで推定できる (Jones-Nachtsheim の特性)
- 中心点が DSD 構造に内在 → 装置 drift モニタが自然に組込まれる

**短所**:
- 各因子は **3 水準 (-1 / 0 / +1)** のみ。 「広く多水準」 として 5 水準
  以上を求めるなら不適
- 「立ち上がりからのマージン」 (応答が片側で急変するケース) を探すには
  3 水準では足りない場面がある (→ §7.3 RSM + augment で補強)
- ライブラリの `dsdDesign 11` は **structural DSD** (verified なのは k=4 のみ、
  k≥5 は Hadamard-like 近似)。 直交性は近似的に成立するが、 厳密 conference
  matrix ではない (`dsdHasOptimal = False`)。 → §10.4 落とし穴参照

**対応コード**:
- `Hanalyze.Design.DSD.dsdDesign` (`src/hanalyze/Analyze/Design/DSD.hs:51`)
- 出力は coded space `{-1, 0, +1}` の `(2k+1) × k` 行列

**コード例 (擬似)**:

```haskell
case dsdDesign 11 of
  Left err  -> error (show err)
  Right res -> do
    -- res は 23 runs (= 2*11 + 1)、 うち 1 行が center
    -- 強制中心 2 を追加して合計 25 runs (3 centers)
    let base   = dsdMatrix res            -- 23 × 11
        full   = base LA.=== centerRows   -- + 2 center rows
        ...
```

#### 選択肢 C — Standard Orthogonal Array (L18) + 7 augment (非推奨)

ライブラリには **L25 (5^6) が無い** (L4 / L8 / L9 / L12 / L16 / L18 のみ)。
L18 (18 runs) を基盤に 7 runs を center + axial で augment して 25 runs に
することは可能だが、 **直交性が完全には保たれない** ため積極推奨できない。

**いつ使うか**: 「2 水準因子 8 + 3 水準因子 1 ぐらい」 と明確に決まっており、
L18 が完璧にハマる場合のみ。 augment 部は「装置 drift モニタ」 兼 「外挿軸」
として割り切る。

**対応コード**:
- `Hanalyze.Design.Orthogonal.l18` (`src/hanalyze/Analyze/Design/Orthogonal.hs:133`)

#### 選択肢の比較サマリ

| 観点 | A: Custom (推奨) | B: DSD k=11 | C: L18 + augment |
|---|---|---|---|
| 因子数 | 任意 (現実 4〜10) | 固定 11 | 8〜9 |
| 水準数 | 任意 | 3 のみ | 主に 2 (一部 3) |
| 二次効果推定 | ◎ | ◎ | △ (augment 後しか出ない) |
| 交互作用推定 | ◎ | ○ (主要のみ) | × (alias 多) |
| 強制中心 2 を綺麗に組込み | ◎ (AddCenter) | △ (3 centers のうち 2) | △ |
| 直交性 | △ (I-/D-optimal は予測最適) | ○ (近似) | × (augment 後) |
| 「広く多水準」 | ◎ | △ | × |
| ライブラリ成熟度 | ◎ (Phase 22-26 主力) | ○ (k=4 のみ verified) | △ |

**推奨**: 因子数が 4〜8 で多水準 (5 水準以上) 振りたい場合は **A (Custom)**。
因子数 11 程度を screening したい場合は **B (DSD)**。

#### 4.0.4 因子少 (k=3)、 多 runs 余裕、 discrete 混在 ケースの戦略

ユーザの典型ケース (イオン注入 工程: **dose / energy / tilt 角** の 3 因子)
のように **因子が 3〜4 個しかなく、 25 runs に対し情報過多** の場面では、
余った自由度を 「**多水準化** + **replicate (純誤差推定)** + **lack-of-fit
検出**」 に振り分けるのが定石。

##### 自由度の見立て

3 因子 + 25 runs (= 23 + 2 中心) で **二次モデル (main + 2-way + quadratic)**
を fit すると、 パラメータ数は:

- intercept: 1
- main effects: 3
- 2-way interactions: C(3,2) = 3
- pure quadratic: 3
- **合計 10 パラメータ**

23 runs - 10 = **13 自由度の余裕**。 これだけ余れば:

- **lack-of-fit** (モデルが説明しきれない応答の歪み) を高い検出力で見つけられる
- **純誤差 (pure error)** を replicate 配置で推定できる
- **5 水準以上** にして高次項 (cubic、 立ち上がり) を捉えられる

##### 多水準化 (連続因子)

dose / energy のような連続因子は **5 水準** (= -1, -0.5, 0, +0.5, +1) まで
広げると、 quadratic + 立ち上がり (saturating) を同時推定できる。

```haskell
-- dose / energy は 5 水準連続として宣言。 ライブラリは内部で
-- Continuous 範囲のまま I-optimal 探索するが、 実機投入時の値割付で
-- 5 水準に丸める運用とする。
let factors =
      [ F.continuousFactor "dose"   1e13   5e13     -- 5 levels: 1e13, 2e13, 3e13, 4e13, 5e13
      , F.continuousFactor "energy" 5      50       -- 5 levels: 5, 16.25, 27.5, 38.75, 50 [keV]
      , F.discreteNumFactor "tilt"  [0, 7, 15, 30]  -- 装置制約で 4 水準
      ]
```

または **明示的に DiscreteNum** で 5 水準を渡せば、 ライブラリの探索集合が
それに限定される (実機での丸めが不要、 確実):

```haskell
let factors =
      [ F.discreteNumFactor "dose"   [1e13, 2e13, 3e13, 4e13, 5e13]
      , F.discreteNumFactor "energy" [5, 15, 25, 35, 50]
      , F.discreteNumFactor "tilt"   [0, 7, 15, 30]
      ]
```

候補組合せ総数: 5 × 5 × 4 = 100 → ここから 23 runs を I-optimal で選択。
I-optimal は 「各水準を何度ずつ使うか」 を自動配分する (典型的に **中央値の水準は
多めに**、 **端値の水準は最小限**)。

##### Replicate と純誤差推定

I-optimal の選択時にある条件が **2 回以上重複** することがある (情報量
極大化の結果)。 これは natural な replicate で **純誤差 σ²** を推定できる。
JMP では 「Replication 確認」 として標準機能。

ライブラリで明示的に replicate を追加したい場合:

```haskell
-- 23 runs の I-optimal の上に「中央条件 1 つを追加で 2 回 replicate」
-- とすると総 runs が 25 を超えるので、 元の 23 から 1 引いて 22 runs +
-- 中央 1 + replicate 1 + 強制中心 2 = 25 (構造的に綺麗)
let spec  = ...{ cdsRuns = 22, cdsCriterion = Coord.IOptimal }
ebase  <- Coord.runCustomDesignBuild spec
withRep <- Aug.augmentMenu spec (Aug.Replicate 1)  -- 22 + 1 = 23
final  <- Aug.augmentMenu spec (Aug.AddCenter 2)   -- 23 + 2 = 25
```

ただし I-optimal が既に同条件を 2 回選んでいる場合もあるため、 設計後
**実際の重複数** を確認するのが先 (`Hanalyze.Design.Diagnostics` で
レプリケート位置レポート出力可)。

##### Lack-of-fit 検出

純誤差が推定できれば、 二次モデルの **lack-of-fit F 検定** が走る:

```
F_LOF = MS_LOF / MS_PureError
```

F が大きければ 「二次では捉えきれない非線形 (cubic / 立ち上がり / 飽和)」 が
ある証拠。 検出されたら §7.3 / §7.4 (RSM + segmented / GP / GBM) に進む。

**対応コード**: `Hanalyze.Design.Anova.lackOfFit` (確認要、 grep で確認):

```bash
grep -n "lackOfFit\|LackOfFit" src/hanalyze/Analyze/Design/Anova.hs
```

存在しない場合は `Hanalyze.Stat.Test` 等で F 検定を組合せて実装。

##### k=3 の代替設計案 (参考)

3 因子で 25 runs の他の設計選択肢:

| 設計 | runs | 各因子水準 | quadratic | 適用場面 |
|---|---|---|---|---|
| **Custom I-optimal (推奨)** | 23 + 2c = 25 | 任意 (5+5+4 等) | ◎ | 標準 |
| Box-Behnken k=3 | 12 + 3c = 15 | 3 | ◎ | runs を絞りたい |
| **Box-Behnken k=3 + replicate** | 12 + 3c + 10rep = 25 | 3 | ◎ + LOF 高検出力 | LOF + 純誤差重視 |
| Face-centered CCD k=3 | 8 + 6 + 1c = 15 | 3 (face) | ◎ | 純 RSM |
| **Face-centered CCD + replicate** | 8 + 6 + 1c + 10 = 25 | 3 | ◎ | LOF + 純誤差重視 |
| DSD k=3 | 7 (= 2k+1) | 3 | ◎ (構造組込) | screening 寄り |
| **DSD k=3 + replicate / augment** | 7 + 18 augment | 3〜5 | ◎ | screening → RSM 移行 |

**おすすめ**: 5 水準で立ち上がり捉えたいなら **Custom I-optimal 5+5+4 水準
で 23 runs + 中心 2**。 純 RSM (二次のみ) で十分なら **Box-Behnken + replicate**
も実装容易性で良い。

> 事実: `Hanalyze.Design.RSM` には `boxBehnken :: Int -> Int -> [[Double]]` と
> `centralComposite` が実装済 (`src/hanalyze/Analyze/Design/RSM.hs:1` で確認済)

### 4.1 Definitive Screening Design (DSD)

(§4.0 で詳述したため、 本節は要点のみ)

- 2k+1 runs で k 因子の主効果 + quadratic + 主要 2-way 交互作用を効率推定
- **screening** (どの因子が効くか絞り込む) フェーズの主力
- 5 水準以上を扱いたい場合は次節 4.2 (Custom Design) へ
- 対応コード: `src/hanalyze/Analyze/Design/DSD.hs:1`

### 4.2 Custom Design (I-optimal / D-optimal)

本ライブラリの **設計主力** (Phase 22-26 で 5 phase 投入)。

#### 4.2.1 D-optimal と I-optimal の違い

- **D-optimal**: 情報行列の行列式を最大化。 **モデル係数の推定精度** を上げる
  - 用途: パラメータそのものに興味がある場合 (各因子の感度を比較したい等)
- **I-optimal**: 因子空間の予測分散の積分を最小化。 **予測精度** を上げる
  - 用途: 応答曲面を描いて最適点を探したい場合 (RSM 的なゴール)
- 半導体現場のマージン探索では **I-optimal を default 推奨** (応答プロファイルを
  見て最適条件を選ぶことが多い)

#### 4.2.2 Custom Design Spec の組み方

```haskell
data CustomDesignSpec = CustomDesignSpec
  { cdsFactors     :: [Factor]
  , cdsModel       :: Model           -- main / interaction / quadratic 構成
  , cdsRuns        :: Int             -- 総 runs (本ケースは 23 → augment で +2)
  , cdsCriterion   :: OptCriterion    -- IOptimal / DOptimal
  , cdsInitial     :: Maybe (LA.Matrix Double)  -- 既存設計 (augment 出発点)
  , ...
  }
```

**対応コード**: `src/hanalyze/Analyze/Design/Custom/Coordinate.hs:80`

#### 4.2.3 augment メニュー

設計後の追加実験は `augmentMenu` で 5 種類:

- `Replicate n`  — 既存を複製 (純誤差推定用)
- `AddCenter n` — **本ケースで使う**。 2 center 追加
- `AddAxial α`  — 各連続因子に ±α の axial 点を追加 (二次効果増強)
- `AddRuns n`   — Fedorov 交換で n 行追加 (情報量増強)
- `Foldover`    — 符号反転で alias 解消

**対応コード**: `src/hanalyze/Analyze/Design/Custom/Augment.hs:80`

### 4.3 Space-filling (Halton / Sobol / LHS)

DoE と区別される **サロゲート学習用の準乱数サンプリング**。 Sim 段階 (Phase A) で
広域から均等にサンプルを取りたいときに使う。

- **Halton 列**: 低次元 (~10) で密度均等、 ライブラリで実装済
- **Sobol 列**: 高次元でも均等性が良い
- **Latin Hypercube**: 各因子の周辺分布を均等化

**対応コード**: `src/hanalyze/Analyze/Design/SpaceFilling.hs:1`,
`src/hanalyze/Analyze/Stat/QuasiRandom.hs:1`

### 4.4 Full / Fractional Factorial / Orthogonal Array

**Full Factorial** は因子数 × 水準数の積の runs が必要 (2^5 = 32 runs 等)。
25 runs 制約下では 4 因子 2 水準 = 16 が最大、 5 因子は不可。 → 通常 25 runs
では Fractional または DSD / Custom に倒す。

**Orthogonal Array (Taguchi 系)** はライブラリで L4 / L8 / L9 / L12 / L16 / L18
が利用可能。 **L25 は未実装**。 雑音因子配置 (§6.3 robust design) の内側配置に
便利。

**対応コード**: `src/hanalyze/Analyze/Design/Orthogonal.hs:1`,
`src/hanalyze/Analyze/Design/Taguchi.hs:1`

---

## 5. Sim 段階の効率化 (サロゲートモデル)

Sim 1 点が数時間〜数日かかる場合、 ナイーブに全条件を Sim で潰すのは不可能。
**少数の Sim サンプルから応答を補間する代理モデル (surrogate model)** を立て、
サロゲート上で広域探索 + 最適化を行なう。

### 5.1 サロゲート種別の使い分け

| サロゲート | 強み | 弱み | 推奨場面 | ライブラリ |
|---|---|---|---|---|
| **多項式 (LM)** | 解釈容易、 高速、 不確実性が出る | 非線形 / 局所構造に弱い | RSM 領域 (~30 点で 2 次曲面) | `Hanalyze.Model.LM` (Phase 13) |
| **Gaussian Process (GP)** | 予測 + 不確実性、 滑らかさ事前 | 計算量 O(n³)、 n>1000 不向き | n=50〜500、 BO 用 | `Hanalyze.Model.GP` |
| **RFF Ridge** | GP 近似で高速 (n>1000 OK) | 不確実性は GP より粗、 外挿弱 | n=100〜10000、 滑らか応答 | `Hanalyze.Model.RFF` |
| **RandomForest** | 解釈容易 (importance)、 robust | 滑らかでない (階段状)、 外挿不可 | 非線形 + 特徴重要度知りたい | `Hanalyze.Model.RandomForestRegressor` (Phase 17) |
| **GBM** | 高精度、 残差ベースで強い | tuning 必要、 BO 不確実性無し | 純予測精度重視 | `Hanalyze.Model.GradientBoosting` (Phase 34-A1) |

**初手は GP** を推奨 (BO の native パートナー + 不確実性が出る)。 n>500 で
GP が重くなったら RFF Ridge に移行。 解釈性重視で因子効果を見たいときは RF
importance を併用。

#### 5.1.5 Sim 「重い」 vs 「軽い」 ケース別戦略

Sim 1 点あたりの所要時間で **取れる戦略の幅が大きく変わる**。 経験則として:

| Sim 1 点 | 取れる戦略 | 推奨 surrogate / 戦略 | n の現実値 |
|---|---|---|---|
| **数秒〜数分** | 全空間 brute-force 可 | LM / RF で十分。 BO 不要 | n=1000〜10000 直接 |
| **数十分〜1 時間** | 初期 N=50〜100 + サロゲート | **GP + BO**、 §5.3 標準 | n=50〜200 |
| **数時間** | 厳しい予算管理が必須 | **GP + BO** で 1 点ずつ追加 + **early stopping** (改善停滞で打切り) | n=20〜80 |
| **半日〜1 日** | サロゲート単体では足りない | **multi-fidelity surrogate**: 粗 Sim 大量 + 精 Sim 少量を相関モデルで結合 | 精: n=5〜20、 粗: n=50〜200 |
| **数日〜週** | Sim 駆動最適化は実質不可能 | サロゲートでスクリーニング後、 実機 DoE 直行 | 精: n=3〜10 |

##### 「軽い」 ケース (~分): brute-force + simple model

Sim が分単位なら **空間を均等にスキャン** して可視化する方が早い:

```haskell
-- 3 因子で 10^3 = 1000 点を Halton で生成 → 全 Sim 実行 → 可視化
let xs = haltonSamples 1000  -- ←§5.2 参照
ys <- mapM runSim (map (toRaw factors) xs)
-- RF で fit して importance + partial dependence を見る
let rf = RFR.fit (LA.fromLists xs) (LA.fromList ys) ...
```

サロゲート選択は LM / RF で十分。 BO を組む手間が割に合わない。

##### 「中程度」 ケース (~時間): GP + BO 標準サイクル

これが本ライブラリの BO が最も活きる場面 (§5.3 で詳述)。

```haskell
let cfg = BO.defaultBayesOptConfig { BO.boIters = 50 }
res <- BO.bayesOptND cfg evalSimND  -- 50 反復 = 50 Sim
```

##### 「重い」 ケース (~半日): early stopping + acquisition tuning

予算がさらに厳しい場合:

1. 初期 N=20 で粗くサロゲート
2. **BO で 5〜10 反復**、 各反復で improvement (best_y の変化) を記録
3. **改善が 3 反復連続で閾値以下** なら打切り (early stopping)
4. acquisition は **EI** ではなく **UCB 小 κ** にして活用寄り

```haskell
let cfg = BO.defaultBayesOptConfig
            { BO.boIters       = 10
            , BO.boAcquisition = BO.UCB 1.0    -- κ=1 で活用寄り
            -- early stopping は手動 callback で実装
            }
```

##### 「超重い」 ケース (~日): multi-fidelity surrogate

精 Sim (full physics) と粗 Sim (簡略モデル or 経験式) の **両方が利用可能**な
場合の戦略:

- 粗 Sim を大量 (N=200〜500) 走らせて **大域構造** をサロゲート化
- 精 Sim は少量 (N=5〜20) で **粗との差分** をサロゲート化
- 合成: 精度予測 = 粗予測 + 差分予測

```haskell
-- 粗 surrogate (RFF Ridge で高速)
let rffCoarse = RFF.fitRFFRidge ... xCoarse yCoarse
-- 差分 surrogate (GP で滑らかに補間)
let yDiff = zipWith (-) yFine (map (RFF.predictRFFRidge rffCoarse) xFine)
    gpDiff = GP.fitGP ... xFine yDiff
-- 合成予測
predict x = RFF.predictRFFRidge rffCoarse x + GP.predictGP gpDiff x
```

**実装注意**: ライブラリは現状 multi-fidelity を 1 関数で提供していないので、
上記のように **2 個 fit して合成** する手作業が必要。 将来 Phase NN 候補
(multi-fidelity Bayes opt) として記録する価値あり。

##### 「巨重」 ケース (~週): Sim 駆動最適化を諦める

Sim 1 点が数日以上なら 「Sim 最適化 + 実機検証」 サイクルは現実的でない。
代わりに:

- **Sim は方向性のみ確認** (3〜5 点で sanity check)
- **実機 DoE 主導** で経験的に最適化 (§4.0 の 25 runs 設計を反復)
- 残課題リストを Sim で **後追い検証** だけ実施

このケースでは本マニュアル §6 (実機マージン探索) が主軸。

### 5.2 初期サンプル数の決め方

経験則 (Loeppky-Sacks-Welch 2009):

- **連続因子 k 個 → 初期サンプル N₀ = 10k** が標準ガイドライン
- 5 因子なら N₀ = 50。 Sim 1 点が 1 時間 → 50 時間 = ~2 日で初期 fit 可能
- 不確実性が見えるサロゲート (GP) を使い、 **予測分散が大きい領域に追加点を
  足す** ことで 2-3 割サンプル数を削減できる (§5.3 BO の active)

**初期点の取り方**: **Halton 列 または Sobol 列** で因子空間を均等に埋める。
ライブラリの `Hanalyze.Stat.QuasiRandom.radicalInverse` で Halton 各次元、
`Hanalyze.Design.SpaceFilling` で LHS が利用可能。

```haskell
-- 5 因子で 50 点を Halton で生成
import qualified Hanalyze.Stat.QuasiRandom as QR
let xs = [ [ QR.radicalInverse (primes !! d) i | d <- [0..4] ]
         | i <- [1..50] ]
    primes = [2, 3, 5, 7, 11]
-- 各 xs[i] ∈ [0,1]^5。 因子の (lo, hi) で raw 単位に変換して Sim 投入
```

**対応コード**: `src/hanalyze/Analyze/Stat/QuasiRandom.hs:1`,
`src/hanalyze/Analyze/Design/SpaceFilling.hs:1`

### 5.3 適応サンプリング (Bayesian Optimization)

BO は GP サロゲート + **獲得関数 (acquisition function)** で 「次にどこを Sim
すべきか」 を 1 点ずつ提案する反復アルゴリズム。 サロゲート学習用サンプルを
**最も情報的な順** に取りに行く。

#### BO の典型サイクル

```
1. 初期 N₀ 点を Halton で投入 → Sim 実行 → (X, y) 取得
2. GP fit → 獲得関数 (EI / UCB) を因子空間で評価
3. 獲得関数が最大の点を 1 つ追加 Sim → (X, y) 拡張
4. 2〜3 を予算 (Sim 回数 or 改善停滞) まで繰返し
```

ライブラリ:

```haskell
import qualified Hanalyze.Optim.BayesOpt as BO
import qualified Hanalyze.Model.GP       as GP

-- 1D サンプル
let cfg = BO.defaultBayesOptConfig
result <- BO.bayesOpt cfg evaluateSim  -- evaluateSim :: Double -> IO Double

-- N 次元
result <- BO.bayesOptND cfg evaluateSimND  -- :: [Double] -> IO Double

-- 多目的 (スカラー化)
result <- BO.bayesOptScalarMO 100 cfg evalListMulti
```

**対応コード**: `src/hanalyze/Analyze/Optim/BayesOpt.hs:1`, `src/hanalyze/Analyze/Model/GP.hs:1`

#### 獲得関数の選び方 (実用)

| 獲得関数 | 意味 | 使い分け |
|---|---|---|
| **EI** (Expected Improvement) | 期待される改善量を最大化 | 既知の最良点を超えたい局所探索寄り |
| **UCB** (Upper Confidence Bound) | μ + κσ を最大化 | 探索-活用のバランス、 大域寄り |
| **PI** (Probability of Improvement) | 改善確率最大化 | 慎重・局所最適化 |

> 事実: ライブラリの `BayesOptConfig` で acquisition / κ 等を設定可能
> (`src/hanalyze/Analyze/Optim/BayesOpt.hs:1`)

**実務 Tips**:
- 初期は **UCB 大 κ** で広域探索 → 後半 **EI / UCB 小 κ** で活用に倒す
- BO は **探索範囲を縮めない** (因子境界で重みが減るバイアスがない)。 一方
  境界探索で実機が壊れる懸念があれば、 **制約 (constraint) を BO に与える**
  か境界を手動で縮める

### 5.4 サロゲート精度の評価

**LOO-CV** (Leave-One-Out Cross-Validation) で N 点で fit したサロゲートの
予測誤差を測る:

1. i=1..N について: i 番目を抜いて N-1 点で fit → i 番目を予測 → 残差
2. 全残差から RMSE / R² / 残差プロット

**残差プロットの読み方**:

- 散布図が **無パターン** = サロゲート OK
- **漏斗状 (heteroskedastic)** = 分散変動あり、 GLM / 重み付き回帰要検討
- **曲線パターン** = 線形項不足、 quadratic / GP / RF へ
- **クラスタ偏り** = 因子間相互作用未モデル、 相互作用項追加

サロゲート精度が要件範囲 (応答スケール 5% 以下が目安) を満たせば Phase B (実機
DoE) へ移行。 満たさなければ **追加サンプル or サロゲート種別変更**。

---

## 6. 実デバイス段階のマージン探索

実デバイスでの DoE 設計には Sim 段階のサロゲートで分かった **感度因子上位** と
**経験則 knob** を統合する。 §4.0 の 25 runs / 2 強制中心の枠で動かす。

### 6.1 一変数振りから多変数 DoE への移行

現場で OFAT (one factor at a time) が定着している場合、 DoE に移行する際の
プロトコル:

| step | 内容 | 補足 |
|---|---|---|
| 1 | 過去 OFAT データを再解析、 **どの因子が主効果** か可視化 | `Hanalyze.Stat.Test` で t / Wilcoxon |
| 2 | 因子を 5〜8 個に絞る (OFAT で大効果 + 経験則 knob) | 多すぎると 23 runs で推定困難 |
| 3 | **DoE の estimated power** を計算 | `Hanalyze.Design.Power` (Phase 14) |
| 4 | DoE 結果と OFAT 結果を **同じ応答で比較** | 効果方向が逆なら相互作用疑い |
| 5 | DoE が見つけた相互作用を装置担当 + プロセスエンジニアと検証 | 物理的に意味あるか |

OFAT データの再解析は **Phase 13 の Fit Y by X / ANOVA** で大半カバーできる。

**対応コード**: `src/hanalyze/Analyze/Design/Anova.hs:1`, `src/hanalyze/Analyze/Design/Power.hs:1`

### 6.2 経験則を制約として組込む方法

「imp 量 × アニール温度の組合せのうち、 imp 量 ≥ 3e13 かつ温度 ≥ 1050℃ は
拡散プロファイルが崩れる」 のような **既知の禁止領域** をどう DoE に入れるか:

**Custom Design Constraint** で線形制約を表現できる:

```haskell
-- 制約: 2*imp + temp ≤ 1.5 (coded space)
-- = "高 imp かつ 高 temp は禁止"
import qualified Hanalyze.Design.Custom.Constraint as Con

let cons = Con.linearConstraint [(0, 2), (1, 1)] Con.LE 1.5
    spec = ...{ cdsConstraints = [cons] }
```

**対応コード**: `src/hanalyze/Analyze/Design/Custom/Constraint.hs:1`

経験則が「単純な不等式」 でなく **空間的 (curved region)** な場合は:
- 候補グリッドから禁止点を **事前除外** したリストを `Optimal` の探索集合と
  して与える
- I-optimal / D-optimal が許可点だけから 23 行を選ぶ

### 6.3 雑音因子の直交振り (Robust Design)

Taguchi-style の内側 × 外側配置:

- **内側配列 (control factors)**: 制御可能因子。 §4.0 の Custom 23 runs
- **外側配列 (noise factors)**: 制御困難な因子。 温度、 ロット差、 ウェハ位置、
  装置号機

外側を 4 水準 (L4 / L8) 程度に振り、 内側 × 外側の cross 配置で評価すると、
**ロバスト最適点 (雑音感度が低い制御因子設定)** が見つかる。 ただし内側 23
× 外側 4 = 92 runs となり 25 runs 制約とは別予算 (ロット内 4 sub-lot 等)。

**現場の現実解**:
- 外側を **ロット間** で振る (1 ロットは 25 runs 内側、 ロット 4 つで雑音条件
  4 通り)
- ロット間の雑音影響を **ブロック因子** として扱い、 GLM / LM に投入

**対応コード**: `src/hanalyze/Analyze/Design/Block.hs:1`, `src/hanalyze/Analyze/Design/Taguchi.hs:1`

---

## 7. 非線形 + 境界値の応答解析

実機データの解析は **応答タイプに応じた適切なモデル** (§3.2) を使うのが鉄則。
ここでは典型ケース別に手順を示す。

### 7.1 カウントデータ → Poisson / NB GLM

良品ダイ数、 リークビット数、 故障モード数のような **非負整数応答** は Poisson
分布で扱う。 過分散 (Var > Mean) があれば Negative Binomial へ。

```haskell
import qualified Hanalyze.Model.GLM as GLM

-- y = 良品ダイ数 (Poisson)
let fit = GLM.fitGLM GLM.Poisson GLM.LogLink x y
    pred = GLM.predictGLM fit xNew
-- 過分散判定: residual deviance / df > 1.5 なら NB へ
```

**Poisson link = log** なので、 因子の係数は **対数加法** で解釈する (例:
β = 0.3 → exp(0.3) ≈ 1.35 倍の発生率)。

**対応コード**: `src/hanalyze/Analyze/Model/GLM.hs:1`

### 7.2 上下限 (0,1 範囲) → Logistic / Beta / Tobit

歩留 (= 良品率)、 規格内率のような **比率応答** は:

- **ダイ単位の二値 (pass/fail) データ** が取れる場合: **Logistic GLM** で
  ダイレベルに fit (一番情報量が高い)
- **ロット集計後の比率** しか手元にない場合: **Beta regression** (要 GLMM 拡張)
  または **arcsin √p 変換 + LM** (古典的近似)
- **下限 0 で打切 (left-censored)**: **Tobit model** (現状ライブラリ未実装、
  GLM の派生として要望出れば実装可)

```haskell
-- ダイ単位 pass/fail (二値)
let fit = GLM.fitGLM GLM.Binomial GLM.LogitLink x y
```

**Logit link** なので係数は **オッズ対数**。 因子効果は 「オッズ比 = exp(β)」 で
解釈。

### 7.3 二次極小 / 立ち上がりマージン → RSM + canonical analysis

「駆動電流 vs ゲート長」 のように **二次関数の極小 / 極大** を持つ応答は
**Response Surface (二次モデル)** で fit:

```haskell
import qualified Hanalyze.Design.RSM as RSM

-- 設計データを quadratic basis に展開
let qDesign = RSM.quadraticDesign xData  -- main + 2-way + quadratic
    qFit    = RSM.fitQuadratic xData y
    (xStar, yStar, eigvals) = RSM.optimumPoint qFit
-- xStar: 推定極値の因子座標
-- yStar: そこでの応答
-- eigvals: 固有値 (正なら極小、 負なら極大、 混在なら鞍点)
```

**canonical analysis の読み方**:

- **全て正** = 局所極小 (この点で応答最小)
- **全て負** = 局所極大 (応答最大)
- **正負混在 (saddle)** = 鞍点。 ある方向に動かすと改善できる
- **絶対値小の eigenvalue** = その固有ベクトル方向は応答が平坦 (動かしても
  応答変わらない) → **マージン方向** として活用

「立ち上がりからのマージン」 は二次モデルでは表現しきれない (一次 + 二次の
スムーズな曲線にしかならない) ため、 **二次モデル + 区分線形 / GP** の併用が
有効 (§7.3.2 で詳述)。

**対応コード**: `src/hanalyze/Analyze/Design/RSM.hs:1`, `src/hanalyze/Analyze/Design/MultiRSM.hs:1`

#### 7.3.1 二次極小 / 鞍点の工学的判断 (深掘り)

`optimumPoint` の返す eigenvalue + 固有ベクトルから設計判断に落とすパターン:

##### パターン 1: 全 eigenvalue > 0 (局所極小、 応答最小)

- 例: 「リーク Ioff の最小化」 で全因子に対し 2 次が凸 → 中心が最適
- **設計判断**: `xStar` (極小座標) を採用候補に。 ただし境界の確認 (因子範囲
  の中央付近なら堅牢、 端なら範囲拡大で更に下げられる可能性)

##### パターン 2: 全 eigenvalue < 0 (局所極大、 応答最大)

- 例: 「歩留 max」 で凸 (上に凸) → `xStar` が最大化点
- **設計判断**: 採用候補だが、 規格を満たしていれば **最大点よりやや内側** を
  選ぶことも (端効果に弱いため、 robust 観点)

##### パターン 3: 正負混在 (鞍点)

- 例: 駆動電流 vs Lg / tox は飽和方向と凹方向が混在しがち
- **設計判断**: `xStar` は鞍点。 **改善方向** = 負 eigenvalue に対応する固有
  ベクトル方向に動かす → augment 実験で確認

```haskell
let (xStar, yStar, eigs) = RSM.optimumPoint qFit
    posDirs = [ vi | (eig, vi) <- zip eigs eigVecs, eig > 0 ]
    negDirs = [ vi | (eig, vi) <- zip eigs eigVecs, eig < 0 ]
-- negDirs 方向に `xStar` から ±α 動かした点を augment で追加実験
```

##### パターン 4: |eigenvalue| が極小 (応答平坦方向)

- 例: 「電気特性に効かない方向」 = プロセス変動を吸収できる方向
- **設計判断**: その固有ベクトル方向は **プロセスマージン** として活用
  (ロット間 drift / ウェハ位置依存をその軸方向に逃がす)

#### 7.3.2 「立ち上がりからのマージン」 を捉える

二次関数は **滑らかな放物線** しか書けないが、 現場応答には:

- **閾値型 (threshold)**: dose < D_th では何も起きず、 D_th を超えると急増
- **飽和型 (saturating)**: energy 増加で初期は伸びるが、 ある値で頭打ち
- **片側急変 (cliff)**: tilt 角を超えると一気に特性劣化

これらは二次モデルでは **平均化されて見えなくなる**。 対策:

##### A. Segmented (区分線形) regression

「立ち上がり点 (knot)」 を 1 つ仮定し、 knot 前後で別 slope を fit:

```
y = β₀ + β₁ x          (x ≤ knot)
y = β₀ + β₁ x + β₂ (x - knot)   (x > knot)
```

ライブラリの直接 segmented regression API は **現状未実装** (Phase NN 候補)。
代替:
- **knot を grid search** で複数試行し、 各 fit の RSS を比較
- **Adaptive Lasso** (Phase 31) で hinge basis (max(0, x - c)) を多数候補から
  選択
- **Spline 基底** で柔軟に fit (`Hanalyze.Model.FDA` の `smoothBasis` を流用、
  Phase 33)

##### B. Hinge basis + LM

「立ち上がり candidate 点」 を grid で複数置き、 hinge basis として LM に投入:

```haskell
-- dose の立ち上がり candidate を 5 点置く
let doseKnots = [1.5e13, 2e13, 2.5e13, 3e13, 3.5e13]
    hingeBasis x k = max 0 (x - k)
    -- design matrix に hinge basis 列を追加
    xExt = LA.fromColumns
             (LA.toColumns x ++
              [ LA.fromList [ hingeBasis (LA.atIndex x (i,0)) k | i <- [0..n-1] ]
              | k <- doseKnots ])
    fit = LM.fitLM xExt y
-- Adaptive Lasso で重要 knot を選択 (Phase 31-A1)
```

立ち上がり点が特定できれば、 **マージン = 立ち上がり点 - 装置設定中央値** と
して定量化。

##### C. GP で滑らかに補間 (knot 自動推定)

GP の RBF kernel は滑らかすぎて閾値を捉えにくいが、 **Matern 1/2** や **piecewise
constant kernel** なら閾値型に近い応答を捉える:

```haskell
let kernel = GP.Matern52   -- 滑らかさ控えめ、 閾値捉えやすい
    gpRes  = GP.fitGP (GP.GPModel kernel ...) ...
-- GP 予測の傾き |d/dx prediction| が大きい点 = 立ち上がり候補
```

> 事実: `Hanalyze.Model.GP.Kernel` には RBF / Matern 系が実装済
> (`src/hanalyze/Analyze/Model/GP.hs:1` の `data Kernel` で確認)

##### D. 二次モデル + lack-of-fit が高い場合の signal

§4.0.4 で触れた lack-of-fit が **dose 軸方向に大きい場合**、 それは「dose に
対し二次では捉えきれない閾値 / 飽和」 の signal。 LOF が出たら automaticaly
§7.3.2 のセグメント or GP 解析へ進む判断基準にする。

#### 7.3.3 dose / energy / tilt の典型応答パターン例

イオン注入工程での経験則 (ユーザの典型ケース):

| 因子 | 期待応答パターン | 推奨モデル |
|---|---|---|
| **dose** vs シート抵抗 | 立ち上がり (D_th 以上で急減) + 飽和 | hinge + LM、 または Matern GP |
| **dose** vs リーク | 中央極小 (低 dose で接合不良、 高 dose で結晶損傷) | 二次 RSM + canonical |
| **energy** vs 接合深さ | ほぼ線形 (E 増 → x_j 増) | LM (一次のみ) |
| **energy** vs ダメージ | 二次 (中央極小 or 単調増) | 二次 RSM |
| **tilt** vs チャネリング抑制 | 階段状 (0° で大、 4-7° で急減、 高 tilt で再増) | discrete factor + 平均/分散比較 (ANOVA) |
| **dose × energy** | 強相関 (実効 dose) | 2-way interaction 必須 |
| **dose × tilt** | tilt 段により dose 効果のオフセットが変わる | 2-way interaction |
| **energy × tilt** | tilt によりプロジェクテッドレンジが変化 | 2-way interaction |

→ 結論: dose / energy は **5 水準連続** で振り立ち上がり捉え、 tilt は **離散
4 水準 (装置可選択値)**、 2-way 全部入れる、 が最低限。 これは §4.0.4 で
推奨した 「Custom I-optimal 5+5+4 水準で 23 runs + 中心 2」 とちょうど整合する。

### 7.4 非線形がきつい → GP / GBM / RF で可視化

二次でも捉えられない非線形 (段差、 飽和、 多峰性) は **非パラメトリック** で
描く:

```haskell
-- GP で 1 次元プロファイル (因子 j を動かし、 他を中央固定)
let gpModel = GP.GPModel ...
    gpRes   = GP.fitGP gpModel xs ys hyperparams
-- gpRes から partial dependence plot に近い可視化
```

**RF / GBM の partial dependence** は:

```haskell
-- 因子 j を grid で動かし、 他は train 分布で marginalize
let pd j = [ avgPredict rf (fixCol j v xTrain) | v <- gridOf j ]
```

可視化 (matplotlib 等の Python 連携 or `Hanalyze.Viz.*`):
- `Hanalyze.Viz.GP` — GP プロファイル / 不確実性帯
- `Hanalyze.Viz.Pareto` — 多目的 Pareto front

**対応コード**: `src/hanalyze/Analyze/Model/GP.hs:1`,
`src/hanalyze/Analyze/Model/GradientBoosting.hs:1`, `src/hanalyze/Analyze/Viz/GP.hs:1`

### 7.5 交互作用の発見

DoE 解析で **交互作用効果** を見つけるには:

1. **Fit Model に 2-way 項を入れる** + lasso で重要 2-way を選択
   - `Hanalyze.Model.Lasso` (Phase 13/31)
2. **ANOVA で 2-way の F 検定**
   - `Hanalyze.Design.Anova` (Phase 12)
3. **Interaction plot** (visualize) で物理的に意味があるか確認
   - `Hanalyze.Viz.*` (現状の可視化選択肢を確認)

ライブラリの **Adaptive Lasso (Phase 31-A1)** は弱い 2-way 項にバイアスのない
選択ができるため、 sparse な相互作用構造があるときに有効。

### 7.6 応答間の因果探索 (LiNGAM) と DoE 解析の連携

DoE で因子 → 応答の関係を fit するだけでなく、 **応答間の因果構造** が知りた
くなる場面が多い。 たとえば:

- **「dark current の悪化が直接 fwc を削るのか、 別の経路 (defect 増加 →
  リーク → fwc 削る) か」**
- **「歩留低下が defect 直接由来か、 中間特性 (Vth ばらつき) 経由か」**

ライブラリの **`Hanalyze.Model.LiNGAM.*`** + **`Hanalyze.Model.DAG`** で観測
データから因果 DAG を推定できる。 Phase 30 の因果推論 (介入効果) が **DAG 既知
前提**であるのに対し、 LiNGAM はその前段の **構造そのものを学習**する点が
特徴。

#### 7.6.1 LiNGAM variant の使い分け

| variant | 使う場面 |
|---|---|
| **DirectLiNGAM** | 第一選択。 ICA 不要で安定、 中規模 (p ≤ 20) 向き |
| **Bootstrap** | edge の信頼度を出したい (符号合致率 + 出現頻度) |
| **Pairwise** | 2 変数だけ方向判定したい (軽量) |
| **ICA-LiNGAM** | 因子数大、 並列化したい (Shimizu 2006 原典) |
| **VAR-LiNGAM** | 時系列 (in-line モニタの経時データ等) |
| **MultiGroup** | 工場/装置/世代ごとの 「共通構造」 抽出 |
| **Parce** | 潜在交絡 (未観測変数の影響) を疑う場合 |

#### 7.6.2 適用条件 (重要)

LiNGAM は以下を前提とする:

- **線形** : X = B X + e (応答が線形に他応答に依存)
- **非ガウシアン** : noise e の各成分が非ガウシアン分布
- **acyclic** : 因果に循環なし (DAG)
- **独立 noise** : noise 成分間に相関なし (潜在交絡があれば Parce へ)

応答が完全ガウシアンだと ΔMI ≈ 0 で順序が一意決まらない (Bool: 完全ガウス
シアンは識別不能)。 **半導体現場の応答 (defect count、 歩留、 dark current
log) は通常非ガウシアンなので適用可**。

#### 7.6.3 カウントデータ (defect 数万オーダ) と LiNGAM の組合せ

defect count のような **Poisson 系応答** を LiNGAM に直接投入すると注意点:

- **線形仮定が崩れる**: defect は exp(線形関数) の形で他応答に依存することが
  多く、 「log(defect) を入力する」 と LiNGAM の線形前提に乗りやすい
- **数万オーダの値**: 中心極限定理で近似ガウシアン化する (n が大きい時)
  → 識別が落ちる。 **log 変換** + **標準化前提**で扱う方が安全
- **過分散**: defect の分散が平均より大きい場合は Quasi-Poisson 的に扱うが、
  LiNGAM 自体は OLS residual で動くため過分散は順序推定に影響しない (B の
  値だけ歪む)

実用パターン:

```haskell
-- 3 応答行列を組む際、 defect は log 変換
let respMat = LA.fromColumns
      [ LA.cmap log (LA.fromList (map fromIntegral defects))  -- log(defect)
      , yFwc                                                  -- linear
      , LA.cmap log darks                                     -- log(dark)
      ]
    fit = LNG.fitDirectLiNGAM LNG.defaultDirectLiNGAMConfig respMat
```

#### 7.6.4 DoE 因子 + LiNGAM 因果探索の組合せパターン

通常の DoE 解析 (因子 → 応答) と LiNGAM (応答 → 応答) は **直交した情報**を
出す。 両方を組合せると:

1. **DoE 解析で**: dose × tilt → defect 強い (主効果 + 交互作用)
2. **LiNGAM で**: defect → fwc (応答間の因果)
3. **統合判断**: 「dose × tilt の効きは defect 経由で fwc に伝わる」
   = **dose × tilt の制御が fwc 改善の真の knob**

この統合可視化は 「DoE 因子」 + 「応答間 DAG」 を1 つの図に描くことで強力。
ライブラリの `DAG.toDOT` で Graphviz エクスポートして手動編集する想定 (将来、
複合 DAG 自動構築 API は別 NN 候補)。

#### 7.6.5 Bootstrap で edge 信頼度を見る

LiNGAM 単発 fit は noise + n に応じて edge が変動する。 **BootstrapLiNGAM**
で B 回の resample fit を取り、 出現頻度 + 符号合致率を見ると安全:

```haskell
import qualified Hanalyze.Model.LiNGAM.Bootstrap as LBoot

let cfg = LBoot.defaultBootstrapConfig { LBoot.bcNumBootstraps = 100 }
res <- LBoot.fitBootstrapLiNGAM cfg respMat
let dag = LBoot.confidenceDAG 0.7 0.8 res
       -- 出現頻度 ≥ 0.7 かつ符号合致率 ≥ 0.8 のエッジだけ採用
```

実用ライン: **出現頻度 0.7 / 符号合致率 0.8** を最低ラインとする。 これ以下の
edge は 「弱い因果 or 偽陽性」 と切り捨て、 上位だけ採用判断に使う。

#### 7.6.6 DAG 可視化と Graphviz

`DAG.toDOT` で Graphviz DOT 形式に変換可能:

```haskell
import qualified Data.Text.IO as TIO
import qualified Hanalyze.Model.DAG as DAG

let dag = LNG.dlDAG cfg fit
    dagWithLabels = DAG.withNames (V.fromList ["defect", "fwc", "log_dark"]) dag
TIO.writeFile "dag.dot" (DAG.toDOT dagWithLabels)
```

シェルで:

```
$ dot -Tpng dag.dot -o dag.png
```

または:

```
$ dot -Tsvg dag.dot -o dag.svg
```

**実装デモ**: `cabal run cis-implant-workflow-demo` で本マニュアルの典型ケース
(3 応答 + 3 因子) を実際に走らせると、 §6 で LiNGAM 因果探索、 §7 で DOT
出力までを動作確認できる。

---

## 8. 多目的最適化

複数の応答 (歩留 / 駆動電流 / リーク / 信頼性) すべてに規格を満たし、 さらに
**全体スコアを最大化** したい。 ライブラリは 2 つのアプローチを提供:

### 8.1 Desirability function (Derringer-Suich)

各応答に desirability d ∈ [0, 1] を定義 (= 0 が許容外、 1 が望み通り):

- **Target type** (目標値): d = 1 at target、 LSL/USL で 0、 区間で線形に減衰
- **Maximize type** (大きいほど良い): d = 0 below LSL、 d = 1 above target
- **Minimize type** (小さいほど良い): d = 0 above USL、 d = 1 below target

全体スコアは **幾何平均** D = (d₁ · d₂ · … · dₘ)^(1/m) (= 1 つでも 0 なら全体 0、
バランス重視)。

```haskell
import qualified Hanalyze.Optim.Desirability as Des

let types = [ Des.Maximize 100 200       -- 駆動電流: 100=LSL、 200=target
            , Des.Minimize 1e-9 1e-12    -- リーク: 1e-9=USL、 1e-12=target
            , Des.Target 1.0 0.5 1.5     -- Vth: 1.0=target、 0.5/1.5=spec
            ]
    score = Des.overallDesirability types [drainI, leak, vth]
-- score を最大化する因子設定を探す (BO や Optimal で)
```

**対応コード**: `src/hanalyze/Analyze/Optim/Desirability.hs:1`

### 8.2 Pareto front + Hypervolume

Desirability で重み付けせず **トレードオフを全部見たい** 場合は Pareto 最適解
集合を取得:

```haskell
import qualified Hanalyze.Optim.Pareto as Pa

-- candidates :: [[Double]] (各サンプルの応答ベクトル)
let front = Pa.paretoFront candidates    -- 非劣解だけ残す
    hv    = Pa.hypervolume refPoint front  -- 集合の質を 1 数値で
```

**Pareto front の使い方**:

- Sim で 100 候補生成 → サロゲート評価 → Pareto front を可視化
- ユーザがトレードオフを見て **絞り込み点** を選択 (人間のドメイン知識を
  最終判断に組み込む)
- BO 多目的版 `bayesOptScalarMO` で hypervolume を最大化する point を提案

**対応コード**: `src/hanalyze/Analyze/Optim/Pareto.hs:1`, `src/hanalyze/Analyze/Viz/Pareto.hs:1`

### 8.3 検証実験計画

最適化結果を採用する前に:

1. **最適点周辺で 3〜5 点の追加実験** (中央 1 + 端 2〜4) を計画
2. 応答が予測通り出るか確認 → 大きく外れたら **model misspecification** 疑い
3. 雑音因子変動下でも規格内かを **robustness check** (§6.3)
4. 最終 condition は **ロット 2-3 回反復** で再現性確認

---

---

## 9. 進め方ベスプラ チェックリスト

### 9.1 Phase A (Sim 駆動設計) 着手前

- [ ] 要件 (LSL / USL / Target) を全応答について書き出した
- [ ] 因子のタイプ (Continuous / Discrete / Categorical) を確定した
- [ ] 因子の探索範囲 (lo, hi) を装置仕様 / 経験範囲から決めた
- [ ] Sim 1 点あたりの所要時間を計測した (サロゲート要否判定材料)
- [ ] 雑音因子 (温度、 ロット等) と制御因子を分離した

### 9.2 Phase A → B 移行時

- [ ] サロゲートの hold-out RMSE が許容範囲内 (応答スケールの 5% 以下を目安)
- [ ] サロゲート上の最適点が因子境界の **端に張り付いていない** (張り付いて
      いるなら範囲拡大を検討)
- [ ] 残課題リスト (Sim では確認できないリスク) を明文化した

### 9.3 Phase B (実デバイス DoE) 着手前

- [ ] **§4.0 の選択肢 A/B/C から DoE を選んだ** (本マニュアル必須ステップ)
- [ ] 強制中心 2 runs の配置タイミング (先頭・末尾・中央) を装置側と相談済
- [ ] ロット内の **run 実行順序** を randomize するか、 経時 drift を別軸に
      とるかを決めた
- [ ] 応答測定の規準点 (どのダイ位置、 何点平均) を確定した

### 9.4 Phase B → C 移行時

- [ ] 効果分解 (main / interaction / quadratic) の有意性 + 物理的意味を確認
- [ ] **線形近似が破綻していないか** を残差プロット + lack-of-fit で確認
      (破綻していたら §7.3 RSM / §7.4 非線形モデルへ)
- [ ] サロゲートと実物の予測の **乖離が大きい因子** を特定した
- [ ] augment (axial 追加 or 別ロット) で何を確認するかを明文化した

---

## 10. 落とし穴集

### 10.1 単独振り (OFAT) に戻る誘惑

「DoE は組むのが面倒、 1 因子ずつ振った方が結果が読みやすい」 という現場感情は
強い。 しかし **交互作用が大きい系では OFAT は最適点を逃す** ことが理論的に示
されている (Box-Hunter-Hunter 1978)。 25 runs の余裕があるなら、 DoE で **全因子
を同時動かす** 方が遥かに効率的。

### 10.2 線形仮定の罠

「とりあえず LM (Linear Model) でフィッティング」 で済ませがちだが:

- **歩留応答 (Binomial)** を LM で扱うと、 予測が [0, 1] から外れる、 残差が
  漏斗状になる、 等の問題で精度劇悪化
- **二次極小** を持つ応答 (例: 駆動電流 vs ゲート長) を LM では捉えられない
- **立ち上がりからのマージン** を LM で測ると、 立ち上がり前の領域の重みで
  係数が引っ張られて立ち上がり点の推定が劣化

対応: **必ず GLM (適切な分布 + link) + RSM の比較を行なう** (§7)。

### 10.3 サロゲート過信

サロゲートで Sim 範囲外を外挿すると平気で間違える。 とくに:

- RFF Ridge は外挿で 0 に戻る (カーネルの性質)
- RandomForest は train データの上下限でクリップされる (segmenting の性質)
- GP は予測分散が膨大に膨らむ (= 信頼できない、 と分散が教える) ので、 これを
  無視しないこと

サロゲートの **uncertainty estimate** (予測分散) を必ず併せて見る (§5.3 BO)。

### 10.4 DSD の `dsdHasOptimal = False` を見落とす

ライブラリの `dsdDesign k` は **k=4 のみ verified** (Jones-Nachtsheim 2011
Table 1 の conference matrix 由来)。 k≥5 は Hadamard-like 近似で構造的に
DSD を組んでいるが、 直交性は近似的。 k=11 で使う場合は、 推定結果の効果
有意性を **augment + 別ロット** で再確認することが望ましい。

**対応コード根拠**: `src/hanalyze/Analyze/Design/DSD.hs:48-53`

### 10.5 強制中心の配置を「ロット内で偏らせる」

2 つの強制中心 run を 「両方ロット先頭」 等に並べると、 装置 drift と中心応答の
時系列相関で **drift 影響が中心 run のみに集中** する。 必ず先頭・末尾 (or
先頭・中盤・末尾) のように **時系列で分散** させる。

### 10.6 推測ベースで進める

「これが効くと思う」 「LM で十分なはず」 等の **推測ベースの判断** で進めて
後で破綻する事例は枚挙にいとまがない。 本マニュアルでも CLAUDE.md 規律
(「推測するな、 計測せよ」) を踏襲し、 サロゲート選択 / 解析モデル選択 /
DoE 種別選択は **必ず計測 (CV、 hold-out、 lack-of-fit) で根拠を持って決定**
する。

---

## 付録 A: ユースケース → ライブラリ機能 早見表

| やりたいこと | ライブラリ関数 / モジュール | 対応 Phase |
|---|---|---|
| 25 runs / 2 強制中心 で多因子設計 | `Hanalyze.Design.Custom.*` + `augmentAddCenter` | Phase 22-26 |
| k=11 因子の screening (3 水準、 2k+1 runs) | `Hanalyze.Design.DSD.dsdDesign` | (既存) |
| L9 / L18 直交表 | `Hanalyze.Design.Orthogonal.{l9,l18,lookupOA}` | (既存) |
| Halton / Sobol / LHS サンプル | `Hanalyze.Stat.QuasiRandom`, `Hanalyze.Design.SpaceFilling` | (既存) |
| I-optimal / D-optimal 探索 | `Hanalyze.Design.Optimal` | Phase 14 |
| augment (Replicate/AddCenter/AddAxial/AddRuns/Foldover) | `Hanalyze.Design.Custom.Augment` | Phase 25-6/7/8 |
| 連続応答の LM / RFF Ridge | `Hanalyze.Model.{LM,RFFRidge}` | Phase 13, 17 |
| カウント / 比率応答の GLM | `Hanalyze.Model.GLM` | Phase 13 |
| 二値 / 信頼性試験合否 | `Hanalyze.Model.GLM` (Logit), `Hanalyze.Model.AFT` (Weibull) | Phase 13, 12 |
| 二次曲面 (RSM) | `Hanalyze.Design.RSM`, `Hanalyze.Design.MultiRSM` | (既存) |
| RandomForest / GBM サロゲート | `Hanalyze.Model.{RandomForestRegressor,GradientBoosting}` | Phase 17, 34 |
| ANOVA / Fit Y by X | `Hanalyze.Design.Anova`, `Hanalyze.Stat.Test.*` | Phase 12-13 |
| Robust regression (外れ値耐性) | `Hanalyze.Model.Robust` | Phase 31 |
| Bayesian Optimization | `Hanalyze.Optim.BayesOpt` (要確認) | (確認要) |
| 因果探索 (LiNGAM 系) | (Phase NN doc 作成済、 未実装) | 未着手 |

> ※ 「要確認」 は本マニュアル Phase 1 執筆時点で実装存在を grep 確認していない
> 項目。 Phase 2 執筆で確定する。

---

## 付録 B: サンプルコード (一気通し)

仮想ケース: **4 因子 (imp 量、 アニール温度、 ゲート長 Lg、 thickness tox)、
3 応答 (駆動電流 Id、 リーク Ioff、 Vth)** で Sim → 実機 → 解析 → 最適化を
一通り行なう。

> ⚠ 本コードは API 整合性を示す **設計テンプレート** で、 動作確認 (build /
> runtime) は未実施。 実コンパイル / 数値検証は次回セッションで `examples/`
> 配下に動作可能形で配置予定。

### B.1 因子定義

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Examples.SemiWorkflow where

import qualified Numeric.LinearAlgebra as LA
import qualified Data.Vector          as V
import qualified Data.Text            as T

import qualified Hanalyze.Design.Custom.Factor      as F
import qualified Hanalyze.Design.Custom.Coordinate  as Coord
import qualified Hanalyze.Design.Custom.Augment     as Aug
import qualified Hanalyze.Design.Optimal            as Opt
import qualified Hanalyze.Stat.QuasiRandom          as QR
import qualified Hanalyze.Model.RFF                 as RFF
import qualified Hanalyze.Model.GP                  as GP
import qualified Hanalyze.Model.GLM                 as GLM
import qualified Hanalyze.Model.LM                  as LM
import qualified Hanalyze.Design.RSM                as RSM
import qualified Hanalyze.Optim.BayesOpt            as BO
import qualified Hanalyze.Optim.Desirability        as Des
import qualified Hanalyze.Optim.Pareto              as Pa

-- 4 連続因子の定義 (raw 単位)
factors :: [F.Factor]
factors =
  [ F.continuousFactor "imp_dose"  1e13  5e13
  , F.continuousFactor "anneal_T"  900   1100
  , F.continuousFactor "Lg_nm"     28    36
  , F.continuousFactor "tox_nm"    1.5   3.0
  ]
```

### B.2 Phase A: Sim サンプル + サロゲート

```haskell
-- Halton 列で 50 点を 4 次元空間に均等配置
haltonSamples :: Int -> [[Double]]
haltonSamples n =
  let primes = [2, 3, 5, 7]
  in [ [ QR.radicalInverse (primes !! d) i | d <- [0..3] ]
     | i <- [1..n] ]

-- coded [0,1]^4 を raw 単位に変換
toRaw :: [F.Factor] -> [Double] -> [Double]
toRaw fs cs = zipWith codedToRaw fs cs
  where
    codedToRaw f c =
      let (lo, hi) = F.factorRange f  -- 仮想 helper
      in lo + c * (hi - lo)

-- Sim 実行 (本来は外部プロセス呼び出し)
runSim :: [Double] -> IO (Double, Double, Double)
                                -- (Id, Ioff, Vth) を返す
runSim = error "TODO: Sim binding"

-- 1) 50 点 Sim
phaseA_sim :: IO ([[Double]], [(Double, Double, Double)])
phaseA_sim = do
  let xsCoded = haltonSamples 50
      xsRaw   = map (toRaw factors) xsCoded
  ys <- mapM runSim xsRaw
  pure (xsCoded, ys)

-- 2) GP サロゲートを Id 応答に対し fit
phaseA_surrogate :: [[Double]] -> [Double] -> GP.GPResult
phaseA_surrogate xsCoded yId =
  let xMat   = LA.fromLists xsCoded
      yVec   = LA.fromList yId
      hyper0 = GP.initParamsFromDataMV xMat yVec
      kernel = GP.RBF  -- 仮: RBF kernel
      hyper  = GP.optimizeGP kernel (concat xsCoded) yId hyper0
      gpRes  = GP.fitGP (GP.GPModel kernel hyper) (concat xsCoded) yId
                        (concat xsCoded)  -- predict at training (LOO 用)
  in gpRes
```

### B.3 Phase B: 実機 I-optimal DoE (23 runs + 2 centers)

```haskell
-- Custom Design Spec: 23 runs、 model = main + 2-way + quadratic
phaseB_design :: IO (LA.Matrix Double)
phaseB_design = do
  let model = Coord.mainEffects <> Coord.twoWayInteractions <> Coord.pureQuadratic
      spec0 = Coord.defaultCustomDesignSpec
                { Coord.cdsFactors   = factors
                , Coord.cdsModel     = model
                , Coord.cdsRuns      = 23
                , Coord.cdsCriterion = Coord.IOptimal
                }
  ebase <- Coord.runCustomDesignBuild spec0  -- :: IO (Either Text Matrix)
  case ebase of
    Left err   -> error (T.unpack err)
    Right base -> do
      -- AddCenter 2 で 25 行に
      case Aug.augmentAddCenter factors base 2 of
        Left err  -> error (T.unpack err)
        Right res -> pure (Aug.amrMatrix res)
```

### B.4 実機データ解析: 多応答パラレル fit

```haskell
-- 25 runs を実機で流し、 3 応答を測定 (CSV 等で取得想定)
type Lot = LA.Matrix Double  -- 25 × 4 (因子)
type Resp = (LA.Vector Double, LA.Vector Double, LA.Vector Double)
                                -- (Id, Ioff, Vth) — それぞれ length 25

analyze :: Lot -> Resp -> IO ()
analyze x (yId, yIoff, yVth) = do
  -- 1) Id (連続): 二次 RSM
  let qFit = RSM.fitQuadratic (LA.toLists x) (LA.toList yId)
      (xStarId, yStarId, eigvalsId) = RSM.optimumPoint qFit
  putStrLn $ "Id 極値: " ++ show xStarId ++ " → " ++ show yStarId
  putStrLn $ "  eigenvalues: " ++ show eigvalsId

  -- 2) Ioff (カウント的、 log-scale): Poisson GLM (実際は値が小さいので
  --    対数応答で LM の方が現実的、 両方比較する)
  let glmFit = GLM.fitGLM GLM.Poisson GLM.LogLink x (LA.cmap round yIoff)
  putStrLn $ "Ioff Poisson 係数: " ++ show (GLM.glmCoef glmFit)

  -- 3) Vth (連続、 ±範囲): LM + 残差プロットで lack-of-fit 確認
  let lmFit = LM.fitLM x yVth
  putStrLn $ "Vth LM 係数: " ++ show (LM.lmCoef lmFit)
```

### B.5 多目的最適化 (Desirability)

```haskell
-- 規格: Id ≥ 100 で target 200 / Ioff ≤ 1e-9 で target 1e-12 / Vth = 1.0 ± 0.5
desirabilityScore :: (Double, Double, Double) -> Double
desirabilityScore (id_, ioff, vth) =
  let types = [ Des.Maximize 100  200
              , Des.Minimize 1e-9 1e-12
              , Des.Target   1.0  0.5 1.5
              ]
  in Des.overallDesirability types [id_, ioff, vth]

-- サロゲートを使った desirability 最大化 (Sim 上で BO)
phaseB_optimize :: IO [Double]
phaseB_optimize = do
  let cfg = BO.defaultBayesOptConfig
      evalScore :: [Double] -> IO Double
      evalScore xCoded = do
        let xRaw = toRaw factors xCoded
        -- 本来はサロゲートで予測 (Sim 呼ばずに)
        (id_, ioff, vth) <- predictFromSurrogate xRaw
        pure (desirabilityScore (id_, ioff, vth))
  res <- BO.bayesOptND cfg evalScore
  pure (BO.boBestX res)  -- 最適 condition (coded)
  where
    predictFromSurrogate = error "TODO: surrogate predict 3-channel"
```

### B.6 Pareto front 可視化 (人手判断併用)

```haskell
-- 候補 1000 点を Halton で生成 → サロゲートで応答予測 → Pareto front
phaseB_pareto :: IO [[Double]]
phaseB_pareto = do
  let candidates_coded = haltonSamples 1000
      candidates_raw   = map (toRaw factors) candidates_coded
  preds <- mapM predictFromSurrogate candidates_raw  -- [(Id, Ioff, Vth)]
  -- 最大化方向に揃える (Pareto は max を仮定): Ioff は負号、 Vth は |1 - x|
  let objs = [ [id_, -ioff, -(abs (vth - 1.0))] | (id_, ioff, vth) <- preds ]
      front = Pa.paretoFront objs
  pure front  -- 可視化はここでは省略 (Hanalyze.Viz.Pareto を利用)
  where
    predictFromSurrogate = error "TODO"
```

---

---

## 改訂履歴

- 2026-05-30 v0.1: Phase 1 (骨格 + 章 1〜4 + 9〜10 + 付録 A) を新規作成
  - 章 4.0 で「25 runs / 2 強制中心」 制約に対応する選択肢 A/B/C を詳述
- 2026-05-30 v0.4: Phase 4 LiNGAM 因果探索連携 (§7.6 新規、 ユーザ要望)
  - §7.6.1〜7.6.6: LiNGAM 7 variant の使い分け、 適用条件、 カウントデータ
    との組合せ (log 変換 + 標準化)、 DoE × LiNGAM 統合パターン、 Bootstrap
    edge 信頼度、 DAG Graphviz 可視化
  - cis-implant-workflow-demo §6 §7 と整合 (実機動作リンク付き)
- 2026-05-30 v0.3: Phase 3 深掘り (A+B+D+E、 ユーザ要望 2026-05-30)
  - §3.1.1: discrete / categorical coding 方式 (Reference/Sum/Polynomial/
    Ordinal numeric)、 水準数 → quadratic 推定可能性、 DiscreteNum を
    Custom Design に渡す手順 (落とし穴: Continuous 化して手動離散化は NG)
  - §4.0.4: 因子少 (k=3) + 多 runs 余裕 + discrete 混在ケース戦略
    (5+5+4 水準で 23 runs + 中心 2、 自由度 13 余りを LOF + 純誤差に振る、
    Box-Behnken + replicate 代替案)
  - §5.1.5: Sim 1 点コスト別戦略 (~分 brute / ~時間 BO 標準 / ~半日 BO
    early stop / ~日 multi-fidelity / ~週 実機主導)
  - §7.3.1: 二次極小 / 鞍点 / 平坦方向の工学的判断 4 パターン
  - §7.3.2: 立ち上がりからのマージン捉え方 (segmented / hinge basis +
    Adaptive Lasso / Matern GP / LOF signal)
  - §7.3.3: dose / energy / tilt 各因子の典型応答パターン表
- 2026-05-30 v0.2: Phase 2 充填 (章 5〜8 + 付録 B 設計テンプレート)
  - 章 5: サロゲート種別比較 + 初期 N=10k 経験則 + BO サイクル + LOO-CV
  - 章 6: OFAT→DoE 移行プロトコル + 経験則の制約化 + Robust Design
  - 章 7: GLM (Poisson / Logit) / RSM canonical / GP partial dependence /
         Adaptive Lasso interaction
  - 章 8: Desirability + Pareto front + 検証実験計画
  - 付録 B: 4 因子 3 応答の一気通しサンプル (Phase A サロゲート → Phase B
         I-optimal 23+2 → 解析 (RSM/GLM/LM 並行) → Desirability/Pareto)
  - **付録 B コード動作確認 + `examples/` 配置は次回**
